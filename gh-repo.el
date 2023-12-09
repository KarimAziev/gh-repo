;;; gh-repo.el --- Create and manage gh repositories -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gh-repo
;; Keywords: lisp, vc, tools
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (request "0.3.2") (transient "0.4.1") (ghub "3.6.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Create and manage gh repositories.

;;; Code:


(require 'url-parse)
(eval-when-compile
  (require 'subr-x))
(require 'ghub)


(require 'shell)
(require 'comint)
(require 'request)
(require 'auth-source)
(require 'transient)

(defvar ivy-last)
(defvar ivy-regex)
(defvar ivy--old-cands)
(defvar ivy-index-functions-alist)
(defvar ivy--all-candidates)
(defvar ivy--index)
(declare-function ivy--insert-minibuffer "ivy")
(declare-function ivy--exhibit "ivy")
(declare-function ivy-state-preselect "ivy")
(declare-function ivy--preselect-index "ivy")
(declare-function ivy--recompute-index "ivy")
(declare-function ivy-set-index "ivy")
(declare-function ivy-alist-setting "ivy")
(declare-function ivy-re-to-str "ivy")
(declare-function ivy--sort-maybe "ivy")
(declare-function ivy--set-candidates "ivy")
(declare-function ivy-state-current "ivy")


(defcustom gh-repo-excluded-dirs '("~/Dropbox"
                                   "~/melpa"
                                   "~/.cache"
                                   "~/.cask")
  "List of directories to exlude from directories to clone."
  :group 'gh-repo
  :type '(repeat directory))

(defcustom gh-repo-excluded-dirs-regex '("\\`\\.")
  "Regular expression list to exclude directories from GitHub repository searches.

A regular expression list to exclude certain directories when searching for Git
repositories. The default value excludes directories starting with a dot, which
typically denotes hidden directories in Unix-like systems.

Each element in the list should be a string representing a regular expression.
Directories matching any of these regular expressions will be ignored during the
search for Git repositories. This can be useful for excluding vendor
directories, build output, or other areas of a file system that should not be
considered part of a project's source code.

To modify this list, use the customization interface or set the value in your
Emacs configuration file using `setq'. Ensure that each regular expression is
properly escaped to match the intended directory patterns."
  :group 'gh-repo
  :type '(repeat directory))

(defcustom gh-repo-default-license "gpl-3.0"
  "Default repository license."
  :type 'string
  :group 'gh-repo)



(defcustom gh-repo-ghub-auth-info '("" . gh-repo)
  "String of USERNAME^MARKER in auth sources."
  :type '(radio
          (cons :tag "Cons cell"
                (string :tag "Github Username")
                (radio
                 :tag "Marker"
                 (symbol :tag "Suffix" gh-repo)
                 (string :tag "OAuth Token")))
          (function-item  :tag "Use gh cli" gh-repo-auth-from-gh-config)
          (function :tag "Custom Function"))
  :group 'gh-repo)

(defcustom gh-repo-download-default-repo-dir 'gh-repo-find-clone-directories
  "Default directory to use when `gh-repo' reads destination."
  :group 'gh-repo
  :type '(radio (repeat
                 :tag "Directory list"
                 :value ("~/") directory)
                (directory :tag "Directory")
                (function-item  :tag "Auto" gh-repo-find-clone-directories)
                (function :tag "Custom Function")))

(defcustom gh-repo-default-repos-limit 30
  "How many repositories to load during completion `gh-repo-read-user-repo'."
  :type 'integer
  :group 'gh-repo)

(defcustom gh-repo-browse-function 'browse-url
  "Function for browsing preview page.

It will be called with one argument - url to open.

Default value is to use xwidgets if available, othervise `browse-url'."
  :type '(radio  (function-item gh-repo--browse-with-xwidget)
                 (function-item browse-url)
                 (function :tag "Custom function"))
  :group 'gh-repo)



(defcustom gh-repo-actions '((?c "clone" gh-repo-clone-repo)
                             (?b "browse" gh-repo-browse)
                             (?e "open with github explorer"
                                 gh-repo-run-github-explorer))
  "Actions for `gh-repo-read-user-repo'.

Each element is a list comprising (KEY LABEL ACTION)

KEY is a character for `read-multiple-choice', and LABEL is a
string which describes an action.

ACTION is a a function which should accept one argument
- repository of user name."
  :type '(alist
          :key-type (character
                     :tag "Key"
                     :value ?c)
          :value-type (list (string
                             :tag "Label"
                             :value "<description>")
                            (function :tag "Function")))
  :group 'gh-repo)

(defcustom gh-repo-annotation-spec-alist '((description "%s" 40)
                                           (visibility "👁️%s" 20)
                                           (stargazers_count "⭐%s" 10)
                                           (open_issues "⁉️%s" 10))
  "Alist of symbol, format string and width for displaying a GitHub repository."
  :group 'gh-repo
  :type '(alist
          :key-type symbol
          :value-type (list
                       (string :tag "Column Name" "%s")
                       (integer :tag "Column Width" 20))))

(defvar gh-repo--search-langs-alist nil)
(defvar gh-repo--search-langs nil)
(defvar gh-repo--cached-auth-data nil)

(defun gh-repo--download-url (url)
  "Download URL and return string."
  (let ((download-buffer (url-retrieve-synchronously url)))
    (prog1
        (with-current-buffer download-buffer
          (set-buffer download-buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-char)
          (delete-region (point-min)
                         (point))
          (buffer-string))
      (kill-buffer download-buffer))))

(defun gh-repo--json-parse-string (str &optional object-type array-type
                                       null-object false-object)
  "Parse STR with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-string str
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'array))
                         :null-object (or null-object :null)
                         :false-object (or false-object :false))
    (require 'json)
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read-from-string str))))

(defun gh-repo--init-languages ()
  "Fetch github languages and stotre them to `gh-repo--search-langs-alist'."
  (setq gh-repo--search-langs-alist
        (mapcar
         (lambda (it)
           (let-alist it
             (cons .name .aliases)))
         (gh-repo--json-parse-string
          (gh-repo--download-url
           "https://api.github.com/languages")
          'alist
          'list))))


(defvar gh-repo-util-host-regexp
  (concat "\\("
          "\\(\\(github\\|gitlab\\|gitlab\\.[a-z]+\\)\\.com\\)"
          "\\|"
          "\\(\\(bitbucket\\|salsa[\\.]debian\\|framagit\\|codeberg\\|git[\\.]savannah[\\.]gnu\\|git[\\.]kernel\\|git[\\.]suckless\\|code[\\.]orgmode\\|gitlab[\\.]gnome\\)[\\.]org\\)"
          "\\|"
          "\\(\\(repo[\\.]or\\)[\\.]cz\\)"
          "\\|"
          "\\(git\\.sr\\.ht\\)"
          "\\)")
  "Regexp matching common git hosts.")

(defvar gh-repo-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'gh-repo-browse-current-repo)
    (define-key map (kbd "C-c C-o") #'gh-repo-browse-current-repo-and-exit)
    map)
  "Keymap to use in minibuffer when searching repos.")


(defmacro gh-repo-util--pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             (if (symbolp fn)
                 `(funcall #',fn ,acc)
               `(funcall ,fn ,acc)))
           functions
           (if (symbolp init-fn)
               `(apply #',init-fn args)
             `(apply ,init-fn args)))))))

(defmacro gh-repo-util--rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defmacro gh-repo-util--compose (&rest functions)
  "Compose FUNCTIONS in reverse order from FUNCTIONS.

Remaining arguments FUNCTIONS are the functions to be composed."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(gh-repo-util--pipe ,@(reverse functions)))

(defmacro gh-repo-util-when (pred fn)
  "Return a function that call FN if the result of calling PRED is non-nil.
Both PRED and FN are called with one argument.
If the result of PRED is nil, return the argument as is."
  (declare
   (indent defun))
  `(lambda (arg)
     (if ,(if (symbolp pred)
              `(,pred arg)
            `(funcall ,pred arg))
         ,(if (symbolp fn)
              `(,fn arg)
            `(funcall ,fn arg))
       arg)))

(defun gh-repo-util-compose-while-not-nil (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (let ((fn))
    (setq functions (reverse functions))
    (setq fn (pop functions))
    (lambda (&rest args)
      (let ((arg
             (unless (null (flatten-list args))
               (apply fn args))))
        (while (setq fn
                     (unless (null arg)
                       (pop functions)))
          (let ((res (apply fn (list arg))))
            (setq arg res)))
        arg))))

(defun gh-repo-util-alist-ssh-hosts (file)
  "Extract SSH hosts and properties from FILE.

Argument FILE is the name of the file to read SSH host configurations from."
  (when (file-exists-p file)
    (let ((result)
          (curr))
      (with-temp-buffer
        (insert-file-contents file)
        (while
            (pcase-let
                ((`(,prop ,value)
                  (split-string (buffer-substring-no-properties
                                 (point)
                                 (line-end-position))
                                nil
                                t)))
              (pcase prop
                ("Host"
                 (when curr
                   (push curr result))
                 (setq curr (cons value (list (cons prop value)))))
                ("#")
                ((pred (stringp))
                 (when curr
                   (let ((cell (cdr curr)))
                     (setcdr curr (append cell
                                          (list (cons prop value))))))))
              (zerop (forward-line 1))))
        (when curr
          (push curr result)))
      result)))

(defun gh-repo-util-normalize-url-filename (filename)
  "Transform FILENAME to git filename."
  (funcall (gh-repo-util-compose-while-not-nil
            (gh-repo-util-when (gh-repo-util--compose
                            not
                            (apply-partially #'string-suffix-p
                                             ".git"))
              (gh-repo-util--rpartial concat ".git"))
            (gh-repo-util--rpartial string-join "/")
            (gh-repo-util-when
              (gh-repo-util--compose
               (apply-partially #'<= 2)
               length)
              (gh-repo-util--rpartial seq-take 2))
            (gh-repo-util--rpartial split-string "/")
            (apply-partially
             #'replace-regexp-in-string
             "^/\\|/$" ""))
           filename))

(defun gh-repo-util-https-url-p (url)
  "Return t if URL string is githost with https protocol."
  (not (null
        (string-match-p
         (concat "https://" gh-repo-util-host-regexp)
         url))))

(defun gh-repo-util-url-https-to-ssh (url &optional ssh-host)
  "Transform URL with https protocol to ssh.
With optional argument SSH-HOST also replace host."
  (require 'url-parse)
  (when-let ((urlobj
              (when (and url
                         (gh-repo-util-https-url-p url))
                (url-generic-parse-url url))))
    (when-let ((host (url-host urlobj))
               (reponame (gh-repo-util-normalize-url-filename
                          (url-filename urlobj))))
      (string-trim (concat "git@" (or ssh-host host)
                           ":" reponame)))))

(defun gh-repo-util-ssh-url-p (url)
  "Return t if URL string is githost with git protocol."
  (string-match-p
   (concat "git@" gh-repo-util-host-regexp)
   url))

(defun gh-repo-util-get-ssh-variants (ssh-url)
  "Return variants of git ssh for SSH-URL."
  (let* ((ssh-config (gh-repo-util-alist-ssh-hosts "~/.ssh/config"))
         (local-alist (mapcar (pcase-lambda (`(,k . ,v))
                                (cons k (cdr (assoc-string "HostName" v))))
                              ssh-config))
         (cell (with-temp-buffer
                 (save-excursion
                   (insert (replace-regexp-in-string "^git@" ""
                                                     ssh-url)))
                 (let ((beg (point))
                       (end))
                   (setq end (re-search-forward
                              gh-repo-util-host-regexp nil t 1))
                   (cons (buffer-substring-no-properties beg end)
                         (string-trim (buffer-substring-no-properties
                                       end
                                       (point-max))))))))
    (setq local-alist (seq-filter (lambda (it)
                                    (equal
                                     (car cell)
                                     (cdr it)))
                                  local-alist))
    (seq-uniq
     (append
      (list ssh-url)
      (mapcar (lambda (it)
                (concat "git@" (car it)
                        (cdr cell)))
              local-alist)))))

(defun gh-repo--expand-pattern (curr pattern)
  "Recoursively expand PATTERN for string CURR.
PATTERN can be either string or function, or list of strings and functions."
  (pcase pattern
    ((pred functionp)
     (funcall pattern curr))
    ((pred stringp)
     (string-match-p pattern curr))
    (_ (seq-find
        (apply-partially #'gh-repo--expand-pattern curr)
        pattern))))

(defun gh-repo--find-in-dir (dir &optional pattern non-visit-pattern max-depth
                               transform-fn current-depth)
  "Return list of files that matches PATTERN in DIR at MAX-DEPTH.

Both PATTERN and NON-VISIT-PATTERN, if non nil,
will be tested against the directory to visit.

It should be either:
- string that will be tested against the current file name of the directory.
- function (will be called with one argument local directory name)
- list of patterns, that will be tested until first non nil result.

If PATTERN matches, it will be added to result, and not be visited.

If NON-VISIT-PATTERN matches, directory will not be visited.

If TRANSFORM-FN is non nil, it should be a function that will be called with one
argument - full directory name.

CURRENT-DEPTH is used for recoursive purposes."
  (setq current-depth (1+ (or current-depth 0)))
  (unless max-depth (setq max-depth 1))
  (when (>= max-depth current-depth)
    (let ((non-essential t))
      (let ((found-dirs))
        (let
            ((default-directory (expand-file-name (file-name-as-directory dir))))
          (dolist
              (curr
               (directory-files default-directory nil
                                directory-files-no-dot-files-regexp t))
            (let ((full-dir (expand-file-name curr))
                  (tramp-archive-enabled nil))
              (cond ((not (file-directory-p full-dir)))
                    ((and non-visit-pattern
                          (gh-repo--expand-pattern curr non-visit-pattern)))
                    ((and pattern
                          (gh-repo--expand-pattern curr pattern))
                     (setq found-dirs
                           (push (if transform-fn
                                     (funcall transform-fn full-dir)
                                   full-dir)
                                 found-dirs)))
                    (t
                     (unless pattern
                       (setq found-dirs
                             (push (if transform-fn
                                       (funcall transform-fn full-dir)
                                     full-dir)
                                   found-dirs)))
                     (when-let ((subdirs (gh-repo--find-in-dir full-dir
                                                             pattern
                                                             non-visit-pattern
                                                             max-depth
                                                             transform-fn
                                                             current-depth)))
                       (setq found-dirs
                             (if found-dirs
                                 (nconc
                                  found-dirs
                                  subdirs)
                               subdirs))))))))
        found-dirs))))


(require 'project)
(defun gh-repo-find-clone-directories ()
  "Return list of git parents directories."
  (let ((parents (delq nil
                       (when (fboundp 'straight--repos-dir)
                         (list (straight--repos-dir))))))
    (gh-repo--find-in-dir "~/" #'project--find-in-directory
                                (append
                                 (list vc-ignore-dir-regexp)
                                 gh-repo-excluded-dirs-regex
                                 (mapcar (apply-partially #'format "\\`%s\\'")
                                         vc-directory-exclusion-list)
                                 (mapcar (apply-partially #'apply-partially
                                                          'file-equal-p)
                                         gh-repo-excluded-dirs))
                                3
                                (lambda (proj)
                                  (let ((parent
                                         (file-name-parent-directory proj)))
                                    (unless (member parent parents)
                                      (push parent parents))
                                    parent)))
    parents))


(defun gh-repo--browse-with-xwidget (url)
  "Visit an URL in xwidget in other window."
  (require 'xwidget)
  (let ((orig-wind (selected-window)))
    (with-selected-window
        (if (minibuffer-window-active-p orig-wind)
            (with-minibuffer-selected-window
              (let ((wind (selected-window)))
                (or
                 (window-right wind)
                 (window-left wind)
                 (split-window-right))))
          (let ((wind (selected-window)))
            (or
             (window-right wind)
             (window-left wind)
             (split-window-right))))
      (xwidget-webkit-browse-url url)
      (when (fboundp 'xwidget-webkit-fit-width)
        (xwidget-webkit-fit-width)))))


(defvar gh-repo-after-create-repo-hook nil
  "List of hooks to run after cloning new repository.")

(defun gh-repo-exec-in-dir (command project-dir &optional callback)
  "Execute COMMAND in PROJECT-DIR, optionally running CALLBACK on completion.

Argument COMMAND is a string representing the shell command to execute.

Argument PROJECT-DIR is a string specifying the directory in which to execute
COMMAND.

Optional argument CALLBACK is a function to call when COMMAND execution finishes
successfully."
  (let ((proc)
        (buffer (generate-new-buffer (format "*%s*" command))))
    (progn (switch-to-buffer buffer)
           (with-current-buffer buffer
             (if (file-exists-p project-dir)
                 (setq default-directory project-dir)
               (mkdir project-dir)
               (setq default-directory project-dir))
             (setq proc (start-process-shell-command
                         (nth 0
                              (split-string command))
                         buffer command))
             (shell-command-save-pos-or-erase)
             (when (fboundp 'shell-mode)
               (shell-mode))
             (view-mode +1))
           (set-process-sentinel
            proc
            (lambda (process _state)
              (let ((output (with-current-buffer
                                (process-buffer process)
                              (buffer-string))))
                (kill-buffer (process-buffer process))
                (if (= (process-exit-status process) 0)
                    (progn
                      (message "finished")
                      (dired project-dir)
                      (let ((default-directory project-dir))
                        (when callback
                          (funcall callback))))
                  (user-error "%s\n%s" command output)))))
           (when (fboundp 'comint-output-filter)
             (set-process-filter proc #'comint-output-filter)))))

(defun gh-repo-auth-info-password (auth-info)
  "Return secret from AUTH-INFO."
  (let ((secret (plist-get auth-info :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun gh-repo--auth-source-get (keys &rest spec)
  "Retrieve authentication data for GitHub repositories.

Argument KEYS is a list of keys for which values are to be retrieved from
the authentication source.

Optional argument SPEC is a variable argument list that specifies the
search criteria for the authentication source."
  (declare (indent 1))
  (let ((plist (car (apply #'auth-source-search
                           (append spec (list :max 1))))))
    (mapcar (lambda (k)
              (plist-get plist k))
            keys)))

(defun gh-repo-authenticate (&optional force)
  "Authenticate a GitHub repository, optionally forcing a `re-authentication'.

Optional argument FORCE is a boolean.
If non-nil, it forces the function to re-authenticate by clearing the cached
authentication data.
The default value is nil."
  (when force (setq gh-repo--cached-auth-data nil))
  (or gh-repo--cached-auth-data
      (setq gh-repo--cached-auth-data
            (pcase gh-repo-ghub-auth-info
              ((pred functionp)
               (funcall gh-repo-ghub-auth-info))
              (`((and ,username
                      (stringp ,username)
                      (not (string-empty-p ,username)))
                 .
                 (and ,marker
                      (symbolp ,marker)
                      ,marker))
               (let* ((user (format "%s^%s" username marker))
                      (token
                       (or (car (gh-repo--auth-source-get (list :secret)
                                  :host "api.github.com"
                                  :user user))
                           (auth-source-forget (list
                                                :host "api.github.com"
                                                :user user
                                                :max 1)))))
                 (cons username
                       (if (functionp token)
                           (funcall token)
                         token))))
              (`((and ,username
                      (stringp ,username)
                      (not (string-empty-p ,username)))
                 .
                 (and ,marker
                      (stringp ,marker)
                      ,marker))
               (cons username marker))
              (_ (gh-repo-read-auth-marker))))))



;;;###autoload
(defun gh-repo-change-user ()
  "Search for gh token in `auth-sources'."
  (interactive)
  (if gh-repo--cached-auth-data
      (setq gh-repo-ghub-auth-info nil)
    (gh-repo-authenticate t))
  (when transient-current-command
    (transient-setup transient-current-command)))


(defun gh-repo-read-auth-marker ()
  "Retrieve and optionally save GitHub auth info."
  (when-let ((variants
              (seq-uniq
               (auth-source-search
                :host "api.github.com"
                :require '(:user :secret)
                :max most-positive-fixnum)
               (lambda (a b)
                 (equal (auth-info-password a)
                        (auth-info-password b))))))
    (pcase-let* ((users (seq-filter (apply-partially #'string-match-p "\\^")
                                    (delq nil
                                          (mapcar
                                           (lambda (it)
                                             (when (plist-get it :user)
                                               (plist-get it :user)))
                                           variants))))
                 (user (if (> (length users) 1)
                           (completing-read
                            "Source:\s"
                            users
                            nil t)
                         (read-string "User: " (car users))))
                 (`(,user ,marker)
                  (split-string user "\\^" t))
                 (cell (cons user
                             (if marker
                                 (intern marker)
                               (read-string "Ghub token: ")))))
      (if (yes-or-no-p "Save for future?")
          (customize-save-variable 'gh-repo-ghub-auth-info cell
                                   "Saved by gh-repo")
        (setq gh-repo-ghub-auth-info cell))
      gh-repo-ghub-auth-info)))

(defun gh-repo-load-licences ()
  "Return list of available licences from github api."
  (let ((response (request "https://api.github.com/licenses"
                    :type "GET"
                    :headers
                    `(("accept" . "application/json")
                      ("User-Agent" . "Emacs Restclient")
                      ("content-type" . "application/json"))
                    :sync t
                    :parser 'json-read)))
    (let ((status-code (request-response-status-code response)))
      (cond ((not status-code)
             (user-error "Request failed: Could not reach the server"))
            ((= status-code 200)
             (request-response-data response))
            (t
             (error "Request failed"))))))

(defun gh-repo-load-gitignore-templates ()
  "Fetch list of .gitignore templates from GitHub API."
  (let ((response (request "https://api.github.com/gitignore/templates"
                    :type "GET"
                    :headers
                    `(("accept" . "application/json")
                      ("User-Agent" . "Emacs Restclient")
                      ("content-type" . "application/json"))
                    :sync t
                    :parser 'json-read)))
    (let ((status-code (request-response-status-code response)))
      (cond ((not status-code)
             (user-error "Request failed: Could not reach the server"))
            ((= status-code 200)
             (request-response-data response))
            (t
             (error "Request failed"))))))

(defun gh-repo-values-to-columns (data)
  "Convert repository values to columns.

Converts values in DATA to columns using the format specified in
`gh-repo-annotation-spec-alist'.
Each value is formatted using the corresponding format string and padded to
the specified width.

Returns a string with the formatted values separated by newlines."
  (mapconcat
   (pcase-lambda (`(,key ,format-str ,width))
     (let ((value (alist-get key data))
           (space-char 32))
       (truncate-string-to-width (format format-str (or value "")) width
                                 0 space-char t)))
   gh-repo-annotation-spec-alist " "))

(defun gh-repo-plist-omit (keys plist)
  "Remove KEYS and values from PLIST."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and (not (memq key keys))
                       (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))


(defun gh-repo-get (resource &optional params &rest args)
  "Retrieve a GitHub repository's data using specified RESOURCE and parameters.

Argument RESOURCE: This is a required argument that specifies the RESOURCE to
get from the GitHub repository.

It should be a string.

Optional argument PARAMS: This argument is optional and can be used to pass
additional parameters to the `ghub-get' function.

It should be a list.

ARGS can be used to pass any number of additional arguments
to the `ghub-get' function."
  (let* ((auth (gh-repo-authenticate))
         (query (plist-get args :query))
         (query-str (mapconcat
                     (pcase-lambda (`(,k . ,v))
                       (format "%s=%s" k v))
                     (seq-filter #'cdr query)
                     "&"))
         (url
          (if (string-empty-p query-str)
              resource
            (concat (if (string-suffix-p "/" resource)
                        (substring-no-properties resource (1- (length
                                                               resource)))
                      resource)
                    "?" query-str))))
    (apply #'ghub-get url params
           :auth (cdr auth)
           :username (car auth)
           (gh-repo-plist-omit '(:query)
                               args))))

(defun gh-repo--ivy-read-repo (prompt url)
  "Read a repo in the minibuffer, with Ivy completion.

PROMPT is a string to prompt with; normally it ends in a colon and a space.

Argument URL is the url of a GitHub gist."
  (interactive)
  (when (and
         (require 'ghub nil t)
         (fboundp 'ghub-continue)
         (fboundp 'ghub-get)
         (fboundp 'ivy-update-candidates)
         (fboundp 'ivy--reset-state)
         (fboundp 'ivy--exhibit)
         (boundp 'ivy-text)
         (boundp 'ivy-exit)
         (boundp 'ivy-last)
         (boundp 'ivy--all-candidates)
         (boundp 'cl-struct-ivy-state-tags)
         (boundp 'ivy--index)
         (fboundp 'ivy-read)
         (fboundp 'ivy-recompute-index-swiper-async)
         (fboundp 'ivy-configure))
    (let ((caller this-command))
      (ivy-configure caller
        :index-fn #'ivy-recompute-index-swiper-async)
      (let* ((response (make-hash-table
                        :test #'equal))
             (cands)
             (maxlen 30)
             (buff (current-buffer))
             (done)
             (output-buffer
              (gh-repo-get url nil
                           :query `((per_page . ,gh-repo-default-repos-limit))
                           :callback
                           (lambda (value _headers _status req)
                             (when (and (active-minibuffer-window)
                                        (buffer-live-p buff)
                                        (not done))
                               (with-current-buffer buff
                                 (let ((names))
                                   (dolist (item value)
                                     (let ((name (alist-get 'full_name item)))
                                       (puthash name item response)
                                       (push name names)))
                                   (setq cands (nreverse names))
                                   (setq maxlen (if cands
                                                    (apply #'max
                                                           (mapcar #'length
                                                                   cands))
                                                  30))
                                   (ivy-update-candidates
                                    cands)))
                               (let ((input ivy-text)
                                     (pos
                                      (when-let ((wind
                                                  (active-minibuffer-window)))
                                        (with-selected-window
                                            wind
                                          (point)))))
                                 (when (active-minibuffer-window)
                                   (with-selected-window (active-minibuffer-window)
                                     (delete-minibuffer-contents)))
                                 (progn
                                   (or
                                    (progn
                                      (and
                                       (memq
                                        (type-of ivy-last)
                                        cl-struct-ivy-state-tags)
                                       t))
                                    (signal 'wrong-type-argument
                                            (list 'ivy-state ivy-last)))
                                   (let* ((v ivy-last))
                                     (aset v 2 ivy--all-candidates)))
                                 (when (fboundp 'ivy-state-preselect)
                                   (progn
                                     (or
                                      (progn
                                        (and
                                         (memq
                                          (type-of ivy-last)
                                          cl-struct-ivy-state-tags)
                                         t))
                                      (signal 'wrong-type-argument
                                              (list 'ivy-state ivy-last)))
                                     (let* ((v ivy-last))
                                       (aset v 7 ivy--index))))
                                 (ivy--reset-state
                                  ivy-last)
                                 (when-let ((wind
                                             (active-minibuffer-window)))
                                   (with-selected-window
                                       wind
                                     (insert input)
                                     (goto-char
                                      (when pos
                                        (if (> pos
                                               (point-max))
                                            (point-max)
                                          pos)))
                                     (ivy--exhibit)))
                                 (ghub-continue req))))))
             (annotf (lambda (key)
                       (when (> (length key) maxlen)
                         (setq maxlen (1+ (length key))))
                       (let* ((data (gethash key response))
                              (annot-str (gh-repo-values-to-columns
                                          data)))
                         (concat (make-string (- maxlen (length key)) ?\ )
                                 " "
                                 annot-str)))))
        (unwind-protect
            (ivy-read prompt
                      (lambda (str pred action)
                        (if (eq action 'metadata)
                            `(metadata
                              (annotation-function . ,annotf))
                          (complete-with-action action cands str pred)))
                      :action (lambda (item)
                                (if ivy-exit
                                    item
                                  (gh-repo-browse item)))
                      :unwind (lambda ()
                                (setq done t))
                      :caller caller)
          (when (buffer-live-p output-buffer)
            (let ((message-log-max nil))
              (with-temp-message (or (current-message) "")
                (kill-buffer output-buffer)))))))))

;;;###autoload
(defun gh-repo-ivy-read-current-user-repo (&optional prompt)
  "Read a repo in the minibuffer with PROMPT and Ivy completion."
  (interactive)
  (unless gh-repo--cached-auth-data
    (gh-repo-authenticate))
  (gh-repo--ivy-read-repo (or prompt "Repo: ")
                          (concat "user/repos")))

;;;###autoload
(defun gh-repo-ivy-read-other-user-repo (user)
  "Read a repo of USER in the minibuffer, with Ivy completion."
  (interactive (read-string "User: "))
  (unless gh-repo--cached-auth-data
    (gh-repo-authenticate))
  (gh-repo--ivy-read-repo "Repo: " (concat "users/"
                                           user
                                           "/repos")))

(defun gh-repo-read-dir (prompt basename)
  "Read directory with PROMPT and BASENAME."
  (let* ((variants
          (mapcar
           (lambda (dir)
             (if (file-exists-p
                  (expand-file-name basename dir))
                 (let ((count 0)
                       (name (seq-copy basename)))
                   (while (file-exists-p
                           (expand-file-name name dir))
                     (setq count (1+ count))
                     (setq name (format "%s-%s"
                                        (replace-regexp-in-string
                                         "-[0-9]+$" "" name)
                                        count)))
                   (expand-file-name name dir))
               (expand-file-name basename dir)))
           (let ((items (if (functionp gh-repo-download-default-repo-dir)
                            (funcall gh-repo-download-default-repo-dir)
                          gh-repo-download-default-repo-dir)))
             (if (listp items)
                 items
               (list items))))))
    (file-name-as-directory
     (completing-read (or prompt "Directory:\s") variants nil nil))))

(defun gh-repo--confirm-url (name)
  "Convert repo NAME to ssh format and read it from minibuffer."
  (let ((variants (gh-repo-util-get-ssh-variants
                   (cond ((string-prefix-p "https://" name)
                          (gh-repo-util-url-https-to-ssh name))
                         ((gh-repo-util-ssh-url-p name)
                          name)
                         (t (gh-repo-util-url-https-to-ssh
                             (concat "https://github.com/"
                                     name)))))))
    (if (> (length variants)
           1)
        (completing-read "git clone\s" variants)
      (car variants))))

;;;###autoload
(defun gh-repo-clone-repo (name)
  "Read target directory from minibuffer and clone repository NAME."
  (interactive (list (gh-repo-read-user-repo "Clone repository: "
                                             #'identity)))
  (if-let* ((basename (car (reverse (split-string name "/" t))))
            (project-dir (gh-repo-read-dir
                          (format "Clone %s to " basename) basename))
            (url (gh-repo--confirm-url name)))
      (let ((command (read-string "" (string-join
                                      (list "git" "clone" url project-dir)
                                      "\s"))))
        (setq project-dir (expand-file-name
                           (car (reverse (split-string command)))))
        (gh-repo-exec-in-dir command project-dir
                             (lambda ()
                               (when (file-exists-p project-dir)
                                 (let ((default-directory project-dir))
                                   (run-hooks
                                    'gh-repo-after-create-repo-hook))))))
    (message "Cannot clone %s" name)))

(defun gh-repo-remove-request (fullname)
  "Remove a GitHub repository request.

Remove a repository request with the given FULLNAME."
  (let ((auth (gh-repo-authenticate)))
    (ghub-delete (concat "/repos/" fullname) nil
                 :auth (cdr auth)
                 :username (car auth)
                 :callback
                 (lambda (_value &rest _)
                   (message "Repository %s removed" fullname)))))

;;;###autoload
(defun gh-repo-delete (fullname)
  "Remove a repository request with the given FULLNAME."
  (interactive (list (gh-repo-read-user-repo "Delete repo: "
                                             #'identity)))
  (unless (string-empty-p fullname)
    (when (yes-or-no-p (format "Really remove %s repo?"
                               fullname))
      (gh-repo-remove-request fullname))))


(defun gh-repo-browse (repo)
  "Visit github REPO."
  (funcall gh-repo-browse-function
           (if (string-match-p "^https://" repo)
               repo
             (concat "https://github.com/" repo))))



(defun gh-repo-prompt-repo-action (repo)
  "Prompt for an action to perform on a given repository.

Argument REPO is a variable that represents the repository for which an action
is to be prompted."
  (let ((choice (read-multiple-choice
                 (format "Action for %s" repo)
                 gh-repo-actions)))
    (if (functionp (caddr choice))
        (funcall (caddr choice) repo)
      choice)))

;;;###autoload
(defun gh-repo-read-user-repo (&optional prompt action)
  "Read user repository with PROMPT and execute ACTION.
If ACTION is nil read it from `gh-repo-actions'."
  (interactive)
  (let ((repo (gh-repo-ivy-read-current-user-repo prompt)))
    (when repo
      (if action
          (funcall action repo)
        (let ((choice (and (read-multiple-choice
                            (format "Action for %s" repo)
                            gh-repo-actions))))
          (if (and (nth 2 choice)
                   (functionp (nth 2 choice)))
              (funcall (nth 2 choice) repo)
            choice))))))
(defun gh-repo-retrieve-logins (alist)
  "Retrieve user logins from a given ALIST.

Argument ALIST is a list where each element is a cons cell that contains a
`key-value' pair."
  (mapcar
   (apply-partially #'alist-get 'login)
   (alist-get 'items alist)))

(defvar gh-repo-minibuffer-timer nil)
(defun gh-repo-debounce--run-in-buffer (buffer timer-sym fn &rest args)
  "Run a function FN in a BUFFER and cancel timer TIMER-SYM.

Argument TIMER-SYM is a symbol that represents a timer.
Argument BUFFER is the buffer in which the function/macro will be executed.
Argument FN is the function or macro that will be executed.
Argument ARGS is a list of additional arguments that will be passed to the FN."
  (when (and buffer (buffer-live-p buffer))
    (let ((buff-wnd (get-buffer-window buffer)))
      (with-current-buffer buffer
        (if (and buff-wnd
                 (not (eq (selected-window)
                          buff-wnd)))
            (with-selected-window buff-wnd
              (apply fn args))
          (apply fn args))
        (when-let ((timer-value (symbol-value timer-sym)))
          (when (timerp timer-value)
            (cancel-timer timer-value)))))))

(defun gh-repo-debounce (timer-sym delay fn &rest args)
  "Debounce execution FN with ARGS for DELAY.
TIMER-SYM is a symbol to use as a timer."
  (when-let ((timer-value (symbol-value timer-sym)))
    (when (timerp timer-value)
      (cancel-timer timer-value)))
  (set timer-sym (apply #'run-with-timer delay nil
                        #'gh-repo-debounce--run-in-buffer
                        (current-buffer)
                        timer-sym
                        fn
                        args)))

(defun gh-repo-run-github-explorer (repo)
  "Retrieve and display the file structure of a specified GitHub repository.

Argument REPO is the name of the GitHub repository that the function will
interact with."
  (when (and (fboundp 'github-explorer-paths--put)
             (fboundp 'github-explorer--tree))
    (url-retrieve
     (format
      "https://api.github.com/repos/%s/git/trees/HEAD:?recursive=1"
      repo)
     (lambda (arg)
       (cond ((equal :error (car arg))
              (message arg))
             (t
              (with-current-buffer (current-buffer)
                (goto-char (point-min))
                (re-search-forward "^$")
                (delete-region (+ 1 (point))
                               (point-min))
                (goto-char (point-min))
                (let* ((paths
                        (remove nil
                                (mapcar
                                 (lambda (x)
                                   (if (equal
                                        (cdr
                                         (assoc
                                          'type x))
                                        "blob")
                                       (cdr (assoc 'path x))))
                                 (cdr (assoc 'tree
                                             (json-read)))))))
                  (github-explorer-paths--put repo paths)
                  (github-explorer--tree repo
                                         (format
                                          "https://api.github.com/repos/%s/git/trees/%s"
                                          repo "HEAD")
                                         "/")))))))))

(defun gh-repo-update-ivy-cands (items)
  "Update the list of candidates in Ivy's completion buffer.

Argument ITEMS is a list of ITEMS to be updated in the Ivy candidates."
  (let ((cur (ivy-state-current ivy-last)))
    (ivy--set-candidates
     items)
    (let ((re (ivy-re-to-str ivy-regex)))
      (if ivy--old-cands
          (ivy--recompute-index re ivy--all-candidates)
        (unless (string= cur (nth ivy--index ivy--all-candidates))
          (let ((func (ivy-alist-setting ivy-index-functions-alist)))
            (if func
                (funcall func re ivy--all-candidates)
              (ivy--preselect-index
               (if (> (length re) 0)
                   cur
                 (ivy-state-preselect ivy-last))
               ivy--all-candidates))))))
    (setq ivy--old-cands ivy--all-candidates)
    (if ivy--all-candidates
        (ivy--exhibit)
      (ivy--insert-minibuffer ""))))

(defun gh-repo-minibuffer-get-update-fn ()
  "Update the minibuffer's completion candidates based on the current mode."
  (pcase completing-read-function
    ((guard (bound-and-true-p helm-mode))
     (when (fboundp 'helm-force-update)
       (lambda (_items cand)
         (helm-force-update cand))))
    ('ivy-completing-read
     (when (and (fboundp 'ivy-update-candidates))
       (lambda (items _input)
         (gh-repo-update-ivy-cands items))))
    ((guard (bound-and-true-p icomplete-mode))
     (lambda (&rest _)
       (completion--flush-all-sorted-completions)
       (when (fboundp 'icomplete-exhibit)
         (icomplete-exhibit))))
    ('completing-read-default
     (lambda (&rest _)
       (completion--flush-all-sorted-completions)
       (minibuffer-completion-help)))))

(defvar gh-repo-repos-hash (make-hash-table :test #'equal))
(defvar gh-repo-last-query nil)

(defun gh-repo-hash-keys ()
  "Extract keys from the `gh-repo-repos-hash' hash table."
  (hash-table-keys gh-repo-repos-hash))



(defun gh-repo-search-repos (&optional query)
  "Search and interactively select GitHub repositories using a QUERY.

Optional argument QUERY is a string that specifies the search QUERY."
  (interactive (list (gh-repo-get-search-query)))
  (setq gh-repo-last-query query)
  (setq this-command 'gh-repo-search-repos)
  (gh-repo-async-comp-read "Repo: " "/search/repositories"
                           'gh-repo-search-repos
                           query
                           'full_name
                           'gh-repo-values-to-columns))


(defun gh-repo-async-comp-read (prompt url caller query hash-key &optional
                                       annotate-fn)
  "Fetch GitHub repo data with minibuffer completion.

Argument PROMPT is a string displayed as the prompt in the minibuffer.

Argument URL is a string representing the endpoint to which the HTTP request is
made.

Argument CALLER is a symbol representing the command that invoked the
completion.

Argument QUERY is a plist containing additional parameters for the HTTP request.

Argument HASH-KEY is a symbol or string used to extract the relevant value from
the JSON response.

Optional argument ANNOTATE-FN is a function that takes an item and returns an
annotation string."
  (gh-repo-authenticate)
  (when caller
    (setq this-command caller))
  (let* ((hash (make-hash-table :test #'equal))
         (done)
         (update-fn
          (gh-repo-minibuffer-get-update-fn))
         (maxlen 90)
         (annotf
          (when annotate-fn
            (lambda (key)
              (when (> (length key) maxlen)
                (setq maxlen (1+ (length key))))
              (let* ((data (gethash key hash))
                     (annot-str (funcall annotate-fn data))
                     (str (concat
                           (propertize " " 'display
                                       `(space :align-to
                                               ,maxlen))
                           annot-str)))
                str))))
         (handler
          (lambda (text)
            (unless done
              (message "searching... %s" text)
              (gh-repo-get
               url
               nil
               :query (gh-repo-make-query text
                                          query
                                          nil
                                          100)
               :host "api.github.com"
               :callback
               (lambda (resp _headers _status _req)
                 (message nil)
                 (let ((items (alist-get 'items resp))
                       (str (gh-repo-get-minibuffer-input))
                       (miniwind (active-minibuffer-window))
                       (keys))
                   (dolist (item items)
                     (let ((name
                            (alist-get hash-key item)))
                       (push name keys)
                       (puthash name item hash)))
                   (when (and miniwind items text str (equal text str))
                     (with-selected-window
                         miniwind
                       (funcall update-fn keys text)))))))))
         (hook-fn (lambda (&rest _)
                    (let ((text
                           (gh-repo-get-minibuffer-input)))
                      (unless (or (not text)
                                  (string-empty-p text))
                        (gh-repo-debounce
                         'gh-repo-minibuffer-timer
                         0.5
                         handler text))))))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (when (minibufferp)
                (use-local-map (make-composed-keymap gh-repo-minibuffer-map
                                                     (current-local-map)))
                (add-hook 'after-change-functions hook-fn nil t)))
          (completing-read
           prompt
           (lambda (str pred action)
             (if (and annotf
                      (eq action 'metadata))
                 `(metadata
                   (annotation-function . ,annotf))
               (complete-with-action action (hash-table-keys hash)
                                     str pred)))
           nil nil))
      (setq done t))))


;;;###autoload
(defun gh-repo-github-user ()
  "Retrieve and display GitHub users based on input in the minibuffer."
  (interactive)
  (gh-repo-authenticate)
  (gh-repo-async-comp-read "User: " "search/users"
                           'gh-repo-github-user
                           nil
                           'login))

(defun gh-repo-search-users (str page query &optional callback)
  "Send an asynchronous request to GitHub's CODE search API.

Argument STR is the string segment or keyword that the user wants to
search for in the GitHub users api.
Argument QUERY is an optional additional search term that can be used to
refine the search results.
Argument PAGE is the specific PAGE number of the search results that the
user wants to view.
Argument CALLBACK is a function that will be called once the search
results are returned, with the search results passed as an argument."
  (let ((q (string-join (delq nil
                              (list str query))
                        "")))
    (if callback
        (gh-repo-get
         (concat "search/users?q=" q
                 (format "&per_page=100&page=%s" page))
         nil
         :host "api.github.com"
         :callback (lambda (value &rest _)
                     (funcall callback value)))
      (gh-repo-get
       (concat "search/users?q=" q
               (format "&per_page=100&page=%s" page))
       nil
       :host "api.github.com"))))

(defun gh-repo-minibuffer-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun gh-repo-minibuffer-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors (gh-repo-minibuffer-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun gh-repo-minibuffer-get-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (gh-repo-minibuffer-get-metadata) 'category)
       all))))

(defun gh-repo-get-minibuffer-get-default-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (gh-repo-minibuffer-get-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar gh-repo-minibuffer-targets-finders
  '(gh-repo-minibuffer-ivy-selected-cand
    gh-repo-get-minibuffer-get-default-completion))

(defun gh-repo-minibuffer-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'gh-repo-minibuffer-targets-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun gh-repo-minibuffer-exit-with-action (action)
  "Call ACTION with current candidate and exit minibuffer."
  (pcase-let ((`(,_category . ,current)
               (gh-repo-minibuffer-get-current-candidate)))
    (progn (run-with-timer 0 nil action current)
           (abort-minibuffers))))

(defun gh-repo-minibuffer-web-restore-completions-wind ()
  "Restore *Completions* window height."
  (when (eq this-command 'minibuffer-next-completion)
    (remove-hook 'post-command-hook
                 #'gh-repo-minibuffer-web-restore-completions-wind)
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (fit-window-to-buffer win completions-max-height))))

(defun gh-repo-minibuffer-action-no-exit (action)
  "Call ACTION with minibuffer candidate in its original window."
  (pcase-let ((`(,_category . ,current)
               (gh-repo-minibuffer-get-current-candidate)))
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (minimize-window win)
      (add-hook 'post-command-hook
                #'gh-repo-minibuffer-web-restore-completions-wind))
    (with-minibuffer-selected-window
      (funcall action current))))


(defun gh-repo-browse-current-repo-and-exit ()
  "Browse the current GitHub repository and exit the minibuffer."
  (interactive)
  (gh-repo-minibuffer-exit-with-action #'gh-repo-browse))

(defun gh-repo-browse-current-repo ()
  "Open the current repository without exiting minibuffer."
  (interactive)
  (gh-repo-minibuffer-action-no-exit #'gh-repo-browse))

(defun gh-repo-get-minibuffer-input ()
  "Retrieve user input from the minibuffer in GitHub repository."
  (when-let ((wind (active-minibuffer-window)))
    (with-selected-window wind
      (let ((str (string-trim (or (car (split-string (buffer-substring-no-properties
                                                      (minibuffer-prompt-end)
                                                      (line-end-position))
                                                     nil t))
                                  ""))))
        (unless (string-empty-p str)
          str)))))





;;;###autoload
(defun gh-repo-search-internal-repos (repo &optional action)
  "Search for internal repositories and perform an optional ACTION.

Argument REPO is a required argument that should be a repository object.

Optional argument ACTION is a function that will be called with REPO as its
argument.
If ACTION is not provided, the default behavior is to prompt the user for an
ACTION to perform on the repository."
  (interactive (list
                (let ((query (gh-repo-get-search-query)))
                  (gh-repo-search-repos query))))
  (if (functionp action)
      (funcall action repo)
    (gh-repo-prompt-repo-action repo)))


(defun gh-repo--read-auth-from-gh-config ()
  "Extract and return GitHub username and OAuth token from gh config file."
  (pcase-let* ((`(,username . ,file)
                (with-temp-buffer
                  (when (zerop
                         (call-process
                          "gh" nil t
                          nil "auth" "status"))
                    (re-search-backward
                     "Logged in to github.com as \\([a-zA-Z0-9-]*[a-zA-Z0-9]\\{1\\}\\)[\s\t\n]+[(]\\([^)]+\\)[)]"
                     nil t 1)
                    (cons (match-string-no-properties 1)
                          (match-string-no-properties 2)))))
               (token (and username file
                           (file-exists-p file)
                           (with-temp-buffer
                             (insert-file-contents file)
                             (when (re-search-forward
                                    "github\\.com:[\s\n]+oauth_token:[\s]+\\([^\n]+\\)"
                                    nil t 1)
                               (match-string-no-properties
                                1))))))
    (and token (cons username token))))

(defun gh-repo-auth-from-gh-config ()
  "Extract OAuth token from either `gh-repo--cached-auth-data' or gh config."
  (or gh-repo--cached-auth-data
      (setq gh-repo--cached-auth-data
            (gh-repo--read-auth-from-gh-config))))

(defun gh-repo-make-query (code search-query &optional page per_page)
  "Construct a GitHub repository search query with optional pagination.

Argument CODE is a string representing the code to search for in the repository.

Argument SEARCH-QUERY is a string representing the additional search query.

Optional argument PAGE is an integer representing the page number of the search
results.

Optional argument PER_PAGE is an integer representing the number of results per
page."
  (let* ((q (string-join (delq nil
                               (list code search-query))
                         "")))
    (seq-filter #'cdr
                `((q . ,q)
                  (page . ,page)
                  (per_page . ,per_page)))))

(defun gh-repo-search-repos-request (code page &optional search-query callback)
  "Send an asynchronous request to GitHub's CODE search API.

Argument CODE is the code segment or keyword that the user wants to
search for in the GitHub codebase.
Argument SEARCH-QUERY is an optional additional search term that can be used to
refine the search results.
Argument PAGE is the specific PAGE number of the search results that the
user wants to view.
Argument CALLBACK is a function that will be called once the search
results are returned, with the search results passed as an argument."
  (let* ((q (string-join (delq nil
                               (list code search-query))
                         ""))
         (query (seq-filter #'cdr
                            `((q . ,q)
                              ;; (per_page . 100)
                              (page . ,page)))))
    (if callback
        (gh-repo-get
         "/search/repositories"
         nil
         :query query
         :host "api.github.com"
         :callback callback)
      (gh-repo-get
       (concat "search/repositories?q=" q
               (format "&per_page=100&page=%s" (or page 1)))
       nil
       :host "api.github.com"))))

;;;###autoload
(defun gh-repo-clone-other-user-repo (name)
  "Clone other user's NAME repository."
  (interactive (list (gh-repo-ivy-read-other-user-repo
                      (gh-repo-github-user))))
  (gh-repo-clone-repo name))


(defvar gh-repo-licences nil)
(defvar gh-repo-gitignore-templates nil)

(defcustom gh-repo-default-arguments '("--private"
                                       "--license_template=gpl-3.0"
                                       "--has_wiki"
                                       "--has_projects"
                                       "--has_downloads"
                                       "--has_issues")
  "Initial arguments for `gh-repo-menu'."
  :type '(repeat string)
  :group 'gh-repo)


(defun gh-repo-argument-to-cell (arg)
  "Parse transient argument ARG to cons cell."
  (cond ((string-match-p "^--\\([a-z_]+\\)=" arg)
         (pcase-let ((`(,key . ,value)
                      (split-string arg "=" t)))
           (setq key (substring-no-properties key (length "--")))
           (setq value (string-join value "="))
           (cons key value)))
        (t (cons (substring-no-properties arg (length "--")) t))))



(defun gh-repo-post-request (payload)
  "Send a POST request with PAYLOAD to create a repository on GitHub."
  (let ((auth (gh-repo-authenticate)))
    (ghub-post "/user/repos" nil
               :payload payload
               :auth (cdr auth)
               :username (car auth)
               :callback
               (lambda (value &rest _)
                 (if
                     (yes-or-no-p (format "Clone repo %s?" (alist-get
                                                            'full_name value)))
                     (gh-repo-clone-repo (alist-get 'full_name value))
                   (message "Repository created."))))))

;;;###autoload
(defun gh-repo-create-repo (args)
  "Create a new repository from transient arguments ARGS."
  (interactive (list (transient-args transient-current-command)))
  (let ((obj (mapcar #'gh-repo-argument-to-cell args)))
    (while (or (not (cdr (assoc-string "name" obj)))
               (string-empty-p
                (cdr (assoc-string "name" obj))))
      (let* ((name (read-string "Name of the repository: "))
             (new-cell (cons "name" name)))
        (setq obj (assoc-delete-all "name" obj))
        (setq obj (push new-cell obj))))
    (gh-repo-post-request obj)))


(defun gh-repo-get-search-query ()
  "Generate a search query string from the current transient command arguments."
  (let ((args (transient-args transient-current-command)))
    (seq-reduce
     (lambda (acc arg)
       (let ((value
              (transient-arg-value arg args)))
         (if (not value)
             acc
           (let* ((neg (string-prefix-p "--not-" arg))
                  (query (if neg
                             (replace-regexp-in-string
                              "\\(^--not-\\)\\|\\(=$\\)" ""
                              arg)
                           (replace-regexp-in-string "^--\\|=$" ""
                                                     arg)))
                  (separator (if neg "+-" "+")))
             (setq acc (concat acc separator query ":" value))))))
     '("--language=")
     "")))

(defun gh-repo--get-languages (str pred action)
  "Initialize and complete GitHub languages based on given parameters.

Argument STR is a string that is used as the input for the completion
function.
Argument PRED is a predicate function that filters the completion
candidates.
Argument ACTION is an ACTION to be performed on the completion
candidates."
  (setq gh-repo--search-langs-alist (or gh-repo--search-langs-alist
                                        (gh-repo--init-languages)))
  (setq gh-repo--search-langs
        (or gh-repo--search-langs
            (mapcan #'cdr (copy-tree
                           gh-repo--search-langs-alist))))
  (if (eq action 'metadata)
      nil
    (complete-with-action action gh-repo--search-langs str pred)))

;;;###autoload (autoload 'gh-repo-menu "gh-repo" nil t)
(transient-define-prefix gh-repo-menu ()
  "Command dispatcher for GitHub repositories."
  :value
  (lambda ()
    gh-repo-default-arguments)
  ["New repository arguments"
   ("n" "name" "--name=" :unsavable t)
   ("d" "description" "--description="
    :class transient-option
    :unsavable t)
   ("p" "private" "--private"
    :unsavable t)
   ("l" "license_template" "--license_template="
    :class transient-option
    :choices (lambda (&rest _)
               (unless gh-repo-licences
                 (setq gh-repo-licences (gh-repo-load-licences)))
               (mapcar (apply-partially #'alist-get 'key) gh-repo-licences)))
   ("g" "gitignore_template" "--gitignore_template="
    :class transient-option
    :choices (lambda (&rest _)
               (unless gh-repo-gitignore-templates
                 (setq gh-repo-gitignore-templates
                       (append
                        (gh-repo-load-gitignore-templates) nil)))
               gh-repo-gitignore-templates))
   ("t" "template repository" "--is_template")
   ("i" "has_issues" "--has_issues")
   ("P" "projects" "--has_projects")
   ("w" "wiki" "--has_wiki")
   ("L" "downloads" "--has_downloads")
   ("S" "discussions" "--has_discussions")]
  ["Config"
   ("u" gh-repo-change-user
    :description (lambda ()
                   (concat  "Github User: "
                            (if
                                (or (not (car-safe gh-repo--cached-auth-data))
                                    (string-empty-p (car
                                                     gh-repo--cached-auth-data))
                                    (not (cdr-safe gh-repo--cached-auth-data)))
                                "(not logged)"
                              (propertize
                               (substring-no-properties
                                (or (car-safe
                                     gh-repo--cached-auth-data)
                                    ""))
                               'face 'transient-value)))))]
  ["Search query"
   ("l" "language" "--language="
    :choices (lambda ()
               (setq gh-repo--search-langs-alist (or gh-repo--search-langs-alist
                                                     (gh-repo--init-languages)))
               (setq gh-repo--search-langs
                     (or gh-repo--search-langs
                         (mapcan #'cdr (copy-tree
                                        gh-repo--search-langs-alist))))))
   ("s" "Search for repos" gh-repo-search-internal-repos :transient nil)]
  ["Actions"
   ("o" "Clone other user repo" gh-repo-clone-other-user-repo)
   ("c" "Clone my repo" gh-repo-clone-repo)
   ("R" "Remove my repo" gh-repo-delete)
   ("RET" "Create" gh-repo-create-repo)]
  (interactive)
  (gh-repo-authenticate)
  (transient-setup #'gh-repo-menu))


(provide 'gh-repo)
;;; gh-repo.el ends here