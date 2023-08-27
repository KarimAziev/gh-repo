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

(eval-when-compile
  (require 'subr-x))
(require 'url-parse)

(require 'shell)
(require 'transient)
(require 'ghub)

(defcustom gh-repo-excluded-dirs '("~/Dropbox"
                                   "~/melpa"
                                   "~/.cache"
                                   "~/.cask")
  "List of directories to exlude from directories to clone."
  :group 'gh-repo
  :type '(repeat directory))

(defcustom gh-repo-default-license "gpl-3.0"
  "Default repository license."
  :type 'string
  :group 'gh-repo)

(defcustom gh-repo-ghub-auth-info '("" . gh-repo)
  "String of USERNAME^MARKER in auth sources."
  :type '(cons :tag "Auth" (string :tag "Github Username")
               (radio
                :tag "Marker"
                (symbol :tag "Suffix" gh-repo)
                (string :tag "OAuth Token")))
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

(defcustom gh-repo-browse-function (if (and window-system
                                            (featurep 'xwidget-internal))
                                       'gh-repo--browse-with-xwidget
                                     'browse-url)
  "Function for browsing preview page.

It will be called with one argument - url to open.

Default value is to use xwidgets if available, othervise `browse-url'."
  :type '(radio  (function-item gh-repo--browse-with-xwidget)
                 (function-item browse-url)
                 (function :tag "Custom function"))
  :group 'gh-repo)

(defcustom gh-repo-actions '((?c "clone" gh-repo-clone-repo)
                             (?b "browse" gh-repo-browse))
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
(require 'comint)
(require 'request)
(require 'auth-source)

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
  "Return right-to-left composition from FUNCTIONS."
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

(defun gh-repo-util-alist-ssh-hosts ()
  "Return hosts found in .ssh/config."
  (when (file-exists-p "~/.ssh/config")
    (with-temp-buffer
      (insert-file-contents
       "~/.ssh/config")
      (let ((alist))
        (while (re-search-forward
                "\\(HOST[\s\t]\\([^\n]+\\)[\n\s\t]+HOSTNAME[\s\t\n]\\([^\s\t\n]+\\)\\)"
                nil t 1)
          (let ((host (match-string-no-properties 2))
                (hostname (match-string-no-properties 3)))
            (push (cons host hostname)
                  alist)))
        alist))))

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
  (let* ((local-alist (gh-repo-util-alist-ssh-hosts))
         (cell (with-temp-buffer
                 (save-excursion
                   (insert (replace-regexp-in-string "^git@" ""
                                                     ssh-url)))
                 (let ((beg (point))
                       (end))
                   (setq end (re-search-forward gh-repo-util-host-regexp nil t 1))
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


(defun gh-repo-project-expand-wildcards (pattern dir depth &optional full)
  "Return list of files that matches PATTERN in DIR at max DEPTH.
If FULL is non-nil, files are absolute."
  (let ((tramp-archive-enabled nil))
    (let ((dir (file-name-as-directory dir)))
      (mapcan (lambda (n)
                (let ((tramp-archive-enabled nil))
                  (file-expand-wildcards
                   (concat dir
                           (string-join
                            (append (make-vector n "**")
                                    (list pattern))
                            "/"))
                   full)))
              (number-sequence 0 (1- depth))))))

(defun gh-repo-project-get-projects (&optional depth dir)
  "Return all git repositories at DEPTH in DIR.
Default value for DEPTH is 3.
Default value for DIR is home directory."
  (unless depth (setq depth 3))
  (unless dir (setq dir "~/"))
  (let ((tramp-archive-enabled nil)
        (projects)
        (excluded (delq nil (append
                             gh-repo-excluded-dirs
                             (if
                                 (require 'xdg nil t)
                                 nil
                               (mapcar (lambda (it)
                                         (when (fboundp it)
                                           (funcall it)))
                                       '(xdg-state-home
                                         xdg-data-home
                                         xdg-runtime-dir)))))))
    (dolist (dir (directory-files dir t directory-files-no-dot-files-regexp))
      (when (and (file-directory-p dir)
                 (file-accessible-directory-p dir)
                 (not (or (seq-find (apply-partially #'file-equal-p dir)
                                    excluded)
                          (seq-find (apply-partially #'file-in-directory-p dir)
                                    excluded))))
        (setq projects
              (if (file-exists-p (concat dir "/.git"))
                  (push (concat dir "/.git") projects)
                (nconc (gh-repo-project-expand-wildcards "*/.git" dir
                                                         (1- depth))
                       projects)))))
    (mapcar (lambda (dir)
              (abbreviate-file-name (file-name-parent-directory dir)))
            projects)))

(defun gh-repo-find-clone-directories ()
  "Return list of git parents directories."
  (append (delete-dups (mapcar #'file-name-parent-directory
                               (gh-repo-project-get-projects 3)))
          (when (fboundp 'straight--repos-dir)
            (list (straight--repos-dir)))))


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
      (xwidget-webkit-browse-url url))))


(defvar gh-repo-after-create-repo-hook nil
  "List of hooks to run after cloning new repository.")

(defun gh-repo-exec-in-dir (command project-dir &optional callback)
  "Execute COMMAND in PROJECT-DIR.
If PROJECT-DIR doesn't exists, create new.
Invoke CALLBACK without args."
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

;;;###autoload
(defun gh-repo-change-user ()
  "Search for gh token in `auth-sources'."
  (interactive)
  (gh-repo-read-auth-marker)
  (when transient-current-command
    (transient-setup transient-current-command)))

(defun gh-repo-read-auth-marker ()
  "Search for gh token in `auth-sources'."
  (when-let ((variants
              (seq-uniq
               (auth-source-search
                :host "api.github.com"
                :require '(:user :secret)
                :max most-positive-fixnum)
               (lambda (a b)
                 (equal (gh-repo-auth-info-password a)
                        (gh-repo-auth-info-password b))))))
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
  "Return list of available licences from github api."
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

(defun gh-repo-convert-region-pad-right (str width)
  "Pad STR with spaces on the right to increase the length to WIDTH."
  (unless str (setq str ""))
  (let ((exceeds (> (length str) width))
        (separator "..."))
    (cond ((and exceeds
                (> width
                   (length separator)))
           (concat (substring str 0 (- width (length separator))) separator))
          ((and exceeds)
           str)
          (t (concat str (make-string (- width (length str)) ?\ ))))))

(defun gh-repo-values-to-columns (data)
  "Convert repository values to columns.

Converts values in DATA to columns using the format specified in
`gh-repo-annotation-spec-alist'.
Each value is formatted using the corresponding format string and padded to
the specified width.

Returns a string with the formatted values separated by newlines."
  (mapconcat
   (pcase-lambda (`(,key ,format-str ,width))
     (let ((value (alist-get key data)))
       (gh-repo-convert-region-pad-right (format format-str (or value ""))
                                         width)))
   gh-repo-annotation-spec-alist))

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
             (output-buffer
              (ghub-get url nil
                        :query `((per_page . ,gh-repo-default-repos-limit))
                        :auth (cdr gh-repo-ghub-auth-info)
                        :username (car gh-repo-ghub-auth-info)
                        :callback
                        (lambda (value _headers _status req)
                          (when (and (active-minibuffer-window)
                                     (buffer-live-p buff))
                            (with-current-buffer buff
                              (let ((names))
                                (dolist (item value)
                                  (let ((name (alist-get 'full_name item)))
                                    (puthash name item response)
                                    (push name names)))
                                (setq cands (nreverse names))
                                (setq maxlen (if
                                                 cands
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
                      :caller caller)
          (when (buffer-live-p output-buffer)
            (let ((message-log-max nil))
              (with-temp-message (or (current-message) "")
                (kill-buffer output-buffer)))))))))

;;;###autoload
(defun gh-repo-ivy-read-current-user-repo (&optional prompt)
  "Read a repo in the minibuffer with PROMPT and Ivy completion."
  (interactive)
  (when (or (not (car gh-repo-ghub-auth-info))
            (string-empty-p (car gh-repo-ghub-auth-info)))
    (gh-repo-read-auth-marker))
  (gh-repo--ivy-read-repo (or prompt "Repo: ")
                          (concat "user/repos")))

;;;###autoload
(defun gh-repo-ivy-read-other-user-repo (user)
  "Read a repo of USER in the minibuffer, with Ivy completion."
  (interactive (read-string "User: "))
  (when (or (not (car gh-repo-ghub-auth-info))
            (string-empty-p (car gh-repo-ghub-auth-info)))
    (gh-repo-read-auth-marker))
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
                                   (run-hooks 'gh-repo-after-create-repo-hook))))))
    (message "Cannot clone %s" name)))

(defun gh-repo-remove-request (fullname)
  "Remove a GitHub repository request.

Remove a repository request with the given FULLNAME."
  (ghub-delete (concat "/repos/" fullname) nil
               :auth (cdr gh-repo-ghub-auth-info)
               :username (car gh-repo-ghub-auth-info)
               :callback
               (lambda (_value &rest _)
                 (message "Repository %s removed" fullname))))

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
  (browse-url
   (if (string-match-p "^https://" repo)
       repo
     (concat "https://github.com/" repo))))


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

;;;###autoload
(defun gh-repo-clone-other-user-repo (name)
  "Clone other user's NAME repository."
  (interactive (list (gh-repo-ivy-read-other-user-repo
                      (read-string "Username: "))))
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
  (require 'ghub)
  (when (fboundp 'ghub-post)
    (ghub-post "/user/repos" nil
               :payload payload
               :auth (cdr gh-repo-ghub-auth-info)
               :username (car gh-repo-ghub-auth-info)
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

;;;###autoload (autoload 'gh-repo-menu "gh-repo" nil t)
(transient-define-prefix gh-repo-menu ()
  "Command dispatcher for GitHub repositories."
  :value
  (lambda ()
    gh-repo-default-arguments)
  ["New repository arguments"
   ("n" "name" "--name=")
   ("d" "description" "--description=" :class transient-option)
   ("p" "private" "--private")
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
                 (setq gh-repo-gitignore-templates (append (gh-repo-load-gitignore-templates) nil)))
               gh-repo-gitignore-templates))
   ("t" "template repository" "--is_template")
   ("i" "has_issues" "--has_issues")
   ("P" "projects" "--has_projects")
   ("w" "wiki" "--has_wiki")
   ("L" "downloads" "--has_downloads")
   ("s" "discussions" "--has_discussions")]
  ["Config"
   ("u" gh-repo-change-user
    :description (lambda ()
                   (concat  "Github User: "
                            (if
                                (or (not (car gh-repo-ghub-auth-info))
                                    (string-empty-p (car
                                                     gh-repo-ghub-auth-info))
                                    (not (cdr gh-repo-ghub-auth-info)))
                                "(not logged)"
                              (propertize
                               (substring-no-properties
                                (car
                                 gh-repo-ghub-auth-info))
                               'face 'transient-value)))))]
  ["Actions"
   ("o" "Clone other user repo" gh-repo-clone-other-user-repo)
   ("c" "Clone my repo" gh-repo-clone-repo)
   ("R" "Remove my repo" gh-repo-delete)
   ("RET" "Create" gh-repo-create-repo)]
  (interactive)
  (transient-setup #'gh-repo-menu))


(provide 'gh-repo)
;;; gh-repo.el ends here