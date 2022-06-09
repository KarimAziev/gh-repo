;;; gh-repo.el --- Create and manage gh repositories. -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gh-repo
;; Keywords: lisp, vc, tools
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

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
(require 'hydra)
(require 'shell)
(require 'comint)
(require 'request)
(require 'auth-source)

(defcustom gh-repo-default-license "gpl-3.0"
  "Default repository license."
  :type 'string
  :group 'gh-repo)

(defcustom gh-repo-download-default-repo-dir "~/"
  "Default directory for downloading and cloning repositories."
  :group 'gh-repo
  :type 'directory)

(defcustom gh-repo-actions '((?c "clone" gh-repo-clone-repo)
                             (?b "browse" gh-repo-visit)
                             (?v "view" gh-repo-view-repo)
                             (?D "delete" gh-repo-remove))
  "Actions for `gh-repo-read-user-repo'.

Each element is a list comprising (KEY LABEL ACTION)

KEY is a character for `read-multiple-choice', and LABEL is a
string which describes an action.

ACTION is a a function which should accept one argument
- repository of user name."
  :type '(alist :key-type (character :tag "Key" :value ?c)
                :value-type (list (string :tag "Label" :value "<description>")
                                  (function :tag "Function")))
  :group 'gh-repo)

(defmacro gh-repo--pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
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

(defmacro gh-repo--rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defmacro gh-repo--partial (fn &rest args)
  "Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append (list ,@args) pre-args))
                   `(apply ,fn (append (list ,@args) pre-args)))))))

(defmacro gh-repo--compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(gh-repo--pipe ,@(reverse functions)))

(defun gh-repo-fontify (content &optional mode-fn &rest args)
  "Fontify CONTENT according to MODE-FN called with ARGS.
If CONTENT is not a string, instead of MODE-FN emacs-lisp-mode will be used."
  (with-temp-buffer
    (delay-mode-hooks
      (apply (or mode-fn 'emacs-lisp-mode) args)
      (goto-char (point-min))
      (insert (if (or (eq major-mode 'emacs-lisp-mode)
                      (not (stringp content)))
                  (pp-to-string content)
                content))
      (font-lock-ensure)
      (buffer-string))))

(defun gh-repo-stringify (x)
  "Convert X to string effeciently.
X can be any object."
  (cond
   ((stringp x)
    x)
   ((symbolp x)
    (symbol-name x))
   ((integerp x)
    (number-to-string x))
   ((floatp x)
    (number-to-string x))
   (t (format "%s" x))))

(defun gh-repo-file-same-p (path-a path-b)
  "Return t if PATH-A and PATH-B are references to same file."
  (when (and (file-exists-p path-a)
             (file-exists-p path-b))
    (let (file-name-handler-alist)
      (equal
       (file-truename (directory-file-name (expand-file-name path-a)))
       (file-truename (directory-file-name (expand-file-name path-b)))))))

(defun gh-repo-boolean (x)
  "Convert X to t or nil."
  (if x t nil))

(defmacro gh-repos-defun-var-toggler-with-variants (var toggler variants
                                                        default-value)
  "Create VAR and VARIANTS TOGGLER."
  (declare (indent 2) (debug t))
  `(progn (defvar ,var ,default-value)
          (defun ,toggler ()
            (interactive)
            (let ((variants ,variants))
              (unless ,var (setq ,var (car variants)))
              (setq ,var (if (> (length variants) 2)
                             (completing-read "Variant:\s" variants)
                           (car (remove nil (remove ,var variants)))))))))

(defmacro gh-repos-defun-var-toggler (var toggler &optional initial-value)
  "Define VAR with INITIAL-VALUE and function with name TOGGLER."
  (declare (indent 2) (debug t))
  `(progn (defvar ,var ,initial-value)
          (defun ,toggler ()
            (interactive)
            (setq ,var (not (gh-repo-boolean ,var))))))

(defun gh-repos-get-prop (item property)
  "Get PROPERTY from ITEM.
ITEM can be propertized string or plist."
  (if (stringp item)
      (get-text-property 0 property item)
    (when (listp item)
      (plist-get item property))))

(defun gh-repos-add-props (string &rest properties)
  "Propertize STRING with PROPERTIES."
  (setq string (gh-repo-stringify string))
  (let* ((result (list 'head))
         (last result))
    (while properties
      (let* ((key (pop properties))
             (val (pop properties))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (apply 'propertize string (cdr result))))

(defun gh-repo-file-parent (path)
  "Return the parent directory to PATH without slash."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (file-name-absolute-p path)
          (directory-file-name parent)
        (file-relative-name parent)))))

(defun gh-repo-file-ancestor-of-p (prefix-a path-b)
  "Return t if PREFIX-A is ancestor of PATH-B."
  (unless (gh-repo-file-same-p prefix-a path-b)
    (let ((a (expand-file-name prefix-a))
          (b (expand-file-name path-b)))
      (string-prefix-p (if (file-directory-p a)
                           (file-name-as-directory a)
                         a)
                       (if (file-directory-p b)
                           (file-name-as-directory b)
                         b)))))

(gh-repos-defun-var-toggler-with-variants
 gh-repo--private gh-repo--private-toggle
 '("--private" "--public")
 "--private")

(gh-repos-defun-var-toggler gh-repo--push
                            gh-repo--push-toggle)

(gh-repos-defun-var-toggler gh-repo--disable-wiki
    gh-repo--disable-wiki-toggle)

(gh-repos-defun-var-toggler gh-repo--disable-issues
    gh-repo--disable-issues-toggle)

(gh-repos-defun-var-toggler gh-repo--internal
                            gh-repo--internal-toggle)

(defun gh-repo-exec (command)
  "Run a shell COMMAND and return its output as a string, whitespace trimmed."
  (string-trim (shell-command-to-string command)))

(defun gh-repo-file-dirs-recoursively (directory &optional match depth
                                                 filter-fn)
  "Return list of directories in DIRECTORY that matches MATCH.
With optional argument DEPTH limit max depth.
If FILTER-FN passed call it with directories."
  (when (or (not (numberp depth))
            (> depth 0))
    (let ((dirs (directory-files directory nil match))
          (it)
          (acc))
      (while (setq it (pop dirs))
        (setq it (expand-file-name it directory))
        (when (and
               (file-readable-p it)
               (file-directory-p it)
               (if filter-fn
                   (funcall filter-fn it)
                 t))
          (push it acc)
          (setq acc (if (or (not (numberp depth))
                            (> depth 0))
                        (append acc (gh-repo-file-dirs-recoursively
                                     it
                                     match
                                     (when depth
                                       (1- depth))
                                     filter-fn))
                      acc))))
      acc)))

(defun gh-repo-non-git-dirs-recoursively (directory &optional match depth)
  "Return list of non git directories in DIRECTORY that matches MATCH.
With optional argument DEPTH limit max depth."
  (gh-repo-file-dirs-recoursively
   directory match depth
   (lambda (it)
     (let ((result (not
                    (or
                     (member (file-name-base it)
                             '("snap" "node_modules" "share"))
                     ;; (string-match-p "[0-9]" it)
                     (file-exists-p (expand-file-name ".git" it))
                     (file-exists-p (expand-file-name "node_modules" it))))))
       result))))

(defun gh-repo-vc-dir-p (directory)
  "Return the root directory for the DIRECTORY VC tree."
  (let ((default-directory directory))
    (vc-root-dir)))

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
                      (if callback
                          (funcall callback)
                        (dired project-dir)))
                  (user-error (format "%s\n%s" command output))))))
           (when (fboundp 'comint-output-filter)
             (set-process-filter proc #'comint-output-filter)))))

(defvar gh-repo--description nil)
(defvar gh-repo--license gh-repo-default-license)
(defvar gh-repo--gitignore nil)
(defvar gh-repo--name nil)

(defvar gh-repo-current-user nil)
(defun gh-repo-get-current-user ()
  "Return name of currently logged gh user or nil."
  (with-temp-buffer
    (save-excursion (insert (shell-command-to-string "gh auth status")))
    (when (re-search-forward
           "Logged in to github.com as[\s\t\n\r\f]\\([^\s\t\n\r\f]+\\)"
           nil t 1)
      (match-string-no-properties 1))))

(defun gh-repo-login-with-token (token)
  "Feed TOKEN to gh auth and return currently logged user or nil."
  (with-temp-buffer
    (insert token)
    (let ((status (call-process-region
                   (point-min)
                   (point-max)
                   "gh" t t nil
                   "auth" "login"
                   "--with-token")))
      (when (eq 0 status)
        (gh-repo-get-current-user)))))

(defun gh-repo-read-token ()
  "Search for gh token in `auth-sources'."
  (when-let ((variants
              (seq-uniq
               (auth-source-search
                :host "api.github.com"
                :max most-positive-fixnum)
               (lambda (a b) (equal (auth-info-password a)
                               (auth-info-password b))))))
    (auth-info-password
     (car (auth-source-search
           :host "api.github.com"
           :user (completing-read
                  "Source:\s"
                  (mapcar
                   (lambda (it) (plist-get it :user))
                   variants)
                  nil t))))))

;;;###autoload
(defun gh-repo-change-user ()
  "Change gh user."
  (interactive)
  (setq gh-repo-current-user (gh-repo-get-current-user))
  (if (or (null gh-repo-current-user)
          (yes-or-no-p (format "Change user %s?" gh-repo-current-user)))
      (setq gh-repo-current-user
            (gh-repo-login-with-token (or
                                       (gh-repo-read-token)
                                       (read-passwd
                                        "GH token:\s"))))
    gh-repo-current-user))

(defvar gh-repo-licence-types nil)

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

;;;###autoload
(defun gh-repo-read-license ()
  "Read a licence in the minibuffer, with completion."
  (interactive)
  (unless gh-repo-licence-types
    (setq gh-repo-licence-types
          (seq-sort-by (lambda (it) (if (equal (gh-repos-get-prop it :value)  "gpl-3.0")
                                   1
                                 -1))
                       '>
                       (mapcar
                        (lambda (cell) (let ((value (cdr (assoc 'key cell)))
                                        (label (cdr (assoc 'name cell))))
                                    (gh-repos-add-props label :value value)))
                        (append (gh-repo-load-licences) nil)))))
  (setq gh-repo--license
        (gh-repos-get-prop
         (completing-read "License"
                          gh-repo-licence-types)
         :value)))

;;;###autoload
(defun gh-repo--description-read ()
  "Read a description in the minibuffer, with completion."
  (interactive)
  (setq gh-repo--description (prin1-to-string
                              (read-string
                               "--description\s"
                               gh-repo--description))))

;;;###autoload
(defun gh-repo-read-gitignore-read ()
  "Read a gitignore template in the minibuffer, with completion."
  (interactive)
  (setq gh-repo--gitignore (completing-read
                            "--gitignore\s"
                            '("AL" "Actionscript" "Ada" "Agda" "Android" "AppEngine" "AppceleratorTitanium" "ArchLinuxPackages" "Autotools" "C++" "C" "CFWheels" "CMake" "CUDA" "CakePHP" "ChefCookbook" "Clojure" "CodeIgniter" "CommonLisp" "Composer" "Concrete5" "Coq" "CraftCMS" "D" "DM" "Dart" "Delphi" "Drupal" "EPiServer" "Eagle" "Elisp" "Elixir" "Elm" "Erlang" "ExpressionEngine" "ExtJs" "Fancy" "Finale" "FlaxEngine" "ForceDotCom" "Fortran" "FuelPHP" "GWT" "Gcov" "GitBook" "AL" "Anjuta" "Ansible" "Archives" "Backup" "Bazaar" "BricxCC" "CVS" "Calabash" "Cloud9" "CodeKit" "DartEditor" "Diff" "Dreamweaver" "Dropbox" "Eclipse" "EiffelStudio" "Emacs" "Ensime" "Espresso" "FlexBuilder" "GPG" "Images" "JDeveloper" "JEnv" "JetBrains" "KDevelop4" "Kate" "Lazarus" "LibreOffice" "Linux" "LyX" "MATLAB" "Mercurial" "Metals" "MicrosoftOffice" "ModelSim" "Momentics" "MonoDevelop" "NetBeans" "Ninja" "NotepadPP" "Octave" "Otto" "PSoCCreator" "Patch" "PuTTY" "Redcar" "Redis" "SBT" "SVN" "SlickEdit" "Stata" "SublimeText" "Syncthing" "SynopsysVCS" "Tags" "TextMate" "TortoiseGit" "Vagrant" "Vim" "VirtualEnv" "Virtuoso" "VisualStudioCode" "WebMethods" "Windows" "Xcode" "XilinxISE" "macOS" "Go" "Godot" "Gradle" "Grails" "Haskell" "IGORPro" "Idris" "JBoss" "JENKINS_HOME" "Java" "Jekyll" "Joomla" "Julia" "KiCad" "Kohana" "Kotlin" "LabVIEW" "Laravel" "Leiningen" "LemonStand" "Lilypond" "Lithium" "Lua" "Magento" "Maven" "Mercury" "MetaProgrammingSystem" "Nanoc" "Nim" "Node" "OCaml" "Objective-C" "Opa" "OpenCart" "OracleForms" "Packer" "Perl" "Phalcon" "PlayFramework" "Plone" "Prestashop" "Processing" "PureScript" "Python" "Qooxdoo" "Qt" "R" "ROS" "Rails" "Raku" "RhodesRhomobile" "Ruby" "Rust" "SCons" "Sass" "Scala" "Scheme" "Scrivener" "Sdcc" "SeamGen" "SketchUp" "Smalltalk" "Stella" "SugarCRM" "Swift" "Symfony" "SymphonyCMS" "TeX" "Terraform" "Textpattern" "TurboGears2" "TwinCAT3" "Typo3" "Unity" "UnrealEngine" "VVVV" "VisualStudio" "Waf" "WordPress" "Xojo" "Yeoman" "Yii" "ZendFramework" "Zephir" "SAM" "AltiumDesigner" "AutoIt" "B4X" "Bazel" "Beef" "InforCMS" "Kentico" "Umbraco" "core" "Phoenix" "Exercism" "GNOMEShellExtension" "Go.AllowList" "Hugo" "Gretl" "JBoss4" "JBoss6" "Cordova" "Meteor" "NWjs" "Vue" "LensStudio" "Snap" "Logtalk" "NasaSpecsIntact" "OpenSSL" "Bitrix" "CodeSniffer" "Drupal7" "Jigsaw" "Magento1" "Magento2" "Pimcore" "ThinkPHP" "Puppet" "JupyterNotebooks" "Nikola" "ROS2" "Racket" "Red" "SPFx" "Splunk" "Strapi" "V" "Xilinx" "AtmelStudio" "IAR_EWARM" "esp-idf" "uVision"))))

(defun gh-repo-guess-repos-dirs ()
  "Execute `fdfind' and return list parent directories of git repos."
  (or
   (let ((command (seq-find 'executable-find
                            '("fdfind" "fd" "find"))))
     (pcase command
       ((or "fd" "fdfind")
        (funcall
         (gh-repo--compose
          #'delete-dups
          (gh-repo--partial mapcar
                            (gh-repo--compose #'gh-repo-file-parent
                                              #'gh-repo-file-parent))
          (gh-repo--rpartial split-string "\n" t)
          #'shell-command-to-string
          (gh-repo--rpartial string-join "\s"))
         (list command
               "--max-depth 4 --color=never -H -t d -g '.git' -E node_modules -E .cache -E .local -E .nvm . ~/")))
       ("find" (funcall
                (gh-repo--compose
                 #'delete-dups
                 (gh-repo--partial mapcar #'gh-repo-file-parent)
                 (gh-repo--rpartial split-string "\n" t)
                 #'shell-command-to-string)
                "find ~/ -name .git -maxdepth 4 -exec dirname {} \\; -prune 2>&1 | grep -v \"Permission denied\""))))
   (nconc
    (list (expand-file-name "~/"))
    (gh-repo-non-git-dirs-recoursively
     "~/" "^[^\\.]"))))

(defun gh-repo-read-dir (prompt basename)
  "Read directory with PROMPT and BASENAME."
  (let* ((default-variants
          (mapcar
           (lambda (dir)
             (if (file-exists-p
                  (expand-file-name basename dir))
                 (let ((count 0)
                       (name basename))
                   (while (and (file-exists-p
                                (expand-file-name name dir))
                               (not (gh-repo-vc-dir-p
                                     (expand-file-name name dir))))
                     (setq count (1+ count))
                     (setq name (format "%s-%s"
                                        (replace-regexp-in-string
                                         "-[0-9]+$" "" name)
                                        count)))
                   (expand-file-name name dir))
               (expand-file-name basename dir)))
           (gh-repo-guess-repos-dirs)))
         (variants (if (null (gh-repo-vc-dir-p default-directory))
                       (append `(,default-directory) default-variants)
                     default-variants)))
    (file-name-as-directory
     (completing-read (or prompt "Directory:\s") variants nil nil
                      (gh-repo-file-parent
                       (car variants))))))

;;;###autoload
(defun gh-repo-create-read-repo-name ()
  "Read a repository name to create."
  (interactive)
  (let ((initial-input (if buffer-file-name
                           (when buffer-file-name
                             (file-name-base buffer-file-name))
                         (car
                          (split-string
                           (if (gh-repo-file-ancestor-of-p
                                gh-repo-download-default-repo-dir
                                default-directory)
                               (replace-regexp-in-string
                                gh-repo-download-default-repo-dir
                                ""
                                (expand-file-name default-directory))
                             (file-name-base default-directory))
                           "/" t)))))
    (when (or (null initial-input)
              (string-empty-p initial-input))
      (setq initial-input gh-repo--name))
    (setq gh-repo--name (read-string "Name of repository:\s" initial-input))))

(defun gh-repo-generic-command ()
  "Make comamnds."
  (let ((vars '(gh-repo--push
                gh-repo--disable-wiki
                gh-repo--disable-issues
                gh-repo--internal
                gh-repo--description
                gh-repo--license
                gh-repo--gitignore))
        (flags))
    (setq flags (mapcar
                 (lambda (it)
                   (when-let* ((value (symbol-value it))
                               (flag
                                (and value
                                     (concat "--"
                                             (car
                                              (reverse
                                               (split-string
                                                (format "%s" it) "--" t)))))))
                     (cond
                      ((and (stringp value)
                            (equal value flag))
                       flag)
                      ((stringp value)
                       (list flag value))
                      (t flag))))
                 vars))
    (setq flags (append flags (list
                               (if (or
                                    (equal gh-repo--private "--private")
                                    (equal gh-repo--private t))
                                   "--private"
                                 "--public"))))
    (when gh-repo--name
      (flatten-list
       (append `("gh" "repo" "create" ,gh-repo--name)
               (delete nil flags))))))

(defun gh-repo-create ()
  "Create new gh repository and return t if success."
  (when-let ((cmd (gh-repo-generic-command)))
    (with-temp-buffer
      (let ((status (apply 'call-process (car cmd) nil t nil (cdr cmd))))
        (message (buffer-string))
        (eq status 0)))))

;;;###autoload
(defun gh-repo-create-repo ()
  "Create new repository with gh."
  (interactive)
  (while (null gh-repo--name)
    (setq gh-repo--name (gh-repo-create-read-repo-name)))
  (when (gh-repo-create)
    (let ((name gh-repo--name))
      (setq gh-repo--name nil)
      (when (yes-or-no-p (format "Clone repo %s?" name))
        (gh-repo-clone-repo name)))))

(defun gh-repo-view-repo (repo)
  "View REPO in help buffer fontified with org mode or markdown mode.
Org mode detected by #+begin_ blocks."
  (let* ((body
          (with-temp-buffer
            (shell-command
             (concat "gh repo view " repo)
             (current-buffer)
             (current-buffer))
            (let ((mode (if (re-search-forward
                             (regexp-quote "#+begin_") nil t 1)
                            'org-mode
                          'markdown-mode)))
              (gh-repo-fontify (buffer-substring-no-properties
                                (point-min) (point-max))
                               mode))))
         (buffer (with-current-buffer (get-buffer-create "*gh-repo*")
                   (setq buffer-read-only t)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (save-excursion
                       (insert body)))
                   (local-set-key (kbd "q") #'quit-window)
                   (current-buffer))))
    (display-buffer buffer t)
    (if help-window-select
        (progn
          (pop-to-buffer buffer)
          (message "Type \"q\" to restore previous buffer"))
      (message (concat "Type \"q\" to exit")))))

(defun gh-repo-fetch-repos (&rest flags)
  "Execute gh repo list with FLAGS.
Return list with user repositories.

Each item is propertized with :type (private or public) and :description."
  (setq flags (delete nil (flatten-list flags)))
  (delete nil
          (mapcar
           (lambda (it) (let ((parts (split-string it))
                         (name))
                     (when (setq name (pop parts))
                       (gh-repos-add-props
                        name :description (string-join parts " - ")
                        :type (if (member "private" parts)
                                  "private"
                                "public")))))
           (split-string
            (gh-repo-exec (if flags
                         (concat "gh repo list "
                                 (mapconcat
                                  (apply-partially 'format "%s")
                                  flags "\s"))
                       "gh repo list"))
            "\n"))))

(defvar gh-repo-repos-limit 50)
(defvar gh-repo-user-repos nil)

(defun gh-repo-annotate-repo (repo)
  "Fontify REPO :description text property depending on :type."
  (if-let* ((type  (gh-repos-get-prop repo :type))
            (face (if (equal type "public")
                      font-lock-keyword-face
                    font-lock-builtin-face)))
      (concat "\s" (propertize (or (gh-repos-get-prop repo :description)
                                   "")
                               'face
                               face))
    ""))

;;;###autoload
(defun gh-repo-clone-repo (name)
  "Clone repository NAME into target-dir or `gh-repo-download-default-repo-dir'."
  (interactive)
  (if-let* ((basename (car (reverse (split-string name "/" t))))
            (project-dir (gh-repo-read-dir
                          (format "Clone %s to " basename) basename)))
      (let ((command (read-string "" (string-join
                                      (list "gh" "repo" "clone" name
                                            project-dir)
                                      "\s"))))
        (setq project-dir (expand-file-name
                           (car (reverse (split-string command)))))
        (gh-repo-exec-in-dir command project-dir))
    (message "Cannot clone")))

(defun gh-repo-remove (repo)
	"Ask user a \"y or n\" question and remove gh REPO if y."
  (when (and (stringp repo)
             (not (string-empty-p (string-trim repo)))
             (yes-or-no-p (format "Remove %s?" repo)))
    (message (shell-command-to-string
              (concat "gh repo delete "
                      repo
                      " --confirm")))))

(defun gh-repo-visit (repo)
  "Visit github REPO."
  (browse-url
   (if (string-match-p "^https://" repo)
       repo
     (concat "https://github.com/" repo))))

;;;###autoload
(defun gh-repo-switch-to-hydra ()
  "During active minibuffer completion just exit it.
During inactive minibuffer call `gh-repo-hydra/body'."
  (interactive)
  (if (active-minibuffer-window)
      (exit-minibuffer)
    (gh-repo-hydra/body)))

;;;###autoload
(defun gh-repo-change-repos-limit ()
  "During active minibuffer completion just exit it.
During inactive minibuffer read value for `gh-repo-repos-limit',
and invoke `gh-repo-read-user-repo'."
  (interactive)
  (if (active-minibuffer-window)
      (exit-minibuffer)
    (setq gh-repo-repos-limit
          (read-number "gh repo list --limit\s"))
    (gh-repo-read-user-repo)))

(defvar gh-repos-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C->") 'gh-repo-switch-to-hydra)
    (define-key map (kbd "M-<up>") 'gh-repo-change-repos-limit)
    map)
  "Keymap for files sources.")

(defun gh-repo--read-user-repo ()
  "Read user repository with completions.
You can change limit in minibuffer with \\<gh-repos-minibuffer-map>\ `\\[gh-repo-change-repos-limit]'."
  (setq gh-repo-user-repos
        (if gh-repo-repos-limit
            (gh-repo-fetch-repos
             (format "--limit %s"
                     gh-repo-repos-limit))
          (gh-repo-fetch-repos)))
  (minibuffer-with-setup-hook
      (lambda () (use-local-map
             (let ((map (copy-keymap gh-repos-minibuffer-map)))
               (set-keymap-parent map (current-local-map))
               map)))
    (let ((minibuffer-help-form
           (substitute-command-keys
            "\\<gh-repos-minibuffer-map>\ `\\[gh-repo-change-repos-limit]' - to change number of displayed repositories,\n`\\[gh-repo-switch-to-hydra]' switch to hydra")))
      (completing-read "Repository:\s"
                       (lambda (str pred action)
                         (if (eq action 'metadata)
                             `(metadata
                               (annotation-function . gh-repo-annotate-repo))
                           (complete-with-action
                            action gh-repo-user-repos str pred)))))))

;;;###autoload
(defun gh-repo-read-user-repo (&optional action)
  "Read user repository and execute ACTION.
If ACTION is nil read it from `gh-repo-actions'.

During minibuffer completion next commands are available:

\\<gh-repos-minibuffer-map>\ `\\[gh-repo-change-repos-limit]' - to change number of displayed repositories,
`\\[gh-repo-switch-to-hydra]' switch to hydra."
  (interactive)
  (setq gh-repo-current-user (or gh-repo-current-user
                                 (gh-repo-get-current-user)))
  (let ((repo (gh-repo--read-user-repo)))
    (pcase this-command
      ('gh-repo-change-repos-limit
       (setq repo nil)
       (setq gh-repo-repos-limit
             (read-number "gh repo list --limit\s"))
       (gh-repo-read-user-repo))
      ('gh-repo-switch-to-hydra (gh-repo-hydra/body)
                                (setq repo nil)))
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

(defhydra gh-repo-hydra
  (:color pink :pre
          (unless gh-repo-current-user
            (setq gh-repo-current-user (gh-repo-get-current-user))))
  "
Create new repository options:
_n_ ame                                %`gh-repo--name
_u_ change [u]ser                      %`gh-repo-current-user
_p_ [p]rivate or [p]ublic              %`gh-repo--private
_P_ toggle --push                      %`gh-repo--push
_w_ disable wiki                       %`gh-repo--disable-wiki
_i_ disable issues                     %`gh-repo--disable-issues
_I_ make [I]nternal                    %`gh-repo--internal
_d_ description                        %`gh-repo--description
_l_ specify license                    %`gh-repo--license
_g_ specify [g]itignore template       %`gh-repo--gitignore
_C_ run [C]ommand                      %(string-join (gh-repo-generic-command) \"\s\")
_C->_ show all my repos
"
  ("n" gh-repo-create-read-repo-name nil)
  ("u" gh-repo-change-user nil)
  ("p" gh-repo--private-toggle nil)
  ("P" gh-repo--push-toggle nil)
  ("w" gh-repo--disable-wiki-toggle nil)
  ("i" gh-repo--disable-issues-toggle nil)
  ("I" gh-repo--internal-toggle nil)
  ("d" gh-repo--description-read nil)
  ("l" gh-repo-read-license nil)
  ("g" gh-repo-read-gitignore-read nil)
  ("C" gh-repo-create-repo nil :exit t)
  ("C->" gh-repo-read-user-repo nil :exit t)
  ("q" nil "quit"))

(provide 'gh-repo)
;;; gh-repo.el ends here