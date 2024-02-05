;;; gh-repo.el --- Create and manage gh repositories -*- lexical-binding: t -*-

;; Copyright © 2020-2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gh-repo
;; Keywords: lisp, vc, tools
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (transient "0.5.3") (ghub "3.6.0") (project "0.10.0"))
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

;; This library provides interactive Emacs commands for interacting with GitHub
;; repositories using the GitHub API. Users can create, list, and manage GitHub
;; repositories directly from Emacs.

;;; Installation:

;; To install, simply add `gh-repo.el' to your load-path, require the package, and
;; configure your GitHub authentication.

;; Example configuration:
;; (require 'gh-repo)
;; (setq gh-repo-ghub-auth-info '("YOUR_GITHUB_USERNAME" . gh-repo))

;;; Usage:

;; This library offers several interactive commands to work with GitHub
;; repositories. A transient menu is available to access all the key features.
;; To open the transient menu, use:

;; `M-x gh-repo-menu';

;; Key features include:

;; - Creating new repositories
;; - Viewing repositories as a rich tree structure
;; - Listing user repositories
;; - Cloning repositories
;; - Starring/unstarring repositories
;; - Deleting repositories
;; - Searching for repositories with advanced filtering

;; Functions like gh-repo-clone-repo, gh-repo-delete, and gh-repo-list-repos
;; provide quick access to the most common tasks, while the transient menu
;; offers an organized interface for all features.


;;; Code:

(eval-when-compile
  (require 'subr-x))

(defvar json-object-type)
(defvar json-array-type)
(defvar json-false)
(defvar json-null)

(require 'vtable)
(require 'url-parse)
(require 'color)
(require 'project)
(require 'ghub)
(require 'parse-time)

(defvar gh-repo-languages nil)
(defvar gh-repo-licences nil)
(defvar gh-repo-gitignore-templates nil)

(defvar gh-repo--cached-auth-data nil)
(defvar gh-repo-special-color-column (color-darken-name
                                      (frame-parameter
                                       (selected-frame)
                                       'background-color)
                                      40))

(defcustom gh-repo-search-code-queries '(("in"
                                          :choices
                                          ("name" "topics"
                                           "name,topics" "readme"
                                           "name,readme"
                                           "topics,readme"
                                           "name,topics,readme"
                                           "decription"
                                           "name,decription"
                                           "topics,decription"
                                           "name,topics,decription"
                                           "readme,decription"
                                           "name,readme,decription"
                                           "topics,readme,decription"
                                           "name,topics,readme,decription")
                                          :class transient-option)
                                         ("language"
                                          :choices gh-repo--language-choices
                                          :class transient-option)
                                         ("user"
                                          :reader gh-repo-github-user
                                          :class transient-option)
                                         ("org"
                                          :class transient-option))
  "Code search queries."
  :group 'gh-repo
  :type '(alist
          :key-type string
          :value-type
          (plist
           :key-type symbol
           :value-type sexp)))

(defun gh-repo--time-formatter (v)
  "Format a given time V as a human-readable time difference.

Argument V is a string representing an ISO 8601 time."
  (or
   (and v
        (gh-repo-format-time-diff
         (parse-iso8601-time-string v)))
   ""))

(define-widget 'gh-repo-list-format 'lazy
  "An alist of field paths and column properties for the repository listing."
  :type '(alist
          :key-type (radio :value full_name
                     (symbol
                      :tag "Field Name"
                      :completions
                      gh-repo--field-name-completion-table)
                     (repeat
                      :tag "Field Path to the value"
                      :value (owner login)
                      (symbol
                       :completions gh-repo--field-name-completion-table)))
          :value-type (plist
                       :options
                       (((const
                          :format "%v "
                          :doc "The name of the column"
                          :name)
                         (string
                          :tag "Column Name"
                          :completions gh-repo--column-name-completions))
                        ((const :width)
                         (radio
                          :value 10
                          (natnum :tag
                           "The width of that many ‘x’ characters")
                          (string :tag
                           "‘Xpx’ denoting a number of pixels"
                           "50px")
                          (string :tag
                           "‘X%’ a percentage of the window’s width"
                           "10%")))
                        ((const
                          :format "%t%n%h"
                          :doc "The minimum width (overrides ‘width’ if ‘width’ is smaller than this).
             ‘min-width’/‘max-width’ can be useful if ‘width’ is
             given as a percentage of the window width, and you want to
             ensure that the column doesn’t grow pointlessly large or
             unreadably narrow."
                          :min-width)
                         (radio
                          :value "10%"
                          (natnum :tag
                           "The width of that many ‘x’ characters"
                           10)
                          (string :tag
                           "‘Xpx’ denoting a number of pixels"
                           "50px")
                          (string :tag
                           "‘X%’ a percentage of the window’s width"
                           "5%")))
                        ((const
                          :format "%t%n%h"
                          :doc "The maximum width (overrides ‘width’ if ‘width’ is greater than this).
             ‘min-width’/‘max-width’ can be useful if ‘width’ is
             given as a percentage of the window width, and you want to
             ensure that the column doesn’t grow pointlessly large or
             unreadably narrow."
                          :max-width)
                         (radio
                          :value "10%"
                          (natnum :tag
                           "The width of that many ‘x’ characters"
                           20)
                          (string :tag
                           "‘X%’ a percentage of the window’s width"
                           "30%")
                          (string :tag
                           "‘Xpx’ denoting a number of pixels"
                           "200px")))
                        ((const
                          :doc
                          "Whether this is the primary column—this will be used for initial sorting"
                          :primary)
                         (radio
                          :value ascend
                          (const ascend)
                          (const descend)))
                        ((const
                          :tag "Custom getter (object table)"
                          :format "%t%n%h"
                          :doc
                          "Function that called with two parameters: `object' and `table'."
                          :getter)
                         (function))
                        ((const
                          :tag "Custom formatter"
                          :format "%t%n%h"
                          :doc
                          "Function that called with one parameter: the column value."
                          :formatter)
                         (function :tag "Function (VALUE)"))
                        ((const
                          :tag
                          "Custom function displayer to prepare the formatted value for display"
                          :doc
                          "This function (FVALUE MAX-WIDTH TABLE) should return a truncated string to display.
FVALUE is the formatted value
MAX-WIDTH is the maximum width (in pixels),
TABLE is the table."
                          :format "%t%n%h"
                          :displayer)
                         (function :tag "Function (FVALUE MAX-WIDTH TABLEN)"))
                        ((const
                          :format "%v "
                          :align)
                         (radio
                          :value "left"
                          (const "right")
                          (const "left")))
                        ((const
                          :format "%t%n%h"
                          :doc "Background color or face on the columns.
    The most common use case here is to have alternating background colors on
    the columns, so this would usually be a list of two colors."
                          :color)
                         (radio
                          color
                          face))))))

(defvar gh-repo--field-names '(name full_name private
                               owner login
                               url type site_admin
                               description fork
                               created_at updated_at
                               pushed_at size
                               stargazers_count
                               watchers_count language
                               has_issues
                               has_projects
                               has_downloads has_wiki
                               has_pages
                               has_discussions
                               forks_count archived
                               disabled
                               open_issues_count
                               license key spdx_id
                               allow_forking
                               is_template
                               web_commit_signoff_required
                               topics
                               visibility forks
                               open_issues watchers
                               default_branch
                               permissions admin
                               maintain push
                               triage pull))

(defun gh-repo--field-name-completion-table (string pred action)
  "Provide completion for GitHub repository field names.

Argument STRING is the prefix for completion.

Argument PRED is a predicate function that filters completion candidates.

Argument ACTION determines the behavior of the completion function."
  (let ((completion-ignore-case
         t))
    (complete-with-action
     action
     gh-repo--field-names
     string
     pred)))

(defun gh-repo--column-name-completions (string pred action)
  "Provide completions for GitHub repository column names.

Argument STRING is the prefix for completion.

Argument PRED is a predicate function that can be used to filter the completion
results.

Argument ACTION determines what kind of operation to perform: \\='metadata for
metadata, nil for a list of all possible completions, t for the best completion,
or a STRING for an actual completion operation."
  (let ((completion-ignore-case
         t))
    (complete-with-action
     action
     (mapcar (lambda (sym)
               (mapconcat #'capitalize
                          (split-string
                           (symbol-name sym)
                           "_" t)
                          " "))
             gh-repo--field-names)
     string
     pred)))



(defcustom gh-repo-list-user-repo-columns `((name
                                             :name "Name"
                                             :width 25)
                                            (description
                                             :name "Description"
                                             :width 40)
                                            (visibility
                                             :name "Visibilty"
                                             :width 10)
                                            (language
                                             :name "Lang"
                                             :width 15)
                                            (stargazers_count
                                             :name "Stars"
                                             :color
                                             ,gh-repo-special-color-column
                                             :width 4)
                                            (forks_count
                                             :name "Forks"
                                             :color ,(color-lighten-name
                                                      (frame-parameter
                                                       (selected-frame)
                                                       'background-color)
                                                      40)
                                             :width 4)
                                            (watchers_count
                                             :name "Watch"
                                             :color
                                             ,gh-repo-special-color-column
                                             :width 4)
                                            (open_issues_count
                                             :name "Issues"
                                             :color ,(color-lighten-name
                                                      (frame-parameter
                                                       (selected-frame)
                                                       'background-color)
                                                      40)
                                             :width 4)
                                            (fork
                                             :name "Fork"
                                             :color
                                             ,gh-repo-special-color-column
                                             :align "right"
                                             :width 4))
  "Columns to display when listing a user's repositories in a formatted table.

It is an alist of field paths and column properties for the repository listing.

Each column is a cons cell, where the car is a path (a field name or a list of
field names) to the value, and the cdr is a property list with the following
keys:

‘:name’ The name of the column.

     ‘:width’
          The width of the column.  This is either a number (the width
          of that many ‘x’ characters in the table’s face), or a string
          on the form ‘XeX’, where X is a number of ‘x’ characters, or a
          string on the form ‘XpX’ (denoting a number of pixels), or a
          string on the form ‘X%’ (a percentage of the window’s width).

     ‘:min-width’
          This uses the same format as ‘width’, but specifies the
          minimum width (and overrides ‘width’ if ‘width’ is smaller
          than this.

     ‘:max-width’
          This uses the same format as ‘width’, but specifies the
          maximum width (and overrides ‘width’ if ‘width’ is larger than
          this.  ‘min-width’/‘max-width’ can be useful if ‘width’ is
          given as a percentage of the window width, and you want to
          ensure that the column doesn’t grow pointlessly large or
          unreadably narrow.

     ‘:primary’
          Whether this is the primary column—this will be used for
          initial sorting.  This should be either ‘ascend’ or ‘descend’
          to say in which order the table should be sorted.

     ‘:getter’
          If present, this function will be called to return the column
          value. In this case the field path will be ignored.

           -- Function: column-getter object table
               It’s called with two parameters: the object and the
               table.

     ‘:formatter’
          If present, this function will be called to format the value.

           -- Function: column-formatter value
               It’s called with one parameter: the column value.

     ‘:displayer’
          If present, this function will be called to prepare the
          formatted value for display.  This function should return a
          string with the table face applied, and also limit the width
          of the string to the display width.

           -- Function: column-displayer fvalue max-width tablen
               FVALUE is the formatted value; MAX-WIDTH is the maximum
               width (in pixels), and TABLE is the table.

     ‘:align’
          Should be either ‘right’ or ‘left’.

     ‘:color' - the color of the column's content."
  :group 'gh-repo
  :type 'gh-repo-list-format)

(defcustom gh-repo-list-search-columns '((name
                                          :name "Name"
                                          :width 25)
                                         ((owner login)
                                          :name "Owner"
                                          :width 20)
                                         (description
                                          :name "Description"
                                          :width 40)
                                         (language
                                          :name "Lang"
                                          :width 15)
                                         (stargazers_count
                                          :name "Stars"
                                          :width 4)
                                         (updated_at
                                          :name "Updated At"
                                          :formatter gh-repo--time-formatter))
  "An alist of field paths and column properties for the repository listing.

Each column is a cons cell, where the car is a path (a field name or a list of
field names) to the value, and the cdr is a property list with the following
keys:

    ‘:name’ The name of the column.

    ‘:width’
          The width of the column.  This is either a number (the width
          of that many ‘x’ characters in the table’s face), or a string
          on the form ‘XeX’, where X is a number of ‘x’ characters, or a
          string on the form ‘XpX’ (denoting a number of pixels), or a
          string on the form ‘X%’ (a percentage of the window’s width).

     ‘:min-width’
          This uses the same format as ‘width’, but specifies the
          minimum width (and overrides ‘width’ if ‘width’ is smaller
          than this.

     ‘:max-width’
          This uses the same format as ‘width’, but specifies the
          maximum width (and overrides ‘width’ if ‘width’ is larger than
          this.  ‘min-width’/‘max-width’ can be useful if ‘width’ is
          given as a percentage of the window width, and you want to
          ensure that the column doesn’t grow pointlessly large or
          unreadably narrow.

     ‘:primary’
          Whether this is the primary column—this will be used for
          initial sorting.  This should be either ‘ascend’ or ‘descend’
          to say in which order the table should be sorted.

     ‘:getter’
          If present, this function will be called to return the column
          value. In this case the field path will be ignored.

           -- Function: column-getter object table
               It’s called with two parameters: the object and the
               table.

     ‘:formatter’
          If present, this function will be called to format the value.

           -- Function: column-formatter value
               It’s called with one parameter: the column value.

     ‘:displayer’
          If present, this function will be called to prepare the
          formatted value for display.  This function should return a
          string with the table face applied, and also limit the width
          of the string to the display width.

           -- Function: column-displayer fvalue max-width tablen
               FVALUE is the formatted value; MAX-WIDTH is the maximum
               width (in pixels), and TABLE is the table.

     ‘:align’
          Should be either ‘right’ or ‘left’.

     ‘:color' - the color of the column's content."
  :group 'gh-repo
  :type 'gh-repo-list-format)

(defcustom gh-repo-cache-dir (locate-user-emacs-file "var/gh-repo/")
  "The directory used to cache GitHub repository data.

The value can be set to nil to disable caching, or to a specific directory path
where the cache files should be stored.

To change the cache directory, set this variable to the desired directory path
as a string."
  :group 'gh-repo
  :type '(choice
          (item
           :tag "Don't store cache"
           :value nil)
          (directory)))

(defmacro gh-repo-tree--window-with-other-window (&rest body)
  "Execute BODY in other window.
If other window doesn't exists, split selected window right."
  `(with-selected-window
       (gh-repo--get-other-wind)
     (progn ,@body)))

(defvar-local gh-repo-list--items nil)
(defvar-local gh-repo-list--columns nil)
(defvar-local gh-repo-list--page 1)
(defvar-local gh-repo-list--timer nil)
(defvar-local gh-repo-list--total-count nil)
(defvar-local gh-repo-list--text nil)
(defvar-local gh-repo-list--query nil)
(defvar-local gh-repo-list--stop nil)


(defun gh-repo-query-to-option (key descr arg props)
  "Convert GitHub repository query to option list.

Argument KEY is a symbol representing the option key.

Argument DESCR is a string describing the option.

Argument ARG is a string representing the command-line argument.

Arguments PROPS is a list of additional properties for the option."
  (append (list key descr arg)
          (append props
                  (list :init-value (lambda (obj)
                                      (let* ((alist
                                              (and
                                               gh-repo-list--query
                                               (gh-repo-format-query-to-args
                                                gh-repo-list--query)))
                                             (value
                                              (cadr (assoc-string
                                                     arg alist))))
                                        (setf
                                         (slot-value obj 'value)
                                         value)))))))
(defun gh-repo-search-queries-to-options (queries)
  "Convert GitHub search QUERIES into command line options.

Argument QUERIES is a list."
  (mapcan
   (pcase-lambda (`(,v . ,props))
     (let ((k (substring-no-properties v 0 1)))
       (list (append (gh-repo-query-to-option
                      (format "+%s" k)
                      v
                      (format "--%s=" v)
                      props))
             (append (gh-repo-query-to-option
                      (format "-%s" k)
                      (format "not %s" v)
                      (format "--not-%s=" v)
                      props)))))
   queries))

(require 'shell)
(require 'comint)
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
(declare-function ivy-alist-setting "ivy")
(declare-function ivy-re-to-str "ivy")
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
                             (?v "view files tree" gh-repo-tree)
                             (?i "insert" insert)
                             (?w "copy" kill-new))
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

(defcustom gh-repo-annotation-spec-alist '(((owner login) "%s" 20)
                                           (language "%s" 20)
                                           (updated_at "%s" 15)
                                           (description "%s" 40)
                                           (stargazers_count "⭐%s" 10)
                                           (visibility "👁️%s" 20)
                                           (open_issues "⁉️%s" nil))
  "Alist mapping GitHub repo attributes to display specs.

An alist where each element specifies how to display a column of GitHub
repository information in the minibuffer during selection.

Each element of the alist is a list with the following structure: - The first
item is either a symbol or a list of symbols that specifies the path to the
value in the repository data structure.

- The second item is a format string used to display the value, where \"%s\"
will be replaced by the actual value. -
The third item is an integer that specifies the width of the column in
characters.

The default value includes specifications for the repository owner's login,
language, description, star count, visibility, and open issue count, with
appropriate formatting and column widths.

To customize, add or modify elements in the alist according to the desired
repository data and display format. Each column will be displayed in the order
specified in the alist when selecting a repository."
  :group 'gh-repo
  :type '(alist
          :key-type (choice
                     symbol
                     (repeat :tag "Column Path" symbol))
          :value-type (list
                       (string :tag "Column Name" "%s")
                       (choice
                        (integer :tag "Column Width" 20)
                        (const :tag "None" nil)))))

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

(defvar-local gh-repo-tree--loading nil)
(defvar-local gh-repo-tree--error-loading nil)

(defmacro gh-repo--with-live-buffer (buffer &rest body)
  "Execute BODY in BUFFER if it's a live buffer.

Argument BUFFER is the buffer to be checked for liveliness and used for the BODY
execution.

Remaining arguments BODY are forms to be executed within the context of the live
BUFFER."
  (declare (indent 1)
           (debug t))
  (let ((buffer-var (make-symbol "buffer")))
    `(let ((,buffer-var ,buffer))
      (when (buffer-live-p ,buffer-var)
       (with-current-buffer ,buffer-var
        ,@body)))))


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

(defun gh-repo--serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `elisp-eval-unserialize'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert
       (let (print-level print-length print-circle)
         (prin1-to-string data))))))

(defun gh-repo--unserialize (filename)
  "Read data serialized by `gh-repo--serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (buffer-string))))))

(defun gh-repo--download-json-sync (url)
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
          (gh-repo--json-parse-string
           (buffer-string)
           'alist 'list))
      (kill-buffer download-buffer))))

(defun gh-repo--init-variable (variable fetcher &rest args)
  "Initialize or fetch cached VARIABLE value.

Argument VARIABLE is a symbol representing the variable to initialize.

Argument FETCHER is a function that is called to fetch the value when VARIABLE
is not already set.

Remaining arguments ARGS are passed to the FETCHER function when calling it to
obtain the value for VARIABLE."
  (or (symbol-value variable)
      (let* ((file (and gh-repo-cache-dir
                        (expand-file-name (symbol-name variable)
                                          gh-repo-cache-dir)))
             (value
              (when (and gh-repo-cache-dir
                         (file-exists-p file))
                (gh-repo--unserialize file))))
        (if value
            (set variable value)
          (setq value (apply fetcher args))
          (set variable value)
          (when (and value file)
            (let ((dir (file-name-directory file)))
              (unless (file-exists-p dir)
                (make-directory dir 'parents))
              (gh-repo--serialize value file)))
          value))))

(defun gh-repo--language-choices ()
  "Fetch and return a list of programming languages."
  (gh-repo--init-variable 'gh-repo-languages
                          #'gh-repo--fetch-langs))

(defun gh-repo--fetch-langs ()
  "Fetch language aliases from GitHub's API."
  (mapcan
   (lambda (it)
     (cdr (assq 'aliases it)))
   (gh-repo--download-json-sync
    "https://api.github.com/languages")))

(defun gh-repo--license-choices ()
  "Fetch and return GitHub license keys."
  (gh-repo--init-variable 'gh-repo-licences #'gh-repo--download-json-sync
                          "https://api.github.com/licenses")
  (mapcar (apply-partially #'alist-get 'key) gh-repo-licences))

(defun gh-repo--gitignore-template-choices ()
  "Fetch gitignore template choices from GitHub API."
  (gh-repo--init-variable 'gh-repo-gitignore-templates
                          #'gh-repo--download-json-sync
                          "https://api.github.com/gitignore/templates"))


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
             (when (flatten-list args)
               (apply fn args))))
        (while (setq fn
                     (when arg
                       (pop functions)))
          (let ((res (apply fn (list arg))))
            (setq arg res)))
        arg))))

(defun gh-repo--take-last (n lst)
  "Return the last N elements from a list.

Argument N is an integer that specifies the number of elements to take from the
end of the list.
Argument LST is a list from which the last N elements will be taken."
  (let ((len (length lst)))
    (if (> n len)
        lst
      (nthcdr (- len n) lst))))

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
  (string-match-p
   (concat "https://" gh-repo-util-host-regexp)
   url))

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

(defun gh-repo-values-to-columns (spec data)
  "Transform repository DATA into formatted columns.

Argument SPEC is a list where each element is a list containing a key, a format
string, and a width for the column.

Argument DATA is an alist where each key corresponds to a column key in SPEC and
its value is the DATA to be formatted for that column."
  (mapconcat
   (pcase-lambda (`(,key ,format-str ,width))
     (let ((value (if (listp key)
                      (seq-reduce
                       (lambda (acc sym)
                         (setq acc (alist-get sym acc)))
                       key
                       data)
                    (alist-get key data))))
       (if width
           (truncate-string-to-width (format format-str (or value "")) width
                                     nil ?\s "...")
         (format format-str (or value "")))))
   spec " "))


(defun gh-repo--plist-omit (keys plist)
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

(defun gh-repo--plist-merge (plist-a plist-b)
  "Add props from PLIST-B to PLIST-A."
  (dotimes (idx (length plist-b))
    (when (eq (logand idx 1) 0)
      (let ((prop-name (nth idx plist-b)))
        (let ((val (plist-get plist-b prop-name)))
          (plist-put plist-a prop-name val)))))
  plist-a)

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
           (gh-repo--plist-omit '(:query)
                                args))))

(defun gh-repo--put (resource &optional params &rest args)
  "Send a PUT request to a GitHub repository resource.

Argument RESOURCE is a string representing the GitHub API resource to interact
with.

Optional argument PARAMS is an alist of parameters to send with the PUT request.

Remaining arguments ARGS are additional arguments passed to the `ghub-put'
function."
  (let ((auth (gh-repo-authenticate)))
    (apply #'ghub-put resource params
           :auth (cdr auth)
           :username (car auth)
           args)))

(defun gh-repo--delete (resource &optional params &rest args)
  "Send a DELETE request to a GitHub repository resource.

Argument RESOURCE is a string representing the GitHub API resource to interact
with.

Optional argument PARAMS is an alist of parameters to send with the DELETE
request.

Remaining arguments ARGS are additional arguments passed to the `ghub-delete'
function."
  (let ((auth (gh-repo-authenticate)))
    (apply #'ghub-delete resource params
           :auth (cdr auth)
           :username (car auth)
           args)))

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
                                   (with-selected-window
                                       (active-minibuffer-window)
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
                                          gh-repo-annotation-spec-alist
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
  (let ((fn
         (pcase completing-read-function
           ((guard (bound-and-true-p helm-mode))
            (when (fboundp 'helm-force-update)
              (lambda (_items cand)
                (helm-force-update cand))))
           ('ivy-completing-read
            (when (and (fboundp 'ivy-update-candidates))
              (lambda (items _input)
                (gh-repo-update-ivy-cands
                 items))))
           ((guard (bound-and-true-p icomplete-mode))
            (lambda (&rest _)
              (completion--flush-all-sorted-completions)
              (when (fboundp 'icomplete-exhibit)
                (icomplete-exhibit))))
           ('completing-read-default
            (lambda (&rest _)
              (completion--flush-all-sorted-completions)
              (minibuffer-completion-help)))
           (_
            (lambda (&rest _)
              (completion--flush-all-sorted-completions))))))
    (lambda (&rest args)
      (when-let ((wind (active-minibuffer-window)))
        (with-selected-window wind
          (apply fn args))))))

(defvar gh-repo-repos-hash (make-hash-table :test #'equal))

(defun gh-repo-search-repos (&optional initial-input query)
  "Search and interactively select GitHub repositories using a QUERY.

Optional argument QUERY is a string that specifies the search QUERY.

Optional argument INITIAL-INPUT is a string to start search with."
  (interactive (list (gh-repo-get-search-query)))
  (setq this-command 'gh-repo-search-repos)
  (let ((data (gh-repo-async-comp-read "Repo: " "/search/repositories"
                                       'gh-repo-search-repos
                                       query
                                       'name
                                       (apply-partially
                                        #'gh-repo-values-to-columns
                                        gh-repo-annotation-spec-alist)
                                       t
                                       initial-input)))
    (alist-get 'full_name data)))

(defun gh-repo-async-comp-read (prompt url caller query hash-key &optional
                                       annotate-fn full-data initial-input)
  "Fetch GitHub repo data asynchronously with completion.

Argument PROMPT is a string displayed as the prompt in the minibuffer.

Argument URL is a string representing the GitHub API endpoint to query.

Argument CALLER is a symbol representing the command that invoked the
completion.

Argument QUERY is a string or list representing additional query parameters for
the GitHub API request.

Argument HASH-KEY is a symbol or string used to extract the relevant value from
the API response.

Optional argument ANNOTATE-FN is a function that takes a single argument and
returns an annotation string for each candidate.

Optional argument FULL-DATA is a boolean; when non-nil, the function returns the
full data associated with the selected candidate instead of just the name.


Optional argument INITIAL-INPUT is a string to start search with."
  (gh-repo-authenticate)
  (when caller
    (setq this-command caller))
  (let* ((hash (make-hash-table :test #'equal))
         (done)
         (update-fn
          (gh-repo-minibuffer-get-update-fn))
         (prefix-hash (make-hash-table :test #'equal))
         (maxlen 90)
         (annotf
          (when annotate-fn
            (lambda (key)
              ;; (when (> (length key) maxlen)
              ;;   (setq maxlen (1+ (length key))))
              (let* ((data (gethash key hash))
                     (annot-str (funcall annotate-fn data))
                     (str (concat
                           (propertize " " 'display
                                       `(space :align-to
                                         ,maxlen))
                           annot-str)))
                str))))
         (all-cands)
         (last-text)
         (handler
          (lambda (text)
            (unless done
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
                 (unless done
                   (let ((items (alist-get 'items resp))
                         (str (gh-repo-get-minibuffer-input))
                         (keys))
                     (puthash text items prefix-hash)
                     (dolist (item items)
                       (let ((name
                              (alist-get hash-key item)))
                         (push name keys)
                         (unless (member name all-cands)
                           (push name all-cands))
                         (puthash name item hash)))
                     (when (and items text str (equal text str))
                       (funcall update-fn keys text)))))))))
         (hook-fn (lambda (&rest _)
                    (when-let ((text
                                (gh-repo-get-minibuffer-input)))
                      (unless (or done
                                  (and last-text
                                       (string= last-text text)))
                        (setq last-text text)
                        (if-let ((cache (gethash text prefix-hash)))
                            (gh-repo-debounce
                             'gh-repo-minibuffer-timer
                             0.5
                             update-fn (mapcar (lambda (item)
                                                 (alist-get hash-key item))
                                               cache)
                             text)
                          (gh-repo-debounce
                           'gh-repo-minibuffer-timer
                           0.5
                           handler text)))))))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (when (minibufferp)
                (use-local-map (make-composed-keymap gh-repo-minibuffer-map
                                                     (current-local-map)))
                (add-hook 'after-change-functions hook-fn nil t)
                (add-hook 'minibuffer-exit-hook (lambda ()
                                                  (setq done t)
                                                  (when
                                                      (timerp
                                                       gh-repo-minibuffer-timer)
                                                    (cancel-timer
                                                     gh-repo-minibuffer-timer)))
                          nil t)
                (when initial-input
                  (insert initial-input))))
          (let ((result (completing-read
                         prompt
                         (lambda (str pred action)
                           (if (and annotf
                                    (eq action 'metadata))
                               `(metadata
                                 (annotation-function . ,annotf))
                             (complete-with-action action (hash-table-keys hash)
                                                   str pred)))
                         nil nil)))
            (if full-data
                (gethash result hash)
              result)))
      (setq done t))))

;;;###autoload
(defun gh-repo-github-user (&rest _)
  "Retrieve and display GitHub users based on input in the minibuffer."
  (interactive)
  (gh-repo-authenticate)
  (gh-repo-async-comp-read "User: " "search/users"
                           'gh-repo-github-user
                           nil
                           'login))

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

(defun gh-repo--minibuffer-exit-with-action (action)
  "Call ACTION with current candidate and exit minibuffer."
  (pcase-let ((`(,_category . ,current)
               (gh-repo-minibuffer-get-current-candidate)))
    (progn (run-with-timer 0 nil action current)
           (abort-minibuffers))))

(defun gh-repo--minibuffer-restore-completions-window ()
  "Restore *Completions* window height."
  (when (eq this-command 'minibuffer-next-completion)
    (remove-hook 'post-command-hook
                 #'gh-repo--minibuffer-restore-completions-window)
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (fit-window-to-buffer win completions-max-height))))

(defun gh-repo-browse-current-repo-and-exit ()
  "Browse the current GitHub repository and exit the minibuffer."
  (interactive)
  (gh-repo--minibuffer-exit-with-action #'gh-repo-browse))

(defun gh-repo-browse-current-repo ()
  "Open the current repository without exiting minibuffer."
  (interactive)
  (gh-repo--minibuffer-action-no-exit #'gh-repo-browse))

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

(defun gh-repo-get-arguments ()
  "Return current transient arguments ARGS."
  (let ((raw-args))
    (cond (transient-current-command
           (setq raw-args (transient-args transient-current-command)))
          (transient--prefix
           (setq transient-current-prefix transient--prefix)
           (setq transient-current-command (oref transient--prefix command))
           (setq transient-current-suffixes transient--suffixes)
           (setq raw-args (transient-args transient-current-command))))
    raw-args))

(defun gh-repo-get-args-for-query ()
  "Remove specific arguments from the GitHub code search query."
  (seq-filter
   (apply-partially  #'string-match-p
                     (concat "^--"
                             (regexp-opt
                              (mapcan (pcase-lambda (`(,k . ,_v))
                                        (list (regexp-quote k)
                                              (regexp-quote (concat "not-" k))))
                                      gh-repo-search-code-queries))))
   (gh-repo-get-arguments)))

(defun gh-repo-format-args-to-query (args)
  "Generate a GitHub code search query from given arguments.

Argument ARGS is a list of strings, each representing a search argument."
  (seq-reduce
   (lambda (acc argument)
     (let* ((parts
             (split-string argument "=" t))
            (arg (pop parts))
            (value (string-join parts "=")))
       (if (not value)
           acc
         (let* ((neg (string-prefix-p "--not-" arg))
                (query (if neg
                           (replace-regexp-in-string
                            "\\(^--not-\\)" ""
                            arg)
                         (replace-regexp-in-string "^--" ""
                                                   arg)))
                (separator (if neg "+-" "+")))
           (setq acc (concat acc separator query ":" value))))))
   args
   ""))

(defun gh-repo-format-query-to-args (query)
  "Convert search QUERY to argument list.

Argument QUERY is a string representing the search query to be formatted into
command-line arguments."
  (let ((result))
    (with-temp-buffer
      (insert query)
      (while (re-search-backward "[+]-?\\([^:]+\\):\\([^+]+\\)" nil t 1)
        (let ((arg (match-string-no-properties 1))
              (value (match-string-no-properties 2)))
          (setq arg (concat (if (looking-at "[+]-")
                                "--not-"
                              "--")
                            arg "="))
          (push (list arg value) result))))
    result))

(defun gh-repo-query-description ()
  "Formats a GitHub code search query based on specified arguments."
  (format "/search/repositories?q=%s"
          (gh-repo-format-args-to-query
           (gh-repo-get-args-for-query))))

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
                  (gh-repo-search-repos
                   nil
                   query))))
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

;;;###autoload
(defun gh-repo-clone-other-user-repo (name)
  "Clone other user's NAME repository."
  (interactive (list (gh-repo-ivy-read-other-user-repo
                      (gh-repo-github-user))))
  (gh-repo-clone-repo name))


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

(defun gh-repo--post (payload)
  "Send a POST request with PAYLOAD to create a repository on GitHub."
  (let ((auth (gh-repo-authenticate)))
    (ghub-post "/user/repos" nil
               :payload payload
               :auth (cdr auth)
               :username (car auth)
               :callback
               (lambda (value &rest _)
                 (if
                     (yes-or-no-p (format "Clone repo %s?"
                                          (alist-get
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
    (gh-repo--post obj)))

(defun gh-repo-get-search-query ()
  "Generate a search query string from the current transient command arguments."
  (gh-repo-format-args-to-query
   (gh-repo-get-args-for-query)))

(defvar-local gh-repo-req-buffer nil)

;;;###autoload
(defun gh-repo-export-repos (&optional public)
  "Export GitHub repository data to a buffer.

Optional argument PUBLIC is a prefix argument that determines whether to include
private repositories. If non-nil, private repositories are excluded."
  (interactive "P")
  (let* ((spec '((full_name "%s" 35 "Name")
                 (language "%s" 20 "Language")
                 (description "%s" 70 "Description")
                 (forks_count "%s" 5 "Forks")
                 (stargazers_count "%s" 5 "Stars")
                 (fork "%s" 5 "Fork")))
         (buff (get-buffer-create "gh-repo-user-repos"))
         (rendered)
         (annotf (lambda (data)
                   (concat "|"
                           (mapconcat
                            (pcase-lambda (`(,key ,format-str ,width))
                              (let ((value (alist-get key data)))
                                (concat (truncate-string-to-width
                                         (format format-str
                                                 (or
                                                  value
                                                  ""))
                                         width
                                         nil
                                         ?\s
                                         t))))
                            spec "|")
                           "|")))
         (header (lambda (cols)
                   (concat
                    "|" (mapconcat
                         (pcase-lambda (`(,_key ,_format-str ,width ,title))
                           (concat (truncate-string-to-width
                                    (or title "")
                                    width
                                    nil
                                    ?\s
                                    t)))
                         cols "|")
                    "|"
                    "\n"
                    "|" (mapconcat
                         (pcase-lambda (`(,_key ,_format-str ,width ,_title))
                           (make-string width ?-))
                         cols "+")
                    "|"))))
    (with-current-buffer buff
      (when (buffer-live-p gh-repo-req-buffer)
        (let ((message-log-max nil))
          (with-temp-message (or (current-message) "")
            (kill-buffer gh-repo-req-buffer))))
      (erase-buffer)
      (insert (funcall header spec))
      (when (and (not (get-buffer-window buff)))
        (pop-to-buffer-same-window buff))
      (setq gh-repo-req-buffer
            (gh-repo-get (concat "user/repos") nil
                         :query `((per_page . 5))
                         :callback
                         (lambda (value _headers _status req)
                           (when (buffer-live-p buff)
                             (with-current-buffer buff
                               (save-excursion
                                 (goto-char (point-max))
                                 (dolist (item value)
                                   (let ((name (alist-get 'full_name item)))
                                     (unless (member name rendered)
                                       (push name rendered)
                                       (puthash name item gh-repo-repos-hash)
                                       (unless (and public
                                                    (alist-get 'private item))
                                         (let ((line (funcall annotf item)))
                                           (insert "\n" line))))))))
                             (ghub-continue req))))))))

(defun gh-repo-format-time-diff (time)
  "Calculate and format the time difference from the current TIME.

Argument TIME is the time value that will be compared with the current time to
calculate the time difference."
  (let ((diff-secs (- (float-time (current-time))
                      (float-time time))))
    (pcase-let* ((`(,format-str . ,value)
                  (cond ((< diff-secs 60)
                         (cons "%d second" (truncate diff-secs)))
                        ((< diff-secs 3600)
                         (cons "%d minute" (truncate (/ diff-secs 60))))
                        ((< diff-secs 86400)
                         (cons "%d hour" (truncate (/ diff-secs 3600))))
                        ((< diff-secs 2592000)
                         (cons "%d day" (truncate (/ diff-secs 86400))))
                        ((< diff-secs 31536000)
                         (cons "%d month" (truncate (/ diff-secs 2592000))))
                        (t
                         (cons "%d year" (truncate (/ diff-secs 31536000))))))
                 (format-str-pl (concat format-str (if (= value 1) " ago"
                                                     "s ago"))))
      (format format-str-pl value))))

(defun gh-repo--pipe (&rest fns)
  "Compose FNS into a single composite function.
Return a function that takes a variable number of ARGS, applies
the last function in FNS to ARGS, and returns the result of
calling each remaining function on the result of the previous
function, right-to-left.  If no FNS are given, return a variadic
`identity' function."
  (declare (pure t)
           (side-effect-free error-free))
  (let* ((head (car fns))
         (tail (cdr fns)))
    (cond (tail
           (lambda (&rest args)
             (let ((acc
                    (apply head args)))
               (let ((list tail)
                     (i 0))
                 (while list
                   (let ((it
                          (car-safe
                           (prog1 list
                             (setq list
                                   (cdr list)))))
                         (it-index i))
                     (ignore it it-index)
                     (setq acc
                           (funcall it acc)))
                   (setq i
                         (1+ i))))
               acc)))
          (fns head)
          ((lambda (&optional arg &rest _) arg)))))

(defun gh-repo-list-action (action item)
  "Execute ACTION on the repository's full name from ITEM.

Argument ACTION is a function to be called with the repository name.

Argument ITEM is an alist representing the repository, where \\='full_name is
expected to be a key."
  (when-let ((name (alist-get
                    'full_name
                    item)))
    (funcall action name)))


(defvar gh-repo-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") #'gh-repo-list-next-page)
    (define-key map (kbd "K") #'gh-repo-search-stop-loading)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    map))

(define-derived-mode gh-repo-list-mode special-mode
  "Github Repository List Viewer."
  "Display a list of GitHub repositories.

Display a list of GitHub repositories in a read-only buffer with custom
keybindings for repository operations."
  (when (bound-and-true-p visual-line-mode)
    (visual-line-mode -1))
  (use-local-map gh-repo-list-mode-map))



(defun gh-repo-list-render (cols value &rest props)
  "Display a GitHub repository's file structure.

Argument COLS is a list of column configurations for rendering the repository
list.

Argument VALUE is a list of repository data objects to be rendered in the table.

Remaining arguments PROPS are additional properties to customize the rendering
of the repository list."
  (let* ((inhibit-read-only t)
         (colors)
         (columns))
    (pcase-dolist (`(,field-path . ,pl) cols)
      (let ((color (plist-get pl :color))
            (col (if (plist-get pl :getter)
                     pl
                   (gh-repo--plist-merge
                    (seq-copy pl)
                    (list :getter
                          (gh-repo-util--compose (lambda (it)
                                                   (or it ""))
                                                 (gh-repo-list-map-getter
                                                  field-path)))))))
        (push (gh-repo--plist-omit '(:color)
                                   col)
              columns)
        (push color colors)))
    (setq colors (nreverse colors))
    (setq columns (nreverse columns))
    (apply #'make-vtable
           (gh-repo--plist-merge
            (list
             :columns columns
             :column-colors colors
             :use-header-line nil
             :objects value
             :actions `("RET"
                        ,(apply-partially
                          #'gh-repo-list-action
                          #'gh-repo-tree)
                        "r"
                        ,(apply-partially
                          #'gh-repo-list-action
                          #'gh-repo-browse)
                        "C" ,(apply-partially
                              #'gh-repo-list-action
                              #'gh-repo-clone-repo)
                        "." ,(apply-partially
                              #'gh-repo-list-action
                              #'gh-repo-prompt-repo-action)
                        "v" ,(apply-partially
                              #'gh-repo-list-action
                              #'gh-repo-tree-no-select)
                        "C-j" ,(apply-partially
                                #'gh-repo-list-action
                                #'gh-repo-tree-no-select)
                        "o"
                        (lambda (item)
                          (when-let ((login (alist-get 'login
                                             (alist-get 'owner
                                              item))))
                           (gh-repo-list-user-repos
                            login)))
                        "S" ,(apply-partially
                              #'gh-repo-list-action
                              #'gh-repo-star)
                        "U" ,(apply-partially
                              #'gh-repo-list-action
                              #'gh-repo-unstar)))
            props))))

(defun gh-repo-list-revert (&rest _)
  "Refresh the GitHub repository list and update the display."
  (let ((inhibit-read-only t)
        (pos (point))
        (wnd (get-buffer-window (current-buffer)))
        (saved-wind-start))
    (when wnd
      (setq saved-wind-start (window-start
                              wnd)))
    (delete-region (point-min)
                   (point-max))
    (gh-repo-list-render gh-repo-list--columns
                         gh-repo-list--items)
    (when (and wnd
               (>= (point-max) pos))
      (set-window-point wnd pos)
      (when saved-wind-start
        (set-window-start wnd
                          saved-wind-start)))))

(defun gh-repo-search-stop-loading ()
  "Stop the GitHub repository search loading process."
  (interactive)
  (setq gh-repo-list--stop t))

(defun gh-repo--display-buffer-other-window (buffer)
  "Display BUFFER in another window, splitting if necessary.

Argument BUFFER is the buffer to be displayed in another window."
  (unless (get-buffer-window buffer)
    (gh-repo-tree--window-with-other-window
     (pop-to-buffer-same-window
      buffer))))

(defun gh-repo-list-next-page (&rest _)
  "Increment the page number and request the next page of GitHub code search."
  (interactive)
  (cond ((and
          (not gh-repo-tree--loading)
          gh-repo-list--total-count
          (> gh-repo-list--total-count
             (length gh-repo-list--items)))
         (setq gh-repo-tree--loading t)
         (gh-repo--search-list-request gh-repo-list--text
                                       gh-repo-list--query
                                       (1+ gh-repo-list--page)))))



;;;###autoload
(defun gh-repo-list-search (&optional text query)
  "Search GitHub repositories and interact with the results.

Optional argument TEXT is a string representing the text to search for in the
repository.

Optional argument QUERY is a string representing the additional search query."
  (interactive (list (read-string "Text: ")
                     (gh-repo-get-search-query)))
  (gh-repo-authenticate)
  (gh-repo--search-list-request
   text
   query
   1
   gh-repo-default-repos-limit))

(defun gh-repo--search-list-request (text query &optional page per-page)
  "Fetch and display a GitHub repository's file structure.

Argument TEXT is a string representing the text to search for in the repository.

Argument QUERY is a string representing the additional search query.

Optional argument PAGE is an integer representing the page number of the search
results.

Optional argument PER-PAGE is an integer representing the number of results per
page."
  (unless page (setq page 1))
  (let ((buff-name (concat "*gh-repo " "search/repositories" "*")))
    (cond ((when-let ((buff (get-buffer buff-name)))
             (and
              (= page (buffer-local-value 'gh-repo-list--page buff))
              (equal (buffer-local-value 'gh-repo-list--text buff) text)
              (equal (buffer-local-value 'gh-repo-list--query buff) query)
              (not (buffer-local-value 'gh-repo-tree--error-loading buff))))
           (message "gh-repo: already loading"))
          (t
           (with-current-buffer (get-buffer-create buff-name)
             (unless (derived-mode-p 'gh-repo-list-mode)
               (gh-repo-list-mode))
             (when (= page 1)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (setq gh-repo-list--items nil)
                 (setq gh-repo-list--total-count nil)))
             (setq gh-repo-list--text text
                   gh-repo-list--query query)
             (setq gh-repo-list--columns gh-repo-list-search-columns)
             (setq gh-repo-tree--loading t
                   gh-repo-tree--error-loading nil
                   gh-repo-list--page page)
             (gh-repo-list--update-header-line)
             (when (= page 1)
               (gh-repo--display-buffer-other-window (current-buffer))))
           (gh-repo-get "search/repositories" nil
                        :query
                        (gh-repo-make-query text query page
                                            (or per-page
                                                gh-repo-default-repos-limit))
                        :errorback (lambda (_err _headers status _req)
                                     (let ((err
                                            (gh-repo-tree--get-status-error
                                             status)))
                                       (when (buffer-live-p (get-buffer
                                                             buff-name))
                                         (with-current-buffer (get-buffer
                                                               buff-name)
                                           (setq gh-repo-tree--loading nil
                                                 gh-repo-tree--error-loading
                                                 (or err
                                                     "An error occured")
                                                 gh-repo-list--page
                                                 (max 1
                                                      (1- gh-repo-list--page)))
                                           (when gh-repo-list--stop
                                             (setq gh-repo-list--stop nil))
                                           (gh-repo-list--update-header-line)))
                                       (message (or err
                                                    "An error occured"))))
                        :callback
                        (lambda (response _headers _status _req)
                          (let ((value (alist-get 'items response))
                                (total (alist-get 'total_count response))
                                (buff (get-buffer buff-name)))
                            (cond ((or (not (buffer-live-p buff))
                                       (not
                                        (and (equal text (buffer-local-value
                                                          'gh-repo-list--text
                                                          buff))
                                             (equal query (buffer-local-value
                                                           'gh-repo-list--query
                                                           buff)))))
                                   nil)
                                  (t
                                   (with-current-buffer buff
                                     (setq gh-repo-list--page page
                                           gh-repo-list--total-count
                                           (or gh-repo-list--total-count
                                               total))
                                     (setq gh-repo-list--items (nconc gh-repo-list--items value)
                                           gh-repo-list--columns gh-repo-list-search-columns)
                                     (setq gh-repo-tree--loading (and (not gh-repo-list--stop)
                                                                      (> total (length gh-repo-list--items))))
                                     (gh-repo-list--update-header-line)
                                     (gh-repo-list-revert)
                                     (when gh-repo-list--stop
                                       (setq gh-repo-list--stop nil))
                                     (when gh-repo-tree--loading
                                       (gh-repo-debounce
                                        'gh-repo-list--timer
                                        2
                                        #'gh-repo--search-list-request
                                        text
                                        query
                                        (1+ (or page 1))
                                        (or per-page
                                            gh-repo-default-repos-limit)))))))))))))



(defun gh-repo--list-repos (url page cols &optional value-transformer)
  "List GitHub repositories and render them interactively.

Argument URL is the GitHub API endpoint to query.

Argument PAGE is an integer representing the page number of the search results.

Argument COLS is a list of column configurations for displaying repository data.

Optional argument VALUE-TRANSFORMER is a function that transforms the data
returned from the GitHub API before it is displayed."
  (unless page (setq page 1))
  (let* ((buff-name (concat "*gh-repo "
                            url
                            "*"))
         (buff (get-buffer-create buff-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'gh-repo-list-mode)
        (gh-repo-list-mode))
      (when (= page 1)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq gh-repo-list--items nil)
          (setq gh-repo-list--total-count nil)))
      (setq gh-repo-list--columns cols)
      (setq gh-repo-tree--loading t
            gh-repo-tree--error-loading nil
            gh-repo-list--total-count nil
            gh-repo-list--page page)
      (gh-repo-list--update-header-line)
      (when (= page 1)
        (gh-repo--display-buffer-other-window (current-buffer))))
    (gh-repo-get url nil
                 :query `((page . ,page)
                          (per_page . ,gh-repo-default-repos-limit))
                 :errorback (lambda (_err _headers status _req)
                              (let ((err
                                     (gh-repo-tree--get-status-error
                                      status)))
                                (when (buffer-live-p buff)
                                  (with-current-buffer buff
                                    (when gh-repo-list--stop
                                      (setq gh-repo-list--stop nil))
                                    (setq gh-repo-tree--loading nil
                                          gh-repo-list--page (max 1
                                                                  (1- gh-repo-list--page))
                                          gh-repo-tree--error-loading
                                          (or err
                                              "An error occured"))
                                    (gh-repo-list--update-header-line)))
                                (message (or err
                                             "An error occured"))))
                 :callback
                 (lambda (value _headers _status _req)
                   (when (buffer-live-p buff)
                     (when value-transformer
                       (setq value (funcall value-transformer value)))
                     (with-current-buffer buff
                       (setq gh-repo-list--page page
                             gh-repo-list--total-count
                             nil)
                       (setq gh-repo-list--items (nconc gh-repo-list--items value)
                             gh-repo-list--columns cols)
                       (setq gh-repo-tree--loading (and
                                                    value
                                                    (not gh-repo-list--stop)
                                                    (= gh-repo-default-repos-limit
                                                       (length value))))
                       (gh-repo-list--update-header-line)
                       (gh-repo-list-revert)
                       (when gh-repo-list--stop
                         (setq gh-repo-list--stop nil))
                       (when gh-repo-tree--loading
                         (gh-repo-debounce
                          'gh-repo-list--timer
                          1
                          #'gh-repo--list-repos
                          url
                          (1+ (or page 1))
                          cols
                          value-transformer))))))))

;;;###autoload
(defun gh-repo-list-user-repos (user)
  "List USER repositories in a formatted table."
  (interactive (list (gh-repo-github-user)))
  (gh-repo-authenticate)
  (gh-repo--list-repos (format "/users/%s/repos" user)
                       1
                       gh-repo-list-user-repo-columns))

;;;###autoload
(defun gh-repo-list-repos ()
  "List GitHub user repositories interactively."
  (interactive)
  (gh-repo-authenticate)
  (gh-repo--list-repos "/user/repos"
                       1
                       gh-repo-list-user-repo-columns))

;;;###autoload
(defun gh-repo-list-starred-repos ()
  "List starred GitHub user repositories interactively."
  (interactive)
  (gh-repo-authenticate)
  (gh-repo--list-repos "/user/starred"
                       1
                       gh-repo-list-user-repo-columns))

(defun gh-repo-unstar (owner/repo)
  "Unstar a GitHub repository specified by OWNER/REPO."
  (interactive (list (or (gh-repo-tree--current-buffer-repo)
                         (gh-repo-search-repos)
                         (read-string "User and repository (user/repo): "))))
  (gh-repo--delete (concat "/user/starred/" owner/repo)
                   nil
                   :errorback (lambda (_err _headers status _req)
                                (let* ((prompt (format "Couldn't unstar %s: "
                                                       owner/repo))
                                       (err
                                        (concat prompt " " (or (gh-repo-tree--get-status-error
                                                                status)
                                                               ""))))
                                  (message err)))
                   :callback (lambda (&rest _args)
                               (message "Unstarred %s" owner/repo))))

(defun gh-repo-star (owner/repo)
  "Star a GitHub repository specified by OWNER/REPO.

Argument OWNER/REPO is a string in the format \"user/repo\"."
  (interactive (list (or (gh-repo-tree--current-buffer-repo)
                         (gh-repo-search-repos)
                         (read-string "User and repository (user/repo): "))))
  (gh-repo--put (concat "/user/starred/" owner/repo) nil
                :errorback (lambda (_err _headers status _req)
                             (let* ((prompt (format "Couldn't star %s: "
                                                    owner/repo))
                                    (err
                                     (concat prompt " " (or (gh-repo-tree--get-status-error
                                                             status)
                                                            ""))))
                               (message err)))
                :callback (lambda (&rest _args)
                            (message "Starred %s" owner/repo))))

(defun gh-repo-list-map-getter (getter)
  "Create a GETTER function for accessing repository data.

Argument GETTER is a function, symbol, string, or list that specifies how to
extract data from a repository object."
  (pcase getter
    ((pred (symbolp))
     (lambda (obj &rest _)
       (cdr (assq getter obj))))
    ((pred (stringp))
     (lambda (obj &rest _)
       (cdr (assoc-string getter obj))))
    ((pred (listp))
     (apply #'gh-repo--pipe
            (mapcar #'gh-repo-list-map-getter getter)))))




;;;###autoload (autoload 'gh-repo-menu "gh-repo" nil t)
(transient-define-prefix gh-repo-menu ()
  "Command dispatcher for GitHub repositories."
  :value
  (lambda ()
    gh-repo-default-arguments)
  [["New repository arguments"
    ("n" "name" "--name=" :unsavable t)
    ("d" "description" "--description="
     :class transient-option
     :unsavable t)
    ("p" "private" "--private" :unsavable t)
    ("t" "license_template" "--license_template="
     :class transient-option
     :choices gh-repo--license-choices)
    ("g" "gitignore_template" "--gitignore_template="
     :class transient-option
     :always-read t
     :choices gh-repo--gitignore-template-choices)
    ("T" "template repository" "--is_template")
    ("i" "has_issues" "--has_issues")
    ("P" "projects" "--has_projects")
    ("w" "wiki" "--has_wiki")
    ("L" "downloads" "--has_downloads")
    ("a" "auto readme" "--auto_init")
    ("S" "discussions" "--has_discussions")]
   ["List"
    ("l l" "list my repos" gh-repo-list-repos)
    ("l s" "list starred repos" gh-repo-list-starred-repos)
    ("l o" "list other user repos" gh-repo-list-user-repos)
    "Actions"
    ("s" "Search repos" gh-repo-search-menu)
    ("o" "Clone other user repo" gh-repo-clone-other-user-repo)
    ("c" "Clone my repo" gh-repo-clone-repo)
    ("R" "Remove my repo" gh-repo-delete)]
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
                                'face 'transient-value)))))]]
  [["Create"
    ("RET"  gh-repo-create-repo
     :description
     (lambda ()
       (let* ((arg "--name=")
              (name (seq-find
                     (apply-partially #'string-prefix-p arg)
                     (gh-repo-get-arguments))))
         (if name
             (concat "Create repo "
                     (propertize (substring-no-properties
                                  name
                                  (length
                                   arg))
                                 'face 'transient-value))
           (propertize "Create repo " 'face 'transient-inapt-suffix)))))]]
  (interactive)
  (gh-repo-authenticate)
  (transient-setup #'gh-repo-menu))

(defvar gh-repo-tree-file-buffer-name-prefix "*gh-repo-tree*: "
  "Prefix for buffer names used in GitHub code search results.")

(defvar gh-repo-tree--paths-hash (make-hash-table :test 'equal)
  "Hash table mapping code search paths to their results.")


(defun gh-repo--minibuffer-action-no-exit (action)
  "Invoke ACTION on current minibuffer candidate without closing.

Argument ACTION is a function to be called with the current minibuffer candidate
as its argument."
  (pcase-let ((`(,_category . ,current)
               (gh-repo-minibuffer-get-current-candidate)))
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (minimize-window win)
      (add-hook 'post-command-hook
                #'gh-repo--minibuffer-restore-completions-window))
    (with-minibuffer-selected-window
      (funcall action current))))

(defun gh-repo-tree--minibuffer-preview-file ()
  "Preview a GitHub repo file without exiting minibuffer."
  (interactive)
  (gh-repo--minibuffer-action-no-exit
   #'gh-repo-tree--preview-repo-file-action))

(defvar gh-repo-tree-completion-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                #'gh-repo-tree--minibuffer-preview-file)
    map)
  "Keymap for minibuffer file search with preview function.")

(defun gh-repo-tree--debounce-run-in-buffer (buffer timer-sym fn &rest args)
  "Execute function FN with ARGS in BUFFER if it's live and visible.

Argument BUFFER is the buffer in which to run the function FN.

Argument TIMER-SYM is a symbol whose value is expected to be a timer object.

Argument FN is the function to be applied in the buffer.

Remaining arguments ARGS are the arguments to be passed to the function FN."
  (gh-repo--with-live-buffer buffer
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

(defun gh-repo-tree--debounce (timer-sym delay fn &rest args)
  "DELAY execution of FN with ARGS after delay, canceling previous timer.

Argument TIMER-SYM is a symbol whose value is the timer object to be potentially
canceled and reset.

Argument DELAY is a number representing the time, in seconds, to wait before
executing FN.

Argument FN is the function to be called after the delay.

Remaining arguments ARGS are passed to FN when it is called."
  (when-let ((timer-value (symbol-value timer-sym)))
    (when (timerp timer-value)
      (cancel-timer timer-value)))
  (set timer-sym (apply #'run-with-timer delay nil
                        #'gh-repo-tree--debounce-run-in-buffer
                        (current-buffer)
                        timer-sym
                        fn
                        args)))

(defun gh-repo-tree-completing-read-with-keymap (prompt collection &optional
                                                        keymap setup-fn
                                                        predicate require-match
                                                        initial-input hist def
                                                        inherit-input-method)
  "PROMPT for input with completion, optional KEYMAP, and setup function.

Argument PROMPT is a string to prompt the user.

Argument COLLECTION is a list of strings or an alist from which the user can
choose.

Optional argument KEYMAP is a keymap to use while reading from the minibuffer.

Optional argument SETUP-FN is a function to call before reading from the
minibuffer.

Optional argument PREDICATE is a function to filter the choices in COLLECTION.

Optional argument REQUIRE-MATCH is a boolean; if non-nil, the user is required
to select an existing entry in COLLECTION.

Optional argument INITIAL-INPUT is a string to prefill the minibuffer with.

Optional argument HIST is a symbol representing a minibuffer history list.

Optional argument DEF is the default value to return if the user enters an empty
string.

Optional argument INHERIT-INPUT-METHOD is a boolean; if non-nil, the minibuffer
inherits the current input method."
  (let ((collection (if (stringp (car-safe collection))
                        (copy-tree collection)
                      collection)))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (when keymap
              (let ((map (make-composed-keymap keymap
                                               (current-local-map))))
                (use-local-map map)))
            (when setup-fn
              (funcall setup-fn))))
      (completing-read prompt
                       collection
                       predicate
                       require-match initial-input hist
                       def inherit-input-method))))

(defvar gh-repo-tree-minibuffer-timer nil)

(defun gh-repo-tree-find-other-file (repo)
  "Switch to a file from a GitHub repo.

Argument REPO is a string representing the name of the GitHub repository."
  (interactive (list (or (gh-repo-tree--current-buffer-repo)
                         (gh-repo-search-repos)
                         (read-string "User and repository (user/repo): "))))
  (let ((complete-fn (lambda (tree)
                       (let* ((alist   (mapcar
                                        (lambda (x)
                                          (cons (cdr (assq 'path x))
                                                (cdr (assq 'url x))))
                                        tree))
                              (path
                               (gh-repo-tree-completing-read-with-keymap
                                repo
                                (mapcar
                                 #'car
                                 alist)
                                gh-repo-tree-completion-file-map
                                (lambda
                                  ()
                                  (add-hook
                                   'after-change-functions
                                   (lambda
                                     (&rest
                                      _)
                                     (gh-repo-tree--debounce
                                      'gh-repo-tree-minibuffer-timer
                                      1
                                      'gh-repo-tree--minibuffer-preview-file))
                                   nil
                                   t))))
                              (url (cdr (assoc-string path
                                                      alist))))
                         (gh-repo-tree--download-repo-path
                          repo
                          path
                          url)))))
    (if (gh-repo-tree--paths-get repo)
        (funcall complete-fn (gh-repo-tree--paths-get repo))
      (gh-repo-tree--fetch-repo-tree
       repo
       complete-fn))))

(defun gh-repo-tree--highlight-matches (str)
  "Highlight matches of a given string in GitHub code search.

Argument STR is a string that represents the pattern to be highlighted in the
code."
  (let ((case-fold-search t)
        (re (regexp-quote str)))
    (save-excursion
      (goto-char (point-min))
      (with-silent-modifications (while (re-search-forward re nil t 1)
                                   (add-face-text-property
                                    (match-beginning 0)
                                    (match-end 0)
                                    'highlight))))))

(defun gh-repo-tree-act-on-repo (repo)
  "Prompt user to perform an action on a GitHub repository.

Argument REPO is a string representing the GitHub repository."
  (interactive (list (or (gh-repo-tree--current-buffer-repo)
                         (gh-repo-search-repos)
                         (read-string "User and repository (user/repo): "))))
  (gh-repo-prompt-repo-action repo))

(define-minor-mode gh-repo-tree-file-mode
  "Minor mode for displaying remote github files."
  :lighter " gh-repo-tree-file"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'gh-repo-tree-find-other-file)
    (define-key map (kbd "C-c C-l") #'gh-repo-tree-clone-repo)
    (define-key map (kbd "C-x d") #'gh-repo-tree)
    (define-key map (kbd "C-.") #'gh-repo-tree-act-on-repo)
    (define-key map (kbd "q") #'quit-window)
    map)
  (setq buffer-read-only t))

(defun gh-repo-tree-clone-repo (repo)
  "Clone a GitHub repository.

Argument REPO is the repository to be cloned."
  (interactive (list (or (gh-repo-tree--current-buffer-repo)
                         (gh-repo-search-repos)
                         (read-string "User and repository (user/repo): "))))
  (gh-repo-clone-repo repo))

(defun gh-repo-tree--set-major-mode (filename)
  "Set buffer's major mode based on FILENAME and ensure font-lock.

Argument FILENAME is a string representing the name of the file for which to set
the major mode."
  (let ((buffer-file-name (or
                           (if (file-name-absolute-p filename)
                               filename
                             (expand-file-name filename default-directory)))))
    (ignore-errors
      (set-auto-mode)
      (font-lock-ensure))))

(defun gh-repo--json-reader (&optional object-type array-type null-object
                                       false-object)
  "Parse JSON from GitHub response.

Optional argument OBJECT-TYPE specifies which Lisp type is used to represent
objects; it can be `hash-table', `alist' or `plist'. It defaults to `alist'.

Optional argument ARRAY-TYPE specifies which Lisp type is used to represent
arrays; it can be `list' or `vector'. It defaults to `vector'.

Optional argument NULL-OBJECT specifies which object to use to represent a JSON
null value. It defaults to `:null'.

Optional argument FALSE-OBJECT specifies which object to use to represent a JSON
false value. It defaults to `:false'."
  (let ((raw (ghub--decode-payload)))
    (and raw
         (condition-case nil
             (gh-repo--json-parse-string raw
                                         object-type
                                         array-type
                                         null-object
                                         false-object)
           ((json-parse-error json-readtable-error)
            `((message
               . ,(if (looking-at "<!DOCTYPE html>")
                      (if (re-search-forward
                           "<p>\\(?:<strong>\\)?\\([^<]+\\)"
                           nil t)
                          (match-string 1)
                        "error description missing")
                    (string-trim (buffer-substring
                                  (point)
                                  (point-max)))))
              (documentation_url . "https://github.com/magit/ghub/wiki/Github-Errors")))))))

(defun gh-repo-tree--download-repo-path (repo-name path url &optional
                                                   search-str)
  "Download and display a file from a GitHub repository.

Argument REPO-NAME is a string representing the name of the repository.

Argument PATH is a string representing the path within the repository.

Argument URL is a string representing the url to retrieve the file content.

Optional argument SEARCH-STR is a string used to highlight matches in the
retrieved file content."
  (let ((buff-name (concat gh-repo-tree-file-buffer-name-prefix
                           repo-name "/"
                           path)))
    (if (get-buffer buff-name)
        (unless (get-buffer-window buff-name)
          (pop-to-buffer-same-window buff-name))
      (with-current-buffer (get-buffer-create buff-name)
        (setq header-line-format
              (list (concat (propertize " " 'display
                                        '(space :align-to 0))
                            (format "%s " (buffer-name)))
                    (propertize " Loading" 'face 'warning)))
        (pop-to-buffer-same-window buff-name))
      (gh-repo-get
       (url-filename
        (url-generic-parse-url
         url))
       nil
       :errorback (lambda (_err _headers status _req)
                    (when-let ((err
                                (gh-repo-tree--get-status-error status))
                               (buff (get-buffer buff-name)))
                      (with-current-buffer buff
                        (setq header-line-format
                              (list (car header-line-format)
                                    (propertize " Error " 'face 'error)
                                    err)))))
       :reader
       (lambda (&rest _)
         (gh-repo--json-reader))
       :callback
       (lambda (data &rest _)
         (let* ((code (decode-coding-string
                       (base64-decode-string
                        (alist-get
                         'content
                         data))
                       'utf-8))
                (buff (get-buffer buff-name)))
           (when (buffer-live-p buff)
             (with-current-buffer buff
               (erase-buffer)
               (setq buffer-read-only nil)
               (progn
                 (save-excursion
                   (insert code))
                 (setq buffer-file-name
                       (expand-file-name
                        (substring-no-properties
                         buff-name
                         (length
                          gh-repo-tree-file-buffer-name-prefix))
                        default-directory))
                 (gh-repo-tree--set-major-mode
                  buffer-file-name))
               (setq-local header-line-format
                           (list " " 'header-line-indent
                                 (buffer-name)
                                 (propertize " Ready " 'face 'success)))
               (unless (symbol-value
                        'gh-repo-tree-file-mode)
                 (gh-repo-tree-file-mode))
               (setq buffer-undo-list nil)
               (set-buffer-modified-p nil)
               (goto-char (point-min))
               (when search-str
                 (gh-repo-tree--highlight-matches
                  search-str)
                 (unless (get-buffer-window buff)
                   (pop-to-buffer-same-window buff))
                 (let ((wnd (get-buffer-window buff)))
                   (with-selected-window wnd
                     (when-let ((found (re-search-forward
                                        (regexp-quote
                                         search-str)
                                        nil t 1)))
                       (set-window-point wnd found)
                       found))))))))))))

(defun gh-repo-tree--current-repo-path ()
  "Return cons with repository name and file path."
  (when-let* ((buff-name (buffer-name))
              (segments
               (when (string-prefix-p
                      gh-repo-tree-file-buffer-name-prefix
                      buff-name)
                 (split-string (substring-no-properties
                                buff-name
                                (length gh-repo-tree-file-buffer-name-prefix))
                               "/"
                               t))))
    (cons (string-join (seq-take segments 2) "/")
          (string-join (seq-drop segments 2) "/"))))

(defun gh-repo-tree--paths-put (repo paths)
  "Store PATHS in a hash table with REPO as the key.

Argument REPO is the repository identifier for which PATHS are being stored.

Argument PATHS is a list of directory paths associated with the REPO."
  (puthash repo paths gh-repo-tree--paths-hash))

(defun gh-repo-tree--get-status-error (status)
  "Display error details from GitHub code search status.

Argument STATUS is a plist containing the status information, including any
error details."
  (when-let ((err (plist-get status :error)))
    (concat (propertize
             "gh-repo error: "
             'face
             'error)
            (mapconcat (apply-partially #'format "%s")
                       (delq nil
                             (list (or
                                    (when-let ((type
                                                (ignore-errors
                                                  (cadr
                                                   err))))
                                      type)
                                    err)
                                   (ignore-errors (caddr
                                                   err))
                                   (ignore-errors
                                     (alist-get 'message
                                                (car-safe
                                                 (last
                                                  err))))
                                   (ignore-errors
                                     (alist-get 'documentation_url
                                                (car-safe
                                                 (last
                                                  err))))))
                       " "))))

(defun gh-repo--get-other-wind ()
  "Return another window or split sensibly if needed."
  (let ((wind-target
         (if (minibuffer-selected-window)
             (with-minibuffer-selected-window
               (let ((wind (selected-window)))
                 (or
                  (window-right wind)
                  (window-left wind)
                  (split-window-sensibly)
                  wind)))
           (let ((wind (selected-window)))
             (or
              (window-right wind)
              (window-left wind)
              (split-window-sensibly)
              wind)))))
    wind-target))


(defun gh-repo-tree--fetch-repo-tree (repo &optional callback on-error)
  "Fetch GitHub REPO's file structure as a tree.

Argument REPO is a string representing the GitHub repository in the format
\"owner/repo\".

Optional argument CALLBACK is a function to be called with the result of the
fetch operation.

Optional argument ON-ERROR is a function to be called if an error occurs during
the fetch operation."
  (gh-repo-get (format "repos/%s/git/trees/HEAD:?recursive=1" repo)
               nil
               :reader
               (lambda (&rest _)
                 (gh-repo--json-reader
                  'alist
                  'list))
               :callback (lambda (response &rest _)
                           (let* ((tree (cdr (assq 'tree response)))
                                  (value (gh-repo-tree--paths-put
                                          repo
                                          (seq-filter
                                           (lambda (x)
                                             (when (equal
                                                    (cdr
                                                     (assq
                                                      'type
                                                      x))
                                                    "blob")
                                               (cdr (assoc 'path x))))
                                           tree))))
                             (if callback
                                 (funcall callback value)
                               tree)))
               :errorback (lambda (_err _headers status _req)
                            (if-let ((err
                                      (gh-repo-tree--get-status-error status)))
                                (funcall (or on-error #'message) err)))))

(defun gh-repo-tree--paths-get (repo)
  "Retrieve stored paths for a given REPO.

Argument REPO is the repository name for which to retrieve the code search
paths."
  (gethash repo gh-repo-tree--paths-hash))

(defun gh-repo-tree--current-buffer-repo ()
  "Search GitHub code in the current buffer's repository."
  (car (gh-repo-tree--current-repo-path)))

(defun gh-repo-tree--preview-repo-file-action (path)
  "Preview and download a file from a GitHub repository.

Argument PATH is a string representing the path within the repository to
preview."
  (let* ((repo (gh-repo-tree--current-buffer-repo))
         (tree (gh-repo-tree--paths-get repo))
         (url (cdr (assq 'url (seq-find (lambda (it)
                                          (equal path (cdr (assq 'path it))))
                                        tree)))))
    (gh-repo-tree--download-repo-path
     repo
     path
     url)))

(defun gh-repo-tree--group-files (files)
  "Group FILES by their directory structure."
  (let ((tree (gh-repo-tree--group-files-1
               (mapcar (lambda (it)
                         (alist-get 'path it))
                       files))))
    tree))

(defun gh-repo-tree--group-files-1 (files)
  "Group FILES into a tree structure based on their paths.

Argument FILES is a list of file paths to be grouped."
  (cl-labels ((insert (tree parts)
                (let ((node (assoc (car parts) tree)))
                  (if (cdr parts)
                      (progn
                        ;; If the node for this part doesn't exist, create it.
                        (unless node
                          (setq node (list (car parts)))
                          (setq tree (append tree (list node))))
                        ;; Insert the remaining parts into the tree
                        (setcdr node (insert (cdr node)
                                             (cdr parts))))
                    ;; If this is the last part, simply append the file.
                    (unless (member (car parts) tree)
                      (setq tree (append tree (list (car parts)))))))
                tree))
    (let ((tree '()))
      ;; Process each file path
      (dolist (file files tree)
        (setq tree (insert tree (split-string file "/" t))))
      ;; Return the completed tree
      tree)))

(defun gh-repo-tree--concat-dir-file (file &optional dirname)
  "Concatenate FILE with DIRNAME, if provided, separated by \"/\".

Argument FILE is the name of the file to expand.

Optional argument DIRNAME is the directory name to append to FILE; if nil, file
is returned unchanged."
  (if dirname
      (concat dirname "/" file)
    file))

(defvar-local gh-repo-tree--opened-dirs nil)

(defun gh-repo-tree--open-p (dir)
  "Check if DIR is in `gh-repo-tree--opened-dirs'.

Argument DIR is the directory to check if it has been opened in the GitHub code
search."
  (member dir gh-repo-tree--opened-dirs))

(defun gh-repo-tree--toggle-is-open (dir)
  "Toggle if DIR is in `gh-repo-tree--opened-dirs'.

Argument DIR is the directory to check if it has been opened in the GitHub code
search."
  (if (gh-repo-tree--open-p dir)
      (setq gh-repo-tree--opened-dirs
            (delete dir gh-repo-tree--opened-dirs))
    (push dir gh-repo-tree--opened-dirs)))

(defun gh-repo-tree--insert-directory-entry (row parent &rest props)
  "Insert ROW with optional PADDING into a buffer and add text PROPS.

Argument ROW is the string to be inserted as a row in the search results.

Argument PARENT is the string representing the parent path of ROW; it influences
the padding of the inserted row."
  (let ((inhibit-read-only t)
        (beg (point))
        (padding (if parent
                     (make-string (+ 2
                                     (length
                                      (split-string parent "/" t)))
                                  ?\ )
                   " ")))
    (insert padding)
    (insert row)
    (add-text-properties beg (point)
                         (append (list 'parent parent
                                       'child row)
                                 props))))

(defun gh-repo-tree--parent-of (parent value)
  "Check if VALUE is a child of PARENT directory.

Argument PARENT is a string representing the parent path to match against.

Argument VALUE is a string representing the value to be checked if it is a child
of PARENT."
  (and parent value
       (or (string= parent value)
           (string-prefix-p (concat parent "/")
                            value))))

(defun gh-repo-tree--toggle-row-at-point ()
  "Toggle visibility of a directory tree row."
  (when (get-text-property (point) 'children)
    (pcase-let* ((`(,beg . ,end)
                  (gh-repo-tree--property-boundaries
                   'id))
                 (id (and beg (get-text-property beg 'id)))
                 (parent (get-text-property beg 'parent))
                 (child (get-text-property beg 'child))
                 (children (and id
                                (get-text-property beg 'children))))
      (when children
        (forward-line 1)
        (while
            (when-let ((parent-id (get-text-property (point) 'parent)))
              (when (gh-repo-tree--parent-of id
                                             parent-id)
                (zerop (forward-line 1)))))
        (setq end (point)))
      (let ((inhibit-read-only t)
            (opened))
        (delete-region beg end)
        (gh-repo-tree--toggle-is-open id)
        (setq opened (gh-repo-tree--open-p id))
        (gh-repo-tree--insert-expandable-row
         child
         parent
         opened
         'id id
         'children children)
        (if opened
            (progn
              (gh-repo-tree--render-tree children
                                         id)
              (insert ?\n))
          (insert ?\n))
        (goto-char beg)))))

(defun gh-repo-tree--property-boundaries (prop &optional pos)
  "Return property boundaries for PROP at POS."
  (if pos
      (goto-char pos)
    (setq pos (point)))
  (let ((position
         (cond ((and (bolp)
                     (not (eobp)))
                (1+ (line-beginning-position)))
               ((and (eolp)
                     (not (bobp)))
                (1- (line-end-position)))
               (t (point)))))
    (when-let ((value (get-text-property position prop)))
      (when-let ((beg (or (previous-single-property-change position prop)
                          (point-min)))
                 (end (or (next-single-property-change position prop)
                          (point-max))))
        (cons beg (if (equal value (get-text-property end prop))
                      end
                    (1- end)))))))

(defun gh-repo-tree--insert-expandable-row (row parent opened &rest props)
  "Insert expandable ROW with button in buffer.

Argument ROW is a string representing the row to be inserted.

Argument PARENT is a string representing the parent of the ROW, or nil if there
is no parent.

Argument OPENED is a boolean indicating whether the ROW is initially expanded
\(t) or collapsed (nil).

Remaining arguments PROPS are additional properties to be set on the inserted
row."
  (let ((inhibit-read-only t)
        (beg (point))
        (indicator (if opened "- " "+ "))
        (padding (if parent
                     (make-string (length
                                   (split-string parent "/" t))
                                  ?\ )
                   "")))
    (insert padding indicator)
    (insert-text-button row 'action 'gh-repo-tree-toggle-row)
    (add-text-properties beg (point)
                         (append (list 'parent parent
                                       'child row)
                                 props))))

(defun gh-repo-tree--render-tree (tree &optional parent)
  "Display a hierarchical TREE of directories and files.

Argument TREE is a list representing the directory structure to print.

Optional argument PARENT is a string representing the parent directory path."
  (let ((inhibit-read-only t)
        (children))
    (while tree
      (let ((entry (car tree)))
        (cond ((stringp entry)
               (push entry children))
              ((listp entry)
               (let* ((dirname (gh-repo-tree--concat-dir-file
                                (car entry)
                                parent))
                      (opened (gh-repo-tree--open-p
                               dirname))
                      (subtree (cdr entry)))
                 (unless (bobp)
                   (insert ?\n))
                 (gh-repo-tree--insert-expandable-row (car entry)
                                                      parent
                                                      opened
                                                      'id dirname
                                                      'children
                                                      subtree)
                 (when opened
                   (gh-repo-tree--render-tree subtree
                                              dirname))))))
      (setq tree (cdr tree)))
    (while children
      (unless (bobp)
        (insert ?\n))
      (let ((child (car children)))
        (gh-repo-tree--insert-directory-entry child
                                              parent
                                              'id (gh-repo-tree--concat-dir-file
                                                   child
                                                   parent)))
      (setq children (cdr children)))))

(defvar gh-repo-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") #'gh-repo-tree-toggle-row)
    (define-key map (kbd "<tab>") #'gh-repo-tree-toggle-row)
    (define-key map (kbd "r") #'gh-repo-tree-browse-repo)
    (define-key map (kbd "-") #'gh-repo-tree-toggle-row)
    (define-key map (kbd "C") #'gh-repo-tree-clone-repo)
    (define-key map (kbd "S") #'gh-repo-star)
    (define-key map (kbd "U") #'gh-repo-unstar)
    (define-key map (kbd "f") #'gh-repo-tree-find-other-file)
    (define-key map (kbd ".") #'gh-repo-tree-act-on-repo)
    (define-key map (kbd "C-j") #'gh-repo-tree--view-file-no-select)
    (define-key map (kbd "RET") #'gh-repo-tree--visit-or-expand)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    map))

(dolist (sym '(gh-repo-tree--visit-or-expand
               gh-repo-tree-toggle-rowp))
  (function-put sym 'command-modes '(gh-repo-tree-mode)))

(defun gh-repo-tree-browse-repo (repo)
  "Open the current buffer's GitHub repository in a web browser.

Argument REPO is a string representing the GitHub repository to browse."
  (interactive (list (or (gh-repo-tree--current-buffer-repo)
                         (gh-repo-search-repos)
                         (read-string "User and repository (user/repo): "))))
  (gh-repo-browse repo))

(defun gh-repo-tree-toggle-row (&rest _)
  "Toggle visibility of a directory row in a tree view."
  (interactive)
  (gh-repo-tree--toggle-row-at-point))

(defun gh-repo-tree--visit-or-expand (&rest _)
  "Toggle or preview GitHub repo file at point."
  (interactive)
  (if (get-text-property (point) 'children)
      (gh-repo-tree--toggle-row-at-point)
    (when-let ((id (get-text-property (point) 'id)))
      (gh-repo-tree--preview-repo-file-action id))))

(defun gh-repo-tree--view-file-no-select (&rest _)
  "Toggle or preview GitHub repo file at point in other window."
  (interactive)
  (if (get-text-property (point) 'children)
      (gh-repo-tree--toggle-row-at-point)
    (when-let* ((id (get-text-property (point) 'id))
                (repo (gh-repo-tree--current-buffer-repo))
                (tree (gh-repo-tree--paths-get repo))
                (url (cdr (assq 'url (seq-find
                                      (lambda (it)
                                        (equal id (cdr (assq 'path it))))
                                      tree)))))
      (gh-repo-tree--window-with-other-window
       (gh-repo-tree--download-repo-path
        repo
        id
        url)))))

(defvar-local gh-repo-tree--old-header-line nil)

(defconst gh-repo--loading-label (propertize "Loading" 'face 'warning))
(defconst gh-repo--error-label (propertize "Error" 'face 'error))
(defconst gh-repo--ready-label (propertize "Ready" 'face 'success))

(defun gh-repo--get-status-line ()
  "Generate a status line based on the repository's loading state."
  (cond (gh-repo-tree--loading
         gh-repo--loading-label)
        ((stringp gh-repo-tree--error-loading)
         (concat gh-repo--error-label
                 " "
                 (truncate-string-to-width
                  (string-join
                   (split-string
                    gh-repo-tree--error-loading
                    nil t)
                   " ")
                  70 nil nil t)))
        (gh-repo-tree--error-loading
         gh-repo--error-label)
        (t gh-repo--ready-label)))

(defun gh-repo--get-page-indicator ()
  "Display a formatted page indicator with current page and total repositories."
  (when gh-repo-list--total-count
    (propertize (format (concat " Page %s (%s/%s) ")
                        (or gh-repo-list--page 1)
                        (length gh-repo-list--items)
                        gh-repo-list--total-count)
                'face
                'font-lock-number-face)))

(defun gh-repo-tree--update-header-line ()
  "Update header line for Github repository tree viewing.

Optional argument HIDDEN-COUNT is an integer representing the number of search
results that are not displayed."
  (setq header-line-format
        (list (concat (propertize " " 'display '(space :align-to 0))
                      (format "%s " (buffer-name)))
              (gh-repo--get-status-line))))

(defun gh-repo-list--update-header-line ()
  "Update header line with GitHub code list info.

Optional argument HIDDEN-COUNT is an integer representing the number of search
results that are not displayed."
  (setq header-line-format
        (list (concat (propertize " " 'display '(space :align-to 0))
                      (format "%s " (buffer-name)))
              (concat (gh-repo--get-status-line)
                      (gh-repo--get-page-indicator)))))

(defvar-local gh-repo-tree--request-buffer nil)

(defun gh-repo-tree--abort-url-retrieve (buff)
  "Cancel the URL retrieval process and kill the associated buffer.

Argument BUFF is a buffer object that represents the buffer to be checked and
potentially killed."
  (when (buffer-live-p buff)
    (message "gh-repo-tree aborting request")
    (let ((proc (get-buffer-process buff)))
      (when proc
        (delete-process proc))
      (kill-buffer buff))))

(defun gh-repo--find-readme (files)
  "Find and return the README file from a list of FILES.

Argument FILES is a list of file entries where each entry is an alist containing
file properties."
  (seq-find
   (lambda (it)
     (string= "readme"
              (downcase (file-name-sans-extension
                         (or
                          (cdr
                           (assq
                            'path
                            it))
                          "")))))
   files))

(defun gh-repo-tree--fetch-readme (readme-cell &optional on-success on-error)
  "Fetch and decode a repository's README.

Argument README-CELL is an association list containing repository README
information.

Optional argument ON-SUCCESS is a function called with the README content when
the fetch is successful.

Optional argument ON-ERROR is a function called with the error information when
the fetch fails."
  (gh-repo-get
   (url-filename
    (url-generic-parse-url
     (cdr (assq 'url readme-cell))))
   nil
   :errorback (lambda (_err _headers status _req)
                (let ((err
                       (gh-repo-tree--get-status-error status)))
                  (funcall on-error err)))
   :reader
   (lambda (&rest _)
     (gh-repo--json-reader))
   :callback
   (lambda (data &rest _)
     (let ((code))
       (condition-case err
           (setq code (with-temp-buffer
                        (insert (decode-coding-string
                                 (base64-decode-string
                                  (alist-get
                                   'content
                                   data))
                                 'utf-8))
                        (gh-repo-tree--set-major-mode
                         (cdr (assq 'path readme-cell)))
                        (buffer-string)))
         (error (funcall on-error err)))
       (when code
         (funcall on-success
                  code))))))


(defun gh-repo-tree--handle-error (buffer err)
  "Handle errors by updating BUFFER's state and header.

Argument BUFFER is the buffer where the error handling will take place.

Argument ERR is the error to be handled."
  (gh-repo--with-live-buffer buffer
    (setq gh-repo-tree--error-loading err)
    (setq gh-repo-tree--loading nil)
    (gh-repo-tree--update-header-line)))

(defun gh-repo-tree--render (buffer value)
  "Render a GitHub repository file tree in BUFFER.

Argument BUFFER is the buffer to render the GitHub repository tree.

Argument VALUE is the data structure representing the GitHub repository tree to
be rendered."
  (gh-repo--with-live-buffer buffer
    (let ((inhibit-read-only t))
      (delete-region (point-min)
                     (point-max))
      (setq gh-repo-tree--opened-dirs nil)
      (funcall #'gh-repo-tree--render-tree
               (gh-repo-tree--group-files
                value))
      (if-let ((readme (gh-repo--find-readme value)))
          (gh-repo-tree--fetch-readme
           readme
           (lambda (code)
             (gh-repo--with-live-buffer buffer
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (insert "\n\n" (propertize "README"
                                            'face 'header-line)
                         "\n\n" code))
               (setq gh-repo-tree--loading nil)
               (setq gh-repo-tree--error-loading nil)
               (gh-repo-tree--update-header-line)
               (goto-char (point-min))))
           (apply-partially #'gh-repo-tree--handle-error buffer))
        (setq gh-repo-tree--loading nil)
        (setq gh-repo-tree--error-loading nil)
        (gh-repo-tree--update-header-line)
        (goto-char (point-min))))))

(defun gh-repo-tree--revert (&rest _)
  "Re-render the GitHub repository tree view."
  (let* ((repo (gh-repo-tree--current-buffer-repo))
         (tree (gh-repo-tree--paths-get
                repo))
         (buff (current-buffer)))
    (when gh-repo-tree--request-buffer
      (gh-repo-tree--abort-url-retrieve
       gh-repo-tree--request-buffer)
      (setq gh-repo-tree--error-loading nil)
      (setq gh-repo-tree--loading nil)
      (gh-repo-tree--update-header-line))
    (if tree
        (gh-repo-tree--render buff tree)
      (setq gh-repo-tree--loading t)
      (gh-repo-tree--update-header-line)
      (setq gh-repo-tree--request-buffer
            (gh-repo-tree--fetch-repo-tree
             repo
             (apply-partially #'gh-repo-tree--render buff)
             (apply-partially #'gh-repo-tree--handle-error buff))))))

(define-derived-mode gh-repo-tree-mode special-mode
  "Github Repository Tree Viewer."
  "Display a GitHub repository's file structure.

Provide a tree view of a GitHub repository's file structure, allowing users to
browse directories and files. Toggle the visibility of directory contents with
expandable rows, and update the view to reflect changes in the repository's
structure."
  (setq-local buffer-undo-list t)
  (setq-local text-scale-remap-header-line t)
  (unless gh-repo-tree--old-header-line
    (setq gh-repo-tree--old-header-line header-line-format))
  (gh-repo-tree--update-header-line)
  (use-local-map gh-repo-tree-mode-map))

(put 'gh-repo-tree-mode 'mode-class 'special)

;;;###autoload
(defun gh-repo-tree-no-select (repo)
  "Display a GitHub repository's file structure without selecting it.

Argument REPO is a string representing the GitHub repository in the format
\"owner/repo\"."
  (interactive (list (or (gh-repo-tree--current-buffer-repo)
                         (gh-repo-search-repos)
                         (read-string "User and repository (user/repo): "))))
  (let ((buff (gh-repo-tree--setup-repo-buffer repo)))
    (unless (get-buffer-window buff)
      (gh-repo-tree--window-with-other-window
       (pop-to-buffer-same-window buff)))))

(defun gh-repo-tree--setup-repo-buffer (repo)
  "Create and setup a buffer for GitHub repository file tree.

Argument REPO is a string representing the GitHub repository in the format
\"owner/repo\"."
  (let ((buff (get-buffer-create (concat
                                  gh-repo-tree-file-buffer-name-prefix
                                  repo))))
    (with-current-buffer buff
      (let ((inhibit-read-only t))
        (gh-repo-tree-mode)
        (gh-repo-tree--revert)
        (setq buffer-read-only t)
        buff))))

;;;###autoload
(defun gh-repo-tree (repo)
  "Display a GitHub repository's file structure in a buffer.

Argument REPO is a string representing the GitHub repository in the format
\"owner/repo\"."
  (interactive (list (or (gh-repo-tree--current-buffer-repo)
                         (gh-repo-search-repos)
                         (read-string "User and repository (user/repo): "))))
  (let ((buff (gh-repo-tree--setup-repo-buffer repo)))
    (if-let ((wnd (get-buffer-window buff)))
        (select-window wnd)
      (gh-repo-tree--window-with-other-window
       (pop-to-buffer-same-window buff)))))

(defalias 'gh-repo-run-github-explorer #'gh-repo-tree)

(defun gh-repo--shuffle-words (words)
  "Shuffle WORDS into all possible comma-separated combinations.

Argument WORDS is a list of strings to be shuffled."
  (let ((combo-list (list nil)))
    (dolist (word words combo-list)
      (setq combo-list
            (append combo-list
                    (mapcar (lambda (combo)
                              (if combo
                                  (concat combo "," word)
                                word))
                            combo-list))))
    (delete nil combo-list)))

(transient-define-argument gh-repo-search-term-argument ()
  "Read description and assign it in the variable `gh-repo-list--text'."
  :description "?q="
  :class 'transient-lisp-variable
  :always-read t
  :reader #'read-string
  :argument ""
  :variable 'gh-repo-list--text)


(transient-define-suffix gh-repo-echo-arguments (arguments)
  "Show the transient's active ARGUMENTS in the echo area.
Intended for use in prefixes used for demonstration purposes,
such as when suggesting a new feature or reporting an issue."
  :transient t
  :description "Echo arguments"
  :key "x"
  (interactive (list (transient-args transient-current-command)))
  (print arguments)
  (message "%s: %s"
           (key-description (this-command-keys))
           (mapconcat (lambda (arg)
                        (propertize (if (string-match-p " " arg)
                                        (format "%S" arg)
                                      arg)
                                    'face 'transient-argument))
                      arguments " ")))

;;;###autoload (autoload 'gh-repo-search-menu "gh-repo" nil t)
(transient-define-prefix gh-repo-search-menu ()
  "Command dispatcher for GitHub search queries."
  [:description (lambda ()
                  (format "/search/repositories?q=%s%s"
                          (or gh-repo-list--text "")
                          (gh-repo-format-args-to-query
                           (gh-repo-get-args-for-query))))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       transient--prefix)
      (append
       (list '("."  "Search for: " gh-repo-search-term-argument))
       (delq nil
             (mapcar (lambda (it)
                       (cond ((equal (car it)
                                     "+i")
                              (setcar it "i")
                              it)
                             ((equal (car it)
                                     "-i")
                              nil)
                             (t it)))
                     (gh-repo-search-queries-to-options
                      gh-repo-search-code-queries))))))]
  [:setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       transient--prefix)
      (mapcar (pcase-lambda (`(,k ,descr ,fn))
                (list (char-to-string k)
                      descr
                      (lambda ()
                        (interactive)
                        (funcall fn
                                 (gh-repo-search-repos
                                  gh-repo-list--text
                                  (gh-repo-get-search-query))))))
              gh-repo-actions)))]
  ["Search and show results in"
   ("C-c C-a" gh-repo-echo-arguments)
   ("RET" "List" (lambda ()
                   (interactive)
                   (setq gh-repo-list--text
                         (if (or (not gh-repo-list--text)
                                 (string-empty-p gh-repo-list--text))
                             (read-string "Search for: ")
                           gh-repo-list--text))
                   (gh-repo-list-search gh-repo-list--text
                                        (gh-repo-get-search-query))))
   ("s" "In minibuffer" (lambda ()
                          (interactive)
                          (gh-repo-prompt-repo-action
                           (gh-repo-search-repos
                            gh-repo-list--text
                            (gh-repo-get-search-query)))))]
  (interactive)
  (gh-repo-authenticate)
  (transient-setup #'gh-repo-search-menu))

(provide 'gh-repo)
;;; gh-repo.el ends here
