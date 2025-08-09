;;; org-xob-core.el --- Advanced knowledge management system in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Willy Rempel
;; Author: Willy Rempel <willy.rempel@acm.org>
;; URL: https://github.com/vv111y/org-xob.el
;; Version: 0.5-pre
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines

;; This file is not part of GNU Emacs.

;;; Commentary:
;; core functionality for org-xob

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

;;;; Requirements

(require 'org)
;; (require 'org-element)
(require 'org-id)
(require 'org-ql)
(require 'org-ql-search)
(require 'cl)
(require 'cl-lib)
(require 'org-super-links)
(require 'hydra)
(require 'pulse)

(declare-function org-super-links-link "org-super-links.el")
(declare-function org-super-links-store-link "org-super-links.el")
(declare-function org-super-links-insert-link "org-super-links.el")

(setq org-super-links-backlink-into-drawer t)
(setq org-super-links-link-prefix nil)
(setq org-super-links-link-postfix nil)
(setq org-super-links-backlink-postfix nil)
(setq org-super-links-related-into-drawer nil)
(setq org-super-links-search-function #'org-xob-get-node)

;;;; Customization

(defgroup org-xob nil
  "Settings for `org-xob'."
  :group 'org-mode
  :link '(url-link "http://github.com/vv111y/org-xob.el"))

(defcustom org-xob-something nil
  "This setting does something."
  :type 'something)

(defcustom org-xob-known-dirs (list (cons "Default" "~/xob/"))
  "List of known xob directories, as an alist of (name . directory-path)."
  :type '(alist :key-type string :value-type directory)
  :group 'org-xob)

(defcustom org-xob-current-name "Default"
  "Name of the currently selected xob repository."
  :type 'string
  :group 'org-xob)

(defcustom org-xob-auto-display-links t
  "Automatically display backlinks and forlinks when opening a node.
When t, both backlinks and forlinks are displayed.
When 'backlinks, only backlinks are displayed.
When 'forlinks, only forlinks are displayed.
When nil, no links are automatically displayed."
  :type '(choice (const :tag "Display both backlinks and forlinks" t)
                 (const :tag "Display only backlinks" backlinks)
                 (const :tag "Display only forlinks" forlinks)
                 (const :tag "Don't auto-display links" nil))
  :group 'org-xob)

(defcustom org-xob-auto-dual-pane t
  "Automatically set up dual-pane window layout when starting org-xob.
When t, automatically create and display dual-pane layout with context buffer on the right.
When nil, start with single window and let user manually configure layout."
  :type 'boolean
  :group 'org-xob)

;;;; Variables
;;;;; State variables

(defvar org-xob-on-p nil)

(defvar org-xob-today nil "The current day node.")

(defvar org-xob-new-day-timer nil
  "Timer that opens a new day node at midnight.")

(defvar org-xob-buffers nil "List of active xob buffers.")

(defvar org-xob-all-buffers nil "List of active xob buffers, including context buffers.")

(defvar org-xob-last-buffer "" "Last xob buffer used.")

(defvar org-xob--open-nodes nil
  "List of all nodes that are opened for editing.")

;; "associate list of displayed context items for an opened node ID."
(cl-defstruct open-node
  "State information about nodes that have been opened for viewing/editing."
  ID title sources)

;;;;; hash tables

(defvar org-xob--table-size 1000000
  "Size of the hash tables.")

(defvar org-xob--title-id nil)

(defvar org-xob--id-title nil)

;;;;; knowledge base sources

(defvar org-xob-available-sources nil
  "List of context information sources that are available in the xob system.")

(defvar org-xob--source-backlinks
  '(:name backlinks
          :tags ("KB" "backlinks")
          :title nil
          :ID nil
          :PID nil
          :getfn org-xob--node-get-link-entries
          :items nil))

(defvar org-xob--source-forlinks
  '(:name forlinks
          :tags ("KB" "forlinks")
          :title nil
          :ID nil
          :PID nil
          :getfn org-xob--node-get-link-entries
          :items nil))

;;;;; file variables
(defvar org-xob-dir "~/xob/"
  "Core directory for exobrain system.")

(defvar org-xob-max-KB-filesize 524288
  "Specifies the largest size the knowledge base org-mode files should grow to. Once the current file reaches the limit, a new file is created.")

(defvar org-xob--KB-filename-prefix "KB-file-"
  "suffix for KB filenames. A simple filecount value is appended for a new name")

(defvar org-xob--log-filename-prefix "log-file-"
  "suffix for log filenames. A simple filecount value is appended for a new name")

(defvar org-xob--agenda-filename-prefix "agenda-file-"
  "suffix for agenda filenames. A simple filecount value is appended for a new name")

(defvar org-xob--archive-filename-prefix "archive-file-"
  "suffix for archive filenames. A simple filecount value is appended for a new name")

;; lists of the xob files
(defvar org-xob--KB-files nil
  "List of all knowledge base files.")

(defvar org-xob--log-files nil
  "List of all xog log files.")

(defvar org-xob--agenda-files nil
  "List of all xob agenda files.")

(defvar org-xob--archive-files nil
  "List of all xob archive files.")

;; the currently active files
(defvar org-xob--KB-file nil
  "The currently active KB file to store previous versions of nodes.")

(defvar org-xob--log-file nil
  "The current log file where day nodes and general activity is recorded.")

(defvar org-xob--agenda-file nil
  "The current xob agenda file where all activity nodes other than day nodes go.")

(defvar org-xob--archive-file nil
  "The current xob archive file where archived nodes go.")

;; file header strings
(defvar org-xob--xob-header "#+PROPERTY: xob t\n")
(defvar org-xob--log-header "#+PROPERTY: xob-log t\n")
(defvar org-xob--agenda-header "#+PROPERTY: xob-agenda t\n")
(defvar org-xob--archive-header "#+PROPERTY: xob-archive t\n")
(defvar org-xob--current-header "#+PROPERTY: xob-current-file t\n")

;;;;; node/capture variables

(defvar org-xob-labels '("one" "two" "cat" "dog"))

(defvar org-xob--node-types
  '("a.day" "a.project" "a.session" "a.log" "a.log.life" "a.log.tools" "a.log.project" "a.todo" "n.n" "n.topic" "n.bib.article" "n.bib.book" "n.bib.web" "t.free" "t.project")
  "List of different types of nodes. The default is 'n.n', a generic content node. Type prefixes:
a    -- activity nodes related to tasks and scheduling
t    -- todo type nodes
n    -- node. knowledge base contents
n.b  -- bibliographic entries")

(defvar org-xob--auto-templates '("ad" "as" "al" "all" "alit" "alt" "lp" "nt" "na" "nw" "tf" "tp"))

(defvar org-xob--templates
  `(("nn" "new node" entry (file org-xob--KB-file)
     "* %(eval org-xob--last-title) \n:BACKLINKS:\n:END:\n"
     :xob-node t
     :ntype "n.n"
     :immediate-finish t
     :empty-lines-after 1)

    ("ad" "today" entry (file+function org-xob--log-file ,(lambda () (org-datetree-find-month-create (calendar-current-date))))
     "**** %<%F %A> \n:BACKLINKS:\n:END:\n"
     :xob-node t
     :immediate-finish t
     :ntype "a.day"
     )

    ;; org-projectile for now
    ("ap" "new project" entry (file org-xob--agenda-file)
     "* %^{description} \n:BACKLINKS:\n:END:\n"
     :xob-node t
     :ntype "a.project"
     :immediate-finish t
     )

    ;; not sure
    ("as" "new session" entry (file org-xob--agenda-file)
     "* %^{description}  \n:BACKLINKS:\n:END:\n"
     :xob-node t
     :ntype "a.session"
     :immediate-finish t
     )

    ;; regular templates for now
    ("tf" "todo general" entry (file org-xob--agenda-file)
     "* %^{description} \n:BACKLINKS:\n:END:\n\n%?"
     :xob-node t
     :todo t
     :ntype "a.todo"
     :immediate-finish t
     )

    ;; org-projectile for now
    ("tp" "todo project" entry (file org-xob--agenda-file)
     "* %^{description} \n:BACKLINKS:\n:END:\n\n%a\n%?"
     :xob-node t
     :todo t
     :ntype "a.todo"
     :immediate-finish t
     ))
  "Org capture templates required for org-xob to run. More templates can be added for customization.")

;;;;; link variables

;; NOTE if I use org-id-store-link in either xob links, then org-id asks which function to use for storing
;; some reason link store functions cannot be reused. org-id-open for follow does work though
;; modus vivendi link colour ("blue-alt-other" . "#00bcff")
(org-link-set-parameters "xob"
                         :follow #'org-id-open
                         :face '(:foreground "#ccbafa" :underline t)
                         :store nil
                         )

(org-link-set-parameters "xobdel"
                         :follow #'org-id-open
                         :face '(:foreground "dim gray" :underline t)
                         :store nil
                         )

(org-link-set-parameters "id"
                         :follow #'org-id-open
                         :face '(:foreground "#00aaff" :underline t)
                         :store #'org-id-store-link)

(defconst org-xob--x-link-re "\\[\\[xob:"
  "Regex for xob link types.")

(defconst org-xob--xdel-link-re "\\[\\[xobdel:"
  "Regex for xobdel link types.")

(defconst org-xob--x-link-str "[[xob:"
  "String for xobdel link types.")

(defconst org-xob--xdel-link-str "[[xobdel:"
  "String for xobdel link types.")

(defconst org-xob--id-link-str "[[id:"
  "String for id link types.")

(defconst org-xob--log-re "[^[:alnum:] : _ , . ? ; + = ! @ # $ % & ( ) < > -]"
  "Regex to clean log entry descriptions.")


;;;;; Keymaps

(defvar org-xob-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "org-xob-map"))
        (maps (list
               ;; Mappings go here, e.g.:
               ;; "C-RET" #'(lambda () (message "Override!"))
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))

(defvar org-xob-context-mode-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "org-xob-context-mode-map"))
        (maps (list
               ;; "C-RET" #'(lambda () (message "Override!"))
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))
;;;; Macros
(defmacro org-xob-with-xob-on (&rest body)
  (declare (debug (body)))
  `(if org-xob-on-p
       (progn
         ,@body)
     (message "xob is not on.")))

(defmacro org-xob-with-xob-buffer (&rest body)
  `(progn (or (org-xob-edit-buffer-p (current-buffer))
              (and (org-xob-edit-buffer-p org-xob-last-buffer)
                   (switch-to-buffer org-xob-last-buffer))
              (switch-to-buffer (setq org-xob-last-buffer
                                      (org-xob-new-buffer))))
          ,@body))


(provide 'org-xob-core)
;;; org-xob-core.el ends here
