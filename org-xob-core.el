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

;;;; Faces

(defface org-xob-edit-heading-face
  '((((class color) (background dark))
     :background "#2a2e39" :foreground "#ffd75f" :weight bold)
    (((class color) (background light))
     :background "#fff5cc" :foreground "#5f00af" :weight bold))
  "Face to highlight the node heading line in xob edit buffers."
  :group 'org-xob)

;; Faces for context buffer source headings
(defface org-xob-context-backlinks-face
  '((t :background "#2a2e4e" :foreground "dodgerblue" :weight bold))
  "Face for context buffer backlinks source headings.")

(defface org-xob-context-forlinks-face
  '((t :background "#4a1e1e" :foreground "orange" :weight bold))
  "Face for context buffer forlinks source headings.")

(defface org-xob-properties-drawer-face
  '((t :inherit nil :foreground "#222222" :height 0.2 :weight light))
  "Face to visually minimize PROPERTIES drawers in xob context buffers."
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
;;;; Minor Mode & Keybindings

;;;###autoload
(define-minor-mode org-xob-mode
  "Org-Exobrain Minor Mode."
  :lighter "Ⓧ"
  :keymap  (let ((map (make-sparse-keymap))) map)
  :group 'org-xob
  :require 'org-xob
  (if org-xob-mode
      (progn
        ;; (remove-hook 'kill-buffer-hook #'org-xob--close-buffer-hook 'local)
        )
    (progn
      ;; (unless org-xob-on-p (org-xob-start))
      ;; (add-hook 'kill-buffer-hook #'org-xob--close-buffer-hook nil 'local)
      )))

;;;###autoload
(define-minor-mode org-xob-context-mode
  "Org-Exobrain Minor Mode."
  :lighter "Ⓧ"
  :keymap org-xob-context-mode-map
  :group 'org-xob
  :require 'org-xob
  (if org-xob-context-mode
      (progn
        )
    (progn
      (org-xob--minimize-context-drawer-indicator)
      )))

;;;; Hydra option
;;;###autoload
(defhydra org-xob-hydra (:columns 4)
  ("h" (org-xob--up-heading) "up")
  ("j" (org-goto-sibling) "next")
  ("k" (org-goto-sibling 'previous) "previous")
  ("l" (org-xob--down-heading) "down")
  ("L" (org-show-children) "children")
  ("c" (org-xob-clear-heading) "clear")
  ("s" (org-xob-to-summary) "summary")
  ("S" (org-xob-to-section) "section")
  ("t" (org-xob-to-node-tree) "tree")
  ("T" (org-xob-to-full-node) "full")
  ("e" (org-xob-to-edit) "edit")

  ;; Bulk operations (capital letters)
  ("C-s" (org-xob-bulk-to-summary) "bulk: summary")
  ("C-S" (org-xob-bulk-to-section) "bulk: section")
  ("C-t" (org-xob-bulk-to-node-tree) "bulk: tree")
  ("C-T" (org-xob-bulk-to-full-node) "bulk: full")
  ("C-c" (org-xob-bulk-clear-all) "bulk: clear all")

  ("q" nil "Quit" :exit t)
  )

;;;; context buffer navigation
(defun org-xob--up-heading ()
  "Xob hydra navigation: fold heading at point first, otherwise go up to parent."
  (if (or (org-xob-folded-p)
          (org-xob-empty-entry-p))
      (org-up-heading-safe)
    (outline-hide-subtree)))

(defun org-xob--down-heading ()
  "Xob hydra navigation: unfold heading one level first before going in."
  (if (org-xob-folded-p)
      (progn (org-show-entry)
             (org-show-children))
    (org-goto-first-child)))

(defun org-xob-folded-p ()
  "Returns non-nil if point is on a folded headline or plain list
item. (credit https://emacs.stackexchange.com/a/26840)."
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (invisible-p (point-at-eol))))

(defun org-xob-empty-entry-p ()
  "Returns true if this entry is empty."
  (when (org-at-heading-p)
    (let (a b)
      (save-excursion
        (org-end-of-subtree)
        (setq a (point)))
      (save-excursion
        (end-of-line)
        (setq b (point)))
      (= a b))))

;;;; Keymaps

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
               ;; TODO need single key option and evil override too
               ;; "c" #'org-xob-clear-heading
               ;; "s" #'org-xob-to-summary
               ;; "S" #'org-xob-to-section
               ;; "t" #'org-xob-to-node-tree
               ;; "f" #'org-xob-to-full-node
               ;; "e" #'org-xob-to-edit
               "C-c c" #'org-xob-clear-heading
               "C-c s" #'org-xob-to-summary
               "C-c S" #'org-xob-to-section
               "C-c t" #'org-xob-to-node-tree
               "C-c f" #'org-xob-to-full-node
               "C-c e" #'org-xob-to-edit
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))
;;;; Backend
;;;;; org-ql predicates
(org-ql-defpred is-xob-node-deep ()
  "Deepcheck if heading is a xob node."
  :body (and (property "xob" t)
             (member (org-entry-get (point) "TYPE")
                     org-xob--node-types)
             (eq 0 (org-uuidgen-p (or (org-entry-get (point) "ID")
                                      (org-entry-get (point) "PID")
                                      (org-entry-get (point) "EDIT"))))))

(org-ql-defpred is-xob-node ()
  "Quick check if the heading is in the xob system."
  :body  (gethash (property "ID") org-xob--id-title))

(org-ql-defpred is-xob-id (&optional ID)
  "Like is-xob-node-deep, but only checks for ID property."
  :body (and (property "xob" "t")
             (member (org-entry-get (point) "TYPE") org-xob--node-types)
             (eq 0 (org-uuidgen-p (property "ID" ID)))))

(org-ql-defpred is-xob-edit (&optional ID)
  "Checks that EDIT property is not empty. If argument ID is given,
then also check if EDIT is equal to it."
  :body (when-let ((eid (property "EDIT")))
          (if ID
              (string= ID eid)
            eid)))

(org-ql-defpred is-xob-edit-deep (&optional ID)
  "Checks if heading has EDIT with optional ID,
then checks using org-xob--is-edit-node-p."
  :body (and (if ID (string= ID (property "EDIT")))
             (org-xob--is-edit-node-p)))

;; TODO redo without using tags, no longer being used
;; (org-ql-defpred is-xob-original (&optional ID)
;;   "Quick check if heading has an ID, but no edit tag."
;;   :body (and (property "ID" ID)
;;              (not (tags "EDIT"))))

(org-ql-defpred is-xob-original-deep ()
  "Deepcheck if heading is a xob original node."
  :body (and t ;; (not (tags "EDIT"))
             (not (property "PID"))
             (not (property "EDIT"))
             (property "xob" t)
             (member (org-entry-get (point) "TYPE")
                     org-xob--node-types)
             (eq 0 (org-uuidgen-p (org-entry-get (point) "ID")))))

(org-ql-defpred is-xob-source (PID)
  "Checks if heading is a source for node with ID 'PID'"
  :body  (org-xob--is-source-p
          (property "PID" PID)))

;;;;; org-ql mapping functions

(defun org-xob-map-buffers (func)
  "Apply func in all xob buffers."
  (mapc #'(lambda (win) (with-current-buffer win
                          (funcall func)))))

(defun org-xob-map-headings-in-buffers (func)
  "Apply func to all headings with an ID property in all xob buffers."
  (org-ql-select org-xob-buffers
    `(property "ID")
    :action func))

(defun org-xob-find-in-buffers (prop val)
  "Find all open headings with property prop that has value val."
  (org-ql-select org-xob-buffers
    `(property ,prop ,val)))

(defun org-xob-find-any-in-buffers ()
  "Find any node in any xob buffers.")

(defun org-xob-find-all-in-buffers (ID)
  "Find all nodes with same ID in all buffers.")

(defun org-xob-map-if-in-buffers (pred func)
  "Apply func to all headings that satisfy paredicate pred in all xob buffers."
  (org-ql-select org-xob-buffers
    `(,pred)
    :action func))

(defun org-xob-map-all-nodes (func)
  "Apply FUNC on all nodes in all xob buffers."
  (org-ql-select org-xob-buffers
    '(is-xob-node)
    :action func))

(defun org-xob-map-all-edits (func)
  "Apply FUNC on all edit nodes in all xob buffers."
  (org-ql-select org-xob-buffers
    '(is-xob-edit)
    :action func))

(defun org-xob-map-buffer-edit-nodes (func)
  "Apply FUNC to all edit nodes in current buffer."
  (org-ql-select (current-buffer)
    '(is-xob-edit)
    :action func))

(defun org-xob-map-all-copies (func)
  (org-ql-select org-xob-buffers
    '(is-xob-copy)
    :action func))

(defun org-xob-map-all-sources (func)
  (org-ql-select org-xob-buffers
    '(is-xob-source)
    :action func))

(defun org-xob-map-node-sources (ID func)
  (org-ql-select org-xob-all-buffers
    `(is-xob-source ,ID)
    :action func))

(defun org-xob-node-source (ID source func)
  "Apply FUNC to all sources with ID and SOURCE tag."
  (org-ql-select org-xob-buffers
    `(and (is-xob-source ,ID)
          (tags source))
    :action func))

;;;;; xob Management

;;;###autoload
(defun org-xob-info ()
  "Give basic information about the xob system."
  (interactive)
  (org-xob-with-xob-on
   (display-message-or-buffer
    (concat
     "XOB State\n"
     "---------\n"
     "title-id-table entries:\t\t\t"
     (number-to-string (hash-table-count org-xob--title-id)) "\n"
     "id-title-table entries:\t\t\t"
     (number-to-string (hash-table-count org-xob--id-title)) "\n"
     "org-id entries:\t\t\t\t\t\t\t"
     (number-to-string (hash-table-count org-id-locations)) "\n"
     "\n"
     "KB files count:\t\t\t\t\t\t\t"
     (number-to-string (length org-xob--KB-files)) "\n"
     "Agenda files count:\t\t\t\t\t"
     (number-to-string (length org-xob--agenda-files)) "\n"
     "Log files count:\t\t\t\t\t\t\t"
     (number-to-string (length org-xob--log-files)) "\n"
     "Archive files count:\t\t\t\t\t"
     (number-to-string (length org-xob--archive-files)) "\n"
     "\n"
     "current KB file:\t\t\t\t\t\t\t" org-xob--KB-file "\n"
     "current agenda file:\t\t\t\t\t" org-xob--agenda-file "\n"
     "current log file:\t\t\t\t\t\t" org-xob--log-file "\n"
     "current archive file:\t\t\t\t" org-xob--archive-file "\n"))))

;;;###autoload
(defun org-xob-rebuild ()
  "Remakes xob data structures, traverse all nodes in all KB files in the xob directory."
  (interactive)
  (clrhash org-xob--id-title)
  (clrhash org-xob--title-id)
  (message "XOB: cleared hash tables.")
  (and
   (org-xob--scan-and-register-files)
   (message "XOB: re-registered all xob files."))
  (let ((filelist (append org-xob--KB-files
                          org-xob--agenda-files
                          (list org-xob--log-file)))
        ID title)
    (and
     (org-id-update-id-locations filelist)
     (message "XOB: updated org-id hashtable."))
    (message "XOB: traversing all KB files...")
    (org-xob-visit-nodes
     filelist
     #'(lambda ()
         (setq ID (org-id-get (point)))
         (setq title (nth 4 (org-heading-components)))
         (if (string= title org-xob-today-string)
             (progn
               (org-id-add-location ID (buffer-file-name))
               (setq org-xob-today ID)
               (message "XOB: found today node in %s" (buffer-file-name))))
         (puthash ID title org-xob--id-title)
         (puthash title ID org-xob--title-id))))
  (message "XOB: finished rebuilding xob hashtables.")
  (org-xob-info)
  (org-xob--save-state)
  (message "XOB: saved xob state."))

(defun org-xob-visit-nodes (filelist func)
  "Iterate over all KB nodes in all files. Apply function func to each node at point."
  (save-window-excursion
    (save-excursion
      (dolist (filename filelist)
        (if (stringp filename)
            (with-current-buffer (find-file filename)
              (org-with-wide-buffer
               (goto-char (point-min))
               (while
                   (progn
                     (if (org-xob--is-node-p nil 'DEEPCHECK)
                         (funcall func))
                     (outline-next-heading))))))))))

(defun org-xob-node-info (&optional arg)
  "Get node id, list of for and back links. Default are node names, with
C-u only ids are returned. Used mostly for debugging purposes."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (let* ((eiid (org-entry-get (point) "ID"))
             (node (org-xob--get-open-node (org-entry-get (point) "EDIT")))
             (srcs (open-node-sources node))
             (blid (plist-get (nth 1 srcs) :ID))
             (flid (plist-get (car srcs) :ID)))
        (org-xob-goto-original)
        (let ((id (org-entry-get (point) "ID"))
              (bl (org-xob--node-get-links 'backlinks))
              (fl (org-xob--node-get-links 'forlinks)))
          (unless (equal arg '(4))
            (setq bl (mapcar (lambda (s) (gethash s org-xob--id-title)) bl))
            (setq fl (mapcar (lambda (s) (gethash s org-xob--id-title)) fl)))
          (setq bl (mapconcat (lambda (s) s) bl " || "))
          (setq fl (mapconcat (lambda (s) s) fl " || "))
          (print node)
          (display-message-or-buffer
           (concat
            "Node Info:  " id " || eiid: " eiid " || fl.id: " flid " || bl.id: " blid "\n"
            "BACKLINKS:  " bl "\n"
            "FORLINKS:   " fl "\n")))))))

;; --- persistent objects ---
(defun org-xob--save-state ()
  "Save exobrain state. For current version this means the lookup hashtables only."
  (unless (file-directory-p org-xob-dir)
    (make-directory org-xob-dir))
  (cl-mapcar #'(lambda (table filename)
                 (org-xob--save-object (concat org-xob-dir filename) table))
             '(org-xob--title-id org-xob--id-title)
             '("title-id-table" "id-title-table")))

(defun org-xob--load-state ()
  "Load exobrain state. For current version this means the lookup hashtables only.
If there are no saved tables, then create new empty ones."
  (cl-mapcar #'(lambda (table filename)
                 (if (file-exists-p (concat org-xob-dir filename))
                     (prog1 (message "XOB: found %s" filename)
                       (org-xob--load-object filename table))
                   (progn
                     (message "XOB: hashtable %s missing, initializing new %s" filename table)
                     (set table (make-hash-table
                                 :test 'equal
                                 :size org-xob--table-size)))))
             '(org-xob--title-id org-xob--id-title)
             '("title-id-table" "id-title-table")))

(defun org-xob--save-object (file data)
  "save emacs object. "
  (with-temp-file file
    (prin1 (symbol-value data) (current-buffer))))

(defun org-xob--load-object (file symbol)
  "load saved object."
  (when (boundp symbol)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents (concat org-xob-dir file))
          (goto-char (point-min))
          (set symbol (read (current-buffer))))
      (error (message "Error loading file %s" file)))))

;; --- contextual resources ---

(defun org-xob-add-info-source (source)
  "Add a contextual information source to xob system."
  (interactive)
  (and (symbolp source)
       (plist-member source :name)
       (plist-member source :tags)
       (plist-member source :title)
       (plist-member source :ID)
       (plist-member source :PID)
       (plist-member source :getfn)
       (plist-member source :items)
       (push source org-xob-available-sources)))

;; --- file management ---
(defun org-xob--scan-and-register-files ()
  "Scan directory and register all xob files. Create missing files if needed.
This replaces both org-xob--register-files and org-xob--process-files."
  (org-xob--clear-file-variables)

  ;; First pass: scan existing files and register them
  (dolist (filepath (directory-files org-xob-dir t "\\.org$"))
    (unless (string-match-p "#" filepath)
      (org-xob--register-single-file filepath)))

  ;; Second pass: ensure we have current files of each type
  (org-xob--ensure-current-files)

  ;; Set up org-id files after registration
  (setq org-id-extra-files (append org-xob--KB-files
                                   org-xob--agenda-files
                                   org-xob--log-files))
  t)

(defun org-xob--register-single-file (filepath)
  "Register a single file based on its properties."
  (with-temp-buffer
    (insert-file-contents-literally filepath nil 0 1024)
    (let* ((props (car (org-collect-keywords '("PROPERTY"))))
           (is-xob (member "xob t" props))
           (is-current (member "xob-current-file t" props)))
      (when is-xob
        (cond
         ((member "xob-log t" props)
          (push filepath org-xob--log-files)
          (when is-current (setq org-xob--log-file filepath)))
         ((member "xob-agenda t" props)
          (push filepath org-xob--agenda-files)
          (when is-current (setq org-xob--agenda-file filepath)))
         ((member "xob-archive t" props)
          (push filepath org-xob--archive-files)
          (when is-current (setq org-xob--archive-file filepath)))
         (t
          (push filepath org-xob--KB-files)
          (when is-current (setq org-xob--KB-file filepath))))))))

(defun org-xob--ensure-current-files ()
  "Ensure we have a current file for each type. Create if missing."
  (let ((file-specs '((org-xob--agenda-file  org-xob--agenda-filename-prefix  org-xob--agenda-files  org-xob--agenda-header)
                      (org-xob--log-file     org-xob--log-filename-prefix     org-xob--log-files     org-xob--log-header)
                      (org-xob--archive-file org-xob--archive-filename-prefix org-xob--archive-files org-xob--archive-header)
                      (org-xob--KB-file      org-xob--KB-filename-prefix      org-xob--KB-files      nil))))
    (dolist (spec file-specs)
      (let ((current-var (nth 0 spec))
            (prefix-var (nth 1 spec))
            (list-var (nth 2 spec))
            (type-header (nth 3 spec)))
        (unless (and (symbol-value current-var)
                     (file-exists-p (symbol-value current-var)))
          (org-xob--create-new-file current-var prefix-var list-var type-header))))))

(defun org-xob--create-new-file (current-var prefix-var list-var type-header)
  "Create a new file of the specified type."
  (let* ((prefix (symbol-value prefix-var))
         (existing-files (symbol-value list-var))
         (file-number (+ 1 (length existing-files)))
         (filename (format "%s%03d.org" prefix file-number))
         (filepath (expand-file-name filename org-xob-dir)))

    ;; Remove current status from any existing file of this type
    (when (symbol-value current-var)
      (org-xob--remove-current-status (symbol-value current-var)))

    ;; Create the new file
    (with-temp-file filepath
      (insert org-xob--xob-header)
      (when type-header
        (insert type-header))
      (insert org-xob--current-header))

    ;; Update the variables
    (add-to-list list-var filepath)
    (set current-var filepath)


    (message "XOB: Created new %s file: %s"
             (symbol-name current-var) filename)
    filepath))

(defun org-xob--remove-current-status (filepath)
  "Remove xob-current-file property from a file."
  (when (and filepath (file-exists-p filepath))
    (with-current-buffer (find-file-noselect filepath)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+PROPERTY: xob-current-file t$" nil t)
          (delete-region (line-beginning-position) (1+ (line-end-position)))
          (save-buffer))))))

(defun org-xob--clear-file-variables ()
  "All file associated variables set to nil."
  (setq org-xob--KB-files nil
        org-xob--agenda-files nil
        org-xob--log-files nil
        org-xob--archive-files nil
        org-xob--KB-file nil
        org-xob--agenda-file nil
        org-xob--log-file nil
        org-xob--archive-file nil))

(defun org-xob--uncurrent-file (file)
  "Remove current status from given file."
  (save-excursion
    (with-current-buffer (find-file (eval file))
      (goto-char (point-min))
      (when (re-search-forward "^#\\+PROPERTY: xob-current-file t$" nil t)
        (kill-whole-line 1)
        (save-buffer)))))

;; --- misc functions ---

(defun org-xob-dt-to-ts ()
  "Convenience function: datetree to timestamp when just under heading."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-set-property
     "CREATED"
     (concat "["
             (truncate-string-to-width
              (nth 4 (org-heading-components)) 14)
             "]"))))

(defun org-xob-dt-to-ts-parent ()
  "Convenience function: datetree to timestamp for subheadings."
  (interactive)
  (org-set-property
   "CREATED"
   (concat "["
           (truncate-string-to-width
            (save-excursion
              (org-up-heading-safe)
              (nth 4 (org-heading-components))) 14)
           "]")))

;; --- Region to Node Conversion ---

(defun org-xob--region-to-node-with-link (beg end title)
  "Convert text region to a new node and replace with a link.
BEG and END define the region, TITLE is the node title.
Returns the ID of the created node."
  (let* ((region-text (buffer-substring-no-properties beg end))
         (timestamp (concat "[" (format-time-string "%F %a") "]"))
         node-id)
    ;; Create new node in KB file
    (save-excursion
      (save-window-excursion
        (with-current-buffer (find-file org-xob--KB-file)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "\n* " title "\n")
          (let ((node-start (point)))
            ;; Set node properties
            (setq node-id (org-id-get-create))
            (org-entry-put (point) "xob" "t")
            (org-entry-put (point) "TYPE" "n.n")
            (org-entry-put (point) "CREATED" timestamp)
            (org-entry-put (point) "MODIFIED" timestamp)
            ;; Add content
            (org-end-of-meta-data t)
            (insert region-text)
            (unless (string-suffix-p "\n" region-text)
              (insert "\n"))
            ;; Update hash tables
            (puthash node-id title org-xob--id-title)
            (puthash title node-id org-xob--title-id)
            (org-id-add-location node-id (buffer-file-name))
            ;; Log event
            (org-xob--log-event "region->node" node-id)
            (save-buffer)))))

    ;; Replace region with org-super-link
    (delete-region beg end)
    (goto-char beg)
    (org-super-links--insert-link (org-id-find node-id 'MARKERP))

    node-id))

(defun org-xob--node-to-region (node-id)
  "Convert a node link to inline text content.
NODE-ID is the ID of the node to convert back to text.
The link at point should be replaced with the node's content."
  (let* ((link (org-element-context))
         (link-beg (org-element-property :begin link))
         (link-end (org-element-property :end link))
         node-content)

    ;; Get node content
    (save-excursion
      (save-window-excursion
        (org-id-goto node-id)
        (org-end-of-meta-data t)
        (let ((content-start (point))
              (content-end (save-excursion
                             (org-end-of-subtree t t)
                             (point))))
          (setq node-content (buffer-substring-no-properties content-start content-end))
          ;; Clean up trailing whitespace
          (setq node-content (string-trim-right node-content)))))

    ;; Replace link with content
    (when (and link-beg link-end)
      (delete-region link-beg link-end)
      (goto-char link-beg)
      (insert node-content)
      ;; Log event
      (org-xob--log-event "node->region" node-id))))

;;; org-xob-core.el ends here
