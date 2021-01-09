;;; org-xob.el --- advanced knowledge management system in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Willy Rempel  
;; Author: Willy Rempel <willy.rempel@acm.org>
;; URL: https://github.com/vv111y/org-xob.el
;; Version: 0.5-pre
;; Package-Requires: ((emacs "25.2") (org) (org-element) (org-id) (org-ql) (cl-lib) (org-super-links))
;; Keywords: 

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Making the most of org-mode. Another attempt at an exo-brain inspired by: zettlekasten, wikis, roam, and all the other ways to organize ourselves. 

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:
;; + org
;; + org-element
;; + org-id
;; + org-ql
;; + cl-lib
;; + org-super-links

;; Then put this file in your load-path, and put this in your init
;; file:
;; (require 'org-xob)

;;;; Usage

;; Run one of these commands:

;; `org-xob-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `org-xob' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

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
(require 'org-element)
(require 'org-id)
(require 'org-ql)
(require 'cl-lib)
(require 'org-super-links)

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
  :link '(url-link "http://example.com/org-xob.el"))

(defcustom org-xob-something nil
  "This setting does something."
  :type 'something)

;;;; Variables

(defvar org-xob-on-p nil)

;;;;; hash tables 

(defvar org-xob--table-size 1000000
  "Size of the hash tables.")

(defvar org-xob--title-id nil)

(defvar org-xob--id-title nil) 

;;;;; state

(defvar org-xob-today nil
  "The current day node.")

(defvar org-xob--objects '((org-xob--title-id . "title-id-table")
                                (org-xob--id-title . "id-node-table")
                                (org-xob--KB-files . "KB-files")
                                (org-xob--KB-file . "current-KB-file")))

;;;;; knowledge base sources

(defvar org-xob--source-backlinks
  '(:name "backlinks"
          :tags ("KB" "backlinks")
          :title nil 
          :ID nil
          :PID nil
          :func org-xob--get-backlinks
          :items nil))

(defvar org-xob--source-forlinks
  '(:name "forlinks"
          :tags ("KB" "forlinks")
          :title nil 
          :ID nil
          :PID nil
          :func org-xob--get-forlinks
          :items nil))

;;;;; capture variables

(defvar org-xob--auto-templates '("ad" "as" "al" "all" "alit" "alt" "lp" "nt" "na" "nw" "tf" "tp"))

(defvar org-xob--node-types '("a.day" "a.project" "a.session" "a.log" "a.log.life" "a.log.tools" "a.log.project" "a.todo" "n.n" "n.topic" "n.bib.article" "n.bib.web" "t.free" "t.project"))

(defvar org-xob--templates
  `(("nn" "new node" entry (file ,(concat org-xob-dir org-xob--KB-file))
         "* %(eval org-xob--last-title) \n:PROPERTIES:\n:TYPE:\t\t\tn.n\n:CREATED:\t\t%U\n:MODIFIED:\t\t%U\n:END:\n:BACKLINKS:\n:END:\n"
         :xob-node t
         :ntype "n.n"
         :func (lambda () t)
         :immediate-finish t
         :empty-lines-after 1)

        ("ad" "today" entry (file+datetree ,(concat org-xob-dir org-xob--log-file))
         "* %u\n:PROPERTIES:\n:TYPE:\t\t\ta.day\n:END:\n:BACKLINKS:\n:END:\n"
         :xob-node t
         :func (lambda () t)
         :immediate-finish t
         :ntype "a.day"
         )

        ;; TODO finish agenda entries
        ("ap" "new project" entry (file ,(concat org-xob-dir org-xob--agenda-file))
         "* Project  \n:PROPERTIES:\n:TYPE:\t\t\ta.project\n:END:\n:BACKLINKS:\n:END:\n"
         :xob-node t
         :ntype "a.project"
         :immediate-finish t
         )

        ("as" "new session" entry (file ,(concat org-xob-dir org-xob--agenda-file))
         :xob-node t
         :ntype "a.session"
         :immediate-finish t
         )

        ("tf" "todo general" entry (file ,(concat org-xob-dir org-xob--agenda-file))
         "* %^{description} \n:BACKLINKS:\n:END:\n\n%?"
         :xob-node t
         :todo t
         :ntype "a.todo"
         :immediate-finish t
         )

        ("tp" "todo project" entry (file ,(concat org-xob-dir org-xob--agenda-file))
         "* %^{description} \n:BACKLINKS:\n:END:\n\n%a\n%?"
         :xob-node t
         :todo t
         :ntype "a.todo"
         :immediate-finish t
         )
        ))

;;;;; file variables
(defvar org-xob-dir "~/xob/" 
  "Core directory for exobrain system.")

;; TODO remove when done
(setq org-xob-dir "~/xob/")

(defvar org-xob-max-KB-filesize 524288
  "Specifies the largest size the knowledge base org-mode files should grow to. Once the current file reaches the limit, a new file is created.")

(defvar org-xob--KB-files nil
  "List of all knowledge base files.")

(defvar org-xob--KB-file nil
  "The currently active KB file to store previous versions of nodes.")

(defvar org-xob--KB-filename-prefix "KB-file-"
  "suffix for KB filenames. A simple filecount value is appended for a new name")

(defvar org-xob--log-file "xob-logfile.org"
  "The current log file where day nodes and general activity is recorded.")

(defvar org-xob--agenda-file "xob-agendafile.org"
  "The current agenda file where all activity nodes other than day nodes go.")

(defvar org-xob--active-nodes nil
  "a-list of active nodes. Those that were extracted from the KB and into the workspace.")

(defvar org-xob-syncedp nil
  "Buffer local variable that indicates whether the current contents of a buffer have been synced with the Knowledge Base.")
;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps.

(defvar org-xob-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "org-xob-map"))
        (maps (list
               ;; Mappings go here, e.g.:
               "RET" #'org-xob-RET-command
               [remap search-forward] #'org-xob-search-forward
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))

;;;; Minor Mode & Macros

;;;###autoload
(define-minor-mode org-xob-minor-mode
  "Org-Exobrain Minor Mode."
  :lighter " xob"
  :keymap  (let ((map (make-sparse-keymap))) map)
  :group 'org-xob
  :require 'org-xob
  (progn 
    (unless org-xob-on-p
      (org-xob-start))
    (if org-xob-minor-mode t t)))

(defmacro with-org-xob-on (&rest body)
  (unless org-xob-on-p
    (org-xob-start))
  ,@body)

;;;; Commands

;;;;; Main Commands
;;;###autoload
(defun org-xob-start ()
  "Start the xob system: load state or initialize new. Open new day node."
  (interactive)
  (if (and
       (if org-xob-on-p (progn (message "XOB: already started.") nil) t)
       (and
        (add-hook 'org-capture-prepare-finalize-hook #'org-xob--new-node)
        (add-hook 'org-follow-link-hook #'org-xob--link-hook-fn)
        (message "XOB: hooks enabled."))
       (if (file-directory-p org-xob-dir) (message "XOB: directory found.") 
         (prog1 (message "XOB: directory not found, creating.")
           (make-directory org-xob-dir t)))
       (cl-loop for (k . v) in org-xob--objects
                do (if (file-exists-p (concat org-xob-dir v))
                       (prog1 (message "XOB: found %s" v)
                         (org-xob--load-object v k))
                     (progn 
                       (message "XOB: file %s missing, initializing new %s" v k)
                       (cond
                        ((equal "org-xob--KB-file" (symbol-name k))
                         (set k (org-xob--new-KB-file)))
                        ((equal "org-xob--KB-files" (symbol-name k))
                         (set k nil))
                        ((or (equal "org-xob--title-id" (symbol-name k))
                             (equal "org-xob--id-title" (symbol-name k)))
                         (set k (make-hash-table
                                 :test 'equal
                                 :size org-xob--table-size))))))
                finally return t)
       (if (file-exists-p (concat org-xob-dir org-xob--log-file))
           (message "XOB: found log file.")
         (with-temp-file (concat org-xob-dir org-xob--log-file)
           (message "XOB: log file missing, initializing new.")
           (insert "") t))
       (setq org-id-extra-files org-xob--KB-files)
       (setq org-xob--kb-file-counter (length org-xob--KB-files))
       (setq org-xob-today-string (concat "[" (format-time-string "%F %a") "]"))
       (and 
        (or
         (setq org-xob-today (gethash org-xob-today-string 
                                      org-xob--title-id))
         (setq org-xob-today (org-xob--capture "ad")))
        (save-window-excursion
          (setq org-xob-today-buffer
                (find-file (concat org-xob-dir org-xob--log-file))))
        (message "XOB: Todays log entry opened."))
       (and
        (if (file-exists-p (concat org-xob-dir org-xob--agenda-file))
            (message "XOB: found xob agenda file.")
          (with-temp-file (concat org-xob-dir org-xob--agenda-file)
            (message "XOB: xob agenda file missing, initializing new.")
            (insert "") t))
        (unless (member org-xob--agenda-file org-agenda-files)
          (push (concat org-xob-dir org-xob--agenda-file) org-agenda-files))))
      (prog1 
        (setq org-xob-on-p t)
        (message "XOB: started."))
    (message "XOB: Unable to (re)start.")))

;;;###autoload
(defun org-xob-stop ()
  "Stop xob system: save all state and close active buffers."
  (interactive)
  (if org-xob-on-p
      (progn 
        (org-xob-save-state)
        (with-current-buffer org-xob-today-buffer
          (save-buffer)
          (kill-buffer))
        (setq org-xob-today nil)
        (remove-hook 'org-capture-prepare-finalize-hook #'org-xob--new-node)
        (remove-hook 'org-follow-link-hook #'org-xob--link-hook-fn)
        (setq org-id-extra-files nil)
        (setq org-xob-on-p nil))))

;;;###autoload
(defun org-xob-open-day ()
  "Open todays node."
  (interactive)
  (unless org-xob-on-p
    (org-xob-start))
  (condition-case nil
      (progn
        (org-id-goto org-xob-today)
        (org-narrow-to-subtree))
    (error (message "todays day node missing."))))

;;;###autoload
(defun org-xob-get-node ()
  "Focus on a node for editing. If it does not exist, create it."
  (interactive)
  (unless org-xob-on-p
    (org-xob-start))
  (pcase-let ((`(,ID ,title) (org-xob--get-create-node)))
    (org-xob--edit-node ID title)))

;;;###autoload
(defun org-xob-insert-link ()
  "Inserts a properly formatted xob node link at point."
  (interactive)
  (unless org-xob-on-p
    (org-xob-start))
  (pcase-let ((`(,ID ,title) (org-xob--get-create-node)))
    (org-super-links--insert-link (org-id-find ID 'MARKERP))
    (org-xob--source-refresh 'forlinks)))

;; TODO
;; for each exo-link in body, visit node and remove backlink
;; for each exo-link in backlinks, visite node and kill link, leave link text
;;;###autoload
(defun org-xob-remove-node (&optional ID)
  "Removes node at point from xob system, but does not delete the node itself.
Removes node from the hash tables, and any backlinks in other nodes referencing it.
If called with optional ID argument, then remove the node with that ID."
  (interactive)
  (unless org-xob-on-p (org-xob-start))
  (save-window-excursion
    (save-excursion
      (if ID
          (org-id-goto ID))
      (let* ((ID (org-id-get (point)))
             (title (gethash ID org-xob--id-title))
             (forelinks (org-xob--node-get-links "forelinks"))
             link-element)
        (dolist (el forelinks)
          (org-id-goto el)
          (save-restriction
            (org-narrow-to-subtree)
            (setq link-element (org-super-links--find-link ID))
            (if link-element
                (org-super-links--delete-link link-element))))
        (remhash ID org-xob--id-title)
        (remhash title org-xob--title-id)
        (org-entry-put (point) "ID" "")
        (org-id-update-id-locations (list (buffer-file-name)) 'silent)))))

;;;###autoload
(defun org-xob-heading-to-node ()
  "Convenience function to convert current content into xob KB nodes."
  (interactive)
  (unless (org-xob--is-node-p)
    (org-xob--new-node (point))
    (let ((filename (buffer-file-name)))
      (unless (member filename org-xob--KB-files)
        (push filename org-xob--KB-files)))
    (org-xob--save-object
     (alist-get 'org-xob--KB-files org-xob--objects)
     org-xob--KB-files)))

;;;;; Sideline Commands
;;;###autoload
(defun org-xob-show-side-buffer (abuffer)
  "Show abuffer in the sideline window."
  (interactive)
  (unless org-xob--sideline-window
    (org-xob-toggle-sideline))
  (save-excursion
    (select-window org-xob--sideline-window)
    (display-buffer-same-window abuffer nil)))

;;;###autoload
(defun org-xob-toggle-sideline ()
  "Toggles display of the sideline window."
  (interactive)
  (save-excursion 
    (if org-xob--sideline-window 
        (progn 
          (delete-window org-xob--sideline-window)
          (setq org-xob--sideline-window nil))
      (setq org-xob--sideline-window 
            (split-window-right)))))

;;;###autoload
(defun org-xob-toggle-context ()
  "Toggles display of the contextual side window."
  (interactive)
  ;; (display-buffer-same-window org-xob--context-buffer nil)))))
  )

;;;;; KB Context Commands

;;;###autoload
(defun org-xob-show-backlinks ()
  "Add backlinks contents to the context buffer."
  (interactive)
  (org-xob--node-get-link-entries org-xob--source-backlinks)
  (org-xob--source-build org-xob--source-backlinks))

;;;###autoload
(defun org-xob-show-forlinks ()
  "Add forelinks contents to the context buffer."
  (interactive)
  (org-xob--node-get-link-entries org-xob--source-forlinks)
  (org-xob--source-build org-xob--source-forlinks))

;;;###autoload
(defun org-xob-ql-search ()
  "Use org-ql to search the KB. Creates a new source in the context buffer."
  (interactive))

;;;;; Context Presentation Commands

;;;###autoload
(defun org-xob-refresh-context ()
  "Refresh all displayed sources"
  (interactive)
  (dolist (el org-xob--node-sources)
    (org-xob--source-refresh el)))

;;;###autoload
(defun org-xob-clear-heading ()
  "Converts subtree to the headline and properties drawer only.
This is idempotent and application to such a heading makes no change.
This can be applied to heading at point or used in a mapping."
  (interactive)
  (org-with-wide-buffer
   (org-back-to-heading t)
   (org-mark-subtree)
   (org-end-of-meta-data 1)
   (call-interactively #'delete-region))
  (goto-char (- (point) 1)))

;; TEST 
;;;###autoload
(defun org-xob-to-summary ()
  "Show KB node summary. This is defined as the first paragraph if it exists."
  (interactive)
  (org-xob--kb-copy-paste
   #'(lambda () (progn
                 (org-end-of-meta-data 'full)
                 (let ((p (org-element-at-point)))
                   (if (equal (org-element-type p)
                              'paragraph)
                       (buffer-substring-no-properties
                        (org-element-property :contents-begin p)
                        (org-element-property :contents-end p))))))))

;; TEST 
;;;###autoload
(defun org-xob-to-node-tree ()
  "Show only subheadings of KB node."
  (interactive)
  (org-xob--kb-copy-paste
   #'(lambda () (let ((str))
                  (org-map-tree
                   (lambda ()
                     (setq str (concat str 
                                       (buffer-substring
                                        (line-beginning-position)
                                        (line-end-position)
                                        "\n")))))
                  str))))

;; TEST 
;;;###autoload
(defun org-xob-to-section ()
  "Show the top section of KB node, no subheadings."
  (interactive)
  (org-xob--kb-copy-paste
   #'(lambda () (let ((beg) (end))
                  (org-end-of-meta-data 1)
                  (setq beg (point))
                  (outline-next-heading)
                  (setq end (- (point) 1))
                  (buffer-substring beg end)))))

;;;###autoload
(defun org-xob-to-full-node ()
  "Show the full KB node, excepting properties drawer, planning & clocking information."
  (interactive)
  (org-xob--kb-copy-paste
   #'(lambda () (progn
                  (org-mark-subtree)
                  (org-end-of-meta-data 1)
                  (buffer-substring (point) (mark))))))

;;;; Backend

;;;;; Buffer Functions 

(defun org-xob--edit-node (ID title)
  "Create an indirect buffer of the node with name title."
  (let ((short-title (truncate-string-to-width title 20)))
    (if (get-buffer short-title)
        (kill-buffer short-title))
    (save-excursion
      (save-window-excursion
        (org-id-goto ID)
        (clone-indirect-buffer-other-window short-title t)
        (org-narrow-to-subtree)))
    (switch-to-buffer short-title)
    (setq-local ID ID title title org-xob-short-title short-title))
  (setq-local log-entry (org-xob--insert-link-header ID title org-xob-today))
  (setq-local org-xob--context-buffer
              (get-buffer-create (concat  "*context-" title)))
  (setq-local org-xob--sideline-window nil)
  (setq-local org-xob--source-backlinks)
  (setq-local org-xob--source-forlinks)
  (add-hook 'kill-buffer-hook #'org-xob--kill-context-buffer-hook nil :local)
  (org-xob-minor-mode 1)
  (org-xob--make-context-buffer org-xob-short-title
                                (current-buffer)
                                org-xob--source-backlinks
                                org-xob--source-forlinks))

(defun org-xob--make-context-buffer (title edit-buffer backlinks forlinks)
  "Create context buffer, leave it empty by default. set title and buffer
local variables for the edit buffer and the back and for links source objects."
  (with-current-buffer org-xob--context-buffer
    (org-mode)
    (setq-local org-xob--edit-buffer edit-buffer)
    (setq-local org-xob--source-backlinks backlinks)
    (setq-local org-xob--source-forlinks forlinks)))

(defun org-xob--kill-context-buffer-hook ()
  "Kill the context buffer when closing the node edit buffer. Made local variable."
  (with-current-buffer org-xob--context-buffer
    (kill-buffer)))

;;;;; Contexts Functions

(defun org-xob--node-get-link-entries (source)
  "Populates source item list from the node. The items are represented by their
respective node IDs. Two kinds of links are distinguished: backlinks and forlinks
(which are all other links to xob KB nodes). Assumes org-super-links convention
where the backlinks are in a BACKLINKS drawer."
  (save-window-excursion
    (save-excursion
      (org-id-goto (plist-get source :PID))
      (plist-put source :items
                 (org-xob--node-get-links (plist-get source :name))))))

;; --source tree fns--
(defun org-xob--is-source-p (&optional ID)
  "Check if a heading is a valid xob source.
Called interactively it defaults to heading at point.
If an ID argument is supplied, then check the heading associated with it."
  (interactive)
  (let ((temp (if ID ID
                (org-id-get nil))))
    (if temp
        (if (member temp org-xob--node-sources) t nil)
      nil)))

(defun org-xob--source-build (source)
  "Open a source tree into the context buffer. If it is already there,
then refresh it. source items are shown as org headings.
source is a plist that describes the content source."
  (interactive)
  (save-window-excursion 
    (with-current-buffer org-xob--context-buffer
      (if (member source org-xob--node-soures)
          (progn 
            (unless (org-xob--goto-heading (plist-get source :PID))
              (goto-char (point-max)) ;; respecting content below is this needed?
              (org-insert-heading (4) 'invisible-ok 'TOP)
              (org-edit-headline (plist-get source :title))
              (plist-put source :ID (org-id-get-create))
              (dolist (el (plist-get source :tags))
                (org-toggle-tag el 'ON))
              (org-toggle-tag (plist-get source :name)'ON)
              ;; FIX? for kb needs the parent id , but not all sources
              ;; (plist-put source :items (funcall (plist-get source :func)))
              (funcall (plist-get source :func) source)
              (cons source 'org-xob--node-sources))
            (org-xob-refresh-source source))
        (message "XOB: not a valid context source.")))))

(defun org-xob--source-refresh (source)
  "Remake source tree. Check if items need to be added or removed.
todo - possibly refresh item contents if changes were made.
(this requires knowing what is displayed)"
  (let ((temp (copy-tree (plist-get source :items))))
    (org-xob--map-source
     (lambda ()
       (let ((pid (org-entry-get (point) "PID")))
         (if (member pid temp)
             (setq temp (delete pid temp))
           (progn
             (org-mark-subtree)
             (delete-region)))
         (if temp
             (dolist (el temp)
               (org-xob--source-add-item el)))))
     (plist-get source :ID))))

(defun org-xob--source-add-item (ID)
  "Appends a single entry to the end of the source subtree.
Assumes point is on the source heading."
  (let ((title (gethash ID org-xob--id-title)))
    (if title 
        (save-excursion 
          (org-insert-subheading '(4))
          (org-edit-headline title)
          (org-entry-put (point) "PID" ID)
          (org-id-get-create 'FORCE)) ;; needed?
      (message "not a valid knowledge base ID: %s" ID))))

(defun org-xob--kb-copy-paste (payload)
  "Wrapper function to display new content in a context item from the
knowledge base. Executes function payload while point is at the heading
of the origin node in the KB. payload must be a lambda that returns
the node content as a string.
When called with point on the given context item, only that item will be
updated. If called on a context source heading, then the update is applied
to all source items."
  (let ((func (lambda () (progn 
                           (org-xob-clear-heading)
                           (org-end-of-meta-data)
                           (insert
                            (save-excursion
                              (org-id-goto (org-entry-get (point) "PID"))
                              (org-with-wide-buffer
                               (org-save-outline-visibility t
                                 (org-narrow-to-subtree)
                                 (outline-show-all)
                                 (funcall payload)))))))))
    (if (org-xob--is-source-p)
        (org-xob--map-source func)
      (funcall func))))

(defun org-xob--map-source (func &optional ID)
  "Apply the function func to every child-item of a xob source.
If the optional ID of a xob source is given, then apply func to that source.
Otherwise apply to source at point."
  (if ID
      (org-id-goto ID))
  (org-with-wide-buffer 
   (if (org-xob--is-source-p) 
       (progn
         (org-narrow-to-subtree)
         (outline-show-all)
         (outline-next-heading)
         (while
             (progn 
               (funcall func)
               (outline-get-next-sibling))))
     (message "not a xob source."))))

;;;;; Activity
;;;;;; Clocking
(defun org-xob--auto-clock-in ())
(defun org-xob--auto-clock-out ())
;;;;; Node Functions

(defun org-xob--is-node-p (&optional ID DEEPCHECK)
  "Check if a heading is a xob node. Called interactively it defaults to heading at point.
If an ID argument is supplied, then check the heading associated with it.
With option DEEPCHECK, do not use any table lookup, but check whether the heading
has valid UUID formatted ID and xob TYPE properties in the property drawer.
Deepcheck only works on heading at point, any ID argument is ignored."
  (interactive)
  (let (temp type)
    (if DEEPCHECK
        (if (and
             (org-at-heading-p)
             (setq temp (org-entry-get (point) "ID"))
             (eq 0 (org-uuidgen-p temp))
             (setq type (org-entry-get (point) "TYPE"))
             (member type org-xob--node-types))
            t nil)
      (setq temp (if ID ID (org-id-get nil)))
      (if temp
          (if (gethash temp org-xob--id-title) t nil)
        nil))))

;; --new nodes and links--
(defun org-xob--get-create-node ()
  "Find or create new xob KB node using helm. Returns node (ID title) as a list."
  (unless org-xob-on-p
    (org-xob-start))
  (helm :buffer "*xob get node*"
        :sources (helm-build-sync-source "xob-kb"
                   :candidates (lambda ()
                                 (let* ((cans (hash-table-keys org-xob--title-id)))
                                   (cons helm-input cans)))
                   :volatile t
                   :action (lambda (title) (let ((ID (gethash title org-xob--title-id)))
                                             (unless ID
                                               (setq ID (org-xob--capture title)))
                                             (list ID title))))))

;; leaving commented as refs for now 
(defun org-xob--new-node (&optional heading)
  "Both a hook function and for general node creation. If orgmode 'heading' is given,
then convert it into a new node in place. Otherwise it is assumed to be called
as a capture hook function."
  (if (org-capture-get :xob-node)
      (let ((ID (org-id-get-create))
            (title (nth 4 (org-heading-components)))
            type node)
        (if heading
            (setq type "n.n")
          (setq type (org-capture-get :ntype))
          ;; (funcall (org-capture-get :func))
          (if (org-capture-get :todo) (org-todo)))
        ;; (org-entry-put (point) "CREATED" timestamp)
        ;; (org-entry-put (point) "MODIFIED" timestamp)
        ;; (org-entry-put (point) "TYPE" type)
        (puthash ID title org-xob--id-title)
        (puthash title ID org-xob--title-id)
        (setq org-xob--last-captured ID))))

(defun org-xob--capture (title)
  (let* ((org-capture-templates org-xob--templates)
         ID)
    (if (member title org-xob--auto-types)
        (org-capture nil title)
      (progn
        (setq org-xob--last-title title)
        (org-capture nil "nn")))
    org-xob--last-captured))

(defun org-xob--link-hook-fn ()
  "If a link is a xob node, then reopen node in xob edit mode." 
  (let ((link (org-element-context))
        ID)
    (if (equal "ID" (org-element-property :type link))
        (progn 
          (setq ID (org-element-property :path link))
          (if (gethash ID org-xob--id-title)
              (org-xob--edit-node ID)))
      nil)))

;; --navigation--
(defun org-xob--goto-buffer-heading (ID)
  "Go to heading in current buffer with ID. Does not require org-id."
  (let ((m (point)))
    (org-with-wide-buffer 
     (goto-char (point-min))
     (if (re-search-forward ID nil t)
         (org-back-to-heading 'invisible-ok)
       (progn
         (goto-char m)
         (message "%s not found." ID))))))

;; --link headers--
(defun org-xob--insert-link-header (ID title target)
  "Checks if link subheader exist at target. If not, inserts a
subheading with an org link to the node with ID and title.
Returns mark for the link subheader."
  (save-excursion
    (save-window-excursion 
      (with-current-buffer org-xob-today-buffer)
      (let (place)
        (org-id-goto target)
        (org-map-entries (lambda () (when (equal (nth 4 (org-heading-components))
                                                 (title))
                                      (setq place (point)))) 'tree)
        (unless place
          (org-insert-subheading (4))
          (org-edit-headline (org-insert-link nil ID title)))
        (set-marker m)))))

(defun org-xob--node-get-links (linktype)
  "Return list of link paths within the node at point. If linktype is 'backlinks'
then return only links in the backlinks drawer. If linktype is 'forelinks'
then return all other links."
  (let* ((test (if (equal linktype "backlinks")
                   (lambda (x) (x))
                 (if (equal linktype "forelinks")
                     (lambda (x) (not x))))))
    (save-excursion
      (save-restriction
        (org-back-to-heading t)
        (org-narrow-to-subtree)
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (if (funcall test (equal (org-element-property
                                      :drawer-name (cadr (org-element-lineage link)))
                                     "BACKLINKS"))
                (org-element-property :path link))))))))

;;;;; Node Versioning


;; (defun org-xob--sync-node (node)
;;   "Update entry based on local edits."
;;   ;; is node in KB? no, add, else
;;   ;; is node different? no, ignore, else sync/update
;;   nil
;;   )

;; (defun org-xob--diff-node (now-node last-node)
;;   "Creates a diff using =org-xob--delta-executable=.
;; The order of versions is reversed; the diff allows the reconstruction of
;; the last-node from the now-node.
;; The diff is stored in the currently active =org-xob--KB-file=."
;;   (shell-command))
;; ;; (defun org-xob--new-node-diff (nodeID)
;; ;;   (let ((old-id (org-id-store-link node)))))

;; (defun org-xob--diff-filename (node)
;;   (concat
;;    ;; node id
;;    "-"
;;    (format-time-string "%j-%H-%M")))

(defun org-xob--node-add-time-property (property)
  "Convenience function to add high resolution time property.
Maybe useful for syncing."
  (org-entry-put (point) property
                 (number-to-string
                  (car (time-convert (current-time) '10000)))))




;;;;; xob Management
(defun org-xob-visit-nodes (func)
  "Iterate over all KB nodes in all files. Apply function func to each node at point."
  (save-window-excursion
    (save-excursion
      (dolist (kb-file-name org-xob--KB-files)
        (with-current-buffer (find-file kb-file-name)
          (goto-char (point-min))
          (while 
              (progn 
                (if (org-xob--is-node-p "" 'DEEPCHECK)
                    (funcall func))
                (outline-next-heading))))))))

(defun org-xob--save-state ()
  "Save exobrain state."
  (interactive)
  (unless (file-directory-p org-xob-dir)
      (make-directory org-xob-dir))
  (cl-loop for (k . v) in org-xob--objects
           do (org-xob--save-object (concat org-xob-dir v) k)))

(defun org-xob--load-state ()
  "Load exobrain state."
  (cl-loop for (k . v) in org-xob--objects
           do (org-xob--load-object (concat org-xob-dir v) k)))

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

(defun org-xob--new-KB-file ()
  "Create new KB file for next node in the brain. Returns the filename."
  (interactive)
  (let* ((filename (concat 
                   org-xob--KB-filename-prefix
                   (format "%03d" org-xob--kb-file-counter)
                   ".org"))
         (fullname (concat org-xob-dir filename)))
    (with-temp-file fullname
      (insert ""))
    (push filename org-xob--KB-files)
    (setq org-xob--kb-file-counter (+ 1 org-xob--kb-file-counter))
    (setq org-xob--KB-file filename)
    (save-excursion
      (find-file-noselect fullname))
    filename))

;;;###autoload
(defun org-xob-rebuild ()
  "Remakes xob data structures, traverse all nodes in all KB files in the xob directory."
  (interactive)
  ;; empty current structs, keep current kb file, logfile, agendafile
  (setq org-xob--KB-files nil)
  (clrhash org-xob--id-title)
  (clrhash org-xob--title-id)
  (message "XOB: cleared KB file list & hash tables.")
  ;; rebuild kbfiles: goto dir, for each file: has name prefix, .org suffix -> add
  (and 
   (mapc
    (lambda (filename)
      ;; disable, allow for any filename
      ;; (if (string-prefix-p org-xob--KB-filename-prefix filename))
      (add-to-list 'org-xob--KB-files (concat org-xob-dir filename)))
    (directory-files org-xob-dir nil "\.org$" t))
   (message "XOB: re-registered all KB files."))	
  (and 
   (org-id-update-id-locations)
   (message "XOB: updated org-id hashtable."))
  (message "XOB: traversing all KB files...")
  (let (ID title)
    (org-xob-visit-nodes 
     #'(lambda ()
         (setq ID (org-id-get (point)))
         (setq title (nth 4 (org-heading-components)))
         (puthash ID title org-xob--id-title)
         (puthash title ID org-xob--title-id))))
  (message "XOB: finished rebuilding xob hashtables.")
  (org-xob-stats)
  (org-xob--save-state)
  (message "XOB: saved xob state."))

;;;###autoload
(defun org-xob-stats ()
  "Give basic information about the xob system."
  (interactive)
  ;; popup window
  (display-buffer-use-some-window (get-buffer-create "XOB Stats")
                                  '(display-buffer-below-selected))
  (with-current-buffer "XOB Stats"
    (erase-buffer)
    (insert "XOB Statistics\n")
    (insert "--------------\n")
    (insert (concat "title-id-table entries:\t\t\t"
                    (number-to-string (hash-table-count org-xob--title-id)) "\n"))
    (insert (concat "id-title-table entries:\t\t\t"                       
                    (number-to-string (hash-table-count org-xob--id-node)) "\n"))
    (insert (concat "org-id entries:\t\t\t\t\t\t\t"                               
                    (number-to-string (hash-table-count org-id-locations)) "\n"))
    (insert (concat "KB files count:\t\t\t\t\t\t\t"
                    (number-to-string (length org-xob--KB-files)) "\n"))
    (insert (concat "current-KB-file:\t\t\t\t\t\t" org-xob--KB-file "\n"))))

;;; End

(provide 'org-xob)

;;; org-xob.el ends here
