;;; org-xob.el --- advanced knowledge management system in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Willy Rempel  
;; Author: Willy Rempel <willy.rempel@acm.org>
;; URL: http://example.com/org-xob.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2") (dash))
;; Keywords: something

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Making the most of org-mode. Another attempt at an exo-brain inspired by: zettlekasten, wikis, roam, and all the other ways to organize ourselves. 

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

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
(require 'org-id)
(require 'org-ql)
;; (require 'org-ml)
(require 'cl-lib)

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

(defvar org-xob--id-node nil) 

;;;;; file variables
(defvar org-xob--dir "xob/" 
  "Core directory for exobrain system.")

(defvar org-xob-workspace "~/exobrain/" 
  "Directory for all exobrain files.")

(defvar org-xob-path (concat org-xob-workspace
                                  org-xob--dir)
  "Path to the core exobrain files.")

(defvar org-xob-max-KB-filesize 524288
  "Specifies the largest size the knowledge base org-mode files should grow to. Once the current file reaches the limit, a new file is created.")

(defvar org-xob--KB-files nil
  "List of all knowledge base files.")
;; (setq org-xob--KB-files nil)

(defvar org-xob--KB-file nil
  "The currently active KB file to store previous versions of nodes.")

(defvar org-xob--KB-filename-prefix "KB-file-"
  "suffix for KB filenames. A simple filecount value is appended for a new name")

(defvar org-xob--active-nodes nil
  "a-list of active nodes. Those that were extracted from the KB and into the workspace.")

(defvar org-xob-syncedp nil
  "Buffer local variable that indicates whether the current contents of a buffer have been synced with the Knowledge Base.")
;;;;; state

;; TODO clean
(defvar org-xob-current-context
  "The current, active buffer for adding context material.")

(cl-defstruct xob-state kb-count kb-current kb-files t-id-table-fn id-n-table-fn)
(setq xob (make-xob-state :kb-count 0 :t-id-table-fn "title-id-table" :id-n-table-fn "id-node-table"))

(defvar org-xob--objects '((org-xob--title-id . "title-id-table")
                                (org-xob--id-node . "id-node-table")
                                (org-xob--KB-files . "KB-files")
                                (org-xob--KB-file . "current-KB-file")))

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps.

(defvar org-xob-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "org-xob map"))
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

;;;;; capture variables

(defvar org-xob--auto-types '(("day" . a.day)
                                   ("session" . a.session)
                                   ("project" . a.project)
                                   ("log" . a.log)
                                   ("log personal" . a.log.life)
                                   ("log it tools" . a.log.it-tools)
                                   ("log tools" . a.log.tools)
                                   ("log project" . a.log.project)
                                   ("article" . n.bib.article)
                                   ("webpage" . n.bib.web)
                                   ("fast" . n.n)
                                   ("topic" . n.topic)
                                   ))

(defvar org-xob--templates
      '(("f" "fast" entry (file org-xob--KB-file)
         "* %(eval title)  :node:\n%?\n** backlinks :bl:"
         :exobrain-node t
         :ntype "node"
         :vid "0"
         ;; :immediate-finish t
         :empty-lines-after 1)
        ("ct" "today" entry (file org-xob--KB-file)
         "* %() :node:\n\n** backlinks :bl:"
         :exobrain-node t
         :immediate-finish t
         :ntype "context.day")))

;;;; Minor Mode

;;;###autoload
(define-minor-mode org-xob-minor-mode
  "Org-Exobrain Minor Mode."
  :lighter " xob"
  :keymap  (let ((map (make-sparse-keymap))) map)
  :group 'org-xob
  :require 'org-xob
  (progn 
    (unless (org-xob-on-p)
      (org-xob-start))
    (if org-xob-minor-mode
        (progn 
          (setq-local org-xob-syncedp nil)
          ;; (add-hook 'after-change-functions #'org-xob--sync-edits)
          ;; (add-hook 'after-change-functions (lambda () set (make-local-variable 'org-xob-syncedp nil 'APPEND 'LOCAL)))
          (setq-local org-id-extra-files 'org-xob--KB-files))
      ;; (remove-hook 'after-change-functions #'org-xob--sync-edits)
      )))

;;;; Commands
;;;;; Main Commands
;;;###autoload
(defun org-xob-start ()
  "Start the xob system: check if state files exist and load state or initialize new."
  (interactive)
  (setq org-xob-on-p t)
  (cl-loop for (k . v) in org-xob--objects
           do (if (file-exists-p (concat org-xob-path v))
                       (org-xob--load-object v k)
                     (progn 
                       (message "XOB: file %s missing, initializing new %s" v k)
                       (if (equal "org-xob--KB-files" (symbol-name k))
                           (set k ()))
                       (if (equal "org-xob--KB-file" (symbol-name k))
                           (set k (org-xob--new-KB-file))
                         (set k (make-hash-table
                                 :test 'equal
                                 :size org-xob--table-size))))))
  (unless (AND (org-xob-today)
               ;; TODO 
              (org-time= nil nil))
    (setq org-xob-today (org-xob--capture (org-insert-time-stamp (current-time) nil 'INACTIVE)))))

;;;###autoload
(defun org-xob-stop ()
  "Stop xob system: save all state and close active buffers."
  (interactive)
  (org-xob--save-state)
  ;; save/clode active buffers
  ;; maybe delete other objects
  (setq org-xob-on-p nil))

;; TODO
;;;###autoload
(defun org-xob-open-day ()
  "Open todays node."
  (interactive)
  (unless (org-xob-on-p)
      (org-xob-start))
  ;; (org-xob-)
  ;; open buffer for a selected day node
  ;; can open yesterdays file as well? 
  ;; is exobrain started? 
  )

;;;###autoload
(defun org-xob-get-node ()
  "Focus on a node for editing. If it does not exist, create it."
  (interactive)
  (when (not org-xob-on-p)
    (org-xob-start))
  ;; Get node by title, or create new one 
  (helm :buffer "*xob get node*"
        :sources (helm-build-sync-source "xob-kb"
                   :candidates (lambda ()
                                 (let* ((cans (hash-table-keys org-xob--title-id)))
                                   (cons helm-input candidates)))
                   :volatile t
                   :action (lambda (title) (let ((ID (gethash title org-xob--title-id)))
                                             (unless ID
                                               (setq ID (org-xob--capture title)))
                                             (if (eq arg 4) 
                                                 (org-xob--activate-node ID)
                                               (org-xob--edit-node ID title)))))))

;; TODO finish and/or superlink
;; TODO do I change hash table name?
;; TODO do I call this differently? 
;;;###autoload
(defun org-xob-link ()
  "Inserts a properly formatted xob node link at point."
  (interactive)
  (when (not org-xob-on-p)
    (org-xob-start))
  (org-insert-link nil (concat "ID:" ID) (org-xob--ID-title ID))
  ;; call find-node 
  ;; if not there, make new node
  ;; make link 
  ;; is backlink already there?
  ;; else make backlink 
  ;; add entry to forelink tree, source plist
  )

;;;###autoload
(defun org-xob-toggle-sideline ()
  "Toggles display of the contextual side window."
  (interactive)
  (if org-xob--sideline-window 
      (progn 
        (delete-window org-xob--sideline-window)
        (setq org-xob--sideline-window nil))
    (progn 
      (setq org-xob--sideline-window 
            (split-window-right))
      (select-window org-xob--sideline-window)
      (display-buffer-same-window org-xob--context-buffer nil))))

;; TODO
;;;###autoload
(defun org-xob-heading-to-node ()
  "Convenience function to convert current content into xob KB nodes."
  (interactive)
  (unless (org-xob--is-node-p)
    ;; TODO get heading title and call modified make node (capture)
    ;; want to leave it in place
    ))

;; TODO
;;;###autoload
(defun org-xob-remove-node ()
  "Converts node item at point to a heading.
Simply removes heading ID from the hash tables."
  (interactive)
  (when (not org-xob-on-p)
    (org-xob-start))
  ;; TODO remove from other tables
  ;; delete node
  ;; delete context footprint
  ;; for each exo-link in body, visit node and remove backlink
  ;; for each exo-link in backlinks, visite node and kill link, leave link text
  (remhash (org-id-get (point)) org-xob--id-node))

;;;;; KB Context Commands

;; TEST
;;;###autoload
(defun org-xob-show-backlinks ()
  "Show backlinks contents, including subheading content."
  (interactive)
  (save-window-excursion
    (org-xob--node-get-links org-xob--source-backlinks)
    (org-xob--source-build org-xob--source-backlinks)))

;; TEST
;;;###autoload
(defun org-xob-show-forlinks ()
  (interactive)
  (save-window-excursion
    (org-xob--node-get-links org-xob--source-forlinks)
    (org-xob--source-build org-xob--source-forlinks)))

;; TODO
;;;###autoload
(defun org-xob-ql-search ()
  "Use org-ql to search the KB. Creates a new source in the context buffer."
  (interactive))

;;;;; Context Presentation Commands

;; TODO for all, should only do at a subheading, at source head do map 
;;;###autoload
(defun org-xob-clear-heading ()
  "Converts subtree to the headline and properties drawer only.
This is idempotent and application to such a heading makes no change.
This can be applied to heading at point or used in a mapping."
  (interactive)
  (org-with-wide-buffer
    (org-back-to-heading t)
    (org-mark-subtree)
    (org-end-of-meta-data t)
    (call-interactively #'delete-region)))

;; TEST 
;;;###autoload
(defun org-xob-to-summary ()
  "Show backlinks with summaries. This is defined as the first paragraph if it exists."
  (interactive)
  (save-excursion
    (org-xob-clear-heading)
    (org-end-of-meta-data 1)
    (insert
     (org-id-goto (org-entry-get (point) "PID"))
     (org-with-wide-buffer
      (org-end-of-meta-data 'full)
      (let ((p (org-element-at-point)))
        (buffer-substring-no-properties (org-element-property :contents-begin p)
                                        (org-element-property :contents-end p)))))))

;; TEST 
;;;###autoload
(defun org-xob-to-node-tree ()
  "Show only subheadings of node."
  (interactive)
  (org-end-of-meta-data 'full)
  (org-xob-clear-heading)
  (newline)
  (insert
   (let ((str))
     (org-with-wide-buffer
       ;; (org-id-goto (org-entry-get (point) "PID"))
       (org-narrow-to-subtree)
       (org-map-tree (lambda ()
                       (setq str (concat str 
                                         (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position)
                                          "\n"))))))
     str)))


;; TODO TEST 

;;;###autoload
(defun org-xob-to-section ()
  "Get the top section only, no subheadings."
  (interactive)
  (org-xob-clear-heading)
  (org-with-wide-buffer
   (org-id-goto (org-entry-get (point) "PID"))
   (org-end-of-meta-data t)
   ;; TODO fail, moves point 
   (let ((beg (progn (org-end-of-meta-data t)
                     (point)))
         (end (if (org-goto-first-child)
                  (progn
                    (previous-line)
                    (move-end-of-line nil)
                    (point)))))
     (copy-region-as-kill beg end)))
  (org-back-to-heading)
  (org-end-of-meta-data t)
  (yank)
  (pop kill-ring))

;;;###autoload
(defun org-xob-to-full-node ()
  "Converts node item at point to full node. The ID is still modified as
it is still a copy, however all other property drawer contents is unchanged."
  (interactive)
  (org-xob-clear-heading)
  (org-with-wide-buffer
    (org-id-goto (org-entry-get (point) "PID"))
    (org-mark-subtree)
    (org-end-of-meta-data t)
    (copy-region-as-kill 'REGION))
  (org-back-to-heading)
  (org-end-of-meta-data t)
  (yank)
  (pop kill-ring))

;;;; Backend
;;;;; Buffer Functions 

;; Parsing <- heading?

;; TODO local var on indirect buffer?

;; TODO recheck: probably do more for both

(defun org-xob--edit-node (ID title)
  "Create an indirect buffer of the node with name title."
  (setq org-xob-short-title (title (truncate-string-to-width title 12)))
  ;; (setq org-xob-node-buffer (get-buffer-create org-xob-short-title))
  ;; (set-buffer org-xob-node-buffer)
  ;; (org-mode)
  (setq org-xob-node-buffer (org-tree-to-indirect-buffer
                             (org-id-goto ID)))
  (org-xob-minor-mode 1)
  (set-window-buffer nil org-xob-node-buffer)
  (org-xob--make-context-buffer org-xob-short-title))

(defun org-xob--make-context-buffer (title)
  "Create context buffer, but leave it empty by default."
  (interactive)
  (setq org-xob--context-buffer (get-buffer-create (concat  "*context-" title)))
  (with-current-buffer org-xob--context-buffer
    (org-mode)))

;;;;; Contexts Functions

;; TODO unfinished - local or not? fill in blanks, test
(setq-local org-xob--source-backlinks
            '(:name "backlinks"
                    :tags ("KB")
                    :title nil 
                    :ID nil
                    :PID nil
                    :func org-xob--get-backlinks
                    :items nil))

(setq-local org-xob--source-forlinks
            '(:name "forlinks"
                    :tags ("KB")
                    :title nil 
                    :ID nil
                    :PID nil
                    :func org-xob--get-forlinks
                    :items nil))

;; TODO maybe instead use my hashtable to get heading titles
;; TODO refactor
(defun org-xob--node-get-links (source)
  "Populates sources item list from the node. The items are represented by their
respective node IDs. Two kinds of links are distinguished: backlinks and forlinks
which are all other links xob KB nodes. Assumes org-superlinks convention
where the backlinks are in a BACKLINKS drawer."
  ;; TODO window?
  (save-window-excursion
    ;; for name, test equal of not-equal
    (let* ((linktype (plist-get source :name))
           (test (if (equal linktype "backlinks")
                     (lambda (x) (x))
                   (if (equal linktype "forelinks")
                       (lambda (x) (not x))))))
      (org-id-goto (plist-get source :PID))
      (plist-put source :items
                 (org-element-map (org-element-parse-buffer) 'link
                   (lambda (link)
                     (if (funcall test (equal (org-element-property
                                               :drawer-name (cadr (org-element-lineage link)))
                                              "BACKLINKS"))
                         (org-element-property :path link))))))))
;; -----------------------


;; --source tree fns--
(defun org-xob--source-build (source)
  "Open a source tree for node mainID into the context buffer.
If it is already there, then refresh it. source items are shown as org headings.
source is a plist that describes the content source."
  (interactive)
  (save-window-excursion 
    (with-current-buffer org-xob--context-buffer
      ;; TODO ok?
      (if (not (member source org-xob--node-soures)))
      ;; TODO ok?
      (if (not (org-xob--goto-heading (plist-get source :PID)))
          (progn
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
            (cons source 'org-xob--node-sources)))
      (org-xob-refresh-source source))))

(defun org-xob--source-refresh (source)
  "Remake source tree. Check if items need to be added or removed.
todo - possibly refresh item contents if changes were made.
(this requires knowing what is displayed)"
  (let ((temp (copy-tree (plist-get source :items))))
    (org-xob--map-source
     (lambda ()
       (let ((pid (org-entry-get (point) "PID")))
         ;; TODO any quoting? is that best way to delete?
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
  (let ((title (gethash ID org-xob--id-node)))
    (if title 
        (save-excursion 
          (org-insert-subheading '(4))
          (org-edit-headline title)
          (org-entry-put (point) "PID" ID)
          (org-id-get-create 'FORCE)) ;; needed?
      (message "not a valid knowledge base ID: %s" ID))))

(defun org-xob--map-source (func &optional ID)
  "Apply the function func to every child-item of a xob source.
If the optional ID of a xob source is given, then apply func to that source.
Otherwise apply to source at point."
  (save-excursion
    (save-restriction 
      (if ID
          (org-id-goto ID))
      (if (org-xob--is-source-p) 
          (progn
            (org-narrow-to-subtree)
            (outline-show-all)
            (outline-next-heading)
            (while
                (progn 
                  (funcall func)
                  (outline-get-next-sibling))))
        (message "not a xob source.")))))

;; --predicates--
(defun org-xob--is-node-p (&optional ID)
  "Check if a heading is a xob node. Called interactively it defaults to heading at point.
If an ID argument is supplied, then check the heading associated with it."
  (interactive)
  (let ((temp (if ID ID
                (org-id-get nil))))
    (if temp
        (if (gethash temp org-xob--id-node) t nil)
      nil)))

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
(defun org-xob--insert-link-header (ID)
  "Inserts a subheading with an org link to the node."
  (condition-case nil
      (progn
        (org-insert-subheading (4))
        (org-edit-headline (org-xob--node-link ID)))
    (error (message "Node %s not found" ID))))

(defun org-xob--push-heading-link (ID target)
  "Inserts a subheading into target with a link to node ID as the title.
Used for activity material in day node."
  (save-window-excursion
    (condition-case nil 
        (progn 
          (org-id-goto target)
          (org-insert-subheading)
          (org-insert-link nil ID (node-title (org-xob--id-node ID))))
      (error (message "Node %s not found" target)))))


;;;;; Activity
;;;;;; Clocking
(defun org-xob--auto-clock-in ())
(defun org-xob--auto-clock-out ())
;;;;; Node Functions

(defun org-xob--new-node ()
  (when (org-capture-get :exobrain-node)
    (let ((ID (org-id-get-create))
          (type (org-capture-get :ntype))
          node)
      ;; (push (nth 4 (org-heading-components)) title)
      (org-entry-put (point) "VID" (org-capture-get :vid))
      (org-entry-put (point) "CREATED" (concat "["
                                               (format-time-string "%F %a %R")
                                               "]"))
      (org-entry-put (point) "TYPE" type)
      (setq node (make-node :title title
                            :type type 
                            :backlinks (list)))
      (puthash title ID org-xob--title-id)
      (puthash ID node org-xob--id-node)
      ID)))

(defun org-xob--capture (title)
  (let* ((org-capture-templates org-xob--templates)
         ID)
    ;; TODO test
    (if (member 'title 'org-xob--auto-types)
        (org-capture nil title)
      (org-capture))
    ID))

(add-hook 'org-capture-mode-hook #'org-xob--new-node)

;; since I have forelinks, clicking link usually means to attend to
(defun org-xob--link-hook-fn ()
  "If a link is a xob node, then reopen node in xob edit mode." 
  ;; TODO if on xob node, then open as edit
  ;; call get node or --edit-node
  (let ((ID (org-id-get (point) nil nil)))
    (if (org-xob--id-node ID)
        (org-xob--activate-node ID))))

(add-hook 'org-follow-link-hook #'org-xob--link-hook-fn)


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
  "Iterate over all KB in all files"
  (interactive)
  ;; (maphash '#func org-id-locations)
  ;; (maphash '#func org-xob--title-id)
  )

(defun org-xob--save-state ()
  "Save exobrain state."
  (if (not (file-directory-p org-xob--workspace))
      (make-directory org-xob--workspace))
  (if (not (file-directory-p org-xob-path))
      (make-directory org-xob-path))
  (cl-loop for (k . v) in org-xob--objects
           do (org-xob--save-object (concat org-xob-path v) k))
  (org-xob-sync))

(defun org-xob--load-state ()
  "Load exobrain state."
  (cl-loop for (k . v) in org-xob--objects
           do (org-xob--load-object (concat org-xob-path v) k)))

(defun org-xob--save-object (file data)
  "save emacs object. "
  (with-temp-file file
    (prin1 (symbol-value data) (current-buffer))))

(defun org-xob--load-object (file symbol)
  "load saved object."
  (when (boundp symbol)
    (with-temp-buffer
      (insert-file-contents (concat org-xob-path file))
      (goto-char (point-min))
      (set symbol (read (current-buffer))))))

(defun org-xob--new-KB-file ()
  "Create new KB file for next node in the brain."
  (interactive)
  (let ((filename (concat 
                   org-xob--KB-filename-prefix
                   (format "%03d" (xob-state-kb-count xob))
                   ".org")))
    (with-temp-buffer
      (write-file filename))
    (cl-pushnew filename (xob-state-kb-files xob))
    (setf (xob-state-kb-current xob) filename)
    (setf (xob-state-kb-count xob) (+ 1))))

(defun org-xob-rebuild ()
  "Traverse all nodes and correct any meta errors."
  (interactive)
  nil
  )

;;; df
;; (defmacro org-xob--)
;;; End

(provide 'org-xob)

;;; org-xob.el ends here
