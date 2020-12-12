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

;;;; Frontend
;;;;; Minor Mode
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

;;;;; Commands

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

;;;###autoload
(defun org-xob-link ()
  "Insert node exo-link here."
  (interactive)
  (when (not org-xob-on-p)
    (org-xob-start))
  ;; call find-node 
  ;; make link 
  ;; is backlink already there?
  ;; else make backlink 
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

;;;###autoload
(defun org-xob-clone-node ()
  (interactive)
  (when (not org-xob-on-p)
    (org-xob-start))
  ;; is exobrain started? 
  ;; if point not on node, call get-node
  ;; copy whole node
  ;; generate new ID
  ;; add vparent property, and/or exo-link to parent
  ;; parent backlink has subheading 'clones' 
  )

;;;###autoload
(defun org-xob-delete-node ()
  (interactive)
  (when (not org-xob-on-p)
    (org-xob-start))
  ;; delete node
  ;; delete context footprint
  ;; for each exo-link in body, visit node and remove backlink
  ;; for each exo-link in backlinks, visite node and kill link, leave link text
  nil
  )

;;;###autoload
(defun org-xob-heading-to-node ()
  (interactive)
  (when (not org-xob-on-p)
    (org-xob-start))
  )

;;;###autoload
(defun org-xob-show-backlinks (ID)
  "Load the backlinks tree for node ID into the context buffer, unless it is already there."
  (interactive)
  (save-window-excursion 
    (with-current-buffer org-xob--context-buffer
      ;; TODO this won't work if I just delete heading
      (unless (cdr (org-xob-node-backlinks)) 
        ;; TODO better choice
        (goto-char (point-min))
        (org-insert-heading (4) 'invisible-ok 'TOP)
        (insert org-xob-short-title)
        (setq org-xob-backlinks-tree (org-id-get-create))
        (org-toggle-tag "backlinks" 'ON)
        (setq org-xob-node-backlinks (cons (org-xob--get-backlinks ID)
                                           (org-xob-backlinks-tree)))
        (org-xob-update-context org-xob-node-backlinks)))))


;;;;; Buffer functions 
;; Parsing <- heading?

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


;;;;; Contexts

;; TODO
(defun org-xob-update-context (source)
  (interactive)
  (mapcar (lambda (ID)
            ) (car source)))

(defun org-xob--get-backlinks (ID)
  "Return backlinks as a list of IDs. Assumes org-superlinks convention where the backlinks are in a drawer named BACKLINKS."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (if (equal (org-element-property
                  :drawer-name (cadr (org-element-lineage link)))
                 "BACKLINKS")
          (org-element-property :path link)))))

(defun org-xob-show-forlinks ()
  (interactive))

(defun org-xob-context--inline ()
  "Show the contextual nodes as a subheading."
  )

(defun org-xob-context--outline ()
  "Show the contextual nodes as adjacent headings."
  )

(defun org-xob-refresh-sources ()
  "Show the contextual nodes as adjacent headings."
  )

;;;;; Clocking
(defun org-xob--auto-clock-in ())
(defun org-xob--auto-clock-out ())
;;;; Backend

;;;;; Node Objects
(cl-defstruct node title type backlinks)

;; TODO do I change hash table?
;; TODO do I call this differently? 
(defun org-xob--node-link (ID)
  "Inserts a properly formatted org link"
  (org-insert-link nil (concat "ID:" ID) (org-xob--ID-title ID)))

(defun org-xob--node-header (ID)
  "Inserts a subheading with title of the node."
  (org-insert-subheading (4))
  (org-edit-headline (org-xob--ID-title ID)))

(defun org-xob--node-link-header (ID)
  "Inserts a subheading with an org link to the node."
  (org-insert-subheading (4))
  (org-edit-headline (org-xob--node-link ID)))

(defun org-xob--node-summary (ID))

;; TODO maybe replace activate, now that indirect buffer being used
(defun org-xob--node-full (ID)
  "Inserts the full node as a subheading."
  (save-window-excursion
    (org-id-goto ID)
    (org-copy-subtree))
  (org-paste-subtree nil nil nil 'REMOVE)
  (org-entry-put (point) "PARENT" 
                 (org-entry-get (point) "ID" nil nil))
  (org-xob--node-add-timed-property "MODIFIED")
  (org-toggle-tag "A" 'ON)
  (org-id-get-create 'FORCE))

(defun org-xob--node-add-time-property (property)
  (org-entry-put (point) property
                 (number-to-string
                  (car (time-convert (current-time) '10000)))))

(defun org-xob-push--heading-link (ID target)
 (save-window-excursion
   (org-xob--activate-node target)
   (org-insert-subheading (org-insert-link nil ID (node-title (org-xob--id-node ID))))))

(defun org-xob--link-hijack ()
  "After following org-id link, jump to the activated node, creating it if necessary."
  (let ((ID (org-id-get (point) nil nil)))
    (if (org-xob--id-node ID)
        (org-xob--activate-node ID))))

;; (add-hook 'org-follow-link-hook #'org-xob--link-hijack)

;; TODO replace?
(defun org-xob--activate-node (ID)
  "Copies KB node with ID to current location and sets
appropriate properties as a derivative node."
  ;; TODO make main buffer, and whatever else for sync-editing style
  (org-xob--node-full))

(defun org-xob--sync-edits (beg end len)
  (goto-char beg)
  ;; (if org-xob--kb-node-p)
  (if (member "kb" (org-get-tags))
      (org-xob--sync-node)))

(defun org-xob--sync-node ()
  )

;;;;;; Node org-capture

(defun org-xob--capture (title)
  (let* ((org-capture-templates org-xob--templates)
         ID)
    ;; TODO test
    (if (member 'title 'org-xob--auto-types)
        (org-capture nil title)
      (org-capture))
    ID))

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

(add-hook 'org-capture-mode-hook #'org-xob--new-node)

;;;;;; Node links


;;;;;; Node Versioning


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



;;;;;; KB Traversal

;;;;;; exobrain management
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

;;;;;; diagnostics

(defun org-xob-rebuild ()
  "Traverse all nodes and correct any meta errors."
  (interactive)
  nil
  )

;;; End

(provide 'org-xob)

;;; org-xob.el ends here
