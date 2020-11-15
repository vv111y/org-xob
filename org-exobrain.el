;;; org-exobrain.el --- advanced knowledge management system in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Willy Rempel  
;; Author: Willy Rempel <willy.rempel@acm.org>
;; URL: http://example.com/org-exobrain.el
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

;; (require 'org-exobrain)

;;;; Usage

;; Run one of these commands:

;; `org-exobrain-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `org-exobrain' group.

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
(require 'org-ml)
(require 'cl-lib)

;;;; Customization

(defgroup org-exobrain nil
  "Settings for `org-exobrain'."
  :link '(url-link "http://example.com/org-exobrain.el"))

(defcustom org-exobrain-something nil
  "This setting does something."
  :type 'something)

;;;; Variables

(defvar org-exobrain-on-p nil)
;;;;; hash tables 

(defvar org-exobrain--table-size 1000000
  "Size of the hash tables.")

(defvar org-exobrain--title-id nil)

(defvar org-exobrain--id-node nil) 

;;;;; file variables
(defvar org-exobrain--dir "xob/" 
  "Core directory for exobrain system.")

(defvar org-exobrain-workspace "~/exobrain/" 
  "Directory for all exobrain files.")

(defvar org-exobrain-path (concat org-exobrain-workspace
                                  org-exobrain--dir)
  "Path to the core exobrain files.")

(defvar org-exobrain-max-KB-filesize 524288
  "Specifies the largest size the knowledge base org-mode files should grow to. Once the current file reaches the limit, a new file is created.")

(defvar org-exobrain--KB-files nil
  "List of all knowledge base files.")
;; (setq org-exobrain--KB-files nil)

(defvar org-exobrain--KB-file nil
  "The currently active KB file to store previous versions of nodes.")

(defvar org-exobrain--KB-filename-prefix "KB-file-"
  "suffix for KB filenames. A simple filecount value is appended for a new name")

(defvar org-exobrain--active-nodes nil
  "a-list of active nodes. Those that were extracted from the KB and into the workspace.")

(defvar org-exobrain-syncedp nil
  "Buffer local variable that indicates whether the current contents of a buffer have been synced with the Knowledge Base.")
;;;;; state

;; TODO clean
(cl-defstruct xob-state kb-count kb-current kb-files t-id-table-fn id-n-table-fn)
(setq xob (make-xob-state :kb-count 0 :t-id-table-fn "title-id-table" :id-n-table-fn "id-node-table"))

(defvar org-exobrain--objects '((org-exobrain--title-id . "title-id-table")
                                (org-exobrain--id-node . "id-node-table")
                                (org-exobrain--KB-files . "KB-files")
                                (org-exobrain--KB-file . "current-KB-file")))

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps.

(defvar org-exobrain-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "org-exobrain map"))
        (maps (list
               ;; Mappings go here, e.g.:
               "RET" #'org-exobrain-RET-command
               [remap search-forward] #'org-exobrain-search-forward
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))

;;;;; Frontend
;;;;; Commands

;;;###autoload
(defun org-exobrain-start ()
  "Start the xob system: check if state files exist and load state or initialize new."
  (interactive)
  (setq org-exobrain-on-p t)
  (cl-loop for (k . v) in org-exobrain--objects
           do (if (file-exists-p (concat org-exobrain-path v))
                       (org-exobrain--load-object v k)
                     (progn 
                       (message "XOB: file %s missing, initializing new %s" v k)
                       (if (equal "org-exobrain--KB-files" (symbol-name k))
                           (set k ()))
                       (if (equal "org-exobrain--KB-file" (symbol-name k))
                           (set k (org-exobrain--new-KB-file))
                         (set k (make-hash-table
                                 :test 'equal
                                 :size org-exobrain--table-size)))))))

(defun org-exobrain-stop ()
  "Stop xob system: save all state and close active buffers."
  (interactive)
  (org-exobrain--save-state)
  ;; save/clode active buffers
  ;; maybe delete other objects
  (setq org-exobrain-on-p nil))

;;;###autoload
(defun org-exobrain-open-day ()
  (interactive)
  (when (not org-exobrain-on-p)
      (org-exobrain-start))
  ;; open buffer for a selected day node
  ;; can open yesterdays file as well? 
  ;; is exobrain started? 
  )

;;;###autoload
(defun org-exobrain-get-node ()
  (interactive)
  (when (not org-exobrain-on-p)
    (org-exobrain-start))
  (unless org-exobrain-today
    (setq org-exobrain-today (org-exobrain--capture "today")))
  ;; Get node by title, or create new one 
  (helm :buffer "*xob get node*"
        :sources (helm-build-sync-source "vv-sss"
                   :candidates (lambda ()
                                 (let* ((cans (hash-table-keys org-exobrain--title-id)))
                                   (cons helm-input candidates)))
                   :volatile t
                   :action (lambda (title) (let ((ID (gethash title org-exobrain--title-id)))
                                             (unless ID
                                               (setq ID (org-exobrain--capture title)))
                                             (org-exobrain--activate-node ID))))))

;;;###autoload
(defun org-exobrain-link ()
  "Insert node exo-link here."
  (interactive)
  (when (not org-exobrain-on-p)
    (org-exobrain-start))
  ;; call find-node 
  ;; make link 
  ;; is backlink already there?
  ;; else make backlink 
  )

;;;###autoload
(defun org-exobrain-clone-node ()
  (interactive)
  (when (not org-exobrain-on-p)
    (org-exobrain-start))
  ;; is exobrain started? 
  ;; if point not on node, call get-node
  ;; copy whole node
  ;; generate new ID
  ;; add vparent property, and/or exo-link to parent
  ;; parent backlink has subheading 'clones' 
  )

;;;###autoload
(defun org-exobrain-delete-node ()
  (interactive)
  (when (not org-exobrain-on-p)
    (org-exobrain-start))
  ;; delete node
  ;; delete context footprint
  ;; for each exo-link in body, visit node and remove backlink
  ;; for each exo-link in backlinks, visite node and kill link, leave link text
  nil
  )

;;;###autoload
(defun org-exobrain-heading-to-node ()
  (interactive)
  (when (not org-exobrain-on-p)
    (org-exobrain-start))
  )

;;;###autoload
(defun org-exobrain-sync ()
  "Update KB from all active nodes."
  (interactive)
  (when (not org-exobrain-on-p)
    (org-exobrain-start))
  (dolist (buf (remove-if-not org-exobrain-syncedp
                              (org-exobrain--active-buffers)))
    (if
        (mapc #'org-exobrain--sync-node (org-exobrain--nodes-in-buffer buf))
        (set (make-local-variable 'org-exobrain-syncedp) t)
      (progn
        (set (make-local-variable 'org-exobrain-syncedp) nil)
        (message (format "sync failed for buffer %s" buf))))))


;;;;;; Workspace
;;;###autoload
(define-minor-mode org-exobrain-minor-mode
  "Minor Mode "
  :lighter " Xo"
  ;; :keymap  (let ((map (make-sparse-keymap)))
  ;;            (define-key map [remap org-store-link] 'org-roam-store-link)
  ;;            map)
  :group 'org-exobrain
  :require 'org-exobrain
  ;; :global t
  (progn 
    (when (not org-exobrain-on-p)
      (org-exobrain-start))
    (setq-local org-exobrain-syncedp nil)
    (add-hook 'after-change-functions (lambda () set (make-local-variable 'org-exobrain-syncedp nil 'APPEND 'LOCAL)))
    ;; TODO check it works
    (setq-local org-id-extra-files 'org-exobrain--KB-files))

;;;;; Support
;;;;;; Node Objects
(cl-defstruct node title type backlinks)

(defun org-exobrain-push--heading-link (ID target)
 (save-window-excursion
   (org-exobrain--activate-node target)
   (org-insert-subheading (org-insert-link nil ID (node-title (org-exobrain--id-node ID))))))

(defun org-exobrain--link-hijack ()
  "After following org-id link, jump to the activated node, creating it if necessary."
  (let ((ID (org-id-get (point) nil nil)))
    (if (org-exobrain--id-node ID)
        (org-exobrain--activate-node ID))))

(add-hook 'org-follow-link-hook #'org-exobrain--link-hijack)

(defun org-exobrain--activate-node (ID)
  "Activate the node. If it is already live, display it or go to it's window."
  (let* ((m (org-id-find ID 'marker))
         (anode (org-exobrain--id-node ID))
         (buf-name (concat (node-title anode) "-" (format-time-string "%F" ) ".org"))
         (buf-win (get-buffer-window buf-name)))
    (if buf-win 
        (select-window buf-win)
      (if (get-buffer buf-name)
          (switch-to-buffer buf-name)
        (progn 
          (org-exobrain-push--heading-link ID org-exobrain-today)
          (save-window-excursion
            (org-id-goto ID)
            (org-copy-subtree))
          (switch-to-buffer buf-name) 
          (unless org-mode 
            (org-mode))
          (unless org-exobrain-minor-mode 
            (org-exobrain-minor-mode))
          (org-paste-subtree nil nil nil 'remove))))))

;;;;;; Node org-capture



;;;;;; Node Versioning
(defun org-exobrain--sync-node (node)
  "Update entry based on local edits."
  ;; is node in KB? no, add, else
  ;; is node different? no, ignore, else sync/update
  nil
  )


(defun org-exobrain--active-buffers ()
  "Returns list of all active exobrain buffers"
  (remove-if-not (lambda (buf) (with-current-buffer buf org-exobrain)) (buffer-list)))

(defun org-exobrain--nodes-in-buffer (buff)
  ;; traverse headings, check if node, append ID to list
  nil 
  )

;;;;;; Contexts

(defun org-exobrain-context--inline ()
  "Show the contextual nodes as a subheading."
  )

(defun org-exobrain-context--sideline ()
  "Show the contextual nodes in an adjacent buffer & window."
  )

(defun org-exobrain-context--outline ()
  "Show the contextual nodes as adjacent headings."
  )

(defun org-exobrain-refresh-sources ()
  "Show the contextual nodes as adjacent headings."
  )

;;;;;; Parsing
;;;;;; Clocking
(defun org-exobrain--auto-clock-in ())
(defun org-exobrain--auto-clock-out ())
;;;;; Backend
;;;;;; Node Objects
(cl-defstruct node title type backlinks)

(defun org-exobrain-push--heading-link (ID target)
 (save-window-excursion
   (org-exobrain--activate-node target)
   (org-insert-subheading (org-insert-link nil ID (node-title (org-exobrain--id-node ID))))))

(defun org-exobrain--link-hijack ()
  "After following org-id link, jump to the activated node, creating it if necessary."
  (let ((ID (org-id-get (point) nil nil)))
    (if (org-exobrain--id-node ID)
        (org-exobrain--activate-node ID))))

(add-hook 'org-follow-link-hook #'org-exobrain--link-hijack)

(defun org-exobrain--activate-node (ID)
  "Activate the node. If it is already live, display it or go to it's window."
  (let* ((m (org-id-find ID 'marker))
         (anode (org-exobrain--id-node ID))
         (buf-name (concat (node-title anode) "-" (format-time-string "%F" ) ".org"))
         (buf-win (get-buffer-window buf-name)))
    (if buf-win 
        (select-window buf-win)
      (if (get-buffer buf-name)
          (switch-to-buffer buf-name)
        (progn 
          (org-exobrain-push--heading-link ID org-exobrain-today)
          (save-window-excursion
            (org-id-goto ID)
            (org-copy-subtree))
          (switch-to-buffer buf-name) 
          (unless org-mode 
            (org-mode))
          (unless org-exobrain-minor-mode 
            (org-exobrain-minor-mode))
          (org-paste-subtree nil nil nil 'remove))))))

;;;;;; Node org-capture


;;;;;; Node links
;;;;;; Node Versioning
;; (defun org-exobrain--sync-node (node)
;;   "Update entry based on local edits."
;;   ;; is node in KB? no, add, else
;;   ;; is node different? no, ignore, else sync/update
;;   nil
;;   )

;; (defun org-exobrain--diff-node (now-node last-node)
;;   "Creates a diff using =org-exobrain--delta-executable=.
;; The order of versions is reversed; the diff allows the reconstruction of
;; the last-node from the now-node.
;; The diff is stored in the currently active =org-exobrain--KB-file=."
;;   (shell-command))
;; ;; (defun org-exobrain--new-node-diff (nodeID)
;; ;;   (let ((old-id (org-id-store-link node)))))

;; (defun org-exobrain--diff-filename (node)
;;   (concat
;;    ;; node id
;;    "-"
;;    (format-time-string "%j-%H-%M")))



;;;;;; KB Traversal

;;;;;; exobrain management
(defun org-exobrain-visit-nodes (func)
  "Iterate over all KB in all files"
  (interactive)
  ;; (maphash '#func org-id-locations)
  ;; (maphash '#func org-exobrain--title-id)
  )

(defun org-exobrain--save-state ()
  "Save exobrain state."
  (if (not (file-directory-p org-exobrain--workspace))
      (make-directory org-exobrain--workspace))
  (if (not (file-directory-p org-exobrain-path))
      (make-directory org-exobrain-path))
  (cl-loop for (k . v) in org-exobrain--objects
           do (org-exobrain--save-object (concat org-exobrain-path v) k))
  (org-exobrain-sync))

(defun org-exobrain--load-state ()
  "Load exobrain state."
  (cl-loop for (k . v) in org-exobrain--objects
           do (org-exobrain--load-object (concat org-exobrain-path v) k)))

(defun org-exobrain--save-object (file data)
  "save emacs object. "
  (with-temp-file file
    (prin1 (symbol-value data) (current-buffer))))

(defun org-exobrain--load-object (file symbol)
  "load saved object."
  (when (boundp symbol)
    (with-temp-buffer
      (insert-file-contents (concat org-exobrain-path file))
      (goto-char (point-min))
      (set symbol (read (current-buffer))))))

(defun org-exobrain--new-KB-file ()
  "Create new KB file for next node in the brain."
  (interactive)
  (let ((filename (concat 
                   org-exobrain--KB-filename-prefix
                   (format "%03d" (xob-state-kb-count xob))
                   ".org")))
    (with-temp-buffer
      (write-file filename))
    (cl-pushnew filename (xob-state-kb-files xob))
    (setf (xob-state-kb-current xob) filename)
    (setf (xob-state-kb-count xob) (+ 1))))

;;;;;; diagnostics

(defun org-exobrain-rebuild ()
  "Traverse all nodes and correct any meta errors."
  (interactive)
  nil
  )

;;; End

(provide 'org-exobrain)

;;; org-exobrain.el ends here
