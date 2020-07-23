;;; org-exobrain.el --- advanced knowledge management system in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Willy Rempel  

;; Author: Willy Rempel <willy.rempel@acm.org>
;; URL: http://example.com/org-exobrain.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
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

(require 'org-mode)
;; (require 'bar)

;;;; Customization

(defgroup org-exobrain nil
  "Settings for `org-exobrain'."
  :link '(url-link "http://example.com/org-exobrain.el"))

(defcustom org-exobrain-something nil
  "This setting does something."
  :type 'something)

;;;; Variables

(defvar org-exobrain-var nil
  "A variable.")

(defvar org-exobrain-max-KB-filesize 524288
  "Specifies the largest size the knowledge base org-mode files should grow to. Once the current file reaches the limit, a new file is created.")

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

;;;; Functions
;;;;; org-exobrain minor mode 
;;;###autoload
(define-minor-mode org-exobrain
  "Minor Mode "
  :lighter " Xo"
  ;; :keymap  (let ((map (make-sparse-keymap)))
  ;;            (define-key map [remap org-store-link] 'org-roam-store-link)
  ;;            map)
  :group 'org-exobrain
  :require 'org-exobrain
  :global t
  ;; ()
  )

;;;;; Commands

;;;###autoload
(defun org-exobrain-now ()
  (interactive)
  ;; open NOW buffer
  ;; maybe last day stuff?
  ;;  
  )

;;;;;; Node Objects
;;;###autoload
(defun org-exobrain-add-node ()
  (interactive)
  ()
  (org-id-get-create)
  )

;;;###autoload
(defun org-exobrain-delete-node ()
  (interactive)
  ;; delete node
  ;; delete context footprint
  )

;;;###autoload
(defun org-exobrain-heading-to-node ()
  (interactive)
  )

;;;;;; Node Versioning
;;;###autoload
(defun org-exobrain-sync ()
  "Update KB from all active nodes."
  (interactive)
  (when (buffer-list))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when org-exobrain-minor-mode 
        (when (buffer-modified-p)
          (mapc 'org-exobrain-sync-node
                (org-exo-brain-nodes-in-buffer buf)))))))


(defun org-exobrain--nodes-in-buffer (buff)
  ;; traverse headings, check if node, append ID to list

  )

(defun org-exobrain--sync-node (node)
  "Update entry based on local edits."
  (interactive)
  )

;; (defun org-exobrain--new-node-diff (nodeID)
;;   (let ((old-id (org-id-store-link node)))

;;     )
;;   )

(defun org-exobrain--diff-filename (node)
  (concat
   ;; node id
   "-"
   (format-time-string "%j-%H-%M")))


;;;;; Support
;;;;;; Parsing
(defun org-exobrain-)
;;;;;; Clocking
;; (defun org-exobrain--auto-clock-in ())
;; (defun org-exobrain--auto-clock-out ())
;;;;;; KB Traversal

(defun org-exobrain-)
;;;;;; KB Management
(defun org-exobrain-rebuild ()
  "Traverse all nodes and correct any meta errors."
  (interactive)
  )

(defun org-exobrain--select-location ()
  "Determine location for next node in the brain files."
  )

(defun org-exobrain--new-KB-file ()
  "Determine location for next node in the brain files."
  )



;;;; Footer

(provide 'org-exobrain)

;;; org-exobrain.el ends here
