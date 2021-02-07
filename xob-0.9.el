;;; xob-0.9.el --- next version                      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Will Rempel

;; Author: Will Rempel <Will@WillMBA>
;; Keywords: lisp, tools

(defun org-xob-in-xob-buffer (&rest body)
  (or (org-xob-buffer-p (current-buffer))
        (and (org-xob-buffer-p org-xob-last-buffer)
             (switch-to-buffer org-xob-last-buffer))
        (switch-to-buffer (setq org-xob-last-buffer
                                (org-xob--new-buffer)))))

(defun org-xob--edit-node (ID title)
  (interactive)
  (org-xob-with-xob-on
   (when (member ID orb-xob-edit-nodes) ;; todo better check, maybe deleted
     (org-xob--id-goto ID))
   (org-xob-in-xob-buffer)
   (save-window-excursion
     (save-excursion
       (org-id-goto ID)
       (org-copy-subtree)))
   (while (org-up-heading-safe))
   (org-forward-heading-same-level 1)
   (org-paste-subtree 1 nil nil 'remove)
   (org-back-to-heading)
   ;; (let ((ID (org-entry-get (point) ID))))
   (org-entry-put (point) "EDIT" ID)
   (org-entry-delete (point) "ID")
   (push ID org-xob-edit-nodes)))

(defun org-xob--new-buffer ()
  (let ((buf (get-buffer-create (concat "xob-" (length org-xob-buffers)))))
    (with-current-buffer buf
      (org-mode)
      (org-xob-mode 1))
    (push buf org-xob-buffers)
    buf))

(defvar org-xob-buffers () "List of active xob buffers.")
(defvar org-xob-edit-nodes () "List of active nodes in edit state.")

(defun org-xob-buffer-p (buf)
  (and (buffer-live-p buf)
       (equal major-mode 'org-mode)
       (bound-and-true-p org-xob-mode)))

(defun org-xob-map-buffers (func)
  (cl-loop for buf in org-xob-buffers
           do (funcall func))) ;; todo maybe collect

(defun org-xob-map-nodes (func)
  (org-with-wide-buffer
   (org-save-outline-visibility nil
     (goto-char (point-min))
     (while (not (eobp))
       (if (org-xob--is-node-p)
           (funcall func)
         (outline-next-heading))))))

(defun org-xob-map-all-nodes (func)
  (org-xob-map-buffers (org-xob-map-nodes func)))

(defun org-xob-id-goto (ID)
  (and (org-xob-buffer-p (current-buffer))
       ())
  ())

(defun org-xob-goto-buffer ()
  (interactive))