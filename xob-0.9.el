;;; xob-0.9.el --- next version                      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Will Rempel

;; Author: Will Rempel <Will@WillMBA>
;; Keywords: lisp, tools

;;; my implementation tryout
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

;; ---- can be redone with org-ql ---
(defun org-xob-map-buffers (func)
  (cl-loop for buf in org-xob-buffers
           do (with-current-buffer buf (funcall func)))) ;; todo maybe collect

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
  (let ((place (make-marker)))
    (org-xob-map-buffers
     (lambda (x) (set-marker place (or (org-find-property "PID" ID)
                                       (org-find-property "COPY" ID)
                                       (org-find-property "EDIT" ID)))))
    (when place
      (set-buffer (marker-buffer place))
      (goto-char (place))
      (setq place nil))))

;; (let ((check (lambda (x) (or (org-entry-get (point) "PID")
;;                                (org-entry-get (point) "COPY")
;;                                (org-entry-get (point) "EDIT"))))))
;;   (and (org-xob-buffer-p (current-buffer))
;;        (or (string= ID check)))
;;   ())

(defun org-xob-find-in-buffers (prop val)
  (cl-loop for buf in org-xob-buffers
           until (let (place)
                   (with-current-buffer buf
                     (if (setq place (org-find-property prop val))
                         (list place buf)
                       nil)))))

(defun org-xob-find-any-in-buffers ())

(defun org-xob-find-all-in-buffers (ID)
  (cl-loop for buf in org-xob-buffers
           collect (let (place)
                     (with-current-buffer buf
                       (org-with-wide-buffer
                        (goto-char (point-min))
                        (cl-loop until (eobp)
                                 collect (when (re-search-forward ID nil t nil)
                                           (org-back-to-heading)
                                           (point))))))))

(defun org-xob-if-apply-in-buffers (cond func)
  (cl-loop for buf in org-xob-buffers
           do (let (place)
                (with-current-buffer buf
                  (org-with-wide-buffer
                   (goto-char (point-min))
                   (org-xob-next-node)
                   (when cond (funcall func)))))))

(defun org-xob-next-node ()
  (unless (and (org-at-heading-p)
               (org-entry-get (point) "xob"))
    (outline-next-heading)))

(defun org-xob-update-copies (ID)
  (org-xob-if-apply-in-buffers
   (lambda () (string= ID (org-entry-get (point) "COPY")))
   (lambda () (let ((shows (org-entry-get (point) "show")))
                (cond
                 ((string= "s" shows)
                  (org-xob-to-summary))
                 ((string= "S" shows)
                  (org-xob-to-section))
                 ((string= "t" shows)
                  (org-xob-to-node-tree))
                 ((string= "f" shows)
                  (org-xob-to-full-node)))))))

;; (pcase-let ((`(,ID ,title) (org-xob--get-create-node))))

;; TODO might not work, since loop continues. use until clause in cl-loop in map-buffers
;; or maybe have more than one map fn, or optional args
(defun org-xob-find-source (ID)
  (let (place))
  (org-xob-map-buffers
   (lambda (x) (org-find-property "PID" ID))))

(defun org-xob-find-edit-node (ID)
  (org-xob-map-buffers
   (lambda (x) (org-find-property "EDIT" ID))))

(defun org-xob-goto-buffer ()
  (interactive))

;; NO more than one
;; (defun org-xob-find-copy (ID)
;;   (org-xob-map-buffers
;;    (lambda (x) (org-find-property "COPY" ID))))

;;; org-ql implementation

(require 'org-ql-search-block)
(require 'org-ql-search)
(require 'org-ql)
