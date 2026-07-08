;;; test-link-hook.el --- Tests for org-xob link hook -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for following id: links through the org-xob hook.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'org-xob-backend)

(ert-deftest org-xob-link-hook-opens-registered-id-link ()
  "Following a registered id: link opens the matching XOB edit node."
  (let* ((node-id "12345678-1234-1234-1234-123456789abc")
         (node-title "Linked XOB Node")
         (org-xob--id-title (make-hash-table :test 'equal))
         opened)
    (puthash node-id node-title org-xob--id-title)
    (with-temp-buffer
      (org-mode)
      (insert (format "[[id:%s][%s]]" node-id node-title))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'org-xob--edit-node)
                 (lambda (id title)
                   (setq opened (list id title)))))
        (should (org-xob--link-hook-fn))
        (should (equal opened (list node-id node-title)))))))

(provide 'test-link-hook)
;;; test-link-hook.el ends here
