;;; tests/test_1.el --- simple loader for CI -*- lexical-binding: t; -*-

;; CI-friendly loader: add the repository root to load-path and require org-xob.
;; This replaces the old developer-local absolute-path references.

(let ((project-root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path project-root))

(require 'org-xob)

(provide 'test-1)
