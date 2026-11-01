;;; tui-load-path.el --- Reuse CI's installed test dependencies -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Willy Rempel
;; Assisted-by: OpenAI Codex
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(let ((paths (with-temp-buffer
               (insert-file-contents (or (getenv "ORG_XOB_TEST_LOAD_PATH_FILE")
                                         (error "Missing test load-path file")))
               (read (current-buffer)))))
  (unless (and (listp paths) (cl-every (lambda (p) (or (null p) (stringp p))) paths))
    (error "Invalid test load path"))
  (setq load-path paths))
