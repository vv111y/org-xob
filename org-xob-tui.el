;;; org-xob-tui.el --- Floating XOB context for Emacs 31.1+ -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Willy Rempel
;; Assisted-by: OpenAI Codex
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Optional child-frame presentation.  Load XOB explicitly, then enable
;; `org-xob-tui-mode'.  This module never starts XOB or changes its files.
;; All toolkit sessions are opaque; only public emacs-tui APIs are used.

;;; Code:

(require 'org-xob-context)
(require 'emacs-tui)
(require 'face-remap)

(defvar org-xob-context-display-function)
(defvar org-xob--id-title)
(defvar org-xob-buffers)
(defvar org-xob-mode-hook)
(defvar org-xob--pair-buf)
(declare-function org-xob-buffer-p "org-xob-backend" (&optional buffer))
(declare-function org-xob--edit-node "org-xob-backend" (id title))

(defgroup org-xob-tui nil
  "Floating context for XOB."
  :group 'org)

(defface org-xob-tui-text '((t (:inherit shadow)))
  "Subdued body and list text in XOB child frames."
  :group 'org-xob-tui)

(defcustom org-xob-tui-text-scale 0.9
  "Relative graphical text size.  Terminal font sizes remain unchanged."
  :type 'number :group 'org-xob-tui)

(defcustom org-xob-tui-panels
  '((backlinks north-west 32 9) (forlinks north-east 32 9) (preview south-east 54 14))
  "Initial (ID POSITION WIDTH HEIGHT) descriptions for the three child frames."
  :type '(repeat (list symbol symbol integer integer)) :group 'org-xob-tui)

(defcustom org-xob-tui-refresh-delay 0.25
  "Idle seconds before visible context follows edits or a different node."
  :type 'number :group 'org-xob-tui)

(defvar org-xob-tui-mode nil)
(defvar org-xob-tui-context-mode nil)
(defvar org-xob-tui--previous-renderer nil)
(defvar-local org-xob-tui--sessions nil)
(defvar-local org-xob-tui--snapshot nil)
(defvar-local org-xob-tui--preview nil)
(defvar-local org-xob-tui--pinned nil)
(defvar-local org-xob-tui--active 'forlinks)
(defvar-local org-xob-tui--positions nil)
(defvar-local org-xob-tui--selections nil)
(defvar-local org-xob-tui--expanded nil)
(defvar-local org-xob-tui--timer nil)
(defvar-local org-xob-tui--tick nil)
(defvar-local org-xob-tui--busy nil)

(defun org-xob-tui--check ()
  "Check the required runtime and child-frame display before changing UI."
  (when (version< emacs-version "31.1")
    (user-error "XOB popups require Emacs 31.1 or newer"))
  (unless (emacs-tui-popup-supported-p)
    (user-error "This display does not support child frames; no windows were split")))

(defun org-xob-tui--style ()
  "Apply local styling to a newly created popup view."
  (face-remap-add-relative 'default 'org-xob-tui-text
                          :height (if (display-graphic-p) org-xob-tui-text-scale 1.0))
  (face-remap-add-relative 'header-line
                          :foreground (face-foreground 'shadow nil t)
                          :background (face-background 'default nil t)
                          :box nil :weight 'normal)
  (face-remap-add-relative 'emacs-tui-selection
                          :foreground (face-foreground 'shadow nil t)
                          :background (face-background 'default nil t)
                          :weight 'normal :underline t)
  (setq-local truncate-lines nil word-wrap t))

(defun org-xob-tui--session (panel)
  "Return PANEL's live opaque session, or nil."
  (let ((session (alist-get panel org-xob-tui--sessions)))
    (and (emacs-tui-live-p session) session)))

(defun org-xob-tui--remember (panel session)
  "Remember PANEL's selected identity before disposing SESSION."
  (when (and (memq panel '(backlinks forlinks)) (emacs-tui-live-p session))
    (setf (alist-get panel org-xob-tui--selections)
          (plist-get (emacs-tui-current-item session panel) :id))))

(defun org-xob-tui--hide (panel)
  "Hide PANEL, retaining its placement and selected identity."
  (let ((session (alist-get panel org-xob-tui--sessions)))
    (org-xob-tui--remember panel session)
    (setq org-xob-tui--sessions (assq-delete-all panel org-xob-tui--sessions))
    (when session (emacs-tui-close session))))

(defun org-xob-tui-hide-all ()
  "Hide every XOB child frame in this source buffer."
  (interactive)
  (when (timerp org-xob-tui--timer) (cancel-timer org-xob-tui--timer))
  (setq org-xob-tui--timer nil)
  (let ((panels (mapcar #'car org-xob-tui--sessions)) failure)
    (dolist (panel panels)
      (condition-case err (org-xob-tui--hide panel)
        ((error quit) (unless failure (setq failure err)))))
    (when failure (signal (car failure) (cdr failure)))))

(defun org-xob-tui--preview-text ()
  "Return the current preview's clean text, including its title."
  (let* ((id (or org-xob-tui--pinned org-xob-tui--preview
                 (plist-get org-xob-tui--snapshot :id)))
         (snapshot (and id (org-xob-context-snapshot id (current-buffer)))))
    (if (not snapshot) "Select a linked node to preview it."
      (concat (propertize (plist-get snapshot :title) 'face 'bold) "\n\n"
              (if (string-empty-p (plist-get snapshot :text))
                  "No body text." (plist-get snapshot :text))))))

(defun org-xob-tui--title (panel)
  "Return a concise title for PANEL."
  (if (eq panel 'preview)
      (if org-xob-tui--pinned "Preview · pinned" "Preview")
    (format "%s (%d)" (if (eq panel 'backlinks) "Backlinks" "Forward links")
            (length (plist-get org-xob-tui--snapshot
                               (if (eq panel 'backlinks) :backlinks :forlinks))))))

(defun org-xob-tui--items (panel)
  "Return the current snapshot's items for PANEL."
  (plist-get org-xob-tui--snapshot (if (eq panel 'backlinks) :backlinks :forlinks)))

(defun org-xob-tui--update-preview ()
  "Refresh an already visible preview without revealing hidden panels."
  (when-let* ((session (org-xob-tui--session 'preview)))
    (emacs-tui-update session 'preview :title (org-xob-tui--title 'preview)
                      :text (org-xob-tui--preview-text))))

(defun org-xob-tui--selected (panel item)
  "Select ITEM from PANEL for read-only preview."
  (setq org-xob-tui--active panel org-xob-tui--preview (plist-get item :id))
  (setf (alist-get panel org-xob-tui--selections) org-xob-tui--preview)
  (org-xob-tui--update-preview))

(defun org-xob-tui--show (panel)
  "Show PANEL in a child frame, leaving the source window intact."
  (unless (org-xob-tui--session panel)
    (org-xob-tui--check)
    (let* ((source (current-buffer))
           (spec (assq panel org-xob-tui-panels))
           (expanded (memq panel org-xob-tui--expanded))
           (emacs-tui-view-mode-hook (cons #'org-xob-tui--style emacs-tui-view-mode-hook))
           session ready)
      (unless spec (user-error "No popup configuration for %s" panel))
      (unwind-protect
          (progn
            (setq session
                  (emacs-tui-popup
                   :view (if (eq panel 'preview)
                             (emacs-tui-view :id panel :title (org-xob-tui--title panel)
                                             :text (org-xob-tui--preview-text))
                           (emacs-tui-view
                            :id panel :title (org-xob-tui--title panel)
                            :items (org-xob-tui--items panel)
                            :on-select (lambda (_session item)
                                         (when (buffer-live-p source)
                                           (with-current-buffer source
                                             (org-xob-tui--selected panel item))))))
                   :position (or (alist-get panel org-xob-tui--positions) (nth 1 spec))
                   :width (+ (nth 2 spec) (if expanded 16 0))
                   :height (+ (nth 3 spec) (if expanded 6 0))
                   :on-close
                   (lambda (closed)
                     (when (buffer-live-p source)
                       (with-current-buffer source
                         (when (eq closed (alist-get panel org-xob-tui--sessions))
                           (setq org-xob-tui--sessions
                                 (assq-delete-all panel org-xob-tui--sessions))))))))
            (unless (and (eq source (current-buffer)) (emacs-tui-live-p session))
              (user-error "Popup source changed during creation"))
            (when-let* ((id (and (not (eq panel 'preview))
                               (alist-get panel org-xob-tui--selections))))
              (when (cl-find id (org-xob-tui--items panel)
                             :key (lambda (x) (plist-get x :id)) :test #'equal)
                (emacs-tui-set-selection session panel id)))
            (setf (alist-get panel org-xob-tui--sessions) session)
            (setq ready t))
        (unless ready (when session (emacs-tui-close session)))))))

(defun org-xob-tui--read ()
  "Read context for the current source node, resetting selection on node changes."
  (let ((id (org-xob-context-node-at-point)))
    (unless id (user-error "Point is not inside an indexed XOB node"))
    (unless (equal id (plist-get org-xob-tui--snapshot :id))
      (setq org-xob-tui--selections nil org-xob-tui--preview nil
            org-xob-tui--pinned nil))
    (setq org-xob-tui--snapshot (org-xob-context-snapshot id (current-buffer))
          org-xob-tui--tick (buffer-chars-modified-tick))))

(defun org-xob-tui-refresh ()
  "Refresh visible context from the current node and its unsaved edits."
  (interactive)
  (let ((org-xob-tui--busy t))
    (org-xob-tui--read)
    (dolist (panel '(backlinks forlinks))
      (when-let* ((session (org-xob-tui--session panel)))
        (emacs-tui-update session panel :title (org-xob-tui--title panel)
                          :items (org-xob-tui--items panel))))
    (org-xob-tui--update-preview)))

(defun org-xob-tui--idle (buffer)
  "Refresh BUFFER only if it still owns the selected source window."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq org-xob-tui--timer nil)
      (when (and org-xob-tui-context-mode (eq buffer (window-buffer (selected-window)))
                 org-xob-tui--sessions)
        (condition-case err (org-xob-tui-refresh)
          (error (org-xob-tui-hide-all) (message "XOB context: %s" (error-message-string err))))))))

(defun org-xob-tui--after-command ()
  "Schedule a bounded idle refresh when the visible source context changes."
  (when (and org-xob-tui--sessions (not org-xob-tui--busy)
             (or (not (equal org-xob-tui--tick (buffer-chars-modified-tick)))
                 (not (equal (org-xob-context-node-at-point)
                             (plist-get org-xob-tui--snapshot :id)))))
    (when (timerp org-xob-tui--timer) (cancel-timer org-xob-tui--timer))
    (setq org-xob-tui--timer
          (run-with-idle-timer org-xob-tui-refresh-delay nil #'org-xob-tui--idle
                              (current-buffer)))))

(defun org-xob-tui-toggle (panel)
  "Toggle PANEL, or all three panels when PANEL is `all'."
  (interactive (list (intern (completing-read "Panel: " '(backlinks forlinks preview all) nil t))))
  (unless org-xob-tui-mode (user-error "Enable org-xob-tui-mode first"))
  (unless (org-xob-buffer-p) (user-error "Use this command in an XOB editing buffer"))
  (org-xob-tui-context-mode 1)
  (if (if (eq panel 'all) org-xob-tui--sessions (org-xob-tui--session panel))
      (if (eq panel 'all) (org-xob-tui-hide-all) (org-xob-tui--hide panel))
    (org-xob-tui--read)
    (let (opened ready)
      (unwind-protect
          (progn
            (dolist (id (if (eq panel 'all) '(backlinks forlinks preview) (list panel)))
              (org-xob-tui--show id)
              (push id opened))
            (setq ready t))
        (unless ready (dolist (id opened) (org-xob-tui--hide id)))))))

(defun org-xob-tui-cycle-panel ()
  "Choose which visible panel receives navigation and positioning commands."
  (interactive)
  (let* ((ids (cl-remove-if-not #'org-xob-tui--session '(backlinks forlinks preview)))
         (tail (cdr (memq org-xob-tui--active ids))))
    (unless ids (user-error "Show a context panel first"))
    (setq org-xob-tui--active (or (car tail) (car ids)))
    (message "XOB active panel: %s" org-xob-tui--active)))

(defun org-xob-tui-next (&optional count)
  "Move COUNT rows in the active list, or scroll the preview by COUNT lines."
  (interactive "p")
  (let ((session (org-xob-tui--session org-xob-tui--active)))
    (unless session (user-error "Show the active panel or use C-c x TAB to select one"))
    (if (eq org-xob-tui--active 'preview)
        (emacs-tui-scroll-view session 'preview (or count 1))
      (emacs-tui-next-item session (or count 1))
      (org-xob-tui--selected org-xob-tui--active (emacs-tui-current-item session)))))

(defun org-xob-tui-previous (&optional count)
  "Move backward COUNT rows or preview lines."
  (interactive "p") (org-xob-tui-next (- (or count 1))))

(defun org-xob-tui-pin-preview ()
  "Pin the preview to its node, or release it to follow list selection."
  (interactive)
  (setq org-xob-tui--pinned
        (unless org-xob-tui--pinned
          (or org-xob-tui--preview (plist-get org-xob-tui--snapshot :id))))
  (org-xob-tui--update-preview))

(defun org-xob-tui-reposition ()
  "Cycle the active popup through the eight positions around its anchor."
  (interactive)
  (let ((session (org-xob-tui--session org-xob-tui--active)))
    (unless session (user-error "Show the active panel first"))
    (setf (alist-get org-xob-tui--active org-xob-tui--positions)
          (emacs-tui-reposition-popup session))))

(defun org-xob-tui-resize ()
  "Toggle the active child frame between its normal and enlarged size."
  (interactive)
  (let* ((id org-xob-tui--active) (session (org-xob-tui--session id))
         (spec (assq id org-xob-tui-panels)) (expand (not (memq id org-xob-tui--expanded))))
    (unless session (user-error "Show the active panel first"))
    (emacs-tui-resize-popup session (+ (nth 2 spec) (if expand 16 0))
                            (+ (nth 3 spec) (if expand 6 0)))
    (setq org-xob-tui--expanded (if expand (cons id org-xob-tui--expanded)
                                (remq id org-xob-tui--expanded)))))

(defun org-xob-tui-open-selected ()
  "Explicitly open the active selection using XOB's normal editing workflow."
  (interactive)
  (let* ((panel org-xob-tui--active) (session (org-xob-tui--session panel))
         (id (if (eq panel 'preview)
                 (or org-xob-tui--pinned org-xob-tui--preview
                     (plist-get org-xob-tui--snapshot :id))
               (and session (plist-get (emacs-tui-current-item session) :id))))
         (snapshot (and id (org-xob-context-snapshot id (current-buffer)))))
    (unless (and id (gethash id org-xob--id-title) (plist-get snapshot :available))
      (user-error "No available XOB node is selected"))
    (org-xob-tui-hide-all)
    (org-xob--edit-node id (plist-get snapshot :title))))

(defvar org-xob-tui-context-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding '(("1" . backlinks) ("2" . forlinks) ("3" . preview) ("0" . all)))
      (let ((panel (cdr binding)))
        (define-key map (kbd (concat "C-c x " (car binding)))
                    (lambda () (interactive) (org-xob-tui-toggle panel)))))
    (dolist (binding '(("TAB" . org-xob-tui-cycle-panel) ("n" . org-xob-tui-next)
                       ("p" . org-xob-tui-previous) ("m" . org-xob-tui-reposition)
                       ("e" . org-xob-tui-resize) ("d" . org-xob-tui-pin-preview)
                       ("g" . org-xob-tui-refresh) ("q" . org-xob-tui-hide-all)
                       ("RET" . org-xob-tui-open-selected)))
      (define-key map (kbd (concat "C-c x " (car binding))) (cdr binding)))
    map)
  "Source-buffer controls; ordinary typing and Org bindings are retained.")

(define-minor-mode org-xob-tui-context-mode
  "Buffer-local controls and cleanup for XOB floating context."
  :lighter " XPop" :keymap org-xob-tui-context-mode-map
  (if org-xob-tui-context-mode
      (progn
        (add-hook 'post-command-hook #'org-xob-tui--after-command nil t)
        (add-hook 'kill-buffer-hook #'org-xob-tui-hide-all nil t)
        (add-hook 'change-major-mode-hook #'org-xob-tui-hide-all nil t))
    (remove-hook 'post-command-hook #'org-xob-tui--after-command t)
    (remove-hook 'kill-buffer-hook #'org-xob-tui-hide-all t)
    (remove-hook 'change-major-mode-hook #'org-xob-tui-hide-all t)
    (org-xob-tui-hide-all)))

(defun org-xob-tui--attach ()
  "Attach source controls when an XOB edit buffer is initialized."
  (when (and org-xob-tui-mode (bound-and-true-p org-xob-mode))
    (org-xob-tui-context-mode 1)))

(defun org-xob-tui--display (source &optional _refresh)
  "Render context SOURCE through child frames instead of Org headings."
  (unless (memq source '(backlinks forlinks refresh)) (user-error "Unknown context source: %s" source))
  (org-xob-tui-context-mode 1)
  (org-xob-tui-refresh)
  (unless (eq source 'refresh)
    (org-xob-tui--show source)
    (org-xob-tui--show 'preview)))

(defun org-xob-tui--close-legacy-panes ()
  "Remove paired context windows only where the editor remains visible."
  (dolist (buffer org-xob-buffers)
    (when (buffer-live-p buffer)
      (let ((context (with-current-buffer buffer
                       (and (boundp 'org-xob--pair-buf) org-xob--pair-buf))))
        (when (buffer-live-p context)
          (dolist (window (get-buffer-window-list context nil t))
            (when (get-buffer-window buffer (window-frame window))
              (delete-window window))))))))

;;;###autoload
(define-minor-mode org-xob-tui-mode
  "Use toggled child frames for XOB context, without splitting source windows.
Requires Emacs 31.1+ and native child-frame support.  XOB must already be
loaded explicitly; enabling this mode never starts a repository."
  :global t :group 'org-xob-tui
  (if org-xob-tui-mode
      (condition-case err
          (progn
            (org-xob-tui--check)
            (unless (featurep 'org-xob) (user-error "Load org-xob explicitly first"))
            (unless (eq org-xob-context-display-function #'org-xob-tui--display)
              (setq org-xob-tui--previous-renderer org-xob-context-display-function))
            (setq org-xob-context-display-function #'org-xob-tui--display)
            (add-hook 'org-xob-mode-hook #'org-xob-tui--attach)
            (dolist (buffer org-xob-buffers)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (org-xob-tui--attach))))
            (org-xob-tui--close-legacy-panes))
        ((error quit)
         (org-xob-tui-mode -1)
         (signal (car err) (cdr err))))
    (remove-hook 'org-xob-mode-hook #'org-xob-tui--attach)
    (when (and (boundp 'org-xob-context-display-function)
               (eq org-xob-context-display-function #'org-xob-tui--display))
      (setq org-xob-context-display-function org-xob-tui--previous-renderer))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when org-xob-tui-context-mode (org-xob-tui-context-mode -1))))))

(provide 'org-xob-tui)
;;; org-xob-tui.el ends here
