;;; org-xob-context.el --- Read-only XOB context snapshots -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Willy Rempel
;; Assisted-by: OpenAI Codex
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Read node data without opening an editor, exporting Org, evaluating Babel,
;; changing the kill ring, or writing context headings into application buffers.
;; XOB must already be loaded and its index initialized by the caller.

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-element)
(require 'cl-lib)
(require 'subr-x)

(defvar org-xob--id-title)
(defvar org-xob-buffers)

(defun org-xob-context-node-at-point ()
  "Return the nearest indexed node ID at point, including unsaved edit nodes."
  (when (and (derived-mode-p 'org-mode)
             (boundp 'org-xob--id-title) (hash-table-p org-xob--id-title))
    (save-excursion
      (save-restriction
        (widen)
        (unless (org-before-first-heading-p)
          (org-back-to-heading t)
          (catch 'found
            (while t
              (let ((id (or (org-entry-get nil "EDIT") (org-entry-get nil "ID"))))
                (when (and id (gethash id org-xob--id-title)) (throw 'found id)))
              (unless (org-up-heading-safe) (throw 'found nil)))))))))

(defun org-xob-context--marker-in-buffer (buffer id property)
  "Find ID as PROPERTY in BUFFER without changing its point or restriction."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (catch 'found
              (while (re-search-forward org-heading-regexp nil t)
                (when (equal id (org-entry-get nil property))
                  (beginning-of-line)
                  (throw 'found (point-marker)))))))))))

(defun org-xob-context--find (id preferred)
  "Resolve ID, preferring unsaved edits in PREFERRED and active XOB buffers.
Only the existing Org ID index is consulted for unopened original files."
  (let ((buffers (delete-dups
                  (delq nil (cons preferred
                                  (and (boundp 'org-xob-buffers)
                                       (copy-sequence org-xob-buffers)))))))
    (or (cl-some (lambda (b) (org-xob-context--marker-in-buffer b id "EDIT")) buffers)
        (cl-some (lambda (b) (org-xob-context--marker-in-buffer b id "ID")) buffers)
        (let ((file (and (hash-table-p org-id-locations) (gethash id org-id-locations))))
          (when (and (stringp file) (or (get-file-buffer file) (file-readable-p file)))
            (org-xob-context--marker-in-buffer
             (or (get-file-buffer file) (find-file-noselect file)) id "ID"))))))

(defun org-xob-context--label (id &optional fallback)
  "Return a single-line label for ID, using FALLBACK for unresolved nodes."
  (replace-regexp-in-string
   "[\n\r\t]+" " "
   (or (and (hash-table-p org-xob--id-title) (gethash id org-xob--id-title))
       fallback "Unavailable node")))

(defun org-xob-context--render (element)
  "Render Org ELEMENT as display text, omitting storage metadata.
This is a deliberately small presentation renderer, not an Org exporter."
  (concat (org-xob-context--render-element element)
          (when (and (not (stringp element))
                     (memq (org-element-type element)
                           '(link bold italic underline strike-through code verbatim
                                  entity latex-fragment)))
            (make-string (or (org-element-property :post-blank element) 0) ?\s))))

(defun org-xob-context--render-element (element)
  "Render ELEMENT itself, without its following object whitespace."
  (if (stringp element)
      (substring-no-properties element)
    (let* ((type (org-element-type element))
           (children (lambda () (mapconcat #'org-xob-context--render
                                           (org-element-contents element) "")))
           (value (org-element-property :value element)))
      (pcase type
        ((or 'property-drawer 'node-property 'drawer 'planning 'keyword
             'comment 'comment-block 'babel-call) "")
        ('headline
         (concat (propertize
                  (mapconcat #'org-xob-context--render
                             (org-element-property :title element) "")
                  'face 'bold)
                 "\n" (funcall children)))
        ('link
         (let* ((id (org-element-property :path element))
                (internal (member (org-element-property :type element) '("id" "xob")))
                (label (if (org-element-contents element) (funcall children)
                         (if internal (org-xob-context--label id) id))))
           (propertize label 'org-xob-node-id (and internal id))))
        ((or 'bold 'italic 'underline 'strike-through)
         (propertize (funcall children) 'face (if (eq type 'strike-through)
                                                '(:strike-through t) type)))
        ((or 'code 'verbatim) (propertize (or value "") 'face 'fixed-pitch))
        ('entity (or (org-element-property :utf-8 element) ""))
        ('line-break "\n")
        ('paragraph (concat (string-trim-right (funcall children)) "\n\n"))
        ('item (concat "• " (string-trim (funcall children)) "\n"))
        ('plain-list (concat (funcall children) "\n"))
        ('table-row
         (if (eq (org-element-property :type element) 'rule) ""
           (concat (mapconcat (lambda (cell)
                                (string-trim (org-xob-context--render cell)))
                              (org-element-contents element) " | ") "\n")))
        ((or 'src-block 'example-block 'fixed-width)
         (concat (propertize (or value "") 'face 'fixed-pitch) "\n"))
        ('horizontal-rule "────────\n")
        (_ (if (org-element-contents element) (funcall children)
             (if (stringp value) value "")))))))

(defun org-xob-context-snapshot (id &optional preferred-buffer)
  "Return read-only display data for ID, preferring PREFERRED-BUFFER edits.
The plist contains :id, :title, :available, :text, :backlinks and :forlinks.
Link items contain :id and :label; IDs, not titles, identify nodes.  Missing
nodes return an unavailable snapshot.  No global ID rescan is performed."
  (unless (and (stringp id) (boundp 'org-xob--id-title)
               (hash-table-p org-xob--id-title))
    (user-error "XOB's node index must be initialized"))
  (let ((marker (org-xob-context--find id preferred-buffer))
        (snapshot (list :id id :title (org-xob-context--label id) :available nil
                        :text "This node is unavailable in the current XOB index.")))
    (unwind-protect
        (if (not marker) snapshot
          (with-current-buffer (marker-buffer marker)
            (save-excursion
              (save-restriction
                (widen)
                (goto-char marker)
                (org-narrow-to-subtree)
                (let* ((tree (org-element-parse-buffer))
                       (root (car (org-element-contents tree))) backlinks forlinks)
                  (org-element-map tree 'link
                    (lambda (link)
                      (when (member (org-element-property :type link) '("id" "xob"))
                        (let* ((target (org-element-property :path link))
                               (drawer (org-element-lineage link '(drawer)))
                               (item (list :id target :label
                                           (org-xob-context--label
                                            target (when (org-element-contents link)
                                                     (mapconcat #'org-xob-context--render
                                                                (org-element-contents link) ""))))))
                          (if (equal (org-element-property :drawer-name drawer) "BACKLINKS")
                              (cl-pushnew item backlinks :key (lambda (x) (plist-get x :id))
                                          :test #'equal)
                            (unless drawer
                              (cl-pushnew item forlinks :key (lambda (x) (plist-get x :id))
                                          :test #'equal)))))))
                  (setq snapshot
                        (list :id id :available t
                              :title (string-trim
                                      (mapconcat #'org-xob-context--render
                                                 (org-element-property :title root) ""))
                              :text (string-trim
                                     (mapconcat #'org-xob-context--render
                                                (org-element-contents root) ""))
                              :backlinks (nreverse backlinks) :forlinks (nreverse forlinks))))))))
      (when marker (set-marker marker nil)))
    snapshot))

(provide 'org-xob-context)
;;; org-xob-context.el ends here
