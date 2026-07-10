;;; org-xob-backend.el --- Advanced knowledge management system in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Willy Rempel
;; Author: Willy Rempel <willy.rempel@acm.org>
;; URL: https://github.com/vv111y/org-xob.el
;; Version: 0.5-pre
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines

;; This file is not part of GNU Emacs.

;;; Commentary:
;; backend for org-xob.el

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

(require 'org-xob-core)

;;;; Backend
;;;;; Buffer Functions

(defun org-xob-buffer-p (&optional buffer)
  (let ((buf (or buffer (current-buffer))))
    (if (buffer-live-p buf)
        (with-current-buffer buf
          (and (bound-and-true-p org-xob-mode)
               (eq major-mode 'org-mode)
               (member buf org-xob-buffers))))))

(defun org-xob-edit-buffer-p (&optional buffer)
  (let ((buf (or buffer (current-buffer))))
    (and (org-xob-buffer-p buf)
         (with-current-buffer buf
           (eq org-xob--buf 'parent)))))

;;;###autoload
(defun org-xob-new-buffer (&optional single)
  "Create new xob buffer. Defaults to dual-pane buffer pair."
  (interactive)
  (when-let ((numbufs (number-to-string
                       (length org-xob-buffers)))
             (session-name (concat "xob-"
                                   numbufs
                                   "-"
                                   (format-time-string "%F") ".org"))
             (session-path (concat org-xob-dir "/" session-name))
             (buf1 (if (file-exists-p session-path)
                       (find-file session-path)
                     ;; Create a new buffer and ensure it's empty
                     (with-current-buffer (get-buffer-create session-name)
                       (erase-buffer)
                       (current-buffer))))
             (buf2 (with-current-buffer (get-buffer-create
                                         (concat "xob-C-" numbufs))
                     (erase-buffer)
                     (current-buffer))))
    (with-current-buffer buf1
      (org-mode)
      (org-xob-mode 1)
      (auto-save-mode -1)
      (add-hook 'kill-buffer-hook #'org-xob--close-buffer-hook nil 'local)
      (setq-local org-xob--buf 'parent
                  org-xob--pair-buf buf2
                  buffer-offer-save nil)
      (if single
          (progn (setq-local org-xob--display 'single)
                 (setq-local org-xob--c-buff (current-buffer)))
        (setq-local org-xob--display 'dual
                    org-xob--c-buff buf2)))
    (with-current-buffer buf2
      (org-mode)
      (org-xob-context-mode 1)
      (setq-local org-xob--buf 'child
                  org-xob--pair-buf buf1
                  buffer-offer-save nil))
    ;; (setq org-xob-last-buffer buf1)
    (push buf1 org-xob-buffers)
    (push buf1 org-xob-all-buffers)
    (push buf2 org-xob-all-buffers)
    buf1))

(defun org-xob--close-buffer-hook ()
  "Properly close xob buffer: remove it from org-xob-buffers, kill context buffer."
  (let ((buf (current-buffer)))
    (call-interactively #'org-xob-close-node '(4))
    (org-xob-refresh-open-nodes)
    (setq org-xob-buffers (cl-delete buf org-xob-buffers))
    (setq org-xob-all-buffers (cl-delete buf org-xob-all-buffers))
    (setq org-xob-all-buffers (cl-delete org-xob--pair-buf org-xob-all-buffers))
    (if (eq buf org-xob-last-buffer)
        (setq org-xob-last-buffer (car-safe org-xob-buffers)))
    (if (buffer-live-p org-xob--pair-buf)
        (kill-buffer org-xob--pair-buf))))

;; NOTE keep? not being used
(defun org-xob--id-create ()
  "Create a UUID formatted ID. org-id will not work with buffers that are
not visiting a file. This function is meant for such a case. Use in conjunction
with org-xob--id-goto to return to this heading.
Returns ID if successful, nil otherwise."
  (let ((ID (uuidgen-4)))
    (org-entry-put (point) "ID" ID)
    ID))

(defun org-xob--id-goto (ID)
  (let (m)
    (when (and (org-not-nil ID)
               org-xob-all-buffers)
      (setq m
            (car
             (org-ql-select org-xob-all-buffers
               `(or (property "ID" ,ID)
                    (property "EDIT" ,ID)
                    (property "PID" ,ID))
               :action '(point-marker))))
      (when (markerp m)
        (set-buffer (marker-buffer m))
        (goto-char m)
        (point)))))

(defun org-xob--goto-edit (ID)
  (let (m)
    (when (and (org-not-nil ID)
               org-xob-buffers)
      (setq m
            (car
             (org-ql-select org-xob-buffers
               `(and (property "EDIT" ,ID)
                     t) ;; (tags "edit"))
               :action '(point-marker))))
      (when (markerp m)
        (set-buffer (marker-buffer m))
        (goto-char m)
        (point)))))

(defun org-xob--goto-buffer-heading (ID)
  "Find heading with ID in current buffer. If found then return point at
heading beginning, else nil."
  (when-let ((m (car (org-ql-select (current-buffer)
                       `(property "ID" ,ID)
                       :action '(point)))))
    (goto-char m)))

(defun org-xob--close-buffers ()
  "Close all open xob buffers. First sync any outstanding edits to the
knowledge base."
  (let ((current-prefix-arg 4))
    (funcall-interactively #'org-xob-sync-edit))
  (dolist (buf org-xob-buffers)
    (if (buffer-live-p buf)
        (kill-buffer buf))))

;; TODO
;; (defun org-xob--rewrite-buffer-1-pane ()
;;   "Rewrites all open nodes in single pane format."
;;   )

;; TODO
;; (defun org-xob--rewrite-buffer-2-pane ()
;;   "Rewrites all open nodes in dual pane format."
;;   )

;; WIP
(defun org-xob--rewrite-buffer-1-pane ()
  "Rewrites all open nodes in single pane format."
  (org-map-entries
   (lambda ()
     (when (org-xob--is-edit-node-p)
       (let ((pid (org-entry-get (point) "EDIT")))
         (org-xob--edit-write-single-pane pid (gethash pid org-xob--id-title)))))
   "EDIT<>\"\"" 'file))

;; WIP
(defun org-xob--rewrite-buffer-2-pane ()
  "Rewrites all open nodes in dual pane format."
  (org-map-entries
   (lambda ()
     (when (org-xob--is-edit-node-p)
       (let ((pid (org-entry-get (point) "EDIT")))
         (org-xob--edit-write-dual-pane pid (gethash pid org-xob--id-title)))))
   "EDIT<>\"\"" 'file))

;;;;; Windows

(defun org-xob--single-pane (win)
  "Use single pane interface. If dual-pane is open, then kill
the windows."
  (when (window-atom-root win)
    (when-let* ((buf (if (org-xob-buffer-p)
                         (window-buffer win)
                       org-xob-last-buffer))
                (oldwin (selected-window))
                (winnew (split-window-right)))
      (delete-window oldwin)
      (select-window winnew)
      (set-buffer buf)
      (setq org-xob--display 'single))))

(defun org-xob--dual-pane (win)
  "Use dual-pane interface."
  (when-let* ((buf1 (switch-to-buffer
                     org-xob-last-buffer))
              (buf2 (buffer-local-value 'org-xob--pair-buf
                                        buf1))
              (win1 (selected-window))
              (win2 (split-window-right)))
    (with-selected-window win2
      (switch-to-buffer buf2))
    (setq org-xob--display 'dual)))


;;;;; Edit Node Functions

;; TODO fails if there are other edit buffers open
;;;###autoload
(defun org-xob-refresh-open-nodes ()
  "Clear out any incorrect or closed entries of open nodes."
  (interactive)
  (let ((onodes (org-xob-map-all-edits
                 #'(lambda ()
                     (org-entry-get (point) "EDIT")))))
    (setq org-xob--open-nodes
          (remove nil
                  (mapcar #'(lambda (x)
                              (if (and (open-node-p x)
                                       (member (open-node-ID x)
                                               onodes))
                                  x))
                          org-xob--open-nodes)))))

(defun org-xob--get-open-node (id)
  "Return the open node object associated with id or nil if it is not found."
  (cl-find-if #'(lambda (x) (string= x id))
              org-xob--open-nodes
              :key #'(lambda (x) (open-node-ID x))))

(defun org-xob--get-open-node-ids ()
  "Return list of open node ids."
  (mapcar #'(lambda (x) (open-node-ID x))
          org-xob--open-nodes))

(defun org-xob--edit-write-single-pane (ID title)
  "write a node for editing in single pane mode. Point is assumed to be in
the correct location."
  (org-xob--edit-write
   #'(lambda ()
       (message "[xob] Writing single-pane edit node in buffer: %s" (buffer-name))
       (insert "* " title) ;; Removed :edit: tag
       (insert (org-xob--select-content
                ID
                #'(lambda () (org-xob--get-full-node 2 'meta)))))))

(defun org-xob--edit-write-dual-pane (ID title)
  "write a node for editing in dual pane mode. Point is assumed to be in
the correct location."
  (org-xob--edit-write
   #'(lambda ()
       (insert (org-xob--select-content
                ID
                #'(lambda () (org-xob--get-full-node 1 'meta)))))))

(defun org-xob--edit-write (func)
  "change tag and properties for newly written edit node."
  (goto-char (point-max))
  (newline)
  (save-excursion (funcall func))
  (org-xob--mod-to-edit-node)
  (org-flag-subtree t))

(defun org-xob--mod-to-edit-node (&optional refresh)
  ;; (org-set-tags "edit")
  (org-entry-put (point) "EDIT" (org-entry-get (point) "ID"))
  (unless refresh
    (org-entry-put (point) "ID" (uuidgen-4))))

(defun org-xob--edit-node (ID title)
  "Open node for editing. Selects the last current xob buffer, if none are
found, then create a new one. Defaults to dual-pane display, with C-u opens node
in a single-pane display format."
  ;; check if node already open
  (if (member ID (org-xob--get-open-node-ids))
      (progn (org-xob--id-goto ID)
             (unless (get-buffer-window)
               (pop-to-buffer (current-buffer))))
    (org-xob-with-xob-buffer
     (atomic-change-group
       (if (eq org-xob--display 'single)
           (org-xob--edit-write-single-pane ID title)
         (org-xob--edit-write-dual-pane ID title))
       ;; Visual: highlight edit heading line (only in edit buffer)
       (org-back-to-heading t)
       (org-xob--apply-edit-heading-visuals)
       ;; Now add sources/context (may switch buffer)
       (let ((node (make-open-node :ID ID :title title :sources nil)))
         (add-to-list 'org-xob--open-nodes node)
         (org-xob--add-source node org-xob--source-backlinks)
         (org-xob--add-source node org-xob--source-forlinks))
       ;; Auto-display links if enabled (after node is fully set up)
       (org-xob--auto-display-links ID)))))

(defun org-xob--auto-display-links (node-id)
  "Automatically display links for a newly opened node based on org-xob-auto-display-links setting."
  (when org-xob-auto-display-links
    (cond
     ((eq org-xob-auto-display-links t)
      ;; Display both backlinks and forlinks
      (org-xob-show-source 'backlinks)
      (org-xob-show-source 'forlinks))
     ((eq org-xob-auto-display-links 'backlinks)
      ;; Display only backlinks
      (org-xob-show-source 'backlinks))
     ((eq org-xob-auto-display-links 'forlinks)
      ;; Display only forlinks
      (org-xob-show-source 'forlinks)))))

(defun org-xob--auto-setup-dual-pane ()
  "Automatically set up dual-pane window layout if enabled."
  (when org-xob-auto-dual-pane
    (let ((current-window (selected-window))
          (buf1 (current-buffer)))
      ;; Only set up dual-pane if we're not already in a split configuration
      ;; and we have a proper xob buffer with a pair buffer
      (when (and (= (length (window-list)) 1)
                 (org-xob-buffer-p buf1)
                 (buffer-local-value 'org-xob--pair-buf buf1)
                 (not (get-buffer-window (buffer-local-value 'org-xob--pair-buf buf1))))
        (org-xob--dual-pane current-window)
        (message "XOB: Auto-setup dual-pane layout complete")))))

;;;###autoload
(defun org-xob-revert-edit ()
  "Revert the edit node at point back to the original."
  (interactive)
  (org-back-to-heading) ;; todo improve - back to proper heading
  (when-let ((pid (org-entry-get (point) "EDIT"))
             (tid (org-entry-get (point) "ID"))
             (m (point-marker)))
    (org-mark-subtree)
    (call-interactively #'delete-region)
    (deactivate-mark 'force)
    (insert (org-xob--select-content
             pid
             #'(lambda () (org-xob--get-full-node 1 'meta))))
    (goto-char m)
    (org-xob--mod-to-edit-node)
    (org-entry-put (point) "ID" tid)
    (outline-hide-entry)))

(defun org-xob--update-modified-time ()
  "Update the modified timestamp for xob node at point."
  (if (or (org-xob--is-node-p)
          (org-xob--is-edit-node-p))
      (org-entry-put (point) "MODIFIED"
                     (concat "[" (format-time-string "%F %a %R") "]")))
  nil)

(defun org-xob--modified-time= ()
  "If on an edit node, check if the modified time of the original has
changed since opening this copy."
  (when (org-xob--is-edit-node-p)
    (let ((etime (org-entry-get (point) "MODIFIED"))
          otime)
      (save-window-excursion
        (save-excursion
          (org-xob-goto-original)
          (setq otime (org-entry-get (point) "MODIFIED"))
          (org-time= etime otime))))))

(defun org-xob--paste-top-section (&optional clip)
  "paste clip at the end of the headings top section.
Point needs to be on the heading."
  (org-with-wide-buffer
   (let ((destination-level (org-current-level)))
     (if (org-goto-first-child)
         (progn
           (newline 2)
           (forward-line -1))
       (org-end-of-subtree)
       (newline))
     (org-xob--smart-paste clip destination-level))))

(defun org-xob--normalize-clip-headings (clip destination-level)
  "Return CLIP with its minimum heading level below DESTINATION-LEVEL.
CLIP is normalized in an Org temporary buffer.  Leading non-heading text is
left unchanged while every heading in CLIP is shifted by the same amount, so
relative nesting is preserved."
  (with-temp-buffer
    (org-mode)
    (insert clip)
    (goto-char (point-min))
    (let (minimum-level)
      (while (re-search-forward "^\\(\\*+\\) " nil t)
        (let ((level (length (match-string 1))))
          (setq minimum-level (if minimum-level
                                  (min minimum-level level)
                                level))))
      (when minimum-level
        (let ((shift (- (1+ destination-level) minimum-level)))
          (goto-char (point-min))
          (while (re-search-forward "^\\(\\*+\\) " nil t)
            (replace-match
             (make-string (+ (length (match-string 1)) shift) ?*)
             t t nil 1)))))
    (string-trim-right (buffer-string) "\n")))

(defun org-xob--smart-paste (&optional clip destination-level)
  "If the paste is an org subtree, then properly adjust levels for the current heading.
Otherwise just yank. If heading is a xob node, then update modified time property."
  (save-excursion
    (if clip
        (if (org-kill-is-subtree-p clip)
            (org-paste-subtree nil clip nil nil)
          (if (string-match-p "^\\*+ " clip)
              (insert (org-xob--normalize-clip-headings
                       clip
                       (or destination-level (org-current-level) 0)))
            (insert clip)))
      (if (org-kill-is-subtree-p)
          (org-paste-subtree nil clip t t)
        (yank))))
  (org-xob--update-modified-time))

;;;;; Sync & Ediff Node

(defun org-xob--update-original (ID)
  "update contents of KB node with contents of the edit node with ID.
Does not use the kill-ring."
  (save-window-excursion
    (save-excursion
      (org-xob--id-goto ID)
      (when (org-xob--is-edit-node-p)
        (let* ((tbuffer (marker-buffer (org-id-find
                                        (org-entry-get (point) "EDIT") t)))
               (changes (nconc (prepare-change-group (current-buffer))
                               (prepare-change-group tbuffer)
                               (prepare-change-group org-xob-today-buffer)))
               m nclip oclip flag)
          (catch 'nochange
            (unless (org-xob--modified-time=)
              (if (y-or-n-p "Original node has changed. Run ediff?")
                  (progn (org-xob-ediff-edit)
                         (throw 'nochange))
                (unless (y-or-n-p "Really change?")
                  (throw 'nochange))))
            (unwind-protect
                (progn
                  (activate-change-group changes)
                  (org-xob--update-modified-time)
                  (setq nclip (org-xob--get-full-node (org-current-level)
                                                      'meta))
                  (org-xob-goto-original)
                  (setq m (point))
                  (when (org-xob--is-node-p)
                    (setq oclip (org-xob--get-full-node (org-current-level)
                                                        'meta))
                    (org-xob--save-version oclip nclip)
                    (org-xob--update-node nclip 'meta 'parsedit))
                  (goto-char m)
                  (org-xob--log-event "edited"
                                      (org-entry-get (point) "ID"))
                  (setq flag t))
              (if flag
                  (progn
                    (accept-change-group changes)
                    (org-xob--id-goto ID)
                    (org-xob-revert-edit))
                (cancel-change-group changes)
                (message "xob: failed to sync node: %s"
                         (gethash ID org-xob--id-title))))))))))

(defun org-xob--update-node (clip &optional meta parsedit)
  "Update any node with the given string ~clip~. If optional argument
meta is selected, then update the meta section as well (whole subtree).
With argument parsedit selected, then treat this as an edit update
and parse node."
  (org-with-wide-buffer
   (org-save-outline-visibility
       (let ((lev (org-current-level)))
         (org-narrow-to-subtree)
         (org-show-subtree)
         (org-mark-subtree)
         (unless meta (org-end-of-meta-data t))
         (call-interactively #'delete-region)
         (deactivate-mark 'force)
         (unless meta (org-end-of-meta-data t))
         (org-paste-subtree lev clip)
         ;; (insert clip)
         (when parsedit (org-xob--parse-edit-node))))))

(defun org-xob--parse-edit-node ()
  "Parse out relevant parts of a node after syncing into the xob KB (knowledge base).
Requires that point be on the relevant inserted text."
  (goto-char (point-min))
  (let ((id (org-entry-get (point) "EDIT"))
        (title (nth 4 (org-heading-components)))
        lid m)
    (org-entry-put (point) "ID" id)
    (org-delete-property "EDIT")
    (org-toggle-tag "edit" 'OFF)
    (when (re-search-forward org-logbook-drawer-re nil t)
      (kill-region (match-beginning 0)
                   (match-end 0))
      (setq m (org-xob--insert-link-header id title org-xob-today))
      (with-current-buffer (marker-buffer m)
        (org-end-of-subtree)
        (newline)
        (yank)))
    (setq m (org-end-of-subtree))
    (goto-char (point-min))
    (org-show-subtree)
    (while (or (re-search-forward org-xob--x-link-re m t)
               (re-search-forward org-xob--xdel-link-re m t))
      (if (string= org-xob--xdel-link-str (match-string 0))
          (progn
            (setq lid (org-element-property :path (org-element-context)))
            (org-super-links-delete-link)
            (org-xob--log-event "X link" lid))
        (replace-match org-xob--id-link-str t t)
        (setq lid (org-element-property :path (org-element-context)))
        (org-super-links-convert-link-to-super t)
        (org-xob--log-event "link" id)
        (org-xob--log-event "-> to" lid)))))

;;;###autoload
(defun org-xob-ediff-edit ()
  "Run ediff on the edit node at point with the original node."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((oid (org-entry-get (point) "EDIT"))
            bege ende bego endo frmw frmh)
        (when (org-xob--is-edit-node-p)
          (setq org-xob--ediff-bufe (current-buffer))
          (org-narrow-to-subtree)
          (setq bege (point-min) ende (point-max))
          (deactivate-mark 'force)
          (org-id-goto oid)
          (setq org-xob--ediff-bufo (current-buffer))
          (org-narrow-to-subtree)
          (setq bego (point-min) endo (point-max))
          (deactivate-mark 'force)
          (setq frmw (frame-width))
          (setq frmh (frame-height))
          (select-frame
           (setq org-xob--ediff-frm (make-frame `((width . ,frmw)
                                                  (height . ,frmh)))))
          (ediff-regions-internal org-xob--ediff-bufe bege ende
                                  org-xob--ediff-bufo bego endo
                                  nil 'xob-ediff nil nil))))))

(defun org-xob--ediff-quit-hook ()
  "returns buffers to previous state and closes frame."
  (when (eq 'xob-ediff ediff-job-name)
    (dolist (buf (list org-xob--ediff-bufo org-xob--ediff-bufe))
      (when buf
        (with-current-buffer buf
          (goto-char (point-min))
          (outline-hide-subtree)
          (widen))))
    (when org-xob--ediff-frm (delete-frame org-xob--ediff-frm))
    (setq org-xob--ediff-bufo nil)
    (setq org-xob--ediff-bufe nil)
    (setq org-xob--ediff-frm nil)))

;;;;; Node Functions

(defun org-xob--is-node-p (&optional ID DEEPCHECK top)
  "Check if a heading is a xob node. Called interactively it defaults to heading at point.
If an ID argument is supplied, then check the heading associated with it.
With option DEEPCHECK, do not use any table lookup, but check whether the heading
has valid UUID formatted ID and xob TYPE properties in the property drawer.
Deepcheck only works on heading at point, any ID argument is ignored.
If TOP is selected, then only check immediate heading, no inheritance check
in subheadings. Returns the ID if true, nil otherwise."
  (interactive)
  (let ((temp (or ID (org-id-get nil))))
    (if temp
        (if DEEPCHECK
            (and
             ;; Must have explicit xob property set to "t" at THIS heading level (no inheritance)
             (string= "t" (org-entry-get (point) "xob" top))
             ;; Must have a valid xob node type at THIS heading level (no inheritance)
             (member (org-entry-get (point) "TYPE" top) org-xob--node-types)
             ;; Must have a valid UUID format
             (eq 0 (org-uuidgen-p temp))
             ;; Must not be an edit node
             (not (org-entry-get (point) "EDIT" top))
             temp)
          (if (gethash temp org-xob--id-title) temp)))))

(defun org-xob--is-edit-node-p (&optional top)
  "Is point on a node that is in an edit state? Return it's ID if true,
nil otherwise. With optional TOP, check the specific heading at point,
no inheritance check in subheadings."
  (when-let ((id (org-entry-get (point) "EDIT" (not top))))
    (and (string= "t" (org-entry-get (point) "xob" (not top)))
         (gethash id org-xob--id-title)
         ;; (member "edit" (org-get-tags))
         id)))

(defun org-xob--is-open-node-p (ID)
  "Is node with ID currently open?"
  (member ID (org-xob--get-open-node-ids)))

(defun org-xob--node-1D-p (&optional ID)
  "Test whether node at point (or optional node with ID) is displayed inline."
  (let ((p (if ID (org-id-find ID 'markerp) (point))))
    (string= "1D" (org-entry-get p "D" 'inherit nil))))

(defun org-xob--node-2D-p (&optional ID)
  "Test whether node at point (or optional node with ID) is displayed with sideline."
  (let ((p (if ID (org-id-find ID 'markerp) (point))))
    (string= "2D" (org-entry-get p "D" 'inherit nil))))

(defun org-xob--node-?D (&optional ID)
  "Returns the display type of node at point (or optional node with ID)
as a text string, either \'1D\' or \'2D\'."
  (let ((p (if ID (org-id-find ID 'markerp) (point))))
    (org-entry-get p "D" 'inherit nil)))

(defun org-xob--to-node-top ()
  "Goto the top heading of the node, whether edit or original."
  (when (or (org-xob--is-node-p)
            (org-xob--is-edit-node-p))
    (org-back-to-heading)
    (while (and (not (or (org-xob--is-node-p nil nil t)
                         (org-xob--is-edit-node-p t)))
                (org-up-heading-safe)))))

(defun org-xob--eval-capture-templates ()
  "Re-evaluate the capture templates so they are up to date."
  (setq org-xob--templates
        `(("nn" "new node" entry (file org-xob--KB-file)
           "* %(eval org-xob--last-title) \n:BACKLINKS:\n:END:\n"
           :xob-node t
           :ntype "n.n"
           :immediate-finish t
           :empty-lines-after 1)

          ("ad" "today" entry (file+function org-xob--log-file ,(lambda () (org-datetree-find-month-create (calendar-current-date))))
           "**** %<%F %A> \n:BACKLINKS:\n:END:\n"
           :xob-node t
           :immediate-finish t
           :ntype "a.day"
           )

          ;; org-projectile for now
          ("ap" "new project" entry (file org-xob--agenda-file)
           "* %^{description} \n:BACKLINKS:\n:END:\n"
           :xob-node t
           :ntype "a.project"
           :immediate-finish t
           )

          ;; not sure
          ("as" "new session" entry (file org-xob--agenda-file)
           "* %^{description}  \n:BACKLINKS:\n:END:\n"
           :xob-node t
           :ntype "a.session"
           :immediate-finish t
           )

          ;; regular templates for now
          ("tf" "todo general" entry (file org-xob--agenda-file)
           "* %^{description} \n:BACKLINKS:\n:END:\n\n%?"
           :xob-node t
           :todo t
           :ntype "a.todo"
           :immediate-finish t
           )

          ;; org-projectile for now
          ("tp" "todo project" entry (file org-xob--agenda-file)
           "* %^{description} \n:BACKLINKS:\n:END:\n\n%a\n%?"
           :xob-node t
           :todo t
           :ntype "a.todo"
           :immediate-finish t
           )
          )))

(defun org-xob--select-content (id selector)
  "Sets point to beginning of kb node with id and uses the function argument selector to mark the content to return.
Returns content as a string with properties."
  (let (str)
    (save-window-excursion
      (save-excursion
        (org-id-goto id)
        (org-with-wide-buffer   ;; TODO maybe remove
         (org-save-outline-visibility
             (org-narrow-to-subtree)
           (outline-show-all)
           (condition-case err
               (setq str (funcall selector))
             (t (message "xob: failed to select for %s" id)))
           (deactivate-mark 'force)))))
    str))

;; NOTE written for org-xob--select-content, for syncing to xob kb
(defun org-xob--get-full-node (level &optional meta trimtop trimend)
  "Return a full node as a string (with properties). Used for both edit
node and context presentation. Returns the full node as a string, but with
adjusted specified level. Requires that source nodes are not nested. Options:
meta - Include all contents. Adds the heading and all drawers at the top.
       Otherwise just the body is returned.
trimtop - Exclude any empty space between the heading and the first content.
          Only relevant if meta is excluded (nil).
trimend - Exclude empty lines at the bottom."
  (let ((org-yank-folded-subtrees nil)
        (org-yank-adjusted-subtrees t))
    (org-copy-subtree)
    (with-temp-buffer
      (org-mode)
      (org-paste-subtree level)
      (goto-char (point-min))
      (org-mark-subtree)
      (unless meta
        (org-end-of-meta-data t)
        (unless trimtop
          (org-back-over-empty-lines)))
      (when trimend
        (exchange-point-and-mark)
        (org-back-over-empty-lines)
        (exchange-point-and-mark))
      (buffer-substring (point) (mark)))))

;;;;;; --new nodes and links--
(defun org-xob--get-create-node ()
  "Find or create new xob KB node using helm. Returns node (ID title) as a list."
  (unless org-xob-on-p
    (org-xob-start))
  (helm :buffer "*xob get node*"
        :sources (helm-build-sync-source "xob-kb"
                   :candidates (lambda ()
                                 (let* ((cans (hash-table-keys org-xob--title-id)))
                                   (cons helm-input cans)))
                   :volatile t
                   :action #'org-xob--get-create-node-action)))

(defun org-xob--get-node-by-type (&optional type)
  "Find a node by type."
  (unless org-xob-on-p
    (org-xob-start))
  (if-let ((type (if (stringp type)
                     type
                   (org-xob--select-node-type))))
      (helm :buffer "*xob get typed node*"
            :sources (helm-build-sync-source "xob-papers"
                       :candidates (org-xob--find-nodes-by-type type)
                       :volatile t
                       :action #'org-xob--get-create-node-action))))

(defun org-xob--get-create-node-action (title)
  "Builds a list of one or more cons cells for selected nodes of form (ID . title)."
  (mapcar (lambda (title)
            (if-let ((ID (gethash title org-xob--title-id)))
                (list ID title)
              (list (org-xob--capture title) title)))
          (helm-marked-candidates)))

(defun org-xob--do-select-nodes (single types func)
  "Run FUNC for each selected node with corresponding
ID and title. SINGLE forces the use of one selection, TYPES allows you
to select the node type first."
  (let ((selected (if types
                      (org-xob--get-node-by-type types)
                    (org-xob--get-create-node)))
        (dothis (lambda (sel) (let ((ID (car sel))
                                    (title (cadr sel)))
                                (funcall func ID title)))))
    (if (not single)
        (dolist (sel selected)
          (funcall dothis sel))
      (funcall dothis (car selected)))))

(defun org-xob--select-node-type ()
  (helm :buffer "xob types"
        :sources (helm-build-sync-source "xob-types"
                   :candidates org-xob--node-types
                   :action (lambda (c) c))))

(defun org-xob--find-nodes-by-type (type)
  (org-ql-select (append org-xob--KB-files org-xob--agenda-files)
    `(and (is-xob-node)
          (property "TYPE" ,type))
    :action #'(nth 4 (org-heading-components))))

(defun org-xob--new-node (&optional heading)
  "Both a hook function and for general node creation.
   If 'heading' marker is on an org heading,
then convert it into a new node in place. Otherwise it is assumed to be called
as a capture hook function."
  (if (or (org-capture-get :xob-node) heading)
      (let ((ID (org-id-get-create))
            (title (nth 4 (org-heading-components)))
            (timestamp (concat "[" (format-time-string "%F %a") "]")))
        (if (org-capture-get :todo) (org-todo))
        (org-entry-put (point) "xob" "t")
        (org-entry-put (point) "TYPE" (if heading "n.n"
                                        (org-capture-get :ntype)))
        (org-entry-put (point) "CREATED" timestamp)
        (org-entry-put (point) "MODIFIED" timestamp)
        (puthash ID title org-xob--id-title)
        (puthash title ID org-xob--title-id)
        (when heading
          (org-id-add-location ID (buffer-file-name
                                   (marker-buffer heading))))
        (org-xob--log-event "new node" ID)
        (setq org-xob--last-captured ID))))

(defun org-xob--capture (title)
  (let* ((org-capture-templates org-xob--templates))
    (if (member title org-xob--auto-templates)
        (org-capture nil title)
      (progn
        (setq org-xob--last-title title)
        (org-capture nil "nn")))
    (if-let* ((file (buffer-file-name
                     (marker-buffer org-capture-last-stored-marker))))
        (org-id-add-location org-xob--last-captured file)
      (message "xob: org-id failed to add new node %s" title))
    org-xob--last-captured))

(defun org-xob--link-hook-fn ()
  "If a link is a xob node, then reopen node in xob edit mode.
Only acts on ID links that are registered xob nodes in the hash table."
  (let ((link (org-element-context))
        ID title)
    (when (equal "id" (org-element-property :type link))
      (setq ID (org-element-property :path link))
      (setq title (gethash ID org-xob--id-title))
      ;; Only act if this ID is actually a registered xob node
      (when title
        (org-xob--edit-node ID title)
        ;; Return t to indicate we handled this link
        t))))

(defun org-xob--super-links-hook ()
  "Adjust linking actions depending on whether the source is an original
or edit node."
  (if (org-xob--is-edit-node-p)
      (org-xob-goto-original)))

(defun org-xob--insert-link-header (ID title target)
  "Checks if link subheader exist at target. If not, inserts a
subheading with an org link to the node with ID and title.
Returns mark for the link subheader."
  (save-excursion
    (save-window-excursion
      (org-id-goto target)
      (let ((place nil))
        (org-map-tree
         (lambda () (if (string-match-p (regexp-quote ID)
                                        (nth 4 (org-heading-components)))
                        (setq place (point)))))
        (if place (progn
                    (goto-char place)
                    (org-back-to-heading))
          (newline 2)
          (org-insert-subheading '(4))
          (org-insert-link nil (concat "ID:" ID) title)
          (newline 2)
          (org-back-to-heading))
        (point-marker)))))

;;;;; UX: visuals helpers

(defun org-xob--apply-edit-heading-visuals ()
  "Apply custom face to all edit node headings in the buffer."
  (save-excursion
    (goto-char (point-min))
    (unless (boundp 'org-xob--edit-heading-ovs)
      (setq-local org-xob--edit-heading-ovs nil))
    ;; Remove old overlays
    (mapc #'delete-overlay org-xob--edit-heading-ovs)
    (setq org-xob--edit-heading-ovs nil)
    ;; Apply overlays to all headings with EDIT property
    (while (re-search-forward "^\*+ .*$" nil t)
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (save-excursion
          (org-back-to-heading t)
          (when (org-entry-get (point) "EDIT")
            (let ((ov (make-overlay beg end)))
              (overlay-put ov 'face 'org-xob-edit-heading-face)
              (push ov org-xob--edit-heading-ovs))))))))

(defun org-xob--apply-context-source-heading-faces ()
  "Apply custom faces to context buffer source headings by tag."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\* .+:KB:\\(backlinks\\|forlinks\\):" nil t)
      (let* ((beg (line-beginning-position))
             (end (line-end-position))
             (tag (match-string 1))
             (face (cond
                    ((string= tag "backlinks") 'org-xob-context-backlinks-face)
                    ((string= tag "forlinks") 'org-xob-context-forlinks-face))))
        (let ((ov (make-overlay beg end)))
          (overlay-put ov 'face face))))))




;;;;;; --- Node Versioning ---

(defun org-xob--save-version (old new)
  "Create a diff between prior node state and current, then save it."
  (let ((old (org-xob--get-full-node (org-current-level) 'meta)))
    ()))

(defun org-xob--diff-node (new old)
  "Creates a diff using =org-xob--delta-executable=.
The order of versions is reversed; the diff allows the reconstruction of
the prior state from the current."
  (shell-command))
;; (defun org-xob--new-node-diff (nodeID)
;;   (let ((old-id (org-id-store-link node)))))

(defun org-xob--diff-filename (node)
  (concat
   ;; node id
   "-"
   (format-time-string "%j-%H-%M")))

(defun org-xob--node-add-time-property (property)
  "Convenience function to add high resolution time property.
Maybe useful for syncing."
  (org-entry-put (point) property
                 (number-to-string
                  (car (time-convert (current-time) '10000)))))

;;;;; Contexts Functions

;; TEST
(defun org-xob--is-source-p (&optional PID ID)
  "Check if heading at point is a valid xob source. If PID and ID
arguments are supplied, then check the associated heading."
  (interactive)
  (if-let ((temp (or ID
                     (org-entry-get (point) "ID")))
           (pid (or PID
                    (org-entry-get (point) "PID")))
           (node (org-xob--get-open-node pid))
           (srcs (open-node-sources node))
           ((cl-find-if #'(lambda (x) (string= temp x))
                        srcs
                        :key #'(lambda (s) (plist-get s :ID)))))
      t nil))

(defun org-xob--add-source (node source-type)
  "Create a new source of source-type for the given node."
  (let ((newsrc (copy-tree source-type)))
    (plist-put newsrc :ID (uuidgen-4))
    (plist-put newsrc :PID (open-node-ID node))
    (plist-put newsrc :title (open-node-title node))
    (funcall (plist-get newsrc :getfn) newsrc)
    ;; (setq srcs (add-to-list srcs newsrc))
    (push newsrc (open-node-sources node))
    newsrc))

(defun org-xob-show-source (source &optional arg)
  "Show context source for opened node at point. If this context material is already
displayed, then refresh it. With optional C-u, force repopulating the item list."
  ;; in an edit node? get id, name, and which buffer is the context buffer
  (if-let* ((eid (org-xob--is-edit-node-p))
            (node (org-xob--get-open-node eid))
            (srcs (open-node-sources node))
            (src (cl-find-if #'(lambda (x) (if (equal source x) x))
                             srcs
                             :key #'(lambda (x) (car-safe (cdr-safe x)))))
            (title (truncate-string-to-width
                    (nth 4 (org-heading-components)) 25))
            (bufc (if (eq org-xob--display 'dual)
                      org-xob--pair-buf
                    (current-buffer))))
      (save-window-excursion
        (save-excursion
          (with-current-buffer bufc
            (org-with-wide-buffer
             (evil-save-state
               ;; (when (eq '(4) arg))                    ;; if arg then repop items
               (funcall (plist-get src :getfn) src eid)
               (org-show-subtree))))))))

(provide 'org-xob-backend)
;;; org-xob-backend.el ends here
