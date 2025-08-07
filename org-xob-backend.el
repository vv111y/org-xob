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
                     (get-buffer-create session-name)
                     (write-file session-path)))
             (buf2 (get-buffer-create
                    (concat "xob-C-" numbufs))))
    (with-current-buffer buf1
      (org-mode)
      (org-xob-mode 1)
      (auto-save-mode 1)
      (add-hook 'kill-buffer-hook #'org-xob--close-buffer-hook nil 'local)
      (setq-local org-xob--buf 'parent
                  org-xob--pair-buf buf2)
      (if single
          (progn (setq-local org-xob--display 'single)
                 (setq-local org-xob--c-buff (current-buffer)))
        (setq-local org-xob--display 'dual
                    org-xob--c-buff buf2)))
    (with-current-buffer buf2
      (org-mode)
      (org-xob-mode 1)
      (setq-local org-xob--buf 'child
                  org-xob--pair-buf buf1))
    (setq org-xob-last-buffer buf1)
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
      (point))))

(defun org-xob--goto-edit (ID)
  (when (and (org-not-nil ID)
             org-xob-buffers)
    (setq m
          (car
           (org-ql-select org-xob-buffers
             `(and (property "EDIT" ,ID)
                   (tags "edit"))
             :action '(point-marker))))
    (when (markerp m)
      (set-buffer (marker-buffer m))
      (goto-char m)
      (point))))

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
    (kill-buffer buf)))

;; TODO
(defun org-xob--rewrite-buffer-1-pane ()
  "Rewrites all open nodes in single pane format."
  )

;; TODO
(defun org-xob--rewrite-buffer-2-pane ()
  "Rewrites all open nodes in dual pane format."
  )

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
       (insert "* " title "  :edit:")
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
  (org-set-tags "edit")
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
       (let ((node (make-open-node :ID ID :title title :sources nil)))
         (add-to-list 'org-xob--open-nodes node)
         (org-xob--add-source node org-xob--source-backlinks)
         (org-xob--add-source node org-xob--source-forlinks))))))

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
   (if (org-goto-first-child)
       (progn
         (newline 2)
         (forward-line -1))
     (org-end-of-subtree)
     (newline))
   (org-xob--smart-paste clip)))

(defun org-xob--smart-paste (&optional clip)
  "If the paste is an org subtree, then properly adjust levels for the current heading.
Otherwise just yank. If heading is a xob node, then update modified time property."
  (save-excursion
    (if clip
        (if (org-kill-is-subtree-p clip)
            (org-paste-subtree nil clip nil nil)
          (insert clip))
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
             (string= "t" (org-entry-get (point) "xob" (not top)))
             (member (org-entry-get (point) "TYPE" (not top)) org-xob--node-types)
             (eq 0 (org-uuidgen-p temp))
             (not (org-entry-get (point) "EDIT" (not top)))
             temp)
          (if (gethash temp org-xob--id-title) temp)))))

(defun org-xob--is-edit-node-p (&optional top)
  "Is point on a node that is in an edit state? Return it's ID if true,
nil otherwise. With optional TOP, check the specific heading at point,
no inheritance check in subheadings."
  (when-let ((id (org-entry-get (point) "EDIT" (not top))))
    (and (string= "t" (org-entry-get (point) "xob" (not top)))
         (gethash id org-xob--id-title)
         (member "edit" (org-get-tags))
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
    (while (and (or (not (org-xob--is-node-p top))
                    (not (org-xob--is-edit-node-p top)))
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
  "Both a hook function and for general node creation. If orgmode 'heading' is given,
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
        (org-xob--log-event "new node" ID)
        (setq org-xob--last-captured ID))))

(defun org-xob--capture (title)
  (let* ((org-capture-templates org-xob--templates)
         ID)
    (if (member title org-xob--auto-templates)
        (org-capture nil title)
      (progn
        (setq org-xob--last-title title)
        (org-capture nil "nn")))
    org-xob--last-captured))

(defun org-xob--link-hook-fn ()
  "If a link is a xob node, then reopen node in xob edit mode."
  (let ((link (org-element-context))
        ID title)
    (if (equal "ID" (org-element-property :type link))
        (progn
          (setq ID (org-element-property :path link))
          (setq title (gethash ID org-xob--id-title))
          (if title
              (org-xob--edit-node ID title)))
      nil)))

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
               ;; TODO redo for overheading
               (if (org-xob--goto-buffer-heading
                    (plist-get src :ID))
                   (org-xob--source-refresh src)		;; found, refresh
                 (org-xob--source-write src))			;; not found, then write src
               (if (pulse-available-p)
                   (pulse-momentary-highlight-one-line (point))))))))
    (message "Source is not available.")))

(defun org-xob--this-node-sources (id)
  "Returns node context sources as a list of their ids."
  (when-let* ((node (org-xob--get-open-node id))
              (srcs (open-node-sources node)))
    (mapcar #'(lambda (x) (plist-get x :ID))
            srcs)))

;; TODO need over-heading both dual and single pane
(defun org-xob--source-write (source)
  "Open a source tree into the context buffer. If it is already there,
then refresh it. source items are shown as org headings.
source is a plist that describes the content source."
  (let (m)
    (unless (org-xob--goto-buffer-heading (plist-get source :ID))
      (goto-char (point-max)) ;; TODO goto super-heading, then to end of tree
      (org-insert-heading '(4) 'invisible-ok 'TOP) ;; TODO insert sub-tree
      (org-edit-headline (plist-get source :title))
      (org-set-tags (plist-get source :tags))
      (org-entry-put (point) "ID" (plist-get source :ID))
      (org-entry-put (point) "PID" (plist-get source :PID)))
    (org-xob--source-refresh source)
    (org-flag-subtree t)
    (org-show-children 1)))


;; TODO check state type, lookup + call
(defun org-xob--source-refresh (source)
  "Remake source tree. Check if items need to be added or removed."
  (if (string= (org-entry-get (point) "ID") ;; TODO new, test
               (plist-get source :ID))
      (save-restriction
        (org-narrow-to-subtree)
        (let ((temp (copy-tree (plist-get source :items))))
          (org-xob--map-source
           (lambda ()
             (let ((pid (org-entry-get (point) "PID")))
               (if (member pid temp)
                   (setq temp (delete pid temp))
                 (progn
                   (org-mark-subtree)
                   (call-interactively 'delete-region))))))
          (if temp
              (dolist (el temp)
                (org-xob--source-add-item el)))))
    (message "xob: can't refresh context here, source ID does not match.")))

(defun org-xob--source-add-item (ID)
  "Appends a single entry to the end of the source subtree.
Assumes point is on the source heading."
  (let ((title (gethash ID org-xob--id-title)))
    (save-excursion
      (if title
          (progn
            (org-insert-subheading '(4))
            (org-edit-headline title)
            ;; todo replace copy
            (org-entry-put (point) "PID" ID))
        (message "no kb node found for ID: %s" ID)))))

(defun org-xob--map-source (func &optional ID)
  "Apply the function func to every child-item of a xob source.
If the optional ID of a xob source is given, then apply func to that source.
Otherwise apply to source at point."
  (if ID (org-xob--id-goto ID))
  (org-with-wide-buffer
   (org-save-outline-visibility
       ;; (org-narrow-to-subtree)
       (if (and (org-xob--is-source-p)
                (org-goto-first-child))
           (while (progn
                    (save-excursion (funcall func))
                    (outline-get-next-sibling)))
         (message "XOB: map-source, nothing to do here.") nil)
     (org-up-heading-safe)
     (outline-hide-subtree))))

;;;;; KB Context Functions

(defun org-xob--node-get-link-entries (source &optional EID)
  "Populates source item list from the node. The items are represented by their
respective node IDs. Two kinds of links are distinguished: backlinks and forlinks
(which are all other links to xob KB nodes). Assumes org-super-links convention
where the backlinks are in a BACKLINKS drawer."
  (save-window-excursion
    (save-excursion
      (if EID
          (org-xob--goto-edit EID)
        (org-id-goto (plist-get source :PID)))
      (plist-put source :items
                 (org-xob--node-get-links (plist-get source :name))))))

(defun org-xob--node-get-links (linktype)
  "Return list of link paths within the node at point. If linktype is 'backlinks'
then return only links in the backlinks drawer. If linktype is 'forlinks'
then return all other links."
  (let* ((test (if (eq linktype 'backlinks)
                   (lambda (x) x)
                 (if (eq linktype 'forlinks)
                     (lambda (x) (not x))))))
    (save-excursion
      (save-restriction
        (org-back-to-heading t)
        (org-narrow-to-subtree)
        (delete-dups
         (delq nil
               (org-element-map (org-element-parse-buffer) 'link
                 (lambda (link)
                   (let (ID)
                     (if (funcall test (equal (org-element-property
                                               :drawer-name (cadr (org-element-lineage link)))
                                              "BACKLINKS"))
                         (if (and (not (string= "xobdel"
                                                (org-element-property :type link)))
                                  (org-xob--is-node-p
                                   (setq ID (org-element-property :path link))))
                             ID
                           nil)))))))))))

;; TODO test with new sources packaging
(defun org-xob--context-copy-paste (&optional tag selector insertor)
  "Wrapper function to display new content in a context item from the
knowledge base. Executes function selector while point is at the heading
of the origin node in the KB. selector must be a lambda that returns
the the contents of interest as a string.
If no arguments are given, then the context item is cleared.
When called with point on the given context item, only that item will be
updated. If called on a context source heading, then the update is applied
to all source items."
  (let ((func #'(lambda ()

                  (let ((pid (org-entry-get (point) "PID")) str)
                    (when (org-uuidgen-p pid)
                      (org-xob--clear-node)
                      (org-set-tags tag)
                      (and selector
                           (stringp
                            (setq str (org-xob--select-content pid selector)))
                           (progn
                             (org-end-of-subtree)
                             (newline)
                             (if insertor
                                 (funcall insertor str)
                               (insert str)
                               (outline-hide-subtree)
                               (org-show-entry)))))))))
    (save-window-excursion
      (org-with-wide-buffer
       (if (pulse-available-p)
           (pulse-momentary-highlight-one-line (point)))
       (if (org-xob--is-source-p)
           (org-xob--map-source func)
         (funcall func))))))

(defun org-xob--clear-node ()
  "clears the contents of the heading but does not touch the Properties drawer."
  (save-excursion
    (org-back-to-heading t)
    (org-mark-subtree)
    (org-end-of-meta-data)
    (call-interactively #'delete-region)
    (deactivate-mark 'force)))

;;;;; org-ql predicates
(org-ql-defpred is-xob-node-deep ()
  "Deepcheck if heading is a xob node."
  :body (and (property "xob" t)
             (member (org-entry-get (point) "TYPE")
                     org-xob--node-types)
             (eq 0 (org-uuidgen-p (or (org-entry-get (point) "ID")
                                      (org-entry-get (point) "PID")
                                      (org-entry-get (point) "EDIT"))))))

(org-ql-defpred is-xob-node ()
  "Quick check if the heading is in the xob system."
  :body  (gethash (property "ID") org-xob--id-title))

(org-ql-defpred is-xob-id (&optional ID)
  "Like is-xob-node-deep, but only checks for ID property."
  :body (and (property "xob" "t")
             (member (org-entry-get (point) "TYPE") org-xob--node-types)
             (eq 0 (org-uuidgen-p (property "ID" ID)))))

(org-ql-defpred is-xob-edit (&optional ID)
  "Checks for edit tag and that EDIT property is not empty. If argument ID is given,
then also check if EDIT is equal to it."
  :body (and (tags "edit")
             (when-let ((eid (property "EDIT")))
               (if ID
                   (string= ID eid)
                 eid))))

(org-ql-defpred is-xob-edit-deep (&optional ID)
  "Checks if heading has EDIT with optional ID,
then checks using org-xob--is-edit-node-p."
  :body (and (if ID (string= ID (property "EDIT")))
             (org-xob--is-edit-node-p)))

(org-ql-defpred is-xob-original (&optional ID)
  "Quick check if heading has an ID, but no edit tag."
  :body (and (property "ID" ID)
             (not (tags "EDIT"))))

(org-ql-defpred is-xob-original-deep ()
  "Deepcheck if heading is a xob original node."
  :body (and (not (tags "EDIT"))
             (not (property "PID"))
             (not (property "EDIT"))
             (property "xob" t)
             (member (org-entry-get (point) "TYPE")
                     org-xob--node-types)
             (eq 0 (org-uuidgen-p (org-entry-get (point) "ID")))))

(org-ql-defpred is-xob-source (PID)
  "Checks if heading is a source for node with ID 'PID'"
  :body  (org-xob--is-source-p
          (property "PID" PID)))

;;;;; org-ql mapping functions

(defun org-xob-map-buffers (func)
  "Apply func in all xob buffers."
  (mapc #'(lambda (win) (with-current-buffer win
                          (funcall func)))))

(defun org-xob-map-headings-in-buffers (func)
  "Apply func to all headings with an ID property in all xob buffers."
  (org-ql-select org-xob-buffers
    `(property "ID")
    :action func))

(defun org-xob-find-in-buffers (prop val)
  "Find all open headings with property prop that has value val."
  (org-ql-select org-xob-buffers
    `(property ,prop ,val)))

(defun org-xob-find-any-in-buffers ()
  "Find any node in any xob buffers.")

(defun org-xob-find-all-in-buffers (ID)
  "Find all nodes with same ID in all buffers.")

(defun org-xob-map-if-in-buffers (pred func)
  "Apply func to all headings that satisfy paredicate pred in all xob buffers."
  (org-ql-select org-xob-buffers
    `(,pred)
    :action func))

(defun org-xob-map-all-nodes (func)
  "Apply FUNC on all nodes in all xob buffers."
  (org-ql-select org-xob-buffers
    '(is-xob-node)
    :action func))

(defun org-xob-map-all-edits (func)
  "Apply FUNC on all edit nodes in all xob buffers."
  (org-ql-select org-xob-buffers
    '(is-xob-edit)
    :action func))

(defun org-xob-map-buffer-edit-nodes (func)
  "Apply FUNC to all edit nodes in current buffer."
  (org-ql-select (current-buffer)
    '(is-xob-edit)
    :action func))

(defun org-xob-map-all-copies (func)
  (org-ql-select org-xob-buffers
    '(is-xob-copy)
    :action func))

(defun org-xob-map-all-sources (func)
  (org-ql-select org-xob-buffers
    '(is-xob-source)
    :action func))

(defun org-xob-map-node-sources (ID func)
  (org-ql-select org-xob-all-buffers
    `(is-xob-source ,ID)
    :action func))

(defun org-xob-node-source (ID source func)
  "Apply FUNC to all sources with ID and SOURCE tag."
  (org-ql-select org-xob-buffers
    `(and (is-xob-source ,ID)
          (tags source))
    :action func))

;;;;; Activity

(defun org-xob--open-today ()
  "Open today node for logging."
  (org-xob-with-xob-on
   (setq org-xob-today-string  (format-time-string "%F %A"))
   (and (or (setq org-xob-today (gethash org-xob-today-string
                                         org-xob--title-id))
            (setq org-xob-today
                  (save-window-excursion
                    (save-excursion
                      (find-file org-xob--log-file)
                      (org-with-wide-buffer
                       (if (re-search-forward org-xob-today-string nil t nil)
                           (let (id)
                             (setq id (org-entry-get (point) "ID"))
                             (puthash id org-xob-today-string org-xob--id-title)
                             (puthash org-xob-today-string id org-xob--title-id)
                             id)
                         nil)))))
            (setq org-xob-today (org-xob--capture "ad")))
        (save-window-excursion
          (save-excursion
            (org-id-goto org-xob-today)
            (setq org-xob-today-buffer (current-buffer))))
        (message "XOB: Todays log entry opened.") t)))

(defun org-xob--log-event (event id &optional description)
  "General log function used to send activity entries to the log."
  (save-window-excursion
    (save-excursion
      (save-restriction
        (let ((title (if (org-uuidgen-p id) (gethash id org-xob--id-title)
                       (replace-regexp-in-string org-xob--log-re "" id)))
              (descrpt (if description
                           (replace-regexp-in-string org-xob--log-re "" description))))
          (unless org-xob-today
            (org-xob--open-today))
          (org-id-goto org-xob-today)
          (if t
              (progn
                (org-narrow-to-subtree)
                (org-xob--paste-top-section
                 (concat "| " (format-time-string "%r")
                         " | " event
                         " | " title
                         ;; " | " descrpt
                         " |"))
                t)
            nil))))))

(defun org-xob--auto-clock-in ()
  "Maybe? automatically clock node editing activity.
This function starts clock for a given node.")

(defun org-xob--auto-clock-out ()
  "Maybe?. This functions stops the automatic clock for the given node.")
;;;;; xob Management

;;;###autoload
(defun org-xob-info ()
  "Give basic information about the xob system."
  (interactive)
  (org-xob-with-xob-on
   (display-message-or-buffer
    (concat
     "XOB State\n"
     "---------\n"
     "title-id-table entries:\t\t\t"
     (number-to-string (hash-table-count org-xob--title-id)) "\n"
     "id-title-table entries:\t\t\t"
     (number-to-string (hash-table-count org-xob--id-title)) "\n"
     "org-id entries:\t\t\t\t\t\t\t"
     (number-to-string (hash-table-count org-id-locations)) "\n"
     "\n"
     "KB files count:\t\t\t\t\t\t\t"
     (number-to-string (length org-xob--KB-files)) "\n"
     "Agenda files count:\t\t\t\t\t"
     (number-to-string (length org-xob--agenda-files)) "\n"
     "Log files count:\t\t\t\t\t\t\t"
     (number-to-string (length org-xob--log-files)) "\n"
     "Archive files count:\t\t\t\t\t"
     (number-to-string (length org-xob--archive-files)) "\n"
     "\n"
     "current KB file:\t\t\t\t\t\t\t" org-xob--KB-file "\n"
     "current agenda file:\t\t\t\t\t" org-xob--agenda-file "\n"
     "current log file:\t\t\t\t\t\t" org-xob--log-file "\n"
     "current archive file:\t\t\t\t" org-xob--archive-file "\n"))))

;;;###autoload
(defun org-xob-rebuild ()
  "Remakes xob data structures, traverse all nodes in all KB files in the xob directory."
  (interactive)
  (clrhash org-xob--id-title)
  (clrhash org-xob--title-id)
  (message "XOB: cleared hash tables.")
  (and
   (org-xob--register-files)
   (message "XOB: re-registered all xob files."))
  (and
   (org-id-update-id-locations)
   (message "XOB: updated org-id hashtable."))
  (message "XOB: traversing all KB files...")
  (let ((filelist (append org-xob--KB-files
                          org-xob--agenda-files
                          (list org-xob--log-file)))
        ID title)
    (org-xob-visit-nodes
     filelist
     #'(lambda ()
         (setq ID (org-id-get (point)))
         (setq title (nth 4 (org-heading-components)))
         (puthash ID title org-xob--id-title)
         (puthash title ID org-xob--title-id))))
  (message "XOB: finished rebuilding xob hashtables.")
  (org-xob-info)
  (org-xob--save-state)
  (message "XOB: saved xob state."))

(defun org-xob-visit-nodes (filelist func)
  "Iterate over all KB nodes in all files. Apply function func to each node at point."
  (save-window-excursion
    (save-excursion
      (dolist (filename filelist)
        (with-current-buffer (find-file filename)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while
               (progn
                 (if (org-xob--is-node-p nil 'DEEPCHECK)
                     (funcall func))
                 (outline-next-heading)))))))))

(defun org-xob-node-info (&optional arg)
  "Get node id, list of for and back links. Default are node names, with
C-u only ids are returned. Used mostly for debugging purposes."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (let* ((eiid (org-entry-get (point) "ID"))
             (node (org-xob--get-open-node (org-entry-get (point) "EDIT")))
             (srcs (open-node-sources node))
             (blid (plist-get (nth 1 srcs) :ID))
             (flid (plist-get (car srcs) :ID)))
        (org-xob-goto-original)
        (let ((id (org-entry-get (point) "ID"))
              (bl (org-xob--node-get-links 'backlinks))
              (fl (org-xob--node-get-links 'forlinks)))
          (unless (equal arg '(4))
            (setq bl (mapcar (lambda (s) (gethash s org-xob--id-title)) bl))
            (setq fl (mapcar (lambda (s) (gethash s org-xob--id-title)) fl)))
          (setq bl (mapconcat (lambda (s) s) bl " || "))
          (setq fl (mapconcat (lambda (s) s) fl " || "))
          (print node)
          (display-message-or-buffer
           (concat
            "Node Info:  " id " || eiid: " eiid " || fl.id: " flid " || bl.id: " blid "\n"
            "BACKLINKS:  " bl "\n"
            "FORLINKS:   " fl "\n")))))))

;; --- persistent objects ---
(defun org-xob--save-state ()
  "Save exobrain state. For current version this means the lookup hashtables only."
  (unless (file-directory-p org-xob-dir)
    (make-directory org-xob-dir))
  (cl-mapcar #'(lambda (table filename)
                 (org-xob--save-object (concat org-xob-dir filename) table))
             '(org-xob--title-id org-xob--id-title)
             '("title-id-table" "id-title-table")))

(defun org-xob--load-state ()
  "Load exobrain state. For current version this means the lookup hashtables only.
If there are no saved tables, then create new empty ones."
  (cl-mapcar #'(lambda (table filename)
                 (if (file-exists-p (concat org-xob-dir filename))
                     (prog1 (message "XOB: found %s" filename)
                       (org-xob--load-object filename table))
                   (progn
                     (message "XOB: hashtable %s missing, initializing new %s" filename table)
                     (set table (make-hash-table
                                 :test 'equal
                                 :size org-xob--table-size)))))
             '(org-xob--title-id org-xob--id-title)
             '("title-id-table" "id-title-table")))

(defun org-xob--save-object (file data)
  "save emacs object. "
  (with-temp-file file
    (prin1 (symbol-value data) (current-buffer))))

(defun org-xob--load-object (file symbol)
  "load saved object."
  (when (boundp symbol)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents (concat org-xob-dir file))
          (goto-char (point-min))
          (set symbol (read (current-buffer))))
      (error (message "Error loading file %s" file)))))

;; --- contextual resources ---

(defun org-xob-add-info-source (source)
  "Add a contextual information source to xob system."
  (interactive)
  (and (symbolp source)
       (plist-member source :name)
       (plist-member source :tags)
       (plist-member source :title)
       (plist-member source :ID)
       (plist-member source :PID)
       (plist-member source :getfn)
       (plist-member source :items)
       (push source org-xob-available-sources)))

;; --- file management ---
(defun org-xob--register-files ()
  "Scan through the xob directory, properly identify and register various xob files."
  (org-xob--clear-file-variables)
  (mapc
   (lambda (filename)
     (unless (string-match-p "#" filename)
       (with-temp-buffer
         (insert-file-contents-literally filename nil 0 1024 nil)
         (let* ((x (car (org-collect-keywords '("PROPERTY"))))
                (current (if (member "xob-current-file t" x) t nil)))
           (if (member "xob t" x)
               (cond
                ((member "xob-log t" x)
                 (push  filename org-xob--log-files)
                 (if current (setq org-xob--log-file filename)))
                ((member "xob-agenda t" x)
                 (push  filename org-xob--agenda-files)
                 (if  current (setq org-xob--agenda-file filename)))
                ((member "xob-archive t" x)
                 (push  filename org-xob--archive-files)
                 (if current (setq org-xob--archive-file filename)))
                (t
                 (push filename org-xob--KB-files)
                 (if current (setq org-xob--KB-file filename)))))))))
   (directory-files org-xob-dir 'full "\.org$" t))
  t)

(defun org-xob--process-files ()
  "Called after files have been regisetered. Properly setup various file variables.
If necessary create new files."
  (cl-mapcar #'(lambda (filetype prefix filelist)
                 (save-window-excursion
                   (save-excursion
                     (let (filename)
                       (unless
                           (and (boundp filetype)
                                filetype
                                (setq filename (eval filetype))
                                (file-exists-p filename)
                                (not (equal filename org-xob-dir))
                                (find-file-noselect filename)
                                (message "XOB: found file for %s" filetype))
                         (message "XOB: current file for %s missing, initializing new." filetype)
                         (org-xob--new-file filetype prefix filelist))))))
             '(org-xob--agenda-file
               org-xob--log-file
               org-xob--archive-file
               org-xob--KB-file)
             '(org-xob--agenda-filename-prefix
               org-xob--log-filename-prefix
               org-xob--archive-filename-prefix
               org-xob--KB-filename-prefix)
             '(org-xob--agenda-files
               org-xob--log-files
               org-xob--archive-files
               org-xob--KB-files))
  (setq org-id-extra-files (append org-xob--KB-files
                                   org-xob--agenda-files
                                   org-xob--log-files))
  ;; (setq org-agenda-files (append org-agenda-files
  ;;                                org-xob--agenda-files
  ;;                                org-xob--log-files))
  t)

(defun org-xob--new-file (filepointer fileprefix filelist)
  "creates a new file, pushes it to it's appropriate list and sets it as current.
Buffer remains open. Returns the filename."
  (let* ((filename (concat
                    (symbol-value fileprefix)
                    (format "%03d" (+ 1 (length (eval filelist))))
                    ".org")))
    (save-window-excursion
      (save-excursion
        (find-file (concat org-xob-dir filename))
        (goto-char (point-min))
        (insert org-xob--xob-header)
        (if (string= fileprefix org-xob--agenda-filename-prefix)
            (insert org-xob--agenda-header))
        (if (string= fileprefix org-xob--log-filename-prefix)
            (insert org-xob--log-header))
        (if (string= fileprefix org-xob--archive-filename-prefix)
            (insert org-xob--archive-header))
        (insert org-xob--current-header)
        (save-buffer)))
    (add-to-list filelist filename)
    (if (eval filepointer) (org-xob--uncurrent-file filepointer))
    (set filepointer filename)
    filename))

(defun org-xob--clear-file-variables ()
  "All file associated variables set to nil."
  (setq org-xob--KB-files nil
        org-xob--agenda-files nil
        org-xob--log-files nil
        org-xob--archive-files nil
        org-xob--KB-file nil
        org-xob--agenda-file nil
        org-xob--log-file nil
        org-xob--archive-file nil))

(defun org-xob--uncurrent-file (file)
  "Remove current status from given file."
  (save-excursion
    (with-current-buffer (find-file (eval file))
      (goto-char (point-min))
      (re-search-forward "CURRENT")
      (kill-whole-line 1)
      (save-buffer))))

;; --- misc functions ---

(defun org-xob-dt-to-ts ()
  "Convenience function: datetree to timestamp when just under heading."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-set-property
     "CREATED"
     (concat "["
             (truncate-string-to-width
              (nth 4 (org-heading-components)) 14)
             "]"))))

(defun org-xob-dt-to-ts-parent ()
  "Convenience function: datetree to timestamp for subheadings."
  (interactive)
  (org-set-property
   "CREATED"
   (concat "["
           (truncate-string-to-width
            (save-excursion
              (org-up-heading-safe)
              (nth 4 (org-heading-components))) 14)
           "]")))

(provide 'org-xob-backend)
;;; org-xob-backend.el ends here
