;;; org-xob.el --- Advanced knowledge management system in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Willy Rempel
;; Author: Willy Rempel <willy.rempel@acm.org>
;; URL: https://github.com/vv111y/org-xob.el
;; Version: 0.5-pre
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Making the most of org-mode. Another attempt at an exo-brain inspired by: zettlekasten, wikis, roam, and all the other ways to organize ourselves.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:
;; + org
;; + org-element
;; + org-id
;; + org-ql
;; + cl-lib
;; + org-super-links

;; Then put this file in your load-path, and put this in your init
;; file:
;; (require 'org-xob)

;;;; Usage

;; Run one of these commands:

;; `org-xob-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `org-xob' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2]

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
(require 'org-xob-backend)

;;;; Minor Mode & Keybindings

;;;###autoload
(define-minor-mode org-xob-mode
  "Org-Exobrain Minor Mode."
  :lighter "Ⓧ"
  :keymap  (let ((map (make-sparse-keymap))) map)
  :group 'org-xob
  :require 'org-xob
  (if org-xob-mode
      (progn
        ;; (remove-hook 'kill-buffer-hook #'org-xob--close-buffer-hook 'local)
        )
    (progn
      ;; (unless org-xob-on-p (org-xob-start))
      ;; (add-hook 'kill-buffer-hook #'org-xob--close-buffer-hook nil 'local)
      )))

;;;###autoload
(defhydra org-xob-hydra (:columns 4)
  ("h" (org-xob--up-heading) "up")
  ("j" (org-goto-sibling) "next")
  ("k" (org-goto-sibling 'previous) "previous")
  ("l" (org-xob--down-heading) "down")
  ("L" (org-show-children) "children")
  ("c" (org-xob-clear-heading) "clear")
  ("s" (org-xob-to-summary) "summary")
  ("S" (org-xob-to-section) "section")
  ("t" (org-xob-to-node-tree) "tree")
  ("T" (org-xob-to-full-node) "full")
  ("e" (org-xob-to-edit) "edit")

  ;; Bulk operations (capital letters)
  ("C-s" (org-xob-bulk-to-summary) "bulk: summary")
  ("C-S" (org-xob-bulk-to-section) "bulk: section")
  ("C-t" (org-xob-bulk-to-node-tree) "bulk: tree")
  ("C-T" (org-xob-bulk-to-full-node) "bulk: full")
  ("C-c" (org-xob-bulk-clear-all) "bulk: clear all")

  ("q" nil "Quit" :exit t)
  )

(defun org-xob--up-heading ()
  "Xob hydra navigation: fold heading at point first, otherwise go up to parent."
  (if (or (org-xob-folded-p)
          (org-xob-empty-entry-p))
      (org-up-heading-safe)
    (outline-hide-subtree)))

(defun org-xob--down-heading ()
  "Xob hydra navigation: unfold heading one level first before going in."
  (if (org-xob-folded-p)
      (progn (org-show-entry)
             (org-show-children))
    (org-goto-first-child)))

(defun org-xob-folded-p ()
  "Returns non-nil if point is on a folded headline or plain list
item. (credit https://emacs.stackexchange.com/a/26840)."
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (invisible-p (point-at-eol))))

(defun org-xob-empty-entry-p ()
  "Return non-nil if the current heading contains no text.
Whitespace and blank lines are ignored."
  (when (org-at-heading-p)
    (let ((heading-end (save-excursion
                         (end-of-line)
                         (point)))
          (entry-end (save-excursion
                       (outline-next-heading)
                       (skip-chars-backward " \t\n")
                       (point))))
      (= heading-end entry-end))))

;;;; Commands
;;;;; Main Commands
;;;###autoload
(defun org-xob-start (&optional arg)
  "Start the xob system: load state or initialize new. Open new day node.
Calling with C-u will force a restart."
  (interactive "P")
  ;; First set the directory based on current name
  ;; (unless (and org-xob-dir (file-directory-p org-xob-dir))
  ;;   (let ((dir (cdr (assoc org-xob-current-name org-xob-known-dirs))))
  ;;     (if dir
  ;;         (setq org-xob-dir dir)
  ;;       (message "Warning: No valid directory for current xob repository"))))
  (let* ((repo (completing-read "Select xob repository: "
                                (mapcar #'car org-xob-known-dirs)
                                nil t))
         (dir (cdr (assoc repo org-xob-known-dirs))))
    (when (and dir (file-directory-p dir))
      (setq org-xob-dir dir)
      (setq org-xob-current-name repo)
      (if (equal arg '(4))
          (setq org-xob-on-p nil))
      (if (and
           (if org-xob-on-p (progn (message "XOB: already started.") nil) t)
           (and
            (add-hook 'org-capture-prepare-finalize-hook #'org-xob--new-node)
            (add-hook 'org-follow-link-hook #'org-xob--link-hook-fn)
            (add-hook 'ediff-quit-hook #'org-xob--ediff-quit-hook)
            (add-hook 'org-super-links-pre-link-hook #'org-xob--super-links-hook)
            (message "XOB: hooks enabled."))
           (not (setq org-xob--open-nodes nil))
           (org-xob--load-state)
           (org-xob--scan-and-register-files)
           (org-xob--eval-capture-templates)
           (org-link-make-regexps)
           (org-xob--open-today)
           (setq org-xob-new-day-timer
                 (run-at-time "00:00"
                              (* 24 60 60)
                              'org-xob--open-today))
           (org-xob-with-xob-buffer
            ;; Auto-setup dual-pane if enabled
            (org-xob--auto-setup-dual-pane)
            (setq org-xob-on-p t)
            (message "XOB: started.")))
          (message "Invalid xob repository: %s" repo)))
    (message "XOB: Unable to (re)start.")))

;;;###autoload
(defun org-xob-stop ()
  "Stop xob system: save all state and cleanup."
  (interactive)
  (if org-xob-on-p
      (progn
        (org-xob--save-state)
        (org-xob--close-buffers)
        (if (and (bound-and-true-p org-xob-today-buffer)
                 (buffer-live-p org-xob-today-buffer))
            (with-current-buffer org-xob-today-buffer
              (save-buffer)
              (kill-buffer)
              ))
        (setq org-xob-today nil)
        (setq org-id-extra-files
              (set-difference org-id-extra-files
                              (append org-xob--KB-files
                                      org-xob--agenda-files
                                      org-xob--log-files)))
        (org-xob--clear-file-variables)
        (remove-hook 'org-capture-prepare-finalize-hook #'org-xob--new-node)
        (remove-hook 'org-follow-link-hook #'org-xob--link-hook-fn)
        (remove-hook 'ediff-quit-hook #'org-xob--ediff-quit-hook)
        (remove-hook 'org-super-links-pre-link-hook #'org-xob--super-links-hook)
        (cancel-timer org-xob-new-day-timer)
        (setq org-xob-on-p nil)
        (message "XOB: stopped."))))

;;;###autoload
(defun org-xob-switch-repo (&optional repo-name)
  "Switch to a different xob repository.
If REPO-NAME is provided, switch to that repository.
Otherwise prompt for selection from known repositories."
  (interactive)
  (let* ((repo (or repo-name
                   (completing-read "Select xob repository: "
                                    (mapcar #'car org-xob-known-dirs)
                                    nil t)))
         (dir (cdr (assoc repo org-xob-known-dirs))))

    (when (and dir (file-directory-p dir))
      ;; Stop current xob if running
      (when org-xob-on-p
        (org-xob-stop))

      ;; Update variables
      (setq org-xob-dir dir)
      (setq org-xob-current-name repo)

      ;; Start the xob system
      (org-xob-start)
      (message "Switched to xob repository: %s (%s)" repo dir)

      ;; Rebuild with new directory
      ;; (org-xob-rebuild)
      )

    (unless dir
      (message "Invalid xob repository: %s" repo))))

;;;###autoload
(defun org-xob-add-repo (name directory)
  "Add a new xob repository to the list of known repositories.
NAME is a user-friendly name for the repository.
DIRECTORY is the path to the xob directory."
  (interactive "sRepository name: \nDDirectory: ")
  (let ((dir (file-name-as-directory (expand-file-name directory))))
    ;; Create the directory if it doesn't exist
    (unless (file-directory-p dir)
      (if (y-or-n-p (format "Directory %s doesn't exist. Create it? " dir))
          (make-directory dir t)
        (error "Cannot add repository without directory")))

    ;; Add to the list
    (add-to-list 'org-xob-known-dirs (cons name dir))
    (message "Added xob repository: %s (%s)" name dir)))

;;;###autoload
(defun org-xob-remove-repo (name)
  "Remove a repository from the list of known repositories."
  (interactive
   (list (completing-read "Remove repository: "
                          (mapcar #'car org-xob-known-dirs))))
  (setq org-xob-known-dirs
        (assoc-delete-all name org-xob-known-dirs))
  (message "Removed xob repository: %s" name))


;;;;; Node Commands
;;;###autoload
(defun org-xob-open-day ()
  "Open todays node."
  (interactive)
  (org-xob-with-xob-on
   (condition-case nil
       (progn
         (org-id-goto org-xob-today)
         (org-narrow-to-subtree))
     (error (message "todays day node missing.")))))

;;;###autoload
(defun org-xob-get-node (&optional arg)
  "Open one or more nodes for editing. If node does not exist, create it."
  (interactive "P")
  (org-xob-with-xob-on
   (org-xob--do-select-nodes nil arg #'org-xob--edit-node)))

;;;###autoload
(defun org-xob-close-node (&optional arg ID)
  "Delete a node that has been open for editing. If argument ID
is supplied, then close that node, otherwise close node at point.
All displayed contextual material will also be deleted. Requires
that point be under the nodes top heading, not a subheading."
  (interactive "P")
  (cl-flet ((close-node ()
              (when-let ((ID (org-entry-get (point) "EDIT"))
                         ((org-xob--is-edit-node-p)))
                (if (buffer-live-p org-xob--c-buff)
                    (with-current-buffer org-xob--c-buff
                      (mapc #'(lambda (src)
                                (progn (when (org-xob--id-goto src)
                                         (org-mark-subtree)
                                         (call-interactively 'delete-region))))
                            (org-xob--this-node-sources ID))))
                (setq org-xob--open-nodes
                      (cl-delete-if #'(lambda (x) (string= x ID))
                                    org-xob--open-nodes
                                    :key #'(lambda (x) (open-node-ID x))))
                (org-mark-subtree)
                (call-interactively #'delete-region)
                (goto-char (point-min)))))
    (cond (arg (org-xob-map-buffer-edit-nodes #'close-node))
          ((and ID (org-xob--id-goto ID)) (close-node))
          (t (close-node)))))

;;;###autoload
(defun org-xob-sync-edit (&optional arg sID)
  "Update original xob node with any edits. With optional arg sID
update node with that ID. With universal arg C-u, update all open edit nodes.
Current version performs simple, blunt, whole content replacement."
  (interactive "P")
  (org-xob-with-xob-on
   (let ((id (or sID (org-entry-get (point) "ID"))))
     (if arg
         (org-xob-map-all-edits
          #'(lambda () (org-xob--update-original
                        (org-entry-get (point) "ID"))))
       (org-xob--update-original id)))))

;;;###autoload
(defun org-xob-remove-node (&optional ID delete-content)
  "Removes node at point from xob system, with optional content deletion.
Removes node from the hash tables, and cleans up both outgoing (forlinks)
and incoming (backlinks) references to maintain knowledge graph integrity.
Removes xob properties to de-register the node from the system.

If DELETE-CONTENT is non-nil, also deletes the actual org subtree content.
If called with optional ID argument, then remove the node with that ID.

When called interactively:
- With no prefix: removes from xob system but keeps content
- With C-u prefix: prompts whether to also delete content
- With C-u C-u prefix: removes from system AND deletes content"
  (interactive
   (list nil
         (cond
          ((equal current-prefix-arg '(16)) t)  ; C-u C-u - delete content
          ((equal current-prefix-arg '(4))     ; C-u - prompt
           (y-or-n-p "Also delete node content? "))
          (t nil))))  ; no prefix - keep content
  (org-xob-with-xob-on
   (save-window-excursion
     (save-excursion
       (when ID (org-id-goto ID))
       (let* ((ID (org-id-get (point)))
              (title (gethash ID org-xob--id-title))
              (forlinks (org-xob--node-get-links 'forlinks))
              (backlinks (org-xob--node-get-links 'backlinks))
              link-element)

         ;; Clean up outgoing links (forlinks) - links FROM this node TO others
         ;; Note: Link cleanup may not always succeed due to org-super-links limitations
         ;; Users may need to manually remove remaining links if necessary
         (dolist (el forlinks)
           (save-excursion
             (condition-case err
                 (progn
                   (org-id-goto el)
                   (save-restriction
                     (org-narrow-to-subtree)
                     (outline-show-all)
                     (setq link-element (org-super-links--find-link ID))
                     (when link-element
                       (org-super-links--delete-link link-element))))
               (error
                (message "Warning: Could not remove outgoing link to %s (may need manual cleanup)" el)))))

         ;; Clean up incoming links (backlinks) - links FROM others TO this node
         ;; Note: Link cleanup may not always succeed due to org-super-links limitations
         (dolist (el backlinks)
           (save-excursion
             (condition-case err
                 (progn
                   (org-id-goto el)
                   (save-restriction
                     (org-narrow-to-subtree)
                     (outline-show-all)
                     ;; Find and remove the link from this node back to our target
                     (setq link-element (org-super-links--find-link ID))
                     (when link-element
                       (org-super-links--delete-link link-element))))
               (error
                (message "Warning: Could not remove incoming link from %s (may need manual cleanup)" el)))))         ;; Remove from xob system
         (org-xob--log-event "removed" ID)
         (remhash ID org-xob--id-title)
         (remhash title org-xob--title-id)
         (org-entry-delete (point) "TYPE")
         (org-entry-delete (point) "xob")

         ;; Optionally delete the actual content
         (when delete-content
           (org-xob--log-event "content deleted" ID)
           (org-cut-subtree))

         (org-xob--save-state)

         (message "Node '%s' %s from xob system"
                  title
                  (if delete-content "removed and deleted" "removed")))))))

;;;###autoload
(defun org-xob-heading-to-node ()
  "Convenience function to convert current subtree into a xob KB node."
  (interactive)
  (org-xob-with-xob-on
   (unless (org-xob--is-node-p)
     (let ((title (nth 4 (org-heading-components))))
       (if (gethash title org-xob--title-id)
           (message "heading title conflicts with a xob node. Please rename.")
         (when (org-at-heading-p)
           (org-xob--new-node (point-marker))
           (let ((filename (buffer-file-name)))
             (unless (member filename org-xob--KB-files)
               (save-excursion
                 (goto-char (point-min))
                 (insert org-xob--xob-header))
               (save-buffer)
               (push filename org-xob--KB-files)))))))))

;;;###autoload
(defun org-xob-rename-node ()
  "Updates xob system with node heading at point."
  (interactive)
  (when (org-xob--is-node-p)
    (org-back-to-heading)
    (when-let ((newname (nth 4 (org-heading-components)))
               (ID (org-entry-get (point) "ID"))
               (oldname (gethash ID org-xob--id-title)))
      (org-xob--log-event "rename" ID)
      (puthash ID newname org-xob--id-title)
      (puthash newname ID org-xob--title-id)
      (remhash oldname org-xob--title-id)
      (org-xob--log-event "-> new" ID))))

;;;###autoload
(defun org-xob-goto-original ()
  "Go to the original node entry in the knowledge base."
  (interactive)
  (cond ((org-xob--is-edit-node-p)
         (org-id-goto (org-entry-get (point) "EDIT")))
        ((org-id-goto (org-entry-get (point) "PID")))))

;;;;; Node Contents Commands
;;;###autoload
(defun org-xob-insert-link (&optional arg)
  "Inserts a properly formatted xob node link at point. If we are in a
xob edit buffer, use a xob placeholder link and update the forlinks source.
Note: linking does not occur until edit is synced with the KB."
  (interactive "P")
  (org-xob-with-xob-on
   (org-xob--do-select-nodes
    nil arg
    #'(lambda (ID title)
        (if (org-xob--is-edit-node-p)
            (progn
              (org-insert-link nil (concat "xob:" ID) title)
              (insert " ")
              (org-xob-show-source 'forlinks))
          (org-super-links--insert-link (org-id-find ID 'MARKERP))
          (goto-char (org-element-property :end  (org-element-context)))
          (insert " ")
          (org-xob--log-event "link" (org-entry-get (point) "ID"))
          (org-xob--log-event "-> to" ID))))))

;;;###autoload
(defun org-xob-delete-link ()
  "Delete link at point in a xob node. If this is an edit node, then insert
placeholder link (deletion will occur on sync). If it is a regular node,
then just use org-super-links."
  (interactive)
  (if (org-xob--is-edit-node-p)
      (let* ((link (org-element-context))
             (loc (org-element-property :path link))
             (beg (org-element-property :begin link))
             (end (org-element-property :end link))
             (desc (buffer-substring-no-properties
                    (org-element-property :contents-begin link)
                    (org-element-property :contents-end link))))
        (funcall-interactively #'delete-region beg end)
        (org-insert-link nil (concat "xobdel:"
                                     loc)
                         desc))
    (org-xob--log-event "X link" (org-element-property :path (org-element-context)))
    (org-xob--log-event "-> from" (org-entry-get (point) "ID"))
    (org-super-links-delete-link)))

(defun org-xob--refile-region-internal (ID title)
  (let* ((tbuffer (marker-buffer (org-id-find ID t)))
         (changes (nconc (prepare-change-group (current-buffer))
                         (prepare-change-group tbuffer)
                         (prepare-change-group org-xob-today-buffer)))
         (beg (region-beginning))
         (end (region-end))
         (snip (buffer-substring-no-properties beg
                                               (min end
                                                    (+ 70 beg))))
         eid flag)
    (unwind-protect
        (progn
          (activate-change-group changes)
          (kill-region (point) (mark))
          (save-window-excursion
            (save-excursion
              (if (setq eid (and (org-xob--goto-edit ID)
                                 (org-xob--is-edit-node-p)))
                  (progn
                    (org-xob--paste-top-section)
                    (org-xob-sync-edit))
                (org-id-goto ID)
                (org-xob--paste-top-section))
              (org-xob--log-event "refile" ID)
              (org-xob--log-event "-> snip" snip))
            (setq flag t))))
    (if flag
        (accept-change-group changes)
      (cancel-change-group changes)
      (message "xob: failed to refile section to node: %s"
               (gethash ID org-xob--id-title)))))

;;;###autoload
(defun org-xob-refile-region (&optional arg)
  "Move text in region to the end of the top section of a selected node.
If an open edit node is present, then refiled text is sent there, otherwise it is
written to the original KB node, any open edit nodes are automatically
updated."
  (interactive "P")
  (org-xob-with-xob-on
   (when (use-region-p)
     (org-xob--do-select-nodes
      t arg
      #'org-xob--refile-region-internal))))

;;;###autoload
(defun org-xob-add-node-labels ()
  "Select labels to apply to node at point, or at optional node specified by ID."
  (interactive)
  (org-xob-with-xob-on
   (if (org-xob--is-node-p)
       (helm :buffer "xob labels"
             :sources (helm-build-sync-source "xob-labels"
                        :candidates org-xob-labels
                        :action (lambda (c)
                                  (org-entry-put (point) "LABELS"
                                                 (string-join (helm-marked-candidates) " ")))))
     (message "XOB: not on a xob node."))))

;;;###autoload
(defun org-xob-change-node-type ()
  "Change the type for node at point, or at optional node specified by ID."
  (interactive)
  (org-xob-with-xob-on
   (if (or (org-xob--is-node-p)
           (org-xob--is-edit-node-p))
       (helm :buffer "xob types"
             :sources (helm-build-sync-source "xob-types"
                        :candidates org-xob--node-types
                        :action (lambda (c)
                                  (org-entry-put (point) "TYPE" c)))))))

;;;;; Special node access

;;;###autoload
(defun org-xob-get-project-nodes ()
  "Select from project nodes."
  (interactive)
  (unless org-xob-on-p
    (org-xob-start))
  (org-xob-with-xob-on
   (org-xob--do-select-nodes nil "a.project" #'org-xob--edit-node)))

;;;###autoload
(defun org-xob-file-paper ()
  "refile region into a n.bib.article entry. Prompts for the title."
  (interactive)
  (org-xob-with-xob-on
   (when (use-region-p)
     (save-window-excursion
       (save-excursion
         (let ((name (read-string "Paper title:")))
           (org-xob--capture name)
           (org-xob--refile-region-internal org-xob--last-captured
                                            name)
           (org-id-goto org-xob--last-captured)
           (org-entry-put (point) "TYPE" "n.bib.article")
           (org-entry-put (point) "NOTER_DOCUMENT" "~/Zotero/storage/")))))))

;;;;; Region to Node Conversion

;;;###autoload
(defun org-xob-region-to-node ()
  "Convert selected text region to a new xob node and replace with a link.
Prompts for the node title and creates a new node with the region content."
  (interactive)
  (org-xob-with-xob-on
   (if (use-region-p)
       (let ((title (read-string "Node title: "))
             (beg (region-beginning))
             (end (region-end)))
         (when (and title (not (string-empty-p title)))
           (let ((node-id (org-xob--region-to-node-with-link beg end title)))
             (message "Created node '%s' with ID: %s" title node-id)
             (org-xob--log-event "region->node conversion" node-id))))
     (message "No region selected"))))

;;;###autoload
(defun org-xob-node-to-region ()
  "Convert a node link at point back to inline text content.
Replaces the link with the content of the referenced node."
  (interactive)
  (org-xob-with-xob-on
   (let ((link (org-element-context)))
     (if (and (eq (org-element-type link) 'link)
              (string= (org-element-property :type link) "id"))
         (let ((node-id (org-element-property :path link)))
           (when (gethash node-id org-xob--id-title)
             (org-xob--node-to-region node-id)
             (message "Converted node to inline text: %s"
                      (gethash node-id org-xob--id-title))))
       (message "Point is not on an xob node link")))))

;;;;; Display Commands

;; org-xob--display

;;;###autoload
(defun org-xob-dual-display ()
  (interactive)
  (org-xob--dual-pane (selected-window)))

;;;###autoload
(defun org-xob-single-display ()
  (interactive)
  (org-xob--single-pane (selected-window)))

;;;###autoload
(defun org-xob-toggle-display ()
  "Switch between single or dual pane display."
  (interactive)
  (if (and (boundp 'org-xob--display)
           (eq 'dual org-xob--display))
      (atomic-change-group
        (org-xob--single-pane (selected-window))
        (org-xob--rewrite-buffer-1-pane))
    (atomic-change-group
      (org-xob--dual-pane (selected-window))
      (org-xob--rewrite-buffer-2-pane))))

;;;###autoload
(defun org-xob-setup-dual-pane ()
  "Manually set up dual-pane window layout with context buffer."
  (interactive)
  (org-xob-with-xob-buffer
   (org-xob--auto-setup-dual-pane)))

;;;;; Context Presentation Commands

;;;###autoload
(defun org-xob-show-backlinks (&optional arg)
  "Add backlinks contents to the context buffer."
  (interactive)
  (org-xob-with-xob-buffer
   (org-xob-show-source 'backlinks arg)))

;;;###autoload
(defun org-xob-show-forlinks (&optional arg)
  "Add forlinks contents to the context buffer."
  (interactive)
  (org-xob-with-xob-buffer
   (org-xob-show-source 'forlinks arg)))

;;;###autoload
(defun org-xob-toggle-auto-display-links ()
  "Toggle the auto-display links setting.
Cycles through: t -> backlinks -> forlinks -> nil -> t"
  (interactive)
  (setq org-xob-auto-display-links
        (cond
         ((eq org-xob-auto-display-links t) 'backlinks)
         ((eq org-xob-auto-display-links 'backlinks) 'forlinks)
         ((eq org-xob-auto-display-links 'forlinks) nil)
         (t t)))
  (message "org-xob auto-display links: %s"
           (cond
            ((eq org-xob-auto-display-links t) "both backlinks and forlinks")
            ((eq org-xob-auto-display-links 'backlinks) "backlinks only")
            ((eq org-xob-auto-display-links 'forlinks) "forlinks only")
            (t "disabled"))))

;;;###autoload
(defun org-xob-toggle-auto-dual-pane ()
  "Toggle the auto dual-pane layout setting."
  (interactive)
  (setq org-xob-auto-dual-pane (not org-xob-auto-dual-pane))
  (message "org-xob auto dual-pane: %s"
           (if org-xob-auto-dual-pane "enabled" "disabled"))
  ;; If enabling and xob is running, set up dual-pane now
  (when (and org-xob-auto-dual-pane org-xob-on-p)
    (org-xob--auto-setup-dual-pane)))

;;;###autoload
(defun org-xob-ql-search (qname query)
  "Use org-ql to search the KB. Creates a new source in the context buffer."
  (interactive "sQuery Name:
sQuery Form: ")
  (org-xob-with-xob-buffer
   ;; get query
   ;; store it / create source object
   ;; send it through to display
   ;; list of saved queries / persist too
   nil))

;;;###autoload
(defun org-xob-refresh-contexts ()
  "Refresh all displayed sources for node at point."
  (interactive)
  (org-xob-map-node-sources (org-entry-get (point) "EDIT")
                            'org-xob--source-refresh))

;;;###autoload
(defun org-xob-clear-heading ()
  "Clears contents of context entry at point, or for whole context source."
  (interactive)
  (org-xob--context-copy-paste ""))

;;;###autoload
(defun org-xob-to-summary ()
  "Show KB node summary. This is defined as the first paragraph if it exists."
  (interactive)
  (org-xob--context-copy-paste
   "sum"
   #'(lambda ()
       (progn
         (org-end-of-meta-data t)
         (let ((p (org--paragraph-at-point)))
           (if p
               (buffer-substring-no-properties
                (or (org-element-property :contents-begin p)
                    (org-element-property :begin p))
                (or (org-element-property :contents-end p)
                    (org-element-property :end p)))))))))

;;;###autoload
(defun org-xob-to-node-tree ()
  "Show only subheadings of KB node."
  (interactive)
  (org-xob--context-copy-paste
   "tree"
   #'(lambda ()
       (let (lines)
         (org-map-tree
          (lambda ()
            (push (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))
                  lines)))
         (setq lines (nreverse lines))
         (pop lines)
         (mapconcat 'identity lines "\n")))
   #'(lambda (str) (if (org-kill-is-subtree-p str)
                       (org-paste-subtree
                        (+ 1 (org-current-level)) str)))))

;;;###autoload
(defun org-xob-to-section ()
  "Show the top section of KB node, no subheadings."
  (interactive)
  (org-xob--context-copy-paste
   "sec"
   #'(lambda () (let ((beg) (end))
                  (org-end-of-meta-data t)
                  (org-back-over-empty-lines)
                  (setq beg (point))
                  (outline-next-heading)
                  (setq end (- (point) 1))
                  (buffer-substring beg end)))))

;;;###autoload
(defun org-xob-to-full-node ()
  "Show the full KB node, excepting properties drawer, planning & clocking information."
  (interactive)
  (org-xob--context-copy-paste
   "full"
   #'(lambda () (org-xob--get-full-node 3 nil))))  ;; TODO change from hard code 3

;;;###autoload
(defun org-xob-to-edit ()
  "If context entry is a xob node, then open the xob node in edit mode."
  (interactive)
  (save-window-excursion
    (save-excursion
      (when-let ((id (org-entry-get (point) "PID")))
        (org-xob--edit-node id (gethash id org-xob--id-title))))))

;;;;; Bulk Context Operations

;;;###autoload
(defun org-xob-bulk-to-summary ()
  "Show summary for ALL context entries in the buffer."
  (interactive)
  (org-xob--map-all-sources
   #'(lambda ()
       (let ((pid (org-entry-get (point) "PID")))
         (when (org-uuidgen-p pid)
           (org-xob--clear-node)
           (org-set-tags "sum")
           (when-let ((str (org-xob--select-content
                            pid
                            #'(lambda ()
                                (progn
                                  (org-end-of-meta-data t)
                                  (let ((p (org--paragraph-at-point)))
                                    (if p
                                        (buffer-substring-no-properties
                                         (or (org-element-property :contents-begin p)
                                             (org-element-property :begin p))
                                         (or (org-element-property :contents-end p)
                                             (org-element-property :end p))))))))))
             (org-end-of-subtree)
             (newline)
             (insert str)
             (outline-hide-subtree)
             (org-show-entry)))))))

;;;###autoload
(defun org-xob-bulk-to-section ()
  "Show section for ALL context entries in the buffer."
  (interactive)
  (org-xob--map-all-sources
   #'(lambda ()
       (let ((pid (org-entry-get (point) "PID")))
         (when (org-uuidgen-p pid)
           (org-xob--clear-node)
           (org-set-tags "sec")
           (when-let ((str (org-xob--select-content
                            pid
                            #'(lambda ()
                                (let ((beg) (end))
                                  (org-end-of-meta-data t)
                                  (org-back-over-empty-lines)
                                  (setq beg (point))
                                  (outline-next-heading)
                                  (setq end (- (point) 1))
                                  (buffer-substring beg end))))))
             (org-end-of-subtree)
             (newline)
             (insert str)
             (outline-hide-subtree)
             (org-show-entry)))))))

;;;###autoload
(defun org-xob-bulk-to-node-tree ()
  "Show node tree for ALL context entries in the buffer."
  (interactive)
  (org-xob--map-all-sources
   #'(lambda ()
       (let ((pid (org-entry-get (point) "PID")))
         (when (org-uuidgen-p pid)
           (org-xob--clear-node)
           (org-set-tags "tree")
           (when-let ((str (org-xob--select-content
                            pid
                            #'(lambda ()
                                (let (lines)
                                  (org-map-tree
                                   (lambda ()
                                     (push (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position))
                                           lines)))
                                  (setq lines (nreverse lines))
                                  (pop lines)
                                  (mapconcat 'identity lines "\n"))))))
             (org-end-of-subtree)
             (newline)
             (if (org-kill-is-subtree-p str)
                 (org-paste-subtree (+ 1 (org-current-level)) str)
               (insert str))
             (outline-hide-subtree)
             (org-show-entry)))))))

;;;###autoload
(defun org-xob-bulk-to-full-node ()
  "Show full node for ALL context entries in the buffer."
  (interactive)
  (org-xob--map-all-sources
   #'(lambda ()
       (let ((pid (org-entry-get (point) "PID")))
         (when (org-uuidgen-p pid)
           (org-xob--clear-node)
           (org-set-tags "full")
           (when-let ((str (org-xob--select-content
                            pid
                            #'(lambda () (org-xob--get-full-node 3 nil)))))
             (org-end-of-subtree)
             (newline)
             (insert str)
             (outline-hide-subtree)
             (org-show-entry)))))))

;;;###autoload
(defun org-xob-bulk-clear-all ()
  "Clear ALL context entries in the buffer."
  (interactive)
  (org-xob--map-all-sources
   #'(lambda ()
       (org-xob--clear-node))))

;;;;; Activity Commands
;;;###autoload
(defun org-xob-log-done (&optional ID)
  "Convert a complete TODO into a log entry for future reference.
If ID is given, then convert todo with that ID."
  (interactive)
  (save-excursion
    (save-window-excursion
      (when ID (org-id-goto ID))
      (when (org-xob--is-node-p)
        (when (org-entry-is-done-p)
          (org-todo 'none)
          (org-schedule '(4))
          (org-deadline '(4))
          (when-let* ((log-start (org-log-beginning))
                      (log-entry (org-xob--insert-link-header
                                  (org-id-get)
                                  (nth 4 (org-heading-components))
                                  org-xob-today)))
            (goto-char log-start)
            (forward-line -1)
            (org-mark-element)
            (kill-region (point) (mark))
            (save-excursion
              (org-id-goto log-entry)
              (org-end-of-meta-data)
              (yank))))
        (org-entry-put (point) "TYPE" "a.log")
        (org-cut-subtree)
        (with-current-buffer (find-file org-xob--KB-file)
          (goto-char (point-max))
          (org-paste-subtree 1 nil nil 'remove))))))

(defun org-xob-todo-at-point ()
  "create a todo entry for node at point. Todo is filed in current inbox."
  (interactive)
  (org-super-links-store-link)
  (save-excursion
    (org-id-goto (org-xob--capture "tf"))
    (org-end-of-meta-data)
    (insert "Re:\n")
    (org-super-links--insert-link)))


(provide 'org-xob)
;;; org-xob.el ends here
