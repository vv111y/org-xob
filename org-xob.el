;;; org-xob.el --- advanced knowledge management system in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Willy Rempel
;; Author: Willy Rempel <willy.rempel@acm.org>
;; URL: https://github.com/vv111y/org-xob.el
;; Version: 0.5-pre
;; Package-Requires: ((emacs "25.2") (org) (org-element) (org-id) (org-ql) (cl-lib) (org-super-links))
;; Keywords:

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
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'org-ql)
(require 'org-ql-search)
(require 'cl)
(require 'cl-lib)
(require 'org-super-links)
(require 'hydra)

(declare-function org-super-links-link "org-super-links.el")
(declare-function org-super-links-store-link "org-super-links.el")
(declare-function org-super-links-insert-link "org-super-links.el")

(setq org-super-links-backlink-into-drawer t)
(setq org-super-links-link-prefix nil)
(setq org-super-links-link-postfix nil)
(setq org-super-links-backlink-postfix nil)
(setq org-super-links-related-into-drawer nil)
(setq org-super-links-search-function #'org-xob-get-node)

;;;; Customization

(defgroup org-xob nil
  "Settings for `org-xob'."
  :link '(url-link "http://github.com/vv111y/org-xob.el"))

(defcustom org-xob-something nil
  "This setting does something."
  :type 'something)

;;;; Variables
;;;;; State variables

(defvar org-xob-on-p nil)

(defvar org-xob-today nil "The current day node.")

(defvar org-xob-buffers nil "List of active xob buffers.")

(defvar org-xob-all-buffers nil "List of active xob buffers, including context buffers.")

(defvar org-xob-last-buffer "" "Last xob buffer used.")

(defvar org-xob--open-nodes nil
  "List of all nodes that are opened for editing.")

;; "associate list of displayed context items for an opened node ID."
(cl-defstruct open-node
  "State information about nodes that have been opened for viewing/editing."
  ID title sources)

;;;;; hash tables

(defvar org-xob--table-size 1000000
  "Size of the hash tables.")

(defvar org-xob--title-id nil)

(defvar org-xob--id-title nil)

;;;;; knowledge base sources

(defvar org-xob-available-sources nil
  "List of context information sources that are available in the xob system.")

(defvar org-xob--source-backlinks
  '(:name backlinks
          :tags ("KB" "backlinks")
          :title nil
          :ID nil
          :PID nil
          :getfn org-xob--node-get-link-entries
          :items nil))

(defvar org-xob--source-forlinks
  '(:name forlinks
          :tags ("KB" "forlinks")
          :title nil
          :ID nil
          :PID nil
          :getfn org-xob--node-get-link-entries
          :items nil))

;;;;; file variables
(defvar org-xob-dir "~/xob/"
  "Core directory for exobrain system.")

(defvar org-xob-max-KB-filesize 524288
  "Specifies the largest size the knowledge base org-mode files should grow to. Once the current file reaches the limit, a new file is created.")

(defvar org-xob--KB-filename-prefix "KB-file-"
  "suffix for KB filenames. A simple filecount value is appended for a new name")

(defvar org-xob--log-filename-prefix "log-file-"
  "suffix for log filenames. A simple filecount value is appended for a new name")

(defvar org-xob--agenda-filename-prefix "agenda-file-"
  "suffix for agenda filenames. A simple filecount value is appended for a new name")

(defvar org-xob--archive-filename-prefix "archive-file-"
  "suffix for archive filenames. A simple filecount value is appended for a new name")

;; lists of the xob files
(defvar org-xob--KB-files nil
  "List of all knowledge base files.")

(defvar org-xob--log-files nil
  "List of all xog log files.")

(defvar org-xob--agenda-files nil
  "List of all xob agenda files.")

(defvar org-xob--archive-files nil
  "List of all xob archive files.")

;; the currently active files
(defvar org-xob--KB-file nil
  "The currently active KB file to store previous versions of nodes.")

(defvar org-xob--log-file nil
  "The current log file where day nodes and general activity is recorded.")

(defvar org-xob--agenda-file nil
  "The current xob agenda file where all activity nodes other than day nodes go.")

;; file header strings
(defvar org-xob--xob-header "#+PROPERTY: xob t\n")
(defvar org-xob--log-header "#+PROPERTY: xob-log t\n")
(defvar org-xob--agenda-header "#+PROPERTY: xob-agenda t\n")
(defvar org-xob--archive-header "#+PROPERTY: xob-archive t\n")
(defvar org-xob--current-header "#+PROPERTY: xob-current-file t\n")

;;;;; node/capture variables

(defvar org-xob-labels '("one" "two" "cat" "dog"))

(defvar org-xob--node-types
  '("a.day" "a.project" "a.session" "a.log" "a.log.life" "a.log.tools" "a.log.project" "a.todo" "n.n" "n.topic" "n.bib.article" "n.bib.web" "t.free" "t.project")
  "List of different types of nodes. The default is 'n.n', a generic content node. Type prefixes:
a    -- activity nodes related to tasks and scheduling
t    -- todo type nodes
n    -- node. knowledge base contents
n.b  -- bibliographic entries")

(defvar org-xob--auto-templates '("ad" "as" "al" "all" "alit" "alt" "lp" "nt" "na" "nw" "tf" "tp"))

;;;;; Keymaps

(defvar org-xob-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "org-xob-map"))
        (maps (list
               ;; Mappings go here, e.g.:
               ;; "C-RET" #'(lambda () (message "Override!"))
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))

(defvar org-xob-context-mode-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "org-xob-context-mode-map"))
        (maps (list
               ;; "C-RET" #'(lambda () (message "Override!"))
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))

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
  ("q" nil "Quit" :exit t)
  )

(defun org-xob--up-heading ()
  (if (or (org-folded-p)
          (org-empty-entry-p))
      (org-up-heading-safe)
    (outline-hide-subtree)))

(defun org-xob--down-heading ()
  (if (org-folded-p)
      (progn (org-show-entry)
             (org-show-children))
    (org-goto-first-child)))

(defun org-folded-p ()
  "Returns non-nil if point is on a folded headline or plain list
item. (credit https://emacs.stackexchange.com/a/26840)"
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (invisible-p (point-at-eol))))

(defun org-empty-entry-p ()
  "Returns true if this entry is empty."
  (when (org-at-heading-p)
    (let (a b)
      (save-excursion
        (org-end-of-subtree)
        (setq a (point)))
      (save-excursion
        (end-of-line)
        (setq b (point)))
      (= a b))))

;;;; Macros DONE
(defmacro org-xob-with-xob-on (&rest body)
  (declare (debug (body)))
  `(if org-xob-on-p
       (progn
         ,@body)
     (message "xob is not on.")))

(defmacro org-xob-with-xob-buffer (&rest body)
  `(progn (or (org-xob-edit-buffer-p (current-buffer))
              (and (org-xob-edit-buffer-p org-xob-last-buffer)
                   (switch-to-buffer org-xob-last-buffer))
              (switch-to-buffer (setq org-xob-last-buffer
                                      (org-xob-new-buffer))))
          (if (eq org-xob--display 'dual)
              (org-xob--dual-pane (selected-window))
            (org-xob--single-pane (selected-window)))
          ,@body))

;;;; Commands
;;;;; Main Commands
;;;###autoload
(defun org-xob-start (&optional arg)
  "Start the xob system: load state or initialize new. Open new day node.
Calling with C-u will force a restart."
  (interactive "P")
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
       (if (file-directory-p org-xob-dir) (message "XOB: directory found.")
         (prog1 (message "XOB: directory not found, creating.")
           (make-directory org-xob-dir t)))
       (org-xob--load-state)
       (org-xob--register-files)
       (org-xob--process-files)
       (org-xob--eval-capture-templates)
       (org-xob--open-today)
       (setq org-xob-new-day-timer
             (run-at-time "00:00"
                          (* 24 60 60)
                          'org-xob--open-today)))
      (progn
        (setq org-xob-on-p t)
        (message "XOB: started.")
        (org-xob-info))
    (message "XOB: Unable to (re)start.")))

;;;###autoload
(defun org-xob-stop ()
  "Stop xob system: save all state and cleanup."
  (interactive)
  (if org-xob-on-p
      (progn
        (org-xob--save-state)
        (org-xob--close-buffers)
        (with-current-buffer org-xob-today-buffer
          (save-buffer)
          (kill-buffer))
        (setq org-xob-today nil)
        (setq org-id-extra-files
              (set-difference org-id-extra-files
                              (append org-xob--KB-files
                                      org-xob--agenda-files
                                      org-xob--log-files)))
        (setq org-agenda-files
              (set-difference org-agenda-files
                              (append org-agenda-files
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
  "Focus on a node for editing. If it does not exist, create it."
  (interactive "P")
  (org-xob-with-xob-on
   (pcase-let ((`(,ID ,title) (if arg
                                  (org-xob--get-node-by-type)
                                (org-xob--get-create-node))))
     (and ID
          title
          (org-xob--edit-node ID title)))))

;;;###autoload
(defun org-xob-close-node (&optional ID)
  "Delete a node that has been open for editing. If argument ID
is supplied, then close that node, otherwise close node at point.
All displayed contextual material will also be deleted. Requires
that point be under the nodes top heading, not a subheading."
  (interactive)
  (when-let ((ID (or (and (org-xob--id-goto ID)
                          ID)
                     (org-entry-get (point) "EDIT")))
             ((org-xob--is-edit-node-p)))
    (with-current-buffer org-xob--c-buff
      (mapc #'(lambda (src) (progn (when (org-xob--id-goto src)
                                     (org-mark-subtree)
                                     (call-interactively 'delete-region))))
            (org-xob--this-node-sources ID)))
    (setq org-xob--open-nodes
          (cl-delete-if #'(lambda (x) (string= x ID))
                        org-xob--open-nodes
                        :key #'(lambda (x) (open-node-ID x))))
    (org-mark-subtree)
    (call-interactively #'delete-region)))

;;;###autoload
(defun org-xob-remove-node (&optional ID)
  "Removes node at point from xob system, but does not delete the node itself.
Removes node from the hash tables, and any backlinks in other nodes referencing it.
But ignore any links that reference it. Override xob property.
If called with optional ID argument, then remove the node with that ID."
  (interactive)
  (org-xob-with-xob-on
   (save-window-excursion
     (save-excursion
       (if ID
           (org-id-goto ID))
       (let* ((ID (org-id-get (point)))
              (title (gethash ID org-xob--id-title))
              (forlinks (org-xob--node-get-links "forlinks"))
              link-element)
         (dolist (el forlinks)
           (save-excursion
             (org-id-goto el)
             (save-restriction
               (org-narrow-to-subtree)
               (outline-show-all)
               (setq link-element (org-super-links--find-link ID))
               (if link-element
                   (org-super-links--delete-link link-element)))))
         (remhash ID org-xob--id-title)
         (remhash title org-xob--title-id)
         (org-entry-delete (point) "TYPE")
         (org-entry-delete (point) "xob")
         (org-xob--save-state))))))

;;;###autoload
(defun org-xob-insert-link ()
  "Inserts a properly formatted xob node link at point. If we are in a
xob edit buffer, then also update the forlinks source."
  (interactive)
  (org-xob-with-xob-on
   (save-window-excursion
     (save-excursion
       (pcase-let ((`(,ID ,title) (org-xob--get-create-node)))
         (org-super-links--insert-link (org-id-find ID 'MARKERP)))))
   (when (org-xob--is-edit-node-p)
     (org-xob-sync-edit)
     (org-xob--source-refresh 'forlinks))))

;;;###autoload
(defun org-xob-delete-link ()
  "simple wrapper to call org-superlinks-delete-link"
  (interactive)
  (org-super-links-delete-link))

;;;###autoload
(defun org-xob-refile-region ()
  "Move text in region to the end of the top section of a selected node."
  (interactive)
  (org-xob-with-xob-on
   (when (use-region-p)
     (pcase-let ((`(,ID ,title) (org-xob--get-create-node)))
       (when ID
         (kill-region (point) (mark))
         (save-window-excursion
           (org-with-wide-buffer
            (org-id-goto ID)
            (if (org-goto-first-child)
                (progn
                  (newline 2)
                  (forward-line -1))
              (org-end-of-subtree)
              (newline))
            (org-xob--smart-paste))))
       )
     )))

;;;###autoload
(defun org-xob-heading-to-node ()
  "Convenience function to convert current subtree into a xob KB node."
  (interactive)
  (org-xob-with-xob-on
   (unless (org-xob--is-node-p)
     (let ((title (nth 4 (org-heading-components))))
       (if (gethash title org-xob--title-id)
           (message "heading title conflicts with a xob node. Please rename.")
         (org-xob--new-node (point))
         (let ((filename (buffer-file-name)))
           (unless (member filename org-xob--KB-files)
             (save-excursion
               (goto-char (point-min))
               (insert org-xob--xob-header))
             (save-buffer)
             (push filename org-xob--KB-files))))))))

;;;###autoload
(defun org-xob-rename-node ()
  "Updates xob system with node heading at point."
  (interactive)
  (when (org-xob--is-node-p)
    (org-back-to-heading)
    (when-let ((newname (nth 4 (org-heading-components)))
               (ID (org-entry-get (point) "ID"))
               (oldname (gethash ID org-xob--id-title)))
      (puthash ID newname org-xob--id-title)
      (puthash newname ID org-xob--title-id)
      (remhash oldname org-xob--title-id))))

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

;;;###autoload
(defun org-xob-goto-original ()
  "Go to the original node entry in the knowledge base."
  (interactive)
  (cond ((org-xob--is-node-p nil 'deepcheck))
        ((org-xob--is-edit-node-p)
         (org-id-goto (org-entry-get (point) "EDIT")))
        ((org-id-goto (org-entry-get (point) "PID")))))

;;;;; Display Commands

;;;###autoload
(defun org-xob-toggle-display ()
  "Switch between single or dual pane display."
  (interactive)
  (if (and (boundp org-xob--display)
           (eq 'dual org-xob--display))
      (atomic-change-group
        (org-xob--single-pane (selected-window))
        (org-xob--rewrite-buffer-1-pane))
    (atomic-change-group
      (org-xob--dual-pane (selected-window))
      (org-xob--rewrite-buffer-2-pane))))

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
   #'(lambda () (progn
                 (org-end-of-meta-data t)
                 (let ((p (org--paragraph-at-point)))
                   (if p
                       (buffer-substring-no-properties
                        (or (print (org-element-property :contents-begin p))
                            (print (org-element-property :begin p)))
                        (or (print (org-element-property :contents-end p))
                            (print (org-element-property :end p))))))))))

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

;;;;; Activity Commands DONE
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

;;;; Backend
;;;;; Buffer Functions DONE

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
             (buf1 (get-buffer-create
                         (concat "xob-E-" numbufs)))
             (buf2 (get-buffer-create
                         (concat "xob-C-" numbufs))))
    (with-current-buffer buf1
      (org-mode)
      (org-xob-mode 1)
      (add-hook 'kill-buffer-hook #'org-xob--close-buffer-hook nil 'local)
      (setq-local org-xob--buf 'parent)
      (setq-local org-xob--pair-buf buf2)
      (if single
          (progn (setq-local org-xob--display 'single)
                 (setq-local org-xob--c-buff (current-buffer)))
        (setq-local org-xob--display 'dual)
        (setq-local org-xob--c-buff buf2)))
    (with-current-buffer buf2
      (org-mode)
      (org-xob-mode 1)
      (setq-local org-xob--buf 'child)
      (setq-local org-xob--pair-buf buf1))
    (setq org-xob-last-buffer buf1)
    (push buf1 org-xob-buffers)
    (push buf1 org-xob-all-buffers)
    (push buf2 org-xob-all-buffers)
    buf1))

(defun org-xob--close-buffer-hook ()
  "Properly close xob buffer: remove it from org-xob-buffers, kill context buffer."
  (let ((buf (current-buffer)))
    (setq org-xob-buffers (cl-delete buf org-xob-buffers))
    (setq org-xob-all-buffers (cl-delete buf org-xob-all-buffers))
    (setq org-xob-all-buffers (cl-delete org-xob--pair-buf org-xob-all-buffers))
    (if (eq buf org-xob-last-buffer)
        (setq org-xob-last-buffer (car-safe org-xob-buffers)))
    (kill-buffer org-xob--pair-buf)
    (when (eq 'dual org-xob--display)
      (split-window-right)
      (delete-window))))

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
      (when-let* ((buf (if (org-xob--buffer-p)
                           (window-buffer win)
                         org-xob-last-buffer))
                  (oldwin (selected-window))
                  (winnew (split-window-right)))
        (delete-window oldwin)
        (select-window winnew)
        (set-buffer buf)
        (setq org-xob--display 'single))))

(defun org-xob--dual-pane (win)
  "Use dual-pane interface"
  (unless (window-atom-root win)
    (when-let* ((buf1 (switch-to-buffer
                       org-xob-last-buffer))
                (buf2 (buffer-local-value 'org-xob--pair-buf
                                          buf1))
                (win1 (selected-window))
                (win2 (display-buffer-in-atom-window
                       buf2
                       `((window . ,(selected-window)) (side . right)))))
      (setq org-xob--display 'dual))))


;;;;; Edit Node Functions TODO

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
  (let ((m (point-marker)))
    (funcall func)
    (goto-char m)
    (set-marker m nil))
  (org-xob--mod-to-edit-node)
  (outline-hide-entry))

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
  (if (org-xob--is-node-p)
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
          (not (org-time= etime otime)))))))

(defun org-xob--smart-paste (&optional clip)
  "If the paste is an org subtree, then properly adjust levels for the current heading.
Otherwise just yank. If heading is a xob node, then update modified time property."
  (save-excursion
    (if clip
        (org-paste-subtree nil clip nil nil)
      (if (org-kill-is-subtree-p)
          (org-paste-subtree nil clip t t)
        ;; (insert clip)
        (yank))))
  (org-xob--update-modified-time))

;; -- SYNC --

;; TODO record diff, check if deleted open nodes
;;;###autoload
(defun org-xob-sync-edit (&optional arg sID)
  "Update original xob node with any edits. With optional arg sID
update node with that ID. With universal arg C-u, update all open edit nodes.
Current version performs simple, blunt, whole content replacement."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (let ((updater #'(lambda (ID)
                         (progn
                           (org-xob--id-goto ID)
                           (when-let (((org-xob--is-edit-node-p))
                                      (clip (org-xob--get-full-node 1 nil)))
                             (catch 'nochange
                               (unless (org-xob--modified-time=)
                                   (if (y-or-n-p "Original node has changed. Run ediff?")
                                       (org-xob-ediff-edit)
                                     (unless (y-or-n-p "Really change?")
                                       (throw 'nochange))))
                               (org-xob-goto-original)
                               (org-xob--update-original clip)))))))
        (if (eq current-prefix-arg '(4))
            (dolist (ID (org-xob--get-open-node-ids))
              (funcall updater ID))
          (if sID (funcall updater sID)
            (funcall updater (org-entry-get (point) "ID"))))))))

(defun org-xob--update-original (clip)
  "update contents of KB node at point with string ~clip~.
Note, requires that all KB nodes are stored at level 1.
Does not use the kill-ring."
  (when (org-xob--is-node-p)
    (org-xob--update-node clip)
    (org-xob--update-modified-time)))

(defun org-xob--update-node (clip &optional meta)
  "Update any node with the given string ~clip~. If optional argument
meta is selected, then update the meta section as well (whole subtree)."
  (org-with-wide-buffer
   (org-save-outline-visibility
       (org-narrow-to-subtree)
     (org-show-subtree)
     (org-mark-subtree)
     (unless meta (org-end-of-meta-data t))
     (call-interactively #'delete-region)
     (deactivate-mark 'force)
     (unless meta (org-end-of-meta-data t))
     (insert clip))))

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

;;;;; Node Functions DONE UNCHANGED

(defun org-xob--is-node-p (&optional ID DEEPCHECK)
  "Check if a heading is a xob node. Called interactively it defaults to heading at point.
If an ID argument is supplied, then check the heading associated with it.
With option DEEPCHECK, do not use any table lookup, but check whether the heading
has valid UUID formatted ID and xob TYPE properties in the property drawer.
Deepcheck only works on heading at point, any ID argument is ignored.
Returns the ID if true, nil otherwise."
  (interactive)
  (let ((temp (if ID ID (org-id-get nil))))
    (if temp
        (if DEEPCHECK
            (and
             (string= "t" (org-entry-get (point) "xob"))
             (member (org-entry-get (point) "TYPE") org-xob--node-types)
             (eq 0 (org-uuidgen-p temp))
             (not (org-entry-get (point) "EDIT"))
             temp)
          (if (gethash temp org-xob--id-title) temp)))))

(defun org-xob--is-edit-node-p ()
  "Is point on a node that is in an edit state? Return it's ID if true, nil otherwise."
  (when-let ((id (org-entry-get (point) "EDIT" 'inherit)))
    (and (string= "t" (org-entry-get (point) "xob" 'inherit))
         (eq 0 (org-uuidgen-p id))
         (member "edit" (org-get-tags))
         id)))

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
        ;; todo replace with copy
        (org-id-goto id)
        (org-with-wide-buffer 	;; TODO maybe remove
         (org-save-outline-visibility
             (org-narrow-to-subtree)
           (outline-show-all)
           (condition-case err
               (setq str (funcall selector))
             (t (message "xob: failed to select for %s" id)))
           (deactivate-mark 'force)))))
    str))

;; NOTE written for org-xob--select-content,
;; for syncing to xob kb, requires all source nodes
;; to be at level one - NO nested nodes
(defun org-xob--get-full-node (level &optional meta trimtop trimend)
  "Return a full node as a string (with properties). Used for both edit
node and context presentation. Returns the full node as a string, but with
adjusted specified level. Options:
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

;; --new nodes and links--
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
                   :action (lambda (title) (let ((ID (gethash title org-xob--title-id)))
                                             (unless ID
                                               (setq ID (org-xob--capture title)))
                                             (list ID title))))))

(defun org-xob--get-node-by-type ()
  "Find a node by type."
  (unless org-xob-on-p
    (org-xob-start))
  (if-let ((type (org-xob--select-node-type)))
      (helm :buffer "*xob get paper*"
            :sources (helm-build-sync-source "xob-papers"
                       :candidates (org-xob--find-nodes-by-type type)
                       :volatile t
                       :action (lambda (title) (let ((ID (gethash title org-xob--title-id)))
                                                 (list ID title)))))))

(defun org-xob--select-node-type ()
  (helm :buffer "xob types"
        :sources (helm-build-sync-source "xob-types"
                   :candidates org-xob--node-types
                   :action (lambda (c) c))))

(defun org-xob--find-nodes-by-type (type)
  (org-ql-select org-xob--KB-files
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
          (newline)
          (org-insert-subheading '(4))
          (org-insert-link nil (concat "ID:" ID) title)
          (newline)
          (org-back-to-heading))
        (point-marker)))))

;; --- Node Versioning ---

;; (defun org-xob--sync-node (node)
;;   "Update entry based on local edits."
;;   ;; is node in KB? no, add, else
;;   ;; is node different? no, ignore, else sync/update
;;   nil
;;   )

;; (defun org-xob--diff-node (now-node last-node)
;;   "Creates a diff using =org-xob--delta-executable=.
;; The order of versions is reversed; the diff allows the reconstruction of
;; the last-node from the now-node.
;; The diff is stored in the currently active =org-xob--KB-file=."
;;   (shell-command))
;; ;; (defun org-xob--new-node-diff (nodeID)
;; ;;   (let ((old-id (org-id-store-link node)))))

;; (defun org-xob--diff-filename (node)
;;   (concat
;;    ;; node id
;;    "-"
;;    (format-time-string "%j-%H-%M")))

(defun org-xob--node-add-time-property (property)
  "Convenience function to add high resolution time property.
Maybe useful for syncing."
  (org-entry-put (point) property
                 (number-to-string
                  (car (time-convert (current-time) '10000)))))

;;;;; Contexts Functions TODO

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
               (when (eq '(4) arg) 										;; if arg then repop items
                 (funcall (plist-get src :getfn) src))
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

;;;;; KB Context Functions DONE UNCHANGED

(defun org-xob--node-get-link-entries (source)
  "Populates source item list from the node. The items are represented by their
respective node IDs. Two kinds of links are distinguished: backlinks and forlinks
(which are all other links to xob KB nodes). Assumes org-super-links convention
where the backlinks are in a BACKLINKS drawer."
  (save-window-excursion
    (save-excursion
      ;; todo replace with copy
      (org-id-goto (plist-get source :PID))
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
                         (if (org-xob--is-node-p
                              (setq ID (org-element-property :path link)))
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
                  ;; todo replace with copy
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

;;;;; org-ql predicates TODO test
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
          (property "PID" PID)
          ))

;;;;; org-ql mapping functions TODO test

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

(defun org-xob-find-any-in-buffers ())

(defun org-xob-find-all-in-buffers (ID))

(defun org-xob-map-if-in-buffers (pred func)
  "Apply func to all headings that satisfy paredicate pred in all xob buffers."
  (org-ql-select org-xob-buffers
    `(,pred)
    :action func))

(defun org-xob-map-all-nodes (func)
  (org-ql-select org-xob-buffers
    '(is-xob-node)
    :action func))

(defun org-xob-map-all-edits (func)
  (org-ql-select org-xob-buffers
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
  ;; timestamp
  ;; event
  ;; node
  ;; description
  (save-window-excursion
    (save-excursion
      (save-restriction
        (org-xob--id-goto org-xob-today)
        (org-narrow-to-subtree)
        ()))))

(defun org-xob--auto-clock-in ())
(defun org-xob--auto-clock-out ())
;;;;; xob Management DONE UNCHANGED?

;;;###autoload
(defun org-xob-info ()
  "Give basic information about the xob system."
  (interactive)
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
    "current archive file:\t\t\t\t" org-xob--archive-file "\n")))

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
       (push souce org-xob-available-sources)))

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
  (setq org-agenda-files (append org-agenda-files
                                 org-xob--agenda-files
                                 org-xob--log-files))
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
  (save-excursion
    (with-current-buffer (find-file (eval file))
      (goto-char (point-min))
      (re-search-forward "CURRENT")
      (kill-whole-line 1)
      (save-buffer))))


(provide 'org-xob)

;;; org-xob.el ends here
