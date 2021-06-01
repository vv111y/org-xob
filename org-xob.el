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

(defvar org-xob-last-buffer "" "Last xob buffer used.")

(defvar org-xob--open-nodes ()
  "List of all nodes that are opened for editing.")

;; "associate list of displayed context items for an opened node ID."
(cl-defstruct open-node ID sources)

;;;;; hash tables

(defvar org-xob--table-size 1000000
  "Size of the hash tables.")

(defvar org-xob--title-id nil)

(defvar org-xob--id-title nil)

;;;;; knowledge base sources

(defvar org-xob-available-sources
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

(defvar org-xob--node-types '("a.day" "a.project" "a.session" "a.log" "a.log.life" "a.log.tools" "a.log.project" "a.todo" "n.n" "n.topic" "n.bib.article" "n.bib.web" "t.free" "t.project"))

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

;;;; Minor Mode

;;;###autoload
(define-minor-mode org-xob-mode
  "Org-Exobrain Minor Mode."
  :lighter "Ⓧ"
  :keymap  (let ((map (make-sparse-keymap))) map)
  :group 'org-xob
  :require 'org-xob
  (if org-xob-mode
      (progn
        (unless org-xob-on-p (org-xob-start))
        ;; (evil-define-key 'normal 'local (kbd "t") 'org-xob-to-node-tree)
        )
    ))

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
;;;;; Main Commands DONE
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
(defun org-xob-get-node ()
  "Focus on a node for editing. If it does not exist, create it."
  (interactive)
  (org-xob-with-xob-on
   (pcase-let ((`(,ID ,title) (org-xob--get-create-node)))
     (org-xob--edit-node ID title))))

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

;; TODO some macro expansion error
;;;###autoload
(defun org-xob-insert-link ()
  "Inserts a properly formatted xob node link at point. If we are in a xob buffer,
then also update the forlinks source."
  (interactive)
  (org-xob-with-xob-on
   (pcase-let ((`(,ID ,title) (org-xob--get-create-node)))
     (org-super-links--insert-link (org-id-find ID 'MARKERP))
     (org-xob-with-xob-buffer
      (org-xob--source-refresh 'forlinks)))))

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
   (if (use-region-p)
       (pcase-let ((`(,ID ,title) (org-xob--get-create-node)))
         (when (bound-and-true-p ID)
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
              ;; todo refactor out, for reuse
              (if (org-kill-is-subtree-p)
                  (org-paste-subtree
                   (+ 1 (org-current-level)) nil t t)
                (yank)
                (org-xob--modify-time)))))))))

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
   (if (org-xob--is-node-p)
       (helm :buffer "xob types"
             :sources (helm-build-sync-source "xob-types"
                        :candidates org-xob--node-types
                        :action (lambda (c)
                                  (org-entry-put (point) "TYPE" c)))))))

;;;;; Display Commands TODO redo again

;; TODO redo with new regime
;;;###autoload
(defun org-xob-to-side-window (side buf)
  "Move subtree at point to window on side of current one. If there is no window
then make one. If no xob buffer is there, make a new one. If optional buf is
specified, then use that buffer."
  (interactive (list (completing-read "side:" '(left right))
                     ;; don't select
                     (completing-read "buffer:" (cons "[?]" org-xob-buffers))))
  ;; check if dual-pane
  ;; check if proper node tree
  ;; cut context, goto side buf, paste at end
  (org-cut-subtree)
  (save-excursion
    (if-let ((if (eq side 'left)
                 (win (window-left (selected-window)))
               (win (window-right (selected-window)))))
        (select-window win)
      (select-window (split-window nil nil side)))
    (if (string= buf "[?]")
        (org-xob-new-buffer)
      (set-buffer buf))
    (goto-char (point-max))
    (org-paste-subtree 1 nil nil 'remove)))

;;;;; KB Context Commands DONE

;;;###autoload
(defun org-xob-show-backlinks (source &optional arg)
  "Add backlinks contents to the context buffer."
  (interactive)
  (org-xob-with-xob-buffer
   (org-xob-show-source 'backlinks 'org-xob--source-backlinks arg)))

;;;###autoload
(defun org-xob-show-forlinks (&optional arg)
  "Add forlinks contents to the context buffer."
  (interactive)
  (org-xob-with-xob-buffer
   (org-xob-show-source 'forlinks 'org-xob--source-forlinks arg)))

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

;;;;; Context Presentation Commands DONE

;; todo test
;;;###autoload
(defun org-xob-refresh-node-context ()
  "Refresh all displayed sources for node at point."
  (interactive)
  (org-xob-map-node-sources (org-entry-get (point) "EDIT")
                            'org-xob--source-refresh))

;;;###autoload
(defun org-xob-clear-heading ()
  "Clears contents of context entry at point, or for whole context source."
  (interactive)
  (org-xob--context-copy-paste))

;;;###autoload
(defun org-xob-to-summary ()
  "Show KB node summary. This is defined as the first paragraph if it exists."
  (interactive)
  (org-xob--context-copy-paste
   #'(lambda () (progn
                 (org-end-of-meta-data t)
                 (let ((p (org--paragraph-at-point)))
                   (if p
                       (buffer-substring-no-properties
                        (org-element-property :contents-begin p)
                        (org-element-property :contents-end p))))))))

;;;###autoload
(defun org-xob-to-node-tree ()
  "Show only subheadings of KB node."
  (interactive)
  (org-xob--context-copy-paste
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
   #'(lambda () (let ((beg) (end))
                  (org-end-of-meta-data t)
                  (setq beg (point))
                  (outline-next-heading)
                  (setq end (- (point) 1))
                  (buffer-substring beg end)))))

;;;###autoload
(defun org-xob-to-full-node ()
  "Show the full KB node, excepting properties drawer, planning & clocking information."
  (interactive)
  (org-xob--context-copy-paste
   #'(lambda ()
       (let ((org-yank-folded-subtrees nil)
             (org-yank-adjusted-subtrees t))
         (org-copy-subtree)
         (with-temp-buffer
           (org-mode)
           (org-paste-subtree 2)
           (goto-char (point-min))
           (org-mark-subtree)
           (org-end-of-meta-data t)
           (buffer-substring (point) (mark)))))
   #'(lambda (str) (insert str))))

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
;;;;; Buffer Functions

(defun org-xob-buffer-p (buf)
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (and (bound-and-true-p org-xob-mode)
             (if (eq major-mode 'org-mode) t nil)))))

(defun org-xob-edit-buffer-p (buf)
  (and (org-xob-buffer-p buf)
       (with-current-buffer buf
         (eq org-xob--buf 'parent))))

(defun org-xob-new-buffer ()
  "Create new xob buffer. Defaults to dual-pane buffer pair."
  (interactive)
  (let ((numbufs (number-to-string
                  (length org-xob-buffers)))
        buf1 buf2)
    (with-current-buffer
        (setq buf1 (get-buffer-create
                    (concat "xob-E-" numbufs)))
      (org-mode)
      (org-xob-mode 1)
      (setq-local org-xob--buf 'parent)
      (setq-local org-xob--pair-buf buf2)
      (setq-local org-xob--display 'dual))
    (with-current-buffer
        (setq buf2 (get-buffer-create
                    (concat "xob-C-" numbufs)))
      (org-mode)
      (org-xob-mode 1)
      (setq-local org-xob--buf 'child)
      (setq-local org-xob--pair-buf buf1))   
    (setq org-xob-last-buffer buf1)
    (push buf1 org-xob-buffers)
    buf1))

;;;;; Buffer Navigation

;; no change
(defun org-xob--id-create ()
  "Create a UUID formatted ID. org-id will not work with buffers that are
not visiting a file. This function is meant for such a case. Use in conjunction
with org-xob--id-goto to return to this heading.
Returns ID if successful, nil otherwise."
  (let ((ID (uuidgen-4)))
    (if (org-at-heading-p)
        (progn (org-entry-put (point) "ID" ID)
               ID)
      ID)))

;; TODO redo with org-ql
(defun org-xob--id-goto (sID)
  "Search buffers for org heading with ID and place point there.
Return point position if found, nil otherwise. This does not display
the buffer."
  (let (place)
    (when (org-not-nil sID)
      (or (and (string= sID (org-entry-get (point) "ID"))
               (org-back-to-heading)
               (point))
          (and (setq place (org-find-entry-with-id sID))
               (goto-char place))
          (dolist (buf org-xob-buffers)
            (when (setq place (with-current-buffer buf
                                (org-find-entry-with-id sID)))
              (set-buffer buf)
              (goto-char place)
              (return place)))))))

;; TODO redo with org-ql
(defun org-xob--goto-buffer-heading (ID)
  "Go to heading in current buffer with ID. Does not require org-id."
  (let ((m (point)))
    (org-with-wide-buffer
     (goto-char (point-min))
     (if (re-search-forward ID nil t)
         (org-back-to-heading 'invisible-ok)
       (progn
         (goto-char m)
         (message "%s not found." ID))))))

;;;;; Windows

(defun org-xob--single-pane ()
  "Use single pane interface. If dual-pane is open, then kill
the windows."
  (if (window-atom-root win)
      (when-let* ((buf (window-buffer win))
                  (oldwin (selected-window))
                  (winnew (split-window-right)))
        (delete-window oldwin)
        (selected-window winnew)
        (set-buffer buf)))) ;; TODO modify contents for single pane view

;; todo maybe redo macro for dual buffer creation
(defun org-xob--dual-pane (win)
  "Use dual-pane interface"
  (unless (window-atom-root win)
    (when-let* ((buf1 (switch-to-buffer
                       (org-xob-new-buffer)))
                (buf2 (buffer-local-value org-xob--pair-buf
                                          buf1))
                (win1 (selected-window))
                (win2 (split-window-right)))
      (window-make-atom (window-parent win2))
      (set-window-buffer win2 buf2)))) ;; TODO modify contents for dual pane view

;;;;; Edit Node Functions

;; TEST
(defun org-xob--edit-node (ID title)
  "Open node for editing. Selects the last current xob buffer, if none are
found, then create a new one. Defaults to dual-pane display, with C-u opens node
in a single-pane display format."
   ;; check if node already open
  (if (org-xob--id-goto ID)
      (unless (get-buffer-window)
        (set-window-buffer (current-buffer)))
    (org-xob-with-xob-buffer ;; todo replace 
     (goto-char (point-max))
     (insert
      (org-xob--select-content ID
                               #'(lambda () (org-copy-subtree))))
     (org-back-to-heading)
     (org-toggle-tag "edit" 'ON)
     (org-entry-put (point) "EDIT" (org-entry-get "ID"))
     (org-entry-put (point) "ID" (uuidgen-4))
     (push (make-open-node :ID id :sources ())
           'org-xob--open-nodes)
     (outline-hide-entry))))

;; TODO redo with org-ql
(defun org-xob--update-modified-time ()
  "Hook to update the modified timestamp of all nodes that are being edited when saving.
ID should be buffer local in a xob edit buffer."
  (save-window-excursion
    (save-excursion
      (dolist (buf org-xob--edit-buffers)
        (if (buffer-live-p buf)
            (with-current-buffer buf
              (goto-char (point-min))
              (re-search-forward bufID)
              (if (org-entry-get (point) "MODIFIED")
                  (org-entry-put (point) "MODIFIED"
                                 (concat "[" (format-time-string "%F %a %R") "]"))))))
      nil)))

;;;;; TODO propogate edits

;; do I need this?
(defun org-xob-update-copies (ID)
  ;; get state
  ;; redo state
  )

;; skip diffs, just replace whole thing
(defun org-xob-sync-edit ()
  ;; apply diff OR replace original
  ;; record diff
  ;; update-copies
  )

;;;;; Node Functions UNCHANGED

(defun org-xob--is-node-p (&optional ID DEEPCHECK)
  "Check if a heading is a xob node. Called interactively it defaults to heading at point.
If an ID argument is supplied, then check the heading associated with it.
With option DEEPCHECK, do not use any table lookup, but check whether the heading
has valid UUID formatted ID and xob TYPE properties in the property drawer.
Deepcheck only works on heading at point, any ID argument is ignored."
  (interactive)
  (let ((temp (if ID ID (org-id-get nil))))
    (if temp
        (if DEEPCHECK
            (and
             (string= "t" (org-entry-get (point) "xob"))
             (member (org-entry-get (point) "TYPE") org-xob--node-types)
             (eq 0 (org-uuidgen-p temp)))
          (if (gethash temp org-xob--id-title) t nil)))))

(defun org-xob--is-edit-node-p ()
  (let ((id (org-entry-get (point) "EDIT")))
    (and (string= "t" (org-entry-get (point) "xob"))
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
  (let ((temp (if ID ID
                (org-entry-get (point) "ID")))
        (pid (if PID PID
               (org-entry-get (point) "PID"))))
    (if (and temp pid
             (member temp (org-xob--this-node-sources PID)))
        t nil)))

;; TEST
(defun org-xob-show-source (source source-type &optional arg)
  "Show context source for opened node at point. The second argument
source-type is the data structure defining the source. If necessary will
make"
  ;; in an edit node? get id, name
  (if-let ((eid (org-xob--is-edit-node-p))
           (title (truncate-string-to-width
                   (nth 4 (org-heading-components) 25))))
      ;; get src list, get src if in list
        (if-let* ((srcs (org-xob--this-node-sources eid))
                  (src (mapcar '(lambda (x)
                                  (if (equal source (car-safe (cdr-safe x)))
                                      x)) srcs)))
            ;; if src in list then it is prepped, find it
            (let (m)
              (when (eq '(4) arg) ;; if arg then repopulate items
                (funcall (plist-get newsrc :getfn) newsrc))
              (if (org-xob-map-node-sources eid src
                                            (lambda () (setq m (point-marker))))
                  ;; if found, maybe pop its buffer, pulse it
                  (progn
                    (unless (get-buffer-window (marker-buffer m) t)
                      (pop-to-buffer (marker-buffer m)))
                    (save-excursion
                      (goto-char m)
                      (if (pulse-available-p)
                          (pulse-momentary-highlight-one-line (point)))))
                ;; not found, then write src
                (org-xob--source-write source)))
          ;; not in list, make new src, add to srcs
          (let ((newsrc (copy-tree source-type)))
            (plist-put newsrc :ID (setq ID (org-xob--id-create)))
            (plist-put newsrc :PID eid)
            (plist-put newsrc :title title)
            (funcall (plist-get newsrc :getfn) newsrc)
            (push newsrc srcs)))))

(defun org-xob--this-node-sources (id)
  (cl-remove nil
             (mapcar '(lambda (x) (when (string= id (open-node-ID x))
                                    (open-node-sources x))) org-xob--open-nodes)))

;; TODO where? below node, side?
;; TODO replace pid with copy
(defun org-xob--source-write (source)
  "Open a source tree into the context buffer. If it is already there,
then refresh it. source items are shown as org headings.
source is a plist that describes the content source."
  (org-xob-with-context-buffer ;; remove
    (org-with-wide-buffer
     (unless (org-xob--id-goto (plist-get source :ID))
       (goto-char (point-max))
       (org-insert-heading '(4) 'invisible-ok 'TOP)
       (org-edit-headline (plist-get source :title))
       (dolist (el (plist-get source :tags))
         (org-toggle-tag el 'ON))
       (org-entry-put (point) "ID" (plist-get source :ID))
       (org-entry-put (point) "PID" (plist-get source :PID)))
     (org-xob--source-refresh source))))

;; TODO check state type, lookup + call
(defun org-xob--source-refresh (source)
  "Remake source tree. Check if items need to be added or removed."
  (org-xob-with-context-buffer ;; remove
   (if (org-xob--id-goto (plist-get source :ID))
       (let ((temp (copy-tree (plist-get source :items))))
         (org-xob--map-source
          (lambda ()
            ;; todo replace pid with copy
            (let ((pid (org-entry-get (point) "PID")))
              (if (member pid temp)
                  (setq temp (delete pid temp))
                (progn
                  (org-mark-subtree)
                  (call-interactively 'delete-region))))))
         (if temp
             (dolist (el temp)
               (org-xob--source-add-item el)))))))

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
  (save-excursion
    (if ID (org-xob--id-goto ID))
    (org-with-wide-buffer
     (if (and (org-xob--is-source-p)
              (org-goto-first-child))
         (while
             (progn
               (funcall func)
               (outline-get-next-sibling)))
       (message "XOB: map-source, nothing to do here.") nil))))

;;;;; KB Context Functions UNCHANGED

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
                           (message "XOB: invalid link %s" ID) nil)))))))))))

;; TODO test with new sources packaging
(defun org-xob--context-copy-paste (&optional selector insertor)
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
                  (when-let ((pid (org-entry-get (point) "PID")) str)
                    (when (org-uuidgen-p pid)
                     (org-xob--clear-node)
                     (and selector
                          (stringp
                           (setq str (org-xob--select-content pid selector)))
                          (progn
                            (org-end-of-subtree)
                            (newline)
                            (if insertor
                                (funcall insertor str)
                              (insert str)))))))))
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

(defun org-xob--select-content (id selector)
  "Sets point to beginning of kb node with id and uses the function argument selector to mark the content to return.
Returns content as a string with properties."
  (let str
    (save-excursion
      ;; todo replace with copy
      (org-id-goto id)
      (org-with-wide-buffer
       (org-save-outline-visibility
           (org-narrow-to-subtree)
         (setq str (funcall selector))
         (deactivate-mark 'force))))
    str))

;;;;; org-ql predicates test
(org-ql-defpred is-deep-xob-node ()
  "Deepcheck xob nodes."
  :body (and (property "xob" t)
             (member (org-entry-get (point) "TYPE")
                     org-xob--node-types)
             (eq 0 (org-uuidgen-p (or (org-entry-get (point) "ID")
                                      (org-entry-get (point) "COPY")
                                      (org-entry-get (point) "EDIT"))))))

(org-ql-defpred is-xob-node (ID)
  "Is xob node in the system."
  :body (if (gethash ID org-xob--id-title) t nil))

(org-ql-defpred is-xob-copy (ID)
  "need"
  :body (property "COPY" ID))

(org-ql-defpred is-xob-edit (&optional ID)
  "need"
  :body (and (property "EDIT" ID)
             (tags "EDIT")))

(org-ql-defpred is-xob-id (&optional ID)
  "need"
  :body (and (property "xob" "t")
             (member (org-entry-get (point) "TYPE") org-xob--node-types)
             (eq 0 (org-uuidgen-p (property "ID" ID)))))

(org-ql-defpred is-xob-original (&optional ID)
  "need"
  :body (and (property "ID" ID)
             (not (tags "EDIT"))))

(org-ql-defpred is-xob-source (&optional ID)
  "doc"
  :normalizers ((`(set-difference (org-get-tags) ,org-xob-available-sources)))
  :body (and (not (set-difference (org-get-tags) org-xob-available-sources))
             (property "PID" ID)))

;;;;; org-ql mapping functions test

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
  (org-ql-select org-xob-buffers
    `(is-xob-source ,ID)
    :action func))

(defun org-xob-node-source (ID source func)
  (org-ql-select org-xob-buffers
    `(and (is-xob-source ,ID)
          (tags source))
    :action func))

;;;;; Activity UNCHANGED

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
                       (if (re-search-forward org-xob-today-string)
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

;;;;;; Clocking
(defun org-xob--auto-clock-in ())
(defun org-xob--auto-clock-out ())
;;;;; xob Management UNCHANGED?

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

(defun orb-xob-add-info-source (source)
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
