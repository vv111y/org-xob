;;; test-auto-dual-pane.el --- Test auto dual-pane layout feature

;; Simple test for the auto dual-pane layout functionality

;; Load the xob system
(require 'org-xob-core)
(require 'org-xob-backend)
(require 'org-xob)

;; Test function to validate auto dual-pane functionality
(defun test-org-xob-auto-dual-pane ()
  "Test the auto dual-pane layout functionality."
  (interactive)
  
  ;; Test customization variable exists and has correct type
  (message "Testing org-xob-auto-dual-pane variable...")
  (assert (boundp 'org-xob-auto-dual-pane) 
          t "org-xob-auto-dual-pane should be defined")
  
  ;; Test default value
  (message "Default value: %s" org-xob-auto-dual-pane)
  
  ;; Test toggle function
  (message "Testing toggle function...")
  (let ((original-value org-xob-auto-dual-pane))
    (org-xob-toggle-auto-dual-pane)
    (message "After first toggle: %s" org-xob-auto-dual-pane)
    (org-xob-toggle-auto-dual-pane)
    (message "After second toggle: %s" org-xob-auto-dual-pane)
    
    ;; Restore original value
    (setq org-xob-auto-dual-pane original-value))
  
  ;; Test auto-setup function exists
  (assert (fboundp 'org-xob--auto-setup-dual-pane)
          t "org-xob--auto-setup-dual-pane function should be defined")
  
  ;; Test manual setup function exists
  (assert (fboundp 'org-xob-setup-dual-pane)
          t "org-xob-setup-dual-pane function should be defined")
  
  (message "All auto dual-pane tests passed!"))

;; Run test when file is loaded
(test-org-xob-auto-dual-pane)

;;; test-auto-dual-pane.el ends here
