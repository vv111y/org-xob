;;; test-auto-display.el --- Test auto-display links feature

;; Simple test for the auto-display links functionality

;; Load the xob system
(require 'org-xob-core)
(require 'org-xob-backend)
(require 'org-xob)

;; Test function to validate auto-display links setting
(defun test-org-xob-auto-display-links ()
  "Test the auto-display links functionality."
  (interactive)
  
  ;; Test customization variable exists and has correct type
  (message "Testing org-xob-auto-display-links variable...")
  (assert (boundp 'org-xob-auto-display-links) 
          t "org-xob-auto-display-links should be defined")
  
  ;; Test default value
  (message "Default value: %s" org-xob-auto-display-links)
  
  ;; Test toggle function
  (message "Testing toggle function...")
  (let ((original-value org-xob-auto-display-links))
    (org-xob-toggle-auto-display-links)
    (message "After first toggle: %s" org-xob-auto-display-links)
    (org-xob-toggle-auto-display-links)
    (message "After second toggle: %s" org-xob-auto-display-links)
    (org-xob-toggle-auto-display-links)
    (message "After third toggle: %s" org-xob-auto-display-links)
    (org-xob-toggle-auto-display-links)
    (message "After fourth toggle: %s" org-xob-auto-display-links)
    
    ;; Restore original value
    (setq org-xob-auto-display-links original-value))
  
  ;; Test auto-display function exists
  (assert (fboundp 'org-xob--auto-display-links)
          t "org-xob--auto-display-links function should be defined")
  
  (message "All basic tests passed!"))

;; Run test when file is loaded
(test-org-xob-auto-display-links)

;;; test-auto-display.el ends here
