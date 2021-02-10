;;; start
(use-package org-xob
  :load-path "../"
  )

(use-package xob-0_9
  :load-path "../"
  :after (org-xob)
  :requires (org-ql org-ql-search org-xob)
  )




(org-ql-select (current-buffer)
  '(todo)
  :action #'(org-no-properties (org-get-heading)))



(ert-deftest org-xob-test-versioing ()
  (should (eq test-text)
          (org-xob-)))
