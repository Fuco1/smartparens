(require 'ert)
(require 'dash)

(require 'smartparens)
(require 'smartparens-test-env)
(require 'smartparens-test-get-paired-expression)
(require 'smartparens-test-get-stringlike-expression)
(require 'smartparens-test-ruby-mode)

(defun sp-test-run-tests ()
  (interactive)
  (ert "sp-test-*"))
