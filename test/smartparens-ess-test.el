(require 'smartparens-ess)
(require 'f)


;; ESS load helpers
(defvar sp-ess-lisp-path
  (f-join (f-dirname (locate-library "ess-autoloads")) "lisp"))

(add-to-list 'load-path sp-ess-lisp-path)


(require 'ess-site)


(defun sp-test--ess-mode ()
  (shut-up (R-mode)))

(ert-deftest sp-test-ess-slurp-forward ()
  (sp-test-with-temp-buffer
   "(|) v[1, 2]  ,3"
   (sp-test--ess-mode)
   (sp-forward-slurp-sexp 2)
   (should (equal (buffer-string) "(v[1, 2], 3)"))))

(ert-deftest sp-test-ess-slurp-operators ()
  (sp-test-with-temp-buffer
   "(|) v [1, 2]%in%c (1, 2)"
   (sp-test--ess-mode)
   (sp-forward-slurp-sexp 5)
   (should (equal (buffer-string) "(v[1, 2] %in% c(1, 2))"))))

(ert-deftest sp-test-ess-slurp-backward ()
  (sp-test-with-temp-buffer
   "v [1, 2] ,3 (|)"
   (sp-test--ess-mode)
   (sp-backward-slurp-sexp 3)
   (should (equal (buffer-string) "(v[1, 2], 3)"))))

(ert-deftest sp-test-ess-raise-sexp ()
  (sp-test-with-temp-buffer
   "list(a = v|[,2],)"
   (sp-test--ess-mode)
   (sp-raise-sexp)
   (should (equal (buffer-string) "v[,2]"))))

(ert-deftest sp-test-ess-dont-pair-quote-in-comment ()
  (sp-test-with-temp-buffer "# foo|"
      (sp-test--ess-mode)
    (execute-kbd-macro "'")
    (sp-buffer-equals "# foo'|")))

;; #813
(ert-deftest sp-test-ess-skip-to-pair-should-not-look-for-prefix-at-closing-delimiter ()
  (sp-test-with-temp-buffer "mean(mtcars$mpg|)"
      (sp-test--ess-mode)
    (sp-backward-kill-word 1)
    (sp-buffer-equals "mean(mtcars$|)")))
