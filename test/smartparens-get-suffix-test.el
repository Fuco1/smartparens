;; #488
(ert-deftest sp-test-get-suffix-short-suffix ()
  (let ((sp-sexp-suffix '((emacs-lisp-mode regexp "'-?"))))
    (sp-test-with-temp-elisp-buffer "(foo)'"
      (should (equal (sp--get-suffix 6 "(") "'")))))

(ert-deftest sp-test-get-suffix-full-suffix ()
  (let ((sp-sexp-suffix '((emacs-lisp-mode regexp "'-?"))))
    (sp-test-with-temp-elisp-buffer "(foo)'-"
      (should (equal (sp--get-suffix 6 "(") "'-")))))

(ert-deftest sp-test-get-suffix-nonsense-suffix ()
  (let ((sp-sexp-suffix '((emacs-lisp-mode regexp "'-?"))))
    (sp-test-with-temp-elisp-buffer "(foo)-"
      (should (equal (sp--get-suffix 6 "(") "")))))

(ert-deftest sp-test-get-suffix-no-suffix ()
  (let ((sp-sexp-suffix '((emacs-lisp-mode regexp "'-?"))))
    (sp-test-with-temp-elisp-buffer "(foo)"
      (should (equal (sp--get-suffix 6 "(") "")))))
