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

;; 664
(ert-deftest sp-test-get-suffix-which-is-a-pair-in-text-mode ()
  (let ((sp-pairs '((t (:open "«" :close "»" :actions (navigate))))))
    (sp-test-with-temp-buffer "«asdasd»|«asdasd»"
        (text-mode)
      (should (equal (sp--get-suffix (point) "«") "")))))
