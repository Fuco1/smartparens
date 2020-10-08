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

(ert-deftest sp-test-get-suffix-always-return-string ()
  "Previously in case of a pair-specific suffix we could return
nil if there was no match."
  (let ((sp-sexp-suffix '((emacs-lisp-mode regexp "\\(?:aaa\\)"))))
    (sp-test-with-temp-elisp-buffer "((abc)|aaa) asd"
      (let ((sp-pairs '((t (:open "(" :close ")" :actions (insert wrap autoskip navigate)))
                        (emacs-lisp-mode (:open "(" :close ")" :actions (insert wrap autoskip navigate) :suffix "x")))))
        (sp--update-local-pairs)
        (should (equal (sp--get-suffix (point) "(") ""))))))
