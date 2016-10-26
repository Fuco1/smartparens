;; #488
(ert-deftest sp-test-get-prefix-full-length ()
  (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "#?[`',]@?"))))
    (sp-test-with-temp-elisp-buffer "#'(fo|o)"
      (should (equal (sp--get-prefix 3 "(") "#'")))))

(ert-deftest sp-test-get-prefix-shorter-prefix ()
  (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "#?[`',]@?"))))
    (sp-test-with-temp-elisp-buffer "'(fo|o)"
      (should (equal (sp--get-prefix 2 "(") "'")))))

(ert-deftest sp-test-get-prefix-nonsense-prefix ()
  (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "#?[`',]@?"))))
    (sp-test-with-temp-elisp-buffer "ad(fo|o)"
      (should (equal (sp--get-prefix 3 "(") "")))))

(ert-deftest sp-test-get-prefix-no-prefix ()
  (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "#?[`',]@?"))))
    (sp-test-with-temp-elisp-buffer "(fo|o)"
      (should (equal (sp--get-prefix 1 "(") "")))))

;; 621
(ert-deftest sp-test-get-thing-with-prefix ()
  (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "\\(?:[_]+\\)"))))
    (sp-test-with-temp-elisp-buffer "(_|(abc))"
      (should (equal (sp-get-thing)
                     '(:beg 3 :end 8 :op "(" :cl ")" :prefix "_" :suffix ""))))
    (sp-test-with-temp-elisp-buffer "(|_(abc))"
      (should (equal (sp-get-thing)
                     '(:beg 3 :end 8 :op "(" :cl ")" :prefix "_" :suffix ""))))))
