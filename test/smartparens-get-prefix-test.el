;; #488, changed for #934
(ert-deftest sp-test-get-prefix-full-length ()
  (sp-test-with-temp-elisp-buffer "#'(fo|o)"
    (should (equal (sp--get-prefix 3 "(") "#'"))))

(ert-deftest sp-test-get-prefix-unquoted-quote ()
  (sp-test-with-temp-elisp-buffer "`'(fo|o)"
    (should (equal (sp--get-prefix 3 "(") "`'"))))

(ert-deftest sp-test-get-prefix-shorter-prefix ()
  (sp-test-with-temp-elisp-buffer "'(fo|o)"
    (should (equal (sp--get-prefix 2 "(") "'"))))

(ert-deftest sp-test-get-prefix-nonsense-prefix ()
  (sp-test-with-temp-elisp-buffer "ad(fo|o)"
    (should (equal (sp--get-prefix 3 "(") ""))))

(ert-deftest sp-test-get-prefix-no-prefix ()
  (sp-test-with-temp-elisp-buffer "(fo|o)"
    (should (equal (sp--get-prefix 1 "(") ""))))

;; 664
(ert-deftest sp-test-get-prefix-which-is-a-pair-in-text-mode ()
  (let ((sp-pairs '((t (:open "«" :close "»" :actions (navigate)))))
        (sp-sexp-prefix '((text-mode regexp "»"))))
    (sp-test-with-temp-buffer "«asdasd»|«asdasd»"
        (text-mode)
      (should (equal (sp--get-prefix (point) "«") "")))))

;; 621
(ert-deftest sp-test-get-thing-with-prefix ()
  (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "\\(?:[_]+\\)"))))
    (sp-test-with-temp-elisp-buffer "(_|(abc))"
      (should (equal (sp-get-thing)
                     '(:beg 3 :end 8 :op "(" :cl ")" :prefix "_" :suffix ""))))
    (sp-test-with-temp-elisp-buffer "(|_(abc))"
      (should (equal (sp-get-thing)
                     '(:beg 3 :end 8 :op "(" :cl ")" :prefix "_" :suffix ""))))))

;; #812
(ert-deftest sp-test-get-prefix-which-is-marked-with-special-prefix-flag ()
  (let ((sp-sexp-prefix nil))
    (sp-test-with-temp-elisp-buffer "?|foo"
     (with-syntax-table (make-syntax-table)
       (modify-syntax-entry ?? "_ p")
       (should (equal (sp--get-prefix) "?"))))))

(prog1 "sp-test-get-prefix-for-skip-backward-to-symbol-respects-pair-prefix"
  (ert-deftest sp-test-get-prefix-for-skip-backward-to-symbol-respects-mode-prefix ()
    (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "\\(?:aaa\\)"))))
      (sp-test-with-temp-elisp-buffer "asd (aaa|(abc))"
        (sp-skip-backward-to-symbol)
        (sp-buffer-equals "asd (|aaa(abc))"))))

  (ert-deftest sp-test-get-prefix-for-skip-backward-to-symbol-respects-pair-prefix ()
    (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "\\(?:aaa\\)"))))
      (sp-test-with-temp-elisp-buffer "asd (aaa|(abc))"
        (let ((sp-pairs '((t (:open "(" :close ")" :actions (insert wrap autoskip navigate)))
                          (emacs-lisp-mode (:open "(" :close ")" :actions (insert wrap autoskip navigate) :prefix "")))))
          (sp--update-local-pairs)
          (sp-skip-backward-to-symbol)
          (sp-buffer-equals "asd (aaa|(abc))"))))))

(ert-deftest sp-test-get-prefix-always-return-string ()
  "Previously in case of a pair-specific prefix we could return
nil if there was no match."
    (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "\\(?:aaa\\)"))))
      (sp-test-with-temp-elisp-buffer "asd (aaa|(abc))"
        (let ((sp-pairs '((t (:open "(" :close ")" :actions (insert wrap autoskip navigate)))
                          (emacs-lisp-mode (:open "(" :close ")" :actions (insert wrap autoskip navigate) :prefix "x")))))
          (sp--update-local-pairs)
          (should (equal (sp--get-prefix (point) "(") ""))))))
