(defun sp-test--string-valid-p (str)
  (with-temp-buffer
    (insert str)
    (emacs-lisp-mode)
    (sp-region-ok-p (point-min) (point-max))))

(ert-deftest sp-test-region-ok-unbalanced-paren ()
  (should-not (sp-test--string-valid-p "foo)")))

(ert-deftest sp-test-region-ok-unbalanced-string ()
  (should-not (sp-test--string-valid-p "foo\"")))

(ert-deftest sp-test-region-ok-balanced-string ()
  (should (sp-test--string-valid-p "\"foo\"")))

(ert-deftest sp-test-region-ok-balanced-parens ()
  (should (sp-test--string-valid-p "(foo)")))

(ert-deftest sp-test-region-ok-with-trailing-garbage ()
  (should (sp-test--string-valid-p "(foo) asdf!$#$^")))

(ert-deftest sp-test-region-ok-unbalanced-paren-in-string ()
  (should (sp-test--string-valid-p "(foo \")\")")))

(ert-deftest sp-test-region-ok-balanced-paren-in-latex ()
  (let ((sp-pairs '((t . ((:open "\\(" :close "\\)" :actions (insert wrap autoskip navigate))
                          (:open "("   :close ")"   :actions (insert wrap autoskip navigate)))))))
    (sp-test-with-temp-buffer "foo \\(foo\\) foo"
        (latex-mode)
      (should (sp-region-ok-p (point-min) (point-max))))))

(ert-deftest sp-test-region-ok-unbalanced-paren-in-latex ()
  (let ((sp-pairs '((t . ((:open "\\(" :close "\\)" :actions (insert wrap autoskip navigate))
                          (:open "("   :close ")"   :actions (insert wrap autoskip navigate)))))))
    (sp-test-with-temp-buffer "foo \\(foo foo"
        (latex-mode)
      (should-not (sp-region-ok-p (point-min) (point-max))))))
