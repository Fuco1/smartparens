(defmacro sp-test--string-valid-p (str &optional init)
  `(sp-test-with-temp-buffer ,str
       ,(if init `,init '(emacs-lisp-mode))
     (and (sp-region-ok-p (point-min) (point-max))
          (sp-region-ok-p (point-max) (point-min)))))

(ert-deftest sp-test-region-ok-unbalanced-paren ()
  (should-not (sp-test--string-valid-p "foo)")))

(ert-deftest sp-test-region-ok-unbalanced-string ()
  (should-not (sp-test--string-valid-p "foo\"")))

(ert-deftest sp-test-region-ok-no-sexp ()
  (should (sp-test--string-valid-p "foo")))

(ert-deftest sp-test-region-ok-balanced-string ()
  (should (sp-test--string-valid-p "\"foo\"")))

(ert-deftest sp-test-region-ok-balanced-string-with-escaped-quote ()
  (should (sp-test--string-valid-p "\"foo \\\" bar\"")))

(ert-deftest sp-test-region-ok-balanced-parens ()
  (should (sp-test--string-valid-p "(foo)")))

(ert-deftest sp-test-region-ok-balanced-parens-with-newline ()
  (should (sp-test--string-valid-p "(foo)\n\n")))

(ert-deftest sp-test-region-ok-flipped-parens ()
  (should-not (sp-test--string-valid-p ")foo(")))

(ert-deftest sp-test-region-ok-c++-mode-no-sexp ()
  (should (sp-test--string-valid-p "foo;" (c++-mode))))

(ert-deftest sp-test-region-ok-c++-mode-balanced-sexp-with-newline ()
  (should (sp-test--string-valid-p "{
    animDivisor = FAWorld::World::getTicksInPeriod(0.1f);
}" (c++-mode))))

(ert-deftest sp-test-region-ok-c++-mode-balanced-sexp-single-line ()
  (should (sp-test--string-valid-p "{ animDivisor = FAWorld::World::getTicksInPeriod(0.1f); }" (c++-mode))))

(ert-deftest sp-test-region-ok-c++-mode-unbalanced-sexp-single-line ()
  (should-not (sp-test--string-valid-p "animDivisor = FAWorld::World::getTicksInPeriod(0.1f); }" (c++-mode))))

(ert-deftest sp-test-region-ok-balanced-parens-with-skip-match ()
  (let ((sp-pairs '((t . ((:open "(" :close ")"
                           :actions (insert wrap autoskip navigate)
                           :skip-match (lambda (ms mb me)
                                         (save-excursion
                                           (goto-char mb)
                                           (sp--looking-back-p "skip" 4)))))))))
    (should (sp-test--string-valid-p "(fo skip) o)"))))

(ert-deftest sp-test-region-ok-with-non-navigable-pairs ()
  (let ((sp-pairs '((t . ((:open "*" :close "*" :actions (wrap)))))))
    (should (sp-test--string-valid-p "*a*"))))

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
