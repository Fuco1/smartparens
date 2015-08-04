;; TODO: separate into separate tests for each invocation -> easier
;; debugging
(ert-deftest sp-test-insertion-specification-parser ()
  (should (equal (sp--parse-insertion-spec "ab")
                 '(progn (insert "ab"))))
  (should (equal (sp--parse-insertion-spec "a|b")
                 '(progn
                    (insert "a")
                    (save-excursion
                      (insert "b")))))
  (should (equal (sp--parse-insertion-spec "a\\|b")
                 '(progn (insert "a|") (insert "b"))))
  (should (equal (sp--parse-insertion-spec "a\\||b")
                 '(progn
                    (insert "a|")
                    (save-excursion
                      (insert "b")))))
  (should (equal (sp--parse-insertion-spec "a\\[b]")
                 '(progn (insert "a[") (insert "b]"))))
  (should (equal (sp--parse-insertion-spec "a\\[b[i]")
                 '(progn
                    (insert "a[")
                    (insert "b")
                    (indent-according-to-mode))))
  (should (equal (sp--parse-insertion-spec "a||b")
                 '(progn
                    (insert "a")
                    (save-excursion
                      (insert "b"))
                    (indent-according-to-mode))))
  (should (equal (sp--parse-insertion-spec "a|[i]b")
                 '(progn
                    (insert "a")
                    (save-excursion
                      (indent-according-to-mode)
                      (insert "b")))))
  (should (equal (sp--parse-insertion-spec "a|b[i]")
                 '(progn
                    (insert "a")
                    (save-excursion
                      (insert "b")
                      (indent-according-to-mode)))))
  (should (equal (sp--parse-insertion-spec "[i]a|b")
                 '(progn
                    (indent-according-to-mode)
                    (insert "a")
                    (save-excursion
                      (insert "b")))))
  (should (equal (sp--parse-insertion-spec "[i]")
                 '(progn
                    (indent-according-to-mode))))
  (should (equal (sp--parse-insertion-spec "[d3]")
                 '(progn
                    (delete-char 3))))
  (should (equal (sp--parse-insertion-spec "[d12]")
                 '(progn
                    (delete-char 12)))))
