(ert-deftest sp-test-search-backward-in-context/code/first-match-same ()
  (sp-test-with-temp-elisp-buffer "f|oo bar baz"
    (should (equal (sp--search-forward-in-context "bar" nil t) 8))
    (sp-buffer-equals "foo bar| baz")))

(ert-deftest sp-test-search-backward-in-context/code/first-match-different ()
  (sp-test-with-temp-elisp-buffer "f|oo ; bar baz"
    (should (equal (sp--search-forward-in-context "bar" nil t) nil))
    (sp-buffer-equals "f|oo ; bar baz")))

(ert-deftest sp-test-search-backward-in-context/code/second-match-same ()
  (sp-test-with-temp-elisp-buffer "f|oo ; bar baz
bar baz"
    (should (equal (sp--search-forward-in-context "bar" nil t) 18))
    (sp-buffer-equals "foo ; bar baz
bar| baz")))

(ert-deftest sp-test-search-backward-in-context/comment/first-match-same ()
  (sp-test-with-temp-elisp-buffer ";f|oo bar baz"
    (should (equal (sp--search-forward-in-context "bar" nil t) 9))
    (sp-buffer-equals ";foo bar| baz")))

(ert-deftest sp-test-search-backward-in-context/comment/first-match-different ()
  (sp-test-with-temp-elisp-buffer ";f|oo
bar baz"
    (should (equal (sp--search-forward-in-context "bar" nil t) nil))
    (sp-buffer-equals ";f|oo
bar baz")))

(ert-deftest sp-test-search-backward-in-context/comment/second-match-same ()
  (sp-test-with-temp-elisp-buffer ";f|oo
bar baz
; bar baz"
    (should (equal (sp--search-forward-in-context "bar" nil t) 19))
    (sp-buffer-equals ";foo
bar baz
; bar| baz")))
