(ert-deftest sp-test-slurp-excludes-semicolon ()
  (sp-test-with-temp-buffer "var foo = bar(|)baz;"
      (js-mode)
    (sp-slurp-hybrid-sexp)
    (should (equal (buffer-string) "var foo = bar(baz);"))))
