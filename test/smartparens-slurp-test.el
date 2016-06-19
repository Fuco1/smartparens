(require 'smartparens)

(ert-deftest sp-test-js-slurp-excludes-semicolon ()
  (sp-test-with-temp-buffer "var foo = bar(|)baz;"
      (js-mode)
    (sp-slurp-hybrid-sexp)
    (should (equal (buffer-string) "var foo = bar(baz);"))))

(ert-deftest sp-test-js-slurp-curly-paren ()
  (sp-test-with-temp-buffer "while (true) {|}
bar;"
      (js-mode)
    (sp-slurp-hybrid-sexp)
    (should (equal (buffer-string) "while (true) {
    bar;
}"))))

(ert-deftest sp-test-python-slurp-exclude-colon ()
  (sp-test-with-temp-buffer "if bar(|)foo:"
      (shut-up (python-mode))
    (sp-slurp-hybrid-sexp)
    (should (equal (buffer-string) "if bar(foo):"))))

(ert-deftest sp-test-python-slurp-include-dot ()
  (sp-test-with-temp-buffer "(|foo).bar"
      (shut-up (python-mode))
    (sp-slurp-hybrid-sexp)
    (should (equal (buffer-string) "(foo.bar)"))))

(ert-deftest sp-test-c++-slurp-include-semicolon ()
  (sp-test-with-temp-buffer "class foo {|
public:
  int a;
};
int b = 7;"
      (c++-mode)
    (sp-slurp-hybrid-sexp)
    (should (equal (buffer-string) "class foo {
public:
  int a;
  int b = 7;
};"))))

(ert-deftest sp-test-rust-slurp-include-dot ()
  (sp-test-with-temp-buffer "(foo|).bar"
      (rust-mode)
    (sp-slurp-hybrid-sexp)
    (should (equal (buffer-string) "(foo.bar)"))))

(ert-deftest sp-test-js-slurp-from-closing-paren ()
  (sp-test-with-temp-buffer "if (foo) {
    x = 1;
|}
y = baz();
"
      (js-mode)
    (sp-slurp-hybrid-sexp)
    (should (equal (buffer-string) "if (foo) {
    x = 1;
    y = baz();
}
"))))
