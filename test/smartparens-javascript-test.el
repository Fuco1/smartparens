(require 'ert)
(require 'smartparens-javascript)
(require 'js2-mode)

(defun sp-test--javascript-mode ()
  "Helper"
  (shut-up (js-mode)))

(ert-deftest sp-test-javascript-slurp-whitespace ()
  "Ensure we don't add unwanted whitespace when slurping."
  (sp-test-with-temp-buffer "(|)foo.bar()"
      (sp-test--javascript-mode)
    (sp-forward-slurp-sexp)
    (sp-forward-slurp-sexp)
    (should (equal (buffer-string) "(foo.bar)()"))))

(ert-deftest sp-test-js2-reindent-after-kill ()
  (sp-test-with-temp-buffer "function f () {
  |
  return 42;  // 2 spaces of indentation
}
"
      (shut-up (js2-mode))
    (sp-kill-hybrid-sexp 1)
    (sp-buffer-equals "function f () {
    |return 42;  // 2 spaces of indentation
}
")))

(ert-deftest sp-test-js2-jsx-html-element-as-sexp ()
  (sp-test-with-temp-buffer "function render() {
  return |<a href=\"/\">Top</a>;
}
"
      (shut-up (js2-jsx-mode))
    (sp-forward-sexp)
    (sp-buffer-equals "function render() {
  return <a href=\"/\">Top</a>|;
}
")
    (sp-backward-sexp)
    (sp-buffer-equals "function render() {
  return |<a href=\"/\">Top</a>;
}
")))

(ert-deftest sp-test-javascript-skip-arrow-fn-bracket ()
  "#872"
  (sp-test-with-temp-buffer "|const test = () => {
  console.log('test')
}M"
      (sp-test--javascript-mode)
    (call-interactively 'sp-kill-region)
    (should (equal (buffer-string) ""))))

(ert-deftest sp-test-javascript-skip-arrow-fn-bracket-region-ok ()
  "#872 The region should not be OK because it contains unclosed bracket.

Before, the > character caused it to skip into the `console'
token over the bracket and this made the region appear OK."
  (sp-test-with-temp-buffer "|const test = () => {M
  console.log('test')
}"
      (sp-test--javascript-mode)
    (should (not (sp-region-ok-p (region-beginning) (region-end))))))
