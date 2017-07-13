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
