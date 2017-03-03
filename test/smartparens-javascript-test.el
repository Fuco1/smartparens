(require 'ert)
(require 'smartparens-javascript)

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
