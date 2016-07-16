(require 'ert)
(require 'smartparens)

(defun sp-test--ocaml-mode ()
  (shut-up (tuareg-mode)))

(ert-deftest sp-test-ocaml-forward-slurp-parenthesis-whitespace ()
  "Ensure we don't add unwanted whitespace when slurping."
  (sp-test-with-temp-buffer "(|foo) List.map"
      (sp-test--ocaml-mode)
    (sp-forward-slurp-sexp)
    (should (equal (buffer-string) "(foo List.map)"))))

(ert-deftest sp-test-ocaml-backward-slurp-parenthesis-whitespace ()
  "Ensure we don't add unwanted whitespace when slurping."
  (sp-test-with-temp-buffer "List.map (|foo)"
      (sp-test--ocaml-mode)
      (sp-backward-slurp-sexp)
    (should (equal (buffer-string) "(List.map foo)"))))

(ert-deftest sp-test-ocaml-forward-slurp-square-bracket-whitespace ()
  "Ensure we don't add unwanted whitespace when slurping."
  (sp-test-with-temp-buffer "[|foo] List.map"
      (sp-test--ocaml-mode)
    (sp-forward-slurp-sexp)
    (should (equal (buffer-string) "[foo List.map]"))))

(ert-deftest sp-test-ocaml-backward-slurp-square-bracket-whitespace ()
  "Ensure we don't add unwanted whitespace when slurping."
  (sp-test-with-temp-buffer "List.map [|foo]"
      (sp-test--ocaml-mode)
    (sp-forward-slurp-sexp)
    (should (equal (buffer-string) "[foo List.map]"))))



