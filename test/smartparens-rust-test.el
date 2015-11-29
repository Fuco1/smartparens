(require 'rust-mode)
(require 'smartparens)

(ert-deftest sp-test-rust-parameterized-lifetimes ()
  "When inserting ' in a <>, don't treat it as a character."
  (sp-test-with-temp-buffer "fn foo<|>(x: T) {}"
      (rust-mode)
    (execute-kbd-macro "'a")
    (should (equal (buffer-string) "fn foo<'a>(x: T) {}"))))

(ert-deftest sp-test-rust-ampersand-parameter ()
  "When inserting ' in a &'foo, don't treat it as a character."
  (sp-test-with-temp-buffer "fn foo(x: |T) {}"
      (rust-mode)
    (execute-kbd-macro "&'a ")
    (should (equal (buffer-string) "fn foo(x: &'a T) {}"))))

(ert-deftest sp-test-rust-character-literal ()
  "When inserting ' for a character literal, insert the closing '."
  (sp-test-with-temp-buffer "let x = |"
      (rust-mode)
    (execute-kbd-macro "'a")
    (should (equal (buffer-string) "let x = 'a'"))))

(ert-deftest sp-test-rust-kill-first-line ()
  "Ensure we can kill words on the first line.
Regression test."
  (sp-test-with-temp-buffer "extern|"
      (rust-mode)
    (sp-backward-kill-word 1)
    (should (equal (buffer-string) ""))))
