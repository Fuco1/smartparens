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

(ert-deftest sp-test-rust-character-in-comment ()
  "When inserting ' in a comment, don't bother matching it."
  (sp-test-with-temp-buffer "// |"
      (rust-mode)
    (execute-kbd-macro "'")
    (should (equal (buffer-string) "// '"))))

(ert-deftest sp-test-rust-character-in-string ()
  "When inserting ' in a string, don't insert a matched '."
  (sp-test-with-temp-buffer "let x = \"|\";"
      (rust-mode)
    (execute-kbd-macro "'")
    (should (equal (buffer-string) "let x = \"'\";"))))

(ert-deftest sp-test-rust-kill-first-line ()
  "Ensure we can kill words on the first line.
Regression test."
  (sp-test-with-temp-buffer "extern|"
      (rust-mode)
    (sp-backward-kill-word 1)
    (should (equal (buffer-string) ""))))

(ert-deftest sp-test-rust-pair-angle-bracket ()
  "When typing < we should insert the matching pair
\(when appropriate\)."
  (sp-test-with-temp-buffer "Option|"
      (rust-mode)
    (execute-kbd-macro "<")
    ;; We should have inserted a pair.
    (should (equal (buffer-string) "Option<>"))))

(ert-deftest sp-test-rust-forward-angle-bracket ()
  "< and > are usually brackets in Rust."
  (sp-test-with-temp-buffer "struct Foo {
    baz: Baz|<T>
}"
      (rust-mode)
    (sp-forward-sexp)
    ;; We should have moved over the closing >.
    (should (looking-at "\n"))))

(ert-deftest sp-test-rust-less-than ()
  "When using < to compare, don't insert >."
  (sp-test-with-temp-buffer "if x |"
      (rust-mode)
    (execute-kbd-macro "<")
    (should (equal (buffer-string) "if x <"))))

(ert-deftest sp-test-rust-left-shift ()
  "When using << for a left shift, don't insert >."
  (sp-test-with-temp-buffer "if x <|"
      (rust-mode)
    (execute-kbd-macro "<")
    (should (equal (buffer-string) "if x <<"))))

(ert-deftest sp-test-rust-left-shift-then-function ()
  "We should still be able to insert -> after a left shift."
  (sp-test-with-temp-buffer "const y: u64 = 1 << 2;

fn foo(x: u64) -|

fn bar(x: u64) -> bool {
    true
}
"
      (rust-mode)
    (smartparens-strict-mode)
    (execute-kbd-macro ">")
    (should (equal (buffer-substring (line-beginning-position) (line-end-position))
                   "fn foo(x: u64) ->"))))

(ert-deftest sp-test-rust-delete-comparison ()
  "We should be able to delete comparisons, even in strict mode."
  (sp-test-with-temp-buffer "a < b; b >|"
      (rust-mode)
    (smartparens-strict-mode)
    (execute-kbd-macro (kbd "<backspace>"))
    (should (equal (buffer-string) "a < b; b "))))

(ert-deftest sp-test-rust-format-string ()
  "Don't pair < when used in a format string."
  (sp-test-with-temp-buffer "println!(\"{:0|}\", x);"
      (rust-mode)
    (execute-kbd-macro "<")
    (should (equal (buffer-string) "println!(\"{:0<}\", x);"))))

(ert-deftest sp-test-rust-pair-angle-bracket-in-function-call ()
  "Pair < when parameterizing function calls."
  (sp-test-with-temp-buffer "iterator.collect::|"
      (rust-mode)
    (execute-kbd-macro "<")
    ;; We should have inserted a pair.
    (should (equal (buffer-string) "iterator.collect::<>"))))

(ert-deftest sp-test-rust-pair-autoskip-closing-bracket ()
  "Autoskip a matching >."
  (sp-test-with-temp-buffer "E<T|>"
      (rust-mode)
    (execute-kbd-macro ">")
    ;; We should have skipped the closing bracket.
    (should (equal (buffer-string) "E<T>"))))

(ert-deftest sp-test-rust-pair-insert-and-autoskip-closing-bracket ()
  "Inserting multiples brackets and closing them."
  (sp-test-with-temp-buffer "E|"
      (rust-mode)
    (execute-kbd-macro "<Rc<RefCell<T>>>")
    ;; We should have inserted a pair without an extra chevron.
    (should (equal (buffer-string) "E<Rc<RefCell<T>>>"))))

(ert-deftest sp-test-rust-insert-match-branch ()
  "Inserting a match branch with a rocket (=>) operator."
  (sp-test-with-temp-buffer "match Some(1) { Some(n) =| }"
                            (rust-mode)
                            (execute-kbd-macro "> n")
                            (should (equal (buffer-string) "match Some(1) { Some(n) => n }"))))
