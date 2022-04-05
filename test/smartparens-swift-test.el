(require 'swift-mode)
(require 'smartparens)

(ert-deftest sp-test-swift-kill-first-line ()
  "Ensure we can kill words on the first line.
Regression test."
  (sp-test-with-temp-buffer "extern|"
      (swift-mode)
    (sp-backward-kill-word 1)
    (should (equal (buffer-string) ""))))

(ert-deftest sp-test-swift-pair-angle-bracket ()
  "When typing < we should insert the matching pair
\(when appropriate\)."
  (sp-test-with-temp-buffer "Result|"
      (swift-mode)
    (execute-kbd-macro "<")
    ;; We should have inserted a pair.
    (should (equal (buffer-string) "Result<>"))))

(ert-deftest sp-test-swift-forward-angle-bracket ()
  "< and > are usually brackets in Swift."
  (sp-test-with-temp-buffer "struct Foo {
    baz: Baz|<T>
}"
      (swift-mode)
    (sp-forward-sexp)
    ;; We should have moved over the closing >.
    (should (looking-at "\n"))))

(ert-deftest sp-test-swift-less-than ()
  "When using < to compare, don't insert >."
  (sp-test-with-temp-buffer "if x |"
      (swift-mode)
    (execute-kbd-macro "<")
    (should (equal (buffer-string) "if x <"))))

(ert-deftest sp-test-swift-left-shift ()
  "When using << for a left shift, don't insert >."
  (sp-test-with-temp-buffer "if x <|"
      (swift-mode)
    (execute-kbd-macro "<")
    (should (equal (buffer-string) "if x <<"))))

(ert-deftest sp-test-swift-left-shift-then-function ()
  "We should still be able to insert -> after a left shift."
  (sp-test-with-temp-buffer "let y: UInt64 = 1 << 2;

func foo(x: UInt64) -|

func bar(x: UInt64) -> Bool {
    true
}
"
      (swift-mode)
    (smartparens-strict-mode)
    (execute-kbd-macro ">")
    (should (equal (buffer-substring (line-beginning-position) (line-end-position))
                   "func foo(x: UInt64) ->"))))

(ert-deftest sp-test-swift-delete-comparison ()
  "We should be able to delete comparisons, even in strict mode."
  (sp-test-with-temp-buffer "a < b; b >|"
      (swift-mode)
    (smartparens-strict-mode)
    (execute-kbd-macro (kbd "<backspace>"))
    (should (equal (buffer-string) "a < b; b "))))

(ert-deftest sp-test-swift-pair-angle-bracket-in-constructor-call ()
  "Pair < when parameterizing constructor calls."
  (sp-test-with-temp-buffer "Array|"
      (swift-mode)
    (execute-kbd-macro "<")
    ;; We should have inserted a pair.
    (should (equal (buffer-string) "Array<>"))))

(ert-deftest sp-test-swift-pair-autoskip-closing-bracket ()
  "Autoskip a matching >."
  (sp-test-with-temp-buffer "Array<T|>"
      (swift-mode)
    (execute-kbd-macro ">")
    ;; We should have skipped the closing bracket.
    (should (equal (buffer-string) "Array<T>"))))

(ert-deftest sp-test-swift-pair-insert-and-autoskip-closing-bracket ()
  "Inserting multiple > and closing them."
  (sp-test-with-temp-buffer "Optional|"
      (swift-mode)
    (execute-kbd-macro "<Foo<Bar<T>>>")
    ;; We should have inserted a pair without an extra chevron.
    (should (equal (buffer-string) "Optional<Foo<Bar<T>>>"))))

(ert-deftest sp-test-swift-insert-range-operator ()
  "Inserting a range operator."
  (sp-test-with-temp-buffer "foo..|"
                            (swift-mode)
                            (execute-kbd-macro "<bar")
                            (should (equal (buffer-string) "foo..<bar"))))

;; #793
(ert-deftest sp-test-swift-skip-forward-over-return-type ()
  "Moving forward over a function's return type."
  (sp-test-with-temp-buffer "func foo() |-> UInt32"
      (swift-mode)
    (sp-forward-sexp)
    (execute-kbd-macro "{")
    (sp-buffer-equals "func foo() -> UInt32{|}")))

;; #793
(ert-deftest sp-test-swift-skip-backward-over-return-type ()
  "Moving backward over a function's return type."
  (sp-test-with-temp-buffer "foo() -> |UInt32 {}"
      (swift-mode)
    (smartparens-strict-mode 1)
    (sp-backward-sexp)
    (execute-kbd-macro "func ")
    (sp-buffer-equals "func |foo() -> UInt32 {}")))

;; #793
(ert-deftest sp-test-swift-kill-defun ()
  "Deleting a region containing a Swift function definition."
  (sp-test-with-temp-buffer "|func foo() ->UInt32 {}"
      (swift-mode)
    (mark-whole-buffer)
    (call-interactively 'sp-kill-region)
    (should (equal (buffer-string) ""))))
