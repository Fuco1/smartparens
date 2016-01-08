(require 'ert)
(require 'smartparens)

(ert-deftest sp-test-sp-autoescape-string-quote-if-empty ()
  (sp-test-with-temp-buffer "def foo():\n    |"
      (python-mode)
    (let ((sp-autoescape-string-quote-if-empty '(python-mode))
          (sp-autoskip-closing-pair 'always)
          (sp-undo-pairs-separately nil))
      (execute-kbd-macro "\"\"\""))
    (should (equal (buffer-string) "def foo():\n    \"\"\"\"\"\""))))

(ert-deftest sp-test-dont-reindent-python ()
  (sp-test-with-temp-buffer "if foo:
    bar()
baz = biz.boz|"
      (python-mode)
    (sp-backward-kill-word 1)
    (should (equal (buffer-string) "if foo:
    bar()
baz = biz."))))

(ert-deftest sp-test-python-slurp-whitespace ()
  "Ensure we don't add unwanted whitespace when slurping."
  (sp-test-with-temp-buffer "foo(|bar)(baz)"
      (python-mode)
    (sp-forward-slurp-sexp)
    (should (equal (buffer-string) "foo(bar(baz))"))))

(ert-deftest sp-test-python-square-bracket-whitespace ()
  "Ensure we don't add unwanted whitespace when slurping."
  (sp-test-with-temp-buffer "foo[|bar][baz]"
      (python-mode)
    (sp-forward-slurp-sexp)
    (should (equal (buffer-string) "foo[bar[baz]]"))))
