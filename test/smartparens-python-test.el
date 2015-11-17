(require 'smartparens)

(ert-deftest sp-test-sp-autoescape-string-quote-if-empty ()
  (sp-test-with-temp-buffer "def foo():\n    |"
      (python-mode)
    (let ((sp-autoescape-string-quote-if-empty '(python-mode))
          (sp-autoescape-string-quote t)
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

(ert-deftest sp-test-slurp-exclude-colon ()
  (sp-test-with-temp-buffer "if bar(|)foo:"
      (python-mode)
    (sp-slurp-hybrid-sexp)
    (should (equal (buffer-string) "if bar(foo):"))))

(ert-deftest sp-test-slurp-include-dot ()
  (sp-test-with-temp-buffer "(|foo).bar"
      (python-mode)
    (sp-slurp-hybrid-sexp)
    (should (equal (buffer-string) "(foo.bar)"))))
