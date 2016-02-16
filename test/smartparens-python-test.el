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

(ert-deftest sp-test-python-backspace-in-strict-mode ()
  (sp-test-with-temp-buffer "def foo()|:"
      (progn (python-mode)
             (smartparens-strict-mode +1))
    (execute-kbd-macro (kbd "DEL"))
    (should (equal (buffer-string) "def foo():"))))

(ert-deftest sp-test-python-apostrophe-in-string ()
  "When inserting ' in a string, don't insert a matched '."
  (sp-test-with-temp-buffer "\" | \""
      (python-mode)
    (execute-kbd-macro "'")
    (should (equal (buffer-string) "\" ' \""))))

(ert-deftest sp-test-python-apostrophe-in-comment ()
  "When inserting ' in a comment, don't insert a matched '."
  (sp-test-with-temp-buffer "# |"
      (python-mode)
    (execute-kbd-macro "'")
    (should (equal (buffer-string) "# '"))))

(ert-deftest sp-test-python-apostrophe-in-code ()
  "When inserting ' in code, insert a matching '."
  (sp-test-with-temp-buffer ""
      (python-mode)
    (execute-kbd-macro "'")
    (should (equal (buffer-string) "''"))))
