(require 'ert)
(require 'smartparens)

(defun sp-test--python-mode ()
  (shut-up (python-mode)))

(ert-deftest sp-test-dont-reindent-python ()
  (sp-test-with-temp-buffer "if foo:
    bar()
baz = biz.boz|"
      (sp-test--python-mode)
    (sp-backward-kill-word 1)
    (should (equal (buffer-string) "if foo:
    bar()
baz = biz."))))

(ert-deftest sp-test-python-slurp-whitespace ()
  "Ensure we don't add unwanted whitespace when slurping."
  (sp-test-with-temp-buffer "foo(|bar)(baz)"
      (sp-test--python-mode)
    (sp-forward-slurp-sexp)
    (should (equal (buffer-string) "foo(bar(baz))"))))

(ert-deftest sp-test-python-square-bracket-whitespace ()
  "Ensure we don't add unwanted whitespace when slurping."
  (sp-test-with-temp-buffer "foo[|bar][baz]"
      (sp-test--python-mode)
    (sp-forward-slurp-sexp)
    (should (equal (buffer-string) "foo[bar[baz]]"))))

(ert-deftest sp-test-python-backspace-in-strict-mode ()
  (sp-test-with-temp-buffer "def foo()|:"
      (progn (sp-test--python-mode)
             (smartparens-strict-mode +1))
    (execute-kbd-macro (kbd "DEL"))
    (should (equal (buffer-string) "def foo():"))))

(ert-deftest sp-test-python-apostrophe-in-string ()
  "When inserting ' in a string, don't insert a matched '."
  (sp-test-with-temp-buffer "\" | \""
      (sp-test--python-mode)
    (execute-kbd-macro "'")
    (should (equal (buffer-string) "\" ' \""))))

(ert-deftest sp-test-python-apostrophe-in-comment ()
  "When inserting ' in a comment, don't insert a matched '."
  (sp-test-with-temp-buffer "# |"
      (sp-test--python-mode)
    (execute-kbd-macro "'")
    (should (equal (buffer-string) "# '"))))

(ert-deftest sp-test-python-apostrophe-in-code ()
  "When inserting ' in code, insert a matching '."
  (sp-test-with-temp-buffer ""
      (sp-test--python-mode)
    (execute-kbd-macro "'")
    (should (equal (buffer-string) "''"))))

(ert-deftest sp-test-python-apostrophe-skip-closing-in-code ()
  "When inserting ' at the end of '-delimited string, skip it."
  (sp-test-with-temp-buffer ""
      (sp-test--python-mode)
    (execute-kbd-macro "'foo'")
    (should (equal (buffer-string) "'foo'"))
    (should (eobp))))

(ert-deftest sp-test-python-apostrophe-skip-closing-after-edit-in-code ()
  "When inserting ' at the end of '-delimited string, skip it.

Make sure to skip even in inactive sexps."
  (sp-test-with-temp-buffer ""
      (sp-test--python-mode)
    (execute-kbd-macro "'foo\C-b\C-db'")
    (should (equal (buffer-string) "'fob'"))
    (should (eobp))))

(ert-deftest sp-test-python-apostrophe-delete-on-backspace-in-comment ()
  "When backspacing over ' it should be deleted."
  (sp-test-with-temp-buffer "\"foo; it's| a nice word!\""
      (sp-test--python-mode)
    (execute-kbd-macro (kbd "<backspace><backspace><backspace><backspace>|"))
    (should (equal (buffer-string) "\"foo; | a nice word!\""))))
