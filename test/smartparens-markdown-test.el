(require 'smartparens-markdown)

(ert-deftest sp-test-markdown-gfm-electric-backquote-disabled ()
  "Test that tripple-backquotes are properly paired in `gfm-mode'
with `markdown-gfm-use-electric-backquote' disabled."
  (let ((markdown-gfm-use-electric-backquote nil))
    (sp-test-with-temp-buffer "|"
        (gfm-mode)
      (execute-kbd-macro "```")
      (sp-buffer-equals "```|```"))))

(ert-deftest sp-test-markdown-gfm-electric-backquote-enabled ()
  "Test that tripple-backquotes are properly paired in `gfm-mode'
with `markdown-gfm-use-electric-backquote' enabled."
  (let ((markdown-gfm-use-electric-backquote t))
    (sp-test-with-temp-buffer "|"
        (gfm-mode)
      (execute-kbd-macro "```php")
      (sp-buffer-equals "``` php
|
```"))))

(ert-deftest sp-test-markdown-tripple-backquote ()
  "Test that tripple-backquotes are properly paired in `markdown-mode'."
  (sp-test-with-temp-buffer "|"
      (markdown-mode)
    (execute-kbd-macro "```")
    (sp-buffer-equals "```|```")))

(ert-deftest sp-test-markdown-insert-new-list-item ()
  (sp-test-with-temp-buffer "|"
      (markdown-mode)
    (execute-kbd-macro "* foo bar")
    (sp-buffer-equals "* foo bar|")))

(ert-deftest sp-test-markdown-insert-star-pair ()
  (sp-test-with-temp-buffer "|"
      (markdown-mode)
    (execute-kbd-macro "foo *bar")
    (sp-buffer-equals "foo *bar|*")))

(ert-deftest sp-test-markdown-do-not-insert-star-pair-if-followed-by-spc ()
  (sp-test-with-temp-buffer "|"
      (markdown-mode)
    (execute-kbd-macro "foo * bar")
    (sp-buffer-equals "foo * bar|")))

(ert-deftest sp-test-markdown-do-not-insert-star-pair-if-preceded-by-word ()
  (sp-test-with-temp-buffer "|"
      (markdown-mode)
    (execute-kbd-macro "foo* bar")
    (sp-buffer-equals "foo* bar|")))

(ert-deftest sp-test-markdown-insert-double-star-pair ()
  (sp-test-with-temp-buffer "|"
      (markdown-mode)
    (execute-kbd-macro "foo **bar")
    (sp-buffer-equals "foo **bar|**")))

(ert-deftest sp-test-markdown-unwrap-single-star ()
  (sp-test-with-temp-buffer "foo *ba|r* baz"
      (markdown-mode)
    (sp-unwrap-sexp)
    (sp-buffer-equals "foo ba|r baz")))

(ert-deftest sp-test-markdown-unwrap-double-star ()
  (sp-test-with-temp-buffer "foo **ba|r** baz"
      (markdown-mode)
    (sp-unwrap-sexp)
    (sp-buffer-equals "foo ba|r baz")))

(ert-deftest sp-test-markdown-ignore-list-bullet-when-parsing ()
  (sp-test-with-temp-buffer "* foo bar| * ads"
      (markdown-mode)
    (should-not (sp-get-enclosing-sexp))))

(ert-deftest sp-test-markdown-insert-underscore-pair ()
  (sp-test-with-temp-buffer "|"
      (markdown-mode)
    (execute-kbd-macro "foo _bar")
    (sp-buffer-equals "foo _bar|_")))

(ert-deftest sp-test-markdown-wrap-with-star-pair ()
  (sp-test-with-temp-buffer "foo |barM baz"
      (markdown-mode)
    (execute-kbd-macro "*")
    (sp-buffer-equals "foo *|barM* baz")))

(ert-deftest sp-test-markdown-wrap-with-doublestar-pair ()
  (sp-test-with-temp-buffer "foo |barM baz"
      (markdown-mode)
    (execute-kbd-macro "**")
    (sp-buffer-equals "foo **|barM** baz")))

(ert-deftest sp-test-markdown-wrap-with-underscore-pair ()
  (sp-test-with-temp-buffer "foo |barM baz"
      (markdown-mode)
    (execute-kbd-macro "_")
    (sp-buffer-equals "foo _|barM_ baz")))
