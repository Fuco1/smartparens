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
