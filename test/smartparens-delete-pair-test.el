(require 'python)

(ert-deftest sp-test-delete-pair ()
  (sp-test-with-temp-elisp-buffer "(|)"
    (smartparens-strict-mode -1)
    (delete-backward-char 1)
    (should (equal (buffer-string) ""))))

(ert-deftest sp-test-delete-pair-closing ()
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-with-temp-elisp-buffer "\\{\\}|"
      (smartparens-strict-mode -1)
      (delete-backward-char 1)
      (should (equal (buffer-string) "\\{")))))

(ert-deftest sp-test-delete-pair-closing ()
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-with-temp-elisp-buffer "\\{|"
      (smartparens-strict-mode -1)
      (delete-backward-char 1)
      (should (equal (buffer-string) "")))))

(ert-deftest sp-test-delete-pair-in-org-mode-according-to-syntax ()
  (sp-test-with-temp-buffer "\"asdasd\"|    \"asdad\""
      (org-mode)
    (smartparens-strict-mode -1)
    (delete-backward-char 1)
    (should (equal (buffer-string) "\"asdasd    \"asdad\""))))

(ert-deftest sp-test-delete-pair-in-org-mode-truly-empty ()
  (let ((sp-pairs '((t (:open "\"" :close "\"" :actions (insert wrap autoskip navigate))
                       (:open "(" :close ")" :actions (insert wrap autoskip navigate)))))
        (sp-navigate-consider-stringlike-sexp '(org-mode)))
    (sp-test-with-temp-buffer "\"asdasd\"  \"|\"   \"asdad\""
        (org-mode)
      (smartparens-strict-mode -1)
      (delete-backward-char 1)
      (should (equal (buffer-string) "\"asdasd\"     \"asdad\"")))))

(ert-deftest sp-test-delete-pair-escaped-double-quote-in-quotes-no-strict-mode-no-spaces ()
  "This is a test for #731."
  (sp-test-with-temp-elisp-buffer "\"\\\"|\""
    (smartparens-strict-mode -1)
    (backward-delete-char 1)
    (sp-buffer-equals "\"|\"")))

(ert-deftest sp-test-delete-pair-escaped-double-quote-in-quotes-no-strict-mode-spaces ()
  "This is a test for #731."
  (sp-test-with-temp-elisp-buffer "\" \\\"| \""
    (smartparens-strict-mode -1)
    (backward-delete-char 1)
    (sp-buffer-equals "\" \\| \"")))

(ert-deftest sp-test-delete-pair-escaped-double-quote-in-quotes-no-strict-mode-no-spaces-python ()
  "This is a test for #731."
  (sp-test-with-temp-buffer "\"\\\"|\""
      (shut-up (python-mode))
    (smartparens-strict-mode -1)
    (python-indent-dedent-line-backspace 1)
    (sp-buffer-equals "\"|\"")))

(ert-deftest sp-test-delete-pair-escaped-double-quote-in-quotes-no-strict-mode-spaces-python ()
  "This is a test for #731."
  (sp-test-with-temp-buffer "\" \\\"| \""
      (shut-up (python-mode))
    (smartparens-strict-mode -1)
    (python-indent-dedent-line-backspace 1)
    (sp-buffer-equals "\" \\| \"")))

(ert-deftest sp-test-delete-pair-escaped-double-quote-in-quotes-strict-mode-no-spaces ()
  "This is a test for #731."
  (sp-test-with-temp-elisp-buffer "\"\\\"|\""
    (smartparens-strict-mode 1)
    (sp-backward-delete-char)
    (sp-buffer-equals "\"|\"")))

(ert-deftest sp-test-delete-pair-escaped-double-quote-in-quotes-strict-mode-spaces ()
  "This is a test for #731."
  (sp-test-with-temp-elisp-buffer "\" \\\"| \""
    (smartparens-strict-mode 1)
    (sp-backward-delete-char)
    (sp-buffer-equals "\" | \"")))
