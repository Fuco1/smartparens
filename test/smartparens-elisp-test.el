(ert-deftest sp-test-elisp-invalid-hyperlink/point-before-contraction ()
  (sp-test-with-temp-elisp-buffer "; foo|'bar"
    (should (sp-lisp-invalid-hyperlink-p nil 'navigate nil))))

(ert-deftest sp-test-elisp-invalid-hyperlink/point-after-contraction ()
  (sp-test-with-temp-elisp-buffer "; foo'|bar"
    (should (sp-lisp-invalid-hyperlink-p nil 'navigate nil))))

(ert-deftest sp-test-elisp-invalid-hyperlink/point-before-before-punctuation ()
  (sp-test-with-temp-elisp-buffer "; foo|'."
    (should-not (sp-lisp-invalid-hyperlink-p nil 'navigate nil))))

(ert-deftest sp-test-elisp-invalid-hyperlink/point-after-before-punctuation ()
  (sp-test-with-temp-elisp-buffer "; foo'|."
    (should-not (sp-lisp-invalid-hyperlink-p nil 'navigate nil))))

;; #782
(ert-deftest sp-test-elisp-invalid-hyperlink/strict-mode-should-delete-over-contraction-after-hyperlink ()
  "In case there was a hyperlink followed by punctuation and a
contraction after that the parser figured that the hyperlink
actually ends at the contraction because the quote before the
punctuation was considered invalid."
  (sp-test-with-temp-elisp-buffer ";; `a-symbol-name'? I'm|"
    (smartparens-strict-mode 1)
    (sp-backward-delete-char)
    (sp-backward-delete-char)
    (sp-buffer-equals ";; `a-symbol-name'? I|")))

(prog1 "#781"
  (ert-deftest sp-test-slurp-insert-space-for-style--next-sexp-paren-no-space ()
    (sp-test-with-temp-elisp-buffer "(foo|)(bar)"
      (call-interactively 'sp-forward-slurp-sexp)
      (sp-buffer-equals "(foo| (bar))")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(foo|) (bar)")
      (call-interactively 'sp-forward-slurp-sexp)
      (sp-buffer-equals "(foo| (bar))")))

  (ert-deftest sp-test-slurp-insert-space-for-style--next-sexp-paren-space ()
    (sp-test-with-temp-elisp-buffer "(foo|) (bar)"
      (call-interactively 'sp-forward-slurp-sexp)
      (sp-buffer-equals "(foo| (bar))")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(foo|) (bar)")
      (call-interactively 'sp-forward-slurp-sexp)
      (sp-buffer-equals "(foo| (bar))")))

  (ert-deftest sp-test-slurp-insert-space-for-style--next-sexp-symbol-no-space ()
    (sp-test-with-temp-elisp-buffer "(foo|)bar"
      (call-interactively 'sp-forward-slurp-sexp)
      (sp-buffer-equals "(foo| bar)")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(foo|) bar")
      (call-interactively 'sp-forward-slurp-sexp)
      (sp-buffer-equals "(foo| bar)")))

  (ert-deftest sp-test-slurp-insert-space-for-style--next-sexp-symbol-space ()
    (sp-test-with-temp-elisp-buffer "(foo|) bar"
      (call-interactively 'sp-forward-slurp-sexp)
      (sp-buffer-equals "(foo| bar)")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(foo|) bar")
      (call-interactively 'sp-forward-slurp-sexp)
      (sp-buffer-equals "(foo| bar)")))

  (ert-deftest sp-test-slurp-insert-space-for-style--no-extra-space-from-empty-sexp ()
    (sp-test-with-temp-elisp-buffer "(|)foo"
      (call-interactively 'sp-forward-slurp-sexp)
      (sp-buffer-equals "(|foo)")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(|)foo")))

  (ert-deftest sp-test-slurp-insert-space-for-style--next-on-new-line ()
    (sp-test-with-temp-elisp-buffer "(foo|)\n(bar)"
      (call-interactively 'sp-forward-slurp-sexp)
      (sp-buffer-equals "(foo|\n (bar))")
      (call-interactively 'sp-forward-barf-sexp)
      (sp-buffer-equals "(foo|)\n(bar)"))))
