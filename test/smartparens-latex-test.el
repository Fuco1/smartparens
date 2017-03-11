(ert-deftest sp-test-latex-dont-insert-space-on-forward-slurp-where-not-necessary ()
  (sp-test-with-temp-buffer "foo (|)(bar)(baz)"
      (latex-mode)
    (sp-forward-slurp-sexp 2)
    (sp-buffer-equals "foo (|(bar)(baz))")
    (sp-forward-barf-sexp 2)
    (sp-buffer-equals "foo (|)(bar)(baz)")))

(ert-deftest sp-test-latex-insert-space-on-forward-slurp-where-necessary ()
  (sp-test-with-temp-buffer "foo (|bar)baz"
      (latex-mode)
    (sp-forward-slurp-sexp)
    (sp-buffer-equals "foo (|bar baz)")
    (sp-forward-barf-sexp)
    (sp-buffer-equals "foo (|bar) baz")))

(ert-deftest sp-test-latex-dont-insert-space-on-backward-slurp-where-not-necessary ()
  (sp-test-with-temp-buffer "foo (bar)(baz)(|)"
      (latex-mode)
    (sp-backward-slurp-sexp 2)
    (sp-buffer-equals "foo ((bar)(baz)|)")
    (sp-backward-barf-sexp 2)
    (sp-buffer-equals "foo (bar)(baz)|()")))

(ert-deftest sp-test-latex-insert-space-on-backward-slurp-where-necessary ()
  (sp-test-with-temp-buffer "foo bar(baz|)"
      (latex-mode)
    (sp-backward-slurp-sexp)
    (sp-buffer-equals "foo (bar baz|)")
    (sp-backward-barf-sexp)
    (sp-buffer-equals "foo bar (baz|)")))
