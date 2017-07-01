(require 'smartparens)
(require 'ert)

(ert-deftest sp-test-next-sexp ()
  (sp-test-with-temp-elisp-buffer "|(foo) (bar)"
    (call-interactively 'sp-next-sexp)
    (sp-buffer-equals "(foo) |(bar)"))

    (sp-test-with-temp-elisp-buffer "(f|oo bar)"
    (call-interactively 'sp-next-sexp)
    (sp-buffer-equals "(|foo bar)"))

  (sp-test-with-temp-elisp-buffer "(f|oo bar)"
    (set (make-local-variable 'sp-navigate-interactive-always-progress-point) t)
    (call-interactively 'sp-next-sexp)
    (sp-buffer-equals "(foo |bar)"))

  (sp-test-with-temp-elisp-buffer "(foo) |(bar)"
    (set (make-local-variable 'sp-navigate-interactive-always-progress-point) t)
    (call-interactively 'sp-next-sexp)
    (sp-buffer-equals "(foo) |(bar)"))

  (sp-test-with-temp-elisp-buffer "((fo|o) (bar))"
    (set (make-local-variable 'sp-navigate-interactive-always-progress-point) t)
    (call-interactively 'sp-next-sexp)
    (sp-buffer-equals "((foo) |(bar))"))

  (sp-test-with-temp-elisp-buffer "(((f|oo))) bar"
    (set (make-local-variable 'sp-navigate-interactive-always-progress-point) t)
    (call-interactively 'sp-next-sexp)
    (sp-buffer-equals "(((foo))) |bar")))

(ert-deftest sp-test-next-sexp-jump-up ()
  "`sp-next-sexp' should jump up a level when
there are no more expressions at the current level."
  (sp-test-with-temp-elisp-buffer "(foo|) (bar)"
    (call-interactively 'sp-next-sexp)
    (sp-buffer-equals "|(foo) (bar)")))

(ert-deftest sp-test-previous-sexp ()
  "Basic test of `sp-previous-sexp'."
  (sp-test-with-temp-elisp-buffer "((foo) bar| (baz quux))"
    (call-interactively 'sp-previous-sexp)
    (sp-buffer-equals "((foo)| bar (baz quux))"))

  (sp-test-with-temp-elisp-buffer "((foo)| bar (baz quux))"
   (call-interactively 'sp-previous-sexp)
   (sp-buffer-equals  "((foo) bar (baz quux))|"))

  (sp-test-with-temp-elisp-buffer "(foo b|ar baz)"
   (set (make-local-variable 'sp-navigate-interactive-always-progress-point) t)
   (call-interactively 'sp-previous-sexp)
   (sp-buffer-equals "(foo| bar baz)"))

  (sp-test-with-temp-elisp-buffer "(f|oo bar baz)"
   (set (make-local-variable 'sp-navigate-interactive-always-progress-point) t)
   (call-interactively 'sp-previous-sexp)
   (sp-buffer-equals "(foo bar baz)|"))

  (sp-test-with-temp-elisp-buffer "(foo (b|ar baz))"
   (set (make-local-variable 'sp-navigate-interactive-always-progress-point) t)
   (call-interactively 'sp-previous-sexp)
   (sp-buffer-equals "(foo| (bar baz))")))
