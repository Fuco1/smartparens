(require 'smartparens)
(require 'ert)

(ert-deftest sp-test-next-sexp ()
  "Basic test of `sp-next-sexp'."
  (sp-test-with-temp-elisp-buffer
   "|(foo) (bar)"
   (call-interactively 'sp-next-sexp)
   ;; We should be just before (bar).
   (should (looking-at "(bar)")))

  (sp-test-with-temp-elisp-buffer
   "(f|oo bar)"
   (call-interactively 'sp-next-sexp)
   (should (looking-at "foo")))

  (sp-test-with-temp-elisp-buffer
   "(f|oo bar)"
   (set (make-local-variable 'sp-interactive-dwim) t)
   (call-interactively 'sp-next-sexp)
   (should (looking-at "bar")))

  (sp-test-with-temp-elisp-buffer
   "((fo|o) (bar))"
   (set (make-local-variable 'sp-interactive-dwim) t)
   (call-interactively 'sp-next-sexp)
   (should (looking-at "(foo)"))))

(ert-deftest sp-test-next-sexp-jump-up ()
  "`sp-next-sexp' should jump up a level when
there are no more expressions at the current level."
  (sp-test-with-temp-elisp-buffer
      "(foo|) (bar)"
    (call-interactively 'sp-next-sexp)
    ;; We should be just before (foo).
    (should (bobp))))

(ert-deftest sp-test-previous-sexp ()
  "Basic test of `sp-previous-sexp'."
  (sp-test-with-temp-elisp-buffer
      "((foo) bar| (baz quux))"
    (call-interactively 'sp-previous-sexp)
    ;; We should be just before (bar).
    (should (looking-back "(foo)")))

  (sp-test-with-temp-elisp-buffer
   "((foo)| bar (baz quux))"
   (call-interactively 'sp-previous-sexp)
   (should (looking-back "((foo) bar (baz quux))")))

  (sp-test-with-temp-elisp-buffer
   "(foo b|ar baz)"
   (set (make-local-variable 'sp-interactive-dwim) t)
   (call-interactively 'sp-previous-sexp)
   (should (looking-back "foo")))

  (sp-test-with-temp-elisp-buffer
   "(f|oo bar baz)"
   (set (make-local-variable 'sp-interactive-dwim) t)
   (call-interactively 'sp-previous-sexp)
   (should (looking-back "(foo bar baz)"))))
