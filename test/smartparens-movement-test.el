(require 'smartparens)
(require 'ert)

(ert-deftest sp-test-next-sexp ()
  "Basic test of `sp-next-sexp'."
  (sp-test-with-temp-elisp-buffer
      "|(foo) (bar)"
    (sp-next-sexp)
    ;; We should be just before (bar).
    (should (looking-at "(bar)"))))

(ert-deftest sp-test-next-sexp-jump-up ()
  "`sp-next-sexp' should jump up a level when
there are no more expressions at the current level."
  (sp-test-with-temp-elisp-buffer
      "(foo|) (bar)"
    (sp-next-sexp)
    ;; We should be just before (foo).
    (should (bobp))))
