(require 'smartparens-latex)

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

(ert-deftest sp-test-latex-navigate-single-quote-pair-backwards-at-opener ()
  (sp-test-with-temp-buffer "\\foo{bar} `|foo bar'baz'"
      (latex-mode)
    (should (equal (sp-get-thing t)
                   '(:beg 11 :end 24 :op "`" :cl "'" :prefix "" :suffix "")))))

(ert-deftest sp-test-latex-navigate-single-quote-pair-backwards-at-closer ()
  (sp-test-with-temp-buffer "\\foo{bar} `foo bar'baz'|"
      (latex-mode)
    (should (equal (sp-get-thing t)
                   '(:beg 11 :end 24 :op "`" :cl "'" :prefix "" :suffix "")))))

;; SKIP:
;; (ert-deftest sp-test-latex-navigate-single-quote-pair-backwards-at-contraction ()
;;   (sp-test-with-temp-buffer "\\foo{bar} `foo bar'|bar'"
;;       (latex-mode)
;;     (should (equal (sp-get-thing t)
;;                    '(:beg 16 :end 19 :op "" :cl "" :prefix "" :suffix "")))))
