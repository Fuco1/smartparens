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
    (sp-buffer-equals "foo (bar)(baz)(|)")))

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

;; #820
(ert-deftest sp-test-latex-do-not-fix-closing-delimiter-on-insert-when-inserting-more-than-one-char ()
  "Some electric keys sometimes insert more than one character.
In this case the behaviour is more complicated and we shouldn't
try to fix the buffer.

For example quote in latex inserts two backticks which messes up
with the search logic (if inserted one-by-one they would pair by
thesmeves and would not break unrelated pair)"
  (sp-test-with-temp-buffer "quote: | $f' = 0$"
      (latex-mode)
    (execute-kbd-macro "\"")
    (sp-buffer-equals "quote: ``|'' $f' = 0$")))

;; #990
(ert-deftest sp-test-latex-do-parse-string-quotes-outside-math ()
  (sp-test-with-temp-buffer "foo ``bar''| baz"
      (latex-mode)
    (sp-backward-sexp)
    (sp-buffer-equals "foo |``bar'' baz")))

;; #990
(ert-deftest sp-test-latex-dont-parse-string-quotes-in-math ()
  (sp-test-with-temp-buffer "$foo ``bar''| baz$"
      (latex-mode)
    (sp-backward-sexp)
    (sp-buffer-equals "$foo ``|bar'' baz$")))

;; #990
(ert-deftest sp-test-latex-do-pair-string-quotes-outside-math ()
  (sp-test-with-temp-buffer "foo | baz"
      (latex-mode)
    (execute-kbd-macro "``")
    (sp-buffer-equals "foo ``|'' baz")))

;; #990
(ert-deftest sp-test-latex-dont-pair-string-quotes-in-math ()
  (sp-test-with-temp-buffer "$foo | baz$"
      (latex-mode)
    (execute-kbd-macro "``")
    (sp-buffer-equals "$foo ``| baz$")))

;; #834
(ert-deftest sp-test-latex-wrap-with-trigger-quote ()
  "A region should be wrapped with a pair if trigger key is pressed."
  (sp-test-with-temp-buffer "foo Mbar baz| bam"
      (latex-mode)
    (execute-kbd-macro "\"")
    (sp-buffer-equals "foo ``bar baz''| bam")))

(ert-deftest sp-test-latex-wrap-with-trigger-quote-delsel ()
  "A region should be wrapped with a pair if trigger key is pressed."
  (sp-test-with-delete-selection-mode
   (sp-test-with-temp-buffer "foo Mbar baz| bam"
        (latex-mode)
      (execute-kbd-macro "\"")
      (sp-buffer-equals "foo ``bar baz''| bam"))))

(ert-deftest sp-test-latex-wrap-with-trigger-backslash-paren ()
  "A region should be wrapped with a pair if trigger key is pressed."
  (sp-test-with-temp-buffer "foo |bar bazM bam"
     (latex-mode)
   (execute-kbd-macro "\\(")
   (sp-buffer-equals "foo \\(|bar baz\\) bam")))

(ert-deftest sp-test-latex-wrap-with-trigger-backslash-paren-delsel ()
  "A region should be wrapped with a pair if trigger key is pressed."
  (sp-test-with-delete-selection-mode
   (sp-test-with-temp-buffer "foo |bar bazM bam"
       (latex-mode)
     (execute-kbd-macro "\\(")
     (sp-buffer-equals "foo \\(|bar baz\\) bam"))))

(ert-deftest sp-test-latex-wrap-with-trigger-dollar ()
  "A region should be wrapped with a pair if trigger key is pressed."
  (sp-test-with-temp-buffer "foo |bar bazM bam"
     (latex-mode)
   (execute-kbd-macro "$")
   (sp-buffer-equals "foo $|bar baz$ bam")))

(ert-deftest sp-test-latex-wrap-with-trigger-dollar-delsel ()
  "A region should be wrapped with a pair if trigger key is pressed."
  (sp-test-with-delete-selection-mode
   (sp-test-with-temp-buffer "foo |bar bazM bam"
       (latex-mode)
     (execute-kbd-macro "$")
     (sp-buffer-equals "foo $|bar baz$ bam"))))
