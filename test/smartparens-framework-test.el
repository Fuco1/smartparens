;; Tests which don't fit anywhere else

(ert-deftest sp-test-sp-point-in-symbol-at-the-eob ()
  "When the point is `eobp' it should not be in symbol."
  (sp-test-with-temp-elisp-buffer "foo-bar|"
    (should (not (sp-point-in-symbol)))))

(ert-deftest sp-test-sp-point-in-symbol-inside-symbol ()
  "When the point is inside symbol it should be in symbol."
  (sp-test-with-temp-elisp-buffer "foo-|bar"
    (should (sp-point-in-symbol)))
  (sp-test-with-temp-elisp-buffer "foo-b|ar"
    (should (sp-point-in-symbol))))

;; #634
(ert-deftest sp-test-sp-skip-backward-to-symbol-sexp-at-the-end-of-comment ()
  "When we are skipping backward and land on a sexp delimiter
right at the end of comment, and we started outside a comment, we
should skip the current comment instead of ending on the
delimiter."
  (sp-test-with-temp-elisp-buffer "foo\n;; (bar)\n|baz"
    (sp-skip-backward-to-symbol)
    (insert "|")
    (should (equal (buffer-string) "foo|\n;; (bar)\nbaz")))

  (sp-test-with-temp-elisp-buffer "foo\n;; \"bar\"\n|baz"
    (sp-skip-backward-to-symbol)
    (insert "|")
    (should (equal (buffer-string) "foo|\n;; \"bar\"\nbaz"))))

(ert-deftest sp-test-looking-back ()
  (sp-test-with-temp-elisp-buffer "foo \\|\\ bar"
    (should (sp--looking-back "\\\\+"))
    (should (eq (match-end 0) 6)))

  (sp-test-with-temp-elisp-buffer "OPEN|CLOSE"
    (should (not (sp--looking-back "OPEN\\>"))))

  (sp-test-with-temp-elisp-buffer "OPEN| CLOSE"
    (should (sp--looking-back "OPEN\\>"))
    (should (eq (match-end 0) 5))))

(ert-deftest sp-test-char-is-escaped-p ()
  (sp-test-with-temp-elisp-buffer "\\|t"
    (should (sp-char-is-escaped-p)))

  (sp-test-with-temp-elisp-buffer "\\\\|t"
    (should (not (sp-char-is-escaped-p))))

  (sp-test-with-temp-elisp-buffer "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|t"
    (should (sp-char-is-escaped-p)))

  (sp-test-with-temp-elisp-buffer "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|t"
    (should (not (sp-char-is-escaped-p))))

  (sp-test-with-temp-elisp-buffer "\\\\|"
    (should (not (sp-char-is-escaped-p))))

  (sp-test-with-temp-elisp-buffer "\"foo bar \\\"| baz\""
    (should (sp-char-is-escaped-p (1- (point))))))

(ert-deftest sp-test-sp--strict-regexp-opt-no-strings ()
  "`sp--strict-regexp-opt' on nil input should return unmatchable regexp.

This is not consistent with `regexp-opt' which returns empty string."
  (should (equal "^\\<$" (sp--strict-regexp-opt nil))))

(ert-deftest sp-test-sp--get-stringlike-regexp-with-delimiter ()
  "In case there are string-like delimiters we should return a
regexp that matches them."
  (let ((sp-pairs '((t . ((:open "\"" :close "\"" :actions (insert wrap autoskip navigate)))))))
    (sp-test-with-temp-elisp-buffer "foo |\"bar\""
      (should (sp--looking-at-p (sp--get-stringlike-regexp))))
    (sp-test-with-temp-elisp-buffer "\"bar\"| baz"
      (should (sp--looking-back-p (sp--get-stringlike-regexp))))))

(ert-deftest sp-test-sp--get-stringlike-regexp-with-no-delimiter ()
  "In case there is no string-like delimiter we should return a
regexp that never matches anything."
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate)))))))
    (sp-test-with-temp-elisp-buffer "foo |\"bar\""
      (should-not (sp--looking-at-p (sp--get-stringlike-regexp))))
    (sp-test-with-temp-elisp-buffer "\"bar\"| baz"
      (should-not (sp--looking-back-p (sp--get-stringlike-regexp))))
    (sp-test-with-temp-elisp-buffer "foo |(bar)"
      (should-not (sp--looking-at-p (sp--get-stringlike-regexp))))
    (sp-test-with-temp-elisp-buffer "(bar)| baz"
      (should-not (sp--looking-back-p (sp--get-stringlike-regexp))))))
