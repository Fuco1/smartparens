(defun sp-test--paired-expression-parse-in-elisp (initial result &optional back)
  (sp-test-with-temp-elisp-buffer initial
    (should (equal (sp-get-paired-expression back) result))))

(ert-deftest sp-test-get-paired-expression-elisp ()
  "Test basic paired expressions in `emacs-lisp-mode'."
  (sp-test--paired-expression-parse-in-elisp "|(foo bar)" '(:beg 1 :end 10 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(foo bar|)" '(:beg 1 :end 10 :op "(" :cl ")" :prefix "" :suffix ""))

  (sp-test--paired-expression-parse-in-elisp "|()" '(:beg 1 :end 3 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(|)" '(:beg 1 :end 3 :op "(" :cl ")" :prefix "" :suffix ""))

  (sp-test--paired-expression-parse-in-elisp "|(foo (bar) (baz) ((quux) (quo)) qua)" '(:beg 1 :end 37 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(foo (bar) (baz) ((quux) (quo)) |qua)" '(:beg 1 :end 37 :op "(" :cl ")" :prefix "" :suffix ""))

  (sp-test--paired-expression-parse-in-elisp "|\\{foo bar (baz) quux\\}" '(:beg 1 :end 23 :op "\\{" :cl "\\}" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "|\\{foo \\{bar baz\\} quux fux \\}" '(:beg 1 :end 30 :op "\\{" :cl "\\}" :prefix "" :suffix ""))

  (sp-test--paired-expression-parse-in-elisp "|[vector foo (bar) lolz]" '(:beg 1 :end 24 :op "[" :cl "]" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "[vector foo (bar) lolz|]" '(:beg 1 :end 24 :op "[" :cl "]" :prefix "" :suffix ""))

  (sp-test--paired-expression-parse-in-elisp "|'(foo)" '(:beg 2 :end 7 :op "(" :cl ")" :prefix "'" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "|`(foo)" '(:beg 2 :end 7 :op "(" :cl ")" :prefix "`" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "|,(foo)" '(:beg 2 :end 7 :op "(" :cl ")" :prefix "," :suffix ""))

  (sp-test--paired-expression-parse-in-elisp "|,[vector foo (bar) lolz]" '(:beg 2 :end 25 :op "[" :cl "]" :prefix "," :suffix ""))

  (sp-test--paired-expression-parse-in-elisp "|(foo (bar) (baz) ((quux) (quo)) qua);;asdasdasd" '(:beg 1 :end 37 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "|(foo (bar) (baz) ((quux) (quo)) qua) ;;asdasdasd" '(:beg 1 :end 37 :op "(" :cl ")" :prefix "" :suffix ""))

  ;; ? is used as character escape, but \\ escapes the escape.
  ;; Combinations of \ and ? can create "funny" effects
  (sp-test--paired-expression-parse-in-elisp "|(#\\?)" '(:beg 1 :end 6 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "|(foo bar?)" '(:beg 1 :end 11 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "|(foo bar ?) baz)" '(:beg 1 :end 17 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "|(foo bar \\?)" '(:beg 1 :end 13 :op "(" :cl ")" :prefix "" :suffix ""))
  )

(ert-deftest sp-test-get-paired-expression-elisp-backward ()
  "Test basic paired expressions in `emacs-lisp-mode' in backwards."
  (sp-test--paired-expression-parse-in-elisp "(|foo bar)" '(:beg 1 :end 10 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "(foo bar|)" '(:beg 1 :end 10 :op "(" :cl ")" :prefix "" :suffix "") t)

  (sp-test--paired-expression-parse-in-elisp "()|" '(:beg 1 :end 3 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "(|)" '(:beg 1 :end 3 :op "(" :cl ")" :prefix "" :suffix "") t)

  (sp-test--paired-expression-parse-in-elisp "(foo (bar) (baz) ((quux) (quo)) qua)|" '(:beg 1 :end 37 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "(foo| (bar) (baz) ((quux) (quo)) qua)" '(:beg 1 :end 37 :op "(" :cl ")" :prefix "" :suffix "") t)

  (sp-test--paired-expression-parse-in-elisp "\\{foo bar (baz) quux\\}|" '(:beg 1 :end 23 :op "\\{" :cl "\\}" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "\\{foo \\{bar baz\\} quux fux \\}|" '(:beg 1 :end 30 :op "\\{" :cl "\\}" :prefix "" :suffix "") t)

  (sp-test--paired-expression-parse-in-elisp "[vector foo (bar) lolz]|" '(:beg 1 :end 24 :op "[" :cl "]" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "[|vector foo (bar) lolz]" '(:beg 1 :end 24 :op "[" :cl "]" :prefix "" :suffix "") t)

  (sp-test--paired-expression-parse-in-elisp "'(foo)|" '(:beg 2 :end 7 :op "(" :cl ")" :prefix "'" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "`(foo)|" '(:beg 2 :end 7 :op "(" :cl ")" :prefix "`" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp ",(foo)|" '(:beg 2 :end 7 :op "(" :cl ")" :prefix "," :suffix "") t)

  (sp-test--paired-expression-parse-in-elisp ",[vector foo (bar) lolz]|" '(:beg 2 :end 25 :op "[" :cl "]" :prefix "," :suffix "") t)

  (sp-test--paired-expression-parse-in-elisp "(foo (bar) (baz) ((quux) (quo)) qua)|;;asdasdasd" '(:beg 1 :end 37 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "(foo (bar) (baz) ((quux) (quo)) qua) |;;asdasdasd" '(:beg 1 :end 37 :op "(" :cl ")" :prefix "" :suffix "") t)

  ;; ? is used as character escape, but \\ escapes the escape.
  ;; Combinations of \ and ? can create "funny" effects
  (sp-test--paired-expression-parse-in-elisp "(#\\?)|" '(:beg 1 :end 6 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "(foo bar?)|" '(:beg 1 :end 11 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "(foo bar ?) baz)|" '(:beg 1 :end 17 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "(foo bar \\?)|" '(:beg 1 :end 13 :op "(" :cl ")" :prefix "" :suffix "") t)
  )

(ert-deftest sp-test-get-paired-expression-elisp-fail ()
  "Test that we fail on incomplete pairs."
  (sp-test--paired-expression-parse-in-elisp "|(foo bar" nil)
  (sp-test--paired-expression-parse-in-elisp "(foo |bar" nil)
  (sp-test--paired-expression-parse-in-elisp "(foo bar|" nil)

  (sp-test--paired-expression-parse-in-elisp "|(" nil)
  (sp-test--paired-expression-parse-in-elisp "(|" nil)

  (sp-test--paired-expression-parse-in-elisp "|(()" nil)
  (sp-test--paired-expression-parse-in-elisp "|(foo (bar) (baz) ((quux) (quo)) qua" nil)

  ;; prefixes
  (sp-test--paired-expression-parse-in-elisp "|'(foo" nil)
  (sp-test--paired-expression-parse-in-elisp "|`(foo" nil)
  (sp-test--paired-expression-parse-in-elisp "|,@(foo" nil)
  (sp-test--paired-expression-parse-in-elisp "|'foo)" nil)
  (sp-test--paired-expression-parse-in-elisp "|`foo)" nil)
  (sp-test--paired-expression-parse-in-elisp "|,@foo)" nil)
  )

(ert-deftest sp-test-get-paired-expression-elisp-backward-fail ()
  "Test that we fail on incomplete pairs parsing backwards."
  (sp-test--paired-expression-parse-in-elisp "foo| bar)" nil t)
  (sp-test--paired-expression-parse-in-elisp "foo bar)|" nil t)

  (sp-test--paired-expression-parse-in-elisp ")|" nil t)
  (sp-test--paired-expression-parse-in-elisp "|)" nil t)

  (sp-test--paired-expression-parse-in-elisp "())|" nil t)

  (sp-test--paired-expression-parse-in-elisp "foo| (bar) (baz) ((quux) (quo)) qua)" nil t)
  (sp-test--paired-expression-parse-in-elisp "foo (bar) (baz) ((quux) (quo)) qua)|" nil t)

  ;; prefixes
  (sp-test--paired-expression-parse-in-elisp "'(foo|" nil t)
  (sp-test--paired-expression-parse-in-elisp "`(foo|" nil t)
  (sp-test--paired-expression-parse-in-elisp ",@(foo|" nil t)
  (sp-test--paired-expression-parse-in-elisp "'foo)|" nil t)
  (sp-test--paired-expression-parse-in-elisp "`foo)|" nil t)
  (sp-test--paired-expression-parse-in-elisp ",@foo)|" nil t)
  )

;; Regression tests
(ert-deftest sp-test-get-paired-expression-505 ()
  "Test https://github.com/Fuco1/smartparens/issues/505"
  (sp-test--paired-expression-parse-in-elisp "(foo (|bar \"baz(\"))" '(:beg 6 :end 18 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(foo (|bar \"baz(\" ))" '(:beg 6 :end 19 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(|\"(asd\") (foo)" '(:beg 1 :end 9 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(| \"(asd\") (foo)" '(:beg 1 :end 10 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(|\"(asd\" ) (foo)" '(:beg 1 :end 10 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(\"(asd\"|) (foo)" '(:beg 1 :end 9 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(\"(asd\"| ) (foo)" '(:beg 1 :end 10 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(\"(asd\" |) (foo)" '(:beg 1 :end 10 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(foo) (\"(asd\"|)" '(:beg 7 :end 15 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "(foo) ( \"(asd\"|)" '(:beg 7 :end 16 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "(foo) ( \"(asd\" |)" '(:beg 7 :end 17 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-elisp "(foo) ( \"(asd\" | )" '(:beg 7 :end 18 :op "(" :cl ")" :prefix "" :suffix "") t)

  ;; also check boundary case with a comment
  (sp-test--paired-expression-parse-in-elisp "(foo (|bar ;baz(\n      ))" '(:beg 6 :end 24 :op "(" :cl ")" :prefix "" :suffix ""))
  )

(ert-deftest sp-test-get-paired-expression-556 ()
  "Test https://github.com/Fuco1/smartparens/issues/556"
  (sp-test--paired-expression-parse-in-elisp "(f| ; ()\n  'x)" '(:beg 1 :end 14 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(f ; ()\n  |'x)" '(:beg 1 :end 14 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(f ; ()\n  '|x)" '(:beg 1 :end 14 :op "(" :cl ")" :prefix "" :suffix ""))
  )

(ert-deftest sp-test-get-paired-expression-653 ()
  "If the point is in a multi-line comment, we should be able to
parse a multi-line sexp.

Test https://github.com/Fuco1/smartparens/issues/653"
  ;; nothing in front
  (sp-test--paired-expression-parse-in-elisp "  ;; asd |(as d\n  ;; asd\n  ;; asd) as\n" '(:beg 10 :end 34 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "  ;; asd (as d\n  ;; asd\n  ;; asd)| as\n" '(:beg 10 :end 34 :op "(" :cl ")" :prefix "" :suffix "") t)

  ;; with sexp in front
  (sp-test--paired-expression-parse-in-elisp "(defun true () t)\n  ;; asd |(as d\n  ;; asd\n  ;; asd) as\n" '(:beg 28 :end 52 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(defun true () t)\n  ;; asd (as d\n  ;; asd\n  ;; asd)| as\n" '(:beg 28 :end 52 :op "(" :cl ")" :prefix "" :suffix "") t))
