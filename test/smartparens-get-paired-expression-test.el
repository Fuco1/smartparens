;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic pairs

(defvar sp-test-get-paired-expression
  '(("(foo bar)" 1 10 "(" ")" "" "")
    ("()" 1 3 "(" ")" "" "")
    ("(foo (bar) (baz) ((quux) (quo)) qua)" 1 37 "(" ")" "" "")
    ("\\{foo bar (baz) quux\\}" 1 23 "\\{" "\\}" "" "")
    ("\\{foo \\{bar baz\\} quux fux \\}" 1 30 "\\{" "\\}" "" "")
    ("[vector foo (bar) lolz]" 1 24 "[" "]" "" "")
    ))

;; new-style tests
(defun sp-test--paired-expression-parse-in-elisp (initial result &optional back)
  (sp-test-with-temp-elisp-buffer initial
    (should (equal (sp-get-paired-expression back) result))))

(ert-deftest sp-test-get-paired-expression ()
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
  )

(ert-deftest sp-test-get-paired-expression-backward ()
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
  )

(defvar sp-test-get-paired-expression-fail
  '("(foo bar"
    "("
    "(()"
    "(foo (bar) (baz) ((quux) (quo)) qua"
    ))

(defvar sp-test-get-paired-expression-backward-fail
  '("foo bar)"
    ")"
    "())"
    "foo (bar) (baz) ((quux) (quo)) qua)"
    ))

(ert-deftest sp-test-get-paired-expression-fail ()
  "Test that we fail on incomplete pairs."
  (sp-test--paired-expression-parse-in-elisp "|(foo bar" nil)
  (sp-test--paired-expression-parse-in-elisp "(foo |bar" nil)
  (sp-test--paired-expression-parse-in-elisp "(foo bar|" nil)

  (sp-test--paired-expression-parse-in-elisp "|(" nil)
  (sp-test--paired-expression-parse-in-elisp "(|" nil)

  (sp-test--paired-expression-parse-in-elisp "|(()" nil)
  (sp-test--paired-expression-parse-in-elisp "|(foo (bar) (baz) ((quux) (quo)) qua" nil)
  )

(ert-deftest sp-test-get-paired-expression-backward-fail ()
  "Test that we fail on incomplete pairs parsing backwards."
  (sp-test--paired-expression-parse-in-elisp "foo| bar)" nil t)
  (sp-test--paired-expression-parse-in-elisp "foo bar)|" nil t)

  (sp-test--paired-expression-parse-in-elisp ")|" nil t)
  (sp-test--paired-expression-parse-in-elisp "|)" nil t)

  (sp-test--paired-expression-parse-in-elisp "())|" nil t)

  (sp-test--paired-expression-parse-in-elisp "foo| (bar) (baz) ((quux) (quo)) qua)" nil t)
  (sp-test--paired-expression-parse-in-elisp "foo (bar) (baz) ((quux) (quo)) qua)|" nil t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs lisp pairs

(defvar sp-test-get-paired-expression-elisp
  '(("'(foo)" 2 7 "(" ")" "'" "")
    ("`(foo)" 2 7 "(" ")" "`" "")
    (",(foo)" 2 7 "(" ")" "," "")
    (",[vector foo (bar) lolz]" 2 25 "[" "]" "," "")
    ("(foo (bar) (baz) ((quux) (quo)) qua);;asdasdasd" 1 37 "(" ")" "" "")
    ("(foo (bar) (baz) ((quux) (quo)) qua) ;;asdasdasd" 1 37 "(" ")" "" "")
    ("(#\\?)" 1 6 "(" ")" "" "")
    ("(foo bar?)" 1 11 "(" ")" "" "")
    ("(foo bar ?) baz)" 1 17 "(" ")" "" "")
    ("(foo bar \\?)" 1 13 "(" ")" "" "")
    ))

(ert-deftest sp-test-get-paired-expression-elisp ()
  "Test basic paired expressions in `elisp-mode'."
  (sp-test-setup-paired-expression-env-elisp
   (--each sp-test-get-paired-expression
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) nil nil))
   (--each sp-test-get-paired-expression-elisp
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) nil nil))))

(ert-deftest sp-test-get-paired-expression-elisp-backward ()
  (sp-test-setup-paired-expression-env-elisp
   (--each sp-test-get-paired-expression
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) t nil))
   (--each sp-test-get-paired-expression-elisp
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) t nil))))

(defvar sp-test-get-paired-expression-elisp-fail
  '("'(foo"
    "`(foo"
    ",@(foo"
    ))

(defvar sp-test-get-paired-expression-elisp-backward-fail
  '("'foo)"
    "`foo)"
    ",@foo)"
    ))

(ert-deftest sp-test-get-paired-expression-elisp-fail ()
  (sp-test-setup-paired-expression-env-elisp
   (--each sp-test-get-paired-expression-fail
     (sp-test-paired-sexp it nil nil t))
   (--each sp-test-get-paired-expression-elisp-fail
     (sp-test-paired-sexp it nil nil t))))

(ert-deftest sp-test-get-paired-expression-elisp-backward-fail ()
  (sp-test-setup-paired-expression-env-elisp
   (--each sp-test-get-paired-expression-backward-fail
     (sp-test-paired-sexp it nil t t))
   (--each sp-test-get-paired-expression-elisp-backward-fail
     (sp-test-paired-sexp it nil t t))))

(defmacro sp-test-setup-paired-expression-env-elisp (&rest forms)
  `(sp-test-setup-paired-expression-env
     sp--test-basic-pairs
     emacs-lisp-mode
     emacs-lisp-mode-hook
     ,@forms))

(ert-deftest sp-test-paired-expression-parse-in-elisp nil
  ;; #505 when jumping out of strings we can skip a valid delimiter
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

  (sp-test--paired-expression-parse-in-elisp "(foo (|bar ;baz(\n      ))" '(:beg 6 :end 24 :op "(" :cl ")" :prefix "" :suffix ""))

  ;; #556
  (sp-test--paired-expression-parse-in-elisp "(f| ; ()\n  'x)" '(:beg 1 :end 14 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(f ; ()\n  |'x)" '(:beg 1 :end 14 :op "(" :cl ")" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-elisp "(f ; ()\n  '|x)" '(:beg 1 :end 14 :op "(" :cl ")" :prefix "" :suffix ""))
  )

(defun sp-test--paired-expression-parse-in-c (initial result &optional back)
  (sp-test-with-temp-buffer initial
      (c-mode)
    (should (equal (sp-get-paired-expression back) result))))

(ert-deftest sp-test-paired-expression-parse-in-c nil
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))
                          (:open "{" :close "}" :actions (insert wrap autoskip navigate))
                          (:open "[" :close "]" :actions (insert wrap autoskip navigate))
                          (:open "/*" :close "*/" :actions (insert wrap autoskip navigate)))))))
    (sp-test--paired-expression-parse-in-c "asd |/* adasdad */" '(:beg 5 :end 18 :op "/*" :cl "*/" :prefix "" :suffix ""))
    (sp-test--paired-expression-parse-in-c "asd /* adasdad */|" '(:beg 5 :end 18 :op "/*" :cl "*/" :prefix "" :suffix "") t)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby pairs

(defvar sp--test-ruby-pairs
  (sp-test-merge-pairs '((:open "def"   :close "end" :actions (insert wrap autoskip navigate))
                         (:open "if"    :close "end" :actions (insert wrap autoskip navigate))
                         (:open "do"    :close "end" :actions (insert wrap autoskip navigate))
                         (:open "begin" :close "end" :actions (insert wrap autoskip navigate)))))

(defvar sp-test-get-paired-expression-ruby
  '(("begin end" 1 10 "begin" "end" "" "")
    ("def foo bar if blaz end end" 1 28 "def" "end" "" "")
    ("def foo end;" 1 12 "def" "end" "" "")
    ))

(ert-deftest sp-test-get-paired-expression-ruby ()
  "Test basic paired expressions in `ruby-mode'."
  (sp-test-setup-paired-expression-env-ruby
   (--each sp-test-get-paired-expression
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) nil nil))
   (--each sp-test-get-paired-expression-ruby
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) nil nil))))

(ert-deftest sp-test-get-paired-expression-ruby-backward ()
  (sp-test-setup-paired-expression-env-ruby
   (--each sp-test-get-paired-expression
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) t nil))
   (--each sp-test-get-paired-expression-ruby
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) t nil))))

(defvar sp-test-get-paired-expression-ruby-fail
  '("def en"
    "do do end do"
    ))

(defvar sp-test-get-paired-expression-ruby-backward-fail
  '("de end"
    ))

(ert-deftest sp-test-get-paired-expression-ruby-fail ()
  (sp-test-setup-paired-expression-env-ruby
   (--each sp-test-get-paired-expression-fail
     (sp-test-paired-sexp it nil nil t))
   (--each sp-test-get-paired-expression-ruby-fail
     (sp-test-paired-sexp it nil nil t))))

(ert-deftest sp-test-get-paired-expression-ruby-backward-fail ()
  (sp-test-setup-paired-expression-env-ruby
   (--each sp-test-get-paired-expression-backward-fail
     (sp-test-paired-sexp it nil t t))
   (--each sp-test-get-paired-expression-ruby-backward-fail
     (sp-test-paired-sexp it nil t t))))

(defmacro sp-test-setup-paired-expression-env-ruby (&rest forms)
  `(sp-test-setup-paired-expression-env
     sp--test-ruby-pairs
     ruby-mode
     ruby-mode-hook
     ,@forms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; latex pairs

(defvar sp-test-get-paired-expression-latex
  '(("``''" 1 5 "``" "''" "" "")
    ("foo ``bar'' baz" 5 12 "``" "''" "" "")

    ("`'" 1 3 "`" "'" "" "")
    ("foo `bar' baz" 5 10 "`" "'" "" "")
    ))

(ert-deftest sp-test-get-paired-expression-latex ()
  "Test basic paired expressions in `latex-mode'."
  (sp-test-setup-paired-expression-env-latex
   (--each sp-test-get-paired-expression
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) nil nil))
   (--each sp-test-get-paired-expression-latex
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) nil nil))))

(ert-deftest sp-test-get-paired-expression-latex-backward ()
  (sp-test-setup-paired-expression-env-latex
   (--each sp-test-get-paired-expression
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) t nil))
   (--each sp-test-get-paired-expression-latex
     (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) t nil))))

(defmacro sp-test-setup-paired-expression-env-latex (&rest forms)
  `(sp-test-setup-paired-expression-env
     sp--test-latex-pairs
     latex-mode
     latex-mode-hook
     ,@forms))
