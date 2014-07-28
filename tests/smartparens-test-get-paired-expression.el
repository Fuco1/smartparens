(require 'smartparens-test-env)

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

(ert-deftest sp-test-get-paired-expression ()
  "Test basic paired expressions in `emacs-lisp-mode'."
  (sp-test-setup-paired-expression-env-basic
    (--each sp-test-get-paired-expression
      (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) nil nil))))

(ert-deftest sp-test-get-paired-expression-backward ()
  (sp-test-setup-paired-expression-env-basic
    (--each sp-test-get-paired-expression
      (sp-test-paired-sexp (car it) (apply 'sp-test-make-pair (cdr it)) t nil))))

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
  (sp-test-setup-paired-expression-env-basic
    (--each sp-test-get-paired-expression-fail
      (sp-test-paired-sexp it nil nil t))))

(ert-deftest sp-test-get-paired-expression-backward-fail ()
  (sp-test-setup-paired-expression-env-basic
    (--each sp-test-get-paired-expression-backward-fail
      (sp-test-paired-sexp it nil t t))))

(defmacro sp-test-setup-paired-expression-env-basic (&rest forms)
  `(sp-test-setup-paired-expression-env
     sp--test-basic-pairs
     emacs-lisp-mode
     emacs-lisp-mode-hook
     ,@forms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs lisp pairs

(defvar sp-test-get-paired-expression-elisp
  '(("'(foo)" 2 7 "(" ")" "'" "")
    ("`(foo)" 2 7 "(" ")" "`" "")
    (",@(foo)" 3 8 "(" ")" ",@" "")
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



(provide 'smartparens-test-get-paired-expression)
