;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs lisp

(defvar sp-test-get-paired-expression
  '(("(foo bar)" 1 10 "(" ")" "" "")
    ("()" 1 3 "(" ")" "" "")
    ("(foo (bar) (baz) ((quux) (quo)) qua)" 1 37 "(" ")" "" "")
    ("\\{foo bar (baz) quux\\}" 1 23 "\\{" "\\}" "" "")
    ("\\{foo \\{bar baz\\} quux fux \\}" 1 30 "\\{" "\\}" "" "")
    ("[vector foo (bar) lolz]" 1 24 "[" "]" "" "")
    ))

;; new-style tests


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
