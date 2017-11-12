(require 'smartparens)

(defun sp-test-command-setup ()
  (cond
   ((not (boundp 'mode)) (emacs-lisp-mode))
   ((eq mode 'elisp) (emacs-lisp-mode))
   ((eq mode 'racket) (racket-mode))
   ((eq mode 'c) (c-mode))
   ((eq mode 'js) (js2-mode))
   ((eq mode 'python) (shut-up (python-mode))))
  (smartparens-mode 1))

;; TODO: don't use this, simply define the tests manually.  Gives more
;; control and less magic
(defmacro sp-test-command (command examples)
  "Define a series of tests for COMMAND using EXAMPLES.

EXAMPLES is a list of (LETBINDINGS TESTCASES...)

LETBINDINGS takes the same form as bindings to `let', but
additionally the symbol mode can be used to set the major mode.

TESTCASES is a list of strings:

\(initial-state state-after-1-call state-after-2-calls...)

that are used to test the resulting state after running the
command. Each string must contain | to specify where point should
be."
  (declare (indent 1))
  `(ert-deftest ,(intern (concat "sp-test-command-"
                                 (symbol-name command))) ()
     ,@(mapcar
        (lambda (example-group)
          `(let ,(car example-group)
             (sp--test-command ',command ',(cdr example-group))))
        examples)))

(defun sp--test-command (command examples)
  "Run the test for COMMAND."
  ;; TODO: get rid of this
  (unless (and (boundp 'mode)
               (eq mode 'racket))
    (shut-up
      (cl-dolist (example examples)
        (let ((before (car example)))
          (cl-dolist (expected (cdr example))
            (with-temp-buffer
              (sp-test-command-setup)
              (insert before)
              (goto-char (point-min))
              (search-forward "|")
              (delete-char -1)
              (call-interactively command)
              (insert "|")
              (cond
               ((eq expected 'error)
                (should (equal before (buffer-string))))
               ((stringp expected)
                (should (equal expected (buffer-string)))))
              (setq before expected))))))))

(sp-test-command sp-forward-sexp
  ((nil
    ("|foo" "foo|")
    ("|foo bar" "foo| bar" "foo bar|")

    ;; test movement over comments
    ("|(foo)\nbar ;; baz (foo) baz\n(quux)"
     "(foo)|\nbar ;; baz (foo) baz\n(quux)"
     "(foo)\nbar| ;; baz (foo) baz\n(quux)"
     "(foo)\nbar ;; baz (foo) baz\n(quux)|")

    ("(foo)\nbar ;; |baz (foo) baz\n(quux)"
     "(foo)\nbar ;; baz| (foo) baz\n(quux)"
     "(foo)\nbar ;; baz (foo)| baz\n(quux)"
     "(foo)\nbar ;; baz (foo) baz|\n(quux)")

    ;; @{ paredit tests
    ("|" "|")

    ("|()" "()|" "()|")
    ("(|)" "()|" "()|")
    ("()|" "()|")

    ("|( )" "( )|" "( )|")
    ("(| )" "( )|" "( )|")
    ("( |)" "( )|" "( )|")
    ("( )|" "( )|")

    ("|\"\"" "\"\"|" "\"\"|")
    ("\"|\"" "\"\"|" "\"\"|")
    ("\"\"|" "\"\"|")

    ("|\")\"" "\")\"|" "\")\"|")
    ;; ("\"|)\"" "\")|\"" "\")\"|" "\")\"|")
    ("\")|\"" "\")\"|" "\")\"|")
    ("\")\"|" "\")\"|")

    ("|\"()\"" "\"()\"|" "\"()\"|")
    ("\"|()\"" "\"()|\"" "\"()\"|" "\"()\"|")
    ("\"(|)\"" "\"()|\"" "\"()\"|" "\"()\"|")
    ("\"()\"|" "\"()\"|")

    ("|(\"x\" \"y\")" "(\"x\" \"y\")|" "(\"x\" \"y\")|")
    ("(|\"x\" \"y\")" "(\"x\"| \"y\")" "(\"x\" \"y\"|)"
     "(\"x\" \"y\")|" "(\"x\" \"y\")|")
    ("(\"|x\" \"y\")" "(\"x|\" \"y\")" "(\"x\"| \"y\")" "(\"x\" \"y\"|)"
     "(\"x\" \"y\")|" "(\"x\" \"y\")|")
    ("(\"x|\" \"y\")" "(\"x\"| \"y\")" "(\"x\" \"y\"|)"
     "(\"x\" \"y\")|" "(\"x\" \"y\")|")
    ("(\"x\"| \"y\")" "(\"x\" \"y\"|)" "(\"x\" \"y\")|" "(\"x\" \"y\")|")
    ("(\"x\" |\"y\")" "(\"x\" \"y\"|)" "(\"x\" \"y\")|" "(\"x\" \"y\")|")
    ("(\"x\" \"|y\")" "(\"x\" \"y|\")" "(\"x\" \"y\"|)"
     "(\"x\" \"y\")|" "(\"x\" \"y\")|")
    ("(\"x\" \"y|\")" "(\"x\" \"y\"|)" "(\"x\" \"y\")|" "(\"x\" \"y\")|")
    ("(\"x\" \"y\"|)" "(\"x\" \"y\")|" "(\"x\" \"y\")|")
    ("(\"x\" \"y\")|" "(\"x\" \"y\")|")
    ;; @}
    )
   (((current-prefix-arg 2))
    ("|foo bar" "foo bar|")
    ("|(foo bar) baz" "(foo bar) baz|"))))

(sp-test-command sp-backward-sexp
  ((nil
    ("foo|" "|foo")
    ("foo bar|" "foo |bar" "|foo bar")

    ;; test movement over comments
    ("(foo)\nbar ;; baz (foo) baz\n(quux)|"
     "(foo)\nbar ;; baz (foo) baz\n|(quux)"
     "(foo)\n|bar ;; baz (foo) baz\n(quux)"
     "|(foo)\nbar ;; baz (foo) baz\n(quux)")

    ("(foo)\nbar ;; baz (foo) baz|\n(quux)"
     "(foo)\nbar ;; baz (foo) |baz\n(quux)"
     "(foo)\nbar ;; baz |(foo) baz\n(quux)"
     "(foo)\nbar ;; |baz (foo) baz\n(quux)")

    ;; @{paredit tests
    ("|" "|")

    ("|()" "|()")
    ("(|)" "|()" "|()")
    ("()|" "|()" "|()")

    ("|( )" "|( )")
    ("(| )" "|( )" "|( )")
    ("( |)" "|( )" "|( )")
    ("( )|" "|( )" "|( )")

    ("|\"\"" "|\"\"")
    ("\"|\"" "|\"\"" "|\"\"")
    ("\"\"|" "|\"\"" "|\"\"")

    ("|\")\"" "|\")\"")
    ("\"|)\"" "|\")\"" "|\")\"")
    ;; ("\")|\"" "|\")\"" "|\")\"")
    ("\")\"|" "|\")\"" "|\")\"")

    ("|\"()\"" "|\"()\"")
    ("\"|()\"" "|\"()\"" "|\"()\"")
    ("\"(|)\"" "\"|()\"" "|\"()\"" "|\"()\"")
    ("\"()\"|" "|\"()\"" "|\"()\"")

    ("|(\"x\" \"y\")" "|(\"x\" \"y\")")
    ("(|\"x\" \"y\")" "|(\"x\" \"y\")" "|(\"x\" \"y\")")
    ("(\"|x\" \"y\")" "(|\"x\" \"y\")" "|(\"x\" \"y\")" "|(\"x\" \"y\")")
    ("(\"x|\" \"y\")" "(\"|x\" \"y\")" "(|\"x\" \"y\")"
     "|(\"x\" \"y\")" "|(\"x\" \"y\")")
    ("(\"x\"| \"y\")" "(|\"x\" \"y\")" "|(\"x\" \"y\")" "|(\"x\" \"y\")")
    ("(\"x\" |\"y\")" "(|\"x\" \"y\")" "|(\"x\" \"y\")" "|(\"x\" \"y\")")
    ("(\"x\" \"|y\")" "(\"x\" |\"y\")" "(|\"x\" \"y\")"
     "|(\"x\" \"y\")" "|(\"x\" \"y\")")
    ("(\"x\" \"y|\")" "(\"x\" \"|y\")" "(\"x\" |\"y\")" "(|\"x\" \"y\")"
     "|(\"x\" \"y\")" "|(\"x\" \"y\")")
    ("(\"x\" \"y\"|)" "(\"x\" |\"y\")" "(|\"x\" \"y\")"
     "|(\"x\" \"y\")" "|(\"x\" \"y\")")
    ("(\"x\" \"y\")|" "|(\"x\" \"y\")" "|(\"x\" \"y\")")
    ;; @}
    )))

(sp-test-command sp-forward-parallel-sexp
  ((nil
    ("|foo" "foo|")
    ("foo|" "foo|")
    ("|foo bar" "foo| bar" "foo bar|")
    ("(|foo bar)" "(foo| bar)" "(foo bar|)" "(foo| bar)")
    ("(foo| bar baz)" "(foo bar| baz)" "(foo bar baz|)" "(foo| bar baz)" )
    ("(|foo)" "(foo|)" "(foo|)")
    )
   (((current-prefix-arg 2))
    ("|foo bar" "foo bar|")
    ("|(foo bar) baz" "(foo bar) baz|")
    ("(foo| bar) baz" "(foo| bar) baz")
    )
   ))

(sp-test-command sp-backward-parallel-sexp
  ((nil
    ("foo|" "|foo")
    ("|foo" "|foo")
    ("foo bar|" "foo |bar" "|foo bar")
    ("(foo bar|)" "(foo |bar)" "(|foo bar)" "(foo |bar)")
    ("(foo| bar baz)" "(|foo bar baz)" "(foo bar |baz)" "(foo |bar baz)" )
    ("(foo|)" "(|foo)" "(|foo)")
    )
   (((current-prefix-arg 2))
    ("foo bar|" "|foo bar")
    ("(foo bar) baz|" "|(foo bar) baz")
    ("(foo |bar) baz" "(foo |bar) baz")
    )
   ))

(sp-test-command sp-forward-slurp-sexp
  ((nil
    ;; beware, this also tests the reindenting/cleanup!!!
    ("(f|oo)\nbar ;; baz (foo) baz\n(quux)"
     "(f|oo\n bar) ;; baz (foo) baz\n(quux)"
     "(f|oo\n bar ;; baz (foo) baz\n (quux))")

    ("(foo)\nbar ;; baz (f|oo) baz\n(quux)"
     "(foo)\nbar ;; baz (f|oo baz)\n(quux)")

    ;; do not slurp outside of comment
    ("(foo)\nbar ;; (|foo) baz\n(asd)\n\n"
     "(foo)\nbar ;; (|foo baz)\n(asd)\n\n"
     "(foo)\nbar ;; (|foo baz)\n(asd)\n\n"))
   (((current-prefix-arg '(4)))
    ("[(fo|o) bar baz]" "[(fo|o bar baz)]")
    ("((progn| bar (baz) (baz)))" "((progn| bar (baz) (baz)))"))

   (((mode 'c))
    ("int funct() { int |foo =} bar;" "int funct() { int |foo = bar;}")
    ("int funct() { int |foo =}; bar" "int funct() { int |foo = bar};")
    ("int funct() { int |foo =}; bar;" "int funct() { int |foo = bar;};"))
   (((mode 'racket)
     (sp-sexp-prefix '((racket-mode regexp "#?['`,]@?"))))
    ("(f|oo)  #'(bar)" "(f|oo  #'(bar))"))
   (((sp-sexp-prefix '((emacs-lisp-mode regexp "\\(?:['`]*,@?\\|[',`]\\)"))))
    ("(fo|o) `',(bar)" "(fo|o `',(bar))")
    ("(fo|o) ,@(bar)" "(fo|o ,@(bar))"))))

(sp-test-command sp-backward-slurp-sexp
  ((nil
    ;; beware, this also tests the reindenting/cleanup!!!
    ("(foo)\nbar ;; baz (foo) baz\n(qu|ux)"
     "(foo)\n(bar ;; baz (foo) baz\n qu|ux)"
     "((foo)\n bar ;; baz (foo) baz\n qu|ux)")

    ("(foo)\nbar ;; baz (f|oo) baz\n(quux)"
     "(foo)\nbar ;; (baz f|oo) baz\n(quux)")

    ;; do not slurp outside of comment
    ("(foo)\nbar ;; baz (foo|)\n(asd)\n\n"
     "(foo)\nbar ;; (baz foo|)\n(asd)\n\n"
     "(foo)\nbar ;; (baz foo|)\n(asd)\n\n"))

   (((sp-sexp-prefix '((emacs-lisp-mode regexp "\\(?:['`]*,@?\\|[',`]\\)"))))
    ("(foo `',(b|ar))" "(`',(foo b|ar))")
    ("(foo ,@(b|ar))" "(,@(foo b|ar))"))))

(sp-test-command sp-forward-barf-sexp
  ((nil
    ;; beware, this also tests the reindenting/cleanup!!!
    ("(f|oo\n bar ;; baz (foo) baz\n (quux))"
     "(f|oo\n bar) ;; baz (foo) baz\n(quux)"
     "(f|oo)\nbar ;; baz (foo) baz\n(quux)")

    ("(foo)\nbar ;; baz (f|oo baz)\n(quux)"
     "(foo)\nbar ;; baz (f|oo) baz\n(quux)")

    ;; #634
    ("(let ((a 4)\n      ;; (fail)\n      |(+ 1)\n      ))\n"
     "(let ((a 4))\n  ;; (fail)\n|  (+ 1)\n  )\n"
     "(let ((a 4)))\n;; (fail)\n|(+ 1)\n\n"))

   (((mode 'racket)
     (sp-sexp-prefix '((racket-mode regexp "#?['`,]@?"))))
    ("(f|oo  #'(bar))" "(f|oo)  #'(bar)"))))

(sp-test-command sp-backward-barf-sexp
  ((nil
    ;; beware, this also tests the reindenting/cleanup!!!
    ("((foo)\n bar ;; baz (foo) baz\n qu|ux)"
     "(foo)\n(bar ;; baz (foo) baz\n qu|ux)"
     "(foo)\nbar ;; baz (foo) baz\n(qu|ux)")

    ("(foo)\nbar ;; (baz f|oo) baz\n(quux)"
     "(foo)\nbar ;; baz (f|oo) baz\n(quux)"))))

(sp-test-command sp-splice-sexp
  ((nil
    ("|(f (g\n    h))" error)
    ("(|f (g\n    h))" "|f (g\n   h)")
    ("(f| (g\n    h))" "f| (g\n   h)")
    ("(f |(g\n    h))" "f |(g\n   h)")
    ("(f (|g\n    h))" "(f |g\n   h)")
    ("(f (g|\n    h))" "(f g|\n   h)")
    ("(f (g\n|    h))" "(f g\n|   h)")
    ("(f (g\n |   h))" "(f g\n |  h)")
    ("(f (g\n  |  h))" "(f g\n  | h)")
    ("(f (g\n   | h))" "(f g\n   |h)")
    ("(f (g\n    |h))" "(f g\n   |h)")
    ("(f (g\n    h|))" "(f g\n   h|)")
    ("(f (g\n    h)|)" "f (g\n   h)|")
    ("(f (g\n    h))|" error)

    ;; Omit whitespace if appropriate.
    ("|(f (\n    h))" error)
    ("(|f (\n    h))" "|f (\n   h)")
    ("(f| (\n    h))" "f| (\n   h)")
    ("(f |(\n    h))" "f |(\n   h)")
    ("(f (|\n    h))" "(f |\n h)")
    ("(f (\n|    h))" "(f \n| h)")
    ("(f (\n |   h))" "(f \n |h)")
    ("(f (\n  |  h))" "(f \n |h)")
    ("(f (\n   | h))" "(f \n |h)")
    ("(f (\n    |h))" "(f \n |h)")
    ("(f (\n    h|))" "(f \n h|)")
    ("(f (\n    h)|)" "f (\n   h)|")
    ("(f (\n    h))|" error)

    ;; But leave comments intact.
    ("(f (|   ;xy\n    h))" "(f |   ;xy\n h)")
    ("(f ( |  ;xy\n    h))" "(f  |  ;xy\n h)")
    ("(f (  | ;xy\n    h))" "(f   | ;xy\n h)")
    ("(f (   |;xy\n    h))" "(f    |;xy\n h)")
    ("(f (   ;|xy\n    h))" "(f    ;|xy\n h)")
    ("(f (   ;x|y\n    h))" "(f    ;x|y\n h)")
    ("(f (   ;xy|\n    h))" "(f    ;xy|\n h)")
    ("(f (   ;xy\n|    h))" "(f    ;xy\n| h)")
    ("(f (   ;xy\n |   h))" "(f    ;xy\n |h)")
    ("(f (   ;xy\n  |  h))" "(f    ;xy\n |h)")
    ("(f (   ;xy\n   | h))" "(f    ;xy\n |h)")
    ("(f (   ;xy\n    |h))" "(f    ;xy\n |h)")
    ("(f (   ;xy\n    h|))" "(f    ;xy\n h|)")

    ;; Don't touch indentation outside a limited scope.
    ("(foo (|bar)\n          baz)" "(foo |bar\n          baz)")
    ("(foo (b|ar)\n          baz)" "(foo b|ar\n          baz)")
    ("(foo (ba|r)\n          baz)" "(foo ba|r\n          baz)")
    ("(foo (bar|)\n          baz)" "(foo bar|\n          baz)")
    ("  (foo\n  (|bar baz))" "  (foo\n   |bar baz)")
    ("  (foo\n  (b|ar baz))" "  (foo\n   b|ar baz)")
    ("  (foo\n  (ba|r baz))" "  (foo\n   ba|r baz)")
    ("  (foo\n  (bar| baz))" "  (foo\n   bar| baz)")
    ("  (foo\n  (bar |baz))" "  (foo\n   bar |baz)")
    ("  (foo\n  (bar b|az))" "  (foo\n   bar b|az)")
    ("  (foo\n  (bar ba|z))" "  (foo\n   bar ba|z)")
    ("  (foo\n  (bar baz|))" "  (foo\n   bar baz|)")
    ("  (foo (|(bar\n         baz)\n        quux)\n zot)"
     "(foo |(bar\n      baz)\n     quux\n zot)")

    ("  (foo ((bar\n         b|az)\n        quux)\n zot)"
     "(foo (bar\n      b|az\n        quux)\n zot)"))))

(sp-test-command sp-splice-sexp-killing-backward
  ((nil
    ("(\n    ;; foo bar\n asd\n as\n ;; asds (asdasd asd hgujirjf) asd\n a|sd\n )" "|asd")

    ;; if we are in front of a comment which is on a separate line,
    ;; keep it as it most likely pertains to the line after
    ("(\n    ;; foo bar\n asd\n as\n |;; asds (asdasd asd hgujirjf) asd\n asd\n )" "|;; asds (asdasd asd hgujirjf) asd\nasd")
    ("(\n    ;; foo bar\n asd\n as\n ;; as|ds (asdasd asd hgujirjf) asd\n asd\n )" "|;; asds (asdasd asd hgujirjf) asd\nasd")

    ;; valid sexps inside comments should be treated as such
    ("(\n    ;; foo bar\n asd\n as\n ;; asds (asdasd asd |hgujirjf) asd\n asd\n )" "(\n    ;; foo bar\n asd\n as\n ;; asds |hgujirjf asd\n asd\n )")

    ;; if first "form" is a comment, keep it
    ("(\n  |  ;; foo bar\n asd\n as\n ;; asds (asdasd asd hgujirjf) asd\n asd\n )" "|;; foo bar\nasd\nas\n;; asds (asdasd asd hgujirjf) asd\nasd")

    ;; keep comment before the form from which we are splicing if it is on a separate line
    ("(\n    ;; foo bar\n |asd\n as\n ;; asds (asdasd asd hgujirjf) asd\n asd\n )" "|;; foo bar\nasd\nas\n;; asds (asdasd asd hgujirjf) asd\nasd")
    ("(\n    ;; foo bar\n asd\n as\n ;; asds (asdasd asd hgujirjf) asd\n |asd\n )" "|;; asds (asdasd asd hgujirjf) asd\nasd")

    ;; when at the end of the expression, kill entire expression
    ("(asdad asd (some-func) asdasd|)" "|")
    ("(asdad asd (some-func) asdasd|  )" "|")
    ("(asdad asd (some-func) asdasd  |)" "|")
    ("(asdad asd (some-func) asdasd  |  )" "|")
    ("foo (asdad asd (some-func) asdasd|)" "foo |")
    ("foo (asdad asd (some-func) asdasd|) bar" "foo | bar")

    ;; when there is a comment before the parent sexp and we are at
    ;; the beginning, the comment shouldn't play any role
    (";; foo bar\n\n(|foo bar baz)" ";; foo bar\n\n|foo bar baz")
    )))

(sp-test-command sp-splice-sexp-killing-around
  ((nil
    ("(\n    ;; foo bar\n asd\n as\n ;; asds (asdasd asd hgujirjf) asd\n a|sd\n )" "|asd")

    ;; if we are in front of a comment which is on a separate line,
    ;; keep it as it most likely pertains to the line after
    ("(\n    ;; foo bar\n asd\n as\n |;; asds (asdasd asd hgujirjf) asd\n asd\n )" "|;; asds (asdasd asd hgujirjf) asd\nasd")
    ("(\n    ;; foo bar\n asd\n as\n ;; as|ds (asdasd asd hgujirjf) asd\n asd\n )" "|;; asds (asdasd asd hgujirjf) asd\nasd")

    ;; valid sexps inside comments should be treated as such
    ("(\n    ;; foo bar\n asd\n as\n ;; asds (asdasd asd |hgujirjf) asd\n asd\n )" "(\n    ;; foo bar\n asd\n as\n ;; asds |hgujirjf asd\n asd\n )")

    ;; if first "form" is a comment, keep it
    ("(\n  |  ;; foo bar\n asd\n as\n ;; asds (asdasd asd hgujirjf) asd\n asd\n )" "|;; foo bar\nasd")

    ;; keep comment before the form from which we are splicing if it is on a separate line
    ("(\n    ;; foo bar\n |asd\n as\n ;; asds (asdasd asd hgujirjf) asd\n asd\n )" "|;; foo bar\nasd")
    ("(\n    ;; foo bar\n asd\n as\n ;; asds (asdasd asd hgujirjf) asd\n |asd\n )" "|;; asds (asdasd asd hgujirjf) asd\nasd")

    ;; from #243
    (";; \"quote\" here\nand \"|here\"" ";; \"quote\" here\nand |here")
    (";; \"|quote\" here\nand here" ";; |quote here\nand here")

    ;; from #580
    ("(foo \"|bar\")" "(foo |bar)")

    ;; from #569
    ("(\" \" f|oo)" "|foo")
    ("(\" \" |foo)" "|foo")
    ("(\"x\" |foo)" "|foo")
    )))

(sp-test-command sp-split-sexp
  ((nil
    ("(foo |bar baz)" "(foo) |(bar baz)")
    ("(foo bar|)" "(foo bar)|()")

    ("\"foo |bar baz\"" "\"foo \"|\"bar baz\"")
    ("\"foo bar|\"" "\"foo bar\"|\"\"")

    ("\"(foo |bar) baz\"" "\"(foo \"|\"bar) baz\"")
    )

   (((sp-split-sexp-always-split-as-string nil))
    ("\"foo |bar baz\"" "\"foo\" |\"bar baz\"")
    ("\"foo bar|\"" "\"foo bar\"|\"\"")

    ("\"(foo |bar) baz\"" "\"(foo) |(bar) baz\"")
    )

   (((current-prefix-arg '(4)))
    ("(foo |bar baz)" "(foo) |(bar) (baz)")
    ("\"foo |bar baz\"" "\"foo\" |\"bar\" \"baz\""))))

(sp-test-command sp-extract-before-sexp
  ((nil
    ("(foo\n |bar\n baz\n )" "|bar\n(foo\n baz\n )")
    ("(foo\n |bar\n baz)" "|bar\n(foo\n baz)")
    ("(foo\n bar\n |baz\n )" "|baz\n(foo\n bar\n )")
    ("(foo\n bar\n |baz)" "|baz\n(foo\n bar)"))))

(sp-test-command sp-extract-after-sexp
  ((nil
    ("(foo\n |bar\n baz\n )" "(foo\n baz\n )\nbar|")
    ("(foo\n |bar\n baz)" "(foo\n baz)\nbar|")
    ("(foo\n bar\n |baz\n )" "(foo\n bar\n )\nbaz|")
    ("(foo\n bar\n |baz)" "(foo\n bar)\nbaz|"))))

(sp-test-command sp-join-sexp
  ((nil
    ("(foo bar) |(baz)" "(foo bar |baz)")

    ("[foo bar] |[baz]" "[foo bar |baz]")

    ("(foo bar) |[baz]" error))

   (((current-prefix-arg 2))
    ("(foo) |(bar) (baz)" "(foo |bar baz)")

    ("[foo] |[bar] [baz]" "[foo |bar baz]")

    ("(foo) |[bar] [baz]" error))

   (((current-prefix-arg '(4)))
    ("(foo bar (baz)| (quux) (blob bluq))" "(foo bar (baz| quux blob bluq))"))))

(sp-test-command sp-kill-word
  ((nil
    ("|  'foo-bar-baz" "|-bar-baz")
    ("|'foo-bar-baz" "|-bar-baz")
    ("'|foo-bar-baz" "'|-bar-baz")
    ("'f|oo-bar-baz" "'f|-bar-baz")
    ("'foo-|bar-baz" "'foo-|-baz"))))

(sp-test-command sp-kill-symbol
  ((nil
    ("|  'foo-bar-baz" "|")
    ("|'foo-bar-baz" "|")
    ("'|foo-bar-baz" "'|")
    ("'f|oo-bar-baz" "'|")
    ("'foo-|bar-baz" "'|"))))

(sp-test-command sp-up-sexp
  ((nil
    ("(;; foo\n b|ar\n baz\n )" "(;; foo\n bar\n baz)|")
    ("(;; foo\n b|ar\n baz\n ;; foo\n )" "(;; foo\n bar\n baz\n ;; foo\n )|")

    ;; #446
    ("(define-key smartparens-mode-map (kbd | \"C-(\") 'sp-down-sexp)" "(define-key smartparens-mode-map (kbd  \"C-(\")| 'sp-down-sexp)" )
    ("(define-key smartparens-mode-map (kbd | \"C-[\") 'sp-up-sexp)" "(define-key smartparens-mode-map (kbd  \"C-[\")| 'sp-up-sexp)" )
    ("(define-key smartparens-mode-map (kbd | \"C-{\") 'sp-beginning-of-next-sexp)" "(define-key smartparens-mode-map (kbd  \"C-{\")| 'sp-beginning-of-next-sexp)")

    ;; #616
    ("\"[\t| ]\"" "\"[\t ]|\"")
    ("\"[\t ]|  \"" "\"[\t ]\"|"))
   (((current-prefix-arg -1))
    ("(\n b|ar\n baz)" "|(bar\n baz)")
    ("(;; foo\n b|ar\n baz)" "|(;; foo\n bar\n baz)")
    ("(`|(depends-on ,pkg))" "|(`(depends-on ,pkg))")
    ("(,@|(depends-on ,pkg))" "|(,@(depends-on ,pkg))"))))

(sp-test-command sp-down-sexp
  ((((mode 'python))
    ("\"foo bar| 'baz qux' fux\"" "\"foo bar '|baz qux' fux\"")
    ("\"foo |bar 'baz qux' fux\"" "\"foo bar '|baz qux' fux\"")
    ("\"foo |bar [baz qux] fux\"" "\"foo bar [|baz qux] fux\""))))

(sp-test-command sp-backward-down-sexp
  ((((mode 'python))
    ("\"foo bar 'baz qux' |fux\"" "\"foo bar 'baz qux|' fux\"")
    ("\"foo bar 'baz qux' fux| bla\"" "\"foo bar 'baz qux|' fux bla\"")
    ("\"foo bar [baz qux] fux| bla\"" "\"foo bar [baz qux|] fux bla\""))))

(sp-test-command sp-end-of-sexp
  ((nil
    ;; #446
    ("(define-key smartparens-mode-map (kbd | \"C-(\") 'sp-down-sexp)" "(define-key smartparens-mode-map (kbd  \"C-(\"|) 'sp-down-sexp)" )
    ("(define-key smartparens-mode-map (kbd | \"C-[\") 'sp-up-sexp)" "(define-key smartparens-mode-map (kbd  \"C-[\"|) 'sp-up-sexp)" )
    ("(define-key smartparens-mode-map (kbd | \"C-{\") 'sp-beginning-of-next-sexp)" "(define-key smartparens-mode-map (kbd  \"C-{\"|) 'sp-beginning-of-next-sexp)" ))))

(sp-test-command sp-beginning-of-sexp
  ((nil
    ;; #446
    ("(define-key smartparens-mode-map (kbd | \"C-(\") 'sp-down-sexp)" "(define-key smartparens-mode-map (|kbd  \"C-(\") 'sp-down-sexp)" )
    ("(define-key smartparens-mode-map (kbd | \"C-[\") 'sp-up-sexp)" "(define-key smartparens-mode-map (|kbd  \"C-[\") 'sp-up-sexp)" )
    ("(define-key smartparens-mode-map (kbd | \"C-{\") 'sp-beginning-of-next-sexp)" "(define-key smartparens-mode-map (|kbd  \"C-{\") 'sp-beginning-of-next-sexp)" ))))

(sp-test-command backward-delete-char
  ((nil
    (";;asdas'|\n'asdasd'" ";;asdas|\n'asdasd'")
    ("foo \"|\" bar" "foo | bar")
    ("foo [|] bar" "foo | bar")
    ("foo \\{|\\} bar" "foo | bar")
    (";;foo \\{|\\}\nbar" ";;foo |\nbar"))))

(sp-test-command sp-comment
  ((((sp-comment-string '(((emacs-lisp-mode) . ";; "))))
    ;; don't do magic inside strings or commends
    (";; | asd" ";; ;| asd")
    (";; (asd|\nbar)" ";; (asd;|\nbar)")
    ("\"| asd\"" "\";| asd\"")
    ("\"(asd|\nbar)\"" "\"(asd;|\nbar)\"")

    ("|asd" ";; |asd")
    ("|(asd asd)" ";; |(asd asd)")
    ("(asd| asd)" "(asd ;; | asd\n )")
    ("(asd |asd)" "(asd ;; |asd\n )")

    ("|(foo\n bar)" ";; |\n(foo\n bar)")
    ("(foo|\n bar)" "(foo ;; |\n bar)")
    ("(foo\n bar|)" "(foo\n bar ;; |\n )")
    ("(foo\n bar| )" "(foo\n bar ;; |\n )")
    ("(foo\n bar | )" "(foo\n bar ;; |\n )")

    ("(foo\n |(bar ) )" "(foo\n ;; |(bar )\n )")
    ("(foo\n (bar| ) )" "(foo\n (bar ;; |\n  ) )")
    ("(foo\n (bar |) )" "(foo\n (bar ;; |\n  ) )")
    ("(foo\n (bar )| )" "(foo\n (bar ) ;; |\n )")
    ("(foo\n (bar ) |)" "(foo\n (bar ) ;; |\n )")

    ;; clean up hanging whitespace
    ("(foo\n |bar (baz\n      qux))" "(foo\n ;; |bar\n (baz\n  qux))")
    ("(foo\n bar| (baz\n      qux))" "(foo\n bar ;; |\n (baz\n  qux))")
    ("(foo\n bar |(baz\n      qux))" "(foo\n bar ;; |\n (baz\n  qux))")
    ("(foo\n bar (baz\n      |qux))" "(foo\n bar (baz\n      ;; |qux\n      ))"))))

(sp-test-command sp-kill-sexp
  ((nil
    ("(foo |(abc) bar)" "(foo | bar)"))
   (((current-prefix-arg 2))
    ("(foo (bar) | baz)" "|"))
   (((current-prefix-arg '(16)))
    ("(foo |(bar) baz)" "|"))
   (((current-prefix-arg '(4)))
    ("(1 |2 3 4 5 6)" "(1|)"))
   (((current-prefix-arg 3))
    ("(1 |2 3 4 5 6)" "(1 | 5 6)"))
   (((current-prefix-arg -2))
    ("(1 2 3 4 5| 6)" "(1 2 3 | 6)"))
   (((current-prefix-arg '(-4)))
    ("(1 2 3 4| 5 6)" "(|5 6)"))
   (((current-prefix-arg '(4)))
    ("(1 2 |   )" "(1 2|)"))
   (((current-prefix-arg 0))
    ("(1 2 3 |4 5 6)" "(|)"))
   (((mode 'python)
     (current-prefix-arg -1))
    ("x - |" "|"))))

(sp-test-command sp-backward-delete-char
  ((nil
    ("[foo]|" "[foo|]")
    ("\\{foo\\}|" "\\{foo|\\}")
    ("\"foo\\\\\"|" "\"foo\\\\|\""))))

(ert-deftest sp-test-command-sp-backward-delete-char-hungry-delete-mode ()
  "In `hungry-delete-mode' we should kill all whitespace."
  (sp-test-with-temp-elisp-buffer "(foo   )   |"
    (require 'hungry-delete)
    (hungry-delete-mode 1)
    (sp-backward-delete-char)
    (sp-buffer-equals "(foo   )|")
    (sp-backward-delete-char)
    (sp-buffer-equals "(foo   |)")
    (sp-backward-delete-char)
    (sp-buffer-equals "(foo|)")))

(defun sp-test--sp-backward-delete-char-textmode (initial expected &optional n)
  (setq n (or n 1))
  (sp-test-with-temp-buffer initial
      (text-mode)
    (sp-backward-delete-char n)
    (sp-buffer-equals expected)))

(ert-deftest sp-test-command-sp-backward-delete-char-textmode ()
  (let ((sp-pairs '((t . ((:open "\"" :close "\"" :actions (insert wrap autoskip navigate))
                          (:open "'" :close "'" :actions (insert wrap autoskip navigate)))))))
    (sp-test--sp-backward-delete-char-textmode "foo \"it'| OK\" baz" "foo \"it| OK\" baz")
    (sp-test--sp-backward-delete-char-textmode "foo \"|OK\" baz" "foo \"|OK\" baz")
    (sp-test--sp-backward-delete-char-textmode "foo \"it's OK\"| baz" "foo \"it's OK|\" baz")
    (sp-test--sp-backward-delete-char-textmode "\"this\" doesn'|" "\"this\" doesn|")
    (sp-test--sp-backward-delete-char-textmode "\"don't\"|" "|" 7)))

(ert-deftest sp-test-command-sp-backward-delete-pair-with-skip-match ()
  (let ((sp-pairs '((t . ((:open "`" :close "'" :actions (insert wrap autoskip navigate)
                           :skip-match (lambda (ms mb me)
                                         (when (equal ms "'")
                                           (save-excursion
                                             (goto-char me)
                                             (looking-at-p "\\sw"))))))))))
    (sp-test-with-temp-buffer "`foo'|bar'"
        (latex-mode)
      (smartparens-strict-mode 1)
      (sp-backward-delete-char 1)
      (sp-buffer-equals "`foo|bar'"))))

(sp-test-command sp-delete-char
  ((nil
    ("|[foo]" "[|foo]")
    ("|\\{foo\\}" "\\{|foo\\}")
    ("|\"foo\\\\\"" "\"|foo\\\\\""))))

(ert-deftest sp-test-command-sp-delete-char-hungry-delete-mode ()
  "In `hungry-delete-mode' we should kill all whitespace."
  (sp-test-with-temp-elisp-buffer "|   (   foo)"
    (require 'hungry-delete)
    (hungry-delete-mode 1)
    (sp-delete-char)
    (sp-buffer-equals "|(   foo)")
    (sp-delete-char)
    (sp-buffer-equals "(|   foo)")
    (sp-delete-char)
    (sp-buffer-equals "(|foo)")))

(defun sp-test--sp-delete-char-textmode (initial expected)
  (sp-test-with-temp-buffer initial
      (text-mode)
    (sp-delete-char)
    (sp-buffer-equals expected)))

(ert-deftest sp-test-command-sp-delete-char-textmode ()
  (let ((sp-pairs '((t . ((:open "\"" :close "\"" :actions (insert wrap autoskip navigate))
                          (:open "'" :close "'" :actions (insert wrap autoskip navigate)))))))
    (sp-test--sp-delete-char-textmode "foo \"it|' OK\" baz" "foo \"it| OK\" baz")
    (sp-test--sp-delete-char-textmode "foo \"OK|\" baz" "foo \"OK|\" baz")
    (sp-test--sp-delete-char-textmode "foo |\"it's OK\" baz" "foo \"|it's OK\" baz")
    (sp-test--sp-delete-char-textmode "foo|'s \"baz bar\" aaa" "foo|s \"baz bar\" aaa")
    ))

(sp-test-command sp-kill-whole-line
  ((nil
    ("(progn (some |long sexp))" "|")
    ("(progn\n  (some |long sexp))" "(progn\n  |)")
    ("(progn\n | (some\nlong\nsexp))" "(progn\n  |)")
    ("(progn\n  (so|me\nlong\nsexp))" "(progn\n  |)"))))

(sp-test-command sp-transpose-sexp
  ((nil
    ;; Preserve whitespace
    ("foo |  bar" "bar   foo|")
    ("(foo bar)|:symbol" ":symbol(foo bar)|")

    ;; Preserve prefix
    ("'foo |  bar" "bar   'foo|")
    (",@(foo bar)| :symbol" ":symbol ,@(foo bar)|")
    )
   (((mode 'python))
    ;; Do not drag suffix
    ("def foo(first, |second):" "def foo(second, first|):"))))


(sp-test-command sp-transpose-hybrid-sexp
  ((nil
    ;; Preserve whitespace
    ("foo   bar|\nbaz" "baz\nfoo   bar|")
    ("foo (bas\n     bar)|\n(next list)" "(next list)\nfoo (bas\n     bar)|"))

   ;; Do not drag suffix
   (((mode 'c))
    ("void f() {\n  int a[] = {\n    foo(1,2),|\n    bar(3,4)\n  };   \n}"
     "void f() {\n  int a[] = {\n    bar(3,4),\n    foo(1,2)\n  |};   \n}"))))

(sp-test-command sp-change-inner
  ((nil
    ("(f|oo [bar] baz)" "(foo [|] baz)"))
   (((mode 'js))
    ("{|'foo': 'bar'}" "{'|': 'bar'}"))))

(defun sp--test-sp-rewrap-sexp (initial pair expected &optional keep)
  (sp-test-with-temp-elisp-buffer initial
    (sp-rewrap-sexp pair keep)
    (insert "|")
    (should (equal (buffer-string) expected))))

(defun sp--test-sp-rewrap-sexp-python (initial pair expected &optional keep)
  (sp-test-with-temp-buffer initial
      (python-mode)
    (sp-rewrap-sexp pair keep)
    (insert "|")
    (should (equal (buffer-string) expected))))

(ert-deftest sp-test-command-sp-rewrap-sexp ()
  (sp--test-sp-rewrap-sexp "[f|oo]" '("(" . ")") "(f|oo)")
  (sp--test-sp-rewrap-sexp "{f|oo}" '("(" . ")") "(f|oo)")
  (sp--test-sp-rewrap-sexp "\"f|oo\"" '("(" . ")") "(f|oo)")
  (sp--test-sp-rewrap-sexp "(f|oo)" '("[" . "]") "[f|oo]")
  (sp--test-sp-rewrap-sexp "(f|oo)" '("\\{" . "\\}") "\\{f|oo\\}")
  (sp--test-sp-rewrap-sexp "(f|oo)" '("\"" . "\"") "\"f|oo\"")

  (sp--test-sp-rewrap-sexp "[f|oo]" '("(" . ")") "([f|oo])" :keep)
  (sp--test-sp-rewrap-sexp "(f|oo)" '("[" . "]") "[(f|oo)]" :keep)
  (sp--test-sp-rewrap-sexp "\\{f|oo\\}" '("[" . "]") "[\\{f|oo\\}]" :keep)
  (sp--test-sp-rewrap-sexp "[f|oo]" '("\\{" . "\\}") "\\{[f|oo]\\}" :keep))

(ert-deftest sp-test-command-sp-rewrap-sexp-escape-after-rewrap ()
  ;; #667
  (sp--test-sp-rewrap-sexp "\"foo (b|ar) baz\"" '("\"" . "\"") "\"foo \\\"b|ar\\\" baz\"")
  (sp--test-sp-rewrap-sexp "\"foo \\\"b|ar\\\" baz\"" '("(" . ")") "\"foo (b|ar) baz\"")

  (sp--test-sp-rewrap-sexp-python "\"foo 'b|ar' baz\"" '("\"" . "\"") "\"foo \\\"b|ar\\\" baz\"")
  (sp--test-sp-rewrap-sexp-python "\"foo 'bar' b|az\"" '("'" . "'") "'foo \\'bar\\' b|az'")
  (sp--test-sp-rewrap-sexp-python "'foo \\'b|ar\\' baz'" '("\"" . "\"") "'foo \"b|ar\" baz'"))

(ert-deftest sp-test-command-sp-rewrap-sexp-invalid-pair ()
  (should-error
   (sp-test-with-temp-elisp-buffer "(fo|o)"
     (let ((unread-command-events (list ?\a)))
       (call-interactively 'sp-rewrap-sexp)))
   :type 'user-error))

(ert-deftest sp-test-command-sp-select-next-thing-empty-buffer ()
  "Ensure we call user-error at buffer end."
  (should-error
   (sp-test-with-temp-elisp-buffer ""
     (call-interactively 'sp-select-next-thing))
   :type 'user-error))

(ert-deftest sp-test-command-sp-select-previous-thing-empty-buffer ()
  "Ensure we call user-error at buffer end."
  (should-error
   (sp-test-with-temp-elisp-buffer ""
     (call-interactively 'sp-select-previous-thing))
   :type 'user-error))

(ert-deftest sp-test-command-sp-mark-sexp ()
  (sp-test-with-temp-elisp-buffer "|(foo) (bar) (baz)"
    (call-interactively 'sp-mark-sexp)
    (sp-buffer-equals "|(foo)M (bar) (baz)")))

(ert-deftest sp-test-command-sp-mark-sexp-extend-existing-region ()
  (sp-test-with-temp-elisp-buffer "|(foo)M (bar) (baz)"
    (call-interactively 'sp-mark-sexp)
    (sp-buffer-equals "|(foo) (bar)M (baz)")))

(ert-deftest sp-test-command-sp-mark-sexp-multiple-invocations ()
  (let ((smartparens-mode-map smartparens-mode-map))
    (sp-test-with-temp-elisp-buffer "|(foo) (bar) (baz)"
      (execute-kbd-macro "mm")
      (sp-buffer-equals "|(foo) (bar)M (baz)"))))

(ert-deftest sp-test-command-sp-mark-sexp-invalid ()
  (let ((smartparens-mode-map smartparens-mode-map))
    (should-error
     (sp-test-with-temp-elisp-buffer "(progn (foo) |(bar)) (baz)"
       (define-key smartparens-mode-map "m" 'sp-mark-sexp)
       (execute-kbd-macro "mm"))
     :type 'user-error)))

(ert-deftest sp-test-command-sp-convolute-sexp-inside-symbol ()
  "Calling `sp-convolute-sexp' with point inside of symbol moves
point to end of symbol before convolving."
  (sp-test-with-temp-elisp-buffer "(foo (bar b|az))"
    (call-interactively #'sp-convolute-sexp)
    (sp-buffer-equals "(bar baz (foo|))")))

(ert-deftest sp-test-command-sp-convolute-sexp-whitespace ()
  "Calling `sp-convolute-sexp' eliminates extra whitespace.

This is the behavior of `paredit-convolute-sexp'."
  (sp-test-with-temp-elisp-buffer "(foo (bar  |  baz))"
    (call-interactively #'sp-convolute-sexp)
    (sp-buffer-equals "(bar (foo |baz))")))

(ert-deftest sp-test-yank-after-multiple-word-kill ()
  "When we `sp-kill-word' multiple times in a row, we should
  `yank' the entire killed sequence."
  (sp-test-with-temp-elisp-buffer "|some-long-symbol"
    (let ((smartparens-mode-map smartparens-mode-map))
      (define-key smartparens-mode-map "d" 'sp-kill-word)
      (execute-kbd-macro "ddd")
      (shut-up (call-interactively 'yank))
      (sp-buffer-equals "some-long-symbol|"))))

(ert-deftest sp-test-yank-after-multiple-backward-word-kill ()
  "When we `sp-backward-kill-word' multiple times in a row, we
  should `yank' the entire killed sequence."
  (sp-test-with-temp-elisp-buffer "some-long-symbol|"
    (let ((smartparens-mode-map smartparens-mode-map))
      (define-key smartparens-mode-map "d" 'sp-backward-kill-word)
      (execute-kbd-macro "ddd")
      (shut-up (call-interactively 'yank))
      (sp-buffer-equals "some-long-symbol|"))))

(ert-deftest sp-test-kill-region ()
  (sp-test-with-temp-elisp-buffer "[fo|o] bar [bMaz]"
    (let ((sp-message-width 1000)) ; need this for sp-message to retrieve the text
      (should-error
       (call-interactively 'sp-kill-region)
       :type 'user-error))))

(ert-deftest sp-test-delete-region ()
  (sp-test-with-temp-elisp-buffer "[fo|o] bar [bMaz]"
    (let ((sp-message-width 1000)) ; need this for sp-message to retrieve the text
      (should-error
       (call-interactively 'sp-delete-region)
       :type 'user-error))))

;; test for #452
(ert-deftest sp-test-sp-kill-hybrid-sexp-excessive-whitespace-nil nil
  (let ((sp-hybrid-kill-excessive-whitespace nil)
        (kill-ring kill-ring))
    (sp-test-with-temp-elisp-buffer "|(baz)\n\n\n\n(bar)"
      (call-interactively 'sp-kill-hybrid-sexp)
      (sp-buffer-equals "|\n\n\n\n(bar)")
      (insert (current-kill 0))
      (sp-buffer-equals "(baz)|\n\n\n\n(bar)"))))

;; test for #452
(ert-deftest sp-test-sp-kill-hybrid-sexp-excessive-whitespace-t nil
  (let ((sp-hybrid-kill-excessive-whitespace t)
        (kill-ring kill-ring))
    (sp-test-with-temp-elisp-buffer "|(baz)\n\n\n\n(bar)"
      (call-interactively 'sp-kill-hybrid-sexp)
      (sp-buffer-equals "|(bar)")
      (insert (current-kill 0))
      (sp-buffer-equals "(baz)|(bar)"))))

;; test for #452
(ert-deftest sp-test-sp-kill-hybrid-sexp-excessive-whitespace-kill nil
  (let ((sp-hybrid-kill-excessive-whitespace 'kill)
        (kill-ring kill-ring))
    (sp-test-with-temp-elisp-buffer "|(baz)\n\n\n\n(bar)"
      (call-interactively 'sp-kill-hybrid-sexp)
      (sp-buffer-equals "|(bar)")
      (insert (current-kill 0))
      (sp-buffer-equals "(baz)\n\n\n\n|(bar)"))))

(ert-deftest sp-test-sp-kill-sexp-cleanup-always-preserve nil
  (let ((sp-successive-kill-preserve-whitespace 0)
        (smartparens-mode-map smartparens-mode-map)
        (kill-ring nil))
    (sp-test-with-temp-elisp-buffer "(foo) |(bar)   (baz)  "
      (define-key smartparens-mode-map "d" 'sp-kill-sexp)
      (execute-kbd-macro "dd")
      (shut-up (call-interactively 'yank))
      (sp-buffer-equals "(foo) (bar)   (baz)  |"))))

(ert-deftest sp-test-sp-kill-sexp-cleanup-preserve-last nil
  (let ((sp-successive-kill-preserve-whitespace 1)
        (smartparens-mode-map smartparens-mode-map)
        (kill-ring kill-ring))
    (sp-test-with-temp-elisp-buffer "(foo) |(bar)   (baz)  "
      (define-key smartparens-mode-map "d" 'sp-kill-sexp)
      (execute-kbd-macro "dd")
      (shut-up (call-interactively 'yank))
      (sp-buffer-equals "(foo) (bar)   (baz)|"))))

(ert-deftest sp-test-sp-kill-sexp-cleanup-never-preserve nil
  (let ((sp-successive-kill-preserve-whitespace 2)
        (smartparens-mode-map smartparens-mode-map)
        (kill-ring kill-ring))
    (sp-test-with-temp-elisp-buffer "(foo) |(bar)   (baz)  "
      (define-key smartparens-mode-map "d" 'sp-kill-sexp)
      (execute-kbd-macro "dd")
      (shut-up (call-interactively 'yank))
      (sp-buffer-equals "(foo) (bar) (baz)|"))))

(ert-deftest sp-test-sp-prefix-save-excursion-keep-indentation nil
  (sp-test-with-temp-elisp-buffer "(progn
  |(foo)
  (bar))"
    ;; TODO: turn this into some helper
    (let ((overlay (make-overlay (point-min) (point-max) nil t t)))
      (overlay-put overlay
                   'keymap
                   (let ((map (make-sparse-keymap)))
                     (define-key map "x" 'sp-extract-before-sexp)
                     (define-key map "e" 'sp-prefix-save-excursion)
                     map))
      (execute-kbd-macro "ex")
      (sp-buffer-equals "(foo)
(progn
  |(bar))"))))
