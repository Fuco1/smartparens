(require 'smartparens)

(defun sp-test-command-setup ()
  (cond
   ((not (boundp 'mode)) (emacs-lisp-mode))
   ((eq mode 'elisp) (emacs-lisp-mode))
   ((eq mode 'racket) (racket-mode))
   ((eq mode 'c) (c-mode))
   ((eq mode 'python) (python-mode)))
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
               (eq mode 'racket)
               (version<= "24.3" emacs-version))
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

(sp-test-command sp-forward-slurp-sexp
  ((nil
    ;; beware, this also tests the reindenting/cleanup!!!
    ("(f|oo)\nbar ;; baz (foo) baz\n(quux)"
     "(f|oo\n bar) ;; baz (foo) baz\n(quux)"
     "(f|oo\n bar ;; baz (foo) baz\n (quux))")

    ("(foo)\nbar ;; baz (f|oo) baz\n(quux)"
     "(foo)\nbar ;; baz (f|oo baz)\n(quux)"))
   (((current-prefix-arg '(4)))
    ("[(fo|o) bar baz]" "[(fo|o bar baz)]")
    ("((progn| bar (baz) (baz)))" "((progn| bar (baz) (baz)))"))

   (((mode 'c))
    ("int funct() { int |foo =} bar;" "int funct() { int |foo = bar;}")
    ("int funct() { int |foo =}; bar" "int funct() { int |foo = bar};")
    ("int funct() { int |foo =}; bar;" "int funct() { int |foo = bar;};"))
   (((mode 'racket)
     (sp-sexp-prefix '((racket-mode regexp "#?['`,]@?"))))
    ("(f|oo)  #'(bar)" "(f|oo  #'(bar))"))))

(sp-test-command sp-backward-slurp-sexp
  ((nil
    ;; beware, this also tests the reindenting/cleanup!!!
    ("(foo)\nbar ;; baz (foo) baz\n(qu|ux)"
     "(foo)\n(bar ;; baz (foo) baz\n qu|ux)"
     "((foo)\n bar ;; baz (foo) baz\n qu|ux)")

    ("(foo)\nbar ;; baz (f|oo) baz\n(quux)"
     "(foo)\nbar ;; (baz f|oo) baz\n(quux)"))))

(sp-test-command sp-forward-barf-sexp
  ((nil
    ;; beware, this also tests the reindenting/cleanup!!!
    ("(f|oo\n bar ;; baz (foo) baz\n (quux))"
     "(f|oo\n bar) ;; baz (foo) baz\n(quux)"
     "(f|oo)\nbar ;; baz (foo) baz\n(quux)")

    ("(foo)\nbar ;; baz (f|oo baz)\n(quux)"
     "(foo)\nbar ;; baz (f|oo) baz\n(quux)"))

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
    )
   ;; from #243
   (((sp-navigate-consider-stringlike-sexp '(emacs-lisp-mode)))
    (";; \"quote\" here\nand \"|here\"" ";; \"quote\" here\nand |here")
    (";; \"|quote\" here\nand here" ";; |quote here\nand here")

    ;; from #580, should also work without sp-navigate-consider-stringlike-sexp
    ("(foo \"|bar\")" "(foo |bar)")

    ;; from #569
    ("(\" \" f|oo)" "|foo")
    ("(\" \" |foo)" "|foo")
    ("(\"x\" |foo)" "|foo"))
   ;; TODO: this should also work without sp-navigate-consider-stringlike-sexp
   ;; (nil
   ;;  ("(\" \" f|oo)" "|foo")
   ;;  ("(\" \" |foo)" "|foo")
   ;;  ("(\"x\" |foo)" "|foo"))
   ))

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
    ("(define-key sp-keymap (kbd | \"C-(\") 'sp-down-sexp)" "(define-key sp-keymap (kbd  \"C-(\")| 'sp-down-sexp)" )
    ("(define-key sp-keymap (kbd | \"C-[\") 'sp-up-sexp)" "(define-key sp-keymap (kbd  \"C-[\")| 'sp-up-sexp)" )
    ("(define-key sp-keymap (kbd | \"C-{\") 'sp-beginning-of-next-sexp)" "(define-key sp-keymap (kbd  \"C-{\")| 'sp-beginning-of-next-sexp)" ))
   (((current-prefix-arg -1))
    ("(\n b|ar\n baz)" "|(bar\n baz)")
    ("(;; foo\n b|ar\n baz)" "|(;; foo\n bar\n baz)")
    ("(`|(depends-on ,pkg))" "|(`(depends-on ,pkg))")
    ("(,@|(depends-on ,pkg))" "|(,@(depends-on ,pkg))"))))

(sp-test-command sp-end-of-sexp
  ((nil
    ;; #446
    ("(define-key sp-keymap (kbd | \"C-(\") 'sp-down-sexp)" "(define-key sp-keymap (kbd  \"C-(\"|) 'sp-down-sexp)" )
    ("(define-key sp-keymap (kbd | \"C-[\") 'sp-up-sexp)" "(define-key sp-keymap (kbd  \"C-[\"|) 'sp-up-sexp)" )
    ("(define-key sp-keymap (kbd | \"C-{\") 'sp-beginning-of-next-sexp)" "(define-key sp-keymap (kbd  \"C-{\"|) 'sp-beginning-of-next-sexp)" ))))

(sp-test-command sp-beginning-of-sexp
  ((nil
    ;; #446
    ("(define-key sp-keymap (kbd | \"C-(\") 'sp-down-sexp)" "(define-key sp-keymap (|kbd  \"C-(\") 'sp-down-sexp)" )
    ("(define-key sp-keymap (kbd | \"C-[\") 'sp-up-sexp)" "(define-key sp-keymap (|kbd  \"C-[\") 'sp-up-sexp)" )
    ("(define-key sp-keymap (kbd | \"C-{\") 'sp-beginning-of-next-sexp)" "(define-key sp-keymap (|kbd  \"C-{\") 'sp-beginning-of-next-sexp)" ))))

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

(defun sp--test-sp-rewrap-sexp (initial pair expected &optional keep)
  (sp-test-with-temp-elisp-buffer initial
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

(ert-deftest sp-test-command-sp-rewrap-sexp-invalid-pair ()
  (condition-case c
      (sp-test-with-temp-elisp-buffer "(fo|o)"
        (let ((unread-command-events (list ?\a)))
          (call-interactively 'sp-rewrap-sexp))
        (error "We should never get here"))
    (user-error t)))

(ert-deftest sp-test-command-sp-select-next-thing-empty-buffer ()
  "Ensure we call user-error at buffer end."
  (condition-case c
      (sp-test-with-temp-elisp-buffer ""
        (call-interactively 'sp-select-next-thing)
        (error "We should never get here"))
    (user-error t)))
