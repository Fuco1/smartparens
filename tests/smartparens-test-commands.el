(require 'dash)
(require 'smartparens-config)

(defun sp-test-command-setup ()
  (cond
   ((not (boundp 'mode)) (emacs-lisp-mode))
   ((eq mode 'elisp) (emacs-lisp-mode))
   ((eq mode 'c) (c-mode))))

(defmacro sp-test-command (command examples)
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
          (setq before expected))))))

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

   (((mode 'c))
    ("int funct() { int |foo =} bar;" "int funct() { int |foo = bar;}")
    ("int funct() { int |foo =}; bar" "int funct() { int |foo = bar};")
    ("int funct() { int |foo =}; bar;" "int funct() { int |foo = bar;};"))))

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
     "(foo)\nbar ;; baz (f|oo) baz\n(quux)"))))

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
    ("(f (   |;xy\n    h))" "(f |   ;xy\n h)")
    ("(f (   ;|xy\n    h))" "(f |   ;xy\n h)")
    ("(f (   ;x|y\n    h))" "(f |   ;xy\n h)")
    ("(f (   ;xy|\n    h))" "(f |   ;xy\n h)")
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

(sp-test-command sp-split-sexp
  ((nil
    ("(foo |bar baz)" "(foo) |(bar baz)")
    ("(foo bar|)" "(foo bar)|()")

    ("\"foo |bar baz\"" "\"foo\" |\"bar baz\"")
    ("\"foo bar|\"" "\"foo bar\"|\"\""))

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
    ("(;; foo\n b|ar\n baz\n ;; foo\n )" "(;; foo\n bar\n baz\n ;; foo\n )|"))
   (((current-prefix-arg -1))
    ("(\n b|ar\n baz)" "|(bar\n baz)")
    ("(;; foo\n b|ar\n baz)" "|(;; foo\n bar\n baz)")
    ("(`|(depends-on ,pkg))" "|(`(depends-on ,pkg))")
    ("(,@|(depends-on ,pkg))" "|(,@(depends-on ,pkg))"))))

(provide 'smartparens-test-commands)
