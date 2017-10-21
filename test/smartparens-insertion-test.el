;; TODO: add proper headers and organize tests a bit better

(require 'smartparens)
(require 'smartparens-scala)
(require 'scala-mode)

(defun sp-test-insertion (initial keys result)
  (sp-test-with-temp-elisp-buffer initial
    (execute-kbd-macro keys)
    (sp-buffer-equals result)))

(ert-deftest sp-test-insertion-basic nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-insertion "|" "(" "()")
    (sp-test-insertion "|" "((" "(())")
    (sp-test-insertion "|" "(()(" "(()())")
    (sp-test-insertion "|" "[[" "[[]]")
    (sp-test-insertion "|" "[[][" "[[][]]")
    (sp-test-insertion "|" "OPEN" "OPENCLOSE")
    (sp-test-insertion "|" "\\langle" "\\langle\\rangle")
    (sp-test-insertion "|" "foo \\langle" "foo \\langle\\rangle")
    (sp-test-insertion "foo |" "\\b" "foo \\big(\\big)")
    (sp-test-insertion "|" "[[][" "[[][]]")
    (sp-test-insertion "|" "\\{" "\\{\\}")))

(ert-deftest sp-test-insertion-pairs-with-shared-prefix ()
  (let ((sp-pairs '((t . ((:open "`" :close "'" :actions (insert wrap autoskip navigate))
                          (:open "``" :close "''" :actions (insert wrap autoskip navigate))
                          (:open "." :close "-" :actions (insert wrap autoskip navigate))
                          (:open ".;" :close "-+" :actions (insert wrap autoskip navigate))
                          (:open ".;:" :close "-+_" :actions (insert wrap autoskip navigate)))))))
    (sp-test-insertion "|" "`" "`'")
    (sp-test-insertion "|" "``" "``''")
    (sp-test-insertion "|" "```" "```'''")
    (sp-test-insertion "|" "````" "````''''")
    (sp-test-insertion "`|'" "`" "``''")
    (sp-test-insertion "```|'''" "`" "````''''")
    (sp-test-insertion "|" "." ".-")
    (sp-test-insertion "|" ".;" ".;-+")
    (sp-test-insertion "|" ".;:" ".;:-+_")
    (sp-test-insertion ".;|-+" ":" ".;:-+_")
    ))

(ert-deftest sp-test-insertion-stringlike-pairs-with-shared-suffix ()
  (let ((sp-pairs '((t . ((:open "*" :close "*" :actions (insert wrap autoskip navigate))
                          (:open "**" :close "**" :actions (insert wrap autoskip navigate)))))))
    (sp-test-insertion "|" "*" "**")
    (sp-test-insertion "|" "**" "****")
    (sp-test-insertion "|" "***" "******")
    (sp-test-insertion "|" "****" "********")
    (sp-test-insertion "*|*" "*" "****")
    (sp-test-insertion "***|***" "*" "********")))

(ert-deftest sp-test-insertion-pairs-with-shared-suffix ()
  (let ((sp-pairs '((t . ((:open "{" :close "}" :actions (insert wrap autoskip navigate))
                          (:open "{-" :close "-}" :actions (insert wrap autoskip navigate))
                          (:open "{--" :close "--}" :actions (insert wrap autoskip navigate) :trigger "a"))))))
    (sp-test-insertion "|" "{" "{}")
    (sp-test-insertion "|" "{-" "{--}")
    (sp-test-insertion "{-|-}" "-" "{----}")
    (sp-test-insertion "|" "{--" "{----}")))

(ert-deftest sp-test-insertion-pair-with-spaces ()
  (let ((sp-pairs '((t . ((:open "[ " :close " ]" :actions (insert wrap autoskip navigate)))))))
    (sp-test-insertion "|" "[ " "[ | ]")))

(ert-deftest sp-test-insertion-pair-when-in-string ()
  (let ((sp-pairs '((t . ((:open "[" :close "]"
                           :actions (insert wrap autoskip navigate)
                           :when (sp-in-string-p)))))))
    (sp-test-insertion "\"foo | bar\"" "[" "\"foo [|] bar\"")
    (sp-test-insertion "foo | bar" "[" "foo [| bar")))

(ert-deftest sp-test-insertion-pair-unless-in-string ()
  (let ((sp-pairs '((t . ((:open "[" :close "]"
                           :actions (insert wrap autoskip navigate)
                           :unless (sp-in-string-p)))))))
    (sp-test-insertion "\"foo | bar\"" "[" "\"foo [| bar\"")
    (sp-test-insertion "foo | bar" "[" "foo [|] bar")))

(ert-deftest sp-test-insertion-pair-when-in-string-override-setting ()
  (let ((sp-pairs sp-pairs))
    (sp-local-pair '(emacs-lisp-mode lisp-mode) "%" "$")
    (sp-test-insertion "foo | bar" "%" "foo %|$ bar")
    (sp-local-pair 'emacs-lisp-mode "%" nil :when '(sp-in-string-p))
    (sp-test-insertion "\"foo | bar\"" "%" "\"foo %|$ bar\"")
    (sp-test-insertion "foo | bar" "%" "foo %| bar")))

(ert-deftest sp-test-insertion-pair-replace-existing-ending-for-pair-with-shared-prefix ()
  "If the to-be-inserted pair has a prefix which is also a pair
we migth be extending the opener of a sexp with this opener.  In
which case we should probably rewrap."
  (let ((sp-pairs sp-pairs))
    (sp-test-with-temp-elisp-buffer "{| foo }"
      (sp-update-local-pairs '(:open "{" :close "}" :actions (insert navigate)))
      (sp-update-local-pairs '(:open "{-" :close "-}" :actions (insert navigate)))
      (execute-kbd-macro "-")
      (sp-buffer-equals "{-| foo -}"))))

(defun sp-test-latex-insertion (initial keys result)
  (sp-test-with-temp-buffer initial
      (latex-mode)
    (shut-up (execute-kbd-macro keys))
    (should (equal (buffer-string) result))))

;; TODO: ideally, we would figure out why that doesn't work on 24.1
;; and 24.2 but it's a waste of time.  If some users are on those
;; versions, they are welcome to figure it out for us :)
(ert-deftest sp-test-insertion-latex nil
  (shut-up (load "auctex-autoloads"))
  (let ((sp-undo-pairs-separately nil)
        (sp-pairs '((latex-mode
                     (:open "$" :close "$" :actions (insert wrap autoskip navigate))
                     (:open "\\[" :close "\\]" :actions (insert wrap autoskip navigate))
                     (:open "\\bigl(" :close "\\bigr)" :actions (insert wrap autoskip navigate))
                     (:open "[" :close "]" :actions (insert wrap autoskip navigate))
                     (:open "``" :close "''" :trigger "\"" :actions (insert wrap autoskip navigate))
                     (:open "`" :close "'" :actions (insert wrap autoskip navigate))))))
    (sp-test-latex-insertion "|" "$" "$$")
    (sp-test-latex-insertion "|" "`" "`'")
    (sp-test-latex-insertion "|" "$$" "$$")
    (sp-test-latex-insertion "|" "$foo$$foo" "$foo$$foo$")
    (sp-test-latex-insertion "foo |" "$" "foo $$")
    (sp-test-latex-insertion "|" "\\[" "\\[\\]")
    (sp-test-latex-insertion "\\|" "[" "\\[\\]")
    (sp-test-latex-insertion "|" "[" "[]")
    (sp-test-latex-insertion "foo | bar" "\\bigl(" "foo \\bigl(\\bigr) bar")
    (sp-test-latex-insertion "foo | bar" "``" "foo ``'' bar")
    (sp-test-latex-insertion "foo | bar" "\"" "foo ``'' bar")
    ))

(defun sp-test--pair-to-insert (initial expected)
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-with-temp-elisp-buffer initial
      (let* ((actual (sp--pair-to-insert))
             (r (and actual (cons (plist-get actual :open) (plist-get actual :close)))))
        (should (equal r expected))))))

(ert-deftest sp-test-insertion-pair-to-insert nil
  (sp-test--pair-to-insert "[|" (cons "[" "]"))
  (sp-test--pair-to-insert "[|]" (cons "[" "]"))
  (sp-test--pair-to-insert "foo [|] bar" (cons "[" "]"))
  (sp-test--pair-to-insert "foo [|" (cons "[" "]"))
  (sp-test--pair-to-insert "OPEN|" (cons "OPEN" "CLOSE"))
  (sp-test--pair-to-insert "OPEN|CLOSE" nil) ;should not expand in the middle of the word
  (sp-test--pair-to-insert "foo OPEN|bar" nil) ;should not expand in the middle of the word
  (sp-test--pair-to-insert "foo OPEN|" (cons "OPEN" "CLOSE"))
  (sp-test--pair-to-insert "\\langle|" (cons "\\langle" "\\rangle"))
  (sp-test--pair-to-insert "\\langle|\\rangle" (cons "\\langle" "\\rangle"))
  (sp-test--pair-to-insert "foo \\langle|\\rangle bar" (cons "\\langle" "\\rangle"))
  (sp-test--pair-to-insert "foo \\langle|" (cons "\\langle" "\\rangle"))
  ;; test trigger
  (sp-test--pair-to-insert "foo \\b|" (cons "\\big(" "\\big)")))

(ert-deftest sp-test-insert-pair-skip-closing ()
  "Typing ) should step over the closing paren."
  (sp-test-insertion "|" "(abc)" "(abc)")
  (should (eobp)))

(ert-deftest sp-test-insert-pair-skip-active-quotes nil
  (sp-test-insertion "|" "\"abc\"" "\"abc\"")
  (should (eobp)))

(ert-deftest sp-test-insert-pair-dont-skip-escaped-quotes nil
  (sp-test-insertion "\"abc|\"" "\\\"" "\"abc\\\"|\\\"\""))

(ert-deftest sp-test-insert-pair-skip-inactive-quotes nil
  (sp-test-insertion "|" "\"ab\C-b\C-dc\"" "\"ac\"|"))

(ert-deftest sp-test-insert-sp-autoskip-opening-pair nil
  (let ((sp-autoskip-opening-pair t))
    (sp-test-insertion "foo |\"bar\" baz" "\"" "foo \"|bar\" baz")))

(ert-deftest sp-test-insert-pair-skip-inactive-quotes-with-escape-enabled nil
  (let ((sp-pairs
         '((t (:open "\"" :close "\""
               :actions (insert wrap autoskip navigate escape)
               :unless (sp-in-string-quotes-p))))))
    (sp-test-insertion "|" "\"ab\C-b\C-dc\"" "\"ac\"|")))

(ert-deftest sp-test-insert-quote-escape-enabled nil
  (let ((sp-pairs
         '((t (:open "\"" :close "\""
               :actions (insert wrap autoskip navigate escape)
               :unless (sp-in-string-quotes-p))))))
    (sp-test-insertion "\"foo | bar\"" "\"" "\"foo \\\" bar\"")))

(ert-deftest sp-test-insert-quote-escape-quote-after-insert nil
  (let ((sp-pairs
         '((t
            (:open "\"" :close "\""
             :actions (insert wrap autoskip navigate)
             :post-handlers (sp-escape-quotes-after-insert))
            (:open "[" :close "]" :actions (insert wrap autoskip navigate))))))
    (sp-test-insertion "\"foo | bar\"" "\"" "\"foo \\\"|\\\" bar\"")))

(ert-deftest sp-test-insert-quote-dont-escape-quote-in-rst-mode nil
  "In text modes where ' and \" are not string syntax, do not
escape them on the top level."
  (let ((sp-pairs
         '((t
            (:open "\"" :close "\""
             :actions (insert wrap autoskip navigate)
             :post-handlers (sp-escape-quotes-after-insert))
            (:open "[" :close "]" :actions (insert wrap autoskip navigate))))))
    (sp-test-with-temp-buffer "foo | bar"
        (rst-mode)
      (execute-kbd-macro "\"")
      (sp-buffer-equals "foo \"|\" bar"))))

(ert-deftest sp-test-insert-quote-dont-escape-in-contraction nil
  "Do not escape ' after a word when it is used as a contraction"
  (let ((sp-pairs
         '((t
            (:open "'" :close "'"
             :actions (insert wrap autoskip navigate escape)
             :unless (sp-in-string-quotes-p sp-point-after-word-p)
             :post-handlers (sp-escape-wrapped-region sp-escape-quotes-after-insert))
            (:open "[" :close "]" :actions (insert wrap autoskip navigate))))))
    (sp-test-with-temp-buffer "foo| bar"
        (rst-mode)
      (execute-kbd-macro "'s")
      (sp-buffer-equals "foo's| bar"))))

;; #665
(ert-deftest sp-test-insert-quote-dont-escape-if-not-in-string-before-insertion nil
  "In `scala-mode' and modes where the syntax classes are hacked
on-the-fly it can happen that the quote character \" is not
always syntax class, and therefore unclosed string is *not*
parsed as string.  Adding the closing quote properly closes it
and therefore we should not escape the just-inserted quote."
  (let ((sp-pairs
         '((t (:open "\"" :close "\""
               :actions (insert wrap autoskip navigate escape)
               :unless (sp-in-string-quotes-p)))))
        (init "ensimeScalaCompilerJarmoduleIDs := {\n  val v = (scalaVersion in LocalProject(\"bootstrap|)).value\n  Seq(scalaOrganization.value % \"scala-compiler\" % v)\n}")
        (result "ensimeScalaCompilerJarmoduleIDs := {\n  val v = (scalaVersion in LocalProject(\"bootstrap\"|)).value\n  Seq(scalaOrganization.value % \"scala-compiler\" % v)\n}"))
    (sp-test-with-temp-buffer init
        (scala-mode)
      (execute-kbd-macro "\"|")
      (should (equal (buffer-string) result)))
    (sp-test-with-temp-buffer init
        (progn
          (scala-mode)
          (smartparens-strict-mode +1))
      (execute-kbd-macro "\"|")
      (should (equal (buffer-string) result)))))

(ert-deftest sp-test-insert-quote-do-not-escape-if-string-unbalanced ()
  "When we have an unbalanced string we should sometimes just close it.

If the point is in a string, we check the \"parity\" state of the
buffer and decide if to close or escape: if the parity at the end
of the buffer is correct, we escape, otherwise we close."
  (sp-test-insertion "[\"asd|]" "\"" "[\"asd\"|]")
  (sp-test-insertion "\"foo |] bar\"" "\"" "\"foo \\\"|] bar\"")
  (sp-test-insertion "\"first| \"second\"" "\"" "\"first\"| \"second\""))

(ert-deftest sp-test-insert-dont-insert-in-overwrite-mode ()
  (sp-test-with-temp-buffer "foo = |\"\""
      (shut-up (python-mode))
    (overwrite-mode 1)
    (execute-kbd-macro "''")
    (sp-buffer-equals "foo = ''|")))
