;; TODO: add proper headers and organize tests a bit better

(require 'smartparens)

(defun sp-test-insertion (initial keys result)
  (sp-test-with-temp-elisp-buffer initial
    (execute-kbd-macro keys)
    (should (equal (buffer-string) result))))

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
    (sp-test-insertion "|" "\\{" "\\{\\}"))
  (let ((sp-pairs '((t . ((:open "`" :close "'" :actions (insert wrap autoskip navigate))
                          (:open "``" :close "''" :actions (insert wrap autoskip navigate)))))))
    (sp-test-insertion "|" "`" "`'")
    (sp-test-insertion "|" "``" "``''")
    (sp-test-insertion "|" "```" "```'''")
    (sp-test-insertion "|" "````" "````''''")
    (sp-test-insertion "`|'" "`" "``''")
    (sp-test-insertion "```|'''" "`" "````''''"))
  (let ((sp-pairs '((t . ((:open "{" :close "}" :actions (insert wrap autoskip navigate))
                          (:open "{-" :close "-}" :actions (insert wrap autoskip navigate))
                          (:open "{--" :close "--}" :actions (insert wrap autoskip navigate) :trigger "a"))))))
    (sp-test-insertion "|" "{" "{}")
    (sp-test-insertion "|" "{-" "{--}")
    (sp-test-insertion "{-|-}" "-" "{----}")
    (sp-test-insertion "|" "{--" "{----}")))

(defun sp-test-latex-insertion (initial keys result)
  (sp-test-with-temp-buffer initial
      (latex-mode)
    (execute-kbd-macro keys)
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
  (sp-test-insertion "\"abc|\"" "\\\"|" "\"abc\\\"|\\\"\""))

(ert-deftest sp-test-insert-pair-skip-inactive-quotes nil
  (sp-test-insertion "|" "\"ab\C-b\C-dc\"|" "\"ac\"|"))

(ert-deftest sp-test-insert-pair-skip-inactive-quotes-with-escape-enabled nil
  (let ((sp-pairs
         '((t (:open "\"" :close "\""
               :actions (insert wrap autoskip navigate escape)
               :unless (sp-in-string-quotes-p))))))
    (sp-test-insertion "|" "\"ab\C-b\C-dc\"|" "\"ac\"|")))

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
    (sp-test-insertion "\"foo | bar\"" "\"|" "\"foo \\\"|\\\" bar\"")))

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
      (execute-kbd-macro "\"|")
      (should (equal (buffer-string) "foo \"|\" bar")))))

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
      (execute-kbd-macro "'s|")
      (should (equal (buffer-string) "foo's| bar")))))
