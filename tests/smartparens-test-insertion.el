;; These are _interactive_ insertion tests and handle both insertion
;; and wrapping.
;; TODO: add proper headers and organize tests a bit better
(require 'dash)
(require 'smartparens-config)

(defun sp-test-insertion (initial keys result)
  (sp-test-with-temp-elisp-buffer initial
    (-each keys 'execute-kbd-macro)
    (should (equal (buffer-string) result))))

(ert-deftest sp-test-basic-insertion nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-insertion "|" '("(" "(") "(())")
    (sp-test-insertion "|" '("(" "(" ")" "(") "(()())")
    (sp-test-insertion "|" '("[" "[") "[[]]")
    (sp-test-insertion "|" '("[" "[" "]" "[") "[[][]]")
    (sp-test-insertion "|" '("O" "P" "E" "N") "OPENCLOSE")
    (sp-test-insertion "|" '("\\" "l" "a" "n" "g" "l" "e") "\\langle\\rangle")
    (sp-test-insertion "|" '("f" "o" "o" " " "\\" "l" "a" "n" "g" "l" "e") "foo \\langle\\rangle")
    (sp-test-insertion "foo |" '("\\" "b" ) "foo \\big(\\big)")
    (sp-test-insertion "|" '("[" "[" "]" "[") "[[][]]")
    (sp-test-insertion "|" '("\\" "{") "\\{\\}"))
  (let ((sp-pairs '((t . ((:open "`" :close "'" :actions (insert wrap autoskip navigate))
                          (:open "``" :close "''" :actions (insert wrap autoskip navigate)))))))
    (sp-test-insertion "|" '("`") "`'")
    (sp-test-insertion "|" '("`" "`") "``''")
    (sp-test-insertion "|" '("`" "`" "`") "```'''")
    (sp-test-insertion "|" '("`" "`" "`" "`") "````''''")
    (sp-test-insertion "`|'" '("`") "``''")
    (sp-test-insertion "```|'''" '("`") "````''''"))
  (let ((sp-pairs '((t . ((:open "{" :close "}" :actions (insert wrap autoskip navigate))
                          (:open "{-" :close "-}" :actions (insert wrap autoskip navigate))
                          (:open "{--" :close "--}" :actions (insert wrap autoskip navigate) :trigger "a"))))))
    (sp-test-insertion "|" '("{") "{}")
    (sp-test-insertion "|" '("{" "-") "{--}")
    (sp-test-insertion "{-|-}" '("-") "{----}")
    (sp-test-insertion "|" '("{" "-" "-") "{----}")))

(defun sp-test-latex-insertion (initial keys result)
  (sp-test-with-temp-buffer initial
      (latex-mode)
    (execute-kbd-macro keys)
    (should (equal (buffer-string) result))))

(ert-deftest sp-test-latex-insertion nil
  (load "auctex-autoloads")
  (let ((sp-pairs '((t . ((:open "$" :close "$" :actions (insert wrap autoskip navigate))
                          (:open "\\[" :close "\\]" :actions (insert wrap autoskip navigate))
                          (:open "\\bigl(" :close "\\bigr)" :actions (insert wrap autoskip navigate))
                          (:open "[" :close "]" :actions (insert wrap autoskip navigate)))))))
    (sp-test-latex-insertion "|" "$" "$$")
    (sp-test-latex-insertion "|" "$$" "$$$$")
    (sp-test-latex-insertion "|" "$foo$$foo" "$foo$$foo$")
    (sp-test-latex-insertion "foo |" "$" "foo $$")
    (sp-test-latex-insertion "|" "\\[" "\\[\\]")
    (sp-test-latex-insertion "\\|" "[" "\\[\\]")
    (sp-test-latex-insertion "|" "[" "[]")
    (sp-test-latex-insertion "foo | bar" "\\bigl(" "foo \\bigl(\\bigr) bar")))

(defun sp-test--pair-to-insert (initial expected)
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-with-temp-elisp-buffer initial
      (let* ((actual (sp--pair-to-insert))
             (r (and actual (cons (plist-get actual :open) (plist-get actual :close)))))
        (should (equal r expected))))))

(ert-deftest sp-test-pair-to-insert nil
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

(provide 'smartparens-test-insertion)
