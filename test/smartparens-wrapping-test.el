(defun sp-test-wrapping (initial keys result)
  (sp-test-with-temp-elisp-buffer initial
    (-each (-list keys) 'execute-kbd-macro)
    (should (equal (buffer-string) (replace-regexp-in-string "[|]" "" result)))
    (should (= (1+ (string-match-p "|" result)) (point)))))

(ert-deftest sp-test-wrap-basic nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-wrapping "|aM" "(" "(|a)")
    (sp-test-wrapping "Ma|" "(" "(a)|")

    (sp-test-wrapping "|aM" "[" "[|a]")
    (sp-test-wrapping "Ma|" "[" "[a]|")

    (sp-test-wrapping "|aM" "\\{" "\\{|a\\}")
    (sp-test-wrapping "Ma|" "\\{" "\\{a\\}|")

    (sp-test-wrapping "|aM" "\\\"" "\\\"|a\\\"")
    (sp-test-wrapping "Ma|" "\\\"" "\\\"a\\\"|")

    (sp-test-wrapping "|aM" "\\langle" "\\langle|a\\rangle")
    (sp-test-wrapping "Ma|" "\\langle" "\\langlea\\rangle|")))

(ert-deftest sp-test-wrap-unbalanced-region nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-wrapping "[a b |c] dM e" "(" "[a b |c] d e")
    (sp-test-wrapping "[a b Mc] d| e" "(" "[a b c] d| e")))

(ert-deftest sp-test-wrap-with-closing nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-wrapping "|aM" "]" "[a]|")
    (sp-test-wrapping "Ma|" "]" "[a]|")
    (sp-test-wrapping "|aM" "\\}" "\\{a\\}|")))

(ert-deftest sp-test-wrap-repeated nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-wrapping "|aM" "[[" "[[|a]]")
    (sp-test-wrapping "Ma|" "[[" "[[a]]|")))

(ert-deftest sp-test-wrap-repeated-wrap-only-pair nil
  (let ((sp-pairs
         '((t
            (:open "*" :close "*" :actions (wrap))
            (:open "[" :close "]" :actions (wrap))))))
    (sp-test-wrapping "|aM" "**" "**|a**")
    (sp-test-wrapping "Ma|" "**" "**a**|")
    (sp-test-wrapping "|aM" "[[" "[[|a]]")
    (sp-test-wrapping "Ma|" "[[" "[[a]]|")))

(defun sp-test-wrapping-latex (initial keys result)
  (sp-test-with-temp-buffer initial
      (latex-mode)
    (-each (-list keys) 'execute-kbd-macro)
    (should (equal (buffer-string) (replace-regexp-in-string "[|]" "" result)))
    (should (= (1+ (string-match-p "|" result)) (point)))))

(ert-deftest sp-test-wrap-basic-latex nil
  (let ((sp-pairs '((latex-mode
                     ;; This following pair is a hack to let
                     ;; `sp-wrap--can-wrap-p' pass the test.  It
                     ;; should probably be replaced with a check if a
                     ;; wrappable pair exists.
                     (:open "\"" :close "\"" :actions (insert wrap autoskip navigate))
                     (:open "$" :close "$" :actions (insert wrap autoskip navigate))
                     (:open "\\[" :close "\\]" :actions (insert wrap autoskip navigate))
                     (:open "\\bigl(" :close "\\bigr)" :actions (insert wrap autoskip navigate))
                     (:open "[" :close "]" :actions (insert wrap autoskip navigate))
                     (:open "``" :close "''" :trigger "\"" :actions (insert wrap autoskip navigate))
                     (:open "`" :close "'" :actions (insert wrap autoskip navigate))))))
    (sp-test-wrapping-latex "|fooM" "`" "`|foo'")
    (sp-test-wrapping-latex "|fooM" "``" "``|foo''")

    (sp-test-wrapping-latex "Mfoo|" "`" "`foo'|")
    (sp-test-wrapping-latex "Mfoo|" "``" "``foo''|")

    (sp-test-wrapping-latex "Mfoo|" "\"" "``foo''|")
    (sp-test-wrapping-latex "|fooM" "\"" "``|foo''")))

(defun sp-test-wrapping-python (initial keys result)
  (sp-test-with-temp-buffer initial
      (shut-up (python-mode))
    (-each (-list keys) 'execute-kbd-macro)
    (should (equal (buffer-string) (replace-regexp-in-string "[|]" "" result)))
    (should (= (1+ (string-match-p "|" result)) (point)))))

(defvar sp-test-wrap-python-pairs
  '((python-mode
     (:open "\"" :close "\"" :actions (insert wrap autoskip navigate) :post-handlers (sp-escape-wrapped-region))
     (:open "'" :close "'" :actions (insert wrap autoskip navigate) :post-handlers (sp-escape-wrapped-region))
     (:open "(" :close ")" :actions (insert wrap autoskip navigate))
     (:open "[" :close "]" :actions (insert wrap autoskip navigate)))))

(ert-deftest sp-test-wrap-python-same nil
  (let ((sp-pairs sp-test-wrap-python-pairs))
    ;; wrap word with the same quote as enclosing
    (sp-test-wrapping-python "'foo |barM baz'"
                             "'"
                             "'foo \\'|bar\\' baz'")
    (sp-test-wrapping-python "'foo Mbar| baz'"
                             "'"
                             "'foo \\'bar\\'| baz'")))

(ert-deftest sp-test-wrap-python-opposite nil
  (let ((sp-pairs sp-test-wrap-python-pairs))
    ;; wrap word with opposing quote as enclosing
    (sp-test-wrapping-python "'foo |barM baz'"
                             "\""
                             "'foo \"|bar\" baz'")
    (sp-test-wrapping-python "'foo Mbar| baz'"
                             "\""
                             "'foo \"bar\"| baz'")))


(ert-deftest sp-test-wrap-python-same-same nil
  (let ((sp-pairs sp-test-wrap-python-pairs))
    ;; wrap same quote with same as enclosing
    (sp-test-wrapping-python "'foo |bar \\' bazM qux'"
                             "'"
                             "'foo \\'|bar \\\\\\' baz\\' qux'")
    (sp-test-wrapping-python "'foo Mbar \\' baz| qux'"
                             "'"
                             "'foo \\'bar \\\\\\' baz\\'| qux'")))

(ert-deftest sp-test-wrap-python-same-opposite nil
  (let ((sp-pairs sp-test-wrap-python-pairs))
    ;; wrap same quote with opposing as enclosing
    (sp-test-wrapping-python "'foo |bar \\' bazM qux'"
                             "\""
                             "'foo \"|bar \\' baz\" qux'")
    (sp-test-wrapping-python "'foo Mbar \\' baz| qux'"
                             "\""
                             "'foo \"bar \\' baz\"| qux'")))

(ert-deftest sp-test-wrap-python-opposite-same nil
  (let ((sp-pairs sp-test-wrap-python-pairs))
    ;; wrap opposing quote with same as enclosing
    (sp-test-wrapping-python "'foo |bar \" bazM qux'"
                             "'"
                             "'foo \\'|bar \" baz\\' qux'")
    (sp-test-wrapping-python "'foo Mbar \" baz| qux'"
                             "'"
                             "'foo \\'bar \" baz\\'| qux'")))

(ert-deftest sp-test-wrap-python-opposite-opposite nil
  (let ((sp-pairs sp-test-wrap-python-pairs))
    ;; wrap opposing quote with opposing as enclosing
    (sp-test-wrapping-python "'foo |bar \" bazM qux'"
                             "\""
                             "'foo \"|bar \\\" baz\" qux'")
    (sp-test-wrapping-python "'foo Mbar \" baz| qux'"
                             "\""
                             "'foo \"bar \\\" baz\"| qux'")
    ))

(ert-deftest sp-test-wrap-python-escaped-opposite-same nil
  (let ((sp-pairs sp-test-wrap-python-pairs))
    ;; wrap escaped opposing quote with same as enclosing
    (sp-test-wrapping-python "'foo |bar \\\" bazM qux'"
                             "'"
                             "'foo \\'|bar \\\\\" baz\\' qux'")
    (sp-test-wrapping-python "'foo Mbar \\\" baz| qux'"
                             "'"
                             "'foo \\'bar \\\\\" baz\\'| qux'")))

(ert-deftest sp-test-wrap-python-escaped-opposite-opposite nil
  (let ((sp-pairs sp-test-wrap-python-pairs))
    ;; wrap escaped opposing quote with opposing as enclosing
    (sp-test-wrapping-python "'foo |bar \\\" bazM qux'"
                             "\""
                             "'foo \"|bar \\\\\" baz\" qux'")

    (sp-test-wrapping-python "'foo Mbar \\\" baz| qux'"
                             "\""
                             "'foo \"bar \\\\\" baz\"| qux'")))
