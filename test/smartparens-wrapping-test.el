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


(ert-deftest sp-test-wrap-with-closing nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-wrapping "|aM" "]" "[a]|")
    (sp-test-wrapping "Ma|" "]" "[a]|")
    (sp-test-wrapping "|aM" "\\}" "\\{a\\}|")))

(ert-deftest sp-test-wrap-repeated nil
  (let ((sp-pairs sp--test-basic-pairs))
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
