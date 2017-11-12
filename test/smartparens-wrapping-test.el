(defun sp-test-wrapping (initial keys result)
  (sp-test-with-temp-elisp-buffer initial
    (-each (-list keys) 'execute-kbd-macro)
    (sp-buffer-equals result)))

(ert-deftest sp-test-wrap-basic nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-wrapping "|aM" "(" "(|aM)")
    (sp-test-wrapping "Ma|" "(" "M(a)|")

    (sp-test-wrapping "|aM" "[" "[|a]")
    (sp-test-wrapping "Ma|" "[" "[a]|")

    (sp-test-wrapping "|aM" "\\{" "\\{|a\\}")
    (sp-test-wrapping "Ma|" "\\{" "\\{a\\}|")

    (sp-test-wrapping "|aM" "\\\"" "\\\"|a\\\"")
    (sp-test-wrapping "Ma|" "\\\"" "\\\"a\\\"|")

    (sp-test-wrapping "|aM" "\\langle" "\\langle|a\\rangle")
    (sp-test-wrapping "Ma|" "\\langle" "\\langlea\\rangle|")))

(ert-deftest sp-test-wrap-basic-respect-direction nil
  (let ((sp-pairs sp--test-basic-pairs)
        (sp-wrap-respect-direction t))
    (sp-test-wrapping "|aM" "(" "|(a)M")
    (sp-test-wrapping "Ma|" "(" "|(a)M")))

(ert-deftest sp-test-wrap-basic-with-quotes nil
  (let ((sp-pairs '((t (:open "\""  :close "\""
                        :actions (insert wrap autoskip navigate escape)
                        :post-handlers (sp-escape-wrapped-region
                                        sp-escape-quotes-after-insert))))))
    ;; #212
    (sp-test-wrapping "\"C-M[|\"" "\"" "\"C-\\\"[\\\"|\"")
    (sp-test-wrapping "\"C-|[M\"" "\"" "\"C-\\\"|[\\\"\"")))

(ert-deftest sp-test-wrap-with-trigger nil
  (let ((sp-pairs '((t (:open "\\(" :close "\\)" :trigger-wrap "$"
                        :actions (insert wrap autoskip navigate escape))
                       (:open "$" :close "$"
                        :actions (insert wrap autoskip navigate escape))
                       (:open "[" :close "]"
                        :actions (insert wrap autoskip navigate escape))))))
    ;; #663
    (sp-test-wrapping "|foobarM" "$" "\\(|foobar\\)")
    (sp-test-wrapping "Mfoobar|" "$" "\\(foobar\\)|")))

(ert-deftest sp-test-wrap-unbalanced-region nil
  (shut-up
    (let ((sp-pairs sp--test-basic-pairs))
      (sp-test-wrapping "[a b |c] dM e" "(" "[a b |c] d e")
      (sp-test-wrapping "[a b Mc] d| e" "(" "[a b c] d| e")
      (sp-test-wrapping "[a b |c] d [e Mf]" "(" "[a b |c] d [e f]")
      (sp-test-wrapping "[a b Mc] d [e |f]" "(" "[a b c] d [e |f]")

      ;; wrapping with quotes
      (sp-test-wrapping "[a b |c] dM e" "\"" "[a b |c] d e")
      (sp-test-wrapping "[a b Mc] d| e" "\"" "[a b c] d| e")

      (sp-test-wrapping "\"a b |c\" dM e" "\"" "\"a b |c\" d e")
      (sp-test-wrapping "\"a b Mc\" d| e" "\"" "\"a b c\" d| e")
      (sp-test-wrapping "[foo |bar] baz \"quMx\"" "\"" "[foo |bar] baz \"qux\"")
      (sp-test-wrapping "[foo Mbar] baz \"qu|x\"" "\"" "[foo bar] baz \"qu|x\"")

      (sp-test-wrapping "\"fo|o\" asd \"baMz\"" "[" "\"fo|o\" asd \"baz\"")
      (sp-test-wrapping "\"foMo\" asd \"ba|z\"" "[" "\"foo\" asd \"ba|z\""))))

(ert-deftest sp-test-wrap-with-closing nil
  (let ((sp-pairs sp--test-basic-pairs))
    (sp-test-wrapping "|aM" "]" "M[a]|")
    (sp-test-wrapping "Ma|" "]" "M[a]|")
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

(ert-deftest sp-test-wrap-in-delete-selection-mode nil
  (sp-test-with-delete-selection-mode
    (sp-test-with-temp-elisp-buffer "|fooM"
      ;; Inserting a character that pairs should wrap instead of
      ;; replacing the selection.
      (execute-kbd-macro "(")
      (sp-buffer-equals "(|fooM)"))))

(ert-deftest sp-test-delete-selection-mode-still-works nil
  "Test that `delete-selection-pre-hook' still works despite our advice on it."
  (sp-test-with-delete-selection-mode
    (sp-test-with-temp-elisp-buffer "|fooM"
      ;; Inserting a character that does not pair should replace the
      ;; selection when delete-selection-mode is on.
      (execute-kbd-macro "x")
      (sp-buffer-equals "x|"))))

;; #763
(ert-deftest sp-test-delete-selection-mode-after-turning-sp-off nil
  "Don't inhibit `delete-selection-mode' after smartparens is turned off."
  (sp-test-with-delete-selection-mode
    (sp-test-with-temp-elisp-buffer "|fooM"
      (smartparens-mode -1)
      (execute-kbd-macro "(")
      (sp-buffer-equals "(|"))))

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
     (:open "\"" :close "\"" :actions (insert wrap autoskip navigate escape) :post-handlers (sp-escape-wrapped-region))
     (:open "'" :close "'" :actions (insert wrap autoskip navigate escape) :post-handlers (sp-escape-wrapped-region))
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

(ert-deftest sp-test-wrap-autodelete-wrap nil
  "Delete the last wrapping pair if DEL is pressed immediately
  after wrapping."
  (let ((sp-autodelete-wrap t))
    (sp-test-with-temp-elisp-buffer "|fooM"
      (execute-kbd-macro (kbd "( DEL"))
      (sp-buffer-equals "|foo"))))

(ert-deftest sp-test-wrap-autodelete-wrap-strict-mode nil
  "Delete the last wrapping pair if DEL is pressed immediately
  after wrapping."
  (let ((sp-autodelete-wrap t))
    (sp-test-with-temp-elisp-buffer "|fooM"
      (smartparens-strict-mode 1)
      (execute-kbd-macro (kbd "( DEL"))
      (sp-buffer-equals "|foo"))))
