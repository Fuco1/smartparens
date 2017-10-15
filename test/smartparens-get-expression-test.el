(defun sp-test--get-expression-skip-before-arrow (ms mb me)
  (save-excursion
    (goto-char me)
    (looking-at-p "<")))

(ert-deftest sp-test-get-expression-string-when-there-is-ignored-regular-before-it-and-valid-after-it ()
  "In case there is a non-string delimiter before a
string-delimiter but it turns out it is invalid and we return a
regular expression which starts after the string one, try to
reparse the string expression because it might be valid and
closer.

  foo | [<-thisisignored \"bar\" [we got this instead of the string]"
  (let ((sp-pairs '((t . ((:open "\"" :close "\"" :actions (insert wrap autoskip navigate))
                          (:open "[" :close "]" :actions (insert wrap autoskip navigate)
                           :skip-match sp-test--get-expression-skip-before-arrow))))))
    (sp-test-with-temp-elisp-buffer "foo | [<-thisisignored \"bar\" [we got this instead of the string]"
      (sp-get (sp-get-expression)
        (should (equal :op "\""))
        (should (equal :beg 23))))))

(ert-deftest sp-test-get-expression-regular-when-there-is-ignored-string-before-it-and-valid-after-it ()
  "In case there is a string delimiter before a
non-string-delimiter but it turns out it is invalid and we return
a string expression which starts after the regular one, try to
reparse the regular expression because it might be valid and
closer.

  foo | '<-thisisignored [brackets] `we got this instead of the brackets'"
  (let ((sp-pairs '((t . ((:open "'" :close "'" :actions (insert wrap autoskip navigate)
                           :skip-match sp-test--get-expression-skip-before-arrow)
                          (:open "[" :close "]" :actions (insert wrap autoskip navigate)))))))
    (sp-test-with-temp-buffer "foo | '<-thisisignored [brackets] 'we got this instead of the brackets'"
        (fundamental-mode)
      (sp-get (sp-get-expression)
        (should (equal :op "["))
        (should (equal :beg 23))))))

(ert-deftest sp-test-get-expression-regular-when-there-is-ignored-string-before-it-and-no-after-it ()
  "If the first delimiter we've found was skipped and then there
were no more string delimiters any following regular pair was
ignored as well.

We should only consider valid delimiters to start parsing."
  (let ((sp-pairs '((t . ((:open "'" :close "'" :actions (insert wrap autoskip navigate)
                           :skip-match sp-test--get-expression-skip-before-arrow)
                          (:open "[" :close "]" :actions (insert wrap autoskip navigate)))))))
    (sp-test-with-temp-buffer "skip this '< and [pick up this]"
        (fundamental-mode)
      (sp-get (sp-get-expression)
        (should (equal :op "["))))))

(ert-deftest sp-test-get-expression-regular-when-there-are-multiple-ignored-strings-before-it-and-no-after-it ()
  "If the first delimiter we've found was skipped and then there
were no more string delimiters any following regular pair was
ignored as well.

We should only consider valid delimiters to start parsing."
  (let ((sp-pairs '((t . ((:open "'" :close "'" :actions (insert wrap autoskip navigate)
                           :skip-match sp-test--get-expression-skip-before-arrow)
                          (:open "*" :close "*" :actions (insert wrap autoskip navigate)
                           :skip-match sp-test--get-expression-skip-before-arrow)
                          (:open "[" :close "]" :actions (insert wrap autoskip navigate)))))))
    (sp-test-with-temp-buffer "skip this '< and also *< this and [pick up this]"
        (fundamental-mode)
      (sp-get (sp-get-expression)
        (should (equal :op "["))))))
