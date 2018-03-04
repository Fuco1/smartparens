;; #860
(ert-deftest sp-test-get-expression-pick-first-delimiter-in-same-context ()
  "When we are deciding on string or regular expression we simply
used to search for first delimiter after the current one and use
the closer valid one as the returned expression.

However, we need to preserve the original search context: if we
start in code the first valid delimiters we pick up must also be
in the code context, otherwise bad things happen.

In particular a situation where there is a string delimiter in
comment preceeding a regular delimiter in code context and we
start the search from a code context: we will find the first
valid expression after the comment but there is a string
delimiter which is closer so we will try that instead.  If there
is no string delimiter after the regular one or it is outside the
parent sexp this ends with an error.

Interestingly, I can not simulate the symatrical reversed
situation."
  (sp-test-with-temp-elisp-buffer "(co|nd
 ;; \"asd\"
 (foo))"
    (should
     (equal (sp-get-expression)
            (list :beg 18 :end 23 :op "(" :cl ")" :prefix "" :suffix "")))))

(ert-deftest sp-test-get-expression-pick-first-delimiter-in-same-context-with-string-after ()
  "See `sp-test-get-expression-pick-first-delimiter-in-same-context'."
  (sp-test-with-temp-elisp-buffer "(co|nd
 ;; \"asd\"
 (foo)
 \"bar\")"
    (should
     (equal (sp-get-expression)
            (list :beg 18 :end 23 :op "(" :cl ")" :prefix "" :suffix "")))))

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
