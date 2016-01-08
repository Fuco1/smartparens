(defun sp-test--paired-expression-parse-in-c (initial result &optional back)
  (sp-test-with-temp-buffer initial
      (c-mode)
    (should (equal (sp-get-paired-expression back) result))))

(ert-deftest sp-test-paired-expression-parse-in-c nil
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))
                          (:open "{" :close "}" :actions (insert wrap autoskip navigate))
                          (:open "[" :close "]" :actions (insert wrap autoskip navigate))
                          (:open "/*" :close "*/" :actions (insert wrap autoskip navigate)))))))
    (sp-test--paired-expression-parse-in-c "asd |/* adasdad */" '(:beg 5 :end 18 :op "/*" :cl "*/" :prefix "" :suffix ""))
    (sp-test--paired-expression-parse-in-c "asd /* adasdad */|" '(:beg 5 :end 18 :op "/*" :cl "*/" :prefix "" :suffix "") t)
    ))
