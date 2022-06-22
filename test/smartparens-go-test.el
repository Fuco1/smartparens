(require 'go-mode)
(require 'smartparens)

(defun sp-test--paired-expression-parse-in-go (initial result &optional back)
  (sp-test-with-temp-buffer initial
                            (go-mode)
                            (should (equal (sp-get-paired-expression back) result))))

(ert-deftest sp-test-paired-expression-parse-in-go nil
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))
                          (:open "{" :close "}" :actions (insert wrap autoskip navigate))
                          (:open "[" :close "]" :actions (insert wrap autoskip navigate))
                          (:open "/*" :close "*/" :actions (insert wrap autoskip navigate)))))))
    (sp-test--paired-expression-parse-in-go "asd |/* adasdad */" '(:beg 5 :end 18 :op "/*" :cl "*/" :prefix "" :suffix ""))
    (sp-test--paired-expression-parse-in-go "asd /* adasdad */|" '(:beg 5 :end 18 :op "/*" :cl "*/" :prefix "" :suffix "") t)))

(ert-deftest sp-test-go-slurp ()
  "Deleting a region containing a rust function definition."
  (sp-test-with-temp-buffer "(|foo).bar"
                            (go-mode)
                            (call-interactively 'sp-slurp-hybrid-sexp)
                            (should (equal (buffer-string) "(foo.bar)"))))
