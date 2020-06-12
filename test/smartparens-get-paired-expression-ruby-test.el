(defun sp-test--paired-expression-parse-in-ruby (initial result &optional back)
  (let ((sp-pairs '((t . ((:open "def"   :close "end" :actions (insert wrap autoskip navigate))
                          (:open "if"    :close "end" :actions (insert wrap autoskip navigate))
                          (:open "do"    :close "end" :actions (insert wrap autoskip navigate))
                          (:open "begin" :close "end" :actions (insert wrap autoskip navigate))
                          (:open "(" :close ")" :actions (insert wrap autoskip navigate)))))))
    (sp-test-with-temp-buffer initial
        (ruby-mode)
      (should (equal (sp-get-paired-expression back) result)))))

(ert-deftest sp-test-get-paired-expression-ruby ()
  (sp-test--paired-expression-parse-in-ruby "|begin end" '(:beg 1 :end 10 :op "begin" :cl "end" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-ruby "begin |end" '(:beg 1 :end 10 :op "begin" :cl "end" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-ruby "|def foo bar if blaz end end" '(:beg 1 :end 28 :op "def" :cl "end" :prefix "" :suffix ""))
  (sp-test--paired-expression-parse-in-ruby "|def foo end;" '(:beg 1 :end 12 :op "def" :cl "end" :prefix "" :suffix ""))
  )

(ert-deftest sp-test-get-paired-expression-ruby-backward ()
  (sp-test--paired-expression-parse-in-ruby "begin end|" '(:beg 1 :end 10 :op "begin" :cl "end" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-ruby "begin| end" '(:beg 1 :end 10 :op "begin" :cl "end" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-ruby "def foo bar if blaz end end|" '(:beg 1 :end 28 :op "def" :cl "end" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-ruby "def foo end;|" '(:beg 1 :end 12 :op "def" :cl "end" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-ruby "asd (asd)|#asdas" '(:beg 5 :end 10 :op "(" :cl ")" :prefix "" :suffix "") t)
  (sp-test--paired-expression-parse-in-ruby "C = %w(asd)|#asdas" '(:beg 7 :end 12 :op "(" :cl ")" :prefix "" :suffix "") t)
  )

(ert-deftest sp-test-get-paired-expression-ruby-fail ()
  (sp-test--paired-expression-parse-in-ruby "|def en" nil)
  (sp-test--paired-expression-parse-in-ruby "|do do end do" nil)
  )

(ert-deftest sp-test-get-paired-expression-ruby-backward-fail ()
  (sp-test--paired-expression-parse-in-ruby "de end|" nil t)
  )

(defun sp-test--thing-parse-in-ruby (initial result &optional back)
  (let ((sp-pairs '((t . ((:open "def"   :close "end" :actions (insert wrap autoskip navigate))
                          (:open "if"    :close "end" :actions (insert wrap autoskip navigate))
                          (:open "do"    :close "end" :actions (insert wrap autoskip navigate))
                          (:open "begin" :close "end" :actions (insert wrap autoskip navigate))
                          (:open "(" :close ")" :actions (insert wrap autoskip navigate)))))))
    (sp-test-with-temp-buffer initial
        (ruby-mode)
      (should (equal (sp-get-thing back) result)))))

(ert-deftest sp-test-get-thing-generic-string-ruby ()
  (sp-test--thing-parse-in-ruby "C = |%w(asd)#asdas"
				'(:beg 5 :end 12 :op "%" :cl ")" :prefix "" :suffix ""))
  (sp-test--thing-parse-in-ruby "C = %w(asd)|#asdas"
				'(:beg 5 :end 12 :op "%" :cl ")" :prefix "" :suffix "") t))
  
