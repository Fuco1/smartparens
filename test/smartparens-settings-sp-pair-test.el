(ert-deftest sp-test-add-global-pair ()
  (let ((sp-pairs nil))
    (sp-pair "(" ")")
    (should (equal sp-pairs '((t (:open "(" :close ")" :actions (wrap insert autoskip navigate))))))))

(ert-deftest sp-test-add-global-pair-with-a-when-condition ()
  (let ((sp-pairs nil))
    (sp-pair "(" ")" :when '(ignore))
    (should (equal sp-pairs '((t (:open "(" :close ")"
                                  :actions (wrap insert autoskip navigate)
                                  :when (ignore))))))))

(ert-deftest sp-test-remove-global-pair-when-condition ()
  (let ((sp-pairs '((t (:open "(" :close ")"
                        :actions (wrap insert autoskip navigate)
                        :when (ignore))))))
    (sp-pair "(" ")" :when nil)
    (should (equal sp-pairs '((t (:open "(" :close ")"
                                  :actions (wrap insert autoskip navigate)
                                  :when nil)))))))

(ert-deftest sp-test-replace-global-pair-actions ()
  (let ((sp-pairs '((t (:open "(" :close ")"
                        :actions (wrap insert autoskip navigate)
                        :when (ignore))))))
    (sp-pair "(" ")" :actions '(wrap insert))
    (should (equal sp-pairs '((t (:open "(" :close ")"
                                  :actions (wrap insert)
                                  :when nil)))))))

(ert-deftest sp-test-remove-global-pair-nonexisting-when-condition ()
  (let ((sp-pairs '((t (:open "(" :close ")"
                        :actions (wrap insert autoskip navigate))))))
    (sp-pair "(" ")" :when nil)
    (should (equal sp-pairs '((t (:open "(" :close ")"
                                  :actions (wrap insert autoskip navigate))))))))
