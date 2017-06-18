(require 'smartparens)

(ert-deftest sp-test-show-mode-point-at-nonpairable-stringlike-delimiter-textmode ()
  (let ((sp-pairs '((t . ((:open "\"" :close "\"" :actions (insert wrap autoskip navigate))
                          (:open "'" :close "'" :actions (insert wrap autoskip navigate)))))))
    (sp-test-with-temp-buffer "\"asdasd'| asdasd asd\""
        (text-mode)
      (show-smartparens-mode 1)
      (sp-show--pair-function)
      (should (eq sp-show-pair-overlays nil)))))
