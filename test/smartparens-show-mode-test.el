(require 'smartparens)
(require 'evil)

(ert-deftest sp-test-show-mode-point-at-nonpairable-stringlike-delimiter-textmode ()
  (let ((sp-pairs '((t . ((:open "\"" :close "\"" :actions (insert wrap autoskip navigate))
                          (:open "'" :close "'" :actions (insert wrap autoskip navigate))))))
        (sp-show-pair-overlays nil))
    (unwind-protect
        (sp-test-with-temp-buffer "\"asdasd'| asdasd asd\""
            (text-mode)
          (show-smartparens-mode 1)
          (sp-show--pair-function)
          (should (eq sp-show-pair-overlays nil)))
      (sp-show--pair-delete-overlays))))

(ert-deftest sp-test-show-mode-point-at-beg-of-sexp ()
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))))))
        (sp-show-pair-overlays nil))
    (unwind-protect
        (sp-test-with-temp-elisp-buffer "|(foo bar)"
          (show-smartparens-mode 1)
          (sp-show--pair-function)
          (should (not (eq sp-show-pair-overlays nil))))
      (sp-show--pair-delete-overlays))))

(ert-deftest sp-test-show-mode-point-at-end-of-sexp ()
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))))))
        (sp-show-pair-overlays nil))
    (unwind-protect
        (sp-test-with-temp-elisp-buffer "(foo bar)|"
          (show-smartparens-mode 1)
          (sp-show--pair-function)
          (should (not (eq sp-show-pair-overlays nil))))
      (sp-show--pair-delete-overlays))))

(ert-deftest sp-test-show-mode-point-at-beg-in-of-sexp-from-inside-t ()
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))))))
        (sp-show-pair-overlays nil)
        (sp-show-pair-from-inside t))
    (unwind-protect
        (sp-test-with-temp-elisp-buffer "(|foo bar)"
          (show-smartparens-mode 1)
          (sp-show--pair-function)
          (should (not (eq sp-show-pair-overlays nil))))
      (sp-show--pair-delete-overlays))))

(ert-deftest sp-test-show-mode-point-at-end-in-sexp-from-inside-t ()
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))))))
        (sp-show-pair-overlays nil)
        (sp-show-pair-from-inside t))
    (unwind-protect
        (sp-test-with-temp-elisp-buffer "(foo bar|)"
          (show-smartparens-mode 1)
          (sp-show--pair-function)
          (should (not (eq sp-show-pair-overlays nil))))
      (sp-show--pair-delete-overlays))))

(ert-deftest sp-test-show-mode-point-at-beg-in-of-sexp-from-inside-nil ()
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))))))
        (sp-show-pair-overlays nil)
        (sp-show-pair-from-inside nil))
    (unwind-protect
        (sp-test-with-temp-elisp-buffer "(|foo bar)"
          (show-smartparens-mode 1)
          (sp-show--pair-function)
          (should (eq sp-show-pair-overlays nil)))
      (sp-show--pair-delete-overlays))))

(ert-deftest sp-test-show-mode-point-at-end-in-sexp-from-inside-nil ()
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))))))
        (sp-show-pair-overlays nil)
        (sp-show-pair-from-inside nil))
    (unwind-protect
        (sp-test-with-temp-elisp-buffer "(foo bar|)"
          (show-smartparens-mode 1)
          (sp-show--pair-function)
          (should (eq sp-show-pair-overlays nil)))
      (sp-show--pair-delete-overlays))))

(ert-deftest sp-test-show-mode-point-at-end-in-sexp-evil ()
  (let ((sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))))))
        (sp-show-pair-overlays nil))
    (unwind-protect
        (sp-test-with-temp-elisp-buffer "(foo bar|)"
          (evil-local-mode)
          (show-smartparens-mode 1)
          (sp-show--pair-function)
          (should (not (eq sp-show-pair-overlays nil))))
      (sp-show--pair-delete-overlays))))
