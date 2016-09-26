(ert-deftest sp-test-delayed-hook-should-trigger-on-RET ()
  (cl-letf (((symbol-function 'sp-test-insert-space)
             (lambda (&rest _)
               (insert " "))))
    (let ((sp-pairs '((emacs-lisp-mode
                       (:open "(" :close ")" :actions (insert wrap autoskip navigate)
                        :post-handlers ((sp-test-insert-space "RET")))))))
      (sp-test-with-temp-elisp-buffer ""
        (execute-kbd-macro "(")
        (execute-kbd-macro (kbd "RET"))
        (insert "|")
        (should (equal (buffer-string) "(
 |)"))))))

(ert-deftest sp-test-delayed-hook-should-not-trigger-on-a ()
  (cl-letf (((symbol-function 'sp-test-insert-space)
             (lambda (&rest _)
               (insert " "))))
    (let ((sp-pairs '((emacs-lisp-mode
                       (:open "(" :close ")" :actions (insert wrap autoskip navigate)
                        :post-handlers ((sp-test-insert-space "RET")))))))
      (sp-test-with-temp-elisp-buffer ""
        (execute-kbd-macro "(")
        (execute-kbd-macro (kbd "a"))
        (insert "|")
        (should (equal (buffer-string) "(a|)"))))))

(ert-deftest sp-test-delayed-hook-should-trigger-on-my/newline ()
  (cl-letf (((symbol-function 'my/test-newline)
             (lambda (&rest _)
               (interactive)
               (insert "\n")))
            ((symbol-function 'sp-test-insert-space)
             (lambda (&rest _)
               (insert " "))))
    (let ((sp-pairs '((emacs-lisp-mode
                       (:open "(" :close ")" :actions (insert wrap autoskip navigate)
                        :post-handlers ((sp-test-insert-space my/test-newline)))))))
      (sp-test-with-temp-elisp-buffer ""
        (execute-kbd-macro "(")
        (setq this-command 'my/test-newline)
        (call-interactively 'my/test-newline)
        (run-hooks 'post-command-hook)
        (insert "|")
        (should (equal (buffer-string) "(
 |)"))))))

(ert-deftest sp-test-delayed-hook-should-not-trigger-on-newline ()
  (cl-letf (((symbol-function 'my/test-newline)
             (lambda (&rest _)
               (interactive)
               (insert "\n")))
            ((symbol-function 'newline)
             (lambda (&rest _)
               (interactive)
               (insert "\n")))
            ((symbol-function 'sp-test-insert-space)
             (lambda (&rest _)
               (insert " "))))
    (let ((sp-pairs '((emacs-lisp-mode
                       (:open "(" :close ")" :actions (insert wrap autoskip navigate)
                        :post-handlers ((sp-test-insert-space my/test-newline)))))))
      (sp-test-with-temp-elisp-buffer ""
        (execute-kbd-macro "(")
        (setq this-command 'newline)
        (call-interactively 'newline)
        (run-hooks 'post-command-hook)
        (insert "|")
        (should (equal (buffer-string) "(
|)"))))))
