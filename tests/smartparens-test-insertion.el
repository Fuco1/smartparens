(require 'dash)
(require 'smartparens-config)

(defun sp-test-insertion (initial keys result)
  (save-window-excursion
    (with-temp-buffer
      (emacs-lisp-mode)
      (smartparens-mode 1)
      (pop-to-buffer (current-buffer))
      (insert initial)
      (goto-char (point-min))
      (search-forward "|")
      (delete-char -1)
      (-each keys 'execute-kbd-macro)
      (should (equal (buffer-string) result)))))

(ert-deftest sp-test-basic-insertion nil
  (sp-test-insertion "|" '("(" "(") "(())")
  (sp-test-insertion "|" '("(" "(" ")" "(") "(()())")
  (sp-test-insertion "|" '("[" "[") "[[]]")
  (sp-test-insertion "|" '("[" "[" "]" "[") "[[][]]"))

(provide 'smartparens-test-insertion)
