(require 'smartparens)

(ert-deftest sp-test-buffer-modified-sp-skip-closing-pair ()
  "Test the correct setting of `buffer-modified-p' flag after
executing `sp-skip-closing-pair'."
  (with-temp-buffer
    (insert "foobar")
    (should (eq (buffer-modified-p) t))
    (goto-char (point-min))
    (insert "(")
    (goto-char (point-max))
    (insert ")")
    (backward-char 1)
    (sp-skip-closing-pair ")")
    (should (eq (buffer-modified-p) t))
    (backward-char 1)
    (set-buffer-modified-p nil)
    (sp-skip-closing-pair ")")
    (should (eq (buffer-modified-p) nil))
    (let ((sp-autoskip-closing-pair 'always))
      (goto-char 3)
      (sp-skip-closing-pair ")")
      (should (eq (buffer-modified-p) nil))
      (goto-char 3)
      (insert "a")
      (sp-skip-closing-pair ")")
      (should (eq (buffer-modified-p) t)))))

(ert-deftest sp-test-escaped-pair-is-not-skipped-in-string ()
  (sp-test-with-temp-elisp-buffer "\"\\|\""
    (execute-kbd-macro "\"")
    (insert "|")
    (should (equal (buffer-string) "\"\\\"|\\\"\""))))

(ert-deftest sp-test-non-escaped-pair-is-skipped-in-string ()
  (sp-test-with-temp-elisp-buffer "\"\\t foo | bar\""
    (execute-kbd-macro "[]")
    (insert "|")
    (should (equal (buffer-string) "\"\\t foo []| bar\""))))
