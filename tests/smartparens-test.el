(require 'ert)
(require 'dash)

(require 'smartparens)
(require 'smartparens-test-env)
(require 'smartparens-test-get-paired-expression)
(require 'smartparens-test-get-stringlike-expression)
(require 'smartparens-test-ruby-mode)
(require 'smartparens-test-commands)

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

(defun sp-test-run-tests ()
  (interactive)
  (ert "sp-test-*"))

;; Local Variables:
;; eval: (push (file-name-directory (buffer-file-name)) load-path)
;; End:
