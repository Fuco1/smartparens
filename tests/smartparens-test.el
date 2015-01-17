(require 'ert)
(require 'dash)
(require 'cl-lib)

(require 'smartparens)
(require 'smartparens-test-env)
(require 'smartparens-test-insertion)
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

(ert-deftest sp-test-insertion-specification-parser ()
  (should (equal (sp--parse-insertion-spec "ab")
                 '(progn (insert "ab"))))
  (should (equal (sp--parse-insertion-spec "a|b")
                 '(progn
                    (insert "a")
                    (save-excursion
                      (insert "b")))))
  (should (equal (sp--parse-insertion-spec "a\\|b")
                 '(progn (insert "a|") (insert "b"))))
  (should (equal (sp--parse-insertion-spec "a\\||b")
                 '(progn
                    (insert "a|")
                    (save-excursion
                      (insert "b")))))
  (should (equal (sp--parse-insertion-spec "a\\[b]")
                 '(progn (insert "a[") (insert "b]"))))
  (should (equal (sp--parse-insertion-spec "a\\[b[i]")
                 '(progn
                    (insert "a[")
                    (insert "b")
                    (indent-according-to-mode))))
  (should (equal (sp--parse-insertion-spec "a||b")
                 '(progn
                    (insert "a")
                    (save-excursion
                      (insert "b"))
                    (indent-according-to-mode))))
  (should (equal (sp--parse-insertion-spec "a|[i]b")
                 '(progn
                    (insert "a")
                    (save-excursion
                      (indent-according-to-mode)
                      (insert "b")))))
  (should (equal (sp--parse-insertion-spec "a|b[i]")
                 '(progn
                    (insert "a")
                    (save-excursion
                      (insert "b")
                      (indent-according-to-mode)))))
  (should (equal (sp--parse-insertion-spec "[i]a|b")
                 '(progn
                    (indent-according-to-mode)
                    (insert "a")
                    (save-excursion
                      (insert "b")))))
  (should (equal (sp--parse-insertion-spec "[i]")
                 '(progn
                    (indent-according-to-mode)))))

(ert-deftest sp-test-sp-autoescape-string-quote-if-empty ()
  (let ((python-indent-offset 4))
    (with-temp-buffer
      (python-mode)
      (smartparens-mode 1)
      (insert "def foo():\n    ")
      (pop-to-buffer (current-buffer))
      (let ((sp-autoescape-string-quote-if-empty '(python-mode))
            (sp-autoescape-string-quote t)
            (sp-autoskip-closing-pair 'always)
            (sp-undo-pairs-separately nil))
        (execute-kbd-macro "\"\"\""))
      (should (equal (buffer-string) "def foo():\n    \"\"\"\"\"\"")))))

(defun sp-test--string-valid-p (str)
  (with-temp-buffer
    (insert str)
    (sp-region-ok-p (point-min) (point-max))))

(ert-deftest sp-test-region-ok-unbalanced-paren ()
  (should-not (sp-test--string-valid-p "foo)")))

(ert-deftest sp-test-region-ok-unbalanced-string ()
  (should-not (sp-test--string-valid-p "foo\"")))

(ert-deftest sp-test-region-ok-balanced-string ()
  (should (sp-test--string-valid-p "\"foo\"")))

(ert-deftest sp-test-region-ok-balanced-parens ()
  (should (sp-test--string-valid-p "(foo)")))

(defun sp-test-run-tests ()
  (interactive)
  (ert "sp-test-*"))

;; Local Variables:
;; eval: (push (file-name-directory (buffer-file-name)) load-path)
;; End:
