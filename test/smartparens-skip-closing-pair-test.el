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

(ert-deftest sp-test-non-escaped-pair-is-skipped-in-string-c++ ()
  (sp-test-with-temp-buffer "std::string a = \"foo\\nbar\";\n\"|\";"
      (c++-mode)
    (execute-kbd-macro "[]|")
    (should (equal (buffer-string) "std::string a = \"foo\\nbar\";\n\"[]|\";"))))

(ert-deftest sp-test-non-escaped-pair-is-skipped-in-string-python ()
  (sp-test-with-temp-buffer "from sys import stdin, \\\n    stdout\n\na = \"|\""
      (c++-mode)
    (execute-kbd-macro "[]|")
    (should (equal (buffer-string) "from sys import stdin, \\\n    stdout\n\na = \"[]|\""))))

;; #421
;; If we are in a balanced context and hit a closing delimiter for
;; an autoskip pair which is not enclosing (so we would jump out of
;; the sexp) we should not insert it as it would just create an
;; imbalance.
(ert-deftest sp-test-strict-mode-inhibit-closing-delim-inside-sexp ()
  (sp-test-with-temp-elisp-buffer "(foo | bar)"
    (smartparens-strict-mode 1)
    (execute-kbd-macro "]")
    (sp-buffer-equals "(foo | bar)")))

;; However, if the pair is not autoskip, then we insert the closing
;; delimiter when typed, regardless of balance context.
(ert-deftest sp-test-strict-mode-dont-inhibit-closing-non-autoskip-delim ()
  (sp-test-with-temp-buffer "" nil
    (let ((sp-pairs sp-pairs))
      (smartparens-strict-mode)
      (sp-local-pair major-mode "{" "}" :actions '(insert autoskip navigate wrap))
      (execute-kbd-macro "}")
      (should (eq (char-before) nil))
      (sp-local-pair major-mode "{" "}" :actions '(:rem autoskip))
      (execute-kbd-macro "}")
      (should (eq (char-before) ?\})))))

(ert-deftest sp-test-strict-mode-inhibit-closing-delim-outside-sexp ()
  (sp-test-with-temp-elisp-buffer "(foo bar) | (bar baz)"
    (smartparens-strict-mode 1)
    (execute-kbd-macro "]")
    (sp-buffer-equals "(foo bar) | (bar baz)")))

(ert-deftest sp-test-strict-mode-insert-closing-delim-in-comment ()
  (sp-test-with-temp-elisp-buffer ";; (foo bar) | (bar baz)"
    (smartparens-strict-mode 1)
    (execute-kbd-macro "]")
    (sp-buffer-equals ";; (foo bar) ]| (bar baz)")))

(ert-deftest sp-test-strict-mode-insert-disallowed-pairs-normally ()
  (sp-test-with-temp-elisp-buffer ";; (foo bar) |foo (bar baz)"
    (smartparens-strict-mode 1)
    (execute-kbd-macro "'")
    (sp-buffer-equals ";; (foo bar) '|foo (bar baz)")))

(ert-deftest sp-test-non-strict-mode-insert-closing-delim ()
  "In non-strict mode, just insert whatever"
  (sp-test-with-temp-elisp-buffer "(foo | bar)"
    (execute-kbd-macro "]")
    (sp-buffer-equals "(foo ]| bar)")))
