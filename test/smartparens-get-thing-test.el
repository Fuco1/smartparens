;; Get thing tests
(shut-up (load "auctex-autoloads"))

(defun sp-test-thing-parse-in-latex (initial result)
  (sp-test-with-temp-buffer initial
      (latex-mode)
    (should (equal (sp-get-thing) result))))

(ert-deftest sp-test-get-thing-latex nil
  (let ((sp-navigate-consider-stringlike-sexp '(latex-mode))
        (sp-sexp-prefix '((latex-mode syntax "\\/")))
        (sp-pairs '((t . ((:open "$" :close "$" :actions (insert wrap autoskip navigate))
                          (:open "'" :close "'" :actions (insert wrap autoskip navigate))
                          (:open "[" :close "]" :actions (insert wrap autoskip navigate))
                          (:open "{" :close "}" :actions (insert wrap autoskip navigate) :prefix "\\\\\\(\\sw\\|\\s_\\)*"))))))
    (sp-test-thing-parse-in-latex "foo | \\bar{baz} qux" '(:beg 10 :end 15 :op "{" :cl "}" :prefix "\\bar" :suffix ""))
    (sp-test-thing-parse-in-latex "foo | \\bar {baz} qux" '(:beg 7 :end 10 :op "" :cl "" :prefix "\\" :suffix ""))
    ))

(require 'racket-mode)

(defun sp-test-thing-parse-in-racket (initial result)
  (sp-test-with-temp-buffer initial
      (racket-mode)
    (should (equal (sp-get-thing) result))))

(ert-deftest sp-test-get-thing-racket nil
  (let ((sp-sexp-prefix '((racket-mode regexp "#?['`,]@?")))
        (sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate)))))))
    (sp-test-thing-parse-in-racket "foo | #'(foo) qux" '(:beg 8 :end 13 :op "(" :cl ")" :prefix "#'" :suffix ""))))

;; 621
(ert-deftest sp-test-get-thing-with-prefix-when-inside-prefix-backward ()
  (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "\\(?:[_]+\\)"))))
    (sp-test-with-temp-elisp-buffer "(_|(abc))"
      (should (equal (sp-get-thing t)
                     '(:beg 1 :end 9 :op "(" :cl ")" :prefix "" :suffix ""))))))


(defun sp-test-get-thing-invalid (initial &optional back)
  (sp-test-with-temp-elisp-buffer initial
    (should-not (sp-get-thing back))))

(ert-deftest sp-test-get-thing-invalid-sexp ()
  (sp-test-get-thing-invalid "\"foo |\\\" bar\"")
  (sp-test-get-thing-invalid "\"foo \\\"| bar\"" t)

  (sp-test-get-thing-invalid "\"foo |[ bar\"")
  (sp-test-get-thing-invalid "\"foo ]| bar\"" t)

  (sp-test-get-thing-invalid "foo |[ bar")
  (sp-test-get-thing-invalid "foo ]| bar" t)

  (sp-test-get-thing-invalid "foo |[ bar (foo)")
  (sp-test-get-thing-invalid "(bar) foo ]| bar" t)

  (sp-test-get-thing-invalid "foo |[ bar \"foo\"")
  (sp-test-get-thing-invalid "\"bar\" foo ]| bar" t))

(ert-deftest sp-test-get-thing-with-skip-match-on-closing-delim ()
  (let ((sp-pairs '((t . ((:open "(" :close ")"
                           :actions (insert wrap autoskip navigate)
                           :skip-match (lambda (ms mb me)
                                         (save-excursion
                                           (goto-char mb)
                                           (sp--looking-back-p "skip" 4)))))))))
    (sp-test-with-temp-elisp-buffer "(fo skip|) o)"
      (should
       (equal (sp-get-thing)
              '(:beg 11 :end 12 :op "" :cl "" :prefix "" :suffix ""))))))
