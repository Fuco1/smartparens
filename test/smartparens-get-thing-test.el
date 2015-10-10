;; Get thing tests
(when (version< "24.3" emacs-version)
  (load "auctex-autoloads"))

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

(when (version<= "24.3" emacs-version)
  (require 'racket-mode)

  (defun sp-test-thing-parse-in-racket (initial result)
    (sp-test-with-temp-buffer initial
        (racket-mode)
      (should (equal (sp-get-thing) result))))

  (ert-deftest sp-test-get-thing-racket nil
    (let ((sp-sexp-prefix '((racket-mode regexp "#?['`,]@?")))
          (sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate)))))))
      (sp-test-thing-parse-in-racket "foo | #'(foo) qux" '(:beg 8 :end 13 :op "(" :cl ")" :prefix "#'" :suffix "")))))
