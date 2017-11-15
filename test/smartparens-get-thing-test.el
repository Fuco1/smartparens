;; Get thing tests
(shut-up (load "auctex-autoloads"))

(defun sp-test-thing-parse-in-latex (initial result)
  (sp-test-with-temp-buffer initial
      (latex-mode)
    (should (equal (sp-get-thing) result))))

(ert-deftest sp-test-get-thing-latex nil
  (let ((sp-sexp-prefix '((latex-mode syntax "\\/")))
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

(require 'clojure-mode)

(defun sp-test-thing-parse-in-clojure (initial result &optional back)
  (sp-test-with-temp-buffer initial
      (clojure-mode)
    (should (equal (sp-get-thing back) result))))

;; #699
(ert-deftest sp-test-get-thing-clojure-with-regexp-based-prefix nil
  "When the point is inside a prefix which is not a syntactic
prefix and we try to skip to previous symbol, the prefix might
stop the skip routine and prevent the previous token from being
picked up, causing `sp-get-thing' to take the 2nd previous one."
  (let ((sp-sexp-prefix '((clojure-mode regexp "#")))
        (sp-pairs '((t . ((:open "(" :close ")" :actions (insert wrap autoskip navigate))
                          (:open "{" :close "}" :actions (insert wrap autoskip navigate)))))))
    (sp-test-thing-parse-in-clojure "(atom #|{})" '(:beg 2 :end 6 :op "" :cl "" :prefix "" :suffix "") t)))

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

;; #811
(ert-deftest sp-test-get-thing-string-with-prefix-syntax-before-prefix ()
  (sp-test-with-temp-elisp-buffer "symbol |,\"string\""
    (sp-get (sp-get-thing)
      (should (equal :prefix ","))
      (should (equal :op "\"")))))

;; #811
(ert-deftest sp-test-get-thing-string-with-prefix-syntax-after-prefix ()
  (sp-test-with-temp-elisp-buffer "symbol ,|\"string\""
    (sp-get (sp-get-thing)
      (should (equal :prefix ","))
      (should (equal :op "\"")))))

;; #811
(ert-deftest sp-test-get-thing-string-with-prefix-regexp-before-prefix ()
  (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "P"))))
    (sp-test-with-temp-elisp-buffer "symbol |P\"foo\""
      (sp-get (sp-get-thing)
        (should (equal :prefix "P"))
        (should (equal :op "\""))))))

;; #811
(ert-deftest sp-test-get-thing-string-with-prefix-regexp-after-prefix ()
  (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "P"))))
    (sp-test-with-temp-elisp-buffer "symbol P|\"foo\""
      (sp-get (sp-get-thing)
        (should (equal :prefix "P"))
        (should (equal :op "\""))))))

;; #812
(ert-deftest sp-test-get-thing-symbol-with-prefix-syntax-before-prefix ()
  (let ((sp-sexp-prefix nil))
    (sp-test-with-temp-elisp-buffer "|?foo"
      (with-syntax-table (make-syntax-table)
        (modify-syntax-entry ?? "_ p")
        (sp-get (sp-get-thing)
          (should (equal :beg 2))
          (should (equal :prefix "?")))))))

;; #812
(ert-deftest sp-test-get-thing-symbol-with-prefix-syntax-after-prefix ()
  (let ((sp-sexp-prefix nil))
    (sp-test-with-temp-elisp-buffer "?|foo"
      (with-syntax-table (make-syntax-table)
        (modify-syntax-entry ?? "_ p")
        (sp-get (sp-get-thing)
          (should (equal :beg 2))
          (should (equal :prefix "?")))))))

;; #812
(ert-deftest sp-test-symbol-backward-should-skip-the-prefix ()
  (let ((sp-sexp-prefix nil))
    (sp-test-with-temp-elisp-buffer "?foo|"
      (with-syntax-table (make-syntax-table)
        (modify-syntax-entry ?? "_ p")
        (sp-backward-symbol 1)
        (sp-buffer-equals "?|foo")))))

;; #813
(ert-deftest sp-test-skip-backward-to-symbol-do-not-use-prefix-at-closing ()
  (let ((sp-sexp-prefix '((emacs-lisp-mode regexp "P"))))
    (sp-test-with-temp-elisp-buffer "(foo«P|)"
      (sp-get (sp-get-symbol t)
        (should (equal :beg 6))
        (should (equal :end 7))))))

;; #813
(ert-deftest sp-test-skip-backward-to-symbol-do-not-use-suffix-at-opening ()
  (let ((sp-sexp-suffix '((emacs-lisp-mode regexp "P"))))
    (sp-test-with-temp-elisp-buffer "(|P«foo)"
      (sp-get (sp-get-symbol)
        (should (equal :beg 2))
        (should (equal :end 3))))))
