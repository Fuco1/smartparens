;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic pairs

(defvar sp-test-get-stringlike-expression
  '(("foo \"bar\" baz \"quux\" bux" 1 5 10 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 5 5 10 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 6 5 10 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 8 5 10 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 9 5 10 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 10 15 21 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 12 15 21 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 15 15 21 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 16 15 21 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 18 15 21 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 20 15 21 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 1 5 12 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 5 5 12 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 10 5 12 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 13 17 25 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 17 17 25 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 22 17 25 "\"" "\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 1 5 12 "\\\"" "\\\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 5 5 12 "\\\"" "\\\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 7 5 12 "\\\"" "\\\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 13 21 29 "\\\"" "\\\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 21 21 29 "\\\"" "\\\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 25 21 29 "\\\"" "\\\"" "" "")
    ))

(defvar sp-test-get-stringlike-expression-backward
  '(("foo \"bar\" baz \"quux\" bux" 15 5 10 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 12 5 10 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 10 5 10 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 9 5 10 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 8 5 10 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 24 15 21 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 21 15 21 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 20 15 21 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 18 15 21 "\"" "\"" "" "")
    ("foo \"bar\" baz \"quux\" bux" 16 15 21 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 16 5 12 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 15 5 12 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 7 5 12 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 6 5 12 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 27 17 25 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 25 17 25 "\"" "\"" "" "")
    ("foo \"b\\\"ar\" baz \"q\\\"uux\" bux" 18 17 25 "\"" "\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 14 5 12 "\\\"" "\\\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 12 5 12 "\\\"" "\\\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 7 5 12 "\\\"" "\\\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 29 21 29 "\\\"" "\\\"" "" "")
    ("foo \\\"bar\\\" baz bam \\\"quux\\\"" 25 21 29 "\\\"" "\\\"" "" "")
    ))

(ert-deftest sp-test-get-stringlike-expression-backward ()
  (sp-test-setup-stringlike-expression-env-basic
    (--each sp-test-get-stringlike-expression-backward
      (sp-test-stringlike-sexp (car it) (apply 'sp-test-make-pair (cddr it)) (cadr it) t nil))))

(defvar sp-test-get-stringlike-expression-fail
  '(("foo \"bar\" baz\" boo" 11)
    ("foo \"bar " 1)
    ("foo \"b\\\"ar\"" 6)
    ))

(ert-deftest sp-test-get-stringlike-expression-fail ()
  (sp-test-setup-stringlike-expression-env-basic
    (--each sp-test-get-stringlike-expression-fail
      (sp-test-stringlike-sexp (car it) nil (cadr it) nil t))))

(defvar sp-test-get-stringlike-expression-backward-fail
  '(("foo \"bar baz" 7)
    ("foo \"bar \\\"baz\"" 13)
    ))

(ert-deftest sp-test-get-stringlike-expression-backward-fail ()
  (sp-test-setup-stringlike-expression-env-basic
    (--each sp-test-get-stringlike-expression-backward-fail
      (sp-test-stringlike-sexp (car it) nil (cadr it) t t))))

(defmacro sp-test-setup-stringlike-expression-env-basic (&rest forms)
  `(sp-test-setup-paired-expression-env
     sp--test-basic-pairs
     emacs-lisp-mode
     emacs-lisp-mode-hook
     ,@forms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; latex pairs

(defvar sp-test-get-stringlike-expression-latex
  '(("foo $bar$ baz $quux$ bux" 1 5 10 "$" "$" "" "")
    ("foo $bar$ baz $quux$ bux" 5 5 10 "$" "$" "" "")
    ("foo $bar$ baz $quux$ bux" 7 5 10 "$" "$" "" "")
    ("foo $bar$ baz $quux$ bux" 10 15 21 "$" "$" "" "")
    ("foo $bar$ baz $quux$ bux" 15 15 21 "$" "$" "" "")
    ("foo $bar$ baz $quux$ bux" 17 15 21 "$" "$" "" "")
    ("foo $b\\$ar$ baz $quux$ bux" 1 5 12 "$" "$" "" "")
    ("foo $b\\$ar$ baz $quux$ bux" 5 5 12 "$" "$" "" "")
    ("foo $b\\$ar$ baz $quux$ bux" 7 5 12 "$" "$" "" "")
    ))

(ert-deftest sp-test-get-stringlike-expression-latex ()
  "Test basic stringlike expressions in `latex-mode'."
  (sp-test-setup-stringlike-expression-env-latex
   (--each sp-test-get-stringlike-expression
     (when (sp-get-pair (nth 4 it))
       (sp-test-stringlike-sexp (car it) (apply 'sp-test-make-pair (cddr it)) (cadr it) nil nil)))
   (--each sp-test-get-stringlike-expression-latex
     (sp-test-stringlike-sexp (car it) (apply 'sp-test-make-pair (cddr it)) (cadr it) nil nil))))

(ert-deftest sp-test-get-textmode-stringlike-expression-latex ()
  "Test basic stringlike expressions in `latex-mode'."
  (sp-test-setup-stringlike-expression-env-latex
   (--each sp-test-get-stringlike-expression
     (when (sp-get-pair (nth 4 it))
       (sp-test-stringlike-sexp (car it) (apply 'sp-test-make-pair (cddr it)) (cadr it) nil nil)))
   (--each sp-test-get-stringlike-expression-latex
     (sp-test-textmode-stringlike-sexp (car it) (apply 'sp-test-make-pair (cddr it)) (cadr it) nil nil))))

(defvar sp-test-get-stringlike-expression-latex-backward
  '(("foo $bar$ baz $quux$ bux" 14 5 10 "$" "$" "" "")
    ("foo $bar$ baz $quux$ bux" 10 5 10 "$" "$" "" "")
    ("foo $bar$ baz $quux$ bux" 7 5 10 "$" "$" "" "")
    ("foo $bar$ baz $quux$ bux" 23 15 21 "$" "$" "" "")
    ("foo $bar$ baz $quux$ bux" 21 15 21 "$" "$" "" "")
    ("foo $bar$ baz $quux$ bux" 17 15 21 "$" "$" "" "")
    ))

(ert-deftest sp-test-get-stringlike-expression-latex-backward ()
  (sp-test-setup-stringlike-expression-env-latex
   (--each sp-test-get-stringlike-expression-backward
     (sp-test-stringlike-sexp (car it) (apply 'sp-test-make-pair (cddr it)) (cadr it) t nil))
   (--each sp-test-get-stringlike-expression-latex-backward
     (sp-test-stringlike-sexp (car it) (apply 'sp-test-make-pair (cddr it)) (cadr it) t nil))))

(ert-deftest sp-test-get-textmode-stringlike-expression-latex-backward ()
  (sp-test-setup-stringlike-expression-env-latex
   (--each sp-test-get-stringlike-expression-backward
     (sp-test-textmode-stringlike-sexp (car it) (apply 'sp-test-make-pair (cddr it)) (cadr it) t nil))
   (--each sp-test-get-stringlike-expression-latex-backward
     (sp-test-textmode-stringlike-sexp (car it) (apply 'sp-test-make-pair (cddr it)) (cadr it) t nil))))

(defvar sp-test-get-stringlike-expression-latex-fail
  '(("foo $bar$ baz $ boo" 14)
    ("foo $ bar" 1)
    ))

(ert-deftest sp-test-get-stringlike-expression-latex-fail ()
  (sp-test-setup-stringlike-expression-env-latex
   (--each sp-test-get-stringlike-expression-fail
     (sp-test-stringlike-sexp (car it) nil (cadr it) nil t))
   (--each sp-test-get-stringlike-expression-latex-fail
     (sp-test-stringlike-sexp (car it) nil (cadr it) nil t))))

(defvar sp-test-get-stringlike-expression-latex-backward-fail
  '(("foo $ bar" 7)
    ))

(ert-deftest sp-test-get-stringlike-expression-latex-backward-fail ()
  (sp-test-setup-stringlike-expression-env-latex
   (--each sp-test-get-stringlike-expression-backward-fail
     (sp-test-stringlike-sexp (car it) nil (cadr it) t t))
   (--each sp-test-get-stringlike-expression-latex-backward-fail
     (sp-test-stringlike-sexp (car it) nil (cadr it) t t))))

(defmacro sp-test-setup-stringlike-expression-env-latex (&rest forms)
  `(sp-test-setup-paired-expression-env
     sp--test-latex-pairs
     latex-mode
     latex-mode-hook
     ,@forms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test the combination of string and regular pairs

(defun sp-test-sexp-parse-in-org (initial result)
  (sp-test-with-temp-buffer initial
      (org-mode)
    (should (equal (sp-get-sexp) result))))

(ert-deftest sp-test-sexp-parse nil
  (let ((sp-navigate-consider-stringlike-sexp '(org-mode))
        (sp-pairs '((t . ((:open "=" :close "=" :actions (insert wrap autoskip navigate))
                          (:open "'" :close "'" :actions (insert wrap autoskip navigate))
                          (:open "/" :close "/" :actions (insert wrap autoskip navigate))
                          (:open "[" :close "]" :actions (insert wrap autoskip navigate)))))))
    (sp-test-sexp-parse-in-org "foo /bar/ |n'est pas [asd] asd /baz/" '(:beg 21 :end 26 :op "[" :cl "]" :prefix "" :suffix ""))
    (sp-test-sexp-parse-in-org "foo /bar/ n'est pas [a|sd] asd /baz/" '(:beg 21 :end 26 :op "[" :cl "]" :prefix "" :suffix ""))
    (sp-test-sexp-parse-in-org "foo /bar/ n'est pas [asd] |asd /baz/" '(:beg 31 :end 36 :op "/" :cl "/" :prefix "" :suffix ""))
    (sp-test-sexp-parse-in-org "foo [b|ar /baz/ asd" '(:beg 10 :end 15 :op "/" :cl "/" :prefix "" :suffix ""))))

(defun sp-test-get-textmode-stringlike-expression-in-org (initial result &optional back)
  (sp-test-with-temp-buffer initial
      (org-mode)
    (should (equal (sp-get-textmode-stringlike-expression back) result))))


(ert-deftest sp-test-get-textmode-stringlike-expression nil
  (let ((sp-navigate-consider-stringlike-sexp '(org-mode))
        (sp-pairs '((t . ((:open "=" :close "=" :actions (insert wrap autoskip navigate))
                          (:open "'" :close "'" :actions (insert wrap autoskip navigate))
                          (:open "/" :close "/" :actions (insert wrap autoskip navigate))
                          (:open "~" :close "~" :actions (insert wrap autoskip navigate)))))))
    (sp-test-get-textmode-stringlike-expression-in-org "|/bar/ asd /baz/" '(:beg 1 :end 6 :op "/" :cl "/" :prefix "" :suffix ""))
    (sp-test-get-textmode-stringlike-expression-in-org "/b|ar/ asd /baz/" '(:beg 1 :end 6 :op "/" :cl "/" :prefix "" :suffix ""))
    (sp-test-get-textmode-stringlike-expression-in-org "/bar|/ asd /baz/" '(:beg 1 :end 6 :op "/" :cl "/" :prefix "" :suffix ""))
    (sp-test-get-textmode-stringlike-expression-in-org "/bar/| asd /baz/" '(:beg 1 :end 6 :op "/" :cl "/" :prefix "" :suffix "") t)
    (sp-test-get-textmode-stringlike-expression-in-org "/bar/ asd |/baz/" '(:beg 11 :end 16 :op "/" :cl "/" :prefix "" :suffix ""))
    (sp-test-get-textmode-stringlike-expression-in-org "/bar/ asd /b|az/" '(:beg 11 :end 16 :op "/" :cl "/" :prefix "" :suffix ""))
    (sp-test-get-textmode-stringlike-expression-in-org "/bar/ asd /baz|/" '(:beg 11 :end 16 :op "/" :cl "/" :prefix "" :suffix ""))
    (sp-test-get-textmode-stringlike-expression-in-org "/bar/ asd /baz/|" '(:beg 11 :end 16 :op "/" :cl "/" :prefix "" :suffix "") t)
    (sp-test-get-textmode-stringlike-expression-in-org "/bar/ asd ~a|sd~" '(:beg 11 :end 16 :op "~" :cl "~" :prefix "" :suffix ""))
    (sp-test-get-textmode-stringlike-expression-in-org "|//" '(:beg 1 :end 3 :op "/" :cl "/" :prefix "" :suffix ""))
    (sp-test-get-textmode-stringlike-expression-in-org "//|" '(:beg 1 :end 3 :op "/" :cl "/" :prefix "" :suffix "") t)))
