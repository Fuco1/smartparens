;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair tests

(defvar sp--test-basic-pairs
  '((t
     .
     ((:open "\\{" :close "\\}" :actions (insert wrap autoskip navigate))
      (:open "("   :close ")"   :actions (insert wrap autoskip navigate))
      (:open "["   :close "]"   :actions (insert wrap autoskip navigate))
      (:open "{"   :close "}"   :actions (insert wrap autoskip navigate))
      (:open "\""  :close "\""  :actions (insert wrap autoskip navigate))
      (:open "\\\""  :close "\\\""  :actions (insert wrap autoskip navigate))))))

(defmacro sp-test-setup-paired-expression-env (pairs mode mode-hook &rest forms)
  (declare (indent 0))
  `(with-temp-buffer
     (let ((sp-pairs ,pairs)
           (,mode-hook nil)
           (change-major-mode-hook nil))
       (,mode)
       (smartparens-mode 1)
       ,@forms)))

(defun sp-test-paired-sexp (string expected back fail)
  (unwind-protect
      (progn
        (insert string)
        (if back (progn
                   (goto-char (point-max))
                   (--when-let (car (sp-get-comment-bounds))
                     (goto-char it)))
          (goto-char (point-min)))
        (let ((pair (sp-get-paired-expression back)))
          (should (equal pair expected))))
    (erase-buffer)))

(defun sp-test-stringlike-sexp (string expected start back fail)
  (unwind-protect
      (progn
        (insert string)
        (goto-char start)
        (let ((pair (sp-get-stringlike-expression back)))
          (should (equal pair expected))))
    (erase-buffer)))

(defun sp-test-make-pair (b e o c p s)
  (list :beg b :end e :op o :cl c :prefix p :suffix s))

(defun sp-test-merge-pairs (extra)
  (list (cons t (append (-map 'identity (cdar sp--test-basic-pairs)) extra))))



(provide 'smartparens-test-env)
