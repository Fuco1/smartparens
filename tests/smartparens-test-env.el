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
      (:open "\\\""  :close "\\\""  :actions (insert wrap autoskip navigate))
      (:open "\\langle"  :close "\\rangle"  :actions (insert wrap autoskip navigate))
      (:open "OPEN"  :close "CLOSE"  :actions (insert wrap autoskip navigate))
      (:open "\\big("  :close "\\big)"  :actions (insert wrap autoskip navigate) :trigger "\\b")))))

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

(defmacro sp-test-with-temp-buffer (initial initform &rest forms)
  "Setup a new buffer, then run FORMS.

First, INITFORM are run in the newly created buffer.

Then `smartparens-mode' is turned on.  Then INITIAL is
inserted (it is expected to evaluate to string).  If INITIAL
contains | put point there as the initial position (the character
is then removed).

Finally, FORMS are run."
  (declare (indent 2)
           (debug (form form body)))
  `(save-window-excursion
     (with-temp-buffer
       ,initform
       (smartparens-mode 1)
       (pop-to-buffer (current-buffer))
       (insert initial)
       (goto-char (point-min))
       (when (search-forward "|" nil t)
         (delete-char -1))
       ,@forms)))

(defmacro sp-test-with-temp-elisp-buffer (initial &rest forms)
  "Setup a new `emacs-lisp-mode' test buffer.

See `sp-test-with-temp-buffer'."
  (declare (indent 1)
           (debug (form body)))
  `(sp-test-with-temp-buffer ,initial
       (emacs-lisp-mode)
     ,@forms))

(provide 'smartparens-test-env)
