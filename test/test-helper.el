;;; test-helper.el --- Helper for tests.

;; Copyright (C) 2015-2016 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 4 Aug 2015

;;; Commentary:

;; Grab bag of utilities for running smartparens tests.

;;; Code:

;; (when (require 'undercover nil t)
;;   (undercover "smartparens*.el"))

(require 'ert)
(require 'dash)
(require 'f)
(require 'cl-lib)

(let ((sp-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path sp-dir))
(require 'smartparens-config)
;; preload latex-mode settings
(with-temp-buffer
  (latex-mode))

(defvar sp--test-basic-pairs
  '((t
     .
     ((:open "\\{" :close "\\}" :actions (insert wrap autoskip navigate))
      (:open "("   :close ")"   :actions (insert wrap autoskip navigate))
      (:open "["   :close "]"   :actions (insert wrap autoskip navigate))
      (:open "{"   :close "}"   :actions (insert wrap autoskip navigate))
      (:open "\""  :close "\""  :actions (insert wrap autoskip navigate escape))
      (:open "\\\""  :close "\\\""  :actions (insert wrap autoskip navigate))
      (:open "\\langle"  :close "\\rangle"  :actions (insert wrap autoskip navigate))
      (:open "OPEN"  :close "CLOSE"  :actions (insert wrap autoskip navigate))
      (:open "\\big("  :close "\\big)"  :actions (insert wrap autoskip navigate) :trigger "\\b")))))

(defun sp-test-merge-pairs (extra)
  (list (cons t (append (-map 'identity (cdar sp--test-basic-pairs)) extra))))

(defvar sp--test-latex-pairs
  (sp-test-merge-pairs '((:open "``"   :close "''" :actions (insert wrap autoskip navigate))
                         (:open "`"   :close "'" :actions (insert wrap autoskip navigate))
                         (:open "$"   :close "$" :actions (insert wrap autoskip navigate)))))

(defmacro sp-test-setup-paired-expression-env (pairs mode mode-hook &rest forms)
  (declare (indent 0))
  `(with-temp-buffer
     (let ((sp-pairs ,pairs)
           (,mode-hook nil)
           (change-major-mode-hook nil))
       (,mode)
       (smartparens-mode 1)
       ,@forms)))

(defun sp-test-stringlike-sexp (string expected start back fail)
  (unwind-protect
      (progn
        (insert string)
        (goto-char start)
        (let ((pair (sp-get-stringlike-expression back)))
          (should (equal pair expected))))
    (erase-buffer)))

(defun sp-test-textmode-stringlike-sexp (string expected start back fail)
  (unwind-protect
      (progn
        (insert string)
        (goto-char start)
        (let ((pair (sp-get-textmode-stringlike-expression back)))
          (should (equal pair expected))))
    (erase-buffer)))

(defun sp-test-make-pair (b e o c p s)
  (list :beg b :end e :op o :cl c :prefix p :suffix s))

(defmacro sp-test-with-temp-buffer (initial initform &rest forms)
  "Setup a new buffer, then run FORMS.

First, INITFORM are run in the newly created buffer.

Then `smartparens-mode' is turned on.  Then INITIAL is
inserted (it is expected to evaluate to string).  If INITIAL
contains | put point there as the initial position (the character
is then removed).  If it contains M, put mark there (the
character is then removed).

Finally, FORMS are run."
  (declare (indent 2)
           (debug (form form body)))
  `(save-window-excursion
     (let ((case-fold-search nil))
       (with-temp-buffer
         (set-input-method nil)
         ,initform
         (smartparens-mode 1)
         (pop-to-buffer (current-buffer))
         (insert ,initial)
         (goto-char (point-min))
         (when (search-forward "M" nil t)
           (delete-char -1)
           (set-mark (point))
           (activate-mark))
         (goto-char (point-min))
         (when (search-forward "|" nil t)
           (delete-char -1))
         ,@forms))))

(defmacro sp-test-with-temp-elisp-buffer (initial &rest forms)
  "Setup a new `emacs-lisp-mode' test buffer.

See `sp-test-with-temp-buffer'."
  (declare (indent 1)
           (debug (form body)))
  `(sp-test-with-temp-buffer ,initial
       (emacs-lisp-mode)
     ,@forms))

(defun sp-buffer-equals (result)
  "Compare buffer to RESULT.

RESULT is a string which should equal the result of
`buffer-string' called in the current buffer.

If RESULT contains |, this represents the position of `point' and
should match.

If RESULT contains M, this represents the position of `mark' and
should match."
  (should (equal (buffer-string) (replace-regexp-in-string "[|M]" "" result)))
  (when (string-match-p "|" result)
    (should (= (1+ (string-match-p
                    "|" (replace-regexp-in-string "[M]" "" result)))
               (point))))
  (when (string-match-p "M" result)
    (should (= (1+ (string-match-p
                    "M" (replace-regexp-in-string "[|]" "" result)))
               (mark)))))

;; put advice on `TeX-update-style' to silence its output
(defadvice TeX-update-style (around fix-output-spam activate)
  (shut-up ad-do-it))

;;; test-helper.el ends here
