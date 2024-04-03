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

(defmacro sp-test-kbd-macro (initial macro result &rest forms)
  "Run an interactive test by emulating keyboard macro.

First, a buffer is initialized usint `sp-test-with-temp-buffer',
then the MACRO is executed via `execute-kbd-macro' and finally
buffer is compared to an expected RESULT using
`sp-buffer-equals'.

Keyword arguments :let, :init-form, :mode are supported, see
`sp-ert-deftest'."
  (let* ((let-form (plist-get forms :let))
         (init-form (plist-get forms :init-form))
         (mode (plist-get forms :mode)))
    `(let ,let-form
       (sp-test-with-temp-buffer ,initial
           ,(append init-form `(,(cadr mode)))
         (execute-kbd-macro ,macro)
         (sp-buffer-equals ,result)))))

(put 'sp-test-kbd-macro 'sp-ert-deftest-keywords (list :let :init-form :mode))

(defmacro sp-test-complex (initial form result &rest forms)
  "Run an interactive test by evaluating FORM.

First, a buffer is initialized usint `sp-test-with-temp-buffer',
then the FORM is executed and finally buffer is compared to an
expected RESULT using `sp-buffer-equals'.

Keyword arguments :let, :init-form, :mode are supported, see
`sp-ert-deftest'."
  (declare (indent 1)
           (debug (form form body)))
  (let* ((let-form (plist-get forms :let))
         (init-form (plist-get forms :init-form))
         (mode (plist-get forms :mode)))
    `(let ,let-form
       (sp-test-with-temp-buffer ,initial
           ,(append init-form `(,(cadr mode)))
         ,form
         (sp-buffer-equals ,result)))))

(put 'sp-test-complex 'sp-ert-deftest-keywords (list :let :init-form :mode))

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

(defmacro sp-test-with-delete-selection-mode (&rest forms)
  "Enable `delete-selection-mode' for current test and disable it
afterwards.

This is necessary to keep tests isolated because it is a global
minor mode."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (delete-selection-mode 1)
         ,@forms)
     (delete-selection-mode -1)))

(defmacro sp-test-with-temp-binding (binding &rest forms)
  "Execute FORMS with temporary keybinding.

BINDING is a list (kbd command)."
  (declare (indent 1))
  `(let ((smartparens-mode-map smartparens-mode-map))
     (define-key smartparens-mode-map (kbd ,(car binding)) ,(cadr binding))
     ,@forms))

;; put advice on `TeX-update-style' to silence its output
(defadvice TeX-update-style (around fix-output-spam activate)
  (shut-up ad-do-it))

(defmacro sp-ert-deftest (name &rest forms)
  "Generate `ert-deftest' declarations out of FORMS.

Each form of FORMS represents a single test.  NAME is suffixed by
the dash and numeric index of the test.

Some keyword arguments inside FORMS are interpreted as keys in a
plist:

- :let => A form in a format accepted by first argument of
  `let'.  If non-nil, it wraps every generated test instance and
  can be used to to set up the test environment.
- :init-form => A form run before the test to set up the environment.
- :mode => a symbol of the `major-mode' to activate.  This is a
  convenient shorter form of :init-form.

These forms are not handled by this macro but are passed to the
test forms which are expected to interpret them.  Whether or not
these properties are passed to the child form as keyword
arguments is governed by the symbol property
`sp-ert-deftest-keywords', see `get', `put', `symbol-plist'.

If the first non plist key/value argument is a string, it is
interpreted as a docstring and no test case is generated from
it."
  (declare (indent 1))
  (let* ((index 0)
         (let-form (plist-get forms :let))
         (init-form (plist-get forms :init-form))
         (mode (plist-get forms :mode))
         (forms (-> forms
                    (map-delete :let)
                    (map-delete :init-form)
                    (map-delete :mode)))
         (forms
          (mapcar
           (lambda (form)
             (setq index (1+ index))
             (let ((supported-keywords
                    (get (car form) 'sp-ert-deftest-keywords)))
               `(ert-deftest ,(intern (concat
                                       (symbol-name name)
                                       "-"
                                       (number-to-string index)))
                    ()
                  (,@form
                   ,@(when (member :let supported-keywords)
                       `(:let ,let-form))
                   ,@(when (member :init-form supported-keywords)
                       `(:init-form ,init-form))
                   ,@(when (member :mode supported-keywords)
                       `(:mode ,mode))))))
           (if (stringp (car forms)) (cdr forms) forms))))
    `(progn ,@forms)))

;;; test-helper.el ends here
