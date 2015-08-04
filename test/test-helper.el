;;; test-helper.el --- Helper for tests.

;; Copyright (C) 2015 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 4 Aug 2015

;;; Commentary:

;; Grab bag of utilities for running smartparens tests.

;;; Code:

(require 'ert)
(require 'dash)
(require 'f)
(require 'cl-lib)

(let ((sp-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path sp-dir))
(require 'smartparens)

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
       ,@forms)))

(defmacro sp-test-with-temp-elisp-buffer (initial &rest forms)
  "Setup a new `emacs-lisp-mode' test buffer.

See `sp-test-with-temp-buffer'."
  (declare (indent 1)
           (debug (form body)))
  `(sp-test-with-temp-buffer ,initial
       (emacs-lisp-mode)
     ,@forms))

;;; test-helper.el ends here
