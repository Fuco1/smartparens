;;; smartparens-latex.el --- Additional configuration for (La)TeX based modes.

;; Copyright (C) 2013-2016 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 14 Feb 2013
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some additional configuration for (La)TeX based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-latex)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.

;; It is advised that you add `latex-mode' to the list
;; `sp-navigate-consider-stringlike-sexp'.  This will tell
;; smartparens to treat the $$ math blocks as sexps, and enable you
;; to use all the sexp-based commands on them (such as
;; `sp-down-sexp', `sp-up-sexp' etc.)

;; If you have good ideas about what should be added please file an
;; issue on the github tracker.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

(defun sp-latex-insert-spaces-inside-pair (id action context)
  "ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (insert "  ")
    (backward-char 1))
  (when (and (eq action 'wrap)
             (save-excursion
               (goto-char (sp-get sp-last-wrapped-region :beg-in))
               (not (sp--looking-back-p "[[{(]"))))
    (save-excursion
      (goto-char (sp-get sp-last-wrapped-region :end-in))
      (insert " ")
      (goto-char (sp-get sp-last-wrapped-region :beg-in))
      (insert " "))))

(defun sp-latex-skip-match-apostrophe (ms mb me)
  "MS, MB, ME."
  (when (equal ms "'")
    (save-excursion
      (goto-char me)
      (looking-at-p "\\sw"))))

(defun sp-latex-skip-double-quote (id action context)
  "ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (when (looking-at-p "''''")
      (delete-char -2)
      (delete-char 2)
      (forward-char 2))))

(defun sp-number-of-backslashes-before-point ()
  (let ((p (point)))
    (while (and (> p 0) (equal (char-before p) ?\\ ))
      (setq p (- p 1)))
    (- (point) p)))

(defun sp--latex-backslash-skip-match (ms mb _me)
  "Skips a match if preceeded by uneven number of backslashes."
  (and ms
       (save-excursion
         (goto-char mb)
         (not (evenp (sp-number-of-backslashes-before-point))))))

(defun sp-latex-point-after-backslash (id action context)
  "Return t if point follows an uneven number of backslashes (a
double backslash is a newline; we'd like to ignore those), nil
otherwise. This predicate is only tested on \"insert\" action.
ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (let* ((trigger (sp-get-pair id :trigger))
           (start (- (point) (length (if trigger trigger id)))))
      (when (> start 1)
        (save-excursion
          (goto-char start)
          (not (evenp (sp-number-of-backslashes-before-point))))))))

(defun sp-latex-point-before-word-p (id action context)
  "Return t if point is before a word while in navigate action.
ID, ACTION, CONTEXT."
  (when (eq action 'navigate)
    (looking-at-p "\\sw")))

(add-to-list 'sp-navigate-skip-match
             '((tex-mode plain-tex-mode latex-mode) . sp--latex-backslash-skip-match))

(sp-with-modes '(
                 tex-mode
                 plain-tex-mode
                 latex-mode
                 LaTeX-mode
                 )
  (sp-local-pair "`" "'"
                 :actions '(:rem autoskip)
                 :skip-match 'sp-latex-skip-match-apostrophe
                 :unless '(sp-latex-point-after-backslash
                           sp-latex-point-before-word-p))

  ;; math modes, yay.  The :actions are provided automatically if
  ;; these pairs do not have global definitions.
  (sp-local-pair "$" "$"
                 :unless '(sp-latex-point-after-backslash))
  (sp-local-pair "\\[" "\\]"
                 :unless '(sp-latex-point-after-backslash))

  ;; disable useless pairs.  Maybe also remove " ' and \"?
  (sp-local-pair "/*" nil :actions nil)
  (sp-local-pair "\\\\(" nil :actions nil)
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "\\\"" nil :actions nil)

  ;; quote should insert ``'' instead of double quotes.  If we ever
  ;; need to insert ", C-q is our friend.
  (sp-local-pair "``" "''"
                 :trigger "\""
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-skip-double-quote))

  ;; add the prefix function sticking to {} pair
  (sp-local-pair "{" "}"
                 :prefix "\\\\\\(\\sw\\|\\s_\\)*"
                 :unless '(sp-latex-point-after-backslash))
  
  ;; do not add more space when slurping
  (sp-local-pair "\\{" "\\}"
                 :unless '(sp-latex-point-after-backslash))
  (sp-local-pair "[" "]"
                 :unless '(sp-latex-point-after-backslash))
  (sp-local-pair "(" ")"
                 :unless '(sp-latex-point-after-backslash))
  (sp-local-pair "\\(" "\\)"
                 :unless '(sp-latex-point-after-backslash))

  ;; pairs for big brackets.  Needs more research on what pairs are
  ;; useful to add here.  Post suggestions if you know some.
  (sp-local-pair "\\left(" "\\right)"
                 :trigger "\\l("
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\left[" "\\right]"
                 :trigger "\\l["
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\left\\{" "\\right\\}"
                 :trigger "\\l{"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\left|" "\\right|"
                 :trigger "\\l|"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\bigl(" "\\bigr)"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\biggl(" "\\biggr)"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Bigl(" "\\Bigr)"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Biggl(" "\\Biggr)"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\bigl[" "\\bigr]"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\biggl[" "\\biggr]"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Bigl[" "\\Bigr]"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Biggl[" "\\Biggr]"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\bigl\\{" "\\bigr\\}"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\biggl\\{" "\\biggr\\}"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Bigl\\{" "\\Bigr\\}"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Biggl\\{" "\\Biggr\\}"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\lfloor" "\\rfloor"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\lceil" "\\rceil"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\langle" "\\rangle"
                 :when '(sp-in-math-p)
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))

  ;; some common wrappings
  (sp-local-tag "\\b" "\\begin{_}" "\\end{_}")
  (sp-local-tag "bi" "\\begin{itemize}" "\\end{itemize}")
  (sp-local-tag "be" "\\begin{enumerate}" "\\end{enumerate}"))

(provide 'smartparens-latex)

;;; smartparens-latex.el ends here
