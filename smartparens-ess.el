;;; smartparens-ess.el --- Smartparens Extension for Emacs Speaks Statistics

;; Copyright (c) 2015-2016 Bernhard Pröll

;; Author: Bernhard Pröll
;; Maintainer: Bernhard Pröll
;; URL: https://github.com/Fuco1/smartparens
;; Created: 2015-02-26
;; Version: 0.2
;; Keywords: abbrev convenience editing

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'smartparens)
(require 'rx)


;; avoid traveling commas when slurping
;; (|a, b), c ---> (|a, b, c)
(dolist (mode '(ess-mode inferior-ess-mode))
  (add-to-list 'sp-sexp-suffix (list mode 'regexp "")))

;; slurping follows Google's R style guide
;; see https://google.github.io/styleguide/Rguide.xml
(defun sp-ess-pre-handler (id action context)
  "Remove spaces before opening parenthesis in a function call.
Remove redundant space around commas."
  (when (equal action 'slurp-forward)
    (save-excursion
      (sp-backward-sexp)
      ;; for style reasons there should be a space before curly brackets
      (unless (looking-at "{")
        ;; (|)v[2] ---> (|v)[2] ---> (|v[2])
        (when (looking-back
               (rx (or
                    (and symbol-end (one-or-more space))
                    (and (syntax close-parenthesis)
                         (one-or-more space)
                         not-word-boundary))))
          (cycle-spacing 0 nil 'single-shot)))
      (cond
        ;; (|)if(cond) ---> (|if (cond))
        ((looking-back (rx symbol-start (or "if" "for" "while")
                           (zero-or-more space)))
         (cycle-spacing 1 nil 'single-shot))
        ;; (|)a , b,    c ---> (|a, b, c)
        ((looking-back
          (rx (zero-or-more space) "," (zero-or-more space))
          (line-beginning-position) 'greedy)
         (replace-match ", ")))))
  ;; v[2](|) ---> v([2]|) ---> (v[2]|)
  (when (equal action 'slurp-backward)
    (save-excursion
      (sp-forward-sexp)
      (cond ((looking-at (rx (one-or-more space) "{"))
             (cycle-spacing 1 nil 'single-shot))
            ((looking-back (rx symbol-start (or "if" "for" "while")))
             (cycle-spacing 1 nil 'single-shot))
            ((looking-at
              (rx (and (zero-or-more space)
                       (or (syntax close-parenthesis)
                           (syntax open-parenthesis)))))
             (cycle-spacing 0 nil 'single-shot))
            ((looking-at
              (rx (zero-or-more space) "," (zero-or-more space)))
             (replace-match ", "))))))

;; function(x) {|} ---> function(x) {\n|\n}
;; ##' \tabular{rrr}{|} --->
;; ##' \tabular{rrr}{
;; ##'   |
;; ##' }
(defun sp-ess-open-sexp-indent (&rest _ignored)
  "Open new brace or bracket with indentation."
  (if (and (fboundp 'ess-roxy-entry-p) (ess-roxy-entry-p))
      (progn
        (save-excursion (ess-roxy-indent-on-newline))
        (when (looking-back ess-roxy-str nil)
          (cycle-spacing 3 nil t)))
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(defun sp-ess-roxy-str-p (id action context)
  (when (and (boundp 'ess-roxy-re) (eq action 'insert))
    (sp--looking-back-p ess-roxy-re)))

(sp-with-modes 'ess-mode
  (sp-local-pair "{" nil
		 :pre-handlers '(sp-ess-pre-handler)
                 ;; the more reasonable C-j interferes with default binding for
                 ;; `ess-eval-line'
		 :post-handlers '((sp-ess-open-sexp-indent "M-j")))
  (sp-local-pair "(" nil
		 :pre-handlers '(sp-ess-pre-handler)
		 :post-handlers '((sp-ess-open-sexp-indent "M-j")))
  (sp-local-pair "[" nil
		 :pre-handlers '(sp-ess-pre-handler)
		 :post-handlers '((sp-ess-open-sexp-indent "M-j")))
  (sp-local-pair "'" nil
                 :unless '(sp-ess-roxy-str-p)))

;;; roxygen2 markup
;; see https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html
(sp-with-modes 'ess-mode
  (sp-local-pair "\\strong{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\strong")
  (sp-local-pair "\\emph{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\emph")
  (sp-local-pair "\\code{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\code")
  (sp-local-pair "\\url{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\url")
  (sp-local-pair "\\link{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\link")
  (sp-local-pair "\\href{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\href"
                 :suffix "{[^}]*}")
  (sp-local-pair "\\email{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\email")
  (sp-local-pair "\\pkg{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\pkg")
  (sp-local-pair "\\item{" "}"
                 :when '(sp-in-comment-p)
		 :post-handlers '((sp-ess-open-sexp-indent "M-j"))
                 :trigger "\\item{")
  (sp-local-pair "\\enumerate{" "}"
                 :when '(sp-in-comment-p)
		 :post-handlers '((sp-ess-open-sexp-indent "M-j"))
                 :trigger "\\enumerate")
  (sp-local-pair "\\itemize{" "}"
                 :when '(sp-in-comment-p)
		 :post-handlers '((sp-ess-open-sexp-indent "M-j"))
                 :trigger "\\itemize")
  (sp-local-pair "\\describe{" "}"
                 :when '(sp-in-comment-p)
		 :post-handlers '((sp-ess-open-sexp-indent "M-j"))
                 :trigger "\\describe")
  (sp-local-pair "\\eqn{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\eqn")
  (sp-local-pair "\\deqn{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\deqn")
  (sp-local-pair "\\tabular{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\tabular"
		 :post-handlers '((sp-ess-open-sexp-indent "M-j"))
                 :suffix "{[^}]*}"))


(provide 'smartparens-ess)
;;; smartparens-ess ends here
