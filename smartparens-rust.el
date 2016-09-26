;;; smartparens-rust.el --- Additional configuration for Haskell based modes.

;; Copyright (C) 2015 Wilfred Hughes

;; Created: 3 November 2015
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

;; This file provides some additional configuration for Rust.  To use
;; it, simply add:
;;
;; (require 'smartparens-config)
;;
;; alternatively, you can explicitly load these preferences:
;;
;; (require 'smartparens-rust)
;;
;; in your configuration.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:
(require 'smartparens)

(defun sp-in-rust-lifetime-context (&rest args)
  "Return t if point is in a Rust context where ' represents a lifetime.
If we return nil, ' should be used for character literals."
  (or
   (condition-case nil
       ;; If point is just after a &', it's probably a &'foo.
       (save-excursion
         (backward-char 2)
         (looking-at "&"))
     ;; If we're at the beginning of the buffer, just carry on.
     (beginning-of-buffer))
   ;; If point is inside < > it's probably a parameterised function.
   (let ((paren-pos (nth 1 (syntax-ppss))))
     (and paren-pos
          (save-excursion
            (goto-char paren-pos)
            (looking-at "<"))))))

(defun sp-rust-filter-angle-brackets (id action context)
  "Return t if we should allow the ACTION in the current CONTEXT
  for angle brackets."
  (cond
   ;; Disallow in format string after any character, since < is used to specify
   ;; alignment
   ((and (eq context 'string)
         (looking-back (rx (seq alphanumeric "<")))) nil)

   ;; Disallow when inserting in code in two situations: 1) when '<' is used for
   ;; the comparison operators '<' and '<=', and 2) when writing a left shift
   ;; '<<'.  In both cases, we assume the user will use a space before the
   ;; opening bracket '<'.
   ((and (eq context 'code)
         (eq action 'insert)
         (looking-back (rx (or (seq space "<")
                               (seq space "<<"))))) nil)

   ;; Otherwise, allow all actions
   (t)))

(sp-with-modes '(rust-mode)
  (sp-local-pair "'" "'" :unless '(sp-in-comment-p sp-in-string-quotes-p sp-in-rust-lifetime-context) :post-handlers'(:rem sp-escape-quotes-after-insert))
  (sp-local-pair "<" ">" :when '(sp-rust-filter-angle-brackets)))

;; Rust has no sexp suffices.  This fixes slurping
;; (|foo).bar -> (foo.bar)
(add-to-list 'sp-sexp-suffix (list #'rust-mode 'regexp ""))

(provide 'smartparens-rust)

;;; smartparens-rust.el ends here
