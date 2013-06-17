;;; smartparens-ruby.el --- Additional configuration for Ruby based modes.

;; Copyright (C) 2013 Jean-Louis Giordano

;; Author: Jean-Louis Giordano <jean-louis@jawaninja.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 16 June 2013
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

;; This file provides some additional configuration for Ruby based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-ruby)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.

;; If you have good ideas about what should be added please file an
;; issue on the github tracker.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

;; helpers
(defun sp-ruby-block-post-handler (id action context)
  (when (equal action 'insert)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode)))

(defun sp-ruby-def-post-handler (id action context)
  (when (equal action 'insert)
    (save-excursion
      (insert " _")
      (newline)
      (indent-according-to-mode))
    (kill-forward-chars 2)
    (indent-according-to-mode)))

(defun sp-ruby-pre-handler (id action context)
  (when (equal action 'slurp-backward)
    (save-excursion
      (sp-forward-sexp)
      (delete-indentation -1))
    (save-excursion
      (newline)))

  (when (equal action 'barf-backward)
    (save-excursion
      (sp-backward-sexp)
      (delete-indentation))
    (save-excursion
      (newline)))

  (when (equal action 'slurp-forward)
    (save-excursion
      (sp-backward-sexp)
      (delete-indentation))
    (newline))

  (when (equal action 'barf-forward)
    (save-excursion
      (sp-forward-sexp)
      (delete-indentation -1))
    (newline)))

(defun sp-ruby-in-string-or-word-p (id action context)
  (or (sp-in-string-p id action context)
      (and (looking-back id)
           (not (looking-back (sp--strict-regexp-quote id))))))

(defun sp-ruby-no-do-block-p (id action context)
  (or (sp-ruby-in-string-or-symbol-p id action context)
      (and (looking-back (sp--strict-regexp-quote id))
           (not (looking-back (concat "[^ ] " id))))))

(sp-with-modes '(ruby-mode)

  ;; Blocks
  (sp-local-pair "do" "end"
                 :unless '(sp-ruby-no-do-block-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-block-post-handler))

  (sp-local-pair "begin" "end"
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-block-post-handler))

  ;; Defs

  (sp-local-pair "def" "end"
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler))

  (sp-local-pair "class" "end"
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler))

  (sp-local-pair "module" "end"
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler))

  (sp-local-pair "if" "end"
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler))

  (sp-local-pair "unless" "end"
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler))
  )

(provide 'smartparens-ruby)

;;; smartparens-ruby.el ends here
