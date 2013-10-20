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
;;

;; If you have good ideas about what should be added please file an
;; issue on the github tracker.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

(defun sp-ruby-maybe-one-space ()
  (while (looking-back " ") (backward-char))
  (when (or (looking-at-p " ")
            (looking-at-p "}")
            (looking-back "{"))
    (save-excursion (just-one-space)))
  (when (and (not (looking-back "^.?"))
             (save-excursion
               (backward-char 2)
               (or (looking-at-p ".[^:] [.([,;]")
                   (looking-at-p ".. ::")
                   (looking-at-p ".[.@$] ")
                   (looking-at-p ":: "))))
    (delete-char 1)))

(defun sp-ruby-delete-indentation (&optional arg)
  "Better way of joining ruby lines"
  (delete-indentation arg)
  (sp-ruby-maybe-one-space))

(defun sp-ruby-block-post-handler (id action context)
  "Handler for ruby block-like inserts"
  (when (equal action 'insert)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))
  (sp-ruby-post-handler id action context))

(defun sp-ruby-def-post-handler (id action context)
  "Handler for ruby def-like inserts"
  (when (equal action 'insert)
    (save-excursion
      (insert "x")
      (newline)
      (indent-according-to-mode))
    (delete-char 1))
  (sp-ruby-post-handler id action context))

(defun sp-ruby-post-handler (id action context)
  (when (equal action 'barf-backward)
    (sp-ruby-delete-indentation 1)
    (indent-according-to-mode)
    (save-excursion
      (sp-backward-sexp) ; move to begining of current sexp
      (sp-backward-sexp arg)
      (sp-ruby-maybe-one-space)))

  (when (equal action 'barf-forward)
    (sp-get enc
      (let ((beg-line (line-number-at-pos :beg-in))
            (end-line (line-number-at-pos :end-in)))
        (sp-forward-sexp arg)
        (sp-ruby-maybe-one-space)
        (when (not (= (line-number-at-pos) beg-line))
          (sp-ruby-delete-indentation -1))
        (indent-according-to-mode)))))

(defun sp-ruby-pre-handler (id action context)
  "Handler for ruby slurp and barf"
  (sp-get enc
    (let ((beg-line (line-number-at-pos :beg-in))
          (end-line (line-number-at-pos :end-in)))

      (when (equal action 'slurp-backward)
        (save-excursion
          (sp-forward-sexp)
          (when (looking-at-p ";") (forward-char))
          (sp-ruby-maybe-one-space)
          (when (not (= (line-number-at-pos) end-line))
            (sp-ruby-delete-indentation -1)))
        (while (thing-at-point-looking-at "\\.[ \n]*")
          (sp-backward-sexp))
        (when (looking-back "[@$:&?!]")
          (backward-char)
          (when (looking-back "[@&:]")
            (backward-char)))
        (just-one-space)
        (save-excursion
          (if (= (line-number-at-pos) end-line)
              (insert " ")
            (newline))))

      (when (equal action 'barf-backward)
        ;; Barf whole method chains
        (while (thing-at-point-looking-at "[.([:][ \n]*")
          (sp-forward-sexp))
        (if (looking-at-p " *$")
            (newline)
          (save-excursion (newline))))

      (when (equal action 'slurp-forward)
        (save-excursion
          (sp-backward-sexp)
          (when (looking-back "\.") (backward-char))
          (sp-ruby-maybe-one-space)
          (when (not (= (line-number-at-pos) beg-line))
            (if (thing-at-point-looking-at "\\.[ \n]*")
                (progn
                  (forward-symbol -1)
                  (sp-ruby-delete-indentation -1))
              (sp-ruby-delete-indentation))))
        (while (looking-at-p "::") (sp-forward-symbol))
        (when (looking-at-p "[?!;]") (forward-char))
        (if (= (line-number-at-pos) beg-line)
            (insert " ")
          (newline)))

      (when (equal action 'barf-forward)
        (when (looking-back "\\.") (backward-char))
        (while (looking-back "::") (sp-backward-symbol))
        (if (= (line-number-at-pos) end-line)
            (insert " ")
          (if (looking-back "^ *")
              (save-excursion (newline))
            (newline)))))))

(defun sp-ruby-in-string-or-word-p (id action context)
  (or (sp-in-string-p id action context)
      (and (looking-back id)
           (not (looking-back (sp--strict-regexp-quote id))))))

(defun sp-ruby-inline-p (id)
  (save-excursion
    (when (looking-back (concat id " *"))
      (backward-word))
    (when (not (looking-back "^ *"))
      (sp-backward-sexp)
      (sp-forward-sexp)
      (looking-at-p (concat " *" id)))))

(defun sp-ruby-skip-inline-match-p (ms mb me)
  (sp-ruby-inline-p ms))

(defun sp-ruby-method-p (id)
  (save-excursion
    (when (looking-back (concat id " *"))
      (backward-word))
    (and (looking-at-p id)
         (or (looking-at-p (concat id "[_?!:]"))
             (looking-back "[_:@.]")
             ;; Check if multiline method call
             ;; But beware of comments!
             (and (looking-back "\\.[ \n]*")
                  (not (save-excursion
                         (search-backward ".")
                         (sp-point-in-comment))))))))

(defun sp-ruby-skip-method-p (ms mb me)
  (sp-ruby-method-p ms))

(defun sp-ruby-in-string-word-or-inline-p (id action context)
  (or (sp-ruby-in-string-or-word-p id action context)
      (and (looking-back id)
           (sp-ruby-inline-p id))))

(defun sp-ruby-pre-pipe-handler (id action context)
  (when (equal action 'insert)
    (save-excursion
      (just-one-space))
    (save-excursion
      (search-backward id)
      (just-one-space))))

(defun sp-ruby-should-insert-pipe-close (id _action _ctx)
  "Test whether to insert the closing pipe for a lambda-binding pipe pair."
  (thing-at-point-looking-at
   (rx-to-string `(and (or "do" "{") (* space) ,id))))

(sp-with-modes '(ruby-mode enh-ruby-mode)
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-block-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "{" "}"
                 :actions '(insert wrap)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-post-handler)
                 :suffix "")

  (sp-local-pair "begin" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-block-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "def" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "class" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "module" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "case" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "if" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-word-or-inline-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "unless" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-word-or-inline-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "while" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-word-or-inline-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "until" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-word-or-inline-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "for" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-word-or-inline-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-inline-match-p)

  (sp-local-pair "|" "|"
                 :when '(sp-ruby-should-insert-pipe-close)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-pipe-handler)
                 :suffix ""))

(add-to-list 'sp-navigate-consider-stringlike-sexp 'ruby-mode)

(provide 'smartparens-ruby)

;;; smartparens-ruby.el ends here
