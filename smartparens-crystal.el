;;; smartparens-crystal.el --- Additional configuration for Crystal based modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Brantou

;; Author: Brantou <brantou89@gmail.com>
;; Maintainer: Brantou <brantou89@gmail.com>
;; Created: 5 March 2018
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; Based on on smartparens-ruby.el

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

;; This file provides some additional configuration for Crystal based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-crystal)
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

(declare-function crystal-forward-sexp "crystal")
(declare-function crystal-backward-sexp "crystal")

(defun sp-crystal-forward-sexp ()
  "Wrapper for `crystal-forward-sexp'."
  (interactive)
  (crystal-forward-sexp))

(defun sp-crystal-backward-sexp ()
  "Wrapper for `crystal-backward-sexp'."
  (interactive)
  (crystal-backward-sexp))

(defun sp-crystal-maybe-one-space ()
  "Turn whitespace around point to just one space."
  (while (looking-back " " nil) (backward-char))
  (when (or (looking-at-p " ")
            (looking-at-p "}")
            (looking-back "{" nil)
            (and (looking-at-p "\\sw")
                 (looking-back ":" nil)))
    (save-excursion (just-one-space)))
  (when (and (not (looking-back "^.?" nil))
             (save-excursion
               (backward-char 2)
               (or (looking-at-p ".[^:] [.([,;]")
                   (looking-at-p ".. ::")
                   (looking-at-p ".[.@$] ")
                   (looking-at-p ":: "))))
    (delete-char 1)))

(defun sp-crystal-delete-indentation (&optional arg)
  "Better way of joining crystal lines.

ARG is how many indentation to delete."
  (delete-indentation arg)
  (sp-crystal-maybe-one-space))

(defun sp-crystal-block-post-handler (id action context)
  "Handler for crystal block-like insertions.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))
  (sp-crystal-post-handler id action context))

(defun sp-crystal-def-post-handler (id action context)
  "Handler for crystal def-like insertions.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (insert "x")
      (newline)
      (indent-according-to-mode))
    (delete-char 1))
  (sp-crystal-post-handler id action context))

(defun sp-crystal-post-handler (_id action _context)
  "Crystal post handler.
ID, ACTION, CONTEXT."
  (-let (((&plist :arg arg :enc enc) sp-handler-context))
    (when (equal action 'barf-backward)
      (sp-crystal-delete-indentation 1)
      (indent-according-to-mode)
      (save-excursion
        (sp-backward-sexp) ; move to begining of current sexp
        (sp-backward-sexp arg)
        (sp-crystal-maybe-one-space)))

    (when (equal action 'barf-forward)
      (sp-get enc
        (let ((beg-line (line-number-at-pos :beg-in))
              (end-line (line-number-at-pos :end-in)))
          (sp-forward-sexp arg)
          (sp-crystal-maybe-one-space)
          (when (not (= (line-number-at-pos) beg-line))
            (sp-crystal-delete-indentation -1))
          (indent-according-to-mode))))))

(defun sp-crystal-pre-handler (_id action _context)
  "Handler for crystal slurp and barf.
ID, ACTION, CONTEXT."
  (let ((enc (plist-get sp-handler-context :enc)))
    (sp-get enc
      (let ((beg-line (line-number-at-pos :beg-in))
            (end-line (line-number-at-pos :end-in)))

        (when (equal action 'slurp-backward)
          (save-excursion
            (sp-forward-sexp)
            (when (looking-at-p ";") (forward-char))
            (sp-crystal-maybe-one-space)
            (when (not (= (line-number-at-pos) end-line))
              (sp-crystal-delete-indentation -1)))
          (when (looking-at-p "::")
            (while (and (looking-back "\\sw" nil)
                        (--when-let (sp-get-symbol t)
                          (sp-get it (goto-char :beg-prf))))))
          (while (thing-at-point-looking-at "\\.[[:blank:]\n]*")
            (sp-backward-sexp))
          (when (looking-back "[@$:&?!]" nil)
            (backward-char)
            (when (looking-back "[@&:]" nil)
              (backward-char)))
          (just-one-space)
          (save-excursion
            (if (= (line-number-at-pos) end-line)
                (insert " ")
              (newline))))

        (when (equal action 'barf-backward)
          ;; Barf whole method chains
          (while (thing-at-point-looking-at "[(.:[][\n[:blank:]]*")
            (sp-forward-sexp))
          (if (looking-at-p " *$")
              (newline)
            (save-excursion (newline))))

        (when (equal action 'slurp-forward)
          (save-excursion
            (sp-backward-sexp)
            (when (looking-back "\." nil) (backward-char))
            (sp-crystal-maybe-one-space)
            (when (not (= (line-number-at-pos) beg-line))
              (if (thing-at-point-looking-at "\\.[[:blank:]\n]*")
                  (progn
                    (forward-symbol -1)
                    (sp-crystal-delete-indentation -1))
                (sp-crystal-delete-indentation))))
          (while (looking-at-p "::") (sp-forward-symbol))
          (when (looking-at-p "[?!;]") (forward-char))
          (if (= (line-number-at-pos) beg-line)
              (insert " ")
            (newline)))

        (when (equal action 'barf-forward)
          (when (looking-back "\\." nil) (backward-char))
          (when (looking-at-p "::")
            (while (and (looking-back "\\sw" nil)
                        (--when-let (sp-get-symbol t)
                          (sp-get it (goto-char :beg-prf))))))
          (if (= (line-number-at-pos) end-line)
              (insert " ")
            (if (looking-back "^[[:blank:]]*" nil)
                (save-excursion (newline))
              (newline))))))))

(defun sp-crystal-inline-p (id)
  "Test if ID is inline."
  (save-excursion
    (when (looking-back id nil)
      (backward-word))
    (when (not (or (looking-back "^[[:blank:]]*" nil)
                   (looking-back "= *" nil)))
      (or (save-excursion
            (forward-symbol -1)
            (forward-symbol 1)
            (looking-at-p (concat " *" id)))
          (save-excursion
            ;; This does not seem to make emacs snapshot happy
            (ignore-errors
              (sp-crystal-backward-sexp)
              (sp-crystal-forward-sexp)
              (looking-at-p (concat "[^[:blank:]]* *" id))))))))

(defun sp-crystal-method-p (id)
  "Test if ID is a method."
  (save-excursion
    (when (looking-back id nil)
      (backward-word))
    (and (looking-at-p id)
         (or
          ;; fix for def_foo
          (looking-at-p (concat id "[_?!:]"))
          ;; fix for foo_def
          (looking-back "[_:@$.]" nil)
          ;; fix for def for; end
          (looking-back "def \\|class \\|module " nil)
          ;; Check if multiline method call
          ;; But beware of comments!
          (and (looking-back "\\.[[:blank:]\n]*" nil)
               (not (save-excursion
                      (search-backward ".")
                      (sp-point-in-comment))))))))

(defun sp-crystal-skip-inline-match-p (ms _mb _me)
  "If non-nil, skip inline match.
MS, MB, ME."
  (or (sp-crystal-method-p ms)
      (sp-crystal-inline-p ms)))

(defun sp-crystal-skip-method-p (ms _mb _me)
  "If non-nil, skip method.
MS, MB, ME."
  (sp-crystal-method-p ms))

(defun sp-crystal-in-string-or-word-p (id action context)
  "Test if point is inside string or word.
ID, ACTION, CONTEXT."
  (or (sp-in-string-p id action context)
      (and (looking-back id nil)
           (not (looking-back (sp--strict-regexp-quote id) nil)))
      (sp-crystal-method-p id)))

(defun sp-crystal-in-string-word-or-inline-p (id action context)
  "Test if point is inside string, word or inline.
ID, ACTION, CONTEXT."
  (or (sp-crystal-in-string-or-word-p id action context)
      (and (looking-back id nil)
           (sp-crystal-inline-p id))))

(defun sp-crystal-pre-pipe-handler (id action _context)
  "Crystal pipe handler.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (just-one-space))
    (save-excursion
      (search-backward id)
      (just-one-space))))

(defun sp-crystal-should-insert-pipe-close (id action _context)
  "Test whether to insert the closing pipe for a lambda-binding pipe pair.
ID, ACTION, CONTEXT"
  (if (eq action 'insert)
      (thing-at-point-looking-at
       (rx-to-string `(and (or "do" "{") (* space) ,id)))
    t))

(defun sp--crystal-skip-match (ms me mb)
  "Crystal skip match.
MS, ME, MB."
  (when (string= ms "end")
    (or (sp-in-string-p ms me mb)
        (sp-crystal-method-p "end"))))

(add-to-list 'sp-navigate-skip-match
             '((crystal-mode motion-mode) . sp--crystal-skip-match))

(dolist (mode '(crystal-mode motion-mode))
  (add-to-list 'sp-sexp-suffix `(,mode syntax "")))

(sp-with-modes '(crystal-mode motion-mode)
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-block-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "{" "}"
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-post-handler)
                 :suffix "")

  (sp-local-pair "begin" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-block-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "def" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "class" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "struct" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "lib" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "fun" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "enum" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "union" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "module" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "macro" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "case" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-method-p
                 :suffix "")

  (sp-local-pair "if" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "unless" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "while" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "until" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-crystal-pre-handler)
                 :post-handlers '(sp-crystal-def-post-handler)
                 :skip-match 'sp-crystal-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "|" "|"
                 :when '(sp-crystal-should-insert-pipe-close)
                 :pre-handlers '(sp-crystal-pre-pipe-handler)
                 :suffix ""))

(provide 'smartparens-crystal)

;;; smartparens-crystal.el ends here
