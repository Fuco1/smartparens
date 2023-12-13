;;; smartparens-clojure.el --- Additional configuration for Emacs Lisp mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Daniel Ziltener

;; Author: Daniel Ziltener <dziltener@lyrion.ch>
;; Created: 13 December 2023
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;;
;; This file is part of Smartparens.
;;
;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This file provides some additional configuration for Emacs Lisp mode.  To use
;; it, simply add:
;;
;; (require 'smartparens-emacs-lisp)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;;; Code:

(require 'smartparens)

(defvar sp-emacs-lisp-prefix "\\(?:[@`'#,_]+\\)"
  "Prefix used in `sp-sexp-prefix' for emacs lisp mode.")

(defvar sp-emacs-lisp-modes '(emacs-lisp-mode lisp-data-mode)
  "List of Emacs Lisp-related modes.")

(dolist (mode sp-emacs-lisp-modes)
  (add-to-list 'sp-sexp-prefix `(,mode regexp ,sp-emacs-lisp-prefix)))

;; Match "`" with "`" in strings and comments
(sp-with-modes sp-emacs-lisp-modes
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "`" "`"
                 :when '(sp-in-string-p
                         sp-in-comment-p)))

(provide 'smartparens-emacs-lisp)
;;; smartparens-emacs-lisp.el ends here

