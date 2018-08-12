;;; smartparens-ml.el --- Additional configuration for ML languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 Ta Quang Trung
;; Copyright (C) 2017 Matus Goljer

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;;         Matus Goljer <matus.goljer@gmail.com>
;;         Louis Roché <louis@louisroche.net>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 14 July 2016
;; Keywords: smartparens, ML, ocaml, reason
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

;; This file provides some additional configuration for ML languages.
;; To use it, simply add:
;;
;; (require 'smartparens-ml)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;; If you have good ideas about what should be added please file an
;; issue on the github tracker.
;;
;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

;;; Local pairs for ML-family languages

(sp-with-modes '(fsharp-mode)
  (sp-local-pair "(*" "*)" ))

(sp-with-modes '(tuareg-mode)
  ;; Disable ` because it is used in polymorphic variants
  (sp-local-pair "`" nil :actions nil)
  ;; Disable ' because it is used in value names and types
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "(*" "*)" )
  ;; Because we want to support strings like {| { |}, this pair has to
  ;; be disabled in strings. Is this behavior complete? It has to
  ;; detect correctly the matching pairs when using the {id| |id}
  ;; syntax.
  (sp-local-pair "{" "}" :unless '(sp-in-string-p))
  (sp-local-pair "if" "then"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "while" "done"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "for" "done"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "begin" "end"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "struct" "end"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "sig" "end"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "object" "end"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "match" "with"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "try" "with"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair)))

(sp-with-modes '(reason-mode)
  ;; Disable ` because it is used in polymorphic variants
  (sp-local-pair "`" nil :actions nil)
  ;; Disable ' because it is used in value names and types
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "/*" "*/" ))

(provide 'smartparens-ml)
;;; smartparens-ml.el ends here
