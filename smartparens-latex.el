;;; smartparens-latex.el --- Additional configuration for (La)TeX based modes.

;; Copyright (C) 2012-2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 14 Feb 2013
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

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

;;; Commentary:

;; This file provides some additional configuration for (La)TeX based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-latex)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.

;; If you have good ideas about what should be added please file an
;; issue on the github tracker.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

(sp-with-modes '(
                 tex-mode
                 plain-tex-mode
                 latex-mode
                 )
  ;; disable useless pairs.  Maybe also remove " ' and \"?
  (sp-local-pair "/*" nil :actions nil)
  (sp-local-pair "\\\\(" nil :actions nil)

  ;; add the prefix funciton sticking to {} pair
  (sp-local-pair "{" nil :prefix "\\\\\\(\\sw\\|\\s_\\)*")

  ;; pairs for big brackets.  Needs more research on what pairs are
  ;; useful to add here.  Post suggestions if you know some.
  (sp-local-pair "\\left(" "\\right)")
  (sp-local-pair "\\left{" "\\right}")
  (sp-local-pair "\\big(" "\\big)")
  (sp-local-pair "\\bigg(" "\\bigg)")
  (sp-local-pair "\\Big(" "\\Big)")
  (sp-local-pair "\\Bigg(" "\\Bigg)")
  (sp-local-pair "\\big{" "\\big}")
  (sp-local-pair "\\bigg{" "\\bigg}")
  (sp-local-pair "\\Big{" "\\Big}")
  (sp-local-pair "\\Bigg{" "\\Bigg}")

  ;; some common wrappings
  (sp-local-tag "bi" "\\begin{itemize}" "\\end{itemize}")
  (sp-local-tag "be" "\\begin{enumerate}" "\\end{enumerate}"))

(provide 'smartparens-latex)

;;; smartparens-latex.el ends here
