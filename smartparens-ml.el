;;; smartparens-ml.el --- Additional configuration for ML languages

;; Author: tddsg
;; Created: 14 July 2016
;; Updated: 29 April 2017
;; Keywords: smartparens, ML, ocaml, reason

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
(sp-with-modes '(tuareg-mode) (sp-local-pair "(*" "*)" ))
(sp-with-modes '(reason-mode) (sp-local-pair "/*" "*/" ))

(provide 'smartparens-ml)
;;; smartparens-ml.el ends here
