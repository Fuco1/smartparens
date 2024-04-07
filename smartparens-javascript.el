;;; smartparens-javascript.el --- Additional configuration for JavaScript based modes.  -*- lexical-binding: t; -*-

;; Copyright (c) 2017-2019 Marinin Tim
;; Author: Tim Marinin <mt@marinin.xyz>
;; Maintainer: Tim Marinin <mt@marinin.xyz>
;; Created: 2017-03-03
;; Keywords: abbrev convenience editing javascript
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

;; This file provides some additional configuration for JavaScript based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-javascript)
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

(defvar sp--javascript-modes '(
                               js-mode
                               javascript-mode
                               js2-mode
                               typescript-mode
                               rjsx-mode
                               js-ts-mode
                               typescript-ts-mode
                               tsx-ts-mode
                               )
  "List of JavaScript modes.")

;; (|sys).path.append---the dot should not travel with the closing
;; paren
(--each sp--javascript-modes
  (add-to-list 'sp-sexp-suffix (list it 'regexp "")))

(defun sp-javascript-skip-match-angle-bracket (ms mb _me)
  "Non-nil if we should ignore the bracket as valid delimiter."
  (and (string= ms ">") (= (char-before mb) ?=)))

(sp-with-modes sp--javascript-modes
  (sp-local-pair "<" ">"
                 :skip-match 'sp-javascript-skip-match-angle-bracket))

(provide 'smartparens-javascript)
;;; smartparens-javascript.el ends here
