;;; sp-handle-enter.el --- Handle situation when insert pair and press enter  -*- lexical-binding: t; -*-
;;
;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Created: 16 January 2022
;; Keywords: convenience editing
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
;; This file configuation to make smartparens more organize
;; and add organize handling of new line in parens.  Example of handling:
;;
;;    function print() {|}
;;
;; press enter:
;;
;;    function print() {
;;        |
;;    }
;;
;; In haskell
;;
;;    type Locator a = {|}
;;
;; press enter:
;;
;;    type Locator a = {|
;;                     }
;;
;; For use this able type
;;
;;     (require 'sp-newline)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;;; Code:

(require 'smartparens)
(require 'dash)


(defgroup sp-handle-enter nil
  "Handle enter in parens for Smartparens."
  :group 'smartparens)


(defcustom sp-handle-enter-in-quotes t
  "When non-nil, if user type newline, handle it."
  :group 'sp-handle-enter
  :type 'bool)


(defcustom sp-handle-enter-in-parens '("{" "(" "[")
  "List of opening parens in which will handle Enter."
  :group 'sp-handle-enter
  :type '(repeat string))


(--each sp-handle-enter-in-parens
  (sp-pair it nil :post-handlers '(("||\n[i]" "RET"))))


(sp-with-modes '(haskell-mode haskell-interactive-mode)
  (sp-local-pair "{" nil :post-handlers '((sp-handle-enter--open-{ "RET"))))


(defun sp-handle-enter--haskell-open-{ (&rest _ignored)
    "Haskell {} block for `smartparens'.
For example:
    newtype Book = {
|}
to:
    newtype Book = {|
                   }"
  (interactive)
  (let* ((prev-line-len (save-excursion
                            (forward-line -1)
                            (length (s-chop-suffix "\n"
                                                   (thing-at-point 'line t))))))
      (indent-to prev-line-len)
      (forward-line -1)
      (end-of-line))
  )

(provide 'sp-handle-enter)

;;; sp-handle-enter.el ends here
