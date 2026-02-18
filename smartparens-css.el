;;; smartparens-css.el --- Additional configuration for CSS mode.  -*- lexical-binding: t; -*-
;;
;; Author: Konstantin Kharlamov <Hi-Angel@yandex.ru>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 25 December 2024
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
;; This file configuation to make smartparens insertion behavae similarly to
;; SublimeText editor.  To use it, simply add:
;;
;;     (require 'smartparens-css)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;;; Code:


(require 'smartparens)

(sp-local-pair 'css-mode "/*" "*/")

(provide 'smartparens-css)
;;; smartparens-css.el ends here
