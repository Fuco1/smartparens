;;; smartparens-clojure.el --- Additional configuration for Clojure mode  -*- lexical-binding: t; -*-

;; Author: Vitalie Spinu <spinuvit@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 14 July 2016

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

;; This file provides some additional configuration for clojure based
;; modes.  To use it, simply add:

;; (require 'smartparens-clojure)

;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.

;; If you have good ideas about what should be added please file an
;; issue on the github tracker.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

(defvar sp-clojure-prefix "\\(?:[@`'#~,_?^]+\\)"
  "Prefix used in `sp-sexp-prefix' for clojure modes.")

(dolist (mode '(clojure-mode clojurescript-mode clojurec-mode cider-repl-mode))
  (add-to-list 'sp-sexp-prefix `(,mode regexp ,sp-clojure-prefix)))

;; Match "`" with "`" in strings and comments
(sp-with-modes sp-clojure-modes
  (sp-local-pair "`" "`"
                 :when '(sp-in-string-p
                         sp-in-comment-p)
                 :unless '(sp-lisp-invalid-hyperlink-p)))

(provide 'smartparens-clojure)
;;; smartparens-clojure.el ends here
