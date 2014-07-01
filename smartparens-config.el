;;; smartparens-config.el --- Default configuration for smartparens package

;; Copyright (C) 2013-2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 30 Jan 2013
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

;; This is a default configuration for smartparens package.  If you
;; wish to set up everything by yourself, you can instead require
;; smartparens directly.

;; However, some configuration is always loaded by default, most
;; notably the built-in list of supported pairs.  If you want to erase
;; this list, simply use (setq sp-pairs nil) and then add your own
;; pairs.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

;; do not autoinsert ' pair if the point is preceeded by word.  This
;; will handle the situation when ' is used as a contraction symbol in
;; natural language.  Nil for second argument means to keep the
;; original definition of closing pair.
(sp-pair "'" nil :unless '(sp-point-after-word-p))

(defun sp-lisp-invalid-hyperlink-p (_ action _)
  (when (eq action 'navigate)
    (or (and (looking-at "\\sw\\|\\s_")
             (save-excursion
               (backward-char 2)
               (looking-at "\\sw\\|\\s_")))
        (and (save-excursion
               (backward-char 1)
               (looking-at "\\sw\\|\\s_"))
             (save-excursion
               (forward-char 1)
               (looking-at "\\sw\\|\\s_"))))))

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'"
                 :when '(sp-in-string-p
                         sp-in-comment-p)
                 :unless '(sp-lisp-invalid-hyperlink-p)
                 :skip-match (lambda (ms mb me)
                               (cond
                                ((equal ms "'")
                                 (or (sp-lisp-invalid-hyperlink-p "`" 'navigate '_)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

;; NOTE: Normally, `sp-local-pair' accepts list of modes (or a single
;; mode) as a first argument.  The macro `sp-with-modes' adds this
;; automatically.  If you want to call sp-local-pair outside this
;; macro, you MUST supply the major mode argument.

(--each sp--html-modes
  (eval-after-load (symbol-name it) '(require 'smartparens-html)))
(eval-after-load "latex"         '(require 'smartparens-latex))
(eval-after-load "tex-mode"      '(require 'smartparens-latex))
(eval-after-load "lua-mode"      '(require 'smartparens-lua))
(eval-after-load "ruby-mode"     '(require 'smartparens-ruby))
(eval-after-load "enh-ruby-mode" '(require 'smartparens-ruby))

(provide 'smartparens-config)

;;; smartparens-config.el ends here
