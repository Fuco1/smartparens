;;; smartparens-config.el --- Default configuration for smartparens package

;; Copyright (C) 2012 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 30 Jan 2013
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

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with-modes '(
                 emacs-lisp-mode
                 inferior-emacs-lisp-mode
                 lisp-interaction-mode
                 scheme-mode
                 common-lisp-mode
                 )
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" nil :when '(sp-in-string-p)))

;; NOTE: Normally, `sp-local-pair' accepts list of modes (or a single
;; mode) as a first argument.  The macro `sp-with-modes' adds this
;; automatically.  If you want to call sp-local-pair outside this
;; macro, you MUST supply the major mode argument.

;; markdown based modes
(sp-with-modes '(
                 markdown-mode
                 gfm-mode
                 rst-mode
                 )
  ;; overload the `' pair with ``, which is used for inline
  ;; code in markdown
  (sp-local-pair "`" "`"))

;; LaTeX modes
(sp-with-modes '(
                 tex-mode
                 plain-tex-mode
                 latex-mode
                 )
  ;; math modes, yay.  The :actions are provided automatically if
  ;; these pairs do not have global definition.
  (sp-local-pair "$" "$")
  (sp-local-pair "\\[" "\\]")
  (sp-local-tag "\\b" "\\begin{_}" "\\end{_}"))

;; html modes
(sp-local-tag '(sgml-mode html-mode) "<" "<_>" "</_>" :transform 'sp-match-sgml-tags)

(provide 'smartparens-config)

;;; smartparens-config.el ends here
