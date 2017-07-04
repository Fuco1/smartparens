;;; smartparens-haskell.el --- Additional configuration for Haskell based modes.

;; Copyright (C) 2015 Michael Xavier

;; Author: Michael Xavier <michael@michaelxavier.net>
;; Maintainer: Michael Xavier <michael@michaelxavier.net>
;; Created: 29 Apr 2016
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

;; This file provides some additional configuration for Haskell based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-haskell)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.

;; If you have good ideas about what should be added please file an
;; issue on the github tracker.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:
(require 'smartparens)

(sp-with-modes '(haskell-mode haskell-interactive-mode)
  (sp-local-pair "{-" "-}")
  (sp-local-pair "{-#" "#-}")
  (sp-local-pair "'" nil :unless '(sp-point-after-word-p))
  (sp-local-pair "\\(" nil :actions nil))

(defun sp--inferior-haskell-mode-backward-bound-fn ()
  "Limit the backward search to the prompt if point is on prompt."
  (-when-let (limit (cond ((bound-and-true-p comint-last-prompt)
                           (marker-position (cdr comint-last-prompt)))
                          ((bound-and-true-p comint-last-prompt-overlay)
                           (overlay-end comint-last-prompt-overlay))
                          (t nil)))
    (and (> (point) limit) limit)))

(defun sp--inferior-haskell-mode-forward-bound-fn ()
  "Limit the forward search to exclude the prompt if point is before prompt."
  (-when-let (limit (cond ((bound-and-true-p comint-last-prompt)
                           (marker-position (car comint-last-prompt)))
                          ((bound-and-true-p comint-last-prompt-overlay)
                           (overlay-start comint-last-prompt-overlay))
                          (t nil)))
    (and (< (point) limit) limit)))

(defun sp--setup-inferior-haskell-mode-search-bounds ()
  "Setup the search bound.

If the point is after the last prompt, limit the backward search
only for the propmt.

If the point is before the last prompt, limit the forward search up until the prompt start."
  (setq sp-forward-bound-fn 'sp--inferior-haskell-mode-forward-bound-fn)
  (setq sp-backward-bound-fn 'sp--inferior-haskell-mode-backward-bound-fn))

(add-hook 'inferior-haskell-mode-hook 'sp--setup-inferior-haskell-mode-search-bounds)

(provide 'smartparens-haskell)

;;; smartparens-haskell.el ends here
