;;; smartparens-ml.el --- Additional configuration for ML languages

;; Copyright (C) 2016 Ta Quang Trung

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;; Maintainer: Ta Quang Trung <taquangtrung@gmail.com>
;; Created: 14 July 2016
;; Keywords: abbrev convenience editing
;; URL: https://github.com/taquangtrung/smartparens

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

;; This fixes slurping from adding space before or after "." if not needed
;;
;;   Forward-slurp:
;;     UNWANTED:
;;       (foo) List.map  ~~> (foo List).map ~~ (foo List .map)
;;     EXPECTED:
;;       (foo) List.map  ~~> (foo List).map ~~ (foo List.map)
;;
;;   Backward-slurp:
;;     UNWANTED:
;;       List.map (foo) ~~> List(.map foo) ~~> (List .map foo)
;;     EXPECTED:
;;       List.map (foo) ~~> List(.map foo) ~~> (List.map foo)

(sp-with-modes '(tuareg-mode fsharp-mode)
  (sp-local-pair "{" nil :pre-handlers '(sp-ml-pre-slurp-handler))
  (sp-local-pair "(" nil :pre-handlers '(sp-ml-pre-slurp-handler))
  (sp-local-pair "[" nil :pre-handlers '(sp-ml-pre-slurp-handler)))

(defun sp-ml-pre-slurp-handler (id action context)
  "ML slurp handler.
ID, ACTION, CONTEXT."
  ;; If there was no space before or after, we shouldn't add on.
  ;; Variable ok, next-thing are defined in `sp-forward-slurp-sexp'
  (-let (((&plist :ok ok :next-thing next-thing) sp-handler-context))
    (when (eq action 'slurp-forward)
      (save-excursion
        (when (and (sp-get ok (/= :len-in 0))
                   (= (sp-get ok :end-suf) (sp-get next-thing :beg-prf)))
          (goto-char (sp-get ok :end))
          (when (looking-back " ")
            (delete-char -1)))))

    (when (eq action 'slurp-backward)
      (save-excursion
        (when (and (sp-get ok (/= :len-in 0))
                   (= (sp-get ok :beg-prf) (sp-get next-thing :end-suf)))
          (goto-char (sp-get ok :beg))
          (when (looking-at " ")
            (delete-char 1)))))))

(provide 'smartparens-ml)
;;; smartparens-ml.el ends here
