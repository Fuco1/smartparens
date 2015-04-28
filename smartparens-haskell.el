(require 'smartparens)

(sp-with-modes '(haskell-mode haskell-interactive-mode)
  (sp-local-pair "{-#" "#-}"))


(provide 'smartparens-haskell)

;;; smartparens-haskell.el ends here
