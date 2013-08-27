(add-to-loadpath "~/.elisp/ghc-mod/")
(add-to-hook 'haskell-mode-hook '(ghc-init
                                  haskell-doc-mode
                                  haskell-indentation-mode))

(provide 'cofi-haskell)
