(add-to-loadpath "~/.elisp/vendor/haskell-mode/"
                 "~/.elisp/ghc-mod/")

(load-and-exec "haskell-site-file"
  (add-all-to-hook 'haskell-mode-hook '(turn-on-haskell-doc-mode
                                        turn-on-haskell-indentation)))
(require-and-exec 'ghc
  (add-hook 'haskell-mode-hook 'ghc-init))

(provide 'cofi-haskell)
