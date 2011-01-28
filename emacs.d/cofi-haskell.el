(add-to-list 'load-path "~/.elisp/vendor/haskell-mode/")
(add-to-list 'load-path "~/.elisp/ghc-mod/")
(load-and-exec "haskell-site-file"
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  )
(require-and-exec 'ghc
                  (add-hook 'haskell-mode-hook 'ghc-init))

(provide 'cofi-haskell)
