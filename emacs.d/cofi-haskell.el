(load-and-exec "haskell-site-file"
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  )

(provide 'cofi-haskell)
