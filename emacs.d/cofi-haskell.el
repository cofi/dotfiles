(when (load "haskell-site-file" t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  )

(provide 'cofi-haskell)
