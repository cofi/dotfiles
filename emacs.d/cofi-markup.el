(let ((modes `(flyspell-mode
               auto-fill-mode
               auto-dictionary-mode)))

  (add-to-hook 'rst-mode-hook modes)

  (add-to-hook 'markdown-mode-hook modes))

(provide 'cofi-markup)
