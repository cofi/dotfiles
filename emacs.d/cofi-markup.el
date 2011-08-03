(let ((modes `(flyspell-mode-on
               turn-on-auto-fill
               ,(turn-on auto-dictionary-mode))))

  (add-all-to-hook 'rst-mode-hook modes)

  (add-all-to-hook 'markdown-mode-hook modes))

(provide 'cofi-markup)
