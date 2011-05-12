(dolist (hook `(flyspell-mode-on
                turn-on-auto-fill
                ,(turn-on auto-dictionary-mode)))

  (add-hook 'rst-mode-hook hook))

(provide 'cofi-rst)
