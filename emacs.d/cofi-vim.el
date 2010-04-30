(setq viper-mode t)
(setq viper-toggle-key (kbd "<pause>"))
(require 'viper)

(setq viper-shift-width 4)
(setq viper-re-search t)
(setq viper-ex-style-editing nil)

(require-and-exec 'vimpulse
  (define-key viper-vi-global-user-map (kbd "r") 'redo)
  (define-key viper-vi-global-user-map (kbd "C-r") 'isearch-backward-regexp)
  (define-key viper-vi-global-user-map (kbd "C-s") 'isearch-forward-regexp)
  (define-key viper-vi-global-user-map (kbd "_") 'viper-bol-and-skip-white)
  (define-key viper-vi-global-user-map (kbd "+") 'viper-next-line-at-bol)
  (define-key viper-vi-global-user-map (kbd "Y") (kbd "y$"))
  (define-key vimpulse-visual-basic-map (kbd "g q") 'fill-region)
  (define-key viper-insert-global-user-map (kbd "j j") 'viper-exit-insert-state)

  (define-key viper-vi-basic-map (kbd "/") 'isearch-forward-regexp)
  (define-key viper-vi-basic-map (kbd "?") 'isearch-backward-regexp)

  (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
)

(require-and-exec 'goto-last-change
  (define-key viper-vi-global-user-map (kbd "g i") 'goto-last-change)
)

(provide 'cofi-vim)
