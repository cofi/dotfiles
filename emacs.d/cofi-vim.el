(setq viper-mode t)
(require 'viper)

(setq viper-shift-width 4)
(setq viper-re-search t)
(setq viper-ex-style-editing nil)

(when (load "vimpulse" t)
  (define-key viper-vi-global-user-map (kbd "r") 'redo)
  (define-key viper-vi-global-user-map (kbd "C-r") 'isearch-backward-regexp)
  (define-key viper-vi-global-user-map (kbd "C-s") 'isearch-forward-regexp)

  (define-key viper-vi-basic-map (kbd "/") 'isearch-forward-regexp)
  (define-key viper-vi-basic-map (kbd "?") 'isearch-backward-regexp)

  (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
)

(when (load "goto-last-change" t)
  (define-key viper-vi-global-user-map (kbd "g i") 'goto-last-change)
  )

(provide 'cofi-vim)
