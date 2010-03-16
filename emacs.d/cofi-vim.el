(setq viper-mode t)
(require 'viper)
(require 'goto-last-change)
(load-library "vimpulse")

(setq viper-shift-width 4)
(setq viper-re-search t)

;; vimpulse settings
(setq viper-ex-style-editing -1)
(setq vimpulse-experimental -1)

(define-key viper-vi-global-user-map (kbd "g i") 'goto-last-change)

(provide 'cofi-vim)
