(setq viper-mode t)
(require 'viper)

(setq viper-shift-width 4)
(setq viper-re-search t)

;; vimpulse settings
(when (load "vimpulse" t)
    (setq viper-ex-style-editing -1)
    (setq vimpulse-experimental -1)
)

(when (load "goto-last-change" t)
  (define-key viper-vi-global-user-map (kbd "g i") 'goto-last-change))

(provide 'cofi-vim)
