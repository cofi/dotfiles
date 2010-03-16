(when (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-a") 'smex)
  (global-set-key (kbd "M-x") 'smex)
)

(provide 'cofi-smex)
