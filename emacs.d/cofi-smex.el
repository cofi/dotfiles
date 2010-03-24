(load "cofi-util")
(require-and-exec 'smex
  (lambda ()
    (progn
      (smex-initialize)
      (global-set-key (kbd "M-a") 'smex)
      (global-set-key (kbd "M-x") 'smex)
      )
    ))

(provide 'cofi-smex)
