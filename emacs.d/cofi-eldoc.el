(when (require 'eldoc nil 'noerror)
  (add-hook 'python-mode-hook (lambda ()
                                (eldoc-mode t)))
  (eldoc-add-command 'autopair-insert-opening)
  )

(provide 'cofi-eldoc)
