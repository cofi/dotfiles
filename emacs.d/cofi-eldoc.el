(require-and-exec 'eldoc
  (add-hook 'python-mode-hook (lambda ()
                                (eldoc-mode t)))
  (eldoc-add-command 'autopair-insert-opening)
  )

(provide 'cofi-eldoc)
