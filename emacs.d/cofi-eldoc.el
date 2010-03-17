(require 'eldoc)
(eldoc-add-command 'autopair-insert-opening)

(add-hook 'python-mode-hook (lambda ()
                              (eldoc-mode t)))

(provide 'cofi-eldoc)
