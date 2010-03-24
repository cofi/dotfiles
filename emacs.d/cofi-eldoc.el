(load "cofi-util")
(require-and-exec'eldoc (lambda ()
                          (progn
                            (eldoc-add-command 'autopair-insert-opening)

                            (add-hook 'python-mode-hook (lambda ()
                                                          (eldoc-mode t)))
                            )))

(provide 'cofi-eldoc)
