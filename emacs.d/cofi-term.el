(require-and-exec 'multi-term
                  (global-set-key (kbd "<f1>") 'multi-term))

(add-hook 'term-mode-hook
          (lambda ()
            (linum-mode -1)
            ))

(provide 'cofi-term)
