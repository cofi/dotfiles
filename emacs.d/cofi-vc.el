(add-hook 'vc-dir-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "j") 'vc-dir-next-line)
              (local-set-key (kbd "k") 'vc-dir-previous-line)
              )))

(provide 'cofi-vc)
