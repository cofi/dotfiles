(add-to-loadpath "~/.elisp/vendor/emacs-eclim"
                 "~/.elisp/vendor/emacs-eclim/vendor/")

(require-and-exec 'eclim
                  (add-hook 'java-mode-hook 'eclim-mode)
                  (setq eclim-auto-save t
                        eclim-use-yasnippet nil)
                  )

(provide 'cofi-java)
