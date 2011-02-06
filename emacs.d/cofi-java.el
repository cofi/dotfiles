(add-to-list 'load-path "~/.elisp/vendor/emacs-eclim")
(add-to-list 'load-path "~/.elisp/vendor/emacs-eclim/vendor/")

(require-and-exec 'eclim
                  (add-hook 'java-mode-hook 'eclim-mode)
                  (setq eclim-auto-save t
                        eclim-use-yasnippet nil)
                  )

(provide 'cofi-java)
