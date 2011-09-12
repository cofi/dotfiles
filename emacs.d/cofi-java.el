(add-to-loadpath "~/.elisp/vendor/emacs-eclim"
                 "~/.elisp/vendor/emacs-eclim/vendor/")

(setq eclim-auto-save t
      eclim-use-yasnippet nil)
(autoload 'eclim-mode "eclim" nil t)
(add-hook 'java-mode-hook 'eclim-mode)

(provide 'cofi-java)
