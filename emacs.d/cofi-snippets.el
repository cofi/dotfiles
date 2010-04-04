(require-and-exec 'yasnippet
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/snippets")

  (global-set-key (kbd "M-RET") 'yas/expand)
)
(provide 'cofi-snippets)
