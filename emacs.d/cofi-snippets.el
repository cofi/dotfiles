(require-and-exec 'yasnippet
  (lambda ()
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/snippets"))

  (global-set-key (kbd "M-RET") 'yas/expand)
)
(provide 'cofi-snippets)
