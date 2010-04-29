(require-and-exec 'yasnippet
    (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/snippets")

    (global-set-key (kbd "M-RET") 'yas/expand)
)
(provide 'cofi-snippets)
