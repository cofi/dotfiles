(require-and-exec 'flymake
  (defun flymake-show-next-error ()
    "Shows next flymake error."
    (interactive)
    (flymake-goto-next-error)
    (flymake-display-err-menu-for-current-line))

  (defun flymake-show-prev-error ()
    "Shows previous flymake error."
    (interactive)
    (flymake-goto-prev-error)
    (flymake-display-err-menu-for-current-line))

  (global-set-key (kbd "<f5>") 'flymake-show-prev-error)
  (global-set-key (kbd "<f6>") 'flymake-show-next-error))

(provide 'cofi-flymake)
