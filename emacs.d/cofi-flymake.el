(when (featurep 'flymake)
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

  (global-set-key (kbd "C-N") 'flymake-show-next-error)
  (global-set-key (kbd "C-P") 'flymake-show-prev-error))

(provide 'cofi-flymake)
