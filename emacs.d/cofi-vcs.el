(dolist (path '(
                "~/.elisp/vendor/magit"
                "~/.elisp/vendor/monky"
                ))
  (add-to-list 'load-path path))

(setq vc-handled-backends '(SVN)
      vc-follow-symlinks t)

;;; (ma)git
(global-set-key (kbd "C-c g") 'magit-status)
(vim-mapleader-add "g" 'magit-status)
(setq magit-commit-signoff t
      magit-completing-read-function 'magit-ido-completing-read
      magit-commit-all-when-nothing-staged nil)

;;; overwrite magit-quit-window with a more sensible fun
(defun cofi/vcs-quit-window (&optional kill-buffer)
  (interactive "P")
  (if (one-window-p)
      (if kill-buffer
          (kill-buffer (current-buffer))
        (bury-buffer))
    (quit-window kill-buffer (selected-window))))
(eval-after-load "magit"
  '(fset 'magit-quit-window #'cofi/vcs-quit-window))

;;; (a)hg
(vim-mapleader-add "h" 'monky-status)
(eval-after-load "monky"
    '(fill-keymap monky-status-mode-map
                  "q" 'cofi/vcs-quit-window))

(provide 'cofi-vcs)
