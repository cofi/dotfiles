(dolist (path '(
                "~/.elisp/vendor/magit"
                "~/.elisp/ahg"
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
(defun cofi/magit-quit-window (&optional kill-buffer)
  (interactive "P")
  (if (one-window-p)
      (if kill-buffer
          (kill-buffer (current-buffer))
        (bury-buffer))
    (quit-window kill-buffer (selected-window))))
(eval-after-load "magit"
  '(fset 'magit-quit-window #'cofi/magit-quit-window))

;;; (a)hg
(require 'ahg)
(setq ahg-global-key-prefix (kbd "C-c h"))
(vim-mapleader-add "h" 'ahg-status)
(vim-mapleader-add "H" ahg-global-map)

(fill-keymap ahg-status-mode-map
             "d" 'ahg-status-diff
             "<tab>" 'ahg-status-diff
             ":" 'ahg-status-do-command
             "n" 'next-line
             "p" 'previous-line
             "P" (cmd (ahg-do-command "push"))
             "F" (cmd (ahg-do-command "fetch"))
             )

(fill-keymap ahg-command-mode-map
             "q" 'quit-window
             "Q" 'kill-buffer-and-window)

(provide 'cofi-vcs)
