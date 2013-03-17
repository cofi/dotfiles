(add-to-loadpath "~/.elisp/vendor/magit"
                 "~/.elisp/vendor/monky")

(setq vc-handled-backends '(SVN)
      vc-follow-symlinks t)

;;; (ma)git
(cofi/set-key 'global "C-c g" 'magit-status)
(setq magit-commit-signoff t
      magit-completing-read-function 'magit-ido-completing-read
      magit-remote-ref-format 'remote-slash-name
      magit-commit-all-when-nothing-staged nil)
(add-hook 'magit-commit-mode-hook 'visual-line-mode)

;;; overwrite magit-quit-window with a more sensible fun
(defun cofi/vcs-quit-window (&optional kill-buffer)
  (interactive "P")
  (if (one-window-p)
      (if kill-buffer
          (kill-buffer (current-buffer))
        (bury-buffer))
    (quit-window kill-buffer (selected-window))))
;; load magit autoloads
(require '50magit)
(eval-after-load "magit"
  '(fset 'magit-quit-window #'cofi/vcs-quit-window))

(add-hook 'magit-status-mode-hook (lambda ()
                                    (if (file-exists-p ".git/svn")
                                        (magit-svn-mode))))

(add-hook 'git-commit-mode-hook #'flyspell-mode)

;;; (a)hg
(eval-after-load "monky"
  '(fill-keymaps '(monky-status-mode-map
                   monky-log-mode-map)
                 "q" 'cofi/vcs-quit-window))

(provide 'cofi-vcs)
