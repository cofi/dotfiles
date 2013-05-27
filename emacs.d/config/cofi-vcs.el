(add-to-loadpath "~/.elisp/vendor/monky")

(setq vc-handled-backends '(SVN)
      vc-follow-symlinks t)

;;; (ma)git
(cofi/set-key 'global "C-c g" 'magit-status)
(setq magit-commit-signoff t
      magit-completing-read-function 'magit-ido-completing-read
      magit-remote-ref-format 'remote-slash-name
      magit-commit-all-when-nothing-staged nil)
(add-hook 'magit-commit-mode-hook 'visual-line-mode)

(add-hook 'magit-status-mode-hook (lambda ()
                                    (if (file-exists-p ".git/svn")
                                        (magit-svn-mode))))

(add-hook 'git-commit-mode-hook #'flyspell-mode)

(defun cofi/open-vcs ()
  (interactive)
  (call-interactively (cond
                       ((locate-dominating-file (buffer-file-name) ".git") #'magit-status)
                       ((locate-dominating-file (buffer-file-name) ".hg") #'monky-status)
                       (t #'magit-status))))

(provide 'cofi-vcs)
