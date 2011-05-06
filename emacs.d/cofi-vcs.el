(setq vc-handled-backends '(SVN Hg)
      vc-follow-symlinks t)

(global-set-key (kbd "C-c g") 'magit-status)

(provide 'cofi-vcs)
