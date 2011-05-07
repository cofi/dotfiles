(dolist (path '(
                "~/.elisp/vendor/magit"
                "~/.elisp/ahg"
                ))
  (add-to-list 'load-path path))

(setq vc-handled-backends '(SVN)
      vc-follow-symlinks t)

(global-set-key (kbd "C-c g") 'magit-status)
(require 'ahg)
(setq ahg-global-key-prefix (kbd "C-c h"))

(provide 'cofi-vcs)
