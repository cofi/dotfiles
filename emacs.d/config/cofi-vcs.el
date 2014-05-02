(add-to-loadpath "~/.elisp/vendor/monky"
                 "~/.elisp/vendor/magit"
                 "~/.elisp/vendor/git-modes/")

(require 'magit-autoloads)

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


(autoload 'git-commit-mode "git-commit-mode" nil t)
(dolist (pattern '("/COMMIT_EDITMSG\\'" "/NOTES_EDITMSG\\'"
                   "/MERGE_MSG\\'" "/TAG_EDITMSG\\'"
                   "/PULLREQ_EDITMSG\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'git-commit-mode)))
(autoload 'git-rebase-mode "git-rebase-mode" nil t)
(add-to-list 'auto-mode-alist
             '("/git-rebase-todo\\'" . git-rebase-mode))

(autoload 'gitattributes-mode "gitattributes-mode" nil t)
(dolist (pattern '("/\\.gitattributes\\'"
                   "/\\.git/info/attributes\\'"
                   "/git/attributes\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'gitattributes-mode)))

(autoload 'gitconfig-mode "gitconfig-mode" nil t)
(dolist (pattern '("/\\.gitconfig\\'" "/\\.git/config\\'"
                   "/git/config\\'"   "/\\.gitmodules\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

(autoload 'gitignore-mode "gitignore-mode" nil t)
(dolist (pattern (list "/\\.gitignore\\'"
                       "/\\.git/info/exclude\\'"
                       "/git/ignore\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

(provide 'cofi-vcs)
