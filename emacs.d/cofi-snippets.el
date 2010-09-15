(add-to-list 'load-path "~/.elisp/vendor/yasnippet")
(require-and-exec 'yasnippet
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/snippets")
    (yas/load-directory "~/dev/snippets")
    (yas/load-directory "~/.elisp/vendor/yasnippet/snippets")
    (setq yas/prompt-functions '(yas/ido-prompt
                                 yas/completing-prompt))
    (setq yas/also-auto-indent-first-line t)

    (global-set-key (kbd "M-RET") 'yas/expand)
    )

(eval-after-load "org"
  (add-hook 'org-mode-hook
              (lambda ()
                 (setq yas/fallback-behavior
                       '(apply org-cycle))
                 (local-set-key [tab] 'yas/expand))))
(eval-after-load "magit"
  (add-hook 'magit-mode-hook (lambda () (setq yas/dont-activate t))))
(eval-after-load "calc"
  (add-hook 'calc-mode-hook (lambda () (setq yas/dont-activate t))))

(provide 'cofi-snippets)
