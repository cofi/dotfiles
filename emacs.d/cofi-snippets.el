(add-to-list 'load-path "~/.elisp/vendor/yasnippet")
(require-and-exec 'yasnippet
    (setq yas/snippet-dirs '(
                             "~/.emacs.d/snippets"
                             "~/.elisp/vendor/yasnippet/snippets"
                             ))
    (yas/initialize)
    (setq yas/prompt-functions '(yas/ido-prompt
                                 yas/completing-prompt))
    (setq yas/fallback-behavior '(apply smart-tab))
    (setq yas/also-auto-indent-first-line t)

    (global-set-key (kbd "M-RET") 'yas/expand)
    )

(eval-after-load "org"
  (add-hook 'org-mode-hook
              (lambda ()
                 (setq yas/fallback-behavior
                       '(apply org-cycle))
                 (local-set-key [tab] 'yas/expand))))

(provide 'cofi-snippets)
