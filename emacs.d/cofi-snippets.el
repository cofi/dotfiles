(add-to-list 'load-path "~/.elisp/vendor/yasnippet")
(require-and-exec 'yasnippet
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/snippets")
    (yas/load-directory "~/dev/snippets")
    (yas/load-directory "~/.elisp/vendor/yasnippet/snippets")
    (setq yas/prompt-functions '(yas/ido-prompt
                                 yas/completing-prompt))
    (setq yas/also-auto-indent-first-line t)
    (setq yas/fallback-behavior 'call-other-command)
    (setq yas/trigger-key (kbd "TAB"))
    (global-set-key (kbd "M-RET") 'yas/expand)
    )

(define-key yas/minor-mode-map "\C-c&" nil)

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet (using the new org-cycle hooks)
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

(eval-after-load "magit"
  (add-hook 'magit-mode-hook (lambda () (setq yas/dont-activate t))))
(eval-after-load "calc"
  (add-hook 'calc-mode-hook (lambda () (setq yas/dont-activate t))))

(provide 'cofi-snippets)
