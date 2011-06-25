(add-to-list 'load-path "~/.elisp/vendor/yasnippet")
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/completing-prompt))
(require-and-exec 'yasnippet
    (yas/initialize)
    (yas/load-directory "~/.yasnippets")
    (yas/load-directory "~/dev/snippets")
    (setq yas/also-auto-indent-first-line nil)
    (setq yas/indent-line 'fixed)
    (setq yas/fallback-behavior 'call-other-command)
    (setq yas/triggers-in-field t)
    (setq yas/trigger-key (kbd "TAB"))
    (yas/define-snippets 'nxhtml-mode nil 'html-mode))

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

(add-hook 'magit-mode-hook (lambda () (setq yas/dont-activate t)))
(add-hook 'calc-mode-hook (lambda () (setq yas/dont-activate t)))

(defun cofi/region-to-snippet (begin end)
  "Write new snippet based on current region."
  (interactive "r")
  (let ((region (buffer-substring begin end)))
    (yas/new-snippet)
    (save-excursion
      (end-of-buffer)
      (insert region))))

(provide 'cofi-snippets)
