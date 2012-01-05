(setq ansi-color-for-comint-mode t)
(setq comint-prompt-read-only nil)

(require-and-exec 'shell-command
                  (shell-command-completion-mode))

(setq dirtrack-list '("[a-zA-Z@]+ \\(~.*?\\) %>" 1))
(add-hook 'shell-mode-hook (turn-on dirtrack-mode))
(add-hook 'shell-mode-hook
          (gen-local-fill-keymap-hook
              "<home>" 'comint-bol))

(provide 'cofi-shell)
