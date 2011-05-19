(require 'cofi-autoloads)

;;; mode keymap
(defkeymap cofi-minor-mode-map
  "c" 'cofi/cdlatex
  "d" 'dedicated-mode
  "f" 'auto-fill-mode
  "g" 'glasses-mode
  "h" 'hs-minor-mode
  "l" 'outline-minor-mode
  "o" 'orgstruct++-mode
  "p" 'pretty-mode
  "r" 'auto-revert-mode
  "s" 'flyspell-mode
  "t" 'orgtbl-mode
  "w" 'whitespace-mode)

;;; insert keymap
(defkeymap cofi-insert-map
  "b" 'insert-buffer
  "c" 'clipper-insert
  "f" 'insert-file
  "s" 'yas/insert-snippet)

(fill-global-keymap
 ;; buffer
 "C-c y"   'bury-buffer
 "C-c Y"   'quit-window
 "C-c b"   'revert-buffer
 "C-x b"   'cofi/buffer
 "C-x B"   'cofi/buffer-alternate
 "C-x C-c" 'cofi/buffer
 ;; files


 ;; files
 "C-x M-f" 'ido-find-file-other-window
 "C-x M-b" 'ido-switch-buffer-other-window
 "C-x C-d" 'ido-display-buffer
 "C-x M-d" 'dired-other-window
 "C-x f"   'cofi/file
 "C-x C-f" 'cofi/file-alternate

 ;; search
 "C-r"     'isearch-backward-regexp
 "C-s"     'isearch-forward-regexp
 "M-r"     'query-replace-regexp
 "M-R"     'query-replace
 ;; editing
 "C-M-h"   'backward-kill-word
 "C-;"     'toggle-comment-on-line-or-region
 ;; macros
 "<f4>"    'cofi/macro-dwim
 "S-<f4>"  'cofi/reset-macro
 ;; point stack
 "<f2>"   'point-stack-push
 "S-<f2>" 'point-stack-pop
 "C-<f2>" 'point-stack-forward-stack-pop

 ;; multi term
 "<f11>"   'multi-term-dedicated-open
 "C-<f11>" 'multi-term-next
 ;; scratch
 "<f12>" 'scratch
 ;; home-end
 "<home>" 'home-end-home
 "<end>"  'home-end-end

 ;; maps
 "C-x m" cofi-minor-mode-map
 "C-x i" cofi-insert-map
 )


(if (fboundp 'anything-imenu)
    (global-set-key (kbd "C-c i") 'anything-imenu)
  (global-set-key (kbd "C-c i") 'idomenu))

(add-hook 'diff-mode-hook '(lambda ()
                            (local-set-key (kbd "q") 'kill-this-buffer)))

(add-hook 'artist-mode-init-hook
            (gen-fill-keymap-hook artist-mode-map
                                  "C-c C-a C-o" 'artist-ido-select-operation
                                  "C-c C-a C-c" 'artist-ido-select-settings))

(define-key isearch-mode-map (kbd "C-h") 'backward-delete-char)

(add-hook 'ido-setup-hook
          (gen-fill-keymap-hook ido-completion-map
                                "C-h" 'ido-prev-match
                                "C-l" 'ido-next-match))

;;; quick exit for some modes
(fill-keymaps (list
               diff-mode-map
               compilation-mode-map
               ahg-diff-mode-map ahg-short-log-mode-map ahg-log-mode-map ahg-glog-mode-map
               ahg-command-mode-map ahg-status-mode-map ahg-mq-patches-mode-map
               )
              "q" 'View-quit
              "Q" 'kill-buffer-and-window
              )

(provide 'cofi-keys)
