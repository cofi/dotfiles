(require 'cofi-autoloads)
(require 'cofi-windowing)

;;; mode keymap
(defkeymap cofi-minor-mode-map
  "c" 'cofi/cdlatex
  "d" 'dedicated-mode
  "f" 'auto-fill-mode
  "g" 'glasses-mode
  "h" 'hs-minor-mode
  "i" 'toggle-input-method
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

;;; anything keymap
(defkeymap cofi-anything-map
    "C-b" 'anything-browse-code
    "b"   'anything-bookmarks
    "c"   'cofi/anything-config
    "m"   'cofi/anything-make
    "u"   'cofi/anything-uni
    "M"   'cofi/anything-lacarte
    "l"   'anything-locate)

;;; breadcrumbs keymap
(defkeymap cofi-breadcrumbs-map
  "s" 'bc-set
  "P" 'bc-previous
  "N" 'bc-next
  "p" 'bc-local-previous
  "n" 'bc-local-next
  "c" 'bc-goto-current
  "l" 'bc-list
  "C" 'bc-clear)

(fill-global-keymap
 ;; buffer
 "C-c y"   'bury-buffer
 "C-c Y"   'quit-window
 "C-c r"   'revert-buffer
 "C-x b"   'cofi/buffer
 "C-x B"   'cofi/buffer-alternate
 "C-x C-c" 'cofi/buffer

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
 "C-<f12>" (cmd (switch-to-buffer "*scratch*"))
 ;; home-end
 "<home>" 'home-end-home
 "<end>"  'home-end-end

 ;; maps
 "C-x m" cofi-minor-mode-map
 "C-x i" cofi-insert-map
 "C-c b" cofi-breadcrumbs-map
 "C-c a" cofi-anything-map
 "C-w"   cofi/window-map
 ;; alternative for buffers were C-w is used
 "C-c w" cofi/window-map
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
(eval-after-load "doc-view"
  '(fill-keymap doc-view-mode-map
                "h"     'image-backward-hscroll
                "j"     'doc-view-next-line-or-next-page
                "k"     'doc-view-previous-line-or-previous-page
                "l"     'image-forward-hscroll
                "K"     'doc-view-kill-proc-and-buffer
                "S-SPC" 'doc-view-scroll-down-or-previous-page))

;;; quick exit for some modes
(dolist (hook '(diff-mode-hook
                compilation-mode-hook
                ahg-diff-mode-hook
                ahg-short-log-mode-hook
                ahg-log-mode-hook
                ahg-glog-mode-hook
                ahg-command-mode-hook
                ahg-status-mode-hook
                ahg-mq-patches-mode-hook))
  (add-hook hook (gen-local-fill-keymap-hook
                  "q" 'quit-window
                  "Q" 'kill-buffer-and-window)))

(provide 'cofi-keys)
