(setq viper-toggle-key [pause])
(require-and-exec 'vimpulse
  (setq viper-shift-width 4)
  (setq viper-re-search t)
  (setq-default viper-auto-indent t)
  (setq viper-vi-style-in-minibuffer nil)
  (setq viper-ex-style-editing nil)

  ;; Window keybindings ========================================
  ;; kill vimpulse bindings (new defined in windowing)
  (define-key viper-vi-basic-map "\C-w" nil)
  (define-key viper-vi-basic-map "\C-w\C-w" nil)
  (define-key viper-vi-basic-map "\C-ww" nil)
  (define-key viper-vi-basic-map "\C-wo" nil)
  (define-key viper-vi-basic-map "\C-wc" nil)
  (define-key viper-vi-basic-map "\C-ws" nil)
  (define-key viper-vi-basic-map "\C-wv" nil)
  (define-key viper-vi-basic-map "\C-wh" nil)
  (define-key viper-vi-basic-map "\C-wj" nil)
  (define-key viper-vi-basic-map "\C-wk" nil)
  (define-key viper-vi-basic-map "\C-wl" nil)
  ;; ============================================================

  ;; Keybindings ==========
  (define-key viper-vi-global-user-map (kbd "Y") (kbd "y$")) ; oh why are you compatible to THAT?!
  (define-key viper-vi-global-user-map (kbd "_") 'viper-bol-and-skip-white)
  (define-key viper-vi-global-user-map (kbd "+") 'viper-next-line-at-bol)
  (define-key viper-vi-global-user-map (kbd "C-t") 'transpose-chars)
  (define-key viper-vi-global-user-map (kbd "C-e") 'viper-goto-eol)

  (when (fboundp 'redo)
    (define-key viper-vi-global-user-map (kbd "r") 'redo))

  (define-key viper-insert-global-user-map (kbd "C-h") 'backward-delete-char)
  (define-key viper-insert-global-user-map (kbd "C-\\") 'viper-intercept-ESC-key)
  (require-and-exec 'goto-last-change
     (define-key viper-vi-global-user-map (kbd "g i") 'goto-last-change))
  (define-key viper-vi-global-user-map (kbd "SPC") 'viper-scroll-up)
  (define-key viper-vi-global-user-map (kbd "S-SPC") 'viper-scroll-down)
  (define-key viper-vi-global-user-map (kbd "+") 'cofi/inc-at-pt)
  (define-key viper-vi-global-user-map (kbd "-") 'cofi/dec-at-pt)
  ;; ==================================================

  ;; Mapleader ========================================
  (defconst vim-mapleader "," "Mapping prefix
Vanilla in vi-state; Prefixed witf `C-' in insert-state and emacs-state.")
  (defvar vim-mapleader-map (make-sparse-keymap) "Mapleader keymap")
  ;; <leader> key in normal-mode
  (define-key viper-vi-global-user-map vim-mapleader vim-mapleader-map)
  ;; C-<leader> key in insert-mode
  (define-key viper-insert-global-user-map (read-kbd-macro
                                            (format "C-%s" vim-mapleader))
                                           vim-mapleader-map)
  ;; C-<leader> key in emacs-mode
  (define-key viper-emacs-global-user-map (read-kbd-macro
                                            (format "C-%s" vim-mapleader))
                                           vim-mapleader-map)
  (defun vim-mapleader-add (keyseq fun)
    (interactive "kKeysequence: \naFunction:")
    (define-key vim-mapleader-map keyseq fun))

  (vim-mapleader-add "e" 'cofi/file)
  (vim-mapleader-add "E" 'cofi/file-alternate)
  (vim-mapleader-add "b" 'cofi/buffer)
  (vim-mapleader-add "B" 'cofi/buffer-alternate)
  (vim-mapleader-add "w" 'save-buffer)
  (vim-mapleader-add "W" 'save-some-buffers)
  (vim-mapleader-add "k" 'kill-buffer-and-window)
  (vim-mapleader-add "K" 'kill-this-buffer)
  (vim-mapleader-add "C" 'cofi-cd-alias)
  (vim-mapleader-add "d" 'dired-jump)
  (vim-mapleader-add "D" 'cofi-dired-alias)

  (vim-mapleader-add "n" 'split-window-horizontally)
  (vim-mapleader-add "c" 'delete-window)
  (vim-mapleader-add "N" 'make-frame-command)
  (vim-mapleader-add "C" 'delete-frame)

  (vim-mapleader-add "g" 'magit-status)

  (vim-mapleader-add ";" 'cofi/end-prog-line)
  ;; ==================================================

  ;; Vim-like backspace (backspace=indent,eol,start) ==========
  (require-and-exec 'sackspace
                    (sack/install-in-viper))
  ;; ==================================================

  ;; Search keybindings ========================================
  (define-key viper-vi-global-user-map (kbd "C-r") 'isearch-backward-regexp)
  (define-key viper-vi-global-user-map (kbd "C-s") 'isearch-forward-regexp)
  (define-key viper-vi-basic-map (kbd "/") 'isearch-forward-regexp)
  (define-key viper-vi-basic-map (kbd "?") 'isearch-backward-regexp)
  (define-key viper-vi-basic-map (kbd "n") 'isearch-repeat-forward)
  (define-key viper-vi-basic-map (kbd "N") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
  (push '("nohighlight" (isearch-done)) ex-token-alist)
  ;; ==================================================

  ;; Additional Modes ================================
  (define-key viper-emacs-global-user-map (kbd "C-\\") 'viper-escape-to-vi)

  (push 'magit-mode viper-emacs-state-mode-list)
  (push 'magit-key-mode viper-emacs-state-mode-list)
  ;; ===================================================

  ;; Conflicts ========================================
  (define-key viper-insert-basic-map (kbd "C-d") nil) ; conflicts with yasnippet
  ;; ==================================================

  ;; Misc ========================================
  (eval-after-load "org"
    '(progn
       (define-key viper-vi-global-user-map (kbd "C-c /") 'org-sparse-tree)
       (add-hook 'org-mode-hook
                 (lambda ()
                   (setq
                    viper-vi-local-user-map
                        (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET") 'org-open-at-point)
                          map)
                    viper-insert-local-user-map
                        (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "M-l") 'org-metaright)
                          (define-key map (kbd "M-h") 'org-metaleft)
                          map))
                   ;; some times local-maps don't get reloaded, this forces it
                   (viper-change-state-to-vi)))
       ))

  (setq cofi/default-cursor-color "OliveDrab4")
  (setq cofi/viper-insert-cursor-color "dark red")
  (setq cof/viper-replace-cursor-color "red")
  (setq cofi/viper-emacs-cursor-color "sienna")

  (defun cofi/viper-bar ()
    "Change cursor color according to viper-state."
    (cond
     ((eq viper-current-state 'insert-state)
      (set-cursor-color cofi/viper-insert-cursor-color))
     ((eq viper-current-state 'emacs-state)
      (set-cursor-color cofi/viper-emacs-cursor-color))
     ((eq viper-current-state 'replace-state)
      (set-cursor-color cofi/viper-replace-cursor-color))
     (t 
      (set-cursor-color cofi/default-cursor-color)
      )))
  
  (add-hook 'post-command-hook 'cofi/viper-bar)
)
(provide 'cofi-vim)
