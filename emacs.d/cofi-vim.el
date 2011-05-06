(add-to-list 'load-path "~/.elisp/vendor/vimpulse")
(add-to-list 'load-path "~/.elisp/vendor/undo-tree")
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
  ;; ============================================================

  ;; Keybindings ==========
  (define-key viper-vi-global-user-map (kbd "Y") (kbd "y$")) ; oh why are you compatible to THAT?!
  (define-key viper-vi-global-user-map (kbd "_") 'viper-bol-and-skip-white)
  (define-key viper-vi-global-user-map (kbd "+") 'viper-next-line-at-bol)
  (define-key viper-vi-global-user-map (kbd "C-t") 'transpose-chars)
  (define-key viper-vi-global-user-map (kbd "C-e") 'viper-goto-eol)

  (define-key viper-insert-global-user-map (kbd "C-h") 'backward-delete-char)
  (define-key viper-insert-global-user-map (kbd "RET") 'reindent-then-newline-and-indent)

  (when (string< vimpulse-version "0.5")
    (require-and-exec 'goto-last-change
       (define-key viper-vi-global-user-map (kbd "g i") 'goto-last-change))
    (require-and-exec 'undo-tree
       (define-key viper-vi-basic-map (kbd "C-r") 'undo-tree-redo)))

  (define-key viper-vi-global-user-map (kbd "SPC") 'viper-scroll-up)
  (define-key viper-vi-global-user-map (kbd "S-SPC") 'viper-scroll-down)
  (define-key viper-vi-global-user-map (kbd "+") 'cofi/inc-at-pt)
  (define-key viper-vi-global-user-map (kbd "-") 'cofi/dec-at-pt)
  ;; ==================================================

  ;; Mapleader ========================================
  (defconst vim-mapleader "," "Mapping prefix
Vanilla in vi-state; Prefixed with `C-' in insert-state and emacs-state.")
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
  (vim-mapleader-add "o" 'cofi-find-at-alias)
  (vim-mapleader-add "b" 'cofi/buffer)
  (vim-mapleader-add "B" 'cofi/buffer-alternate)
  (vim-mapleader-add "w" 'save-buffer)
  (vim-mapleader-add "W" 'save-some-buffers)
  (vim-mapleader-add "k" 'kill-buffer-and-window)
  (vim-mapleader-add "K" 'kill-this-buffer)
  (vim-mapleader-add "<" 'cofi-cd-alias)
  (vim-mapleader-add "d" 'dired-jump)
  (vim-mapleader-add "D" 'cofi-dired-alias)

  (vim-mapleader-add "m" 'compile)

  (vim-mapleader-add "n" 'split-window-horizontally)
  (vim-mapleader-add "c" 'delete-window)
  (vim-mapleader-add "N" 'make-frame-command)
  (vim-mapleader-add "C" 'delete-frame)

  (vim-mapleader-add "g" 'magit-status)

  (vim-mapleader-add "s" 'cofi/switch-file)
  (vim-mapleader-add ";" 'cofi/end-prog-line)
  ;; ==================================================

  ;; Vim-like backspace (backspace=indent,eol,start) ==========
  (require-and-exec 'sackspace
                    (sack/install-in-viper))
  ;; ==================================================

  ;; Search keybindings ========================================
  (when (string< vimpulse-version "0.5")
    (define-key viper-vi-basic-map (kbd "/") 'isearch-forward-regexp)
    (define-key viper-vi-basic-map (kbd "?") 'isearch-backward-regexp)
    (define-key viper-vi-basic-map (kbd "n") 'isearch-repeat-forward)
    (define-key viper-vi-basic-map (kbd "N") 'isearch-repeat-backward)
    (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
    (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
    )
  (push '("nohighlight" (isearch-done)) ex-token-alist)
  ;; ==================================================

  ;; Additional Modes ================================
  (define-key viper-emacs-global-user-map (kbd "C-\\") 'viper-escape-to-vi)
  (define-key viper-insert-global-user-map (kbd "C-\\") 'viper-intercept-ESC-key)
  (define-key viper-vi-global-user-map (kbd "C-\\") 'viper-escape-to-emacs)

  (dolist (mode '(magit-mode magit-key-mode magit-show-branches-mode))
    (push mode viper-emacs-state-mode-list))

  (push 'clojure-mode viper-vi-state-mode-list)
  ;; ===================================================

  ;; Conflicts ========================================
  (define-key viper-insert-basic-map (kbd "C-d") nil) ; conflicts with yasnippet
  (define-key viper-vi-basic-map (kbd "C-c /") nil)  ; conflicts with org
  ;; ==================================================

  ;; Misc ========================================
  (add-hook 'org-mode-hook
            (lambda ()
              (setq viper-vi-local-user-map
                    (let ((map (or viper-vi-local-user-map (make-sparse-keymap))))
                      (define-key map (kbd "RET") 'org-open-at-point)
                      map)
                    viper-insert-local-user-map
                    (let ((map (make-sparse-keymap)))
                      (define-key map (kbd "M-l") 'org-metaright)
                      (define-key map (kbd "M-h") 'org-metaleft)
                      map))
              ;; some times local-maps don't get reloaded, this forces it
              (viper-change-state-to-vi)))

  (eval-after-load 'outline
    '(progn
       (defun vimpulse-outline-setup ()
         (let ((map (or viper-vi-local-user-map (make-sparse-keymap))))
           (define-key map "za" 'outline-toggle-children)
           (define-key map "zm" 'hide-body)
           (define-key map "zr" 'show-all)
           (define-key map "zo" 'show-subtree)
           (define-key map "zc" 'hide-subtree)
           (define-key map "z@" outline-mode-prefix-map)
           (setq viper-vi-local-user-map map)
           (viper-change-state-to-vi)))

       ;; aww why have you no hook?
       (defadvice outline-minor-mode (after setup-vim-outline activate)
         (vimpulse-outline-setup))
       (add-hook 'outline-mode-hook 'vimpulse-outline-setup)
       ))

  (defun vimpulse-org-setup ()
    (let ((map (or viper-vi-local-user-map (make-sparse-keymap))))
      (define-key map "za" 'org-cycle)
      (define-key map "zA" 'org-shifttab)
      (define-key map "zm" 'hide-body)
      (define-key map "zr" 'show-all)
      (define-key map "zo" 'show-subtree)
      (define-key map "zc" 'hide-subtree)
      (setq viper-vi-local-user-map map)
      (viper-change-state-to-vi)))
  (add-hook 'org-mode-hook 'vimpulse-org-setup)

  (setq cofi/default-cursor-color "OliveDrab4")
  (setq cofi/viper-insert-cursor-color "dark red")
  (setq cofi/viper-replace-cursor-color "red")
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

  ;; Disable annoying vim compatibilities
  (define-key viper-vi-basic-map (kbd "C-y") nil)
)
(provide 'cofi-vim)
