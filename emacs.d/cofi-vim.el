(require 'cofi-util)
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
  (fill-keymap viper-vi-global-user-map
               "Y" (kbd "y$") ; oh why are you compatible to THAT?!
               "_" 'viper-bol-and-skip-white
               "+" 'viper-next-line-at-bol
               "C-t" 'transpose-chars
               "C-e" 'viper-goto-eol)

  (define-key viper-insert-global-user-map (kbd "C-h") 'backward-delete-char)

  (when (string< vimpulse-version "0.5")
    (require-and-exec 'goto-last-change
       (define-key viper-vi-global-user-map (kbd "g i") 'goto-last-change))
    (require-and-exec 'undo-tree
       (define-key viper-vi-basic-map (kbd "C-r") 'undo-tree-redo)))

  (fill-keymap viper-vi-global-user-map
               "SPC" 'viper-scroll-up
               "S-SPC" 'viper-scroll-down
               "+" 'cofi/inc-at-pt
               "-" 'cofi/dec-at-pt)
  ;; ==================================================

  ;; Mapleader ========================================
  (defconst vim-mapleader "," "Mapping prefix
Vanilla in vi-state; Prefixed with `C-' in insert-state and emacs-state.")
  (defvar cofi/vim-mapleader-map (make-sparse-keymap) "Mapleader keymap")
  ;; <leader> key in normal-mode
  (define-key viper-vi-global-user-map vim-mapleader cofi/vim-mapleader-map)
  ;; C-<leader> key in insert-mode
  (define-key viper-insert-global-user-map (read-kbd-macro
                                            (format "C-%s" vim-mapleader))
                                           cofi/vim-mapleader-map)
  ;; C-<leader> key in emacs-mode
  (define-key viper-emacs-global-user-map (read-kbd-macro
                                            (format "C-%s" vim-mapleader))
                                           cofi/vim-mapleader-map)
  (fill-keymap cofi/vim-mapleader-map
    "e" 'cofi/file
    "E" 'cofi/file-alternate
    "o" 'cofi-find-at-alias
    "b" 'cofi/buffer
    "B" 'cofi/buffer-alternate
    "w" 'save-buffer
    "W" 'save-some-buffers
    "k" 'kill-buffer-and-window
    "K" 'kill-this-buffer
    "<" 'cofi-cd-alias
    "d" 'dired-jump
    "D" 'cofi-dired-alias

    "m" 'compile

    "n" 'split-window-horizontally
    "c" 'delete-window
    "N" 'make-frame-command
    "C" 'delete-frame

    ;; in vcs
    ;; g -> magit-status
    ;; h -> ahg-status
    ;; H -> ahg keymap

    "s" 'cofi/switch-file
    ";" 'cofi/end-prog-line
    )

  (defun vim-mapleader-add (keyseq fun)
    (interactive "kKeysequence: \naFunction:")
    (define-key cofi/vim-mapleader-map (read-kbd-macro keyseq) fun))
  ;; ==================================================

  ;; Vim-like backspace (backspace=indent,eol,start) ==========
  (require-and-exec 'sackspace
                    (sack/install-in-viper))
  ;; ==================================================

  ;; Search keybindings ========================================
  (when (string< vimpulse-version "0.5")
    (fill-keymap viper-vi-basic-map
                 "/" 'isearch-forward-regexp
                 "?" 'isearch-backward-regexp
                 "n" 'isearch-repeat-forward
                 "N" 'isearch-repeat-backward)
    (fill-keymap isearch-mode-map
                 "C-n" 'isearch-repeat-forward
                 "C-p" 'isearch-repeat-backward)
    )
  (push '("nohighlight" (isearch-done)) ex-token-alist)
  ;; ==================================================

  ;; Additional Modes ================================
  (define-key viper-emacs-global-user-map (kbd "C-\\") 'viper-escape-to-vi)
  (define-key viper-insert-global-user-map (kbd "C-\\") 'viper-intercept-ESC-key)
  (define-key viper-vi-global-user-map (kbd "C-\\") 'viper-escape-to-emacs)

  (dolist (mode '(magit-mode
                  magit-key-mode
                  magit-show-branches-mode
                  prolog-inferior-mode))
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

  (defun vimpulse-outline-setup ()
    (let ((map (or viper-vi-local-user-map (make-sparse-keymap))))
      (fill-keymap map
                   "za" 'outline-toggle-children
                   "zm" 'hide-body
                   "zr" 'show-all
                   "zo" 'show-subtree
                   "zc" 'hide-subtree
                   "z@" outline-mode-prefix-map)
      (setq viper-vi-local-user-map map)
      (viper-change-state-to-vi)))

  ;; aww why have you no hook?
  (defadvice outline-minor-mode (after setup-vim-outline activate)
    (vimpulse-outline-setup))
  (add-hook 'outline-mode-hook 'vimpulse-outline-setup)

  (defun vimpulse-org-setup ()
    (let ((map (or viper-vi-local-user-map (make-sparse-keymap))))
           (fill-keymap map
                        "za" 'org-cycle
                        "zA" 'org-shifttab
                        "zm" 'hide-body
                        "zr" 'show-all
                        "zo" 'show-subtree
                        "zc" 'hide-subtree)
           (setq viper-vi-local-user-map map)
           (viper-change-state-to-vi)))
  (add-hook 'org-mode-hook 'vimpulse-org-setup)

  (setq cofi/default-cursor-color       "OliveDrab4"
        cofi/viper-insert-cursor-color  "dark red"
        cofi/viper-replace-cursor-color "red"
        cofi/viper-emacs-cursor-color   "sienna")

  (defun cofi/viper-bar ()
    "Change cursor color according to viper-state."
    (case viper-current-state
      (insert-state  (set-cursor-color cofi/viper-insert-cursor-color))
      (emacs-state   (set-cursor-color cofi/viper-emacs-cursor-color))
      (replace-state (set-cursor-color cofi/viper-replace-cursor-color))
      (otherwise     (set-cursor-color cofi/default-cursor-color))))

  (add-hook 'post-command-hook 'cofi/viper-bar)

  ;; Disable annoying vim compatibilities
  (define-key viper-vi-basic-map (kbd "C-y") nil)
)
(provide 'cofi-vim)
