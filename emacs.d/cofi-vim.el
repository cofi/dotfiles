(setq viper-mode t)
(setq viper-toggle-key (kbd "<pause>"))
(require-and-exec 'viper
  (setq viper-shift-width 4)
  (setq viper-re-search t)
  (setq-default viper-auto-indent t)
  (setq viper-ex-style-editing nil)
  (require-and-exec 'goto-last-change
     (define-key viper-vi-global-user-map (kbd "g i") 'goto-last-change))

  (define-key viper-insert-global-user-map (kbd "C-h") 'backward-delete-char)

  (require-and-exec 'vimpulse
    (when (fboundp 'redo)
      (define-key viper-vi-global-user-map (kbd "r") 'redo))

    (define-key viper-vi-basic-map (kbd "C-w s") 'split-window-vertically)
    (define-key viper-vi-basic-map (kbd "C-w v") 'split-window-horizontally)
    (define-key viper-vi-basic-map (kbd "C-w o") 'other-window)
    (define-key viper-vi-basic-map (kbd "C-w 1") 'delete-other-windows)
    (define-key viper-vi-basic-map (kbd "C-w d") 'delete-window)
    (define-key viper-vi-basic-map (kbd "C-w m") 'maximize)
    (define-key viper-vi-basic-map (kbd "C-w f") 'fullscreen-toggle)
    (define-key viper-vi-basic-map (kbd "C-w <return>") 'enlarge-window)
    (define-key viper-vi-basic-map (kbd "C-w =") 'enlarge-window-horizontally)
    )

  ;; fix viper binding shortcomings
  (define-key viper-vi-global-user-map (kbd "_") 'viper-bol-and-skip-white)
  (define-key viper-vi-global-user-map (kbd "+") 'viper-next-line-at-bol)
  (define-key viper-vi-global-user-map (kbd "Y") (kbd "y$")) ; oh why are you compatible to THAT?!

  (defconst vim-mapleader "," "Normal mode mapping prefix")
  (defvar vim-mapleader-map (make-sparse-keymap) "Mapleader keymap")
  (define-key viper-vi-global-user-map vim-mapleader vim-mapleader-map)
  
  (defun vim-mapleader-add (keyseq fun)
    (interactive "sKeysequence: \naFunction:")
    (define-key vim-mapleader-map keyseq fun))

  (vim-mapleader-add "e" 'ido-find-file)
  (vim-mapleader-add "w" 'save-buffer)
  (vim-mapleader-add "W" 'save-some-buffers)
  (vim-mapleader-add "d" 'dired-jump)
  (vim-mapleader-add "k" 'kill-this-buffer)
  (vim-mapleader-add "C" 'cofi-cd-alias)
  (vim-mapleader-add "D" 'cofi-dired-alias)

  (eval-after-load "org"
    '(progn
       (define-key viper-vi-global-user-map (kbd "C-c /") 'org-sparse-tree)
       (add-hook 'org-mode-hook
                 (lambda ()
                   (define-key viper-insert-local-user-map (kbd "M-h") 'org-metaleft)
                   (define-key viper-insert-local-user-map (kbd "M-l") 'org-metaright)
                   ))
       ))

  (define-key viper-vi-global-user-map (kbd "C-r") 'isearch-backward-regexp)
  (define-key viper-vi-global-user-map (kbd "C-s") 'isearch-forward-regexp)
  (define-key viper-vi-basic-map (kbd "/") 'isearch-forward-regexp)
  (define-key viper-vi-basic-map (kbd "?") 'isearch-backward-regexp)
  (define-key viper-vi-basic-map (kbd "n") 'isearch-repeat-forward)
  (define-key viper-vi-basic-map (kbd "N") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
  (push '("nohighlight" (isearch-done)) ex-token-alist)

  (define-key viper-insert-basic-map (kbd "C-d") nil) ; conflicts with yasnippet
  )

(provide 'cofi-vim)
