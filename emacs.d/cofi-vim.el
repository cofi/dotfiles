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

    (define-key vimpulse-visual-basic-map (kbd "g q") 'fill-region)
    (define-key viper-vi-basic-map (kbd "g q") 'fill-paragraph)
    (define-key viper-vi-basic-map (kbd "C-w s") 'split-window-vertically)
    (define-key viper-vi-basic-map (kbd "C-w v") 'split-window-horizontally)
    (define-key viper-vi-basic-map (kbd "C-w o") 'other-window)
    (define-key viper-vi-basic-map (kbd "C-w 1") 'delete-other-windows)
    (define-key viper-vi-basic-map (kbd "C-w d") 'delete-window)
    (define-key viper-vi-basic-map (kbd "C-w m") 'maximize)
    (define-key viper-vi-basic-map (kbd "C-w f") 'fullscreen-toggle)
    (define-key viper-vi-basic-map (kbd "C-w <return>") 'enlarge-window)
    (define-key viper-vi-basic-map (kbd "C-w =") 'enlarge-window-horizontally)
    (define-key viper-vi-basic-map (kbd ",") (make-sparse-keymap))
    (define-key viper-vi-basic-map (kbd ",e") 'ido-find-file)
    (define-key viper-vi-basic-map (kbd ",w") 'save-buffer)
    (define-key viper-vi-basic-map (kbd ",W") 'save-some-buffers)
    (define-key viper-vi-basic-map (kbd ",d") 'dired-jump)
    )

  ;; fix viper binding shortcomings
  (define-key viper-vi-global-user-map (kbd "_") 'viper-bol-and-skip-white)
  (define-key viper-vi-global-user-map (kbd "+") 'viper-next-line-at-bol)
  (define-key viper-vi-global-user-map (kbd "Y") (kbd "y$")) ; oh why are you compatible to THAT?!

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
