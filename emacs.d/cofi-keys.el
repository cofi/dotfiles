(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

(if (fboundp 'anything-imenu)
    (global-set-key (kbd "C-c i") 'anything-imenu)
  (global-set-key (kbd "C-c i") 'idomenu))

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x M-b") 'ido-switch-buffer-other-window)
(global-set-key (kbd "C-x C-d") 'ido-display-buffer)
(global-set-key (kbd "C-x M-d") 'dired-other-window)

(add-hook 'diff-mode-hook '(lambda ()
                            (local-set-key (kbd "q") 'kill-this-buffer)))

(global-set-key (kbd "C-c b") 'revert-buffer)

(global-set-key (kbd "C-x f") 'cofi/file)
(global-set-key (kbd "C-x C-f") 'cofi/file-alternate)
(global-set-key (kbd "C-x b") 'cofi/buffer)
(global-set-key (kbd "C-x B") 'cofi/buffer-alternate)
(global-set-key (kbd "C-x C-c") 'cofi/buffer)

(require-and-exec 'cofi-func
  (global-set-key (kbd "C-;") 'comment-or-uncomment-current-line-or-region)
  (global-set-key (kbd "<f4>") 'cofi/macro-dwim)
  (global-set-key (kbd "S-<f4>") 'cofi/reset-macro)

  (add-hook 'artist-mode-init-hook
            (lambda ()
              (define-key artist-mode-map (kbd "C-c C-a C-o")
                'artist-ido-select-operation)
              (define-key artist-mode-map (kbd "C-c C-a C-c")
                'artist-ido-select-settings)))
  )

(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-h") 'backward-delete-char)

(global-set-key (kbd "M-r") 'query-replace-regexp)
(global-set-key (kbd "M-R") 'query-replace)

(when (locate-library "scratch")
  (autoload 'scratch "scratch" nil t)
  (global-set-key (kbd "<f12>") 'scratch))

(when (locate-library "home-end")
  (global-set-key (kbd "<home>") 'home-end-home)
  (global-set-key (kbd "<end>") 'home-end-end))

;;; mode keymap
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "c") 'cofi/cdlatex)
  (define-key map (kbd "d") 'dedicated-mode)
  (define-key map (kbd "f") 'auto-fill-mode)
  (define-key map (kbd "g") 'glasses-mode)
  (define-key map (kbd "h") 'hs-minor-mode)
  (define-key map (kbd "l") 'outline-minor-mode)
  (define-key map (kbd "o") 'orgstruct++-mode)
  (define-key map (kbd "p") 'pretty-mode)
  (define-key map (kbd "r") 'auto-revert-mode)
  (define-key map (kbd "s") 'flyspell-mode)
  (define-key map (kbd "t") 'orgtbl-mode)
  (define-key map (kbd "w") 'whitespace-mode)
  (global-set-key (kbd "C-x m") map))

;;; insert keymap
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "b") 'insert-buffer)
  (define-key map (kbd "c") 'clipper-insert)
  (define-key map (kbd "f") 'insert-file)
  (define-key map (kbd "s") 'yas/insert-snippet)
  (global-set-key (kbd "C-x i") map))

(add-hook 'ido-setup-hook
          (lambda ()
            ;; first won't work because of viper binding
            (define-key ido-completion-map (kbd "C-h") 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-l") 'ido-next-match)))

(provide 'cofi-keys)
