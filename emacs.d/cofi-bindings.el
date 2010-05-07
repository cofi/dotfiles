;; use C-Tab instead of M-Tab
(define-key function-key-map (kbd "C-<tab>") [?\M-\t])

(global-set-key (kbd "C-c f") (make-hippie-expand-function
                               '(try-complete-file-name-partially
                                 try-complete-file-name
                                 )))

(global-set-key (kbd "C-c l") (make-hippie-expand-function
                               '(try-expand-line
                                 try-expand-line-all-buffers
                                 )))

(global-set-key (kbd "C-c d") (make-hippie-expand-function
                               '(try-expand-all-abbrevs
                                 try-expand-dabbrev
                                 try-expand-dabbrev-all-buffers
                                 try-expand-dabbrev-visible
                                 try-expand-dabbrev-from-kill
                                 )))

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)

(global-set-key (kbd "C-M-h") 'backward-kill-word)

(add-hook 'diff-mode
          (lambda ()
            (local-set-key (kbd "q") 'kill-this-buffer)))

;; go away mail
(global-unset-key (kbd "C-x m"))
