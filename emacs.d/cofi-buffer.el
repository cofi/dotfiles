(require-and-exec 'ibuffer (lambda ()
  (progn
    (require-and-exec 'ibuf-ext nil)
    ;; ignore emacs buffers
    (add-to-list 'ibuffer-never-show-predicates "\\*.*\\*")

    (setq ibuffer-show-empty-filter-groups nil)

    (setq ibuffer-saved-filter-groups
          (quote (("default"      
                   ("Programming"
                    (or
                     (mode . python-mode)
                     ))
                   ("Config"
                    (filename . ".emacs.d/"))
                   ("Dired"
                    (mode . dired-mode))
                   ("Org"
                    (mode . org-mode))
                   ))))

    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

    (global-set-key (kbd "C-x C-b") 'ibuffer)
    )))

(provide 'cofi-buffer)
