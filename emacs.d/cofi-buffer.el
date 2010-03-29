(require-and-exec 'ibuffer (lambda ()
  (progn
    (require-and-exec 'ibuf-ext nil)
    (setq ibuffer-show-empty-filter-groups nil)

    (setq ibuffer-saved-filter-groups
          (quote (("default"      
                   ("Programming"
                    (or
                     (mode . python-mode)
                     (name . "\\*Python.*\\*")
                     ))
                   ("Config"
                    (filename . ".emacs.d/"))
                   ("Dired"
                    (mode . dired-mode))
                   ("Org"
                    (mode . org-mode))
                   ("Terminals"
                    (mode . term-mode))
                   ("Shells"
                    (mode . shell-mode))
                   ("Emacs"
                    (name . "\\*.*\\*"))
                   ))))

    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

    (global-set-key (kbd "C-x C-b") 'ibuffer)
    )))

(provide 'cofi-buffer)
