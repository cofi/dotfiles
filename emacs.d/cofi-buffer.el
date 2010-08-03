(require-and-exec 'ibuffer
  (require-and-exec 'ibuf-ext)
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)

  (setq ibuffer-saved-filter-groups
        (quote (("default"      
                 ("Config" (filename . ".emacs.d/"))

                 ("Programming" (or
                                 (mode . python-mode)
                                 (mode . haskell-mode)
                                 (name . "\\*Python.*\\*")
                                 (name . "\\*haskell.*\\*")
                                 (mode . emacs-lisp-mode)
                                 (mode . sh-mode)
                                 ))

                 ("Writing" (or
                             (mode . tex-mode)
                             (mode . latex-mode)
                             (mode . rst-mode)
                             ))

                 ("Org" (mode . org-mode))
                 
                 ("Dired" (mode . dired-mode))

                 ("Wanderlust" (or
                                (mode . wl-folder-mode)
                                (mode . wl-summary-mode)
                                (name . "SMTP")
                                (mode . bbdb-mode)
                                (name . "bbdb$")
                                ))

                 ("Terminals" (mode . term-mode))

                 ("Shells" (mode . shell-mode))

                 ("Magit" (or 
                           (mode . magit-mode)
                           (name . "\\*magit-.*\\*")
                           ))

                 ("VC" (name . "\\*vc.*\\*"))

                 ("Emacs" (name . "^\\*.*\\*$"))
                 ))))

  (require-and-exec 'ibuffer-git
    (setq ibuffer-formats
          '(
            (mark modified read-only git-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  (git-status 8 8 :left)
                  " " filename-and-process)))
    )

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  )

(provide 'cofi-buffer)
