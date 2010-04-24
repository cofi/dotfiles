(require-and-exec 'ibuffer
                  (require-and-exec 'ibuf-ext)
                  (setq ibuffer-show-empty-filter-groups nil)

                  (setq ibuffer-saved-filter-groups
                        (quote (("default"      
                                 ("Programming"
                                  (or
                                   (mode . python-mode)
                                   (mode . haskell-mode)
                                   ))
                                 ("Writing"
                                   (or
                                   (mode . latex-mode)
                                   (mode . rst-mode)
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
                                 ("Interpreter"
                                  (or
                                   (name . "\\*Python.*\\*")
                                   (name . "\\*haskell.*\\*")
                                   ))
                                 ("Magit"
                                  (name . "\\*magit-.*\\*"))
                                 ("Emacs"
                                  (name . "\\*.*\\*"))
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
                                                  " " filename-and-process)
                                            )))

                  (add-hook 'ibuffer-mode-hook
                            (lambda ()
                              (ibuffer-switch-to-saved-filter-groups "default")))

                  (global-set-key (kbd "C-x C-b") 'ibuffer)
                  )

(provide 'cofi-buffer)
