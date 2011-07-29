(require-and-exec 'ibuffer
  (require-and-exec 'ibuf-ext)
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)

  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Config" (filename . ".emacs.d/"))

                 ("Programming" (or
                                 (mode . c-mode)
                                 (mode . c++-mode)
                                 (mode . java-mode)
                                 (mode . lisp-mode)
                                 (mode . clojure-mode)
                                 (mode . python-mode)
                                 (mode . haskell-mode)
                                 (mode . emacs-lisp-mode)
                                 (mode . sh-mode)
                                 ))

                 ("Writing" (or
                             (mode . tex-mode)
                             (mode . latex-mode)
                             (mode . rst-mode)
                             (mode . html-mode)
                             (mode . nxhtml-mode)
                             (mode . css-mode)
                             (mode . nxml-mode)))

                 ("Org-Agenda"
                  (or
                   (filename . "Org/")
                   (mode . org-agenda-mode)
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
                 ("Gnus" (or
                          (mode . message-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)))

                 ("Terminals" (mode . term-mode))

                 ("Shells" (or
                            (mode . eshell-mode)
                            (mode . shell-mode)
                            (mode . slime-repl-mode)
                            (name . "\\*Python.*\\*")
                            (name . "\\*haskell.*\\*")
                            ))

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

  (defun ibuffer-ediff-marked-buffers ()
    (interactive)
    (let* ((marked-buffers (ibuffer-get-marked-buffers))
           (len (length marked-buffers)))
      (unless (= 2 len)
        (error (format "%s buffer%s been marked (needs to be 2)"
                       len (if (= len 1) " has" "s have"))))
      (ediff-buffers (car marked-buffers) (cadr marked-buffers))))

  (define-key ibuffer-mode-map "<" 'ibuffer-ediff-marked-buffers)

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))
  )

(require 'midnight)
(setq midnight-period (* 3600 6))       ; every 6 hours

(setq clean-buffer-list-delay-general 2           ; every 2 day
      clean-buffer-list-delay-special (* 3600 3)) ; every 3 hours

(provide 'cofi-buffer)
