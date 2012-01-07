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

  (defun ibuffer-ediff-marked-buffers (end)
    "Ediff marked buffers based on prefix.
Diffs prefix-1 marked buffer with prefix buffer."
    (interactive "p")
    (let ((marked-buffers (ibuffer-get-marked-buffers)))
      (ediff-buffers (nth (1- end) marked-buffers) (nth end marked-buffers))))

  (define-key ibuffer-mode-map "<" 'ibuffer-ediff-marked-buffers)

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))
  )

(define-ibuffer-column readable-size
  (:name "Size"
         :inline t
         :header-mouse-map ibuffer-size-header-map
         :summarizer (lambda (sizes)
                       (format "%3dM"
                               (/
                                (apply #'+ (mapcar
                                            (lambda (size)
                                              (let ((unit (substring size -1))
                                                    (num (substring size 0 -1)))
                                                (cond
                                                 ((string= unit "k") (* (expt 10 3) (read num)))
                                                 ((string= unit "M") (* (expt 10 6) (read num)))
                                                 (t (read size)))))
                                            sizes))
                                (expt 10 6)))))
  (let ((size (buffer-size))
        (sgroups `(("%4.1fM" ,(expt 10 6))
                   ("%4.1fk" ,(expt 10 3))
                   ("%4d" 1))))
        (loop for (format limit) in sgroups
              if (>= size limit)
              return (if (= (% size limit) 0)
                         (format "%6d" (/ size limit))
                       (format format (/ size (float limit)))))))

(require 'midnight)
(setq midnight-period (* 3600 6))       ; every 6 hours

(setq clean-buffer-list-delay-general 2           ; every 2 day
      clean-buffer-list-delay-special (* 3600 3)) ; every 3 hours

(provide 'cofi-buffer)
