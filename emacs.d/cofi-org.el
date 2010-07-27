(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-startup-folded nil)

(setq org-directory
      (if (file-directory-p "~/Org")
          "~/Org/"
        "~/"))

(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-iimage-mode)
            (local-set-key (kbd "M-n") 'outline-next-visible-heading)
            (local-set-key (kbd "M-p") 'outline-previous-visible-heading)
            (local-set-key (kbd "C-M-<return>") (lambda ()
                                                  (interactive)
                                                  (end-of-line)
                                                  (org-meta-return)))
            (ac-flyspell-mode)
            (org-indent-mode)))

;; Agenda
(setq org-agenda-include-diary t
      org-agenda-skip-unavailable-files t
      org-agenda-files cofi-agenda-files
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      )

;; ToDo
(setq org-todo-keywords '((sequence
                          "TODO"
                          "STARTED"
                          "WAITING"
                          "DEFERRED"
                          "|"           ; Separator, no more action necessary
                          "DELEGATED"
                          "CANCELLED"
                          "DONE"
                          )))
(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map (kbd "C-c s") 'org-todo-state-map)
     (define-key org-todo-state-map "x"
       (lambda () (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       (lambda () (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       (lambda () (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       (lambda () (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       (lambda () (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       (lambda () (interactive) (org-todo "WAITING")))
     ))

;; Tbl
(eval-after-load "org-table"
  '(define-key orgtbl-mode-map (kbd "C-c t") 'orgtbl-insert-radio-table))

;; Appt
(setq
  appt-message-warning-time 15 ;; warn 15 min in advance
  appt-display-mode-line t
  appt-display-format 'window)
(appt-activate 1)

(setq appt-disp-window-function
      (lambda (min-to-app new-time msg)
        (send-notification msg
                           (format "Appointment in %s minutes" min-to-app))
        ))
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; Remember
(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "remember.org"))
(global-set-key (kbd "C-c r") 'org-remember)

;; Exporting
(setq org-emphasis-alist
      '(("*" bold "<b>" "</b>")
        ("/" italic "<i>" "</i>")
        ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
        ("=" org-code "<code>" "</code>" verbatim)
        ("~" org-verbatim "<code>" "</code>" verbatim)
        ("+" (:strike-through t) "<del>" "</del>")
        ("@" org-warning "<b>" "</b>")))

(setq org-export-latex-emphasis-alist
      '(("*" "\\textbf{%s}" nil)
        ("/" "\\emph{%s}" nil) 
        ("_" "\\underline{%s}" nil)
        ("+" "\\st{%s}" nil)
        ("=" "\\verb=%s=" nil)
        ("~" "\\verb~%s~" t)
        ("@" "\\alert{%s}" nil)))

(setq org-export-latex-listings t)

(eval-after-load "org-publish"
  '(progn
     (add-to-list 'org-export-latex-packages-alist '("" "listings"))
     (add-to-list 'org-export-latex-packages-alist '("" "xcolor"))
     )
  )

(provide 'cofi-org)
