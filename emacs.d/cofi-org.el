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
            (ac-flyspell-mode)
            (org-indent-mode)))

;; Agenda
(setq org-agenda-include-diary t)
(setq org-agenda-skip-unavailable-files t)
(setq org-agenda-files cofi-agenda-files)

;; Remember
(org-remember-insinuate)
(setq remember-data-file (concat org-directory "remember.org"))
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

(require-and-exec 'org-publish
                  (add-to-list 'org-export-latex-packages-alist '("" "listings"))
                  (add-to-list 'org-export-latex-packages-alist '("" "xcolor"))
                  )

(provide 'cofi-org)
