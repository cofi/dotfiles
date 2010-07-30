(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(autoload 'wl-user-agent-compose "wl-draft" "Compose mail with Wanderlust" t)

(autoload 'trivial-cite "tc" t t)
(add-hook 'mail-citation-hook 'trivial-cite)

(setq tc-make-attribution (function tc-simple-attribution-kai))

(defun mail-attachment-check ()
  "Check if attachment is mentioned but not included"
  (interactive)
  (let ((attachment-regexp "[Aa]ttachment\\|[Aa]ttached\\|[Aa]nbei\\|[Bb]eiliegend\\|[Aa]anhang\\|angehängt"))
    (save-excursion
      (goto-char 0)
      (unless (re-search-forward "^Content-Disposition: attachment" nil t)
        (when
            ;; Check for mentioned attachments
            (re-search-forward attachment-regexp nil t)
          (unless (y-or-n-p "Attachment maybe missing. Send? ")
            (error "Abort.")))))))

(defun mail-subject-check ()
  "Check if subject is missing."
  (unless (or (> (length (std11-field-body "Subject")) 1)
          (y-or-n-p "Subject missing. Send? "))
      (error "Abort.")))

;; SEMI-PGG files are safe to delete, Hello GPG_AGENT 
(require-and-exec 'pgg
                  (setq pgg-default-scheme 'gpg
                        pgg-scheme 'gpg
                        pgg-query-keyserver t
                        pgg-default-keyserver-address "subkeys.pgp.net"
                        pgg-default-user-id "Michael Markert <markert.michael@googlemail.com>"
                        pgg-gpg-user-id "Michael Markert <markert.michael@googlemail.com>"
                        pgg-encrypt-for-me t
                        pgg-gpg-use-agent t
                        )
                  )

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(eval-after-load "wl"
  '(progn
     (add-hook 'wl-mail-send-pre-hook 'mail-attachment-check)
     (add-hook 'wl-mail-send-pre-hook 'mail-subject-check)
     
     (add-hook 'wl-biff-notify-hook
               (lambda ()
                 (send-notification "New Mail!" "Wanderlust")))

     (setq wl-message-ignored-field-list '("^.*:")
           wl-message-visible-field-list
           '("^\\(To\\|Cc\\):"
             "^Subject:"
             "^\\(From\\|Reply-To\\):"
             "^Organization:"
             "^Message-Id:"
             "^\\(Posted\\|Date\\):"
             )
           wl-message-sort-field-list
           '("^From"
             "^Organization:"
             "^Subject"
             "^Date"
             "^To"
             "^Cc"))

     (require-and-exec 'mime-setup
                       (add-hook 'mime-view-mode-hook
                                 (lambda ()
                                   (local-set-key "f" 'browse-url))))

     (setq wl-summary-always-sticky-folder-list t
           wl-summary-line-format "|%P|%T| %Y/%M/%D %h:%m %t%[%30(%c %f%) %] %s"
           wl-summary-width 150
           wl-summary-weekday-name-lang "de"
           )

     (setq wl-thread-indent-level 2
           wl-thread-insert-opened t)

     (setq wl-summary-toggle-mime "mime")
     (require 'mime-w3m)
     (setq mime-edit-split-message nil)

     (setq wl-forward-subject-prefix "Fwd: " )

     ;; new frame for drafts
     (setq wl-draft-use-frame t
           wl-draft-always-delete-myself t
           )
     (setq wl-generate-mailer-string-function 'wl-generate-user-agent-string-1)
     (define-key wl-draft-mode-map (kbd "C-<tab>") 'bbdb-complete-name)

     ;; Proportion of the summary and message windows
     (setq wl-message-window-size '(3 . 7))

     (add-hook 'wl-mail-setup-hook
               (lambda ()
                 (setq make-backup-files nil)
                 (setq fill-column 72)
                 (turn-on-auto-fill)
                 (ac-flyspell-mode)
                 ))

     ;; Invert behaviour of with and without argument replies.
     (setq wl-draft-reply-without-argument-list
           '(("Reply-To" ("Reply-To") nil nil)
             ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
             ("From" ("From") nil nil)))

     (setq wl-draft-reply-with-argument-list
           '(("Followup-To" nil nil ("Followup-To"))
             ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
             ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
             ("From" ("From") ("To" "Cc") ("Newsgroups"))))

     ;; Switch reply keys
     (define-key wl-summary-mode-map (kbd "A") 'wl-summary-reply)
     (define-key wl-summary-mode-map (kbd "a") 'wl-summary-reply-with-citation)
     ))
(provide 'cofi-mail)
