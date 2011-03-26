(add-hook 'mail-citation-hook 'trivial-cite)
(setq tc-make-attribution (function tc-simple-attribution-kai))

(defun mail-attachment-check ()
  "Check if attachment is mentioned but not included"
  (interactive)
  (let ((attachment-regexp "[Aa]ttachment\\|[Aa]ttached\\|[Aa]nbei\\|[Bb]eiliegend\\|[Aa]anhang\\|angeh\\(ae\\|ä\\)ngt"))
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
(setq pgg-default-scheme 'gpg
      pgg-scheme 'gpg
      pgg-query-keyserver t
      pgg-default-keyserver-address "pgp.mit.edu"
      pgg-default-user-id "Michael Markert <markert.michael@googlemail.com>"
      pgg-gpg-user-id "Michael Markert <markert.michael@googlemail.com>"
      pgg-encrypt-for-me t
      pgg-decrypt-automatically t
      pgg-gpg-use-agent t
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

(defun cofi/write-mail ()
  (interactive)
  (setq make-backup-files nil)
  (setq fill-column 72)
  (turn-on-auto-fill)
  (auto-complete-mode)
  (yas/minor-mode)
  (auto-dictionary-mode)
  (turn-on-flyspell))

(eval-after-load "wl"
  '(progn
     (add-hook 'wl-mail-send-pre-hook 'mail-attachment-check)
     (add-hook 'wl-mail-send-pre-hook 'mail-subject-check)
     
     (add-hook 'wl-biff-notify-hook
               (lambda ()
                 (x-urgency-hint (selected-frame) t)
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
                                   (local-set-key "f" 'browse-url)
                                   (local-set-key "F" 'w3m-view-url-with-external-browser))))

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

     (setq wl-draft-use-frame t
           wl-draft-always-delete-myself t
           wl-auto-save-drafts-interval nil
           )
     (setq wl-generate-mailer-string-function 'wl-generate-user-agent-string-1)
     (define-key wl-draft-mode-map (kbd "C-<tab>") 'bbdb-complete-name)

     ;; Proportion of the summary and message windows
     (setq wl-message-window-size '(3 . 7))

     (add-hook 'wl-mail-setup-hook 'cofi/write-mail)

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
     (define-key wl-summary-mode-map (kbd "D") 'wl-thread-delete)
     (define-key wl-summary-mode-map (kbd "<f12>") 'offlineimap)
     (define-key wl-folder-mode-map (kbd "<f12>") 'offlineimap)
     ))

(require-and-exec 'mairix
    (setq mairix-file-path "~/Mail/"
          mairix-search-file "mairix"
          mairix-mail-program 'wl
          mairix-wl-search-folder-prefix ".")

    ;; From http://www.emacswiki.org/emacs/hgw-init-wl.el
    (defun mairix-wl-display (folder)
      "Display FOLDER using Wanderlust."
      ;; If Wanderlust is running (Folder buffer exists)...
      (if (get-buffer wl-folder-buffer-name)
          ;; Check if we are in the summary buffer, close it and
          ;; goto the Folder buffer
          (if (string= (buffer-name) wl-summary-buffer-name)
              (progn
                (wl-summary-exit t)
                (set-buffer (get-buffer wl-folder-buffer-name))))
        ;; Otherwise Wanderlust is not running so start it
        (wl))
      ;; From the Folder buffer goto FOLDER first stripping off mairix-file-path
      ;; to leave the wl folder name
      (when (string-match
             (concat (regexp-quote (expand-file-name mairix-file-path)) "\\(.*\\)")
             folder)
        (wl-folder-goto-folder-subr
         (concat mairix-wl-search-folder-prefix (match-string 1 folder)))))


    (defun mairix-wl-fetch-field (field)
      "Get mail header FIELD for current message using Wanderlust."
      (when wl-summary-buffer-elmo-folder
        (elmo-message-field
         wl-summary-buffer-elmo-folder
         (wl-summary-message-number)
         (intern (downcase field)))))

    (add-to-list 'mairix-display-functions '(wl mairix-wl-display))
    (add-to-list 'mairix-get-mail-header-functions '(wl mairix-wl-fetch-field))

    (eval-after-load "wl"
      '(progn
         (define-key wl-folder-mode-map (kbd "C-/") 'mairix-search)
         (define-key wl-summary-mode-map (kbd "C-/") 'mairix-search)))

    (global-set-key (kbd "<f9>")
                    (let ((mairix-map (make-sparse-keymap)))
                      (define-key mairix-map "/" 'mairix-search)
                      (define-key mairix-map "s" 'mairix-save-search)
                      (define-key mairix-map "i" 'mairix-use-saved-search)
                      (define-key mairix-map "e" 'mairix-edit-saved-searches)
                      (define-key mairix-map "w" 'mairix-widget-search)
                      (define-key mairix-map "u" 'mairix-update-database)
                      (define-key mairix-map "f" 'mairix-search-from-this-article)
                      (define-key mairix-map "t" 'mairix-search-thread-this-article)
                      (define-key mairix-map "b" 'mairix-widget-search-based-on-article)
                      mairix-map))
    )

(provide 'cofi-mail)
