;; (require 'elscreen-wl)

;;; user agent
;; (if (fboundp 'define-mail-user-agent)
;;     (define-mail-user-agent
;;       'wl-user-agent
;;       'wl-user-agent-compose
;;       'wl-draft-send
;;       'wl-draft-kill
;;       'mail-send-hook))

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

(add-hook 'wl-draft-mode-hook #'cofi/write-mail)

(add-hook 'mime-view-mode-hook
          (gen-local-fill-keymap-hook
           "f" 'browse-url
           "F" 'w3m-view-url-with-external-browser))

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
(setq wl-summary-line-format "|%P|%T| %Y/%M/%D %h:%m %t%[%30(%c %f%) %] %s"
      wl-summary-width nil
      wl-summary-weekday-name-lang "de"
      wl-summary-toggle-mime "mime")

(setq wl-thread-indent-level 2
      wl-thread-insert-opened t)
(setq wl-forward-subject-prefix "Fwd: " )
(setq wl-draft-always-delete-myself t
      wl-auto-save-drafts-interval nil)
(setq mime-edit-split-message nil)
(setq wl-generate-mailer-string-function 'wl-generate-user-agent-string-1)

(setq elmo-folder-update-confirm nil)
;; org-contacts
(add-hook 'wl-draft-mode-hook
          (lambda ()
            (add-to-list 'completion-at-point-functions
                         'org-contacts-message-complete-function)))

;; Proportion of the summary and message windows
(setq wl-message-window-size '(3 . 7))
(eval-after-load "wl"
  '(progn
     (require 'mime-w3m)
     ;; Invert behaviour of with and without argument replies.
     (let ((with-arg wl-draft-reply-without-argument-list)
           (without-arg wl-draft-reply-with-argument-list))
       (setq wl-draft-reply-without-argument-list without-arg
             wl-draft-reply-with-argument-list with-arg))

     (fill-keymap wl-summary-mode-map
                  ;; Switch reply keys
                  "A" 'wl-summary-reply
                  "a" 'wl-summary-reply-with-citation
                  "D" 'wl-thread-delete
                  "<f12>" 'offlineimap)
     (define-key wl-folder-mode-map (kbd "<f12>") 'offlineimap)
     (define-key wl-draft-mode-map (kbd "C-<tab>") 'completion-at-point)
     ))

;;; mu
(require 'elmo-search)
(elmo-search-register-engine
    'mu 'local-file
    :prog "mu"
    :args '("find" pattern "--fields" "l")
    :charset 'utf-8)

;;; you search by `wl-folder-goto-folder' ((kbd "g")) and entering [criterion]
(setq elmo-search-default-engine 'mu)
(setq wl-default-spec "")

(provide 'cofi-wl)
