(defun cofi-do-mail ()
  (interactive)
  (set-buffer-modified-p nil)
  (setq make-backup-files nil)
  (turn-on-auto-fill)
  (setq fill-column 72)
  (ac-flyspell-mode))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

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

(add-hook 'wl-mail-send-pre-hook 'mail-attachment-check)
(add-hook 'wl-mail-send-pre-hook 'mail-subject-check)

;; Those are SEMI-PGG Settings they differ from Emacs own PGG
;; and sadly there is no gpg-agent support ...
(setq pgg-default-scheme 'gpg
      pgg-scheme 'gpg
      pgg-query-keyserver t
      pgg-default-keyserver-address "subkeys.pgp.net"
      pgg-default-user-id "Michael Markert <markert.michael@googlemail.com>"
      pgg-encrypt-for-me t
      pgg-decrypt-automatically t
      )

(provide 'cofi-mail)
