(when (file-exists-p "/usr/local/share/emacs/site-lisp/wl/")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/wl/"))

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

(require-and-exec 'pgg
                  (setq pgg-default-scheme 'gpg
                        pgg-scheme 'gpg
                        pgg-query-keyserver t
                        pgg-default-keyserver-address "subkeys.pgp.net"
                        pgg-gpg-user-id "Michael Markert <markert.michael@googlemail.com>"
                        pgg-gpg-use-agent t
                        pgg-encrypt-for-me t
                        )
                  )

;; from http://box.matto.nl/wanderlustgpg.html
;; (require-and-exec 'mailcrypt
;;    (add-hook 'wl-summary-mode-hook 'mc-install-read-mode)
;;    (add-hook 'wl-mail-setup-hook 'mc-install-write-mode)

;;    (defun mc-wl-verify-signature ()
;;      (interactive)
;;      (save-window-excursion
;;        (wl-summary-jump-to-current-message)
;;        (mc-verify)))

;;    (defun mc-wl-decrypt-message ()
;;      (interactive)
;;      (save-window-excursion
;;        (wl-summary-jump-to-current-message)
;;        (let ((inhibit-read-only t))
;;          (mc-decrypt))))

;;    (setq mc-modes-alist
;;          (append
;;           (quote
;;            ((wl-draft-mode (encrypt . mc-encrypt-message)
;;                            (sign . mc-sign-message))
;;             (wl-summary-mode (decrypt . mc-wl-decrypt-message)
;;                              (verify . mc-wl-verify-signature))))
;;           mc-modes-alist))
;;    )

(provide 'cofi-mail)
