;;; org-contacts
(require-and-exec 'org-contacts
  (org-contacts-gnus-insinuate)
  ;; deactivate store last mail
  (fset 'org-contacts-gnus-store-last-mail #'NOP))

;;; methods
(setq imap-shell-program "/usr/lib/dovecot/imap -c ~/config/dovecot.conf"
      gnus-select-method '(nnimap "Mail"
                                  (nnimap-stream shell))
      gnus-secondary-select-methods '((nntp "news.gmane.org")
                                      (nntp "news.eternal-september.org")))

;;; sending
(setq sendmail-program "msmtp"
      message-sendmail-extra-arguments '("-a" "gmail")
      message-send-mail-function 'message-send-mail-with-sendmail)

(defun cofi/mml-ask-if-send ()
  "Ask for send with preview."
  (save-window-excursion
    (mml-preview)
    (unless (y-or-n-p "Send message? ")
      (error "Abort."))))

(add-all-to-hook 'message-send-hook
                 #'cofi/mml-ask-if-send
                 (lambda ()
                   (if (y-or-n-p "Sign message? ")
                       (mml-secure-message-sign)))
                 #'mail-subject-check
                 #'mail-attachment-check)

;;; writing
(add-hook 'message-mode-hook #'cofi/write-mail)
(setq gnus-add-to-list t)

;;; fancy summary
(setq gnus-summary-line-format "%U|%R|%z|%ur|%(%-20&user-date;|%-30B%[%5L: %-40f%] %s%)\n")
(setq gnus-summary-make-false-root 'dummy
      gnus-sum-thread-tree-false-root      "┈─►"
      gnus-sum-thread-tree-single-indent   "●   "
      gnus-sum-thread-tree-root            "●   "
      gnus-sum-thread-tree-vertical        "│   "
      gnus-sum-thread-tree-leaf-with-other "├─►"
      gnus-sum-thread-tree-single-leaf     "╰─►"
      gnus-sum-thread-tree-indent          "    ")

(setq mm-text-html-renderer 'w3m)

;;; search
(require 'nnir)

;;; washing
;; hide citations
(setq gnus-treat-hide-citation t
      gnus-cited-lines-visible '(2 . 5))
;; fill long lines on first part or text/plain
(setq gnus-treat-fill-long-lines '(or first (typep "text/plain")))

;;; crypt
(setq gnus-message-replysign t
      gnus-message-replyencrypt t
      gnus-message-replysignencrypted t)
(setq gnus-message-replysign t)
(setq gnus-summary-force-verify-and-decrypt t)
(setq mm-verify-option 'always)
(setq mml2015-verbose t
      mml2015-encrypt-to-self t)

;;; mime
(setq gnus-mime-view-all-parts t
      gnus-buttonized-mime-types nil
      gnus-unbuttonized-mime-types '("text/plain"))
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-completing-read-function #'gnus-ido-completing-read)

;;; keys
(add-hook 'gnus-summary-mode-hook
          (gen-local-fill-keymap-hook "M-p"   'gnus-summary-prev-article
                                      "M-n"   'gnus-summary-next-article
                                      "r"     'gnus-summary-wide-reply-with-original
                                      "B d"   'gnus-summary-delete-article
                                      "C-M-n" 'gnus-summary-next-thread
                                      "C-M-p" 'gnus-summary-prev-thread
                                      "A t"   'gnus-summary-refer-thread
                                      "M-t"   'gnus-summary-refer-thread
                                      ))
(require 'offlineimap)
(add-hook 'gnus-group-mode-hook
          (gen-local-fill-keymap-hook "<f12>" 'offlineimap))

(add-hook 'message-mode-hook
          (gen-local-fill-keymap-hook "M-RET" 'yas/expand))

(defun gnus-user-format-function-r (headers)
  "Classify headers on how I'm adressed."
  (let ((to (gnus-extra-header 'To headers))
        (all (remove-duplicates (flatten
                                 (mapcar (lambda (h)
                                           (split-string (gnus-extra-header h headers)
                                                         ", " 'omit-nulls))
                                         '(To Cc BCc)))
                                :test #'string=)))
    (if (string-match user-mail-rx to)
        (if (> (length all) 1)
            ;; other recipients
            "»"
          ;; sole recipient
          "!")
      ;; unadressed
      "-")))

;;; scan for news every 10 minutes
(gnus-demon-add-handler 'gnus-demon-scan-news 10 t)

(require-and-exec 'gnus-desktop-notify
  (setq gnus-desktop-notify-function (lambda (groups)
                                       ;; set urgency hint on news
                                       (x-urgency-hint (selected-frame) t)
                                       (gnus-desktop-notify-send groups)))
  (gnus-desktop-notify-mode))
