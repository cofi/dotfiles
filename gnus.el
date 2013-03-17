;;; org-contacts
(require-and-exec 'org-contacts
  (org-contacts-gnus-insinuate)
  ;; deactivate store last mail
  (fset 'org-contacts-gnus-store-last-mail #'NOP)

  (defvar cofi/org-contacts-ignore-mail-rx
    (rx "@public.gmane.org" string-end))

  ;; override to include the ignore rx
  (defun org-contacts-check-mail-address (mail)
    "Add MAIL address to contact at point if it does not have it."
    (let ((mails (org-entry-get (point) org-contacts-email-property)))
      (unless (or  (member mail (split-string mails))
                  (string-match-p cofi/org-contacts-ignore-mail-rx mail))
        (when (yes-or-no-p
               (format "Do you want to add this address to %s?" (org-get-heading t)))
          (org-set-property org-contacts-email-property (concat mails " " mail)))))))

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
                   ;; only ask if there is no <#secure cookie
                   (unless (save-excursion
                             (goto-char 0)
                             (re-search-forward "<#secure" nil 'noerror))
                     (if (y-or-n-p "Sign message? ")
                         (mml-secure-message-sign))))
                 #'mail-attachment-check)

;;; writing
(add-hook 'message-mode-hook 'cofi/write-mail)
(setq gnus-add-to-list t)
(setq message-elide-ellipsis "\n[%l lines snipped]\n")

(setq message-kill-buffer-on-exit t
      message-dont-reply-to-names user-mail-rx)

(setq gnus-gcc-mark-as-read t)
(setq gnus-group-line-format "%M%S%p%P%5y[%T]:%B%(%G%)\n")

;;; fancy summary
(setq gnus-summary-line-format "%U|%R|%z|%ur|%(%-20&user-date;|%-30B%[%5L: %-40n%] %s%)\n")
(setq gnus-summary-make-false-root 'dummy
      gnus-sum-thread-tree-false-root      "┈─►"
      gnus-sum-thread-tree-single-indent   "◈   "
      gnus-sum-thread-tree-root            "●   "
      gnus-sum-thread-tree-vertical        "│   "
      gnus-sum-thread-tree-leaf-with-other "├─►"
      gnus-sum-thread-tree-single-leaf     "╰─►"
      gnus-sum-thread-tree-indent          "    ")


(setq gnus-cached-mark ?☍
      gnus-canceled-mark ?↗
      gnus-del-mark ?✗
      gnus-dormant-mark ?⚐
      gnus-expirable-mark ?♻
      gnus-forwarded-mark ?↪
      gnus-killed-mark ?☠
      gnus-process-mark ?⚙
      gnus-read-mark ?✓
      gnus-recent-mark ?✩
      gnus-replied-mark ?↺
      gnus-unread-mark ?✉
      gnus-unseen-mark ?★
      gnus-ticked-mark ?⚑)

(setq mm-text-html-renderer 'gnus-w3m)

;;; search
(require 'nnir)

;;; washing
;; hide citations
(setq gnus-treat-hide-citation t
      gnus-cited-lines-visible '(2 . 5))
(setq gnus-cited-closed-text-button-line-format "%(%{[%n lines snipped]%}%)\n")
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
      mml2015-encrypt-to-self t
      mml2015-signers '("B4523295"))

;;; mime
(setq gnus-mime-view-all-parts t
      gnus-buttonized-mime-types nil
      gnus-unbuttonized-mime-types '("text/plain"))
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(dolist (type '("image/.*" "message/rfc822" "text/x-patch"))
  (push type mm-attachment-override-types))
(setq mm-inline-large-images 'resize)

(setq mm-uu-diff-groups-regexp ".")

(defun cofi-gnus-copy-archived-at ()
  "Copy archived-at url of current article to killring and clipboard."
  (interactive)
  (let ((archived (gnus-with-article-headers
                   (mail-extract-address-components
                    (mail-fetch-field "Archived-At")))))
    (when archived
      (let ((x-select-enable-clipboard t)
            (field (second archived)))
        (let ((url (if (not (begins-with field "http:"))
                       (concat "http:" (substring field 5))
                     field)))
          (kill-new url)
          (message "Copied: %s" url))))))

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
                                      "y"     'cofi-gnus-copy-archived-at
                                      "~"     (cmd gnus-mark-as-read (gnus-summary-mark-article nil ?R))
                                      "S-SPC" 'gnus-summary-prev-page))
(require 'offlineimap)
(add-hook 'gnus-group-mode-hook
          (gen-local-fill-keymap-hook "<f12>" 'offlineimap))

(add-hook 'message-mode-hook
          (gen-local-fill-keymap-hook "M-RET" 'yas/expand))

(setq message-citation-line-function #'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a, %b %d %Y (%H:%M), %f wrote: \n")

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
(setq gnus-visible-headers (concat gnus-visible-headers "\\|^Archived-At:"))

(gnus-demon-add-handler 'gnus-demon-scan-news 10 t)

(setq gnus-notifications-use-google-contacts nil)
(require 'gnus-notifications)
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)

(add-hook 'gnus-after-getting-new-news-hook
          (defun cofi/set-mail-urgency ()
            (loop for (group . rest) in gnus-newsrc-alist
                  when (and (<= (gnus-group-level group) gnus-notifications-minimum-level)
                          (let ((unread (gnus-group-unread group)))
                            (and (numberp unread)
                               (> unread 0))))

                  do (prog1
                         (x-urgency-hint (selected-frame) t)
                       (return)))))

(setq gnus-check-new-newsgroups nil)
(setq gnus-asynchronous t)


(gnus-add-configuration '(reply-yank (vertical 1.0
                                               (horizontal 1.0
                                                           (message .5 point)
                                                           (article 1.0)))))
(gnus-add-configuration '(reply (vertical 1.0
                                          (horizontal 1.0
                                                      (message .5 point)
                                                      (article 1.0)))))
