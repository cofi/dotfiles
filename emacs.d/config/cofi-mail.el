(add-to-loadpath "~/.elisp/vendor/gnus/lisp")
(require 'gnus-load)

;;; offlineimap
(setq offlineimap-command "offlineimap -u MachineUI")

(on-mail-instance
 (run-with-timer (* 60 10) (* 60 10) #'offlineimap))

(defun mail-attachment-check ()
  "Check if attachment is mentioned but not included"
  (interactive)
  (let ((attachment-regexp "^[^>].*\\(:?[Aa]ttachment\\|[Aa]ttached\\|[Aa]nbei\\|[Bb]eiliegend\\|[Aa]anhang\\|angeh\\(ae\\|ä\\)ngt\\)"))
    (save-excursion
      (goto-char 0)
      (unless (or (re-search-forward "^Content-Disposition: attachment" nil t)
                  (re-search-forward "disposition=attachment" nil t))
        (goto-char 0)
        (when ;; Check for mentioned attachments
            (re-search-forward attachment-regexp nil t)
          (unless (y-or-n-p "Attachment maybe missing. Send? ")
            (error "Abort.")))))))

(defun mail-subject-check ()
  "Check if subject is missing."
  (let ((subject-found
         (cofi/with-restrict-to-header
          (goto-char (point-min))
          (re-search-forward "^Subject: \\(.*\\)$" nil 'noerror))))
    (unless (or (and subject-found
                     (> (length (match-string 1)) 1))
                (y-or-n-p "Subject missing. Send? "))
      (error "Abort."))))

(defmacro cofi/with-restrict-to-header (&rest body)
  "Restrict buffer to mail headers of messages before executing body.
Also wraps in `save-excursion' and `save-restriction'."
  `(save-excursion
     (save-restriction
       (goto-char (point-min))
       (when (re-search-forward "^--text follows this line--$" nil 'noerror)
         (narrow-to-region (point-min) (point))
         ,@body))))

(setq mail-user-agent 'gnus-user-agent)

;;; for manual setting mail config
(defun cofi/write-mail ()
  (interactive)
  (setq make-backup-files nil)
  (setq fill-column 72)
  (auto-fill-mode)
  (auto-complete-mode)
  (yas/minor-mode)
  (auto-dictionary-mode)
  (mail-abbrevs-mode)
  (flyspell-mode))

(provide 'cofi-mail)
