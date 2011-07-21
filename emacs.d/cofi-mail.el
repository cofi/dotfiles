(add-to-loadpath "~/.elisp/vendor/gnus/lisp")
(require 'gnus-load)

(add-hook 'mail-citation-hook 'trivial-cite)
(setq tc-make-attribution (function tc-simple-attribution-kai))

;;; offlineimap
(setq offlineimap-command "offlineimap -u MachineUI")
(setq offlineimap-enable-mode-line-p '(member major-mode
                                              '(offlineimap-mode gnus-group-mode wl-folder-mode)))

(defun mail-attachment-check ()
  "Check if attachment is mentioned but not included"
  (interactive)
  (let ((attachment-regexp "[Aa]ttachment\\|[Aa]ttached\\|[Aa]nbei\\|[Bb]eiliegend\\|[Aa]anhang\\|angeh\\(ae\\|ä\\)ngt"))
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
  (cofi/with-restrict-to-header
   (goto-char (point-min))
   (let ((subject-found (re-search-forward "^Subject: \\(.*\\)$" nil 'noerror)))
     (unless (or (and subject-found
                      (> (length (match-string 1)) 1))
                 (y-or-n-p "Subject missing. Send? "))
       (error "Abort.")))))

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
  (turn-on-auto-fill)
  (auto-complete-mode 1)
  (yas/minor-mode 1)
  (auto-dictionary-mode 1)
  (mail-abbrevs-mode 1)
  (turn-on-flyspell))

(provide 'cofi-mail)
