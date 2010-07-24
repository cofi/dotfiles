(defun cofi-do-mail ()
  (interactive)
  (set-buffer-modified-p nil)
  (setq make-backup-files nil)
  (turn-on-auto-fill)
  (setq fill-column 72)
  (ac-flyspell-mode))

(when (file-exists-p "/usr/local/share/emacs/site-lisp/wl/")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/wl/"))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(defun cofi-wl-frame ()
  "Start wl and set frame title."
  (set-frame-size (selected-frame) 150 100)
  (setq frame-title-format "Wanderlust Mail")
  (wl))

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

(provide 'cofi-mail)
