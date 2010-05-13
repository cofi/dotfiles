(defun cofi-do-mail ()
  (set-buffer-modified-p nil)
  (setq make-backup-files nil)
  (mail-mode)
  (turn-on-auto-fill)
  (setq fill-column 72)
  (ac-flyspell-mode)
  (mail-text)
  )

;; serve sup
(add-to-list 'auto-mode-alist '("\.compose-mode$" . cofi-do-mail))

(provide 'cofi-mail)
