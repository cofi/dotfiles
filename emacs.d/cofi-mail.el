(defun cofi-do-mail ()
  (interactive)
  (set-buffer-modified-p nil)
  (setq make-backup-files nil)
  (mail-mode)
  (turn-on-auto-fill)
  (setq fill-column 72)
  (ac-flyspell-mode)
  (mail-text))

;; serve sup
(push '("\\(compose\\|forward\\|reply\\|resume\\)-mode$" . cofi-do-mail)
      auto-mode-alist)

(provide 'cofi-mail)
