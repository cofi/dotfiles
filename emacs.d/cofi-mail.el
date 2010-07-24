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

(provide 'cofi-mail)
