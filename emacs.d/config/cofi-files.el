(mapc (lambda (ext)
        (push ext completion-ignored-extensions))
      '(
        ".dvi" ".djvu" ".ps"
        ".mov" ".mp4" ".ogv"
        ".mp3" ".ogg"
        ".doc" ".ods" ".odt" ".pps" ".ppt"
        ))

(setq ido-enable-regexp t
      ido-enable-dot-prefix t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-url-at-point nil
      ido-use-filename-at-point nil
      ido-ignore-extensions t
      ido-save-directory-list-file (cofi/var-file "emacs/ido"))

(eval-after-load "ido"
  '(progn
     (setq ido-ignore-buffers (append
                               ido-ignore-buffers
                               '(
                                 "\\` "
                                 "\\`\\*.*\\*"
                                 "_region_"
                                 )))
     (setq ido-ignore-directories (append
                                   ido-ignore-directories
                                   '(
                                     "^auto/$"
                                     "\\.prv/"
                                     "_region_"
                                     )))
     (setq ido-ignore-files (append
                             ido-ignore-files
                             '(
                               "^auto/$"
                               "_region_"
                               )))
     (fset 'ido-directory-too-big-p #'ignore)))

(ido-mode 'files)
(ido-everywhere 1)

;;; Dired ==============================
(setq dired-recursive-copies 'always
      dired-dwim-target t
      dired-listing-switches "-alh"
      wdired-allow-to-change-permissions t)
(defvar cofi/dired-find-file-external-program "kde-open")
(defun cofi/dired-find-file-external ()
  (interactive)
  (start-process "*cofi-dired-find-filed-external*"
                 nil
                 cofi/dired-find-file-external-program
                 (dired-get-file-for-visit)))

(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'dired-mode-hook (gen-local-fill-keymap-hook
                            "C-<return>" #'cofi/dired-find-file-external
                            "P" #'cofi/dired-pack-marked))

(add-hook 'dired-mode-hook (lambda ()
                             (on-mail-instance
                               (turn-on-gnus-dired-mode))))

(defun cofi/dired-pack-marked (format name)
  "Pack marked files in a dired buffer to an archive."
  (interactive (list (progn
                       (unless (dired-get-marked-files)
                         (error "No files marked."))
                       (completing-read "Archive format: " '("zip"
                                                             "tar.xz"
                                                             "tar.bz2"
                                                             "tar.gz")
                                        nil t nil nil "zip"))
                     (read-string "Archive name: ")))
  (let ((files (mapconcat #'identity (dired-get-marked-files) " ")))
    (async-shell-command
     (cond
      ((string= format "zip") (format "zip %s.zip %s" name files))
      (t (format "tar -caf %s%s %s" name format files))))))

;;; ========================================

(setq ack-prompt-for-directory 'unless-guessed)
(setq confirm-nonexistent-file-or-buffer nil)

;;; Show trailing whitespace in file-buffers
(add-hook 'find-file-hook (lambda () (setq show-trailing-whitespace t)))

(setq require-final-newline t)


(require 'cofi-helm)
(defalias 'cofi/file (f-alt 'helm-find-files
                            'ido-find-file))
(defalias 'cofi/file-alternate (f-alt 'ido-find-file
                                      'helm-find-files))
(defalias 'cofi/buffer (f-alt 'cofi/helm-buffers
                              'ido-switch-buffer))
(defalias 'cofi/buffer-alternate (f-alt 'ido-switch-buffer
                                        'cofi/helm-buffers))
;; bookmarks ==============================
(setq bookmark-default-file "~/var/emacs/bookmarks"
      bmkp-bmenu-commands-file "~/var/emacs/bmkp-bmenu-commands.el"
      bmkp-bmenu-state-file "~/var/emacs/bmkp-bmenu-state.el")

(require-and-exec 'bookmark+)
;;; ========================================
;;; backups & autosave
(setq backup-directory-alist `(("" . ,(cofi/var-file "emacs/backups"))))
(add-hook 'find-file-hook 'auto-save-mode)
(setq auto-save-timeout 20)
;;; ========================================
;;; recent files ====================
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 200)
(setq recentf-save-file (cofi/var-file "emacs/recentf"))
(require-and-exec 'recentf
   (recentf-mode 1)
   (setq recentf-exclude '(
                           "\\.recentf"
                           "\\.ido\\.last"
                           "\\.keychain/.*?-sh\\(-gpg\\)?"
                           ))
   (add-hook 'kill-emacs-hook #'recentf-cleanup)
   )
;;; ========================================
;;; tramp ==============================
(setq tramp-persistency-file-name (cofi/var-file "emacs/tramp"))
(setq tramp-default-method "ssh")
;;; ==============================
(defun cofi/create-directories-for-file  (&optional file)
  (interactive "D")
  (let ((dir (file-name-directory (or file buffer-file-name))))
    (unless (file-exists-p dir)
      (make-directory dir 'create-parents))))

(add-hook 'before-save-hook 'cofi/create-directories-for-file)

(setq bc-bookmark-file (cofi/var-file "emacs/breadcrumb"))

(provide 'cofi-files)
