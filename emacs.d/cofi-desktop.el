(defun emacs-process-p (pid)
  "Returns t if command of pid is emacs.
Also returns nil if pid is nil."
  (when pid
    (let* ((attributes (process-attributes pid))
           (comm (cdr (assq 'comm attributes))))
      (or (string= "emacs" comm)
         (string= "emacs.exe" comm)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(require-and-exec 'desktop
  (require 'desktop-menu))

;; File/path settings
(setq desktop-path '("~/var/emacs/desktop/")
      desktop-dirname "~/var/emacs/desktop/"
      desktop-base-file-name "desktop"
      desktop-menu-path '("~/var/emacs/desktop/menu/")
      desktop-menu-directory "~/var/emacs/desktop/menu/"
      desktop-menu-list-file "menu"
      desktop-menu-base-filename "menu-desktop")

(setq desktop-restore-eager nil
      ;; default \ filename history
      desktop-globals-to-save '(desktop-missing-file-warning
                                tags-file-name
                                tags-table-list
                                search-ring
                                regexp-search-ring
                                register-alist))
(provide 'cofi-desktop)

