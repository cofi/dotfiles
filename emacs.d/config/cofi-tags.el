(eval-after-load "gtags"
  '(fill-keymap gtags-mode-map
                "M-,"   #'gtags-find-tag-from-here
                "C-M-," #'gtags-find-rtag))

(defun cofi/update-or-create-gtags (&optional create)
  "Update a global database or create a new one if none exists
and `create' is passed (or command is interactively called with
prefix."
  (interactive "P")
  (if (= 0 (shell-command "global -p"))
      (shell-command "global -u")
    (when create
      (let ((default-directory (read-directory-name "Source directory: ")))
        (shell-command "gtags")))))

(add-hook 'c-mode-common-hookmmon-hook #'gtags-mode)
(add-hook 'c-mode-common-hook #'cofi/update-or-create-gtags)

(provide 'cofi-tags)
