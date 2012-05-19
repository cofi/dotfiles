(eval-after-load "gtags"
  '(fill-keymap gtags-mode-map
                "M-,"   #'gtags-find-tag-from-here
                "C-M-," #'gtags-find-rtag))

(defun cofi/update-or-create-gtags (&optional create)
  "Update a global database or create a new one if none exists
and `create' is passed (or command is interactively called with
prefix."
  (interactive "P")
  (if (= 0 (call-process "global" nil nil nil "-p"))
      (start-process "update global" nil "global" "-u")
    (when create
      (let ((default-directory (read-directory-name "Source directory: ")))
        (start-process "create global" nil "gtags")))))

(add-hook 'c-mode-common-hookmmon-hook #'gtags-mode)
(add-hook 'c-mode-common-hook #'cofi/update-or-create-gtags)

(provide 'cofi-tags)
