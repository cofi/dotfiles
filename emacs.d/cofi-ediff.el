(defvar ediff-after-quit-hook nil)

(defadvice ediff-quit (after ediff-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hook))

(defvar ediff-git-emacsclient-active nil)
(defvar ediff-git-previous-frames nil)
(defvar ediff-git-previous-windows nil)
(defvar ediff-git-clean-frame nil)

(defun ediff-git-merge-before-setup ()
  (setq ediff-git-previous-frames (current-frame-configuration))
  (setq ediff-git-previous-windows (current-window-configuration))
  (if ediff-git-emacsclient-active
      (raise-frame)))
(add-hook 'ediff-before-setup-hook 'ediff-git-merge-before-setup)

(defun ediff-git-merge (local remote base merged)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged)))

(defun ediff-git-merge-emacsclient (local remote base merged &optional clean-frame)
  "CLEAN-FRAME' should be non-nil if call created a new frame and should be cleaned."
  (setq ediff-git-emacsclient-active t)
  (setq ediff-git-clean-frame clean-frame)
  (ediff-git-merge local remote base merged)
  ;; keep emacsclient from returning
  (recursive-edit))

;;; clean recursive edit
(add-hook 'ediff-after-quit-hook 'exit-recursive-edit)
(add-hook 'ediff-after-quit-hook (lambda () (if ediff-git-clean-frame (delete-frame))))

;;; restore windows and frames
(add-to-hooks (lambda ()
                (set-window-configuration ediff-git-previous-windows)
                (set-frame-configuration ediff-git-previous-frames))
              '(ediff-quit-hook ediff-suspend-hook))

(provide 'cofi-ediff)
