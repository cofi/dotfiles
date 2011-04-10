(defvar ediff-after-quit-hooks nil)

(defadvice ediff-quit (after ediff-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(defvar ediff-git-emacsclient-active nil)

(defun ediff-git-merge-before-setup
  (if ediff-git-emacsclient-active
      (raise-frame)))

(defun ediff-git-merge (local remote base merged)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged)))

(defun ediff-git-merge-emacsclient (local remote base merged)
  (setq ediff-git-emacsclient-active t)
  (ediff-git-merge local remote base merged)
  ;; keep emacsclient from returning
  (recursive-edit))

(defun ediff-git-merge-after-quit
  (if ediff-git-emacsclient-active
      (exit-recursive-edit)))

(provide 'cofi-ediff)
