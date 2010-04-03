(require-and-exec 'recentf
      (setq recentf-auto-cleanup 'never)
      (recentf-mode t)
      (defun recentf-ido-find-file ()
        "Find a recent file using Ido."
        (interactive)
        (let* ((file-assoc-list
                (mapcar (lambda (x)
                          (cons (file-name-nondirectory x)
                                x))
                        recentf-list))
               (filename-list
                (remove-duplicates (mapcar #'car file-assoc-list)
                                   :test #'string=))
               (filename (ido-completing-read "Choose recent file: "
                                              filename-list
                                              nil
                                              t)))
          (when filename
            (find-file (cdr (assoc filename
                                   file-assoc-list))))))
      (global-set-key (kbd "C-c r") 'recentf-ido-find-file)
)
