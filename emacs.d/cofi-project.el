(add-to-loadpath "~/.elisp/vendor/mk-project")
(require-and-exec 'mk-project
  (when (fboundp 'ibuffer)
    (defun mk/proj-buffer-p (b)
      "Is the buffer `b' part of the project?"
      (and mk-proj-name
           (or (mk-proj-buffer-p b)
               (string= (buffer-name b) mk-proj-fib-name)
               (string= (buffer-file-name b) mk-proj-tags-file))))

    (define-ibuffer-column mk-proj-col
      (:name "P")
      (if (mk/proj-buffer-p buffer) "p" ""))

    (push
     '(mark modified read-only mk-proj-col " "
            (name 18 18 :left :elide)
            " "
            (size 9 -1 :right)
            " "
            (mode 16 16 :left :elide)
            " " filename-and-process)
     ibuffer-formats)

    (push
     '(mark modified read-only mk-proj-col git-status-mini " "
            (name 18 18 :left :elide)
            " "
            (size 9 -1 :right)
            " "
            (mode 16 16 :left :elide)
            " "
            (git-status 8 8 :left)
            " " filename-and-process)
     ibuffer-formats)

    (define-ibuffer-filter project
        "Toggle current view to buffers in the defined mk-project."
      (:description "mk-project")
      (mk/proj-buffer-p buf))

    (define-key ibuffer-mode-map (kbd "/ k") 'ibuffer-filter-by-project))

  (setq mk-proj-use-ido-selection t)

  (when (boundp 'cofi-project-files)
    (mapc (lambda (filename)
            (load filename 'noerror))
          cofi-project-files))
  )
(provide 'cofi-project)
