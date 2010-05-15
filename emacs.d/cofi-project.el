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

    (define-key ibuffer-mode-map (kbd "/ k") 'ibuffer-filter-by-project)
    )

  (global-set-key (kbd "C-c p c") 'project-compile)
  (global-set-key (kbd "C-c p l") 'project-load)
  (global-set-key (kbd "C-c p a") 'project-ack)
  (global-set-key (kbd "C-c p g") 'project-grep)
  (global-set-key (kbd "C-c p o") 'project-multi-occur)
  (global-set-key (kbd "C-c p u") 'project-unload)
  (global-set-key (kbd "C-c p f") 'project-find-file-ido)
  (global-set-key (kbd "C-c p i") 'project-index)
  (global-set-key (kbd "C-c p s") 'project-status)
  (global-set-key (kbd "C-c p h") 'project-home)
  (global-set-key (kbd "C-c p d") 'project-dired)
  (global-set-key (kbd "C-c p t") 'project-tags)

  (load "projects" 'noerror)
  )
(provide 'cofi-project)
