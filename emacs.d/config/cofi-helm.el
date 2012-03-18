(add-to-loadpath "~/.elisp/vendor/helm")
;; Settings ----------------------------------------
(setq helm-c-boring-file-regexp
      (rx (or
           ;; directories
           (and "/"
              (or ".svn" "CVS" "_darcs" ".git" ".hg" "auto" "_region_" ".prv" "__pycache__")
              (or "/" eol))
           ;; files
           (and line-start  (or ".#" "."))
           (and (or ".class" ".la" ".o" "~" ".pyc") eol)))

      helm-c-boring-buffer-regexp
      (rx (or
           (and line-start  " ")
           ;; helm-buffer
           "*helm"
           "*ac-mode-"
           "Map_Sym.txt"
           "*Ibuffer*"
           "*Help*"
           "*Pp Eval Output*"
           "*Completions*"
           "*Customize"
           "*Messages*")))

(setq helm-idle-delay 0.3
      helm-input-idle-delay 0
      helm-quick-update t
      helm-candidate-number-limit nil
      helm-su-or-sudo "sudo"
      helm-allow-skipping-current-buffer nil
      helm-enable-shortcuts t)

(setq helm-M-x-requires-pattern 0)

(setq helm-c-locate-command (format "locate -d %s -i -r %%s" (cofi/var-file "locate.db")))

(setq helm-c-default-info-index-list '("elisp" "cl" "org" "gnus" "tramp" "stumpwm"
                                       "zsh" "coreutils" "find" "libc"
                                       "make" "emacs" "eieio" "latex2e"
                                       "gawk" "sed" "wget" "binutils" "ld"
                                       "grep" "gzip" "libtool"
                                       "texinfo" "info" "gdb"
                                       "sphinx" "python"))

;; --------------------------------------------------
(require-and-exec 'helm
  (require 'helm-config)
  (require 'helm-match-plugin)

  ;; helm for ffap behaves broken
  (push  '(find-file-at-point . ido-completing-read) helm-completing-read-handlers-alist)
  (helm-mode 1)
  (cofi/set-key 'global "M-x" 'helm-M-x)

  ;; From browse-kill-ring.el
  (defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
    "If last action was not a yank, run `browse-kill-ring' instead."
    ;; yank-pop has an (interactive "*p") form which does not allow
    ;; it to run in a read-only buffer.  We want browse-kill-ring to
    ;; be allowed to run in a read only buffer, so we change the
    ;; interactive form here.  In that case, we need to
    ;; barf-if-buffer-read-only if we're going to call yank-pop with
    ;; ad-do-it
    (interactive "p")
    (if (not (eq last-command 'yank))
        (helm-show-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))

  (defadvice evil-paste-pop (around evil-browse-kill-ring (arg) activate)
    (interactive "p")
    (if (not (memq last-command '(yank evil-paste-before evil-paste-pop evil-paste-after)))
        (helm-show-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))

  (require-and-exec 'descbinds-helm
    (descbinds-helm-install))
  ;; Sources ----------------------------------------
  (require-and-exec 'lacarte
    (defvar helm-c-source-current-buffer-lacarte
      '((name . "Lacarte")
        (init . (lambda () (require 'lacarte)))
        (candidates . (lambda ()
                      (with-helm-current-buffer
                        (delete '(nil) (lacarte-get-overall-menu-item-alist)))))
        (candidate-number-limit . 9999)
        (action . helm-c-call-interactively))))

  (defun cofi/helm-dir-deep (source-name dir &optional dotfiles fmatch dmatch)
    "Returns an helm source for a particular directory."
    `((name . ,(concat source-name))
      (candidates . ,(ls-files-deep dir dotfiles fmatch dmatch))
      (action . (("Open" . find-file)))
      (type . file)))
  (defun cofi/helm-dir-flat (source-name dir &optional dotfiles fmatch dmatch)
    "Returns an helm source for a particular directory."
    `((name . ,(concat source-name))
      (candidates . ,(ls-files-deep-1 dir dotfiles fmatch dmatch))
      (action . (("Open" . find-file)))
      (type . file)))
  (defun cofi/helm-dir (source-name dir &optional dotfiles match)
    "Returns an helm source for a particular directory."
    `((name . ,(concat source-name))
      (candidates . ,(ls-files dir dotfiles match))
      (action . (("Open" . find-file)))
      (type . file)))
  ;; --------------------------------------------------
  ;; helms ----------------------------------------
  (defalias 'cofi/helm-buffers 'helm-buffers-list)

  (defun cofi/helm-files ()
    (interactive)
    (helm :sources '( helm-c-source-recentf
                      helm-c-source-file-cache
                      helm-c-source-files-in-current-dir+
                      helm-c-source-files-in-all-dired
                      helm-c-source-locate)
          :buffer "*helm with files*"
          :keymap helm-find-files-map))

  (defvar cofi/helm-uni-sources '())
  (defvar cofi/helm-config-sources '())
  (defun cofi/update-helm-sources ()
    (interactive)
    (setq cofi/helm-uni-sources
          (let* ((dirs '("OS" "IPL" "BP" "AI" "CE" "PS"))
                 (subdirs '("aufgaben" "uebungen" "notes" "praktika"))
                 (file-filters `(("aufgaben" ,(gen-extension-re "pdf"))
                                 ("uebungen" ,(concat "Makefile" "\\|"
                                                      (gen-extension-re "tex"
                                                                        "gp"
                                                                        "java"
                                                                        "py"
                                                                        "hs"
                                                                        "lisp"
                                                                        "org"
                                                                        "clj")))
                                 ("notes" ,(gen-extension-re "org"))
                                 ))
                 (combinator (lambda (x y) (format "%s/%s" x y)))
                 (path "~/Work/Uni/")
                 (combinations (combinate dirs subdirs combinator))
                 (full (mapcar (lambda (d) (concat path d)) combinations)))

            (loop for name in combinations
                  for dir in full
                  when (file-accessible-directory-p dir)
                  collect (cofi/helm-dir-deep name dir t
                                              (cadr (assoc (file-name-nondirectory dir)
                                                           file-filters))))))

    (setq cofi/helm-config-sources
          `(
            ,(cofi/helm-dir-flat "Emacs" "~/config/dotfiles/emacs.d/" t ".el$")
            ,(cofi/helm-dir-deep "Snippets" "~/config/dotfiles/yasnippets/" t)
            ,(cofi/helm-dir-deep "Zsh" "~/config/dotfiles/zsh/" t)
            ((name . "Dot")
             (candidates . ,(append (ls-files "~/config/dotfiles/" t)
                                    (ls-files "~/config/dotfiles/bin" t)
                                    (ls-files "~/config/dotfiles/quick" t)))
             (action . (("Open" . find-file)))
             (type . file))
            ))
    )

  (run-with-timer 0 600 #'cofi/update-helm-sources)

  (defvar helm-makefile-path nil)
  (defvar helm-makefile-targets
    `((name . "Make")
      (init . (lambda () (setq helm-makefile-path (find-makefile default-directory))))
      (candidates . (lambda () (makefile-targets (concat helm-makefile-path "Makefile"))))
      (volatile)
      (action . (("Make target" . (lambda (candidate)
                                    (compile (concat "cd " helm-makefile-path
                                                     " && make " candidate))))))))

  (defun cofi/helm-uni ()
    (interactive)
    (helm :sources cofi/helm-uni-sources
          :buffer "*helm uni*"
          :keymap helm-find-files-map))

  (defun cofi/helm-config ()
    (interactive)
    (helm :sources cofi/helm-config-sources
          :buffer "*helm config*"
          :keymap helm-find-files-map))
  (defun cofi/helm-make ()
    (interactive)
    (helm :sources helm-makefile-targets
          :buffer "*helm make*"))

  (when (fboundp 'lacarte-execute-command)
    (defun cofi/helm-lacarte ()
      (interactive)
      (helm :sources helm-c-source-current-buffer-lacarte
            :buffer "*helm lacarte*"))
    (cofi/set-key 'global "<f10>" 'cofi/helm-lacarte))
  )
(provide 'cofi-helm)
