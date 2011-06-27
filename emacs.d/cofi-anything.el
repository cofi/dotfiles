(dolist (path '("~/.elisp/vendor/anything"
                "~/.elisp/vendor/anything/extensions/"))
  (pushnew path load-path))
;; Settings ----------------------------------------
(setq anything-command-map-prefix-key "<f7>")
(setq anything-c-boring-file-regexp
      (rx (or
           ;; directories
           (and "/"
              (or ".svn" "CVS" "_darcs" ".git" ".hg" "auto" "_region_" ".prv")
              (or "/" eol))
           ;; files
           (and line-start  (or ".#" "."))
           (and (or ".class" ".la" ".o" "~") eol)))

      anything-c-boring-buffer-regexp
      (rx (or
           (and line-start  " ")
           ;; anything-buffer
           "*anything"
           ;; echo area
           " *Echo Area" " *Minibuf"
           (and line-start (optional " ") "*" (* anything) "*")
           "Map_Sym.txt"
           )))

(setq anything-idle-delay 0.3
      anything-input-idle-delay 0
      anything-quick-update t
      anything-candidate-number-limit nil
      anything-su-or-sudo "sudo")

(setq anything-c-locate-db-file (cofi/var-file "locate.db"))
(setq anything-c-locate-command (format "locate -d %s -i -r %%s"
                                        anything-c-locate-db-file))
(add-hook 'emacs-startup-hook #'alcs-make-candidates 'append)
;; --------------------------------------------------
(require-and-exec 'anything
    (require 'anything-config)
    (require 'anything-match-plugin)
    (require 'anything-show-completion)

    (require-and-exec 'anything-complete
       (anything-read-string-mode '(buffer variable command)))

    ;; From browse-kill-ring.el
    (defadvice yank-pop (around kill-ring-browse-maybe (arg))
      "If last action was not a yank, run `browse-kill-ring' instead."
      ;; yank-pop has an (interactive "*p") form which does not allow
      ;; it to run in a read-only buffer.  We want browse-kill-ring to
      ;; be allowed to run in a read only buffer, so we change the
      ;; interactive form here.  In that case, we need to
      ;; barf-if-buffer-read-only if we're going to call yank-pop with
      ;; ad-do-it
      (interactive "p")
      (if (not (eq last-command 'yank))
          (anything-show-kill-ring)
        (barf-if-buffer-read-only)
        ad-do-it))
    (ad-activate 'yank-pop)

    (require-and-exec 'descbinds-anything
                      (descbinds-anything-install))
    (remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)

    ;; Sources ----------------------------------------
    (require-and-exec 'lacarte
      (defvar anything-c-lacarte-current-buffer nil)
      (defvar anything-c-source-current-buffer-lacarte
        '((name . "Lacarte")
          (init . (lambda ()
                    (setq anything-c-lacarte-current-buffer (current-buffer))))
          (candidates .
                      (lambda ()
                        (with-current-buffer anything-c-lacarte-current-buffer
                          (delete '(nil) (lacarte-get-overall-menu-item-alist)))))
          (candidate-number-limit . 9999)
          (action . (("Open" . (lambda (candidate)
                                 (call-interactively candidate))))))))

    (defun cofi/anything-dir-deep (source-name dir &optional dotfiles fmatch dmatch)
      "Returns an anything source for a particular directory."
      `((name . ,(concat source-name))
        (candidates . ,(ls-files-deep dir dotfiles fmatch dmatch))
        (action . (("Open" . find-file)))
        (type . file)))
    (defun cofi/anything-dir-flat (source-name dir &optional dotfiles fmatch dmatch)
      "Returns an anything source for a particular directory."
      `((name . ,(concat source-name))
        (candidates . ,(ls-files-deep-1 dir dotfiles fmatch dmatch))
        (action . (("Open" . find-file)))
        (type . file)))
    (defun cofi/anything-dir (source-name dir &optional dotfiles match)
      "Returns an anything source for a particular directory."
      `((name . ,(concat source-name))
        (candidates . ,(ls-files dir dotfiles match))
        (action . (("Open" . find-file)))
        (type . file)))
    ;; --------------------------------------------------
    ;; anythings ----------------------------------------
    ;; FIXME: buffer-not-found dummy source not working
;;     (defun cofi/anything-buffers ()
;;       "Enhanced preconfigured `anything' for buffer."
;;       (interactive)
;;       (anything-other-buffer '(
;;                                anything-c-source-buffers+
;;                                anything-c-source-buffer-not-found
;;                                )
;;                                "*anything buffers*"))
    (defalias 'cofi/anything-buffers 'anything-buffers+)

     (defun cofi/anything-files ()
      "ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate"
      (interactive)
      (anything-other-buffer '( anything-c-source-ffap-line
                                anything-c-source-ffap-guesser
                                anything-c-source-recentf
                                anything-c-source-bookmarks
                                anything-c-source-file-cache
                                anything-c-source-files-in-current-dir+
                                anything-c-source-files-in-all-dired
                                anything-c-source-locate)
                             "*anything with files*"))

     (defvar cofi/anything-uni-sources '())
     (defvar cofi/anything-config-sources '())
     (defun cofi/update-anything-sources ()
       (interactive)
       (setq cofi/anything-uni-sources
             (let* ((dirs '("DKE" "HCS" "NCS" "MfI3" "CMS"))
                    (subdirs '("aufgaben" "uebungen" "notes"))
                    (file-filters `(("aufgaben" ,(gen-extension-re "pdf"))
                                    ("uebungen" ,(concat "Makefile" "\\|"
                                                           (gen-extension-re "tex"
                                                                             "gp"
                                                                             "v"
                                                                             "java"
                                                                             "py"
                                                                             "hs"
                                                                             "lisp"
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
                     collect (cofi/anything-dir-deep name dir t
                                                     (cadr (assoc (file-name-nondirectory dir)
                                                                  file-filters))))))

       (setq cofi/anything-config-sources
             `(
               ,(cofi/anything-dir-flat "Emacs" "~/config/dotfiles/emacs.d/" t ".el$")
               ,(cofi/anything-dir-deep "Snippets" "~/config/dotfiles/yasnippets/" t)
               ,(cofi/anything-dir-deep "Zsh" "~/config/dotfiles/zsh/" t)
               ((name . "Dot")
                (candidates . ,(append (ls-files "~/config/dotfiles/" t)
                                       (ls-files "~/config/dotfiles/bin" t)
                                       (ls-files "~/config/dotfiles/quick" t)))
                (action . (("Open" . find-file)))
                (type . file))
               ))
       )

     (run-with-timer 0 600 #'cofi/update-anything-sources)

     (defvar anything-makefile-path nil)
     (defvar anything-makefile-targets
       `((name . "Make")
         (init . (lambda () (setq anything-makefile-path (find-makefile default-directory))))
         (candidates . (lambda () (makefile-targets (concat anything-makefile-path "Makefile"))))
         (volatile)
         (action . (("Make target" . (lambda (candidate)
                                       (compile (concat "cd " anything-makefile-path
                                                        " && make " candidate))))))))

     (defun cofi/anything-uni ()
       (interactive)
       (anything-other-buffer cofi/anything-uni-sources "*anything uni*"))

     (defun cofi/anything-config ()
       (interactive)
       (anything-other-buffer cofi/anything-config-sources "*anything config*"))
     (defun cofi/anything-make ()
       (interactive)
       (anything-other-buffer anything-makefile-targets "*anything make*"))

     (when (fboundp 'lacarte-execute-command)
       (defun cofi/anything-lacarte ()
         (interactive)
         (anything-other-buffer 'anything-c-source-current-buffer-lacarte "*anything lacarte*"))
       (global-set-key (kbd "<f10>") 'cofi/anything-lacarte))
    )
(provide 'cofi-anything)
