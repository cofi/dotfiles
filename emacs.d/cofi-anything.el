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
           )))

(setq anything-idle-delay 0.3
      anything-input-idle-delay 0
      anything-quick-update t
      anything-candidate-number-limit 100
      anything-su-or-sudo "sudo")

(setq anything-c-locate-db-file "~/var/locate.db")
(setq anything-c-locate-command (format "locate -d %s -i -r %%s"
                                        anything-c-locate-db-file))
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
    (defvar anything-c-source-buffer-not-found
      (anything-c-define-dummy-source
       "Create buffer"
       (lambda () (unless (get-buffer anything-input)
                    (anything-c-dummy-candidate)))
       '(type . buffer)))

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

    (defun make-anything-directory-source (source-name dir &optional deep match)
      "Returns an anything source for a particular directory.
`deep' may be `deep' or `flat'."
      `((name . ,(concat source-name))
        (candidates . ,(cond
                        ((eq deep 'deep) (directory-files-deep dir match))
                        ((eq deep 'flat) (directory-files-flat dir match))
                        (t (directory-files-no-subdirs dir t match))))
        (action . (("Open" . find-file)))
        (type . file)))
    ;; --------------------------------------------------
    ;; anythings ----------------------------------------
    (defun cofi/anything-buffers ()
      "Enhanced preconfigured `anything' for buffer."
      (interactive)
      (anything-other-buffer '(
                               anything-c-source-buffers+
                               anything-c-source-buffer-not-found
                               )
                               "*anything buffers*"))

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

     (defun cofi/anything-uni ()
       (interactive)
       (let* ((dirs '("FoC" "FGdI3" "GdI3" "SE" "TS"))
              (subdirs '("aufgaben" "uebungen"))
              (path "~/Work/Uni/")
              (relevant-files-re (rx "." (or "c" "org" "java" "asm" "tex" "py" "pdf")
                                     string-end))
              (combinations (reduce 'append
                                    (mapcar (lambda (d)
                                              (mapcar (lambda (s)
                                                        (concat d "/" s))
                                                      subdirs))
                                            dirs)))
              (full (mapcar (lambda (d) (concat path d))
                            combinations)))
         (anything-other-buffer (mapcar* (lambda (s d)
                                           (make-anything-directory-source s d 'deep relevant-files-re))
                                         combinations
                                         full)
                                "*anything uni*")))
     
     (defun cofi/anything-config ()
       (interactive)
       (anything-other-buffer
        (list
         (make-anything-directory-source "Dot Emacs"
                                         "~/config/dotfiles/emacs.d/" 'deep)
         (make-anything-directory-source "Dot" "~/config/dotfiles/"))
        "*anything config*"))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "u") 'cofi/anything-uni)
       (define-key map (kbd "c") 'cofi/anything-config)
       (global-set-key (kbd "C-c a") map))

     (when (fboundp 'lacarte-execute-command)
       (defun cofi/anything-lacarte ()
         (interactive)
         (anything-other-buffer 'anything-c-source-current-buffer-lacarte "*anything lacarte*"))
       (global-set-key (kbd "<f10>") 'cofi/anything-lacarte))
    )
(provide 'cofi-anything)
