(setq anything-command-map-prefix-key "<f7>")

(setq anything-c-boring-file-regexp
  (rx (or
       ;; directories
       (and "/"
            (or ".svn" "CVS" "_darcs" ".git" ".hg" "auto" "_region_" ".prv")
            (or "/" eol))
       ;; files
       (and line-start  (or ".#" "."))
       (and (or ".class" ".la" ".o" "~") eol))))

(setq anything-c-boring-buffer-regexp
      (rx (or
           (and line-start  " ")
           ;; anything-buffer
           "*anything"
           ;; echo area
           " *Echo Area" " *Minibuf"
           (and line-start (optional " ") "*" (* anything) "*")
           )))

(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
(require 'anything-show-completion)

(setq anything-idle-delay 0.3
      anything-input-idle-delay 0
      anything-quick-update t
      anything-candidate-number-limit 100
      anything-su-or-sudo "sudo")

(setq anything-c-locate-db-file "~/var/locate.db")
(setq anything-c-locate-command (concat "locate -d " anything-c-locate-db-file
                                        " -i -r %s"))

(require-and-exec 'anything-complete
                  (anything-read-string-mode '(
                                               buffer
                                               variable
                                               command
                                               )))

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
(provide 'cofi-anything)
