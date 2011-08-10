(require 'cofi-util)
(add-to-list 'load-path "~/.elisp/vendor/undo-tree")
(require-and-exec 'undo-tree)
(defconst vim-mapleader "\\"
"Mapping prefix; Vanilla in vi-state Prefixed with `C-' in insert-state and emacs-state.")
(defvar cofi/vim-mapleader-map (make-sparse-keymap) "Mapleader keymap")

(fill-keymap cofi/vim-mapleader-map
             "e" 'cofi/file
             "E" 'cofi/file-alternate
             "o" 'cofi-find-at-alias
             "b" 'cofi/buffer
             "B" 'cofi/buffer-alternate
             "w" 'save-buffer
             "W" 'save-some-buffers
             "k" 'kill-buffer-and-window
             "K" 'kill-this-buffer
             "<" 'cofi-cd-alias
             "d" 'dired-jump
             "D" 'cofi-dired-alias

             "m" 'compile

             "n" 'split-window-horizontally
             "c" 'delete-window
             "N" 'make-frame-command
             "C" 'delete-frame

             ;; in vcs
             ;; g -> magit-status
             ;; h -> ahg-status
             ;; H -> ahg keymap

             "s" 'cofi/switch-file
             ";" 'cofi/end-prog-line
             )

(defun vim-mapleader-add (keyseq fun)
  (interactive "kKeysequence: \naFunction:")
  (define-key cofi/vim-mapleader-map (read-kbd-macro keyseq) fun))

(defun cofi/maybe-exit (exit-fun entry-char exit-char)
  "Return a a function that maybe inserts or calls `EXIT-FUN'.
Insert `ENTRY-CHAR', if it is followed by a `EXIT-CHAR' in the next half second,
delete `ENTRY-CHAR' and call `EXIT-FUN'."
  `(lambda ()
     (interactive)
     (let ((modified (buffer-modified-p)))
       (insert ,entry-char)
       (let ((evt (read-event (format "Insert %c to exit insert state" ,exit-char)
                              nil 0.5)))
         (cond
          ((null evt) (message ""))
          ((and (integerp evt) (char-equal evt ,exit-char))
             (delete-char -1)
             (set-buffer-modified-p modified)
             (,exit-fun))
          (t (setq unread-command-events (append unread-command-events
                                                 (list evt)))))))))

(require 'cofi-evil)
;; (require 'cofi-vimpulse)
(provide 'cofi-vim)
