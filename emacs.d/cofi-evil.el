(require 'cofi-util)
(add-to-loadpath "~/.elisp/vendor/evil/")
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil
      evil-cross-lines t)
(setq evil-default-cursor #'cofi/evil-cursor)

(defun cofi/evil-cursor ()
  "Change cursor color according to viper-state."
  (let ((default "OliveDrab4")
        (cursor-colors '((insert . "dark orange")
                         (emacs  . "sienna")
                         (visual . "white"))))
    (setq cursor-type (if (eq evil-state 'visual)
                          'hollow
                        'bar))
    (set-cursor-color (def-assoc evil-state cursor-colors default))))

(require 'evil)
(require-and-exec 'surround
  (global-surround-mode 1)
  (pour-lists surround-pairs-alist
              '(
                (?\" . ?\")
                (?'  . ?')
                (?`  . ?')
                )))

(evil-set-toggle-key "<pause>")
(evil-mode 1)

(evil-define-command cofi/evil-maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p))
        (entry-key ?j)
        (exit-key ?k))
    (insert entry-key)
    (let ((evt (read-event (format "Insert %c to exit insert state" exit-key) nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt exit-key))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
       (t (push evt unread-command-events))))))

(dolist (mode '(
                inferior-emacs-lisp-mode
                ))
  (push mode evil-emacs-state-modes))

(dolist (mode '(
                ))
  (push mode evil-insert-state-modes))

(define-key evil-normal-state-map vim-mapleader cofi/vim-mapleader-map)
(define-key evil-insert-state-map (read-kbd-macro (concat "C-" vim-mapleader)) cofi/vim-mapleader-map)
(define-key evil-emacs-state-map (read-kbd-macro (concat "C-" vim-mapleader)) cofi/vim-mapleader-map)

(fill-keymap evil-normal-state-map
             "Y"     (cmd (evil-yank (point) (point-at-eol)))
             "+"     'cofi/inc-at-pt
             "-"     'cofi/dec-at-pt
             "SPC"   'ace-jump-mode
             "S-SPC" 'ace-jump-word-mode
             "C-SPC" 'ace-jump-line-mode
             "go"    'goto-char
             "C-t"   'transpose-chars
             "C-:"   'eval-expression
             ":"     'anything-execute-extended-command)

(fill-keymap evil-motion-state-map
             "_"     'evil-first-non-blank
             "C-e"   'end-of-line
             "C-S-d" 'evil-scroll-up
             "C-S-f" 'evil-scroll-page-up
             "_"     'evil-first-non-blank
             "C-y"   nil)

(fill-keymap evil-insert-state-map
             "j"   'cofi/evil-maybe-exit
             "C-h" 'backward-delete-char
             "C-y" 'yank
             "C-e" 'end-of-line)

(require-and-exec 'sackspace
  (sack/install-in-evil))

(evil-declare-key 'normal org-mode-map
  (kbd "RET") 'org-open-at-point
  "za"        'org-cycle
  "zA"        'org-shifttab
  "zm"        'hide-body
  "zr"        'show-all
  "zo"        'show-subtree
  "zO"        'show-all
  "zc"        'hide-subtree
  "zC"        'hide-all
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-declare-key 'insert org-mode-map
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(defadvice evil-goto-definition (around evil-goto-lisp-def activate)
  "Make use of emacs' and slime's possibilities for finding definitions."
  (case major-mode
    (lisp-mode (if slime-mode
                   (or (slime-find-definitions (symbol-name (symbol-at-point)))
                       ad-do-it)
                 ad-do-it))
    (emacs-lisp-mode (condition-case nil
                         (find-function (symbol-at-point))
                       (error (condition-case nil
                                  (find-variable (symbol-at-point))
                                (error ad-do-it)))))
    (otherwise ad-do-it)))

(defun cofi/clear-empty-lines ()
  (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
    (when (string-match "^\\( +\\)$" line)
      (delete-region (point-at-bol) (point-at-eol)))))
(add-hook 'evil-insert-state-exit-hook #'cofi/clear-empty-lines)

(provide 'cofi-evil)
