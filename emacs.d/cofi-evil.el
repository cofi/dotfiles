(require 'cofi-util)
(add-to-loadpath "~/.elisp/vendor/evil/")
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil)
(setq evil-default-cursor #'cofi/evil-cursor)

(defun cofi/evil-cursor ()
  "Change cursor color according to viper-state."
  (let ((default "OliveDrab4")
        (cursor-colors '((insert . "dark orange")
                         (emacs  . "sienna"))))
    (setq cursor-type 'bar)
    (set-cursor-color (def-assoc evil-state cursor-colors default))))

(require 'evil)
(evil-mode 1)

(dolist (mode '(magit-mode
                magit-key-mode
                magit-show-branches-mode
                prolog-inferior-mode
                inferior-python-mode
                monky-mode
                gnus-article-mode))
  (push mode evil-emacs-state-modes))

(define-key evil-normal-state-map vim-mapleader cofi/vim-mapleader-map)
(define-key evil-insert-state-map (read-kbd-macro (concat "C-" vim-mapleader)) cofi/vim-mapleader-map)
(define-key evil-emacs-state-map (read-kbd-macro (concat "C-" vim-mapleader)) cofi/vim-mapleader-map)

(define-key evil-motion-state-map (kbd "C-z") nil)
(define-key evil-emacs-state-map (kbd "C-z") nil)
(define-key evil-motion-state-map (kbd "<pause>") 'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "<pause>") 'evil-exit-emacs-state)

(fill-keymap evil-normal-state-map
             "Y"     (cmd (save-excursion (evil-yank (point) (progn (evil-end-of-line)
                                                                    (point)))))
             "_"     'evil-first-non-blank
             "+"     'cofi/inc-at-pt
             "-"     'cofi/dec-at-pt
             "SPC"   'ace-jump-mode
             "S-SPC" 'ace-jump-word-mode
             "C-SPC" 'ace-jump-line-mode
             "go"    'goto-char
             "C-t"   'transpose-chars
             "C-e"   'end-of-line
             "C-S-d" 'evil-scroll-up
             "C-S-f" 'evil-scroll-page-up
             "C-:"   'eval-expression
             ":"     'anything-execute-extended-command
             "C-y"   nil)

(fill-keymap evil-insert-state-map
             "j"   (cofi/maybe-exit-insert 'evil-normal-state ?j ?k)
             "C-h" 'backward-delete-char
             "C-y" 'yank
             "C-e" 'end-of-line)

(require-and-exec 'sackspace
  (sack/install-in-evil))

(defun cofi/evil-define-keys (state map &rest pairs)
  "Define groups of key cmd `PAIRS' for `MODE' in `STATE'."
  (dolist (mapping (group pairs 2))
    (evil-define-key state map (car mapping) (cadr mapping))))

(add-hook 'org-mode-hook
          (lambda ()
            (cofi/evil-define-keys 'normal org-mode-map
                                   "RET" 'org-open-at-point
                                   "za"  'org-cycle
                                   "zA"  'org-shifttab
                                   "zm"  'hide-body
                                   "zr"  'show-all
                                   "zo"  'show-subtree
                                   "zO"  'show-all
                                   "zc"  'hide-subtree
                                   "zC"  'hide-all)))

;;             (cofi/evil-define-keys 'insert org-mode-map
;;                                    "M-l" 'org-metaright
;;                                    "M-h" 'org-metaleft)))

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

(provide 'cofi-evil)
