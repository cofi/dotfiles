(require 'cofi-util)
(add-to-loadpath "~/.elisp/vendor/evil/")
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil)
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

(dolist (mode '(magit-mode
                magit-key-mode
                magit-show-branches-mode
                prolog-inferior-mode
                inferior-python-mode
                monky-mode
                gnus-article-mode
                gnus-server-mode
                gnus-browse-mode
                comint-mode
                eshell-mode
                shell-mode
                inferior-emacs-lisp-mode
                term-mode
                org-agenda-mode
                cfw:calendar-mode
                docview-mode
                ibuffer-mode
                twittering-mode
                ))
  (push mode evil-emacs-state-modes))

(dolist (mode '(slime-repl-mode
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

(defun cofi/evil-define-keys (state map &rest pairs)
  "Define groups of key cmd `PAIRS' for `MODE' in `STATE'."
  (dolist (mapping (group pairs 2))
    (evil-define-key state map (read-kbd-macro (car mapping)) (cadr mapping))))

(eval-after-load "org"
          '(progn
             (cofi/evil-define-keys 'normal org-mode-map
                                    "<return>" 'org-open-at-point
                                    "za"       'org-cycle
                                    "zA"       'org-shifttab
                                    "zm"       'hide-body
                                    "zr"       'show-all
                                    "zo"       'show-subtree
                                    "zO"       'show-all
                                    "zc"       'hide-subtree
                                    "zC"       'hide-all
                                    "M-H"      'org-metaleft
                                    "M-J"      'org-metadown
                                    "M-K"      'org-metaup
                                    "M-L"      'org-metaright)

             (cofi/evil-define-keys 'insert org-mode-map
                                    "M-j" 'org-shiftleft
                                    "M-k" 'org-shiftright
                                    "M-H" 'org-metaleft
                                    "M-J" 'org-metadown
                                    "M-K" 'org-metaup
                                    "M-L" 'org-metaright)))

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

(defvar cofi/evil-input-method nil)
(make-variable-buffer-local 'cofi/evil-input-method)
(defun cofi/save-kill-input-method ()
  (setq cofi/evil-input-method current-input-method)
  (inactivate-input-method))
(defun cofi/reactivate-input-method ()
  (activate-input-method cofi/evil-input-method))

(add-hook 'evil-normal-state-entry-hook #'cofi/save-kill-input-method)
(add-hook 'evil-visual-state-entry-hook #'cofi/save-kill-input-method)
(add-hook 'evil-normal-state-exit-hook #'cofi/reactivate-input-method)
(add-hook 'evil-visual-state-exit-hook #'cofi/reactivate-input-method)

(defadvice toggle-input-method (after evil-input-method-toggle activate)
  (when evil-mode
    ;; hope that all play well
    (if cofi/evil-input-method
        (setq cofi/evil-input-method nil)
      (setq cofi/evil-input-method current-input-method))
    (if (memq evil-state '(normal visual))
        (inactivate-input-method))))

(defun cofi/clear-empty-lines ()
  (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
    (when (string-match "^\\( +\\)$" line)
      (delete-region (point-at-bol) (point-at-eol)))))
(add-hook 'evil-insert-state-exit-hook #'cofi/clear-empty-lines)

(provide 'cofi-evil)
