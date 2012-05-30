(require 'cofi-util)
(require 'cofi-func)
(add-to-loadpath "~/.elisp/vendor/evil/"
                 "~/.elisp/vendor/evil-surround/"
                 "~/.elisp/vendor/evil-leader/")
(require 'undo-tree)
(require 'evil-numbers)
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil
      evil-cross-lines t)
(setq evil-default-cursor #'cofi/evil-cursor)
(setq evil-mode-line-format nil)
(setq evil-leader/leader ","
      evil-leader/in-all-states t)
(require 'evil-leader)
(require 'evil)

(setq evil-normal-state-tag   (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag    (propertize "E" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag   (propertize "I" 'face '((:background "red")))
      evil-motion-state-tag   (propertize "M" 'face '((:background "blue")))
      evil-visual-state-tag   (propertize "V" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

;;; evil-surround
(require-and-exec 'surround
  (setq-default surround-pairs-alist '((?\( . ("(" . ")"))
                                       (?\[ . ("[" . "]"))
                                       (?\{ . ("{" . "}"))

                                       (?\) . ("( " . " )"))
                                       (?\] . ("[ " . " ]"))
                                       (?\} . ("{ " . " }"))
                                       (?>  . ("< " . " >"))

                                       (?# . ("#{" . "}"))
                                       (?p . ("(" . ")"))
                                       (?b . ("[" . "]"))
                                       (?B . ("{" . "}"))
                                       (?< . ("<" . ">"))
                                       (?t . surround-read-tag)))

  (defun cofi/surround-add-pair (trigger begin-or-fun &optional end)
    "Add a surround pair.
If `end' is nil `begin-or-fun' will be treated as a fun."
    (push (cons (if (stringp trigger)
                    (string-to-char trigger)
                  trigger)
                (if end
                    (cons begin-or-fun end)
                  begin-or-fun))
          surround-pairs-alist))

  (global-surround-mode 1)
  (add-to-hooks (lambda ()
                  (cofi/surround-add-pair "`" "`"  "'"))
                '(emacs-lisp-mode-hook lisp-mode-hook))
  (add-to-hooks (lambda ()
                  (cofi/surround-add-pair "~" "``"  "``"))
                '(markdown-mode-hook rst-mode-hook python-mode-hook))
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (cofi/surround-add-pair "~" "\\texttt{" "}")
                               (cofi/surround-add-pair "=" "\\verb=" "=")
                               (cofi/surround-add-pair "/" "\\emph{" "}")
                               (cofi/surround-add-pair "*" "\\textbf{" "}")))
  (add-to-hooks (lambda ()
                  (cofi/surround-add-pair "c" ":class:`" "`")
                  (cofi/surround-add-pair "f" ":func:`" "`")
                  (cofi/surround-add-pair "m" ":meth:`" "`")
                  (cofi/surround-add-pair "a" ":attr:`" "`")
                  (cofi/surround-add-pair "e" ":exc:`" "`"))
                '(rst-mode-hook python-mode-hook)))

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

(loop for (mode . state) in '((inferior-emacs-lisp-mode      . emacs)
                              (pylookup-mode                 . emacs)
                              (comint-mode                   . emacs)
                              (ebib-entry-mode               . emacs)
                              (ebib-index-mode               . emacs)
                              (ebib-log-mode                 . emacs)
                              (gtags-select-mode             . emacs)
                              (shell-mode                    . emacs)
                              (term-mode                     . emacs)
                              (bc-menu-mode                  . emacs)
                              (magit-branch-manager-mode-map . emacs)
                              (semantic-symref-results-mode  . emacs)
                              (rdictcc-buffer-mode           . emacs))
      do (evil-set-initial-state mode state))

(fill-keymap evil-normal-state-map
             "Y"     (kbd "y$")
             "+"     'evil-numbers/inc-at-pt
             "-"     'evil-numbers/dec-at-pt
             "SPC"   'ace-jump-char-mode
             "S-SPC" 'ace-jump-word-mode
             "C-SPC" 'ace-jump-line-mode
             "go"    'goto-char
             "C-t"   'transpose-chars
             "C-:"   'eval-expression
             ":"     'evil-repeat-find-char-reverse)

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

(fill-keymaps (list evil-operator-state-map
                    evil-visual-state-map)
             ;; works like `t'
             "SPC"   'ace-jump-char-mode
             ;; works like `f'
             "C-SPC" 'cofi/ace-jump-char-direct-mode
             "S-SPC" 'ace-jump-word-mode)

(require-and-exec 'sackspace
  (sack/install-in-evil))

(evil-define-key 'normal org-mode-map
  (kbd "RET") 'org-open-at-point
  "za"        'org-cycle
  "zA"        'org-shifttab
  "zm"        'hide-body
  "zr"        'show-all
  "zo"        'show-subtree
  "zO"        'show-all
  "zc"        'hide-subtree
  "zC"        'hide-all
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'normal orgstruct-mode-map
  (kbd "RET") 'org-open-at-point
  "za"        'org-cycle
  "zA"        'org-shifttab
  "zm"        'hide-body
  "zr"        'show-all
  "zo"        'show-subtree
  "zO"        'show-all
  "zc"        'hide-subtree
  "zC"        'hide-all
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'insert org-mode-map
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'insert orgstruct-mode-map
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'normal rdictcc-permanent-translation-mode
  "j" 'rdictcc-next-line
  "k" 'rdictcc-previous-line
  "h" 'rdictcc-backward-char
  "l" 'rdictcc-forward-char
  "b" 'rdictcc-backward-word
  "w" 'rdictcc-forward-word)

(defadvice evil-goto-definition (around evil-clever-goto-def activate)
  "Make use of emacs', slime's and etags possibilities for finding definitions."
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
    (otherwise
     (let ((tag (symbol-name (symbol-at-point))))
       (if (and (boundp 'gtags-mode) gtags-mode)
           (gtags-goto-tag tag nil)
         (if (and tags-file-name (find-tag-noselect tag))
             (find-tag tag)
           ad-do-it))))))

(defun cofi/clear-empty-lines ()
  (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
    (when (string-match "^ +$" line)
      (delete-region (point-at-bol) (point-at-eol)))))
(add-hook 'evil-insert-state-exit-hook #'cofi/clear-empty-lines)

(evil-leader/set-key
  "e" 'cofi/file
  "E" 'cofi/file-alternate
  "o" 'cofi-find-at-alias
  "O" 'cofi-find-helm-at-alias
  "b" 'cofi/buffer
  "B" 'cofi/buffer-alternate
  "w" 'save-buffer
  "W" 'save-some-buffers
  "k" 'kill-buffer-and-window
  "K" (cmd (kill-buffer (current-buffer)))
  "<" 'cofi-cd-alias
  "d" 'dired-jump
  "D" 'cofi-dired-alias

  "m" 'compile

  "n" 'split-window-horizontally
  "c" 'delete-window
  "N" 'make-frame-command
  "C" 'delete-frame

  "g" 'magit-status
  "h" 'monky-status

  "s" 'cofi/switch-file
  ";" 'cofi/end-prog-line

  "." 'evil-ex)

(defun cofi/evil-cursor ()
  "Change cursor color according to evil-state."
  (let ((default "OliveDrab4")
        (cursor-colors '((insert . "dark orange")
                         (emacs  . "sienna")
                         (visual . "white"))))
    (setq cursor-type (if (eq evil-state 'visual)
                          'hollow
                        'bar))
    (set-cursor-color (def-assoc evil-state cursor-colors default))))

(require 'cofi-windowing)
;; allow C-w to be shadowed in emacs-state -- `evil-want-C-w-in-emacs-state' doesn't allow this
(global-set-key (kbd "C-w") evil-window-map)
;; alternative if shadowed
(global-set-key (kbd "C-c w") evil-window-map)
;; Windowing
(fill-keymap evil-window-map
    "d" 'cofi/window-toggle-dedicate
    ;; Splitting
    "\\" 'split-window-vertically
    "|" 'split-window-horizontally
    "/" 'smart-split

    ;; Deleting
    "D"   'delete-window
    "C-d" 'delete-window
    "1"   'delete-other-windows

    ;; Sizing
    "RET" 'enlarge-window
    "-"   'shrink-window-horizontally
    "+"   'enlarge-window-horizontally

    ;; Moving
    "<left>"  'evil-window-left
    "<down>"  'evil-window-down
    "<up>"    'evil-window-up
    "<right>" 'evil-window-right

    ;; Swapping
    "M-h"       'swap-with-left
    "M-j"       'swap-with-down
    "M-k"       'swap-with-up
    "M-l"       'swap-with-right
    "S-<left>"  'swap-with-left
    "S-<down>"  'swap-with-down
    "S-<up>"    'swap-with-up
    "S-<right>" 'swap-with-right
    "SPC"       'swap-window

    ;; winner-mode
    "u" 'winner-undo
    "C-r" 'winner-reod
    ;; shadow rotating in evil-window-map
    "C-R" 'winner-redo)

(evil-define-key 'insert message-mode-map
  (kbd "RET") #'cofi/mail-return-keep-citation-markers)

;; make ace jump look like a single command to evil
(defadvice ace-jump-word-mode (after evil activate)
  (recursive-edit))

(defadvice ace-jump-char-mode (after evil activate)
  (recursive-edit))

(defadvice ace-jump-line-mode (after evil activate)
  (recursive-edit))

(defadvice ace-jump-done (after evil activate)
  (exit-recursive-edit))

(defun cofi/ace-jump-char-direct-mode ()
  "Do a ace char-jump directly to the char."
  (interactive)
  (ace-jump-char-mode)
  (forward-char 1))

(evil-add-hjkl-bindings *bc-menu-mode-map* 'emacs)

(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)

(provide 'cofi-evil)
