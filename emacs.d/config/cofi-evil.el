(require 'cofi-util)
(require 'cofi-func)
(add-to-loadpath "~/.elisp/vendor/evil/"
                 "~/.elisp/vendor/evil-surround/")
(setq undo-tree-save-history t
      undo-tree-history-directory-alist `(("." . ,(cofi/var-file "undo-tree-history"))))
(require 'undo-tree)
(require 'evil-numbers)
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil
      evil-cross-lines t)
(setq evil-default-cursor #'cofi/evil-cursor)
(setq evil-mode-line-format nil)
(setq evil-leader/leader ","
      evil-leader/in-all-states t)
(setq evil-search-module 'evil-search)
(global-evil-leader-mode)
(require 'evil)
(sackspace-global-mode 1)

(setq evil-normal-state-tag   (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag    (propertize "E" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag   (propertize "I" 'face '((:background "red")))
      evil-motion-state-tag   (propertize "M" 'face '((:background "blue")))
      evil-visual-state-tag   (propertize "V" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

;;; evil-surround
(require-and-exec 'evil-surround
  (setq-default evil-surround-pairs-alist '((?\( . ("(" . ")"))
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
                                       (?t . evil-surround-read-tag)))

  (defun cofi/surround-add-pair (trigger begin-or-fun &optional end)
    "Add a surround pair.
If `end' is nil `begin-or-fun' will be treated as a fun."
    (push (cons (if (stringp trigger)
                    (string-to-char trigger)
                  trigger)
                (if end
                    (cons begin-or-fun end)
                  begin-or-fun))
          evil-surround-pairs-alist))

  (global-evil-surround-mode 1)
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
                               (cofi/surround-add-pair "*" "\\textbf{" "}")
                               (cofi/surround-add-pair "P" "\\(" "\\)")))
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

(cl-loop for (mode . state) in '((inferior-emacs-lisp-mode     . emacs)
                                 (pylookup-mode                . emacs)
                                 (comint-mode                  . emacs)
                                 (ebib-entry-mode              . emacs)
                                 (ebib-index-mode              . emacs)
                                 (ebib-log-mode                . emacs)
                                 (elfeed-show-mode             . emacs)
                                 (elfeed-search-mode           . emacs)
                                 (gtags-select-mode            . emacs)
                                 (shell-mode                   . emacs)
                                 (term-mode                    . emacs)
                                 (bc-menu-mode                 . emacs)
                                 (magit-branch-manager-mode    . emacs)
                                 (semantic-symref-results-mode . emacs)
                                 (rdictcc-buffer-mode          . emacs)
                                 (erc-mode                     . normal))
         do (evil-set-initial-state mode state))

(fill-keymap evil-normal-state-map
             "Y"     (kbd "y$")
             "+"     'evil-numbers/inc-at-pt
             "-"     'evil-numbers/dec-at-pt
             "SPC"   'evil-ace-jump-char-mode
             "S-SPC" 'evil-ace-jump-word-mode
             "C-SPC" 'evil-ace-jump-line-mode
             "go"    'goto-char
             "C-t"   'transpose-chars
             "C-:"   'eval-expression
             ":"     'evil-repeat-find-char-reverse
             "gH"    'evil-window-top
             "gL"    'evil-window-bottom
             "gM"    'evil-window-middle
             "H"     'beginning-of-line
             "L"     'end-of-line
             )

(fill-keymap evil-motion-state-map
             "y"     'evil-yank
             "Y"     (kbd "y$")
             "_"     'evil-first-non-blank
             "C-e"   'end-of-line
             "C-S-d" 'evil-scroll-up
             "C-S-f" 'evil-scroll-page-up
             "_"     'evil-first-non-blank
             "C-y"   nil)

(fill-keymap evil-insert-state-map
             "SPC" 'cofi/yas-expand-or-spc
             "j"   'cofi/evil-maybe-exit
             "C-h" 'backward-delete-char
             "C-k" 'kill-line
             "C-y" 'yank
             "C-e" 'end-of-line)

(fill-keymaps (list evil-operator-state-map
                    evil-visual-state-map)
             ;; works like `f'
             "SPC"   'evil-ace-jump-char-mode
             ;; works like `t'
             "C-SPC" 'evil-ace-jump-char-to-mode
             "S-SPC" 'evil-ace-jump-word-mode)

(defun cofi-evil-adjust-ace-binding ()
  (let ((f (cl-case evil-visual-selection
             (line #'evil-ace-jump-line-mode)
             (t #'evil-ace-jump-char-mode))))
  (define-key evil-visual-state-map (kbd "SPC") f)))

(add-hook 'evil-visual-state-entry-hook #'cofi-evil-adjust-ace-binding)

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

(evil-define-key 'normal rdictcc-permanent-translation-mode-map
  "j" 'rdictcc-next-line
  "k" 'rdictcc-previous-line
  "h" 'rdictcc-backward-char
  "l" 'rdictcc-forward-char
  "b" 'rdictcc-backward-word
  "w" 'rdictcc-forward-word)

(evil-define-key 'normal macrostep-keymap
    (kbd "RET") 'macrostep-expand
    "=" 'macrostep-expand
    "e" 'macrostep-expand

    (kbd "DEL") 'macrostep-collapse
    "u" 'macrostep-collapse
    "c" 'macrostep-collapse

    (kbd "TAB") 'macrostep-next-macro
    "n" 'macrostep-next-macro
    (kbd "M-TAB") 'macrostep-prev-macro
    "p" 'macrostep-prev-macro
    "q" 'macrostep-collapse-all
    (kbd "C-c C-c") 'macrostep-collapse-all)

(defvar slime-mode nil)
(defadvice evil-goto-definition (around evil-clever-goto-def activate)
  "Make use of emacs', slime's and etags possibilities for finding definitions."
  (cl-case major-mode
    (lisp-mode (if slime-mode
                   (or (slime-find-definitions (symbol-name (symbol-at-point)))
                       ad-do-it)
                 ad-do-it))
    (emacs-lisp-mode (cond
                      ((functionp (symbol-at-point)) (find-function-at-point))
                      ((/= (variable-at-point) 0) (find-variable-at-point))
                      ((function-called-at-point) (find-function (function-called-at-point)))
                      (t ad-do-it)))
    (otherwise
     (let ((tag (symbol-name (symbol-at-point))))
       (cond
        ((bound-and-true-p 'ggtags-mode) (ggtags-find-tag tag))
        ((and tags-file-name (find-tag-noselect tag)) (find-tag tag))
        (t ad-do-it))))))

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
  "k" 'kill-current-buffer
  "K" 'kill-buffer-and-window
  "<" 'cofi-cd-alias
  "d" 'dired-jump
  "D" 'cofi-dired-alias


  "g g" #'ack-and-a-half
  "g G" (cmd ack (let ((ack-and-a-half-prompt-for-directory t))
                   (call-interactively #'ack-and-a-half)))
  "g s" #'ack-and-a-half-same
  "g S" (cmd ack-same (let ((ack-and-a-half-prompt-for-directory t))
                        (call-interactively #'ack-and-a-half-same)))

  "h n" #'highlight-symbol-next-in-defun
  "h p" #'highlight-symbol-prev-in-defun
  "h q" #'highlight-symbol-query-replace

  "SPC" #'delete-trailing-whitespace
  "S-SPC" #'cofi/flush-empty-lines

  "m" 'compile

  "n" #'evil-ex-nohighlight

  "c s" #'cofi/switch-file
  "c ;" #'cofi/end-prog-line

  "C" 'delete-window

  "s" 'cofi/split-shell
  "S" 'eshell

  "v" 'cofi/open-vcs

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
    "C-h" nil
    "d" 'cofi/window-toggle-dedicate
    ;; Splitting
    "s" 'cofi/smart-split
    "\\" 'split-window-vertically
    "|" 'split-window-horizontally
    "/" 'cofi/multi-split

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

    "g" 'cofi/goto-window

    ;; winner-mode
    "u" 'winner-undo
    "C-r" 'winner-redo
    ;; shadow rotating in evil-window-map
    "C-R" 'winner-redo)

(fill-keymap evil-normal-state-map
  "[ e"   #'previous-error
  "] e"   #'next-error
  "[ SPC" #'cofi/create-blank-line-previous
  "] SPC" #'cofi/create-blank-line-next
  "[ s"   #'cofi/switch-with-previous-line
  "] s"   #'cofi/switch-with-next-line
  "[ y"   #'cofi/copy-previous-line
  "] y"   #'cofi/copy-next-line
  "[ x"   #'backward-sexp
  "] x"   #'forward-sexp
  )

(cofi/set-key evil-ex-search-keymap "C-r" 'evil-paste-from-register)
(cofi/set-key evil-visual-state-map "/" "y/\C-r\"\C-m")

(evil-define-key 'insert message-mode-map
  (kbd "RET") #'cofi/mail-return-keep-citation-markers)

(evil-add-hjkl-bindings *bc-menu-mode-map* 'emacs)

(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)

(defvar cofi/current-line 0
  "Stores the current line before linum numbers the lines.")

(defadvice linum-update (before set-current-line activate)
  (setq cofi/current-line (line-number-at-pos)))

(defun cofi/relative-line (line-number)
  (let ((relative (abs (- line-number cofi/current-line))))
    (propertize (format "%2d" relative) 'face (if (= relative 0)
                                                  'linum-current-line
                                                'linum))))

(defun cofi/evil-toggle-relative-lines ()
  (if (eq linum-format #'cofi/relative-line)
      (setq linum-format #'cofi/linum-dynamic-lines)
    (setq linum-format #'cofi/relative-line))
  (linum-update-current))

(defun cofi/linum-dynamic-lines (line-number)
  (let ((width (ceiling (log (count-lines (point-min) (point-max)) 10))))
    (propertize (format (format "%%%dd" width) line-number)
                'face (if (= cofi/current-line line-number)
                          'linum-current-line
                        'linum))))

(setq linum-format #'cofi/linum-dynamic-lines)
(add-to-hooks #'cofi/evil-toggle-relative-lines '(evil-operator-state-entry-hook
                                                  evil-operator-state-exit-hook))

;;; TODO: Figure out how a fun can be run if emacs is waiting for more input

(defun cofi/copy-previous-line (count)
  "Copy the line `count' before to the current."
  (interactive "p")
  (save-excursion
    (forward-line (- count))
    (evil-yank-line (point-at-bol) (point-at-eol) 'line)))

(defun cofi/copy-next-line (count)
  "Copy the line `count' after to the current."
  (interactive "p")
  (save-excursion
    (forward-line count)
    (evil-yank-line (point-at-bol) (point-at-eol) 'line)))

(provide 'cofi-evil)
