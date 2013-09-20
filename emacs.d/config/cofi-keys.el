(require 'cofi-util)
(require 'cofi-autoloads)
(require 'cofi-windowing)
(require 'cofi-org)
(require 'cofi-helm)

;;; mode keymap
(defkeymap cofi-minor-mode-map
  "c"   'cofi/cdlatex
  "d"   'rdictcc-tooltip-mode
  "D"   'rdictcc-permanent-translation-mode
  "f"   'auto-fill-mode
  "g"   'glasses-mode
  "h"   'hs-minor-mode
  "i"   'toggle-input-method
  "l"   'outline-minor-mode
  "m"   'flymake-mode
  "o"   'orgstruct++-mode
  "p"   'smartparens-mode
  "P"   'pretty-mode
  "r"   'auto-revert-mode
  "s"   'flyspell-mode
  "S"   'flyspell-prog-mode
  "t"   'orgtbl-mode
  "T"   'gtags-mode
  "v"   'visual-line-mode
  "w"   'whitespace-mode)

;;; insert keymap
(defkeymap cofi-insert-map
  "b" 'insert-buffer
  "f" 'insert-file
  "s" 'yas-insert-snippet)

;;; helm keymap
(defkeymap cofi-helm-map
    "C-b" 'helm-browse-code
    "b"   'helm-bookmarks
    "c"   'cofi/helm-config
    "f"   'cofi/helm-files
    "m"   'cofi/helm-make
    "u"   'cofi/helm-uni
    "M"   'cofi/helm-lacarte
    "l"   'helm-locate)

;;; keymap for often used commands
(defkeymap cofi-quick-map
  "a" 'align-regexp
  "A" 'align
  "M-a" #'align-current
  "c" 'calc
  "d" 'adict-guess-dictionary
  "e" 'eval-and-replace
  "r" 're-builder
  "s" 'sort-lines
  "u" 'cofi-update-all-buffers
  "w" 'count-words-region)

;;; breadcrumbs keymap
(defkeymap cofi-breadcrumbs-map
  "s" 'bc-set
  "P" 'bc-previous
  "N" 'bc-next
  "p" 'bc-local-previous
  "n" 'bc-local-next
  "c" 'bc-goto-current
  "l" 'bc-list
  "C" 'bc-clear)

(defkeymap cofi-project-map
  "c" 'project-compile
  "l" 'project-load
  "a" 'project-ack
  "g" 'project-grep
  "o" 'project-multi-occur
  "u" 'project-unload
  "f" 'project-find-file-ido
  "i" 'project-index
  "s" 'project-status
  "h" 'project-home
  "d" 'project-dired
  "t" 'project-tags)

(defkeymap cofi-org-mode-map
    "a" 'org-agenda-list
    "t" (cmd todo (org-todo-list 0))
    "o a" (cmd agenda-other (let ((org-indirect-buffer-display 'other-window))
               (org-agenda-list)))
    "o t" (cmd todo-other
             (let ((org-indirect-buffer-display 'other-window))
               (org-todo-list 0)))
    "r" 'org-capture
    "l" 'org-store-link
    "v" 'cofi/visit-org-agenda-files
    "V" 'cofi/helm-org-files
    "c" 'cfw:open-org-calendar
    "f" 'org-footnote-action
    "SPC" (cmd set-todo (let ((current-prefix-arg '(4))) (call-interactively 'org-todo))))

(defkeymap cofi-diff-map
  "b" 'ediff-buffers
  "d" 'diff
  "e" 'ediff-files
  "f" 'ediff-files)

(fill-keymap 'global
 ;; buffer
 "C-c y"     'bury-buffer
 "C-c Y"     'quit-window
 "C-c r"     'revert-buffer
 "C-x b"     'cofi/buffer
 "C-x B"     'cofi/buffer-alternate
 "C-x C-c"   'cofi/buffer
 "C-x C-b"   'ibuffer-other-window
 "C-x C-S-b" 'ibuffer

 ;; files
 "C-x M-f" 'ido-find-file-other-window
 "C-x M-b" 'ido-switch-buffer-other-window
 "C-x C-d" 'ido-display-buffer
 "C-x M-d" 'dired-other-window
 "C-x f"   'cofi/file
 "C-x C-f" 'cofi/file-alternate

 ;; search
 "C-r"     'isearch-backward-regexp
 "C-s"     'isearch-forward-regexp
 "M-r"     'query-replace-regexp
 "M-R"     'query-replace-regexp
 ;; movement
 "M-n"     'next-error
 "M-p"     'previous-error
 ;; editing
 "C-M-h"   'backward-kill-word
 "C-;"     'toggle-comment-on-line-or-region
 ;; macros
 "<f4>"    'cofi/macro-dwim
 "S-<f4>"  'cofi/reset-macro
 ;; point stack
 "<f2>"   'point-stack-push
 "S-<f2>" 'point-stack-pop
 "C-<f2>" 'point-stack-forward-stack-pop

 ;; multi term
 "<f11>"   'multi-term-dedicated-open
 "C-<f11>" 'multi-term-next
 ;; scratch
 "<f12>" 'scratch
 "C-<f12>" (cmd to-scratch (switch-to-buffer "*scratch*"))
 ;; imenu
 "C-c i" (f-alt 'helm-imenu 'idomenu)
 ;; rdictcc
 "C-c t" 'rdictcc-translate-word-at-point

 ;; completion
 "M-/" 'cofi/complete
 "C-M-/" 'cofi/uncommon-complete

 "RET"   'newline-and-indent
 ;; maps
 "C-x m" cofi-minor-mode-map
 "C-x i" cofi-insert-map
 "C-c b" cofi-breadcrumbs-map
 "C-c a" cofi-helm-map
 "C-c q" cofi-quick-map
 "C-c o" cofi-org-mode-map
 "<f5>"  cofi-org-mode-map
 "<f7>"  helm-command-map
 "<f8>"  cofi-project-map
 "C-="   cofi-diff-map
 )

(fill-keymap narrow-map
             "p" #'cofi/narrow-to-paragraph
             "P" #'narrow-to-page)

(fill-keymap help-map
  "C-h" nil
  "h" nil
  "C-c" nil
  "RET" nil
  "C-o" nil
  "C-w" nil
  "g" nil
  "C-s" #'yas-visit-snippet-file
  "C-l" #'find-library
  "C-S-l" #'locate-library
  "C-f" #'find-function-at-point
  "M-f" #'find-function
  "C-v" #'find-variable-at-point
  "M-v" #'find-variable
  "C-e" #'toggle-debug-on-error)

(eval-after-load "flymake"
  '(progn
     (defvar flymake-mode-map (make-sparse-keymap))
     (cl-pushnew (cons 'flymake-mode flymake-mode-map) minor-mode-map-alist)
     (fill-keymap flymake-mode-map
       [remap prev-error] #'flymake-goto-prev-error
       [remap next-error] #'flymake-goto-next-error)))

(add-hook 'diff-mode-hook (lambda ()
                            (local-set-key (kbd "q") 'kill-this-buffer)))

(add-hook 'artist-mode-init-hook
          (gen-local-fill-keymap-hook
            "C-c C-o" 'artist-ido-select-operation
            "C-c C-s" 'artist-ido-select-settings))

(define-key isearch-mode-map (kbd "C-h") 'backward-delete-char)

(add-hook 'ido-setup-hook
          (gen-fill-keymap-hook ido-completion-map
                                "C-h" 'ido-prev-match
                                "C-l" 'ido-next-match))
(eval-after-load "doc-view"
  '(fill-keymap doc-view-mode-map
                "h"     'image-backward-hscroll
                "j"     'doc-view-next-line-or-next-page
                "k"     'doc-view-previous-line-or-previous-page
                "l"     'image-forward-hscroll
                "K"     'doc-view-kill-proc-and-buffer
                "S-SPC" 'doc-view-scroll-down-or-previous-page))

;;; quick exit for some modes
(add-to-hooks (gen-local-fill-keymap-hook
                  "q" 'quit-window
                  "Q" 'kill-buffer-and-window)
              '(diff-mode-hook
                compilation-mode-hook))

;;; remapping keys

(global-set-key [remap move-beginning-of-line] 'cofi-dwim-bol)

(provide 'cofi-keys)
