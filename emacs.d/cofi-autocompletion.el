(require 'auto-complete)
(require 'auto-complete-config)

(define-key ac-complete-mode-map (kbd "C-l") 'ac-expand-common)
(define-key ac-complete-mode-map (kbd "C-j") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-k") 'ac-previous)
(define-key ac-complete-mode-map (kbd "ESC") 'keyboard-quit)
(setq-default ac-auto-start 2)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/completion-dicts")

(defun ac-common-setup ()
  (append ac-sources '(
                        ac-source-words-in-buffer
                        ac-source-yasnippet
                        )))

(defun ac-emacs-lisp-mode-setup ()
  (append '(
            ac-source-symbols
            ac-source-functions
            ac-source-variables
            ac-source-features
            )
          'ac-sources))

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)

(defun ac-python-mode-setup ()
  (append '(
            ac-source-ropemacs
            )
          'ac-sources))
(ac-config-default)

(provide 'cofi-autocompletion)
