(require-and-exec 'auto-complete 
    (require 'auto-complete-config)
    (require 'auto-complete-latex)

    (setq ac-fuzzy-enable t)
    (setq-default ac-auto-start 2)
    (define-key ac-complete-mode-map (kbd "C-l") 'ac-expand-common)
    (define-key ac-complete-mode-map (kbd "C-j") 'ac-next)
    (define-key ac-complete-mode-map (kbd "C-k") 'ac-previous)
    (define-key ac-complete-mode-map (kbd "ESC") 'keyboard-quit)

    (add-to-list 'ac-dictionary-directories "~/.emacs.d/completion-dicts")

    (mapc (lambda (mode)
            (funcall 'add-to-list 'ac-modes mode))
          '(
            rst-mode
            latex-mode
            text-mode
            ))
    (defconst cofi-ac-default-sources
      '(
        ac-source-yasnippet
        ac-source-abbrev
        ac-source-dictionary
        ac-source-words-in-same-mode-buffers
        ac-source-words-in-buffer
        ))

    (defun ac-common-setup ()
      (mapc (lambda (source)
              (add-to-list 'ac-sources source))
            cofi-ac-default-sources))

    (defun ac-lisp-mode-setup ()
      (mapc (lambda (source)
              (add-to-list 'ac-sources source))
            '(
              ac-source-symbols
              ac-source-functions
              ac-source-variables
              ac-source-features
              )))

    (add-hook 'emacs-lisp-mode-hook 'ac-lisp-mode-setup)
    (add-hook 'lisp-mode-hook 'ac-lisp-mode-setup)

    (defun ac-python-mode-setup ()
      (mapc (lambda (source)
              (add-to-list 'ac-sources source))
            '(
              ac-source-ropemacs
              )))

    (add-hook 'python-mode-hook 'ac-python-mode-setup)
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    (global-auto-complete-mode t)
    )

(provide 'cofi-autocompletion)
