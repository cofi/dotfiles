(add-to-loadpath "~/.elisp/slime/"
                 "~/.elisp/vendor/clojure-mode/")

(dolist (hook '(clojure-mode-hook lisp-mode-hook))
  (add-hook hook (gen-local-fill-keymap-hook
                     "C-c '"   'slime-selector
                     "C-c C-;" 'slime-insert-balanced-comments
                     "C-c M-;" 'slime-remove-balanced-comments
                     "C-c C-d C-a" 'slime-arglist)))

;;; CL
(add-major-mode "\\.cl$" 'lisp-mode)
(add-hook 'lisp-mode-hook (lambda ()
                            (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)))
(add-hook 'lisp-mode-hook (lambda () (unless (slime-connected-p)
                                  (let ((buf (current-buffer)))
                                    (slime)
                                    (other-window 1)
                                    (switch-to-buffer buf))))
          'append)

;;; paredit
(require 'paredit)
(let ((paredit-mode-hooks '(lisp-mode-hook
                            clojure-mode-hook
                            slime-repl-mode-hook)))
  (add-to-hooks #'enable-paredit-mode paredit-mode-hooks))

(defadvice paredit-newline (before slime-eval-print-in-scratch activate)
  "Call `SLIME-EVAL-PRINT-LAST-EXPRESSION' in slime scratch."
  (if (and slime-mode (string= "*slime-scratch*" (buffer-name)))
      (call-interactively #'slime-eval-print-last-expression)))

;;; slime
(setq slime-lisp-implementations
      `((sbcl ("sbcl" "--core" ,(expand-file-name "~/var/sbcl.swank-core"))
              :init (lambda (port-file _)
                      (format "(swank:start-server %S)\n" port-file))
              :coding-system utf-8-unix)))

(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t)))

;;; keep our repl clean
(setq slime-repl-history-trim-whitespaces t
      slime-repl-history-remove-duplicates t)

;;; spare me this fasl
(let ((fasl-dir (expand-file-name "~/tmp/slime-fasls")))
  (make-directory fasl-dir t)
  (setq slime-compile-file-options `(:fasl-directory ,fasl-dir)))

(require-and-exec 'slime-autoloads
  (slime-setup '(slime-fancy
                 slime-banner
                 helm-slime
                 ))
  (require 'slime)
  (setq slime-complete-symbol-function (f-alt 'helm-slime-complete
                                              'slime-fuzzy-complete-symbol
                                              'slime-simple-complete-symbol))
  (setq slime-protocol-version 'ignore))

(when (fboundp 'anything-slime-complete)
  (setq slime-complete-symbol-function #'anything-slime-complete))

                 slime-autodoc
                 slime-editing-commands
                 slime-fancy-inspector
                 slime-fancy
                 slime-fontifying-fu
                 slime-fuzzy
                 slime-indentation
                 slime-package-fu
                 slime-references
                 slime-scratch
                 slime-xref-browser
                 slime-presentations
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;;; from http://bc.tech.coop/blog/070425.html
(defun slime-new-repl (&optional new-port)
  "Create additional REPL for the current Lisp connection."
  (interactive)
  (if (slime-current-connection)
      (let ((port (or new-port (slime-connection-port (slime-connection)))))
        (slime-eval `(swank::create-server :port ,port))
        (slime-connect slime-lisp-host port))
    (error "Not connected")))

(provide 'cofi-lisp)
