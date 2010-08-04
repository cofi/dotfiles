(setq initial-scratch-message
      ";; Have a lot of fun!\n")

(defconst startup-time
  (current-time)
  "Time Emacs started.")

(mapc (lambda (dir)
             (add-to-list 'load-path dir))
      '(
        "~/.elisp"
        "~/.emacs.d"
        "~/.elisp/vendor/magit"
        "~/.elisp/vendor/sackspace"
        "~/.elisp/vendor/keychain-environment"
        "~/.elisp/vendor/gist"
        "/usr/local/share/emacs/site-lisp/semi/"
        "/usr/local/share/emacs/site-lisp/flim/"
        "/usr/local/share/emacs/site-lisp/apel/"
        "/usr/local/share/emacs/23.2/site-lisp/emu/"
        "/usr/local/share/emacs/site-lisp/wl/"
        ))

(defvar hostname (substring (shell-command-to-string "hostname") 0 -1))

(require 'cofi-util)
(require 'cofi-vim)
(load "private" 'noerror)

(mapc #'load '(
            "cofi-autoloads"
            "cofi-artist"
            "cofi-buffer"
            "cofi-bbdb"
            "cofi-calendar"
            "cofi-color"
            "cofi-flymake"
            "cofi-haskell"
            "cofi-lisp"
            "cofi-mail"
            "cofi-markers"
            "cofi-project"
            "cofi-python"
            "cofi-snippets"
            "cofi-tex"
            "cofi-ui"
            "cofi-org"
            "cofi-windowing"
            "cofi-completion"
            ))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "xdg-open")
                                      (output-pdf "xdg-open")
                                      (output-dvi "xdg-open")
                                      (output-html "xdg-open"))))
 '(coding-category-utf-8 nil t)
 '(column-number-mode t)
 '(comint-prompt-read-only t)
 '(comment-style (quote align))
 '(framepop-enable-keybinding "<f11>")
 '(inhibit-startup-screen t))

(color-theme-cofi)

(message "Time needed to load: %d seconds."
         (time-to-seconds (time-since startup-time)))
