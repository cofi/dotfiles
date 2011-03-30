(defconst startup-time
  (current-time)
  "Time Emacs started.")

(defun cofi/fortune2scratch ()
  "Return a comment-padded fortune cookie."
  (let ((cookie (shell-command-to-string "fortune -a")))
    (concat
     (replace-regexp-in-string "^" ";; " cookie)
     "\n")))

(setq initial-scratch-message (cofi/fortune2scratch))

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("technomancy" . "http://repo.technomancy.us/emacs/"))
  (package-initialize))

;; (load "~/.elisp/nxhtml/autostart.el")

(mapc (lambda (dir)
             (add-to-list 'load-path dir))
      '(
        "~/.elisp"
        "~/.emacs.d"
        "~/.elisp/vendor/full-ack"
        "~/.elisp/vendor/magit"
        "~/.elisp/vendor/sackspace"
        "~/.elisp/vendor/keychain-environment"
        "~/.elisp/vendor/gist"
        "~/.elisp/vendor/rainbow"
        "~/.elisp/vendor/pylookup"
        "~/.elisp/vendor/scratch"
        "~/.elisp/vendor/offlineimap"
        "~/.elisp/vendor/popwin-el"
        "/usr/local/share/emacs/site-lisp/semi/"
        "/usr/local/share/emacs/site-lisp/flim/"
        "/usr/local/share/emacs/site-lisp/apel/"
        "/usr/local/share/emacs/23.2/site-lisp/emu/"
        "/usr/local/share/emacs/site-lisp/wl/"
        ))

(defvar hostname (car (split-string system-name "\\." t)))

(require 'cofi-util)
(require 'cofi-vim)
(load "private" 'noerror)

(mapc #'load '(
            "cofi-autoloads"
            "cofi-anything"
            "cofi-buffer"
            "cofi-calendar"
            "cofi-desktop"
            "cofi-mail"
            "cofi-project"
            "cofi-snippets"
            "cofi-ui"
            "cofi-org"
            "cofi-windowing"
            "cofi-completion"
            ))

(defun cofi/prog-loads ()
  (load "cofi-programming"))
(add-hook 'emacs-startup-hook 'cofi/prog-loads)

;; from http://edward.oconnor.cx/2010/08/standalone-gnus
(add-to-list 'command-switch-alist
             '("wl" . (lambda (&rest ignore)
                        ;; no line numbers with mail
                        (global-linum-mode -1)
                        ;; Start wanderlust when Emacs starts
                        (add-hook 'emacs-startup-hook 'wl t)
                        (remove-hook 'emacs-startup-hook 'cofi/prog-loads)
                        ;; Exit Emacs after quitting WL
                        (add-hook 'wl-exit-hook 'save-buffers-kill-emacs))))

(load-theme 'cofi)
(cofi-file-standard)

(message "Time needed to load: %d seconds."
         (time-to-seconds (time-since startup-time)))
