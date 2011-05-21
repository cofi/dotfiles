(defconst startup-time
  (current-time)
  "Time Emacs started.")

(defun cofi/fortune2scratch ()
  "Return a comment-padded fortune cookie."
  (let ((cookie (shell-command-to-string "fortune -a")))
    (concat
     (replace-regexp-in-string " *$" ""
        (replace-regexp-in-string "^" ";; " cookie))
     "\n")))
(setq initial-scratch-message (cofi/fortune2scratch))

(setq custom-safe-themes '("4a8ee16dec9b957d68e04f64a79d88f10fb49f3f"
                           "315eb5b3d8e17ccaec85d61830858ffc27fc8b02"
                           default))
(load-theme 'cofi-dark)

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (dolist (archive '(("marmalade" . "http://marmalade-repo.org/packages/")
                     ("elpa" . "http://tromey.com/elpa/")))
    (add-to-list 'package-archives archive))
  (package-initialize))

;; (load "~/.elisp/nxhtml/autostart.el")

(dolist (dir '(
               "~/.elisp"
               "~/.emacs.d"
               "~/.elisp/vendor/full-ack"
               "~/.elisp/vendor/sackspace"
               "~/.elisp/vendor/keychain-environment"
               "~/.elisp/vendor/gist"
               "~/.elisp/vendor/rainbow"
               "~/.elisp/vendor/pylookup"
               "~/.elisp/vendor/scratch"
               "~/.elisp/vendor/offlineimap"
               "/usr/local/share/emacs/site-lisp/semi/"
               "/usr/local/share/emacs/site-lisp/flim/"
               "/usr/local/share/emacs/site-lisp/apel/"
               "/usr/local/share/emacs/23.2/site-lisp/emu/"
               "/usr/local/share/emacs/site-lisp/wl/"
               ))
  (add-to-list 'load-path dir))

(require 'cofi-util)
(defvar hostname (car (split-string system-name "\\." t)))
(defvar on-mobile? (find hostname '("hitchhiker") :test #'string=))

(require 'cofi-vim)
(load "private" 'noerror)

(defvar cofi/standard-settings '("cofi-autoloads"
                                 "cofi-anything"
                                 "cofi-buffer"
                                 "cofi-calendar"
                                 "cofi-desktop"
                                 "cofi-ediff"
                                 "cofi-elisp"
;;                                  "cofi-elscreen"
                                 "cofi-files"
                                 "cofi-func"
                                 "cofi-keys"
                                 "cofi-mail"
                                 "cofi-project"
                                 "cofi-snippets"
                                 "cofi-ui"
                                 "cofi-org"
                                 "cofi-vcs"
                                 "cofi-rst"
                                 "cofi-windowing"
                                 "cofi-workgroups"
                                 "cofi-completion"))
(defvar cofi/full-settings '("cofi-programming"))

(defvar cofi/full-emacs t "Load all settings not just minimal.")
(defvar cofi/mail-instance nil "This is an email instance.")

(mapc #'load cofi/standard-settings)

(add-to-list 'command-switch-alist
             '("wl" . (lambda (&rest ignore)
                        (setq cofi/mail-instance t)
                        (setq cofi/full-emacs nil)
                        ;; Exit Emacs after quitting WL
                        (add-hook 'wl-exit-hook 'save-buffers-kill-emacs))))

(add-hook 'emacs-startup-hook (lambda ()
                                (when cofi/mail-instance
                                  (global-linum-mode -1)
                                  (wl)
                                  (add-hook 'emacs-startup-hook 'wl t))
                                (when cofi/full-emacs
                                  (mapc #'load cofi/full-settings))
                                ))
(cofi-file-standard)
(message "Time needed to load: %d seconds."
         (time-to-seconds (time-since startup-time)))
