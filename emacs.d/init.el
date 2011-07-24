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

(setq custom-safe-themes '(;; cofi-dark
                           "c8b6c736e747905c6ab59d4d4d27fded2a6f934c"
                           ;; cofi-light
                           "8d4816cd75e73ead5af307e5e490c33f923699db"
                           default))
(load-theme 'cofi-dark)

(when (or (and (version< emacs-version "24")
               (load (expand-file-name "~/.emacs.d/elpa/package.el")))
          (require 'package))
  (dolist (archive '(("marmalade" . "http://marmalade-repo.org/packages/")
                     ("elpa" . "http://tromey.com/elpa/")))
    (add-to-list 'package-archives archive))
  (package-initialize))

;; (load "~/.elisp/nxhtml/autostart.el")

(defun add-to-loadpath (&rest dirs)
  (dolist (dir dirs load-path)
    (add-to-list 'load-path dir nil #'string=)))

(add-to-loadpath "~/.elisp"
                 "~/.emacs.d"
                 "~/.elisp/vendor/full-ack"
                 "~/.elisp/vendor/sackspace"
                 "~/.elisp/vendor/keychain-environment"
                 "~/.elisp/vendor/gist"
                 "~/.elisp/vendor/rainbow"
                 "~/.elisp/vendor/pylookup"
                 "~/.elisp/vendor/scratch"
                 "~/.elisp/vendor/offlineimap"
                 "~/.elisp/vendor/ace-jump-mode")

(require 'cofi-util)
(defvar hostname (car (split-string system-name "\\." t)))
(defvar on-mobile? (find hostname '("hitchhiker") :test #'string=))

(load "private" 'noerror)
(require 'cofi-vim)

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
                                 "cofi-shell"
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
                        (add-hook 'emacs-startup-hook 'wl 'append)
                        ;; Exit Emacs after quitting WL
                        (add-hook 'wl-exit-hook 'save-buffers-kill-emacs))))

(add-to-list 'command-switch-alist
             '("gnus" . (lambda (&rest ignore)
                        (setq cofi/mail-instance t)
                        (setq cofi/full-emacs nil)
                        (add-hook 'emacs-startup-hook 'gnus 'append)
                        ;; Exit Emacs after quitting gnus
                        (add-hook 'gnus-after-exiting-gnus-hook 'save-buffers-kill-emacs))))

(add-hook 'emacs-startup-hook (lambda ()
                                (when cofi/mail-instance
                                  (global-linum-mode -1))
                                (when cofi/full-emacs
                                  (mapc #'load cofi/full-settings))))
(cofi-next-file-assoc)
(cofi/next-colorscheme)
(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %d seconds."
                                         (time-to-seconds (time-since startup-time))))
          'append)
