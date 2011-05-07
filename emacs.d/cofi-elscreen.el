(add-to-list 'load-path "~/.elisp/elscreen/")

(require 'elscreen)
(require 'elscreen-dired)
(require 'elscreen-server)
;;; in cofi-mail.el: elscreen-wl

(defmacro elscreen-create-automatically (ad-do-it)
  `(if (not (elscreen-one-screen-p))
       ,ad-do-it
     (elscreen-create)
     (elscreen-notify-screen-modification 'force-immediately)
     (elscreen-message "New screen is automatically created")))

(defadvice elscreen-next (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(defadvice elscreen-previous (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(defadvice elscreen-toggle (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

;;; magit support
(defadvice magit-status (before elscreen-create-before-magit activate)
  (elscreen-create))

(add-hook 'magit-mode-hook (lambda ()
                             (local-set-key (kbd "q")
                                            (lambda ()
                                              (interactive)
                                              (quit-window)
                                              (elscreen-kill)))))

(provide 'cofi-elscreen)
