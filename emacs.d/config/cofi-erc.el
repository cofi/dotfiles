(require 'erc)

(pour-lists erc-modules
            '(
              autoaway
              keep-place
              log
              services
              smiley
              spelling
              stamp
              truncate
              ))
(erc-update-modules)
(erc-autojoin-mode 1)
(erc-services-mode 1)
(erc-spelling-mode 1)
(setq erc-prompt-for-nickserv-password nil)

(setq erc-autojoin-channels-alist
      '(("freenode\\.net" . ("#tud-ersties" "#pocoo" "#stumpwm" "#evil-mode"))))

(setq erc-autojoin-timing 'ident)

(setq erc-current-nick-highlight-type 'nick)

(setq erc-prompt (lambda () (format "[%s]" (erc-current-nick))))

(setq erc-track-exclude-types '("NICK" "JOIN" "LEAVE" "QUIT" "PART"
                                "301"   ; away notice
                                "305"   ; return from awayness
                                "306"   ; set awayness
                                "324"   ; modes
                                "329"   ; channel creation date
                                "332"   ; topic notice
                                "333"   ; who set the topic
                                "353"   ; Names notice
                                ))

(setq erc-track-showcount t
      erc-track-shorten-start 4
      erc-track-switch-direction 'importance
      erc-track-visibility 'selected-visible)
(erc-track-mode 1)

(setq erc-keywords nil)
(setq erc-join-buffer nil)
(setq erc-header-line-format "%t(%m): %o")

(defun cofi/erc-monthly-log-directory (&rest ignore)
  (let ((directory (concat (cofi/var-file "emacs/erc-logs/") (format-time-string "%Y/%m"))))
    (cofi/create-directories-for-file (concat directory "/"))
    directory))

(defun cofi/erc-log-file-name (buffer target nick server port)
  (require 'erc-networks)
  (concat (or (with-current-buffer buffer
               (erc-network-name))
             server)
          (and target (concat "." target))
          ".log"))

(setq erc-log-channels-directory #'cofi/erc-monthly-log-directory
      erc-generate-log-file-name-function #'cofi/erc-log-file-name
      erc-log-write-after-insert t
      erc-log-write-after-send t
      erc-truncate-buffer-on-save t)
(erc-log-enable)

(setq erc-log-matches-types-alist '((keyword . "ERC Keywords")
                                    (current-nick . "ERC Calls")))

(setq erc-nick-uniquifier "_"
      erc-nick "cofi")

(setq erc-insert-timestamp-function #'erc-insert-timestamp-left
      erc-timestamp-format "[%H:%M:%S]"
      erc-timestamp-only-if-changed-flag nil)

(require 'tls)
(push "gnutls-cli --priority secure256 --x509cafile=/etc/ssl/certs/ca.pem -p %p %h"
      tls-program)

(defun cofi/erc ()
  (interactive)
  (select-frame (make-frame '((name . "ERC"))))
  (erc-tls
   :server "irc.freenode.net"
   :port 7000)
  (erc
   :server "localhost"))

(defun bitlbee-identify ()
  (when (and (string= "localhost" erc-session-server)
           (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
                                   (erc-default-target)
                                   bitlbee-password))))

(add-hook 'erc-join-hook 'bitlbee-identify)

(setq erc-spelling-dictionaries '(("#tud-ersties" . "german")))

(defun cofi/erc-previous-url-button ()
  "Go to the previous URL button in this buffer."
  (interactive)
  (let ((url-pos (do ((point (point) (previous-single-property-change point 'erc-callback)))
                     ((or (null point) (eq (get-text-property point 'erc-callback) 'browse-url)) point))))
    (when url-pos
      (goto-char url-pos))))

(cofi/set-key erc-mode-map (kbd "<backtab>") 'cofi/erc-previous-url-button)

(defun cofi/erc-buffer ()
  (interactive)
  (let* ((choice (mapcar (lambda (buffer)
                           (cons (buffer-name buffer) buffer))
                         (sort (erc-buffer-list)
                               (lambda (a b)
                                 (let ((a (buffer-name a)) (b (buffer-name b)))
                                   (cond
                                    ((and (begins-with a "#") (begins-with b "#")) (string< a b))
                                    ((begins-with a "#") t)
                                    ((begins-with b "#") nil)
                                    (t (string< a b)))))))))
    (switch-to-buffer (cdr (assoc (completing-read "Goto: " choice nil t)
                                  choice)))))

(defun cofi/erc-frame-urgency (&rest ignore)
  (x-urgency-hint (find-if (p (string= (frame-parameter x 'name) "ERC")) (frame-list))
                  t)
  nil)

(add-hook 'erc-text-matched-hook #'cofi/erc-frame-urgency)

(defun cofi/erc-ignore-lines ()
  (let ((s (buffer-substring-no-properties (point-min) (point-max))))
    (when (string-match-p (rx (or "has joined"
                                  "has quit"
                                  "is now known as"
                                  "has left")) s)
      (goto-char (point-min))
      (insert (format-time-string erc-timestamp-format))
      (put-text-property (point-min) (point-max) 'invisible t))))

(add-hook 'erc-insert-modify-hook #'cofi/erc-ignore-lines)

(provide 'cofi-erc)
