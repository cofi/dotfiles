(require 'erc)
(setq erc-modules '(
                    autoaway
                    autojoin
                    button
                    completion
                    irccontrols
                    keep-place
                    list
                    log
                    match
                    move-to-prompt
                    netsplit
                    networks
                    noncommands
                    pcomplete
                    readonly
                    ring
                    services
                    smiley
                    spelling
                    stamp
                    track
                    truncate))

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

(setq erc-server-reconnect-attempts 4
      erc-server-reconnect-timeout 5)

(setq erc-keywords nil)
(setq erc-join-buffer 'window-noselect)
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
  (unless (find "ERC" (make-frame-names-alist) :key #'car :test #'string=)
    (select-frame (make-frame '((name . "ERC")
                                (alpha . (85 70)))))
    (erc-tls
     :server "irc.freenode.net"
     :port 7000)
    (erc
     :server "localhost")))

(setq erc-spelling-dictionaries '(("#tud-ersties" "german")))

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

(add-hook 'erc-mode-hook (gen-local-fill-keymap-hook
                          "C-n" 'erc-next-command
                          "C-p" 'erc-previous-command))

(defun cofi/erc-mode-line ()
  (setq mode-line-format
        `(
          (evil-mode ("" evil-mode-line-tag))
          " "
          (:propertize "%b " face mode-line-buffer
                       help-echo (buffer-file-name))
          (:eval (let ((ops 0)
                       (members 0))
                   (maphash (lambda (k v)
                              (when (erc-channel-user-op-p k)
                                (incf ops))
                              (incf members))
                            erc-channel-users)
                   (format "%s(@%s) " members ops)))

          (current-input-method
           ("" current-input-method-title))

          appt-mode-string
          " "
          (:eval (let* ((users (mapcar (lambda (user-data) (erc-server-user-nickname (car user-data)))
                                       (erc-sort-channel-users-by-activity (erc-get-channel-user-list))))
                        (user-string (mapconcat #'identity users ", ")))
                   (propertize user-string 'help-echo user-string))))))

(add-hook 'erc-mode-hook #'cofi/erc-mode-line)

(defun erc-cmd-TOP ()
  "Toggle OP by asking chanserv."
  (if (erc-channel-user-op-p (erc-current-nick))
      (erc-cmd-DEOP (erc-current-nick))
    (erc-message "PRIVMSG" (format "chanserv op %s %s" (erc-default-target) (erc-current-nick)))))

(defun cofi/erc-color-nick (&optional user channel-data)
  (cl-flet* ((luminance (r g b) (floor (+ (* 0.299 r) (* 0.587 g) (* 0.117 b))))
             (to-hex (r g b) (format "#%02x%02x%02x" r g b))
             (invert (r g b) (list (- 255 r) (- 255 g) (- 255 b)))
             (nick-to-rgb (nick)
                          (let ((hash (sha1 nick)))
                            (list (mod (string-to-number (substring hash 0 13) 16) 256)
                                  (mod (string-to-number (substring hash 13 26) 16) 256)
                                  (mod (string-to-number (substring hash 26 40) 16) 256))))
             (generate-color (nick)
                             (let ((rgb (nick-to-rgb nick)))
                               (apply #'to-hex
                                (if (< (apply #'luminance rgb) 85)
                                    (apply #'invert rgb)
                                  rgb)))))
    (when user
      (let ((nick (erc-server-user-nickname user))
            (op (and channel-data (erc-channel-user-op channel-data) "@")))
        (propertize (concat op nick) 'face (list :foreground (generate-color nick)))))))

(setq erc-format-nick-function #'cofi/erc-color-nick)

;;; redefine erc function to stop overriding face
(defun erc-format-privmessage (nick msg privp msgp)
  (let* ((msg-face (if privp 'erc-direct-msg-face 'erc-default-face))
         (mark-s (propertize (if msgp (if privp "*" "<") "-") 'face msg-face))
         (mark-e (propertize (if msgp (if privp "*" ">") "-") 'face msg-face)))
    (format "%s%s%s %s" mark-s nick mark-e msg)))

(defun cofi/erc-signal-new-day ()
  (erc-display-line (format-time-string "A new day has come, it is now %A, %y-%m-%d") 'all))

(run-at-time "00:00" (* 24 60 60) #'cofi/erc-signal-new-day)

(provide 'cofi-erc)
