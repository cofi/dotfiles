(defun require-and-exec (feature fun)
  "Require the feature and call fun if it was successfull loaded."
  (if (require feature nil 'noerror)
      (when fun
        (funcall fun))
    (message (format "%s not loaded" feature))))

(defun require-pair (pair)
  "Unpack a pair and call `require-and-exec' on it."
  (require-and-exec
   (car pair)
   (cadr pair)))

(defun plasma-send-notification (msg title &optional timeout)
  "Send plasma notification."
  (let* ((timeouts (if timeout (format "%d" timeout)
                     "10"))
         (command (format "kdialog --passivepopup '%s' --title '%s' %s" msg title timeouts)))
    (shell-command-to-string command)))

(defun libnotify-send-notification (msg title &optional timeout urgency)
  "Send libnotify notification."
  (let* ((timeouts (if timeout (format "%d" timeout)
                     "10"))
         (urgencys (if urgency (format "%s" urgency)
                     "normal"))
        (command
             (format "notify-send -u %s -t %s '%s' '%s'" urgencys timeouts title msg)))
    (shell-command-to-string command)))

(defun send-notification (msg title &optional timeout)
  "Sends notification."
  (funcall 'plasma-send-notification msg title timeout))

(provide 'cofi-util)
