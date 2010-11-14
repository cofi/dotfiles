(defmacro require-and-exec (feature &optional &rest body)
  "Require the feature and execute body if it was successfull loaded."
  `(if (require ,feature nil 'noerror)
        (progn ,@body)
    (message (format "%s not loaded" ,feature))))

(defmacro load-and-exec (file &optional &rest body)
  "Load the file and execute body if it was successfull loaded."
  `(if (load ,file t)
        (progn ,@body)
    (message (format "%s not loaded" ,file))))

(defun in-mode? (mode)
  (eq major-mode mode))

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

(defun x-urgency-hint (frame arg &optional source)
  (let* ((wm-hints (append (x-window-property 
			    "WM_HINTS" frame "WM_HINTS" 
			    (if source
				source
			      (string-to-number 
			       (frame-parameter frame 'outer-window-id)))
			    nil t) nil))
	 (flags (car wm-hints)))
    (setcar wm-hints
	    (if arg
		(logior flags #x00000100)
	      (logand flags #xFFFFFEFF)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun add-to-hooks (fun hooks)
  "Add function to hooks"
  (mapc (lambda (hook)
          (add-hook hook fun))
        hooks))

(defun client-save-kill-emacs(&optional display)
  " This is a function that can bu used to shutdown save buffers and 
shutdown the emacs daemon. It should be called using 
emacsclient -e '(client-save-kill-emacs)'.  This function will
check to see if there are any modified buffers or active clients
or frame.  If so an x window will be opened and the user will
be prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

    ; Check if there are modified buffers or active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
					(> (length (frame-list)) 1)
				       ))  

    ; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
	(message "Initializing x windows system.")
	(x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))

    ; Save the current frame.  
    (setq new-frame (selected-frame))


    ; When displaying the number of clients and frames: 
    ; subtract 1 from the clients for this client.
    ; subtract 2 from the frames this frame (that we just created) and the default frame.
    (when ( or (not active-clients-or-frames)
	       (yes-or-no-p (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2)))) 
      
      ; If the user quits during the save dialog then don't exit emacs.
      ; Still close the terminal though.
      (let((inhibit-quit t))
             ; Save buffers
	(with-local-quit
	  (save-some-buffers)) 
	      
	(if quit-flag
	  (setq quit-flag nil)  
          ; Kill all remaining clients
	  (progn
	    (dolist (client server-clients)
	      (server-delete-client client))
		 ; Exit emacs
	    (kill-emacs))) 
	))

    ; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))
    )
  )


(defun modified-buffers-exist() 
  "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
		 (buffer-modified-p buffer)
		 (not (buffer-base-buffer buffer))
		 (or
		  (buffer-file-name buffer)
		  (progn
		    (set-buffer buffer)
		    (and buffer-offer-save (> (buffer-size) 0))))
		 )
	(setq modified-found t)
	)
      )
    modified-found
    )
  )

(defun filter (predicate xs)
  "Returns elements of `xs' that satisfy `predicate'."
  (if xs
      (if (funcall predicate (car xs))
          (cons (car xs)
                (filter predicate (cdr xs)))
        (filter predicate (cdr xs)))
    '()))

(defun directory-files-no-dots (directory &optional full match nosort)
  "Returns files in `directory' without `.' and `..'.
`full', `match' and `nosort' act as in `directory-files'"
  (filter (lambda (f) (not (string-match (rx (or string-start "/")
                                         (or "." "..")
                                         string-end) f)))
          (directory-files directory full match nosort)))

(defun directory-files-subdirs (directory &optional full match nosort)
  "Returns subdirectories in `directory'.
`full', `match' and `nosort' act as in `directory-files'"
  (let ((dirs (filter 'file-directory-p
                      (directory-files-no-dots directory t match nosort))))
  (if full
      dirs
    (mapcar 'file-name-nondirectory dirs))))

(defun directory-files-subdirs-no-dots (directory &optional full match nosort)
  "Returns subdirectories in `directory' without `.' and `..'.
`full', `match' and `nosort' act as in `directory-files'"
  (let* ((dirs (filter 'file-directory-p
                       (directory-files-no-dots directory t match nosort)))
         (nodots (filter (lambda (d) (not (string-match (rx (or string-start "/")
                                         (or "." "..")
                                         string-end) d)))
                         dirs)))
  (if full
      nodots
    (mapcar 'file-name-nondirectory nodots))))

(defun directory-files-no-subdirs (directory &optional full match nosort)
  "Returns subdirectories in `directory'.
`full', `match' and `nosort' act as in `directory-files'"
  (let ((dirs (filter (lambda (f) (not (file-directory-p f)))
                      (directory-files-no-dots directory t match nosort))))
  (if full
      dirs
    (mapcar 'file-name-nondirectory dirs))))

(defun directory-files-deep (directory &optional match nosort)
  "Returns files in `directory' and its subdirectories with full path.
`match' and `nosort' act as in `directory-files'."
  (let ((subdirs (directory-files-subdirs-no-dots directory t match nosort))
        (files (directory-files-no-subdirs directory t match nosort)))
    (append files
            (reduce 'append
                    (mapcar 'directory-files-deep subdirs)))))

(defun directory-files-flat (directory &optional match nosort)
  "Returns files in `directory' and its toplevel subdirs with full path.
`match' and `nosort' act as in `directory-files'."
  (let ((subdirs (directory-files-subdirs-no-dots directory t match nosort))
        (files (directory-files-no-subdirs directory t match nosort)))
    (append files
            (reduce 'append
                    (mapcar 'directory-files-no-subdirs subdirs)))))

(provide 'cofi-util)
