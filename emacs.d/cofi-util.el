(require 'cl)
(require 'queue)

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
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun save-kill-emacs ()
  (interactive)
  (if (modified-buffers?)
      (progn
        (when (not (eq window-system 'x))
          (x-initialize-window-system))
        (select-frame (make-frame-on-display (getenv "DISPLAY") '((window-system . x))))
        (save-some-buffers)
        (if (yes-or-no-p "Kill Emacs? ")
            (kill-emacs)))
    (kill-emacs)))

(defun modified-buffers? ()
  "Returns first modified buffer or nil if there is none."
  (loop for b in (buffer-list)
        when (and (buffer-live-p b)
                (buffer-modified-p b)
                (buffer-file-name b))
        return b))

(defun dot? (fname &optional dotfiles)
  "Determines if `FNAME' is a dot or dotfile if `DOTFILES' is non-nil."
  (let ((f (file-name-nondirectory fname)))
     (if dotfiles
         (string-prefix-p "." f)
       (or (string= "." f) (string= ".." f)))))

(defun ls-no-dots (directory &optional full dotfiles match)
  "Returns files in `directory' without `.' and `..'.
`full', `match' and `nosort' act as in `directory-files'"
    (remove-if (lambda (f) (dot? f dotfiles))
               (directory-files directory full match)))

(defun ls-dirs (directory &optional dotfiles match)
  "Returns all dirs in `DIR'.
`DOTFILES' -- if non-nil don't include dirs starting with a `.'
`MATCH' -- if non-nil only include dirs matching the regexp"
  (remove-if-not #'file-directory-p
                 (ls-no-dots directory t dotfiles match)))

(defun ls-files (directory &optional dotfiles match)
  "Returns all files in `DIR'.
`DOTFILES' -- if non-nil don't include files starting with a `.'
`MATCH' -- if non-nil only include files matching the regexp"
  (remove-if #'file-directory-p
             (ls-no-dots directory t dotfiles match)))

(defun enqueue-all (queue l)
  "Enqueues in `QUEUE' all entries of `L'."
  (mapc (lambda (e) (queue-enqueue queue e))
        l))

(defun ls-files-deep (dir &optional dotfiles fmatch dmatch)
  "Returns all files within `DIR'.
`DOTFILES' -- if non-nil don't include files and dirs starting with a `.'
`FMATCH' -- if non-nil only include files matching the regexp
`DMATCH' -- if non-nil only include files in dirs matching the regexp; if parent
            dir failed its dirs will not be searched."
  (let ((dirs (queue-create)))
    (queue-enqueue dirs dir)
    (loop while (> (queue-length dirs) 0)
          nconc (let ((d (queue-dequeue dirs)))
                  (enqueue-all dirs (ls-dirs d dotfiles dmatch))
                  (ls-files d dotfiles fmatch)))))

(defun ls-files-deep-1 (dir &optional dotfiles fmatch dmatch)
  "Returns all files within `DIR' descending one level.
`DOTFILES' -- if non-nil don't include files and dirs starting with a `.'
`FMATCH' -- if non-nil only include files matching the regexp
`DMATCH' -- if non-nil only include and search dirs matching the regexp"
  (let ((dirs (cons dir (ls-dirs dir dotfiles dmatch))))
    (loop for d in dirs
          nconc (ls-files d dotfiles fmatch))))

(defun parent-dir (file-name)
  "Parent directory of given file."
  (file-name-directory
   (directory-file-name (file-name-directory (expand-file-name file-name)))))

(defun find-makefile (file-or-dir)
  "Searches for file `Makefile' in up to 3 parent dirs of `file-or-dir'."
  (let ((f "Makefile")
        (dir (file-name-directory file-or-dir)))
    (loop for i from 1 to 3             ; try 3 dirs up
          if (file-exists-p (concat dir f))
          return dir
          else
          do (setq dir (parent-dir dir)))))

(defun makefile-targets (makefile)
  "Gathers alphanumeric makefile targets from `MAKEFILE'."
  (let ((targets '())
        (was-open (find makefile (buffer-list) :key #'buffer-file-name :test #'string=))
        (buf (find-file-noselect makefile)))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:alnum:]].+?\\):\\(:?$\\| \\)" nil t)
        (add-to-list 'targets (car (split-string (match-string-no-properties 1) " " t)) t)))
    (unless was-open
      (kill-buffer buf))
    targets))

(provide 'cofi-util)
