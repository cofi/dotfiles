(require 'cl)
(require 'queue)

(defmacro require-and-exec (feature &optional &rest body)
  "Require the feature and execute body if it was successfull loaded."
  (declare (indent 1))
  `(if (require ,feature nil 'noerror)
        (progn ,@body)
    (message (format "%s not loaded" ,feature))))

(defmacro load-and-exec (file &optional &rest body)
  "Load the file and execute body if it was successfull loaded."
  (declare (indent 1))
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

(defun add-all-to-hook (hook &rest funs)
  "Add functions to hook"
  (dolist (fun funs)
    (add-hook hook fun)))

(defvar cofi/before-kill-hook
  '(
    recentf-save-list
    bookmark-save
    )
  "Functions to be executed by `SAVE-KILL-EMACS'")

(defun save-kill-emacs ()
  (interactive)
  (mapc (lambda (f) (ignore-errors (funcall f)))
        cofi/before-kill-hook
        )
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

(defun combinate (xs ys combinator)
  "Combinate each x with each y using the given combinator."
  (loop for x in xs
       append (loop for y in ys
                    collect (funcall combinator x y))))

(defun range (start &optional end step)
  "Range of numbers from `START' to (including) `END' with stepwidth `STEP'.
If only one argument is supplied it will be the end, 0 will be start.
Mimicks Python's `range'"
  (let ((step (or step 1))
        (start (if end
                   start
                 0))
        (end (if end
                 end
               start)))
    (loop for i from start to end by step
          collect i)))

(defun empty-string? (s)
  "Test if `S' is an empty string."
  (string= s ""))

(defun cofi/org-wmean (values weights)
  "Sum `VALUES' weighted according to `WEIGHTS' and divide by the sum of `WEIGHTS'.
Tailored specific to org tables, i.e. input expected as strings and output are
strings."
  (loop for v in values
        for w in weights
        unless (or (empty-string? v) (empty-string? w))
        sum (string-to-number w) into wsum
        collect (* (string-to-number v) (string-to-number w))
                into weighted-values
        finally (return (let ((weighted-sum (/ (reduce #'+ weighted-values) wsum)))
                          (if (= weighted-sum 0)
                              ""
                            (format "%.1f" weighted-sum))))))
(defmacro turn-on (mode)
  "Return a `turn-on' fun for given mode."
  `(lambda ()
     (interactive)
     (,mode 1)))

(defmacro turn-on-file (mode)
  "Return a `turn-on' fun for given mode that only turns on in buffers that visit files."
  `(lambda ()
     (interactive)
     (if buffer-file-name
         (,mode 1))))

(defmacro defkeymap (symbol &rest mappings)
  (declare (indent 1))
  "Define keymap bound to `SYMBOL'.
See `POUR-MAPPINGS-WITH'."
  `(setq ,symbol (fill-keymap (make-sparse-keymap) ,@mappings)))

(defun fill-keymap (keymap &rest mappings)
  "Fill `KEYMAP' with `MAPPINGS'.
See `POUR-MAPPINGS-WITH'."
  (pour-mappings-with (lambda (key fun) (define-key keymap key fun)) mappings)
  keymap)

(defun fill-keymaps (keymaps &rest mappings)
  "Fill `KEYMAPS' with `MAPPINGS'.
See `POUR-MAPPINGS-WITH'."
  (dolist (keymap keymaps keymaps)
    (pour-mappings-with (lambda (key fun) (define-key keymap key fun)) mappings)))

(defmacro gen-fill-keymap-hook (keymap &rest mappings)
  (declare (indent 1))
  "Build fun that fills `KEYMAP' with `MAPPINGS'.
See `POUR-MAPPINGS-WITH'."
  `(lambda () (fill-keymap ,keymap ,@mappings)))

(defmacro gen-local-fill-keymap-hook (&rest mappings)
  (declare (indent 1))
  "Build fun that fills local keymap with `MAPPINGS'.
See `POUR-MAPPINGS-WITH'."
  `(lambda () (fill-local-keymap ,@mappings)))

(defun fill-local-keymap (&rest mappings)
  "Fill local keymap with `MAPPINGS'.
See `POUR-MAPPINGS-WITH'."
  (pour-mappings-with 'local-set-key mappings))

(defun fill-global-keymap (&rest mappings)
  "Fill global keymap with `MAPPINGS'.
See `POUR-MAPPINGS-WITH'."
  (pour-mappings-with 'global-set-key mappings))

(defun pour-mappings-with (fill-fun mappings)
  "Calls `FILL-FUN' on every key-fun pair in `MAPPINGS'.
`MAPPINGS' is a list of string-fun pairs, with a `READ-KBD-MACRO'-readable string and a interactive-fun."
  (dolist (mapping (group mappings 2))
    (funcall fill-fun (read-kbd-macro (car mapping)) (cadr mapping))))

(defun group (lst n)
  "Group `LST' into portions of `N'."
  (let (groups)
    (while lst
      (push (take n lst) groups)
      (setq lst (nthcdr n lst)))
    (nreverse groups)))

(defun take (n lst)
  "Return atmost the first `N' items of `LST'."
  (let (acc '())
    (while (and lst (> n 0))
      (decf n)
      (push (car lst) acc)
      (setq  lst (cdr lst)))
    (nreverse acc)))

(defmacro cmd (&rest code)
  "Macro for shorter keybindings."
  (declare (indent 1))
  `(lambda ()
     (interactive)
     ,@code))

(defmacro cmd-arg (args iflag &rest code)
  "Macro for shorter keybindings with argument."
  (declare (indent 1))
  `(lambda (,@args)
     (interactive iflag)
     ,@code))

(defun gen-extension-re (&rest extensions)
  "Generate a regexp that matches all `EXTENSIONS'."
  (concat "\\.\\("
          (mapconcat 'identity
                     extensions
                     "\\|")
          "\\)$"))

(defun cofi/var-file (filename)
  (format "~/var/%s" filename))

(defun add-major-mode (re mode)
  "Add new major-mode alist-pair."
  (pushnew (cons re mode) auto-mode-alist))

(defmacro catch-error-hook (&rest body)
  "Create function suitable for hooks and catching errors."
  `(lambda ()
     (ignore-errors
       ,@body)))

(defmacro p (&rest body)
  "Create anonymous predicate"
  `(lambda (x)
    ,@body))

(defstruct (cofi/ring
            (:constructor nil)
            (:constructor cofi/make-ring (vec &key curr))
            (:conc-name cofi/ring--))
  "Constant ring buffer."
  vec curr)

(defun cofi/ring-current (ring)
  "Current element of `RING'."
  (let ((curr (cofi/ring--curr ring)))
    (aref (cofi/ring--vec ring)
          (or curr 0))))

(defun cofi/ring-next (ring)
  "Next element of `RING'."
  (let ((next (mod (1+ (or (cofi/ring--curr ring)
                           -1))
                    (cofi/ring-length ring))))
    (setf (cofi/ring--curr ring) next)
    (aref (cofi/ring--vec ring) next)))

(defun cofi/ring-length (ring)
  "Length of `RING'."
  (length (cofi/ring--vec ring)))

(defun flatten (xs)
  "Flattens `XS' into a single list."
  (cond
   ((null xs) nil)
   ((listp xs) (append (flatten (car xs)) (flatten (cdr xs))))
   (t (list xs))))

(provide 'cofi-util)
