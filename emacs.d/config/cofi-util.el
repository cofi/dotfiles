(require 'cl-lib)
(require 'queue)

(defmacro require-and-exec (feature &rest body)
  "Require the feature and execute body if it was successfull loaded."
  (declare (indent defun))
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

(defun x-urgency-hint (frame set &optional source)
  "Set urgency hint for `frame' to `set' for `source'."
  (let* ((wm-hints (append (x-window-property
                            "WM_HINTS" frame "WM_HINTS"
                            (if source
                                source
                              (string-to-number
                               (frame-parameter frame 'outer-window-id)))
                            nil t)
                           nil))
         (flags (car wm-hints)))
    (setcar wm-hints
            (if set
                (logior flags #x00000100)
              (logand flags #xFFFFFEFF)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun add-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun add-all-to-hook (hook &rest funs)
  "Add functions to hook."
  (add-to-hook hook funs))

(defun add-to-hook (hook funs)
  "Add list of functions to hook."
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
  (cl-loop for b in (buffer-list)
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
  (cl-remove-if (lambda (f) (dot? f dotfiles))
                (directory-files directory full match)))

(defun ls-dirs (directory &optional dotfiles match)
  "Returns all dirs in `DIR'.
`DOTFILES' -- if non-nil don't include dirs starting with a `.'
`MATCH' -- if non-nil only include dirs matching the regexp"
  (cl-remove-if-not #'file-directory-p
                    (ls-no-dots directory t dotfiles match)))

(defun ls-files (directory &optional dotfiles match)
  "Returns all files in `DIR'.
`DOTFILES' -- if non-nil don't include files starting with a `.'
`MATCH' -- if non-nil only include files matching the regexp"
  (cl-remove-if #'file-directory-p
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
    (cl-loop while (> (queue-length dirs) 0)
             nconc (let ((d (queue-dequeue dirs)))
                     (enqueue-all dirs (ls-dirs d dotfiles dmatch))
                     (ls-files d dotfiles fmatch)))))

(defun ls-files-deep-1 (dir &optional dotfiles fmatch dmatch)
  "Returns all files within `DIR' descending one level.
`DOTFILES' -- if non-nil don't include files and dirs starting with a `.'
`FMATCH' -- if non-nil only include files matching the regexp
`DMATCH' -- if non-nil only include and search dirs matching the regexp"
  (let ((dirs (cons dir (ls-dirs dir dotfiles dmatch))))
    (cl-loop for d in dirs
             nconc (ls-files d dotfiles fmatch))))

(defun parent-dir (file-name)
  "Parent directory of given file."
  (file-name-directory
   (directory-file-name (file-name-directory (expand-file-name file-name)))))

(defun find-makefile (file-or-dir)
  "Searches for file `Makefile' in up to 3 parent dirs of `file-or-dir'."
  (let ((f "Makefile")
        (dir (file-name-directory file-or-dir)))
    (cl-loop for i from 1 to 3             ; try 3 dirs up
             if (file-exists-p (concat dir f))
             return dir
             else
             do (setq dir (parent-dir dir)))))

(defun makefile-targets (makefile)
  "Gathers alphanumeric makefile targets from `MAKEFILE'."
  (let ((targets '())
        (was-open (cl-find makefile (buffer-list) :key #'buffer-file-name :test #'string=))
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
  (cl-loop for x in xs
           append (cl-loop for y in ys
                           collect (funcall combinator x y))))

(defun range (end-or-start &optional end step)
  "Range of numbers from `START' to (including) `END' with stepwidth `STEP'.
If only one argument is supplied it will be the end, 0 will be start.
Mimicks Python's `range'"
  (let ((step (or step 1))
        (start (if end
                   end-or-start
                 0))
        (end (if end
                 end
               end-or-start)))
    (cl-loop for i from start to end by step
             collect i)))

(defun empty? (x)
  "Test if `x' is empty."
  (cl-typecase x
    (string (string= x ""))
    (list (null x))
    (vector (= (length x) 0))))

(defun cofi/org-wmean (values weights)
  "Sum `VALUES' weighted according to `WEIGHTS' and divide by the sum of `WEIGHTS'.
Tailored specific to org tables, i.e. input expected as strings and output are
strings."
  (cl-loop for v in values
           for w in weights
           unless (or (empty? v) (empty? w))
           sum (string-to-number w) into wsum
           collect (* (string-to-number v) (string-to-number w))
           into weighted-values
           finally (cl-return (let ((weighted-sum (/ (apply #'+ weighted-values) wsum)))
                                (if (= weighted-sum 0)
                                    ""
                                  (format "%.1f" weighted-sum))))))

(defmacro turn-on-file (mode)
  "Return a `turn-on' fun for given mode that only turns on in buffers that visit files."
  `(lambda ()
     (if buffer-file-name
         (,mode 1))))

;;; keybinding
(defun cofi/set-key (map spec cmd)
  "Set in `map' `spec' to `cmd'.

`Map' may be `'global' `'local' or a keymap.
A `spec' can be a `read-kbd-macro'-readable string or a vector."
  (let ((setter-fun (cl-case map
                      (global #'global-set-key)
                      (local  #'local-set-key)
                      (t      (lambda (key def) (define-key map key def)))))
        (key (cl-typecase spec
               (vector spec)
               (string (read-kbd-macro spec))
               (t (error "wrong argument")))))
    (funcall setter-fun key cmd)))

(defmacro defkeymap (symbol &rest mappings)
  "Define keymap bound to `symbol'.
See `pour-mappings-to'"
  `(progn (unless (boundp ',symbol)
            (defvar ,symbol (make-sparse-keymap)))
          (fill-keymap ,symbol ,@mappings)))

(defun fill-keymap (keymap &rest mappings)
  "Fill `KEYMAP' with `MAPPINGS'.
See `pour-mappings-to'."
  (declare (indent defun))
  (pour-mappings-to keymap mappings))

(defun fill-keymaps (keymaps &rest mappings)
  "Fill `KEYMAPS' with `MAPPINGS'.
See `pour-mappings-to'."
  (declare (indent defun))
  (dolist (keymap keymaps keymaps)
    (let ((map (if (symbolp keymap)
                   (symbol-value keymap)
                 keymap)))
      (pour-mappings-to map mappings))))

(defmacro gen-fill-keymap-hook (keymap &rest mappings)
  "Build fun that fills `KEYMAP' with `MAPPINGS'.
See `pour-mappings-to'."
  (declare (indent defun))
  `(lambda () (fill-keymap ,keymap ,@mappings)))

(defmacro gen-local-fill-keymap-hook (&rest mappings)
  "Build fun that fills local keymap with `MAPPINGS'.
See `pour-mappings-to'."
  (declare (indent defun))
  `(lambda () (fill-keymap 'local ,@mappings)))

(defun pour-mappings-to (map mappings)
  "Calls `cofi/set-key' with `map' on every key-fun pair in `MAPPINGS'.
`MAPPINGS' is a list of string-fun pairs, with a `READ-KBD-MACRO'-readable string and a interactive-fun."
  (dolist (mapping (group mappings 2))
    (cofi/set-key map (car mapping) (cadr mapping)))
  map)

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
      (cl-decf n)
      (push (car lst) acc)
      (setq  lst (cdr lst)))
    (nreverse acc)))

(defmacro cmd (name &rest code)
  "Macro for shorter keybindings."
  `(progn
     (defun ,(intern (concat "cofi-cmd/" (symbol-name name))) ()
       (interactive)
       ,@code)
     ',(intern (concat "cofi-cmd/" (symbol-name name)))))

(defmacro cmd-arg (name args iflag &rest code)
  "Macro for shorter keybindings with argument.

For example:
  (cmd-arg foo (num) \"p\"
    (message \"num-prefix: %d\" num)"
  `(progn
     (defun ,(intern (concat "cofi-cmd/" (symbol-name name))) ,args
       (interactive ,iflag)
       ,@code)
     ',(intern (concat "cofi-cmd/" (symbol-name name)))))

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

(cl-defstruct (cofi/ring
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

(defun get-equal (x)
  "Return a suitable equal fun for `X'."
  (typecase x
    (string             #'string=)
    (character          #'char-equal)
    ((or integer float) #'=)
    (t                  #'equal)))

(defun library-byte-compiled-p (library)
  "Test if library is byte-compiled."
  (string-match-p "\\.elc$" (locate-library library)))

(cl-defun find-index (x xs &key (test (get-equal x)))
  "Alike `cl-find' but return index.
Returns nil if `X' not in `XS'."
  (cl-loop for a being the elements of xs
           count t into i
           do (if (funcall test a x)
                  (cl-return (1- i)))
           finally (cl-return nil)))

(defun def-assoc (key alist default)
  "Return cdr of `KEY' in `ALIST' or `DEFAULT' if key is no car in alist."
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
      default)))

(defun ends-with (string end)
  "Return if `STRING' ends with `END'."
  ;; ignore error if string smaller than end
  (ignore-errors
    (string= end
             (substring string (- (length end))))))

(defun begins-with (string begin)
  "Return if `STRING' begins with `BEGIN'."
  ;; ignore error if string smaller than begin
  (ignore-errors
    (string= begin
             (substring string 0 (length begin)))))

(defun byte-compile-config-on-save ()
  "Compile elisp files in the emacs.d dir unless they are themes."
  (let ((fname (buffer-file-name)))
    (when (string-match "emacs\\.d/config/.*\\.el$" fname)
      (byte-compile-file fname))))

(defun f-alt (&rest alternatives)
  "Test functions in `alternatives' and return first bound."
  (catch 'found
    (dolist (f alternatives)
      (if (functionp f)
          (throw 'found f)))))

(defmacro pour-lists (place &rest lists)
  "Append `LISTS' in front of list at `PLACE'."
  `(setq ,place (append ,@lists ,place)))

(defmacro on-full-instance (&rest body)
  `(when cofi/full-emacs
     ,@body))

(defmacro on-comm-instance (&rest body)
  `(when cofi/comm-instance
     ,@body))

(cl-defun cofi/contains-any (lst test &optional (test-fun #'memq))
  "Test if `lst' contains any of `test'."
  (cl-loop for x in test
           thereis (funcall test-fun x lst)))

(defun cofi/pos-in-string-p (pos)
  (let ((face-props (get-text-property pos 'face)))
    (cofi/contains-any (if (listp face-props)
                           face-props
                         (list face-props))
                       '(font-lock-doc-face
                         font-lock-string-face))))

(defun cofi/pos-in-comment-p (pos)
  (let ((face-props (get-text-property pos 'face)))
    (cofi/contains-any (if (listp face-props)
                           face-props
                         (list face-props))
                       '(font-lock-comment-face))))

(provide 'cofi-util)
