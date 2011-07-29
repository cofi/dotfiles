;;; pause.el --- Take a break!
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-01-19 Sat
(defconst pause:version "0.70");; Version:
;; Last-Updated: 2010-01-18 Mon
;; URL:
;; Keywords: calendar convenience
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; If you are using Emacs then don't you need a little reminder to
;; take a pause?  This library makes Emacs remind you of that.  And
;; gives you a link to a yoga exercise to try in the pause.
;;
;; There are essentially two different ways to use this library.
;; Either you run a separate Emacs process that just reminds you of
;; pauses.  To use it that way see `pause-start-in-new-emacs'.
;;
;; Or run it in the current Emacs.  To do that add to your .emacs
;;
;;   (require 'pause)
;;
;; and do
;;
;;   M-x customize-group RET pause RET
;;
;; and set `pause-mode' to t.
;;
;;
;; Note: I am unsure if it works on all systems to use a separate
;;       Emacs process.  It does work on w32 though.  Please tell me
;;       about other systems.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;###autoload
(defgroup pause nil
  "Customize your health personal Emacs health saver!"
  :group 'convenience)

;; DEBUG
;;(progn (setq pause-after-minutes 0.2) (setq pause-alpha-100-delay 5) (setq pause-extra-fun nil) (pause-mode -1))
;;(progn (setq pause-after-minutes 15) (setq pause-alpha-100-delay 30) (setq pause-extra-fun 'pause-start-get-yoga-poses) (pause-mode 1))
;;(pause-mode 1)
(defcustom pause-after-minutes 15
  "Pause after this number of minutes."
  :type 'number
  :group 'pause)

(defcustom pause-1-minute-delay 60
  "Number of seconds to wait in 1 minutes delay."
  :type 'number
  :group 'pause)

(defcustom pause-idle-delay 5
  "Seconds to wait for user to be idle before pause."
  :type 'number
  :group 'pause)

(defcustom pause-even-if-not-in-emacs t
  "Jump up pause even if not in Emacs."
  :type 'boolean
  :group 'pause)

(defcustom pause-restart-anyway-after 2
  "If user does not use Emacs restart timer after this minutes.
This is used when a user has clicked a link."
  :type 'number
  :group 'pause)

(defcustom pause-tell-again-after 2
  "If user does not exit pause tell again after this minutes."
  :type 'number
  :group 'pause)

(defcustom pause-extra-fun 'pause-start-get-yoga-poses
  "Function to call for extra fun when pausing.
Default is to show a link to a yoga exercise (recommended!).

Set this variable to nil if you do not want any extra fun.

If this variable's value is a function it will be called when the
pause frame has just been shown."
  :type '(choice (function :tag "Extra function")
                 (const :tag "No extra function" nil))
  :group 'pause)

(defvar pause-exited-from-button nil)

(defcustom pause-break-frame-size '(60 . 23)
  "Frame size during pauses."
  :type '(cons (integer :tag "Columns")
               (integer :tag "Rows"))
  :group 'pause)

(defcustom pause-goon-frame-size '(35 . 6)
  "Frame size between pauses.
This is only seen when using a separate Emacs since the pause
frame is otherwise deleted between pauses."
  :type '(cons (integer :tag "Columns")
               (integer :tag "Rows"))
  :group 'pause)

(defcustom pause-hint-alpha 10
  "Initial opacity for break screen.
See also `frame-alpha-lower-limit' which is the lowest alpha that
can be used."
  :type '(integer :tag "Opacity (100 full)")
  :group 'pause)

(defcustom pause-alpha-100-delay 60
  "Delay seconds before setting pause frame opaque."
  :type 'integer
  :group 'pause)

(defcustom pause-break-background-color "orange"
  "Background color during pause."
  :type 'color
  :group 'pause)

(defcustom pause-goon-background-color "green yellow"
  "Background color between pauses.
This is only seen when using a separate Emacs since the pause
frame is otherwise deleted between pauses."
  :type 'color
  :group 'pause)

(defcustom pause-mode-line-color "sienna"
  "Mode line color during pause."
  :type 'color
  :group 'pause)

(defcustom pause-1-minute-mode-line-color "yellow"
  "Mode line color during 1 minute phase of pause."
  :type 'color
  :group 'pause)

(defface pause-text-face
  '((t (:foreground "sienna" :height 1.5 :bold t)))
  "Face main text in pause buffer."
  :group 'pause)

(defface pause-info-text-face
  '((t (:foreground "yellow")))
  "Face info text in pause buffer."
  :group 'pause)

(defface pause-message-face
  '((t (:inherit secondary-selection)))
  "Face for pause messages."
  :group 'pause)

(defface pause-mouse-face
  '((t (:background "yellow" :underline t)))
  "Mouse face used for links in pause buffer."
  :group 'pause)

(defface pause-1-minute-message-face
  '((t (:inherit mode-line-inactive)))
  "Face for pause messages."
  :group 'pause)

(defcustom pause-break-text
  (concat "\n\tHi there,"
          "\n\tYou are worth a PAUSE!"
          "\n\nTry some mindfulness:"
          "\n\t- Look around and observe."
          "\n\t- Listen."
          "\n\t- Feel your body.")
  "Text to show during pause."
  :type 'integer
  :group 'pause)

(defvar pause-in-separate-emacs nil)

(defvar pause-el-file (or load-file-name
                          (when (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))

(defvar pause-default-img-dir
  (let ((this-dir (file-name-directory pause-el-file)))
    (expand-file-name "../etc/img/pause/" this-dir)))

(defcustom pause-img-dir pause-default-img-dir
  "Image directory for pause.
A random image is chosen from this directory for pauses."
  :type 'directory
  :group 'pause)



(defvar pause-timer nil)

;;(defvar pause-break-exit-calls nil)

(defun pause-start-timer ()
  "Start main timer with delay `pause-after-minutes'."
  (pause-start-timer-1 (* 60 pause-after-minutes)))

(defun pause-start-timer-1 (sec)
  "Start main timer with delay SEC seconds."
  (pause-cancel-timer)
  (setq pause-timer (run-with-timer sec nil 'pause-pre-break)))

(defun pause-one-minute ()
  "Give you another minute ...
Start main timer with delay `pause-1-minute-delay'."
  (pause-start-timer-1 pause-1-minute-delay)
  (message (propertize " OK, I will come back in a minute! -- greetings from pause"
                       'face 'pause-message-face)))

(defun pause-save-me ()
  "Start main timer and give a message."
  (pause-start-timer)
  (message (propertize " OK, I will save you again in %d minutes! -- greetings from pause "
                       'face 'pause-message-face)
           pause-after-minutes))

(defun pause-pre-break ()
  "Start waiting for idle `pause-idle-delay' before break."
  (condition-case err
      (save-match-data ;; runs in timer
        ;;(message "pause: enter pause-bre-break")
        (pause-cancel-timer)
        ;;(message "pause: after pause-cancel-timer, pause-idle-delay=%s" pause-idle-delay)
        (setq pause-timer (run-with-idle-timer pause-idle-delay nil 'pause-break-in-timer)))
    (error
     (lwarn 'pause-pre-break
            :error "%s" (error-message-string err)))))

(defvar pause-break-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control meta shift ?p)] 'pause-break-exit-from-button)
    (define-key map [tab]         'forward-button)
    (define-key map [(meta tab)]  'backward-button)
    (define-key map [(shift tab)] 'backward-button)
    (define-key map [backtab]     'backward-button)
    map))

(defvar pause-buffer nil)
(defvar pause-image-buffer nil)
(defvar pause-frame nil)
(defvar pause-before-frame nil)

(define-derived-mode pause-break-mode nil "Pause"
  "Mode used during pause in pause buffer.

It defines the following key bindings:

\\{pause-break-mode-map}"
  (set (make-local-variable 'buffer-read-only) t)
  (setq show-trailing-whitespace nil)
  ;;(set (make-local-variable 'cursor-type) nil)
  ;; Fix-me: workaround for emacs bug
  ;;(run-with-idle-timer 0 nil 'pause-hide-cursor)
  )

;; Fix-me: make one state var
(defvar pause-break-exit-active nil)
(defvar pause-break-1-minute-state nil)

(defun pause-break-topmost ()
  (pause-break-show))

(defun pause-break-no-topmost ()
  "Do the break.
Setup the pause frame and show it.  Enter recursive edit to avoid
bad edits.

After pause exit start timer again after next command.  However
if single pause Emacs start timer immediately."
  (pause-cancel-timer)
  (let ((wcfg (current-frame-configuration))
        (old-mode-line-bg (face-attribute 'mode-line :background))
        old-frame-bg-color
        old-frame-left-fringe
        old-frame-right-fringe
        old-frame-tool-bar-lines
        old-frame-menu-bar-lines
        old-frame-vertical-scroll-bars
        (old-frame-list (unless (pause-use-topmost) (frame-list))))
    (dolist (f old-frame-list)
      (add-to-list 'old-frame-bg-color (cons f (frame-parameter f 'background-color)))
      (add-to-list 'old-frame-left-fringe (cons f (frame-parameter f 'left-fringe)))
      (add-to-list 'old-frame-right-fringe (cons f (frame-parameter f 'right-fringe)))
      (add-to-list 'old-frame-tool-bar-lines (cons f (frame-parameter f 'tool-bar-lines)))
      (add-to-list 'old-frame-menu-bar-lines (cons f (frame-parameter f 'menu-bar-lines)))
      (add-to-list 'old-frame-vertical-scroll-bars (cons f (frame-parameter f 'vertical-scroll-bars))))

    ;; Fix-me: Something goes wrong with the window configuration, try a short pause
    (remove-hook 'window-configuration-change-hook 'pause-break-exit-no-topmost)
    (run-with-idle-timer 0.2 nil 'pause-break-show)
    (setq pause-break-exit-active nil)
    (setq pause-break-1-minute-state nil) ;; set in `pause-break-show'
    (setq pause-exited-from-button nil)
    (unwind-protect
        (let ((n 0)
              (debug-on-error nil))
          (while (and (> 3 (setq n (1+ n)))
                      (not pause-break-exit-active)
                      (not pause-break-1-minute-state))
            (condition-case err
                (recursive-edit)
              (error (message "pause: %s" (error-message-string err))))
            (unless (or pause-break-exit-active
                        pause-break-1-minute-state)
              (when (> 2 n) (message "Too early to pause (%s < 2)" n))
              (add-hook 'window-configuration-change-hook 'pause-break-exit))))

      (remove-hook 'window-configuration-change-hook 'pause-break-exit-no-topmost)
      ;;(pause-cancel-tell-again-timer)
      ;;(set-frame-parameter nil 'background-color "white")
      (dolist (f old-frame-list)
        (set-frame-parameter f 'background-color     (cdr (assq f old-frame-bg-color)))
        (set-frame-parameter f 'left-fringe          (cdr (assq f old-frame-left-fringe)))
        (set-frame-parameter f 'right-fringe         (cdr (assq f old-frame-right-fringe)))
        (set-frame-parameter f 'tool-bar-lines       (cdr (assq f old-frame-tool-bar-lines)))
        (set-frame-parameter f 'menu-bar-lines       (cdr (assq f old-frame-menu-bar-lines)))
        (set-frame-parameter f 'vertical-scroll-bars (cdr (assq f old-frame-vertical-scroll-bars))))
      ;; Fix-me: The frame grows unless we do redisplay here:
      (redisplay t)
      (when (< 1 (length (frame-list)))
        (pause-delete-pause-frame))
      (set-frame-configuration wcfg t)
      ;;(when pause-frame (lower-frame pause-frame))
      (set-face-attribute 'mode-line nil :background old-mode-line-bg)
      (run-with-idle-timer 2.0 nil 'run-hooks 'pause-break-exit-hook)
      (kill-buffer pause-buffer)
      (cond (pause-exited-from-button
             (pause-restart-after-button))
            (pause-break-1-minute-state
             (run-with-idle-timer 0 nil 'pause-one-minute))
            (t
             (run-with-idle-timer 0 nil 'pause-save-me))))))

(defun pause-restart-after-button ()
  ;; Do not start timer until we start working again if not
  ;; in a separate Emacs.
  (if pause-in-separate-emacs
      (progn
        (when (pause-use-topmost) (pause-set-topmost nil))
        (modify-frame-parameters pause-frame
                                 `((background-color . ,pause-goon-background-color)
                                   (width . ,(car pause-goon-frame-size))
                                   (height . ,(cdr pause-goon-frame-size))
                                   (alpha . 100)
                                   ))
        (pause-minimize)
        (pause-start-timer))
    (run-with-idle-timer 1 nil 'add-hook 'post-command-hook 'pause-save-me-post-command)
    ;; But if we do not do that within some minutes then start timer anyway.
    (run-with-idle-timer (* 60 pause-restart-anyway-after) nil 'pause-save-me)))

(defun pause-save-me-post-command ()
  "Start pause timer again.  Version for `post-command-hook'."
  (pause-start-timer))

(defvar pause-break-exit-hook nil
  "Hook run after break exit.
Frame configuration has been restored when this is run.
Please note that it is run in a timer.")

(defvar pause-break-last-wcfg-change (float-time))

(defvar pause-set-alpha-100-timer nil)

(defun pause-cancel-alpha-100-timer ()
  (when (timerp pause-set-alpha-100-timer)
    (cancel-timer pause-set-alpha-100-timer))
  (setq pause-set-alpha-100-timer nil))

(defun pause-start-alpha-100-timer (delay)
  (pause-cancel-alpha-100-timer)
  (setq pause-set-alpha-100-timer (run-with-timer delay nil 'pause-set-alpha-100)))

(defun pause-set-alpha-100 ()
  (condition-case err
      (when (frame-live-p pause-frame)
        (modify-frame-parameters pause-frame '((alpha . 100)))
        ;;(message "pause: alpha 100 done")
        (pause-set-topmost t)
        ;;(redisplay t)
        )
    (error (message "pause-set-alpha-100 error: %s" (error-message-string err)))))

(defun pause-check-alpha-on-click ()
  (if (eq 100 (frame-parameter pause-frame 'alpha))
      t
    (pause-set-alpha-100)
    nil))

(defun pause-create-pause-buffer ()
  (with-current-buffer (setq pause-buffer
                             (get-buffer-create "* P A U S E *"))
    (setq mode-line-format nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pause-break-mode)
      ;;(setq left-margin-width 25)
      ;;(pause-insert-img 'left-margin)
      (insert (propertize pause-break-text 'face 'pause-text-face))
      (goto-char (point-min))
      (when (search-forward "mindfulness" nil t)
        (make-text-button (- (point) 11) (point)
                          'face '(:inherit pause-text-face :underline t)
                          'mouse-face 'pause-mouse-face
                          'action (lambda (btn)
                                     (condition-case err
                                         (when (pause-check-alpha-on-click)
                                           (browse-url "http://www.jimhopper.com/mindfulness/"))
                                       (error (message "pause-button: %s" (error-message-string err)))))))
      (goto-char (point-max))
      (insert (propertize "\n\nClick on a link below to continue\n" 'face 'pause-info-text-face))
      ;;(add-text-properties (point-min) (point-max) (list 'keymap (make-sparse-keymap)))

      ;; Show saved exercises (Clear saved exercises)
      ;; ex1 (done)
      (insert-text-button "I am ready with this break"
                          'mouse-face 'pause-mouse-face
                          'action `(lambda (button)
                                     (condition-case err
                                         (when (pause-check-alpha-on-click)
                                           (pause-cancel-tell-again-timer)
                                           (pause-break-exit-from-button))
                                       (error (message "pause-button: %s" (error-message-string err))))))
      (insert "\n")
      (goto-char (point-min))
      (dolist (m '(hl-needed-mode))
        (when (and (boundp m) (symbol-value m))
          (funcall m -1))))))

(defun pause-break-show-1 ()
  ;; fix-me: temporary:
  ;;(add-hook 'window-configuration-change-hook 'pause-break-exit)
  (unless pause-extra-fun (run-with-idle-timer 1  nil 'pause-break-message))
  (run-with-idle-timer 10 nil 'pause-break-exit-activate)
  (setq pause-break-1-minute-state t)
  (unless (pause-use-topmost)
    (set-face-attribute 'mode-line nil :background pause-1-minute-mode-line-color))
  (with-current-buffer (setq pause-image-buffer
                             (get-buffer-create "* P A U S E *"))
    (setq mode-line-format nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pause-insert-img nil)
      ))
  (pause-create-pause-buffer)
  (when pause-extra-fun (funcall pause-extra-fun))
  (with-current-buffer pause-buffer
    (message "Going to point-min in pause-buffer from %s" (point))
    (goto-char (point-min)))
  ;;(setq pause-break-exit-calls 0)
  ;;(setq pause-frame (selected-frame))
  (pause-get-pause-frame)
  ;;(set-frame-parameter pause-frame 'background-color pause-break-background-color)
  ;;(setq frame-alpha-lower-limit 5)
  (let ((frame-alpha-lower-limit pause-hint-alpha)
        (use-alpha (if (pause-use-topmost) pause-hint-alpha 100)))
    (message "SETTING pause-frame size now!")
    (modify-frame-parameters pause-frame
                             `((background-color . ,pause-break-background-color)
                               (width . ,(car pause-break-frame-size))
                               (height . ,(cdr pause-break-frame-size))
                               (alpha . (100 . ,use-alpha))
                               )))
  ;; Do these first if something goes wrong.
  (setq pause-break-last-wcfg-change (float-time))
  (unless (pause-use-topmost)
    (dolist (f (frame-list))
      (pause-set-frame-size f)))
  ;;(run-with-idle-timer (* 1.5 (length (frame-list))) nil 'add-hook 'window-configuration-change-hook 'pause-break-exit)
  ;; Fix-me: the alpha timer should be handled by pause-extra-fun.
  (pause-tell-again)

  (setq pause-break-last-wcfg-change (float-time))
  (pause-tell-again-start-timer))

(defun pause-break-show ()
  ;; In timer
  (save-match-data
    (condition-case err
        (pause-break-show-1)
      (error
       ;;(remove-hook 'window-configuration-change-hook 'pause-break-exit)
       (pause-break-exit-from-button)
       (message "pause-break-show error: %s" (error-message-string err))))))

(defun pause-set-frame-size (f)
  (let* ((avail-width (- (display-pixel-width)
                         (* 2 (frame-parameter f 'border-width))
                         (* 2 (frame-parameter f 'internal-border-width))))
         (avail-height (- (display-pixel-height)
                          (* 2 (frame-parameter f 'border-width))
                          (* 2 (frame-parameter f 'internal-border-width))))
         (cols (/ avail-width (frame-char-width)))
         (rows (- (/ avail-height (frame-char-height)) 2)))
    ;; Fix-me: bug in Emacs, remove 3 rows
    (setq rows (- rows 3))
    ;;(set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
    ;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
    (setq pause-break-last-wcfg-change (float-time))
    (with-selected-frame f
      (delete-other-windows (frame-first-window f))
      (with-selected-window (frame-first-window)
        (switch-to-buffer pause-buffer)
        (setq cols 55)
        ;; (with-current-buffer pause-buffer
        ;;   (setq rows (+ 5 (line-number-at-pos (point-max))))
        ;;   )
        (set-frame-size f cols rows)
        ))))

(defvar pause-tell-again-timer nil)

(defun pause-tell-again-start-timer ()
  (pause-cancel-tell-again-timer)
  (setq pause-tell-again-timer
        (run-with-idle-timer (* 60 pause-tell-again-after) t 'pause-tell-again)))

(defun pause-cancel-tell-again-timer ()
  (when (timerp pause-tell-again-timer)
    (cancel-timer pause-tell-again-timer))
  (setq pause-tell-again-timer nil))

(defvar pause-dont-use-topmost nil)

(defun pause-use-topmost ()
  (unless pause-dont-use-topmost
    (fboundp 'w32-set-frame-topmost)))

(defun pause-set-topmost (on)
  (cond
   ((fboundp 'w32-set-frame-topmost)
    ;;(message "pause-set-topmost: %s %s" pause-frame on)
    (w32-set-frame-topmost pause-frame on nil)
    ;;(redisplay t)
    t)
   (t
    (message "pause-set-topmost: don't know how")
    nil)))

(defun pause-tell-again ()
  (when (and window-system pause-even-if-not-in-emacs)
    (let ((curr-frame (selected-frame))
          old-make-vis)
      (pause-set-frame-size pause-frame)
      (if (pause-use-topmost)
          (progn
            (pause-set-topmost t)
            (pause-show-no-activate)
            ;;(message "pause-tell-again: : topmost t done")
            (pause-start-alpha-100-timer pause-alpha-100-delay))
        ;;(message "pause-tell-again: raise-frame part")
        (raise-frame pause-frame)
        (x-focus-frame pause-frame)
        (condition-case nil
            (make-frame-visible pause-frame t)
          (error
           (setq old-make-vis t)))
        (when old-make-vis
          (make-frame-visible pause-frame)
          (run-with-idle-timer 5 nil 'pause-tell-again-reset-frame curr-frame))))))

(defun pause-tell-again-reset-frame (frame)
  ;;(message "pause-tell-again-reset-frame: frame=%S" frame)
  (condition-case err
      (select-frame frame)
    (error (message "pause-tell-again-reset-frame frame=%S: %s" frame (error-message-string err)))))

(defun pause-break-message ()
  (when (/= 0 (recursion-depth))
    (message "%s" (propertize "Please take a pause! (Or exit now to take it in 1 minute.)"
                              'face 'pause-1-minute-message-face))))

(defun pause-break-exit-activate ()
  (when (/= 0 (recursion-depth))
    (setq pause-break-exit-active t)
    (setq pause-break-1-minute-state nil)
    (unless (pause-use-topmost)
      (set-face-attribute 'mode-line nil :background pause-mode-line-color))
    (message nil)
    (with-current-buffer pause-buffer
      (let ((inhibit-read-only t))
        ;; Fix-me: This interfere with text buttons.
        ;;(add-text-properties (point-min) (point-max) (list 'keymap nil))
        ))))

(defun pause-break-exit-topmost ()
  (pause-delete-pause-frame)
  (setq pause-frame nil))

(defun pause-break-exit-no-topmost ()
  (interactive)
  (pause-cancel-tell-again-timer)
  (let ((elapsed (- (float-time) pause-break-last-wcfg-change)))
    ;;(message "elapsed=%s pause-break-last-wcfg-change=%s" elapsed pause-break-last-wcfg-change)
    (setq pause-break-last-wcfg-change (float-time))
    (when (> elapsed 1.0)
      (setq pause-break-exit-active t)
      (remove-hook 'window-configuration-change-hook 'pause-break-exit)
      ;;(pause-cancel-tell-again-timer)
      (when (/= 0 (recursion-depth))
        (exit-recursive-edit)))))

(defun pause-break-exit-from-button ()
  (setq pause-break-1-minute-state nil)
  (setq pause-exited-from-button t)
  (pause-restart-after-button)
  (if (pause-use-topmost)
      (pause-break-exit-topmost)
    (pause-break-exit-no-topmost)))

(defun pause-insert-img (where)
  (let* ((inhibit-read-only t)
        img
        src
        (slice '(0 0 200 300))
        (imgs (directory-files pause-img-dir nil nil t))
        skip)
    (setq imgs (delete nil
                       (mapcar (lambda (d)
                                 (unless (file-directory-p d) d))
                               imgs)))
    (if (not imgs)
        (setq img "No images found")
      (setq skip (random (length imgs)))
      (while (> skip 0)
        (setq skip (1- skip))
        (setq imgs (cdr imgs)))
      (setq src (expand-file-name (car imgs) pause-img-dir))
      (if (file-exists-p src)
          (condition-case err
              (setq img (create-image src nil nil
                                      :relief 1
                                      ;;:margin inlimg-margins
                                      ))
            (error (setq img (error-message-string err))))
        (setq img (concat "Image not found: " src))))
    (if (stringp img)
        (insert img)
      (insert-image img nil where slice))))

(defun pause-hide-cursor ()
  ;; runs in timer, save-match-data
  (with-current-buffer pause-buffer
    (set (make-local-variable 'cursor-type) nil)))

(defun pause-cancel-timer ()
  (remove-hook 'post-command-hook 'pause-save-me-post-command)
  (when (timerp pause-timer) (cancel-timer pause-timer))
  (setq pause-timer nil))

;;(pause-break-in-timer)
(defun pause-break-in-timer ()
  (message "pause-break-in-timer")
  (save-match-data ;; runs in timer
    (pause-cancel-timer)
    (if (active-minibuffer-window)
        (pause-pre-break)
      (if (pause-use-topmost)
          (condition-case err
              (pause-break-topmost)
            (error (error-message-string err)))
        (if (or (active-minibuffer-window)
                (and (boundp 'edebug-active)
                     edebug-active))
            (let ((pause-idle-delay 5))
              (pause-pre-break))
          (let ((there-was-an-error nil))
            (condition-case err
                (pause-break-no-topmost)
              (error
               (setq there-was-an-error t)))
            (when there-was-an-error
              (condition-case err
                  (progn
                    (select-frame last-event-frame)
                    (let ((pause-idle-delay nil))
                      (pause-pre-break)))
                (error
                 (lwarn 'pause-break-in-timer2 :error "%s" (error-message-string err))
                 )))))))))

(defcustom pause-only-when-server-mode t
  "Allow `pause-mode' only in the Emacs that has server-mode enabled.
This is to prevent multiple Emacs with `pause-mode'."
  :type 'boolean
  :group 'pause)

;; (setq x (selected-frame))
;; (eq x (selected-frame))
(defvar pause-is-deleting-pause-frame nil)
(defun pause-delete-pause-frame ()
  (let ((pause-is-deleting-pause-frame t))
    (when (frame-live-p pause-frame) (delete-frame pause-frame)))
  (setq pause-frame nil))

(defun pause-stop-on-frame-delete (frame)
  (unless pause-is-deleting-pause-frame
    (when (eq frame pause-frame)
      (pause-mode -1))))


;;;###autoload
(define-minor-mode pause-mode
  "This minor mode tries to make you take a break.
It will jump up and temporary stop your work - even if you are
not in Emacs.  If you are in Emacs it will however try to be
gentle and wait until you have been idle with the keyboard for a
short while. \(If you are not in Emacs it can't be gentle. How
could it?)

Then it will show you a special screen with a link to a yoga
exercise you can do when you pause.

After the pause you continue your work where you were
interrupted."
  :global t
  :group 'pause
  :set-after '(server-mode)
  (if pause-mode
      (if (and pause-only-when-server-mode
               (not server-mode)
               (not (with-no-warnings (called-interactively-p))))
          (progn
            (setq pause-mode nil)
            (message "Pause mode canceled because not server-mode"))
        (add-hook 'delete-frame-functions 'pause-stop-on-frame-delete)
        (pause-start-timer))
    (remove-hook 'delete-frame-functions 'pause-stop-on-frame-delete)
    (pause-cancel-timer)
    (pause-cancel-alpha-100-timer)
    (pause-cancel-tell-again-timer)
    (when (and pause-frame
               (not pause-in-separate-emacs))
      (when (frame-live-p pause-frame) (delete-frame pause-frame))
      (setq pause-frame nil))))

;; (emacs-Q "-l" buffer-file-name "--eval" "(pause-temp-err)")
;; (emacs-Q "-l" buffer-file-name "--eval" "(run-with-timer 1 nil 'pause-temp-err)")
;; (pause-temp-err)
(defun pause-temp-err ()
  (switch-to-buffer (get-buffer-create "pause-temp-err buffer"))
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (point-max) (list 'keymap nil))
    (insert-text-button "click to test"
                        'mouse-face 'pause-mouse-face
                        'action (lambda (btn)
                                  (message "Click worked")))
    ;;(add-text-properties (point-min) (point-max) (list 'keymap nil))
    ))

;; (customize-group-other-window 'pause)
;; (apply 'custom-set-variables (pause-get-group-saved-customizations 'pause custom-file))
;; (pause-get-group-saved-customizations 'w32shell custom-file)
(defun pause-get-group-saved-customizations (group cus-file)
  "Return customizations saved for GROUP in CUS-FILE."
  (let* ((cus-buf (find-buffer-visiting cus-file))
         (cus-old cus-buf)
         (cus-point (when cus-old (with-current-buffer cus-old (point))))
         (cusg-all (get group 'custom-group))
         (cusg-vars (delq nil (mapcar (lambda (elt)
                                        (when (eq (nth 1 elt) 'custom-variable)
                                          (car elt)))
                                      cusg-all)))
         cus-vars-form
         cus-face-form
         cus-saved-vars
         cus-saved-face)
    (unless cus-buf (setq cus-buf (find-file-noselect cus-file)))
    (with-current-buffer cus-buf
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (progn
                 (while (progn (skip-chars-forward " \t\n\^l")
                               (looking-at ";"))
                   (forward-line 1))
                 (not (eobp)))
          (let ((form (read (current-buffer))))
            (cond
             ((eq (car form) 'custom-set-variables)
              (setq cus-vars-form form))
             ((eq (car form) 'custom-set-faces)
              (setq cus-face-form form))
            )))))
    (dolist (vl (cdr cus-vars-form))
      (when (memq (car (cadr vl)) cusg-vars)
        (setq cus-saved-vars (cons (cadr vl) cus-saved-vars))))
    cus-saved-vars))

;; (emacs-Q "-l" buffer-file-name "--eval" "(pause-start 0.1 nil)")
(defun pause-start (after-minutes cus-file)
  "Start `pause-mode' with interval AFTER-MINUTES.
This bypasses `pause-only-when-server-mode'.

You can use this function to start a separate Emacs process that
handles pause, for example like this if you want a pause every 15
minutes:

  emacs -Q -l pause --eval \"(pause-start 15 nil)\"

Note: Another easier alternative might be to use
      `pause-start-in-new-emacs'."
  (interactive (list
                (string-to-number (read-string "Pause after how many minutes: "))
                t))
  (pause-start-1 after-minutes cus-file))

(defun pause-minimize ()
  "Minimize if possible, otherwise lower frame."
  (unless window-system (error "pause-minimize: window-system=%s" window-system))
  (cond ((fboundp 'w32-showwindow)
         ;; #define SW_MINIMIZE 6
         (w32-showwindow pause-frame 6))
        ((window-system 'w32)
         (select-frame pause-frame)
         (w32-send-sys-command #xf020))
        (t (lower-frame))))

(defun pause-show-no-activate ()
  (cond ((fboundp 'w32-showwindow)
         ;; #define SW_SHOWNOACTIVATE 4
         (w32-showwindow pause-frame 4))
        ((window-system 'w32)
         (select-frame pause-frame)
         (w32-send-sys-command #xf120))
        (t (raise-frame))))

(defun pause-start-1 (after-minutes cus-file)
  (setq pause-in-separate-emacs (or (not cus-file) (stringp cus-file)))
  (pause-cancel-timer)
  (setq pause-after-minutes after-minutes)
  (when pause-in-separate-emacs
    (setq debug-on-error t)
    (setq pause-frame (pause-get-pause-frame))
    (pause-minimize)
    (setq frame-title-format "Emacs Pause")
    (when (and cus-file (file-exists-p cus-file))
      (let ((args (pause-get-group-saved-customizations 'pause cus-file)))
        ;;(message "cus-file=%S" cus-file) (message "args=%S" args)
        (apply 'custom-set-variables args))))
  (let ((pause-only-when-server-mode nil))
    (pause-mode 1))
  (when pause-in-separate-emacs
    (set-frame-parameter pause-frame 'background-color pause-goon-background-color)
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create "Pause information"))
    (setq mode-line-format nil)
    (insert (propertize "Emacs pause\n"
                        'face '(:inherit variable-pitch :height 1.5)))
    (insert (format "Pausing every %d minute.\n" after-minutes))
    (insert "But you can ")
    (insert-text-button "pause now"
                        'mouse-face 'pause-mouse-face
                        'action `(lambda (button)
                                   (condition-case err
                                       (progn
                                         (goto-char (point-min))
                                         (if (pause-use-topmost)
                                             (pause-break-topmost)
                                           (pause-break-no-topmost)))
                                     (error (message "pause-start: %s" (error-message-string err))))))
    ;;(insert "!\n")
    (goto-char (point-min))
    (pause-break-mode)
    (message nil)))

(defun pause-get-pause-frame ()
  (if pause-in-separate-emacs
      (setq pause-frame (selected-frame))
    (unless (and pause-frame
                 (frame-live-p pause-frame))
      (setq pause-frame
            (if window-system
                (make-frame `((visibility . nil)
                              (left-fringe . 0)
                              (right-fringe . 0)
                              (tool-bar-lines . 0)
                              (menu-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (left . 0)
                              (top . 0)
                              (width  . 70)
                              (height . 18)))
              (selected-frame))))))

;; (pause-start-in-new-emacs 0.2)
;; (pause-start-in-new-emacs 0.3)
;; (pause-start-in-new-emacs 0.5)
;; (pause-start-in-new-emacs 1)
;; (pause-start-in-new-emacs 15)
;;;###autoload
(defun pause-start-in-new-emacs (after-minutes)
  "Start pause with interval AFTER-MINUTES in a new Emacs instance.
The new Emacs instance will be started with -Q.  However if
`custom-file' is non-nil it will be loaded so you can still
customize pause.

One way of using this function may be to put in your .emacs
something like

  ;; for just one Emacs running pause
  (when server-mode (pause-start-in-new-emacs 15))

See `pause-start' for more info.

"
  (interactive (list pause-after-minutes))
  (let* ((this-emacs (locate-file invocation-name
                                  (list invocation-directory)
                                  exec-suffixes))
         (cus-file (if custom-file custom-file "~/.emacs"))
         (args `("-l" ,pause-el-file
                 "--geometry=40x3"
                 "-D"
                 "--eval" ,(format "(pause-start %s %S)" after-minutes cus-file))))
    (setq args (cons "-Q" args))
    (apply 'call-process this-emacs nil 0 nil args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Link to yoga poses

;; (defun w3-download-callback (fname)
;;   (let ((coding-system-for-write 'binary))
;;     (goto-char (point-min))
;;     (search-forward "\n\n" nil t)
;;     (write-region (point) (point-max) fname))
;;   (url-mark-buffer-as-dead (current-buffer))
;;   (message "Download of %s complete." (url-view-url t))
;;   (sit-for 3))

;;(run-with-idle-timer 0 nil 'pause-get-yoga-poses)
(defvar pause-yoga-poses-host-url "http://www.abc-of-yoga.com/")

(defgroup pause-yoga nil
  "Customization for yoga exercises."
  :group 'pause)

(defcustom pause-yoga-poses-use-dir nil
  "Set this to always use local poses yoga files.
See `pause-yoga-poses-dir' for more info."
  :type 'boolean
  :group 'pause-yoga)

(defcustom pause-yoga-poses-dir ""
  "Directory with yoga poses files.
This should be files your browser can show and the directory
should only contain those files.  They will be used in case the
connection fails or you have set `pause-yoga-poses-use-dir' on."
  :type 'directory
  :group 'pause-yoga)

(defcustom pause-later-file "~/.emacs-pause-later"
  "File for storing pauses to do later."
  :type 'file
  :group 'pause-yoga)

;;(pause-start-get-yoga-poses)
(defun pause-start-get-yoga-poses ()
  (if (and pause-yoga-poses-use-dir
           (< 0 (length pause-yoga-poses-dir))
           (file-directory-p (substitute-in-file-name pause-yoga-poses-dir)))
      (pause-tell-about-yoga-link (pause-get-pose-from-yoga-poses-dir))
    (require 'url-vars)
    (let ((url-show-status nil)) ;; do not show download messages
      (url-retrieve (concat pause-yoga-poses-host-url "yogapractice/mountain.asp")
                    'pause-callback-get-yoga-poses))))

(defvar pause-collected-yoga-poses nil)

(defun pause-make-file-title (file)
  (let ((tit (file-name-nondirectory (file-name-sans-extension file))))
    (setq tit (replace-regexp-in-string "-" " " tit t t))
    (setq tit (capitalize tit))
    tit))

;;(setq x (pause-get-pose-from-yoga-poses-dir))
(defun pause-get-pose-from-yoga-poses-dir ()
  "Get a random file name from `pause-yoga-poses-dir'."
  (let* ((poses-dir (substitute-in-file-name pause-yoga-poses-dir))
         (files (directory-files poses-dir nil "[^.]$"))
         (num (length files))
         (file (pause-random-yoga-pose files))
         (title (pause-make-file-title file)))
    (cons (expand-file-name file poses-dir) title)))

(defun pause-callback-get-yoga-poses (status)
  ;;(message "pause get-yoga-poses: status=%S" status) (message nil)
  ;; pause-callback-get-yoga-poses: status=(:error (error http 500))
  (when (buffer-live-p pause-buffer)
    (let ((err (plist-get status :error))
          pose)
      (if err
          (progn
            (message "Can't connect to %s: %s" pause-yoga-poses-host-url err)
            (if (< 0 (length pause-yoga-poses-dir))
                (setq pose (pause-get-pose-from-yoga-poses-dir))
              (with-current-buffer pause-buffer
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert "Sorry, no yoga pose available at the moment\n  from ")
                  (insert-text-button
                   pause-yoga-poses-host-url
                   'mouse-face 'pause-mouse-face
                   'action (lambda (button)
                             (condition-case err
                                 (when (pause-check-alpha-on-click)
                                   (browse-url pause-yoga-poses-host-url))
                               (error (message "pause-callback-get-yoga-poses: %s" (error-message-string err))))))
                  (insert (format ": %S" (cdr err)))))))
        (setq pose (pause-random-yoga-pose (pause-get-yoga-poses-1 (current-buffer))))
        (setq pose (cons (concat pause-yoga-poses-host-url (car pose)) (cdr pose))))
      (when pose
        (pause-tell-about-yoga-link pose)))))

(defun pause-remove-1-from-line (pos-on-line)
  (let ((here (point-marker))
        (inhibit-read-only t))
    (goto-char pos-on-line)
    (goto-char (point-at-bol))
    (if (re-search-forward " \\* \\([0-9]+\\)" (point-at-eol) t)
        (let ((beg (match-beginning 1))
              (end (match-end 1))
              (num (1- (string-to-number (match-string 1)))))
          (if (= num 1)
              (delete-region (match-beginning 0) (match-end 0))
            (delete-region beg end)
            (goto-char beg)
            (insert (format "%d" num))))
      (delete-region (1- (point-at-bol)) (point-at-eol))
      )
    (goto-char here)))

(defun pause-tell-about-yoga-link (pose)
  (unless (buffer-live-p pause-buffer) (pause-create-pause-buffer))
  (message nil)
  (with-current-buffer pause-buffer
    (let ((here (point))
          (inhibit-read-only t)
          (pose-url (car pose)))
      (goto-char (point-max))
      (insert "Yoga for you: ")
      (insert-text-button
       (cdr pose)
       'mouse-face 'pause-mouse-face
       'action `(lambda (button)
                  (condition-case err
                      (when (pause-check-alpha-on-click)
                        (pause-cancel-tell-again-timer)
                        (browse-url ,pose-url)
                        (run-with-idle-timer 1 nil 'pause-break-exit-from-button))
                    (error (message "pause-tell-about-yoga-link a: %s, %s"
                                    (error-message-string err)
                                    ,pose-url)))))
      (insert " (")
      (insert-text-button
       "Do it later"
       'mouse-face 'pause-mouse-face
       'action `(lambda (button)
                  ;;(condition-case err
                      (when (pause-check-alpha-on-click)
                        (pause-cancel-tell-again-timer)
                        (pause-add-to-later ',pose)
                        (pause-break-exit-from-button))
                    ;;(error (message "pause-tell-about-yoga-link b: %s" (error-message-string err))))
                      ))
      (insert ")\n")
      (let* ((later-buf (pause-get-later))
             (later (car later-buf))
             (buf   (cdr later-buf)))
        (when later
          (insert "\nYou have said you wanted to do these later:\n")
          ;;(msgtrc "pause-tell-about-yoga-link later=%S" later)
          (let ((prev-pose nil)
                prev-point
                n-pose)
            (dolist (pose (reverse (cons nil (reverse later))))
              (if (equal pose prev-pose)
                  (setq n-pose (1+ n-pose))
                (when prev-pose
                  (unless (= n-pose 1)
                    (insert (format " * %d" n-pose)))
                  (insert " (")
                  (insert-text-button
                   "done"
                   'mouse-face 'pause-mouse-face
                   'action `(lambda (button)
                              (condition-case err
                                  (when (pause-check-alpha-on-click)
                                    (pause-cancel-tell-again-timer)
                                    (pause-remove-from-later ',prev-pose)
                                    (pause-remove-1-from-line ,prev-point)
                                    )
                                (error (message "pause-tell-about-yoga-link c: %s" (error-message-string err))))))
                  (insert ")\n"))
                (when pose
                  (setq n-pose 1)
                  (setq prev-point (point-marker))
                  (insert "  ")
                  (insert-text-button
                   (let ((tit (cdr pose)))
                     (save-match-data
                       (when (string-match "\\(.*\\)\\.[a-z0-9]+$" tit)
                         (setq tit (match-string 1 tit))))
                     tit)
                   'mouse-face 'pause-mouse-face
                   'action `(lambda (button)
                              (condition-case err
                                  (when (pause-check-alpha-on-click)
                                    (pause-cancel-tell-again-timer)
                                    (browse-url ,(car pose))
                                    (pause-remove-from-later ',pose)
                                    (pause-remove-1-from-line ,(1- (point)))
                                    ;;(run-with-idle-timer 1 nil 'pause-break-exit-from-button)
                                    )
                                (error (message "pause-tell-about-yoga-link c: %s, %s"
                                                (error-message-string err)
                                                ,(car pose))))))
                  (setq prev-pose pose))))
            )))))
  (dolist (win (get-buffer-window-list pause-buffer nil t))
    (with-current-buffer pause-buffer
      (message "to point-min in pause-buffer/win from %s/%s" (window-point) (point))
      (goto-char (point-min)))
    (set-window-point win (point-min)))
  (with-current-buffer pause-buffer
    (message "pause-tell-about-yoga-link: after set point-min: %s" (point)))
  (pause-break-message)
  (pause-start-alpha-100-timer 60))

(defun pause-get-later ()
  (let ((buf (find-file-noselect pause-later-file))
        later)
    (with-current-buffer buf
      (widen)
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (eq (char-after) ?\()))
        (forward-comment 1)
        (skip-chars-forward " \t\n"))
      (unless (eobp)
        (setq later (read buf))))
    (cons later buf)))

(defun pause-modify-later (pose add)
  (let* ((later-buf (pause-get-later))
         (later (car later-buf))
         (buf   (cdr later-buf)))
    ;;(msgtrc "pause-modify-later pose=%S add=%s later=%S" pose add later)
    ;;(msgtrc "pause-modify-later 1: (length later)=%d" (length later))
    (setq later
          (if add
              (cons pose later)
            ;; Fix me: just delete one
            ;;(delete pose later)
            (let ((new-later nil)
                  )
              (dolist (p later)
                (if (equal p pose)
                    (setq pose nil)
                  (setq new-later (cons p new-later))))
              new-later)
            ))
    ;;(msgtrc "pause-modify-later 2: (length later)=%d" (length later))
    (setq later (sort later (lambda (a b)
                              (string< (upcase (cdr a)) (upcase (cdr b))))))
    (when buf
      (with-current-buffer buf
        (erase-buffer)
        (pp later buf)
        (basic-save-buffer)
        ;;(kill-buffer buf)
        ))))

(defun pause-add-to-later (pose)
  (pause-modify-later pose t))

(defun pause-remove-from-later (pose)
  (pause-modify-later pose nil))

(defun pause-get-yoga-poses ()
  (require 'url-vars)
  (let* ((url-show-status nil) ;; do not show download messages
         (buf (url-retrieve-synchronously "http://www.abc-of-yoga.com/yogapractice/mountain.asp")))
    (pause-get-yoga-poses-1 buf)))

;; (setq x (url-retrieve-synchronously "http://www.abc-of-yoga.com/yogapractice/mountain.asp"))
;; (setq x (url-retrieve-synchronously "http://www.emacswiki.org/emacs/EmacsFromBazaar"))

;; (defun temp-y ()
;;   (message "before y")
;;   ;;(setq y (url-retrieve-synchronously "http://www.emacswiki.org/emacs/EmacsFromBazaar"))
;;   (setq x (url-retrieve-synchronously "http://www.abc-of-yoga.com/yogapractice/mountain.asp"))
;;   (message "after x")
;;   )
;; (run-with-idle-timer 0 nil 'temp-y)

(defun pause-get-yoga-poses-1 (buf)
  (require 'url)
  (setq url-debug t)
  ;; url-insert-file-contents
  (let* ((first-marker "<p>These are all the Yoga Poses covered in this section:</p>")
         (table-patt "<table\\(?:.\\|\n\\)*?</table>")
         table-beg
         table-end
         (pose-patt "<A HREF=\"\\([^\"]*?\\)\" class=\"LinkBold\">\\([^<]*?\\)</A>")
         poses
         (trouble-msg
          (catch 'trouble
            ;;(switch-to-buffer-other-window buf)
            (with-current-buffer buf
              (goto-char 1)
              (rename-buffer "YOGA" t)
              (unless (search-forward first-marker nil t)
                (throw 'trouble "Can't find marker for the poses on the page"))
              (backward-char 10)
              (unless (re-search-forward table-patt nil t)
                (throw 'trouble "Can't find table with poses on the page"))
              (setq table-beg (match-beginning 0))
              (setq table-end (match-end 0))
              (goto-char table-beg)
              (while (re-search-forward pose-patt table-end t)
                (setq poses (cons (cons (match-string 1) (match-string 2))
                                  poses)))
              (unless poses
                (throw 'trouble "Can't find poses in table on the page"))
              (kill-buffer)
              nil))))
    (if trouble-msg
        (progn
          (message "pause-get-yoga-poses: %s" trouble-msg)
          nil)
      (message "Number of yoga poses found=%s" (length poses))
      poses)))

(defun pause-random-yoga-pose (poses)
  (when poses
    (random t)
    (let* ((n-poses (length poses))
           (pose-num (random (1- n-poses)))
           (the-pose (nth pose-num poses)))
      the-pose)))

;;(pause-random-yoga-pose (pause-get-yoga-poses))

(provide 'pause)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pause.el ends here
