;; -*- lexical-binding: t -*-

(setq windmove-wrap-around t)
(autoload 'windmove-find-other-window "windmove")
(defun swap-window (direction)
  "Swap current window with the one in `direction'."
  (interactive (list (ido-completing-read "Swap with window: "
                                          (mapcar 'symbol-name
                                                  '(left right down up)))))
  (let* ((dir (if (symbolp direction)
                  direction
                (intern direction)))
        (other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window (selected-window))
             (this-buffer (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start (window-start this-window))
             (other-start (window-start other-window)))
        (set-window-buffer this-window other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start this-window other-start)
        (set-window-start other-window this-start)))))

(defun swap-with-left () (interactive) (swap-window 'left))
(defun swap-with-down () (interactive) (swap-window 'down))
(defun swap-with-up () (interactive) (swap-window 'up))
(defun swap-with-right () (interactive) (swap-window 'right))

(let ((min-window-width (* 2 81)))
  (defun cofi/multi-split (horizontal)
    "Split the frame into 80-column sub-windows, and make sure no window has
   fewer than 80 columns."
    (interactive "P")
    (let ((dir (if horizontal
                   'below
                 'right)))
      (do ((w nil (split-window w 81 dir)))
          ((<= (window-width w) min-window-width)))))

  (defun cofi/smart-split ()
    "Split window vertically or horizontally in a smart way."
    (interactive)
    (if (or (< (frame-width) min-window-width)
           (< (window-width) min-window-width))
        (split-window-vertically)
      (split-window-horizontally))))

(defun cofi/window-toggle-dedicate ()
  (interactive)
  (set-window-dedicated-p nil (not (window-dedicated-p)))
  (message "Window is now %s" (if (window-dedicated-p) "dedicated" "undedicated")))

(defun cofi/goto-window ()
  "Select window by buffer name."
  (interactive)
  (let ((window-alist (mapcar (lambda (window) (cons (buffer-name (window-buffer window))
                                                window))
                               (window-list))))
    (select-window (cdr (assoc (completing-read "Window: " window-alist nil t) window-alist)))))

(setq winner-dont-bind-my-keys t
      winner-boring-buffers-rx '("\\*helm")
      winner-boring-buffers '("*Completions*"
                              "*Help*"
                              "*compilation*"))

(require-and-exec 'winner
  (winner-mode 1))

;;; override winner-set with a winner-boring-buffers that accepts RE
(defun winner-set (conf)
  ;; For the format of `conf', see `winner-conf'.
  (let* ((buffers nil)
	 (alive
          ;; Possibly update `winner-point-alist'
	  (cl-loop for buf in (mapcar 'cdr (cdr conf))
                   for pos = (winner-get-point buf nil)
                   if (and pos (not (memq buf buffers)))
                   do (push buf buffers)
                   collect pos)))
    (winner-set-conf (car conf))
    (let (xwins)                        ; to be deleted

      ;; Restore points
      (dolist (win (winner-sorted-window-list))
        (unless (and (pop alive)
                     (setf (window-point win)
                           (winner-get-point (window-buffer win) win))
                     ;; modified start
                     (not (let ((buffer-name (buffer-name (window-buffer win))))
                          (or (find-if (lambda (re)
                                        (string-match-p re buffer-name))
                                      winner-boring-buffers-rx)
                             (member buffer-name winner-boring-buffers)))))
                     ;; modified end
          (push win xwins)))            ; delete this window

      ;; Restore marks
      (save-current-buffer
	(cl-loop for buf in buffers
                 for entry = (cadr (assq buf winner-point-alist))
                 do (progn (set-buffer buf)
                           (set-mark (car entry))
                           (setf (winner-active-region) (cdr entry)))))
      ;; Delete windows, whose buffers are dead or boring.
      ;; Return t if this is still a possible configuration.
      (or (null xwins)
	  (progn
            (mapc 'delete-window (cdr xwins)) ; delete all but one
            (unless (one-window-p t)
              (delete-window (car xwins))
              t))))))

(provide 'cofi-windowing)
