;; -*- lexical-binding: t -*-

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

(setq winner-dont-bind-my-keys t
      winner-boring-buffers '("*helm*"
                              "*helm buffers*"
                              "*helm bookmarks*"
                              "*helm make*"
                              "*helm uni*"
                              "*helm config*"
                              "*Completions*"
                              "*Help*"
                              "*compilation*"))

(require-and-exec 'winner
  (winner-mode 1))

(provide 'cofi-windowing)
