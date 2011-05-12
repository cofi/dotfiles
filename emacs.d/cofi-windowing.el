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

(defun smart-split ()
  "Split the frame into 80-column sub-windows, and make sure no window has
   fewer than 80 columns."
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has
     80 columns."
    (if (> (window-width w) (* 2 81))
        (let ((w2 (split-window w 82 t)))
          (smart-split-helper w2))))
  (smart-split-helper nil))

(defun frame/fullscreen-toggle (&optional f)
  "Toggle given or current frame."
  (interactive)
  (set-frame-parameter f 'fullscreen (if (frame-parameter f 'fullscreen)
                                         nil
                                       'fullboth)))

(defun frame/maximize (&optional f)
  "Maximize given or current frame. Needs at least Emacs 23.2"
  (interactive)
  (set-frame-parameter f 'fullscreen 'maximized))

(defun frame/normal ()
  "Return to normal frame size."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-size frame 90 60)
    (set-frame-parameter frame 'fullscreen nil)))

;; Windowing
(defkeymap cofi/window-map
    ;; Splitting
    "s" 'split-window-vertically
    "v" 'split-window-horizontally
    "|" 'split-window-horizontally
    "/" 'smart-split

    ;; Deleting
    "o" 'delete-other-windows
    "1" 'delete-other-windows
    "d" 'delete-window

    ;; Frame sizing
    "m" 'frame/maximize
    "n" 'frame/normal
    "f" 'frame/fullscreen-toggle

    ;; Sizing
    "RET" 'enlarge-window
    "-"   'shrink-window-horizontally
    "+"   'enlarge-window-horizontally
    "S--" 'shrink-window
    "S-+" 'enlarge-window
    "="   'balance-windows

    ;; Moving
    "C-w"     'other-window
    "w"       'other-window
    "h"       'windmove-left
    "j"       'windmove-down
    "k"       'windmove-up
    "l"       'windmove-right
    "<left>"  'windmove-left
    "<down>"  'windmove-down
    "<up>"    'windmove-up
    "<right>" 'windmove-right

    ;; Swapping
    "H"         'swap-with-left
    "J"         'swap-with-down
    "K"         'swap-with-up
    "L"         'swap-with-right
    "S-<left>"  'swap-with-left
    "S-<down>"  'swap-with-down
    "S-<up>"    'swap-with-up
    "S-<right>" 'swap-with-right
    "SPC"       'swap-window

    ;; winner-mode
    "u" 'winner-undo
    "r" 'winner-redo)

(fill-global-keymap
    "C-w"   cofi/window-map
    ;; alternative for buffers were C-w is used
    "C-x w" cofi/window-map)

(setq winner-dont-bind-my-keys t
      winner-boring-buffers '("*anything*"
                              "*anything buffers*"
                              "*anything bookmarks*"
                              "*anything make*"
                              "*anything uni*"
                              "*anything config*"
                              "*Completions*"
                              "*Help*"
                              "*compilation*"))

(require-and-exec 'winner
                  (winner-mode 1))

(provide 'cofi-windowing)
