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
  (let ((map (make-sparse-keymap)))
    ;; Splitting
    (define-key map (kbd "s") 'split-window-vertically)
    (define-key map (kbd "v") 'split-window-horizontally)
    (define-key map (kbd "|") 'split-window-horizontally)
    (define-key map (kbd "/") 'smart-split)

    ;; Deleting
    (define-key map (kbd "o") 'delete-other-windows)
    (define-key map (kbd "1") 'delete-other-windows)
    (define-key map (kbd "d") 'delete-window)

    ;; Frame sizing
    (define-key map (kbd "m") 'frame/maximize)
    (define-key map (kbd "n") 'frame/normal)
    (define-key map (kbd "f") 'frame/fullscreen-toggle)

    ;; Sizing
    (define-key map (kbd "RET") 'enlarge-window)
    (define-key map (kbd "-")   'shrink-window-horizontally)
    (define-key map (kbd "+")   'enlarge-window-horizontally)
    (define-key map (kbd "S--") 'shrink-window)
    (define-key map (kbd "S-+") 'enlarge-window)
    (define-key map (kbd "=")   'balance-windows)

    ;; Moving
    (define-key map (kbd "C-w")     'other-window)
    (define-key map (kbd "w")       'other-window)
    (define-key map (kbd "h")       'windmove-left)
    (define-key map (kbd "j")       'windmove-down)
    (define-key map (kbd "k")       'windmove-up)
    (define-key map (kbd "l")       'windmove-right)
    (define-key map (kbd "<left>")  'windmove-left)
    (define-key map (kbd "<down>")  'windmove-down)
    (define-key map (kbd "<up>")    'windmove-up)
    (define-key map (kbd "<right>") 'windmove-right)

    ;; Swapping
    (define-key map (kbd "H")         'swap-with-left)
    (define-key map (kbd "J")         'swap-with-down)
    (define-key map (kbd "K")         'swap-with-up)
    (define-key map (kbd "L")         'swap-with-right)
    (define-key map (kbd "S-<left>")  'swap-with-left)
    (define-key map (kbd "S-<down>")  'swap-with-down)
    (define-key map (kbd "S-<up>")    'swap-with-up)
    (define-key map (kbd "S-<right>") 'swap-with-right)
    (define-key map (kbd "SPC")       'swap-window)

    ;; winner-mode
    (define-key map (kbd "u") 'winner-undo)
    (define-key map (kbd "r") 'winner-redo)

    (global-set-key (kbd "C-w")   map)
    (global-set-key (kbd "C-x w") map) ;; alternative for buffers were C-w is used
    )

(provide 'cofi-windowing)
