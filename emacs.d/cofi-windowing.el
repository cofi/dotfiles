(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
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

(defun fullscreen-toggle (&optional f)
  "Toggle given or current frame."
  (interactive)
  (set-frame-parameter f 'fullscreen
                       (if (frame-parameter f 'fullscreen)
                           nil
                         'fullboth)))

(defun maximize (&optional f)
  "Maximize given or current frame. Needs at least Emacs 23.2"
  (interactive)
  (set-frame-parameter f 'fullscreen 'maximized))

(global-set-key (kbd "C-S-j") (lambda () (interactive) (swap-with 'down)))
(global-set-key (kbd "C-S-k") (lambda () (interactive) (swap-with 'up)))
(global-set-key (kbd "C-S-h") (lambda () (interactive) (swap-with 'left)))
(global-set-key (kbd "C-S-l") (lambda () (interactive) (swap-with 'right)))

(provide 'cofi-windowing)
