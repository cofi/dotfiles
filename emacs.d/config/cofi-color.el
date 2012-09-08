(setq custom-theme-directory "~/.emacs.d/themes/")

(defvar cofi/current-colorscheme nil)
(defvar cofi/colorschemes '("cofi-dark" "cofi-light"))
(defun cofi/colorscheme (scheme)
  "Move to next colorscheme. If `ARG' is non-nil reload current."
  (interactive (list (completing-read "Colorscheme: " cofi/colorschemes)))
  (let ((custom-safe-themes t)
        (scheme (if (stringp scheme) (intern scheme) scheme)))
    (disable-theme cofi/current-colorscheme)
    (load-theme scheme)
    (setq cofi/current-colorscheme scheme)))

;;; additional faces
(defface mode-line-buffer
  '((t (:bold t :foreground "#FFAA00")))
  ""
  :group 'mode-line-faces)
(defface mode-line-major-mode
  '((t (:bold t :foreground "gold")))
  ""
  :group 'mode-line-faces)
(defface mode-line-minor-mode
  '((t (:foreground "khaki")))
  ""
  :group 'mode-line-faces)

(cofi/colorscheme 'cofi-dark)

(provide 'cofi-color)
