(setq custom-theme-directory "~/.emacs.d/themes/")

(setq hl-paren-colors '("#F08682" "#E85752" "#E20800" "#BF0303" "#9C0F0F" "#840C0C" "#7A0B0B"))
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

(defface linum-current-line '((t (:bold t :background "#202020" :foreground "yellow")))
  "Face linum uses for the current line"
  :group 'linum)

(cofi/colorscheme 'cofi-dark)

(provide 'cofi-color)
