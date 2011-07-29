;;; flymake-files.el --- Keep track of flymake file type support
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-12-27 Mon
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:
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
;; This library can help loading flymake support libraries. This is
;; based naming conventions for these libraries which matches file
;; extensions for file types that should be supported by flymake.
;;
;; See `flymake-load-support' for requirements on the libraries for
;; this to work.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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

(eval-when-compile (require 'flymake))
(require 'flymakemsg nil t)

(declare-function flymake-mode "flymake")

(defcustom flymake-files-elisp-dirs
  (list
   (file-name-directory
    (or load-file-name
        (when (boundp 'bytecomp-filename) bytecomp-filename)
        buffer-file-name)))
  "List of directories where flymake support elisp files are.
See `flymake-load-support'."
  :type '(repeat directory)
  :group 'flymake)

;; Fix-me: defcustom
(defvar flymake-file-ext-aliases
  '(
    (".htm$" "html")
    ))

;; (flymake-load-support "temp.css")
;; (flymake-load-support "temp.htm")
;; (flymake-load-support "temp.el")
(defun flymake-load-support (file-name)
  "Require available support for file name `file-name'.
Return non-nil on success.

If support is not already loaded then try to load a matching
elisp library.  Such a library has a name of the form
\"flymake-for-EXT.el\".  \"EXT\" corresponds to file name
extensions.

Loading is tried after prepending `flymake-files-elisp-dirs' to
`load-path'.  If it fails also try the aliases in
`flymake-file-ext-aliases'.

For loading of a library to work this must be meat:
 1. The naming convention above.
 3. The library must have a function flymake-for-EXT-loader that
    push the file name mask to `flymake-allowed-file-name-masks'.

Note that the second one above breaks Emacs normal conventions.
\(Though I think that is ok here.)

This function is called by `flymake-global-mode' which turns on
`flymake-mode' in a buffer if support for the buffer file was
found."
  ;; Fix-me: We might need to look in the file/buffer to know more.
  (or (flymake-get-file-name-mode-and-masks file-name)
      (let* ((load-path (append flymake-files-elisp-dirs load-path))
             (ext (file-name-extension file-name))
             (module-name (concat "flymake-for-" ext)))
        ;; Fix-me: call the load function
        (when (or (require (intern module-name) nil t)
                  (progn (message "couldn't require %S" module-name) nil)
                  (catch 'alias
                    (dolist (rec flymake-file-ext-aliases)
                      (let ((regexp (nth 0 rec))
                            (alias  (nth 1 rec)))
                        (when (string-match regexp file-name)
                          (setq module-name (concat "flymake-for-" ext))
                          (when (require (intern module-name) nil t)
                            (throw 'alias t)))))))
          (let ((loader-sym (intern (concat module-name "-loader"))))
            (unless (fboundp loader-sym)
              (error "Loader function %s missing in %s.el" loader-sym module-name))
            (funcall loader-sym))
          t))))

;; (flymake-turn-on-support-for-buffer)
(defun flymake-turn-on-support-for-buffer ()
  "Turn on available flymake support for buffer if any.
Call `flymake-load-support' with `buffer-file-name' to get it."
  (and (not noninteractive)
       buffer-file-name
       ;;(progn (message "ftosfb: %S" buffer-file-name) t)
       (flymake-load-support buffer-file-name)
       ;;(progn (message "  found support") t)
       (add-hook 'post-command-hook 'flymake-turn-on-when-selected nil t)))

(defadvice flymake-mode (before
                         flymakemsg-ad-flymake-mode
                         activate compile)
  "Load support before turning on `flymake-mode'."
  (flymake-turn-on-support-for-buffer))

(defun flymake-global-mode-:set (sym val)
  ":set function for `flymake-global-mode'.
Removes left over turn on functions from `post-command-hook' when
turning off."
  (set-default sym val)
  (unless val
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (remove-hook 'post-command-hook 'flymake-turn-on-when-selected t)))))

;;;###autoload
(define-globalized-minor-mode flymake-global-mode flymake-mode
  flymake-turn-on-support-for-buffer
  :set 'flymake-global-mode-:set
  :group 'flymake)

(defun flymake-turn-on-when-selected ()
  "If buffer is displayed in selected window then turn flymake on.
Also remove this function from buffer local `post-command-hook'
then."
  (with-current-buffer (window-buffer (selected-window))
    (remove-hook 'post-command-hook 'flymake-turn-on-when-selected t)
    (when flymake-global-mode
      (flymake-log 3 "flymake-global-mode turned ON flymake mode")
      (flymake-mode 1))))



(provide 'flymake-files)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-files.el ends here
