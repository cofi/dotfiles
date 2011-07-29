;;; tipframe.el --- Create and manage tip style frames
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-04-30 Fri
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
;; Just some tests yet.
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

(when t
  (defun tipframe-make-frame (buf)
    (let* ((mini (minibuffer-window))
           (params `((minibuffer . ,mini)
                     (unsplittable . t)
                     (visibility . nil)
                     (toolbar-lines 0)
                     (menu-bar-lines 0)
                     (vertical-scroll-bars . nil)
                     (left-fringe . 0)
                     (right-fringe . 0)
                     (border-width . 1)
                     (internal-border-width . 2)
                     ))
           (frame (make-frame params))
           )
      (unless buf
        (setq buf (generate-new-buffer "tipframe"))
        (with-current-buffer buf
          (insert (buffer-name))
          (setq mode-line-format nil)
          ))
      (set-window-buffer (frame-first-window frame) buf)
      ;; Fix-me: This breaks make-frame-visible, make-frame-invisible
      ;; have to be called first:
      ;;(w32-set-frame-caption frame nil)
      ;;(w32-set-frame-topmost frame t)
      (w32-set-parent-frame frame (selected-frame))
      frame))

  (defun tipframe-temp ()
    ;;(tipframe-make-frame (get-buffer-create "*scratch*"))
    (tipframe-make-frame nil)
    )

  )
(when nil
  ;; http://www.xtremedotnettalk.com/showthread.php?t=82337
  (let* (
         (gwl-style -16)
         (gwl-exstyle -20)
         (style (w32-get-window-long x gwl-style))
         (tts-balloon #x40)
         (tts-noprefix 2)
         (ws-border #x800000)
         )
    (setq style (logxor style ws-border))
    ;;(setq style (logior style tts-balloon))
    (setq style (logior style tts-noprefix))
    (w32-set-window-long x gwl-style style)
    ;;(w32-after-window-long x)
    ;;(w32-set-frame-caption x nil)
    (set-frame-position x 400 100)
    (make-frame-invisible x)
    )
  (setq x (tipframe-temp))
  (delete-frame x)
  (make-frame-invisible x)
  (make-frame-visible x t) ;; <- this means make visible but don't activate
  (make-frame-visible x nil)
  (w32-set-frame-caption x nil)
  (w32-set-frame-caption x t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tipframe.el ends here
