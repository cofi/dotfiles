;;; mumamo-cmirr.el --- Chunk mirroring
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-05-16 Sun
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
;; Mirror chunks for indentation etc.
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

(defvar mumamo-cmirr-buffers nil
  "Buffers mirroring chunks.
This is an association list with entries

  \(MAJOR BUF CHANGE)

where MAJOR is the major mode and BUF the buffer.

CHANGE is the first position of unhandled changes for the mirror
buffer.")
(make-variable-buffer-local 'mumamo-cmirr-buffers)
(put 'mumamo-cmirr-buffers 'permanent-local t)

(defvar mumamo-cmirr-back-link nil)
(make-variable-buffer-local 'mumamo-cmirr-back-link)
(put 'mumamo-cmirr-back-link 'permanent-local t)

(defun mumamo-cmirror-kill-mirror-buffers ()
  "Kill all mirror buffers when killing main buffer.
For `kill-buffer-hook' in main buffer."
  (when (and (boundp 'mumamo-cmirr-buffers)
             mumamo-cmirr-buffers)
    (condition-case err
        (progn
          (dolist (rec mumamo-cmirr-buffers)
            (let ((mbuf (nth 1 rec)))
              (kill-buffer mbuf)))
          (kill-local-variable 'mumamo-cmirr-buffers))
      (error (message "mumamo-cmirror-kill-mirror-buffers: %s"
                      (error-message-string err))))))

(defvar mumamo-cmirr-nonlisted-buffers nil
  "If non-nil add a space at front of names of mirror buffers.
This will prevent them from beeing listed by `list-buffers' etc.
See Info node `Buffer Names'.")

;; (setq mumamo-cmirr-buffers nil)
;; (mumamo-cmirr-get-mirror 'text-mode (current-buffer))
;;;###autoload
(defun mumamo-cmirr-get-mirror (major for-buffer)
  "Get the mirror rec."
  (with-current-buffer for-buffer
    (let* ((rec (assoc major mumamo-cmirr-buffers))
           (buf (when rec (nth 1 rec))))
      (when buf
        (unless (buffer-live-p buf)
          (setq mumamo-cmirr-buffers (delete rec mumamo-cmirr-buffers))
          (setq buf nil)))
      (unless buf
        (setq buf (generate-new-buffer
                   (format "%s%s [%s]"
                           (if mumamo-cmirr-nonlisted-buffers " " "")
                           (buffer-name for-buffer)
                           major)))
        (with-current-buffer buf
          (setq buffer-undo-list t)
          (font-lock-mode -1)
          (funcall major)
          (setq indent-tabs-mode (with-current-buffer for-buffer indent-tabs-mode)))
        (let* ((back (list major buf 1))
               (back-link (list for-buffer back)))
          (setq rec back)
          (with-current-buffer buf
            (setq mumamo-cmirr-back-link back))
          (setq mumamo-cmirr-buffers (cons back mumamo-cmirr-buffers)))
        (add-hook 'kill-buffer-hook 'mumamo-cmirror-kill-mirror-buffers nil t)
        ;; after-revert-hook is not permanent-local. Just put it globally, it will not harm.
        (add-hook 'after-revert-hook 'mumamo-cmirror-kill-mirror-buffers)
        (add-hook 'after-change-functions 'mumamo-cmirr-after-change nil t))
      (cdr rec))))

(defvar mumamo-cmirr-no-after-change nil)
(make-variable-buffer-local 'mumamo-cmirr-no-after-change)

(defun mumamo-cmirr-after-change (beg end len)
  ;; No errors here, please.
    (when (listp mumamo-cmirr-buffers)
      (dolist (rec mumamo-cmirr-buffers)
        (when (listp rec)
          (let ((buf (nth 1 rec))
                (val2 (nth 2 rec)))
            (when (buffer-live-p buf)
              (unless (and (boundp 'mumamo-cmirr-no-after-change)
                           (with-current-buffer buf mumamo-cmirr-no-after-change))
                (when (and (integerp val2)
                           (> val2 beg))
                  (setcar (nthcdr 2 rec) beg)))))))))
(put 'mumamo-cmirr-after-change 'permanent-local-hook t)

(provide 'mumamo-cmirr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-cmirr.el ends here
