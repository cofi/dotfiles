;;; flymake-for-el.el ---
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
;; Flymake for elisp files.
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

(require 'flymake)

(defcustom flymake-for-el-allowed-file-name-masks '((".+\\.el$"   flymake-for-el-init)
                                                 ("^\\.emacs$" flymake-for-el-init))
  "Filename extensions that switch on elisp syntax checks."
  :type '(repeat (list (regexp :tag "File name regexp")
                       (function :tag "Init function")
                       (choice (const :tag "No cleanup function" nil)
                               (function :tag "Cleanup function"))))
  :group 'flymake-files)

(defun flymake-for-el-init ()
  ;; Fix-me: We should set the allowed error masks here.
  (if (string-match "^ " (buffer-name))
      nil
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list
       (expand-file-name invocation-name invocation-directory)
       (list
        "-Q" "--batch" "--eval"
        (prin1-to-string
         (quote
          (dolist (file command-line-args-left)
            (with-temp-buffer
              (insert-file-contents file)
              (let ((file-name (file-name-nondirectory file)))
                (condition-case err
                    (progn
                      ;; Fix-me: Figure out what shortens the real warning lines!
                      (message (concat "%s:1:1:Messages are truncated in elisp flymake"
                                       " because output of the byte compile is wrapped."
                                       " If you know how to fix it please tell me!")
                               file-name)
                      (byte-compile-file file))
                  (error
                   (message "%s:1:error flymake routine: %S" file
                            (error-message-string err)))))))))
        local-file)))))

(defun flymake-for-el-loader ()
  (dolist (rec flymake-for-el-allowed-file-name-masks)
    (add-to-list 'flymake-allowed-file-name-masks rec)))

(provide 'flymake-for-el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-for-el.el ends here
