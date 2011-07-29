;;; rncdl.el --- Fetching rnc schema files for nxml-mode
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-05-09 Sun
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
;;
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

(eval-when-compile (require 'web-vcs))
(require 'nxhtml-base)
(require 'rng-loc)

(defgroup rncdl nil
  "Customization of rncdl."
  :group 'nxhtml)

(defcustom rncdl-dl-root (expand-file-name ".emacs.d/rnc/" "~")
  "Root directory for downloading RNC files."
  :type 'directory
  :group 'rncdl)

(defun rncdl-html5-dir ()
  (expand-file-name "html5" rncdl-dl-root))

;; rng-schema-locating-files

;;(rncdl-update-html5)
;;;###autoload
(defun rncdl-update-html5 ()
  "Update and setup (x)html5 rnc files.
Download rnc files from url `http://syntax.whattf.org/relaxng/'.

Those files are needed to edit (x)html5 with `nxml-mode' support.

The schemas will be enabled based on file name extensions
'.html5' and '.xhtml5'."
  (interactive)
  (catch 'rncdl-stop
    (let* ((url "http://syntax.whattf.org/relaxng/")
           (folder-res (web-vcs-url-retrieve-synch url))
           (status (cdr folder-res))
           (dir-buf (car folder-res))
           (file-re "href=\"\\([^\"]*\.rnc\\)\"")
           files)
      (unless (memq status '(200 201))
        (message "Status=%s. Could not get %S" status url)
        (throw 'rncdl-stop nil))
      ;;(switch-to-buffer-other-window dir-buf)
      (with-current-buffer dir-buf
        (goto-char (point-min))
        (while (re-search-forward file-re nil t)
          (setq files (cons (match-string-no-properties 1) files)))
        ;;(message "files=%s" files)
        (if (not files)
            (progn
              (y-or-n-p
               (format "No RNC files found on %S. Visit page? " url))
              (throw 'rncdl-stop nil))
          (message "Found %d rnc files, starting downloading ..." (length files))
          (let* ((dl-dir (rncdl-html5-dir))
                 (dl-schemas (expand-file-name "html5-schemas.xml" dl-dir)))
            (unless (file-directory-p dl-dir)
              (when (file-exists-p dl-dir)
                (error "Download location %S is not a directory" dl-dir))
              (unless (yes-or-no-p (format "Create download directory %S? " dl-dir))
                (message "Can't continue because download directory does not exist")
                (throw 'rncdl-stop nil))
              (make-directory dl-dir t))
            (unless (member dl-schemas rng-schema-locating-files)
              ;; Fix-me: Add it last
              (when (y-or-n-p "Add the download dir to the directories searched for schemas? ")
                ;;(setq rng-schema-locating-files (nconc rng-schema-locating-files `(,dl-schemas)))
                (let ((value (nconc rng-schema-locating-files `(,dl-schemas))))
                  (customize-set-variable 'rng-schema-locating-files value)
                  (customize-set-value 'rng-schema-locating-files value)
                  (customize-save-variable 'rng-schema-locating-files value))))
            (unless (file-exists-p dl-schemas)
              (copy-file (expand-file-name "etc/schema/html5-schemas.xml" nxhtml-install-dir)
                         dl-schemas))
            (setq files (cons "LICENSE" files))
            (dolist (f files)
              (let* ((file-url (concat url f))
                     (file-name (expand-file-name f dl-dir))
                     (old-name (concat file-name "-old")))
                (when (file-exists-p old-name) (delete-file old-name))
                (when (file-exists-p file-name) (copy-file file-name old-name))
                (web-vcs-url-copy-file file-url file-name t t))))))
      (message "Downloaded %d rnc files for (x)html5" (length files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rncdl.el ends here
