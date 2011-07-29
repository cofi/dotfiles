;;; org-odt.el --- ODT export for org-mode
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-11-02 Tue
;; Version: 0.1
;; Last-Updated: x
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
;; Export org-file to ODT. Not ready yet.
;;
;; Builds on an idea in muse-odt.el by Paul Rivier.
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

(defgroup org-odt nil
  "Options controlling odt export in org-mode."
  :tag "Org Export ODT"
  :group 'org-export)

(defcustom org-odt-template-file (expand-file-name "odt/default.odt" data-directory)
  "Default OpenDocument file template."
  :type 'string
  :group 'org-odt)

(defcustom org-odt-temp-dir-name "__TEMP_ORG_ODT__"
  "Name for temporary work directory when building odt file.
It is created where you are working. If the process exits
normally, this directory is deleted."
  :type 'string
  :group 'org-odt)

;; (org-odt-create-file "c:/test/odt/temp4.odt" "c:/test/odt/test-content.xml")
(defun org-odt-create-file (odt-output-file content-xml &optional odt-template-file parts-dir)
  "Write ODT-OUTPUT-FILE with content CONTENT-XML.
ODT-OUTPUT-FILE will be an Open Document Format \(ODT) file with
extension .odt, see url `http://www.documentfoundation.org/'.

The file CONTENT-XML will be copied to the member file
content.xml in the created .odt file.

ODT-TEMPLATE-FILE should be a template .odt file. It defaults to
`org-odt-template-file'.

PARTS-DIR should be nil or a directory tree with additional
things to include in the ODT file.  This should be structured the
same way as the internal of the ODT file.  It can for example
include a subdirectory \"Pictures\" with pictures referenced in
CONTENT-XML.
"
  (when (file-exists-p odt-output-file)
    (error "File %S already exists" odt-output-file))
  (setq odt-template-file (or odt-template-file org-odt-template-file))
  (unless (file-exists-p odt-template-file)
    (error "Can't find file %S" odt-template-file))
  (let* ((old-content-buf (when parts-dir (find-buffer-visiting content-xml)))
         (content-buf (when parts-dir (or old-content-buf (find-file-noselect content-xml))))
         (parts-files (when parts-dir (org-odt-get-files parts-dir)))
         (parts-refs (when content-buf
                       (let (refs
                             (re-ref "xlink:href=\"\\([^\"]+\\)\""))
                         (save-match-data
                           (with-current-buffer content-buf
                             (goto-char (point-min))
                             (while (re-search-forward re-ref nil t)
                               (setq refs (cons (match-string 1) refs)))))
                         refs)))
         (odt-template-name (file-name-nondirectory odt-template-file))
	 (temp-odt-dir (expand-file-name org-odt-temp-dir-name
                                         (file-name-directory odt-output-file)))
         (temp-content-xml (expand-file-name "content.xml" temp-odt-dir))
         (default-directory (file-name-as-directory temp-odt-dir))
         (msg-buffer (get-buffer-create "*Messages*"))
         sts)
    (when (or parts-refs parts-files)
      (let ((refs (sort parts-refs
                        (lambda (a b) (string< a b))))
            (files (sort (mapcar (lambda (f)
                                   (file-relative-name f parts-dir))
                                 parts-files)
                         (lambda (a b) (string< a b)))))
        (while refs
          (unless (equal (car refs) (car files))
            (error "Reference %S doesn't match file %S" (car refs) (car files)))
          (setq refs  (cdr refs))
          (setq files (cdr files)))))
    (when (file-directory-p temp-odt-dir) (delete-directory temp-odt-dir t t))
    (mkdir temp-odt-dir)
    (with-current-buffer msg-buffer (goto-char (point-max)))
    (setq sts (call-process "unzip"    ;; program
                            nil        ;; infile
                            msg-buffer ;; buffer
                            nil        ;; display
                            ;; The rest is arguments to program:
                            odt-template-file
                            "-d" temp-odt-dir))
    (unless (eq 0 sts) (error "Unzip terminated with exit status=%S" sts))
    (dolist (f parts-files)
      (let ((f-rel (file-relative-name f parts-dir))
            (f-to (expand-file-name f-rel temp-odt-dir))
            )
        (copy-file f f-to)))
    (delete-file temp-content-xml)
    (if (not parts-dir)
        (copy-file content-xml temp-content-xml)
      (with-current-buffer content-buf
        (write-file temp-content-xml)
        (unless old-content-buf (kill-buffer))))
    (setq sts (call-process "zip"      ;; program
                            nil        ;; infile
                            msg-buffer ;; buffer
                            nil        ;; display
                            ;; The rest is arguments to program:
                            "-R"
                            odt-output-file
                            "*"))
    (unless (eq 0 sts) (error "Zip terminated with exit status=%S" sts))
    (delete-directory temp-odt-dir t t))
  (message "Created %S" odt-output-file))

;; (org-odt-get-files "C:/test/odt/temp2/Pictures")
;; (org-odt-get-files "C:/test/odt/temp/Pictures")
;; (org-odt-get-files "C:/test/odt/temp/")
(defun org-odt-get-files (root)
  "Return all non-directory files under directory ROOT."
  (let ((files nil)
        (subdirs (directory-files root t)))
    (dolist (f (directory-files root t))
      (unless (file-directory-p f)
        (setq files (cons f files))))
    (dolist (subdir subdirs)
      (when (and (file-directory-p subdir)
                 (not (or (string= "/." (substring subdir -2))
                          (string= "/.." (substring subdir -3)))))
        (setq files (append files (org-odt-get-files subdir) nil))))
    files))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-odt.el ends here
