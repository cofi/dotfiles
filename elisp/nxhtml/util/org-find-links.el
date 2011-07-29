;;; org-find-links.el --- Find where org links occur
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-12-23 Thu
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

(require 'org)
(require 'url-parse)
(require 'url-util)

;;;###autoload
(defun orgfl-find-links-in-org-buffers (url &optional nlines)
  "Find links to URL in all `org-mode' buffers.
URL should be an absolute org link.

NLINES are lines to show around matches.  For more info see
`occur'."
  (interactive (list nil
                     (when current-prefix-arg
                       (prefix-numeric-value current-prefix-arg))))
  (orgfl-find-links-1 url (or nlines 0) nil nil))

;;;###autoload
(defun orgfl-find-links-in-org-files (url dir recurse)
  "Find links to URL in all .org files.
URL should be an absolute org link.
DIR is the directory to search.
Search subdirs if RECURSE is 'yes, don't if it is 'no."
  (interactive (list nil nil nil))
  (setq dir (or dir (read-directory-name "Directory to search: " nil nil t)))
  (setq recurse (or recurse (if (y-or-n-p "Recurse subdirs? ") 'yes 'no)))
  (orgfl-find-links-1 url nil dir recurse))

(defun orgfl-link-at-point ()
  (let* ((found-at-point (eq 'org-link (get-text-property (point) 'face)))
         (link-rec-at-point (when found-at-point (org-link-at-point)))
         (raw-link (nth 4 link-rec-at-point))
         (raw-title (nth 2 link-rec-at-point))
         (type (nth 0 link-rec-at-point))
         (url-at-point (or (when found-at-point (org-url-at-point))
                           (url-get-url-at-point)))
         (fullfile-at-point (unless url-at-point
                              (when (string= type "file")
                                (let ((raw-file (substring raw-link 5)))
                                  (if (file-name-absolute-p raw-file)
                                      raw-file
                                    (expand-file-name raw-file)))))))
    (list raw-link raw-title type url-at-point fullfile-at-point)))

(defun orgfl-find-links-1 (url nlines dir recurse)
  (let* ((link-at-point (orgfl-link-at-point))
         (raw-link  (nth 0 link-at-point))
         (title     (nth 1 link-at-point))
         (link-type (nth 2 link-at-point))
         (url-link  (nth 3 link-at-point))
         (file-link (nth 4 link-at-point))
         ;; Fix-me: to prompt or not to prompt?
         (trans-link (or url-link file-link
                         (read-file-name "File name or web url: " buffer-file-name)))
         bufs
         regexp)
    (when (and url url-link) (unless (string= url url-link)
                               (error "Uh? url=%S, url-link=%S" url url-link)))
    (setq raw-link (or raw-link trans-link))
    (setq link-type (or link-type
                        (let ((urlobj (url-generic-parse-url trans-link)))
                          (url-type urlobj))))
    (setq regexp (rx-to-string
                  `(and (or
                         ,(if title
                              `(and "[[" ,raw-link "][")
                            `(and word-start ,raw-link word-end))
                         (and "[[" ,link-type ":" ,trans-link "][")
                         (and word-start ,trans-link word-end)))
                  t))
    ;; (setq x regexp)
    (if (not dir)
        (progn
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (derived-mode-p 'org-mode)
                (setq bufs (cons buf bufs)))))
          (occur-1 regexp nlines bufs))
      (setq regexp (replace-regexp-in-string "?:" "" regexp t t))
      (if (eq recurse 'yes)
          (rgrep regexp "*.org" dir)
        (lgrep regexp "*.org" dir)))))

(provide 'org-find-links)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-find-links.el ends here
