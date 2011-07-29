;;; ediff-url.el --- Diffing buffer against downloaded url
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Sat Nov 24 2007
;; Version: 0.56
;; Last-Updated: 2010-03-18 Thu
;; URL: http://bazaar.launchpad.net/~nxhtml/nxhtml/main/annotate/head%3A/util/ediff-url.el
;;
;; Features that might be required by this library:
;;
  ;; `mail-prsvr', `mm-util', `timer', `url-parse', `url-util',
  ;; `url-vars'.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file contains a simple function, `ediff-url', to help you
;; update a single file from the web.
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
;; published by the Free Software Foundation; either version 2, or
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

(eval-when-compile (require 'cl))
(eval-when-compile (require 'url-http))
(require 'url-util)

(defvar ediff-url-read-url-history nil)

;;(ediff-url-redir-google-code "http://csharpmode.googlecode.com/svn/trunk/csharp-mode.el" "csharp-mode.el")
;;(ediff-url-redir-google-code "http://code.google.com/p/csharpmode/source/browse/trunk/csharp-mode.el" "csharp-mode.el")
(defun ediff-url-redir-google-code (url file-name)
  "Check if link to Google Code.
If URL is a description page for a file on Google Code try to
figure out where the file might be and suggest to use the raw
file URL instead.

Note that this only works if the file is at the top of the trunk.
However many Google Code elisp projects contains only a single
file so this might work in many cases."
  (let ((code-google-url-re (rx string-start
                                "http://code.google.com/p/"
                                (submatch (+ (any ascii)))
                                "/")))
    (cond ((string-match code-google-url-re url)
           ;; Project description page, so guess the raw page.
           ;; Note: This simple guess only works for file at the top of the trunk.
           (let ((proj (match-string-no-properties 1 url)))
             (concat "http://" proj ".googlecode.com/svn/trunk/" file-name)))
          (t url))))

(defun ediff-url-tell-bad-status (http-status url buffer)
  (when buffer (kill-buffer buffer))
  (setq http-status
        (concat (number-to-string http-status)
                (case http-status
                  (401 " (unauthorized)")
                  (403 " (forbidden)")
                  (404 " (not found)")
                  (408 " (request timeout)")
                  (410 " (gone)")
                  (500 " (internal server error)")
                  (503 " (service unavailable)")
                  (504 " (gateway timeout)")
                  (530 " (user access denied)")
                  )))
  (message "Got status %s for %s" http-status url)
  (throw 'command-level nil))

(defvar ediff-url-known-launchpad-files
  '(("http://launchpad.net/mediawiki-el" "http://bazaar.launchpad.net/~hexmode/mediawiki-el/trunk/files"))
  "Known overview -> file list trans. Missing TLS support workaround.")

(defun ediff-url-redir-launchpad-overview (url file-name)
  "Check if bazaar project page on Launchpad.
If URL is a project page for a file try to get description page
URL instead."
  (declare (special url-http-end-of-headers url-http-response-status))
  (let* ((proj-url-re (rx string-start
                          (or "http:" "https:") "//launchpad.net/"
                          (submatch (+ (not (any "/"))))
                          string-end)))
    (if (or (not (string-match proj-url-re url))
            (string-match "/download/" url))
        url
      (require 'url-http)
      (let* ((proj-name (match-string 1 url))
             (url-show-status nil) ;; just annoying showing status here
             buffer
             (handle nil)
             (http-status nil)
             (file-list-patt
              (concat "<a href=\"http://bazaar.launchpad.net/~\"[^/]+/"
                      proj-name
                      "/trunk/files\" class=\"menu-link-source[^\"]*"))
             file-list-url
             (dl-link-patt (concat "href=\"\\([^\"]*/download/[^\"]*" file-name "\\)\""))
             dl-link)
        ;; Get file list page. Unfortunately this requires TLS
        (if t
            ;; No TLS
            (progn
              (dolist (rec ediff-url-known-launchpad-files)
                (let ((o-url (nth 0 rec))
                      (f-url (nth 1 rec)))
                  (when (string= o-url url)
                    (setq file-list-url f-url))))
              (unless file-list-url
                (error "No TLS support, can't get %S" url)))
          (setq buffer (url-retrieve-synchronously url))
          (unless buffer
            (message "Got empty buffer for %s" url)
            (throw 'command-level nil))
          (when (= 0 (buffer-size buffer))
            (message "Got empty page for %s" url)
            (throw 'command-level nil))
          (setq http-status (url-http-parse-response))
          (when (not (memq http-status '(200 201)))
            (ediff-url-tell-bad-status http-status url buffer))
          (with-current-buffer buffer
            (goto-char (point-min))
            (unless (search-forward "\n\n" nil t)
              (error "Could not find header end in buffer for %s" url))
            (unless (re-search-forward file-list-patt nil t)
              (display-buffer buffer)
              (error "Could not find download link patt=%S" file-list-patt))
            (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (setq file-list-url (match-string 1)))
        ;; Now we have the file list page.
        ;; For this to be useful file-name must be on that page.
        (setq buffer (url-retrieve-synchronously file-list-url))
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (re-search-forward "^\r*$" nil t)
            ;; Saw the end of the headers
            (setq url-http-end-of-headers (set-marker (make-marker) (point))))
          (setq http-status (url-http-parse-response)))
        (when (not (memq http-status '(200 201)))
          (ediff-url-tell-bad-status http-status file-list-url buffer))
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (re-search-forward dl-link-patt nil t)
            (display-buffer buffer)
            (error "Could not find download link on page %S" file-list-url))
          (setq dl-link (concat "http://bazaar.launchpad.net" (match-string-no-properties 1))))
        (kill-buffer buffer)
        dl-link
        ))))

(defun ediff-url-redir-launchpad (url file-name)
  "Check if bazaar list page on Launchpad.
If URL is a description page for a file get download URL
instead."
  (require 'url-http)
  (let* ((bazaar-url "http://bazaar.launchpad.net/")
         (bazaar-len (length bazaar-url)))
    (if (and (< bazaar-len (length url))
             (string= bazaar-url (substring url 0 bazaar-len)))
        (let* ((url-show-status nil) ;; just annoying showing status here
               (buffer (url-retrieve-synchronously url))
               (handle nil)
               (http-status nil)
               ;; Fix-me: better more flexible pattern?
               (dl-patt "<a href=\"\\(.*?\\)\">download file</a>")
               dl-url)
          (unless buffer
            (message "Got empty buffer for %s" url)
            (throw 'command-level nil))
          (with-current-buffer buffer
            (if (= 0 (buffer-size))
                (progn
                  (message "Got empty page for %s" url)
                  (throw 'command-level nil))
              (setq http-status (url-http-parse-response))
              (if (memq http-status '(200 201))
                  (progn
                    (goto-char (point-min))
                    (unless (search-forward "\n\n" nil t)
                      (error "Could not find header end in buffer for %s" url))
                    (unless (re-search-forward dl-patt nil t)
                      (display-buffer buffer)
                      (error "Could not find download link patt=%S" dl-patt))
                    (setq dl-url (match-string 1))
                    (set-buffer-modified-p nil)
                    (kill-buffer buffer)
                    dl-url)
                (kill-buffer buffer)
                (setq buffer nil)
                (setq http-status
                      (concat (number-to-string http-status)
                              (case http-status
                                (401 " (unauthorized)")
                                (403 " (forbidden)")
                                (404 " (not found)")
                                (408 " (request timeout)")
                                (410 " (gone)")
                                (500 " (internal server error)")
                                (503 " (service unavailable)")
                                (504 " (gateway timeout)")
                                (530 " (user access denied)")
                                )))
                (message "Got status %s for %s" http-status url)
                (throw 'command-level nil)))))
      url)))

(defun ediff-url-redir-emacswiki-description-page (url file-name)
  "Check if description page on EmacsWiki.
If URL is a description page for a file uploaded to EmacsWiki
suggest to use the download URL instead."
  ;;(let* ((desc-url "http://www.emacswiki.org/emacs/")
  (let* ((emacswiki-url "http://www.emacswiki.org/")
         (emacswiki-len (length emacswiki-url)))
    (if (and (< emacswiki-len (length url))
             (string= emacswiki-url (substring url 0 emacswiki-len))
             (not (string-match-p "/download/" url)))
        (let ((prompt
               (concat "This seem to be the description page on EmacsWiki,"
                       "\n\tdo you want the download url instead? "))
              (resize-mini-windows (or resize-mini-windows t)))
          (when (y-or-n-p prompt)
            ;;(let ((start (+ 6 (string-match "/wiki/" url))))
            (let ((start (+ 0 (string-match file-name url))))
              (concat (substring url 0 start)
                                "download/"
                                (substring url start)))))
      ;; Not on the wiki, just return the url:
      url)))

(defcustom ediff-url-redirects '(
                                 ediff-url-redir-emacswiki-description-page
                                 ediff-url-redir-launchpad
                                 ediff-url-redir-launchpad-overview ;; https can't be used yet
                                 ediff-url-redir-google-code
                                 )
  "List of functions checking url given to `ediff-url'.
Each function should take an URL as argument and return this URL
or a new URL."
  :type '(repeat function)
  :group 'ediff)

;;;###autoload
(defun ediff-url (url)
  "Compare current buffer to a web URL using `ediff-buffers'.
Check URL using `ediff-url-redirects' before fetching the file.

This is for checking downloaded file.  A the file may have a comment
telling the download URL of thise form in the header:

   ;; URL: http://the-server.net/the-path/the-file.el

If not the user is asked for the URL."
  (interactive (let ((url-init (url-get-url-at-point)))
                 (unless (and url-init
                              (string-match-p "`https?://" url-init))
                   (when (eq major-mode 'emacs-lisp-mode)
                     (save-excursion
                       (goto-char (point-min))
                       (when (re-search-forward "URL:[ \t]*" nil t)
                         (setq url-init (url-get-url-at-point))))))
                 (list (read-from-minibuffer "Url for download file: "
                                             (cons (or url-init "") 1) ;nil
                                             nil nil
                                             'ediff-url-read-url-history
                                             ;;url-init
                                             ))))
  (catch 'command-level ;; Fix-me: remove and let go to top later
    (unless (> (length url) 0)
      (message "No URL given, aborted by user")
      (throw 'command-level nil))
    ;; Check if URL seems reasonable
    (dolist (fun ediff-url-redirects)
      (setq url (funcall fun url (file-name-nondirectory buffer-file-name))))
    ;; Fetch URL and run ediff
    (let* ((url-buf-name (concat "URL=" url))
           (url-buf (get-buffer url-buf-name)))
      (when url-buf
        (unless (y-or-n-p "Use previously downloaded url? ")
          (kill-buffer url-buf)
          (setq url-buf nil)))
      (unless url-buf
        (setq url-buf (get-buffer-create url-buf-name))
        (let ((current-major major-mode))
          (with-current-buffer url-buf
            (url-insert-file-contents url)
            ;; Assume same modes:
            (funcall current-major))))
      (ediff-buffers url-buf (current-buffer)))))

(provide 'ediff-url)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ediff-url.el ends here
