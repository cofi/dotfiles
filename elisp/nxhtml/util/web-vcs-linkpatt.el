;;; web-vcs-linkpatt.el --- Find link pattern
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-05-18 Tue
;; Version:
;; Last-Updated: 2011-03-12 Sat
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `advice', `advice-preload', `assoc', `auth-source', `backquote',
;;   `bytecomp', `cal-menu', `calendar', `comint', `cus-edit',
;;   `cus-face', `cus-load', `cus-start', `easymenu', `eieio',
;;   `font-lock', `gnus-util', `help-fns', `ietf-drums', `json',
;;   `mail-parse', `mail-prsvr', `mm-bodies', `mm-decode',
;;   `mm-encode', `mm-util', `moz', `mozadd', `netrc', `noutline',
;;   `ob', `ob-comint', `ob-emacs-lisp', `ob-eval', `ob-keys',
;;   `ob-lob', `ob-ref', `ob-table', `ob-tangle', `org',
;;   `org-compat', `org-complete', `org-entities', `org-faces',
;;   `org-footnote', `org-list', `org-macs', `org-src', `outline',
;;   `overlay', `password-cache', `pcomplete', `re-builder',
;;   `rfc2045', `rfc2047', `rfc2231', `ring', `rx', `rxx', `subword',
;;   `syntax', `time-date', `timer', `url-parse', `url-util',
;;   `url-vars', `warnings', `web-autoload', `web-vcs', `wid-edit'.
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

(require 'mozadd)
(eval-when-compile (require 'web-vcs))

(defvar web-vcs-hit-ovl nil)
(defvar web-vcs-link-ovl nil)
(defvar web-vcs-label-ovl nil)

(defun web-vcs-linkpatt-kill-overlays ()
  (when (overlayp web-vcs-hit-ovl) (delete-overlay web-vcs-hit-ovl))
  (when (overlayp web-vcs-link-ovl) (delete-overlay web-vcs-link-ovl))
  (when (overlayp web-vcs-label-ovl) (delete-overlay web-vcs-label-ovl))
  (kill-local-variable 'web-vcs-hit-ovl)
  (kill-local-variable 'web-vcs-link-ovl)
  (kill-local-variable 'web-vcs-label-ovl)
  )

(defvar web-vcs-linkpatt-href-re "[^\"]*")
(defvar web-vcs-linkpatt-re nil)

;;(setq isearch-string "make")
;;(add-to-history 'isearch-history
;;(isearch-update-ring "make")
;;(isearch-update-ring (propertize "linkpatt" 'face 'font-lock-warning))
;; (setq x (car search-ring))

(defun web-vcs-linkpatt-make-re ()
  (set (make-local-variable 'web-vcs-linkpatt-re)
       (rx-to-string
        `(and
          "<a" (+ space)
          (* (not (any "<>")))
          word-start
          "href" (* space) "=" (* space)
          "\""
          ;;(submatch ,web-vcs-linkpatt-href-re
          (regexp ,(concat "\\(?1:" web-vcs-linkpatt-href-re "\\)"))
          "\""
          ))))

;; Fix-me: just put this in web-vcs.el with an explanation. Delete
;; this file.
(defun web-vcs-init-linkpatt ()
  "Init regexps for isearch and re-builder for link searching."
  (interactive)
  (let ((re (web-vcs-linkpatt-make-re)))
    ;;(setq isearch-string re)
    (setq regexp-search-ring (cons re regexp-search-ring))
    ;;(setq isearch-regexp t)
    (setq isearch-last-case-fold-search t)
    (setq reb-regexp re)))

;; (web-vcs-linkpatt-make-re)
(defun web-vcs-linkpatt-edit-href-re ()
  "Edit inner pattern of href link.
This will be part of the pattern as

   href=\"INNER\""
  ;; Fix-me: put a property on isearch-string part.
  (interactive)
  (let* ((patt web-vcs-linkpatt-href-re)
         (prompt "Edit inner part of href link: ")
         (extra "")
         hist
         done)
    (add-to-history 'hist patt)
    (while (not done)
      (setq patt (read-string (concat extra prompt) patt 'hist))
      (condition-case err
          (progn
            (string-match-p patt "")
            (setq done t)
            (set (make-local-variable 'web-vcs-linkpatt-href-re) patt))
        (error (setq extra (concat (error-message-string err) " - ")))))))

;;(web-vcs-linkpatt-find "url" "label")
(defun web-vcs-linkpatt-find ()
  "Find next link in the buffer."
  (interactive)
  (require 'ediff)
  (web-vcs-linkpatt-make-re)
  (let (hit-ovl link-ovl label-ovl)
    (if (not (re-search-forward web-vcs-linkpatt-re nil t))
        (progn
          (web-vcs-linkpatt-kill-overlays)
          (message "Can't find link forward"))
      (web-vcs-linkpatt-kill-overlays)
      (setq hit-ovl (make-overlay (match-beginning 0) (match-end 0)))
      (overlay-put hit-ovl 'face 'ediff-current-diff-A)
      (overlay-put hit-ovl 'priority 1000)
      (setq link-ovl (make-overlay (match-beginning 1) (match-end 1)))
      (overlay-put link-ovl 'face 'ediff-fine-diff-A)
      (overlay-put link-ovl 'priority 1001)
      ;; (setq label-ovl (make-overlay (match-beginning 2) (match-end 2)))
      ;; (overlay-put label-ovl 'face 'ediff-fine-diff-A)
      ;; (overlay-put label-ovl 'priority 1001)
      (set (make-local-variable 'web-vcs-hit-ovl) hit-ovl)
      (set (make-local-variable 'web-vcs-link-ovl) link-ovl)
      ;; (set (make-local-variable 'web-vcs-label-ovl) label-ovl)
      )))

(defvar web-vcs-linkpatt-re-str nil)
(defvar web-vcs-linkpatt-re-rx nil)

(defun web-vcs-linkpatt-make ()
  (if (not web-vcs-hit-ovl)
      (progn
        (web-vcs-message-with-face 'font-lock-warning-face "Please find a link first with `web-vcs-linkpatt-find'")
        nil)
    (let ((beg (if (use-region-p) (region-beginning) (overlay-start web-vcs-hit-ovl)))
          (end (if (use-region-p) (region-end) (overlay-end web-vcs-hit-ovl)))
          (link-beg (overlay-start web-vcs-link-ovl))
          (link-end (overlay-end   web-vcs-link-ovl))
          (label-beg (overlay-start web-vcs-label-ovl))
          (label-end (overlay-end   web-vcs-label-ovl))
          beg-str mid-str end-str
          re-str re-rx
          )
      (if (or (> beg link-beg) (< end label-end))
          (message "Region must contain link and label")
        (setq beg-str (buffer-substring-no-properties beg link-beg))
        (setq mid-str (buffer-substring-no-properties link-end label-beg))
        (setq end-str (buffer-substring-no-properties label-end end))
        (set (make-local-variable 'web-vcs-linkpatt-re-str)
             (concat beg-str "\\([^\"]+\\)" mid-str "\\([^<]+\\)" end-str))
        (set (make-local-variable 'web-vcs-linkpatt-re-rx)
             `(rx ,beg-str
                  (submatch (+ (not (any "\""))))
                  ,mid-str
                  (submatch (+ (not (any "<"))))
                  ,end-str))))
    t))

;; (defun web-vcs-linkpatt-copy-re-str ()
;;   (interactive)
;;   (when (web-vcs-linkpatt-make)
;;     (kill-new (format "%S" web-vcs-linkpatt-re-str))))

;; (defun web-vcs-linkpatt-copy-re-rx ()
;;   (interactive)
;;   (when (web-vcs-linkpatt-make)
;;     (kill-new (format "%S" web-vcs-linkpatt-re-rx))))

(defun web-vcs-linkpatt-show ()
  "Show regular expression matching current link.
Both string styel and rx style regexps are shown.  You can
directly copy them from the help buffer to use them in elisp
code.

You can select a region to include more or less in the patterns."
  (interactive)
  (when (web-vcs-linkpatt-make)
    (let ((re-str web-vcs-linkpatt-re-str)
          (re-rx web-vcs-linkpatt-re-rx)
          (now (format-time-string "%Y-%m-%d"))
          (buf (get-buffer-create "*web-vcs-linkpatt*")))
      ;; describe-symbol
      (with-current-buffer buf
        (erase-buffer)
        (emacs-lisp-mode)
        (insert
         ";; Possible link pattern\n"
         ";; for use with `web-vcs-links-regexp':\n"
         "\n"
         ";; String Regexp (" now "):\n"
         (format "%S" re-str) "\n"
         "\n"
         ";; RX Regexp (" now "):\n"
         (pp-to-string re-rx)
         ))
      (display-buffer buf)
      )))

(defvar web-vcs-linkpatt-mode-menu-map
  (let ((map (make-sparse-keymap "Linkpatt menu")))
    ;; (define-key map [linkpatt-copy-re]
    ;;   `(menu-item "Copy String Reg Exp" web-vcs-linkpatt-copy-re-str))
    ;; (define-key map [linkpatt-copy-rx]
    ;;   `(menu-item "Copy RX Reg Exp" web-vcs-linkpatt-copy-re-rx))
    ;; (define-key map [div--1]
    ;;   `(menu-item "--"))
    (define-key map [linkpatt-make]
      `(menu-item "Mozadd Isearch Matches Mode" mozadd-isearch-matches-mode
                :button '(:toggle . mozadd-isearch-matches-mode)))
    (define-key map [div--1]
      `(menu-item "--"))
    (define-key map [linkpatt-make]
      `(menu-item "Show" web-vcs-linkpatt-show))
    (define-key map [linkpatt-edit-href]
      `(menu-item "Edit Href Part of Pattern" web-vcs-linkpatt-edit-href-re))
    (define-key map [linkpatt-find]
      `(menu-item "Find" web-vcs-linkpatt-find))
    map))

(defvar web-vcs-linkpatt-mode-map
  (let ((map (make-sparse-keymap "Linkpatt")))
    (define-key map "\C-c." 'web-vcs-linkpatt-find)
    (define-key map "\C-c?" 'web-vcs-linkpatt-show)
    (define-key map "\C-c%" 'web-vcs-linkpatt-edit-href-re)
    ;; (define-key map "\C-c\"" 'web-vcs-linkpatt-copy-re-str)
    ;; (define-key map "\C-c'"  'web-vcs-linkpatt-copy-re-rx)
    (define-key map [menu-bar web-vcs-linkpatt-mode]
      `(menu-item "Linkpatt" ,web-vcs-linkpatt-mode-menu-map))
    map))

(require 're-builder)
(defun web-vcs-linkpatt-start-re-builder ()
  (interactive)
  (unless (local-variable-p 'reb-regexp)
    (set (make-local-variable 'reb-regexp) "web-vcs"))
  (re-builder)
  )

;;;###autoload
(define-minor-mode web-vcs-linkpatt-mode
  "Minor mode helping finding link patt for web-vcs."
  :lighter " WV-link"
  (if web-vcs-linkpatt-mode
      (if (not (derived-mode-p 'nxml-mode))
          (progn
            (setq web-vcs-linkpatt-mode nil)
            (web-vcs-message-with-face 'font-lock-warning-face "You must use a major mode based on `nxml-mode'")))
    (web-vcs-linkpatt-kill-overlays)))

(defun web-vcs-linkpatt-send-mozilla ()
  "Update Firefox to show where current isearch pattern match."
  (interactive)
  (unless (and (boundp 'mozadd-mirror-mode)
               mozadd-mirror-mode)
    (mozadd-mirror-mode 1))
  (message "Sending to mozilla...")
  (mozadd-update-mozilla 0))

(provide 'web-vcs-linkpatt)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-vcs-linkpatt.el ends here
