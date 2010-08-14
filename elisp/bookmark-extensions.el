;;; bookmark-extensions.el - Extensions to standard library `bookmark.el'.

;; Filename: bookmark-extensions.el

;; Author: Drew Adams, Thierry Volpiatto
;; Maintainer: Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Copyright (C) 2000-2009, Drew Adams, all rights reserved.
;; Copyright (C) 2009 ~ 2010, Thierry Volpiatto, all rights reserved.

;; Created: Fri Sep 15 07:58:41 2000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; X-URL: http://mercurial.intuxication.org/hg/emacs-bookmark-extension/

;; Keywords: bookmarks, placeholders, annotations, search, info, w3m, gnus,
;;           man, woman, firefox, delicious, addressbook.

;; Compatibility: GNU Emacs: >=23.x

;; Features that might be required by this library:

;;   `bookmark', `emacs-w3m', `gnus', `firefox', `bookmark-firefox-handler'
;;   `firefox-handler', `anything-delicious', `addressbook-bookmark'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to standard library `bookmark.el'.
;;
;;    This is a fork of bookmark+.el created by Drew Adams.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Auto Documentation
;;  ==================

;;  [UPDATE ALL EVAL] (autodoc-update-all)

;;  * Commands defined here:
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "bmkext-")
;; `bmkext-version'
;; `bmkext-reload-file'
;; `bmkext-bmenu-sort-by-visit-frequency'
;; `bmkext-bmenu-sort-by-last-time-visited'
;; `bmkext-bmenu-sort-alphabetically'
;; `bmkext-bmenu-search'
;; `bmkext-bmenu-edit-bookmark'
;; `bmkext-bmenu-delete-bookmark'
;; `bmkext-bmenu-quit'
;; `bmkext-bmenu-list-only-file-bookmarks'
;; `bmkext-bmenu-list-only-image-file-bookmarks'
;; `bmkext-bmenu-list-only-non-file-bookmarks'
;; `bmkext-bmenu-list-only-info-bookmarks'
;; `bmkext-bmenu-list-only-w3m-bookmarks'
;; `bmkext-bmenu-list-only-gnus-bookmarks'
;; `bmkext-bmenu-list-only-woman-man-bookmarks'
;; `bmkext-bmenu-list-only-last-org-bookmarks'
;; `bmkext-bmenu-list-only-addressbook-bookmarks'
;; `bmkext-bmenu-show-all-bookmarks'
;; `bmkext-bmenu-mark-all-bookmarks'
;; `bmkext-bmenu-unmark-all-deletion-flags'
;; `bmkext-bmenu-unmark-all-non-deletion-flags'
;; `bmkext-bmenu-unmark-all'
;; `bmkext-bmenu-regexp-mark'
;; `bmkext-bmenu-hide-marked'
;; `bmkext-bmenu-hide-unmarked'
;; `bmkext-bmenu-toggle-marks'
;; `bmkext-addressbook-set-mail-buffer'
;; `bmkext-addressbook-set-mail-buffer-and-cc'
;; `bmkext-addressbook-send-to-marked'
;; `bmkext-export-addressbook'
;; `bmkext-sync-abook-from-file'
;; `bmkext-bmenu-list-only-firefox-bookmarks'
;; `bmkext-bmenu-refresh-delicious'
;; `bmkext-bmenu-delicious'

;;  * Gnus functions redefined here (from Emacs24)
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "^gnus-")
;; `gnus-summary-bookmark-make-record'
;; `gnus-summary-bookmark-jump'

;;  * Man functions included here (from Emacs24)
;; [EVAL] (autodoc-document-lisp-buffer :type 'nested-function :prefix "^Man-")
;; `Man-default-bookmark-title'
;; `Man-bookmark-make-record'
;; `Man-bookmark-jump'

;;  * Woman functions included here (from Emacs24)
;; [EVAL] (autodoc-document-lisp-buffer :type 'nested-function :prefix "^woman")
;; `woman-bookmark-make-record'
;; `woman-bookmark-jump'

;;  * Commands redefined here:(from `bookmark.el')
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "^bookmark-")
;; `bookmark-edit-annotation-mode'
;; `bookmark-send-edited-annotation'
;; `bookmark-bmenu-mark'
;; `bookmark-bmenu-unmark'
;; `bookmark-bmenu-this-window'
;; `bookmark-set'
;; `bookmark-rename'
;; `bookmark-delete'
;; `bookmark-bmenu-list'
;; `bookmark-bmenu-other-window'
;; `bookmark-bmenu-2-window'
;; `bookmark-bmenu-switch-other-window'
;; `bookmark-bmenu-execute-deletions'
;; `bookmark-bmenu-rename'

;;  * User options defined here:
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable)
;; `bmkext-save-new-location-flag'
;; `bmkext-su-or-sudo-regexp'
;; `bmkext-w3m-allow-multi-tabs'
;; `bmkext-bookmark-name-length-max'
;; `bmkext-bmenu-sort-function'
;; `bmkext-search-prompt'
;; `bmkext-search-delay'
;; `bmkext-local-man-name-regexp'
;; `bmkext-w3m-bookmarks-regexp'
;; `bmkext-always-save-w3m-imported'
;; `bmkext-external-browse-url-function'
;; `bmkext-firefox-default-directory'
;; `bmkext-annotation-use-org-mode'
;; `bmkext-org-annotation-directory'
;; `Man-name-local-regexp'

;;  * Faces defined here:
;; [EVAL] (autodoc-document-lisp-buffer :type 'faces)
;; `bmkext-gnus'
;; `bmkext-info'
;; `bmkext-local-directory'
;; `bmkext-local-file'
;; `bmkext-non-file'
;; `bmkext-remote-file'
;; `bmkext-su-or-sudo'
;; `bmkext-w3m'
;; `bmkext-woman'
;; `bmkext-adressbook'

;;  * Non-interactive functions defined here:
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "bmkext-")
;; `bmkext-remove-if'
;; `bmkext-remove-if-not'
;; `bmkext-maybe-save-bookmark'
;; `bmkext-edit-bookmark'
;; `bmkext-increment-visits'
;; `bmkext-add-or-update-time'
;; `bmkext-update-time-and-increment-visits'
;; `bmkext-sort-p-1'
;; `bmkext-bmenu-maybe-sort'
;; `bmkext-bmenu-sort-1'
;; `bmkext-bmenu-goto-bookmark'
;; `bmkext-read-search-input'
;; `bmkext-filtered-alist-by-regexp-only'
;; `bmkext-bmenu-filter-alist-by-regexp'
;; `bmkext-bmenu-cancel-search'
;; `bmkext-bmenu-edit-bookmark1'
;; `bmkext-bmenu-propertize-item'
;; `bmkext-bmenu-unmark-all-1'
;; `bmkext-bmenu-unmark-all-2'
;; `bmkext-count-marked'
;; `bmkext-gnus-bookmark-p'
;; `bmkext-w3m-bookmark-p'
;; `bmkext-info-bookmark-p'
;; `bmkext-woman-bookmark-p'
;; `bmkext-man-bookmark-p'
;; `bmkext-woman-man-bookmark-p'
;; `bmkext-file-bookmark-p'
;; `bmkext-image-bookmark-p'
;; `bmkext-non-file-bookmark-p'
;; `bmkext-remote-file-bookmark-p'
;; `bmkext-local-file-bookmark-p'
;; `bmkext-local-directory-bookmark-p'
;; `bmkext-bookmark-marked-p'
;; `bmkext-bookmark-last-org-p'
;; `bmkext-bookmark-addressbook-p'
;; `bmkext-org-last-stored-alist-only'
;; `bmkext-gnus-alist-only'
;; `bmkext-w3m-alist-only'
;; `bmkext-w3m-alist-only-imported'
;; `bmkext-info-alist-only'
;; `bmkext-woman-alist-only'
;; `bmkext-man-alist-only'
;; `bmkext-woman-man-alist-only'
;; `bmkext-remote-file-alist-only'
;; `bmkext-local-file-alist-only'
;; `bmkext-image-file-alist-only'
;; `bmkext-file-alist-only'
;; `bmkext-non-file-alist-only'
;; `bmkext-addressbook-alist-only'
;; `bmkext-marked-bookmarks-only'
;; `bmkext-non-marked-bookmarks-only'
;; `bmkext-current-list-have-marked-p'
;; `bmkext-get-buffer-name'
;; `bmkext-root-or-sudo-logged-p'
;; `bmkext-make-w3m-record'
;; `bmkext-w3m-set-new-buffer-name'
;; `bmkext-jump-w3m-new-session'
;; `bmkext-jump-w3m-only-one-tab'
;; `bmkext-jump-w3m'
;; `bmkext-jump-url-external'
;; `bmkext-html-bookmarks-to-alist'
;; `bmkext-create-alist-from-html'
;; `bmkext-format-html-bmk'
;; `bmkext-get-firefox-user-init-dir'
;; `bmkext-guess-firefox-bookmark-file'
;; `bmkext-firefox-bookmarks-to-alist'
;; `bmkext-w3m-bookmarks-to-alist'
;; `bmkext-create-alist-from-delicious'
;; `bmkext-bmenu-list-only-delicious-bookmarks'
;; `bmkext-delicious-get-url-value'
;; `bmkext-delicious-delete-sentinel'
;; `bmkext-delicious-refresh-sentinel'

;;  * Non-interactive functions redefined here:(From `bookmark.el')
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "^bookmark-")
;; `bookmark-bmenu-mode'
;; `bookmark-bmenu-check-position'
;; `bookmark-show-annotation'
;; `bookmark-default-annotation-text'
;; `bookmark-bmenu-bookmark'
;; `bookmark-prop-set'
;; `bookmark-get-bookmark'
;; `bookmark-location'
;; `bookmark-make-record-default'
;; `bookmark--jump-via'
;; `bookmark-bmenu-surreptitiously-rebuild-list'
;; `bookmark-bmenu-hide-filenames'

;;  * Internal variables defined here:
;; [EVAL] (autodoc-document-lisp-buffer :type 'internal-variable :prefix "bmkext")
;; `bmkext-jump-display-function'
;; `bmkext-latest-bookmark-alist'
;; `bmkext-bookmark-marked-list'
;; `bmkext-bmenu-before-hide-unmarked-list'
;; `bmkext-bmenu-before-hide-marked-list'
;; `bmkext-bmenu-called-from-inside-flag'
;; `bmkext-bmenu-reverse-sort-p'
;; `bmkext-search-pattern'
;; `bmkext-search-timer'
;; `bmkext-quit-flag'
;; `bmkext-w3m-bookmark-url-regexp'
;; `bmkext-firefox-bookmark-url-regexp'
;; `bmkext-delicious-cache'

;;  ***** NOTE: The following variables defined in `bookmark.el'
;;              have been REDEFINED HERE.
;; [EVAL] (autodoc-document-lisp-buffer :type 'internal-variable :prefix "^bookmark-")
;; `bookmark-make-record-function'
;; `bookmark-alist'

;;  *** END auto-documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Bookmark-Extensions Features
;;  ============================
;;
;;  In addition to the kinds of bookmarks provided by vanilla Emacs:
;;
;;    - You can bookmark from W3m.
;;
;;    - You can bookmark from Gnus, Woman or Man (part of Emacs24 now).
;;
;;    - You can have your firefox bookmarks.
;;
;;    - Full integration of Delicious bookmarks using library `anything-delicious.el'.
;;
;;    - You can bookmark FROM firefox (See |bookmark-firefox-handler.el
;;                                         |firefox-protocol.el
;;                                         |bookmark-extensions-documentation.rst)
;;
;;    - You can use bookmark as an addressbook (See addressbook-bookmark.el)
;;
;;    - In addition to the w3m bookmarks you record here you can import
;;      virtually (don't affect .emacs.bmk) your W3m bookmarks here (the ones from `w3m-bookmark-file').
;;
;;    - Support for marking, unmarking, all, by regexp etc...
;;
;;    - Incremental searching of bookmarks (Part of Emacs23+ now)
;;
;;    - Sorting by Time, Visits, Alphabetically.
;;
;;    - Filters for each kind of bookmarks.
;;
;;  Usage:
;;  =====
;;  Put this library in your `load-path'.
;;  Add this to your init file (~/.emacs) : (require 'bookmark-extensions)
;;
;;  As usual use C-h m from *Bookmark List* buffer (C-x r l) to learn 
;;  new commands that are not in vanilla bookmark.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; See the change log at: http://mercurial.intuxication.org/hg/emacs-bookmark-extension/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:

(require 'bookmark)
(eval-when-compile (require 'cl))
(eval-when-compile (require 'w3m nil t))
(eval-when-compile (require 'w3m-bookmark nil t))

(defconst bmkext-version-number "2.6.49")

(defun bmkext-version ()
  "Show version number of library `bookmark-extensions.el'."
  (interactive)
  (message "Bookmark-extensions, version %s" bmkext-version-number))


;; Quiet the byte-compiler
(defvar w3m-current-url)                ; Defined in `w3m.el'.
(defvar w3m-current-title)              ; Defined in `w3m.el'.
(defvar gnus-article-current)           ; Defined in `gnus-sum.el'.
(defvar tramp-file-name-regexp)         ; Defined in `tramp.el'.
(defvar bookmark-make-record-function)  ; Defined in `bookmark.el'.
(defvar Info-current-node)              ; Defined in `info.el'.
(defvar Info-current-file)              ; Defined in `info.el'.


;;; Keymaps -------------------------------------------------------

;;;###autoload
(define-key ctl-x-map "p" bookmark-map)
;;;###autoload
(define-key bookmark-map "j" 'bookmark-jump)
;;;###autoload
(define-key bookmark-map "o" 'bookmark-jump-other-window)

;; *-bmenu-mode-map

;;;###autoload
(define-key bookmark-bmenu-mode-map "." 'bmkext-bmenu-show-all-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "U") nil)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "U <RET>") 'bmkext-bmenu-unmark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "M-<DEL>") 'bmkext-bmenu-unmark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "U D") 'bmkext-bmenu-unmark-all-deletion-flags)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "U >") 'bmkext-bmenu-unmark-all-non-deletion-flags)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-m" 'bmkext-bmenu-mark-all-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-t" 'bookmark-bmenu-toggle-filenames) ; Was `t' in Vanilla
;;;###autoload
(define-key bookmark-bmenu-mode-map "t" 'bmkext-bmenu-toggle-marks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "q" 'bmkext-bmenu-quit)
;;;###autoload
(define-key bookmark-bmenu-mode-map "E" 'bmkext-bmenu-edit-bookmark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "F" 'bmkext-bmenu-list-only-file-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "G" 'bmkext-bmenu-list-only-gnus-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map ">" 'bmkext-bmenu-hide-unmarked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "<" 'bmkext-bmenu-hide-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "I" 'bmkext-bmenu-list-only-info-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "M" 'bmkext-bmenu-list-only-woman-man-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "B" 'bmkext-bmenu-list-only-non-file-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "O" 'bmkext-bmenu-list-only-last-org-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "C" 'bmkext-bmenu-list-only-addressbook-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-c C-c") 'bmkext-addressbook-set-mail-buffer)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-c f c") 'bmkext-addressbook-set-mail-buffer-and-cc)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-c f a") 'bmkext-addressbook-send-to-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\S-V" 'bmkext-bmenu-sort-by-visit-frequency)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\S-T" 'bmkext-bmenu-sort-by-last-time-visited)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\S-S" 'bmkext-bmenu-sort-alphabetically)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-r" 'bookmark-bmenu-relocate) ; Was `R' in vanilla.
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "M-g") 'bmkext-bmenu-search)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-k") 'bmkext-bmenu-delete-bookmark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "W" 'bmkext-bmenu-list-only-w3m-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "D" 'bmkext-bmenu-delicious)
;;;###autoload
(define-key bookmark-bmenu-mode-map "P" 'bmkext-bmenu-list-only-firefox-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-c I") 'bmkext-bmenu-list-only-image-file-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "%" nil)
;;;###autoload
(define-key bookmark-bmenu-mode-map "%m" 'bmkext-bmenu-regexp-mark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "*" nil)


(defadvice bookmark-bmenu-mode (before bmkext-add-keymap () activate)
  "Extras keys added by bmkext:\\<bookmark-bmenu-mode-map>
\\[bmkext-bmenu-edit-bookmark]\t- Edit bookmark
\\[bmkext-bmenu-list-only-file-bookmarks]\t- List only file and directory \
bookmarks (`C-u' for local only)
\\[bmkext-bmenu-show-all-bookmarks]\t- Show all bookmarks
\\[bookmark-bmenu-toggle-filenames]\t- Toggle filenames
\\[bmkext-bmenu-toggle-marks]\t- Toggle marks
\\[bmkext-bmenu-list-only-non-file-bookmarks]\t- List only non-file bookmarks
\\[bmkext-bmenu-list-only-gnus-bookmarks]\t- List only Gnus bookmarks
\\[bmkext-bmenu-list-only-info-bookmarks]\t- List only Info bookmarks
\\[bmkext-bmenu-list-only-woman-man-bookmarks]\t- List only Woman and Man pages
\\[bmkext-bmenu-list-only-w3m-bookmarks]\t- List only W3M bookmarks (`C-u' show also bookmarks from `w3m-bookmark-file')
\\[bmkext-bmenu-list-only-firefox-bookmarks]\t- List only Firefox bookmarks
\\[bmkext-bmenu-list-only-image-file-bookmarks]\t- List only Image bookmarks
\\[bmkext-bmenu-list-only-last-org-bookmarks]\t- List only last stored org bookmarks
\\[bmkext-bmenu-list-only-addressbook-bookmarks]\t- List only addressbook entries
\\[bmkext-addressbook-set-mail-buffer]\t- Set a mail buffer for this bookmark
\\[bmkext-addressbook-set-mail-buffer-and-cc]\t- Set a mail buffer with a cc field for this bookmark
\\[bmkext-addressbook-send-to-marked]\t- Send mail to all marked addressbook bookmarks
\\[bmkext-bmenu-delicious]\t- List only Delicious bookmarks (`C-u' refresh list from delicious server)
\\[bookmark-bmenu-this-window]\t- If bookmark is an URL C-u jump to external browser
\\[bmkext-bmenu-regexp-mark]\t- Mark bookmarks that match a regexp
\\[bmkext-bmenu-hide-marked]\t- Hide marked bookmarks
\\[bmkext-bmenu-hide-unmarked]\t- Hide unmarked bookmarks
\\[bmkext-bmenu-mark-all-bookmarks]\t- Mark all bookmarks
\\[bmkext-bmenu-search]\t- Incremental search in bookmarks
\\[bmkext-bmenu-unmark-all]\t- Unmark all bookmarks (`C-u' for interactive use)
\\[bmkext-bmenu-unmark-all-non-deletion-flags]\t- Unmark all bookmarks with flag >
\\[bmkext-bmenu-unmark-all-deletion-flags]\t- Unmark all bookmarks with flag D
\\[bmkext-bmenu-sort-by-visit-frequency]\t- Sort by visit frequency (`C-u' to reverse)
\\[bmkext-bmenu-sort-by-last-time-visited]\t- Sort by last time visited (`C-u' to reverse)
\\[bmkext-bmenu-sort-alphabetically]\t- Sort alphabetically (`C-u' to reverse)")


;;; Faces (Customizable) ---------------------------------------------

(defface bmkext-gnus
    '((t (:foreground "magenta")))
  "*Face used for a gnus bookmark."
  :group 'bmkext)

(defface bmkext-info
    '((t (:foreground "green")))
  "*Face used for a bookmarked Info node."
  :group 'bmkext)

(defface bmkext-local-directory
    '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for a bookmarked local directory."
  :group 'bmkext)

(defface bmkext-local-file
    '((t (:foreground "Deepskyblue2")))
  "*Face used for a bookmarked local file (without a region)."
  :group 'bmkext)

(defface bmkext-non-file
    '((t (:foreground "grey")))
  "*Face used for a bookmarked buffer not associated with a file."
  :group 'bmkext)

(defface bmkext-remote-file
    '((t (:foreground "pink")))
  "*Face used for a bookmarked tramp remote file (/ssh:)."
  :group 'bmkext)

(defface bmkext-su-or-sudo
    '((t (:foreground "red")))
  "*Face used for a bookmarked tramp file (/su: or /sudo:)."
  :group 'bmkext)

(defface bmkext-w3m
    '((t (:foreground "yellow")))
  "*Face used for a bookmarked w3m url."
  :group 'bmkext)

(defface bmkext-woman
    '((t (:foreground "Orange4")))
  "*Face used for a bookmarked w3m url."
  :group 'bmkext)

(defface bmkext-adressbook
    '((t (:foreground "DarkOrchid")))
  "*Face used for a bookmarked addressbook entry."
  :group 'bmkext)


;;; User Options (Customizable) --------------------------------------

(defgroup bookmark-ext nil
  "Bookmark enhancements."
  :prefix "bmkext-" :group 'bookmark)

(defcustom bmkext-save-new-location-flag t
  "*Non-nil means save relocated bookmarks.
If nil, then the new bookmark location is visited, but it is not saved
as part of the bookmark definition."
  :type 'boolean :group 'bmkext)

(defcustom bmkext-su-or-sudo-regexp "\\(/su:\\|/sudo:\\)"
  "*Regexp to recognize `su' or `sudo' Tramp bookmarks."
  :type 'regexp :group 'bmkext)

(defcustom bmkext-w3m-allow-multi-tabs t
  "*Non-nil means jump to W3m bookmarks in a new session."
  :type 'boolean :group 'bmkext)

(defcustom bmkext-bookmark-name-length-max 70
  "*Maximum number of characters used to name a bookmark with region."
  :type 'integer :group 'bmkext)

(defcustom bmkext-bmenu-sort-function 'bmkext-visited-more-p
  "*Prefered function to sort bookmarks.
Possible values are:
`bmkext-visited-more-p' - sort by visit frequency
`bmkext-last-time-p' - sort by more recents visits
`bmkext-alpha-more-p' - sort alphabetically."
  :type '(choice (const :tag "None" nil) function) :group 'bmkext)

(defcustom bmkext-search-prompt "Pattern: "
  "*Prompt used for `bmkext-bmenu-search'."
  :type 'string :group 'bmkext)

(defcustom bmkext-search-delay 0.6
  "*Display when searching bookmarks is updated all `bmkext-search-delay' seconds."
  :type 'integer :group 'bmkext)

(defcustom bmkext-local-man-name-regexp "^NOM$"
  "*The translation of the uppercase word NAME in your language.
Used in `bookmark-set' to get the default bookmark name."
  :type 'string :group 'bmkext)

(defcustom bmkext-w3m-bookmarks-regexp ">[^><]+.[^</a>]"
  "*The regexp used to parse `w3m-bookmark-file'."
  :type 'regexp :group 'bmkext)

(defcustom bmkext-always-save-w3m-imported nil
  "*When non--nil always save imported w3m bookmarks.
You maybe will not want to set that to non--nil as you can see
your externals w3m bookmarks at any moment with C-u W without saving to file."
  :type 'boolean :group 'bmkext)

(defcustom bmkext-external-browse-url-function 'browse-url-firefox
  "*Function used to call an external navigator on w3m entries with a prefix arg."
  :type 'function :group 'bmkext)

(defcustom bmkext-firefox-default-directory "~/.mozilla/firefox/"
  "The Mozilla Firefox User default directory.
The default value is for GNU/Linux systems."
  :type 'string :group 'bmkext)

(defcustom bmkext-annotation-use-org-mode t
  "*Whether we use `org-mode' to show/edit annotations."
  :type 'boolean :group 'bmkext)

(defcustom bmkext-org-annotation-directory "~/org/bmk-annotations/"
  "*Directory where bookmark annotations are saved as org files.
You should add this directory to `org-agenda-files' list."
  :type 'string :group 'bmkext)

;;; Internal Variables --------------------------------------------------

(defvar bmkext-jump-display-function nil
  "Function used currently to display a bookmark.")

(defvar bmkext-latest-bookmark-alist ()
  "Content of `bookmark-alist' as last filtered.")

(defconst bmkext-non-file-filename "   - no file -"
  "Name to use for `filename' entry, for non-file bookmarks.")

(defvar bmkext-bookmark-marked-list nil
  "A list that contains all marked bookmarks.")

(defvar bmkext-bmenu-before-hide-unmarked-list nil
  "Store the list like it was before hiding unmarked bookmarks.")

(defvar bmkext-bmenu-before-hide-marked-list nil
  "Store the list like it was before hiding marked bookmarks.")

(defvar bmkext-bmenu-called-from-inside-flag nil
  "Signal `bookmark-bmenu-list' is called from bmenu-list buffer.")

(defvar bmkext-bmenu-reverse-sort-p nil
  "Reverse order of sorting.")

(defvar bmkext-search-pattern ""
  "Store keyboard input for incremental search.")

(defvar bmkext-search-timer nil
  "Timer used for searching")

(defvar bmkext-quit-flag nil
  "Non nil make `bmkext-bmenu-search' quit immediately.
See (info \"(elisp)quittinq\")")

;; Preserve compatibility with bookmark+.el in .emacs.bmk.
(defalias 'bookmarkp-jump-gnus 'bmkext-jump-gnus)
(defalias 'bookmarkp-jump-w3m 'bmkext-jump-w3m)
(defalias 'bookmarkp-jump-woman 'bmkext-jump-woman)
(defalias 'bookmarkp-jump-man 'bmkext-jump-man)


;; REPLACES ORIGINAL DOC STRING in `bookmark.el'.
;;
;; Doc string reflects Bookmark-extensions enhancements.
;;

;; Apparently, we need to add this `defvar' stump, in order to get the documentation
;; to show up using `C-h v'.
(defvar bookmark-alist)

(defun bookmark-bmenu-check-position ()
  "If point is not on a bookmark line, move it to one.
If before the first bookmark line, move it to the first.
If after the last, move it to the last.
Return `bookmark-alist'"
  (cond ((< (count-lines (point-min) (point)) 2)
         (goto-char (point-min))
         (forward-line 2)
         bookmark-alist)
        ((and (bolp) (eobp))
         (beginning-of-line 0)
         bookmark-alist)
        (t
         bookmark-alist)))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Use `switch-to-buffer-other-window' to go back to old-buffer.
;; Use `org-mode' when `bmkext-annotation-use-org-mode'
(defun bookmark-show-annotation (bookmark)
  "Display the annotation for bookmark named BOOKMARK in a buffer,
if an annotation exists."
  (let ((annotation (bookmark-get-annotation bookmark)))
    (when (and annotation (not (string-equal annotation "")))
      (if (file-exists-p annotation)
          (find-file-other-window annotation)
          (save-excursion
            (let ((old-buf (current-buffer)))
              (pop-to-buffer (get-buffer-create "*Bookmark Annotation*") t)
              (delete-region (point-min) (point-max))
              ;(insert (concat "Annotation for bookmark '" bookmark "':\n\n"))
              (insert annotation)
              (goto-char (point-min))
              (when bmkext-annotation-use-org-mode (org-mode))
              (switch-to-buffer-other-window old-buf)))))))


;; Use `org-mode-map' as parent map.
;;
;;;###autoload
(defvar bmkext-edit-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (if bmkext-annotation-use-org-mode
                               org-mode-map text-mode-map))
    (define-key map "\C-c\C-c" 'bookmark-send-edited-annotation)
    (define-key map "\C-c\C-k" 'bmkext-quit-annotation)
    map)
  "Keymap for editing an annotation of a bookmark.")

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Use `org-mode' when `bmkext-annotation-use-org-mode'
;;
;;;###autoload
(defun bookmark-edit-annotation-mode (bookmark)
  "Mode for editing the annotation of bookmark BOOKMARK.
When you have finished composing, type \\[bookmark-send-annotation].

BOOKMARK is a bookmark name (a string) or a bookmark record.

\\{bookmark-edit-annotation-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (when bmkext-annotation-use-org-mode (org-mode))
  (make-local-variable 'bookmark-annotation-name)
  (setq bookmark-annotation-name bookmark)
  (use-local-map bmkext-edit-annotation-mode-map)
  (setq major-mode 'bookmark-edit-annotation-mode
        mode-name "Edit Bookmark Annotation")
  (insert (funcall bookmark-edit-annotation-text-func bookmark))
  (let ((annotation (bookmark-get-annotation bookmark)))
    (unless (and bmkext-annotation-use-org-mode
                 (file-directory-p bmkext-org-annotation-directory))
      (make-directory bmkext-org-annotation-directory 'parents))
    (when (and annotation (not (string-equal annotation "")))
      (if (file-exists-p annotation)
          (insert-file-contents annotation)
          (insert annotation))))
  (run-mode-hooks (if bmkext-annotation-use-org-mode
                      'org-mode-hook 'text-mode-hook)))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Signal we can use `org-mode' style to edit.
;;
(defun bookmark-default-annotation-text (bookmark)
  "Return default annotation text for BOOKMARK (a string, not a record).
The default annotation text is simply some text explaining how to use
annotations."
  (concat "#  Type the annotation for bookmark '" bookmark "' here.\n"
	  "#  All lines which start with a '#' will be deleted.\n"
          (when bmkext-annotation-use-org-mode
            "#  You can edit this buffer in `org-mode' style with heading.\n#  \
Type C-u C-c C-c to force save to org file when done.\n")
          "#  C-c C-c maybe save to org file otherwise as text to `bookmark-alist'.\n"
          "#  C-c C-k to abort.\n#\n"
	  "#  Author: " (user-full-name) " <" (user-login-name) "@"
	  (system-name) ">\n"
	  "#  Date:    " (current-time-string) "\n"))

;;;###autoload
(defun bmkext-quit-annotation ()
  "Abort current bookmark annotation and quit."
  (interactive)
  (when (eq major-mode 'bookmark-edit-annotation-mode)
    (kill-buffer)))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Support saving to org file.
;;
;;;###autoload
(defun bookmark-send-edited-annotation (arg)
  "Use buffer contents as annotation for a bookmark.
Lines beginning with `#' are ignored."
  (interactive "P")
  (if (not (eq major-mode 'bookmark-edit-annotation-mode))
      (error "Not in bookmark-edit-annotation-mode"))
  (goto-char (point-min))
  (while (< (point) (point-max))
    (if (looking-at "^#")
        (bookmark-kill-line t)
      (forward-line 1)))
  ;; Take no chances with text properties.
  (let* ((annotation  (buffer-substring-no-properties (point-min) (point-max)))
         (bookmark    bookmark-annotation-name)
         (old-entry   (bookmark-get-annotation bookmark))
         (org-fn      (expand-file-name (format
                                         "%s.org"
                                         (replace-regexp-in-string " " "_" bookmark))
                                        bmkext-org-annotation-directory))
         (old-entry-p (and old-entry (not (string= old-entry ""))
                           (file-exists-p old-entry)
                           (not (string= (file-name-nondirectory old-entry)
                                         (file-name-nondirectory org-fn))))))
    (if (and bmkext-annotation-use-org-mode
             ;; Maybe file have been renamed since last editing.
             ;; Check if old bmk entry is a filename and exists.
             ;; and is different of new one.
             (or arg (file-exists-p org-fn) old-entry-p))
        ;; Store annotation to org file.
        (with-current-buffer (find-file-noselect org-fn)
          (erase-buffer)
          (insert annotation)
          (save-buffer)
          ;; If old file found delete it.
          (when old-entry-p
            (delete-file old-entry))
          ;; The org filename is stored in bookmark annotation entry.
          ;; If no annotation org file is deleted.
          (if (not (eq (point-min) (point-max)))
              (bookmark-set-annotation bookmark org-fn)
              (delete-file org-fn)
              (message "Your annotation file `%s' have been deleted" org-fn)
              (bookmark-set-annotation bookmark ""))
          (kill-buffer))
        ;; Else store annotation to bookmark-alist as text.
        (bookmark-set-annotation bookmark annotation))
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (bookmark-bmenu-surreptitiously-rebuild-list))
  (kill-buffer (current-buffer)))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Add marked bookmark to `bmkext-bookmark-marked-list',
;; Don't call a second time `bookmark-bmenu-check-position'.
;;
;;;###autoload
(defun bookmark-bmenu-mark ()
  "Mark bookmark on this line to be displayed by \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-select]."
  (interactive)
  (beginning-of-line)
  (when (bookmark-bmenu-check-position)
    (let ((inhibit-read-only t)
          (bmk (bookmark-bmenu-bookmark)))
      (unless (bmkext-bookmark-marked-p bmk)
        (push bmk bmkext-bookmark-marked-list))
      (delete-char 1)
      (insert ?>)
      (forward-line 1))))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Remove marked bookmark of `bmkext-bookmark-marked-list'.
;; Don't call a second time `bookmark-bmenu-check-position'.
;;
;;;###autoload
(defun bookmark-bmenu-unmark (&optional backup)
  "Cancel all requested operations on bookmark on this line and move down.
Optional BACKUP means move up."
  (interactive "P")
  (beginning-of-line)
  (when (bookmark-bmenu-check-position)
    (let ((inhibit-read-only t)
          (bmk (bookmark-bmenu-bookmark)))
      (delete-char 1)
      (insert " ")
      (setq bmkext-bookmark-marked-list
            (remove bmk bmkext-bookmark-marked-list)))
    (forward-line (if backup -1 1))))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Get bookmark name from `bmkext-bookmark-name' property of bookmark.
;; 
(defun bookmark-bmenu-bookmark ()
  "Return the name of the bookmark on this line."
  (when (bookmark-bmenu-check-position)
    (save-excursion
      (forward-line 0) (forward-char 3)
      (get-text-property (point) 'bmkext-bookmark-name))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Fix bug in emacs-23.1.1 with `called-interactively-p'
;; Call *-jump-via that is not interactive to fix that.
;;
(defun bookmark-bmenu-this-window ()
  "Select this line's bookmark in this window."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
    (when (bookmark-bmenu-check-position)
      (bookmark--jump-via bookmark 'switch-to-buffer))))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; If handler provide a default value of `bookmark-current-bookmark'
;; and `bookmark-yank-position' use them.
;; Anyway always reset that to nil.
;;
(defun bookmark-set (&optional name no-overwrite)
  "Set a bookmark named NAME at the current location.
If name is nil, then prompt the user.

With a prefix arg (non-nil NO-OVERWRITE), do not overwrite any
existing bookmark that has the same name as NAME, but instead push the
new bookmark onto the bookmark alist.  The most recently set bookmark
with name NAME is thus the one in effect at any given time, but the
others are still there, should the user decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts (at the bookmark name prompt) the name of the last
bookmark used in the document where the new bookmark is being set;
this helps you use a single bookmark name to track progress through a
large document.  If there is no prior bookmark for this document, then
C-u inserts an appropriate name based on the buffer or file.

Use \\[bookmark-delete] to remove bookmarks (you give it a name and
it removes only the first instance of a bookmark with that name from
the list of bookmarks.)"
  (interactive (list nil current-prefix-arg))
  (unwind-protect
       (let* ((record (bookmark-make-record))
              (default (car record)))

         (bookmark-maybe-load-default-file)
         (unless (and bookmark-yank-point
                      bookmark-current-buffer)
           (setq bookmark-yank-point (point))
           (setq bookmark-current-buffer (current-buffer)))

         (let ((str
                (or name
                    (read-from-minibuffer
                     (format "Set bookmark (%s): " default)
                     nil
                     bookmark-minibuffer-read-name-map
                     nil nil default))))
           (and (string-equal str "") (setq str default))
           (bookmark-store str (cdr record) no-overwrite)

           ;; Ask for an annotation buffer for this bookmark
           (when bookmark-use-annotations
             (bookmark-edit-annotation str))))
    (setq bookmark-yank-point nil)
    (setq bookmark-current-buffer nil)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Avoid using `nconc'
;;
(defun bookmark-prop-set (bookmark prop val)
  "Set the property PROP of BOOKMARK to VAL."
  (let ((bmk   (bookmark-get-bookmark bookmark))
        (cell  (assq prop (bookmark-get-bookmark-record bookmark))))
    (if cell
        (setcdr cell val)
        (if (consp (car (cadr bmk)))      ; Old format: ("name" ((filename . "f")...))
            (setcdr bmk (list (cons (cons prop val) (cadr bmk))))
            (setcdr bmk (cons (cons prop val) (cdr bmk))))))) ; New: ("name" (filename . "f")...)

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; In some cases, if `bookmark-alist' is set locally
;; we have to get the value of bookmark form `bmkext-latest-bookmark-alist'
;;
(defun bookmark-get-bookmark (bookmark &optional noerror)
  "Return the bookmark record corresponding to BOOKMARK.
If BOOKMARK is a string, look for the corresponding bookmark record in
`bookmark-alist'; return it if found, otherwise error.  Else if
BOOKMARK is already a bookmark record, just return it."
  (cond
    ((consp bookmark) bookmark)
    ((stringp bookmark)
     (or (assoc-string bookmark bookmark-alist bookmark-completion-ignore-case)
         (assoc-string bookmark bmkext-latest-bookmark-alist bookmark-completion-ignore-case)
         (unless noerror (error "Invalid bookmark %s" bookmark))))))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Location returned can be a buffer name, instead of a file name.
;;
(defun bookmark-location (bookmark)
  "Return the name of the file or buffer associated with BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-maybe-load-default-file)
  (or (bookmark-get-filename bookmark)
      (bookmark-prop-get bookmark 'location) ; Emacs24
      (bmkext-get-buffer-name bookmark)
      (bookmark-prop-get bookmark 'buffer)
      (error "Bookmark has no file or buffer name: %S" bookmark)))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Set timestamp and visit
;; Use also extra args
;;
(defun bookmark-make-record-default (&optional no-file no-context posn visit-number)
  "Return the record describing the location of a new bookmark.
Point should be at the buffer in which the bookmark is being set,
and normally should be at the position where the bookmark is desired,
but see the optional arguments for other possibilities.

If NO-FILE is non-nil, then only return the subset of the
record that pertains to the location within the buffer, leaving off
the part that records the filename.

If NO-CONTEXT is non-nil, do not include the front- and rear-context
strings in the record -- the position is enough.

If POSN is non-nil, record POSN as the point instead of `(point)'."
  (let ((ctime   (float-time))
        (nvisits (or visit-number 0)))
    `(,@(unless no-file `((filename . ,(bookmark-buffer-file-name))))
        ,@(unless no-context `((front-context-string
                                . ,(if (>= (- (point-max) (point))
                                           bookmark-search-size)
                                       (buffer-substring-no-properties
                                        (point)
                                        (+ (point) bookmark-search-size))
                                       nil))))
        ,@(unless no-context `((rear-context-string
                                . ,(if (>= (- (point) (point-min))
                                           bookmark-search-size)
                                       (buffer-substring-no-properties
                                        (point)
                                        (- (point) bookmark-search-size))
                                       nil))))
        (visits . ,nvisits)
        (time . ,ctime)
        (position . ,(or posn (point))))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Update time and visit when jump
;;
(defun bookmark--jump-via (bookmark display-function)
  "Handle BOOKMARK, then call DISPLAY-FUNCTION with current buffer as argument.
Bookmark may be a bookmark name (a string) or a bookmark record.

After calling DISPLAY-FUNCTION, set window point to the point specified
by BOOKMARK, if necessary, run `bookmark-after-jump-hook', and then show
any annotations for this bookmark."
  (bmkext-update-time-and-increment-visits bookmark 'batch)
  (bookmark-handle-bookmark bookmark)
  (save-current-buffer
    (funcall display-function (current-buffer)))
  (let ((win (get-buffer-window (current-buffer) 0)))
    (when win (set-window-point win (point))))
  ;; FIXME: we used to only run bookmark-after-jump-hook in
  ;; `bookmark-jump' itself, but in none of the other commands.
  (run-hooks 'bookmark-after-jump-hook)
  (when bookmark-automatically-show-annotations
      ;; if there is an annotation for this bookmark,
      ;; show it in a buffer.
      (bookmark-show-annotation bookmark)))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Added BATCH arg.
;;
;;;###autoload
(defun bookmark-rename (old &optional new batch)
  "Change bookmark's name from OLD to NEW.
Interactively:
 If called from the keyboard, then prompt for OLD.
 If called from the menubar, select OLD from a menu.
If NEW is nil, then prompt for its string value.

If BATCH is non-nil, then do not rebuild the menu list.

While the user enters the new name, repeated `C-w' inserts consecutive
words from the buffer into the new bookmark name."
  (interactive (list (bookmark-completing-read "Old bookmark name")))
  (bookmark-maybe-historicize-string old)
  (bookmark-maybe-load-default-file)
  (save-excursion (skip-chars-forward " ") (setq bookmark-yank-point (point)))
  (setq bookmark-current-buffer (current-buffer))
  (let ((newname  (or new  (read-from-minibuffer
                            "New name: " nil
                            (let ((now-map  (copy-keymap minibuffer-local-map)))
                              (define-key now-map  "\C-w" 'bookmark-yank-word)
                              now-map)
                            nil 'bookmark-history))))
    (bookmark-set-name old newname)
    (setq bookmark-current-bookmark  newname)
    (unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
    (bmkext-maybe-save-bookmark) newname))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Change arg name: BOOKMARK -> BOOKMARK-NAME.
;; Increment `bookmark-alist-modification-count' even when using `batch' arg.
;;
;;;###autoload
(defun bookmark-delete (bookmark-name &optional batch)
  "Delete the bookmark named BOOKMARK-NAME from the bookmark list.
Removes only the first instance of a bookmark with that name.
If there are other bookmarks with the same name, they are not deleted.
Defaults to the \"current\" bookmark (that is, the one most recently
used in this file), if it exists.  Optional second arg BATCH means do
not update the bookmark list buffer (probably because we were called
from there)."
  (interactive
   (list (bookmark-completing-read "Delete bookmark"
				   bookmark-current-bookmark)))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let ((will-go (bookmark-get-bookmark bookmark-name 'noerror))
        (annot   (bookmark-get-annotation bookmark-name)))
    (if (or (string= (cdr (assoc 'origin will-go)) "firefox-imported")
            (string= (cdr (assoc 'origin will-go)) "delicious-imported")
            (string= (cdr (assoc 'origin will-go)) "w3m-imported"))
        (error "Operation not supported on this type of bookmark.")
        (setq bookmark-alist (delete will-go bookmark-alist))
        (when (and annot (not (string= annot ""))
                   (file-exists-p annot)
                   (y-or-n-p "Delete also Org Annotations file? "))
          (delete-file annot) (message "`%s' have been deleted." annot))
        ;; Added by db, nil bookmark-current-bookmark if the last
        ;; occurrence has been deleted
        (setq bmkext-latest-bookmark-alist (delete will-go bmkext-latest-bookmark-alist))
        (or (bookmark-get-bookmark bookmark-current-bookmark 'noerror)
            (setq bookmark-current-bookmark nil))
        ;; Don't rebuild the list when using `batch' arg
        (unless batch
          (bookmark-bmenu-surreptitiously-rebuild-list))
        (bmkext-maybe-save-bookmark))))


;;; Menu List Replacements (`bookmark-bmenu-*') ----------------------

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Rebuild `bookmark-alist' using the last filtered alist in use.
;; 2. Update the menu-list title.
;;
(defun bookmark-bmenu-surreptitiously-rebuild-list ()
  "Rebuild the Bookmark List if it exists.
Don't affect the buffer ring order."
  (when (get-buffer "*Bookmark List*")
    (save-excursion
      (save-window-excursion
        (let ((bookmark-alist  bmkext-latest-bookmark-alist)
              (title           (with-current-buffer "*Bookmark List*"
                                 (goto-char (point-min))
                                 (buffer-substring (line-beginning-position)
                                                   (line-end-position)))))
          (bookmark-bmenu-list title 'filteredp))))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arguments TITLE and FILTER-ON.
;; 2. Handles also region bookmarks and buffer (non-file) bookmarks.
;;
;;;###autoload
(defsubst bookmark-bmenu-list (&optional title filteredp)
  "Display a list of existing bookmarks, in buffer `*Bookmark List*'.
The following faces are used for the list entries.
Use `customize-face' if you want to change the appearance.

  `bmkext-local-directory', `bmkext-local-file-without-region',
  `bmkext-local-file-with-region', `bmkext-gnus',
  `bmkext-info', `bmkext-non-file', `bmkext-remote-file',
  `bmkext-su-or-sudo', `bmkext-w3m'.

The optional args are for non-interactive use.
TITLE is a string to be used as the title.
 The default title is `% Bookmark'.
The leftmost column of a bookmark entry shows `D' if the bookmark is
 flagged for deletion, or `>' if it is marked for displaying.

Non-nil FILTEREDP indicates that `bookmark-alist' has been filtered
\(e.g gnus, w3m, info, files, or regions).  In that case,
`bmkext-latest-bookmark-alist' is not reset to `bookmark-alist'."
  (interactive)
  (bookmark-maybe-load-default-file)
  (unless bmkext-bmenu-called-from-inside-flag (setq bmkext-bookmark-marked-list nil))
  (unless filteredp (setq bmkext-latest-bookmark-alist bookmark-alist))
  (if (interactive-p)
      (switch-to-buffer (get-buffer-create "*Bookmark List*"))
      (set-buffer (get-buffer-create "*Bookmark List*")))
  (let* ((inhibit-read-only  t)
         (alternate-title    (if title title "% Bookmark"))
         (len-alt-title      (- (length alternate-title) 2)))
    (erase-buffer)
    (insert (format "%s\n- %s\n" alternate-title (make-string len-alt-title ?-)))
    (add-text-properties (point-min) (point) '(font-lock-face bookmark-menu-heading))
    (loop
       with sorted-alist = (bmkext-bmenu-maybe-sort)
       for full-record in sorted-alist
       ;; If a bookmark has an annotation, prepend a "*" in the list of bookmarks.  
       for name        = (bookmark-name-from-full-record full-record)
       for annotation  = (bookmark-get-annotation full-record)
       for marked      = (bmkext-bookmark-marked-p full-record)
       for start       = (+ 2 (point))
       for end         = 0
       do
         (progn
           (insert (cond ((and annotation (not (string-equal annotation "")) marked) ">*")
                         ((and annotation (not (string-equal annotation "")))  " *")
                         (marked "> ")
                         (t "  "))
                   name)
           (setq end (point))
           (bmkext-bmenu-propertize-item name start end)
           (insert "\n")))
    (goto-char (point-min))
    (forward-line 2)
    (bookmark-bmenu-mode)
    (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames t))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Add text properties when hiding filenames.
;;
(defun bookmark-bmenu-hide-filenames (&optional force)
  "Hide filename visibility in bookmark-list buffer."
  (when (and (not force)  bookmark-bmenu-toggle-filenames)
    ;; nothing to hide if above is nil
    (save-excursion
      (save-window-excursion
        (goto-char (point-min))
        (forward-line 2)
        (setq bookmark-bmenu-hidden-bookmarks  (nreverse bookmark-bmenu-hidden-bookmarks))
        (save-excursion
          (let ((inhibit-read-only t)
                (column 2))
            (while bookmark-bmenu-hidden-bookmarks
              (move-to-column column t)
              (bookmark-kill-line)
              (let ((name  (car bookmark-bmenu-hidden-bookmarks))
                    (start (point))
                    end)  
                (insert name) (setq end (point))
                (bmkext-bmenu-propertize-item name start end))
              (setq bookmark-bmenu-hidden-bookmarks  (cdr bookmark-bmenu-hidden-bookmarks))
              (forward-line 1))))))))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Use `pop-to-buffer', not `switch-to-buffer-other-window'.
;; Don't let--bind `bookmark-automatically-show-annotations'
;; in next 3 functions.
(defun bookmark-bmenu-other-window ()
  "Select this line's bookmark in other window, leaving bookmark menu visible."
  (interactive)
  (let ((bookmark  (bookmark-bmenu-bookmark)))
    (when (bookmark-bmenu-check-position)
      (bookmark--jump-via bookmark 'pop-to-buffer))))

(defun bookmark-bmenu-2-window ()
  "Select this line's bookmark, with previous buffer in second window."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark))
        (menu (current-buffer))
        (pop-up-windows t))
    (delete-other-windows)
    (switch-to-buffer (other-buffer))
    (bookmark--jump-via bmrk 'pop-to-buffer)
    (bury-buffer menu)))

(defun bookmark-bmenu-switch-other-window ()
  "Make the other window select this line's bookmark.
The current window remains selected."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark))
        (pop-up-windows t)
        same-window-buffer-names
        same-window-regexps)
    (bookmark--jump-via bookmark 'display-buffer)))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Use  `bookmark-bmenu-surreptitiously-rebuild-list', instead of using
;; `bookmark-bmenu-list', updating the modification count, and saving.
;; 2. Update `bmkext-latest-bookmark-alist' to reflect deletion.
;;
;;;###autoload
(defun bookmark-bmenu-execute-deletions (&optional markedp)
  "Delete bookmarks marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-delete] commands."
  (interactive "P")
  (if (or (not markedp) (yes-or-no-p "Delete bookmarks marked `>' (not `D') "))
      (let* ((o-point    (point))
             (which-mark (if markedp "^>" "^D"))
             (o-str      (unless (looking-at which-mark) (bookmark-bmenu-bookmark)))
             (count      0))
        (message "Deleting bookmarks...")
        (goto-char (point-min))
        (forward-line 2)
        (while (re-search-forward which-mark (point-max) t)
          (let ((bmk (bookmark-bmenu-bookmark))) 
            (bookmark-delete bmk 'batch) ; pass BATCH arg
            (setq bmkext-latest-bookmark-alist
                  (delete (assoc bmk bmkext-latest-bookmark-alist)
                          bmkext-latest-bookmark-alist))
            (setq count (1+ count))))
        (if (> count 0)
            (progn
              (setq bmkext-bmenu-called-from-inside-flag t)
              (bookmark-bmenu-surreptitiously-rebuild-list)
              (message "Deleting %s bookmarks...done" count))
            (message "Nothing to delete here"))
        (if o-str
            (bmkext-bmenu-goto-bookmark o-str)
            (goto-char o-point) (beginning-of-line)))
      (message "OK, nothing deleted")))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Don't call `bookmark-bmenu-list' (it was already called).
;;
;;;###autoload
(defun bookmark-bmenu-rename ()
  "Rename bookmark on current line.  Prompts for a new name."
  (interactive)
  (when (bookmark-bmenu-check-position)
    (let* ((bmk       (bookmark-bmenu-bookmark))
           (new-name  (bookmark-rename bmk)))
      (when (or (search-forward new-name (point-max) t)
                (search-backward new-name (point-min) t))
        (beginning-of-line)))))


;;; Bmkext Functions (`bmkext-*') ------------------------------

;; Don't require cl at runtime.
(defun bmkext-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (loop for i in xs unless (funcall pred i) collect i into result
     finally return result))

(defun bmkext-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (loop for i in xs when (funcall pred i) collect i into result
     finally return result))

(defun bmkext-reload-file ()
  "Reset `bookmark-alist' from `bookmark-default-file'."
  (interactive)
  (setq bookmark-alist nil)
  (bookmark-load bookmark-default-file))

(defun bmkext-maybe-save-bookmark ()
  "Increment save counter and maybe save `bookmark-alist'."
  (setq bookmark-alist-modification-count (1+ bookmark-alist-modification-count))
  (when (bookmark-time-to-save-p) (bookmark-save)))

(defun bmkext-edit-bookmark (bookmark-name)
  "Edit bookmark's name and file name, and maybe save them.
BOOKMARK-NAME is the current (old) name of the bookmark to be renamed."
  (let* ((bookmark-fname (bookmark-get-filename bookmark-name))
         (bookmark-loc   (bookmark-prop-get bookmark-name 'location))
         (new-name       (read-from-minibuffer "Name: " bookmark-name))
         (new-loc        (read-from-minibuffer "FileName or Location: "
                                               (or bookmark-fname bookmark-loc))))
    (when (and (not (equal new-name "")) (not (equal new-loc ""))
               (y-or-n-p "Save changes? "))
      (if bookmark-fname
          (progn
            (bookmark-rename bookmark-name new-name 'batch)
            (bookmark-set-filename new-name new-loc))
          (bookmark-prop-set
           (bookmark-get-bookmark bookmark-name) 'location new-loc)
          (bookmark-rename bookmark-name new-name 'batch))
      (bmkext-maybe-save-bookmark)
      (list new-name new-loc))))

(defun bmkext-increment-visits (bmk)
  "Increment visits entry of bmk.
If bmk have no visits entry, add one with value 0."
  (let ((cur-val (bookmark-prop-get bmk 'visits)))
    (if cur-val
        (bookmark-prop-set bmk 'visits (1+ cur-val))
        (bookmark-prop-set bmk 'visits 0))))

(defun bmkext-add-or-update-time (bmk)
  "Update time entry of bmk.
If bmk have no time entry, add one with current time."
  (let ((time (float-time)))
    (bookmark-prop-set bmk 'time time)))

(defun bmkext-update-time-and-increment-visits (bmk &optional batch)
  "Update time and increment visits entry of BMK.
Unless batch arg is non--nil update display and increment save counter."
  (bmkext-increment-visits bmk)
  (bmkext-add-or-update-time bmk)
  (unless batch
    (setq bmkext-bmenu-called-from-inside-flag t)
    (bookmark-bmenu-surreptitiously-rebuild-list))
  (bmkext-maybe-save-bookmark))

;;; Sorting bookmarks
(defun bmkext-sort-p-1 (s1 s2)
  "General predicate for sorting bookmarks.
Return non-nil if bookmark S1 was visited more often than S2.
Also: S1 < S2 if S1 was visited but S2 was not.
      S1 < S2 if S1 precedes S2 alphabetically and
      neither was visited or both were visited equally."
  (let* ((sym (case bmkext-bmenu-sort-function
                ('bmkext-visited-more-p   'visits)
                ('bmkext-last-time-more-p 'time)
                (t nil)))
         (v1  (when sym (cdr (assq sym s1))))
         (v2  (when sym (cdr (assq sym s2)))))
    (cond ((and v1 v2)
           (or (> v1 v2)
               (and (= v1 v2) (string-lessp (car s1) (car s2)))))
          (v1 t)   ; Only s1 visited
          (v2 nil) ; Only s2 visited
          (t (string-lessp (car s1) (car s2))))))

;; Predicate for sorting bookmarks with visits entry.
(defalias 'bmkext-visited-more-p 'bmkext-sort-p-1)

;; Predicate for sorting bookmarks with time entry.
(defalias 'bmkext-last-time-more-p 'bmkext-sort-p-1)

;; Predicate for sorting bookmarks alphabetically.
(defalias 'bmkext-alpha-more-p 'bmkext-sort-p-1)

;; Menu-List Functions (`bmkext-bmenu-*') -------------------------

(defun bmkext-bmenu-maybe-sort (&optional alist)
  "Sort or reverse-sort using `bmkext-bmenu-sort-function'.
        Sort LIST using `bmkext-bmenu-sort-function'.
        Reverse the result if `bmkext-reverse-sort-p' is non-nil.
        Do nothing if `bmkext-bmenu-sort-function' is nil."
  (let ((bmk-alist (or alist (copy-sequence bookmark-alist))))
    (when bmk-alist
      (if bmkext-bmenu-sort-function
          (sort
           bmk-alist
           (if bmkext-bmenu-reverse-sort-p
               (lambda (a b)
                 (not (funcall bmkext-bmenu-sort-function a b)))
               bmkext-bmenu-sort-function))
          bmk-alist))))

(defun bmkext-bmenu-sort-1 (method &optional batch)
  "Set `bmkext-bmenu-sort-function' to `method' and rebuild alist.
Try to follow position of last bookmark in menu-list."
  (with-current-buffer "*Bookmark List*"
    (let ((bmk (when (bookmark-bmenu-check-position) (bookmark-bmenu-bookmark)))
          (bmkext-bmenu-called-from-inside-flag t))
      (setq bmkext-bmenu-sort-function method)
      (case method
        ('bmkext-visited-more-p
         (if bmkext-bmenu-reverse-sort-p
             (message "Sorting by visit frequency [REVERSED]")
             (message "Sorting by visit frequency")))
        ('bmkext-last-time-more-p
         (if bmkext-bmenu-reverse-sort-p
             (message "Sorting by last time visited [REVERSED]")
             (message "Sorting by last time visited")))
        ('bmkext-alpha-more-p (if bmkext-bmenu-reverse-sort-p
                                  (message "Sorting alphabetically [REVERSED]")
                                  (message "Sorting alphabetically"))))
      (unless batch
        (bookmark-bmenu-surreptitiously-rebuild-list)
        (bmkext-bmenu-goto-bookmark bmk)))))

(defun bmkext-bmenu-goto-bookmark (name)
  "Move point to bookmark whith name NAME."
  (goto-char (point-min))
  (bookmark-bmenu-check-position)
  (while (not (equal name (bookmark-bmenu-bookmark)))
    (forward-line 1))
  (forward-line 0))

;;;###autoload
(defun bmkext-bmenu-sort-by-visit-frequency (&optional reversep)
  (interactive "P")
  (let ((bmkext-bmenu-reverse-sort-p reversep))
    (bmkext-bmenu-sort-1 'bmkext-visited-more-p)))

;;;###autoload
(defun bmkext-bmenu-sort-by-last-time-visited (&optional reversep)
  (interactive "P")
  (let ((bmkext-bmenu-reverse-sort-p reversep))
    (bmkext-bmenu-sort-1 'bmkext-last-time-more-p)))

;;;###autoload
(defun bmkext-bmenu-sort-alphabetically (&optional reversep)
  (interactive "P")
  (let ((bmkext-bmenu-reverse-sort-p reversep))
    (bmkext-bmenu-sort-1 'bmkext-alpha-more-p)))

;;; Searching in bookmarks
;;
;;  Narrow down `bookmark-alist' with only bookmarks matching regexp.
;;  Display is updated at each time a character is entered in minibuffer.
;;
(defun bmkext-read-search-input ()
  "Read each keyboard input and add it to `bmkext-search-pattern'."
  (setq bmkext-search-pattern "")    ; Always reset pattern to empty string
  (let ((tmp-list     ())
        (prompt       (propertize bmkext-search-prompt 'face '((:foreground "cyan"))))
        (inhibit-quit t)
        char)
    (catch 'break
      (while 1
        (catch 'continue
          (condition-case nil
              (setq char (read-char (concat prompt bmkext-search-pattern)))
            (error (throw 'break nil))) ; Break if char is an event.
          (case char
            ((or ?\e ?\r) (throw 'break nil)) ; RET or ESC break search loop and lead to [1].
            (?\d (pop tmp-list) ; Delete last char of `bmkext-search-pattern' with DEL
                 (setq bmkext-search-pattern
                       (mapconcat 'identity (reverse tmp-list) ""))
                 (throw 'continue nil))
            (?\C-g (setq bmkext-quit-flag t) (throw 'break (message "Quit")))
            (t
             (push (text-char-description char) tmp-list)
             (setq bmkext-search-pattern
                   (mapconcat 'identity (reverse tmp-list) ""))
             (throw 'continue nil))))))))

(defun bmkext-filtered-alist-by-regexp-only (regexp alist)
  "Return a filtered ALIST with (only) bookmarks matching REGEXP."
  (bmkext-remove-if-not #'(lambda (x) (string-match regexp (car x))) alist))

(defun bmkext-bmenu-filter-alist-by-regexp (regexp alist title)
  "Display (only) bookmarks of ALIST matching REGEXP."
  (let ((bookmark-alist (bmkext-filtered-alist-by-regexp-only regexp alist))
        (bmkext-bmenu-called-from-inside-flag t)) ; Dont remove marks if some.
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list title 'filteredp)))

;;;###autoload
(defun bmkext-bmenu-search (&optional all)
  "Incremental search of bookmarks matching `bmkext-search-pattern'.
We make search in the current list displayed i.e `bmkext-latest-bookmark-alist'.
If a prefix arg is given search in the whole `bookmark-alist'."
  (interactive "P")
  (when (string= (buffer-name (current-buffer)) "*Bookmark List*")
    (lexical-let* ((ctitle   (save-excursion (goto-char (point-min))
                                             (buffer-substring (point-at-bol) (point-at-eol))))
                   (bmk      (bookmark-bmenu-bookmark))
                   (ntitle   "% Bookmark Filtered by regexp")
                   (bmk-list (if all    ; Prefix arg
                                 (prog1 bookmark-alist (setq ntitle "% Bookmark"
                                                             ctitle "% Bookmark"))
                                 bmkext-latest-bookmark-alist)))
      (unwind-protect
           (progn
             (setq bmkext-search-timer
                   (run-with-idle-timer
                    bmkext-search-delay 'repeat
                    #'(lambda ()
                        (bmkext-bmenu-filter-alist-by-regexp bmkext-search-pattern bmk-list ntitle))))
             (bmkext-read-search-input))
        (progn  ; [1] Stop timer.
          (bmkext-bmenu-cancel-search)
          (if bmkext-quit-flag        ; C-g hit, rebuild menu list as before.
              (let ((bookmark-alist                       bmk-list)
                    (bmkext-bmenu-called-from-inside-flag t))
                (bookmark-bmenu-list ctitle) (bmkext-bmenu-goto-bookmark bmk))
              ;; Else show the narrowed alist only.
              (message "%d bookmarks found matching `%s'"
                       (length bmkext-latest-bookmark-alist) bmkext-search-pattern))
          (setq bmkext-quit-flag nil))))))

(defun bmkext-bmenu-cancel-search ()
  "Cancel timer used for searching in bookmarks."
  (cancel-timer bmkext-search-timer)
  (setq bmkext-search-timer nil))

;;;###autoload
(defun bmkext-bmenu-edit-bookmark1 ()
  "Edit the bookmark name and filename from bmenu list."
  (when (bookmark-bmenu-check-position)
    (let* ((bmk-name  (bookmark-bmenu-bookmark))
           (new-data  (bmkext-edit-bookmark bmk-name))
           (new-name  (car new-data)))
      (if (not new-data)
          (message "No changes made")
          (bookmark-bmenu-surreptitiously-rebuild-list)
          (goto-char (point-min))
          (while (not (equal new-name (bookmark-bmenu-bookmark)))
            (forward-line 1))
          (forward-line 0)
          (bookmark-bmenu-check-position)))))

(defun bmkext-bmenu-edit-bookmark ()
  "Edit the bookmark under the cursor."
  (interactive)
  (let* ((bname  (bookmark-bmenu-bookmark))
         (bentry (assoc bname bookmark-alist)))
    (if (bmkext-bookmark-addressbook-p bentry)
        (addressbook-bmenu-edit)
        (bmkext-bmenu-edit-bookmark1))))
        
;;;###autoload
(defun bmkext-bmenu-delete-bookmark ()
  "Delete bookmark at point in Bookmark Menu list."
  (interactive)
  (when (equal (buffer-name (current-buffer)) "*Bookmark List*")
    (let ((bmk (bookmark-bmenu-bookmark))
          (pos (point)))
      (if (y-or-n-p "Delete this bookmark? ")
          (cond ((assoc bmk bmkext-delicious-cache)
                 (anything-c-delicious-delete-bookmark
                  bmk
                  'bmkext-delicious-get-url-value
                  'bmkext-delicious-delete-sentinel))
                ((string= (cdr (assoc 'origin (bookmark-get-bookmark bmk 'noerror)))
                          "firefox-imported")
                 (message "Operation not supported on this type of bookmark."))
                ((string= (cdr (assoc 'origin (bookmark-get-bookmark bmk 'noerror)))
                          "w3m-imported")
                 (message "Operation not supported on this type of bookmark."))
                (t (bookmark-delete bmk) (goto-char pos)))
          (message "Aborting bookmark deletion")))))

(defsubst bmkext-bmenu-propertize-item (bookmark-name start end)
  "Add text properties to BOOKMARK-NAME, from START to END."
  (let* ((isfile        (bookmark-get-filename bookmark-name))
         (isremote      (and isfile (file-remote-p isfile)))
         (istramp       (and isfile (boundp 'tramp-file-name-regexp)
                             (save-match-data
                               (string-match tramp-file-name-regexp isfile))))
         (isw3m         (bmkext-w3m-bookmark-p bookmark-name))
         (isman         (bmkext-woman-man-bookmark-p bookmark-name))
         (issu          (and istramp (string-match bmkext-su-or-sudo-regexp
                                                   isfile)))
         (isabook       (bmkext-bookmark-addressbook-p
                         (assoc bookmark-name bookmark-alist)))
         (isannotation  (bookmark-get-annotation bookmark-name))
         (ishandler     (bookmark-get-handler bookmark-name))
         (isgnus        (bmkext-gnus-bookmark-p bookmark-name))
         (isbuf         (bmkext-get-buffer-name bookmark-name)))
    (put-text-property start end 'bmkext-bookmark-name bookmark-name)
    (add-text-properties
     start  end
     (cond ((or (eq ishandler 'Info-bookmark-jump) (string= isbuf "*info*")) ; Info
            '(mouse-face highlight follow-link t face bmkext-info
              help-echo "mouse-2: Go to this Info buffer"))
           (isgnus               ; Gnus
            '(mouse-face highlight follow-link t face bmkext-gnus
              help-echo "mouse-2: Go to this Gnus buffer"))
           (isabook              ; Addressbook
            '(mouse-face highlight follow-link t face bmkext-adressbook
              help-echo "mouse-2: Go to addressbook buffer"))
           (isw3m                ; W3m
            `(mouse-face highlight follow-link t face bmkext-w3m
                         help-echo (format "mouse-2 Goto URL: %s",isfile)))
           (isman                ; Woman and Man pages
            `(mouse-face highlight follow-link t face bmkext-woman
                         help-echo (format "mouse-2 Goto URL: %s",isfile)))
           ((and issu (not (bmkext-root-or-sudo-logged-p))) ; Root/sudo not logged
            `(mouse-face highlight follow-link t face bmkext-su-or-sudo
                         help-echo (format "mouse-2 Goto file: %s",isfile)))
           ((and isremote (not issu)) ; Remote file (ssh, ftp)
            `(mouse-face highlight follow-link t face bmkext-remote-file
                         help-echo (format "mouse-2 Goto remote file: %s",isfile)))
           ((and isfile (file-directory-p isfile)) ; Local directory
            `(mouse-face highlight follow-link t face bmkext-local-directory
                         help-echo (format "mouse-2 Goto dired: %s",isfile)))
           ((and isfile (file-exists-p isfile)) ; Local file
            `(mouse-face highlight follow-link t face
                         bmkext-local-file
                         help-echo (format "mouse-2 Goto file: %s",isfile)))
           ((and isbuf (if isfile (not (file-exists-p isfile)) (not isfile))) ; No filename
            `(mouse-face highlight follow-link t face bmkext-non-file
                         help-echo (format "mouse-2 Goto buffer: %s",isbuf)))))))

;;;###autoload
(defun bmkext-bmenu-quit ()
  "Reset the marked bookmark lists and quit."
  (interactive)
  (setq bmkext-bookmark-marked-list nil)
  (setq bmkext-bmenu-before-hide-marked-list nil)
  (setq bmkext-bmenu-before-hide-unmarked-list nil)
  (when (get-buffer "*Bookmark Annotation*")
    (kill-buffer "*Bookmark Annotation*"))
  (quit-window))

;;; Filters *-bmenu-* commands

;;;###autoload
(defun bmkext-bmenu-list-only-file-bookmarks (arg)
  "Display a list of file and directory bookmarks (only).
With a prefix argument, do not include remote files or directories."
  (interactive "P")
  (let ((bookmark-alist  (bmkext-file-alist-only arg))
        (bmkext-bmenu-called-from-inside-flag t))
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark Files&Directories" 'filteredp)))

;;;###autoload
(defun bmkext-bmenu-list-only-image-file-bookmarks ()
  "Display a list of image files bookmarks (only)."
  (interactive)
  (let ((bookmark-alist  (bmkext-image-file-alist-only))
        (bmkext-bmenu-called-from-inside-flag t))
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark Images" 'filteredp)))

;;;###autoload
(defun bmkext-bmenu-list-only-non-file-bookmarks ()
  "Display (only) the non-file bookmarks."
  (interactive)
  (let ((bookmark-alist (bmkext-non-file-alist-only))
        (bmkext-bmenu-called-from-inside-flag t))
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark Non--Files" 'filteredp)))

;;;###autoload
(defun bmkext-bmenu-list-only-info-bookmarks ()
  "Display (only) the Info bookmarks."
  (interactive)
  (let ((bookmark-alist  (bmkext-info-alist-only))
        (bmkext-bmenu-called-from-inside-flag t))
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark Info" 'filteredp)))

;;;###autoload
(defun bmkext-bmenu-list-only-w3m-bookmarks (&optional import)
  "Display (only) the w3m bookmarks.
IMPORT mean display also the in--w3m browser bookmarks.(those that are in `w3m-bookmark-file')."
  (interactive "P")
  (let* ((ext-list (bmkext-w3m-alist-only-imported))
         (local-list (bmkext-w3m-alist-only))
         (all-w3m (append ext-list local-list))
         (bookmark-alist (if import
                             (prog1 all-w3m
                               (message "`%d' W3m bookmarks have been imported."
                                        (length ext-list)))
                             local-list))
         (bmkext-bmenu-called-from-inside-flag t))
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark W3m" 'filteredp)))

;;;###autoload
(defun bmkext-bmenu-list-only-gnus-bookmarks ()
  "Display (only) the Gnus bookmarks."
  (interactive)
  (let ((bookmark-alist  (bmkext-gnus-alist-only))
        (bmkext-bmenu-called-from-inside-flag t))
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark Gnus" 'filteredp)))

;;;###autoload
(defun bmkext-bmenu-list-only-woman-man-bookmarks ()
  "Display (only) the bookmarks that record a Man or Woman page."
  (interactive)
  (let ((bookmark-alist  (bmkext-woman-man-alist-only))
        (bmkext-bmenu-called-from-inside-flag t))
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark Man&Woman pages" 'filteredp)))

;;;###autoload
(defun bmkext-bmenu-list-only-last-org-bookmarks ()
  "Display (only) the bookmarks that record last org stored bookmarks."
  (interactive)
  (let ((bookmark-alist  (bmkext-org-last-stored-alist-only))
        (bmkext-bmenu-called-from-inside-flag t))
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark Last Org Stored" 'filteredp)))

(defun bmkext-bmenu-list-only-addressbook-bookmarks ()
  "Display (only) addressbook bookmarks."
  (interactive)
  (let ((bookmark-alist  (bmkext-addressbook-alist-only))
        (bmkext-bmenu-called-from-inside-flag t))
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark Addressbook" 'filteredp)))

;;;###autoload
(defun bmkext-bmenu-show-all-bookmarks ()
  "Show all bookmarks without removing marks if some."
  (interactive)
  (let ((bmkext-bmenu-called-from-inside-flag t))
    (bookmark-bmenu-list)))

;;; *-bmenu-* Commands and functions for marked bookmarks

;;;###autoload
(defun bmkext-bmenu-mark-all-bookmarks ()
  "Mark all bookmarks with flag >."
  (interactive)
  (with-current-buffer "*Bookmark List*"
    (goto-char (point-min))
    (bookmark-bmenu-check-position)
    (save-excursion
      (while (not (eobp))
        (when (bookmark-bmenu-check-position)
          (bookmark-bmenu-mark)))))
  (bmkext-count-marked))


;;;###autoload
(defun bmkext-bmenu-unmark-all-deletion-flags ()
  "Unmark all bookmarks marked with flag D."
  (interactive)
  (bmkext-bmenu-unmark-all-1 'del)
  (bmkext-count-marked))

;;;###autoload
(defun bmkext-bmenu-unmark-all-non-deletion-flags ()
  "Unmark all bookmarks marked with flag >."
  (interactive)
  (bmkext-bmenu-unmark-all-1 nil 'mark)
  (bmkext-count-marked))


;;;###autoload
(defun bmkext-bmenu-unmark-all ()
  "Unmark all bookmarks marked with flag > or D.
Called with prefix arg provide an interactive interface."
  (interactive)
  (if (or (bmkext-current-list-have-marked-p)
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^>\\|^D" (point-max) t)))
      (progn
        (if current-prefix-arg
            (bmkext-bmenu-unmark-all-2)
            (bmkext-bmenu-unmark-all-1))
        (bmkext-count-marked))
      (message "Nothing to unmark here!")))


(defun bmkext-bmenu-unmark-all-1 (&optional del mark)
  "Unmark all bookmarks or only bookmarks marked with flag > or D.
Whitout args unmark all.
If DEL is non--nil unmark only bookmarks with flag D.
If MARK is non--nil unmark only bookmarks with flag >."
  (with-current-buffer "*Bookmark List*"
    (save-excursion
      (goto-char (point-min))
      (bookmark-bmenu-check-position)
      (while (cond (mark
                    (re-search-forward "^>" (point-max) t))
                   (del
                    (re-search-forward "^D" (point-max) t))
                   (t (re-search-forward "^>\\|^D" (point-max) t)))
        (when (bookmark-bmenu-check-position)
          (bookmark-bmenu-unmark))))))


;;;###autoload
(defun bmkext-bmenu-unmark-all-2 ()
  "Provide an interactive interface to unmark bookmarks."
  (with-current-buffer "*Bookmark List*"
    (let ((prompt "(U)nmark, (I)gnore, (P)recedent line, (N)ext line, (!)Unmark all remaining, (Q)uit")
          action)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^>\\|^D" (point-max) t)
        (forward-line 0)
        (catch 'break
          (while 1
            (catch 'continue
              (when (fboundp 'propertize)
                (setq prompt (propertize prompt 'face '((:foreground "cyan")))))
              (setq action (read-event prompt))
              (case action
                (?U (when (bookmark-bmenu-check-position)
                      (bookmark-bmenu-unmark)
                      (if (re-search-forward "^>\\|^D" (point-max) t)
                          (progn (forward-line 0) (throw 'continue nil))
                          (throw 'break nil))))
                (?I (if (looking-at "^>\\|^D")
                        (progn (forward-char 1)
                               (if (re-search-forward "^>\\|^D" (point-max) t)
                                   (progn (forward-line 0) (throw 'continue nil))
                                   (throw 'break nil)))
                        (throw 'break nil)))
                (?P (forward-line -1) (throw 'continue nil))
                (?N (forward-line 1) (throw 'continue nil))
                (?! (throw 'break
                      (while (re-search-forward "^>\\|^D" (point-max) t)
                        (when (bookmark-bmenu-check-position)
                          (bookmark-bmenu-unmark)))))
                (?Q (throw 'break nil))))))))))


(defun bmkext-count-marked ()
  "Send message with number of marked and unmarked bookmarks in current display."
  (let* ((marked   (length bmkext-bookmark-marked-list))
         (unmarked (- (length bmkext-latest-bookmark-alist) marked)))
    (message "%s Marked, %s Unmarked" marked unmarked)))


;;;###autoload
(defun bmkext-bmenu-regexp-mark (regexp)
  "Mark bookmarks that match REGEXP."
  (interactive "sRegexp: ")
  (with-current-buffer "*Bookmark List*"
    (goto-char (point-min))
    (forward-line 2)
    (while (re-search-forward regexp (point-max) t)
      (when (bookmark-bmenu-check-position)
        (bookmark-bmenu-mark))))
  (bmkext-count-marked))


;;;###autoload
(defun bmkext-bmenu-hide-marked ()
  "Hide all marked bookmarks."
  (interactive)
  (when (or (bmkext-current-list-have-marked-p)
            (bmkext-current-list-have-marked-p 
             bmkext-bmenu-before-hide-marked-list))
    (let ((bookmark-alist  bmkext-latest-bookmark-alist)
          (bmkext-bmenu-called-from-inside-flag t)
          status)
      (if bmkext-bmenu-before-hide-marked-list
          ;; unhide marked
          (progn
            (setq bookmark-alist bmkext-bmenu-before-hide-marked-list)
            (setq bmkext-bmenu-before-hide-marked-list nil)
            (setq bmkext-latest-bookmark-alist  bookmark-alist)
            (setq status 'show))
          ;; hide marked
          (setq bmkext-bmenu-before-hide-marked-list bmkext-latest-bookmark-alist)
          (setq bookmark-alist (bmkext-non-marked-bookmarks-only))
          (setq bmkext-latest-bookmark-alist  bookmark-alist)
          (setq status 'hiden))
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (if (eq status 'hiden)
          (bookmark-bmenu-check-position)
          (goto-char (point-min))
          (when (re-search-forward "^>" (point-max) t)
            (forward-line 0))))))


;;;###autoload
(defun bmkext-bmenu-hide-unmarked ()
  "Hide all unmarked bookmarks."
  (interactive)
  (when (bmkext-current-list-have-marked-p)
    (let ((bookmark-alist  bmkext-latest-bookmark-alist)
          (bmkext-bmenu-called-from-inside-flag t)
          status)
      (if bmkext-bmenu-before-hide-unmarked-list
          ;; unhide non marked
          (progn                                                  
            (setq bookmark-alist bmkext-bmenu-before-hide-unmarked-list)
            (setq bmkext-bmenu-before-hide-unmarked-list nil)
            (setq bmkext-latest-bookmark-alist  bookmark-alist)
            (setq status 'show))
          ;; hide non-marked
          (setq bmkext-bmenu-before-hide-unmarked-list bmkext-latest-bookmark-alist)
          (setq bookmark-alist (bmkext-marked-bookmarks-only))
          (setq bmkext-latest-bookmark-alist  bookmark-alist)
          (setq status 'hiden))
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (if (eq status 'hiden)
          (bookmark-bmenu-check-position)
          (goto-char (point-min))
          (when (re-search-forward "^>" (point-max) t)
            (forward-line 0))))))


;;;###autoload
(defun bmkext-bmenu-toggle-marks ()
  "Toggle mark on each bookmark in menu-list."
  (interactive)
  (with-current-buffer "*Bookmark List*"
    (save-excursion
      (goto-char (point-min))
      (bookmark-bmenu-check-position)
      (if (bmkext-current-list-have-marked-p)
          (while (not (eobp))
            (let ((bmk (bookmark-bmenu-bookmark)))
              (if (member bmk bmkext-bookmark-marked-list)
                  (bookmark-bmenu-unmark)
                  (bookmark-bmenu-mark))))
          (bmkext-bmenu-mark-all-bookmarks)))
    (bmkext-count-marked)))

;;; Addressbook

;;;###autoload
(defun bmkext-addressbook-set-mail-buffer (arg)
  (interactive "P")
  (let ((bmk (bookmark-bmenu-bookmark))) 
    (if (bmkext-bookmark-addressbook-p bmk)
        (addressbook-set-mail-buffer1 bmk arg)
        (bookmark-bmenu-this-window))
    (bmkext-update-time-and-increment-visits bmk 'batch)))

;;;###autoload
(defun bmkext-addressbook-set-mail-buffer-and-cc (arg)
  (interactive "P")
  (let ((bmk (bookmark-bmenu-bookmark)))
    (if (bmkext-bookmark-addressbook-p bmk)
        (addressbook-set-mail-buffer1 bmk arg 'cc)
        (bookmark-bmenu-this-window))
    (bmkext-update-time-and-increment-visits bmk 'batch)))

;;;###autoload
(defun bmkext-addressbook-send-to-marked ()
  (interactive)
  (when bmkext-bookmark-marked-list
    (let ((ls (reverse bmkext-bookmark-marked-list))
          buf)
      (when (bmkext-bookmark-addressbook-p (car ls))
        (save-window-excursion
          (addressbook-set-mail-buffer1 (car ls))
          (bmkext-update-time-and-increment-visits (car ls) 'batch)
          (setq buf (current-buffer))))
      (loop for bmk in (cdr ls)
         when (bmkext-bookmark-addressbook-p bmk)
         do (save-window-excursion
              (addressbook-set-mail-buffer1 bmk 'append)
              (bmkext-update-time-and-increment-visits bmk 'batch)))
      (switch-to-buffer-other-window buf))))

;;;###autoload
(defun bmkext-export-addressbook ()
  "Export all addressbook entries to a file of your choice.
Use `bookmark-load' to import the contents
of the file to an existent `bookmark-alist'."
  (interactive)
  (let ((bookmark-alist  (bmkext-addressbook-alist-only)))
    (bookmark-save 1)))

(defun bmkext-sync-abook-from-file (file)
  (interactive "fFile: ")
  (with-current-buffer (let ((enable-local-variables nil))
                         (find-file-noselect file))
    (goto-char (point-min))
    (bookmark-maybe-upgrade-file-format)
    (let* ((ori-alist      (bmkext-addressbook-alist-only))
           (imported-abook (bookmark-alist-from-buffer))
           (filtered-abook (bmkext-remove-if
                            #'(lambda (x) (member x ori-alist))
                            imported-abook)))
      (bookmark-import-new-list filtered-abook))))

  
;; Predicates --------------------------------------------------------

(defun bmkext-gnus-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Gnus bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'bmkext-jump-gnus)
      (eq (bookmark-get-handler bookmark) 'gnus-summary-bookmark-jump)
      (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-gnus)))

(defun bmkext-w3m-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a W3m bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'bmkext-jump-w3m)
      (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-w3m)))

(defun bmkext-info-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is an Info bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'Info-bookmark-jump))

(defun bmkext-woman-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Woman bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'bmkext-jump-woman)
      (eq (bookmark-get-handler bookmark) 'woman-bookmark-jump)
      (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-woman)))

(defun bmkext-man-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Man bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'bmkext-jump-man)
      (eq (bookmark-get-handler bookmark) 'Man-bookmark-jump)
      (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-man)))

(defun bmkext-woman-man-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Man or Woman bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (bmkext-man-bookmark-p bookmark)
      (bmkext-woman-bookmark-p bookmark)))

(defun bmkext-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
  (let* ((filename   (bookmark-get-filename bookmark))
         (isnonfile  (equal filename bmkext-non-file-filename))) 
    (and filename (not isnonfile) (not (bookmark-get-handler bookmark)))))

(defun bmkext-image-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks an image file."
  (if (stringp bookmark)
      (assoc 'image-type (assoc bookmark bookmark-alist))
      (assoc 'image-type bookmark)))

(defun bmkext-non-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a non-file bookmark (e.g *scratch*).
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
  (let* ((filename   (bookmark-get-filename bookmark))
         (isnonfile  (equal filename bmkext-non-file-filename))) 
    (and isnonfile (not (bookmark-get-handler bookmark)))))

(defun bmkext-remote-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a remote file or directory.
BOOKMARK is a bookmark name or a bookmark record."
  (let* ((file      (bookmark-get-filename bookmark))
         (rem-file  (and file           ; Don't give nil to `file-remote-p'
                         (if (fboundp 'file-remote-p)
                             (file-remote-p file)
                             (and (fboundp 'ffap-file-remote-p) (ffap-file-remote-p file))))))
    (and rem-file  (not (bookmark-get-handler bookmark))  rem-file)))

(defun bmkext-local-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a local file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
  (and (bmkext-file-bookmark-p bookmark)
       (not (bmkext-remote-file-bookmark-p bookmark))))

(defun bmkext-local-directory-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a local directory.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((file  (bookmark-get-filename bookmark)))
    (and (bmkext-local-file-bookmark-p bookmark) (file-directory-p file))))

(defun bmkext-bookmark-marked-p (bookmark)
  "Return non-nil if BOOKMARK is a marked bookmark."
  (if (listp bookmark)
      (member (car bookmark) bmkext-bookmark-marked-list)
      (member bookmark bmkext-bookmark-marked-list)))

(defun bmkext-bookmark-last-org-p (bookmark)
  "Return non-nil if BOOKMARK is a org bookmark."
  (let ((bmk (if (listp bookmark) (car bookmark) bookmark))) 
    (or (string= "org-refile-last-stored" bmk)
        (string= "org-remember-last-stored" bmk)
        (string= "org-capture-last-stored" bmk)
        (string= "org-capture-last-stored-marker" bmk))))

(defun bmkext-bookmark-addressbook-p (bookmark)
  (if (listp bookmark)
      (string= (assoc-default 'type bookmark) "addressbook")
      (string= (assoc-default
                'type (assoc bookmark bookmark-alist)) "addressbook")))

;; Filter Functions --------------------------------------------------

(defun bmkext-org-last-stored-alist-only ()
  "`bookmark-alist', filtered to retain only last stored org  bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-bookmark-last-org-p bookmark-alist))

(defun bmkext-gnus-alist-only ()
  "`bookmark-alist', filtered to retain only Gnus bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-gnus-bookmark-p bookmark-alist))


(defun bmkext-w3m-alist-only ()
  "`bookmark-alist', filtered to retain only W3m bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-w3m-bookmark-p bookmark-alist))


(defun bmkext-w3m-alist-only-imported ()
  "All W3m imported bookmarks."
  (bmkext-create-alist-from-html
   w3m-bookmark-file bmkext-w3m-bookmark-url-regexp))


(defun bmkext-info-alist-only ()
  "`bookmark-alist', filtered to retain only Info bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-info-bookmark-p bookmark-alist))


(defun bmkext-woman-alist-only ()
  "`bookmark-alist', filtered to retain only Woman bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-woman-bookmark-p bookmark-alist))


(defun bmkext-man-alist-only ()
  "`bookmark-alist', filtered to retain only Man bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-man-bookmark-p bookmark-alist))


(defun bmkext-woman-man-alist-only ()
  "`bookmark-alist', filtered to retain only Man or Woman bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-woman-man-bookmark-p bookmark-alist))


(defun bmkext-remote-file-alist-only ()
  "`bookmark-alist', filtered to retain only remote-file bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-remote-file-bookmark-p bookmark-alist))


(defun bmkext-local-file-alist-only ()
  "`bookmark-alist', filtered to retain only local-file bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-local-file-bookmark-p bookmark-alist))

(defun bmkext-image-file-alist-only ()
  "`bookmark-alist', filtered to retain only image-file bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-image-bookmark-p bookmark-alist))


(defun bmkext-file-alist-only (&optional hide-remote)
  "`bookmark-alist', filtered to retain only file and directory bookmarks.
This excludes bookmarks that might contain file information but are
particular in some way - for example, Info bookmarks or Gnus bookmarks.

Non-nil argument HIDE-REMOTE means do not include remote file or
directory bookmarks.

A new list is returned (no side effects)."
  (bmkext-remove-if
   #'(lambda (bookmark)
       (or (bmkext-non-file-bookmark-p bookmark)
           (bmkext-gnus-bookmark-p bookmark)
           (bmkext-w3m-bookmark-p bookmark)
           (bmkext-woman-man-bookmark-p bookmark)
           (bmkext-info-bookmark-p bookmark)
           (bmkext-bookmark-addressbook-p bookmark)
           (and hide-remote (bmkext-remote-file-bookmark-p bookmark))))
   bookmark-alist))


(defun bmkext-non-file-alist-only ()
  "`bookmark-alist', filtered to retain only non-file bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-non-file-bookmark-p bookmark-alist))

(defun bmkext-addressbook-alist-only ()
  "`bookmark-alist', filtered to retain only addressbook bookmarks.
A new list is returned (no side effects)."
  (bmkext-remove-if-not #'bmkext-bookmark-addressbook-p bookmark-alist))

;;; Marked bookmarks

(defun bmkext-marked-bookmarks-only ()
  "Return the list of marked bookmarks."
  (bmkext-remove-if-not #'bmkext-bookmark-marked-p bookmark-alist))

(defun bmkext-non-marked-bookmarks-only ()
  "Return the list of not marked bookmarks."
  (bmkext-remove-if #'bmkext-bookmark-marked-p bookmark-alist))


(defun bmkext-current-list-have-marked-p (&optional alist)
  "Return non--nil if `bmkext-latest-bookmark-alist' have marked bookmarks."
  (when bmkext-bookmark-marked-list
    (let ((last-alist (or alist bmkext-latest-bookmark-alist)))
      (catch 'break
        (dolist (i last-alist)
          (when (bmkext-bookmark-marked-p i)
            (throw 'break t)))))))


;; Other Functions ---------------------------------------------------

(defun bmkext-get-buffer-name (bookmark)
  "Return the buffer-name of BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'buffer-name))

(defun bmkext-root-or-sudo-logged-p ()
  "Return t if the user logged in using Tramp as `root' or `sudo'.
Otherwise, return nil."
  (let ((su-or-sudo-regex  "\\(su\\|sudo\\)"))
    (catch 'break
      (dolist (i (mapcar #'buffer-name (buffer-list)))
        (when (string-match (format "*tramp/%s ." su-or-sudo-regex) i) (throw 'break t))))))

;;; W3M support
(defun bmkext-make-w3m-record ()
  "Make a special entry for w3m buffers."
  (require 'w3m)                        ; For `w3m-current-url'.
  `(,w3m-current-title
    ,@(bookmark-make-record-default 'no-file)
      (filename . ,w3m-current-url)
      (handler . bmkext-jump-w3m)))

(add-hook 'w3m-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'bmkext-make-w3m-record)))

(defun bmkext-w3m-set-new-buffer-name ()
  "Set the w3m buffer name according to the number of w3m buffers already open."
  (let ((len  (length (w3m-list-buffers 'nosort))))
    (if (eq len 0)  "*w3m*"  (format "*w3m*<%d>" (1+ len)))))

(defun bmkext-jump-w3m-new-session (bookmark)
  "Jump to W3m bookmark BOOKMARK, setting a new tab."
  (let ((file  (bookmark-prop-get bookmark 'filename))
        (buf   (bmkext-w3m-set-new-buffer-name)))
    (w3m-browse-url file 'newsession)
    (while (not (get-buffer buf)) (sit-for 1)) ; Be sure we have the W3m buffer.
    (with-current-buffer buf
      (goto-char (point-min))
      ;; Wait until data arrives in buffer, before setting region.
      (while (eq (line-beginning-position) (line-end-position)) (sit-for 1)))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

(defun bmkext-jump-w3m-only-one-tab (bookmark)
  "Close all W3m sessions and jump to BOOKMARK in a new W3m buffer."
  (let ((file  (bookmark-prop-get bookmark 'filename)))
    (w3m-quit 'force)                   ; Be sure we start with an empty W3m buffer.
    (w3m-browse-url file)
    (with-current-buffer "*w3m*" (while (eq (point-min) (point-max)) (sit-for 1)))
    (bookmark-default-handler
     `("" (buffer . ,(buffer-name (current-buffer))) .
          ,(bookmark-get-bookmark-record bookmark)))))

(defun bmkext-jump-w3m (bookmark)
  "Handler function for record returned by `bmkext-make-w3m-record'.
BOOKMARK is a bookmark name or a bookmark record.
Use multi-tabs in W3m if `bmkext-w3m-allow-multi-tabs' is non-nil.
If a prefix arg is given, open an external navigator defined in
`bmkext-external-browse-url-function'."
  (if (and current-prefix-arg bmkext-external-browse-url-function)
      (bmkext-jump-url-external bookmark)
      (if bmkext-w3m-allow-multi-tabs
          (bmkext-jump-w3m-new-session bookmark)
          (bmkext-jump-w3m-only-one-tab bookmark))))

(defun bmkext-jump-url-external (bookmark)
  "Jump to BOOKMARK in an external navigator.
External navigator is defined by `bmkext-external-browse-url-function'."
  (let ((file  (bookmark-prop-get bookmark 'filename)))
    (bmkext-update-time-and-increment-visits bookmark 'batch)
    (funcall bmkext-external-browse-url-function file)))


;;;; External bookmarks importation
;;
;; Html routines
;;
(defvar bmkext-w3m-bookmark-url-regexp "\\(https\\|http\\|ftp\\|file\\)://[^>]*")
(defvar bmkext-firefox-bookmark-url-regexp "\\(https\\|http\\|ftp\\|about\\|file\\)://[^ ]*")
(defun bmkext-html-bookmarks-to-alist (file url-regexp)
  "Parse html bookmark FILE and return an alist with (title . url) as elements."
  (let (bookmarks-alist url title)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (forward-line)
        (when (re-search-forward "href=\\|^ *<DT><A HREF=" nil t)
          (beginning-of-line)
          (when (re-search-forward url-regexp nil t)
            (setq url (concat "\"" (match-string 0))))
          (beginning-of-line)
          (when (re-search-forward bmkext-w3m-bookmarks-regexp nil t)
            (setq title (match-string 0)))
          (push (cons title url) bookmarks-alist))))
    (nreverse bookmarks-alist)))


(defun bmkext-create-alist-from-html (file regexp &optional origin all)
  "Create a bmkext alist from html bookmark FILE.
All doublons are removed."
  (loop
     with w3m-bmks = (bmkext-html-bookmarks-to-alist file regexp)
     with actuals-bmk = (bookmark-all-names)
     with new-list
     for i in w3m-bmks
     for title = (replace-regexp-in-string "^\>" "" (car i))
     for exists = (member title actuals-bmk)
     for fm-bmk = (bmkext-format-html-bmk i origin)
     for doublons = (assoc title new-list)
     unless (or (unless all exists) doublons)
     collect fm-bmk into new-list
     finally return new-list))

(defun bmkext-format-html-bmk (bookmark &optional origin)
  "Create a bmkext bookmark compatible entry from BOOKMARK.
BOOKMARK is an element of alist created with `bmkext-html-bookmarks-to-alist'.
It have the form (title . url).
ORIGIN mention where come from this bookmark."
  (let ((title   (url-unhex-string (car bookmark)))
        (fname   (url-unhex-string (cdr bookmark)))
        (buf     "*w3m*")
        (beg     1)
        (end     1)
        (origin  (if origin origin "w3m-imported"))
        (handler 'bmkext-jump-w3m)
        (ctime   (float-time)))
    (setq title (replace-regexp-in-string "^\>" "" title))
    (setq fname (replace-regexp-in-string "\"" "" fname))
    `(,@`(,title
          (filename . ,fname) 
          (buffer-name . ,buf)
          (front-context-string)
          (rear-context-string)
          (visits . 0)
          (time . ,ctime)
          (position . ,beg)
          (origin . ,origin)
          (handler . ,handler)))))


;;; Firefox bookmarks importation

;; Note: Since firefox version >=3 firefox don't use anymore
;; bookmarks.html file. So you will find this file nearly empty.
;; We need to fill this file to import bookmarks here.
;; To reactivate this firefox functionality, add the following line
;; in your user.js file:
;; user_pref("browser.bookmarks.autoExportHTML", true);

(defun bmkext-get-firefox-user-init-dir ()
  "Guess the default Firefox user directory name."
  (let ((moz-user-dir (with-current-buffer
                          (find-file-noselect
                           (concat bmkext-firefox-default-directory "profiles.ini"))
                        (goto-char (point-min))
                        (when (search-forward "Path=" nil t)
                          (buffer-substring-no-properties (point) (point-at-eol))))))
    (file-name-as-directory
     (concat bmkext-firefox-default-directory moz-user-dir))))

(defun bmkext-guess-firefox-bookmark-file ()
  "Return the path of the Firefox bookmarks file."
  (concat (bmkext-get-firefox-user-init-dir) "bookmarks.html"))

(defun bmkext-firefox-bookmarks-to-alist ()
  "Create a `bookmark-alist' from Firefox bookmarks."
  (bmkext-create-alist-from-html
   (bmkext-guess-firefox-bookmark-file)
   bmkext-firefox-bookmark-url-regexp
   "firefox-imported" 'all))

;;;###autoload
(defun bmkext-bmenu-list-only-firefox-bookmarks ()
  "Display (only) the Firefox imported bookmarks."
  (interactive)
  (let ((bookmark-alist (bmkext-firefox-bookmarks-to-alist)))
    (setq bmkext-bmenu-called-from-inside-flag t)
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark Firefox" 'filteredp)
    (message "`%d' bookmarks imported from Firefox." (length bookmark-alist))))

;;; W3m bookmarks importation

(defun bmkext-w3m-bookmarks-to-alist ()
  "Create an alist structured like `bookmark-alist' from `w3m-bookmark-file'."
  (bmkext-html-bookmarks-to-alist
   w3m-bookmark-file
   bmkext-w3m-bookmark-url-regexp))

;;; Delicious bookmarks importation
;; Dependency needed: http://mercurial.intuxication.org/hg/anything-delicious

(defvar bmkext-delicious-cache nil)
(defun bmkext-create-alist-from-delicious ()
  "Create a bmkext alist from XML file `anything-c-delicious-cache-file'."
  (require 'anything-delicious nil t)
  (if (fboundp 'anything-set-up-delicious-bookmarks-alist)
      (loop
         with delicious-bmks = (anything-set-up-delicious-bookmarks-alist)
         with new-list
         for i in delicious-bmks
         for fm-bmk = (bmkext-format-html-bmk i "delicious-imported")
         for doublon-p = (assoc (car i) new-list)
         unless doublon-p
         collect fm-bmk into new-list
         finally return new-list)
      (error "anything-delicious library not found, please install it.")))

(defun bmkext-bmenu-list-only-delicious-bookmarks (&optional rebuild-cache)
  "Display (only) the Delicious bookmarks."
  (let ((bookmark-alist (if (and (not rebuild-cache) bmkext-delicious-cache)
                            bmkext-delicious-cache
                            (setq bmkext-delicious-cache
                                  (bmkext-create-alist-from-delicious))))
        (bmkext-bmenu-sort-function nil))
    (setq bmkext-bmenu-called-from-inside-flag t)
    (setq bmkext-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark Delicious" 'filteredp)
    (message "`%d' bookmarks imported from Delicious." (length bookmark-alist))))

(defun bmkext-delicious-get-url-value (bmk)
  "Get the url of delicious BMK from `bmkext-delicious-cache'."
  (cdr (assoc 'filename (assoc bmk bmkext-delicious-cache))))

(defun bmkext-delicious-delete-sentinel (process event)
  "Sentinel used for asynchronous delicious bookmark deletion."
  (anything-delicious-delete-sentinel process event)
  (bmkext-bmenu-list-only-delicious-bookmarks 'rebuild-cache))

;;;###autoload
(defun bmkext-bmenu-refresh-delicious ()
  (interactive)
  (if (fboundp 'anything-wget-retrieve-delicious)
      (progn
        (message "Synchronising with Delicious...")
        (anything-wget-retrieve-delicious 'bmkext-delicious-refresh-sentinel))
      (error "anything-delicious library not found, please install it.")))


(defun bmkext-delicious-refresh-sentinel (process event)
  (if (string= event "finished\n")
      (message "Syncing with Delicious...Done.")
      (message "Failed to synchronize with Delicious."))
  (setq anything-c-delicious-cache nil
        bmkext-delicious-cache nil)
  (bmkext-bmenu-list-only-delicious-bookmarks))

;;;###autoload
(defun bmkext-bmenu-delicious (&optional refresh)
  (interactive "P")
  (if refresh
      (bmkext-bmenu-refresh-delicious)
      (bmkext-bmenu-list-only-delicious-bookmarks)))

;;;; Special handlers
(defalias 'bmkext-jump-woman 'woman-bookmark-jump)
(defalias 'bmkext-jump-man 'Man-bookmark-jump)

;;; Gnus support.  Does not handle regions.
(defalias 'bmkext-jump-gnus 'gnus-summary-bookmark-jump)

;; Redefine in `gnus-sum.el'.
;;
;; Allow to bookmark from article buffer and retrieve position.
;;
(defun gnus-summary-bookmark-make-record ()
  "Make a bookmark entry for a Gnus summary buffer."
  (let (pos buf)
    (unless (and (derived-mode-p 'gnus-summary-mode) gnus-article-current)
      (save-restriction              ; FIXME is it necessary to widen?
        (widen) (setq pos (point))) ; Set position in gnus-article buffer.
      (setq buf "art") ; We are recording bookmark from article buffer.
      (setq bookmark-yank-point (point))
      (setq bookmark-current-buffer (current-buffer))
      (gnus-article-show-summary))      ; Go back in summary buffer.
    ;; We are now recording bookmark from summary buffer.
    (unless buf (setq buf "sum"))
    (let* ((subject (elt (gnus-summary-article-header) 1))
           (grp     (car gnus-article-current))
           (art     (cdr gnus-article-current))
           (head    (gnus-summary-article-header art))
           (id      (mail-header-id head)))
      `(,subject
        ,@(bookmark-make-record-default 'no-file 'no-context pos)
        (location . ,(format "Gnus-%s %s:%d:%s" buf grp art id))
        (group . ,grp) (article . ,art)
        (message-id . ,id) (handler . gnus-summary-bookmark-jump)))))

;; Redefine in `gnus-sum.el'.
;;
;; Allow to jump to a bookmark recorded from article buffer.
;;
(defun gnus-summary-bookmark-jump (bookmark)
  "Handler function for record returned by `gnus-summary-bookmark-make-record'.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((group    (bookmark-prop-get bookmark 'group))
        (article  (bookmark-prop-get bookmark 'article))
        (id       (bookmark-prop-get bookmark 'message-id))
        (buf      (car (split-string (bookmark-prop-get bookmark 'location)))))
    (gnus-fetch-group group (list article))
    (gnus-summary-insert-cached-articles)
    (gnus-summary-goto-article id nil 'force)
    ;; FIXME we have to wait article buffer is ready (only large buffer)
    ;; Is there a better solution to know that?
    ;; If we don't wait `bookmark-default-handler' will have no chance
    ;; to set position. However there is no error, just wrong pos.
    (sit-for 1)
    (when (string= buf "Gnus-art")
      (other-window 1))
    (bookmark-default-handler
     `(""
       (buffer . ,(current-buffer))
       . ,(bookmark-get-bookmark-record bookmark)))))

(add-hook 'gnus-article-mode-hook
          #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                            'gnus-summary-bookmark-make-record)))


;;; Only for versions inferior to 24.
;; Use code of Emacs24.
(when (< emacs-major-version 24)
  (add-hook 'gnus-summary-mode-hook
            #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                              'gnus-summary-bookmark-make-record)))


;;; Woman support
  (defalias 'woman-bookmark-make-record 'bmkext-make-woman-record)
  (defun woman-bookmark-make-record ()
    "Make a bookmark entry for a Woman buffer."
    `(,(Man-default-bookmark-title)
       ,@(bookmark-make-record-default 'no-file)
       (location . ,(concat "woman " woman-last-file-name))
       ;; Use the same form as man's bookmarks, as much as possible.
       (man-args . ,woman-last-file-name)
       (handler . woman-bookmark-jump)))

  (defun woman-bookmark-jump (bookmark)
    "Default bookmark handler for Woman buffers."
    (let* ((file (bookmark-prop-get bookmark 'man-args))
           ;; FIXME: we need woman-find-file-noselect, since
           ;; save-window-excursion can't protect us from the case where
           ;; woman-find-file creates a new frame.
           (buf  (save-window-excursion
                   (woman-find-file file) (current-buffer))))
      (bookmark-default-handler
       `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

  (add-hook 'woman-mode-hook
            #'(lambda ()
                (set (make-local-variable 'bookmark-make-record-function)
                     'woman-bookmark-make-record)))


;;; Man Support
  (defalias 'Man-bookmark-make-record 'bmkext-make-man-record)
  (defcustom Man-name-local-regexp (concat "^" (regexp-opt '("NOM" "NAME")) "$")
    "Regexp that matches the text that precedes the command's name.
Used in `bookmark-set' to get the default bookmark name."
    :type 'string :group 'bookmark)

  (defun Man-default-bookmark-title ()
  "Default bookmark name for Man or WoMan pages.
Uses `Man-name-local-regexp'."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward Man-name-local-regexp nil t)
      (skip-chars-forward "\n\t ")
      (buffer-substring-no-properties (point) (line-end-position)))))

  (defun Man-bookmark-make-record ()
    "Make a bookmark entry for a Man buffer."
    `(,(Man-default-bookmark-title)
       ,@(bookmark-make-record-default 'no-file)
       (location . ,(concat "man " Man-arguments))
       (man-args . ,Man-arguments)
       (handler . Man-bookmark-jump)))


  (defun Man-bookmark-jump (bookmark)
    "Default bookmark handler for Man buffers."
    (let* ((man-args (bookmark-prop-get bookmark 'man-args))
           ;; Let bookmark.el do the window handling.
           ;; This let-binding needs to be active during the call to both
           ;; Man-getpage-in-background and accept-process-output.
           (Man-notify-method 'meek)
           (buf (Man-getpage-in-background man-args))
           (proc (get-buffer-process buf)))
      (while (and proc (eq (process-status proc) 'run))
        (accept-process-output proc))
      (bookmark-default-handler
       `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))


  (add-hook 'Man-mode-hook
            #'(lambda ()
                (set (make-local-variable 'bookmark-make-record-function)
                     'Man-bookmark-make-record))))

;; Redefine in image-mode.el
;;
;; Take advantage of new args in `bookmark-make-record-default'.
;; Do not set context in an image avoid garbage in .emacs.bmk.
(defun image-bookmark-make-record ()
  `(,@(bookmark-make-record-default nil 'no-context 0)
      (image-type . ,image-type)
      (handler    . image-bookmark-jump)))

;;;###autoload
(defun image-bookmark-jump (bmk)
  ;; This implements the `handler' function interface for record type
  ;; returned by `bookmark-make-record-function', which see.
  (prog1 (bookmark-default-handler bmk)
    (when (not (string= image-type (bookmark-prop-get bmk 'image-type)))
      (image-toggle-display))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bookmark-extensions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark-extensions.el ends here

