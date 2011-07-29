;;; search-net.el --- Search routines for Internet
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-05-29 Sat
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
;; Routines for calling the web browser to search the Internet .
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

(require 'browse-url)
(require 'thingatpt)

(defvar search-net-search-setup-alist
  '(
    (google
     "Google Search"
     ("http://www.google.com/search?q="))

    (google-code
     "Google Code Search"
     ("http://www.google.com/codesearch?hl=zh-CN&lr=&q="))

    (google-lucky
     "Google Feeling Lucky Search"
     ("http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q="))

    (google-image
     "Google Image Search"
     ("http://images.google.com/images?sa=N&tab=wi&q="))

    (google-blog-cn
     "Google (Chinese) Blog Search"
     ("http://blogsearch.google.com/blogsearch?hl=zh-CN&ie=UTF-8&oe=UTF-8&q="))

    (google-blog-en
     "Google (English) Blog Search"
     ("http://blogsearch.google.com/blogsearch?hl=en&ie=UTF-8&oe=UTF-8&q="))

    (google-group
     "Google Group Search"
     ("http://groups.google.com/groups?hl=zh-CN&ie=UTF-8&oe=UTF-8&q="))

    (google-file
     "Google File Search"
     ("http://www.google.com/search?q="
      " intitle:(\"index of\"\|\"last modified\"\|\"parent of\") -inurl:htm -inurl:html -inurl:php -inurl:asp"))

    (merriam-webster-url
     "Merriam-Webster Dictionary Search"
     ("http://www.m-w.com/cgi-bin/dictionary?book=Dictionary&va="))

    (tyda-se-en
     "Tyda (Swedish/English)"
     ("http://tyda.se/search?form=1&w="))

    )
  "Information for `search-net' setup.
Records in this list have the form

  (TYPE NAME BASE-URL [PRE-INPUT POST-INPUT TAIL-URL])

TYPE is the key symbol.
NAME is a unique string.
BASE-URL is first part of url and is inserted as is.
PRE-INPUT and POST-INPUT are strings added around search string.
They will both be url encoded.
TAIL-URL is last part of url and is inserted as is.

PRE-INPUT and the rest are optional.

Records have nearly the same form as args to w3m-search-advance
\(see url `http://www.emacswiki.org/emacs/w3m-extension.el') so
that new records may be created easily from such calls.")

;;(search-net-get-prompt 'google-code)
(defun search-net-get-prompt (type)
  (let ((rec (assoc type search-net-search-setup-alist)))
    (nth 1 rec)))

;;(search-net-make-url 'google-code "nxhtml")
(defun search-net-make-url (type search-string)
  (let* ((rec (assoc type search-net-search-setup-alist))
         (setup (nth 2 rec))
         (base-url    (nth 0 setup))
         (pre-input   (nth 1 setup))
         (post-input  (nth 2 setup))
         (tail-url    (nth 3 setup))
         (full-url
          (concat base-url
                  (browse-url-encode-url
                   (concat pre-input search-string post-input))
                  tail-url)))
    (message "search-net.url=%S" full-url)
    full-url))

(defvar search-net-engine-hist nil)
(defvar search-net-search-hist nil)

(eval-when-compile (require 'url-util))

(defun search-net-url-at-point ()
  (require 'url-util)
  (let* ((url (url-get-url-at-point))
         (urlobj (when url (url-generic-parse-url url))))
    (when urlobj
      (unless (member (url-type urlobj) '("http" "https"))
        (setq urlobj nil)))
    (when urlobj
      ;; Fix-me: check for more components
      (unless (url-fullness urlobj)
        (setq urlobj nil)))
    (when urlobj url)))

(eval-when-compile (require 'goto-addr))

;;;###autoload
(defun search-net-dwim ()
  (interactive)
  (require 'goto-addr)
  (let* ((mail (save-excursion (goto-address-find-address-at-point)))
         ;; Can't be both mail and normal web url link.
         (url (unless mail (search-net-url-at-point)))
         search
         engine)
    (cond
     (mail (unless (y-or-n-p "Looks like a mail address at point. Send mail? ")
             (setq search (concat "link:" mail))
             (setq mail nil)))
     (url (unless (y-or-n-p "Looks like a web link at point. Go there? ")
            (setq search (concat "link:" url))
            (setq url nil))))
    (cond
     (mail (search-net-mail-with-browser mail))
     (url (browse-url url))
     (t
      (let ((engine (search-net-read-engine search)))
        (setq search (search-net-read-search-string engine search))
        (browse-url (search-net-make-url engine search)))))))

;; (search-net-mail-to "some.one@somewhere123.net")
(defun search-net-mail-with-browser (address)
  "Send mail through link on temporary html page.
This makes it possible to also support web mail.
See for example `http://www.gmailusers.com/tools-mailto.htm'."
  (let* ((mail-helper-file "~/.emacs.d/temp-search-net-mail.html")
         (buf (find-file-noselect mail-helper-file)))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (format
"<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
 <head>
   <title>Mail Helper for `search-net'</title>
<meta http-equiv=\"Refresh\" content=\"1;url=mailto:%s\" />
 </head>
 <body style=\"width:50em;\">
   <h1>Mail Helper for `search-net'</h1>
   <p>
     This little page tries to call your mail program, even if
     you are using web mail.
   </p>
   <p>
     <a href=\"mailto:%s\"
        >%s</a>
   </p>
   <hr style=\"margin-top:4em\" />
   <p>
     <b>Note:</b>

     If mail does not start automatically try clicking the link above.
   </p>
 </body>
</html>"
       address address address))
      (basic-save-buffer))
    (browse-url (convert-standard-filename
                 (expand-file-name mail-helper-file)))))

(defvar search-net-engine-hint-hook nil
  "Normal hook run to suggest search engine.
Every function in this hook is called with two parameter, the
default search string as known so far \(before asking user) and a
symbol whose value is the search engine to use.  The functions
are supposed to modify the search engine choice if they know
something that fits the search better.")

;; Fix-me: better parameters.
(defun search-net-read-engine (search)
  "Ask the user which search engine.
Return chosen key for search engine in assoc list
`search-net-search-setup-alist' or nil if none."
  (setq search (or search (word-at-point)))
  (let ((engine 'google))
    (run-hook-with-args 'search-net-engine-hint-hook search 'engine)
    (let* ((completion-ignore-case t)
           (engine-rec (assoc engine search-net-search-setup-alist))
           (default-engine-name (nth 1 engine-rec))
           (engine-str (completing-read "Search engine: "
                                        (mapcar (lambda (rec)
                                                  (list (nth 1 rec)
                                                        (nth 0 rec)))
                                                search-net-search-setup-alist)
                                        nil ;; predicate
                                        t ;; require match
                                        default-engine-name ;; initial-input
                                        'search-net-engine-hist)))
      (catch 'found-engine-str
        (dolist (rec search-net-search-setup-alist)
          (when (string= engine-str (nth 1 rec))
            (throw 'found-engine-str (nth 0 rec))))
        nil))))

(defvar search-net-search-hint-hook nil
  "Normal hook run to suggest search string.
Every function in this hook is called with two parameters, the
search engine and a symbol holding the search string.  They are
supposed to enhance the search string if they can.")

;; Fix-me: better parameters.
(defun search-net-read-search-string (engine search-default)
  (setq search-default
        (or search-default
            (if (use-region-p)
                (concat "\""
                        (buffer-substring-no-properties (region-beginning) (region-end))
                        "\"")
              (or (let ((w (word-at-point)))
                    (when w (substring-no-properties w)))
                  ""))))
  (run-hook-with-args 'search-net-search-hint-hook engine 'search-default)
  (let (search)
    (setq search (read-string "Search string: "
                              search-default
                              'search-net-search-hist))
    (unless (string= "" search) search)))

;;;###autoload
(defun search-net (engine what)
  "Search the Internet with web browser.
ENGINE is the search engine.
WHAT is the search string.
In interactive use those are prompted for.
Default for WHAT is then what `word-at-point' gives.
However if region is active the region will be default.

WHAT will be url encoded.
ENGINE must be key in `search-net-search-setup-alist'."
  (interactive
     (let* ((engine (search-net-read-engine nil))
            (what (search-net-read-search-string engine nil)))
       (list engine what)))
  (if what
      (browse-url (search-net-make-url engine what))
    (message "No search string given")))

;; (browse-url (search-net-make-url 'google-code "nxhtml"))
;; (browse-url (search-net-make-url 'google "nxhtml"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; search-net.el ends here
