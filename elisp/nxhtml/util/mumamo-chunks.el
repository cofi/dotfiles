;;; mumamo-chunks.el --- Chunk dividing routines etc
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-05-13 Thu
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   Cannot open load file: mumamo-chunks.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Chunk dividing routines must be compiled already when calling
;; `define-mumamo-multi-major-mode'.  Since I can't figure out how to
;; do that in the same file I keep those here instead.
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

(eval-when-compile (require 'cl))
;;(eval-when-compile (add-to-list 'load-path default-directory))
(eval-when-compile (require 'mumamo))
(eval-when-compile (require 'sgml-mode))
(declare-function nxhtml-validation-header-mode "nxhtml-mode")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mumamo Comments Mode
;;
;; Help mode for chunks.
(defconst mumamo-comment-font-lock-keywords
  (list
   (cons "\\(.*\\)" (list 1 font-lock-comment-face))
   ))
(defvar mumamo-comment-font-lock-defaults
  '(mumamo-comment-font-lock-keywords t t))

(define-derived-mode mumamo-comment-mode nil "Comment chunk"
  "For comment blocks."
  (set (make-local-variable 'font-lock-defaults) mumamo-comment-font-lock-defaults))

;; See bug nXhtml bug 610648.
;; The keymap need to be changed in borders.
(define-derived-mode mumamo-border-mode nil "MuMaMo border"
  "For MuMaMo borders.
This is just for the keyboard, not for the fontification."
  ;;(set (make-local-variable 'font-lock-defaults) mumamo-comment-font-lock-defaults)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; File wide key bindings

(defun mumamo-multi-mode-map ()
  "Return mumamo multi mode keymap."
  (symbol-value
   (intern-soft (concat (symbol-name mumamo-multi-major-mode) "-map"))))

;; (defun mumamo-multi-mode-hook-symbol ()
;;   "Return mumamo multi mode hook symbol."
;;   (intern-soft (concat (symbol-name mumamo-multi-major-mode) "-hook")))

;;;###autoload
(defun mumamo-define-html-file-wide-keys ()
  "Define keys in multi major mode keymap for html files."
  (let ((map (mumamo-multi-mode-map)))
    (when map
      (define-key map [(control ?c) (control ?h) ?b] 'nxhtml-browse-file)
      )))
;; (defun mumamo-add-html-file-wide-keys (hook)
;;   (add-hook hook 'mumamo-define-html-file-wide-keys)
;;   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; No mumamo?

;; Fix-me: Generalize etc.
(add-hook 'nxhtml-mumamo-mode-hook 'mumamo-php-if-all-php)
(add-hook 'html-mumamo-mode-hook 'mumamo-php-if-all-php)
(defun mumamo-php-if-all-php ()
  "Switch to php mode if the whole buffer suits `php-mode'.
These conditions must be fullfilled:
- The buffer must begin with '<?php'.
- There should not be any '?>'.
- There should not be any '<<<'.

This function can be added to `nxhtml-mumamo-mode-hook' and
`html-mumamo-mode-hook'.

Note: This is in response to bug #610470, see URL
`https://bugs.launchpad.net/nxhtml/+bug/610470'.  I am not sure
this is a good thing, but let us test it."
  (when (and buffer-file-name
             (string= "php" (file-name-extension buffer-file-name)))
    (message "Checking if all php...")
    (let ((here (point))
          all-php)
      (save-restriction
        (widen)
        (cond
         ((> 6 (buffer-size))
          (setq all-php t))
         ((string= (buffer-substring 1 6)
                   "<?php")
          (goto-char (point-min))
          (unless (or (search-forward "?>" nil t)
                      (search-forward "<<<" nil t))
            (setq all-php t)
            (goto-char here)))))
      (when all-php
        (php-mode))
      (message (if all-php "... switching to php-mode" "... multi major mode")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chunk search routines for XHTML things

(defun mumamo-chunk-attr= (pos max attr= attr=is-regex attr-regex submode)
  "This should work similar to `mumamo-possible-chunk-forward'.
See `mumamo-chunk-style=' for an example of use.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-chunk-attr=-new pos max attr= attr=is-regex attr-regex submode))

(defun mumamo-chunk-attr=-new-fw-exc-fun (pos max)
  ;;(msgtrc "(mumamo-chunk-attr=-new-fw-exc-fun %s %s)" pos max)
  (save-match-data
    (let ((here (point))
          first-dq
          next-dq
          (this-chunk (mumamo-get-existing-chunk-at pos)))
      (if this-chunk
          (goto-char (overlay-end this-chunk))
        (goto-char (overlay-end mumamo-last-chunk)))
      (setq first-dq (search-forward "\"" max t))
      (unless (bobp)
        (backward-char)
        (condition-case err
            (with-syntax-table (standard-syntax-table)
              (setq next-dq (scan-sexps (point) 1)))
          (error nil)))
      (prog1
          next-dq
        (goto-char here)))))

(defun mumamo-chunk-attr=-new-find-borders-fun (start-border end-border dummy)
  (save-match-data
    (let ((here (point))
          (end2 (when end-border (1- end-border)))
          start2)
      (goto-char start-border)
      (save-match-data
        (setq start2 (search-forward "\"" (+ start-border 200) t)))
      (goto-char here)
      (list start2 end2))))

(defun mumamo-chunk-attr=-new (pos
                               max
                               attr=
                               attr=is-regex
                               attr-regex
                               submode)
  (condition-case err
      (save-match-data
        (let ((here (point))
              (next-attr= (progn
                            ;; fix-me:
                            (if (not attr=is-regex)
                                (goto-char (+ pos (length attr=)))
                              (goto-char pos)
                              (skip-chars-forward "a-zA-Z="))
                            (goto-char pos)
                            (if attr=is-regex
                                (re-search-forward attr= max t)
                              (search-forward attr= max t))))
              next-attr-sure
              start start-border
              end   end-border
              exc-mode
              borders
              exc-start-next
              exc-end-next
              exc-start-next
              exc-end-next
              (tries 0)
              (min (1- pos))
	      look-max
              )
          ;; make sure if we have find prev-attr= or not
          (unless (eq (char-after) ?\")
            (setq next-attr= nil))
          (when next-attr=
            (forward-char)
            (skip-chars-forward "^\"")
            (setq look-max (+ (point) 2)))
          (while (and next-attr=
                      (< min (point))
                      (not next-attr-sure)
                      (< tries 5))
            ;;(msgtrc "attr=-new: min=%s, point=%s" min (point))
            (setq tries (1+ tries))
            ;;(if (not (re-search-backward "<[^?]" (- min 300) t))
            (if (not (re-search-backward "<[^?]\\|\?>" (- min 300) t))
                (setq next-attr= nil)
              ;;(if (looking-at attr-regex)
              (if (let ((here (point)))
                    (prog1
                        (re-search-forward attr-regex look-max t)
                      (goto-char here)))
              ;;(if (mumamo-end-in-code (point) next-attr= 'php-mode)
                  (setq next-attr-sure 'found)
                (unless (bobp)
                  (backward-char)
                  ;;(msgtrc "attr=-new 1: min=%s, point=%s" min (point))
                  (setq next-attr= (if attr=is-regex
                                       (re-search-backward attr= (- min 300) t)
                                     (search-backward attr= (- min 300) t)))))))
          (unless next-attr-sure (setq next-attr= nil))


          ;; find prev change and if inside style= the next change
          (when next-attr=
              (setq exc-start-next (match-beginning 1))
              (setq exc-end-next   (match-end 2))
              (when (>= exc-start-next pos)
                (if (> pos exc-end-next)
                    (progn
                      (setq start (+ (match-end 2) 1))
                      ;;(setq start-border (+ (match-end 2) 2))
                      )
                  (setq exc-mode submode)
                  (setq start (match-beginning 1))
                  (setq start-border (match-beginning 2))
                  (setq end (1+ (match-end 2)))
                  (setq end-border (1- end)))
                ))
          ;; find next change
          (unless end
            (if start
                (goto-char start)
              (goto-char pos)
              (search-backward "<" min t))
            ;;(msgtrc "attr=-new 2: min=%s, point=%s" min (point))
            (setq next-attr= (if attr=is-regex
                                 (re-search-forward attr= max t)
                               (search-forward attr= max t)))
            (when (and next-attr=
                       (search-backward "<" min t))
              (when (looking-at attr-regex)
                (setq end (match-beginning 1)))))
          (when start (assert (>= start pos) t))
          (when end   (assert (<= pos end) t))
          ;;(message "start-border=%s end-border=%s" start-border end-border)
          (when (or start-border end-border)
            (setq borders (list start-border end-border nil)))
          ;; (message "mumamo-chunk-attr=-new: %s"
          ;;          (list start
          ;;                end
          ;;                exc-mode
          ;;                borders
          ;;                nil ;; parseable-by
          ;;                'mumamo-chunk-attr=-new-fw-exc-fun ;; fw-exc-fun
          ;;                'mumamo-chunk-attr=-new-find-borders-fun ;; find-borders-fun
          ;;                ))
          (goto-char here)
          (setq end nil)
          (when (or start end)
            (list start
                  end
                  exc-mode
                  borders
                  nil ;; parseable-by
                  'mumamo-chunk-attr=-new-fw-exc-fun ;; fw-exc-fun
                  'mumamo-chunk-attr=-new-find-borders-fun ;; find-borders-fun
                  ))))
    (error (mumamo-display-error 'mumamo-chunk-attr=-new "%s" (error-message-string err)))
    ))

;;;; xml pi

(defvar mumamo-xml-pi-mode-alist
  '(("php"    . php-mode)
    ("python" . python-mode))
  "Alist used by `mumamo-chunk-xml-pi' to get chunk mode." )

(defun mumamo-search-fw-exc-start-xml-pi-new (pos max)
  (let ((here (point))
        start
        spec
        exc-mode
        ret)
    (setq start (search-forward "<?" max t))
    (when (and start
               (looking-at (rx (0+ (any "a-z")))))
      (setq spec (match-string-no-properties 0))
      (unless (string= spec "xml")
        (when (= 0 (length spec))
          (setq spec "php"))
        (setq exc-mode (assoc spec mumamo-xml-pi-mode-alist))
        (if exc-mode
            (setq exc-mode (cdr exc-mode))
          (setq exc-mode 'mumamo-bad-mode))
        (setq ret (list (- start 2) exc-mode nil))))
    (goto-char here)
    ret))

(defun mumamo-xml-pi-end-is-xml-end (pos)
  "Return t if the ?> at pos is end of <?xml."
  (when (> 1000 pos)
;;;     (assert (and (= (char-after pos) ??)
;;;                  (= (char-after (1+ pos)) ?>)))
    (save-excursion
      (save-restriction
        (widen)
        (save-match-data
          (when (search-backward "<" (- pos 150) t)
            (when (looking-at (rx line-start "<\?xml" (1+ space)))
              (mumamo-msgfntfy "mumamo-xml-pi-end-is-xml-end %s => t" pos)
              t)))))))

(defun mumamo-search-fw-exc-end-xml-pi (pos max)
  "Helper for `mumamo-chunk-xml-pi'.
POS is where to start search and MAX is where to stop."
  ;; Fix me: merge xml header
  ;;(let ((end-pos (mumamo-chunk-end-fw-str pos max "?>")))
  (save-match-data
    (let ((end-pos (mumamo-chunk-end-fw-str-inc pos max "?>")))
      (when end-pos
        (unless (mumamo-xml-pi-end-is-xml-end end-pos)
          end-pos)))))

(defun mumamo-search-fw-exc-start-xml-pi-1 (pos max lt-chars)
  "Helper for `mumamo-chunk-xml-pi'.
POS is where to start search and MAX is where to stop.

Used in `mumamo-search-fw-exc-start-xml-pi'.  For an explanation
of LT-CHARS see `mumamo-search-bw-exc-start-xml-pi-1'."
  (goto-char pos)
  (skip-chars-backward "a-zA-Z")
  ;;(let ((end-out (mumamo-chunk-start-fw-str (point) max lt-chars)))
  (let ((end-out (mumamo-chunk-start-fw-str-inc (point) max lt-chars))
        spec
        exc-mode
        hit)
    (when (looking-at "xml")
      (if t ;(= 1 pos)
          (setq end-out (mumamo-chunk-start-fw-str-inc (1+ (point)) max lt-chars))
        (setq end-out nil)))
    (when end-out
      ;; Get end-out:
      (if (looking-at (rx (0+ (any "a-z"))))
          (progn
            ;;(setq end-out (match-end 0))
            (setq end-out (- (match-beginning 0) 2))
            (setq spec (match-string-no-properties 0))
            (setq exc-mode (assoc spec mumamo-xml-pi-mode-alist))
            (if exc-mode
                (setq exc-mode (cdr exc-mode))
              (setq exc-mode 'php-mode))
            (setq end-out (list end-out exc-mode nil))
            )
        (setq end-out nil))
      end-out)))

(defun mumamo-search-fw-exc-start-xml-pi-old (pos max)
  "Helper for `mumamo-chunk-xml-pi'.
POS is where to start search and MAX is where to stop."
  (mumamo-search-fw-exc-start-xml-pi-1 pos max "<?"))

(defun mumamo-find-borders-xml-pi (start end exc-mode)
  (let (start-border
        end-border
        (inc t)
        ;;(begin-mark "<?php")
        (begin-mark "<?")
        (end-mark "?>")
        (here (point)))
    (if (and inc) ;; exc-mode)
        (progn
          (when start
            ;;(setq start-border (+ start (length begin-mark)))
            (goto-char (+ start (length begin-mark)))
            (skip-chars-forward "=a-zA-Z")
            (setq start-border (point))
            )
          (when end
            (setq end-border
                  (- end (length end-mark)))))
      (if (and (not inc) (not exc-mode))
          (progn
            (when start
              (setq start-border
                    (+ start (length end-mark))))
            (when end
              (setq end-border (- end (length begin-mark)))
              ;;(goto-char end)
              ;;(skip-chars-forward "=a-zA-Z")
              ;;(setq end-border (point))
              ))))
    (goto-char here)
    (when (or start-border end-border)
      (list start-border end-border))))

(defun mumamo-chunk-xml-pi (pos max)
  "Find process instruction, <? ... ?>.  Return range and wanted mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-xml-pi-new
                                 'mumamo-search-fw-exc-end-xml-pi
                                 'mumamo-find-borders-xml-pi))


;;;; <style ...>

(defconst mumamo-style-tag-start-regex
  (rx "<style"
      space
      (0+ (not (any ">")))
      "type"
      (0+ space)
      "="
      (0+ space)
      ?\"
      "text/css"
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[")
      ))


(defun mumamo-search-fw-exc-end-inlined-style (pos max)
  "Helper for `mumamo-chunk-inlined-style'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</style>")))

(defun mumamo-search-fw-exc-start-inlined-style (pos max)
  "Helper for `mumamo-chunk-inlined-style'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<style" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 6))
      (when (looking-at mumamo-style-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'css-mode nil)
        ))))

(defun mumamo-chunk-inlined-style (pos max)
  "Find <style>...</style>.  Return range and 'css-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-style
                                 'mumamo-search-fw-exc-end-inlined-style))

;;;; <script ...>

(defconst mumamo-script-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "type"
      (0+ space)
      "="
      (0+ space)
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      (or "'text/javascript'" "\"text/javascript\"")
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-fw-exc-end-inlined-script (pos max)
  "Helper for `mumamo-chunk-inlined-script'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</script>")))

(defun mumamo-search-fw-exc-start-inlined-script (pos max)
  "Helper for `mumamo-chunk-inlined-script'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<script" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-script-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode '(nxml-mode))
        ))))

(defun mumamo-chunk-inlined-script (pos max)
  "Find <script>...</script>.  Return range and 'javascript-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-script
                                 'mumamo-search-fw-exc-end-inlined-script))

;;;; on[a-z]+=\"javascript:"

(defconst mumamo-onjs=-attr=
  (rx
   ;;"on[a-z]+="
   (or "onclick" "ondblclick" "onmousedown" "onmousemove" "onmouseout" "onmouseover" "onmouseup" "onkeydown" "onkeypress" "onkeyup")
   "="))

(defconst mumamo-onjs=-attr-regex
  (rx point
      (or "<" "?>")
      (* (not (any ">")))
      space
      (submatch
       ;;"on" (1+ (any "a-za-z"))
       (or "onclick" "ondblclick" "onmousedown" "onmousemove" "onmouseout" "onmouseover" "onmouseup" "onkeydown" "onkeypress" "onkeyup")
       "=")
      (0+ space)
      ?\"
      (submatch
       (opt "javascript:")
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-onjs=(pos max)
  "Find javascript on...=\"...\".  Return range and 'javascript-mode."
  (mumamo-chunk-attr= pos max mumamo-onjs=-attr= t mumamo-onjs=-attr-regex
                      'javascript-mode))

;;;; py:somthing=\"python\"

(defconst mumamo-py:=-attr= "py:[a-z]+=")

(defconst mumamo-py:=-attr-regex
  (rx point
      (or "<" "?>")
      (* (not (any ">")))
      space
      (submatch
       "py:" (1+ (any "a-za-z"))
       "=")
      (0+ space)
      ?\"
      (submatch
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-py:=(pos max)
  "Find python py:...=\"...\".  Return range and 'python-mode."
  (mumamo-chunk-attr= pos max mumamo-py:=-attr= t mumamo-py:=-attr-regex
                      'python-mode))

(defun mumamo-chunk-py:match (pos max)
  (save-match-data
    (let ((here (point))
          (py:match (progn
                      (goto-char pos)
                      (re-search-forward (rx "py:match"
                                             (1+ space)
                                             (0+ (not (any ">")))
                                             word-start
                                             (submatch "path=")
                                             (0+ space)
                                             ?\"
                                             (submatch
                                              (0+
                                               (not (any "\"")))))
                                         max t)))
          start end borders
          )
      (when py:match
        (setq start (match-beginning 1))
        (setq end   (match-end 2))
        (setq borders (list (match-end 1) (1- end)))
        )
      (goto-char here)
      (when start
        (list start
              end
              'python-mode
              borders
              nil ;; parseable-by
              'mumamo-chunk-attr=-new-fw-exc-fun ;; fw-exc-fun
              'mumamo-chunk-attr=-new-find-borders-fun ;; find-borders-fun
            )))))

;;;; style=

(defconst mumamo-style=start-regex
  (rx "<"
      (0+ (not (any ">")))
      space
      (submatch "style=")
      (0+ space)
      ?\"
      (submatch
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-style=(pos max)
  "Find style=\"...\".  Return range and 'css-mode."
  (mumamo-chunk-attr= pos max "style=" nil mumamo-style=start-regex
                      'css-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HTML w html-mode

(put 'mumamo-alt-php-tags-mode 'permanent-local t)
(define-minor-mode mumamo-alt-php-tags-mode
  "Minor mode for using '(?php' instead of '<?php' in buffer.
When turning on this mode <?php is replace with (?php in the buffer.
If you write the buffer to file (?php is however written as <?php.

When turning off this mode (?php is replace with <?php in the buffer.

The purpose of this minor mode is to work around problems with
using the `nxml-mode' parser in php files.  `nxml-mode' knows
damned well that you can not have the character < in strings and
I can't make it forget that.  For PHP programmers it is however
very convient to use <?php ... ?> in strings.

There is no reason to use this minor mode unless you want XML
validation and/or completion in your php file.  If you do not
want that then you can simply use a multi major mode based on
`html-mode' instead of `nxml-mode'/`nxhtml-mode'.  Or, of course,
just `php-mode' if there is no html code in the file."
  :lighter "<?php "
  (if mumamo-alt-php-tags-mode
      (progn
        ;;(unless mumamo-multi-major-mode (error "Only for mumamo multi major modes"))
        (unless (let ((major-mode (mumamo-main-major-mode)))
                  (derived-mode-p 'nxml-mode))
          ;;(error "Mumamo multi major mode must be based on nxml-mode")
          )
        (unless (memq 'mumamo-chunk-alt-php (caddr mumamo-current-chunk-family))
          (error "Mumamo multi major must have chunk function mumamo-chunk-alt-php"))

        ;; Be paranoid about the file/content write hooks
        (when (<= emacs-major-version 22)
          (with-no-warnings
            (when local-write-file-hooks ;; obsolete, but check!
              (error "Will not do this because local-write-file-hooks is non-nil"))))
        (remove-hook 'write-contents-functions 'mumamo-alt-php-write-contents t)
        (when write-contents-functions
          (error "Will not do this because write-contents-functions is non-nil=%S" write-contents-functions))
        (let ((wff (delq 'recentf-track-opened-file (copy-sequence write-file-functions))))
          (when wff
            (error "Will not do this because write-file-functions is non-nil=%S" wff)))

        (add-hook 'write-contents-functions 'mumamo-alt-php-write-contents t t)
        (put 'write-contents-functions 'permanent-local t)
        (save-restriction
          (let ((here (point)))
            (widen)
            (goto-char (point-min))
            (while (search-forward "<?php" nil t)
              (replace-match "(?php"))
            (goto-char (point-min))
            (while (search-forward "<?=" nil t)
              (replace-match "(?="))
            (goto-char (point-min))
            (while (search-forward "?>" nil t)
                (replace-match "?)"))
            (goto-char here))))
    (save-restriction
      (let ((here (point)))
        (widen)
        (goto-char (point-min))
        (while (search-forward "(?php" nil t)
          (replace-match "<?php"))
        (goto-char (point-min))
        (while (search-forward "(?=" nil t)
          (replace-match "<?="))
        (goto-char (point-min))
        (while (search-forward "?)" nil t)
          (replace-match "?>"))
        (goto-char here)))
    (remove-hook 'write-contents-functions 'mumamo-alt-php-write-contents t)))

(defun mumamo-chunk-alt-php (pos max)
  "Find (?php ... ?), return range and `php-mode'.
Workaround for the problem that I can not tame `nxml-mode' to recognize <?php.

See `mumamo-possible-chunk-forward' for POS and MAX."
  (when mumamo-alt-php-tags-mode
    (mumamo-quick-chunk-forward pos max "(?php" "?)" 'borders 'php-mode)))

(defun mumamo-chunk-alt-php= (pos max)
  "Find (?= ... ?), return range and `php-mode'.
Workaround for the problem that I can not tame `nxml-mode' to recognize <?php.

See `mumamo-possible-chunk-forward' for POS and MAX."
  (when mumamo-alt-php-tags-mode
    (mumamo-quick-chunk-forward pos max "(?=" "?)" 'borders 'php-mode)))


(defun mumamo-alt-php-write-contents ()
  "For `write-contents-functions' when `mumamo-chunk-alt-php' is used."
  (let ((here (point)))
    (save-match-data
      (save-restriction
        (widen)
        (condition-case nil
            (atomic-change-group
              (progn
                (goto-char (point-min))
                (while (search-forward "(?php" nil t)
                  (replace-match "<?php"))
                (goto-char (point-min))
                (while (search-forward "(?=" nil t)
                  (replace-match "<?="))
                (goto-char (point-min))
                (while (search-forward "?)" nil t)
                  (replace-match "?>"))
                (basic-save-buffer-1)
                (signal 'mumamo-error-ind-0 nil)))
          (mumamo-error-ind-0)))
      (set-buffer-modified-p nil))
    (goto-char here))
  ;; saved, return t
  t)

;; Fix-me: does not work with new chunk div
(defun mumamo-whole-line-chunk-fw-exc-end-fun (pos max)
  (let ((here (point)))
    (goto-char pos)
    (prog1
        (line-end-position)
      (goto-char here))))

(defun mumamo-whole-line-chunk (pos max marker mode)
  (let* ((here (point))
         (len-marker (length marker))
         (pattern (rx-to-string `(and bol (0+ blank) ,marker blank) t))
         (whole-line-chunk-borders-fun
          `(lambda (start end dummy)
             (let ((start-border (+ start ,len-marker)))
               (list start-border nil))))
         beg
         end
         ret)
    (goto-char pos)
    (setq beg (re-search-forward pattern max t))
    (when beg
      (setq beg (- beg len-marker 1))
      (setq end (line-end-position))
      (setq ret (list beg
                      end
                      mode
                      (let ((start-border (+ beg len-marker)))
                        (list start-border nil))
                      nil
                      'mumamo-whole-line-chunk-fw-exc-end-fun
                      whole-line-chunk-borders-fun
                      )))
    (goto-char here)
    ret))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mason (not quite ready)
;; http://www.masonhq.com/docs/manual/Devel.html#examples_and_recommended_usage

(defun mumamo-chunk-mason-perl-line (pos max)
  (mumamo-whole-line-chunk pos max "%" 'perl-mode))

(defun mumamo-chunk-mason-perl-single (pos max)
  (mumamo-quick-chunk-forward pos max "<% " " %>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-perl-block (pos max)
  (mumamo-quick-chunk-forward pos max "<%perl>" "</%perl>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-perl-init (pos max)
  (mumamo-quick-chunk-forward pos max "<%init>" "</%init>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-perl-once (pos max)
  (mumamo-quick-chunk-forward pos max "<%once>" "</%once>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-perl-cleanup (pos max)
  (mumamo-quick-chunk-forward pos max "<%cleanup>" "</%cleanup>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-perl-shared (pos max)
  (mumamo-quick-chunk-forward pos max "<%shared>" "</%shared>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-simple-comp (pos max)
  (mumamo-quick-chunk-forward pos max "<& " " &>" 'borders 'text-mode))

(defun mumamo-chunk-mason-args (pos max)
  ;; Fix-me: perl-mode is maybe not the best here?
  (mumamo-quick-chunk-forward pos max "<%args>" "</%args>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-doc (pos max)
  (mumamo-quick-chunk-forward pos max "<%doc>" "</%doc>" 'borders 'mumamo-comment-mode))

(defun mumamo-chunk-mason-text (pos max)
  (mumamo-quick-chunk-forward pos max "<%text>" "</%text>" 'borders 'text-mode))

;; component calls with content

(defun mumamo-chunk-mason-compcont-fw-exc-end-fun (pos max)
  (mumamo-chunk-end-fw-str-inc pos max "</&>"))

(defun mumamo-chunk-mason-compcont-find-borders-fun (start end dummy)
  (when dummy
    (list
     (when start
       (save-match-data
         (let ((here (point))
               ret)
           (goto-char start)
           (when (re-search-forward "[^>]* &>" end t)
             (setq ret (point))
             (goto-char here)
             ret))
         ))
     (when end (- end 4))
     dummy)))

(defun mumamo-chunk-mason-compcont-fw-exc-start-fun (pos max)
  (let ((where (mumamo-chunk-start-fw-str-inc pos max "<&| ")))
    (when where
      (list where 'html-mode nil))))

(defun mumamo-chunk-mason-compcont (pos max)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-chunk-mason-compcont-fw-exc-start-fun
                                 'mumamo-chunk-mason-compcont-fw-exc-end-fun
                                 'mumamo-chunk-mason-compcont-find-borders-fun))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Embperl

(defun mumamo-chunk-embperl-<- (pos max)
  "Find [- ... -], return range and `perl-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "[-" "-]" 'borders 'perl-mode))

(defun mumamo-chunk-embperl-<+ (pos max)
  "Find [+ ... +], return range and `perl-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "[+" "+]" 'borders 'perl-mode))

(defun mumamo-chunk-embperl-<! (pos max)
  "Find [! ... !], return range and `perl-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "[!" "!]" 'borders 'perl-mode))

(defun mumamo-chunk-embperl-<$ (pos max)
  "Find [$ ... $], return range and `perl-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  ;; This is a bit tricky since [$var] etc must be avoided.
  (let* ((begin-mark "[$")
         (end-mark "$]")
         (good-chars '(32 ;space
                       10 ;line feed
                       9  ;tab
                       ))
         (search-fw-exc-start (lambda (pos max)
                                (let ((not-found t)
                                      (next-char nil)
                                      (exc-start (mumamo-chunk-start-fw-str
                                                  pos max begin-mark))
                                      (here (point)))
                                  (while (and not-found
                                              exc-start)
                                    (setq next-char (char-after))
                                    (if (memq next-char good-chars)
                                        (setq not-found nil)
                                      (setq exc-start
                                            (search-forward begin-mark
                                                            max t))))
                                  (when exc-start (list exc-start 'perl-mode)))))
         (search-fw-exc-end (lambda (pos max)
                              (save-match-data
                                (mumamo-chunk-end-fw-str pos max end-mark)))))
    (mumamo-possible-chunk-forward pos max
                                   search-fw-exc-start
                                   search-fw-exc-end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; django

(defun mumamo-chunk-django4(pos max)
  "Find {% comment %}.  Return range and `django-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{% comment %}" "{% endcomment %}" 'borders 'mumamo-comment-mode))

(defun mumamo-chunk-django3(pos max)
  "Find {# ... #}.  Return range and `django-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{#" "#}" 'borders 'mumamo-comment-mode))

(defun mumamo-chunk-django2(pos max)
  "Find {{ ... }}.  Return range and `django-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{{" "}}" 'borders 'django-variable-mode))

(defun mumamo-chunk-django (pos max)
  "Find {% ... %}.  Return range and `django-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk (mumamo-quick-chunk-forward pos max "{%" "%}" 'borders 'django-mode)))
    (when chunk
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-search-fw-exc-start-django (pos max)
  "Helper for `mumamo-chunk-django'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{%"))

(defun mumamo-search-fw-exc-start-django2(pos max)
  "Helper for `mumamo-chunk-django2'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{{"))

(defun mumamo-search-fw-exc-start-django3(pos max)
  "Helper for `mumamo-chunk-django3'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{#"))

(defun mumamo-search-fw-exc-start-django4(pos max)
  "Helper for `mumamo-chunk-django4'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{% comment %}"))

(defun mumamo-search-fw-exc-end-django (pos max)
  "Helper for `mumamo-chunk-django'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "%}"))

(defun mumamo-search-fw-exc-end-django2(pos max)
  "Helper for `mumamo-chunk-django2'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "}}"))

(defun mumamo-search-fw-exc-end-django3(pos max)
  "Helper for `mumamo-chunk-django3'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "#}"))

(defun mumamo-search-fw-exc-end-django4(pos max)
  "Helper for `mumamo-chunk-django4'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "{% endcomment %}"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Genshi / Kid

;; {% python ... %}
(defun mumamo-chunk-genshi%(pos max)
  "Find {% python ... %}.  Return range and `genshi-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{% python" "%}" 'borders 'python-mode))

;; ${expr}
(defun mumamo-chunk-genshi$(pos max)
  "Find ${ ... }, return range and `python-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk
         (mumamo-quick-chunk-forward pos max "${" "}" 'borders 'python-mode)))
    (when chunk
      ;; Test for clash with %}
      (let ((sub-mode (nth 2 chunk))
            (start (nth 0 chunk)))
        (if sub-mode
            chunk
          ;;(message "point.1=%s" (point))
          (when (and start
                     (eq ?% (char-before start)))
            ;;(message "point.2=%s" (point))
            ;;(message "clash with %%}, chunk=%s" chunk)
            ;;(setq chunk nil)
            (setcar chunk (1- start))
            )
          ;;(message "chunk.return=%s" chunk)
          chunk)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MJT

;; ${expr}
(defun mumamo-chunk-mjt$(pos max)
  "Find ${ ... }, return range and `javascript-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "${" "}" 'borders 'javascript-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smarty

(defun mumamo-chunk-smarty-literal (pos max)
  "Find {literal} ... {/literal}.  Return range and 'html-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{literal}" "{/literal}" 'borders 'html-mode))

(defun mumamo-chunk-smarty-t (pos max)
  "Find {t} ... {/t}.  Return range and 'html-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{t}" "{/t}" 'borders 'text-mode))

(defun mumamo-chunk-smarty-comment (pos max)
  "Find {* ... *}.  Return range and 'mumamo-comment-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{*" "*}" t 'mumamo-comment-mode))

(defun mumamo-chunk-smarty (pos max)
  "Find { ... }.  Return range and 'smarty-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{" "}" t 'smarty-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ssjs - server side javascript

(defun mumamo-chunk-ssjs-% (pos max)
  "Find <% ... %>.  Return range and 'javascript-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "<%" "%>" 'borders 'javascript-mode))

(defconst mumamo-ssjs-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "runat"
      (0+ space)
      "="
      (0+ space)
      ?\"
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      (or "server" "both" "server-proxy")
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))


(defun mumamo-search-fw-exc-start-inlined-ssjs (pos max)
  "Helper for `mumamo-chunk-inlined-ssjs'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<script" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-ssjs-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode)
        ))))

(defun mumamo-chunk-inlined-ssjs (pos max)
  "Find <script runat=...>...</script>.  Return range and 'javascript-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-ssjs
                                 'mumamo-search-fw-exc-end-inlined-script))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gsp

(defun mumamo-chunk-gsp (pos max)
  "Find <% ... %>.  Return range and 'groovy-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "<%" "%>" 'borders 'groovy-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jsp - Java Server Pages

(defun mumamo-chunk-jsp (pos max)
  "Find <% ... %>.  Return range and 'java-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "<%" "%>" 'borders 'java-mode))

(defun mumamo-chunk-jsp-hidden-comment (pos max)
  "Find <%-- ... --%>.  Return range and 'mumamo-comment-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "<%--" "--%>" 'borders 'mumamo-comment-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; eruby

(defun mumamo-chunk-eruby (pos max)
  "Find <% ... %>.  Return range and 'ruby-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk (mumamo-quick-chunk-forward pos max '("<%-?" . t) '("-?%>" . t) 'borders 'ruby-mode)))
    (when chunk
      ;; Put indentation type on 'mumamo-next-indent on the chunk:
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-eruby= (pos max)
  "Find <%= ... %>.  Return range and 'ruby-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk (mumamo-quick-chunk-forward pos max "<%=" '("-?%>" . t) 'borders 'ruby-mode)))
    (when chunk
      ;; Put indentation type on 'mumamo-next-indent on the chunk.
      ;; See nXhtml bug 579581 for a case where it is needed.
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-eruby=quoted (pos max)
  "Find \"<%= ... %>\".  Return range and 'ruby-mode.
See `mumamo-possible-chunk-forward' for POS and MAX.

This is a workaround for problems with strings."
  (let ((chunk (mumamo-quick-chunk-forward pos max "\"<%=" '("-?%>\"" . t) 'borders 'ruby-mode)))
    (when chunk
      ;; Put indentation type on 'mumamo-next-indent on the chunk:
      ;; Fix-me: use this!
      ;;(setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-eruby-comment (pos max)
  "Find <%# ... %>.  Return range and 'ruby-mode.
See `mumamo-possible-chunk-forward' for POS and MAX.

This is needed since otherwise the end marker is thought to be
part of a comment."
  (mumamo-quick-chunk-forward pos max "<%#" "%>" 'borders 'mumamo-comment-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; heredoc

(defcustom mumamo-heredoc-modes
  '(
    ("HTML" html-mode)
    ("CSS" css-mode)
    ("JAVASCRIPT" javascript-mode)
    ("JAVA" java-mode)
    ("GROOVY" groovy-mode)
    ("SQL" sql-mode)
    )
  "Matches for heredoc modes.
The entries in this list have the form

  (REGEXP MAJOR-MODE-SPEC)

where REGEXP is a regular expression that should match the
heredoc marker line and MAJOR-MODE-SPEC is the major mode spec to
use in the heredoc part.

The major mode spec is translated to a major mode using
`mumamo-major-mode-from-modespec'."
  :type '(repeat
          (list
           regexp
           (function :tag "Major mode")))
  :group 'mumamo-modes)

(defun mumamo-mode-for-heredoc (marker)
  "Return major mode associated with MARKER.
Use first match in `mumamo-heredoc-modes'.
If no match use `text-mode'."
  (let ((mode (catch 'mode
                (save-match-data
                  (dolist (rec mumamo-heredoc-modes)
                    (let ((regexp (nth 0 rec))
                          (mode   (nth 1 rec)))
                      (when (string-match regexp marker)
                        (throw 'mode mode))))))))
    (if mode
        (mumamo-major-mode-from-modespec mode)
      'text-mode)))

(defun mumamo-chunk-heredoc (pos max lang)
  "This should work similar to `mumamo-possible-chunk-forward'.
POS and MAX have the same meaning as there.

LANG is the programming language.
Supported values are 'perl."
  ;; Fix-me: LANG
  ;; Fix-me: use mumamo-end-in-code
  (mumamo-condition-case err
      (let ((old-point (point)))
        (goto-char pos)
        (beginning-of-line)
        (let (next-<<
              (want-<< t)
              heredoc-mark
              end-mark-len
              heredoc-line
              delimiter
              skipped
              (skip-b "")
              start-inner
              end
              exc-mode
              fw-exc-fun
              border-fun
              allow-code-after
              start-outer
              ps
              )
          (goto-char pos)
          (beginning-of-line)
          (case lang
            ('sh
             (setq allow-code-after t)
             (while want-<<
               (setq next-<< (search-forward "<<" max t))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (point)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 2))
               (when (= (char-after) ?-)
                 (setq skip-b "\t*")
                 (unless (eolp) (forward-char)))
               ;; fix-me: space
               (setq skipped (skip-chars-forward " \t"))
               (when (memq (char-after) '(?\" ?\'))
                 (setq delimiter (list (char-after))))
               (if (and (> skipped 0) (not delimiter))
                   (setq heredoc-mark "")
                 (when (looking-at (rx-to-string
                                    `(and (regexp ,(if delimiter
                                                       (concat delimiter "\\([^\n<>;]+\\)" delimiter)
                                                     "\\([^ \t\n<>;]+\\)"))
                                          (or blank line-end))))
                   (setq heredoc-mark  (buffer-substring-no-properties
                                        (match-beginning 1)
                                        (match-end 1)))))
               (when heredoc-mark
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 (setq start-inner (1+ (point-at-eol)))
                 (setq end-mark-len (length heredoc-mark))
                 )))
            ('w32-ps (error "No support for windows power shell yet"))
            ('php
             (while want-<<
               (setq next-<< (search-forward "<<<" max t))
               ;; Check inside string or comment.
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 (setq ps (parse-partial-sexp (line-beginning-position) (- (point) 0)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 3))
               (skip-chars-forward " \t")
               (when (looking-at (concat "\\([^\n;]*\\)[[:blank:]]*\n"))
                 (setq heredoc-mark  (buffer-substring-no-properties
                                      (match-beginning 1)
                                      (match-end 1)))
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 ;; fix-me: nowdoc
                 (when (and (= ?\' (string-to-char heredoc-mark))
                            (= ?\' (string-to-char (substring heredoc-mark (1- (length heredoc-mark))))))
                   (setq heredoc-mark (substring heredoc-mark 1 (- (length heredoc-mark) 1))))
                 (setq end-mark-len (1+ (length heredoc-mark)))
                 (setq start-inner (match-end 0)))))
            ('perl
             (setq allow-code-after t)
             (while want-<<
               (setq next-<< (search-forward "<<" max t))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (point)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 2))
               ;; fix-me: space
               (setq skipped (skip-chars-forward " \t"))
               (when (memq (char-after) '(?\" ?\'))
                 (setq delimiter (list (char-after))))
               (if (and (> skipped 0) (not delimiter))
                   (setq heredoc-mark "") ;; blank line
                 (when (looking-at (rx-to-string
                                    `(and (regexp ,(if delimiter
                                                       (concat delimiter "\\([^\n;]*\\)" delimiter)
                                                     "\\([^ \t\n<>;]+\\)"))
                                          (or blank ";"))))
                   (setq heredoc-mark  (buffer-substring-no-properties
                                        (match-beginning 1)
                                        (match-end 1)))))
               (when heredoc-mark
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 ;;(setq start-inner (1+ (match-end 0)))
                 (setq start-inner (1+ (point-at-eol)))
                 (setq end-mark-len (length heredoc-mark))
                 )))
            ('python
             (unless (eobp) (forward-char))
             (while want-<<
               (setq next-<< (re-search-forward "\"\"\"\\|'''" max t))
               (setq start-outer (- (point) 3))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (- (point) 3)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil)))))
            ('ruby
             (while want-<<
               (setq next-<< (search-forward "<<" max t))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (point)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 2))
               (when (= (char-after) ?-)
                 (setq skip-b "[ \t]*")
                 (forward-char))
               (when (looking-at (concat "[^\n[:blank:]]*"))
                 (setq heredoc-mark  (buffer-substring-no-properties
                                      (match-beginning 0)
                                      (match-end 0)))
                 (setq end-mark-len (length heredoc-mark))
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 (setq start-inner (match-end 0)))))
            (t (error "next-<< not implemented for lang %s" lang)))
          (when start-inner (assert (<= pos start-inner) t))
          (goto-char old-point)
          (when (or start-inner end)
            (let ((endmark-regexp
                   (case lang
                     ('sh (concat "^" skip-b heredoc-mark "$"))
                     ('php (concat "^" heredoc-mark ";?$"))
                     ('perl (concat "^" heredoc-mark "$"))
                     ('python (concat "^" heredoc-mark "[[:space:]]*"))
                     ('ruby (concat "^" skip-b heredoc-mark "$"))
                     (t (error "mark-regexp not implemented for %s" lang)))))
              (setq border-fun `(lambda (start end exc-mode)
                                  (list
                                   (if ,allow-code-after nil (+ start (- ,start-inner ,start-outer 1)))
                                   (when end (- (1- end) ,end-mark-len)))))
              (setq fw-exc-fun `(lambda (pos max)
                                  (save-match-data
                                    (let ((here (point)))
                                      (goto-char pos)
                                      (prog1
                                          (when (re-search-forward ,endmark-regexp max t)
                                            ;;(- (point) 1 ,(length heredoc-mark))
                                            ;; Extend heredoc chunk
                                            ;; until after newline to
                                            ;; avoid the "syntax-table
                                            ;; (15)" entry on the
                                            ;; newline char in
                                            ;; `sh-mode':
                                            (+ (point) 1)
                                            )
                                        (goto-char here)))))))
            (setq exc-mode (mumamo-mode-for-heredoc heredoc-line))
            (list start-inner end exc-mode nil nil fw-exc-fun nil)
            ;; Fix me: Add overriding for inner chunks (see
            ;; http://www.emacswiki.org/emacs/NxhtmlMode#toc13). Maybe
            ;; make fw-exc-fun a list (or a cons, since overriding is
            ;; probably all that I want to add)? And make the
            ;; corresponding chunk property a list too?
            ;;(list start-outer end exc-mode (list start-inner end) nil fw-exc-fun border-fun 'heredoc)
            (list (if allow-code-after start-inner start-outer)
                  end exc-mode (list start-inner end) nil fw-exc-fun border-fun 'heredoc)
            )))
    (error (mumamo-display-error 'mumamo-chunk-heredoc
                                 "%s" (error-message-string err)))))

;;;; Unix style sh heredoc

(defun mumamo-chunk-sh-heredoc (pos max)
  "Find sh here docs.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((r (mumamo-chunk-heredoc pos max 'sh)))
    r))

;;;; PHP heredoc

(defun mumamo-chunk-php-heredoc (pos max)
  "Find PHP here docs.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((r (mumamo-chunk-heredoc pos max 'php)))
    r))

;;;; Perl heredoc

(defun mumamo-chunk-perl-heredoc (pos max)
  "Find perl here docs.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((r (mumamo-chunk-heredoc pos max 'perl)))
    r))

;;;; Python heredoc

(defun mumamo-chunk-python-heredoc (pos max)
  "Find python here docs.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((r (mumamo-chunk-heredoc pos max 'python)))
    r))

;;;; Ruby heredoc

(defun mumamo-chunk-ruby-heredoc (pos max)
  "Find Ruby here docs.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((r (mumamo-chunk-heredoc pos max 'ruby)))
    r))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tex meta

(defconst mumamo-textext-end-regex
  (rx "textext("
      (0+
       (0+ (not (any "\"()")))
       ?\"
       (0+ (not (any "\"")))
       ?\"
       )
      (0+ (not (any "\"()")))
      ")"))

(defun mumamo-textext-test-is-end (pos)
  "Helper for `mumamo-chunk-textext'.
Return POS if POS is at the end of textext chunk."
  (when pos
    (let ((here (point))
          hit)
      (goto-char (+ 2 pos))
      (when (looking-back mumamo-textext-end-regex)
        (setq hit t))
      (goto-char here)
      (when hit pos))))


(defun mumamo-search-fw-textext-start (pos max)
  "Helper for `mumamo-chunk-textext'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "textext(\"")))
    (when where
      (list where 'plain-tex-mode))))

(defun mumamo-search-fw-textext-end (pos max)
  "Helper for `mumamo-chunk-textext'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (let ((end (mumamo-chunk-end-fw-str pos max "\")")))
      (mumamo-textext-test-is-end end))))

(defun mumamo-chunk-textext (pos max)
  "Find textext or TEX chunks.  Return range and 'plain-tex-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-textext-start
                                 'mumamo-search-fw-textext-end))

(defun mumamo-search-fw-verbatimtex-start (pos max)
  "Helper for `mumamo-chunk-verbatimtextext'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "\nverbatimtex")))
    (when where
      (list where 'plain-tex-mode))))

(defun mumamo-search-fw-verbatimtex-end (pos max)
  "Helper for `mumamo-chunk-verbatimtextext'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "\netex")))

(defun mumamo-chunk-verbatimtex (pos max)
  "Find verbatimtex - etex chunks.  Return range and 'plain-tex-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-verbatimtex-start
                                 'mumamo-search-fw-verbatimtex-end))

(defun mumamo-search-fw-btex-start (pos max)
  "Helper for `mumamo-chunk-btex'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "\nverbatimtex")))
    (when where
      (list where 'plain-tex-mode))))

(defun mumamo-search-fw-btex-end (pos max)
  "Helper for `mumamo-chunk-btex'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "\netex")))

(defun mumamo-chunk-btex (pos max)
  "Find btex - etex chunks.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-btex-start
                                 'mumamo-search-fw-btex-end))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OpenLaszlo

(defconst mumamo-lzx-method-tag-start-regex
  (rx "<method"
      (optional
       space
       (0+ (not (any ">"))))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-bw-exc-start-inlined-lzx-method (pos min)
  "Helper for `mumamo-chunk-inlined-lzx-method'.
POS is where to start search and MIN is where to stop."
  (goto-char (+ pos 7))
  (let ((marker-start (search-backward "<method" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-lzx-method-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'javascript-mode))
        ))))


(defun mumamo-search-fw-exc-start-inlined-lzx-method (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-method'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<method" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-lzx-method-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode)
        ))))

(defun mumamo-search-fw-exc-end-inlined-lzx-method (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-method'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</method>")))

(defun mumamo-chunk-inlined-lzx-method (pos max)
  "Find <method>...</method>.  Return range and 'javascript-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-lzx-method
                                 'mumamo-search-fw-exc-end-inlined-lzx-method))

(defconst mumamo-lzx-handler-tag-start-regex
  (rx "<handler"
      (optional
       space
       (0+ (not (any ">"))))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))


(defun mumamo-search-fw-exc-start-inlined-lzx-handler (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-handler'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<handler" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 8))
      (when (looking-at mumamo-lzx-handler-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode)
        ))))

(defun mumamo-search-fw-exc-end-inlined-lzx-handler (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-handler'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</handler>")))

(defun mumamo-chunk-inlined-lzx-handler (pos max)
  "Find <handler>...</handler>.  Return range and 'javascript-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-lzx-handler
                                 'mumamo-search-fw-exc-end-inlined-lzx-handler))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; csound

(defun mumamo-search-fw-exc-start-csound-orc (pos max)
  "Helper for `mumamo-chunk-csound-orc'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "<csinstruments>")))
    (when where
      (list where 'csound-orc-mode))))

(defun mumamo-search-fw-exc-end-csound-orc (pos max)
  "Helper for `mumamo-chunk-csound-orc'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</csinstruments>")))

(defun mumamo-chunk-csound-orc (pos max)
  "Find <csinstruments>...</...>.  Return range and 'csound-orc-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-csound-orc
                                 'mumamo-search-fw-exc-end-csound-orc))

(defun mumamo-search-fw-exc-start-csound-sco (pos max)
  "Helper for `mumamo-chunk-csound-sco'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "<csscore>")))
    (when where
      (list where 'csound-sco-mode))))

(defun mumamo-search-fw-exc-end-csound-sco (pos max)
  "Helper for `mumamo-chunk-csound-sco'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</csscore>")))

(defun mumamo-chunk-csound-sco (pos max)
  "Found <csscore>...</csscore>.  Return range and 'csound-sco-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-csound-sco
                                 'mumamo-search-fw-exc-end-csound-sco))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; noweb

;;;###autoload
(defgroup mumamo-noweb2 nil
  "Customization group for `noweb2-mumamo-mode'."
  :group 'mumamo-modes)

(defcustom mumamo-noweb2-mode-from-ext
  '(
    ("php" . php-mode)
    ("c" . c-mode)
    )
  "File extension regexp to major mode mapping.
Used by `noweb2-mumamo-mode'."
  :type '(repeat
          (cons regexp major-mode-function))
  :group 'mumamo-noweb2)

;; (defvar mumamo-noweb2-found-mode-from-ext nil
;;   "Major modes determined from file names.  Internal use.")

(defvar mumamo-noweb2-code-major-mode nil)
(put 'mumamo-noweb2-code-major-mode 'permanent-local t)

(defun mumamo-noweb2-set-code-major-mode (major)
  (interactive "CCode major mode: ")
  (message "major=%s" major)
  (set (make-local-variable 'mumamo-noweb2-code-major-mode) major)
  (mumamo-remove-all-chunk-overlays)
  ;; fix-me
  )

(defun mumamo-noweb2-chunk-start-fw (pos max)
  "Helper for `mumamo-noweb2-chunk'.
POS is where to start search and MAX is where to stop."
  (let* ((where (mumamo-chunk-start-fw-re pos max "^<<\\(.*?\\)>>="))
         (border-start (when where (match-beginning 0)))
         (exc-mode 'text-mode))
    (when where
      (if mumamo-noweb2-code-major-mode
          (setq exc-mode mumamo-noweb2-code-major-mode)
        (let* ((file-name (buffer-file-name)) ;(match-string-no-properties 1))
               (file-ext (when file-name (file-name-extension file-name))))
          (when file-ext
            (setq exc-mode (catch 'major
                             (dolist (rec mumamo-noweb2-mode-from-ext)
                               (when (string-match (car rec) file-ext)
                                 (throw 'major (cdr rec))))
                             nil)))))
      (list where exc-mode))))


(defun mumamo-noweb2-chunk-end-fw (pos max)
  "Helper for `mumamo-noweb2-chunk'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-re pos max "^@")))

(defun mumamo-noweb2-code-chunk (pos max)
  "Find noweb chunks.  Return range and found mode.
See `mumamo-possible-chunk-forward' for POS and MAX.
`mumamo-noweb2-set-code-major-mode'"
  (save-match-data
    (mumamo-possible-chunk-forward pos max
                                   'mumamo-noweb2-chunk-start-fw
                                   'mumamo-noweb2-chunk-end-fw)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Template-Toolkit

(defun mumamo-chunk-tt (pos max)
  "Find [% ... %], return range and `tt-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX.

This is for Template Toolkit.
See URL `http://dave.org.uk/emacs/' for `tt-mode'."
  (mumamo-quick-chunk-forward pos max "[%" "%]" t 'tt-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Asp

;;;; asp <%@language="javscript"%>

(defvar mumamo-asp-default-major 'asp-js-mode)
(make-variable-buffer-local 'mumamo-asp-default-major)
(put 'mumamo-asp-default-major 'permanent-local t)

;; Fix-me: Format?
;;<%@ Language="C#" %> ???
;;<%@ Page Language="C#" ... %>
;;<%@ Control ... %>

(defconst mumamo-asp-decl-marker
  (rx "<%@" (+ space)
      (or "Control"
          (seq "Page"
               (+ space)
               "language" (* space) "=" (* space)
               "\"" (submatch (1+ (not (any "\"")))) "\""
               ))))

(defvar mumamo-asp-languages-assoc
  '(("javascript" asp-js-mode)
    ("vbscript" asp-vb-mode)
    ("vb" asp-vb-mode)
    ("c#" asp-csharp-mode))
  "Association between language markers and major mode spec.")

;;(mumamo-asp-page-lang-to-major-spec "C#")
;;(mumamo-asp-page-lang-to-major-spec "C")
(defun mumamo-asp-page-lang-to-major-spec (lang)
  "Translate language marker LANG to a major mode spec.
Note that the spec may be translated further by
`mumamo-major-modes'."
  (setq lang (downcase lang))
  (let ((rec (assoc lang mumamo-asp-languages-assoc)))
    (when rec (nth 1 rec))))

(defun mumamo-chunk-asp% (pos max)
  "Find chunk <% ... %>.
If this is a language specifier use comment mode for the chunk
and set `mumamo-asp-default-major' to the specified language.
Otherwise use `mumamo-asp-default-major' for the chunk.

See `mumamo-possible-chunk-forward' for POS and MAX."
  (let* ((chunk (mumamo-quick-chunk-forward pos max "<%" "%>" 'borders 'fundamental-mode))
         (beg (nth 0 chunk))
         page-lang
         lang)
    (when chunk
      (goto-char beg)
      (cond ((looking-at mumamo-asp-decl-marker)
             (setq page-lang (match-string-no-properties 1))
             (when page-lang
               (setq lang (mumamo-asp-page-lang-to-major-spec page-lang))
               (when lang
                 (setq mumamo-asp-default-major lang)))
             (setcar (nthcdr 2 chunk) 'mumamo-comment-mode))
            (t
             (setcar (nthcdr 2 chunk) mumamo-asp-default-major)))
      chunk)))

;;;; asp <script ...>

(defconst mumamo-asp-server-script-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "language"
      (0+ space)
      "="
      (0+ space)
      (or
       (seq ?\" (submatch (+ (not (any "\""))) (? "#")) ?\")
       (seq ?\' (submatch (+ (not (any "\""))) (? "#")) ?\'))
      (0+ (not (any ">")))
      ">"))

(defun mumamo-asp-search-fw-exc-start-inlined-script (pos max)
  "Helper for `mumamo-chunk-inlined-script'.
POS is where to start search and MAX is where to stop."
  (let ((case-fold-search t))
    ;; Is this fix needed any more???
    (goto-char (1+ pos))
    (skip-chars-backward "^<")
    ;; Handle <![CDATA[
    (when (and
           (eq ?< (char-before))
           (eq ?! (char-after))
           (not (bobp)))
      (backward-char)
      (skip-chars-backward "^<"))
    (unless (bobp)
      (backward-char 1))
    (let ((exc-start (search-forward "<script" max t))
          exc-mode
          lang)
      (when exc-start
        (goto-char (- exc-start 7))
        (when (looking-at mumamo-asp-server-script-tag-start-regex)
          (goto-char (match-end 0))
          (setq lang (match-string-no-properties 1))
          (setq exc-mode (mumamo-asp-page-lang-to-major-spec lang))
          (when exc-mode
            (list (point) exc-mode)))))))

(defun mumamo-chunk-asp-server-script (pos max)
  "Find <script language=...  runat=...>...</script>.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-asp-search-fw-exc-start-inlined-script
                                 'mumamo-search-fw-exc-end-inlined-script))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Org-mode

(defcustom mumamo-org-submodes '((ditaa picture-mode))
  "Alist for conversion of org #+BEGIN_SRC specifier to major mode.
Works kind of like `mumamo-major-modes'.

Note that adding '-mode' to the specifier is tried before this
list.

This may be used for example for org-babel \(see URL
`http://orgmode.org/worg/org-contrib/babel/')."
  :type '(alist
          :key-type (symbol :tag "Symbol in #BEGIN_SRC specifier")
          :value-type (repeat (choice
                               (command :tag "Major mode")
                               (symbol :tag "Major mode (not yet loaded)")))
          )
  :group 'mumamo-modes)

;;(mumamo-org-mode-from-spec 'css)
;;(mumamo-org-mode-from-spec 'abcdef)
(defun mumamo-org-mode-from-spec (major-spec)
  "Translate MAJOR-SPEC to a major mode.
Translate MAJOR-SPEC used in #BEGIN_SRC to a major mode.

See `mumamo-org-submodes' for an explanation."
  (mumamo-major-mode-from-spec major-spec mumamo-org-submodes))

(defun mumamo-chunk-org-html (pos max)
  "Find #+BEGIN_HTML ... #+END_HTML, return range and `html-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "#+BEGIN_HTML" "#+END_HTML" nil 'html-mode))

(defun mumamo-search-fw-org-src-start (pos max)
  "Helper for `mumamo-chunk-org-src'.
POS is where to start search and MAX is where to stop."
  ;;(let ((where (mumamo-chunk-start-fw-str pos max "#+BEGIN_SRC")))
  (let* ((case-fold-search t)
         (where (mumamo-chunk-start-fw-re pos max "#\\+BEGIN_SRC[^\n]*")))
    (when where
      (let ((exc-mode (let ((here (point)))
                        (goto-char where)
                        (forward-line 0)
                        (forward-char 12)
                        (prog1
                            (read (current-buffer))
                          (goto-char here)))))
        (msgtrc "where=%s, exc-mode=%s" where exc-mode)
        (setq exc-mode (mumamo-org-mode-from-spec exc-mode))
        (let ((start where)
              (here (point)))
          (goto-char where)
          (setq start (1+ (line-end-position)))
          (goto-char here)
          (list start exc-mode))))))

(defun mumamo-search-fw-org-src-end (pos max)
  "Helper for `mumamo-chunk-org-src'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "#+END_SRC")))

(defun mumamo-chunk-org-src (pos max)
  "Find #+BEGIN_SRC ... #+END_SRC, return range and choosen major mode.
See `mumamo-possible-chunk-forward' for POS and MAX.

See Info node `(org) Literal Examples' for how to specify major
mode."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-org-src-start
                                 'mumamo-search-fw-org-src-end))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mako

(defun mumamo-chunk-mako-<% (pos max)
  "Find <% ... %> and <%! ... %>. Return range and `python-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk (mumamo-possible-chunk-forward pos max
                                              'mumamo-mako-<%-fw-start
                                              'mumamo-mako-<%-fw-end
                                              'mumamo-mako-<%-find-borders
                                              )))
    (when chunk
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-mako-<%-find-borders (start end exc-mode)
  (when exc-mode
    (list
     (when start
       (+ start
          (if (eq ?! (char-after (+ start 2)))
              3
            2)))
     (when end (- end 2))
     exc-mode)))

(defun mumamo-mako-<%-fw-start (pos max)
  (let ((here (point))
        start
        ret)
    (goto-char pos)
    (setq start
          (re-search-forward "<%!?\\(?:[ \t]\\|$\\)" max t))
    (when start
      (setq ret (match-beginning 0)))
    (goto-char here)
    (when ret
      (list ret 'python-mode))))

(defun mumamo-mako-<%-fw-end (pos max)
  (save-match-data
    (mumamo-chunk-end-fw-str-inc pos max "%>"))) ;; ok



(defun mumamo-chunk-mako-% (pos max)
  "Find % python EOL.  Return range and `python-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk (mumamo-whole-line-chunk pos max "%" 'python-mode)))
    (when chunk
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-mako-one-line-comment (pos max)
  "Find ## comment EOL.  Return range and `python-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-whole-line-chunk pos max "##" 'mumamo-comment-mode))

(defun mumamo-chunk-mako-<%doc (pos max)
  (mumamo-quick-chunk-forward pos max "<%doc>" "</%doc>" 'borders 'mumamo-comment-mode))

;; Fix-me: looks like %call and %def should and several others here
;; should be treated similar to eRuby since their inner are
;; html-mode. However I am not sure how to handle it yet. Maybe it is
;; better rewrite the indenting code to handle this situation?
(defun mumamo-chunk-mako-<%include (pos max)
  (mumamo-quick-chunk-forward pos max "<%include" "/>" 'borders 'html-mode))

(defun mumamo-chunk-mako-<%inherit (pos max)
  (mumamo-quick-chunk-forward pos max "<%inherit" "/>" 'borders 'html-mode))

(defun mumamo-chunk-mako-<%namespace (pos max)
  (mumamo-quick-chunk-forward pos max "<%namespace" "/>" 'borders 'html-mode))

(defun mumamo-chunk-mako-<%page (pos max)
  (mumamo-quick-chunk-forward pos max "<%page" "/>" 'borders 'html-mode))

(defun mumamo-chunk-mako-<%call (pos max)
  (mumamo-quick-chunk-forward pos max "<%call" "/%call>" 'borders 'html-mode))

;; Fix-me: this is not correct
(defun mumamo-chunk-mako-<%def (pos max)
  (mumamo-quick-chunk-forward pos max "<%def" "</%def>" 'borders 'html-mode))

(defun mumamo-chunk-mako$(pos max)
  "Find ${ ... }, return range and `python-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "${" "}" 'borders 'python-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Markdown

(defun mumamo-chunk-markdown-html-1 (pos max)
  (save-restriction
    (goto-char pos)
    (narrow-to-region (point) (or max (point-max)))
    (save-match-data
      (let ((here (point)))
        (when (re-search-forward (rx (* space)
                                     (submatch "<")
                                     (* (any "a-z"))
                                     (or ">" (any " \t\n")))
                                 nil t)
          (let ((beg (match-beginning 1))
                (end))
            (goto-char beg)
            (condition-case err
                (progn
                  (while (not (sgml-skip-tag-forward 1)))
                  (setq end (point)))
              (error (message "mumamo-chunk-markdown-html-1: %s" err)))
            (goto-char here)
            (when (and beg end)
              (cons beg end))))))))

(defun mumamo-chunk-markdown-html-fw-exc-fun (pos max)
  (let ((beg-end (mumamo-chunk-markdown-html-1 pos max)))
    (cdr beg-end)))

(defun mumamo-chunk-markdown-html (pos max)
  "Find a chunk of html code in `markdown-mode'.
Return range and `html-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((beg-end (mumamo-chunk-markdown-html-1 pos max)))
    (when beg-end
      (let ((beg (car beg-end))
            (end (cdr beg-end)))
        (list beg end 'html-mode
              nil ;; borders
              nil ;; parseable y
              'mumamo-chunk-markdown-html-fw-exc-fun
              nil ;; find-borders fun
              )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Latex related

(defun mumamo-latex-closure-chunk (pos max)
  (mumamo-quick-chunk-forward pos max "\\begin{clojure}" "\\end{clojure}" 'borders 'clojure-mode))

(defun mumamo-latex-haskell-chunk (pos max)
  (mumamo-quick-chunk-forward pos max "\\begin{code}" "\\end{code}" 'borders 'haskell-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Python + ReST

;; From Martin Soto

(defun mumamo-python-rst-long-string-chunk (pos max)
 "Find Python long strings.  Return range and 'mumamo-comment-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
 ;;(mumamo-quick-chunk-forward pos max "\"\"\"((" "))\"\"\"" nil 'rst-mode nil))
 (mumamo-quick-chunk-forward pos max "\"\"\"" "\"\"\"" 'borders 'rst-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Amrita

(eval-when-compile (require 'amrita nil t))

(defcustom mumamo-amrita-mode-alist nil
  "Alist used by `mumamo-chunk-amrita-fold' to get chunk mode.
This is used only if the major mode can not by guessed.  See
`mumamo-search-fw-amrita-fold' for more info."
  :type '(alist :key-type string :value-type function)
  :group 'mumamo-modes)

(defun mumamo-search-fw-amrita-fold (pos max)
  (when (require 'amrita nil t)
    (let ((here (point))
          (hit-pos -1)
          start
          (patt (rx word-start
                    "fold::"
                    (submatch (+ (any alpha)))
                    (+ space)
                    "{"
                    (* not-newline)
                    line-end
                    ))
          spec
          chunk-mode)
      (save-match-data
        (while (and (not start) hit-pos)
          (setq hit-pos (re-search-forward patt nil max))
          (when hit-pos
            ;; check if not in string
            (if (mumamo-end-in-code pos hit-pos amrita-mode-syntax-table)
                (setq start (match-end 0))
              (goto-char (match-end 1)))))
        (when start
          (setq spec (match-string-no-properties 1))
          ;; Guess the major mode name
          (setq chunk-mode (intern-soft (concat spec "-mode")))
          (unless (commandp chunk-mode)
            (setq chunk-mode (assoc spec mumamo-amrita-mode-alist))
            (if chunk-mode
                (setq chunk-mode (cdr chunk-mode))
              (setq chunk-mode 'mumamo-bad-mode)))))
      (goto-char here)
      (when start
        (list start chunk-mode nil)))))

(defun mumamo-search-fw-exc-end-amrita-fold (pos max)
  "Helper for `mumamo-chunk-amrita-fold.
POS is where to start search and MAX is where to stop.

Note: This simply matches {}-pairs and will fail if there is a
non-matching pair inside a string."
  (save-match-data
    (let ((here (point))
          ;; {} par level, we start after first {
          (level 1))
      (goto-char pos)
      (while (and (> level 0)
                  (re-search-forward "[{}]" max t))
        (setq level (+ level
                       (if (eq (char-before) ?\{)
                           1 -1))))
      (prog1
          (when (= 0 level) (1- (point)))
        (goto-char here)))))

(defun mumamo-chunk-amrita-fold (pos max)
  "Find Amrita fold::PROGLANG chnks."
  (mumamo-possible-chunk-forward pos max
                                 ;;'mumamo-search-fw-exc-start-xml-pi-new
                                 'mumamo-search-fw-amrita-fold
                                 ;;'mumamo-search-fw-exc-end-xml-pi
                                 'mumamo-search-fw-exc-end-amrita-fold
                                 ;;'mumamo-find-borders-xml-pi
                                 nil
                                 ))

(provide 'mumamo-chunks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-chunks.el ends here
