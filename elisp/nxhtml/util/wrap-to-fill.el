;;; wrap-to-fill.el --- Make a fill-column wide space for editing
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-08-12 Wed
;; Version:
;; Last-Updated: 2010-05-25 Tue
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

(eval-when-compile (require 'mumamo))
(eval-when-compile (require 'org))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Wrapping

;;;###autoload
(defgroup wrap-to-fill nil
  "Customizing of `wrap-to-fill-column-mode'."
  :group 'convenience)

;;;###autoload
(defcustom wrap-to-fill-left-marg nil
  "Left margin handling for `wrap-to-fill-column-mode'.
Used by `wrap-to-fill-column-mode'. If nil then center the
display columns. Otherwise it should be a number which will be
the left margin."
  :type '(choice (const :tag "Center" nil)
                 (integer :tag "Left margin"))
  :group 'wrap-to-fill)
(make-variable-buffer-local 'wrap-to-fill-left-marg)

(defvar wrap-to-fill--saved-state nil)
;;(make-variable-buffer-local 'wrap-to-fill--saved-state)
(put 'wrap-to-fill--saved-state 'permanent-local t)

;;;###autoload
(defcustom wrap-to-fill-left-marg-modes
  '(text-mode
    fundamental-mode)
  "Major modes where `wrap-to-fill-left-marg' may be nil."
  :type '(repeat command)
  :group 'wrap-to-fill)


;;ThisisaVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLongWord ThisisaVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLongWord

(defun wrap-to-fill-wider ()
  "Increase `fill-column' with 10."
  (interactive)
  (setq fill-column (+ fill-column 10))
  (wrap-to-fill-set-values-in-buffer-windows))

(defun wrap-to-fill-narrower ()
  "Decrease `fill-column' with 10."
  (interactive)
  (setq fill-column (- fill-column 10))
  (wrap-to-fill-set-values-in-buffer-windows))

(defun wrap-to-fill-normal ()
  "Reset `fill-column' to global value."
  (interactive)
  ;;(setq fill-column (default-value 'fill-column))
  (kill-local-variable 'fill-column)
  (wrap-to-fill-set-values-in-buffer-windows))

(defvar wrap-to-fill-column-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) ?+] 'wrap-to-fill-wider)
    (define-key map [(control ?c) ?-] 'wrap-to-fill-narrower)
    (define-key map [(control ?c) ?0] 'wrap-to-fill-normal)
    map))

;; Fix-me: Maybe make the `wrap-prefix' behavior an option or separate
;; minor mode.

;; Fix-me: better handling of left-column in mumamo buffers (and other
;; if possible).

;;;###autoload
(define-minor-mode wrap-to-fill-column-mode
  "Use a column of width `fill-column' to display buffer in windows.
By default the column is centered, but this can be changed with
the option `wrap-to-fill-left-marg'.

This mode turns on/off `visual-indent-mode'.

When turning this mode on `visual-line-mode' is also turned on.
This is however not reset when turning off this mode.

Key bindings added by this minor mode:

\\{wrap-to-fill-column-mode-map}

"
  :lighter " WrapFill"
  :group 'wrap-to-fill
  (if wrap-to-fill-column-mode
      (progn
        ;; Old values (idea from visual-line-mode)
	(set (make-local-variable 'wrap-to-fill--saved-state) nil)
	(dolist (var '(visual-line-mode
                       ;;left-margin-width
                       ;;right-margin-width
                       ))
          (push (list var (symbol-value var) (local-variable-p var))
                wrap-to-fill--saved-state))
        ;; Hooks
        (add-hook 'window-configuration-change-hook 'wrap-to-fill-set-values nil t)
        ;; Wrapping
        (visual-line-mode 1)
        (wrap-to-fill-set-values-in-buffer-windows))
    ;; Hooks
    (remove-hook 'window-configuration-change-hook 'wrap-to-fill-set-values t)
    ;; Old values
    (dolist (saved wrap-to-fill--saved-state)
      (let ((var (nth 0 saved))
            (val (nth 1 saved))
            (loc (nth 2 saved)))
        (cond
         ((eq var 'visual-line-mode)
          (unless val (visual-line-mode -1)))
         (t
          (if loc
              (set (make-local-variable var) val)
            (kill-local-variable var))))))
    (kill-local-variable 'wrap-to-fill--saved-state)
    ;; Margins
    (dolist (win (get-buffer-window-list (current-buffer)))
      (set-window-margins win left-margin-width right-margin-width))
    ;; Indentation
    )
  (visual-indent-mode (if wrap-to-fill-column-mode 1 -1)))
(put 'wrap-to-fill-column-mode 'permanent-local t)

(defcustom wrap-to-fill-major-modes '(org-mode
                                      html-mode
                                      nxhtml-mode)
  "Major modes where to turn on `wrap-to-fill-column-mode'"
  ;;:type '(repeat major-mode)
  :type '(repeat command)
  :group 'wrap-to-fill)

(defun wrap-to-fill-turn-on-in-buffer ()
  "Turn on fun for globalization."
  (when (catch 'turn-on
          (dolist (m wrap-to-fill-major-modes)
            (when (derived-mode-p m)
              (throw 'turn-on t))))
    (wrap-to-fill-column-mode 1)))

(define-globalized-minor-mode wrap-to-fill-column-global-mode wrap-to-fill-column-mode
  wrap-to-fill-turn-on-in-buffer
  :group 'wrap-to-fill)

;; Fix-me: There is a confusion between buffer and window margins
;; here. Also the doc says that left-margin-width and dito right may
;; be nil. However they seem to be 0 by default, but when displaying a
;; buffer in a window then window-margins returns (nil).

(defvar wrap-to-fill-timer nil)
(make-variable-buffer-local 'wrap-to-fill-timer)

(defun wrap-to-fill-set-values ()
  (if t
      ;; Seems like this works now. Why did not this work before???
      (wrap-to-fill-set-values-in-window (selected-window))
    (when (timerp wrap-to-fill-timer)
      (cancel-timer wrap-to-fill-timer))
    (setq wrap-to-fill-timer
          (run-with-idle-timer 0 nil 'wrap-to-fill-set-values-in-timer
                               (selected-window) (current-buffer)))))
(put 'wrap-to-fill-set-values 'permanent-local-hook t)

(defun wrap-to-fill-set-values-in-timer (win buf)
  (condition-case err
      (when (buffer-live-p buf)
        ;; Fix-me: Seems like we have to do it in all buffer windows, but why?
        (wrap-to-fill-set-values-in-buffer-windows buf)
        ;;(wrap-to-fill-set-values-in-window win)
        )
    (error (message "ERROR wrap-to-fill-set-values-in-timer: %s"
                    (error-message-string err)))))

(defun wrap-to-fill-set-values-in-buffer-windows (&optional buffer)
  "Use `fill-column' display columns in buffer windows."
  (let* ((buf (or buffer (current-buffer)))
         (wfm (with-current-buffer buf wrap-to-fill-column-mode))
         (buf-windows (when wfm (get-buffer-window-list buf nil t))))
    (dolist (win buf-windows)
      (if wrap-to-fill-column-mode
          (wrap-to-fill-set-values-in-window win)))))

(defvar wrap-old-win-width nil)
(make-variable-buffer-local 'wrap-old-win-width)
;; Fix-me: compensate for left-margin-width etc
(defun wrap-to-fill-set-values-in-window (win)
  (with-current-buffer (window-buffer win)
    (when wrap-to-fill-column-mode
      (let* ((win-width (window-width win))
             (win-margs (window-margins win))
             (win-full (+ win-width
                          (or (car win-margs) 0)
                          (or (cdr win-margs) 0)))
             (extra-width (- win-full fill-column))
             (fill-left-marg (unless (memq major-mode wrap-to-fill-left-marg-modes)
                               (or (when (> left-margin-width 0) left-margin-width)
                                   wrap-to-fill-left-marg)))
             (left-marg (if fill-left-marg
                            fill-left-marg
                          (- (/ extra-width 2) 1)))
             ;; Fix-me: Why do I have to subtract 1 here...???
             (right-marg (- win-full fill-column left-marg 1))
             (old-window-point (window-point win)))
        (setq wrap-old-win-width win-width)
        (unless (> left-marg 0) (setq left-marg 0))
        (unless (> right-marg 0) (setq right-marg 0))
        (set-window-margins win left-marg right-marg)
        ;; This does not change point in window so scrolling might
        ;; still occur (which is good here):
        (set-window-point win old-window-point)))))

;; (add-hook 'post-command-hook 'my-win-post-command nil t)
;; (remove-hook 'post-command-hook 'my-win-post-command t)
;; (defun my-win-post-command ()
;;   (message "win-post-command: l/r=%s/%s %S %S %S" left-margin-width right-margin-width (window-edges) (window-inside-edges) (window-margins)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrapped lines indentation

(defgroup visual-indent nil
  "Customization group for `visual-indent-mode'."
  :group 'indentation
  :group 'vl)

;;(setq visual-indent-use-adaptive-fill nil)
(defcustom visual-indent-use-adaptive-fill t
  "Use adaptive fill variables to determine wrapping if non-nil.
Otherwise use the faster special visual indent functions for this.

The adaptive fill variables which is used are currently:

- `adaptive-fill-regexp'
- `adaptive-fill-function'

"
  :group 'visual-indent)

(define-minor-mode visual-indent-mode
  "Do indentation of continuation lines like `fill-paragraph'.
If `visual-line-mode' and `word-wrap' is on do visual indentation
similar to `fill-paragraph', but without changing the text in the
buffer.

Indent continuation lines if the line is indented and/or begins
like '- ' etc:

  - Indent lines after
    this
  * and after
    this
  1) and when counting
     things
  a) wether using numbers
     or letters.

See `visual-indent-use-adaptive-fill' for more info.

* Note: The text property 'wrap-prefix is set by this mode to
  indent continuation lines for the above.  The property
  'visual-indent-wrap-prefix is used to remember this so it can
  be set back."
  :group 'visual-indent
  ;;(visual-indent-font-lock visual-indent-mode)
  (if visual-indent-mode
      (progn
        (set (make-local-variable 'adaptive-fill-function) nil)
        (jit-lock-register 'visual-indent-jit-lock-fun))
    (kill-local-variable 'adaptive-fill-function)
    (jit-lock-unregister 'visual-indent-jit-lock-fun)
    (let ((here (point))
          (inhibit-field-text-motion t)
          beg-pos
          end-pos)
      (with-silent-modifications
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (setq beg-pos (point))
            (setq end-pos (line-end-position))
            (when (equal (get-text-property beg-pos 'wrap-prefix)
                         (get-text-property beg-pos 'visual-indent-wrap-prefix))
              (remove-list-of-text-properties
               beg-pos end-pos
               '(wrap-prefix)))
            (forward-line))
          ;;(remove-list-of-text-properties (point-min) (point-max) '(wrap-prefix))
          (remove-list-of-text-properties
           (point-min) (point-max)
           '(visual-indent-wrap-prefix)))
        (goto-char here)))))

(defun visual-indent-turn-on-in-buffer ()
  "Turn on fun for globalization."
  (visual-indent-mode 1))

(define-globalized-minor-mode visual-indent-global-mode visual-indent-mode
  visual-indent-turn-on-in-buffer
  :group 'visual-indent)

(defcustom visual-indent-comment-regexps
  '(org-mode
    html-mode
    nxhtml-mode)
  "Major modes where to turn on `visual-indent-mode'"
  :type '(repeat command)
  :group 'visual-indent)

(defun visual-indent-fill-context-prefix (beg end)
  "Compute fill prefix for wrapped lines.
See `visual-indent-use-adaptive-fill' for more information."
  (unless (= beg (point-at-bol))
    (message "visual-indent-fill-context-prefix internal err: not at bol"))
  (if visual-indent-use-adaptive-fill
      ;; Fix-me: fill-match-adaptive-prefix is not the whole
      ;; story. There are citation regexps that could help when
      ;; viewing mail for example.
      (let* ((one-line-comment-prefix
              (and (zerop (length comment-end))
                   comment-start
                   comment-start-skip
                   (let (beg-w end-w)
                     (goto-char beg)
                     (skip-syntax-forward " ")
                     (when (looking-at comment-start-skip)
                       (setq beg-w (point))
                       (goto-char (match-end 0))
                       (skip-syntax-backward " ")
                       (setq end-w (point))
                       (concat (buffer-substring (point-at-bol) beg-w)
                               (propertize
                                (buffer-substring-no-properties beg-w end-w)
                                'face '( ;;
                                        :strike-through t
                                        ;; :slant italic
                                        ;; :weight extralight
                                        ;; :weight light
                                        :weight thin
                                        ))
                               (buffer-substring-no-properties end-w (match-end 0)))))))
             (first-line-prefix (unless one-line-comment-prefix
                                  (goto-char beg)
                                  (let ((adaptive-fill-regexp (if (derived-mode-p 'org-mode)
                                                                  org-adaptive-fill-regexp-backup
                                                                adaptive-fill-regexp)))
                                    (fill-match-adaptive-prefix))))
             (second-line-prefix (or first-line-prefix
                                     one-line-comment-prefix))
             (nc 0))
        ;;(msgtrc "one-line-comment-prefix=%S %s %s %s" one-line-comment-prefix (zerop (length comment-end)) (when comment-start-skip (looking-at comment-start-skip)) comment-start-skip )
        ;; (elt "hej" 1)
        (unless one-line-comment-prefix
          (while (< nc (length second-line-prefix))
            (let ((cc (elt second-line-prefix nc)))
              ;; Whitespace syntax? Otherwise set to space char.
              (unless (memq (char-syntax cc) '(32 ?-))
                (aset second-line-prefix nc 32)))
            (setq nc (1+ nc))))
        second-line-prefix)
    ;; Keep this path too, since it is noticeable faster then using
    ;; the fill-match-adaptive-prefix above.
    (let (ind-str
          ind-str-fill
          skipped)
      ;; Find indentation quickly
      (when (< 0 (skip-chars-forward "[:blank:]"))
        (setq ind-str (buffer-substring-no-properties beg (point))))
      ;; Any special markers like "- ", "* ", "1) ", "1. " etc
      (when (< (1+ (point)) (point-max))
        (or (and (memq (char-after) '(?- ;; 45
                                      ?– ;; 8211
                                      ?*
                                      ))
                 (eq (char-after (1+ (point))) ?\ )
                 (setq ind-str-fill (concat "  " ind-str)))
            (and (setq skipped (skip-chars-forward "[:digit:]"))
                 (or (> skipped 0)
                     (= 1 (setq skipped (skip-chars-forward "[:alpha:]"))))
                 (memq (char-after (point)) '(?\) ?.))
                 (eq (char-after (1+ (point))) ?\ )
                 (setq ind-str-fill (concat ind-str (make-string (+ 2 skipped) 32))))))
      (or ind-str-fill ind-str))))

(defsubst visual-indent-while (limit counter where)
  (let ((count (symbol-value counter)))
    (if (= count limit)
        (progn
          (message "Reached (while limit=%s, where=%s)" limit where)
          nil)
      (set counter (1+ count)))))

(defun visual-indent-jit-lock-fun (beg end)
  (when (and visual-line-mode word-wrap)
    (save-restriction
      (widen)
      (let ((n-while 0)
            (bound end) ;;(save-excursion (goto-char end) (point-at-eol))))
            (last-point (1- beg)) ;; Protect against display engine errors.
            )
        (goto-char beg)
        (goto-char (point-at-bol))
        ;;(unless (or (bolp) (eobp)) (forward-line 1))
        (while (and (visual-indent-while 200 'n-while "visual-indent-jit-lock-fun")
                    (< last-point (point))
                    (< (point) bound)) ;; Max bound = (point-max)
          (let (ind-str-fill
                (beg-pos (point))
                (end-pos (point-at-eol)))
            (unless (= beg-pos (point-at-bol))
              (message "visual-indent-jit-lock-fun internal err: beg-pos /= point-at-bol")
              (gdb-deb-print "visual-indent-jit-lock-fun internal err: beg-pos /= point-at-bol")
              )
            ;; Fix-me: Why did I check this? Step aside from org-mode or?
            (when (equal (get-text-property beg-pos 'wrap-prefix)
                         (get-text-property beg-pos 'visual-indent-wrap-prefix))
              (setq ind-str-fill
                    (visual-indent-fill-context-prefix beg-pos end-pos))
              ;;(msgtrc "visual-indent-jit-lock-fun:ind-str-fill=%S" ind-str-fill)
              ;; Fix-me: ind-str-fill could be nil.
              (when (< 0 (length ind-str-fill))
                (with-silent-modifications
                  (put-text-property beg-pos end-pos 'wrap-prefix ind-str-fill)
                  (put-text-property beg-pos end-pos 'visual-indent-wrap-prefix ind-str-fill)))))
          ;; This moves to the end of line if there is no more lines. That
          ;; means we will not get stuck here.
          (unless (eobp) (forward-line 1))
          (unless (< last-point (point))
            (message "visual-indent-jit-lock-fun display engine error")
            (gdb-deb-print "visual-indent-jit-lock-fun display engine error"))
          )))))


;;; Code below is obsolete.

;; (defun visual-indent-fontify (bound)
;;   "During fontification mark lines for indentation.
;; This is called as a matcher in `font-lock-keywords' in
;; `visual-indent-mode'.  BOUND is the limit of fontification.

;; Put the property 'wrap-prefix on lines whose continuation lines
;; \(see `visual-line-mode') should be indented.  Only do this if
;; `visual-line-mode' and `word-wrap' is on.

;; Return nil."
;;   ;; Fix-me: break up for `jit-lock-register': two args, beg end, no rules for return value.
;;   ;; See (require 'glasses)
;;   (visual-indent-jit-lock-fun (point) bound)
;;   ;; Do not set match-data, there is none, just return nil.
;;   nil)

;; (defun visual-indent-font-lock (on)
;;   ;; See mlinks.el
;;   (let* ((add-or-remove (if on 'font-lock-add-keywords 'font-lock-remove-keywords))
;;          (fontify-fun 'visual-indent-fontify)
;;          (args (list nil `(( ,fontify-fun ( 0 'font-lock-warning-face t ))))))
;;     (when fontify-fun
;;       (when on (setq args (append args (list t))))
;;       (apply add-or-remove args)
;;       (font-lock-mode -1)
;;       (font-lock-mode 1))))

(provide 'wrap-to-fill)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wrap-to-fill.el ends here
