;; mozadd.el --- Additional functionality for MozRepl
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-07-22 Wed
(defconst mozadd:version "0.2") ;; Version:
;; Last-Updated: 2009-08-04 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `cc-cmds', `cc-defs', `cc-engine', `cc-vars', `comint', `json',
  ;; `moz', `regexp-opt', `ring'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Live tracking of editing changes, see
;;   `mozadd-mirror-mode'
;;   `mozadd-refresh-edited-on-save-mode'
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

(require 'moz)
(require 'json)
(eval-when-compile (require 'org))
(require 're-builder)
(require 'url-util)
;;(load 'isearch)

(defun mozadd-warning (format-string &rest args)
  (let ((str (apply 'format format-string args)))
    (message "%s" (propertize str 'face 'secondary-selection))))

(defun mozadd-warn-not-shown (format-string &rest args)
  (with-help-window (help-buffer)
    (help-setup-xref (list #'describe-function 'inferior-moz-start-process) (interactive-p))
    (with-current-buffer standard-output
      (insert (propertize (apply 'format format-string args)
                          'face 'secondary-selection)
              "\n\n"
              "Explanation:\n\n"
              "The buffer must be shown in a *visible* tab in Firefox."
              "\n\n- So if the buffer is shown in Firefox just make the tab visible."
              "\n- Otherwise just show the buffer's file again in Firefox."
              "\n\n\n"
              "This error could also be caused by problems with communication with Firefox."
              "  Firefox might be confused over what tab to use, because it can't find the old tab."
              "  So if the above does not help then try:"
              "\n\n  `M-x mozadd-restart-mirror'"
              "\n\nThis should make Firefox forget about old tabs."
              )
      (fill-region 1 (point))
      ))
  (apply 'mozadd-warning format-string args))

(defun mozadd-init-href-pattern ()
  "Init regexps for re-builder and isearch for link searching."
  (interactive)
  ;; Fix-me: move to defcustom/var
  (let ((re (rx-to-string
             `(and
               "<a" (+ space)
               (* (not (any "<>")))
               word-start
               "href" (* space) "=" (* space)
               "\""
               ;;(submatch ,web-vcs-linkpatt-href-re
               (regexp ,(concat "\\(?1:" "[^\"]*" "\\)"))
               "\""
               ))))
    ;;(setq isearch-string re)
    (setq regexp-search-ring (cons re regexp-search-ring))
    ;;(setq isearch-regexp t)
    (setq isearch-last-case-fold-search t)
    (setq reb-regexp re)))

(defgroup mozadd nil
  "Customization group for `mozadd-mirror-mode'."
  :group 'moz)

;;(defvar mozadd-auto-update-mirror nil)
(put 'mozadd-auto-update-mirror-mode 'permanent-local t)
;;(define-minor-mode mozadd-toggle-auto-update ()
(define-minor-mode mozadd-auto-update-mirror-mode
  "Auto update after buffer changes in `mozadd-mirror-mode'."
  :group 'mozadd)

(defcustom mozadd-auto-update-delay 2.0
  "Seconds to delay before auto update Firefox."
  :type 'number
  :group 'mozadd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refresh Firefox after save etc

;; Partly after an idea on EmacsWiki

(defvar mozadd-edited-buffer nil)
(setq mozadd-edited-buffer nil)

;;;###autoload
(define-minor-mode mozadd-refresh-edited-on-save-mode
  "Refresh mozadd edited file in Firefox when saving file.
The mozadd edited file is the file in the last buffer visited in
`mozadd-mirror-mode'.  If the current buffer is an html file then
this file will be refreshed.

You can use this for example when you edit CSS files.

The mozadd edited file must be shown in Firefox and visible."
  :lighter "MozRefresh"
  (if mozadd-refresh-edited-on-save-mode
      (add-hook 'after-save-hook 'mozadd-queue-reload-mozilla-edited-file nil t)
    (remove-hook 'after-save-hook 'mozadd-queue-reload-mozilla-edited-file t)))
(put 'mozadd-refresh-edited-on-save-mode 'permanent-local t)

;;;###autoload
(define-globalized-minor-mode global-mozadd-refresh-edited-on-save-mode
  mozadd-refresh-edited-on-save-mode
  (lambda ()
    (when (or (derived-mode-p 'css-mode)
              (mozadd-html-buffer-file-p))
      (mozadd-refresh-edited-on-save-mode 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mirror html buffer in Firefox

;; Partly after an idea on
;; http://people.internetconnection.net/2009/02/interactive-html-development-in-emacs/

;; Fun, it kind of works, but is perhaps totally useless .... - slow
;; and maybe scrolling... - but the file I am testing with have 3000
;; lines...

;; Fix-me: How do you get the currently shown page in Firefox?

;; (process-status (inferior-moz-process))
;; (process-type (inferior-moz-process))
;; (process-contact (inferior-moz-process))
;; (process-exit-status (inferior-moz-process))
;; (mozadd-moz-process)

(defvar mozadd-moz-process-is-ill nil)

(defun mozadd-moz-process ()
  "(Re)start if MozRepl if not running or not OK.
Like `inferior-moz-process' but more careful."
  (when (and inferior-moz-buffer
             (bufferp inferior-moz-buffer))
    (unless (and (buffer-live-p inferior-moz-buffer)
                 (let* ((proc (get-buffer-process inferior-moz-buffer))
                        (exit-status (when proc (process-exit-status proc)))
                        (status (when proc (process-status proc)))
                        )
                   (if (and exit-status (zerop exit-status))
                       (if (eq status 'open)
                           t
                         (message "MozRepl process has not exited, but is closed")
                         (delete-process proc)
                         nil)
                     (message "MozRepl process has exited with exit-status=%s, status=%s" exit-status status)
                     ;; Fix-me: should the process be deleted if it has exited?
                     (when proc (delete-process proc))
                     nil)))
      (kill-buffer inferior-moz-buffer)
      (setq inferior-moz-buffer nil)))
  (unless inferior-moz-buffer
    (setq mozadd-moz-process-is-ill t)
    (condition-case err
        (progn
          (inferior-moz-start-process)
          (setq mozadd-moz-process-is-ill nil))
      (error
       (mozadd-warning (error-message-string err)))))
  (unless mozadd-moz-process-is-ill
    (inferior-moz-process)))

(defvar mozadd-mirror-location nil)
(make-variable-buffer-local 'mozadd-mirror-location)
(put 'mozadd-mirror-location 'permanent-local t)

(defvar mozadd-initial-mirror-location nil)
(make-variable-buffer-local 'mozadd-initial-mirror-location)
(put 'mozadd-initial-mirror-location 'permanent-local t)

;;(mozadd-get-comint-string-part "\"hi\" there")
(defun mozadd-get-comint-string-part (comint-output)
  (save-match-data
    (let ((len (length comint-output)))
      (if (< len 500)
          (message "Received from Mozilla %S ..." comint-output)
        (message "Received from Mozilla (%d chars) ..." (length comint-output))))
    (if (string-match "^\".*?\"" comint-output)
        (match-string 0 comint-output)
      comint-output)))

(defun mozadd-get-initial-mirror-location (comint-output)
  ;;(message "mozadd-get-initial-mirror-location %S" comint-output)
  (with-current-buffer mozadd-edited-buffer
    (setq mozadd-initial-mirror-location (mozadd-get-comint-string-part comint-output))
    (require 'org)
    (setq mozadd-initial-mirror-location
	  (org-link-escape mozadd-initial-mirror-location
                           org-link-escape-chars-browser)))

  (mozadd-exec-next)
  comint-output)

(defun mozadd-get-mirror-location (comint-output)
  ;;(message "mozadd-get-mirror-location %S" comint-output)
  (with-current-buffer mozadd-edited-buffer
    (setq mozadd-mirror-location (mozadd-get-comint-string-part comint-output)))
  (mozadd-exec-next)
  comint-output)

(defun mozadd-add-queue-get-mirror-location ()
  (mozadd-add-task "content.location.href" 'mozadd-get-mirror-location))

(defun mozadd-skip-output-until-prompt (comint-output)
  ;;(message "mozadd-skip-output-until-prompt %S" comint-output)
  (if (not (string-match-p "\\(\\w+\\)> $" comint-output))
      ""
    (message "Recieved ready (prompt) from Mozilla %s" (current-time-string))
    (mozadd-exec-next)
    comint-output
    ""
    ))

(defun mozadd-queue-send-buffer-content-to-mozilla (buffer)
  ;; In timer
  (condition-case err
      (progn
        (mozadd-add-queue-get-mirror-location)
        (setq mozadd-edited-buffer buffer)
        ;; Queue only a single send of buffer content.
        (mozadd-add-task-1 'mozadd-send-buffer-content-to-mozilla t))
    (error (message "%s" (error-message-string err)))))

(defun mozadd-edited-file-is-shown ()
  (with-current-buffer mozadd-edited-buffer
    (message "mirror A: %S" mozadd-mirror-location)
    (message "mirror B: %S" mozadd-initial-mirror-location)
    (let ((res (compare-strings mozadd-mirror-location 0 nil
                                mozadd-initial-mirror-location 0 nil
                                t)))
      (message "res=%s" res)
      (eq t res)
      )))

(defvar mozadd-send-buffer-hook '(mozadd-isearch-send-buffer-hook-fun
                                  mozadd-re-builder-send-buffer-hook-fun)
  "Hook run before sending to the moz process.
Called by `mozadd-send-buffer-content-to-mozilla' before sending
buffer content.

Every function in the hook is called with one parameter, a symbol
whose variable value is a list.  The functions should add to this
list a record with information where they want the CSS property
outline added.  The record should have the format

  (START-TAG-END . OUTLINE-STYLE)

- START-TAG-END is the end of a start tag \(i.e. the position of
  the '>').
- OUTLINE-STYLE is the CSS style for the outline \(for example
  '1px solid red')."  )

;;(setq where-points (sort where-points '<)))))
;; (setq y nil)
;; (push 0 (symbol-value 'y))

(defun mozadd-send-buffer-content-to-mozilla ()
  "Update the remote mozrepl instance.
This runs the hook `mozadd-send-buffer-hook' before sending."
  (with-current-buffer mozadd-edited-buffer
    (if (mozadd-edited-file-is-shown)
        (mozadd-requeue-me-as-task
         (concat "content.document.body.innerHTML="
                 (json-encode
                  (save-restriction
                    (widen)
                    (let ((where-points nil)
                          (str "")
                          (p1 (point-min)))
                      ;; If nxml-where-mode is on add corresponding outline style.
                      (run-hook-with-args 'mozadd-send-buffer-hook 'where-points)
                      ;; (when (and (boundp 'nxml-where-mode) nxml-where-mode)
                      ;;   (mapc (lambda (ovl)
                      ;;           (when (overlay-get ovl 'nxml-where)
                      ;;             (when (/= ?/ (1+ (char-after (overlay-start ovl))))
                      ;;               (push (1- (overlay-end ovl)) where-points))))
                      ;;         (overlays-in (point-min) (point-max)))
                      ;;   (setq where-points (sort where-points '<)))
                      (setq where-points (sort where-points (lambda (a b)
                                                              (< (car a) (car b)))))
                      ;;(message "where-points =%S" where-points)
                      (dolist (rec where-points)
                        (let ((p2    (nth 0 rec))
                              (style (nth 1 rec)))
                          (setq str (concat str
                                            (buffer-substring-no-properties p1
                                                                            p2)))
                          (setq str (concat str " style=\"outline: " style "\""))
                          (setq p1 p2)))
                      (setq str (concat str
                                        (buffer-substring-no-properties p1
                                                                        (point-max))))
                      ;;(message "STRSTR=\n%s" str)
                      str)))
                 ";")
         'mozadd-skip-output-until-prompt)
      (mozadd-skip-current-task)
      (mozadd-warn-not-shown "Mozadd: Edited buffer %s is not visible in Firefox, can't reload it."
                             (buffer-name mozadd-edited-buffer)))
    ;; Timer to avoid looping
    (run-with-idle-timer 0 nil 'mozadd-maybe-exec-next)
    ))

(defvar mozadd-current-task nil)
(setq mozadd-current-task nil)

(defvar mozadd-task-queue nil)
(setq mozadd-task-queue nil)
;;(mozadd-add-task "content.location.href" 'mozadd-get-initial-mirror-location)
;;(mozadd-add-task "hi" 1)
;;(mozadd-add-task "hm" 2)

(defun mozadd-clear-exec-queue ()
  (setq mozadd-current-task nil)
  (setq mozadd-task-queue nil)
  (when (buffer-live-p inferior-moz-buffer)
    (with-current-buffer inferior-moz-buffer
      (dolist (fun (buffer-local-value 'comint-preoutput-filter-functions (current-buffer)))
        (remove-hook 'comint-preoutput-filter-functions fun t)))))

(defun mozadd-add-task (input task)
  (message "mozadd-add-task %S %S" input task)
  (mozadd-add-task-1 (list input task)))

(defun mozadd-add-task-1 (task &optional single)
  (message "mozadd-add-task-1: %S" (if nil ;;(listp task)
                                       (nth 1 task)
                                     task))
  (when single
    (setq mozadd-task-queue (delete task mozadd-task-queue)))
  (message "mozadd queue a: %S" mozadd-task-queue)
  (setq mozadd-task-queue (cons task mozadd-task-queue))
  (message "mozadd queue b: %S" mozadd-task-queue)
  (setq mozadd-task-queue (reverse mozadd-task-queue))
  ;;(message "add-task: mozadd-task-queue=%S, current=%s" mozadd-task-queue mozadd-current-task)
  (message "mozadd queue c: %S" mozadd-task-queue)
  (mozadd-maybe-exec-next))

(defun mozadd-maybe-exec-next ()
  ;;(message "mozadd-maybe-exec-next, current=%s" mozadd-current-task)
  (unless mozadd-current-task
    (mozadd-exec-next)))

(defun mozadd-exec-next ()
  (mozadd-moz-process)
  (when mozadd-current-task
    (let* ((old-task mozadd-current-task) ;;(pop mozadd-task-queue))
           (old-filter (when (listp old-task) (nth 1 old-task))))
      (when (and old-filter (buffer-live-p inferior-moz-buffer))
        (with-current-buffer inferior-moz-buffer
          (remove-hook 'comint-preoutput-filter-functions old-filter t)))))
  (setq mozadd-current-task nil)
  (when mozadd-task-queue
    (let* ((this  (pop mozadd-task-queue))
           (input (when (listp this) (nth 0 this)))
           (task  (when (listp this) (nth 1 this))))
      (setq mozadd-current-task this)
      ;;(message "EXEC: %s" this)
      (when task
        (message "EXEC task: %s %d char, start=%S (queue %s)" task
                 (length input)
                 (substring input 0 (min 50 (length input)))
                 (mapcar (lambda (rec)
                           (if (listp rec) (nth 1 rec) rec))
                         mozadd-task-queue)))
      (if (not (listp this))
          (funcall this)
        (when (buffer-live-p inferior-moz-buffer)
          (with-current-buffer inferior-moz-buffer
            (add-hook 'comint-preoutput-filter-functions task nil t)))
        (message "Sending to Mozilla now (%d chars: %S) ..." (length input) (substring input 0
                                                                                       (min 150 (length input))))
        (comint-send-string (inferior-moz-process) input)))))

(defun mozadd-skip-current-task ()
  ;;(message "mozadd-skip-current-task")
  ;;(pop mozadd-task-queue)
  (message "mozadd-skip-current-task: %S" mozadd-current-task)
  (setq mozadd-current-task nil))

(defun mozadd-requeue-me-as-task (input task)
  (message "mozadd-requeue-me-as-task enter queue: %S" (mapcar (lambda (rec)
                                                           (nth 1 rec))
                                                         mozadd-task-queue))
  (mozadd-skip-current-task)
  ;;(message "mozadd-requeue-me-as-task %S %S" input task)
  (setq mozadd-task-queue (cons (list input task) mozadd-task-queue))
  (message "mozadd-requeue-me-as-task exit queue: %S" (mapcar (lambda (rec)
                                                           (nth 1 rec))
                                                         mozadd-task-queue))
  )

(defcustom mozadd-browseable-file-extensions
  '("html" "htm" "xhtml")
  "File extensions possibly viewable in a web browser."
  :type '(repeat (string :tag "File extension (without leading dot)"))
  :group 'mozadd)

(defun mozadd-html-buffer-file-p ()
  "Return non-nil if buffer file is viewable in a web browser."
  (when (buffer-file-name)
    (member (file-name-extension (buffer-file-name))
            mozadd-browseable-file-extensions)))

(defvar mozadd-update-key [(control ?c)(control ?c)])
(defvar mozadd-submatch-key [(control ?c)(control ?0)])
(defvar mozadd-mirror-mode-map
  (let ((map (make-sparse-keymap "MozMirror"))
        (menu-map (make-sparse-keymap)))
    (define-key map mozadd-update-key 'mozadd-update-mozilla)
    (define-key map mozadd-submatch-key 'mozadd-set-outline-regexp-submatch-num)
    (define-key map [(control ?c)(control ?a)] 'mozadd-auto-update-mirror-mode)
    (define-key map [(control ?c)(control ?b)] 'mozadd-add-href-base)
    ;; Fix-me: menu
    (define-key map [menu-bar mozadd-mirror-mode]
      `(menu-item "MozMirror" ,menu-map))
    (define-key menu-map [toggle]
      `(menu-item "Toggle Auto Update of Firefox" mozadd-auto-update-mirror-mode
                  :button '(:toggle ,mozadd-auto-update-mirror-mode)))
    (define-key menu-map [base]
      `(menu-item "Set Base URL" mozadd-add-href-base))
    (define-key menu-map [div2] '(menu-item "--"))
    (define-key menu-map [restart]
      '(menu-item "Restart Mirroring" mozadd-restart-mirror))
    (define-key menu-map [div1] '(menu-item "--"))
    (define-key menu-map [update]
      '(menu-item "Update Firefox" mozadd-update-mozilla))
    map))

(defun mozadd-add-href-base (url)
  "Add <base href=... /> tag with url URL."
  (interactive "sOriginal URL: ")
  (if (zerop (length url))
             (message "No URL given")
    (let ((urlobj (url-generic-parse-url url)))
      (cond
       ((not (member (url-type urlobj) '("http" "https")))
        (message "Must be a http url"))
       ((not (url-fullness urlobj))
        (message "Must be a full url"))
       (t
        (save-excursion
          (let ((re-base (rx "<base"
                             (+ whitespace)
                             "href=\""
                             (submatch
                              (+ (not (any "\""))))
                             "\""))
                (re-head (rx "<head"
                             (* (not (any ">")))
                             ">")))
            (goto-char (point-min))
            (cond ((re-search-forward re-base nil t)
                   (delete-region (match-beginning 1) (match-end 1))
                   (goto-char (match-beginning 1))
                   (insert url)
                   (message "Change old base tag to new url"))
                  ((re-search-forward re-head nil t)
                   (insert "\n\n<base href=\"" url "\" />\n\n")
                   (message "Added base tag"))
                  (t
                   (error "Can't find <head ...>, don't know where to add <base ...> tag"))))))))))

;;;###autoload
(define-minor-mode mozadd-mirror-mode
  "Mirror content of current file buffer in Firefox.
When you turn on this mode the html file you are editing will be
opened in Firefox.
\\<mozadd-mirror-mode-map>
Updating of Firefox is made when the buffer is saved and can be
made any time with \\[mozadd-update-mozilla].

This can be done also during `isearch-mode' and from
`re-builder'.  Tags containing matches are then shown as CSS
outlines in Firefox.  To show submatches instead use
\\[mozadd-set-outline-regexp-submatch-num].

The style for the outlines is `mozadd-matches-outline-style'.

If `nxml-where-mode' is on its marks will also be shown in
Firefox as CSS outline style.  These outlines have the style
`mozadd-xml-path-outline-style'.

If you are editing a file from a web URL you may want to add a
<base href=... /> tag to get the page looking better in Firefox.
You can add that with the command \\[mozadd-add-href-base].

When updating Firefox the hook `mozadd-send-buffer-hook' is run
first.  \(This adds the CSS outlines above.)

Updating Firefox can also be done automatically.  In this case
every change you make in the buffer will trigger a redraw \(after
a short delay) in Firefox - regardless of if you save the file or
not.  This is maybe slow currently.  However to turn this on use
`mozadd-auto-update-mirror-mode'.

This mode also turn on `mozadd-refresh-edited-on-save-mode'.
Note that the latter can be used when you edit CSS files to
update Firefox when you save the CSS file."
  :lighter " MozMirror"
  :group 'mozadd
  (if mozadd-mirror-mode
      (unless (catch 'ok
                (unless (mozadd-html-buffer-file-p)
                  (mozadd-warning "You can only mirror html file buffers")
                  (throw 'ok nil))
                ;; Fix-me: why did I do this?
                ;; (when (buffer-modified-p)
                ;;   (mozadd-warning "Please save buffer first")
                ;;   (throw 'ok nil))
                (unless (mozadd-moz-process)
                  (throw 'ok nil))
                (mozadd-clear-exec-queue)
                (browse-url-of-file) ;; To open a new tab
                (setq mozadd-edited-buffer (current-buffer))
                (mozadd-add-task (concat "content.location.href = "
                                         "\"file:///" (buffer-file-name) "\";")
                                 'mozadd-get-initial-mirror-location)
                (add-hook 'after-change-functions 'mozadd-auto-update-mozilla t t)
                (add-hook 'nxhtml-where-hook 'mozadd-auto-update-mozilla t t)
                (add-hook 'post-command-hook 'mozadd-edited-buffer-post-command)
                ;; Fix-me: move to isearch-mode-hook, or???
                (define-prefix-command 'mozadd-isearch-control-c-map)
                (define-key isearch-mode-map [(control ?c)] 'mozadd-isearch-control-c-map)
                (define-key isearch-mode-map mozadd-update-key 'mozadd-update-mozilla)
                (define-key isearch-mode-map mozadd-submatch-key 'mozadd-set-outline-regexp-submatch-num)
                (define-key reb-mode-map mozadd-update-key 'mozadd-update-mozilla-from-reb)
                (define-key reb-mode-map mozadd-submatch-key 'mozadd-set-outline-regexp-submatch-from-reb)
                t)
        (setq mozadd-mirror-mode nil))
    (setq mozadd-edited-buffer nil)
    (remove-hook 'post-command-hook 'mozadd-edited-buffer-post-command)
    (remove-hook 'nxhtml-where-hook 'mozadd-auto-update-mozilla t)
    (remove-hook 'after-change-functions 'mozadd-auto-update-mozilla t)
    (define-key isearch-mode-map [(control ?c)] 'isearch-other-meta-char)
    ;;(define-key isearch-mode-map mozadd-update-key 'isearch-other-meta-char)
    ;;(define-key isearch-mode-map mozadd-submatch-key 'isearch-other-meta-char)
    (define-key reb-mode-map mozadd-update-key nil)
    (define-key reb-mode-map mozadd-submatch-key nil)
    )
  (mozadd-refresh-edited-on-save-mode mozadd-mirror-mode))
(put 'mozadd-mirror-mode 'permanent-local t)

(defun mozadd-restart-mirror ()
  "Restart `mozadd-mirror-mode'."
  (interactive)
  ;;(when mozadd-mirror-mode (mozadd-mirror-mode -1))
  ;;(mozadd-mirror-mode 1)
  (when (buffer-live-p inferior-moz-buffer)
    (let ((proc (get-buffer-process inferior-moz-buffer)))
      (when proc (delete-process proc)))
    (kill-buffer inferior-moz-buffer))
  (setq inferior-moz-buffer nil)
  (mozadd-skip-current-task)
  (mozadd-exec-next))

(defun mozadd-edited-buffer-post-command ()
  "Check if we are in a new edited buffer."
  (when mozadd-mirror-mode
    (setq mozadd-edited-buffer (current-buffer))))

(defun mozadd-queue-reload-mozilla-edited-file ()
  "Reload edited file."
  ;; Runs in after-save-hook
  (condition-case err
      (progn
        (when (mozadd-html-buffer-file-p)
          (unless mozadd-mirror-mode
            (mozadd-mirror-mode 1))
          (setq mozadd-edited-buffer (current-buffer)))
        (when (buffer-live-p mozadd-edited-buffer)
          (if (buffer-modified-p mozadd-edited-buffer)
              (mozadd-warning "Mozadd: Edited buffer %s is not saved, can't reload browser."
                              (buffer-name mozadd-edited-buffer))
            (mozadd-add-queue-get-mirror-location)
            (mozadd-add-task-1 'mozadd-send-refresh-edited-to-mozilla))))
    (error (message "mozadd-queue-reload-mozilla-edited-file: %s" (error-message-string err)))))

(defun mozadd-send-refresh-edited-to-mozilla ()
  "Update the remote mozrepl instance"
  (with-current-buffer mozadd-edited-buffer
    (if (not (mozadd-edited-file-is-shown))
        (mozadd-warn-not-shown "Mozadd: Edited buffer %s not in visible tab in Firefox, can't reload it."
                               (buffer-name mozadd-edited-buffer))
      (comint-send-string (inferior-moz-process)
                          "setTimeout(BrowserReload(), \"1000\");")))
  (mozadd-exec-next))


(defvar mozadd-buffer-content-to-mozilla-timer nil)

(defun mozadd-auto-update-mozilla (&rest ignored)
  (when mozadd-auto-update-mirror-mode
    (mozadd-update-mozilla mozadd-auto-update-delay)))
(put 'mozadd-auto-update-mozilla 'permanent-local-hook t)

(defun mozadd-update-mozilla-from-reb ()
  "Update Firefox from re-builder."
  (interactive)
  (with-current-buffer reb-target-buffer
    (mozadd-update-mozilla 0)))

(defun mozadd-update-mozilla (delay)
  "Update Firefox."
  (interactive (list 0))
  (if (not mozadd-mirror-mode)
      (message "This buffer is not mirrored in Firefox")
    (when (timerp mozadd-buffer-content-to-mozilla-timer)
      (cancel-timer mozadd-buffer-content-to-mozilla-timer))
    (setq mozadd-buffer-content-to-mozilla-timer
          (run-with-idle-timer delay nil 'mozadd-queue-send-buffer-content-to-mozilla (current-buffer)))
    (message "Asked Firefox to update %s..." (current-time-string))))

;; (define-minor-mode mozadd-isearch-matches-mode
;;   "Show isearch matches when updating buffer in Firefox.
;; Use the style in `mozadd-matches-outline-style' for the
;; outlines."
;;   :group 'mozadd
;;   (if mozadd-isearch-matches-mode
;;       (add-hook 'mozadd-send-buffer-hook 'mozadd-isearch-send-buffer-hook-fun nil t)
;;     (remove-hook 'mozadd-send-buffer-hook 'mozadd-isearch-send-buffer-hook-fun t)))

(defcustom mozadd-matches-outline-style "1px solid red"
  "CSS style for matches outline when shown in Firefox.
This is added as

  style=\"outline: THIS-STYLE\""
  :type 'string
  :group 'mozadd)

(defcustom mozadd-xml-path-outline-style "1px solid green"
  "CSS style for `nxml-where-mode' outline when shown in Firefox.
This is added as

  style=\"outline: THIS-STYLE\""
  :type 'string
  :group 'mozadd)

(defvar mozadd-outline-regexp-submatch-num 0)
(put 'mozadd-outline-regexp-submatch-num 'permanent-local t)
(defun mozadd-set-outline-regexp-submatch-num (num)
  "Set submatch number for regexp's outlines.
Set submatch num to NUM.

This number is used when showing matches for isearch and
re-builder.  It is per buffer."
  (interactive "NMozadd mirror outline regexp submatch num in this buffer: ")
  (set (make-local-variable 'mozadd-outline-regexp-submatch-num) num))

(defun mozadd-set-outline-regexp-submatch-from-reb (num)
  "Set submatch number for regexp's outlines."
  (interactive "NMozadd mirror outline regexp submatch num for target buffer: ")
  (with-current-buffer reb-target-buffer
    (mozadd-set-outline-regexp-submatch-num num)))

(defun mozadd-isearch-send-buffer-hook-fun (mozadd-points)
  "Add outlines to tags matched by isearch when updating Firefox.
Use the style in `mozadd-matches-outline-style' for the
outlines."
  ;; isearch-lazy-highlight-overlays isearch-overlay
  (when isearch-mode
    (let ((pattern isearch-string)
          (is-regexp isearch-regexp)
          (submatch mozadd-outline-regexp-submatch-num)
          (outline-style mozadd-matches-outline-style))
      (message "isearch mozadd: %s %s %s" pattern is-regexp submatch)
      (mozadd-add-matches-outlines mozadd-points pattern is-regexp submatch outline-style))))

(defun mozadd-re-builder-send-buffer-hook-fun (mozadd-points)
  "Add outlines to tags matched by re-builder when updating Firefox.
Use the style in `mozadd-matches-outline-style' for the
outlines."
  ;; isearch-lazy-highlight-overlays isearch-overlay
  (when (and (eq (current-buffer) reb-target-buffer)
             (or (not reb-valid-string)
                 (zerop (length reb-valid-string))))
    (let ((pattern reb-regexp)
          (is-regexp t)
          (submatch mozadd-outline-regexp-submatch-num)
          (outline-style mozadd-matches-outline-style))
      (message "re-builder mozadd: %s %s %s" pattern is-regexp submatch)
      (mozadd-add-matches-outlines mozadd-points pattern is-regexp submatch outline-style))))

(defun mozadd-add-matches-outlines (mozadd-points pattern is-regexp submatch outline-style)
  (let ((my-points (symbol-value mozadd-points))
        (matches nil) ;;`(,(overlay-start isearch-overlay)))
        tag-starts
        (here (point)))
    (save-match-data
      (goto-char (point-min))
      (while (if is-regexp
                 (re-search-forward pattern nil t)
               (search-forward pattern nil t))
        (let ((match-point (if is-regexp
                               (match-beginning submatch)
                             (- (point) (length pattern))
                             )))
          (message "match-point=%s, submatch=%s, is-regexp=%s" match-point submatch is-regexp)
          (unless (memq match-point matches)
            (setq matches (cons match-point matches)))))
      (dolist (match matches)
        (let (tag-start
              tag-start-end)
          (goto-char match)
          (while (and (not tag-start)
                      (search-backward "<" nil t))
            (unless (eq ?/
                        (char-after (1+ (point))))
              (setq tag-start (point))))
          (when tag-start
            (when (search-forward ">" nil t)
              (setq tag-start-end (1- (point)))
              (unless (memq tag-start-end tag-starts)
                (setq tag-starts (cons tag-start-end tag-starts))))))))
    (dolist (start tag-starts)
      (let ((rec `(,start ,outline-style)))
        (setq my-points (cons rec my-points))))
    (set mozadd-points my-points)
    (goto-char here)))


(provide 'mozadd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mozadd.el ends here
