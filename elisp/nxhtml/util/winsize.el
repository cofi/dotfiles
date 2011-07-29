;;; winsize.el --- Interactive window structure editing
;;
;; Author: Lennart Borgman <lennart dot borgman at gmail dot com >
;; Maintainer:
;; Created: Wed Dec 07 15:35:09 2005
(defconst winsize:version "0.98") ;;Version: 0.97
;; Last-Updated: 2010-05-27 Thu
;; Keywords:
;; Compatibility:
;;
;; Fxeatures that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file contains functions for interactive resizing of Emacs
;; windows.  To use it put it in your `load-path' and add the following
;; to your .emacs:
;;
;;     (require 'winsize)
;;     (global-set-key [(control x) ?+] 'resize-windows)
;;
;; For more information see `resize-windows'.
;;
;; These functions are a slightly rewritten version of the second part
;; of the second part my proposal for a new `balance-windows' function
;; for Emacs 22.  The rewrite is mostly a restructure to more easily
;; add new functions.  All functions and variables have been renamed.
;; The file was originally named bw-interactive.el.
;;
;; New ideas for functionality have been to a large part adopted from
;; the Emacs Devel mailing list.  Probably most of them originated from
;; Drew Adams and Bastien.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: Change mouse pointer shape during resizing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'windmove))
(eval-when-compile (require 'view))
(eval-when-compile (require 'winsav nil t))
(eval-when-compile (require 'ourcomments-widgets))
(eval-when-compile (require 'ring))

;;; Custom variables

(defcustom winsize-juris-way t
  ""
  :type 'boolean
  :group 'winsize)

(defcustom winsize-autoselect-borders t
  "Determines how borders are selected by default.
If nil hever select borders automatically (but keep them on the
same side while changing window).  If 'when-single select border
automatically if there is only one possible choice.  If t alwasy
select borders automatically if they are not selected."
  :type '(choice (const :tag "Always" t)
                 (const :tag "When only one possbility" when-single)
                 (const :tag "Never" nil))
  :group 'winsize)

(defcustom winsize-mode-line-colors (list t (list "green" "green4"))
  "Mode line colors used during resizing."
  :type '(list (boolean :tag "Enable mode line color changes during resizing")
               (list
                (color :tag "- Active window mode line color")
                (color :tag "- Inactive window mode line color")))
  :group 'winsize)

(defcustom winsize-mark-selected-window t
  "Mark selected window if non-nil."
  :type 'boolean
  :group 'winsize)

(defcustom winsize-make-mouse-prominent t
  "Try to make mouse more visible during resizing.
The mouse is positioned next to the borders that you can move.
It can however be hard to see if where it is.  Setting this to on
makes the mouse jump a few times."
  :type 'boolean
  :group 'winsize)

(defvar widget-command-prompt-value-history nil
  "History of input to `widget-function-prompt-value'.")

(defvar winsize-keymap nil
  "Keymap used by `resize-windows'.")

(defun winsize-ignore ()
  (interactive)
  (message "Key is ignored during resizing"))

(defun winsize-make-keymap (let-me-use)
  "Build the keymap that should be used by `winsize-keymap'."
  (let ((map (make-sparse-keymap "Window Resizing")))
    (define-key map [t] 'winsize-ignore)
    (when (featurep 'winsav)
      (define-key map [?|] 'winsav-rotate))
    (define-key map [?+] 'balance-windows)
    (define-key map [?.] 'winsize-balance-siblings)
    ;;(define-key map [?h] 'fit-window-to-buffer)
    (define-key map [?w] 'winsize-fit-window-to-desired-width)
    (define-key map [?W] 'winsize-fit-windows-to-desired-widths)
    (define-key map [?h] 'shrink-window-if-larger-than-buffer)

    (define-key map [?f] 'winsize-fit-frame-width)
    (define-key map [?F] 'winsize-fitw-and-maxh-frame)
    (define-key map [?H] 'winsize-max-frame-height)

    (define-key map [(up)]    'winsize-move-border-up)
    (define-key map [(down)]  'winsize-move-border-down)
    (define-key map [(left)]  'winsize-move-border-left)
    (define-key map [(right)] 'winsize-move-border-right)

    (define-key map [(shift up)]    'winsize-move-other-border-up)
    (define-key map [(shift down)]  'winsize-move-other-border-down)
    (define-key map [(shift left)]  'winsize-move-other-border-left)
    (define-key map [(shift right)] 'winsize-move-other-border-right)

    (define-key map [(meta left)]   'winsize-to-border-or-window-left)
    (define-key map [(meta up)]     'winsize-to-border-or-window-up)
    (define-key map [(meta right)]  'winsize-to-border-or-window-right)
    (define-key map [(meta down)]   'winsize-to-border-or-window-down)

    (define-key map [?0] 'delete-window)
    (define-key map [?1] 'delete-other-windows)
    (define-key map [?2] 'split-window-vertically)
    (define-key map [?3] 'split-window-horizontally)
    (define-key map [?4] 'other-window)

    (define-key map [?!] 'winsize-save-window-configuration)
    (define-key map [?>] 'winsize-next-window-configuration)
    (define-key map [?<] 'winsize-previous-window-configuration)

    ;; Fix-me: These keys could also be set to nil
    (define-key map [mouse-1]                        'mouse-set-point)
    ;;(define-key map [down-mouse-1]                   'mouse-set-point)
    (define-key map [(mode-line) (down-mouse-1)]     'mouse-drag-mode-line)
    (define-key map [(vertical-line) (down-mouse-1)] 'mouse-drag-vertical-line)
    (define-key map [(vertical-scroll-bar) (mouse-1)] 'scroll-bar-toolkit-scroll)

    (define-key map [??] 'winsize-help)
    (define-key map [(control ?g)]     'winsize-quit)
    (define-key map [(control return)] 'winsize-stop-go-back)
    (define-key map [(return)]         'winsize-stop)
    ;;(define-key map [t]                'winsize-stop-and-execute)

    (dolist (ks let-me-use)
      (if (and (not (vectorp ks))
               (not (stringp ks))
               (commandp ks))
          (let ((ks-list (where-is-internal ks)))
            (dolist (ks ks-list)
              (unless (lookup-key map ks)
                (define-key map ks nil))))
        (unless (lookup-key map ks)
          (define-key map ks nil))))

    (setq winsize-keymap map)

    (easy-menu-define nil winsize-keymap
      "Menu for winsize."
      '("WinSize"
        ["Help" winsize-help]
        "--"
        ("Balance or Fit Windows"
         ["Balance Windows" balance-windows]
         ["Balance Siblings" winsize-balance-siblings]
         "--"
         ["Fit Window Width" winsize-fit-window-to-desired-width]
         ["Fit Windows Widths" winsize-fit-windows-to-desired-widths]
         ["Shrink Window If Too High" shrink-window-if-larger-than-buffer]
         "--"
         ["Fit Frame Width" winsize-fit-frame-width]
         ["Max Frame Height" winsize-max-frame-height]
         ["Fit Frame Width + Max Height" winsize-fitw-and-maxh-frame]
         )
        ("Move Borders Step by Step"
         ["Move Bottom Up" winsize-move-border-up     :active (winsize--vertical-moveable)]
         ["Move Bottom Down" winsize-move-border-down :active (winsize--vertical-moveable)]
         ["Move Right Border Left" winsize-move-border-left   :active (winsize--horizontal-moveable)]
         ["Move Right Border Right" winsize-move-border-right :active (winsize--horizontal-moveable)]
         )
        "--"
        ("Jump To New Window"
         ["Up" winsize-to-border-or-window-up       :active (winsize--has-window-above)]
         ["Down" winsize-to-border-or-window-down   :active (winsize--has-window-below)]
         ["Left" winsize-to-border-or-window-left   :active (winsize--has-window-left)]
         ["Right" winsize-to-border-or-window-right :active (winsize--has-window-right)]
         )
        "--"
        ("Divide Or Delete Windows"
         ["Delete Window" delete-window :active (< 1 (length (window-list nil 'no-mini)))]
         ["Delete Other Windows" delete-other-windows]
         ["Split Vertically" split-window-vertically]
         ["Split Horizontally" split-window-horizontally]
         ;;["" other-window]
         )
        "--"
        ["Rotate" winsav-rotate :active (featurep 'winsav)]
        "--"
        ["Exit" winsize-stop-go-back]
        ["Exit With Selected Window" winsize-stop]
        ["Undo And Quit" winsize-quit]
        ))
    ))

(defun winsize--has-window-left ()
  (/= (nth 0 (window-edges (selected-window)))
      (nth 0 (window-edges (frame-root-window)))))

(defun winsize--has-window-above ()
  (/= (nth 1 (window-edges (selected-window)))
      (nth 1 (window-edges (frame-root-window)))))

(defun winsize--has-window-right ()
  (/= (nth 2 (window-edges (selected-window)))
      (nth 2 (window-edges (frame-root-window)))))

(defun winsize--has-window-below ()
  (/= (nth 3 (window-edges (selected-window)))
      (nth 3 (window-edges (frame-root-window)))))

(defun winsize--vertical-moveable ()
  (or (winsize--has-window-above)
      (winsize--has-window-below)))

(defun winsize--horizontal-moveable ()
  (or (winsize--has-window-left)
      (winsize--has-window-right)))

(defcustom winsize-let-me-use '(next-line ;;[(control ?n)]
                                previous-line ;;[(control ?p)]
                                forward-char ;;[(control ?f)]
                                backward-char ;;[(control ?b)]
                                [(home)]
                                [(end)]
                                [(next)]
                                [(prior)]
                                ;; Fix-me: replace this with something
                                ;; pulling in help-event-list:
                                [(f1)]
                                [(control ?h)]
                                execute-extended-command
                                eval-expression)
  "Key sequences or commands that should not be overriden during resize.
The purpose is to make it easier to switch windows.  The functions
`windmove-left' etc depends on the position when chosing the
window to move to."
  :type '(repeat
          (choice
           ;; Note: key-sequence must be before command here, since
           ;; the key sequences seems to match command too.
           key-sequence command))
  :set (lambda (sym val)
         (set-default sym val)
         (winsize-make-keymap val))
  :group 'winsize)

(defcustom winsize-selected-window-face 'winsize-selected-window-face
  "Variable holding face for marking selected window.
This variable may be nil or a face symbol."
  :type '(choice (const :tag "Do not mark selected window" nil)
                 face)
  :group 'winsize)

(defface winsize-selected-window-face
  '((t (:inherit secondary-selection)))
  "Face for marking selected window."
  :group 'winsize)


;;; These variables all holds values to be reset when exiting resizing:

(defvar winsize-old-mode-line-bg nil)
(defvar winsize-old-mode-line-inactive-bg nil)
(defvar winsize-old-overriding-terminal-local-map nil)
(defvar winsize-old-overriding-local-map-menu-flag nil)
(defvar winsize-old-temp-buffer-show-function nil)
(defvar winsize-old-mouse-avoidance-mode nil
  "Hold the value of `mouse-avoidance-mode' at resizing start.")
(defvar winsize-old-view-exit-action nil)
(make-variable-buffer-local 'winsize-old-view-exit-action)

(defvar winsize-message-end nil
  "Marker, maybe at end of message buffer.")

(defvar winsize-resizing nil
  "t during resizing, nil otherwise.")

(defvar winsize-window-config-init nil
  "Hold window configuration from resizing start.")

(defvar winsize-frame nil
  "Frame that `resize-windows' is operating on.")


;;; Borders

(defvar winsize-window-for-side-hor nil
  "Window used internally for resizing in vertical direction.")

(defvar winsize-window-for-side-ver nil
  "Window used internally for resizing in horizontal direction.")

(defvar winsize-border-hor nil
  "Use internally to remember border choice.
This is set by `winsize-pre-command' and checked by
`winsize-post-command', see the latter for more information.

The value should be either nil, 'left or 'right.")

(defvar winsize-border-ver nil
  "Use internally to remember border choice.
This is set by `winsize-pre-command' and checked by
`winsize-post-command', see the latter for more information.

The value should be either nil, 'up or 'down.")

(defvar winsize-window-at-entry nil
  "Window that was selected when `resize-windows' started.")


;;; Keymap, interactive functions etc

(defun winsize-pre-command ()
  "Do this before every command.
Runs this in `pre-command-hook'.

Remember the currently used border sides for resizing. Also
remember position in message buffer to be able to see if next
command outputs some message.

For more information see `winsize-post-command'."
  (setq winsize-message-end (winsize-message-end))
  (setq winsize-border-hor (winsize-border-used-hor))
  (setq winsize-border-ver (winsize-border-used-ver)))

(defun winsize-post-command ()
  "Done after every command.
Run this in `post-command-hook'.

Check the border sides \(left/right, up/down) remembered in
`winsize-pre-command' and use the the same side if possible,
otherwise the opposite side if that is possible. \(This check is
of course not done if the last command changed the border side.)

The reason for selecting borders this way is to try to give the
user a coherent and easy picture of what is going on when
changing window or when window structure is changed.  \(Note that
the commands moving to another window or changing the window
structure does not have to belong to this package. Those commands
can therefore not select the border sides.)

Give the user feedback about selected window and borders.  Also
give a short help message unless last command gave some message."
  (unless winsize-juris-way
    (unless winsize-border-hor
      (winsize-select-initial-border-hor))
    (when winsize-border-hor
      (winsize-set-border winsize-border-hor t))
    (unless winsize-border-ver
     (winsize-select-initial-border-ver))
    (when winsize-border-ver
      (winsize-set-border winsize-border-ver t)))
  (winsize-tell-user))

;;;###autoload
(defun resize-windows ()
  "Start window resizing.
During resizing a window is selected.  You can move its
borders. In the default configuration the arrow keys moves the
right or bottom border if they are there. To move the opposite
border use S-arrowkeys.

You can also do other window operations, like splitting, deleting
and balancing the sizes.  The keybindings below describes the key
bindings during resizing:\\<winsize-keymap>

 `balance-windows'                       \\[balance-windows]
 `winsize-balance-siblings'              \\[winsize-balance-siblings]
 `winsize-fit-window-to-desired-width'   \\[winsize-fit-window-to-desired-width]
 `winsize-fit-windows-to-desired-widths' \\[winsize-fit-windows-to-desired-widths]
 `shrink-window-if-larger-than-buffer'   \\[shrink-window-if-larger-than-buffer]

 `winsize-fit-frame-width'      \\[winsize-fit-frame-width]
 `winsize-fitw-and-maxh-frame'  \\[winsize-fitw-and-maxh-frame]
 `winsize-max-frame-height'     \\[winsize-max-frame-height]

 `winsav-rotate'  \\[winsav-rotate]

 `winsize-move-border-up'      \\[winsize-move-border-up]
 `winsize-move-border-down'    \\[winsize-move-border-down]
 `winsize-move-border-left'    \\[winsize-move-border-left]
 `winsize-move-border-right'   \\[winsize-move-border-right]

 `winsize-to-border-or-window-left'    \\[winsize-to-border-or-window-left]
 `winsize-to-border-or-window-up'      \\[winsize-to-border-or-window-up]
 `winsize-to-border-or-window-right'   \\[winsize-to-border-or-window-right]
 `winsize-to-border-or-window-down'    \\[winsize-to-border-or-window-down]

 `delete-window'                \\[delete-window]
 `delete-other-windows'         \\[delete-other-windows]
 `split-window-vertically'      \\[split-window-vertically]
 `split-window-horizontally'    \\[split-window-horizontally]
 `other-window'                 \\[other-window]

 `winsize-save-window-configuration'       \\[winsize-save-window-configuration]
 `winsize-next-window-configuration'       \\[winsize-next-window-configuration]
 `winsize-previous-window-configuration'   \\[winsize-previous-window-configuration]

 `mouse-set-point'   \\[mouse-set-point]

 `winsize-quit'               \\[winsize-quit]
 `winsize-stop-go-back'       \\[winsize-stop-go-back]
 `winsize-stop'               \\[winsize-stop]
 `winsize-stop-and-execute'   \\[winsize-stop-and-execute]

 `winsize-help'          \\[winsize-help]
 `describe-key'          \\[describe-key]
 `describe-key-briefly'  \\[describe-key-briefly]
 (All the normal help keys work, and at least those above will
  play well with resizing.)

You can use keys and commands listed in `winsize-let-me-use' as
normal.  This means that you by default can use your normal keys
for `forward-char', `backward-char', `next-line',
`previous-line' and what you have on HOME and END to move in the
windows.  That might sometimes be necessary to directly select a
window.  \(You may however also use `other-window' or click with
the mouse, see below.)


The colors of the modelines are changed to those given in
`winsize-mode-line-colors' to indicate that you are resizing
windows.  To make this indication more prominent the text in the
selected window is marked with the face hold in the variable
`winsize-selected-window-face'.

The option `winsize-juris-way' decides how the borders to move
are selected. If this option is non-nil then the right or bottom
border are the ones that are moved with the arrow keys and the
opposite border with shift arrow keys.

If `winsize-juris-way' is nil then the following apply:

As you select other borders or move to new a window the mouse
pointer is moved inside the selected window to show which borders
are beeing moved. The mouse jumps a little bit to make its
position more visible. You can turn this off by customizing
`winsize-make-mouse-prominent'.

Which borders initially are choosen are controlled by the
variable `winsize-autoselect-borders'.

** Example: Border selection, movements and windows.

  Suppose you have a frame divided into windows like in the
  figure below.  If window B is selected when you start resizing
  then \(with default settings) the borders marked with 'v' and
  'h' will be the ones that the arrow keys moves. To indicate
  this the mouse pointer is placed in the right lower corner of
  the selected window B.

    +----------+-----------+--------+
    |          |           v        |
    |          |           v        |
    |    A     |    _B_    v        |
    |          |           v        |
    |          |           v        |
    |          |         x v        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+

  Now if you press M-<left> then the picture below shows what has
  happened. Note that the selected vertical border is now the one
  between A and B. The mouse pointer has moved to the
  corresponding corner in the window B, which is still selected.

    +----------+-----------+--------+
    |          v           |        |
    |          v           |        |
    |    A     v    _B_    |        |
    |          v           |        |
    |          v           |        |
    |          v x         |        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+

  Press M-<left> once again. This gives this picture:

    +----------+-----------+--------+
    |          v           |        |
    |          v           |        |
    |   _A_    v     B     |        |
    |          v           |        |
    |          v           |        |
    |        x v           |        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+

  Note that the window A is now selected. However there is no
  border that could be moved to the left of this window \(which
  would otherwise be chosen now) so the border between A and B is
  still the one that <left> and <right> moves. The mouse has
  moved to A.

  If we now delete window A the new situation will look like
  this:

    +----------+-----------+--------+
    |                      |        |
    |                      |        |
    |         _B_          |        |
    |                      |        |
    |                      |        |
    |                    x |        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+



>>>> testing stuff >>>>
`help-mode-hook'
`temp-buffer-show-function'
`view-exit-action'
<<<<<<<<<<<<<<<<<<<<<<<
"
  (interactive)
  (setq winsize-resizing t)
  ;; Save old values:
  (unless winsize-old-mouse-avoidance-mode
    (setq winsize-old-mouse-avoidance-mode mouse-avoidance-mode))
  ;; Setup user feedback things:
  (mouse-avoidance-mode 'none)
  (winsize-set-mode-line-colors t)
  (winsize-create-short-help-message)
  (setq winsize-message-end (winsize-message-end))
  ;; Save config for exiting:
  (setq winsize-window-config-init (current-window-configuration))
  (setq winsize-window-at-entry (selected-window))
  (setq winsize-frame (selected-frame))
  ;; Setup keymap and command hooks etc:
  (winsize-setup-local-map)
  (winsize-add-command-hooks)
  (setq winsize-window-for-side-hor nil)
  (setq winsize-window-for-side-ver nil))

(defcustom winsize-desired-width 'major
  "Column to use for `winsize-fit-window-to-desired-width' etc."
  :type '(choice (const :tag "Use fill-column" 'fill-column)
                 (const :tag "Per major mode" 'major)
                 integer)
  :group 'winsize)

(defcustom winsize-desired-width-per-mode
  '((fill-column text-mode fundamental-mode org-mode)
    (80))
  "List of windows width for different major modes.
Each entry has the form

  (WIDTH . (MAJ1 MAJ2 ...))

where WIDTH is the desired width and MAJ1 etc are major modes.
The last entry may be \(WIDTH) which catches all modes.

First matching entry is used."
  :type '(repeat (cons (choice (const :tag "Fill column" 'fill-column)
                               integer)
                       (repeat command)))
  :group 'winsize)

(defun winsize-desired-width (window)
  (cond ( (wholenump winsize-desired-width)
          winsize-desired-width)
        ( (eq 'fill-column winsize-desired-width)
          (with-current-buffer (window-buffer window)
            fill-column))
        ( (eq 'major winsize-desired-width)
          (let* ((major (with-current-buffer
                           (window-buffer window)
                         major-mode))
                 (val
                  (catch 'val
                    (dolist (rec winsize-desired-width-per-mode)
                      (when (or (memq major rec)
                                (not (cdr rec)))
                        (throw 'val (car rec)))))))
            (cond
             ((not val) fill-column)
             ((eq val 'fill-column) fill-column)
             ((wholenump val) val)
             (t
              (message
               "Bad car in winsize-desired-width-per-mode: %S" val)
              80))))
        (t
         (message "Bad winsize-desired-width=%S" winsize-desired-width)
         80)))

;;;###autoload
(defun winsize-fit-window-to-desired-width (window only-trailing desired-width)
  "Adjust width of WINDOW to desired width for its buffer.
This will not delete any window but may widen the window as much
as possible up to desired width.

The desired width is defined by `winsize-desired-width'.  It can
a major mode specific \(a number or `fill-column') or fall back
to a number or `fill-column'.

When used non-interactively ONLY-TRAILING can be non-nil and in
this case only the trailing edge of the window can change.

For non-interactive use you can also specify the wanted width in
DESIRED-WIDTH."
  (interactive (list (selected-window) current-prefix-arg nil))
  (let* ((our-frame (window-frame window))
         (our-edges (window-edges window))
         (root-window (frame-root-window our-frame))
         (root-edges (window-edges root-window))
         (our-buffer (window-buffer window))
         (our-desired-width (or desired-width (winsize-desired-width window)))
         (max-width (window-width root-window))
         (our-width (let* ((margs (window-margins window))
                           (lm (or (car margs) 0))
                           (rm (or (cdr margs) 0)))
                      (+ (window-width window) lm rm)))
         ;;(our-width (- (nth 2 our-edges) (nth 0 our-edges)))
         (diff (- (min max-width our-desired-width)
                  our-width))
         (old-wcfg (current-window-configuration our-frame))
         (num-windows (length (window-list our-frame 'no-mini)))
         new-num-windows
         (nn (abs diff))
         (orig-diff diff)
         (problem "Problem fitting window"))
    (cond
     ((= 1 num-windows)
      (message "There is only one window on the frame, can't widen window"))
     ((and (= 0 (nth 0 our-edges)) ;; left
           (= (nth 2 our-edges) (nth 2 root-edges))) ;; right
      (message "This window already is as wide as the frame"))
     (t
      (with-selected-window window
        (catch 'done
          (while (< 0 (setq nn (1- nn)))
            (if only-trailing
                (condition-case err
                    (progn
                      (adjust-window-trailing-edge window diff t)
                      (throw 'done t))
                  (error (setq problem (error-message-string err))))
              (enlarge-window diff t)
              (setq new-num-windows (length (window-list our-frame 'no-mini)))
              (when (= num-windows new-num-windows) (throw 'done 't))
              (setq problem
                    "Could not fit window to desired width without deleting windows")
              (set-window-configuration old-wcfg)
              (setq diff (+ diff (if (> diff 0) -1 1)))))))
      (unless (= diff orig-diff)
        (message "%s" problem))))))

(defvar winsize-windows-desired-width nil
  "Internal use in `winsize-fit-windows-to-desired-widths'.")

;;;###autoload
(defun winsize-fit-window-to-buffer (&optional window max-height min-height)
  "A more complete `fit-window-to-buffer'. Fix-me: not ready, bug# 7822.
Find through an iterative search minimal height to display whole
buffer \(narrowed part if narrowed) and set window height to that
height.  Or, if that can not be done then set the height to the
best possible height for fit.

Try first to adjust window below and if that is not enough window
above.
"
  (let* ((window (or window (selected-window)))
         (frm (window-frame window))
         (frm-height (frame-height frm))
         here
         (above (windmove-find-other-window 'up   nil window))
         (below (windmove-find-other-window 'down nil window))
         ;;(wcfg (current-window-configuration frm))
         window-configuration-change-hook
         (eob-in-win (= (window-end window t) (point-max)))
         (curh (window-height window))
         (minh (if (not eob-in-win)
                   curh
                 (or min-height window-min-height)))
         (orig-minh minh)
         (maxh (if eob-in-win
                   curh
                 (or max-height frm-height)))
         (orig-maxh maxh)
         midh
         done)
    (when (window-minibuffer-p below)
      (setq below nil))

    (when (or above below)
      (with-current-buffer (window-buffer window)
        (setq here (point))

        ;; First try resizing window below.
        (when below
          (while (and (not done) (> maxh minh))
            (setq midh (+ minh (/ (- maxh minh) 2)))
            (let* ((winh (window-height window))
                   (delta (- midh winh))
                   did-it)
              (condition-case err
                  (progn
                    (adjust-window-trailing-edge window delta nil)
                    (setq did-it t))
                (error
                 (message "%S" err)))
              (if did-it
                  (progn
                    (goto-char (point-min))
                    ;; Fix-me: This unfortunately returns t even if
                    ;; the last line is partly hidden (test example
                    ;; help for posn-at-x-y):
                    (setq eob-in-win (= (window-end window t) (point-max)))
                    ;; So let us try another way to check if eob is in window:
                    (let* ((edges (window-inside-pixel-edges))
                           (left (1+ (nth 0 edges)))
                           (bottom (1- (nth 3 edges))))
                      (setq eob-in-win
                            (= (point-max)
                               (posn-point (posn-at-x-y left bottom frm)))))
                    (if eob-in-win
                        ;; Fix-me: This assumes that posn-at-point is
                        ;; relative to window text area.
                        (let* ((loc (event-end (posn-at-point (point-max) window)))
                               (loc-bottom (cdr loc))
                               (edges (window-inside-pixel-edges window))
                               (win-top (nth 1 edges))
                               (win-bottom (nth 3 edges))
                               (win-rel-bottom (- win-bottom win-top))
                               ;; Fix-me: This returns nil even though
                               ;; we have updated above with
                               ;; (window-end window t) and also after
                               ;; (redisplay t) or (sit-for 0).
                               (dummy (redisplay t))
                               (dummy2 (sit-for 0))
                               (bottom-line-height (window-line-height
                                                    (line-number-at-pos (1- (point-max)))
                                                    window)))
                          ;; Make a guess if we are ready.
                          (when (> bottom-line-height
                                   (- win-rel-bottom loc-bottom))
                            (setq done t))
                          (setq maxh midh))
                      (setq minh midh)))
                (if (< 0 delta)
                    (setq maxh (1- maxh))
                  (setq minh (1+ minh))))
              )))

        ;; If we are not done try window above.
        (when above
          (while (and (not done) (> maxh minh))
            (setq midh (+ minh (/ (- maxh minh) 2)))
            (let* ((winh (window-height window))
                   (delta (- winh midh))
                   did-it)
              ;; (when (> 0 delta) ;; Check window above min height
              ;;   (setq delta (max delta
              ;;                    (- window-min-height
              ;;                       (window-height above))))
              ;;   (setq midh (- delta winh)))
              (condition-case err
                  (progn
                    (adjust-window-trailing-edge above delta nil)
                    (setq did-it t))
                (error
                 (message "%S" err)))
              (if did-it
                  (progn
                    (goto-char (point-min))
                    (setq eob-in-win (= (window-end window t) (point-max)))
                    (if eob-in-win
                        (setq maxh midh)
                      (setq minh midh)))
                (if (> 0 delta)
                    (setq maxh (1- maxh))
                  (setq minh (1+ minh))))
              )))
        (goto-char here))
      )))

;;(winsize-fit-windows-to-desired-widths)
;;;###autoload
(defun winsize-fit-windows-to-desired-widths ()
  "Fit window width to desired width for buffers.
Set widths by calling `winsize-fit-window-to-desired-width'.

Change windows in the order of left to right and only change
trailing edges.  \(This means that the windows on the right edge
of the frame gets the resulting width of the changes to the width
of the windows left of them.)"
  (interactive)
  (winsize-fit-windows-to-desired-widths-1 nil))

(defun winsize-fit-windows-to-desired-widths-1 (resize-frame)
  (when (catch 'redo-after-wcfg
          (let* ((our-tree (window-tree))
                 (our-sizes (winsize-get-window-tree-desired-width-1
                             (car our-tree)))
                 (tot-width (nth 1 our-sizes)))
            ;;(setq x our-sizes) ;; Debug.

            ;; Just collect the windows and sizes first. Sort them
            ;; according to left column. Then set sizes in that order.
            (setq winsize-windows-desired-width nil)
            (winsize-collect-window-tree-desired-width-1 our-sizes)
            (setq winsize-windows-desired-width
                  (sort winsize-windows-desired-width
                        ;; Sort from left to right.
                        (lambda (a b)
                          (< (car a) (car b)))))
            (when resize-frame
              (let* ((frame (selected-frame))
                     (curr-width (frame-width frame)))
                (when (/= curr-width tot-width)
                  (when (fboundp 'w32-frame-placement)
                    (let* ((pm (w32-frame-placement frame))
                           (state (nth 4 pm)))
                      (when (/= state 2) ;; Not normal, i.e. min/max/hidden
                        ;; SW_NORMAL
                        (w32-showwindow frame 1)
                        ;; We have to do it again when frame is
                        ;; redisplayed.
                        (throw 'redo-after-wcfg t))))
                  (set-frame-size (selected-frame) tot-width
                                  (frame-height (selected-frame))))))
            (dolist (rec winsize-windows-desired-width)
              (let ((win (nth 1 rec))
                    (width-goal (nth 2 rec)))
                (when (< 0 width-goal)
                  (winsize-fit-window-to-desired-width win t width-goal))
                ;;(msgtrc "after set: %S => %s, buf=%S, %d" win width-goal (window-buffer win) (nth 0 (window-edges win)))
                )))
          ;;(remove-hook 'window-configuration-change-hook this-command)
          (remove-hook 'window-configuration-change-hook 'winsize-fitw-and-maxh-frame)
          (remove-hook 'window-configuration-change-hook 'winsize-fit-windows-to-desired-widths)
          nil)
    (when (memq this-command
                '(winsize-fitw-and-maxh-frame winsize-fit-windows-to-desired-widths))
      (add-hook 'window-configuration-change-hook this-command))
    ;;window-configuration-change-hook
    ))

;;;###autoload
(defun winsize-max-frame-height ()
  "Maximize frame height."
  (interactive)
  (let* ((frame (selected-frame))
        (w32-pm(when (fboundp 'w32-frame-placement)
                 (w32-frame-placement frame)))
        (is-max (when w32-pm
                  (let ((state (nth 4 w32-pm)))
                    (= state 3))))
        (char-height-pixels (frame-char-height frame))
        (root-window-rows (window-height (frame-root-window frame)))
        (other-pixels (- (frame-pixel-height frame)
                         (* root-window-rows char-height-pixels)))
        (pix-diff (- (display-pixel-height)
                     (frame-pixel-height frame)
                     other-pixels))
        ;; fix-me: Why -1???
        (char-diff (1- (floor (/ pix-diff char-height-pixels))))
        (left (frame-parameter frame 'left))
        (top (frame-parameter frame 'top))
        (cols (frame-width frame))
        (rows (window-height (frame-root-window frame)))
        )
    ;; Fix-me: bug in Emacs? No, propably I am missing the tool-bar
    ;; height here... For now remove 3 lines
    (setq rows (- rows 3))
    (unless is-max
      ;; Fix-me: There is a bug in w32 Emacs here. Setting just 'top
      ;; will make 'left to the value that it had when the frame was
      ;; maximized if it was maximized before. The correct value is in
      ;; the placement record, use that instead.
      (when (listp left) ;; was max
        (setq left (nth 0 w32-pm))
        (when left (set-frame-parameter frame 'left left)))
      (set-frame-parameter frame 'top 0)
      (set-frame-size frame cols (+ rows char-diff)))))

;;;###autoload
(defun winsize-fitw-and-maxh-frame ()
  "Fit width and max height of frame.
Just like `winsize-fit-frame-width' + `winsize-max-frame-height'."
  (interactive)
  (winsize-fit-frame-width)
  (winsize-max-frame-height))

;;;###autoload
(defun winsize-fit-frame-width ()
  "Fit frame FRAME to buffers desired width.
Call `winsize-fit-window-to-desired-width' for all windows and
adjust frame width to the resulting width.
"
  (interactive)
  (winsize-fit-windows-to-desired-widths-1 t))

(defun winsize-collect-window-tree-desired-width-1 (tree-sizes)
  (let* ((vertical (nth 0 tree-sizes))
         (tot-width (nth 1 tree-sizes))
         (wins-and-trees (nth 2 tree-sizes)))
    ;; Do it top-down, i.e. windows first
    (dolist (win-or-tree wins-and-trees)
      (when (windowp (car win-or-tree))
        (let* ((win (car win-or-tree))
               (width-goal (if vertical tot-width (nth 1 win-or-tree))))
          ;;(msgtrc "collect: %S => %s, buf=%S, %d" win width-goal (window-buffer win) (nth 0 (window-edges win)))
          (when width-goal
            ;; Don't set, just collect. This makes it possible to set
            ;; the sizes in a better order (i.e. from left to right).
            (let* ((left-edge (nth 0 (window-edges win)))
                   (cell (list left-edge win width-goal)))
              (setq winsize-windows-desired-width
                    (cons cell
                          winsize-windows-desired-width)))))))
    (dolist (win-or-tree wins-and-trees)
      (unless (windowp (car win-or-tree))
        (winsize-collect-window-tree-desired-width-1 win-or-tree)))))

(defun winsize-get-window-tree-desired-width-1 (tree)
  (if (not (listp tree))
      ;; Just one window.
      (list t (winsize-desired-width tree))
    (let* ((vertical (nth 0 tree))
           (wins-and-trees (nthcdr 2 tree))
           result
           (node-width 0))
      (dolist (win-or-tree wins-and-trees)
        (let (desired-width
              res-rec)
          (setq res-rec
                (if (windowp win-or-tree)
                    (let* ((win win-or-tree)
                           (win-right (nth 2 (window-edges win)))
                           (root-right (nth 2 (window-edges (frame-root-window))))
                           (win-desired (if (= win-right root-right)
                                            (- (winsize-desired-width win))
                                          (winsize-desired-width win))))
                      (list win win-desired))
                  (winsize-get-window-tree-desired-width-1 win-or-tree)))
          (setq result (cons res-rec result))))
      (dolist (rec result)
        (let ((type (nth 0 rec))
              (width (or (abs (nth 1 rec)) ;; Right most negative.
                         0)))
          (if vertical
              (when (> width node-width) (setq node-width width))
            (setq node-width (+ width node-width))
            )))
      (list vertical node-width (reverse result)))))


(defun winsize-setup-local-map ()
  "Setup an overriding keymap and use this during resizing.
Save current keymaps."
  ;; Fix-me: use copy-keymap for old?
  (unless winsize-old-overriding-terminal-local-map
    (setq winsize-old-overriding-terminal-local-map overriding-terminal-local-map))
  (setq overriding-terminal-local-map (copy-keymap winsize-keymap))
  (setq winsize-old-overriding-local-map-menu-flag overriding-local-map-menu-flag)
  (setq overriding-local-map-menu-flag t))

(defun winsize-restore-local-map ()
  "Restore keymaps saved by `winsize-setup-local-map'."
  (setq overriding-terminal-local-map winsize-old-overriding-terminal-local-map)
  (setq winsize-old-overriding-terminal-local-map nil)
  (setq overriding-local-map-menu-flag winsize-old-overriding-local-map-menu-flag)
  (setq winsize-old-overriding-local-map-menu-flag nil))


(defvar winsize-window-config-help nil
  "Hold window configuration when help is shown.")

(defvar winsize-window-config-init-help nil
  "Hold window configuration from resizing start during help.")

(defvar winsize-help-frame nil
  "The frame from which help was called.")

(defun winsize-restore-after-help (buffer)
  "Restore window configuration after help.
Raise frame and reactivate resizing."
  (remove-hook 'temp-buffer-setup-hook 'winsize-help-mode-hook-function)
  (setq temp-buffer-show-function winsize-old-temp-buffer-show-function)
  ;; Get rid of the view exit action and the extra text in the help
  ;; buffer:
  (with-current-buffer (help-buffer)
    (setq view-exit-action winsize-old-view-exit-action)
    (setq winsize-old-view-exit-action nil)
    (let ((here (point-marker))
          (inhibit-read-only t))
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (point-min) (point))
      (goto-char (point-max))
      (forward-line -2)
      (delete-region (point) (point-max))
      (goto-char here)))
  ;; Restart resizing, restoring window configurations:
  (when (select-frame winsize-help-frame)
    (raise-frame)
    (set-window-configuration winsize-window-config-help)
    (resize-windows)
    (setq winsize-window-config-init winsize-window-config-init-help)))

(defun winsize-help-mode-hook-function ()
  "Setup temp buffer show function to only run second step.
The first step, `winsize-temp-buffer-show-function', has already been run."
  (setq temp-buffer-show-function 'winsize-temp-buffer-show-function-1))

(defun winsize-temp-buffer-show-function (buffer)
  "First step of setup for showing help during resizing.
This step is run when showing help during resizing.

Save window configuration etc to be able to resume resizing. Stop
resizing. Delete other windows.

Run second step (`winsize-temp-buffer-show-function-1') and
arrange so that second step is run when following help links."
  (setq winsize-window-config-help (current-window-configuration))
  (setq winsize-window-config-init-help winsize-window-config-init)
  (setq winsize-help-frame (selected-frame))
  (winsize-stop)
  (delete-other-windows)
  (winsize-temp-buffer-show-function-1 buffer)
  (add-hook 'temp-buffer-setup-hook 'winsize-help-mode-hook-function))

(defun winsize-temp-buffer-show-function-1 (buffer)
  "Second step of setup for showing help during resizing.
This is run after the first step when accessing help during
resizing. It is also when following help links."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-read-only t) ;; It is reverted in `help-mode-finish'
          )
      (run-hooks 'temp-buffer-show-hook))
    (let ((here (point-marker))
          (str "*** Type q to return to window resizing ***"))
      (put-text-property 0 (length str) 'face 'highlight str)
      (goto-char (point-min))
      (insert str "\n\n")
      (goto-char (point-max))
      (insert "\n\n" str)
      (goto-char here)
      (setq buffer-read-only t))
    (unless winsize-old-view-exit-action
      (setq winsize-old-view-exit-action view-exit-action)
      (setq view-exit-action 'winsize-restore-after-help))
    (goto-char (point-min)))
  (set-window-buffer (selected-window) buffer)
  (message "Type q to return to window resizing"))

(defun winsize-help ()
  "Give help during resizing.
Save current window configuration and pause resizing."
  (interactive)
  (if pop-up-frames
      (progn
        (winsize-exit-resizing nil)
        (describe-function 'resize-windows))
    ;; Fix-me: move setup of view-exit-action etc here. Or was it
    ;; temp-buffer-show-function?
    ;; Setup help hooks etc:
    (unless (or winsize-old-temp-buffer-show-function
                ;; These things should not happen... :
                (eq temp-buffer-show-function 'winsize-temp-buffer-show-function)
                (eq temp-buffer-show-function 'winsize-temp-buffer-show-function-1))
      (setq winsize-old-temp-buffer-show-function temp-buffer-show-function))
    (setq temp-buffer-show-function 'winsize-temp-buffer-show-function)
    (with-output-to-temp-buffer (help-buffer)
      (help-setup-xref (list #'winsize-help) (interactive-p))
      (with-current-buffer (help-buffer)
        (insert "resize-windows is ")
        (describe-function-1 'resize-windows)))))

(defun winsize-quit ()
  "Quit resing, restore window configuration at start."
  (interactive)
  (set-window-configuration winsize-window-config-init)
  (winsize-exit-resizing nil))

(defun winsize-stop-go-back ()
  "Exit window resizing.  Go back to the window started in."
  (interactive)
  (winsize-exit-resizing nil t))

(defun winsize-stop-and-execute ()
  "Exit window resizing and put last key on the input queue.
Select the window marked during resizing before putting back the
last key."
  ;; Fix-me: maybe replace this with a check of this-command in
  ;; post-command-hook instead?
  (interactive)
  (winsize-exit-resizing t))

(defun winsize-stop ()
  "Exit window resizing.
Select the window marked during resizing."
  (interactive)
  (winsize-exit-resizing nil))

;;;###autoload
(defun winsize-balance-siblings ()
  "Make current window siblings the same height or width.
It works the same way as `balance-windows', but only for the
current window and its siblings."
  (interactive)
  (balance-windows (selected-window)))

(defun winsize-to-border-or-window-left ()
  "Switch to border leftwards, maybe moving to next window.
If already at the left border, then move to left window, the same
way `windmove-left' does."
  (interactive) (winsize-switch-border 'left t))

(defun winsize-to-border-or-window-right ()
  "Switch to border rightwards, maybe moving to next window.
For more information see `winsize-to-border-or-window-left'."
  (interactive) (winsize-switch-border 'right t))

(defun winsize-to-border-or-window-up ()
  "Switch to border upwards, maybe moving to next window.
For more information see `winsize-to-border-or-window-left'."
  (interactive) (winsize-switch-border 'up t))

(defun winsize-to-border-or-window-down ()
  "Switch to border downwards, maybe moving to next window.
For more information see `winsize-to-border-or-window-left'."
  (interactive) (winsize-switch-border 'down t))


(defun winsize-move-border-left ()
  "Move border left, but select border first if not done."
  (interactive) (winsize-resize 'left nil))

(defun winsize-move-border-right ()
  "Move border right, but select border first if not done."
  (interactive) (winsize-resize 'right nil))

(defun winsize-move-border-up ()
  "Move border up, but select border first if not done."
  (interactive) (winsize-resize 'up nil))

(defun winsize-move-border-down ()
  "Move border down, but select border first if not done."
  (interactive) (winsize-resize 'down nil))


(defun winsize-move-other-border-left ()
  "Move border left, but select border first if not done."
  (interactive) (winsize-resize 'left t))

(defun winsize-move-other-border-right ()
  "Move border right, but select border first if not done."
  (interactive) (winsize-resize 'right t))

(defun winsize-move-other-border-up ()
  "Move border up, but select border first if not done."
  (interactive) (winsize-resize 'up t))

(defun winsize-move-other-border-down ()
  "Move border down, but select border first if not done."
  (interactive) (winsize-resize 'down t))


;;; Internals



(defun winsize-exit-resizing (put-back-last-event &optional stay)
  "Stop window resizing.
Put back mode line colors and keymaps that were changed.

Upon exit first select window.  If STAY is non-nil then select
the window which was selected when `resize-windows' was called,
otherwise select the last window used during resizing.  After
that, if PUT-BACK-LAST-EVENT is non-nil, put back the last input
event on the input queue."
  (setq winsize-resizing nil)
  ;; Reset user feedback things:
  (mouse-avoidance-mode winsize-old-mouse-avoidance-mode)
  (setq winsize-old-mouse-avoidance-mode nil)
  (winsize-set-mode-line-colors nil)
  (winsize-mark-selected-window nil)
  ;; Remove all hooks etc for help:
  (if (or (eq winsize-old-temp-buffer-show-function 'winsize-temp-buffer-show-function)
          (eq winsize-old-temp-buffer-show-function 'winsize-temp-buffer-show-function-1))
      (setq temp-buffer-show-function nil)
    (setq temp-buffer-show-function winsize-old-temp-buffer-show-function))
  (setq winsize-old-temp-buffer-show-function nil)
  (remove-hook 'help-mode-hook 'winsize-help-mode-hook-function)
  (remove-hook 'temp-buffer-setup-hook 'winsize-help-mode-hook-function)
  ;; Restore keymap and command hooks:
  (winsize-restore-local-map)
  (winsize-remove-command-hooks)
  ;; Exit:
  (when stay (select-window winsize-window-at-entry))
  (message "Exited window resizing")
  (when (and put-back-last-event)
    ;; Add this to the input queue again:
    (isearch-unread last-command-event)))

(defun winsize-add-command-hooks ()
  (add-hook 'pre-command-hook 'winsize-pre-command)
  (add-hook 'post-command-hook 'winsize-post-command))

(defun winsize-remove-command-hooks ()
  (remove-hook 'pre-command-hook 'winsize-pre-command)
  (remove-hook 'post-command-hook 'winsize-post-command))


;;; Borders

(defun winsize-border-used-hor ()
  "Return the border side used for horizontal resizing."
  (let ((hor (when winsize-window-for-side-hor
               (if (eq (selected-window) winsize-window-for-side-hor)
                   'right
                 'left))))
    hor))

(defun winsize-border-used-ver ()
  "Return the border side used for vertical resizing."
  (let ((ver (when winsize-window-for-side-ver
               (if (eq (selected-window) winsize-window-for-side-ver)
                   'down
                 'up))))
    ver))

(defun winsize-switch-border (dir allow-windmove)
  "Switch border that is beeing resized.
Switch to border in direction DIR.  If ALLOW-WINDMOVE is non-nil
then change window if necessary, otherwise stay and do not change
border."
  (let* ((window-in-that-dir (windmove-find-other-window
                              dir nil (selected-window))))
    (when (window-minibuffer-p window-in-that-dir)
      (setq window-in-that-dir nil))
    (if winsize-juris-way
        (if (not window-in-that-dir)
            (message "No window in that direction")
          (windmove-do-window-select dir nil))
      (if (not window-in-that-dir)
          (message "No window or border in that direction")
        (let* ((is-hor (memq dir '(left right)))
               (border-used (if is-hor
                                (winsize-border-used-hor)
                              (winsize-border-used-ver)))
               (using-dir-border (eq dir border-used)))
          (if using-dir-border
              (when allow-windmove
                (setq winsize-window-for-side-hor nil)
                (setq winsize-window-for-side-ver nil)
                (windmove-do-window-select dir nil)
                (message "Moved to new window"))
            (winsize-select-border dir)
            (message "Switched to border %swards" dir)))))))


(defun winsize-select-initial-border-hor ()
  "Select a default border horizontally."
  (if winsize-juris-way
      (winsize-set-border 'right t)
    (let ((has-left  (winsize-window-beside (selected-window) 'left))
          (has-right (winsize-window-beside (selected-window) 'right)))
      (cond
       ((not winsize-autoselect-borders) t)
       ((eq winsize-autoselect-borders 'when-single)
        (when (= 1 (length (delq nil (list has-left has-right))))
          (winsize-select-border 'right)))
       (t
        (winsize-select-border 'right))))))

(defun winsize-select-initial-border-ver ()
  "Select a default border vertically."
  (if winsize-juris-way
      (winsize-set-border 'up t)
    (let ((has-up  (winsize-window-beside (selected-window) 'up))
          (has-down (winsize-window-beside (selected-window) 'down)))
      (cond
       ((not winsize-autoselect-borders) t)
       ((eq winsize-autoselect-borders 'when-single)
        (when (= 1 (length (delq nil (list has-up has-down))))
          (winsize-select-border 'up)))
       (t
        (winsize-select-border 'up))))))

(defun winsize-select-border (dir)
  "Select border to be set for resizing.
The actually setting is done in `post-command-hook'."
  (cond
   ((memq dir '(left right))
    (setq winsize-border-hor dir))
   ((memq dir '(up down))
    (setq winsize-border-ver dir))
   (t (error "Bad DIR=%s" dir))))

(defun winsize-set-border (dir allow-other-side)
  "Set border for resizing."
  (let ((window-beside (winsize-window-beside (selected-window) dir))
        (horizontal (memq dir '(left right))))
    (unless window-beside
      (when allow-other-side
        (setq dir (winsize-other-side dir))
        (setq window-beside
              (winsize-window-beside (selected-window) dir))))
    (if horizontal
        (progn
          (setq winsize-border-hor nil)
          (setq winsize-window-for-side-hor nil))
      (setq winsize-border-ver nil)
      (setq winsize-window-for-side-ver nil))
    (when window-beside
      (let ((window-for-side (if (memq dir '(right down))
                                 (selected-window)
                               window-beside)))
        (if horizontal
            (setq winsize-window-for-side-hor window-for-side)
          (setq winsize-window-for-side-ver window-for-side))))))

(defun winsize-resize (dir other-side)
  "Choose border to move.  Or if border is chosen move that border.
Used by `winsize-move-border-left' etc."
  (when winsize-juris-way
    (let ((bside (if (memq dir '(left right))
                     (if other-side 'left 'right)
                   (if other-side 'up 'down))))
      (winsize-set-border bside t)))
  (let* ((horizontal (memq dir '(left right)))
         (arg (if (memq dir '(left up)) -1 1))
         (window-for-side (if horizontal 'winsize-window-for-side-hor 'winsize-window-for-side-ver))
         (window-for-side-val (symbol-value window-for-side)))
    (if (not window-for-side-val)
        (winsize-select-border dir)
      (when (and winsize-resizing
                 (not (eq window-for-side-val 'checked)))
        (condition-case err
            (adjust-window-trailing-edge (symbol-value window-for-side) arg horizontal)
          (error (message "%s" (error-message-string err))))))))

(defun winsize-other-side (side)
  "Return other side for 'left etc, ie 'left => 'right."
  (cond
    ((eq side 'left) 'right)
    ((eq side 'right) 'left)
    ((eq side 'up) 'down)
    ((eq side 'down) 'up)
    (t (error "Invalid SIDE=%s" side))))

(defun winsize-window-beside (window side)
  "Return a window directly beside WINDOW at side SIDE.
That means one whose edge on SIDE is touching WINDOW.  SIDE
should be one of 'left, 'up, 'right and 'down."
  (require 'windmove)
  (let* ((windmove-wrap-around nil)
         (win (windmove-find-other-window side nil window)))
    (unless (window-minibuffer-p win)
      win)))


;;; Window configs

(defconst winsize-window-configuration-ring (make-ring 20)
  "Hold window configurations.")

(defun winsize-ring-rotate (ring forward)
  (when (< 1 (ring-length ring))
    (if forward
        (ring-insert ring (ring-remove ring nil))
      (ring-insert-at-beginning ring (ring-remove ring 0)))))

(defun winsize-ring-index (ring elem)
  (let ((memb (member elem (ring-elements ring))))
    (when memb
      (- (ring-length ring)
         (length memb)))))

(defun winsize-previous-window-configuration ()
  (interactive)
  (winsize-goto-window-configuration nil))

(defun winsize-next-window-configuration ()
  (interactive)
  (winsize-goto-window-configuration t))

(defun winsize-goto-window-configuration (forward)
  (let* ((curr-conf (current-window-configuration))
         (ring winsize-window-configuration-ring)
         (idx (winsize-ring-index ring curr-conf)))
    (if idx
        (progn
          (setq idx (if forward (1- idx) (1+ idx)))
          (set-window-configuration (ring-ref ring idx)))
      ;; Unfortunately idx often seems to be nil so we will have to
      ;; rotate the ring (or something similar).
      (winsize-ring-rotate ring forward)
      (set-window-configuration (ring-ref ring 0)))))

;;;###autoload
(defun winsize-save-window-configuration ()
  (interactive)
  ;; Fix-me: Isn't there something like winring to use instead??
  (let* ((curr-conf (current-window-configuration))
         (ring winsize-window-configuration-ring))
    (if (winsize-ring-index ring curr-conf)
        (error "Current configuration was already stored")
      (ring-insert ring curr-conf)
      (message "Saved window config, use '<' or '>' to get it back"))))


;;; User feedback

;;;###autoload
(defun winsize-set-mode-line-colors (on)
  "Turn mode line colors on if ON is non-nil, otherwise off."
  (if on
      (progn
        (unless winsize-old-mode-line-inactive-bg
          (setq winsize-old-mode-line-inactive-bg (face-attribute 'mode-line-inactive :background)))
        (unless winsize-old-mode-line-bg
          (setq winsize-old-mode-line-bg (face-attribute 'mode-line :background)))
        (let* ((use-colors (car winsize-mode-line-colors))
               (colors (cadr winsize-mode-line-colors))
               (active-color (elt colors 0))
               (inactive-color (elt colors 1)))
          (when use-colors
            (set-face-attribute 'mode-line-inactive nil :background inactive-color)
            (set-face-attribute 'mode-line nil :background active-color))))
    (when winsize-old-mode-line-inactive-bg
      (set-face-attribute 'mode-line-inactive nil :background winsize-old-mode-line-inactive-bg))
    (setq winsize-old-mode-line-inactive-bg nil)
    (when winsize-old-mode-line-bg
      (set-face-attribute 'mode-line nil :background winsize-old-mode-line-bg))
    (setq winsize-old-mode-line-bg nil)))

(defvar winsize-short-help-message nil
  "Short help message shown in echo area.")

(defun winsize-create-short-help-message ()
  "Create short help message to show in echo area."
  (let ((msg ""))
    (mapc (lambda (rec)
            (let ((fun (elt rec 0))
                  (desc (elt rec 1))
                  (etc (elt rec 2)))
              (when (< 0 (length msg))
                (setq msg (concat msg ", ")))
              (setq msg (concat msg
                                desc
                                ":"
                                (key-description
                                 (where-is-internal fun winsize-keymap t))
                                (if etc " etc" "")))))
          '(
            (balance-windows "balance" nil)
            (winsize-move-border-left "resize" t)
            (winsize-to-border-or-window-left "border" nil)
            ))
    (setq msg (concat msg ", exit:RET, help:?"))
    (setq winsize-short-help-message msg)))

(defun winsize-move-mouse-to-resized ()
  "Move mouse to show which border(s) are beeing moved."
  (let* ((edges (window-edges (selected-window)))
         (L (nth 0 edges))
         (T (nth 1 edges))
         (R (nth 2 edges))
         (B (nth 3 edges))
         (x (/ (+ L R) 2))
         (y (/ (+ T B) 2)))
    (when (and winsize-window-for-side-hor
               (not (eq winsize-window-for-side-hor 'checked)))
      (setq x (if (eq (selected-window) winsize-window-for-side-hor) (- R 6) (+ L 2))))
    (when (and winsize-window-for-side-ver
               (not (eq winsize-window-for-side-ver 'checked)))
      (setq y (if (eq (selected-window) winsize-window-for-side-ver) (- B 2) (+ T 0))))
    (set-mouse-position (selected-frame) x y)))

(defvar winsize-selected-window-overlay nil)

(defun winsize-mark-selected-window (active)
  (when winsize-selected-window-overlay
    (delete-overlay winsize-selected-window-overlay)
    (setq winsize-selected-window-overlay nil))
  (when active
    (with-current-buffer (window-buffer (selected-window))
      (let ((ovl (make-overlay (point-min) (point-max) nil t)))
        (setq winsize-selected-window-overlay ovl)
        (overlay-put ovl 'window (selected-window))
        (overlay-put ovl 'pointer 'arrow)
        (overlay-put ovl 'priority 1000)
        (when winsize-selected-window-face
          (overlay-put ovl 'face winsize-selected-window-face))))))

(defun winsize-message-end ()
  "Return a marker at the end of the message buffer."
  (with-current-buffer (get-buffer-create "*Messages*")
    (point-max-marker)))

(defvar winsize-move-mouse 1)

(defvar winsize-make-mouse-prominent-timer nil)

(defun winsize-move-mouse ()
  ;;(setq winsize-move-mouse (- winsize-move-mouse))
  (save-match-data ;; runs in timer
    (let* ((fxy (mouse-pixel-position))
           (f (car fxy))
           (x (cadr fxy))
           (y (cddr fxy))
           (m (mod winsize-move-mouse 2))
           (d (* (if (= 0 m) 1 -1) 1)))
      (set-mouse-pixel-position f (+ d x) (+ d y))
      (when (< 1 winsize-move-mouse)
        (setq winsize-move-mouse (1- winsize-move-mouse))
        (setq winsize-make-mouse-prominent-timer
              (run-with-timer 0.2 nil 'winsize-move-mouse))))))

(defun winsize-make-mouse-prominent-f (doit)
  (when (and winsize-make-mouse-prominent-timer
             (timerp winsize-make-mouse-prominent-timer))
    (cancel-timer winsize-make-mouse-prominent-timer))
  (when doit
    (setq winsize-move-mouse 3)
    (setq winsize-make-mouse-prominent-timer
          (run-with-idle-timer 0.1 nil 'winsize-move-mouse))))

(defun winsize-tell-user ()
  "Give the user feedback."
  (when winsize-mark-selected-window
    (winsize-mark-selected-window t))
  (unless winsize-juris-way
    (let ((move-mouse (not (member this-command
                                   '(mouse-drag-mode-line
                                     mouse-drag-vertical-line
                                     scroll-bar-toolkit-scroll)))))
      ;;(message "%s, move-mouse=%s" this-command move-mouse);(sit-for 2)
      (when move-mouse
        (winsize-move-mouse-to-resized))
      (when winsize-make-mouse-prominent
        (winsize-make-mouse-prominent-f move-mouse))))
  (when (= winsize-message-end (winsize-message-end))
    (message "%s" winsize-short-help-message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window rotating and mirroring

;;;###autoload
(defun winsav-rotate (mirror transpose)
  "Rotate window configuration on selected frame.
MIRROR should be either 'mirror-left-right, 'mirror-top-bottom or
nil.  In the first case the window configuration is mirrored
vertically and in the second case horizontally.  If MIRROR is nil
the configuration is not mirrored.

If TRANSPOSE is non-nil then the window structure is transposed
along the diagonal from top left to bottom right (in analogy with
matrix transosition).

If called interactively MIRROR will is 'mirror-left-right by
default, but 'mirror-top-bottom if called with prefix.  TRANSPOSE
is t. This mean that the window configuration will be turned one
quarter clockwise (or counter clockwise with prefix)."
  (interactive (list
                (if current-prefix-arg
                    'mirror-left-right
                  'mirror-top-bottom)
                t))
  (require 'winsav)
  (let* ((wintree (winsav-get-window-tree))
         (tree (cadr wintree))
         (win-config (current-window-configuration)))
    ;;(winsav-log "old-wintree" wintree)
    (winsav-transform-1 tree mirror transpose)
    ;;(winsav-log "new-wintree" wintree)
    ;;
    ;; Fix-me: Stay in corresponding window. How?
    (delete-other-windows)
    (condition-case err
        (winsav-put-window-tree wintree (selected-window))
      (error
       (set-window-configuration win-config)
       (message "Can't rotate: %s" (error-message-string err))))
    ))


(provide 'winsize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winsize.el ends here
