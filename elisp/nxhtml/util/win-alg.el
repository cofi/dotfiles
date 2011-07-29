;;; win-alg.el --- Window size computation
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-08-12 Wed
;; Version: 0.21
;; Last-Updated: 2010-06-14 Mon
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window and tree creation, basic setters/getters etc

(defun wa-make-window (name parent size wumin wumax real-window)
  "Make a new child and add it to its parent.
Return child."
  (let ((this
         (list (list 'id name parent (wa-name parent)) ;; Easier debugging + parent
               (list 'wchild nil)                      ;; Child windows list
               (list 'wusr-size size wumin wumax)      ;; Current size and restrictions: wumin, wumax
               (list 'wreq-size nil nil nil)           ;; Slot for computing requirements: wrmin wrmax wfixed
               (list 'wset-size nil nil)               ;; Slot for new size: result-flag, wset
               (list 'real real-window))))
    (when parent
      (wa-set-children parent (cons this (wa-children parent))))
    this))

(defun wa-node-p (arg)
  (and (listp arg)
       (eq 'id (car (nth 0 arg)))
       ;; Fix-me
       ))

(defun wa-get-root (node)
  (assert (wa-node-p node) t)
  (while (wa-parent node)
    (setq node (wa-parent node)))
  node)

;; Fix-me: Maybe make defmacro to make those getters setters... -
;; including checks...

;; You can probably make this much easier and fancier with cl.el, but
;; I find it awful to debug that so I do not want to use it.

;;; Getters
(defun wa-name      (window) (nth 1 (nth 0 window))) ;; 'id
(defun wa-parent    (window) (nth 2 (nth 0 window))) ;; 'id
;; Current
(defun wa-children  (window) (nth 1 (nth 1 window))) ;; 'wchild
(defun wa-wucur     (window) (nth 1 (nth 2 window))) ;; 'wusr-size
(defun wa-wumin     (window) (nth 2 (nth 2 window))) ;; 'wusr-size
(defun wa-wumax     (window) (nth 3 (nth 2 window))) ;; 'wusr-size
;; Computation
(defun wa-wrmin     (window) (nth 1 (nth 3 window))) ;; 'wreq-size
(defun wa-wrmax     (window) (nth 2 (nth 3 window))) ;; 'wreq-size
(defun wa-wfixed    (window) (nth 3 (nth 3 window))) ;; 'wreq-size
;; Result
(defun wa-wres-flag (window) (nth 1 (nth 4 window))) ;; 'wset-size
(defun wa-wset      (window) (nth 2 (nth 4 window))) ;; 'wset-size
;; Real window
(defun wa-real      (window) (nth 1 (nth 5 window))) ;; 'real

;;; Setters
(defun wa-set-name      (window name)     (setcar (nthcdr 1 (nth 0 window)) name))  ;; 'id
(defun wa-set-children  (window children) (setcar (nthcdr 1 (nth 1 window)) children)) ;; 'wchild
;; Current
(defun wa-set-wucur     (window wucur)    (setcar (nthcdr 1 (nth 2 window)) wucur)) ;; 'wusr-size
(defun wa-set-wumin     (window wumin)    (setcar (nthcdr 2 (nth 2 window)) wumin)) ;; 'wusr-size
(defun wa-set-wumax     (window wumax)    (setcar (nthcdr 3 (nth 2 window)) wumax)) ;; 'wusr-size
;; Computation
(defun wa-set-wrmin     (window wrmin)    (setcar (nthcdr 1 (nth 3 window)) wrmin)) ;; 'wreq-size
(defun wa-set-wrmax     (window wrmax)    (setcar (nthcdr 2 (nth 3 window)) wrmax)) ;; 'wreq-size
(defun wa-set-wfixed    (window wrfix)    (setcar (nthcdr 3 (nth 3 window)) wrfix)) ;; 'wreq-size
;; Result
(defun wa-set-wres-flag (window flag)     (setcar (nthcdr 1 (nth 4 window)) flag))  ;; 'wset-size
(defun wa-set-wset      (window size)     (setcar (nthcdr 2 (nth 4 window)) size))  ;; 'wset-size
;; Real window
(defun wa-set-real      (window real)     (setcar (nthcdr 1 (nth 5 window)) real))  ;; 'real

;; Fix-me: remove this...
(defvar wa-failed nil)

(defun wa-set-child-windows (parent &rest sizes)
  "For testing."
  (unless wa-failed (assert (<= 1 (length sizes)) t))
  (let ((num 0))
    (mapc (lambda (size)
            (setq num (1+ num))
            (wa-make-window (format "%s-%d" (wa-name parent) num)
                            parent
                            nil
                            (nth 0 size)
                            (nth 1 size)
                            nil))
          sizes))
    parent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Errors

(defun wa-error (format-string &rest args)
  (setq wa-failed t)
  (apply 'message (propertize format-string 'face 'secondary-selection)
         args)
  (throw 'top-level nil))

(defun wa-win-error (win format-string &rest args)
  (wa-set-wres-flag win (concat "FAILED: " (apply 'format format-string args)))
  ;;(apply 'wa-error format-string args)
  (throw 'win-error (apply 'format format-string args)))

(defun wa-did-not-fit (win)
  ;; Fix-me: throw both value diff and message. Any ambiguity in the
  ;; value diff?
  (catch 'win-error
    (let* ((wumin (wa-wumin win))
           (wumax (wa-wumax win))
           (wrmin (wa-wrmin win))
           (wrmax (wa-wrmax win))
           (wset  (wa-wset win))
           (wfix  (wa-wfixed win))
           (sf (if wfix "fix" "set")))
      (wa-set-wres-flag win 'FAILED)
      ;; Top window
      (when (and wset wrmin)
        (unless (<= wrmin wset)
          (wa-win-error win "Window %s %s size too small=%d, min=%d" (wa-name win) sf wset wrmin)))
      (when (and wset wrmax)
        (unless (>= wrmax wset)
          (wa-win-error win "Window %s %s size too large=%d, max=%s" (wa-name win) sf wset wrmax)))
      ;; All
      (when (and wumax wrmin)
        (unless (<= wrmin wumax)
          (wa-win-error win "Window %s too small, min=%d, but can be max=%d" (wa-name win) wrmin wumax)))
      (when (and wrmax wumin)
        (unless (>= wrmax wumin)
          (wa-win-error win "Window %s's children too small, max=%d, but can be min=%d" (wa-name win) wrmax wumin)))
      (wa-set-wres-flag win 'OK))
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Computation of sizes

(defun wa-clear-computed (win)
  (wa-set-wrmin win nil)
  (wa-set-wrmax win nil)
  (wa-set-wset  win nil)
  (dolist (c (wa-children win))
    (wa-clear-computed c)))

(defun wa-init-fail-flag (win)
  (wa-set-wres-flag win 'INIT)
  (dolist (c (wa-children win))
    (wa-init-fail-flag c)))

(defun wa-compute-required-to-message (win)
  (catch 'wa-fit-error
    (wa-compute-required win)))

;; Fi-me: Remember that the children are actually two steps down in
;; the tree, since every second tree level is split horizontally and
;; those in between vertically. This must be added to
;; `wa-compute-requied' and `wa-compute-resulting'.
(defun wa-compute-required (win)
  "Walk up from window WIN collecting needed sizes.
Throw string message to 'wa-fit-error if does not fit."
  (let ((children (wa-children win))
        (wumin (wa-wumin win))
        (wumax (wa-wumax win))
        (cmin 0)
        (cmax -1)
        (can-fit t))
    (if (not children)
        (setq cmax nil)
      ;; Clear childes set sizes.
      (dolist (c children)
        (wa-set-wset c nil))
      (dolist (c children)
          (let* ((res (wa-compute-required c))
                 (res-min (nth 0 res))
                 (res-max (nth 1 res)))
            ;; Fix-me: Use emacs window min.
            (unless res-min (setq res-min 1)) ;; Fix-me: emacs window min
            ;; If fixed size just set MAX and MIN to desired size.
            (when (wa-wfixed c)
              (setq res-min (wa-wfixed c))
              (setq res-max (wa-wfixed c)))
            ;; Just sum the MIN.
            (setq cmin (+ cmin res-min))
            ;; Check MAX
            (if (and res-max cmax)
                ;; ... ok, let us sum MAX to see how big we can be ...
                (if (> cmax 0)
                    (setq cmax (+ cmax res-max))
                  ;; First time:
                  (setq cmax res-max))
              ;; Hurray, at least one child can grow!
              (setq cmax nil))))
      ;; Sanity. Fix-me: use emacs window min.
      (unless wa-failed (assert (<= (* 1 (length children)) cmin) t))
      (unless wa-failed (assert (or (not cmax) (<= (* 1 (length children)) cmax)) t)))
    (when wumin (setq cmin (max wumin (or cmin wumin))))
    (when wumax (setq cmax (min wumax (or cmax wumax))))
    (wa-set-wrmin win cmin)
    (wa-set-wrmax win cmax)
    (let ((did-not-fit (wa-did-not-fit win)))
      (if did-not-fit
          (throw 'wa-fit-error did-not-fit)
        (list (wa-wrmin win)
              (wa-wrmax win))))))

(defun wa-compute-resulting (win strategy)
  "Walk down compute resulting sizes and apply them."
  ;; NOTE: This is the part that can tie into the C functions. This
  ;; computes the sizes to apply level by level when going down.
  ;;
  ;; To apply it to the C level I suggest implementing a function in C
  ;; that accept a list of sizes, one size per window on that
  ;; level. Walk the C structures in parallel with this when applying
  ;; the sizes. (I do not think it is necessary to have this code in
  ;; C.)
  (when (wa-children win)
    (let ((cmin   (wa-wrmin  win))
          (cmax   (wa-wrmax  win))
          (width  (wa-wset win))
          (children (wa-children win)))
      (setq strategy (or strategy 'eq-sizes))
      (case strategy
        ('eq-sizes
         (let ((rest-width width)
               (goal (/ width (length children)))
               (rest-children (copy-sequence children)))
           ;; Clear childes
           (dolist (c children) (wa-set-wset c nil))
           ;; Check child min requirements
           (dolist (c (copy-sequence rest-children))
             (let ((wrmin (wa-wrmin c)))
               (when (and wrmin (<= goal wrmin))
                 (wa-set-wset c (wa-wrmin c))
                 (setq rest-children (delete c rest-children))
                 (setq rest-width (- rest-width (wa-wrmin c))))))
           (setq goal (/ rest-width (length children)))
           ;; Check child max requirements
           (dolist (c (copy-sequence rest-children))
             (let ((wrmax (wa-wrmax c)))
               (when (and wrmax (>= goal wrmax))
                 (wa-set-wset c (wa-wrmax c))
                 (setq rest-children (delete c rest-children))
                 (setq rest-width (- rest-width (wa-wrmax c))))))
           (setq goal (/ rest-width (length children)))
           ;; Distribute the rest, taking care of rounding
           (wa-set-wset (car rest-children)
                        (- rest-width (* goal (1- (length rest-children)))))
           (dolist (c (cdr rest-children))
             (wa-set-wset c goal))))
        (t (wa-error "Unknown strategy: %s" strategy)))
      ;; Check
      (let ((w 0))
        (dolist (c children)
          (let ((wset (wa-wset c)))
            (unless wa-failed (assert (<= 0 wset) t))
            (setq w (+ w wset))))
        (unless (= w (wa-wset win))
          (wa-error "Bad set sizes child sum w=%d, win width=%d" w (wa-wset win))))
      ;; Call the suggested C level function here for example.
      ;; .......
      ;; Walk down
      (dolist (c children)
        (wa-compute-resulting c strategy))))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example of new implementation resizing functions

(eval-when-compile (require 'cl))

(defun wa-siblings (window which)
  (let* ((all (wa-children (wa-parent window)) )
         (me-tail (cdr (member window all)))
         (me-rtail (cdr (member window (reverse all)))))
    (case which
     ((all nil) (list me-rtail me-tail))
     (lower me-rtail)
     (upper me-tail))))

(defun wa-resize-window (window size-diff edge)
  "Replacement code for `enlarge-window' and friends.
WINDOW is the Emacs window to resize.
SIZE-DIFF is the size difference.
EDGE is 'after or 'before which means left respectively right
edge for width etc.

First try to enlarge windows below WINDOW since that is probably
most visually appealing.  If that fails, then also try windows
above.

If that also fails, then ... - just fail for now.  This can be
expanded so that this strategy is applied upwards in the tree,
requesting parent window to be enlarged by the amount we need to
enlarge successfully. If that also fails (applying that all the
way up to the root window) then we must fail.

\(I do not have time to implement that right now, but I hope the
idea is clear. I think there are no problems with this since we
are just going upwards in the tree to the root.)

However there will still be a choice: failing with no enlargement
at all or failing with partial enlargement.

Return value?"
  ;; Fix-me: vertical
  (let* ((parent (wa-parent window))
         (new-size (+ size-diff (wa-wset window)))
         ret
         (siblings (wa-siblings window)) ;; includes WINDOW
         (sib-above (nth 0 siblings))
         (sib-below (nth 1 siblings))
         )
    (if (catch 'done
          ;; Set fixed computational size.
          (dolist (w sib-above) (wa-set-wfixed w (wa-wset w)))
          (dolist (w sib-below) (wa-set-wfixed w (wa-wset w)))
          (cond
           ((eq edge 'after) ;; After.
            (dolist (w sib-below)
              (wa-set-wfixed w nil)
              (unless (wa-compute-required-to-message parent)
                (throw 'done t))))
           ((eq edge 'before) ;; Before.
            (dolist (w sib-above)
              (wa-set-wfixed w nil)
              (unless (wa-compute-required-to-message parent)
                (throw 'done t))))
           (t (error "Bad arg, edge=%s" edge))))
        (progn
          (wa-compute-resulting win 'eq-sizes)
          (setq ret t))
      ;; Fix-me: Partial resizing etc?
      ;; Go up in tree if no fit. Fix-me: is this really needed? Is it good at all?
      (setq ret (wa-enlarge-window parent size)))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Accessing the real windows

;; (setq x (wa-get-frame-windows nil t))
;; (setq y (wa-get-window-node (selected-window) x))
(defun wa-get-frame-windows (frame horflag)
  "Get frame FRAME windows as a WA window tree.
If HORFLAG get the tree for horizontal dividing, otherwise for
vertical.

WA trees are those create with `wa-make-window',
`wa-set-children' and friends.

Note 1: One WA can only be used for either horizontal or vertical
dividing.

Note 2: The routines for resizing Emacs windows from elisp works
for either horizontal or vertical dividing.  So WA and those
routines fits together."
  (setq frame (or frame (selected-frame)))
  (let* ((window-tree (window-tree frame))
         (root (frame-root-window frame))
         (root-size (if horflag
                        (window-height root)
                      (window-width root)))
         (wa-tree (wa-make-window "Root" nil root-size nil nil root))
         )
    (wa-get-tree-windows wa-tree (car window-tree) horflag)
    ))


(defun wa-get-tree-windows (parent win-subtree horflag)
  (let* ((hor (nth 0 win-subtree))
         (size-rec (nth 1 win-subtree))
         (nn 0))
    (mapc (lambda (child)
            (let ((wt (if (windowp child)
                          child
                        (wa-get-tree-windows parent child horflag)))
                  (name (if (windowp child)
                            (format "%S" child)
                          (format "sub%d" (setq nn (1+ nn)))))
                  (size (when (windowp child)
                          ;; Compare `resize-window-apply' and the arg
                          ;; HORIZONTAL there.
                          (if horflag (window-width child)
                            (window-height child))))
                  )
              (wa-make-window name parent size nil nil wt)))
          (nthcdr 2 win-subtree)))
  parent)

(defun wa-get-window-node (window wa-tree)
  (assert (wa-node-p wa-tree) t)
  (catch 'found
    (wa-get-window-node-1 window wa-tree)
    ))

(defun wa-get-window-node-1 (window wa-tree)
  (assert (wa-node-p wa-tree) t)
  (when (eq window (wa-real wa-tree))
    (throw 'found wa-tree))
  (dolist (node (wa-children wa-tree))
    (wa-get-window-node-1 window node)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rules

;; (wa-set-all-fixed x)
(defun wa-set-all-fixed (tree)
  (assert (wa-node-p tree) t)
  (wa-set-wfixed tree (wa-wucur tree)))

;; (wa-set-node-goal y 100)
(defun wa-set-node-goal (node goal)
  (assert (wa-node-p node) t)
  (wa-set-wfixed node nil)
  (let* ((curr (wa-wucur node))
         (grow (< curr goal)))
    (if grow
        (progn
          (wa-set-wumin node curr)
          (wa-set-wumax node goal))
      (wa-set-wumin node goal)
      (wa-set-wumax node curr))))


;; (wa-allow-lower-siblings y)
(defun wa-allow-lower-siblings (node)
  (let ((siblings (wa-siblings node 'lower)))
    (dolist (sib siblings)
      (wa-set-wfixed sib nil)))
  (catch 'wa-fit-error
    (wa-compute-required node)
    nil))

(defun wa-allow-upper-siblings (node)
  (let ((siblings (wa-siblings node 'upper)))
    (dolist (sib siblings)
      (wa-set-wfixed sib nil)))
  (catch 'wa-fit-error
    (wa-compute-required node)
    nil))

;; (setq r '(wa-allow-lower-siblings wa-allow-upper-siblings))
;; (wa-run-rules-and-set x 100 r nil nil)
(defun wa-run-rules-and-set (node goal rules strategy tree)
  (let ((root (wa-get-root node)))
    (setq tree (or tree root))
    (wa-set-all-fixed tree)
    (let ((err (run-hook-with-args-until-failure 'rules node)))
      (if err
          (message "%s" err)
        (wa-compute-resulting root strategy)
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing part

(defvar wa-root-window nil)

(defun wa-add-test-children ()
  (wa-set-child-windows wa-root-window
                         '(nil 12)
                         '(14 nil)
                         '(nil nil)
                         '(3 nil)
                         )
  (wa-set-child-windows (car (wa-children wa-root-window))
                        '(nil nil)
                        '(8 15))
  )

;; (wa-children wa-root-window)
;; (wa-wset wa-root-window)
;; (wa-wumin wa-root-window)
;; (wa-wumax wa-root-window)
;; (wa-clear-computed wa-root-window)

(defun wa-temp-test ()
  ;; Setup
  (setq wa-root-window (wa-make-window "Root" nil 80 nil nil  nil))
  (setq wa-root-window (wa-make-window "Root" nil 80 nil 8  nil))
  (setq wa-root-window (wa-make-window "Root" nil 80 nil 6  nil))
  (setq wa-root-window (wa-make-window "Root" nil 43 15 nil nil))
  (setq wa-root-window (wa-make-window "Root" nil 18 15 nil nil))
  (setq wa-root-window (wa-make-window "Root" nil 15 15 nil nil))
  (setq wa-root-window (wa-make-window "Root" nil 80 5 nil nil))

  (wa-add-test-children)
  (wa-init-fail-flag     wa-root-window)
  (setq wa-failed nil)

  ;; Show state now in case we want to stop on errors
  (describe-variable    'wa-root-window)

  ;; Compute required, may fail.
  (let ((msg (catch 'wa-fit-error
               (wa-set-wset wa-root-window (wa-wucur wa-root-window))
               (wa-compute-required wa-root-window)
               ;; Now it should not fail
               (wa-compute-resulting  wa-root-window 'eq-sizes))))
    (when msg
      (message "%s" (propertize msg 'face 'secondary-selection))))

  ;; Show final state
  (describe-variable    'wa-root-window)
  (with-current-buffer (help-buffer)
    (hi-lock-face-buffer "\"FAILED.*\"" 'hi-red-b)
    (hi-lock-face-buffer "OK" 'hi-green)
    (hi-lock-face-buffer "INIT" 'hi-blue)))

;;(wa-temp-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; win-alg.el ends here
