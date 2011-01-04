;;; queue.el --- queue data structure

;; Copyright (C) 1991-1995, 2008-2009 Free Software Foundation

;; Author: Inge Wallin <inge@lysator.liu.se>
;;         rewritten by Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: queue, extensions, lisp

;; This file is part of the GNU Emacs lisp library, Elib.

;; GNU Elib is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Elib is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.
;;

;;; Commentary:



;;; Code:

(eval-when-compile (require 'cl))


(defstruct (queue
            ;; A tagged list is the pre-defstruct representation.
            ;; (:type list)
	    :named
	    (:constructor nil)
	    (:constructor queue-create ())
	    (:copier nil))
  head tail)


(defun queue-enqueue (queue element)
  "Append an ELEMENT to the end of the QUEUE."
  (if (queue-head queue)
      (setcdr (queue-tail queue)
	      (setf (queue-tail queue) (cons element nil)))
    (setf (queue-head queue)
	  (setf (queue-tail queue) (cons element nil)))))

(defalias 'queue-append 'queue-enqueue)


(defun queue-prepend (queue element)
  "Prepend an ELEMENT to the front of the QUEUE."
  (if (queue-head queue)
      (push element (queue-head queue))
    (setf (queue-head queue)
	  (setf (queue-tail queue) (cons element nil)))))


(defun queue-dequeue (queue)
  "Remove the first element of QUEUE and return it.
Returns nil if the queue is empty."
  (unless (cdr (queue-head queue)) (setf (queue-tail queue) nil))
  (pop (queue-head queue)))


(defmacro queue-empty (queue)
  "Return t if QUEUE is empty, otherwise return nil."
  (null (queue-head queue)))


(defmacro queue-first (queue)
  "Return the first element of QUEUE or nil if it is empty,
without removing it from the QUEUE."
  (car (queue-head queue)))


(defun queue-nth (queue n)
  "Return the nth element of a queue, without removing it.
If the length of the queue is less than N, return nil. The first
element in the queue has index 0."
  (nth n (queue-head queue)))


(defun queue-last (queue)
  "Return the last element of QUEUE, without removing it.
Returns nil if the QUEUE is empty."
  (car (queue-tail queue)))


(defun queue-all (queue)
  "Return a list of all elements of QUEUE or nil if it is empty.
The oldest element in the queue is the first in the list."
  (queue-head queue))


(defun queue-copy (queue)
  "Return a copy of QUEUE.
The new queue contains the elements of QUEUE in the same
order. The elements themselves are *not* copied."
  (let ((q (queue-create))
	(list (queue-head queue)))
    (when (queue-head queue)
      (setf (queue-head q) (cons (car (queue-head queue)) nil)
	    (queue-tail q) (queue-head q))
      (while (setq list (cdr list))
	(setf (queue-tail q)
	      (setcdr (queue-tail q) (cons (car list) nil)))))
    q))


(defun queue-length (queue)
  "Return the number of elements in QUEUE."
  (length (queue-head queue)))


(defun queue-clear (queue)
  "Remove all elements from QUEUE."
  (setf (queue-head queue) nil
	(queue-tail queue) nil))


(provide 'queue)


;;; Local Variables:
;;; fill-column: 72
;;; End:

;;; queue.el ends here
