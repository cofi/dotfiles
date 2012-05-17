;;; eieio-persist.el --- Tests for eieio-persistent class
;;
;; Copyright (C) 2011, 2012 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; The eieio-persistent base-class provides a vital service, that
;; could be used to accidentally load in malicious code.  As such,
;; something as simple as calling eval on the generated code can't be
;; used.  These tests exercises various flavors of data that might be
;; in a persistent object, and tries to save/load them.

;;; Code:
(defun persist-test-save-and-compare (original)
  "Compare the object ORIGINAL against the one read fromdisk."

  (eieio-persistent-save original)

  (let* ((file (oref original :file))
	 (class (object-class original))
	 (fromdisk (eieio-persistent-read file class))
	 (cv (class-v class))
	 (slot-names  (aref cv class-public-a))
	 (slot-deflt  (aref cv class-public-d))
	 )
    (unless (object-of-class-p fromdisk class)
      (error "Persistent class %S != original class %S"
	     (object-class fromdisk)
	     class))

    (while slot-names
      (let* ((oneslot (car slot-names))
	     (origvalue (eieio-oref original oneslot))
	     (fromdiskvalue (eieio-oref fromdisk oneslot))
	     (initarg-p (eieio-attribute-to-initarg class oneslot))
	     )

	(if initarg-p
	    (unless (equal origvalue fromdiskvalue)
	      (error "Slot %S Original Val %S != Persistent Val %S"
		     oneslot origvalue fromdiskvalue))
	  ;; Else !initarg-p
	  (unless (equal (car slot-deflt) fromdiskvalue)
	    (error "Slot %S Persistent Val %S != Default Value %S"
		   oneslot fromdiskvalue (car slot-deflt))))
	
	(setq slot-names (cdr slot-names)
	      slot-deflt (cdr slot-deflt))
	))))

;;; Simple Case
;;
;; Simplest case is a mix of slots with and without initargs.

(defclass persist-simple (eieio-persistent)
  ((slot1 :initarg :slot1
	  :type symbol
	  :initform moose)
   (slot2 :initarg :slot2
	  :initform "foo")
   (slot3 :initform 2))
  "A Persistent object with two initializable slots, and one not.")

(defvar persist-simple-1 nil)
(setq persist-simple-1
      (persist-simple "simple 1" :slot1 'goose :slot2 "testing"
		      :file (concat default-directory "test-ps1.pt")))

;; When the slot w/out an initarg has not been changed
(persist-test-save-and-compare persist-simple-1)

;; When the slot w/out an initarg HAS been changed
(oset persist-simple-1 slot3 3)
(persist-test-save-and-compare persist-simple-1)

;;; Slot Writers
;;
;; Replica of the test in eieio-tests.el - 

(defclass persist-:printer (eieio-persistent)
  ((slot1 :initarg :slot1
	  :initform 'moose
	  :printer PO-slot1-printer)
   (slot2 :initarg :slot2
	  :initform "foo"))
  "A Persistent object with two initializable slots.")

(defun PO-slot1-printer (slotvalue)
  "Print the slot value SLOTVALUE to stdout.
Assume SLOTVALUE is a symbol of some sort."
  (princ "'")
  (princ (symbol-name slotvalue))
  (princ " ;; RAN PRINTER")
  nil)

(defvar persist-:printer-1 nil)
(setq persist-:printer-1 (persist-:printer "persist" :slot1 'goose :slot2 "testing"
			     :file (concat default-directory "test-ps2.pt")))

(persist-test-save-and-compare persist-:printer-1)

(let* ((find-file-hooks nil)
       (tbuff (find-file-noselect "test-ps2.pt"))
       )
  (condition-case nil
      (unwind-protect
	  (save-excursion
	    (set-buffer tbuff)
	    (goto-char (point-min))
	    (re-search-forward "RAN PRINTER"))
	(kill-buffer tbuff))
    (error "persist-:printer-1's Slot1 printer function didn't work.")))

;;; Slot with Object
;;
;; A slot that contains another object that isn't persistent
(defclass persist-not-persistent ()
  ((slot1 :initarg :slot1
	  :initform 1)
   (slot2 :initform 2))
  "Class for testing persistent saving of an object that isn't
persistent.  This class is instead used as a slot value in a
persistent class.")

(defclass persistent-with-objs-slot (eieio-persistent)
  ((pnp :initarg :pnp
	:type (or null persist-not-persistent)
	:initform nil))
  "Class for testing the saving of slots with objects in them.")

(defvar persist-wos nil)
(setq persist-wos (persistent-with-objs-slot 
		   "persist wos 1"
		   :pnp (persist-not-persistent "pnp 1" :slot1 3)
		   :file (concat default-directory "test-ps3.pt")))
					     
(persist-test-save-and-compare persist-wos)

;;; Slot with Object child of :type
;;
;; A slot that contains another object that isn't persistent
(defclass persist-not-persistent-subclass (persist-not-persistent)
  ((slot3 :initarg :slot1
	  :initform 1)
   (slot4 :initform 2))
  "Class for testing persistent saving of an object subclass that isn't
persistent.  This class is instead used as a slot value in a
persistent class.")

(defclass persistent-with-objs-slot-subs (eieio-persistent)
  ((pnp :initarg :pnp
	:type (or null persist-not-persistent-child)
	:initform nil))
  "Class for testing the saving of slots with objects in them.")

(defvar persist-woss nil)
(setq persist-woss (persistent-with-objs-slot-subs 
		    "persist woss 1"
		    :pnp (persist-not-persistent-subclass "pnps 1" :slot1 3)
		    :file (concat default-directory "test-ps4.pt")))
					     
(persist-test-save-and-compare persist-woss)

;;; Slot with a list of Objects
;;
;; A slot that contains another object that isn't persistent
(defclass persistent-with-objs-list-slot (eieio-persistent)
  ((pnp :initarg :pnp
	:type persist-not-persistent-list
	:initform nil))
  "Class for testing the saving of slots with objects in them.")

(defvar persist-wols nil)
(setq persist-wols (persistent-with-objs-list-slot 
		   "persist wols 1"
		   :pnp (list (persist-not-persistent "pnp 1" :slot1 3)
			      (persist-not-persistent "pnp 2" :slot1 4)
			      (persist-not-persistent "pnp 3" :slot1 5))
		   :file (concat default-directory "test-ps5.pt")))
					     
(persist-test-save-and-compare persist-wols)



(provide 'eieio-persist)

;;; eieio-persist.el ends here
