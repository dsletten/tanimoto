;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               roman1.lisp
;;;
;;;   STARTED:            Sun Jul 28 05:23:27 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE: Roman numeral conversion (unordered) production system.
;;;            Tanimoto ch. 3
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;

(defun roman1 ()
  (let ((x nil))
    (loop
     (cond ((null x) (format t "Enter number: ") (setf x (read))) ;NUMBERP?
	   ((and (not (null x)) (> x 39))
	    (format t "Too big.~%")
	    (setf x nil))
	   ((and (not (null x)) (<= 10 x 39))
	    (format t "X")
	    (decf x 10))
	   ((and (not (null x)) (= x 9))
	    (format t "IX")
	    (setf x 0))
	   ((and (not (null x)) (<= 5 x 8))
	    (format t "V")
	    (decf x 5))
	   ((and (not (null x)) (= x 4))
	    (format t "IV")
	    (setf x 0))
	   ((and (not (null x)) (<= 1 x 3))
	    (format t "I")
	    (decf x))
	   ((and (not (null x)) (zerop x))
	    (format t "~%")
	    (setf x nil)))) ))