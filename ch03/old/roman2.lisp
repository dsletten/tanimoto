;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               roman2.lisp
;;;
;;;   STARTED:            Sun Jul 28 22:28:21 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE: Roman numeral conversion (ordered) production system.
;;;            Tanimoto ch. 3
;;;
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

(defun roman2 ()
  (let ((x nil))
    (loop
     (cond ((null x) (format t "Enter number: ") (setf x (read)))
	   ((> x 39) (format t "Too big.~%") (setf x nil))
	   ((> x 9) (format t "X") (decf x 10))
	   ((= x 9) (format t "IX") (setf x 0))
	   ((> x 4) (format t "V") (decf x 5))
	   ((= x 4) (format t "IV") (setf x 0))
	   ((> x 0) (format t "I") (decf x))
	   ((zerop x) (format t "~%") (setf x nil)))) ))
