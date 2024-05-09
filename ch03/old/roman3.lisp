;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               roman3.lisp
;;;
;;;   STARTED:            Mon Jul 29 23:53:47 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE: Roman numeral conversion production system using discrimination
;;;            net.
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

(defun roman3 ()
  (let ((x nil))
    (loop
     (cond ((null x) (format t "Enter number: ") (setf x (read)))
	   (t (cond ((> x 4)
		     (cond ((> x 9)
			    (cond ((> x 39)
				   (format t "Too big.~%") (setf x nil))
				  (t (format t "X") (decf x 10))))
			   (t
			    (cond ((< x 9) (format t "V") (decf x 5))
				  (t (format t "IX") (setf x 0)))) ))
		     (t
		      (cond ((< x 4)
			     (cond ((> x 0) (format t "I") (decf x))
				   (t (format t "~%") (setf x nil))))
			    (t
			     (format t "IV") (setf x 0)))) )))) ))

