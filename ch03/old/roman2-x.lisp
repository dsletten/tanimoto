;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               roman2-x.lisp
;;;
;;;   STARTED:            Sun Jul 28 22:32:59 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE: Variations on roman1.lisp using closures to embody rules.
;;;
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

(load "../tanimoto")

;;;
;;;    Order of rules is significant here.
;;;    
(let* ((x nil)
       (rules (defrules (((null x) (format t "Enter number: ") (setf x (read)))
			 ((> x 39) (format t "Too big.~%") (setf x nil))
			 ((> x 9) (format t "X") (decf x 10))
			 ((= x 9) (format t "IX") (setf x 0))
			 ((> x 4) (format t "V") (decf x 5))
			 ((= x 4) (format t "IV") (setf x 0))
			 ((> x 0) (format t "I") (decf x))
			 ((zerop x) (format t "~%") (setf x nil)))) ))
  
  (defun roman2 ()
    (loop
     (dolist (rule rules)
       (when (funcall (first rule))
	 (funcall (second rule))
	 (return)))) )

  (defun show-rules ()
    rules))
