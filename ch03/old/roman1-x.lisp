;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               roman1-x.lisp
;;;
;;;   STARTED:            Sun Jul 28 18:35:54 2002
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

(let* ((x nil)
       (rules (list (list #'(lambda () (null x))
			  #'(lambda ()
			      (format t "Enter number: ")
			      (setf x (read))))
		    (list #'(lambda () (and (not (null x)) (> x 39)))
			  #'(lambda () (format t "Too big.~%") (setf x nil)))
		    (list #'(lambda () (and (not (null x)) (<= 10 x 39)))
			  #'(lambda () (format t "X") (decf x 10)))
		    (list #'(lambda () (and (not (null x)) (= x 9)))
			  #'(lambda () (format t "IX") (setf x 0)))
		    (list #'(lambda () (and (not (null x)) (<= 5 x 8)))
			  #'(lambda () (format t "V") (decf x 5)))
		    (list #'(lambda () (and (not (null x)) (= x 4)))
			  #'(lambda () (format t "IV") (setf x 0)))
		    (list #'(lambda () (and (not (null x)) (<= 1 x 3)))
			  #'(lambda () (format t "I") (decf x)))
		    (list #'(lambda () (and (not (null x)) (zerop x)))
			  #'(lambda () (format t "~%") (setf x nil)))) ))

  (defun roman ()
    (loop
     (dolist (rule rules)
       (when (funcall (first rule))
	 (funcall (second rule))
	 (return)))) ))

;;;
;;;    This doesn't work with local X. EVAL doesn't see it.
;;;    
(defvar *x* nil)
(let ((rules '(((null *x*) (format t "Enter number: ") (setf *x* (read)))
	       ((and (not (null *x*)) (> *x* 39))
		(format t "Too big.~%")
		(setf *x* nil))
	       ((and (not (null *x*)) (<= 10 *x* 39))
		(format t "X")
		(decf *x* 10))
	       ((and (not (null *x*)) (= *x* 9))
		(format t "IX")
		(setf *x* 0))
	       ((and (not (null *x*)) (<= 5 *x* 8))
		(format t "V")
		(decf *x* 5))
	       ((and (not (null *x*)) (= *x* 4))
		(format t "IV")
		(setf *x* 0))
	       ((and (not (null *x*)) (<= 1 *x* 3))
		(format t "I")
		(decf *x*))
	       ((and (not (null *x*)) (zerop *x*))
		(format t "~%")
		(setf *x* nil)))) )

  (defun roman1 ()
    (loop
     (dolist (rule rules)
       (when (eval (first rule))
	 (dolist (action (rest rule))
	   (eval action))
	 (return)))) ))

(let* ((x nil)
       (rules (defrules (((null x)
			  (format t "Enter number: ") (setf x (read)))
			 ((and (not (null x)) (> x 39))
			  (format t "Too big.~%") (setf x nil))
			 ((and (not (null x)) (<= 10 x 39))
			  (format t "X") (decf x 10))
			 ((and (not (null x)) (= x 9))
			  (format t "IX") (setf x 0))
			 ((and (not (null x)) (<= 5 x 8))
			  (format t "V") (decf x 5))
			 ((and (not (null x)) (= x 4))
			  (format t "IV") (setf x 0))
			 ((and (not (null x)) (<= 1 x 3))
			  (format t "I") (decf x))
			 ((and (not (null x)) (zerop x))
			  (format t "~%") (setf x nil)))) ))

  (defun roman2 ()
    (loop
     (dolist (rule rules)
       (when (funcall (first rule))
	 (funcall (second rule))
	 (return)))) )

  (defun show-rules ()
    rules))
