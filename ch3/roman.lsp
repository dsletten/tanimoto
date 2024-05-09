;#!/usr/local/bin/clisp

;;
;   NAME:               roman.lsp
;
;   STARTED:            010402
;   MODIFICATIONS:
;
;   PURPOSE:
;
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES:
;
;;

;;;
;;;    Roman numeral conversion using production system
;;;
(defun roman1 ()
  (let ((x nil))
    (loop
     (cond ((null x) 
	    (format t "Enter number: ")
	    (setf x (read)))
	   ((and (not (null x)) (> x 39))
	    (format t "Too big~%")
	    (setf x nil))
	   ((and (not (null x)) (<= x 39) (>= x 10))
	    (format t "X")
	    (setf x (- x 10)))
	   ((and (not (null x)) (= x 9))
	    (format t "IX")
	    (setf x 0))
	   ((and (not (null x)) (<= x 8) (>= x 5))
	    (format t "V")
	    (setf x (- x 5)))
	   ((and (not (null x)) (= x 4))
	    (format t "IV")
	    (setf x 0))
	   ((and (not (null x)) (<= x 3) (>= x 1))
	    (format t "I")
	    (setf x (1- x)))
	   ((and (not (null x)) (zerop x))
	    (format t "~%")
	    (setf x nil)))) ) )

;;;
;;;    Production system using ordered production rules.
;;;
(defun roman2 ()
  (let ((x nil))
    (loop
     (cond ((null x) (format t "Enter number: ") (setf x (read)))
	   ((> x 39) (format t "Too big~%") (setf x nil))
	   ((> x 9) (format t "X") (setf x (- x 10)))
	   ((= x 9) (format t "IX") (setf x 0))
	   ((> x 4) (format t "V") (setf x (- x 5)))
	   ((= x 4) (format t "IV") (setf x 0))
	   ((> x 0) (format t "I") (setf x (1- x)))
	   ((zerop x) (format t "~%") (setf x nil)))) ) )

;;;
;;;    Production system using discrimination net.
;;;
(defun roman3 ()
  (let ((x nil))
    (loop
     (cond ((null x) (format t "Enter number: ") (setf x (read)))
	   (t (cond ((> x 39) (format t "Too big~%") (setf x nil))
		    (t (cond ((> x 4)
			      (cond ((> x 9) (format t "X") (setf x (- x 10)))
				    (t (cond ((= x 9) (format t "IX") (setf x 0))
					     (t (format t "V") (setf x (- x 5)))) )))
			     (t (cond ((= x 4) (format t "IV") (setf x 0))
				      (t (cond ((> x 0) (format t "I") (setf x (1- x)))
					       (t (format t "~%") (setf x nil)))) )))) )))) ) )

;;;
;;;    Production system using ordered production rules.
;;;    Extended to handle numbers up to 399.
;;;
(defun roman2-b ()
  (let ((x nil))
    (loop
     (cond ((null x) (format t "Enter number: ") (setf x (read)))
	   ((> x 399) (format t "Too big~%") (setf x nil))
	   ((> x 99) (format t "C") (setf x (- x 100)))
	   ((> x 89) (format t "XC") (setf x (- x 90)))
	   ((> x 49) (format t "L") (setf x (- x 50)))
	   ((> x 39) (format t "XL") (setf x (- x 40)))
	   ((> x 9) (format t "X") (setf x (- x 10)))
	   ((= x 9) (format t "IX") (setf x 0))
	   ((> x 4) (format t "V") (setf x (- x 5)))
	   ((= x 4) (format t "IV") (setf x 0))
	   ((> x 0) (format t "I") (setf x (1- x)))
	   ((zerop x) (format t "~%") (setf x nil)))) ) )

