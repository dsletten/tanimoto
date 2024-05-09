;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               trivial.lisp
;;;
;;;   STARTED:            Sun Jul 28 22:51:14 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE: Trivial reimplementation of routine functions as production
;;;   systems.
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

(defmacro defrules (rules)
  (let ((result ()))
    (dolist (rule rules)
      (push `(list #'(lambda () ,(first rule))
	           #'(lambda () ,@(rest rule))) result))
    (cons 'list (nreverse result))))

;;;
;;;    Factorial.
;;;
(let* ((n nil)
       (factorial nil)
       (rules (defrules (((or (null n) (not (numberp n)) (minusp n))
			  (format t "Enter non-negative integer: ")
			  (setf n (read)))
			 ((null factorial) (setf factorial 1))
			 ((and (not (null n)) (> n 0))
			  (setf factorial (* factorial n))
			  (decf n))
			 ((and (not (null n)) (= n 0))
			  (format t "~D~%" factorial)
			  (setf n nil factorial nil)))) ))

  (defun factorial ()
    (loop
     (dolist (rule rules)
       (when (funcall (first rule))
	 (funcall (second rule))
	 (return)))) ))

;;;
;;;    anyoddp (Touretzky 8.2)
;;;
(let* ((l nil)
       (flag nil)
       (rules (defrules (((and (null l) (null flag))
			  (format t "Enter a list: ")
			  (setf l (read)))
			 ((and (not (null l)) (not (numberp (car l))))
			  (format t "Not a list of numbers.~%")
			  (setf l nil flag nil))
			 ((and (not (null l)) (numberp (car l))
			       (not (oddp (car l))))
			  (setf l nil flag 'no))
			 ((and (not (null l)) (numberp (car l))
			       (oddp (car l)))
			  (setf l (cdr l) flag 'yes))
			 ((and (null l) (eq flag 'no))
			  (format t "Not all odd.~%")
			  (setf l nil flag nil))
			 ((and (null l) (eq flag 'yes))
			  (format t "All odd.~%")
			  (setf l nil flag nil)))) ))

  (defun anyoddp ()
    (loop
     (read)
     (dolist (rule rules)
       (when (funcall (first rule))
	 (format t "~S~%" (first rule))
	 (funcall (second rule))
	 (return)))) ))