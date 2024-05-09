;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               match.lisp
;;;
;;;   STARTED:            Tue Aug  6 03:04:10 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
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

(defun match1 (p s)
  (equal p s))

(defun match2 (p s)
  (cond ((atom p) (atom s))
	((atom s) nil)
	((match2 (car p) (car s)) (match2 (cdr p) (cdr s)))
	(t nil)))

(defun match3 (p s)
  (cond ((null p) (null s))
	((or (atom p) (atom s)) nil)
	((equal (car p) (car s)) (match3 (cdr p) (cdr s)))
	((eq (car p) '?) (match3 (cdr p) (cdr s)))
	(t nil)))

;;;
;;;    The idea of scattering matched values in an untold number of global
;;;    variables is not particularly appealing here.
;;;    
(defun match4 (p s)
  (cond ((null p) (null s))
	((or (atom p) (atom s)) nil)
	((equal (car p) (car s)) (match4 (cdr p) (cdr s)))
	((and (listp (car p))
	      (= (length (car p)) 2)
	      (eq (caar p) '?)
	      (match4 (cdr p) (cdr s)))
	 (set (cadar p) (car s))
	 t)
	(t nil)))

(let ((matches (make-hash-table :test #'eq)))
  (defun match4a (p s)
    (cond ((null p) (null s))
	  ((or (atom p) (atom s)) nil)
	  ((equal (car p) (car s)) (match4a (cdr p) (cdr s)))
	  ((and (listp (car p))
		(= (length (car p)) 2)
		(eq (caar p) '?)
		(match4a (cdr p) (cdr s)))
	   (setf (gethash (cadar p) matches) (car s))
	   t)
	  (t nil)))

  (defun show-matches ()
    (let ((results ()))
      (maphash #'(lambda (key val)
		   (push (list key val) results))
	       matches)
      results)))

(defun match4b (p s)
  (labels ((match-aux (p s)
	     (cond ((null p) (null s))
		   ((or (atom p) (atom s)) nil)
		   ((equal (car p) (car s)) (match-aux (cdr p) (cdr s)))
		   ((and (listp (car p))
			 (= (length (car p)) 2)
			 (eq (caar p) '?)
			 (match-aux (cdr p) (cdr s)))
		    (setf (get 'match4b (cadar p)) (car s))
		    t)
		   (t nil))))
    (setf (symbol-plist 'match4b) nil)
    (match-aux p s)))

(defun get-match (symbol)
  (get 'match4b symbol))

(defun match5 (p s)
  (cond ((null p) (null s))
	((or (atom p) (atom s)) nil)
	((equal (car p) (car s)) (match5 (cdr p) (cdr s)))
	((and (listp (car p))
	      (= (length (car p)) 2)
	      (eq (caar p) '?)
	      (match5 (cdr p) (cdr s)))
	 (set (cadar p) (car s))
	 t)
	((and (listp (car p))
	      (= (length (car p)) 2)
	      (not (eq (caar p) '?))
	      (apply (caar p) (list (car s)))
	      (match5 (cdr p) (cdr s)))
	 (set (cadar p) (car s))
	 t)
	(t nil)))

(defun match5a (p s)
  (cond ((null p) (null s))
	((or (atom p) (atom s)) nil)
	((equal (car p) (car s)) (match5a (cdr p) (cdr s)))
	((and (listp (car p))
	      (= (length (car p)) 2)
	      (or (eq (caar p) '?)
		  (funcall (caar p) (car s)))
	      (match5a (cdr p) (cdr s)))
	 (set (cadar p) (car s))
	 t)
	(t nil)))

;;;
;;;    Match type is either:
;;;        ? - single wildcard
;;;        * - sequence wildcard (0 or more elements)
;;;        or a predicate.
;;;        
(defun match (p s)
  (cond ((null p) (null s))
	((atom p) nil)
	((atom (car p))
	 (and s (equal (car p) (car s)) (match (cdr p) (cdr s))))
	((eq (match-type (car p)) '*)
	 (cond ((and s (match (cdr p) (cdr s)))
		(setf (match-variable (car p)) (list (car s)))
		t)
	       ((match (cdr p) s)
		(setf (match-variable (car p)) ())
		t)
	       ((and s (match p (cdr s)))
		(setf (match-variable (car p))
		      (cons (car s) (match-variable (car p))))
		t)
	       (t nil)))
	((and s
	      (or (eq (match-type (car p)) '?)
;(format t "about to die?")
		  (funcall (match-type (car p)) (car s))))
	 (cond ((match (cdr p) (cdr s))
		(setf (match-variable (car p)) (car s))
		t)
	       (t nil)))
	(t nil)))

(defun match-type (l)
  (first l))

(defun match-variable (l)
  (symbol-value (second l)))

(defsetf match-variable (l) (val)
  `(setf (symbol-value (second ,l)) ,val))

; (defun match (p s)
;   (cond ((null p) (null s))
; 	((atom (car p))
; 	 (and s (equal (car p) (car s)) (match (cdr p) (cdr s))))
; 	((and s (eq (caar p) '?))
; 	 (cond ((match (cdr p) (cdr s))
; 		(set (cadar p) (car s))
; 		t)
; 	       (t nil)))
; 	((eq (caar p) '*)
; 	 (cond ((and s (match (cdr p) (cdr s)))
; 		(set (cadar p) (list (car s)))
; 		t)
; 	       ((match (cdr p) s)
; 		(set (cadar p) nil)
; 		t)
; 	       ((and s (match p (cdr s)))
; 		(set (cadar p) (cons (car s) (eval (cadar p))))
; 		t)
; 	       (t nil)))
; 	((and s
; 	      (apply (caar p) (list (car s)))
; 	      (match (cdr p) (cdr s)))
; 	 (set (cadar p) (car s))
; 	 t)
; 	(t nil)))
	      