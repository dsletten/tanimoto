;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch02.lisp
;;;
;;;   STARTED:            Sun Jul 28 00:58:28 2002
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
;;;   NOTES: Tanimoto 1e.
;;;
;;;
(load "~/lisp/programs/utils.lisp")
;;;
;;;    Ex. 6
;;;
(defun exify (obj)
  (cond ((null obj) nil)
	((atom obj) 'x)
	(t (cons (exify (car obj))
		 (exify (cdr obj)))) ))

;;;
;;;    Ex. 7
;;;
(defun my-replace (obj old new)
  (cond ((equal obj old) new)
	((atom obj) obj)
	(t (cons (my-replace (car obj) old new)
		 (my-replace (cdr obj) old new)))) )

;;;
;;;    Ex. 8
;;;
(let ((max-val 15))
  (defun cubes-1 (&optional (n 1))
    (cond ((<= n max-val)
	   (format t "~D " (* n n n))
	   (cubes-1 (1+ n)))
	  (t (format t "~%"))))

  (defun cubes-2 ()
    (let ((l ()))
      (dotimes (i max-val)
	(push (1+ i) l))
      (format t "~{~D ~}~%" (mapcar #'(lambda (x) (* x x x)) (reverse l)))) ))

;;;
;;;    Ex. 11
;;;
(defun palindromep (l)
  (equal l (reverse l)))

;;;
;;;    Ex. 14
;;;
(defun equalelts-1 (l)
  (cond ((null (cdr l)) t)
	((equal (car l) (cadr l)) (equalelts-1 (cdr l)))
	(t nil)))

(defun equalelts-2 (l)
  (notany #'null (mapcar #'(lambda (elt) (equal elt (car l))) (cdr l))))

(defun equalelts-3 (l)
  (every (complement #'null)
	 (mapcar #'(lambda (elt) (equal elt (car l))) (cdr l))))

(defun equalelts-4 (l)
  (null (find-if-not #'(lambda (elt) (equal elt (car l))) (cdr l))))

(defun equalelts-5 (l)
  (null (find-if (complement #'(lambda (elt) (equal elt (car l)))) (cdr l))))

(let ((test-data '((((a a a a a)) t)
		   ((((a b) (a b) (a b) (a b))) t)
		   ((((a b) (a b) (a b) (a b) (a))) nil)
		   ((((a b))) t)
		   ((()) t))))
  (dolist (f '(equalelts-1 equalelts-2 equalelts-3 equalelts-4 equalelts-5))
    (test f test-data)))

;;;
;;;    Ex. 15
;;;
(defun quasi-balanced-p (tree)
  (and (equalelts-1 (mapcar #'length tree))
       (equalelts-1 (mapcar #'depth tree))))

(defun depth (obj)
  (cond ((atom obj) 0)
	(t (1+ (max (depth (car obj))
		    (depth (cdr obj)))) )))

(test 'quasi-balanced-p '(((((a b) (c d) (e f))) t)
			  ((((a b) (c d) (e f g))) nil)
			  ((((a b) (c (d)) (e f))) nil)))
;;;
;;;    Ex. 16
;;;
(defun treemax (obj)
  (cond ((and (numberp (first obj))
	      (numberp (second obj)))
	 (max (first obj) (second obj)))
	((numberp (first obj))
	 (max (first obj) (treemin (second obj))))
	((numberp (second obj))
	 (max (treemin (first obj)) (second obj)))
	(t (max (treemin (first obj)) (treemin (second obj)))) ))

(defun treemin (obj)
  (cond ((and (numberp (first obj))
	      (numberp (second obj)))
	 (min (first obj) (second obj)))
	((numberp (first obj))
	 (min (first obj) (treemax (second obj))))
	((numberp (second obj))
	 (min (treemax (first obj)) (second obj)))
	(t (min (treemax (first obj)) (treemax (second obj)))) ))

(test 'treemax '(((((3 (2 5)) (7 (3 1)))) 3)
		 (((((1 2) (3 4)) ((5 (6 7)) 8))) 6)
		 (((1 (8 (2 (7 (3 (6 (4 5)))) )))) 5)))

(test 'treemin '(((((3 (2 5)) (7 (3 1)))) 3)
		 (((((1 2) (3 4)) ((5 (6 7)) 8))) 3)
		 (((1 (8 (2 (7 (3 (6 (4 5)))) )))) 1)))

;;;
;;;    Ex. 18
;;;
(defun next (num-list)
  (when num-list
    (let ((diff (arithmeticp num-list)))
      (if diff
	  (+ (car (last num-list)) diff)
	  (let ((ratio (geometricp num-list)))
	    (if ratio
		(* (car (last num-list)) ratio)
		'unknown)))) ))

(defun arithmeticp (num-list)
  (labels ((arithmetic-prog (diff l)
	     (cond ((null (cdr l)) diff)
		   ((= (- (second l) (first l)) diff)
		    (arithmetic-prog diff (cdr l)))
		   (t nil))))
    (if (null (cdr num-list))
	nil
	(arithmetic-prog (- (second num-list) (first num-list))
			 (cdr num-list)))) )

(defun geometricp (num-list)
  (labels ((geometric-prog (ratio l)
	     (cond ((null (cdr l)) ratio)
		   ((= (/ (second l) (first l)) ratio)
		    (geometric-prog ratio (cdr l)))
		   (t nil))))
    (if (null (cdr num-list))
	nil
	(geometric-prog (/ (second num-list) (first num-list))
			(cdr num-list)))) )

(test 'next `((((2 20 40)) unknown)
	      (((4 -12 36 -108)) 324)
	      (((3 1 4 1)) unknown)
	      (((1 2 4 8 16 32 64 128)) 256)
	      ((,(reverse '(2 4 8 16 32 64 128))) 1)
	      ((,(reverse '(2 4 6 8))) 0)
	      (((2 4 6 8)) 10)))

;;;
;;;    Ex. 20
;;;
(defun simplify (expr)
  (cond ((null expr) ())
	((atom expr) expr)
	((eq (car expr) '+) (simplify-add expr))
	((eq (car expr) '*) (simplify-mul expr))))

;;;
;;;    Process each element of EXPR, transferring processed elements into
;;;    NEW-EXPR. If the final NEW-EXPR is a single-element list, then
;;;    eliminate the addition operation. Otherwise stick + at the beginning
;;;    of NEW-EXPR.
;;;
;;;    If we encounter a 0 in EXPR we skip it. Other atoms get transferred 'as
;;;    is'. A CONS is a sub-expression which requires recursive simplifying. If
;;;    it comes back as a CONS, add it to NEW-EXPR. Otherwise it could be 0,
;;;    so take another look at it.
;;;    
(defun simplify-add (expr)
  (labels ((add-aux (expr new-expr)
;  	     (format t "expr: ~S new: ~S " expr new-expr)
;  	     (read-line)
	     (cond ((null expr) (if (null (cdr new-expr))
				    (car new-expr)
				    (cons '+ (reverse new-expr))))
		   ((and (numberp (car expr))
			 (zerop (car expr)))
		    (add-aux (cdr expr) new-expr))
		   ((atom (car expr))
		    (add-aux (cdr expr) (cons (car expr) new-expr)))
		   (t (let ((expr1 (simplify (car expr))))
			(if (consp expr1)
			    (add-aux (cdr expr) (cons expr1 new-expr))
			    (add-aux (cons expr1 (cdr expr)) new-expr)))) )))
    (add-aux (cdr expr) ())))

(defun simplify-mul (expr)
  (labels ((mul-aux (expr new-expr)
	     (cond ((null expr) (if (null (cdr new-expr))
				    (car new-expr)
				    (cons '* (reverse new-expr))))
		   ((and (numberp (car expr))
			 (zerop (car expr)))
		    0)
		   ((and (numberp (car expr))
			 (= (car expr) 1))
		    (mul-aux (cdr expr) new-expr))
		   ((atom (car expr))
		    (mul-aux (cdr expr) (cons (car expr) new-expr)))
		   (t (let ((expr1 (simplify (car expr))))
			(if (consp expr1)
			    (mul-aux (cdr expr) (cons expr1 new-expr))
			    (mul-aux (cons expr1 (cdr expr)) new-expr)))) )))
    (mul-aux (cdr expr) ())))

(defun simplify (expr)
  (cond ((null expr) ())
	((atom expr) expr)
	((eq (car expr) '+) (simplify-aux expr '+ #'add-test))
	((eq (car expr) '*) (simplify-aux expr '* #'mul-test))))

;;;
;;;    TEST should return NIL to indicate that the number is to be ignored.
;;;    If it returns 0 (as in the case of multiplication) terminate processing
;;;    immediately--the product is 0. Otherwise include the number as part of
;;;    the result.
;;;    
(defun simplify-aux (expr op test)
  (labels ((simplify-aux-1 (expr new-expr)
	     (cond ((null expr) (if (null (cdr new-expr))
				    (car new-expr)
				    (cons op (reverse new-expr))))
		   ((numberp (car expr))
		    (let ((num-expr (funcall test (car expr))))
		      (cond ((numberp num-expr)
			     (if (zerop num-expr)
				 0
				 (simplify-aux-1 (cdr expr)
						 (cons (car expr) new-expr))))
			    (t (simplify-aux-1 (cdr expr) new-expr)))) )
		   ((atom (car expr))
		    (simplify-aux-1 (cdr expr) (cons (car expr) new-expr)))
		   (t (let ((expr1 (simplify (car expr))))
			(if (consp expr1)
			    (simplify-aux-1 (cdr expr) (cons expr1 new-expr))
			    (simplify-aux-1 (cons expr1 (cdr expr))
					    new-expr)))) )))
    (simplify-aux-1 (cdr expr) ())))

(defun add-test (num)
  (unless (zerop num) num))

(defun mul-test (num)
  (unless (= num 1) num))
