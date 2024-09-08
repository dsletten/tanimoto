;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Sat May 22 04:00:56 2004
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes: Tanimoto 1e.
;;;;
;;;;
(load "/Users/dsletten/lisp/programs/utils.lisp")

(defpackage ch02 (:use common-lisp common-lisp-user ext) (:shadow replace))

(in-package ch02)

;;;
;;;   Ex. 6
;;;   
(defun exify (expr)
  (cond ((null expr) '())
	((atom expr) 'x)
	(t (cons (exify (car expr))
		 (exify (cdr expr)))) ))

;;;
;;;    Ex. 7
;;;
(defun replace (s1 s2 s3)
  (cond ((null s1) '())
	((equal s1 s2) s3)
	((atom s1) s1)
	(t (cons (replace (car s1) s2 s3)
		 (replace (cdr s1) s2 s3)))) )

;; ;;;
;; ;;;    Ex. 8
;; ;;;
;; (defun cubes (n)
;;   (labels ((cubes-aux (i result)
;; 	     (cond ((zerop i) result)
;; 		   (t (cubes-aux (1- i) (cons (expt i 3) result)))) ))
;;     (cubes-aux n '())))

;; (defun cubes-1 (n)
;;   (do* ((i n (1- i))
;; 	(result (list (expt i 3)) (cons (expt i 3) result)))
;;        ((= i 1) result)))

;; (defun cubes-2 (n)
;;   (loop for i from 1 to n
;; 	collect (expt i 3)))

;; (defun make-range (start end)
;;   (when (> start end)
;;     (rotatef start end))
;;   (do ((i start (1+ i))
;;        (result ()))
;;       ((> i end) (nreverse result))
;;     (push i result)))

;; (defun cubes-3 (n)
;;   (mapcar #'(lambda (x) (* x x x)) (make-range 1 n)))

;; ;;;
;; ;;;    Ex. 10
;; ;;;
;; (defun sum (ilist)
;;   (apply #'+ ilist))

;; ;;;
;; ;;;    Ex. 11
;; ;;;
;; (defun palindromep (l)
;;   (equal l (reverse l)))

;; ;;;
;; ;;;    Ex. 13
;; ;;;
;; (defun double-elts (l)
;;   (mapcar #'(lambda (elt) (list elt elt)) l))

;; ;;;
;; ;;;    Ex. 14
;; ;;;
;; (defun equalelts (l)
;;   (cond ((null l) t)
;; 	((null (cdr l)) t)
;; 	((equal (first l) (second l))
;; 	 (equalelts (cdr l)))
;; 	(t nil)))

;; (defun equalelts-1 (l)
;;   (every #'(lambda (elt) (equal elt (car l))) (cdr l)))

;; ;;;
;; ;;;    Doesn't work!
;; ;;;    (equal (equal elt1 elt2) elt3) => NIL
;; ;; (defun equalelts-2 (l)
;; ;;   (or (null l)
;; ;;       (reduce #'equal l)))

;; ;;;
;; ;;;    Ex. 15
;; ;;;
;; (defun tree-depth (obj)
;;   (cond ((atom obj) 0)
;; 	(t (1+ (max (tree-depth (car obj))
;; 		    (tree-depth (cdr obj)))) )))

;; (defun quasi-balanced-p (l)
;;   (and (apply #'= (mapcar #'length l))
;;        (apply #'= (mapcar #'tree-depth l))))

;; (test 'quasi-balanced-p '(((((a b) (c d) (e f))) t)
;; 			  ((((a b) (c d) (e f g))) nil)
;; 			  ((((a b) (c (d)) (e f))) nil)))

;; ;;;
;; ;;;    Ex. 16
;; ;;;    (Compare to disgusting version in old/ch02.lisp!!)
;; ;;;
;; ;;;    (treemax '((3 (2 5)) (7 (3 1))))
;; ;;;
;; ;;;                (3)                max
;; ;;;               /   \
;; ;;;              /     \
;; ;;;            (3)     (3)            min
;; ;;;           / |       | \
;; ;;;          /  |       |  \
;; ;;;         3  (5)      7  (3)        max
;; ;;;           /   \       /   \
;; ;;;          /     \     /     \
;; ;;;         2       5   3       1
;; ;;;
;; ;;;    (treemax '(((1 2) (3 4)) ((5 (6 7)) 8)))
;; ;;;
;; ;;;                (6)                max
;; ;;;               /   \
;; ;;;              /     \
;; ;;;            (2)     (6)            min
;; ;;;           / |       | \
;; ;;;          /  |       |  \
;; ;;;        (2) (4)     (6)  8         max
;; ;;;       / |   | \     | \
;; ;;;      /  |   |  \    |  \
;; ;;;     1   2   3   4   5  (6)        min
;; ;;;                       /   \
;; ;;;                      /     \
;; ;;;                     6       7
;; ;;;
;; ;;;    (treemax '(1 (8 (2 (7 (3 (6 (4 5)))) ))))
;; ;;;
;; ;;;                (5)                             max
;; ;;;               /   \
;; ;;;              /     \
;; ;;;             1      (5)                         min
;; ;;;                   /   \
;; ;;;                  /     \
;; ;;;                 8      (5)                     max
;; ;;;                       /   \
;; ;;;                      /     \
;; ;;;                     2      (5)                 min
;; ;;;                           /   \
;; ;;;                          /     \
;; ;;;                         7      (5)             max
;; ;;;                               /   \
;; ;;;                              /     \
;; ;;;                             3      (5)         min
;; ;;;                                   /   \
;; ;;;                                  /     \
;; ;;;                                 6      (5)     max
;; ;;;                                       /   \
;; ;;;                                      /     \
;; ;;;                                     4       5
;; ;;;
;; ;;;               
;; ;;;
;; ;;;    (treemin '((3 (2 5)) (7 (3 1))))
;; ;;;
;; ;;;                (3)                min
;; ;;;               /   \
;; ;;;              /     \
;; ;;;            (3)     (7)            max
;; ;;;           / |       | \
;; ;;;          /  |       |  \
;; ;;;         3  (2)      7  (1)        min
;; ;;;           /   \       /   \
;; ;;;          /     \     /     \
;; ;;;         2       5   3       1
;; ;;;
;; ;;;    (treemin '(((1 2) (3 4)) ((5 (6 7)) 8)))
;; ;;;
;; ;;;                (3)                min
;; ;;;               /   \
;; ;;;              /     \
;; ;;;            (3)     (8)            max
;; ;;;           / |       | \
;; ;;;          /  |       |  \
;; ;;;        (1) (3)     (5)  8         min
;; ;;;       / |   | \     | \
;; ;;;      /  |   |  \    |  \
;; ;;;     1   2   3   4   5  (7)        max
;; ;;;                       /   \
;; ;;;                      /     \
;; ;;;                     6       7
;; ;;;
;; ;;;    (treemin '(1 (8 (2 (7 (3 (6 (4 5)))) ))))
;; ;;;
;; ;;;                (1)                             min
;; ;;;               /   \
;; ;;;              /     \
;; ;;;             1      (8)                         max
;; ;;;                   /   \
;; ;;;                  /     \
;; ;;;                 8      (2)                     min
;; ;;;                       /   \
;; ;;;                      /     \
;; ;;;                     2      (7)                 max
;; ;;;                           /   \
;; ;;;                          /     \
;; ;;;                         7      (3)             min
;; ;;;                               /   \
;; ;;;                              /     \
;; ;;;                             3      (6)         max
;; ;;;                                   /   \
;; ;;;                                  /     \
;; ;;;                                 6      (4)     min
;; ;;;                                       /   \
;; ;;;                                      /     \
;; ;;;                                     4       5
;; ;;;
;; ;;;               
;; (defun treemax (obj)
;;   (cond ((numberp obj) obj)
;; 	(t (max (treemin (first obj))
;; 		(treemin (second obj)))) ))

;; (defun treemin (obj)
;;   (cond ((numberp obj) obj)
;; 	(t (min (treemax (first obj))
;; 		(treemax (second obj)))) ))

;; (labels ((tree-aux (obj op1 op2)
;; 	   (cond ((numberp obj) obj)
;; 		 (t (funcall op1
;; 			     (tree-aux (first obj) op2 op1)
;; 			     (tree-aux (second obj) op2 op1)))) ))
;;   (defun treemax-1 (obj)
;;     (tree-aux obj #'max #'min))
;;   (defun treemin-1 (obj)
;;     (tree-aux obj #'min #'max)))

;; (dolist (f '(treemax treemax-1))
;;   (test f '(((((3 (2 5)) (7 (3 1)))) 3)
;; 	    (((((1 2) (3 4)) ((5 (6 7)) 8))) 6)
;; 	    (((1 (8 (2 (7 (3 (6 (4 5)))) )))) 5))))

;; (dolist (f '(treemin treemin-1))
;;   (test f '(((((3 (2 5)) (7 (3 1)))) 3)
;; 	    (((((1 2) (3 4)) ((5 (6 7)) 8))) 3)
;; 	    (((1 (8 (2 (7 (3 (6 (4 5)))) )))) 1))))

;; ;;;
;; ;;;    Ex. 17
;; ;;;
;; ;;
;; ;; (defun make-past (present)
;; ;;   (mapcar #'(lambda (word)
;; ;; 	      (cond ((eq word 'am) 'was)
;; ;; 		    ((eq word 'are) 'were)
;; ;; 		    ((eq word 'is) 'was)
;; ;; 		    (t word)))
;; ;; 	  present))
;; (let ((present-past-alist '((am was) (are were) (is was)
;; 			    (eat ate) (eats ate)
;; 			    (sing sang) (sings sang)
;; 			    (fly flew) (flies flew)
;; 			    (dance danced) (dances danced)
;; 			    (write wrote) (writes wrote))))
;;   (defun make-past (present)
;;     (mapcar #'(lambda (word)
;; 		(let ((entry (assoc word present-past-alist)))
;; 		  (if entry
;; 		      (second entry)
;; 		      word)))
;; 	    present)))

;; (let ((opposites-alist '((good bad)
;; 			 (hot cold)
;; 			 (fast slow)
;; 			 (tall short)
;; 			 (shiny dull)
;; 			 (new old)
;; 			 (interesting boring)
;; 			 (sleepy alert)
;; 			 (happy sad)
;; 			 (strong weak))))
;;   (defun make-opposites (sentence)
;;     (mapcar #'(lambda (word)
;; 		(let ((entry (assoc word opposites-alist)))
;; 		  (if entry
;; 		      (second entry)
;; 		      (let ((rev-entry (rassoc word opposites-alist
;; 					       :key #'car)))
;; 			(if rev-entry
;; 			    (first rev-entry)
;; 			    word)))) )
;; 	    sentence)))


;; ;;;
;; ;;;    Ex. 18
;; ;;;    
;; (defun next (seq)
;;   (labels ((next-aux (seq op factor elt)
;; 	     (cond ((null seq) (funcall op factor elt))
;; 		   ((= (first seq) (funcall op factor elt))
;; 		    (next-aux (rest seq) op factor (first seq)))
;; 		   (t nil))))
;;     (cond ((null (cdr seq)) 'invalid-sequence) ;length < 2
;; 	  (t (let ((next-elt (or (next-aux (rest seq)
;; 					   #'+
;; 					   (- (second seq) (first seq))
;; 					   (first seq))
;; 				 (next-aux (rest seq)
;; 					   #'*
;; 					   (/ (second seq) (first seq))
;; 					   (first seq)))) )
;; 	       (if next-elt
;; 		   next-elt
;; 		   'unknown)))) ))

;; ;;;
;; ;;;    Work only with rational numbers to avoid problems with floating-point.
;; ;;;    If the sequence is a valid arithmetic or geometric progression then
;; ;;;    the next value will either be an integer or a ratio. If a ratio and
;; ;;;    the sequence consists of floats, then convert to a float. Otherwise
;; ;;;    return as is.
;; ;;;    
;; (defun next (seq)
;;   (labels ((next-aux (seq op factor elt)
;; 	     (cond ((null seq) (funcall op factor elt))
;; 		   ((= (rationalize (first seq)) (funcall op factor elt))
;; 		    (next-aux (rest seq) op factor (rationalize (first seq))))
;; 		   (t nil))))
;;     (cond ((null (cdr seq)) 'invalid-sequence) ;length < 2
;; 	  (t (let* ((first-rat (rationalize (first seq)))
;; 		    (second-rat (rationalize (second seq)))
;; 		    (next-elt (or (next-aux (rest seq)
;; 					    #'+
;; 					    (- second-rat first-rat)
;; 					    first-rat)
;; 				  (next-aux (rest seq)
;; 					    #'*
;; 					   (/ second-rat first-rat)
;; 					   first-rat))))
;; 	       (cond ((integerp next-elt) next-elt)
;; 		     ((some #'floatp seq) (float next-elt))
;; 		     ((numberp next-elt) next-elt)
;; 		     ((null next-elt) 'unknown)
;; 		     (t *error*)))) )))

;; (test 'next '( (((7 5 3 1)) -1)
;; 	       (((4 11 18 25)) 32)
;; 	       (((-1 1 3 5)) 7)
;; 	       (((23 35 47 59)) 71)
;; 	       (((22 23.2 24.4 25.6)) 26.8)
;; 	       (((22 116/5 122/5 128/5)) 134/5)
;; 	       (((7 27/4 13/2 25/4)) 6)
;; 	       (((1/4 1/3 5/12 1/2)) 7/12)

;; 	       (((64 -16 4 -1)) 1/4)
;; 	       (((7 0.7 0.07 0.007)) 0.0007)
;; 	       (((0.003 0.03 0.3)) 3)
;; 	       (((3/1000 3/100 3/10)) 3)
;; 	       (((5 15 45)) 135)
;; 	       (((3 1 1/3)) 1/9)
;; 	       (((-40 20 -10 5)) -5/2)
;; 	       (((3 2 4/3 8/9)) 16/27) ))

;; ;;;
;; ;;;    Ex. 20
;; ;;;
;; (defun simplify (expr)
;;   (cond ((symbolp expr) expr)
;; 	((numberp expr) expr)
;; 	(t (case (first expr)
;; 	     (* (cond ((some #'(lambda (x) (eql x 0)) (rest expr)) 0)
;; 		      ((= (length expr) 2) (second expr))
;; 		      (t 
;; 		       (collect-constants
;; 			(cons '*
;; 			      (remove 1 (mapcar #'simplify
;; 						(rest expr)))) ))))
;; 	     (+ (if (= (length expr) 2)
;; 		    (second expr)
;; 		    (collect-constants
;; 		     (cons '+
;; 			   (remove 0 (mapcar #'simplify
;; 					     (rest expr)))) )))
;; 	     (- (cond ((= (length expr) 2)
;; 		       (if (numberp (second expr))
;; 			   (- (second expr))
;; 			   (second expr)))
;; 		      (t (collect-constants
;; 			  (cons '- (remove 0 (mapcar #'simplify
;; 						     (rest expr))
;; 					   :start 1))))) )
;; 	     (/ (cond ((= (length expr) 2)
;; 		       (if (numberp (second expr))
;; 			   (/ (second expr))
;; 			   (second expr)))
;; 		      (t (collect-constants
;; 			  (cons '/ (remove 1 (mapcar #'simplify
;; 						     (rest expr))
;; 					   :start 1)))) )))) ))

;; ;;;
;; ;;;    EXPR must be form with operator as first elt
;; ;;;
;; (defun leading-identity-elt (expr)
;;   (and (numberp (second expr))
;;        (= (second expr) (case (first expr)
;; 			  ((+ -) 0)
;; 			  ((* /) 1)))) )

;; (defun collect-constants (expr)
;;   (cond ((notany #'numberp expr) expr)
;; 	((numberp (second expr))
;; 	 (if (and (member (first expr) '(- /))
;; 		  (leading-identity-elt expr))
;; 	     (cons (first expr)
;; 		   (collect-constants-aux (case (first expr)
;; 					    ((+ -) #'+)
;; 					    ((* /) #'*))
;; 					  (cddr expr) (case (first expr)
;; 							((+ -) 0)
;; 							((* /) 1))))
;; 	     (cons (first expr)
;; 		   (collect-constants-first (first expr)
;; 					    (cddr expr)
;; 					    (second expr)
;; 					    '()))) )
;; 	(t (cons (first expr)
;; 		 (collect-constants-aux (case (first expr)
;; 					  ((+ -) #'+)
;; 					  ((* /) #'*))
;; 					(cdr expr) (case (first expr)
;; 						     ((+ -) 0)
;; 						     ((* /) 1)))) )))

;; (defun collect-constants-first (op expr const result)
;;   (cond ((null expr) (cons const (nreverse result)))
;; 	((numberp (first expr))
;; 	 (collect-constants-first op
;; 				  (cdr expr)
;; 				  (funcall op const (first expr))
;; 				  result))
;; 	(t (collect-constants-first op (cdr expr) const
;; 				    (cons (first expr) result)))) )

;; (defun collect-constants-aux (op expr const)
;;   (cond ((null expr) (list const))
;; 	((numberp (first expr))
;; 	 (collect-constants-aux op (cdr expr)
;; 				(funcall op const (first expr))))
;; 	(t (cons (first expr)
;; 		 (collect-constants-aux op (cdr expr) const)))) )
