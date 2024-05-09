;#!/usr/local/bin/clisp

;;
;   NAME:               match.lsp
;
;   STARTED:            010404
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
(defun match1 (p s)
  (equal p s) )

(defun match2 (p s)
  (cond ((atom p) (atom s))
	((atom s) nil)
	((match2 (car p) (car s)) (match2 (cdr p) (cdr s)))
	(t nil)) )

(defun match3 (p s)
  (cond ((null p) (null s))
	((or (atom p) (atom s)) nil)
	((equal (car p) (car s)) (match3 (cdr p) (cdr s)))
	((eq (car p) '?)
	 (match3 (cdr p) (cdr s)))
	(t nil)) )

(defun match4 (p s)
  (cond ((null p) (null s))
	((or (atom p) (atom s)) nil)
	((equal (car p) (car s)) (match4 (cdr p) (cdr s)))
	((and (equal (length (car p)) 2)
	      (eq (caar p) '?)
	      (match4 (cdr p) (cdr s)))
	 (set (cadar p) (car s))
	 t)
	(t nil)) )

(defun match5 (p s)
  (cond ((null p) (null s))
	((or (atom p) (atom s)) nil)
	((equal (car p) (car s)) (match5 (cdr p) (cdr s)))
	((and (equal (length (car p)) 2)
	      (eq (caar p) '?)
	      (match5 (cdr p) (cdr s)))
	 (set (cadar p) (car s))
	 t)
	((and (equal (length (car p)) 2)
	      (not (eq (caar p) '?))
	      (apply (caar p) (list (car s)))
	      (match5 (cdr p) (cdr s)))
	 (set (cadar p) (car s))
	 t)
	(t nil)) )

(defun match (p s)
  (cond ((null p) (null s))
	((atom (car p)) (and s
			     (equal (car p) (car s))
			     (match (cdr p) (cdr s))))
	((and s (eq (caar p) '?)) (cond ((match (cdr p) (cdr s))
					 (set (cadar p) (car s))
					 t)
					(t nil)))
	((eq (caar p) '*) (cond ((and s (match (cdr p) (cdr s)))
				 (set (cadar p) (list (car s)))
				 t)
				((match (cdr p) s)
				 (set (cadar p) nil)
				 t)
				((and s (match p (cdr s)))
				 (set (cadar p) (cons (car s) (eval (cadar p))))
				 t)
				(t nil)))
	((and s (apply (caar p) (list (car s))) (match (cdr p) (cdr s)))
	 (set (cadar p) (car s))
	 t)
	(t nil)) )
