;#!/usr/local/bin/clisp

;;
;   NAME:               leibniz.lsp
;
;   STARTED:            010412
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
(load "match.lsp")

(setf diff-sum-rule
      '(differentiate
	(d ((lambda (f)
	      (and (not (atom f))
		   (match '(+ (? e1) (? e2)) f)))
	    e3)
	   (? v1))
	(list '+ (list 'd e1 v1) (list 'd e2 v1))
	diff-sum-rule))

(setf diff-x-rule
      '(differentiate
	(d ((lambda (v)
	      (setf e1 v))
	    e1)
	   ((lambda (v)
	      (eq v e1))
	    e2))
	1
	diff-x-rule))

(setf diff-const-rule
      '(differentiate
	(d ((lambda (f)
	      (setf e1 f))
	    f)
	   ((lambda (v1)
	      (no-v1 e1 v1))
	    v1))
	0
	diff-const-rule))

(defun no-v1 (f v1)
  (cond ((null f) t)
	((atom f) (not (eq f v1)))
	((no-v1 (car f) v1)
	 (no-v1 (cdr f) v1))
	(t nil)) )

(setf diff-product-rule
      '(differentiate
	(d ((lambda (f)
	      (and (not (atom f))
		   (match '(* (? e1) (? e2)) f)))
	    e3)
	   (? v1))
	(list '+
	      (list '* e2 (list 'd e1 v1))
	      (list '* e1 (list 'd e2 v1)))
	diff-product-rule))

(setf diff-power-rule
      '(differentiate
	(d ((lambda (f)
	      (and (not (atom f))
		   (match '(exp (? e1) (numberp e2)) f)))
	    e3)
	   (? v1))
	(list '* e2
	      (list '* (list 'exp e1 (list '1- e2))
		    (list 'd e1 v1)))
	diff-power-rule))

(setf sub1-rule
      '(simplify
	(1- (numberp e1))
	(1- e1)
	sub1-rule))

(setf constant-addition-rule
      '(simplify
	(+ (numberp e1) (numberp e2))
	(+ e1 e2)
	constant-addition-rule))

(setf constant-multiplication-rule
      '(simplify
	(* (numberp e1) (numberp e2))
	(* e1 e2)
	constant-multiplication-rule))

(setf exp0-rule
      '(simplify
	(exp (? e1) 0)
	1
	exp0-rule))

(setf exp1-rule
      '(simplify
	(exp (? e1) 1)
	e1
	exp1-rule))

(setf times1-rule
      '(simplify
	(* (? e1) 1)
	e1
	times1-rule))

(setf one-times-rule
      '(simplify
	(* 1 (? e1))
	e1
	one-times-rule))

(setf plus0-rule
      '(simplify
	(+ (? e1) 0)
	e1
	plus0-rule))

(setf zero-plus-rule
      '(simplify
	(+ 0 (? e1))
	e1
	zero-plus-rule))

(setf times0-rule
      '(simplify
	(* (? e1) 0)
	0
	times0-rule))

(setf zero-times-rule
      '(simplify
	(* 0 (? e1))
	0
	zero-times-rule))

(setf goal-change-rule
      '(differentiate
	((* f))
	(prog () (setf current-goal 'simplify)
	      (return f))
	goal-change-rule))

(setf rules (list diff-sum-rule diff-x-rule
		  diff-const-rule diff-product-rule
		  diff-power-rule
		  goal-change-rule
		  sub1-rule exp0-rule exp1-rule
		  times1-rule one-times-rule
		  times0-rule zero-times-rule
		  plus0-rule zero-plus-rule
		  constant-addition-rule
		  constant-multiplication-rule))

(defun control ()
  (loop (cond ((not (try-rules rules))
	       (return current-formula)))) )

(defun try-rules (rules-left)
  (cond ((null rules-left) nil)
	((atom current-formula) nil)
	(t (let ((temp (try-rule (car rules-left) current-formula)))
	     (if temp
		 (setf current-formula temp)
	         (try-rules (cdr rules-left)))) )) )

(defun try-rule (rule expression)
  (if (not (eq current-goal (rule-goal rule)))
      nil
      (try-rule1 rule expression)) )

(defun rule-goal (rule)
  (first rule) )

(defun rule-pattern (rule)
  (second rule) )

(defun rule-action (rule)
  (third rule) )

(defun name (rule)
  (fourth rule) )

(defun try-rule1 (rule expression)
  (cond ((atom expression) nil)
	((match (rule-pattern rule) expression)
	 (fire rule))
	(t (try-rule-on-list rule expression))) )

(defun try-rule-on-list (rule expression-list)
  (let (temp)
    (cond ((null expression-list) nil)
	  ((setf temp (try-rule1 rule (car expression-list)))
	   (const temp (cdr expression-list)))
	  ((setf temp (try-rule-on-list rule (cdr expression-list)))
	   (cons (car expression-list) temp))
	  (t nil))) )

(defun fire (rule)
  (format t "~%~A fires." (name rule))
  (eval (rule-action rule)) )
