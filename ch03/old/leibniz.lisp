;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   NAME:               leibniz.lisp
;;;;
;;;;   STARTED:            Fri Aug  9 01:52:16 2002
;;;;   MODIFICATIONS:
;;;;
;;;;   PURPOSE:
;;;;
;;;;
;;;;
;;;;   CALLING SEQUENCE:
;;;;
;;;;
;;;;   INPUTS:
;;;;
;;;;   OUTPUTS:
;;;;
;;;;   EXAMPLE:
;;;;
;;;;   NOTES: defvar vs. defparameter
;;;;
;;;;

(load "match")

(defstruct (rule (:print-function print-rule))
  name
  (goal 'simplify)
  pattern
  action)

(defun print-rule (rule stream depth)
  (format stream "#<Rule ~A>" (rule-name rule)))

(defun no-v1 (f v1)
  (cond ((null f) t)
	((atom f) (not (eq f v1)))
	((no-v1 (car f) v1) (no-v1 (cdr f) v1))
	(t nil)))

(let ((rules (list
	      (make-rule :name "diff-sum-rule"
			 :goal 'differentiate
			 :pattern `(d
				    (,#'(lambda (f)
					  (and (not (atom f))
					       (match '(+ (? e1) (? e2)) f)))
				     e3)
				    (? v1))
			 :action `(+ (d ,e1 ,v1) (d ,e2 ,v1)))
	      (make-rule :name "diff-x-rule"
			 :goal 'differentiate
			 :pattern `(d
				    (,#'(lambda (v) (setq e1 v)) e1)
				    (,#'(lambda (v) (eq v e1)) e2))
			 :action 1)
	      (make-rule :name "diff-const-rule"
			 :goal 'differentiate
			 :pattern `(d
				    (,#'(lambda (f) (setq e1 f)) f)
				    (,#'(lambda (v1) (no-v1 e1 v1)) v1))
			 :action 0)

	      (make-rule :name "diff-product-rule"
			 :goal 'differentiate
			 :pattern `(d
				    (,#'(lambda (f)
					  (and (not (atom f))
					       (match '(* (? e1) (? e2)) f)))
				     e3)
				    (? v1))
			 :action `(+ (* ,e2 (d ,e1 ,v1)) (* ,e1 (d ,e2 ,v1))))
	      (make-rule :name "diff-power-rule"
			 :goal 'differentiate
			 :pattern `(d
				    (,#'(lambda (f)
					  (and (not (atom f))
					       (match '(exp (? e1)
							(numberp e2)) f)))
				     e3)
				    (? v1))
			 :action `(* ,e2 (* (exp ,e1 (1- ,e2)) (d ,e1 ,v1))))
	      (make-rule :name "goal-change-rule"
			 :goal 'differentiate
			 :pattern '((* f))
			 :action '(prog ()
				   (setq *current-goal* 'simplify) (return f)))
	      (make-rule :name "sub1-rule"
			 :pattern '(1- (numberp e1))
			 :action '(1- e1))
	      (make-rule :name "exp0-rule"
			 :pattern '(exp (? e1) 0)
			 :action 1)
	      (make-rule :name "exp1-rule"
			 :pattern '(exp (? e1) 1)
			 :action 'e1)
	      (make-rule :name "times1-rule"
			 :pattern '(* (? e1) 1)
			 :action 'e1)
	      (make-rule :name "one-times-rule"
			 :pattern '(* 1 (? e1))
			 :action 'e1)
	      (make-rule :name "times0-rule"
			 :pattern '(* (? e1) 0)
			 :action 0)
	      (make-rule :name "zero-times-rule"
			 :pattern '(* 0 (? e1))
			 :action 0)
	      (make-rule :name "plus0-rule"
			 :pattern '(+ (? e1) 0)
			 :action 'e1)
	      (make-rule :name "zero-plus-rule"
			 :pattern '(+ 0 (? e1))
			 :action 'e1)
	      (make-rule :name "constant-addition-rule"
			 :pattern '(+ (numberp e1) (numberp e2))
			 :action '(+ e1 e2))
	      (make-rule :name "constant-multiplication-rule"
			 :pattern '(* (numberp e1) (numberp e2))
			 :action '(* e1 e2)))) )
	      
  (defun control ()
    (loop
     (cond ((not (try-rules rules))
	    (return *current-formula*)))) ))

(defun try-rules (rules-left)
  (cond ((null rules-left) nil)
	(t (let ((temp (try-rule (car rules-left)
				 *current-formula*)))
	     (if temp
		 (setq *current-formula* temp)
		 (try-rules (cdr rules-left)))) )))

(defun try-rule (rule expression)
  (labels ((try-rule-aux (rule expression)
	     (cond ((atom expression) nil)
		   ((match (rule-pattern rule) expression)
;   (format t "Rule: ~S~%" rule)
;   (format t "Action: ~S~%" (rule-action rule))
		    (fire rule))
		   (t (try-rule-on-list rule expression))))
	   (try-rule-on-list (rule expression-list)
	     (let (temp)
	       (cond ((null expression-list) nil)
		     ((setq temp (try-rule-aux rule
					       (car expression-list)))
		      (cons temp (cdr expression-list)))
		     ((setq temp (try-rule-on-list rule
						   (cdr expression-list)))
		      (cons (car expression-list) temp))
		     (t nil)))) )
    (if (not (eq *current-goal* (rule-goal rule)))
	nil
	(try-rule-aux rule expression))))

(defun fire (rule)
  (format t "~&~A fires." (rule-name rule))
  (eval (rule-action rule)))

(defun reset (goal)
  (setf *current-goal* 'differentiate)
  (setf *current-formula* goal))

  
     