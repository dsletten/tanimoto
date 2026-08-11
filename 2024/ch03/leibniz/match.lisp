;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;    Lisp...not just beautiful, but strangely beautiful.
;;;;    -- Paul Graham
;;;;
;;;;    Name:               match.lisp
;;;;
;;;;    Started:            Mon Aug  3 19:07:39 2026
;;;;    Modifications:
;;;;
;;;;    Purpose:
;;;;
;;;;
;;;;
;;;;    Calling Sequence:
;;;;
;;;;
;;;;    Inputs:
;;;;
;;;;    Outputs:
;;;;
;;;;    Example:
;;;;
;;;;    Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core")
(load "/home/slytobias/lisp/packages/test")

(defpackage :matchl (:use :common-lisp :core :test))

(in-package :matchl)

(defun matchl (pattern subject)
  (labels ((capture-binding-on-successful-match (var val p s)
             (multiple-value-bind (match subs) (matchl p s)
               (if match
                   (check-consistency var val match subs)
                   (failed-match))) )
           (check-consistency (var val match subs)
             (cond ((new-variable-p var subs) (values match (acons var val subs)))
                   ((consistentp var val subs) (values match subs))
                   (t (failed-match))))
           (failed-match ()
             (values nil '()))
           (pattern-operator (expr)
             (handler-case (destructuring-bind (operator var) expr
                             (values operator var))
               (error () (values nil nil))) ))
    (cond ((and (null pattern) (null subject)) (values t '()))
          ((null subject) (multiple-value-bind (operator var) (pattern-operator (first pattern))
                            (case operator
                              (* (capture-binding-on-successful-match var '() (rest pattern) subject))
                              (otherwise (failed-match))) ))
          ((or (atom pattern) (atom subject)) (failed-match))
          (t (destructuring-bind (p . ps) pattern
               (destructuring-bind (s . ss) subject
                 (cond ((equalp p s) (matchl ps ss)) ; Supersedes wildcard match?! (match '((? x) b c) '((? x) b c))
                       ((atom p) (failed-match)) ; No literal match => must be wildcard (not atom)
                       (t (multiple-value-bind (operator var) (pattern-operator p)
                            (case operator
                              ((nil) (failed-match)) ; Malformed
                              (? (capture-binding-on-successful-match var s ps ss))
                              (* (multiple-value-bind (match subs) (matchl ps subject) ; Try match consuming 0 elts
                                   (if match
                                       (values match (acons var '() subs))
                                       (multiple-value-bind (match subs) (matchl pattern ss) ; Retry match consuming 1 elt
                                         (if match
                                             (destructuring-bind (entry . entries) subs
                                               (assert (eq var (first entry)))
                                               (values match (acons var (cons s (rest entry)) entries)))
                                             (failed-match))) )))
                              (otherwise (if (funcall operator s)
                                             (capture-binding-on-successful-match var s ps ss)
                                             (failed-match))) )))) )))) ))

;;;
;;;    Here is the answer to whether or not the order of clauses for (* X) match matters:
;;;
;; (* (multiple-value-bind (match subs) (matchl ps subject) ; Try match consuming 0 elts
;;      (if match
;;          (values match (acons var '() subs))
;;        (multiple-value-bind (match subs) (matchl pattern ss) ; Retry match consuming 1 elt
;;          (if match
;;              (destructuring-bind (entry . entries) subs
;;                (assert (eq var (first entry)))
;;                (values match (acons var (cons s (rest entry)) entries)))
;;            (failed-match))) )))


;; (matchl '((* x) (* y)) '(a b c d)) => T; ((X) (Y A B C D))


;; (* (multiple-value-bind (match subs) (matchl pattern ss)
;;      (if match
;;          (destructuring-bind (entry . entries) subs
;;            (assert (eq var (first entry)))
;;            (values match (acons var (cons s (rest entry)) entries)))
;;        (multiple-value-bind (match subs) (matchl ps subject)
;;          (if match
;;              (values match (acons var '() subs))
;;            (failed-match))) )))

;; (matchl '((* x) (* y)) '(a b c d)) => T; ((X A B C D) (Y))

(defun new-variable-p (var subs)
  (not (assoc var subs)))

(defun consistentp (var val subs)
  (equal val (value var subs)))

(defun value (var subs)
  (rest (assoc var subs)))

(setf (symbol-function 'match) #'matchl)

(deftest test-matchl ()
  (check
   (matchl '() '())

   (matchl '(a b c) '(a b c))
   (not (matchl '(a b c) '(a b c d)))
   (not (matchl '(a b c d) '(a b c)))

   (matchl '(1d0 2d0 3d0) '(1 2 3))
   (not (matchl '(1d0 2d0 3d0) '(1 2 3 4)))
   (not (matchl '(1d0 2d0 3d0 4d0) '(1 2 3)))

   (not (matchl 'a 'a))
   (not (matchl '(? x) 'a))
   (not (matchl '(a . b) '(a . b)))
   (not (matchl '((? x) . b) '(a . b)))

   (matchl '("pung" "foo" "bar") '("PUNG" "FOO" "BAR"))

   (matchl '((a)) '((a)))
   (matchl '(a (b (c d))) '(a (b (c d)))) ; 2 elts (No recursive descent)

   ;;    Malformed patterns
   (not (matchl '(a b () d) '(a b c d)))
;   (not (matchl '(a b (z x) d) '(a b c d)))
;   (not (matchl '(a b (! x) d) '(a b c d)))
   (not (matchl '(a b (? x m) d) '(a b c d)))

   ;;   Wildcard matches any top-level element
   (equal '(t ((x . c)))
          (multiple-value-list (matchl '(a b (? x) d)
                                        '(a b c d))))
   (equal '(t ((x . (c))))
          (multiple-value-list (matchl '(a b (? x) d)
                                        '(a b (c) d))))
   (equal '(t ((x . (c is (the speed (of light) (in vacuum)))) ))
          (multiple-value-list (matchl '(a b (? x) d)
                                        '(a b (c is (the speed (of light) (in vacuum))) d))))

   (matchl '(* + & %) '(* + & %))
   (not (matchl '(? + & %) '(* + & %))) ; ? is no longer wildcard
   (matchl '(? + & %) '(? + & %))
   (not (matchl '(a b c d) '(a b (? x) d))) ; ? is not wildcard in subject

   (equal '(t ((x . (is (this (not (pung?)))) )))
          (multiple-value-list (matchl '(a (? x) c)
                                        '(a (is (this (not (pung?)))) c)))) ; 3 elts
   (equal '(t ((x . 1) (y . 3) (z . 5)))
          (multiple-value-list (matchl '((? x) (? y) (? z))
                                        '(1 3 5)))) ; Multiple single matches
   (equal '(t ((x . 1) (y . 3) (z . 5)))
          (multiple-value-list (matchl '(0 (? x) 2 (? y) 4 (? z) 6)
                                       '(0 1 2 3 4 5 6))))

   ;;    Wild sequence
   (equal '(t ((x . (b))))
          (multiple-value-list (matchl '(a (* x) c) '(a b c))))
   (equal '(t ((x . (b c))))
          (multiple-value-list (matchl '(a (* x)) '(a b c))))
   (equal '(t ((x . ())))
          (multiple-value-list (matchl '(a (* x)) '(a))))
   (equal '(t ((x . ())))
          (multiple-value-list (matchl '(a (* x) b) '(a b))))
   (equal '(t ((x . (b c d))))
          (multiple-value-list (matchl '(a (* x) b) '(a b c d b))))

   (equal '(t ((x . ())))
          (multiple-value-list (matchl '(a (* x) c d) '(a c d))))
   (equal '(t ((x . (c d))))
          (multiple-value-list (matchl '(a (* x) c d) '(a c d c d))))
   (equal '(t ((x . (c d c d))))
          (multiple-value-list (matchl '(a (* x) c d) '(a c d c d c d))))

   ;;    Ambiguous?
   (equal '(t ((x))) ; i.e., (x . nil)
          (multiple-value-list (matchl '(a (* x)) '(a))))
   (equal '(t ((x))) ; i.e., (x . nil)
          (multiple-value-list (matchl '(a (? x)) '(a nil))))

   ;;    Consistency check
   (not (matchl '((? x) b (* x)) '(a b c d)))
   (not (matchl '((? x) (? y) (? x)) '(foo bar baz)))


   ;;    Predicate
   (not (matchl '(a b (numberp x) d) '(a b c d)))
   (equal '(t ((x . 17)))
          (multiple-value-list (matchl '(a b (numberp x) d) '(a b 17 d))))
   (equal '(t ((z . 4) (y . 2) (x . 9) (p . 1.8d0)))
          (multiple-value-list (matchl '((plusp z) (evenp y) (numberp x) (floatp p)) '(4 2 9 1.8d0))))
   (not (matchl '((plusp z) (evenp y) (numberp x) (floatp p)) '(4 2 9 1)))
   (equal '(t ((z . 4) (y . 2) (x . 9) (p . 1)))
          (multiple-value-list (matchl (list '(plusp z) '(evenp y) '(numberp x) (list #'(lambda (x) (< x 8)) 'p)) '(4 2 9 1))))
   ;;    Equivalent of wildcard
   (equal '(t ((x . 9)))
          (multiple-value-list (matchl (list 4 2 (list (constantly t) 'x) 1) '(4 2 9 1))))

   (equal '(t ((x . (* specifies a)) (y . card) (z . (sequence element))))
          (multiple-value-list (matchl '((* x) wild (? y) (* z))
                                       '(* specifies a wild card sequence element))))

   (equal '(t ((x my crazy) (y likes to tell bad jokes) (z very annoying to me)))
          (multiple-value-list (matchl '(i do not like (* x) coach because he (* y) all of the time which is (* z))
                                       '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me)))) ))
