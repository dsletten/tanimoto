;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;    APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;    -- Joel Moses (attributed)
;;;;
;;;;    Name:               ps-match.lisp
;;;;
;;;;    Started:            Mon Jul 27 23:21:38 2026
;;;;    Modifications:
;;;;
;;;;    Purpose: Implement MATCH as production system
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
(load "/home/slytobias/Thelio/modified/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :ps-match (:use :common-lisp :core :test :production-system))

(in-package :ps-match)

(defun match1 (p s)
  (let ((pattern p)
        (subject s))
    (loop (cond ((equalp pattern subject) (return t))
                (t (return nil)))) ))

(deftest test-match1 ()
  (check
   (match1 'a 'a)
   (match1 '((a)) '((a)))
   (match1 '(1d0 2d0 3d0) '(1 2 3))
   (not (match1 '(1d0 2d0 3d0) '(1 2 3 4)))
   (match1 '("pung" "foo" "bar") '("PUNG" "FOO" "BAR"))
   (match1 '(* + & %) '(* + & %))
   (not (match1 '(* + & %) '(? + & %)))
   (match1 '(? + & %) '(? + & %))
   (match1 '(a (b (c d))) '(a (b (c d)))) ))
   
(defun match2 (p s)    
  (let ((pattern p)
        (subject s))
    (loop (cond ((atom pattern) (return (atom subject)))
                ((atom subject) (return nil))
                ((match2 (first pattern) (first subject)) ; Recursive call
                 (setf pattern (rest pattern)
                       subject (rest subject)))
                (t (return nil)))) ))

(deftest test-match2 ()
  (check
   (match2 '(a (b) c) '(x (y) nil))
   (not (match2 '(a (b) c) '(a b c)))
   (match2 8 "Sure, why not?")))

(defun match3 (p s)
  (let ((pattern p)
        (subject s))
    (loop (cond ((null pattern) (return (null subject)))
                ((or (atom subject) (atom pattern)) (return nil))
                ((equalp (first pattern) (first subject))
                 (setf pattern (rest pattern)
                       subject (rest subject)))
                ((eq (first pattern) '?)
                 (setf pattern (rest pattern)
                       subject (rest subject)))
                (t (return nil)))) ))

(deftest test-match3 ()
  (check
   (match3 '(1d0 2d0 3d0) '(1 2 3))
   (not (match3 '(1d0 2d0 3d0) '(1 2 3 4)))
   (match3 '("pung" "foo" "bar") '("PUNG" "FOO" "BAR"))

   ;; Wildcard matches any top-level element
   (match3 '(a b ? d) '(a b c d))
   (match3 '(a b ? d) '(a b (c) d))
   (match3 '(a b ? d) '(a b (c is (the speed (of light) (in vacuum))) d))

   (not (match3 '(a b c d) '(a b ? d))) ; ? is only wildcard in pattern
   (match3 '(* + & %) '(* + & %))
   (not (match3 '(* + & %) '(? + & %)))
   (match3 '(? + & %) '(? + & %))
   (not (match3 'a 'a))
   (match3 '((a)) '((a)))
   (match3 '(a (b (c d))) '(a (b (c d)))) ; 2 elts (No recursive descent)
   (match3 '(a ? c) '(a (is (this (not (pung?)))) c)) ; 3 elts
   (match3 '(0 ? 2 ? 4 ? 6) '(0 1 2 3 4 5 6)))) ; Multiple single matches

(defun match4 (p s)
  (let ((pattern p)
        (subject s))
    (loop (cond ((and (null pattern) (null subject)) (return '((:yes . :yes))))
                ((or (atom subject) (atom pattern)) (return nil))
                ((equalp (first pattern) (first subject))
                 (setf pattern (rest pattern)
                       subject (rest subject)))
                ((atom (first pattern)) (return nil))
                ((and (= (length (first pattern)) 2)
                      (eq (first (first pattern)) '?)
                      (let ((match (match4 (rest pattern) (rest subject))))
                        (if match
                            (return (acons (first (rest (first pattern))) (first subject) match))
                            (return nil)))) )
                (t (return nil)))) ))

(defun 1st-pattern-op (p)
  (first (first p)))

(defun 1st-pattern-variable (p)
  (second (first p)))

(defun match4 (p s)
  (let ((pattern p)
        (subject s))
    (loop (cond ((and (null pattern) (null subject)) (return '((:yes . :yes))))
                ((or (atom subject) (atom pattern)) (return nil))
                ((equalp (first pattern) (first subject))
                 (setf pattern (rest pattern)
                       subject (rest subject)))
                ((atom (first pattern)) (return nil))
                ((and (= (length (first pattern)) 2)
                      (eq (1st-pattern-op pattern) '?)
                      (let ((match (match4 (rest pattern) (rest subject))))
                        (if match
                            (return (acons (1st-pattern-variable pattern) (first subject) match))
                            (return nil)))) )
                (t (return nil)))) ))

(deftest test-match4 ()
  (check
   (equal #1='#2=((:yes . :yes)) (match4 '() '()))
   (equal #1# (match4 '(1d0 2d0 3d0) '(1 2 3)))
   (not (match4 '(1d0 2d0 3d0) '(1 2 3 4)))

   (not (match4 'a 'a))
   (not (match4 '(? x) 'a))
   (not (match4 '(a . b) '(a . b)))
   (not (match4 '((? x) . b) '(a . b)))

   (equal #1# (match4 '("pung" "foo" "bar") '("PUNG" "FOO" "BAR")))

   ;;    Malformed patterns
   (not (match4 '(a b () d) '(a b c d)))
   (not (match4 '(a b (z x) d) '(a b c d)))
   (not (match4 '(a b (! x) d) '(a b c d)))
   (not (match4 '(a b (? x m) d) '(a b c d)))

   ;;   Wildcard matches any top-level element
   (equal '((x . c) . #2#)
          (match4 '(a b (? x) d) '(a b c d)))
   (equal '((x c) . #2#)
          (match4 '(a b (? x) d) '(a b (c) d)))
   (equal '((x c is (the speed (of light) (in vacuum))) . #2#)
          (match4 '(a b (? x) d) '(a b (c is (the speed (of light) (in vacuum))) d)))

   (not (match4 '(a b c d) '(a b ? d))) ; ? is not wildcard in subject
   (equal #1# (match4 '(* + & %) '(* + & %)))
   (not (match4 '(* + & %) '(? + & %)))
   (equal #1# (match4 '(? + & %) '(? + & %))) ; ? is no longer wildcard
   (equal #1# (match4 '((a)) '((a))))
   (equal #1# (match4 '(a (b (c d))) '(a (b (c d))))) ; 2 elts (No recursive descent)
   (equal '((X IS (THIS (NOT (PUNG?)))) . #2#)
          (match4 '(a (? x) c) '(a (is (this (not (pung?)))) c))) ; 3 elts
   (equal '((x . 1) (y . 3) (z . 5) . #2#)
          (match4 '((? x) (? y) (? z)) '(1 3 5))) ; Multiple single matches
   (equal '((x . 1) (y . 3) (z . 5) . #2#)
          (match4 '(0 (? x) 2 (? y) 4 (? z) 6) '(0 1 2 3 4 5 6)))) )

(defun match4* (p s)
  (let ((pattern p)
        (subject s))
    (loop (cond ((and (null pattern) (null subject)) (return (values t '())))
                ((or (atom subject) (atom pattern)) (return (values nil '())))
                (t (destructuring-bind (p . ps) pattern
                     (destructuring-bind (s . ss) subject
                       (cond ((equalp p s) (setf pattern ps subject ss))
                             ((atom p) (return (values nil '())))
                             (t (handler-case (destructuring-bind (operator var) p
                                                (ecase operator
                                                  (? (multiple-value-bind (match subs) (match4* ps ss)
                                                       (if match
                                                           (return (values match (acons var s subs)))
                                                           (return (values nil '()))) ))))
                                  (error () (return (values nil '()))) )))) )))) ))

(deftest test-match4* ()
  (check
   (match4* '() '())
   (match4* '(1d0 2d0 3d0) '(1 2 3))
   (not (match4* '(1d0 2d0 3d0) '(1 2 3 4)))
   (not (match4* '(1d0 2d0 3d0 4d0) '(1 2 3)))

   (not (match4* 'a 'a))
   (not (match4* '(? x) 'a))
   (not (match4* '(a . b) '(a . b)))
   (not (match4* '((? x) . b) '(a . b)))

   (match4* '("pung" "foo" "bar") '("PUNG" "FOO" "BAR"))

   (match4* '((a)) '((a)))
   (match4* '(a (b (c d))) '(a (b (c d)))) ; 2 elts (No recursive descent)

   ;;    Malformed patterns
   (not (match4* '(a b () d) '(a b c d)))
   (not (match4* '(a b (z x) d) '(a b c d)))
   (not (match4* '(a b (! x) d) '(a b c d)))
   (not (match4* '(a b (? x m) d) '(a b c d)))

   ;;   Wildcard matches any top-level element
   (equal '(t ((x . c)))
          (multiple-value-list (match4* '(a b (? x) d)
                                        '(a b c d))))
   (equal '(t ((x . (c))))
          (multiple-value-list (match4* '(a b (? x) d)
                                        '(a b (c) d))))
   (equal '(t ((x . (c is (the speed (of light) (in vacuum)))) ))
          (multiple-value-list (match4* '(a b (? x) d)
                                        '(a b (c is (the speed (of light) (in vacuum))) d))))

   (match4* '(* + & %) '(* + & %))
   (not (match4* '(? + & %) '(* + & %))) ; ? is no longer wildcard
   (match4* '(? + & %) '(? + & %))
   (not (match4* '(a b c d) '(a b (? x) d))) ; ? is not wildcard in subject

   (equal '(t ((x . (is (this (not (pung?)))) )))
          (multiple-value-list (match4* '(a (? x) c)
                                        '(a (is (this (not (pung?)))) c)))) ; 3 elts
   (equal '(t ((x . 1) (y . 3) (z . 5)))
          (multiple-value-list (match4* '((? x) (? y) (? z))
                                        '(1 3 5)))) ; Multiple single matches
   (equal '(t ((x . 1) (y . 3) (z . 5)))
          (multiple-value-list (match4* '(0 (? x) 2 (? y) 4 (? z) 6)
                                        '(0 1 2 3 4 5 6)))) ))
(defun match5 (p s)
  (labels ((capture-binding-on-successful-match (var val p s)
             (multiple-value-bind (match subs) (match5 p s)
               (if match
                   (values match (acons var val subs))
                   (values nil '()))) ))
    (let ((pattern p)
          (subject s))
      (loop (cond ((and (null pattern) (null subject)) (return (values t '())))
                  ((or (atom subject) (atom pattern)) (return (values nil '())))
                  (t (destructuring-bind (p . ps) pattern
                       (destructuring-bind (s . ss) subject
                         (cond ((equalp p s) (setf pattern ps subject ss))
                               ((atom p) (return (values nil '())))
                               (t (handler-case (destructuring-bind (operator var) p
                                                  (case operator
                                                    (? (return (capture-binding-on-successful-match var s ps ss)))
                                                    (otherwise (if (funcall operator s)
                                                                   (return (capture-binding-on-successful-match var s ps ss))
                                                                   (return (values nil '()))) )))
                                    (error () (return (values nil '()))) )))) )))) )))

(deftest test-match5 ()
  (check
   (match5 '() '())
   (match5 '(1d0 2d0 3d0) '(1 2 3))
   (not (match5 '(1d0 2d0 3d0) '(1 2 3 4)))
   (not (match5 '(1d0 2d0 3d0 4d0) '(1 2 3)))

   (not (match5 'a 'a))
   (not (match5 '(? x) 'a))
   (not (match5 '(a . b) '(a . b)))
   (not (match5 '((? x) . b) '(a . b)))

   (match5 '("pung" "foo" "bar") '("PUNG" "FOO" "BAR"))

   (match5 '((a)) '((a)))
   (match5 '(a (b (c d))) '(a (b (c d)))) ; 2 elts (No recursive descent)

   ;;    Malformed patterns
   (not (match5 '(a b () d) '(a b c d)))
   (not (match5 '(a b (z x) d) '(a b c d)))
   (not (match5 '(a b (! x) d) '(a b c d)))
   (not (match5 '(a b (? x m) d) '(a b c d)))

   ;;   Wildcard matches any top-level element
   (equal '(t ((x . c)))
          (multiple-value-list (match5 '(a b (? x) d)
                                        '(a b c d))))
   (equal '(t ((x . (c))))
          (multiple-value-list (match5 '(a b (? x) d)
                                        '(a b (c) d))))
   (equal '(t ((x . (c is (the speed (of light) (in vacuum)))) ))
          (multiple-value-list (match5 '(a b (? x) d)
                                        '(a b (c is (the speed (of light) (in vacuum))) d))))

   (match5 '(* + & %) '(* + & %))
   (not (match5 '(? + & %) '(* + & %))) ; ? is no longer wildcard
   (match5 '(? + & %) '(? + & %))
   (not (match5 '(a b c d) '(a b (? x) d))) ; ? is not wildcard in subject

   (equal '(t ((x . (is (this (not (pung?)))) )))
          (multiple-value-list (match5 '(a (? x) c)
                                        '(a (is (this (not (pung?)))) c)))) ; 3 elts
   (equal '(t ((x . 1) (y . 3) (z . 5)))
          (multiple-value-list (match5 '((? x) (? y) (? z))
                                        '(1 3 5)))) ; Multiple single matches
   (equal '(t ((x . 1) (y . 3) (z . 5)))
          (multiple-value-list (match5 '(0 (? x) 2 (? y) 4 (? z) 6)
                                       '(0 1 2 3 4 5 6))))
   ;;    Predicate
   (not (match5 '(a b (numberp x) d) '(a b c d)))
   (equal '(t ((x . 17)))
          (multiple-value-list (match5 '(a b (numberp x) d) '(a b 17 d))))
   (equal '(t ((z . 4) (y . 2) (x . 9) (p . 1.8d0)))
          (multiple-value-list (match5 '((plusp z) (evenp y) (numberp x) (floatp p)) '(4 2 9 1.8d0))))
   (not (match5 '((plusp z) (evenp y) (numberp x) (floatp p)) '(4 2 9 1)))
   (equal '(t ((z . 4) (y . 2) (x . 9) (p . 1)))
;          (multiple-value-list (match5 (list '(plusp z) '(evenp y) '(numberp x) (list #'(lambda (x) (< x 8)) 'p)) '(4 2 9 1))))
          (multiple-value-list (match5 (list '(plusp z) '(evenp y) '(numberp x) (list (partial* #'< 8) 'p)) '(4 2 9 1))))
   ;;    Equivalent of wildcard
   (equal '(t ((x . 9)))
          (multiple-value-list (match5 (list 4 2 (list (constantly t) 'x) 1) '(4 2 9 1)))) ))

(defun match6 (p s)
  (labels ((capture-binding-on-successful-match (var val p s)
             (multiple-value-bind (match subs) (match6 p s)
               (if match
                   (values match (acons var val subs))
                   (values nil '()))) )
           (pattern-operator (expr)
             (handler-case (destructuring-bind (operator var) expr
                             (values operator var))
               (error () (return-from match6 (values nil '()))) )))
    (let ((pattern p)
          (subject s))
      (loop (cond ((and (null pattern) (null subject)) (return (values t '())))
                  ((null pattern) (return (values nil '())))
                  ((null subject) (multiple-value-bind (operator var) (pattern-operator (first pattern))
                                    (case operator
                                      (* (return (capture-binding-on-successful-match var '() (rest pattern) subject)))
                                      (otherwise (return (values nil '()))) )))
                  ((or (atom subject) (atom pattern)) (return (values nil '())))
                  (t (destructuring-bind (p . ps) pattern
                       (destructuring-bind (s . ss) subject
                         (cond ((equalp p s) (setf pattern ps subject ss))
                               ((atom p) (return (values nil '())))
                               (t (multiple-value-bind (operator var) (pattern-operator p)
                                    (case operator
                                      ((nil) (return (values nil '())))
                                      (? (return (capture-binding-on-successful-match var s ps ss)))
                                      (* (multiple-value-bind (match subs) (match6 ps subject)
                                           (if match
                                               (return (values match (acons var '() subs)))
                                               (multiple-value-bind (match subs) (match6 pattern ss)
                                                 (if match
                                                     (destructuring-bind (entry . entries) subs
                                                       (assert (eq var (first entry)))
                                                       (return (values match (acons var (cons s (rest entry)) entries))))
                                                     (return (values nil '()))) ))))
                                      (otherwise (if (funcall operator s)
                                                     (return (capture-binding-on-successful-match var s ps ss))
                                                     (return (values nil '()))) )))) )))) )))) )

(deftest test-match6 ()
  (check
   (match6 '() '())

   (match6 '(a b c) '(a b c))
   (not (match6 '(a b c) '(a b c d)))
   (not (match6 '(a b c d) '(a b c)))

   (match6 '(1d0 2d0 3d0) '(1 2 3))
   (not (match6 '(1d0 2d0 3d0) '(1 2 3 4)))
   (not (match6 '(1d0 2d0 3d0 4d0) '(1 2 3)))

   (not (match6 'a 'a))
   (not (match6 '(? x) 'a))
   (not (match6 '(a . b) '(a . b)))
   (not (match6 '((? x) . b) '(a . b)))

   (match6 '("pung" "foo" "bar") '("PUNG" "FOO" "BAR"))

   (match6 '((a)) '((a)))
   (match6 '(a (b (c d))) '(a (b (c d)))) ; 2 elts (No recursive descent)

   ;;    Malformed patterns
   (not (match6 '(a b () d) '(a b c d)))
;   (not (match6 '(a b (z x) d) '(a b c d)))
;   (not (match6 '(a b (! x) d) '(a b c d)))
   (not (match6 '(a b (? x m) d) '(a b c d)))

   ;;   Wildcard matches any top-level element
   (equal '(t ((x . c)))
          (multiple-value-list (match6 '(a b (? x) d)
                                        '(a b c d))))
   (equal '(t ((x . (c))))
          (multiple-value-list (match6 '(a b (? x) d)
                                        '(a b (c) d))))
   (equal '(t ((x . (c is (the speed (of light) (in vacuum)))) ))
          (multiple-value-list (match6 '(a b (? x) d)
                                        '(a b (c is (the speed (of light) (in vacuum))) d))))

   (match6 '(* + & %) '(* + & %))
   (not (match6 '(? + & %) '(* + & %))) ; ? is no longer wildcard
   (match6 '(? + & %) '(? + & %))
   (not (match6 '(a b c d) '(a b (? x) d))) ; ? is not wildcard in subject

   (equal '(t ((x . (is (this (not (pung?)))) )))
          (multiple-value-list (match6 '(a (? x) c)
                                        '(a (is (this (not (pung?)))) c)))) ; 3 elts
   (equal '(t ((x . 1) (y . 3) (z . 5)))
          (multiple-value-list (match6 '((? x) (? y) (? z))
                                        '(1 3 5)))) ; Multiple single matches
   (equal '(t ((x . 1) (y . 3) (z . 5)))
          (multiple-value-list (match6 '(0 (? x) 2 (? y) 4 (? z) 6)
                                       '(0 1 2 3 4 5 6))))

   ;;    Wild sequence
   (equal '(t ((x . (b))))
          (multiple-value-list (match6 '(a (* x) c) '(a b c))))
   (equal '(t ((x . (b c))))
          (multiple-value-list (match6 '(a (* x)) '(a b c))))
   (equal '(t ((x . ())))
          (multiple-value-list (match6 '(a (* x)) '(a))))
   (equal '(t ((x . ())))
          (multiple-value-list (match6 '(a (* x) b) '(a b))))
   (equal '(t ((x . (b c d))))
          (multiple-value-list (match6 '(a (* x) b) '(a b c d b))))

   (equal '(t ((x . ())))
          (multiple-value-list (match6 '(a (* x) c d) '(a c d))))
   (equal '(t ((x . (c d))))
          (multiple-value-list (match6 '(a (* x) c d) '(a c d c d))))
   (equal '(t ((x . (c d c d))))
          (multiple-value-list (match6 '(a (* x) c d) '(a c d c d c d))))

   ;;    Ambiguous?
   (equal '(t ((x))) ; i.e., (x . nil)
          (multiple-value-list (match6 '(a (* x)) '(a))))
   (equal '(t ((x))) ; i.e., (x . nil)
          (multiple-value-list (match6 '(a (? x)) '(a nil))))

   ;;    No consistency check
   (equal '(t ((x . a) (x c d)))
          (multiple-value-list (match6 '((? x) b (* x)) '(a b c d))))

   ;;    Predicate
   (not (match6 '(a b (numberp x) d) '(a b c d)))
   (equal '(t ((x . 17)))
          (multiple-value-list (match6 '(a b (numberp x) d) '(a b 17 d))))
   (equal '(t ((z . 4) (y . 2) (x . 9) (p . 1.8d0)))
          (multiple-value-list (match6 '((plusp z) (evenp y) (numberp x) (floatp p)) '(4 2 9 1.8d0))))
   (not (match6 '((plusp z) (evenp y) (numberp x) (floatp p)) '(4 2 9 1)))
   (equal '(t ((z . 4) (y . 2) (x . 9) (p . 1)))
          (multiple-value-list (match6 (list '(plusp z) '(evenp y) '(numberp x) (list #'(lambda (x) (< x 8)) 'p)) '(4 2 9 1))))
   ;;    Equivalent of wildcard
   (equal '(t ((x . 9)))
          (multiple-value-list (match6 (list 4 2 (list (constantly t) 'x) 1) '(4 2 9 1))))

   (equal '(t ((x . (* specifies a)) (y . card) (z . (sequence element))))
          (multiple-value-list (match6 '((* x) wild (? y) (* z))
                                       '(* specifies a wild card sequence element))))

   (equal '(t ((x my crazy) (y likes to tell bad jokes) (z very annoying to me)))
          (multiple-value-list (match6 '(i do not like (* x) coach because he (* y) all of the time which is (* z))
                                       '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me)))) ))
