;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               match.lisp
;;;;
;;;;   Started:            Mon Aug 11 12:32:52 2025
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;    This file traces the evolution of a matching function that allows
;;;;    flexibly comparing a pattern P with a subject S. Beginning with
;;;;    MATCH1, which is simply EQUALP, the function evolves over various
;;;;    iterations ultimately to MATCH6, which supports various wildcard
;;;;    matches as well as the application of predicate functions in the match.
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
;;;;   Notes:
;;;;   Other examples of matching functions
;;;;   PAIP ch. 5, 6 Optimization ch. 10
;;;;   SICP ch. 4
;;;;   On Lisp ch. 18 Non-binding wildcard _
;;;;   Prog. for AI (Kreutzer/McKenzie) §3.3, Appendix III 534 页 Patterns Toolbox
;;;;   Slade ch. 4, 7
;;;;   Wilensky
;;;;   Programming Paradigms in Lisp
;;;;
;;;;    Ambiguity?
;;;;    (match '(a (* x)) '(a)) => (x), i.e., (x . nil)
;;;;    (match '(a (? x)) '(a nil)) => (x)
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :match
  (:use :common-lisp :core :io :test)
  (:export :match :? :*)
  (:shadow :*))

(in-package :match)

;;;
;;;    EQUALP is too strict for interesting matches.
;;;    
(defun match1 (p s)
  (equalp p s))

;;;
;;;    Compares tree isomorphism
;;;    
(defun match2 (p s)
  (cond ((atom p) (atom s))
        ((atom s) nil)
        ((match2 (first p) (first s)) (match2 (rest p) (rest s)))
        (t nil)))

(deftest test-match2 ()
  (check
   (match2 '(a (b) c) '(x (y) nil))
   (not (match2 '(a (b) c) '(a b c)))
   (match2 8 "Sure, why not?"))) ; !

;;;
;;;    PAIP pg. 76
;;;    
(defun same-shape-tree-p (a b)
  "Do trees A and B have the same structure even if values are different?"
  (tree-equal a b :test (constantly t)))

(deftest test-same-shape-tree-p ()
  (check
   (same-shape-tree-p '(a (b) c) '(x (y) nil))
   (not (same-shape-tree-p '(a (b) c) '(a b c)))
   (match2 8 "Sure, why not?"))) ; !

;;;
;;;    Allow single element wildcard matches. Only examine top-level structure.
;;;    
(defun match3 (p s)
  (cond ((null p) (null s))
        ((or (atom p) (atom s)) nil)
        ((equalp (first p) (first s)) (match3 (rest p) (rest s)))
        ((eq (first p) '?) (match3 (rest p) (rest s)))
        (t nil)))

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

;;;
;;;    Match single element and capture corresponding value.
;;;    
(defun match4 (p s)
  (cond ((and (null p) (null s)) '((:yes . :yes)))
        ((or (atom p) (atom s)) nil) ; Fail if either P or S is an atom--including NIL (Both are not NIL at this point.)
        ((equalp (first p) (first s)) (match4 (rest p) (rest s)))
        ((atom (first p)) nil) ; Missing from book's definition!
        ((and (= (length (first p)) 2)
              (eq (first (first p)) '?)
              (let ((match (match4 (rest p) (rest s))))
                (if match
                    (acons (first (rest (first p))) (first s) match)
                    nil))))
        (t nil)))

;;;
;;;    Tanimoto's original
;;;    Broken for failed match:
;;;    (match4 '(a b d) '(a b c))
;;;    4th clause evaluated: (length 'd) => Error
;;;    
;; (defun match4 (p s)
;;   (cond ((and (null p) (null s)) '((:yes . :yes)))
;;         ((or (atom p) (atom s)) nil)
;;         ((equalp (first p) (first s)) (match4 (rest p) (rest s)))
;;         ((and (equalp (length (first p)) 2)
;;               (eql (first (first p)) '?)
;;               (let ((match (match4 (rest p) (rest s))))
;;                 (if match
;;                     (acons (first (rest (first p))) (first s) match)))) ) ; No consequent for this COND clause.
;;         (t nil)))

;; (match4 '(a b d) '(a b c))

;; debugger invoked on a TYPE-ERROR @B800013FBA in thread
;; #<THREAD tid=1580178 "main thread" RUNNING {1000C18133}>:
;;   The value
;;     D
;;   is not of type
;;     SEQUENCE

;; Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

;; restarts (invokable by number or by possibly-abbreviated name):
;;   0: [ABORT] Exit debugger, returning to top level.

;; (LENGTH D)
;; 0]

(defun 1st-pattern-op (p)
  (first (first p)))

(defun 1st-pattern-variable (p)
;  (first (rest (first p))))
  (second (first p)))

;;;
;;;    With helper functions
;;;    
(defun match4 (p s)
  (cond ((and (null p) (null s)) '((:yes . :yes)))
        ((or (atom p) (atom s)) nil)
        ((equalp (first p) (first s)) (match4 (rest p) (rest s)))
        ((atom (first p)) nil)
        ((and (= (length (first p)) 2)
              (eq (1st-pattern-op p) '?)
              (let ((match (match4 (rest p) (rest s))))
                (if match
                    (acons (1st-pattern-variable p) (first s) match)
                    nil))))
        (t nil)))

(defun match4 (p s)
  (cond ((and (null p) (null s)) '((:yes . :yes)))
        ((or (atom p) (atom s)) nil) ; Fail if either P or S is an atom--including NIL (Both are not NIL at this point.)
        (t (destructuring-bind (pattern . ps) p
             (destructuring-bind (subject . ss) s
               (cond ((equalp pattern subject) (match4 ps ss))
                     ((atom pattern) nil)
                     (t (destructuring-bind (operator &optional var &rest _) pattern
                          (cond ((null var) nil)
                                ((not (null _)) nil)
                                (t (case operator
                                     (? (let ((match (match4 ps ss)))
                                          (and match (acons var subject match))))
                                     (otherwise nil)))) )))) ))))

(defun match4 (p s)
  (cond ((and (null p) (null s)) '((:yes . :yes)))
        ((or (atom p) (atom s)) nil) ; Fail if either P or S is an atom--including NIL (Both are not NIL at this point.)
        (t (destructuring-bind (pattern . ps) p
             (destructuring-bind (subject . ss) s
               (cond ((equalp pattern subject) (match4 ps ss))
                     ((atom pattern) nil)
                     (t (handler-case (destructuring-bind (operator var) pattern
                                        (ecase operator
                                          (? (let ((match (match4 ps ss)))
                                               (and match (acons var subject match)))) ))
                          (error () nil)))) )))) )

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

;;;
;;;    Alternative to Tanimoto, using multiple values
;;;    
(defun match4* (p s)
  (cond ((and (null p) (null s)) (values t '()))
        ((or (atom p) (atom s)) (values nil '())) ; Fail if either P or S is an atom--including NIL (Both are not NIL at this point.)
        (t (destructuring-bind (pattern . ps) p
             (destructuring-bind (subject . ss) s
               (cond ((equalp pattern subject) (match4* ps ss))
                     ((atom pattern) (values nil '()))
                     (t (handler-case (destructuring-bind (operator var) pattern
                                        (ecase operator
                                          (? (multiple-value-bind (match subs) (match4* ps ss)
                                               (if match
                                                   (values match (acons var subject subs))
                                                   (values nil '()))) )))
                          (error () (values nil '()))) )))) )))

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
               (values match (if match (acons var val subs) subs)))) )
    (cond ((and (null p) (null s)) (values t '()))
          ((or (atom p) (atom s)) (values nil '())) ; Fail if either P or S is an atom--including NIL (Both are not NIL at this point.)
          (t (destructuring-bind (pattern . ps) p
               (destructuring-bind (subject . ss) s
                 (cond ((equalp pattern subject) (match5 ps ss))
                       ((atom pattern) (values nil '()))
                       (t (handler-case (destructuring-bind (operator var) pattern
                                          (case operator
                                            (? (capture-binding-on-successful-match var subject ps ss))
                                            (otherwise (if (funcall operator subject)
                                                           (capture-binding-on-successful-match var subject ps ss)
                                                           (values nil '()))) ))
                            (error () (values nil '()))) )))) ))))

(defun match5 (p s)
  (labels ((capture-binding-on-successful-match (var val p s)
             (multiple-value-bind (match subs) (match5 p s)
               (if match
                   (values match (acons var val subs))
                   (values nil '()))) ))
    (cond ((and (null p) (null s)) (values t '()))
          ((or (atom p) (atom s)) (values nil '())) ; Fail if either P or S is an atom--including NIL (Both are not NIL at this point.)
          (t (destructuring-bind (pattern . ps) p
               (destructuring-bind (subject . ss) s
                 (cond ((equalp pattern subject) (match5 ps ss))
                       ((atom pattern) (values nil '()))
                       (t (handler-case (destructuring-bind (operator var) pattern
                                          (case operator
                                            (? (capture-binding-on-successful-match var subject ps ss))
                                            (otherwise (if (funcall operator subject)
                                                           (capture-binding-on-successful-match var subject ps ss)
                                                           (values nil '()))) ))
                            (error () (values nil '()))) )))) ))))

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
          (multiple-value-list (match5 (list '(plusp z) '(evenp y) '(numberp x) (list #'(lambda (x) (< x 8)) 'p)) '(4 2 9 1))))
   ;;    Equivalent of wildcard
   (equal '(t ((x . 9)))
          (multiple-value-list (match5 (list 4 2 (list (constantly t) 'x) 1) '(4 2 9 1)))) ))

;;;
;;;    Tanimoto examines 3 cases WRT to "wild sequences":
;;;    1. (match (rest p) (rest s)) ; Match one elt (same as (? x))
;;;    2. (match (rest p) s) ; Match no elts (ignore wildcard)
;;;    3. (match p (rest s)) ; Match multiple elts (consume one elt, continue wild sequence match)
;;;    He doesn't need any special handling for a case such as (match '((* x)) '()) since he doesn't destructure.
;;;
;;;    I have to handle that case before I try to destructure S. Consequently, later I only need to consider
;;;    his 2nd and 3rd cases.
;;;    
(defun match6 (p s)
  (labels ((capture-binding-on-successful-match (var val p s)
             (multiple-value-bind (match subs) (match6 p s)
               (if match
                   (values match (acons var val subs))
                   (values nil '()))) )
           (pattern-operator (expr)
             (handler-case (destructuring-bind (operator var) expr
                             (values operator var))
               (error () (values nil nil)))) )
    (cond ((and (null p) (null s)) (values t '()))
          ((null p) (values nil '()))
          ((null s) (multiple-value-bind (operator var) (pattern-operator (first p))
                      (case operator
                        (* (multiple-value-bind (match subs) (match6 (rest p) s)
                             (if match
                                 (values match (acons var '() subs))
                                 (values nil '()))) )
                        (otherwise (values nil '()))) ))
          ((or (atom p) (atom s)) (values nil '()))
          (t (destructuring-bind (pattern . ps) p
               (destructuring-bind (subject . ss) s
                 (cond ((equalp pattern subject) (match6 ps ss)) ; Supersedes wildcard match?! (match '((? x) b c) '((? x) b c))
                       ((atom pattern) (values nil '())) ; No literal match => must be wildcard (not atom)
                       (t (multiple-value-bind (operator var) (pattern-operator pattern)
                            (case operator
                              ((nil) (values nil '())) ; Malformed
                              (? (capture-binding-on-successful-match var subject ps ss))
                              (* (multiple-value-bind (match subs) (match6 ps s) ; Order important here?
                                   (if match
                                       (values match (acons var '() subs))
                                       (multiple-value-bind (match subs) (match6 p ss)
                                         (if match
                                             (destructuring-bind (entry . entries) subs
                                               (assert (eq var (first entry)))
                                               (values match (acons var (cons subject (rest entry)) entries)))
                                             (values nil '()))) )))
                              (otherwise (if (funcall operator subject)
                                             (capture-binding-on-successful-match var subject ps ss)
                                             (values nil '()))) )))) )))) ))

(setf (symbol-function 'match) #'match6)

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

#|
Tanimoto's MATCH (AKA MATCH6) fails many of the tests passed by MATCH5
- Strictly speaking, all of the EQUAL assertions below fail, but that is simply due
  to the difference in our implementations. Mine returns multiple values. His version
  returns a correct result for each test case even though the exact form is different.

(deftest test-match ()
  (check
   (match '() '())
       (match '(1d0 2d0 3d0) '(1 2 3)) ; EQL test used to be EQUALP
   (not (match '(1d0 2d0 3d0) '(1 2 3 4)))
   (not (match '(1d0 2d0 3d0 4d0) '(1 2 3)))

       (not (match 'a 'a)) ; These are all errors! ;;;;;;;;;;;
       (not (match '(? x) 'a))
       (not (match '(a . b) '(a . b)))
       (not (match '((? x) . b) '(a . b))) ;;;;;;;;;

       (match '("pung" "foo" "bar") '("PUNG" "FOO" "BAR")) ; EQL test used to be EQUALP

       (match '((a)) '((a))) ; Error!
       (match '(a (b (c d))) '(a (b (c d)))) ; Error!

   ;;    Malformed patterns
   (not (match '(a b () d) '(a b c d)))
       (not (match '(a b (z x) d) '(a b c d))) ; Error!
       (not (match '(a b (! x) d) '(a b c d))) ; Error!
       (not (match '(a b (? x m) d) '(a b c d))) ; Match succeeds. Malformed pattern not detected.

   ;;   Wildcard matches any top-level element
   (equal '(t ((x . c)))
          (multiple-value-list (match '(a b (? x) d)
                                        '(a b c d))))
   (equal '(t ((x . (c))))
          (multiple-value-list (match '(a b (? x) d)
                                        '(a b (c) d))))
   (equal '(t ((x . (c is (the speed (of light) (in vacuum)))) ))
          (multiple-value-list (match '(a b (? x) d)
                                        '(a b (c is (the speed (of light) (in vacuum))) d))))

   (match '(* + & %) '(* + & %))
   (not (match '(? + & %) '(* + & %))) ; ? is no longer wildcard
   (match '(? + & %) '(? + & %))
   (not (match '(a b c d) '(a b (? x) d))) ; ? is not wildcard in subject

   (equal '(t ((x . (is (this (not (pung?)))) )))
          (multiple-value-list (match '(a (? x) c)
                                        '(a (is (this (not (pung?)))) c)))) ; 3 elts
   (equal '(t ((x . 1) (y . 3) (z . 5)))
          (multiple-value-list (match '((? x) (? y) (? z))
                                        '(1 3 5)))) ; Multiple single matches
   (equal '(t ((x . 1) (y . 3) (z . 5)))
          (multiple-value-list (match '(0 (? x) 2 (? y) 4 (? z) 6)
                                       '(0 1 2 3 4 5 6))))
   ;;    Predicate
   (not (match '(a b (numberp x) d) '(a b c d)))
   (equal '(t ((x . 17)))
          (multiple-value-list (match '(a b (numberp x) d) '(a b 17 d))))
   (equal '(t ((z . 4) (y . 2) (x . 9) (p . 1.8d0)))
          (multiple-value-list (match '((plusp z) (evenp y) (numberp x) (floatp p)) '(4 2 9 1.8d0))))
   (not (match '((plusp z) (evenp y) (numberp x) (floatp p)) '(4 2 9 1)))
   (equal '(t ((z . 4) (y . 2) (x . 9) (p . 1)))
          (multiple-value-list (match (list '(plusp z) '(evenp y) '(numberp x) (list #'(lambda (x) (< x 8)) 'p)) '(4 2 9 1))))
   ;;    Equivalent of wildcard
   (equal '(t ((x . 9)))
          (multiple-value-list (match (list 4 2 (list (constantly t) 'x) 1) '(4 2 9 1)))) ))
|#
