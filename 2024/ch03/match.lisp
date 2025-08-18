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
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :match (:use :common-lisp :core :test))

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
                    match)))) ; ???
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
  (first (rest (first p))))


;; (defun match4 (p s)
;;   (cond ((and (null p) (null s)) '((:yes . :yes)))
;;         ((or (atom p) (atom s)) nil) ; Fail if either P or S is an atom--including NIL (Both are not NIL at this point.)
;;         (t (destructuring-bind (p . ps) p
;;              (destructuring-bind (s . ss) s
;;                (cond ((equalp p s) (match4 ps ss))
;;                      ((atom p) nil)
;;                      (t (destructuring-bind (? (and (= (length (first p)) 2)
;;               (eq (first (first p)) '?)
;;               (let ((match (match4 (rest p) (rest s))))
;;                 (if match
;;                     (acons (first (rest (first p))) (first s) match)
;;                     match)))) ; ???
;;         (t nil)))

(deftest test-match4 ()
  (check
   (equal #1='((:yes . :yes)) (match4 '(1d0 2d0 3d0) '(1 2 3)))
   (not (match4 '(1d0 2d0 3d0) '(1 2 3 4)))
   (equal #1# (match4 '("pung" "foo" "bar") '("PUNG" "FOO" "BAR")))
   ;; Wildcard matches any top-level element
   (equal '((x . c) (:yes . :yes))
          (match4 '(a b (? x) d) '(a b c d)))
   (equal '((x c) (:yes . :yes))
          (match4 '(a b (? x) d) '(a b (c) d)))
   (equal '((x c is (the speed (of light) (in vacuum))) (:yes . :yes))
          (match4 '(a b (? x) d) '(a b (c is (the speed (of light) (in vacuum))) d)))

   (not (match4 '(a b c d) '(a b ? d))) ; ? is not wildcard in subject
   (equal #1# (match4 '(* + & %) '(* + & %)))
   (not (match4 '(* + & %) '(? + & %)))
   (equal #1# (match4 '(? + & %) '(? + & %))) ; ? is no longer wildcard
   (not (match4 'a 'a))
   (equal #1# (match4 '((a)) '((a))))
   (equal #1# (match4 '(a (b (c d))) '(a (b (c d))))) ; 2 elts (No recursive descent)
   (equal '((X IS (THIS (NOT (PUNG?)))) (:YES . :YES))
          (match4 '(a (? x) c) '(a (is (this (not (pung?)))) c)))) ) ; 3 elts


