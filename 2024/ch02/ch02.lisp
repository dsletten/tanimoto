;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   In Lisp there is always more than one way to solve a problem.
;;;;   -- David Touretzky
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Thu May  9 02:35:59 2024
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

(defpackage :ch02 (:use :common-lisp :core :test) (:shadow :setq :replace :stream))

(in-package :ch02)

(defun count-sublists (l)
  (cond ((null l) 0)
        ((atom l) 0)
        ((atom (first l)) (count-sublists (rest l)))
        (t (+ 1
              (count-sublists (first l))
              (count-sublists (rest l)))) ))

(deftest test-count-sublists ()
  (check
   (= 0 (count-sublists '(meat vegetables sweets)))
   (= 4 (count-sublists '((beef pork) (potato carrots) ((apple-pie cherry-pie) candy))))
   (= 3 (count-sublists '((a b) ((c d) e)))) ))

;;;
;;;    Tanimoto's MATCHKTH - 1-based index?!
;;;
(defun matchkth (elt lst k)
  (cond ((null lst) nil)
        ((< k 1) nil)
        ((and (= k 1) (equal (first lst) elt)) elt) ; Should be done here either way.
        (t (matchkth elt (rest lst) (1- k)))) )

(deftest test-matchkth ()
  (check
   (null (matchkth 'croissant '(bread roll croissant) 2))
   (equal 'croissant (matchkth 'croissant '(bread roll croissant) 3))))

;;;
;;;    Rewritten for 0-based index
;;;    
(defun matchjth (elt lst j)
  (cond ((null lst) nil)
        ((minusp j) nil)
        ((and (zerop j) (equal (first lst) elt)) elt)
        (t (matchjth elt (rest lst) (1- j)))) )

(deftest test-matchjth ()
  (check
   (null (matchjth 'croissant '(bread roll croissant) 1))
   (equal 'croissant (matchjth 'croissant '(bread roll croissant) 2))))

;;;
;;;    My version
;;;
(defun match-kth (obj l k)
  (cond ((null l) nil)
        ((zerop k) (and (equal (first l) obj) (first l)))
        (t (match-kth obj (rest l) (1- k)))) )

(deftest test-match-kth ()
  (check
   (null (match-kth 'croissant '(bread roll croissant) 1))
   (equal 'croissant (match-kth 'croissant '(bread roll croissant) 2))))

(defun match-kth (obj l k)
  (labels ((match (l k)
             (if (null l)
                 nil
                 (destructuring-bind (elt . more) l
                   (if (zerop k)
                       (and (equal elt obj) elt)
                       (match more (1- k)))) )))
    (if (minusp k)
        nil
        (match l k))))

;;;
;;;    46 页
;;;    
(let* ((x 5)
       (x² (* x x))
       (x⁴ (* x² x²))
       (fx ((lambda (x) (+ (* 3 (expt x 4)) (* 5 (expt x 2)) 7)) x)))
  (* fx 2))

;;;
;;;    §2.8.3 Not really sure what Tanimoto is after here...
;;;    This only works with special variables.
;;;
(defun misguided (arg1 arg2)
  (setf (symbol-value arg1) arg2))

(let ((x 99))
  (declare (special x))
  (let ((arg1 'x)
        (arg2 2))
    (misguided arg1 arg2))
  x)

;;;
;;;    §2.9.3 PROG equivalent
;;;
(loop for i from 0
      until (= i 10) 
      do (format t "~D~%" (* i i)) 
      finally (return i))

(do ((x 10 (1- x))
     (y 0 (1+ y)))
    ((zerop x) y)
  (print (* y y)))

;;;
;;;    Very similar to Tanimoto's PROG
;;;    
(macroexpand-1 '(do ((x 10 (1- x))
                     (y 0 (1+ y)))
                 ((zerop x) y)
                 (print (* y y))))
;;;
;;;    The macro expansion cannot be evaluated itself! The 2 occurrences
;;;    of the tag #:G490 are distinct uninterned symbols to the reader!
;;;    
;; (BLOCK NIL
;;   (LET ((X 10) (Y 0))
;;     (DECLARE (IGNORABLE X Y))
;;     (TAGBODY
;;       (GO #:G490)         <---------+
;;      #:G489                         |
;;       (TAGBODY (PRINT (* Y Y)))     |
;;       (PSETQ X (1- X)               |
;;              Y (1+ Y))              |
;;      #:G490               <---------+
;;       (UNLESS (ZEROP X) (GO #:G489))
;;       (RETURN-FROM NIL (PROGN Y)))))

(prog ((x 10)
       (y 0))
 head
   (print (* y y))
   (setf x (1- x))
   (setf y (1+ y))
   (cond ((zerop x) (return y))
         (t (go head))))

(macroexpand-1 '(prog ((x 10)
                       (y 0))
                 head
                 (print (* y y))
                 (setf x (1- x))
                 (setf y (1+ y))
                 (cond ((zerop x) (return y))
                       (t (go head)))))

(BLOCK NIL
  (LET ((X 10) (Y 0))
    (TAGBODY
     HEAD
      (PRINT (* Y Y))
      (SETF X (1- X))
      (SETF Y (1+ Y))
      (COND ((ZEROP X) (RETURN Y)) (T (GO HEAD))))))


(defun make-past (present)
  (mapcar #'(lambda (word)
              (cond ((eql word 'am) 'was)
                    ((eql word 'are) 'were)
                    ((eql word 'is) 'was)
                    (t word)))
          present))

(defun make-past (present)
  (mapcar #'(lambda (word)
              (case word
                ((am is) 'was)
                (are 'were)
                (otherwise word)))
          present))

;(make-past '(mt st helens is an active volcano)) => (MT ST HELENS WAS AN ACTIVE VOLCANO)
 
;;;
;;;    §2.13.1
;;;
(defmacro setq (a e)
  `(setf ,a ,e))

;;;
;;;    D'oh!
;;;    CLHS says: (set symbol value) ==  (setf (symbol-value symbol) value)
;;;    More importantly: set cannot change the value of a lexical variable.
;;;    
(defmacro setq2 (a e) ; No way to do this for lexical variables??
  `(set ,a ',e))

;;;
;;;    This doesn't work!
;;;
#|
  (SETQ2 X (FIRST Y))
Compile-time error:
  during macroexpansion of (SETQ2 X (FIRST Y)). Use *BREAK-ON-SIGNALS* to
intercept.

 The variable X is unbound.
 It is a local variable not available at compile-time.
|#
;; (defmacro setq2 (a e)
;;   `(setf ,(eval a) ',e))

;(macroexpand-1 '(setq2 x (first y))) => (SET X '(FIRST Y))

(let (x y)
  (setq x 'y)
  (setq2 x (first y)) ; No dynamic Y!!
  y)
;=> NIL

;;;
;;;    Ex. 4
;;;
(defun exify (expr)
  (cond ((null expr) nil)
        ((atom expr) 'x)
        (t (cons (exify (car expr))
                 (exify (cdr expr)))) ))

(deftest test-exify ()
  (check
   (equal '(x (x x) x x nil x) (exify '(a (b c) x y nil z)))) )

;;;
;;;    Ex. 5
;;;
(defun replace (s1 s2 s3)
  (cond ((null s1) '())
        ((equalp s1 s2) s3)
        ((atom s1) s1)
        (t (cons (replace (car s1) s2 s3)
                 (replace (cdr s1) s2 s3)))) )

(deftest test-replace ()
  (check
   (equal '((that one) contains (2 occurrences (that one)))
          (replace '((this 1) contains (2 occurrences (this 1)))
                   '(this 1)
                   '(that one)))) )

;;;
;;;    Ex. 6
;;;
(defun print-cubes-1 (n)
  (labels ((process (i)
             (unless (> i n)
               (format t "~D~%" (expt i 3))
               (process (1+ i)))) )
    (process 1)))

(defun print-cubes-2 (n)
  (do ((i 1 (1+ i)))
      ((> i n))
    (format t "~D~%" (expt i 3))))

(defun print-cubes-3 (n)
  (dotimes (i n)
    (format t "~D~%" (expt (1+ i) 3))))

(defun print-cubes-4 (n)
  (format t "~{~D~%~}" (loop for i from 1 to n collect (expt i 3))))

(defclass stream ()
  ((generator :initform (error "No generator provided") :initarg :generator)))

(defun read-stream (stream)
  (with-slots (generator) stream
    (funcall generator)))

(defmacro generator ((i init) &body body)
  `(make-instance 'stream
                  :generator (let ((,i ,init))
                               #'(lambda ()
                                   ,@body))))

(defun print-cubes-5 (n)
  (loop repeat n
        with cubes-stream = (generator (i 1)
                              (prog1 (* i i i)
                                (incf i)))
        do (format t "~D~%" (read-stream cubes-stream))))

;;;
;;;    Ex. 7
;;;    见 ~/lisp/programs/primes.lisp
;;;
;; (defun primep (n)
;;   (cond ((< n 2) nil)
;;         ((= n 2) t)
;;         ((evenp n) nil)
;;         (t (loop for i from 3 upto (isqrt n) by 2
;;                  when (zerop (mod n i)) do (return nil)
;;                  finally (return t)))) )

(defun primep (n)
  (cond ((< n 2) nil)
        ((= n 2) t)
        ((evenp n) nil)
        (t (loop for factor from 3 upto (isqrt n) by 2
                 never (zerop (mod n factor)))) ))

;;;
;;;    Ex. 8
;;;
;; (defvar *x*)
;; (setf *x* '(eval '(setf *x* 'eval)))
;; *x* => (EVAL '(SETF *X* 'EVAL))
;; (setf *x* (eval '(setf *x* 'eval)))
;; *x* => EVAL

;;;
;;;    Ex. 9
;;;
(defun sum (l)
  (apply #'+ l))

(let ((ilist '(3 7 11 13)))
  (sum ilist))

;;;
;;;    Ex. 10
;;;
(defun palindromep (l)
  (equal l (reverse l)))

(deftest test-palindromep ()
  (check
   (palindromep '(r o t a t o r))
   (palindromep '(n u r s e s r u n))))

;;;
;;;    Ex. 11
;;;
(mapcar #'reverse '((a b) (c (d e)) (f g)))

;;;
;;;    Ex. 12
;;;
(defun doubles (l)
  (mapcar #'(lambda (elt) (list elt elt)) l))

(deftest test-doubles ()
  (check
   (equal '((X X) (Y Y) ((Z W) (Z W)))
          (doubles '(x y (z w)))) ))

;;;
;;;    Ex. 13
;;;
(defun equalelts (l)
  (if (null l)
      t
      (destructuring-bind (x . xs) l
        (if (null xs)
            t
            (destructuring-bind (y . ys) xs
              (if (null ys)
                  (equal x y)
                  (equalelts xs)))) )))

(defun equalelts (l)
  (cond ((null l) t)
        ((singlep l) t)
        (t (and (equal (first l) (second l))
                (equalelts (rest l)))) ))

(defun equalelts (l)
  (or (null l)
      (singlep l)
      (and (equal (first l) (second l))
           (equalelts (rest l)))) )

(defun equalelts (l &key (test #'equal))
  (or (null l)
      (destructuring-bind (x . more) l
        (every #'(lambda (elt) (funcall test elt x)) more))))

(deftest test-equalelts ()
  (check
   (equalelts '())
   (equalelts '(a))
   (equalelts '(a a))
   (equalelts '(a a a))
   (not (equalelts '(a b)))
   (not (equalelts '(a a b)))
   (not (equalelts '(a a a b)))
   (not (equalelts '(a b c d)))
   (equalelts '((a b) (a b) (a b)))
   (equalelts '("foo" "foo" "foo" "foo"))
   (not (equalelts '("foo" "foo" "Foo" "foo")))
   (equalelts '("foo" "foo" "Foo" "foo") :test #'string-equal)))

;;;
;;;    Ex. 14
;;;    
(defun depth (tree)
  "Parentheses depth"
  (cond ((atom tree) 0)
        (t (max (1+ (depth (car tree)))
                (depth (cdr tree)))) ))

(deftest test-depth ()
  (check
   (= 0 (depth :a))
   (= 1 (depth '(:a)))
   (= 1 (depth '(:a :b :c)))
   (= 1 (depth '(:a :b () :c)))
   (= 2 (depth '((:a))))
   (= 2 (depth '((:a) :b)))
   (= 2 (depth '(:b (:a))))
   (= 3 (depth '(((:a) :b) :c ((:d) :e))))
   (= 3 (depth '(:a (:b) (:c (:d)))) )
   (= 4 (depth '(:a :b (:c (:d (:e)))) ))))

;;;
;;;    Tanimoto's definition specifies this!
;;;    
(defun tree-depth (obj)
  (cond ((atom obj) 0)
	(t (1+ (max (tree-depth (car obj))
		    (tree-depth (cdr obj)))) )))

(deftest test-tree-depth ()
  (check
   (= 0 (tree-depth '()))
   (= 1 (tree-depth '(:a)))
   (= 2 (tree-depth '(:a :b)))
   (= 3 (tree-depth '(:a :b :c)))
   (= 4 (tree-depth '(:a :b () :c)))
   (= 5 (tree-depth '(((:a) :b) :c ((:d) :e))))
   (= 6 (tree-depth '(:a (:b) (:c (:d)))) )
   (= 7 (tree-depth '(:a (:b (:c (:d)))) ))
   (= 3 (tree-depth '((1) :a :b)))
   (= 3 (tree-depth '(:c (2) :d)))
   (= 4 (tree-depth '(:e :f (3)))) ))

(defun quasi-balanced-p (l)
  (and (equalelts (mapcar #'length l))
       (equalelts (mapcar #'tree-depth l))))

(deftest test-quasi-balanced-p ()
  (check
   (quasi-balanced-p '((a b) (c d) (e f)))
   (not (quasi-balanced-p '(() (a b) (c d e))))
   (quasi-balanced-p '(((1) :a :b) (:c (2) :d)))
   (not (quasi-balanced-p '(((1) :a :b) (:c (2) :d) (:e :f (3)))) )
   (quasi-balanced-p '(((1) :a :b) (:c (2) :d) (:e :f 3)))) )

;;;
;;;    Ex. 15
;;;
(defun treemax (tree)
  (if (atom tree)
      tree
      (destructuring-bind (left right) tree
        (max (treemin left)
             (treemin right)))) )

(defun treemin (tree)
  (if (atom tree)
      tree
      (destructuring-bind (left right) tree
        (min (treemax left)
             (treemax right)))) )
