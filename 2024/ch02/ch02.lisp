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
              (and (equal x y)
                   (or (null ys)
                       (equalelts xs)))) ))))

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
   (not (equalelts '(a b b)))
   (not (equalelts '(a a b b)))
   (not (equalelts '(a c d b b)))
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

(defun tree-depth* (obj)
  (cond ((atom obj) 0)
	(t (1+ (apply #'max (mapcar #'tree-depth* obj)))) ))

(deftest test-tree-depth* ()
  (check
   (= 0 (tree-depth* 'a))
   (= 0 (tree-depth* '()))
   (= 1 (tree-depth* '(:a)))
   (= 1 (tree-depth* '(:a :b)))
   (= 1 (tree-depth* '(:a :b :c)))
   (= 1 (tree-depth* '(:a :b () :c)))
   (= 3 (tree-depth* '(((:a) :b) :c ((:d) :e))))
   (= 3 (tree-depth* '(:a (:b) (:c (:d)))) )
   (= 4 (tree-depth* '(:a (:b (:c (:d)))) ))
   (= 2 (tree-depth* '((1) :a :b)))
   (= 2 (tree-depth* '(:c (2) :d)))
   (= 2 (tree-depth* '(:e :f (3)))) ))

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

;'(((1 . 2) . (3 . 4)) . ((5 . (6 . 7)) . 8)) => (((1 . 2) 3 . 4) (5 6 . 7) . 8)
;'(1 . (8 . (2 . (7 . (3 . (6 . (4 . 5))))))) => (1 8 2 7 3 6 4 . 5)

;;;
;;;    Modified functions: (destructuring-bind (left . right) tree
;;;    
;; (treemax '((3 . (2 . 5)) . (7 . (2 . 1))))
;;   0: (CH02::TREEMAX ((3 2 . 5) 7 2 . 1))
;;     1: (CH02::TREEMIN (3 2 . 5))
;;       2: (CH02::TREEMAX 3)
;;       2: TREEMAX returned 3
;;       2: (CH02::TREEMAX (2 . 5))
;;         3: (CH02::TREEMIN 2)
;;         3: TREEMIN returned 2
;;         3: (CH02::TREEMIN 5)
;;         3: TREEMIN returned 5
;;       2: TREEMAX returned 5
;;     1: TREEMIN returned 3
;;     1: (CH02::TREEMIN (7 2 . 1))
;;       2: (CH02::TREEMAX 7)
;;       2: TREEMAX returned 7
;;       2: (CH02::TREEMAX (2 . 1))
;;         3: (CH02::TREEMIN 2)
;;         3: TREEMIN returned 2
;;         3: (CH02::TREEMIN 1)
;;         3: TREEMIN returned 1
;;       2: TREEMAX returned 2
;;     1: TREEMIN returned 2
;;   0: TREEMAX returned 3
;; 3

;; (treemin '((3 . (2 . 5)) . (7 . (2 . 1))))
;;   0: (CH02::TREEMIN ((3 2 . 5) 7 2 . 1))
;;     1: (CH02::TREEMAX (3 2 . 5))
;;       2: (CH02::TREEMIN 3)
;;       2: TREEMIN returned 3
;;       2: (CH02::TREEMIN (2 . 5))
;;         3: (CH02::TREEMAX 2)
;;         3: TREEMAX returned 2
;;         3: (CH02::TREEMAX 5)
;;         3: TREEMAX returned 5
;;       2: TREEMIN returned 2
;;     1: TREEMAX returned 3
;;     1: (CH02::TREEMAX (7 2 . 1))
;;       2: (CH02::TREEMIN 7)
;;       2: TREEMIN returned 7
;;       2: (CH02::TREEMIN (2 . 1))
;;         3: (CH02::TREEMAX 2)
;;         3: TREEMAX returned 2
;;         3: (CH02::TREEMAX 1)
;;         3: TREEMAX returned 1
;;       2: TREEMIN returned 1
;;     1: TREEMAX returned 7
;;   0: TREEMIN returned 3
;; 3


;; (treemax '(1 . (8 . (2 . (7 . (3 . (6 . (4 . 5))))))))
;;   0: (CH02::TREEMAX (1 8 2 7 3 6 4 . 5))
;;     1: (CH02::TREEMIN 1)
;;     1: TREEMIN returned 1
;;     1: (CH02::TREEMIN (8 2 7 3 6 4 . 5))
;;       2: (CH02::TREEMAX 8)
;;       2: TREEMAX returned 8
;;       2: (CH02::TREEMAX (2 7 3 6 4 . 5))
;;         3: (CH02::TREEMIN 2)
;;         3: TREEMIN returned 2
;;         3: (CH02::TREEMIN (7 3 6 4 . 5))
;;           4: (CH02::TREEMAX 7)
;;           4: TREEMAX returned 7
;;           4: (CH02::TREEMAX (3 6 4 . 5))
;;             5: (CH02::TREEMIN 3)
;;             5: TREEMIN returned 3
;;             5: (CH02::TREEMIN (6 4 . 5))
;;               6: (CH02::TREEMAX 6)
;;               6: TREEMAX returned 6
;;               6: (CH02::TREEMAX (4 . 5))
;;                 7: (CH02::TREEMIN 4)
;;                 7: TREEMIN returned 4
;;                 7: (CH02::TREEMIN 5)
;;                 7: TREEMIN returned 5
;;               6: TREEMAX returned 5
;;             5: TREEMIN returned 5
;;           4: TREEMAX returned 5
;;         3: TREEMIN returned 5
;;       2: TREEMAX returned 5
;;     1: TREEMIN returned 5
;;   0: TREEMAX returned 5
;; 5

;; (treemin '(1 . (8 . (2 . (7 . (3 . (6 . (4 . 5))))))))
;;   0: (CH02::TREEMIN (1 8 2 7 3 6 4 . 5))
;;     1: (CH02::TREEMAX 1)
;;     1: TREEMAX returned 1
;;     1: (CH02::TREEMAX (8 2 7 3 6 4 . 5))
;;       2: (CH02::TREEMIN 8)
;;       2: TREEMIN returned 8
;;       2: (CH02::TREEMIN (2 7 3 6 4 . 5))
;;         3: (CH02::TREEMAX 2)
;;         3: TREEMAX returned 2
;;         3: (CH02::TREEMAX (7 3 6 4 . 5))
;;           4: (CH02::TREEMIN 7)
;;           4: TREEMIN returned 7
;;           4: (CH02::TREEMIN (3 6 4 . 5))
;;             5: (CH02::TREEMAX 3)
;;             5: TREEMAX returned 3
;;             5: (CH02::TREEMAX (6 4 . 5))
;;               6: (CH02::TREEMIN 6)
;;               6: TREEMIN returned 6
;;               6: (CH02::TREEMIN (4 . 5))
;;                 7: (CH02::TREEMAX 4)
;;                 7: TREEMAX returned 4
;;                 7: (CH02::TREEMAX 5)
;;                 7: TREEMAX returned 5
;;               6: TREEMIN returned 4
;;             5: TREEMAX returned 6
;;           4: TREEMIN returned 3
;;         3: TREEMAX returned 7
;;       2: TREEMIN returned 2
;;     1: TREEMAX returned 8
;;   0: TREEMIN returned 1
;; 1


;; (treemax '(((1 . 2) . (3 . 4)) . ((5 . (6 . 7)) . 8)))
;;   0: (CH02::TREEMAX (((1 . 2) 3 . 4) (5 6 . 7) . 8))
;;     1: (CH02::TREEMIN ((1 . 2) 3 . 4))
;;       2: (CH02::TREEMAX (1 . 2))
;;         3: (CH02::TREEMIN 1)
;;         3: TREEMIN returned 1
;;         3: (CH02::TREEMIN 2)
;;         3: TREEMIN returned 2
;;       2: TREEMAX returned 2
;;       2: (CH02::TREEMAX (3 . 4))
;;         3: (CH02::TREEMIN 3)
;;         3: TREEMIN returned 3
;;         3: (CH02::TREEMIN 4)
;;         3: TREEMIN returned 4
;;       2: TREEMAX returned 4
;;     1: TREEMIN returned 2
;;     1: (CH02::TREEMIN ((5 6 . 7) . 8))
;;       2: (CH02::TREEMAX (5 6 . 7))
;;         3: (CH02::TREEMIN 5)
;;         3: TREEMIN returned 5
;;         3: (CH02::TREEMIN (6 . 7))
;;           4: (CH02::TREEMAX 6)
;;           4: TREEMAX returned 6
;;           4: (CH02::TREEMAX 7)
;;           4: TREEMAX returned 7
;;         3: TREEMIN returned 6
;;       2: TREEMAX returned 6
;;       2: (CH02::TREEMAX 8)
;;       2: TREEMAX returned 8
;;     1: TREEMIN returned 6
;;   0: TREEMAX returned 6
;; 6

;; (treemin '(((1 . 2) . (3 . 4)) . ((5 . (6 . 7)) . 8)))
;;   0: (CH02::TREEMIN (((1 . 2) 3 . 4) (5 6 . 7) . 8))
;;     1: (CH02::TREEMAX ((1 . 2) 3 . 4))
;;       2: (CH02::TREEMIN (1 . 2))
;;         3: (CH02::TREEMAX 1)
;;         3: TREEMAX returned 1
;;         3: (CH02::TREEMAX 2)
;;         3: TREEMAX returned 2
;;       2: TREEMIN returned 1
;;       2: (CH02::TREEMIN (3 . 4))
;;         3: (CH02::TREEMAX 3)
;;         3: TREEMAX returned 3
;;         3: (CH02::TREEMAX 4)
;;         3: TREEMAX returned 4
;;       2: TREEMIN returned 3
;;     1: TREEMAX returned 3
;;     1: (CH02::TREEMAX ((5 6 . 7) . 8))
;;       2: (CH02::TREEMIN (5 6 . 7))
;;         3: (CH02::TREEMAX 5)
;;         3: TREEMAX returned 5
;;         3: (CH02::TREEMAX (6 . 7))
;;           4: (CH02::TREEMIN 6)
;;           4: TREEMIN returned 6
;;           4: (CH02::TREEMIN 7)
;;           4: TREEMIN returned 7
;;         3: TREEMAX returned 7
;;       2: TREEMIN returned 5
;;       2: (CH02::TREEMIN 8)
;;       2: TREEMIN returned 8
;;     1: TREEMAX returned 8
;;   0: TREEMIN returned 3
;; 3

(deftest test-minmax ()
  (check
   (= 7 (treemax 7))
   (= 7 (treemin 7))
   (= 7 (treemax '(7 (2 1))))
   (= 2 (treemin '(7 (2 1))))
   (= 3 (treemax '((3 (2 5)) (7 (2 1)))) )
   (= 3 (treemin '((3 (2 5)) (7 (2 1)))) )
   (= 6 (treemax '(((1 2) (3 4)) ((5 (6 7)) 8))))
   (= 3 (treemin '(((1 2) (3 4)) ((5 (6 7)) 8))))
   (= 5 (treemax '(1 (8 (2 (7 (3 (6 (4 5)))) )))) )
   (= 1 (treemin '(1 (8 (2 (7 (3 (6 (4 5)))) )))) )))

;;;
;;;    Ex. 16
;;;    
(let* ((opposites '((good bad)
                    (tall short)
                    (right wrong)
                    (left right)
                    (hot cold)
                    (heavy light)
                    (skinny fat)
                    (strong weak)
                    (rich poor)
                    (old young)
                    (inner outer)
                    (new old) ; !
                    (winner loser)
                    (fast slow)))
       (opposites* (mapcar #'reverse opposites)))
  (defun opposite (word)
    (if-let (opp (assoc word opposites))
        (second opp)
      (if-let (opp* (assoc word opposites*))
          (second opp*)
        word))))

(defun make-opposites (sentence)
  (mapcar #'opposite sentence))

(deftest test-make-opposites ()
  (check
   (equal '(THE SHORT MAN IS YOUNG) (make-opposites '(the tall man is old)))
   (equal '(A OLD CAR IS SLOW) (make-opposites '(a new car is fast)))
   (equal '(THE SKINNY POOR GIRL IS VERY LIGHT) (make-opposites '(the fat rich girl is very heavy)))
   (equal '(A COLD CUP OF COFFEE IS ALWAYS BAD) (make-opposites '(a hot cup of coffee is always good)))) )

;;;
;;;    Ex. 17
;;;
(defun sentence-mood (sentence)
  (labels ((interrogativep (sentence)
             (member (first sentence) '(who what when where how why)))
           (imperativep (sentence)
             (member (first sentence) '(hold go take do make help leave run excuse dig give)))
           (indicativep (sentence)
             (declare (ignore sentence))
             t)
           (classify (sentence)
             (cond ((interrogativep sentence) "an interrogative")
                   ((imperativep sentence) "an imperative")
                   ((indicativep sentence) "an indicative")
                   (t "an unusual"))))
    (format nil "This probably ~A sentence." (classify sentence))))


(deftest test-sentence-mood ()
  (check
   (equal "This probably an interrogative sentence."
          (sentence-mood '(why is lisp so popular?)))
   (equal "This probably an imperative sentence."
          (sentence-mood '(make a new attempt at the assignment)))
   (equal "This probably an indicative sentence."
          (sentence-mood '(the sky is blue)))
   (equal "This probably an interrogative sentence."
          (sentence-mood '(where did that thing come from?)))
   (equal "This probably an imperative sentence."
          (sentence-mood '(hold my beer!)))
   (equal "This probably an indicative sentence."
          (sentence-mood '(we have nothing to fear but fear itself)))
   (equal "This probably an imperative sentence."
          (sentence-mood '(do not ask what your country can do for you)))
   (equal "This probably an interrogative sentence."
          (sentence-mood '(why is it so hard to make it in america?)))) )

;;;
;;;    Ex. 20
;;;
;; (defun permutations (l)
;;   (labels ((permute (in out)
;;              (cond ((null in) '())
;;                    (t (destructuring-bind (elt . more) in
;;                         (append (mapcar #'(lambda (l) (cons elt l)) (permute out '()))
;;                                 (permute more (cons elt out)))) ))))
;;     (permute l '())))

(defun permutations (l)
  (permute l '()))

(defun permute (in out)
  (cond ((null in) (list '()))
        (t (destructuring-bind (elt . more) in
             (let ((a (mapcar #'(lambda (l) (cons elt l))
                              (permute (append more out) '()))))
               (if (null more)
                   a
                   (append a (permute more (cons elt out)))) )))) )
                     

(defun permutations2 (l)
  (if (null l)
      (list '())
      (mapcan #'(lambda (cycle)
                  (mapcar #'(lambda (l1)
                              (cons (first cycle) l1))
                          (permutations2 (rest cycle))))
              (cycles l))))

(defun cycles (l)
  (loop for out = () then (cons (first in) out)
        for in on l
        collect (append in out)))

;;;
;;;    见 ~/lisp/programs/combinatorics/permute.lisp
;;;    
(defun permute-3 (l1)
  (cond ((null l1) (list '()))
	(t (cycle-list l1 '()))) )

(defun cycle-list (l1 l2)
  (cond ((null l1) '())
	(t (cycle-list-aux (car l1) (cdr l1) l2
			   (cycle-list (cdr l1) (cons (car l1) l2)))) ))

(defun cycle-list-aux (elt l1 l2 result)
  (cond ((null l1) (spread-elt elt (permute-3 l2) result))
	(t (cycle-list-aux elt (cdr l1) (cons (car l1) l2) result))))

(defun spread-elt (elt partial result)
  (cond ((null partial) result)
	(t (cons (cons elt (car partial))
		 (spread-elt elt (cdr partial) result)))) )
(defun spread-elt (elt partial result)
  (if (endp partial)
      result
      (destructuring-bind (l . more) partial
        (cons (cons elt l) (spread-elt elt more result)))) )

;;;
;;;    Don't care about order -> TR
;;;    
(defun spread-elt2 (elt partial result)
  (if (endp partial)
      result
      (destructuring-bind (l . more) partial
        (spread-elt elt more (cons (cons elt l) result)))) )

(defun spread-elt* (elt partial)
  (mapcar #'(lambda (l) (cons elt l)) partial))
;;;
;;;    Ex. 21
;;;
(defun next (seq)
  (labels ((arithmeticp (seq)
             (let ((diff (- (second seq) (first seq))))
               (if (every #'(lambda (x y) (= diff (- y x))) seq (rest seq))
                   #'(lambda (x) (+ x diff))
                   nil)))
           (geometricp (seq)
             (let ((factor (/ (second seq) (first seq))))
               (if (every #'(lambda (x y) (= factor (/ y x))) seq (rest seq))
                   #'(lambda (x) (* x factor))
                   nil))))
    (if-let (arithmetic (arithmeticp seq))
        (apply arithmetic (last seq))
      (if-let (geometric (geometricp seq))
          (apply geometric (last seq))
        'unknown))))

(deftest test-next ()
  (check
   (= 10 (next '(2 4 6 8)))
   (= 0 (next '(8 6 4 2)))
   (= 324 (next '(4 -12 36 -108)))
   (= 64 (next '(1 2 4 8 16 32)))
   (= 1 (next '(64 32 16 8 4 2)))
   (= 1 (next '(1 -1 1 -1)))
   (= -1 (next '(1 -1 1 -1 1)))
   (= 6 (next '(10 9 8 7)))
   (equal 'UNKNOWN (next '(2 3 5 7 11)))) )

;;;
;;;    Ex. 22
;;;
(setf (get 'daffodil :color) 'yellow
      (get 'daffodil :family) 'amaryllidaceae)

;;;
;;;    Ex. 23
;;;
(defun simplify (expr)
  (cond ((symbolp expr) expr)
        ((numberp expr) expr)
        (t (ecase (first expr)
             (+ (simplify-sum expr))
             (* (simplify-product expr)))) ))

(defun simplify-sum (expr)
  (let ((args (remove 0 (mapcar #'simplify (rest expr)) :test #'equalp)))
    (if (= (length args) 1)
        (first args)
        (cons '+ args))))

(defun simplify-product (expr)
  (let ((args (remove 1 (mapcar #'simplify (rest expr)) :test #'equalp)))
    (cond ((some #'(lambda (x) (equalp x 0)) args) 0)
          ((= (length args) 1) (first args))
          (t (cons '* args)))) )

(deftest test-simplify ()
  (check
   (equal '(+ X 3) (simplify '(+ x 3)))
   (equal '(* X 3) (simplify '(* x 3)))
   (equal 'x (simplify '(+ x 0)))
   (equal 'x (simplify '(* x 1)))
   (equal 0 (simplify '(* x 3 0)))
   (equal 'y (simplify '(+ y (* x 3 0))))
   (equal '(+ X 3 5) (simplify '(+ x 3 5 (* (* x y z) 0))))
   (equal 'x (simplify '(* x (+ 1 (* 0 3)))) )
   (equal '(* X 5) (simplify '(* x (+ 1 (* 0 3)) 5)))) )
