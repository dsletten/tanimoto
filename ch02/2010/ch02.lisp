;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Sat Dec  4 23:55:05 2010
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
;;;;   Notes: REASSEMBLE as generalized tree processing function:
;;;;   - EXIFY
;;;;   - MAKE-COPY
;;;;   - TREE-UPCASE
;;;;   - TREE-PLUS
;;;;   - DUPLICATE-TREE
;;;;
(load "/Users/dsletten/lisp/packages/test")

(defpackage ch02
  (:use common-lisp test)
  (:shadow replace))

(in-package ch02)

(defun count-sublists (obj)
  (cond ((null obj) 0)
        ((atom (first obj)) (count-sublists (rest obj)))
        (t (+ 1
              (count-sublists (first obj))
              (count-sublists (rest obj)))) ))

(deftest test-count-sublists ()
  (check
   (= (count-sublists '(a b c)) 0)
   (= (count-sublists '(a b (c))) 1)
   (= (count-sublists '((a (b)) b (c))) 3)
   (= (count-sublists '((beef pork) (potato carrots) ((apple pie cherry pie) candy))) 4)))

(defun match-kth (elt l k)
  (cond ((endp l) nil)
        ((zerop k) (and (eql elt (first l)) elt))
        (t (match-kth elt (rest l) (1- k)))) )

(deftest test-match-kth ()
  (check
   (match-kth 'a '(a b c d) 0)
   (match-kth 'c '(a b c d) 2)
   (not (match-kth 'a '(a b c d) 1))
   (not (match-kth 'a '(a b c d) 10))
   (not (match-kth 'a '() 5))))

;;;;
;;;;    Exercises
;;;;

;;;
;;;    4.
;;;
(defun exify (obj)
  (cond ((null obj) obj)
        ((atom obj) 'x)
        (t (cons (exify (car obj))
                 (exify (cdr obj)))) ))

(deftest test-exify ()
  (check
   (equal (exify '(a (b c) x y nil z)) '(x (x x) x x nil x))))

;;;
;;;    Generalized procedure for taking arbitrary tree apart and putting it back
;;;    together after processing each leaf.
;;;
;;;    Not needed for lists--just use MAPCAR.
;;;    
(defun reassemble (obj f)
  (cond ((null obj) obj)
        ((atom obj) (funcall f obj))
        (t (cons (reassemble (car obj) f)
                 (reassemble (cdr obj) f)))) )

(defun exify (obj)
  (reassemble obj #'(lambda (_) (declare (ignore _)) 'x)))

(defun make-copy (obj)
  (reassemble obj #'identity))

(defun tree-plus (obj delta)
  (reassemble obj #'(lambda (x) (+ x delta))))

(deftest test-tree-plus ()
  (check
   (equal (tree-plus 8 9) 17)
   (equal (tree-plus '(1 (2 ((3) 4) (5 (6)))) 9) '(10 (11 ((12) 13) (14 (15)))) )))

(defun tree-upcase (obj)
  (reassemble obj #'string-upcase))

(deftest test-tree-upcase ()
  (check
   (equal (tree-upcase "pung") "PUNG")
   (equal (tree-upcase '("is" ("this") ("not" ((("pung?")))))) '("IS" ("THIS") ("NOT" ((("PUNG?"))))) )))

;;;
;;;    5.
;;;
(defun replace (obj old new)
  (cond ((equal obj old) new)
        ((atom obj) obj)
        (t (cons (replace (first obj) old new)
                 (replace (rest obj) old new)))) )        

(deftest test-replace ()
  (check
   (equal (replace 'pung 'pung 'foo) 'foo)
   (equal (replace 'pung 'foo 'bar) 'pung)
   (equal (replace '(1 2 3 4 5) 5 1) '(1 2 3 4 1))
   (equal (replace '((this 1) contains (2 occurrences (this 1))) '(this 1) '(that one))
          '((that one) contains (2 occurrences (that one)))) ))

;;;
;;;    6.
;;;
;;;   (n + 1)^3 = n^3 + 3n^2 + 3n + 1
;;;   Let n' = n - 1 <=> n' + 1 = n
;;;   n^3 = (n' + 1)^3
;;;       = n'^3 + 3n'^2 + 3n' + 1
;;;       = (n - 1)^3 + 3n'(n' + 1) + 1
;;;       = (n - 1)^3 + 3n(n - 1) + 1
;;;        
(defun cube (n)
  (case n
    (1 1)
    (otherwise (+ (cube (1- n)) (* 3 n (1- n)) 1))))

(defun cube (n)
  (power n 3))

(defun power (base pow)
  (case pow
    (0 1)
    (otherwise (* base (power base (1- pow)))) ))

(defun cube (n)
  (do* ((i 0 (1+ i))
        (cube 0 (+ cube (* 3 i (1- i)) 1)))
       ((= i n) cube)))

(deftest test-cube ()
  (check
   (= (cube 1) 1)
   (= (cube 2) 8)
   (= (cube 100) 1000000)))

(deftest test-power ()
  (check
   (= (power 1 0) 1)
   (= (power 2 10) 1024)
   (= (power 10 5) 100000)))

(defun cubes (n)
  (loop for i from 1 to n
        for cube = 1 then (+ cube (* 3 i (1- i)) 1)
        collect cube))

(defun cubes (n)
  (do ((i n (1- i))
       (result '() (cons (expt i 3) result)))
       ((zerop i) result)))

;;;
;;;    Backwards!
;;;    
;; (defun cubes (n)
;;   (case n
;;     (0 '())
;;     (otherwise (cons (cube n) (cubes (1- n)))) ))

(defun cubes (n)
  (labels ((cubes-aux (i)
             (if (> i n)
                 '()
                 (cons (cube i) (cubes-aux (1+ i)))) ))
    (cubes-aux 1)))

(defun cubes (n)
  (labels ((cubes-aux (i result)
             (case i
               (0 result)
               (otherwise (cubes-aux (1- i) (cons (cube i) result)))) ))
    (cubes-aux n '())))

(deftest test-cubes ()
  (check
   (equal (cubes 10) '(1 8 27 64 125 216 343 512 729 1000))))

(defun make-cubes-stream ()
  (let ((i 1)
        (cube 1))
    #'(lambda ()
        (prog1 cube (incf i) (incf cube (+ (* 3 i (1- i)) 1)))) ))
      
(defclass cubes-stream ()
  ((stream :reader get-stream :initform (make-cubes-stream))))

(defgeneric next-cube (stream)
  (:method ((stream cubes-stream)) (funcall (get-stream stream))))

;;;
;;;    7.
;;;
(defun primep (n)
  (cond ((not (typep n '(integer 2 *))) nil)
        ((= n 2) t)
        ((evenp n) nil)
        (t (loop for factor from 3 to (isqrt n) by 2
                 never (zerop (mod n factor)))) ))

;; (defun primep (n)
;;   (if (typep n '(integer 2 *))
;;       (loop for factor from 2 to (isqrt n)
;;             never (zerop (mod n factor)))
;;       nil))

(deftest test-primep ()
  (check
   (not (primep 1))
   (primep 2)
   (primep 3)
   (not (primep 4))
   (primep 5)
   (not (primep 6))
   (primep 7)
   (not (primep 8))
   (not (primep 9))
   (not (primep 10))
   (primep 11)))

;;;
;;;    9.
;;;
(defun sum-1 (ilist)
  (apply #'+ ilist))

(defun sum-2 (ilist)
  (reduce #'+ ilist))

(defun sum-3 (ilist)
  (loop for elt in ilist
        summing elt))

(defun sum-4 (ilist)
  (if (endp ilist)
      0
      (+ (first ilist) (sum-4 (rest ilist)))) )

(defun sum-5 (ilist)
  (labels ((sum-aux (l result)
             (if (endp l)
                 result
                 (sum-aux (rest l) (+ result (first l)))) ))
    (sum-aux ilist 0)))

(deftest test-sum-1 ()
  (check
   (= (sum-1 '()) 0)
   (= (sum-1 '(1)) 1)
   (= (sum-1 '(1 2 3 4 5)) 15)
   (= (sum-1 '(1.0 2.0 3.0 4.0 5.0)) 15.0)))

(deftest test-sum-2 ()
  (check
   (= (sum-2 '()) 0)
   (= (sum-2 '(1)) 1)
   (= (sum-2 '(1 2 3 4 5)) 15)
   (= (sum-2 '(1.0 2.0 3.0 4.0 5.0)) 15.0)))

(deftest test-sum-3 ()
  (check
   (= (sum-3 '()) 0)
   (= (sum-3 '(1)) 1)
   (= (sum-3 '(1 2 3 4 5)) 15)
   (= (sum-3 '(1.0 2.0 3.0 4.0 5.0)) 15.0)))

(deftest test-sum-4 ()
  (check
   (= (sum-4 '()) 0)
   (= (sum-4 '(1)) 1)
   (= (sum-4 '(1 2 3 4 5)) 15)
   (= (sum-4 '(1.0 2.0 3.0 4.0 5.0)) 15.0)))

(deftest test-sum-5 ()
  (check
   (= (sum-5 '()) 0)
   (= (sum-5 '(1)) 1)
   (= (sum-5 '(1 2 3 4 5)) 15)
   (= (sum-5 '(1.0 2.0 3.0 4.0 5.0)) 15.0)))

(defmacro test-sum (n)
  (let ((sym (intern (format nil "TEST-SUM-~D" n))))
    `(,sym)))

;; (defun test-all-sums ()
;;   (loop for i from 1 to 5
;;         do (test-sum i)))

(defmacro test-sums (n)
  `(progn ,@(loop for i from 1 to n
                  collect (list (intern (format nil "TEST-SUM-~D" i)))) ))

(defmacro test-sums (n)
  `(dolist (f ',(loop for i from 1 to n
                      collect (intern (format nil "TEST-SUM-~D" i))))
     (funcall f)))

;;;
;;;    10.
;;;
(defun palindromep (l)
  (equal l (reverse l)))

(deftest test-palindromep ()
  (check
   (palindromep '(a b a))
   (palindromep '(a b b a))
   (palindromep '())
   (palindromep '(a))
   (palindromep '(a a))
   (not (palindromep '(a b)))) )

;;;
;;;    12.
;;;
(defun duplicate-tree (obj)
  (cond ((null obj) obj)
        ((atom obj) (list obj obj))
        (t (cons (duplicate-tree (car obj))
                 (duplicate-tree (cdr obj)))) ))

(defun duplicate-tree (obj)
  (reassemble obj #'(lambda (x) (list x x))))

(deftest test-duplicate-tree ()
  (check
   (equal (duplicate-tree 9) '(9 9))
   (equal (duplicate-tree '(a b c)) '((a a) (b b) (c c)))
   (equal (duplicate-tree '(x y (z w))) '((x x) (y y) ((z z) (w w)))) ))

(defun duplicate (l)
  (if (endp l)
      '()
      (let ((elt (first l)))
        (cons (list elt elt) (duplicate (rest l)))) ))

(defun duplicate (l)
  (if (endp l)
      '()
      (destructuring-bind (head . tail) l
        (cons (list head head) (duplicate tail)))) )

(defun duplicate (l)
  (mapcar #'(lambda (elt) (list elt elt)) l))

(deftest test-duplicate ()
  (check
   (equal (duplicate '(a b c)) '((a a) (b b) (c c)))
   (equal (duplicate '(x y (z w))) '((x x) (y y) ((z w) (z w)))) ))

;;;
;;;    13.
;;;
(defun equal-elts (l)
  (if (null l)
      t
      (destructuring-bind (head . tail) l
        (if (null tail)
            t
            (if (equal head (first tail))
                (equal-elts tail)
                nil)))) )

(defun equal-elts (l)
  (if (null l)
      t
      (every #'(lambda (elt) (equal elt (first l))) (rest l))))

(deftest test-equal-elts ()
  (check
   (equal-elts '())
   (equal-elts '(a))
   (equal-elts '((a b) (a b) (a b) (a b)))
   (equal-elts '(1 1 1 1 1))
   (not (equal-elts '(1 2)))
   (not (equal-elts '(1 1 1 2 1)))
   (not (equal-elts '(is this not pung?)))) )

;;;
;;;    14.
;;;
;;;
;;;    Fixed this based on Touretzky ch. 8
;;;    
;; (defun depth (tree)
;;   (if (atom tree)
;;       0
;;       (max (1+ (depth (car tree)))
;;            (depth (cdr tree)))) )

;; (deftest test-depth ()
;;   (check
;;    (= (depth 'a) 0)
;;    (= (depth '(a b c)) 1)
;;    (= (depth '(a (b (c (d)))) ) 4)))

(defun depth (tree)
  (if (atom tree)
      0
      (1+ (max (depth (car tree))
               (depth (cdr tree)))) ))

(deftest test-depth ()
  (check
   (= (depth 'a) 0)
   (= (depth '(a b c)) 3)
   (= (depth '(a (b (c (d)))) ) 7))) ; Draw the diagram!

(defun quasi-balanced-p (l)
  (and (equal-elts (mapcar #'length l))
       (equal-elts (mapcar #'depth l))))  

(deftest test-quasi-balanced-p ()
  (check
   (quasi-balanced-p '((a b) (c d) (e f)))
   (quasi-balanced-p '((a b (1)) (c d (2)) (e f (3))))
   (quasi-balanced-p '(((1) a b) (c (2) d) (e f 3)))             ; Draw the 
   (not (quasi-balanced-p '(((1) a b) (c (2) d) (e f (3)))) )))  ;    diagrams!

;;;
;;;    15.
;;;
;;;    Sufficient if all dotted pairs.
;;;    
;; (defun tree-max (obj)
;;   (cond ((atom obj) obj)
;;         (t (max (tree-min (car obj))
;;                 (tree-min (cdr obj)))) ))

;; (defun tree-min (obj)
;;   (cond ((atom obj) obj)
;;         (t (min (tree-max (car obj))
;;                 (tree-max (cdr obj)))) ))

(defun tree-max (obj)
  (cond ((atom obj) obj)
        (t (let ((min-head (tree-min (car obj)))
                 (min-tail (tree-min (cdr obj))))
             (if (null min-tail)
                 min-head
                 (max min-head min-tail)))) ))

(defun tree-min (obj)
  (cond ((atom obj) obj)
        (t (let ((max-head (tree-max (car obj)))
                 (max-tail (tree-max (cdr obj))))
             (if (null max-tail)
                 max-head
                 (min max-head max-tail)))) ))

(defun tree-max (obj)
  (cond ((atom obj) obj)
        (t (max (tree-min (first obj))
                (tree-min (second obj)))) ))

(defun tree-min (obj)
  (cond ((atom obj) obj)
        (t (min (tree-max (first obj))
                (tree-max (second obj)))) ))

(deftest test-tree-max ()
  (check
   (= (tree-max '((3 (2 5)) (7 (3 1)))) 3)
   (= (tree-max '(((1 2) (3 4)) ((5 (6 7)) 8))) 6)
   (= (tree-max '(1 (8 (2 (7 (3 (6 (4 5)))) )))) 5)))

(deftest test-tree-min ()
  (check
   (= (tree-min '((3 (2 5)) (7 (3 1)))) 3)
   (= (tree-min '(((1 2) (3 4)) ((5 (6 7)) 8))) 3)
   (= (tree-min '(1 (8 (2 (7 (3 (6 (4 5)))) )))) 1)))


;;;
;;;    20.
;;;
;; (defun insert-elt (elt l1 l2)
;;   (if (endp l1)
;;       (cons elt l2)
;;       (cons (first l1) (insert-elt elt (rest l1) l2))))

(defun insert-elt (elt l1 l2)
  (revappend l1 (cons elt l2)))

(defun spread-elt (elt l)
  (spread-elt-aux elt '() l))

(defun spread-elt-aux (elt l1 l2)
  (if (endp l2)
      (list (insert-elt elt l1 l2))
      (cons (insert-elt elt l1 l2) (spread-elt-aux elt (cons (first l2) l1) (rest l2)))) )

(defun permutations (l)
  (if (endp l)
      (list '())
      (mapcan #'(lambda (permutation) (spread-elt (first l) permutation)) (permutations (rest l)))) )

;;;
;;;    21.
;;;
(defun next (l)
  (or (arithmetic-progression (- (second l) (first l)) (first l) (rest l))
      (geometric-progression (/ (second l) (first l)) (first l) (rest l))
      'unknown))

(defun arithmetic-progression (sum this l)
  (cond ((endp l) (+ sum this))
        ((= (- (first l) this) sum) (arithmetic-progression sum (first l) (rest l)))
        (t nil)))

(defun geometric-progression (factor this l)
  (cond ((endp l) (* factor this))
        ((= (/ (first l) this) factor) (geometric-progression factor (first l) (rest l)))
        (t nil)))
