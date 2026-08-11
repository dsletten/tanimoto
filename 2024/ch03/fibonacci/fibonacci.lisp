;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;    What I like about Lisp is that you can feel the bits between your toes.
;;;;    -- Drew McDermott
;;;;
;;;;    Name:               fibonacci.lisp
;;;;
;;;;    Started:            Fri Jun 12 19:39:47 2026
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
(load "/home/slytobias/lisp/packages/io")
(load "/home/slytobias/lisp/packages/test")
;(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/production-system.lisp")
(load "/home/slytobias/Thelio/modified/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :fibonacci (:use :common-lisp :core :io :test :production-system))

(in-package :fibonacci)

(defun fast-fibonacci ()
  (let ((a 1)
        (b 0)
        (p 0)
        (q 1)
        (n nil))
    (loop (cond ((null n) (setf n (get-num "Enter value for N: " :test (conjoin #'integerp (complement #'minusp)))) )
                ((and (not (null n)) (zerop n)) (format t "~D~%" b)
                 (setf a 1
                       b 0
                       p 0
                       q 1
                       n nil))
                ((and (not (null n)) (not (zerop n)) (evenp n))
                 (psetf p (+ (* p p) (* q q))
                        q (+ (* q q) (* 2 p q))
                        n (/ n 2)))
                ((and (not (null n)) (not (zerop n)) (oddp n))
                 (psetf a (+ (* b q) (* a q) (* a p))
                        b (+ (* b p) (* a q))
                        n (1- n)))) )))

;;;
;;;    Ordered
;;;    
(defun fast-fibonacci* ()
  (let ((a 1)
        (b 0)
        (p 0)
        (q 1)
        (n nil))
    (flet ((reset ()
             (setf a 1 b 0 p 0 q 1 n nil)))             
      (loop (pscond ((null n)
                     (setf n (get-num "Enter value for N: " :test (conjoin #'integerp (complement #'minusp)))) )
                    ((zerop n)
                     (format t "~D~%" b)
                     (reset))
                    ((evenp n)
                     (psetf p (+ (* p p) (* q q))
                            q (+ (* q q) (* 2 p q))
                            n (/ n 2)))
                    ((oddp n)
                     (psetf a (+ (* b q) (* a q) (* a p))
                            b (+ (* b p) (* a q))
                            n (1- n)))) ))))
