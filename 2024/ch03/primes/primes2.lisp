;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               primes2.lisp
;;;;
;;;;   Started:            Sun Jun  1 12:54:48 2025
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
;;;;   Notes: Ordered PS rules
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :primes2 (:use :common-lisp :core :io :production-system))

(in-package :primes2)

(defun primes2 ()
  (flet ((prime (n) (format t "~D is prime.~%" n))
         (not-prime (n) (format t "~D is not prime.~%" n)))
  (let (n factor limit)
    (loop (pscond ((null n)
                   (setf n (get-num "Enter number: "
                                    :test (conjoin #'integerp #'plusp))
                         factor 3
                         limit (isqrt n)))
                  ((= n 1)
                   (not-prime n)
                   (setf n nil))
                  ((= n 2)
                   (prime n)
                   (setf n nil))
                  ((evenp n)
                   (not-prime n)
                   (setf n nil))
                  ((> factor limit)
                   (prime n)
                   (setf n nil))
                  ((zerop (mod n factor))
                   (not-prime n)
                   (setf n nil))
                  (t (incf factor 2)))) )))
