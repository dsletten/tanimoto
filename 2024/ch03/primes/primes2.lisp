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
(load "/home/slytobias/lisp/packages/core")
(load "/home/slytobias/lisp/packages/io")
;(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/production-system.lisp")
(load "/home/slytobias/Thelio/modified/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :primes2 (:use :common-lisp :core :io :production-system))

(in-package :primes2)

(defun primes2 ()
  (let (n factor limit)
    (flet ((report-prime (n) (format t "~D is prime.~%" n))
           (report-not-prime (n) (format t "~D is not prime.~%" n))
           (initialize ()
             (setf n (get-num "Enter number: " :test (conjoin #'integerp #'plusp))
                   factor 3
                   limit (isqrt n))))
      (loop (pscond ((null n) (initialize))
                    ((= n 1)
                     (report-not-prime n)
                     (setf n nil))
                    ((= n 2)
                     (report-prime n)
                     (setf n nil))
                    ((evenp n)
                     (report-not-prime n)
                     (setf n nil))
                    ((> factor limit)
                     (report-prime n)
                     (setf n nil))
                    ((zerop (mod n factor))
                     (report-not-prime n)
                     (setf n nil))
                    (:increase-factor (incf factor 2)))) )))
