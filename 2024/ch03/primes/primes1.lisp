;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   In Lisp there is always more than one way to solve a problem.
;;;;   -- David Touretzky
;;;;
;;;;   Name:               primes1.lisp
;;;;
;;;;   Started:            Sun Jun  1 10:54:02 2025
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
;;;;   Notes: Unordered PS
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core")
(load "/home/slytobias/lisp/packages/io")
;(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/production-system.lisp")
(load "/home/slytobias/Thelio/modified/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :primes1 (:use :common-lisp :core :io :production-system))

(in-package :primes1)

(defun primes1 ()
  (let (n factor limit)
    (flet ((report-prime (n) (format t "~D is prime.~%" n))
           (report-not-prime (n) (format t "~D is not prime.~%" n))
           (initialize ()
             (setf n (get-num "Enter number: " :test (conjoin #'integerp #'plusp))
                   factor 3
                   limit (isqrt n))))
    (loop (pscond ((null n) (initialize))
                  ((and (not (null n)) (= n 1))
                   (report-not-prime n)
                   (setf n nil))
                  ((and (not (null n)) (= n 2))
                   (report-prime n)
                   (setf n nil))
                  ((and (not (null n)) (> n 2) (evenp n))
                   (report-not-prime n)
                   (setf n nil))
                  ((and (not (null n)) (> n 1) (oddp n) (> factor limit))
                   (report-prime n)
                   (setf n nil))
                  ((and (not (null n)) (> n 1) (oddp n) (<= factor limit) (zerop (mod n factor)))
                   (report-not-prime n)
                   (setf n nil))
                  ((and (not (null n)) (> n 1) (oddp n) (<= factor limit) (not (zerop (mod n factor))))
                   (incf factor 2)))) )))

;;;
;;;    Shuffled unordered rules
;;;
(defun primes1x ()
  (let (n factor limit)
    (flet ((report-prime (n) (format t "~D is prime.~%" n))
           (report-not-prime (n) (format t "~D is not prime.~%" n))
           (initialize ()
             (setf n (get-num "Enter number: " :test (conjoin #'integerp #'plusp))
                   factor 3
                   limit (isqrt n))))
    (loop (pscond ((AND (NOT (NULL N)) (> N 1) (ODDP N) (<= FACTOR LIMIT)
                        (ZEROP (MOD N FACTOR)))
                   (REPORT-NOT-PRIME N) (SETF N NIL))
                  ((NULL N) (initialize))
                  ((AND (NOT (NULL N)) (= N 1)) (REPORT-NOT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 1) (ODDP N) (<= FACTOR LIMIT)
                        (NOT (ZEROP (MOD N FACTOR))))
                   (INCF FACTOR 2))
                  ((AND (NOT (NULL N)) (= N 2)) (REPORT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 2) (EVENP N)) (REPORT-NOT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 1) (ODDP N) (> FACTOR LIMIT)) (REPORT-PRIME N)
                   (SETF N NIL)))) )))

(defun primes1y ()
  (let (n factor limit)
    (flet ((report-prime (n) (format t "~D is prime.~%" n))
           (report-not-prime (n) (format t "~D is not prime.~%" n))
           (initialize ()
             (setf n (get-num "Enter number: " :test (conjoin #'integerp #'plusp))
                   factor 3
                   limit (isqrt n))))
    (loop (pscond ((AND (NOT (NULL N)) (> N 1) (ODDP N) (> FACTOR LIMIT)) (REPORT-PRIME N)
                   (SETF N NIL))
                  ((AND (NOT (NULL N)) (= N 1)) (REPORT-NOT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 2) (EVENP N)) (REPORT-NOT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 1) (ODDP N) (<= FACTOR LIMIT)
                        (ZEROP (MOD N FACTOR)))
                   (REPORT-NOT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (= N 2)) (REPORT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 1) (ODDP N) (<= FACTOR LIMIT)
                        (NOT (ZEROP (MOD N FACTOR))))
                   (INCF FACTOR 2))
                  ((NULL N) (initialize)))) )))
