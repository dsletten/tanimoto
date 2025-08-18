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
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :primes1 (:use :common-lisp :core :io :production-system))

(in-package :primes1)

(defun primes1 ()
  (flet ((prime (n) (format t "~D is prime.~%" n))
         (not-prime (n) (format t "~D is not prime.~%" n)))
  (let (n factor limit)
    (loop (pscond ((null n)
                   (setf n (get-num "Enter number: " :test (conjoin #'integerp #'plusp))
                         factor 3
                         limit (isqrt n)))
                  ((and (not (null n)) (= n 1))
                   (not-prime n)
                   (setf n nil))
                  ((and (not (null n)) (= n 2))
                   (prime n)
                   (setf n nil))
                  ((and (not (null n)) (> n 2) (evenp n))
                   (not-prime n)
                   (setf n nil))
                  ((and (not (null n)) (> n 1) (oddp n) (> factor limit))
                   (prime n)
                   (setf n nil))
                  ((and (not (null n)) (> n 1) (oddp n) (<= factor limit) (zerop (mod n factor)))
                   (not-prime n)
                   (setf n nil))
                  ((and (not (null n)) (> n 1) (oddp n) (<= factor limit) (not (zerop (mod n factor))))
                   (incf factor 2)))) )))

;;;
;;;    Shuffled unordered rules
;;;
(defun primes1x ()
  (flet ((prime (n) (format t "~D is prime.~%" n))
         (not-prime (n) (format t "~D is not prime.~%" n)))
  (let (n factor limit)
    (loop (pscond ((AND (NOT (NULL N)) (> N 1) (ODDP N) (<= FACTOR LIMIT)
                        (ZEROP (MOD N FACTOR)))
                   (NOT-PRIME N) (SETF N NIL))
                  ((NULL N)
                   (SETF N (GET-NUM "Enter number: " :TEST (CONJOIN #'INTEGERP #'PLUSP))
                         FACTOR 3
                         LIMIT (ISQRT N)))
                  ((AND (NOT (NULL N)) (= N 1)) (NOT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 1) (ODDP N) (<= FACTOR LIMIT)
                        (NOT (ZEROP (MOD N FACTOR))))
                   (INCF FACTOR 2))
                  ((AND (NOT (NULL N)) (= N 2)) (PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 2) (EVENP N)) (NOT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 1) (ODDP N) (> FACTOR LIMIT)) (PRIME N)
                   (SETF N NIL)))) )))

(defun primes1y ()
  (flet ((prime (n) (format t "~D is prime.~%" n))
         (not-prime (n) (format t "~D is not prime.~%" n)))
  (let (n factor limit)
    (loop (pscond ((AND (NOT (NULL N)) (> N 1) (ODDP N) (> FACTOR LIMIT)) (PRIME N)
                   (SETF N NIL))
                  ((AND (NOT (NULL N)) (= N 1)) (NOT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 2) (EVENP N)) (NOT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 1) (ODDP N) (<= FACTOR LIMIT)
                        (ZEROP (MOD N FACTOR)))
                   (NOT-PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (= N 2)) (PRIME N) (SETF N NIL))
                  ((AND (NOT (NULL N)) (> N 1) (ODDP N) (<= FACTOR LIMIT)
                        (NOT (ZEROP (MOD N FACTOR))))
                   (INCF FACTOR 2))
                  ((NULL N)
                   (SETF N (GET-NUM "Enter number: " :TEST (CONJOIN #'INTEGERP #'PLUSP))
                         FACTOR 3
                         LIMIT (ISQRT N))))) )))

