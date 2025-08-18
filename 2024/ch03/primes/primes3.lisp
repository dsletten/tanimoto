;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               primes3.lisp
;;;;
;;;;   Started:            Sun Jun  1 13:02:32 2025
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
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :primes3 (:use :common-lisp :core :io :production-system))

(in-package :primes3)

;;;
;;;    Not completely balanced.
;;;    Possible to do better?
;;;    
(defun primes3 ()
  (flet ((prime (n) (format t "~D is prime.~%" n))
         (not-prime (n) (format t "~D is not prime.~%" n)))
  (let (n factor limit)
    (loop (if (null n)
              (setf n (get-num "Enter number: "
                               :test (conjoin #'integerp #'plusp))
                    factor 3
                    limit (isqrt n))
              (if (evenp n)
                  (if (= n 2)
                      (progn (prime n)
                             (setf n nil))
                      (progn (not-prime n)
                             (setf n nil)))
                  (if (= n 1)
                      (progn (not-prime n)
                             (setf n nil))
                      (if (> factor limit)
                          (progn (prime n)
                                 (setf n nil))
                          (if (zerop (mod n factor))
                              (progn (not-prime n)
                                     (setf n nil))
                              (incf factor 2)))) )))) ))

(defun primes3a ()
  (flet ((prime (n) (format t "~D is prime.~%" n))
         (not-prime (n) (format t "~D is not prime.~%" n)))
  (let (n factor limit)
    (loop (if (null n)
              (setf n (get-num "Enter number: "
                               :test (conjoin #'integerp #'plusp))
                    factor 3
                    limit (isqrt n))
              (pscond ((evenp n) (pscond ((= n 2) (prime n) (setf n nil))
                                         (t (not-prime n) (setf n nil))))
                      (t (pscond ((= n 1) (not-prime n) (setf n nil))
                                 (t (pscond ((> factor limit) (prime n) (setf n nil))
                                            (t (pscond ((zerop (mod n factor)) (not-prime n) (setf n nil))
                                                       (t (incf factor 2)))) )))) )))) ))

;;;
;;;    Some (PS)COND tests are more verbose than needed to facilitate tracing.
;;;    
(defun primes3a* ()
  (flet ((prime (n) (format t "~D is prime.~%" n))
         (not-prime (n) (format t "~D is not prime.~%" n)))
  (let (n factor limit)
    (loop (if (null n)
              (setf n (get-num "Enter number: "
                               :test (conjoin #'integerp #'plusp))
                    factor 3
                    limit (isqrt n))
              (pscond ((evenp n) (pscond ((= n 2) (prime n) (setf n nil))
                                         ((> n 2) (not-prime n) (setf n nil))))
                      ((oddp n) (pscond ((= n 1) (not-prime n) (setf n nil))
                                        ((> n 1) (pscond ((> factor limit) (prime n) (setf n nil))
                                                         ((<= factor limit) (pscond ((zerop (mod n factor)) (not-prime n) (setf n nil))
                                                                                    ((not (zerop (mod n factor))) (incf factor 2)))) )))) )))) ))
