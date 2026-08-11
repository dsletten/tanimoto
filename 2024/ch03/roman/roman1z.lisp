;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               roman1z.lisp
;;;;
;;;;   Started:            Fri Apr 18 03:18:32 2025
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

(defpackage :roman1z (:use :common-lisp :core :io))

(in-package :roman1z)

(defun roman1 ()
  (let ((x nil))
    (loop (cond ((and (not (null x)) (= x 9))
                 (format t "IX")
                 (setf x 0))
                ((null x)
                 (setf x (get-num "Enter number: "
                                  :test (conjoin #'integerp (complement #'minusp)))) )
                ((and (not (null x)) (= x 4))
                 (format t "IV")
                 (setf x 0))
                ((and (not (null x)) (zerop x))
                 (format t "~%")
                 (setf x nil))
                ((and (not (null x)) (<= 1 x 3))
                 (format t "I")
                 (decf x))
                ((and (not (null x)) (<= 10 x 39))
                 (format t "X")
                 (decf x 10))
                ((and (not (null x)) (> x 39))
                 (format t "Too big.~%")
                 (setf x nil))
                ((and (not (null x)) (<= 5 x 8))
                 (format t "V")
                 (decf x 5)))) ))
