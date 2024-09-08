;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   In Lisp there is always more than one way to solve a problem.
;;;;   -- David Touretzky
;;;;
;;;;   Name:               roman1.lisp
;;;;
;;;;   Started:            Mon Aug  5 17:08:45 2024
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

(defpackage :roman1 (:use :common-lisp :core :test))

(in-package :roman1)

(defun roman1 ()
  (let ((x nil))
    (loop (cond ((null x)
                 (setf x (get-num "Enter number: " :test #'integerp)))
                ((and (not (null x)) (> x 39))
                 (format t "Too big.~%")
                 (setf x nil))
                ((and (not (null x)) (<= 10 x 39))
                 (format t "X")
                 (decf x 10))
                ((and (not (null x)) (= x 9))
                 (format t "IX")
                 (setf x 0))
                ((and (not (null x)) (<= 5 x 8))
                 (format t "V")
                 (decf x 5))
                ((and (not (null x)) (= x 4))
                 (format t "IV")
                 (setf x 0))
                ((and (not (null x)) (<= 1 x 3))
                 (format t "I")
                 (decf x))
                ((and (not (null x)) (zerop x))
                 (format t "~%")
                 (setf x nil)))) ))

                
                 
