;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               roman2.lisp
;;;;
;;;;   Started:            Sat Aug 10 21:18:54 2024
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

(defpackage :roman2 (:use :common-lisp :core :test))

(in-package :roman2)

(defun roman2 ()
  (let ((x nil))
    (loop (cond ((null x)
                 (setf x (get-num "Enter number: " :test #'integerp)))
                ((> x 39)
                 (format t "Too big.~%")
                 (setf x nil))
                ((> x 9)
                 (format t "X")
                 (decf x 10))
                ((= x 9)
                 (format t "IX")
                 (setf x 0))
                ((> x 4)
                 (format t "V")
                 (decf x 5))
                ((= x 4)
                 (format t "IV")
                 (setf x 0))
                ((> x 0)
                 (format t "I")
                 (decf x))
                (t (format t "~%")
                   (setf x nil)))) ))

                
                 
