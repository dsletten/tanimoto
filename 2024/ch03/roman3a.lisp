;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               roman3a.lisp
;;;;
;;;;   Started:            Sat Aug 24 00:17:12 2024
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
;;;;   Notes: Arrange rules as discrimination net. Better balanced tree!
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :roman3a (:use :common-lisp :core :test))

(in-package :roman3a)

(defun roman3a ()
  (let ((x nil))
    (loop (if (null x)
              (setf x (get-num "Enter number: " :test #'integerp))
              (if (> x 4)
                  (if (> x 9)
                      (if (> x 39)
                          (progn (format t "Too big.~%")
                                 (setf x nil))
                          (progn (format t "X")
                                 (decf x 10)))
                      (if (= x 9)
                          (progn (format t "IX")
                                 (setf x 0))
                          (progn (format t "V")
                                 (decf x 5))))
                  (if (= x 4)
                      (progn (format t "IV")
                             (setf x 0))
                      (if (> x 0)
                          (progn (format t "I")
                                 (decf x))
                          (progn (format t "~%")
                                 (setf x nil)))) )))) )


                
                 
