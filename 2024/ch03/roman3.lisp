;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               roman3.lisp
;;;;
;;;;   Started:            Sat Aug 24 00:17:10 2024
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
;;;;   Notes: Arrange rules as discrimination net.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :roman3 (:use :common-lisp :core :test))

(in-package :roman3)

(defun roman3 ()
  (let ((x nil))
    (loop (if (null x)
              (setf x (get-num "Enter number: " :test #'integerp))
              (if (> x 39)
                  (progn (format t "Too big.~%")
                         (setf x nil))
                  (if (> x 4)
                      (if (> x 9)
                          (progn (format t "X")
                                 (decf x 10))
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
                                     (setf x nil)))) )))) ))



                
                 
