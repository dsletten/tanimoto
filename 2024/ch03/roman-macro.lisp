;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               roman-macro.lisp
;;;;
;;;;   Started:            Sat Aug 10 20:54:26 2024
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
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :roman-macro (:use :common-lisp :core :test :production-system))

(in-package :roman-macro)

(defvar *roman* (defproductions ((x nil)
                                 (stream (make-string-output-stream)))
                    (null-x
                     (null x)
                     (setf x (get-num "Enter number: " :test #'integerp)))
                    (too-big
                     (and (not (null x)) (> x 39))
                     (format t "Too big.~%")
                     (setf x nil))
                    (x
                     (and (not (null x)) (<= 10 x 39))
                     (format stream "X")
                     (decf x 10))
                    (ix
                     (and (not (null x)) (= x 9))
                     (format stream "IX")
                     (setf x 0))
                    (v
                     (and (not (null x)) (<= 5 x 8))
                     (format stream "V")
                     (decf x 5))
                    (iv
                     (and (not (null x)) (= x 4))
                     (format stream "IV")
                     (setf x 0))
                    (i
                     (and (not (null x)) (<= 1 x 3))
                     (format stream "I")
                     (decf x))
                    (0
                     (and (not (null x)) (zerop x))
                     (format t "~A~%" (get-output-stream-string stream))
                     (setf x nil))))

(defvar *roman-shuffle* (defproductions ((x nil)
                                         (stream (make-string-output-stream)))
                          (null-x
                           (null x)
                           (setf x (get-num "Enter number: " :test #'integerp)))
                          (x
                           (and (not (null x)) (<= 10 x 39))
                           (format stream "X")
                           (decf x 10))
                          (i
                           (and (not (null x)) (<= 1 x 3))
                           (format stream "I")
                           (decf x))
                          (v
                           (and (not (null x)) (<= 5 x 8))
                           (format stream "V")
                           (decf x 5))
                          (iv
                           (and (not (null x)) (= x 4))
                           (format stream "IV")
                           (setf x 0))
                          (too-big
                           (and (not (null x)) (> x 39))
                           (format t "Too big.~%")
                           (setf x nil))
                          (ix
                           (and (not (null x)) (= x 9))
                           (format stream "IX")
                           (setf x 0))
                          (0
                           (and (not (null x)) (zerop x))
                           (format t "~A~%" (get-output-stream-string stream))
                           (setf x nil))))

