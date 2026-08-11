;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               roman1x.lisp
;;;;
;;;;   Started:            Thu Apr 17 03:44:41 2025
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
;;;;   Notes: Production rules can be written in any order.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
;(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/production-system.lisp")
(load "/home/slytobias/Thelio/modified/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :roman1x (:use :common-lisp :core :io :production-system))

(in-package :roman1x)

(defun roman1 ()
  (let ((x nil))
    (loop (cond ((null x)
                 (setf x (get-num "Enter number: "
                                  :test (conjoin #'integerp (complement #'minusp)))) )
                ((and (not (null x)) (<= 10 x 39))
                 (format t "X")
                 (decf x 10))
                ((and (not (null x)) (= x 4))
                 (format t "IV")
                 (setf x 0))
                ((and (not (null x)) (= x 9))
                 (format t "IX")
                 (setf x 0))
                ((and (not (null x)) (zerop x))
                 (format t "~%")
                 (setf x nil))
                ((and (not (null x)) (> x 39))
                 (format t "Too big.~%")
                 (setf x nil))
                ((and (not (null x)) (<= 1 x 3))
                 (format t "I")
                 (decf x))
                ((and (not (null x)) (<= 5 x 8))
                 (format t "V")
                 (decf x 5)))) ))

(defun roman1 ()
  (let ((x nil)
        (s (make-string-output-stream)))
    (loop (pscond ((null x)
                 (setf x (get-num "Enter number: "
                                  :test (conjoin #'integerp (complement #'minusp)))) )
                ((and (not (null x)) (<= 10 x 39))
                 (format s "X")
                 (decf x 10))
                ((and (not (null x)) (= x 4))
                 (format s "IV")
                 (setf x 0))
                ((and (not (null x)) (= x 9))
                 (format s "IX")
                 (setf x 0))
                ((and (not (null x)) (zerop x))
                 (format t "~A~%" (get-output-stream-string s))
                 (setf x nil))
                ((and (not (null x)) (> x 39))
                 (format t "Too big.~%")
                 (setf x nil))
                ((and (not (null x)) (<= 1 x 3))
                 (format s "I")
                 (decf x))
                ((and (not (null x)) (<= 5 x 8))
                 (format s "V")
                 (decf x 5)))) ))
