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
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :roman1 (:use :common-lisp :core :io :production-system))

(in-package :roman1)

(defun roman1 ()
  (let ((x nil))
    (loop (cond ((null x)
                 (setf x (get-num "Enter number: "
                                  :test (conjoin #'integerp (complement #'minusp)))) )
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

(defun roman1* ()
  (let ((x nil)
        (s (make-string-output-stream)))
    (loop (cond ((null x)
                 (setf x (get-num "Enter number: "
                                  :test (conjoin #'integerp (complement #'minusp)))) )
                ((and (not (null x)) (> x 39))
                 (format t "Too big.~%")
                 (setf x nil))
                ((and (not (null x)) (<= 10 x 39))
                 (format s "X")
                 (decf x 10))
                ((and (not (null x)) (= x 9))
                 (format s "IX")
                 (setf x 0))
                ((and (not (null x)) (<= 5 x 8))
                 (format s "V")
                 (decf x 5))
                ((and (not (null x)) (= x 4))
                 (format s "IV")
                 (setf x 0))
                ((and (not (null x)) (<= 1 x 3))
                 (format s "I")
                 (decf x))
                ((and (not (null x)) (zerop x))
                 (format t "~A~%" (get-output-stream-string s))
                 (setf x nil)))) ))

;;;
;;;    Semi-unordered production system.
;;;    Once input has been validated, rules are unordered.
;;;    
(defun roman1a ()
  (let ((x nil)
        (s (make-string-output-stream)))
    (loop (if (null x)
              (setf x (get-num "Enter number: "
                               :test (conjoin #'integerp (complement #'minusp))))
              (pscond ((> x 39)
                     (format t "Too big.~%")
                     (setf x nil))
                    ((<= 10 x 39)
                     (format s "X")
                     (decf x 10))
                    ((= x 9)
                     (format s "IX")
                     (setf x 0))
                    ((<= 5 x 8)
                     (format s "V")
                     (decf x 5))
                    ((= x 4)
                     (format s "IV")
                     (setf x 0))
                    ((<= 1 x 3)
                     (format s "I")
                     (decf x))
                    ((zerop x)
                     (format t "~A~%" (get-output-stream-string s))
                     (setf x nil)))) )))

                
                 
