;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               roman-fp.lisp
;;;;
;;;;   Started:            Tue Apr 22 21:28:50 2025
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

(defpackage :roman-fp (:use :common-lisp :core :io :production-system))

(in-package :roman-fp)

(defun roman ()
  (labels ((process (x stream)
             (pscond ((> x 39)
                      (format t "Too big.~%"))
                     ((<= 10 x 39)
                      (format stream "X")
                      (process (- x 10) stream))
                     ((= x 9)
                      (format stream "IX")
                      (process 0 stream))
                     ((<= 5 x 8)
                      (format stream "V")
                      (process (- x 5) stream))
                     ((= x 4)
                      (format stream "IV")
                      (process 0 stream))
                     ((<= 1 x 3)
                      (format stream "I")
                      (process (- x 1) stream))
                     ((zerop x)
                      (get-output-stream-string stream)))) )
    (loop (let ((result (process (get-num "Enter number: "
                                          :test (conjoin #'integerp (complement #'minusp)))
                                 (make-string-output-stream))))
            (when result
              (format t "~A~%" result)))) ))

(defun roman* ()
  (labels ((process (x result)
             (pscond ((> x 39) (format t "Too big.~%"))
                     ((<= 10 x 39) (process (- x 10) (cons "X" result)))
                     ((= x 9) (process 0 (cons "IX" result)))
                     ((<= 5 x 8) (process (- x 5) (cons "V" result)))
                     ((= x 4) (process 0 (cons "IV" result)))
                     ((<= 1 x 3) (process (- x 1) (cons "I" result)))
                     ((zerop x) (reverse result)))) )
    (loop (let ((result (process (get-num "Enter number: "
                                          :test (conjoin #'integerp (complement #'minusp)))
                                 '())))
            (when result
              (format t "~A~%" (apply #'concatenate 'string result)))) )))

;;;
;;;    见 roman3a.lisp
;;;    
(defun roman-dn ()
  (labels ((process (x result)
             (pscond ((> x 4) (pscond ((> x 9) (pscond ((> x 39) (format t "Too big.~%"))
                                                       (t (process (- x 10) (cons "X" result)))) )
                                      (t (pscond ((= x 9) (process 0 (cons "IX" result)))
                                                 (t (process (- x 5) (cons "V" result)))) )))
                     (t (pscond ((= x 4) (process 0 (cons "IV" result)))
                                (t (pscond ((> x 0) (process (- x 1) (cons "I" result)))
                                           (t (reverse result)))) )))) )
    (loop (let ((result (process (get-num "Enter number: "
                                          :test (conjoin #'integerp (complement #'minusp)))
                                 '())))
            (when result
              (format t "~A~%" (apply #'concatenate 'string result)))) )))

;;;
;;;    The above is logically correct, but this reads better in terms of what rules are triggered.
;;;    I.e., T is not a useful rule...
;;;    
(defun roman-dn* ()
  (labels ((process (x result)
             (pscond ((> x 4) (pscond ((> x 9) (pscond ((> x 39) (format t "Too big.~%"))
                                                       ((<= x 39) (process (- x 10) (cons "X" result)))) )
                                      (t (pscond ((= x 9) (process 0 (cons "IX" result)))
                                                 ((< x 9) (process (- x 5) (cons "V" result)))) )))
                     (t (pscond ((= x 4) (process 0 (cons "IV" result)))
                                ((< x 4) (pscond ((> x 0) (process (- x 1) (cons "I" result)))
                                                 ((= x 0) (reverse result)))) )))) )
    (loop (let ((result (process (get-num "Enter number: "
                                          :test (conjoin #'integerp (complement #'minusp)))
                                 '())))
            (when result
              (format t "~A~%" (apply #'concatenate 'string result)))) )))

