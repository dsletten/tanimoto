;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;    If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;    -- Guy Steele
;;;;
;;;;    Name:               accumulate.lisp
;;;;
;;;;    Started:            Fri Jun 12 19:12:15 2026
;;;;    Modifications:
;;;;
;;;;    Purpose:
;;;;
;;;;
;;;;
;;;;    Calling Sequence:
;;;;
;;;;
;;;;    Inputs:
;;;;
;;;;    Outputs:
;;;;
;;;;    Example:
;;;;
;;;;    Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core")
(load "/home/slytobias/lisp/packages/io")
(load "/home/slytobias/lisp/packages/test")

(defpackage :accumulate (:use :common-lisp :core :io :test))

(in-package :accumulate)

;;;
;;;    SICP ex. 1.33
;;;    
(defun filtered-accumulate (pred combiner identity-elt term next)
  (let ((a nil)
        (b nil)
        (result identity-elt))
    (loop (cond ((null a) (setf a (get-num "Enter start value: " :test #'integerp)))
                ((and (not (null a)) (null b)) (setf b (get-num "Enter end value: " :test #'integerp)))
                ((and (not (null a)) (not (null b)) (> a b)) (format t "~F~%" result) (setf a nil b nil result identity-elt))
                ((and (not (null a)) (not (null b)) (not (> a b)) (funcall pred a))
                 (setf result (funcall combiner result (funcall term a))
                       a (funcall next a)))
                ((and (not (null a)) (not (null b)) (not (> a b)) (not (funcall pred a)))
                 (setf a (funcall next a)))) )))

;; SUM-INTEGERS
;; (filtered-accumulate (constantly t) #'+ 0 #'identity #'1+)

;; SUM-CUBES
;; (filtered-accumulate (constantly t) #'+ 0 #'(lambda (x) (* x x x)) #'1+)

;; PI-SUM
;; (filtered-accumulate (constantly t) #'+ 0 #'(lambda (x) (/ 1d0 (* x (+ x 2)))) (partial #'+ 4))

;; FACTORIAL
;; (filtered-accumulate (constantly t) #'* 1 #'identity #'1+)

(defun filtered-accumulate* (pred combiner identity-elt term next)
  (let ((a nil)
        (b nil)
        (result identity-elt))
    (loop (cond ((null a) (setf a (get-num "Enter start value: " :test #'integerp)))
                ((null b) (setf b (get-num "Enter end value: " :test #'integerp)))
                (t (cond ((> a b)
                          (format t "~F~%" result)
                          (setf a nil
                                b nil
                                result identity-elt))
                         ((and (not (> a b)) (funcall pred a))
                          (setf result (funcall combiner result (funcall term a))
                                a (funcall next a)))
                         ((and (not (> a b)) (not (funcall pred a)))
                          (setf a (funcall next a)))) )))) )

;;;
;;;    Ordered
;;;    
(defun filtered-accumulate** (pred combiner identity-elt term next)
  (let ((a nil)
        (b nil)
        (result identity-elt))
    (flet ((reset ()
             (setf a nil
                   b nil
                   result identity-elt)))
      (loop (cond ((null a) (setf a (get-num "Enter start value: " :test #'integerp)))
                  ((null b) (setf b (get-num "Enter end value: " :test #'integerp)))
                  (t (cond ((> a b)
                            (format t (if (integerp result) "~D~%" "~F~%") result)
                            (reset))
                           ((funcall pred a)
                            (setf result (funcall combiner result (funcall term a))
                                  a (funcall next a)))
                           (t (setf a (funcall next a)))) )))) ))
