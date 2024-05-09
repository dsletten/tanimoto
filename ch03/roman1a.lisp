;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Name:               roman1a.lisp
;;;;
;;;;   Started:            Tue May 25 06:06:37 2004
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

(defpackage roman1a (:use common-lisp))

(in-package roman1a)

;;; Should be LET*?? Scope of X?
(let* ((x nil)
      (roman-rules (list (list #'(lambda ()
                                   (null x))
                               #'(lambda ()
                                   (format t "Enter number: ")
                                   (setf x (read))))
                         (list #'(lambda ()
                                   (and (not (null x)) (> x 39)))
                               #'(lambda ()
                                   (format t "too big~%")
                                   (setf x nil)))
                         (list #'(lambda ()
                                   (and (not (null x)) (< x 40) (> x 9)))
                               #'(lambda ()
                                   (prin1 'x)
                                   (setf x (- x 10))))
                         (list #'(lambda ()
                                   (and (not (null x)) (= x 9)))
                               #'(lambda ()
                                   (prin1 'ix)
                                   (setf x 0)))
                         (list #'(lambda ()
                                   (and (not (null x)) (< x 9) (> x 4)))
                               #'(lambda ()
                                   (prin1 'v)
                                   (setf x (- x 5))))
                         (list #'(lambda ()
                                   (and (not (null x)) (= x 4)))
                               #'(lambda ()
                                   (prin1 'iv)
                                   (setf x 0)))
                         (list #'(lambda ()
                                   (and (not (null x)) (< x 4) (> x 0)))
                               #'(lambda ()
                                   (prin1 'i)
                                   (setf x (1- x))))
                         (list #'(lambda ()
                                   (zerop x))
                               #'(lambda ()
                                   (setf x nil)
                                   (terpri)))) ))

  (defun roman1 ()
    "Roman numeral conversion with an unordered P.S."
    (loop
     (dolist (rule roman-rules)
       (when (funcall (first rule))
         (funcall (second rule)))) )))