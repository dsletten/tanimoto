#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               roman-dn.lisp
;;;;
;;;;   Started:            Mon Sep 16 18:16:43 2024
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
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/discrimination-net.lisp")

(defpackage :roman-dn (:use :common-lisp :core :discrimination-net))

(in-package :roman-dn)

(let ((x nil))
  (defnet *roman* "Roman numerals"
    (start (null x)
      (setf x (get-num "Enter number: " :test #'integerp)) 
      greater4)
    (greater4 (> x 4)
      greater9
      equal4)
    (greater9 (> x 9)
      greater39
      equal9)
    (greater39 (> x 39)
      (progn (format t "Too big.~%") (setf x nil))
      (progn (format t "X") (decf x 10)))
    (equal9 (= x 9)
      (progn (format t "IX") (setf x 0))
      (progn (format t "V") (decf x 5)))
    (equal4 (= x 4)
      (progn (format t "IV") (setf x 0))
      positive)
    (positive (> x 0)
      (progn (format t "I") (decf x))
      (progn (format t "~%") (setf x nil)))) )

(let ((x nil))
  (defnet *roman3999* "Roman numerals"
    (start (null x)
      (setf x (get-num "Enter number: " :test #'integerp))
      >=100)
    (>=100 (>= x 100)
      >=500
      >=50)
    (>=500 (>= x 500)
      >=1000
      >=400)
    (>=1000 (>= x 1000)
      >3999
      >=900)
    (>3999 (> x 3999)
      (progn (format t "Too big.~%") (setf x nil))
      (progn (format t "M") (decf x 1000)))
    (>=900 (>= x 900)
      (progn (format t "CM") (decf x 900))
      (progn (format t "D") (decf x 500)))
    (>=400 (>= x 400)
      (progn (format t "CD") (decf x 400))
      (progn (format t "C") (decf x 100)))
    (>=50 (>= x 50)
      >=90
      >=10)
    (>=90 (>= x 90)
      (progn (format t "XC") (decf x 90))
      (progn (format t "L") (decf x 50)))
    (>=10 (>= x 10)
      >=40
      >=5)
    (>=40 (>= x 40)
      (progn (format t "XL") (decf x 40))
      (progn (format t "X") (decf x 10)))
    (>=5 (>= x 5)
      =9
      >0)
    (=9 (= x 9)
        (progn (format t "IX") (decf x 9))
        (progn (format t "V") (decf x 5)))
    (>0 (> x 0)
      =4
      (progn (format t "~%") (setf x nil)))
    (=4 (= x 4)
      (progn (format t "IV") (decf x 4))
      (progn (format t "I") (decf x)))) )

(run *roman3999* 'start)
