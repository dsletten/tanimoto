#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               median-dn.lisp
;;;;
;;;;   Started:            Mon Sep 16 18:16:50 2024
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
;;;;   Notes: Determine median of 3 numbers. 见 SICP notes A-10.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/discrimination-net.lisp")

(defpackage :median-dn (:use :common-lisp :core :discrimination-net))

(in-package :median-dn)

(let ((a nil)
      (b nil)
      (c nil))
  (flet ((reset () (setf a nil b nil c nil)))
    (defnet *median* "Median of three"
      (starta (null a)
        (setf a (get-num "Enter first number: "))
        startb)
      (startb (null b)
        (setf b (get-num "Enter second number: "))
        startc)
      (startc (null c)
        (setf c (get-num "Enter third number: "))
        a<b)
      (a<b (< a b)
        b<c
        a<c)
      (a<c (< a c)
        (progn (format t "~D~%" a) (reset))
        c>b)
      (b<c (< b c)
        (progn (format t "~D~%" b) (reset))
        c>a)
      (c>a (> c a)
        (progn (format t "~D~%" c) (reset))
        (progn (format t "~D~%" a) (reset)))
      (c>b (> c b)
        (progn (format t "~D~%" c) (reset))
        (progn (format t "~D~%" b) (reset)))) ))

(run *median* 'starta)

