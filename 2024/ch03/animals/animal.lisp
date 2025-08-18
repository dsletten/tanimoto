;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               animal.lisp
;;;;
;;;;   Started:            Sun Aug 10 21:27:20 2025
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
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/discrimination-net.lisp")

(defpackage :animal (:use :common-lisp :core :io :test :discrimination-net))

(in-package :animal)

(defnet *animals* "Animals 20 Questions"
  (purr (confirm "Does it purr?")
    gasoline
    gray)
  (gasoline (confirm "Does it run on regular gas?")
    (format t "Ferrari~%")
    (format t "cat~%"))
  (gray (confirm "Is it gray?")
    (format t "elephant~%")
    person-sized)
  (person-sized (confirm "Is it bigger than a person?")
    stripes
    tail)
  (stripes (confirm "Does it have stripes?")
    (format t "tiger~%")
    domestic)
  (domestic (confirm "Is it a domestic animal?")
    (format t "horse~%")
    (format t "gorilla~%"))
  (tail (confirm "Can it regrow a tail?")
    (format t "gecko~%")
    (format t "monkey~%")))

(defun play ()
  (handler-case (run *animals* 'purr)
    (error ()
      (format *error-output* "Sorry, something went wrong...~%"))))
