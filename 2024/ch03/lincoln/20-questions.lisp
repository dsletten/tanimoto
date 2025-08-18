;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               20-questions.lisp
;;;;
;;;;   Started:            Sun Aug 10 21:19:38 2025
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

(defpackage :20-questions (:use :common-lisp :core :io :test :discrimination-net))

(in-package :20-questions)

(defnet *20-questions* "20 Questions Game"
  (people (confirm "Is the person a man?")
    male
    female)
  (male (confirm "Is he living?")
    liveman
    deadman)
  (deadman (confirm "Was he American?")
    us
    them)
  (us (confirm "Is he on a coin?")
    coin
    cidence)
  (coin (confirm "Is the coin a penny?")
    penny
    coins)
  (penny t
    (format t "Lincoln~%")
    (error "How did we get here?")))

(defun play ()
  (handler-case (run *20-questions* 'people)
    (error ()
      (format *error-output* "Sorry, something went wrong...~%"))))

