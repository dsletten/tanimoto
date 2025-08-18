;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               auto-dn.lisp
;;;;
;;;;   Started:            Sat Aug  9 20:27:40 2025
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
;;;;   Notes: Touretzky ch. 12 Keyboard Exercise (374 页)
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/discrimination-net.lisp")

(defpackage :auto-dn (:use :common-lisp :core :io :test :discrimination-net))

(in-package :auto-dn)

(defnet *automotive-diagnostic* "Diagnose problems with automobile"
  (start (confirm "Does the engine turn over?")
    engine-turns-over
    engine-wont-turn-over)
  (engine-turns-over (confirm "Will the engine run for any period of time?") 
    engine-will-run-briefly
    engine-wont-run)
  (engine-wont-run (confirm "Is there gas in the tank?")
    gas-in-tank
    (format t "Fill the tank and try starting the engine again.~%"))
  (engine-wont-turn-over (confirm "Do you hear any sound when you turn the key?")
    sound-when-turn-key
    no-sound-when-turn-key)
  (no-sound-when-turn-key (confirm "Is the battery voltage low?")
    (format t "Replace the battery~%")
    battery-voltage-ok)
  (battery-voltage-ok (confirm "Are the battery cables dirty or loose?")
    (format t "Clean the cables and tighten the connections.~%")
    battery-cables-good))

(defun diagnose ()
  (handler-case (run *automotive-diagnostic*)
    (error ()
      (format *error-output* "Sorry, something went wrong...~%"))))


