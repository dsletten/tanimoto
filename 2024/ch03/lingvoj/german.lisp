;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a bean bag--you can sit on a bean bag and squash it, but it will always rise again.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               german.lisp
;;;;
;;;;   Started:            Sun May 11 13:43:55 2025
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
;;;;   Notes: Convert an integer between 0 and 99 into German text.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core")
(load "/home/slytobias/lisp/packages/io")
(load "/home/slytobias/lisp/packages/test")
(load "/home/slytobias/Thelio/modified/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :german (:use :common-lisp :core :io :test :production-system))

(in-package :german)

(defun deutsch ()
  (let ((n nil)
        (s (make-string-output-stream))
        (compoundp nil))
    (flet ((reset () (setf n nil compoundp nil)))
      (loop (if (null n)
                (setf n (get-num "Enter number: " :test (conjoin #'integerp (complement #'minusp))))
                (pscond ((> n 99)
                         (format t "Too big.~%")
                         (reset))
                        ((<= 90 n 99)
                         (setf compoundp "neunzig")
                         (decf n 90))
                        ((<= 80 n 89)
                         (setf compoundp "achtzig")
                         (decf n 80))
                        ((<= 70 n 79)
                         (setf compoundp "siebzig")
                         (decf n 70))
                        ((<= 60 n 69)
                         (setf compoundp "sechzig")
                         (decf n 60))
                        ((<= 50 n 59)
                         (setf compoundp "fünfzig")
                         (decf n 50))
                        ((<= 40 n 49)
                         (setf compoundp "vierzig")
                         (decf n 40))
                        ((<= 30 n 39)
                         (setf compoundp "dreißig")
                         (decf n 30))
                        ((<= 20 n 29)
                         (setf compoundp "zwanzig")
                         (decf n 20))
                        ((= n 19)
                         (format s "neunzehn")
                         (decf n 19))
                        ((= n 18)
                         (format s "achtzehn")
                         (decf n 18))
                        ((= n 17)
                         (format s "siebzehn")
                         (decf n 17))
                        ((= n 16)
                         (format s "sechzehn")
                         (decf n 16))
                        ((= n 15)
                         (format s "fünfzehn")
                         (decf n 15))
                        ((= n 14)
                         (format s "vierzehn")
                         (decf n 14))
                        ((= n 13)
                         (format s "dreizehn")
                         (decf n 13))
                        ((= n 12)
                         (format s "zwölf")
                         (decf n 12))
                        ((= n 11)
                         (format s "elf")
                         (decf n 11))
                        ((= n 10)
                         (format s "zehn")
                         (decf n 10))
                        ((= n 9)
                         (format s "neun")
                         (when compoundp
                           (format s "und~A" compoundp)
                           (setf compoundp nil))
                         (decf n 9))
                        ((= n 8)
                         (format s "acht")
                         (when compoundp
                           (format s "und~A" compoundp)
                           (setf compoundp nil))
                         (decf n 8))
                        ((= n 7)
                         (format s "sieben")
                         (when compoundp
                           (format s "und~A" compoundp)
                           (setf compoundp nil))
                         (decf n 7))
                        ((= n 6)
                         (format s "sechs")
                         (when compoundp
                           (format s "und~A" compoundp)
                           (setf compoundp nil))
                         (decf n 6))
                        ((= n 5)
                         (format s "fünf")
                         (when compoundp
                           (format s "und~A" compoundp)
                           (setf compoundp nil))
                         (decf n 5))
                        ((= n 4)
                         (format s "vier")
                         (when compoundp
                           (format s "und~A" compoundp)
                           (setf compoundp nil))
                         (decf n 4))
                        ((= n 3)
                         (format s "drei")
                         (when compoundp
                           (format s "und~A" compoundp)
                           (setf compoundp nil))
                         (decf n 3))
                        ((= n 2)
                         (format s "zwei")
                         (when compoundp
                           (format s "und~A" compoundp)
                           (setf compoundp nil))
                         (decf n 2))
                        ((= n 1)
                         (if compoundp
                             (format s "ein")
                             (format s "eins"))
                         (when compoundp
                           (format s "und~A" compoundp)
                           (setf compoundp nil))
                         (decf n 1))
                        ((= n 0)
                         (when compoundp
                           (format s compoundp)
                           (setf compoundp nil))
                         (format t "~A~%" (get-output-stream-string s))
                         (reset)) )))) ))
