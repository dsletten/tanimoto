;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               permutation-stream.lisp
;;;;
;;;;   Started:            Sat Jan  1 21:21:33 2011
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

(defpackage elements (:use common-lisp))

(in-package elements)

(defclass permutation-stream ()
  ((elements :initarg :elements)
   (stream)))

(defmethod initialize-instance :after ((s permutation-stream) &rest args)
  (setf (slot-value s 'stream) (make-stream (length (slot-value s 'elements)) (slot-value s 'elements))))

(defun make-stream (n elements)
  (let ((sequence (make-array (list n) :element-type 'integer)))
    (loop for i from 0 below n
          do (setf (aref sequence i) i))
    #'(lambda ()
        (if (null sequence)
            nil
            (prog1 (translate sequence elements) 
              (loop
                 (setf sequence (increment-sequence sequence))
;(print sequence)
                 (when (or (null sequence)
                           (equalp sequence (remove-duplicates sequence)))
                   (return)))) ))))

(defun increment-sequence (seq)
  (let ((size (length seq)))
    (loop for i from (1- size) downto 0
          do (incf (aref seq i))
          if (>= (aref seq i) size)
          do (setf (aref seq i) 0)
          else
          do (return seq)
          end
          finally (return nil))))

(defun translate (seq elts)
  (map 'list #'(lambda (i) (elt elts i)) seq))

(defgeneric next-permutation (s))

(defmethod next-permutation ((s permutation-stream))
  (funcall (slot-value s 'stream)))
