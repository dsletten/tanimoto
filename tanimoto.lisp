;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   NAME:               tanimoto.lisp
;;;;
;;;;   STARTED:            Fri Aug  9 00:25:31 2002
;;;;   MODIFICATIONS:
;;;;
;;;;   PURPOSE: This file contains code needed by multiple programs from the
;;;;   specific chapter subdirectories of Tanimoto's book.
;;;;
;;;;   CALLING SEQUENCE:
;;;;
;;;;
;;;;   INPUTS:
;;;;
;;;;   OUTPUTS:
;;;;
;;;;   EXAMPLE:
;;;;
;;;;   NOTES:
;;;;
;;;;

(defmacro defrules (rules)
  (let ((result ()))
    (dolist (rule rules)
      (push `(list #'(lambda () ,(first rule))
	           #'(lambda () ,@(rest rule))) result))
    (cons 'list (nreverse result))))

