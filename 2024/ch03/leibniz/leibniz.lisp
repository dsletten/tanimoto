#!/usr/bin/sbcl --script
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;    Lisp isn't a language, it's a building material.
;;;;    -- Alan Kay
;;;;
;;;;    Name:               leibniz.lisp
;;;;
;;;;    Started:            Mon Aug  3 19:07:42 2026
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
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :leibniz (:use :common-lisp :core :test))

(in-package :leibniz)

