;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a bean bag--you can sit on a bean bag and squash it, but it will always rise again.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               production-system.lisp
;;;;
;;;;   Started:            Sat Aug 10 19:45:29 2024
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
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :production-system
  (:use :common-lisp :core :test)
  (:export :defproductions :scan))

(in-package :production-system)

(defun build-rules (rules)
  (loop for (name condition . actions) in rules
        collect `(list ',name #'(lambda () ,condition) #'(lambda () ,@actions))))

(defmacro defproductions ((&rest bindings) &body rules)
  (let ((table (make-symbol "PRODUCTION-RULES"))
        (name (make-symbol "NAME"))
        (condition (make-symbol "CONDITION"))
        (action (make-symbol "ACTION")))
    `(let ((,table (make-hash-table))
           ,@bindings)
       (loop for (,name ,condition ,action) in (list ,@(build-rules rules))
             do (setf (gethash ,name ,table) (list ,condition ,action))
             finally (return ,table)))) )

(defun scan (production-rules &key verbose debug)
  (loop for name being the hash-keys in production-rules using (hash-value rule)
        do (destructuring-bind (condition action) rule
             (let ((triggerp (funcall condition)))
               (cond ((not triggerp) (when verbose (format *error-output* "~A failed.~%" name)))
                     (t (when debug (format *error-output* "~A fired.~%" name))
                        (funcall action)
                        (return)))) )))

(defun execute (production-system &key verbose debug)
  (loop (scan production-system :verbose verbose :debug debug)))
