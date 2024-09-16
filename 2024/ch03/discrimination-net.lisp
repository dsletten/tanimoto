;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               discrimination-net.lisp
;;;;
;;;;   Started:            Tue Aug 27 02:11:34 2024
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
;;;;   - Touretzky
;;;;   - Graham
;;;;   - Slade 20 Questions - add new nodes
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :discrimination-net
  (:use :common-lisp :test :core)
  (:export :defnet 
           :make-node :node :name :test :true :false :run
           :make-discrimination-net :find-node :process-node :add-node :category :nodes
           :action :operation :fire))

(in-package :discrimination-net)

;;;
;;;    The TRUE and FALSE slots are references to other NODEs (or terminal strings!).
;;;    However, referencing other NODE objects directly creates a timing
;;;    problem. What if those instances don't yet exist while the current
;;;    NODE is being instantiated?
;;;    Instead these slots simply hold the names of (possibly future) NODEs.
;;;    
(defclass node ()
  ((name :reader name :initarg :name)
   (test :initarg :test)
   (true :reader true :initarg :true :type (or symbol action))
   (false :reader false :initarg :false :type (or symbol action))))

(defun test (node)
  (with-slots (test) node
    (funcall test)))

(defun make-node (name test true false)
  (make-instance 'node :name name :test test :true (make-case true) :false (make-case false)))

(defun make-case (node-case)
  (if (functionp node-case)
      (make-instance 'action :operation node-case)
      node-case))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream :type t)
    (format stream "~A ~S" (name n) (test n))))

(defclass action ()
  ((operation :reader operation :initarg :operation)))

(defgeneric fire (action)
  (:documentation "Invoke the action's operation."))
(defmethod fire ((a action))
  (funcall (operation a)))

(defclass discrimination-net ()
  ((category :reader category :initarg :category)
   (nodes :reader nodes :initform (make-hash-table))))

(defun make-discrimination-net (category node-list)
  (let ((net (make-instance 'discrimination-net :category category)))
    (with-slots (nodes) net
      (dolist (node node-list)
        (setf (gethash (name node) nodes) node)))
    net))

(defmethod print-object ((net discrimination-net) stream)
  (print-unreadable-object (net stream :type t)
    (format stream "~S Node~P: ~:*~D" (category net) (hash-table-count (nodes net)))) )

(defun prep-node (node)
  (flet ((prep (branch)
           (if (symbolp branch)
               `',branch
               `#'(lambda () ,branch))))
    (destructuring-bind (name test true false) node
      `(list ',name #'(lambda () ,test) ,(prep true) ,(prep false)))) )

(defmacro defnet (sym category &rest node-list)
  (let ((nodes (gensym)))
    `(let ((,nodes (mapcar #'(lambda (node) 
                               (apply #'make-node node))
                           (list ,@(mapcar #'prep-node node-list)))) )
       (defparameter ,sym (make-discrimination-net ',category ,nodes)))) )

(defgeneric find-node (net name)
  (:documentation "Locate a node specified by NAME in NET."))
(defmethod find-node ((net discrimination-net) (name symbol))
  (with-slots (nodes) net
    (gethash name nodes)))

(defgeneric process-node (net name)
  (:documentation "Process the node specified by NAME."))
(defmethod process-node ((net discrimination-net) (name symbol))
  (let ((node (find-node net name)))
    (cond ((null node) (format t "Node ~A not yet defined.~%" name))
          ((test node) (true node))
          (t (false node)))) )

(defgeneric run (net &optional start)
  (:documentation "Evaluate the discrimination net."))
(defmethod run ((net discrimination-net) &optional (start 'start))
  (labels ((execute (current-node)
             (typecase current-node
               (null (format t "At a loss here...~%"))
               (action (fire current-node) (execute start))
               (otherwise (execute (process-node net current-node)))) ))
    (execute start)))
