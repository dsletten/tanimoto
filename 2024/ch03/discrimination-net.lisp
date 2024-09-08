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
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :discrimination-net
  (:use :common-lisp :test :core)
  (:export :defnet 
           :make-node :node :name :test :true :false 
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

;; (defmacro defnet (sym category &rest node-list)
;;   (let ((nodes (mapcar #'(lambda (node) 
;;                            (apply #'make-node node))
;;                        node-list)))
;;   `(defparameter ,sym (make-discrimination-net ',category (list ,@nodes)))) )

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

(let ((x nil))
  (defnet *roman* "Roman numerals"
    (start (null x) (setf x (get-num "Enter number: " :test #'integerp)) greater4)
    (greater4 (> x 4) greater9 equal4)
    (greater9 (> x 9) greater39 equal9)
    (greater39 (> x 39) (progn (format t "Too big.~%") (setf x nil)) (progn (format t "X") (decf x 10)))
    (equal9 (= x 9) (progn (format t "IX") (setf x 0)) (progn (format t "V") (decf x 5)))
    (equal4 (= x 4) (progn (format t "IV") (setf x 0)) positive)
    (positive (> x 0) (progn (format t "I") (decf x)) (progn (format t "~%") (setf x nil)))) )

(let ((x nil))
  (defnet *roman3999* "Roman numerals"
    (start (null x) (setf x (get-num "Enter number: " :test #'integerp)) >=100)
    (>=100 (>= x 100) >=500 >=50)
    (>=500 (>= x 500) >=1000 >=400)
    (>=1000 (>= x 1000) >3999 >=900)
    (>3999 (> x 3999) (progn (format t "Too big.~%") (setf x nil)) (progn (format t "M") (decf x 1000)))
    (>=900 (>= x 900) (progn (format t "CM") (decf x 900)) (progn (format t "D") (decf x 500)))
    (>=400 (>= x 400) (progn (format t "CD") (decf x 400)) (progn (format t "C") (decf x 100)))
    (>=50 (>= x 50) >=90 >=10)
    (>=90 (>= x 90) (progn (format t "XC") (decf x 90)) (progn (format t "L") (decf x 50)))
    (>=10 (>= x 10) >=40 >=5)
    (>=40 (>= x 40) (progn (format t "XL") (decf x 40)) (progn (format t "X") (decf x 10)))
    (>=5 (>= x 5) =9 >0)
    (=9 (= x 9) (progn (format t "IX") (decf x 9)) (progn (format t "V") (decf x 5)))
    (>0 (> x 0) =4 (progn (format t "~%") (setf x nil)))
    (=4 (= x 4) (progn (format t "IV") (decf x 4)) (progn (format t "I") (decf x)))) )
