;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               binary-tree.lisp
;;;;
;;;;   Started:            Wed Jul 17 16:47:51 2024
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

(defpackage :binary-tree (:use :common-lisp :core :test))

(in-package :binary-tree)

;;;
;;;    SBCL enforce slot :TYPE
;;;    
(declaim (optimize safety))

;(deftype binary-tree () '(or integer node))

(defclass binary-tree ()
  ((root :accessor root :initarg :root :type (or integer node))))

(defclass node ()
  ((left :accessor left :initarg :left :type (or integer node))
   (right :accessor right :initarg :right :type (or integer node))))

;'((3 . (2 . 5)) . (7 . (2 . 1)))
;'(((1 . 2) . (3 . 4)) . ((5 . (6 . 7)) . 8))
;'(1 . (8 . (2 . (7 . (3 . (6 . (4 . 5)))))))
(defun make-tree (nodes)
  (labels ((make-node (tree)
             (cond ((atom tree) tree)
                   (t (make-instance 'node
                                     :left (make-node (car tree))
                                     :right (make-node (cdr tree)))) )))
    (make-instance 'binary-tree :root (make-node nodes))))

;; '((3 (2 5)) (7 (2 1)))
;; '(((1 2) (3 4)) ((5 (6 7)) 8))
;; '(1 (8 (2 (7 (3 (6 (4 5)))) )))
(defun make-tree* (nodes)
  (labels ((make-node (tree)
             (cond ((atom tree) tree)
                   (t (make-instance 'node
                                     :left (make-node (first tree))
                                     :right (make-node (second tree)))) )))
    (make-instance 'binary-tree :root (make-node nodes))))

(defun treemax (tree)
  (etypecase tree
    (binary-tree (treemax (root tree)))
    (integer tree)
    (node (with-slots (left right) tree
            (max (treemin left)
                 (treemin right)))) ))

(defun treemin (tree)
  (etypecase tree
    (binary-tree (treemin (root tree)))
    (integer tree)
    (node (with-slots (left right) tree
            (min (treemax left)
                 (treemax right)))) ))

(deftest test-minmax ()
  (check
   (= 7 (treemax (make-tree 7)))
   (= 7 (treemin (make-tree 7)))
   (= 7 (treemax (make-tree '(7 . (2 . 1)))) )
   (= 2 (treemin (make-tree '(7 . (2 . 1)))) )
   (= 3 (treemax (make-tree '((3 . (2 . 5)) . (7 . (2 . 1)))) ))
   (= 3 (treemin (make-tree '((3 . (2 . 5)) . (7 . (2 . 1)))) ))
   (= 6 (treemax (make-tree '(((1 . 2) . (3 . 4)) . ((5 . (6 . 7)) . 8)))) )
   (= 3 (treemin (make-tree '(((1 . 2) . (3 . 4)) . ((5 . (6 . 7)) . 8)))) )
   (= 5 (treemax (make-tree '(1 . (8 . (2 . (7 . (3 . (6 . (4 . 5)))) )))) ))
   (= 1 (treemin (make-tree '(1 . (8 . (2 . (7 . (3 . (6 . (4 . 5)))) )))) ))))

;; (treemax (make-tree* '((3 (2 5)) (7 (2 1)))) ))
;; 3
;; * (treemin (make-tree* '((3 (2 5)) (7 (2 1)))) ))
;; 3
