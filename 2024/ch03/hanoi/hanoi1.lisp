;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               hanoi1.lisp
;;;;
;;;;   Started:            Sun Jun  1 18:36:08 2025
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
(load "/home/slytobias/lisp/packages/hanoi.lisp")
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/production-system.lisp")

(defpackage :hanoi1
  (:use :common-lisp :core :io :hanoi :production-system)
  (:shadowing-import-from :hanoi :remove :transfer))

(in-package :hanoi1)

(defun hanoi1 ()
  (let (move temple)
    (loop (pscond ((null temple)
                 (setf temple (make-temple :a (add-disks (make-instance 'peg)
                                                         (loop for i from 1 to (get-num "Enter number of disks: "
                                                                                        :test (every-pred #'integerp
                                                                                                          #'(lambda (n) (<= 1 n 6))))
                                                               collect i)))
                       move 0))
                ((and (not (null temple))
                      (= 0 (mod move 6)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*a *b) (transfer a b)
                     (setf temple (make-temple :a *a :b *b :c c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple))
                      (= 1 (mod move 6)))
                 (with-slots (a b c) temple
                   (cond ((and (depletedp a) (depletedp c))
                          (print-history (make-temple :a a :b b :c c :history (add-history temple)))
                          (setf temple nil))
                         ((can-transfer a c)
                          (multiple-value-bind (*a *c) (transfer a c)
                            (setf temple (make-temple :a *a :b b :c *c :history (add-history temple)))
                            (incf move)))
                         ((can-transfer c a)
                          (multiple-value-bind (*c *a) (transfer c a)
                            (setf temple (make-temple :a *a :b b :c *c :history (add-history temple)))
                            (incf move)))) ))
                ((and (not (null temple))
                      (= 2 (mod move 6)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*b *c) (transfer b c)
                     (setf temple (make-temple :a a :b *b :c *c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple))
                      (= 3 (mod move 6)))
                 (with-slots (a b c) temple
                   (cond ((and (depletedp a) (depletedp b))
                          (print-history (make-temple :a a :b b :c c :history (add-history temple)))
                          (setf temple nil))
                         ((can-transfer a b)
                          (multiple-value-bind (*a *b) (transfer a b)
                            (setf temple (make-temple :a *a :b *b :c c :history (add-history temple)))
                            (incf move)))
                         ((can-transfer b a)
                          (multiple-value-bind (*b *a) (transfer b a)
                            (setf temple (make-temple :a *a :b *b :c c :history (add-history temple)))
                            (incf move)))) ))
                ((and (not (null temple))
                      (= 4 (mod move 6)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*c *a) (transfer c a)
                     (setf temple (make-temple :a *a :b b :c *c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple))
                      (= 5 (mod move 6)))
                 (with-slots (a b c) temple
                   (cond ((and (depletedp b) (depletedp c))
                          (print-history (make-temple :a a :b b :c c :history (add-history temple)))
                          (setf temple nil))
                         ((can-transfer b c)
                          (multiple-value-bind (*b *c) (transfer b c)
                            (setf temple (make-temple :a a :b *b :c *c :history (add-history temple)))
                            (incf move)))
                         ((can-transfer c b)
                          (multiple-value-bind (*c *b) (transfer c b)
                            (setf temple (make-temple :a a :b *b :c *c :history (add-history temple)))
                            (incf move)))) )))) ))

;;;
;;;    Simpler but more verbose production rules...
;;;    
(defun hanoi1* ()
  (let (move temple)
    (loop (cond ((null temple)
                 (setf temple (make-temple :a (add-disks (make-instance 'peg)
                                                         (loop for i from 1 to (get-num "Enter number of disks: "
                                                                                        :test (every-pred #'integerp
                                                                                                          #'(lambda (n) (<= 1 n 6))))
                                                               collect i)))
                       move 0))
                ((and (not (null temple)) (= 0 (mod move 6)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*a *b) (transfer a b)
                     (setf temple (make-temple :a *a :b *b :c c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple)) (= 1 (mod move 6)) (depletedp (a temple)) (depletedp (c temple)))
                 (with-slots (a b c) temple
                   (print-history (make-temple :a a :b b :c c :history (add-history temple)))
                   (setf temple nil)))
                ((and (not (null temple)) (= 1 (mod move 6)) (can-transfer (a temple) (c temple)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*a *c) (transfer a c)
                     (setf temple (make-temple :a *a :b b :c *c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple)) (= 1 (mod move 6)) (can-transfer (c temple) (a temple)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*c *a) (transfer c a)
                     (setf temple (make-temple :a *a :b b :c *c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple)) (= 2 (mod move 6)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*b *c) (transfer b c)
                     (setf temple (make-temple :a a :b *b :c *c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple)) (= 3 (mod move 6)) (depletedp (a temple)) (depletedp (b temple)))
                 (with-slots (a b c) temple
                   (print-history (make-temple :a a :b b :c c :history (add-history temple)))
                   (setf temple nil)))
                ((and (not (null temple)) (= 3 (mod move 6)) (can-transfer (a temple) (b temple)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*a *b) (transfer a b)
                     (setf temple (make-temple :a *a :b *b :c c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple)) (= 3 (mod move 6)) (can-transfer (b temple) (a temple)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*b *a) (transfer b a)
                     (setf temple (make-temple :a *a :b *b :c c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple)) (= 4 (mod move 6)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*c *a) (transfer c a)
                     (setf temple (make-temple :a *a :b b :c *c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple)) (= 5 (mod move 6)) (depletedp (b temple)) (depletedp (c temple)))
                 (with-slots (a b c) temple
                   (print-history (make-temple :a a :b b :c c :history (add-history temple)))
                   (setf temple nil)))
                ((and (not (null temple)) (= 5 (mod move 6)) (can-transfer (b temple) (c temple)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*b *c) (transfer b c)
                     (setf temple (make-temple :a a :b *b :c *c :history (add-history temple)))
                     (incf move))))
                ((and (not (null temple)) (= 5 (mod move 6)) (can-transfer (c temple) (b temple)))
                 (with-slots (a b c) temple
                   (multiple-value-bind (*c *b) (transfer c b)
                     (setf temple (make-temple :a a :b *b :c *c :history (add-history temple)))
                     (incf move)))) ))))


;;;
;;;    Super refactored!
;;;    Unorderd production system
;;;    
(defun hanoi1** ()
  (labels ((xfer (src dest temple)
             (multiple-value-bind (src* dest*) (transfer (get-peg src temple) (get-peg dest temple))
               (with-slots (a b c) temple
                 (apply #'make-temple (list src src* dest dest* :a a :b b :c c :history (add-history temple)))) ))
           (get-peg (k temple)
             (ecase k
               (:a (a temple))
               (:b (b temple))
               (:c (c temple))))
           (cond-xfer (current previous temple)
             (if (can-transfer (get-peg current temple) (get-peg previous temple))
                 (xfer current previous temple)
                 (xfer previous current temple))))
    (let (temple current next previous state)
      (labels ((terminated ()
                 (and (depletedp (get-peg current temple))
                      (depletedp (get-peg previous temple)))) )
        (loop (pscond ((null temple)
                       (setf temple (make-temple :a (add-disks (make-instance 'peg)
                                                               (loop for i from 1 to (get-num "Enter number of disks: "
                                                                                              :test (every-pred #'integerp
                                                                                                                #'(lambda (n) (<= 1 n 6))))
                                                                     collect i)))
                             current :a
                             next :b
                             previous :c
                             state :odd))
                      ((eq :odd state)
                       (setf temple (xfer current next temple)
                             state :even))
                      ((and (eq :even state) (terminated))
                       (with-slots (a b c) temple
                         (print-history (make-temple :a a :b b :c c :history (add-history temple))))
                       (setf temple nil))
                      ((and (eq :even state) (not (terminated)))
                       (setf temple (cond-xfer current previous temple)
                             state :odd)
                       (rotatef current next previous)))) ))))
