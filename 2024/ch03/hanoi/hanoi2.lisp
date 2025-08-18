;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               hanoi2.lisp
;;;;
;;;;   Started:            Sun Jun  1 19:42:39 2025
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

(defpackage :hanoi2
  (:use :common-lisp :core :io :hanoi :production-system)
  (:shadowing-import-from :hanoi :remove :transfer))

(in-package :hanoi2)

(defun hanoi2 ()
  (let (move temple)
    (loop (if (null temple)
              (setf temple (make-temple :a (add-disks (make-instance 'peg)
                                                      (loop for i from 1 to (get-num "Enter number of disks: "
                                                                                     :test (every-pred #'integerp
                                                                                                       #'(lambda (n) (<= 1 n 6))))
                                                            collect i)))
                    move 0)
              (ecase (mod move 6)
                (0 (with-slots (a b c) temple
                     (multiple-value-bind (*a *b) (transfer a b)
                       (setf temple (make-temple :a *a :b *b :c c :history (add-history temple)))
                       (incf move))))
                (1 (with-slots (a b c) temple
                     (pscond ((and (depletedp a) (depletedp c))
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
                (2 (with-slots (a b c) temple
                     (multiple-value-bind (*b *c) (transfer b c)
                       (setf temple (make-temple :a a :b *b :c *c :history (add-history temple)))
                       (incf move))))
                (3 (with-slots (a b c) temple
                     (pscond ((and (depletedp a) (depletedp b))
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
                (4 (with-slots (a b c) temple
                     (multiple-value-bind (*c *a) (transfer c a)
                       (setf temple (make-temple :a *a :b b :c *c :history (add-history temple)))
                       (incf move))))
                (5 (with-slots (a b c) temple
                     (pscond ((and (depletedp b) (depletedp c))
                            (print-history (make-temple :a a :b b :c c :history (add-history temple)))
                            (setf temple nil))
                           ((can-transfer b c)
                            (multiple-value-bind (*b *c) (transfer b c)
                              (setf temple (make-temple :a a :b *b :c *c :history (add-history temple)))
                              (incf move)))
                           ((can-transfer c b)
                            (multiple-value-bind (*c *b) (transfer c b)
                              (setf temple (make-temple :a a :b *b :c *c :history (add-history temple)))
                              (incf move)))) )))) )))

(defun hanoi2* ()
  (let (move temple)
    (flet ((update (a b c)
             (setf temple (make-temple :a a :b b :c c :history (add-history temple)))
             (incf move)))
      (loop (if (null temple)
                (setf temple (make-temple :a (add-disks (make-instance 'peg)
                                                        (loop for i from 1 to (get-num "Enter number of disks: "
                                                                                       :test (every-pred #'integerp
                                                                                                         #'(lambda (n) (<= 1 n 6))))
                                                              collect i)))
                      move 0)
                (with-slots (a b c) temple
                  (ecase (mod move 6)
                    (0 (multiple-value-bind (*a *b) (transfer a b)
                         (update *a *b c)))
                    (2 (multiple-value-bind (*b *c) (transfer b c)
                         (update a *b *c)))
                    (4 (multiple-value-bind (*c *a) (transfer c a)
                         (update *a b *c)))
                    (1 (pscond ((and (depletedp a) (depletedp c))
                                (print-history (make-temple :a a :b b :c c :history (add-history temple)))
                                (setf temple nil))
                               ((can-transfer a c)
                                (multiple-value-bind (*a *c) (transfer a c)
                                  (update *a b *c)))
                               ((can-transfer c a)
                                (multiple-value-bind (*c *a) (transfer c a)
                                  (update *a b *c)))) )
                    (3 (pscond ((and (depletedp a) (depletedp b))
                                (print-history (make-temple :a a :b b :c c :history (add-history temple)))
                                (setf temple nil))
                               ((can-transfer a b)
                                (multiple-value-bind (*a *b) (transfer a b)
                                  (update *a *b c)))
                               ((can-transfer b a)
                                (multiple-value-bind (*b *a) (transfer b a)
                                  (update *a *b c)))) )
                    (5 (pscond ((and (depletedp b) (depletedp c))
                                (print-history (make-temple :a a :b b :c c :history (add-history temple)))
                                (setf temple nil))
                               ((can-transfer b c)
                                (multiple-value-bind (*b *c) (transfer b c)
                                  (update a *b *c)))
                               ((can-transfer c b)
                                (multiple-value-bind (*c *b) (transfer c b)
                                  (update a *b *c)))) )))) ))))

;;;
;;;    Super refactored!
;;;    Orderd production system
;;;    
(defun hanoi2** ()
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
                      ((terminated)
                       (with-slots (a b c) temple
                         (print-history (make-temple :a a :b b :c c :history (add-history temple))))
                       (setf temple nil))
                      ((eq :even state) ; Better trace with PSCOND than simply T
                       (setf temple (cond-xfer current previous temple)
                             state :odd)
                       (rotatef current next previous)))) ))))
