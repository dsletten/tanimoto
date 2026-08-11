;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a bean bag--you can sit on a bean bag and squash it, but it will always rise again.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               hanoi-dn.lisp
;;;;
;;;;   Started:            Thu Jun  5 22:11:20 2025
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
(load "/home/slytobias/lisp/packages/core")
(load "/home/slytobias/lisp/packages/io")
(load "/home/slytobias/lisp/packages/hanoi")
;(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/discrimination-net.lisp")
(load "/home/slytobias/Thelio/modified/lisp/books/Tanimoto/2024/ch03/discrimination-net.lisp")

(defpackage :hanoi-dn
  (:use :common-lisp :core :io :hanoi :discrimination-net)
  (:shadowing-import-from :hanoi :remove :transfer))

(in-package :hanoi-dn)

;; (let (temple state)
;;   (defnet *hanoi* "Tower of Hanoi"
;;     (start (null temple)
;;       (setf temple (make-temple :a (add-disks (make-instance 'peg)
;;                                               (loop for i from 1 to (get-num "Enter number of disks: "
;;                                                                              :test (conjoin #'integerp
;;                                                                                                #'(lambda (n) (<= 1 n 6))))
;;                                                     collect i)))
;;             state 'transfer-ab)
;;       transfer-ab)
;;     (transfer-ab (eq state 'transfer-ab)
;;       (with-slots (a b c) temple
;;         (multiple-value-bind (*a *b) (transfer a b)
;;           (setf temple (make-temple :a *a :b *b :c c :history (add-history temple))
;;                 state 'choose-ac)))
;;       choose-ac)
;;     (transfer-bc (eq state 'transfer-bc)
;;       (with-slots (a b c) temple
;;         (multiple-value-bind (*b *c) (transfer b c)
;;           (setf temple (make-temple :a a :b *b :c *c :history (add-history temple))
;;                 move 'choose-ab)))
;;       choose-ab)
;;     (transfer-ca (eq state 'transfer-ca)
;;       (with-slots (a b c) temple
;;         (multiple-value-bind (*c *a) (transfer c a)
;;           (setf temple (make-temple :a *a :b b :c *c :history (add-history temple))
;;                 state 'choose-bc)))
;;       choose-bc)
;;     (choose-ac

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
    (defnet *hanoi* "Tower of Hanoi"
      (start (null temple)
        (setf temple (make-temple :a (add-disks (make-instance 'peg)
                                                (loop for i from 1 to (get-num "Enter number of disks: "
                                                                               :test (conjoin #'integerp
                                                                                                 #'(lambda (n) (<= 1 n 6))))
                                                      collect i)))
              current :a
              next :b
              previous :c
              state :odd)
        odd-state)
      (odd-state (eq :odd state)
        (setf temple (xfer current next temple)
              state :even)
        terminated)
      (terminated (and (depletedp (get-peg current temple)) (depletedp (get-peg previous temple)))
        (with-slots (a b c) temple
          (print-history (make-temple :a a :b b :c c :history (add-history temple)))
          (setf temple nil))
        (progn (setf temple (cond-xfer current previous temple)
                     state :odd)
               (rotatef current next previous)))) ))
