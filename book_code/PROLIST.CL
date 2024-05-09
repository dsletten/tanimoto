;;; PROLIST.CL
;;; Experiments in list manipulation using the mock PROLOG interpreter.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 6 ("Logical Reasoning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; We adopt the convention that a list is represented as a logical term:
;;; Thus the term (CONS A (CONS B EMPTY-LIST)) represents the list (A B).
;;; The use of the Lisp symbol CONS is only to communicate the intent of
;;; these terms --- these terms are never evaluated.

(setf database3 '(

((empty empty-list))           ; Predicate that tests if a list is empty.

((prolog-first x (cons x r)))  ; True if X is first on list.

((prolog-rest r (cons x r)))   ; True if R is rest of list.

;;; Now let's give a definition for a member function.
;;; (PROLOG-MEMBER X LST) is true if X is a member of the list LST.
((prolog-member x lst)(prolog-first x lst)) ; True if X is first on list
((prolog-member x lst)(prolog-rest lst2 lst)(prolog-member x lst2)) ; or recursively true.

;;; The following forces binding to an example list:
((long-example (cons a (cons b (cons c (cons d empty-list)))))) 
) )

;;; Let's test these functions.
(setf *database* database3)

(query '((prolog-first x (cons a (cons b empty-list)))))

(query '((long-example x)(prolog-first y x)))

(query '((long-example x)(prolog-rest  y x)))

(query '((prolog-member x (cons a (cons b (cons c empty-list))))))

;;; Now here are definitions for deletion, insertion and permutation.

(setf database4 '(

;;; Deleting X from (CONS X Y) leaves Y.
((prolog-delete x (cons x y) y))
;;; Deleting X from (CONS Y Z) gives (CONS Y W) provided
;;;  deleting X from Z gives W.
((prolog-delete x (cons y z) (cons y w))(prolog-delete x z w))

;;; Inserting X into Y gives Z provided deleting X from Z gives Y.
((prolog-insert x y z)(prolog-delete x z y))

;;; This version of PERMUTATION generates all permutations and then goes
;;;  into an infinite loop!
((permutation empty-list empty-list))
;;; We get a permutation of Z by consing X onto Y provided that
;;; Y is a permutation of W and we can get Z by inserting X somewhere in W.
((permutation (cons x y) z)(permutation y w)(prolog-insert x w z))
))

(setf *database* database4)

;;; Test the delete predicate:
(query '((prolog-delete b (cons a (cons b (cons c empty-list))) x)))

;;; Test the permutation predicate.  Be ready to abort the Prolog evaluation
;;; after the last permutation has been printed.
(query '((permutation x (cons a (cons b (cons c empty-list))))))

