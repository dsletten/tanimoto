;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               primes-dn.lisp
;;;;
;;;;   Started:            Sun Jun  1 13:46:01 2025
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
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/discrimination-net.lisp")

(defpackage :primes-dn (:use :common-lisp :core :io :discrimination-net))

(in-package :primes-dn)

(let (n factor limit)
  (flet ((prime (n) (format t "~D is prime.~%" n))
         (not-prime (n) (format t "~D is not prime.~%" n)))
    (defnet *primes* "Primality test"
      (start (null n)
        (setf n (get-num "Enter number: " :test (conjoin #'integerp #'plusp))
              factor 3
              limit (isqrt n))
        even?)
      (even? (evenp n)
        =2
        =1)
      (=2 (= n 2)
        (progn (prime n) (setf n nil))
        (progn (not-prime n) (setf n nil)))
      (=1 (= n 1)
        (progn (not-prime n) (setf n nil))
        >limit)
      (>limit (> factor limit)
        (progn (prime n) (setf n nil))
        divisible?)
      (divisible? (zerop (mod n factor))
        (progn (not-prime n) (setf n nil))
        (incf factor 2)))) )

(let (n
      factor
      limit
      (cache (make-hash-table :test #'eql)))
  (flet ((prime (n)
           (setf (gethash n cache) t)
           (format t "~D is prime.~%" n))
         (not-prime (n)
           (setf (gethash n cache) nil)
           (format t "~D is not prime.~%" n)))
    (defnet *primes-cache* "Primality test with cache"
      (start (null n)
        (setf n (get-num "Enter number: " :test (conjoin #'integerp #'plusp))
              factor 3
              limit (isqrt n))
        cached?)
      (cached? (nth-value 1 (gethash n cache))
        (progn (funcall (if (gethash n cache) #'prime #'not-prime) n) (setf n nil))
        even?)
      (even? (evenp n)
        =2
        =1)
      (=2 (= n 2)
        (progn (prime n) (setf n nil))
        (progn (not-prime n) (setf n nil)))
      (=1 (= n 1)
        (progn (not-prime n) (setf n nil))
        >limit)
      (>limit (> factor limit)
        (progn (prime n) (setf n nil))
        divisible?)
      (divisible? (zerop (mod n factor))
        (progn (not-prime n) (setf n nil))
        (progn (incf factor 2)
               (format t "New factor: ~D~%" factor)))) ))

;;
;;    Check whether FACTOR is prime (Cache as necessary)
;;    Skip if not prime?
;;    Must preserve/restore state of database!
;;    
(let (n
      factor
      limit
      abort
      (cache (make-hash-table :test #'eql)))
  (flet ((prime (n &optional cachedp)
           (unless cachedp
             (setf (gethash n cache) t))
           (format t "~D is prime. ~:[✘~;✔~]~%" n cachedp))
         (not-prime (n &optional cachedp)
           (unless cachedp
             (setf (gethash n cache) nil))
           (format t "~D is not prime. ~:[✘~;✔~]~%" n cachedp)))
    (defnet *primes-cache-deluxe* "Primality test with cache. Factors cached too."
      (start (null n)
        (if (functionp abort)
            (funcall abort) ; Non-local exit
            (setf n (get-num "Enter number: " :test (conjoin #'integerp #'plusp))
                  factor 3
                  limit (isqrt n)))
        cached?)
      (cached? (nth-value 1 (gethash n cache))
        (progn (funcall (if (gethash n cache) #'prime #'not-prime) n t) (setf n nil))
        even?)
      (even? (evenp n)
        =2
        =1)
      (=2 (= n 2)
        (progn (prime n) (setf n nil))
        (progn (not-prime n) (setf n nil)))
      (=1 (= n 1)
        (progn (not-prime n) (setf n nil))
        >limit)
      (>limit (> factor limit)
        (progn (prime n) (setf n nil))
        prime-factor?)
      (prime-factor? (cond ((nth-value 1 (gethash factor cache))
                            (format t "Cached factor: ~D ~:[C~;P~]~%" factor (gethash factor cache))
                            (gethash factor cache))
                           (t (let ((n* n)
                                    (factor* factor)
                                    (limit* limit))
                                (block check-factor
                                  (setf n factor
                                        factor 3
                                        limit (isqrt n)
                                        abort #'(lambda () (return-from check-factor)))
                                  (run *primes-cache-deluxe*))
                                (setf n n*
                                      factor factor*
                                      limit limit*
                                      abort nil))
                              (gethash factor cache)))
        divisible?
        next-factor)
      (divisible? (zerop (mod n factor))
        (progn (not-prime n) (setf n nil))
        next-factor)
      (next-factor t ; ???
        (progn (incf factor 2)
               (format t "New factor: ~D~%" factor))
        (error "How did we get here?")))) )


