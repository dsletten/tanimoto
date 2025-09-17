;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;    What I like about Lisp is that you can feel the bits between your toes.
;;;;    -- Drew McDermott
;;;;
;;;;    Name:               shrink.lisp
;;;;
;;;;    Started:            Wed Sep 10 14:15:59 2025
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
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/shrink/match.lisp")

(defpackage :shrink
  (:use :common-lisp :core :test :match :io)
  (:shadowing-import-from :match :*))

(in-package :shrink)

(defun get-input (&key verbose)
  (let ((*read-eval* nil))
    (handler-case (mapcar #'read-from-string
                          (read-words (make-string-input-stream (read-line))))
      (error (e)
        (when verbose (format *error-output* "Your input is not so good: ~A~%" e))
        nil))))

(defun shrink ()
  (format t "Welcome to my sofa!~%")
  (let ((wword-count 0)
        (punt-count 0))
    (labels ((you-are-match (input)
               (multiple-value-bind (match subs) (match '(you are (* x)) input)
                 (when match
                   (incf wword-count)
                   (format t "Please tell me ~A you are ~A~%" (wword wword-count) (rest (assoc 'x subs))))
                 match))
             (you-have-match (input)
               (multiple-value-bind (match subs) (match '(you have (* x)) input)
                 (when match
                   (format t "How long have you had ~A~%" (rest (assoc 'x subs))))
                 match))
             (you-feel-match (input)
               (multiple-value-bind (match subs) (match '(you feel (* x)) input)
                 (declare (ignore subs))
                 (when match
                   (format t "I sometimes feel the same way.~%"))
                 match))
             (because-match (input)
               (multiple-value-bind (match subs) (match '(because (* x)) input)
                 (declare (ignore subs))
                 (when match
                   (format t "Is that really the reason?~%"))
                 match))
             (yes-match (input) "Yes"
               (multiple-value-bind (match subs) (match '(yes (* x)) input)
                 (when match
                   (format t "How can you be so sure ~A?~%" (rest (assoc 'x subs))))
                 match))
             (me-are-match (input)
               (multiple-value-bind (match subs) (match '(me are (* x)) input) ; ?????????????
                 (when match
                   (format t "Oh yeah I am ~A~%" (rest (assoc 'x subs))))
                 match))
             (verb-match (input)
               (multiple-value-bind (match subs) (match '((verbp v) (* x)) input)
                 (when match
                   (format t "Why do you want me to ~A ~A?~%" (rest (assoc 'v subs)) (rest (assoc 'x subs))))
                 match))
             (question-match (input)
               (multiple-value-bind (match subs) (match '((questionp w) (* x)) input) ; X??
                 (when match
                   (format t "You tell me ~A.~%" (rest (assoc 'w subs))))
                 match))
             (think-match (input)
               ;; (multiple-value-bind (match subs) (match '(do me think (* x)) input) ; X??
               ;;   (when match
               (let ((match (match '(do me think (* x)) input)))
                 (when match
                   (format t "I think that you should answer that yourself.~%"))
                 match))
             (auxiliary-match (input)
               (multiple-value-bind (match subs) (match '((auxiliaryp w) me (* x)) input)
                 (when match
                   (format t "Perhaps I ~A ~A.~%" (rest (assoc 'w subs)) (rest (assoc 'x subs))))
                 match))
             )
      (loop (let ((input (map-you-me (get-input))))
;              (terpri)
;            (force-output *terminal-io*)
              (cond ((match '(bye) input) (return 'goodbye))
                    ((you-are-match input))
                    ((you-have-match input))
                    ((you-feel-match input))
                    ((because-match input))
                    ((match '() input) (format t "Please say something!~%"))
                    ((yes-match input))
                    ((me-are-match input))
                    ((verb-match input))
                    ((question-match input))
                    ((think-match input))
                    ((auxiliary-match input))
                    ((member 'dream input) (format t "For dream analysis see Freud.~%"))
                    ((member 'love input) (format t "All is fair in love and war.~%"))
                    ((member 'no input) (format t "Don't be so negative.~%"))
                    ((member 'maybe input) (format t "Be more decisive!~%"))
                    ((member 'you input) (format t "~A.~%" input))
                    (t (write-line (punt (incf punt-count)))) )))) ))


(defun you-me (word)
  (case word
    (i 'you)
    (me 'you)
    (you 'me) ;;;; 
    (my 'your)
    (your 'my)
    (yours 'mine)
    (mine 'yours)
    (am 'are)
    (are 'am)
    (otherwise word)))

(defun map-you-me (list)
  (mapcar #'you-me list))

;; (map-you-me '(i am the one that you need to treat))
;; (YOU ARE THE ONE THAT ME NEED TO TREAT)

(let ((wwords #("when" "why" "where" "how")))
  (defun wword (i)
    (aref wwords (mod i (length wwords)))) )

(defun questionp (word)
  (member word '(why where when what which how)))

(defun auxiliaryp (word)
  (member word '(do can should would)))

(let ((punts #("Please go on."
               "Tell me more."
               "I see."
               "What does that indicate?"
               "But why be concerned about it?"
               "Just tell me how you feel.")))
  (defun punt (punt-count)
    (aref punts (mod punt-count (length punts)))) )

(defun verbp (word)
  (member word '(go have be try eat take help make get jump
                 write type fill put turn compute
                 think drink blink crash crunch add)))
