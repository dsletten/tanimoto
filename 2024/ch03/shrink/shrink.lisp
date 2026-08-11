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
;;;;    Notes: Incorporates rules from PAIP 172 页 as well as capacity to
;;;;    generate various responses to a given input.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core")
(load "/home/slytobias/lisp/packages/io")
(load "/home/slytobias/lisp/packages/strings")
(load "/home/slytobias/lisp/packages/test")
;(load "/home/slytobias/lisp/books/Tanimoto/2024/ch03/shrink/match.lisp")
(load "/home/slytobias/Thelio/modified/lisp/books/Tanimoto/2024/ch03/shrink/match.lisp")

(defpackage :shrink
  (:use :common-lisp :core :test :match :io :strings)
  (:shadowing-import-from :match :*))

(in-package :shrink)

(defun get-input (&key verbose)
  (let ((*read-eval* nil))
    (handler-case (read-words (make-string-input-stream (string-right-trim ".?!" (read-line))))
      (error (e)
        (when verbose (format *error-output* "Your input is not so good: ~A~%" e))
        nil))))

(defun val (subs key)
  (rest (assoc key subs)))

(defun find-vars (pattern)
  (loop for clause in pattern
        when (listp clause) collect (second clause)))

;; (defun choose-random (body)
;;   (let ((n (length body)))
;;     (if (= 1 n)
;;         (first body)
;;         `(case (random ,n)
;;            ,@(loop for i below n
;;                    for response in body
;;                    collect (list i response)))) ))

(defun choose-random (body)
  (let ((n (length body)))
    (case n
      (1 (first body))
      (otherwise `(case (random ,n)
                    ,@(loop for i from 0
                            for response in body
                            collect (list i response)))) )))

(defmacro match-with-bindings ((pattern input) &body body)
  (let ((match (make-symbol "MATCH"))
        (subs (make-symbol "SUBS"))
        (vars (find-vars pattern)))
    `(multiple-value-bind (,match ,subs) (match ',pattern ,input)
       (when ,match
         (let (,@(loop for var in vars collect `(,var (val ,subs ',var))))
           ,(choose-random body)))
       ,match)))

(defun motherp (m)
  (member m '("mother" "mom" "ma" "mommy") :test #'string-equal))

(defun fatherp (p)
  (member p '("father" "dad" "pa" "daddy") :test #'string-equal))

(defun shrink ()
  (format t "Welcome to my sofa!~%")
  (let (;(wword-count 0)
        (punt-count 0))
    (loop (let ((input (map-you-me (get-input))))
            (cond ((match '("bye") input) (return '"Goodbye!"))
                  ((match '() input) (format t "Please say something!~%"))
                  ((match-with-bindings (((* x) "hello" (* y)) input)
                     (format t "How do you do? Please state your problem.~%")))
                  ((match-with-bindings (((* x) "computer" (* y)) input)
                     (format t "Do computers worry you?~%")
                     (format t "What do you think about machines?~%")
                     (format t "Why do you mention computers?~%")
                     (format t "What do you think machines have to do with your problem?~%")))
                  ((match-with-bindings (((* x) "name" (* y)) input)
                     (format t "I am not interested in names.~%")))
                  ((match-with-bindings (((* x) "sorry" (* y)) input)
                     (format t "Please don't apologize.~%")
                     (format t "Apologies are not necessary.~%")
                     (format t "What feelings do you have when you apologize?~%")))
                  ((match-with-bindings (((* x) "you" "remember" (* y)) input)
                     (format t "Do you often think of ~A?~%" (join y " "))
                     (format t "Does thinking of ~A bring anything else to mind?~%" (join y " "))
                     (format t "What else do you remember?~%")
                     (format t "Why do you recall ~A right now?~%" (join y " "))
                     (format t "What in the present situation reminds you of ~A?~%" (join y " "))
                     (format t "What is the connection between me and ~A?~%" (join y " "))))
                  ((match-with-bindings (((* x) "do" "me" "remember" (* y)) input)
                     (format t "Did you think I would forget ~A?~%" (join y " "))
                     (format t "Why do you think I should recall ~A now?~%" (join y " "))
                     (format t "What about ~A?~%" (join y " "))
                     (format t "You mentioned ~A?~%" (join y " "))))
                  ((match-with-bindings (((* x) "if" (* y)) input)
                     (format t "Do you really think its likely that ~A?~%" (join y " "))
                     (format t "Do you wish that ~A?~%" (join y " "))
                     (format t "What do you think about ~A?~%" (join y " "))
                     (format t "Really-- if ~A?~%" (join y " "))))
                  ((match-with-bindings (((* x) "you" "dreamt" (* y)) input)
                     (format t "Really-- ~A~%" (join y " "))
                     (format t "Have you ever fantasized ~A while you were awake?~%" (join y " "))
                     (format t "Have you dreamt ~A before?~%" (join y " "))))
                  ((match-with-bindings (((* x) "dream" "about" (* y)) input)
                     (format t "How do you feel about ~A in reality?~%" (join y " "))))
                  ((match-with-bindings (((* x) "dream" (* y)) input)
                     (format t "What does this dream suggest to you?~%")
                     (format t "Do you dream often?~%")
                     (format t "What persons appear in your dreams?~%")
                     (format t "Don't you believe that dream has to do with your problem?~%")))
                  ((match-with-bindings (((* x) "your" (motherp m) (* y)) input)
                     (format t "Who else in your family ~A?~%" (join y " "))                        ; (null y) <----------------------------
                     (format t "Tell me more about your family~%")))
                  ((match-with-bindings (((* x) "your" (fatherp p) (* y)) input)
                     (format t "Your father~%")
                     (format t "Does he influence you strongly?~%")
                     (format t "What else comes to mind when you think of your father?~%")))
                  ((match-with-bindings (((* x) "you" "want" (* y)) input)
                     (format t "What would it mean if you got ~A?~%" (join y " "))
                     (format t "Why do you want ~A?~%" (join y " "))
                     (format t "Suppose you got ~A soon~%" (join y " "))))
                  ((match-with-bindings (((* x) "you" "are" "glad" (* y)) input)
                     (format t "How have I helped you to be ~A?~%" (join y " "))
                     (format t "What makes you happy just now?~%")
                     (format t "Can you explain why you are suddenly ~A?~%" (join y " "))))
                  ((match-with-bindings (((* x) "you" "are" "sad" (* y)) input)
                     (format t "I am sorry to hear you are depressed.~%")
                     (format t "I'm sure it's not pleasant to be sad.~%")))
                  ((match-with-bindings (((* x) "am" "like" (* y)) input) ; ??!?!
                     (format t "What resemblance do you see between ~A and ~A?~%" (join x " ") (join y " "))))
                  ((match-with-bindings (((* x) "is" "like" (* y)) input)
                     (format t "In what way is it that ~A is like ~A?~%" (join x " ") (join y " "))
                     (format t "What resemblance do you see?~%")
                     (format t "Could there really be some connection?~%")
                     (format t "How?~%")))
                  ((match-with-bindings (((* x) "alike" (* y)) input)
                     (format t "In what way?~%")
                     (format t "What similarities are there?~%")))
                  ((match-with-bindings (((* x) "same" (* y)) input)
                     (format t "What other connections do you see?~%")))
                  ((match-with-bindings (((* x) "you" "were" (* y)) input)
                     (format t "Were you really?~%")
                     (format t "Perhaps I already knew you were ~A~%" (join y " "))
                     (format t "Why do you tell me you were ~A now?~%" (join y " "))))
                  ((match-with-bindings (((* x) "were" "you" (* y)) input)
                     (format t "What if you were ~A?~%" (join y " "))
                     (format t "Do you think you were ~A~%" (join y " "))
                     (format t "What would it mean if you were ~A~%" (join y " "))))
                  ((match-with-bindings (((* x) "you" "are" (* y)) input)
                     (format t "In what way are you ~A~%" (join y " "))
                     (format t "Do you want to be ~A?~%" (join y " "))))
                  ((match-with-bindings (((* x) "are" "you" (* y)) input)
                     (format t "Do you believe you are ~A~%" (join y " "))
                     (format t "Would you want to be ~A~%" (join y " "))
                     (format t "You wish I would tell you that you are ~A~%" (join y " "))
                     (format t "What would it mean if you were ~A~%" (join y " "))))
                  ((match-with-bindings (((* x) "are" (* y)) input) ; ????
                     (format t "Why do you say \"AM\"?~%")
                     (format t "I don't understand that~%")))
                  ((match-with-bindings (((* x) "am" "me" (* y)) input) ; Flip order with following patern?
                     (format t "Why are you interested in whether I am ~A or not?~%" (join y " "))
                     (format t "Would you prefer if I weren't ~A~%" (join y " "))
                     (format t "Perhaps I am ~A in your fantasies~%" (join y " "))))
                  ((match-with-bindings (((* x) "me" "am" (* y)) input)
                     (format t "What makes you think I am ~A?~%" (join y " "))))
                  ((match-with-bindings (((* x) "because" (* y)) input)
                     (format t "Is that the real reason?~%")
                     (format t "What other reasons might there be?~%")
                     (format t "Does that reason seem to explain anything else?~%")))
                  ((match-with-bindings (((* x) "was" "me" (* y)) input)
                     (format t "Perhaps I was ~A~%" (join y " "))
                     (format t "What do you think?~%")
                     (format t "What if I had been ~A~%" (join y " "))))
                  ((match-with-bindings (((* x) "you" "can't" (* y)) input)
                     (format t "Maybe you could ~A now~%" (join y " "))
                     (format t "What if you could ~A?~%" (join y " "))))
                  ((match-with-bindings (((* x) "you" "feel" (* y)) input)
                     (format t "Do you often feel ~A?~%" (join y " "))))
                  ((match-with-bindings (((* x) "you" "felt" (* y)) input)
                     (format t "What other feelings do you have?~%")))
                  ;;
                  ;;    I don't get around how you get around.
                  ;;    Perhaps in your fantasy we don't get around how each other
                  ;;    
                  ((match-with-bindings (((* x) "you" (* y) "me" (* z)) input)
                     (format t "Perhaps in your fantasy we ~A each other~%" (join y " "))))
                  ((match-with-bindings (((* x) "why" "don't" "me" (* y)) input)
                     (format t "Should you ~A yourself?~%" (join y " "))
                     (format t "Do you believe I don't ~A~%" (join y " "))
                     (format t "Perhaps I will ~A in good time~%" (join y " "))))
                  ((match-with-bindings (((* x) "yes" (* y)) input)
                     (format t "You seem quite positive~%")
                     (format t "You are sure~%")
                     (format t "I understand~%")))
                  ((match-with-bindings (((* x) "no" (* y)) input)
                     (format t "Why not?~%")
                     (format t "You are being a bit negative~%")
                     (format t "Are you saying \"NO\" just to be negative?~%")))
                  ((match-with-bindings (((* x) "someone" (* y)) input)
                     (format t "Can you be more specific?~%")))
                  ((match-with-bindings (((* x) "everyone" (* y)) input)
                     (format t "Surely not everyone~%")
                     (format t "Can you think of anyone in particular?~%")
                     (format t "Who for example?~%")
                     (format t "You are thinking of a special person~%")))
                  ((match-with-bindings (((* x) "always" (* y)) input)
                     (format t "Can you think of a specific example~%")
                     (format t "When?~%")
                     (format t "What incident are you thinking of?~%")
                     (format t "Really--always~%")))
                  ((match-with-bindings (((* x) "what" (* y)) input)
                     (format t "Why do you ask?~%")
                     (format t "Does that question interest you?~%")
                     (format t "What is it that you really want to know?~%")
                     (format t "What do you think?~%")
                     (format t "What comes to your mind when you ask that?~%")))
                  ((match-with-bindings (((* x) "perhaps" (* y)) input)
                     (format t "You do not seem quite certain~%")))
                  ((match-with-bindings (((* x) "am" (* y)) input)
                     (format t "Did you think they might not be ~A?~%" (join y " "))
                     (format t "Possibly they are ~A~%" (join y " "))))
                  ;; ((match-with-bindings (("you" "are" (* x)) input)
                  ;;    (incf wword-count)
                     ;; (format t "Please tell me ~A you are ~A~%" (wword wword-count) (join x " "))))
                  ((match-with-bindings (("you" "have" (* x)) input)
                     (format t "How long have you had ~A?~%" (join x " "))))
                  ((match-with-bindings (("you" "feel" (* x)) input)
                     (format t "I sometimes feel the same way.~%")))
                  ((match-with-bindings (("yes" (* x)) input)
                     (format t "How can you be so sure ~A?~%" (join x " "))))
                  ;; ((match-with-bindings (("me" "are" (* x)) input) ; ?????????????
                  ;;    (format t "Oh yeah I am ~A~%" x)))

                  ((match-with-bindings (((verbp v) (* x)) input)
                     (format t "Why do you want me to ~A ~A?~%" v (join x " "))))
                  ((match-with-bindings (((questionp w) (* x)) input) ; X??
                     (format t "You tell me ~A.~%" w)))
                  ((match-with-bindings (("do" "me" "think" (* x)) input)
                     (format t "I think that you should answer that yourself.~%")))
                  ((match-with-bindings (((auxiliaryp w) "me" (* x)) input)
                     (format t "Perhaps I ~A ~A.~%" w (join x " "))))

;                  ((member "dream" input :test #'equalp) (format t "For dream analysis see Freud.~%"))
                  ((member "love" input :test #'equalp) (format t "All is fair in love and war.~%"))
                  ((member "no" input :test #'equalp) (format t "Don't be so negative.~%"))
                  ((member "maybe" input :test #'equalp) (format t "Be more decisive!~%"))
                  ((member "you" input :test #'equalp) (format t "~A.~%" (join input " ")))
                  (t (write-line (punt (incf punt-count)))) )))) )

;; (defun you-me (word)
;;   (case word
;;     (i 'you)
;;     (me 'you)
;;     (you 'me) ;;;; 
;;     (my 'your)
;;     (your 'my)
;;     (yours 'mine)
;;     (mine 'yours)
;;     (am 'are)
;;     (are 'am)
;;     (otherwise word)))

;; (defun map-you-me (list)
;;   (mapcar #'you-me list))

(let ((you-me-map '(("I" . "you")
                    ("me" . "you")
                    ("you" . "me") ;;;; 
                    ("my" . "your")
                    ("your" . "my")
                    ("yours" . "mine")
                    ("mine" . "yours")
                    ("am" . "are")
                    ("are" . "am")
                    ("was" . "were")
                    ("were" . "was"))))
  (defun map-you-me (words)
    (sublis you-me-map words :test #'equalp)))

;; (map-you-me '(i am the one that you need to treat))
;; (YOU ARE THE ONE THAT ME NEED TO TREAT)

;; (let ((wwords #("when" "why" "where" "how")))
;;   (defun wword (i)
;;     (aref wwords (mod i (length wwords)))) )

(defun questionp (word)
  (member word '("why" "where" "when" "what" "which" "how") :test #'equalp))

(defun auxiliaryp (word)
  (member word '("do" "can" "should" "would") :test #'equalp))

(let ((punts #("Please go on."
               "Tell me more."
               "I see."
               "What does that indicate?"
               "But why be concerned about it?"
               "Just tell me how you feel."
               "Very interesting"
               "I am not sure I understand you fully"
               "What does that suggest to you?"
               "Do you feel strongly about discussing such things?")))
  (defun punt (punt-count)
    (aref punts (mod punt-count (length punts)))) )

(defun verbp (word)
  (member word '("go" "have" "be" "try" "eat" "take" "help" "make" "get" "jump"
                 "write" "type" "fill" "put" "turn" "compute"
                 "think" "drink" "blink" "crash" "crunch" "add") :test #'equalp))
