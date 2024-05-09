;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               shrink.lisp
;;;
;;;   STARTED:            Thu Aug  8 01:48:38 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;

(load "match")

(let ((wword-count 0)
      (punt-count 0)
      (punts '((please go on)
	       (tell me more)
	       (i see)
	       (what does that indicate)
	       (but why be concerned about it)
	       (just tell me how you feel)))
      (s nil))
  (defun shrink ()
    (format t "Welcome to my sofa!~%")
    (format t "Please enclose your input in parentheses.~2%")
    (loop
     (setf s (you-me-map (read)))
     (cond ((match '(bye) s)
	    (return 'goodbye))
	   ((match '(you are (* x)) s)
	    (printl `(please tell me ,(wword) you are ,@x)))
	   ((match '(you have (* x)) s)
	    (printl `(how long have you had ,@x)))
	   ((match '(you feel (* x)) s)
	    (formt t "I sometimes feel the same way.~%"))
	   ((match '(because (* x)) s)
	    (format t "Is that really the reason?~%"))
	   ((match () s)
	    (format t "Please say something!~%"))
	   ((match '(yes (* x)) s)
	    (printl `(how can you be so sure ,@x)))
	   ((match '(me are (* x)) s) ;!!!! you are -> me are
	    (printl `(oh yeah i am ,@x)))
	   ((match '((verbp v) (* x)) s)
	    (printl `(why do you want me to ,v ,@x)))
	   ((match '((wpred w) (* x)) s)
	    (printl `(you tell me ,w)))
	   ((match '((dpred w) me (* x)) s)
	    (printl `(perhaps i ,w ,@x)))
	   ((match '(do me think (* x)) s)
	    (format t "I think you should answer that yourself.~%"))
	   ((member 'dream s)
	    (format t "For dream analysis see Freud.~%"))
	   ((member 'love s)
	    (format t "All is fair in love and war...~%"))
	   ((member 'no s)
	    (format t "Don't be so negative.~%"))
	   ((member 'maybe s)
	    (format t "Be more decisive!~%"))
	   ((member 'you s) (printl s))
	   (t (incf punt-count)
	      (printl (nth (mod punt-count (length punts)) punts)))) ))

  (defun printl (message)
    (format t "~@(~{~A ~}~)~%" message))

  (let ((wwords '(when why where how)))
    (defun wword ()
      (incf wword-count)
      (nth (mod wword-count (length wwords)) wwords)))

  (defun wpred (w)
    (member w '(why where when what which how)))

  (defun dpred (w)
    (member w '(do can should would)))

  (defun verbp (w)
    (member w '(go have be try eat take help make get jump
		write type fill put turn compute
		think drink blink crash crunch add
		talk chew fish swim program sing die laugh)))
  
  (let ((you-me '((i you) ;;i -> you, you -> ? i/me
		  (me you)
		  (you me)
		  (my your)
		  (your my)
		  (yours mine)
		  (mine yours)
		  (am are))))
    (defun you-me-map (l)
      (mapcar #'(lambda (w)
		  (or (second (assoc w you-me)) w))
	      l)))
  )