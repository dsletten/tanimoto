;#!/usr/local/bin/clisp

;;
;   NAME:               shrink.lsp
;
;   STARTED:            010407
;   MODIFICATIONS:
;
;   PURPOSE:
;
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES:
;
;;
(load "match.lsp")

(defun shrink ()
  (setf wword-count 0)
  (setf punt-count 0)
  (format t "Welcome to my sofa!~%")
  (format t "Please enclose your input in parentheses.~%")
  (loop (setf s (you-me-map (read)))
	(cond ((match '(bye) s)
	       (return 'goodbye))
	      ((match '(you are (* x)) s)
	       (printl (append '(please tell me)
			        (list (wword))
			       '(you are)
			        x)))
	      ((match '(you have (* x)) s)
	       (printl (append '(how long have you had) x)))
	      ((match '(you feel (* x)) s)
	       (format t "I sometimes feel the same way.~%"))
	      ((match '(because (* x)) s)
	       (format t "Is that really the reason?~%"))
	      ((match nil s)
	       (format t "Please say something!~%"))
	      ((match '(yes (* x)) s)
	       (printl (append '(how can you be so sure) x)))
	      ((match '(me are (* x)) s)
	       (printl (append '(oh yeah i am) x)))
	      ((match '((verbp v) (* x)) s)
	       (printl (append '(why do you want me to)
			       (list v) x)))
	      ((match '((wpred w) (* x)) s)
	       (printl (append '(you tell me)
			       (list w))))
	      ((match '((dpred w) me (* x)) s)
	       (printl (append '(perhaps i)
			       (list w) x)))
	      ((match '(do me think (* x)) s)
	       (format t "I think you should answer that yourself.~%"))  ;This never triggers because of previous rule?
	      ((member 'dream s) (format t "For dream analysis, see Freud.~%"))
	      ((member 'love s) (format t "All is fair in love and war. . .~%"))
	      ((member 'no s) (format t "Don't be so negative.~%"))
	      ((member 'maybe s) (format t "Be more decisive!~%"))
	      ((member 'you s) (printl s))
	      (t (setf punt-count (1+ punt-count))
		 (cond ((= punt-count (length punts))
			(setq punt-count 0)))
		 (printl (nth punt-count punts))))) )

(defun printl (message)
  (mapcar #'(lambda (txt) (format t "~A " txt))
	  message)
;;  (terpri) )
  (format t "~%") )

(defun wword ()
  (setf wword-count (1+ wword-count))
  (cond ((= wword-count 4) (setf wword-count 0)))
  (nth wword-count '(when why where how)) )

(defun wpred (w)
  (member w '(why where when what which how)) )

(defun dpred (w)
  (member w '(do can should would)) )

(setf punts '((please go on)
	      (tell me more)
	      (i see)
	      (what does that indicate)
	      (but why be concerned about it)
	      (just tell me how you feel)))

(defun you-me (w)
  (cond ((eq w 'i) 'you)
	((eq w 'me) 'you)
	((eq w 'you) 'me)
	((eq w 'my) 'your)
	((eq w 'your) 'my)
	((eq w 'yours) 'mine)
	((eq w 'mine) 'yours)
	((eq w 'am) 'are)
	(t w)) )

(defun you-me-map (lst)
  (mapcar (function you-me) lst) )

(defun verbp (w)
  (member w '(go have be try eat take help make get jump
	      write type fill put turn compute think
	      drink blink crash crunch add)) )


