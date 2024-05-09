;#!/usr/local/bin/clisp

;;
;   NAME:               shrink.lsp
;
;   STARTED:            000829
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
(defun shrink ()
  (setq wword-count 0)
  (setq punt-count 0)
  (format t "Welcome to my sofa!~%")
  (format t "[Please enclose your input in parentheses.]~%")
  (loop (setq s (you-me-map (read)))
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
               (printl (append '(oh yeah I am) x)))
              ((match '((verbp v) (* x)) s)
               (printl (append '(why do you want me to)
                               (list v) x)))
              ((match '((wpred w) (* x)) s)
               (printl (append '(you tell me) (list w))))
              ((match '((dpred w) me (* x)) s)
               (printl (append '(perhaps I) (list w) x)))
              ((match '(do me think (* x)) s)
               (format t "I think you should answer that yourself.~%"))
              ((member 'dream s)
               (format t "For dream analysis, see Freud.~%"))
              ((member 'love s)
               (format t "All is fair in love and war.~%"))
              ((member 'no s)
               (format t "Don't be so negative.~%"))
              ((member 'maybe s)
               (format t "Be more decisive!~%"))
              ((member 'you s)
               (printl s))
              (t (setq punt-count (1+ punt-count))
                 (cond ((= punt-count 6)
                        (setq punt-count 0)))
                 (printl (nth punt-count punts))))))

(defun printl (message)
  (mapcar #'(lambda (txt) (format t "~a " txt))
          message)
  (terpri))

(defun wword ()
  (setq wword-count (1+ wword-count))
  (cond ((= wword-count 4)
         (setq wword-count 0)))
  (nth wword-count '(when where why how)))

(defun wpred (w)
  (member w '(why where when what which how)))

(defun dpred (w)
  (member w '(do can should would)))

(setq punts '((please go on)
              (tell me more)
              (I see)
              (what does that indicate)
              (but why be concerned about it)
              (just tell me how you feel)))

(defun you-me (w)
  (cond ((eq w 'I) 'you)
        ((eq w 'me) 'you)
        ((eq w 'you) 'me)
        ((eq w 'my) 'your)
        ((eq w 'your) 'my)
        ((eq w 'yours) 'mine)
        ((eq w 'mine) 'yours)
        ((eq w 'am) 'are)
        (t w)))

(defun you-me-map (lst)
  (mapcar (function you-me) lst))

(defun verbp (w)
  (member w '(go have be try eat take help make get jump
              write type fill put turn compute
              think drink blink crash crunch add)))

(defun match (p s)
  (cond ((null p)(null s))
        ((atom (car p))
         (and s (equal (car p) (car s))
              (match (cdr p) (cdr s))))
        ((and s (eq (caar p) '?))
         (cond ((match (cdr p) (cdr s))
                (set (cadar p) (car s)) t)
               (t nil)))
        ((eq (caar p) '*)
         (cond ((and s (match (cdr p) (cdr s)))
                (set (cadar p) (list (car s))) t)
               ((match (cdr p) s)
                (set (cadar p) nil) t)
               ((and s (match p (cdr s)))
                (set (cadar p)
                     (cons (car s) (eval (cadar p)))) t)
               (t nil)))
         ((and s (apply (caar p) (list (car s)))
               (match (cdr p) (cdr s)))
          (set (cadar p) (car s)) t)
         (t nil)))