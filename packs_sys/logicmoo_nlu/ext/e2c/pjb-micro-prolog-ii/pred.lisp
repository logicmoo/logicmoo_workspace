;;; Mini-PrologII
;;; pred.lsp
;;;
(defparameter *ob-micro-log*  '())


(defmacro define-prolog-function (name arguments &body body)
  `(progn
     (defun ,name ,arguments ,@body)
     (setf (get ',name 'evaluable) ,(loop
                                       :for arg :in arguments
                                       :until (member arg LAMBDA-LIST-KEYWORDS)
                                       :count 1))
     (pushnew ',name *ob-micro-log*)
     ',name))

 
;;; !/0
(define-prolog-function ! (&optional n)
  "
!/0
"
  (setq *backtracking-local-register* (Cut *cl*)
        *backtracking-global-register* (if (zerop *backtracking-local-register*)
                                           +bottom-of-global-stack+
                                           (backtracking-global-register *backtracking-local-register*))
        *top-of-local-stack* (+ *cl* 4 n))) ; updates local stack

(define-prolog-function |call| (x)
  "
call/1 (+term)
"
  (if (var? x)
      (let ((te (ultimate x *current-environment* *current-goal*)))	; dereferences it
        (unless *current-continuation*
          (when (<= *backtracking-local-register* *cl*)
            (setq *top-of-local-stack* *cl*)) ; applies LCO
          (setq *current-continuation* (current-continuation *cl*)
                *cl* (cl *cl*)))        ; new continuation
        (push_cont)                     ; saves continuation
        (vset *memory* (+ *top-of-local-stack* 2) (cdr te))	; global env.
        (vset *memory* (+ *top-of-local-stack* 3) *specific-cut-point*)	; cut point
        (setq *current-continuation* (list (dec_goal (car te)))
              *cl* *top-of-local-stack*)
        (maj_L 0))                                 ; ends local block
      (push (dec_goal x) *current-continuation*))) ; adds it to *current-continuation*


(define-prolog-function |freeze| (x p)
  "
freeze/2 (?var,+term)
"
  (let ((xte (ultimate x *current-environment* *current-goal*))) ; dereferences the var
    (if (var? (car xte))                ; unbound
        (let ((y (adr (car xte) (cdr xte))) ; the location 
              (pte (ultimate p *current-environment* *current-goal*))) ; dereferences the goal
          (bindfg y (dec_goal (car pte)) (cdr pte) (fgblock y)))
        (|call| p))))                   ; else call p


(define-prolog-function |dif| (x y)
  "
dif/2 (?term,?term)
"
  (let ((*backtracking-local-register* *top-of-local-stack*)
        (*backtracking-global-register* *top-of-global-stack*)
        (str *top-of-trail*)
        (*awakened-goals* nil))         ; saves registers
    (if (eq (uni x y) 'fail)            ; unification fails
        (poptrail str)                  ; restores env and succeeds
        (if (/= *top-of-trail* str)     ; one var bound
            (let* ((xv (svref *memory* (1- *top-of-trail*))) ; selects one var
                   (v (if (numberp xv)
                          xv
                          (car xv)))) 
              (poptrail str)            ; restores env
              (bindfg v *current* *current-goal* (fgblock v))) ; perpetuates the delaying
            'fail))))                   ; fails if equals

(define-prolog-function |statistics| ()
  "
statistics/0
"
  (format t " local stack : ~A (~A used)~%" 
          (- +bottom-of-trail+ +bottom-of-local-stack+) (- *top-of-local-stack* +bottom-of-local-stack+))
  (format t " global stack : ~A (~A used)~%" 
          (- +bottom-of-local-stack+ +bottom-of-global-stack+) (- *top-of-global-stack* +bottom-of-global-stack+))
  (format t " trail : ~A (~A used)~%" 
          (- +max-of-trail+ +bottom-of-trail+) (- *top-of-trail* +bottom-of-trail+))
  (format t " frozen-goals stack : ~A (~A used)~%" 
          +bottom-of-global-stack+ (- *top-of-frozen-goals-stack* +bottom-of-frozen-goals-stack+)))


(define-prolog-function |frozen_goals| ()
  "
frozen_goals/0
"
  (do ((i (- *top-of-frozen-goals-stack* 4) (- i 4))) ; scans the frozen goals stack
      ((< i 0))
    (if (eq (car (svref *memory* (FGvar i))) 'LIBRE) ; unbound
        (let ((b (if (numberp (FGgoal i)) (FGgoal i) i)))
          (writesf (pred (FGgoal b)) (largs (FGgoal b)) (FGenv b))
          (format t " frozen upon X~A~%" (FGvar i))))))


;;;; THE END ;;;;
