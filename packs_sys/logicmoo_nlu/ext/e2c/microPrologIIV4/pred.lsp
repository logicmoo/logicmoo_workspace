; Mini-PrologII
; pred.lsp
;
(defvar Ob_Micro_Log 
      '(|write| |nl| |tab| |read| |get| |get0|
	|var| |nonvar| |atomic| |atom| |number|
	! |fail| |true|
	|divi| |mod| |plus| |minus| |mult| |le| |lt| 
	|name| |consult| |abolish| |cputime| |statistics|
	|call| |freeze| |dif| |frozen_goals|))
(mapc #'(lambda (x) (setf (get x 'evaluable) t)) Ob_Micro_Log)
 
; !/0
(defun ! (n) 
  (setq BL (Cut CL) BG (if (zerop BL) BottomG (BG BL))
	L (+ CL 4 n)))			; updates local stack

; call/1 (+term)
(defun |call| (x)
  (if (var? x)
      (let ((te (ultimate x PCE PCG)))	; dereferences it
	(unless CP
		(if (<= BL CL) (setq L CL)) ; applies LCO
		(setq CP (CP CL) CL (CL CL))) ; new continuation
	(push_cont)			; saves continuation
	(vset Mem (+ L 2) (cdr te))	; global env.
	(vset Mem (+ L 3) Cut_pt)	; cut point
	(setq CP (list (dec_goal (car te))) CL L)
	(maj_L 0))			; ends local block
    (push (dec_goal x) CP)))		; adds it to CP

; freeze/2 (?var,+term)
(defun |freeze| (x p)
  (let ((xte (ultimate x PCE PCG)))	; dereferences the var
    (if (var? (car xte))		; unbound
	(let ((y (adr (car xte) (cdr xte))) ; the location 
	      (pte (ultimate p PCE PCG))) ; dereferences the goal
	  (bindfg y (dec_goal (car pte)) (cdr pte) (fgblock y)))
      (|call| p))))			; else call p

; dif/2 (?term,?term)
(defun |dif| (x y)
  (let ((BL L) (BG G) (str TR) (FRCP nil)) ; saves registers
    (if (eq (uni x y) 'fail)		; unification fails
	(poptrail str)			; restores env and succeeds
      (if (/= TR str)			; one var bound
	  (let* ((xv (svref Mem (1- TR))) ; selects one var
		 (v (if (numberp xv) xv (car xv)))) 
	    (poptrail str)		; restores env
	    (bindfg v PC PCG (fgblock v))) ; perpetuates the delaying
	'fail))))			; fails if equals

; statistics/0 
(defun |statistics| ()
  (format t " local stack : ~A (~A used)~%" 
	  (- BottomTR BottomL) (- L BottomL))
  (format t " global stack : ~A (~A used)~%" 
	  (- BottomL BottomG) (- G BottomG))
  (format t " trail : ~A (~A used)~%" 
	  (- A BottomTR) (- TR BottomTR))
  (format t " frozen-goals stack : ~A (~A used)~%" 
	  BottomG (- FR BottomFR)))

; frozen_goals/0
(defun |frozen_goals| ()
  (do ((i (- FR 4) (- i 4)))		; scans the frozen goals stack
      ((< i 0))
      (if (eq (car (svref Mem (FGvar i))) 'LIBRE) ; unbound
	  (let ((b (if (numberp (FGgoal i)) (FGgoal i) i)))
	    (writesf (pred (FGgoal b)) (largs (FGgoal b)) (FGenv b))
	    (format t " frozen upon X~A~%" (FGvar i))))))
