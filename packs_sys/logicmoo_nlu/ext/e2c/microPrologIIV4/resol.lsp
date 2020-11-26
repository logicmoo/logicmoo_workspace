; mini-PrologII
; resol.lsp
;

(defun forward () 
  (do () ((null Duboulot) (format t "no More~%"))      
      (cond ((and (null CP)		; empty continuation
		  (null FRCP))		; no awaken goals
	     (answer))      
	    ((load_PC)			; selects first goal
	     (cond                   
	      ((user? PC)		; user defined
	       (let ((d (def_of PC)))	; associated set of clauses
		 (if d (pr2 d) (backtrack)))) 
	      ((builtin? PC)		; builtin
	       (if (eq (apply (car PC) (cdr PC)) 'fail) 
		   (backtrack)		; evaluation fails
		 (cont_eval)))               
	      ((backtrack)))))))	; undefined predicate
 
(defun load_A (largs el eg)		; loads Ai registers with
  (dotimes (i (length largs) (vset Mem A i)) ; goal's args
	   (vset Mem (+ A i 1) (ultimate (pop largs) el eg))))

(defun load_PC () 
  (if FRCP  
      (let ((x ()))   
	(do ()				; sorts the goals depending on
	    ((null FRCP))		; their freezing times
	    (setq x (add_fg (pop FRCP) x)))
	(do ()				; allocates blocks in the 
	    ((null x))			; corresponding order
	    (create_block (abs (pop x))))))
  (setq PC (pop CP) PCE (E CL) PCG (G CL) Cut_pt BL))

(defun other_fg (b r)
  (if (< (FGtail b) BottomG) (add_fg (FGtail b) r) r))

(defun add_fg (b r)			; sorts the various awakened
  (let ((b1 (if (numberp (FGgoal b)) (FGgoal b) b))) ; goals
    (if (eq (pred (FGgoal b1)) '|dif|)	; dif first
	(insert (- b1) (other_fg b r))
      (let* ((v (svref Mem (FGvar b1))) ; it is a freeze
	     (te (val (car v) (cdr v))))
	(if (var? (car te))		; the var is still unbound
	    (let ((y (adr (car te) (cdr te))))
	      (bindfg y b1 nil (fgblock y)) ; delaying is perpetuated
	      (other_fg b r))
	  (insert b1 (other_fg b r))))))) ; insert the goal

(defun insert (b l)			; sorts the goals according to 
  (if (or (null l) (> b (car l)))	; their freezing times
      (cons b l) 
    (cons (car l) (insert b (cdr l))))) 

(defmacro dec_goal (x)
  `(if (atom ,x) (list ,x) (cons (caar ,x) (cdr ,x))))

(defun create_block (b)			; block for an awakened goal
  (push_cont)				; saves current continuation
  (vset Mem (+ L 2) (FGenv b))		; its global env
  (vset Mem (+ L 3) Cut_pt)		; the corresponding cut point
  (setq CP (list (FGgoal b)) CL L)	; new continuation
  (maj_L 0))				; ends the block
 
(defun pr2 (paq)			; proves PC
  (load_A (largs PC) PCE PCG)		; loads its arguments
  (if CP  
      (pr paq) 
    (progn				; if last call and no interm.
      (if (<= BL CL) (setq L CL))	; choice point then LCO
      (setq CP (CP CL) CL (CL CL))	; next continuation
      (pr paq))))
         
(defun cont_eval () 
  (unless CP (if (<= BL CL) (setq L CL)) (setq CP (CP CL) CL (CL CL)))) 
 
(defun pr (paq)                   
  (if (cdr paq)				; alloc. choice point
      (progn (push_choix) (pr_choice paq))
    (pr_det (car paq))))		; else deterministic

(defun pr_det (c) 
  (if (eq (unify_with			; first tries to unify
	   (largs (head c))
	   (push_E (nloc c))
	   (push_G (nglob c)))  
          'fail) 
      (backtrack) 
    (progn				; success
      (maj_G (nglob c))			; terminates global env
      (when (tail c)			; c is rule
	    (push_cont) saves current cont.
	    (setq CP (tail c) CL L)	; new one
	    (maj_L (nloc c))))))	; terminates local block

(defun pr_choice (paq) 
  (let* ((resu (shallow_backtrack paq)) (c (car resu)) (r (cdr resu)))
    (cond ((null r)			; only one candidate remains
	   (pop_choix)			; pops the rerun part
	   (pr_det c))			; proof is now deterministic
	  ( (push_bpr r)                  
	    (maj_G (nglob c))                    
	    (when (tail c)		; c is a rule
		  (push_cont)		; saves current cont.              
		  (setq CP (tail c) CL L) ; new one
		  (maj_L (nloc c))))))) ; terminates local block

(defun shallow_backtrack (paq)		; looks for a first success
  (if (and (cdr paq)			; more than one candidate
	   (eq (unify_with		; and unification fails
		(largs (head (car paq)))
		(push_E (nloc (car paq)))
		(push_G (nglob (car paq))))
	       'fail)) 
      (progn 
	(setq FRCP nil FR (FR BL))	; restores registers
	(poptrail (TR BL))		; restores env
	(shallow_backtrack (cdr paq)))
    paq))

(defun backtrack ()            
  (if (zerop BL)			; no more choice points
      (setq Duboulot nil)		; restores registers
    (progn (setq L BL G BG FR (FR L) FRCP nil Cut_pt (BL BL)
		 CP (BCP L) CL (BCL L) Cut_pt (BL BL))
	   (load_A2)            
	   (poptrail (TR BL))		; restores env
	   (pr_choice (BP L)))))	; relaunch the proof
    
(defun load_A2 ()			; restores Ai registers
  (let ((deb (- L (size_C L)))) 
    (dotimes (i (A L) (vset Mem A i)) 
	     (vset Mem (+ A i 1) (svref Mem (+ deb i))))))
 
(defun myloop (c) 
  (setq FR BottomFR G BottomG L BottomL TR BottomTR Cut_pt 0
        CP nil CL 0  BL 0 BG BottomG FRCP nil Duboulot t) 
  (push_cont)				; initial continuation
  (push_E (nloc c))			; local env. for the query
  (push_G (nglob c))			; global env. for the query
  (setq CP (cdr c) CL L)		; current continuation
  (maj_L (nloc c))			; ends local block
  (maj_G (nglob c)) (read-char)		; ends global block
  (catch 'debord (forward))
  (myloop (read_prompt))) 
 
