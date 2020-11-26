; mini-PrologII
; blocks.lsp
;

; I. Registers
;
(defconstant BottomFR 1)		; bottom of frozen goals stack
(defconstant BottomG 3000)		; bottom of global stack
(defconstant BottomL 6000)		; bottom of local stack
(defconstant BottomTR 10000)		; bottom of trail
(defconstant A 12000)			; max of trail

(defvar Mem (make-array 12050 :initial-element 0))
(defvar FR)				; top of frozen goals stack
(defvar TR)				; top of trail
(defvar L)				; top of local stack
(defvar G)				; top of global stack
(defvar CP)				; current continuation
(defvar CL) 
(defvar Cut_pt)				; specific cut
(defvar FRCP)				; awakened goals
(defvar BL)				; backtracking registers
(defvar BG)
(defvar PC)				; currents goal and its
(defvar PCE)				; environments
(defvar PCG) 
(defvar Duboulot)

(defmacro vset (v i x) `(setf (svref ,v ,i) ,x))

; II. Local Stack
;
;deterministic block [CL CP G Cut E]
;
(defmacro CL (b) `(svref Mem ,b))
(defmacro CP (b) `(svref Mem (1+ ,b)))
(defmacro G (b) `(svref Mem (+ ,b 2)))
(defmacro Cut (b) `(svref Mem (+ ,b 3)))
(defmacro E (b) `(+ ,b 4))            

(defmacro push_cont ()			; saves continuation
  `(progn (vset Mem L CL) (vset Mem (1+ L) CP)))
  
(defmacro push_E (n)			; allocates local env
  `(let ((top (+ L 4 ,n)))	
     (if (>= top BottomTR)
	 (throw 'debord (print "Local Stack Overflow")))
     (vset Mem (+ L 3) Cut_pt)
     (dotimes (i ,n top)		; n local free variables
	      (vset Mem (decf top) (cons 'LIBRE BottomG)))))
 
(defmacro maj_L (nl)			; updates top of local stack
  `(incf L (+ 4 ,nl)))


;choice-point : [a1 .. an A FR BCP BCL BG BL BP TR]
;
(defmacro TR (b) `(svref Mem (1- ,b)))
(defmacro BP (b) `(svref Mem (- ,b 2)))
(defmacro BL (b) `(svref Mem (- ,b 3)))
(defmacro BG (b) `(svref Mem (- ,b 4)))
(defmacro BCL (b) `(svref Mem (- ,b 5)))
(defmacro BCP (b) `(svref Mem (- ,b 6)))
(defmacro FR (b) `(svref Mem (- ,b 7)))
(defmacro A (b) `(svref Mem (- ,b 8)))

(defun save_args ()			; save args of current goal
  (dotimes (i (svref Mem A) (vset Mem (incf L i) i))
	   (vset Mem (+ L i) (svref Mem (+ A i 1)))))

(defun push_choix ()			; allocates a choice point
  (save_args)				; goal args
  (vset Mem (incf L) FR)		; top of f.g. stack
  (vset Mem (incf L) CP)		; continuation
  (vset Mem (incf L) CL)
  (vset Mem (incf L) G)			; top of global stack
  (vset Mem (incf L) BL)		; last choice point
  (vset Mem (incf L 2) TR)		; top of trail
  (setq BL (incf L) BG G))		; new last choice point
        
(defun push_bpr (reste) (vset Mem (- BL 2) reste))
    
(defmacro size_C (b) `(+ 8 (A ,b)))

(defun pop_choix ()		       
  (setq L (- BL (size_C BL))		; pops the block
	BL (BL BL)			; updates backtrack reg.
	BG (if (zerop BL) BottomG (BG BL))))

; III. Global Stack
;
(defmacro push_G (n)
  `(let ((top (+ G ,n)))
     (if (>= top BottomL) 
	 (throw 'debord (print "Global Stack Overflow")))
     (dotimes (i ,n (vset Mem (+ L 2) G)) ; n free global vars
	      (vset Mem (decf top) (cons 'LIBRE BottomG)))))
(defmacro maj_G (n) `(incf G ,n))

;IV. Trail
;
(defmacro fgblock (x) `(cdr (svref Mem ,x)))

(defun pushtrail (x)
  (if (>= TR A) (throw 'debord (print "Trail Overflow")))
  (vset Mem TR x)
  (incf TR))

(defun poptrail (top)	       
  (do () ((= TR top))
      (let ((x (svref Mem (decf TR))))
	(if (numberp x)			; classical var else frozen
	    (vset Mem x (cons 'LIBRE BottomG))
	  (vset Mem (car x) (cons 'LIBRE (cdr x)))))))

; V. Frozen Goals Stack
;
(defmacro FGvar (x) `(svref Mem ,x))
(defmacro FGtail (x) `(svref Mem (1+ ,x)))
(defmacro FGgoal (x) `(svref Mem (+ 2 ,x)))
(defmacro FGenv (x) `(svref Mem (+ 3 ,x)))
(defmacro frozen? (x) `(< (fgblock ,x) BottomG))

(defmacro push_fg (v b eb r)		; allocates a frozen block
  `(if (>= (+ FR 3) BottomG)
       (throw 'debord (print "Frozen Goals Stack Overflow"))
     (progn (vset Mem FR ,v)
	    (vset Mem (incf FR) ,r)
	    (vset Mem (incf FR) ,b)
	    (vset Mem (incf FR) ,eb)
	    (incf FR))))
