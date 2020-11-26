; Version 3
; boot.lsp
; (load "cynd/microPrologII.lisp")
; Mini-PrologII
; boot.lsp
;

(defun mini-PrologII ()			; to run it
  (banner)
  (format t "~A~%" (load "cynd/mlg.Start"))
  (myloop (read_prompt)))

(defun read_prompt () 
  (terpri)
  (format t "| ?- ")
  (read_code_tail))

(defun banner ()
  (dotimes (i 2) (terpri))
  (format t "Mini-PrologII~%")
  (dotimes (i 2) (terpri)))

(defun l ()
  (format t "Back to Mini-PrologII top-level~%")
  (myloop (read_prompt)))

; mini-Cprolog & mini-PrologII
; cparser.lsp
;

(defvar *lvarloc nil)                   ; list of local vars
(defvar *lvarglob nil)                  ; list of global vars
(set-macro-character #\% (get-macro-character #\;))

(defun rch ()                           ; skips Newline
  (do ((ch (read-char) (read-char)))
      ((char/= ch #\Newline) ch)))
(defun rchnsep ()                       ; skips Newline and Space
  (do ((ch (rch) (rch)))
      ((char/= ch #\space) ch)))

(defun special-char (ch) (char= ch '#\_))
(defun alphanum (ch)                    ; symbol normal constituant
  (or (alphanumericp ch) (special-char ch)))
(defun valdigit (ch) (digit-char-p ch)) 
 
(defun read_number (ch)                 ; next integer
  (do ((v (valdigit ch) (+ (* v 10) (valdigit (read-char)))))
      ((not (digit-char-p (peek-char))) v)))

(defun implode (lch) (intern (map 'string #'identity lch)))

(defun read_atom (ch)                   ; next normal symbol
  (do ((lch (list ch) (push (read-char) lch)))
      ((not (alphanum (peek-char))) (implode (reverse lch)))))

(defun read_at (ch)                     ; next special symbol
  (do ((lch (list ch) (push (read-char) lch)))
      ((char= (peek-char) #\') (read-char) (implode (reverse lch)))))

(defun read_string (ch)                 ; Prolog list of ascii codes
  (do ((lch (list (char-int ch)) (push (char-int (read-char)) lch)))
      ((char= (peek-char) #\") (read-char) (do_l (reverse lch)))))

(defun read_var (ch n)                  ; next variable
  (status (read_atom ch) n))

(defun status (nom n)                   ; n is the term's depth
  (if (= n 1)                           ; local if not yet global
      (unless (member nom *lvarglob) (pushnew nom *lvarloc))
    (progn (if (member nom *lvarloc)    ; else global
               (setq *lvarloc (delete nom *lvarloc)))
           (pushnew nom *lvarglob)))
  nom)

(defun read_simple (ch n)               ; next simple term
  (cond
   ((or (upper-case-p ch) (special-char ch)) (read_var ch n))
   ((digit-char-p ch) (read_number ch))
   ((char= ch #\") (read_string (read-char)))
   ((char= ch #\') (read_at (read-char)))
   (t (read_atom ch))))

(defun read_fct (ch n)                  ; next functional term
  (let ((fct (read_simple ch n)) (c (rchnsep)))
    (if (char= c #\()                   ; reads its arguments
        (let ((la (read_args (rchnsep) (1+ n))))
          (cons (list fct (length la)) la)) ; adds its descriptor
      (progn (unread-char c) fct))))    ; else atom
                
(defun read_args (ch n)                 ; args of a functional term
  (let ((arg (read_term ch n)))
    (if (char= (rchnsep) #\,)
        (cons arg (read_args (rchnsep) n))
      (list arg))))
   
(defun read_list (ch n)                 ; next list
  (if (char= ch #\])                    ; ending with the empty list
      ()
    (let ((te (read_term ch n)))        ; gets the head
      (case (rchnsep)
            (#\, (list '(\.  2) te (read_list (rchnsep) n)))
            (#\| (prog1                 ; dotted pair
                     (list '(\. 2) te (read_term (rchnsep) n)) 
                   (rchnsep))) 
            (#\] (list '(\. 2) te nil))))))

(defun read_term (ch n)                 ; next term
  (if (char= ch #\[) (read_list (rchnsep) (1+ n)) (read_fct ch n))) 

(defun read_tail (ch)                   ; read the body of a rule
  (let ((tete (read_pred ch)))          ; gets the first goal
    (if (char= (rchnsep) #\.)
        (list tete)
      (cons tete (read_tail (rchnsep))))))

(defun read_clause (ch)                 ; reads a clause
  (let ((tete (read_pred ch)))
    (if (char= (rchnsep) #\.)
        (list tete)
      (progn (read-char) (cons tete (read_tail (rchnsep)))))))

(defun c (l)                            ; codes once read
  (if (atom l)
      (if (member l *lvarloc)
          (cons 'L (position l *lvarloc))
        (if (member l *lvarglob) (cons 'G (position l *lvarglob)) l))
    (if (eq (car l) '!)                 ; the cut with its intern
        (list '! (length *lvarloc))     ; argument
      (cons (c (car l)) (c (cdr l))))))
; mini-PrologII
; parser.lsp
;

(defun read_code_cl ()			; to read and code a clause
  (let ((*lvarloc ()) (*lvarglob ()))
    (let ((x (read_clause (rchnsep))))
      (maj_locglob (car x) (car (last x)))
      (cons				; number of local and global 
       (cons (length *lvarloc) (length *lvarglob)) ; vars
       (c x)))))
              
(defun read_code_tail ()		; idem for a query
  (setq *lvarloc () *lvarglob ())
  (let ((x (read_tail (rchnsep))))
    (cons
     (cons (length *lvarloc) (length *lvarglob))
     (append (c x) (list '(|true|))))))

(defun read_pred (ch)			; to read a literal
  (let ((nom (read_atom ch)) (c (rchnsep)))
    (if (char= c #\()
	(cons nom
	      (read_args (rchnsep)    
			 (if (member nom '(|dif| |freeze|)) 
			     2		; a var occurring in a dif
			   1)))		; or in a freeze is global
      (progn (unread-char c) (list nom)))))

(defun unsafe? (x h q)			; occurs in the body 
  (and (member x q) (not (member x h)))) ; but not in the head

(defun maj_locglob (h q)		; to classify the variables
  (mapc #'(lambda (x) 
	    (when (unsafe? x h q)
		  (setq *lvarloc (delete x *lvarloc))
		  (push x *lvarglob)))
	*lvarloc))
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
; miniCprolog & Mini-PrologII
; cutili.lsp
;

(defmacro nloc (c) `(caar ,c))          ; number of local vars
(defmacro nglob (c) `(cdar ,c))         ; number of global vars
(defmacro head (c) `(cadr ,c))          ; head of a clause
(defmacro tail (c) `(cddr ,c))          ; body of a clause
(defmacro pred (g) `(car ,g))           ; predicate symbol
(defmacro largs (g) `(cdr ,g))          ; list of arguments

(defmacro functor (des) `(car ,des))
(defmacro arity (des) `(cadr ,des))
(defmacro des (te) `(car ,te))          ; functor/arity
(defmacro var? (v) `(and (consp ,v) (numberp (cdr ,v))))
(defmacro list? (x) `(eq (functor (des ,x)) '\.))
 
(defmacro user? (g)                     ; user defined predicate
  `(get (pred ,g) 'def))
(defmacro builtin? (g)                  ; builtin predicate
  `(get (pred ,g) 'evaluable))
(defmacro def_of (g)                    ; gets the subset of clauses
  `(get (pred ,g)                       ; depending on the nature of
        (if (largs ,g)                  ; the first arg of goal PC
            (nature (car (ultimate (car (largs ,g)) PCE PCG)))
          'def)))

(defun nature (te)                      ; determines the associated
  (cond                                 ; subset according to
   ((var? te) 'def)                     ; clause indexing
   ((null te) 'empty)
   ((atom te) 'atom)
   ((list? te) 'list)
   (t 'fonct)))

(defun add_cl (pred c ind)              ; adds a clause to the end
  (setf (get pred ind) (append (get pred ind) (list c))))

(set-macro-character                    ; to automatically read,
 #\$                                    ; code and index
 #'(lambda (stream char)
     (let* ( (*standard-input* stream) (c (read_code_cl)))
       (add_cl (pred (head c)) c 'def)  ; always in the var subset
       (if (largs (head c)) 
           (let ((b (nature (car (largs (head c))))))
             (if (eq b 'def)            ; first arg is a variable
                 (mapc                  ; add c to all the subsets
                  #' (lambda (x) (add_cl (pred (head c)) c x))
                  '(atom empty list fonct))
               (add_cl (pred (head c)) c b))))) ; add to only one subset
     (values)))
  
(defun answer ()                        ; prints the values of vars
  (printvar)                            ; occurring in the query
  (if (zerop BL)                        ; no more choice point
      (setq Duboulot nil)
    (if (and (princ "More : ")          ; asks the user
             (string= (read-line) ";")) ; if necessary
        (backtrack)                     ; forces backtracking
      (setq Duboulot nil))))
        
(defun printvar ()
  (if (and (null *lvarloc)              ; ground query ?
           (null *lvarglob))
      (format t "Yes ~%")               ; success
    (let ((nl -1) (ng -1))
      (mapc                             ; first, local variables
       #' (lambda (x)
            (format t "~A = " x)
            (write1 (ult (cons 'L (incf nl)) (E BottomL))) (terpri))
       *lvarloc)
      (mapc                             ; then global variables
       #' (lambda (x) 
            (format t "~A = " x)
            (write1 (ult (cons 'G (incf ng)) BottomG)) (terpri))
       *lvarglob))))
; mini-PrologII
; unify.lsp
;

(defmacro bind (x sq e xt)		; binds x to (sq,e)
  `(progn (if (or (and (> ,x BottomL) (< ,x BL)) (< ,x BG)) 
	      (pushtrail ,xt))		; if post-bound trail it
	  (rplaca (svref Mem ,x) ,sq)
	  (rplacd (svref Mem ,x) ,e)))

(defun bindte (x sq e)			; binds x to (sq,e)
  (if (frozen? x)
      (let ((y (fgblock x)))
	(push y FRCP)			; awakes the delayed goals
	(bind x sq e (cons x y)))	; to trail the old value
    (bind x sq e x)))

(defun bindfg (x b eb r)		; binds x to the frozen goal b
  (bind x 'LIBRE FR (if (frozen? x) (cons x r) x))
  (push_fg x b eb r))			; allocates a new frozen block

(defun unify_with (largs el eg)		; unifies head of clause
  (catch 'impossible			; with registers Ai
    (dotimes (i (svref Mem A))
	     (unif
	      (let ((te (svref Mem (+ A 1 i)))) (val (car te) (cdr te)))
	      (ultimate (pop largs) el eg)))))

; mini-Cprolog & mini-PrologII
; cunify.lsp
;
(defmacro adr (v e) `(+ (cdr ,v) ,e)) 
(defmacro value (v e) `(svref Mem (adr ,v ,e))) 
     
(defun ult (v e)                        ; dereferences variable v
  (let ((te (value v e)))               ; in environment e
    (cond 
     ((eq (car te) 'LIBRE) (cons v e))  ; if unbound, itself
     ((var? (car te)) (ult (car te) (cdr te))) 
     ( te))))                           ; its value
 
(defun val (x e)                        ; generalises to a term
  (if (var? x) (ult x e) (cons x e))) 
 
(defun ultimate (x el eg)               ; idem but selects env
  (if (var? x)  
      (if (eq (car x) 'L) (ult x el) (ult x eg)) 
    (cons x eg))) 

(defmacro bindv (x ex y ey)             ; L2 Binding
  `(let ((ax (adr ,x ,ex)) (ay (adr ,y ,ey)))
     (if (< ax ay)                      ; the younger one is always 
         (bindte ay ,x ,ex)             ; bound to the senior one
       (bindte ax ,y ,ey))))
 
(defun unif (t1 t2)                     ; unify two terms t1 and t2
  (let ((x (car t1)) (ex (cdr t1)) (y (car t2)) (ey (cdr t2)))
    (cond 
     ((var? y)  
      (if (var? x)                      ; two variables
          (if (= (adr x ex) (adr y ey)) t (bindv y ey x ex))
        (bindte (adr y ey) x ex)))      ; binds y
     ((var? x) (bindte (adr x ex) y ey))
     ((and (atom x) (atom y))           ; two constants
      (if (eql x y) t (throw 'impossible 'fail)))
     ((or (atom x) (atom y)) (throw 'impossible 'fail))
     ( (let ((dx (pop x)) (dy (pop y))) ; two structured terms
         (if (and (eq (functor dx) (functor dy)) 
                  (= (arity dx) (arity dy)))
             (do () ((null x))          ; same functor and arity
                 (unif (val (pop x) ex) (val (pop y) ey)))
           (throw 'impossible 'fail)))))))
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
	    (push_cont) 		; saves current cont.
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
; mini-Cprolog & mini-PrologII
; cpred.lsp
;

(defmacro value1 (x) `(car (ultimate ,x PCE PCG)))
(defun uni (x y)
  (catch 'impossible
    (unif (ultimate x PCE PCG) (ultimate y PCE PCG))))
					;write/1 (?term)
(defun |write| (x)
  (write1 (ultimate x PCE PCG)))
(defun write1 (te)			; depending on te
  (let ((x (car te)) (e (cdr te)))
    (cond 
     ((null x) (format t "[]"))
     ((atom x) (format t "~A" x))
     ((var? x) (format t "X~A" (adr x e)))
     ((list? x) (format t "[")
      (writesl (val (cadr x) e) (val (caddr x) e))
      (format t "]"))
     ((writesf (functor (des x)) (largs x) e)))))
(defun writesl (te r)			; for a list
  (write1 te)
  (let ((q (car r)) (e (cdr r)))
    (cond
     ((null q))
     ((var? q) (format t "|X~A" (adr q e)))
     (t (format t ",") 
	(writesl (val (cadr q) e) (val (caddr q) e))))))
(defun writesf (fct largs e)		; for a functional term
  (format t "~A(" fct)
  (write1 (val (car largs) e))
  (mapc #' (lambda (x) (format t ",") (write1 (val x e))) 
	   (cdr largs))
  (format t ")"))
					;nl/0
(defun |nl| () (terpri))
					;tab/1 (+int)
(defun |tab| (x)
  (dotimes (i (value1 x)) (format t " ")))
					;read/1 (?term)
(defun |read| (x) 
  (let ((te (read_terme)))		; gets the term
    (catch 'impossible 
      (unif (ultimate x PCE PCG) 
	    (cons (cdr te) (push1_g (car te)))))))
(defun read_terme ()
  (let ((*lvarloc nil) (*lvarglob nil))
    (let ((te (read_term (rchnsep) 2)))
      (rchnsep) (cons (length *lvarglob) (c te)))))
(defun push1_g (n)
  (if (>= (+ G n) BottomL)		; allocates a global env
      (throw 'debord (print "Global Stack Overflow")))
  (dotimes (i n (- G n)) 
	   (vset Mem G (cons 'LIBRE BottomG)) 
	   (incf G)))
					;get/1 (?car)
(defun |get| (x)
  (uni x (char-int (rchnsep))))
					;get0/1 (?car)
(defun |get0| (x)
  (uni x (char-int (read-char))))
					;var/1 (?term)
(defun |var| (x)
  (unless (var? (value1 x)) 'fail))
					;nonvar/1 (?term)
(defun |nonvar| (x)
  (if (var? (value1 x)) 'fail))
					;atomic/1 (?term)
(defun |atomic| (x)
  (if (listp (value1 x)) 'fail))
					;atom/1 (?term)
(defun |atom| (x)
  (unless (symbolp (value1 x)) 'fail))
					;number/1 (?term)
(defun |number| (x)
  (unless (numberp (value1 x)) 'fail))
					;fail/0
(defun |fail| () 'fail)
					;true/0
(defun |true| ())
					;divi/3 (+int,+int,?int)
(defun |divi| (x y z)
  (uni z (floor (value1 x) (value1 y))))
					;mod/3 (+int,+int,?int)
(defun |mod| (x y z)
  (uni z (rem (value1 x) (value1 y))))
					;plus/3 (+int,+int,?int)
(defun |plus| (x y z)
  (uni z (+ (value1 x) (value1 y))))
					;minus/3 (+int,+int,?int)
(defun |minus| (x y z)
  (uni z (- (value1 x) (value1 y))))
					;mult/3 (+int,+int,?int)
(defun |mult| (x y z)
  (uni z (* (value1 x) (value1 y))))
					;le/2 (+int,+int)
(defun |le| (x y)
  (if (> (value1 x) (value1 y)) 'fail))
					;lt/2 (+int,+int)
(defun |lt| (x y)
  (if (>= (value1 x) (value1 y)) 'fail))
					;name/2 (?atom,?list)
(defun |name| (x y)
  (let ((b (value1 x)))
     (if (var? b) 
         (uni x (impl (undo_l (ultimate y PCE PCG))))
         (uni y (do_l (expl b))))))

(defun undo_l (te)
  (let ((x (car te)) (e (cdr te)))
    (if (atom x) 
	x
      (cons (undo_l (val (cadr x) e)) (undo_l (val (caddr x) e))))))
(defun do_l (x)
  (if (atom x) x (list '(\. 2) (car x) (do_l (cdr x)))))
(defun impl (l)
  (intern (map 'string #'int-char l)))
(defun expl (at)
  (map 'list #'char-int (string at)))

					;consult/1 (+atom)
(defun |consult| (f)
  (format t "~A~%" (load (value1 f))))
					; abolish/1 (+ atom)
(defun |abolish| (p)
  (mapc  #'(lambda (x) (setf (get p x) nil))
	 '(atom empty list fonct def)))
					; cputime/1 (? int)
(defun |cputime| (x)
  (uni x (float (/ (get-internal-run-time) 
		   internal-time-units-per-second))))

