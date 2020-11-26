;;; mini-PrologII
;;; resol.lsp
;;;

(defun forward () 
  (do ()
      ((null *not-done*) (format t "no More~%"))
    (cond ((and (null *current-continuation*) ; empty continuation
                (null *awakened-goals*))      ; no awaken goals
           (answer))      
          ((load_PC)                    ; selects first goal
           (cond                   
             ((user? *current*)             ; user defined
              (let ((d (def_of *current*)))	; associated set of clauses
                (if d
                    (pr2 d)
                    (backtrack)))) 
             ((builtin? *current*)      ; builtin
              (if (eq (apply (car *current*) (cdr *current*)) 'fail)
                  (backtrack)           ; evaluation fails
                  (cont_eval)))               
             ((backtrack)))))))         ; undefined predicate
 
(defun load_A (largs el eg)             ; loads Ai registers with
  (dotimes (i (length largs)
              (vset *memory* +max-of-trail+ i)) ; goal's args
    (vset *memory* (+ +max-of-trail+ i 1) (ultimate (pop largs) el eg))))

(defun load_PC () 
  (if *awakened-goals*  
      (let ((x ()))   
        (do ()                          ; sorts the goals depending on
            ((null *awakened-goals*))   ; their freezing times
          (setq x (add_fg (pop *awakened-goals*) x)))
        (do ()                          ; allocates blocks in the 
            ((null x))                  ; corresponding order
          (create_block (abs (pop x))))))
  (setq *current* (pop *current-continuation*)
        *current-environment* (E *cl*)
        *current-goal* (top-of-global-stack *cl*)
        *specific-cut-point* *backtracking-local-register*))

(defun other_fg (b r)
  (if (< (FGtail b) +bottom-of-global-stack+)
      (add_fg (FGtail b) r)
      r))

(defun add_fg (b r)                     ; sorts the various awakened
  (let ((b1 (if (numberp (FGgoal b))
                (FGgoal b)
                b)))                    ; goals
    (if (eq (pred (FGgoal b1)) '|dif|)  ; dif first
        (insert (- b1) (other_fg b r))
        (let* ((v (svref *memory* (FGvar b1))) ; it is a freeze
               (te (val (car v) (cdr v))))
          (if (var? (car te))           ; the var is still unbound
              (let ((y (adr (car te) (cdr te))))
                (bindfg y b1 nil (fgblock y)) ; delaying is perpetuated
                (other_fg b r))
              (insert b1 (other_fg b r))))))) ; insert the goal

(defun insert (b l)                    ; sorts the goals according to 
  (if (or (null l) (> b (car l)))      ; their freezing times
      (cons b l) 
      (cons (car l) (insert b (cdr l))))) 

(defmacro dec_goal (x)
  `(if (atom ,x)
       (list ,x)
       (cons (caar ,x) (cdr ,x))))

(defun create_block (b)                 ; block for an awakened goal
  (push_cont)                           ; saves current continuation
  (vset *memory* (+ *top-of-local-stack* 2) (FGenv b)) ; its global env
  (vset *memory* (+ *top-of-local-stack* 3) *specific-cut-point*) ; the corresponding cut point
  (setq *current-continuation* (list (FGgoal b)) *cl* *top-of-local-stack*) ; new continuation
  (maj_L 0))                            ; ends the block
 
(defun pr2 (paq)                        ; proves *current*
  (load_A (largs *current*) *current-environment* *current-goal*) ; loads its arguments
  (if *current-continuation*  
      (pr paq) 
      (progn                            ; if last call and no interm.
        (when (<= *backtracking-local-register* *cl*)
          (setq *top-of-local-stack* *cl*)) ; choice point then LCO
        (setq *current-continuation* (current-continuation *cl*)
              *cl* (cl *cl*))           ; next continuation
        (pr paq))))
         
(defun cont_eval () 
  (unless *current-continuation*
    (when (<= *backtracking-local-register* *cl*)
      (setq *top-of-local-stack* *cl*))
    (setq *current-continuation* (current-continuation *cl*)
          *cl* (cl *cl*)))) 
 
(defun pr (paq)                   
  (if (cdr paq)                         ; alloc. choice point
      (progn (push_choix) (pr_choice paq))
      (pr_det (car paq))))              ; else deterministic

(defun pr_det (c) 
  (if (eq (unify_with                   ; first tries to unify
           (largs (head c))
           (push_E (nloc c))
           (push_G (nglob c)))  
          'fail) 
      (backtrack) 
      (progn                            ; success
        (maj_G (nglob c))               ; terminates global env
        (when (tail c)                  ; c is rule
          (push_cont)                   ; saves current cont.
          (setq *current-continuation* (tail c)
                *cl* *top-of-local-stack*) ; new one
          (maj_L (nloc c))))))             ; terminates local block

(defun pr_choice (paq) 
  (let* ((resu (shallow_backtrack paq))
         (c (car resu))
         (r (cdr resu)))
    (cond ((null r)                     ; only one candidate remains
           (pop_choix)                  ; pops the rerun part
           (pr_det c))                  ; proof is now deterministic
          (t (push_bpr r)                  
             (maj_G (nglob c))                    
             (when (tail c)        ; c is a rule
               (push_cont)         ; saves current cont.              
               (setq *current-continuation* (tail c)
                     *cl* *top-of-local-stack*) ; new one
               (maj_L (nloc c)))))))    ; terminates local block

(defun shallow_backtrack (paq)          ; looks for a first success
  (if (and (cdr paq)                    ; more than one candidate
           (eq (unify_with              ; and unification fails
                (largs (head (car paq)))
                (push_E (nloc (car paq)))
                (push_G (nglob (car paq))))
               'fail)) 
      (progn 
        (setq *awakened-goals* nil
              *top-of-frozen-goals-stack* (top-of-frozen-goals-stack *backtracking-local-register*)) ; restores registers
        (poptrail (top-of-trail *backtracking-local-register*)) ; restores env
        (shallow_backtrack (cdr paq)))
      paq))

(defun backtrack ()            
  (if (zerop *backtracking-local-register*) ; no more choice points
      (setq *not-done* nil)                 ; restores registers
      (progn
        (setq *top-of-local-stack*        *backtracking-local-register*
              *top-of-global-stack*       *backtracking-global-register*
              *top-of-frozen-goals-stack* (top-of-frozen-goals-stack *top-of-local-stack*)
              *awakened-goals*            nil
              *specific-cut-point*        (backtracking-local-register *backtracking-local-register*)
              *current-continuation*      (BCP *top-of-local-stack*)
              *cl*                        (BCL *top-of-local-stack*)
              *specific-cut-point*        (backtracking-local-register *backtracking-local-register*))
        (load_A2)            
        (poptrail (top-of-trail *backtracking-local-register*)) ; restores env
        (pr_choice (BP *top-of-local-stack*))))) ; relaunch the proof
    
(defun load_A2 ()                       ; restores Ai registers
  (let ((deb (- *top-of-local-stack* (size_C *top-of-local-stack*)))) 
    (dotimes (i (max-of-trail *top-of-local-stack*)
                (vset *memory* +max-of-trail+ i)) 
      (vset *memory* (+ +max-of-trail+ i 1) (svref *memory* (+ deb i))))))
 
(defun myloop (c) 
  (setq *top-of-frozen-goals-stack* +bottom-of-frozen-goals-stack+
        *top-of-global-stack* +bottom-of-global-stack+
        *top-of-local-stack* +bottom-of-local-stack+
        *top-of-trail* +bottom-of-trail+
        *specific-cut-point* 0
        *current-continuation* nil
        *cl* 0
        *backtracking-local-register* 0
        *backtracking-global-register* +bottom-of-global-stack+
        *awakened-goals* nil
        *not-done* t) 
  (push_cont)                           ; initial continuation
  (push_E (nloc c))                     ; local env. for the query
  (push_G (nglob c))                    ; global env. for the query
  (setq *current-continuation* (cdr c)
        *cl* *top-of-local-stack*)      ; current continuation
  (maj_L (nloc c))                      ; ends local block
  (maj_G (nglob c))
  (read-char)                           ; ends global block
  (catch 'debord (forward))
  (myloop (read_prompt))) 


