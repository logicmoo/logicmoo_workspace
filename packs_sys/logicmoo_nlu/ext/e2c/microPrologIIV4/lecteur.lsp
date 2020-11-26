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
