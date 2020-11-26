(in-package "EXPDT")

(eval-when (:compile-toplevel :load-toplevel)
   (let ((p (find-package "PDDL")))
     (cond (p
	    (import (mapcar #'(lambda (s) (intern s p))
			    '("DISCRIM" "CONTENTS" "TREE" "SIZE"))
		    (find-package "EXPDT"))))))

(export '(EXP_INDEX REHASH_SPEC EXPCOORD TOPCOORD
	  EXPTREE-INIT EXP-INDEX-INIT
	  EXP-FETCH EXP-OB-INDEX EXP-REHASH-KEY
	  EXP-INDEX-SUBCONTS EXP-INDEX-COPY
	  CONTENTS TREE SIZE exp_index-contents
	  FORCE-REHASH DISCRIM EXPINDEX-SEE TOPCOORD*)
	(find-package "EXPDT"))

; Discrimination trees of objects indexed by s-expressions they contain
; depends on symboid.lisp, where the package "EXPDT" is defined

;; Discriminations are made on the basis of the S-expressions
;; appearing at a given position in the data to be discriminated.  A
;; position is called an "expression coordinate" (expcoord).  A
;; position is represented by an a "symbol" with a print name
;; CC(A | D)*R.  Actually, this thing is not a symbol at all, but the
;; unwary user is not going to realize that, poor devil.
;; Every expcoord is related to others in a
;; straightforward way.  E.g., for expcoord CCAAR,
;;   CCR
;;    CCAR                its UP is (CCAAR CCAR CCR)
;;      CCAAR
;;        CCAAAR          its DOWNLEFT is CCAAAR
;;        CCDAAR          its DOWNRIGHT is CCDAAR
;;      CCDAR

(defstruct (expcoord
	      (:print-function
	          (lambda (c srm k)
		     (declare (ignore k))
		     (format srm "CC")
		     (do ((cl (expcoord-up c) (cdr cl)))
			 ((null (cdr cl)))
			(cond ((expcoord-is-cdr (car cl))
			       (format srm "D"))
			      (t
			       (format srm "A"))))
		     (format srm "R"))))
   apply
   is-cdr
   up
   dl
   dr
   is-var)
   
(defun new-expcoord (c x is-var)
   (let ((newc (make-expcoord
		  :apply (cond (c
				(cond ((eq x 'd)
				       (let ((td (try-cdr is-var)))
					 (cond ((null (cdr (expcoord-up c)))
						td)
					       (T
						#'(lambda (x)
						    (funcall
						       td
						       (funcall
							  (expcoord-apply c)
							  x)))))))
				      (t
				       (let ((ta (try-car is-var)))
					 (cond ((null (cdr (expcoord-up c)))
						ta)
					       (t
						#'(lambda (x)
						    (funcall
						       ta
						       (funcall
							  (expcoord-apply c)
							  x)))))))))
				 (t #'identity))
		  :is-cdr (eq x 'd)
		  :up (if (not c) '() (expcoord-up c))
		  :dl nil
		  :dr nil
		  :is-var is-var)))
      (push newc (expcoord-up newc))
      newc))
  
(declaim (inline expcoord-is-car))

(defun expcoord-is-car (c) (not (expcoord-is-cdr c)))
  
(defun expcoord-downright (c)
    (or (expcoord-dr c)
	(let ((new (new-expcoord c 'd (expcoord-is-var c))))
	  (setf (expcoord-dr c) new)
	  new)))
  
(defun expcoord-downleft (c)
    (or (expcoord-dl c)
	(let ((new (new-expcoord c 'a (expcoord-is-var c))))
	  (setf (expcoord-dl c) new)
	  new)))

(declaim (inline apply-coord))

(defun apply-coord (c x)
   (exp-keyify (funcall (expcoord-apply c) x)
	       (expcoord-is-var c))  )

(defun try-car (is-var)
   #'(lambda (x)
      (cond ((or (funcall is-var x) (eq x '*dontcare))
	     '*dontcare)
	    ((consp x)
	     (car x))
	    (t '*na))))

(defun try-cdr (is-var)
   #'(lambda (x)
      (cond ((or (funcall is-var x) (eq x '*dontcare))
	     '*dontcare)
	    ((consp x) 
	     (cdr x))
	    (t '*na))))

(defun exp-keyify (x is-var)
   (cond ((funcall is-var x) '*dontcare)
         ((atom x) (symbolify x))
         (t '*struct)))

(defvar topcoord*)   ; must be set by package using this system.

(defun topcoord (is-var)
   (new-expcoord nil 'a is-var))

(defstruct (exp_key (:type list)
		(:constructor
		    make-exp_key (coord val)))
   coord
   val)

(defun make-exptree_cell (key cell)
    (cons key cell))

(defvar exptree-dbg* nil)
(defvar dofix* nil)
(defvar rehash-imbalance-thresh* 2)
(defvar imbalance-thresh* 0.9)

(defun make-exptree_table (coord)
   (cons coord (vector '*rehash-in-progress
		       t '())))

(defun exptree_table-coord (tb)
   (car tb))

(defun exptree_table-status (tb)
   (aref (cdr tb) 0))

(defun set-exptree_table-status (tb v)
   (setf (aref (cdr tb) 0) v))

(defsetf exptree_table-status set-exptree_table-status)

(defun exptree_table-assqable (tb)
   (aref (cdr tb) 1))

(defun set-exptree_table-assqable (tb v)
   (setf (aref (cdr tb) 1) v))

(defsetf exptree_table-assqable set-exptree_table-assqable)

(defun exptree_table-entries (tb)
   (aref (cdr tb) 2))

(defun set-exptree_table-entries (tb v)
   (setf (aref (cdr tb) 2) v))

(defsetf exptree_table-entries set-exptree_table-entries)

(defun exptree_table-operative (table tree)
    (or (eq (exptree_table-status table) '*rehash-done)
	(progn
	   (exptree-cleanup tree table)
	   nil)))

(defun make-exptree_entry (val tree)
  (cons val tree))

(defun exptree_entry-val (e) (car e))
(defun exptree_entry-tree (e) (cdr e))

(defvar blip-sw* nil)
(defvar rehash-thresh* 2)

(defstruct (exptree
	      (:print-function
	         (lambda (self srm k)
		    (declare (ignore k))
		    (format srm "#<exptree ~s/" (exptree-cell self))
		    (cond ((exptree-first-operative-table self)
			   (format srm "~s"
			      (mapcan #'(lambda (tab)
					   (cond ((exptree_table-operative
						     tab self)
						  (list (exptree_table-coord
                                                           tab)))
						 (t '())))
				      (exptree-tables self))))
			  (t
			   (format srm "~s objects"
				   (length (exptree-objects self)))))
		    (format srm ">")))
	      (:constructor
	          new-exptree (cell superior objects tables rehash-prospects)))
   cell   ; exptree_cell
   superior   ; exptree or nil
   objects    ; list of objects here when here is terminal
   tables     ; list of subtables when nonterminal
   rehash-prospects)   ; prospects for adding to tables
         ; If not *UNKNOWN, this is a list of expcoords that must be nontrivial
         ; in fetch pattern if rehashing should be attempted.
			    
(defun make-exptree (cell sup)
   (new-exptree cell sup '() '() '*unknown))

(defun exptree-table (tr prop)
   (assoc prop (exptree-tables tr) :test #'eq))

(defun exptree-operative-table (tree coord)
    (let ((table (exptree-table tree coord)))
      (if (and table (exptree_table-operative table tree))
	  table nil)))

(defun exptree-first-operative-table (tree)
    (do ((tl (exptree-tables tree) (cdr tl)))
	((or (null tl)
	     (exptree_table-operative (car tl) tree))
	 (cond (tl (car tl))
	       (t nil)))))

(defun exptree-contents (tree copy)
     (labels ((collect (tree copy)
		(let ((table (exptree-first-operative-table tree)))
		   (if (not table)
		       (cond (copy (copy-list (exptree-objects tree)))
			     (t (exptree-objects tree)))
		     (mapcan #'(lambda (e) (collect (exptree_entry-tree e) t))
			     (exptree_table-entries table))))))
	(collect tree copy)))

(defun exptree-empty (dt)
      (let ((table (exptree-first-operative-table dt)))
        (if (not table) 
            (null (exptree-objects dt))
            (every #'(lambda (e)
		        (exptree-empty (exptree_entry-tree e)))
		   (exptree_table-entries table)))))

(defun exptree-size (tree)
     (let ((table (exptree-first-operative-table tree)))
       (if (not table)
	   (list-length (exptree-objects tree))
	   (reduce #'(lambda (n entry)
		        (+ n (exptree-size (exptree_entry-tree entry))))
		   (exptree_table-entries TABLE)
		   :initial-value 0))))

(declaim (inline exptree-object-add
		 exptree-object-del
		 exptree-table-add
		 exptree-table-del))

(defun exptree-object-add (tr x)
	 (push x (exptree-objects tr)))

(defun exptree-object-del (tr x)
	 (setf (exptree-objects tr)
	       (delete x (exptree-objects tr) :test #'eq :count 1)))

(defun exptree-table-add  (tr tb)
	 (push tb (exptree-tables tr)))

(defun exptree-table-del (tr tb)
	 (setf (exptree-tables tr)
	       (delete tb (exptree-tables tr) :test #'eq :count 1)))

(defun exptree-cleanup (tree table)
   (cond ((and dofix* (eq (exptree_table-status table) '*rehash-in-progress))
	  (format *error-output*
		   "***Rendering table for ~s inoperative***~%"
 		   (exptree_table-coord table))
	  (exptree-table-del tree table))))

(defun exptree-init () (make-exptree '() nil))

(declaim (inline expcoord-used))

(defun expcoord-used (c dt)
    (assoc c (exptree-tables dt) :test #'eq))

(defstruct rehash_spec
   coordfn 
   piecefn
   topcoord)

(defun rehash_spec-is-var (rs)
      (expcoord-is-var (rehash_spec-topcoord rs)))

; At the top level, need to store the procedure PIECEFN that extracts
; index pattern from objects.
(defstruct (exp_index
              (:print-function
	          (lambda (c srm k)
		     (declare (ignore k))
                     (format srm "#<exp_index ~s>"
                             (exp_index-tree c)))))

   tree rehash)

(defun exp_index-empty (ei)
   (exptree-empty (exp_index-tree ei)))

(defun exp_index-contents (ei copy)
      (exptree-contents (exp_index-tree ei) copy))

(defun exp-index-init (piecefn &optional (topcoord topcoord*))
   (make-exp_index :tree (exptree-init)
		   :rehash (make-rehash_spec :coordfn #'exp-rehash-key
					     :piecefn piecefn
					     :topcoord topcoord)))

(defun exp-index-copy (e)
   (labels ((exptree-copy (tr sup)
	       (new-exptree
		  (exptree-cell tr)
		  sup
		  (exptree-objects tr)
		  (mapcar #'(lambda (tab) (tab-copy tab sup))
			  (exptree-tables tr))
		  (exptree-rehash-prospects tr)))
	    (tab-copy (tab sup)
		(let ((newtab
		          (make-exptree_table
			     (exptree_table-coord tab))))
		   (setf (exptree_table-status newtab)
			 (exptree_table-status tab))
		   (setf (exptree_table-assqable newtab)
			 (exptree_table-assqable tab))
		   (setf (exptree_table-entries newtab)
			 (mapcar #'(lambda (ent)
				      (make-exptree_entry
				         (exptree_entry-val ent)
					 (exptree-copy
					     (exptree_entry-tree ent)
					     sup)))
				 (exptree_table-entries tab)))
		   newtab)))
	 (make-exp_index
	     :tree (exptree-copy (exp_index-tree e) nil)
	     :rehash (exp_index-rehash e))))

; Is every element of e1 in e2 (eq-tested)?
(defun exp-index-subconts (e1 e2)
   (let ((piecefn (rehash_spec-piecefn (exp_index-rehash e1))))
      (labels ((search1 (subtree)
		  (let ((optab (exptree-first-operative-table subtree)))
		     (cond ((not optab)
			    (every #'find2 (exptree-contents subtree nil)))
			   (t
			    (every #'(lambda (e) (search1 (exptree_entry-tree e)))
				   (exptree_table-entries optab))))))
	       (find2 (b1)
		  (member b1 (exp-fetch (funcall piecefn b1)
					e2 t)
			  :test #'eq)))
	  (search1 (exp_index-tree e1)))))

(defun exp-ob-index (ob ind add-sw)
   (let ((r (exp_index-rehash ind)))
      (exptree-index (exp_index-tree ind)
		     ob (funcall (rehash_spec-piecefn r) ob)
		     add-sw)))

(defun exptree-index (tree thing expr add-sw)
    (labels
	((process (tables found)
	   (cond (tables
		  (let ((tab (car tables)))
		     (case (exptree_table-status (car tables))
			(*rehash-done
			 (exp-table-index tab thing expr add-sw tree)
			 (process (cdr tables) t))
			(*rehash-in-progress
			 (exptree-cleanup tree tab)
			 (process (cdr tables) found))
			(t
			 (cond ((and add-sw
				     (not (eql (apply-coord (exptree_table-coord tab)
							    expr)
					      (exptree_table-status tab))))
                                ; un-nix-key if new thing differs
                                ; at this coord from the dominant symbol
				(exptree-table-del tree tab)
                                (setf (exptree-rehash-prospects tree) '*unknown)))
			 (process (cdr tables) found)))))
		 (found)
		 (add-sw
		  (cond (blip-sw* (format *error-output* "'")))
		  (cond (exptree-dbg*
			 (format *error-output*
			   "***DT debug: adding ~s to exptree ~s~%" 
			   thing
			   (exptree-cell tree))))
		  (exptree-object-add tree thing))
		 (T
		  (cond (blip-sw* (format *error-output* "'")))
		  (cond (exptree-dbg*
			 (format *error-output*
			   "***DT debug: removing ~s from exptree ~s~%" 
			   thing
			   (exptree-cell tree))))
		  (exptree-object-del tree thing)))))
      (process (exptree-tables TREE) nil)))

(defun exp-table-index (table thing expr add-sw tree)
  (let ((entry (coord-entry (exptree_table-coord table) expr table tree)))
    (exptree-index (exptree_entry-tree entry)
		   thing expr add-sw)))

(defun coord-entry (c expr table tree)
   (let ((val (apply-coord c expr)))
      (let ((entry (best-assoc-fetch val table)))
	(or entry
	    (let ((entry (make-exptree_entry val
				 (make-exptree
				       (make-exptree_cell
					     (make-exp_key
					         (exptree_table-coord table)
						 val)
					     (exptree-cell tree))
                                       tree))))
	       (push entry (exptree_table-entries table))
               (cond ((and (exptree_table-assqable table)
                           (not (is-symboid val)))
                      (setf (exptree_table-assqable table) nil)))
	       entry)))))

(defun exp-fetch (pat ind variant)
   (exptree-fetch pat (exp_index-tree ind) variant (exp_index-rehash ind)))

(defun exptree-fetch (pat dt variant rehash)
  (multiple-value-bind
      (table val)
      (choose-exptree-table pat dt variant rehash)
    (if table
	(nconc (let ((entry1 (best-assoc-fetch val table)))
		  (cond (entry1
			 (exptree-fetch
			    pat (exptree_entry-tree entry1) variant rehash))
			(t '())))
	       (cond (variant '())
		     (t
		      (let ((entry2 (assoc '*dontcare
					   (exptree_table-entries TABLE)
					   :test #'eq)))
			 (cond (entry2
				(exptree-fetch pat (exptree_entry-tree entry2)
					       variant rehash))
			       (t '()))))))
	(exptree-contents DT t))))

(defun best-assoc-fetch
    (val table)
  (if (exptree_table-assqable table)
      (assoc  val (exptree_table-entries table) :test #'eq)
      (assoc val (exptree_table-entries table))))

;; All of the complexity in this algorithm is due to the fact that
;; there is a substantial overhead for realizing that there is no point 
;; in further discrimination ("rehashing") at a dtree node.  
;; After an unsuccessful rehash, we look hard to see if there's ever going
;; to be another chance.
;; REHASH-PROSPECTS is a list of all pieces of last unsuccessful fetch pattern
;; that, if they had been present, might have caused a successful rehash.
;; We hope that in the typical case, this can be shown to be (), so
;; that you never have to think about rehashing this node again.

;; In the old regime, the only 
;; way to detect that rehashes were impossible was  to try all possible ways and reject them
;; all.  Unfortunately, these futile searches were the norm.  If fetching (FOO (F
;; ?X)), and already discriminated on CCAR, CCAADR, and determined
;; that the CCADR doesn't help, then you're done.  Hence we provide
;; two mechanisms for generating expcoords to discriminate on: a
;; general-purpose, slow keylist generator; and a fast traverser that
;; finds the first key the keylist version would generate, or returns
;; nil if the keylist version would generate nothing.  In the
;; fully-discriminated case, this is the norm, and it should happen as
;; fast as possible.  It's not clear this complexity is still worth the trouble,;; but it's been left in.

;; Returns subindex plus obj at coord for dt within it
(defun choose-exptree-table (pat dt variant rehash)
  (do ((table)
       (tbls (exptree-tables dt) (cdr tbls))
       (found nil) (val))
      ((or found (null tbls))
       (cond (found (values table val))
	     (t
	      (let ((rhp (exptree-rehash-prospects dt)))
		(cond ((and (not (null rhp))
			    (or (exptree-first-operative-table dt)
				(>= (exptree-size dt) rehash-thresh*))
			    (or (eq rhp '*unknown)
				(some #'(lambda (c)
					  (let ((p (apply-coord c pat)))
					     (and (not (eq p '*na))
						  (or variant
						      (not (eq p '*dontcare)))
						  (not (key-nixed c p dt)))))
				      rhp)))
		       (exptree-rehash pat dt variant rehash))
		      (t
		       (cond (exptree-dbg*
			      (format *error-output*
				      "No prospect of rehash~%")))
		       (values nil nil)))))))
      (setq table (car tbls))
      (if (exptree_table-operative table dt)
	(multiple-value-setq
	    (found val)
            (pat-has-key-val pat (exptree_table-coord table) variant)))))

(defun pat-has-key-val (pat c variant)
  (let ((disc-val (apply-coord c pat)))
    (cond ((eq disc-val '*dontcare)
	   (if variant (values t '*dontcare) (values nil '())))
          (t (values t disc-val)))))

;; Pick a new key to break the contents of DT down by.  Create, plug in,
;; and return new subindex for it.  If no key worth rehashing on, return NIL.
; Returns subindex plus dnkeyval for dt within it.
(defun exptree-rehash (pat dt variant rehash)
  (multiple-value-bind (coord val rehash-keys)
		       (funcall (rehash_spec-coordfn rehash)
                               pat dt (rehash_spec-topcoord REHASH) VARIANT)
    ; First call fast algorithm to see if there's a candidate key.
    (cond ((not coord)
	   (cond (exptree-dbg*
		  (format *error-output* "Not worth rehashing ~%")))
           (setf (exptree-rehash-prospects dt)
		 (missing-pieces pat dt (rehash_spec-topcoord rehash)))
	   (values nil nil))
	  (t
	   (cond (exptree-dbg*
		  (format *error-output*
                          "Rehashing exptree ~s ~%  Attempting rehash on coord "
			  (exptree-cell dt) coord)))
	   (let ((dofix* nil))
	     (let* ((things (exptree-contents dt nil))
		    (table (new-exptree-table dt coord)))
	       (cond ((rehash-exptree-things table things dt
                                             (rehash_spec-piecefn rehash)
                                             val)
		      (values table val))
		     (t
		      ; If the first candidate doesn't work, fall back on
		      ; the complex method.
		      (do ((key)
			   (kl (cond (rehash-keys
				      (funcall rehash-keys
					       pat dt
					       (rehash_spec-topcoord rehash)
					       variant))
				     (t '()))
			       (cdr kl))
			   (table) (pos))
			  ((null kl)
			   (progn
			      (cond (exptree-dbg*
				     (format *error-output*
					     "No more keys to rehash on ~
					       -- rehashing failed")))
			      (setf (exptree-rehash-prospects dt)
				    (missing-pieces
				       pat dt
				       (rehash_spec-topcoord rehash)))
			      (values nil nil)))
			  (setf key (car kl))
			  (setf pos (exp_key-coord key))
                          (setf table
			      (new-exptree-table dt pos))
			  (cond (exptree-dbg*
				 (format *error-output*
                                   "Attempting rehash on coord ~s~%")))
			  (cond ((and table
				      (rehash-exptree-things
				         table things dt
					 (rehash_spec-piecefn rehash) 
					 (exp_key-val key)))
				 (return (values table (exp_key-val key))))
			  ))))))))))

(defun missing-pieces (pat et topcoord)
   (let ((is-var (expcoord-is-var topcoord)))
      (labels ((collect-pieces (pat c)
		 (cond ((funcall is-var pat)
			(cond ((key-useless-here c et) '())
			      (t (list c))))
		       ((atom pat) '())
		       (t
			(do ((pl pat (cdr pl))
			     (R '()))
			    ((null pl)
			     r)
			  (setf r (nconc (collect-pieces
					     (car pl)
					     (expcoord-downleft C))
					r))
			  (setf c (expcoord-downright c)))))))
	 (collect-pieces pat topcoord))))

; Try to rehash IND so that a fetch for PAT won't find AVOID
; Return t if successful.  PAT and AVOID differ in value at 
; coord DIF
(defun force-rehash (pat avoid dif ind variant)
   (exptree-force pat avoid dif 
                  (exp_index-tree ind) variant (exp_index-rehash ind)))

(defun exptree-force (pat avoid dif dt variant rehash)
  (multiple-value-bind
      (table val)
      (choose-exptree-table pat dt variant rehash)
       ; general CHOOSEr is not needed, but probably won't hurt.
    (if table
	(or (let ((entry1 (best-assoc-fetch val table)))
	       (cond (entry1
		      (exptree-force
			    pat avoid dif
			    (exptree_entry-tree entry1)
			    variant rehash))
		     (t nil)))
	    (cond (variant nil)
		  (t
		   (let ((entry2 (assoc '*dontcare
					(exptree_table-entries TABLE)
					:test #'eq)))
		      (cond (entry2
			     (exptree-force pat avoid dif
					    (exptree_entry-tree entry2)
					    variant rehash))
			    (t nil))))))
	(cond ((member avoid (exptree-contents dt nil) :test #'eq)
               (setf table (new-exptree-table dt dif))
	       (let ((piecefn (rehash_spec-piecefn rehash)))
                  (dolist (thing (exptree-contents dt nil))
                     (exp-table-index table thing (funcall piecefn thing)
				      t dt)))
	       (setf (exptree-objects DT) '())
	       (setf (exptree_table-status table) '*rehash-done)
	       (setf (exptree-rehash-prospects dt) '*unknown)
               (let ((sub (best-assoc-fetch (apply-coord dif pat)
                                            table)))
                  (or (not sub)
                      (not (member avoid
				   (exptree-contents (exptree_entry-tree SUB)
						     nil)
				   :test #'eq)))))
              (t nil)))))

(defun new-exptree-table (tree coord)
  (let ((table (expcoord-used coord tree)))
    (cond ((not table)
	   (let ((table (make-exptree_table coord)))
	     (exptree-table-add tree table)
	     table))
	  ((eq (exptree_table-status table) '*rehash-in-progress)
	   (cerror "I'll ignore it"
                   "Table's status already indicates *REHASH-IN-PROGRESS")
	   (setf (exptree_table-entries table) '())
	   table)
	  (t (setf (exptree_table-status table) '*rehash-in-progress)
	     (setf (exptree_table-entries table) '())
	     table))))

(defun rehash-exptree-things (table things tree piecefn val)
  (dolist (thing things)
     (exp-table-index table thing (funcall piecefn thing) t tree))
  (do ((thresh (* imbalance-thresh* (list-length things)))
       (entries (exptree_table-entries TABLE) (cdr entries))
       (entry))
      ((null entries)
       (progn
	  (cond (exptree-dbg*
		 (format *error-output* "Rehashing succeeded ~%")))
	  (setf (exptree-objects tree) '())
	  (setf (exptree_table-status table) '*rehash-done)
	  (setf (exptree-rehash-prospects tree) '*unknown)
	  table))
    (setf entry (car entries))
    (cond ((and (eql val (exptree_entry-val entry))
		(>= (exptree-size (exptree_entry-tree entry))
		    thresh))
	   (cond (exptree-dbg*
		  (format *error-output*
		      "Rehashing failed to discriminate -- undoing it~%")))
	     (setf (exptree_table-status  table) (exptree_entry-val entry))
	     (setf (exptree_table-entries table) '())
	     (return nil)))))

(defun exp-rehash-key (pat dt topcoord variant)
  (multiple-value-bind (c b)
		       (first-rehash-key pat topcoord dt variant)
     (values c b #'exp-rehash-keys)))

(defun listrehashkey (pat c dt skip-car variant)
  (let ((pat  (if skip-car (cdr pat) pat))
	(c    (if skip-car (expcoord-downright c) c)))
     (cond ((or (atom pat) (funcall (expcoord-is-var c) pat))
            (values nil nil))
	   (T
	    (do ((x)
		 (pl pat (cdr pl))
		 (coord) (val))
		((null pl)
		 (values nil nil))
	      (setf x (car pl))
	      (multiple-value-setq (coord val)
				   (first-rehash-key x (expcoord-downleft c)
						     dt variant))
	      (cond (coord
		     (return (values coord val))))
	      (setf c (expcoord-downright c)))))))

(defun first-rehash-key (pat c dt variant)
   (cond ((funcall (expcoord-is-var c) pat) 
          (cond (variant
                 (cond ((key-nixed c '*dontcare dt)
                        (values nil nil))
                       (t
                        (values c '*dontcare))))
                (t (values nil nil))))
         ((atom pat)
          (setf pat (symbolify pat))
          (cond ((key-nixed c pat dt)
                 (values nil nil))
                (t (values c pat))))
         ((key-nixed c '*struct dt)
          (listrehashkey pat c dt nil variant))
         (t
          (values c '*struct))))

(defun exp-rehash-keys (pat et topcoord variant)
  (keygen pat topcoord et variant))

;; Generates a list of keys for a pat appearing at expcoord.
(defun keygen (pat c et variant)
  (cond ((atom pat)
         (cond ((key-nixed c pat et) '())
               (t (list (make-exp_key c (symbolify pat))))))
	((funcall (expcoord-is-var c) pat)  
	 (cond ((and variant (not (key-nixed c '*dontcare et)))
		(list (make-exp_key c '*dontcare)))
	       (t '())))
	((key-nixed c '*struct et)
         (listkeygen pat c et variant))
        (t
	 (cons (make-exp_key c '*struct)
	       (listkeygen pat c et variant)))))

(defun listkeygen (pat c et variant)
  (if (null pat)
      '()
      (nconc (keygen (car pat) (expcoord-downleft  c) et variant)
	     (listkeygen (cdr pat) (expcoord-downright c) et variant))))


(defun discrim (pat1 pat2 topcoord)
   (let ((is-VAR (expcoord-is-var TOPCOORD)))
      (LABELS ((find-diff (pat1 pat2 c)
		 (cond ((or (funcall is-var pat1)
			    (funcall is-var pat2))
			nil)
		       ((or (atom pat1) (atom pat2))
			(cond ((eq pat1 pat2) nil)
			      (t c)))
		       (t
			(do ((pl1 pat2 (cdr pl1))
			     (pl2 pat2 (cdr pl2))
			     (r))
			    ((or (null pl1) (null pl2))
			     nil)
			   (setf r (find-diff (car pl1) (car pl2)
					      (expcoord-downleft C)))
			   (cond (r
				  (return r)))
			   (setf c (expcoord-downright c)))))))
	 (find-diff pat1 pat2 topcoord))))

(defun key-nixed (k val dt)
   (do ((c '*here))
       ((let ((tab (expcoord-used-before k dt c)))
	  (and tab
	       (or (eql (exptree_table-status tab) val)
		   (eq (exptree_table-status tab) '*rehash-done))))
	t)
      (setf c (cond ((eq c '*here) (exptree-cell dt))
		    (t (cdr C))))
      (setf dt (exptree-superior DT))
      (cond ((not dt)
	     (return nil)))))

; Test whether K will never be of any use because it's already been used
; to discriminate.  Only count successful rehashes, because unsuccessfuls
; can be retried after future additions.
(defun key-useless-here (k dt)
   (do ((C '*HERE))
       ((let ((tab (expcoord-used-before k dt c)))
	  (and tab (eq (exptree_table-status tab) '*rehash-done)))
	t)
      (setf c (cond ((eq c '*here) (exptree-cell dt))
                  (t (cdr C))))
      (setf dt (exptree-superior dt))
      (cond ((not dt)
	     (return nil)))))

; Get table for expcoord POS at ET, *if* it occurs before subtree
; corresponding to cell C.  
(defun expcoord-used-before (pos et c)
   (let ((check (and c
                     (not (eq c '*here))
                     (exp_key-coord (car c)))))
      (dolist (tab (exptree-tables ET)
		   nil)
         (cond ((eq (exptree_table-coord tab) check)
		(return (cond ((eq pos check)
			       tab)
			      (t nil))))
	       ((eq (exptree_table-coord TAB) POS)
		(return tab))))))

;; Make A into an object that can be tested using EQ or EQL
(defun symbolify (a)
  (cond ((null a) a)
        ((or (symbolp a) (numberp a) (is-wheresym a) (characterp a)) a)
        ((stringp   a) (intern A))
	(t a)))

(defun expindex-see (ei depth)
   (exptree-see (exp_index-tree ei) depth 0 nil
		(rehash_spec-piecefn (exp_index-rehash ei))))

(defun exptree-see (tree depth indent mid piecefn)
     (cond ((not mid)
	    (format t "~&")
	    (print-spaces t indent)
	    (format t "Cell ~s" (exptree-cell tree))))
     (cond ((not (eq (exptree-rehash-prospects tree) '*unknown))
	    (format t " [Rehash prospects: ~s]"
		    (exptree-rehash-prospects TREE))))
     (if (not (exptree-first-operative-table tree))
	 (let ((i (+ indent 3)) (l (exptree-contents tree nil)))
	    (dolist (A L)
	       (format t "~&")
	       (print-spaces t i)
	       (let ((*print-pretty* t))
		  (format t "~s"
			  (funcall piecefn a))))))
     (let ((k (list-length (exptree-tables tree))))
        (format t "~%")
	(print-spaces t indent)
	(format t " -- ~s tables~%" K)
	(dolist (table (exptree-tables tree))
	  (exptree-table-see table depth (+ indent 3) piecefn))))

(defun exptree-table-see (table depth indent piecefn)
  (let ((coord (exptree_table-coord table)))
    (case (exptree_table-status table)
      (*rehash-in-progress
       (format t "~&")
       (print-spaces t indent)
       (format t "~s : Rehash in progress ~%"
	       coord))
      (*rehash-done
       (dolist (entry (exptree_table-entries table))
	  (format t "~&")
	  (print-spaces t indent)
	  (cond ((eq (exptree_entry-val entry) '*na)
		 (format t "~s discrimination not applicable"
			 coord))
		(t
		 (format t "~s: ~s" COORD (exptree_entry-val ENTRY))))
	  (cond ((> depth 0)
		 (exptree-see (exptree_entry-tree entry)
			      (- depth 1)
			      (+ indent 3)
			      t piecefn))
		(t (print-spaces t 3 )
		   (format t "...")))
	  (format t "~%")
	  (print-spaces t indent)
	  (format t "------------")))
      (T
       (format t "~&")
       (print-spaces t indent)
       (format t "~s: Rehash failed due to frequency of: ~s~%"
	       coord (exptree_table-status table))))))


; For debugging
(defun strip-nixes (et)
   (setf (exptree-rehash-prospects et) '*unknown)
   (setf (exptree-tables et) 
       (remove-if-not
	   #'(lambda (tab) (eq (exptree_table-status tab) '*rehash-done))
           (exptree-tables et)))
   (dolist (tab (exptree-tables et))
      (dolist (e (exptree_table-entries tab))
         (strip-nixes (exptree_entry-tree e)))))

(defun print-spaces (srm num)
  (dotimes (i num)
     #-:cmu (declare (ignore i))
     (format srm " ")))

