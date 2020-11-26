(IN-PACKAGE "PDDL")

; This unifier renames variables in constant time by defining a
; variable as a pair <symbol, varid>, where varid is an arbitrary integer.
; To rename all the variables in an expression, just generate a new varid.
; For technical reasons, it's important that no variable with varid I is
; bound to a variable with varid J>I.  Varids grow monotonically, so
; this means that no "old" variable is bound to a "new" variable.  

(defvar vid* 0)

(defun new-varid () (setq vid* (+ vid* 1)) vid*)

(defstruct (uvar (:type list)
		 (:constructor make-uvar (name)))
   (flag '\?)
   (name nil :type symbol))

(defun is-uvar (x) (and (consp x) (eq (car x) '\?)))

(defstruct (expclo (:type vector)
		   (:constructor make-expclo (skel id)))
   skel
   (id nil)) ;  :type integer or nil

;Key idea: New data type, bdgenv, which is a list of entries of the form:
;   (varname varid val)
;which specifies the value val of variable named varname with id varid

(defstruct (varbdg (:type list)
		   (:constructor make-varbdg (varname id val)))
   (varname nil :type symbol)
   (id nil :type integer)
   val)

(declaim (inline vars=))

(defun vars= (v1 v2 i1 i2)
   (and (eq v1 v2) (eql i1 i2)))

(defun empty-env () '())

(defun uvar-lookup (name id env)
   (do     ((bl env (cdr bl)))
	   ((or (null bl)
		(vars= (varbdg-varname (car bl))
		       name
		       (varbdg-id (car bl))
		       id))
	    (cond ((null bl) nil)
		  (t (car bl))))))

; Find a binding of a variable with eid in env whose value
; is variable with given name and id.  (id is never nil)
(defun uvar-lookup-backwards (name id eid env)
   (do ((bl env (cdr bl)))
       ((or (null bl)
	    (and (= (varbdg-id (car bl))
		    eid)
		 (let ((v (varbdg-val (car bl))))
		    (let ((vsk (expclo-skel v))
			  (vid (expclo-id v)))
		       (and vid
			    (= vid id)
			    (is-uvar vsk)
			    (eq (uvar-name vsk)
				name))))))
	(cond ((null bl) nil)
	      (t (car bl))))))

(defvar remove-duplicate-bdgs* t)

(defun new-bdg (b prev e)
  (cons b 
	(cond ((and prev remove-duplicate-bdgs*)
	       (remove prev e :count 1 :test #'eq))
	      (t e))))

(declaim (inline no-new-dbsg first-new-bdg))

(defun no-new-bdgs (new old)
   (eq new old))

(defun first-new-bdg (new old)
   (declare (ignore old))
   (car new))

(defun new-bdgs (new old)
   (ldiff new old))

(defun first-real-bdg (new old)
   (loop
      (cond ((or (null new) (eq new old))
	     (error "No real bdgs: ~s~%  [before ~s]" new old))
	    ((varbdg-val (car new))
	     (return (car new))))
      (setq new (cdr new))))

(defvar unify-count* 0)
(defvar unify-success-count* 0)

; t1 and t2 are terms, e1 and e2 are varids, e is bdgenv
(defun unify (t1 t2 e1 e2 e)
   (setq unify-count* (+ unify-count* 1))
   (multiple-value-bind (ok e)
			(pat-unify t1 t2 e1 e2 e)
      (cond (ok (setf unify-success-count* (+ unify-success-count* 1))))
      (values ok e)))

; Not really a bdgenv; used as flag to tell caller how match failed.
(defvar DISCRIM-BDGS* (list (make-varbdg 'discrim 0
					 (make-expclo 'discrim 'nil))))

(defun pat-unify (t1 t2 e1 e2 e)
   (cond ((is-uvar t1)
          (var-unify (uvar-name t1) t2 e1 e2 e))
         ((is-uvar t2)
          (var-unify (uvar-name t2) t1 e2 e1 e))
         ((or (atom t1) (atom t2))
          (cond ((eq t1 t2)
                 (values t e))
                (t
                 (values nil discrim-bdgs*))))
         ((eq (car t1) (car t2))
          (do ((l1 (cdr t1) (cdr l1))
               (l2 (cdr t2) (cdr l2))
	       (okaysofar t))
              ((or (null l1) (null l2)
                   (not okaysofar))
	       (values (and okaysofar (null l1) (null l2))
                       e))
	    (multiple-value-setq (okaysofar e)
				 (pat-unify (car l1) (car l2) e1 e2 e))))
	 (t (values nil discrim-bdgs*))))

(defun var-unify (var val varid valid e)
   (let ((bdg (uvar-lookup var varid e)))
      (cond ((and bdg (varbdg-val bdg))
             (let ((varval (varbdg-val bdg)))
                (multiple-value-bind (ok sub-e)
				     (pat-unify (expclo-skel varval) val
		                                (expclo-id varval)   valid
						e)
                   (cond (ok (values ok sub-e))
                         (t (values nil e))))))
            (t
	     (try-bind-uvar var val varid valid e)))))

(defun try-bind-uvar (var val varid valid e)
   (let ((destid (and valid (min varid valid))))
      (labels (;(bind-it (var varid val valid env)
		;  (new-bdg (make-varbdg var varid
		;			(make-expclo val valid))
		;	   nil env))
	       (resolve-vars (subval subid e)
                  (cond ((or (not subid) (atom subval))
			 (values '*const subval e))
			((< subid varid)
			 (values '*var subval e))
			((is-uvar subval)
			 (let ((b (uvar-lookup (uvar-name subval) subid e)))
			   (cond ((and b (varbdg-val b))
				  (let ((v (varbdg-val b)))
				    (let ((skel (expclo-skel v))
					  (id (expclo-id v)))
				      (multiple-value-bind
						(c v e)
						(resolve-vars skel id e)
					 (cond ((eq c '*var)
						(cond ((= id destid)
						       (values '*var v e))
						      ((= subid destid)
						       (values '*var subval e))
						      (t
					               ; subid > destid
						       (flag-new-dummy-var
                                                        destid v
                                                        (min id destid)
                                                        e))))
					       (t (values c v e)))))))
				 ((= subid destid)
				  (values (cond ((eq (uvar-name subval) var)
						 '*self)
						(t '*var))
					  subval e))
				 (t
				  ; SUBID > DESTID
				  (let ((newsym (new-var-sym)))
				    (let ((newvar (make-uvar newsym)))
				       (let ((newenv
					        (create-new-bdg 
					           (uvar-name subval)
						   newvar
						   subid destid
						   b e)))
					 (values '*var
						 newvar
						 newenv))))))))
			(t
			 (do ((xl subval (cdr xl))
			      (x) 
                              (res '())
                              (v) (c)
                              (outcome '*const))
			     ((null xl)
			      (values outcome (nreverse res) e))
			   (setq x (car xl))
			   (multiple-value-setq (c v e)
						(resolve-vars x subid e))
			   (cond ((member c '(*self *circular) :test #'eq)
				  (return (values '*circular nil e))))
			   (setf res (cons v res))
			   (cond ((eq c '*var)
				  (setf outcome '*var))))))))
         (multiple-value-bind (outcome newval newenv)
			      (resolve-vars val valid e)
            (case outcome
               (*self (values t e))
               (*circular (values nil e))
               (t
		(values t
			(create-new-bdg
			       var newval
                               varid (cond ((eq outcome '*var) destid)
                                           (t nil))
                               nil
			       newenv))))))))

(defun flag-new-dummy-var (destid valskel valid e)
   (multiple-value-bind (newvar newenv)
			(new-dummy-var destid valskel valid e)
      (values '*var newvar newenv)))

(defun new-dummy-var (destid valskel valid e)
   (let ((newvar (new-var-sym)))
      (values (make-uvar newvar)
              (new-bdg (make-varbdg newvar destid
                                    (make-expclo valskel valid))
                       nil
                       e))))

; Get the car & cdr  of an expression.  In some cases, this may require
; augmenting the environment, so return the new env.
(defun expression-car-cdr (exp id env)
   (labels ((pursue (x subid)
	       (cond ((is-uvar x)
		      (let ((p (uvar-lookup (uvar-name x) subid env)))
			 (cond (p
				(let ((val (varbdg-val p)))
				   (pursue (expclo-skel val)
					   (expclo-id val))))
			       (t
				(cerror "Expression has no car and cdr ~s"
					x)))))
		     ((consp x)
		      (cond ((or (not subid) (= subid id))
			     (values (car x) (cdr x) env))
			    (t
			     (multiple-value-bind
			                (newx newenv)
					(varid-convert x subid id env)
			        (values (car newx) (cdr newx) newenv)))))
		     (t
		      (error "Expression has no car and cdr ~s" x)))))
       (pursue exp id)))

;; Substitutes for mvars in pat, getting values from alist. 
; *Not guaranteed to substitute to any particular depth,* if
; going too far would cause it to insert variables with the wrong
; varid.
(defun varsubst (pat id alist)
   (cond ((null alist) pat)
	 (t (varsubst1 pat id alist))))

(defun varsubst1 (pat id alist)
   (cond ((atom pat) pat)
	 ((is-uvar pat)
	  (let ((p (uvar-lookup (uvar-name pat) id alist)))
	     (cond (p
		    (let ((val (varbdg-val p)))
		       (cond ((expclo-id val)
                              (multiple-value-bind (x hasvars)
						   (pat-flatten
						      (expclo-skel val)
						      (expclo-id val)
						      alist)
                                 (cond (hasvars pat)
                                       (t x))))
			     (t (expclo-skel val)))))
		   (t pat))))
	 (t
	  (let ((p1 (varsubst1 (car pat) id alist))
		(p2 (varsubst1 (cdr pat) id alist)))
	     (cons-if-new p1 p2 pat)))))

; This substitutes as far as possible, and as a consequence may have to
; return a revised env
(defun safe-varsubst (pat id alist)
   (subst-and-convert pat id id alist))

(defun subst-and-convert (pat subid destid alist)
   (cond ((atom pat)
	  (values pat alist))
	 ((is-uvar pat)
	  (let ((p (uvar-lookup (uvar-name pat) subid alist)))
	     (cond (p
		    (let ((val (varbdg-val p)))
		       (subst-and-convert (expclo-skel val)
					  (expclo-id val)
					  destid alist)))
		   (t
		    (var-convert pat subid destid alist)))))
	 (t
	  (multiple-value-bind (p1 a1)
			       (subst-and-convert
				  (car pat) subid destid alist)
	     (multiple-value-bind (p2 a2)
				  (subst-and-convert
				      (cdr pat) subid destid a1)
		(values (cons-if-new p1 p2 pat)
			a2))))))

; no longer used:
(defun pat-flatten (x id alist)
   (cond ((atom x) (values x nil))
         ((is-uvar x)
          (let ((p (uvar-lookup (uvar-name x) id alist)))
             (cond ((and p (varbdg-val p))
                    (let ((val (varbdg-val p)))
                       (cond ((expclo-id val)
                              (multiple-value-bind (y hasvars)
						   (pat-flatten
						      (expclo-skel val)
						      (expclo-id val)
						      alist)
                                 (cond (hasvars (values nil t))
                                       (t (values y nil)))))
                             (t (values (expclo-skel val) nil)))))
                   (t (values nil t)))))
         (t
          (multiple-value-bind (p1 v1)
			       (pat-flatten (car x) id alist)
             (cond (v1 (values nil t))
                   (t
                    (multiple-value-bind (p2 v2)
                                         (pat-flatten (cdr x) id alist)
                       (cond (v2 (values nil t))
                             (t (values (cons-if-new p1 p2 x) nil))))))))))

; This is for debugging only, because it builds patterns with inconsistent
; varids.
; Actually, it is safe in the case where the value returned contains no
; vars.
(defun unsafe-varsubst (pat id alist)
   (cond ((atom pat) pat)
	 ((is-uvar pat)
	  (let ((p (uvar-lookup (uvar-name pat) id alist)))
	     (cond (p
		    (let ((val (varbdg-val p)))
		       (cond ((expclo-id val)
			      (unsafe-varsubst (expclo-skel val)
					       (expclo-id val)
					       alist))
			     (t (expclo-skel val)))))
		   (t pat))))
	 (t
	  (let ((p1 (unsafe-varsubst (car pat) id alist))
		(p2 (unsafe-varsubst (cdr pat) id alist)))
	     (cons-if-new p1 p2 pat)))))

; We desire to combine x with another expression whose free vars
; have id new-id.  To do so, we have to convert x from an expression
; whose free vars have old-id.  
; Returns new expression plus revised env
(defun varid-convert (x source-id dest-id env)
   (cond ((not source-id)
	  (values x env))
	 (t
	  (vars-id-convert x source-id dest-id env))))

(defun vars-id-convert (subval subid destid e)
   (cond ((atom subval)
	  (values subval e))
	 ((= subid destid)
	  (values subval e))
	 ((is-uvar subval)
	  (let ((b (uvar-lookup (uvar-name subval) subid e)))
	     (cond (b
		    (let ((v (varbdg-val b)))
		       (let ((skel (expclo-skel v))
			     (id (expclo-id v)))
			  (vars-id-convert skel id destid e))))
		   (t
		    (var-convert subval subid destid e)))))
	 (t
	  (do ((xl subval (cdr xl))
	       (x) 
	       (res '()))
	      ((null xl)
	       (values (nreverse res) e))
	      (setq x (car xl))
	      (multiple-value-bind
	                     (v newenv)
			     (vars-id-convert x subid destid e)
	         (setf res (cons v res))
		 (setq e newenv))))))

(defun var-convert (subval subid destid e)
  (cond ((= subid destid)
	 (values subval e))
	((> subid destid)
	 (let ((newsym (new-var-sym)))
	    (let ((newvar (make-uvar newsym)))
	       (let ((newenv
			(create-new-bdg 
			   (uvar-name subval)
			   newvar
			   subid destid
			   nil e)))
		   (values newvar newenv)))))
	(t
	 (let ((bb (uvar-lookup-backwards
		      (uvar-name subval)
		      subid destid e)))
	    (cond (bb
		   (values (make-uvar
			      (varbdg-varname
				 bb))
			   e))
		  (t
		   (new-dummy-var
		      destid subval subid e)))))))

; Return a version of X that has no bound variables or variable dependencies
(defun env-elim (x id e)
   (labels ((x-walk (x sub-id e)
	      (cond ((atom x) (values x e))
		    ((is-uvar X)
		     (let ((b (uvar-lookup (uvar-name x) sub-id e)))
			(cond ((and b (varbdg-val b))
			       (let ((val (varbdg-val b)))
				  (cond ((expclo-id val)
					 (x-walk (expclo-skel val)
						 (expclo-id val)
						 e))
					(t (values (expclo-skel val) e)))))
			      ((= sub-id id)
			       (values x e))
			      (t
			       (let ((newvar (make-uvar (new-var-sym))))
				  (let ((e (cons (make-varbdg
						       (uvar-name x) sub-id
						       (make-expclo
							     newvar id))
						 e)))
				    (values newvar e)))))))
		    (t
		     (multiple-value-bind (a ea)
					  (x-walk (car x) sub-id e)
			(multiple-value-bind (d ed)
					     (x-walk (cdr x) sub-id ea)
			   (values (cons-if-new a d x)
				   ed)))))))
	 (X-WALK x ID E)))

(defun create-new-bdg (var val varid valid bdg e)
      (new-bdg (make-varbdg var varid
			    (make-expclo val valid))
	       bdg e))

; Discard all bindings with the given id or later (greater)
(defun bdgenv-contract (e discard)
   (cond ((null e) '())
         (t
          (let ((r (bdgenv-contract (cdr e) discard)))
             (cond ((>= (varbdg-id (car e)) discard)
                    r)
                   (t
                    (cons-if-new (car e) r e)))))))

; PAT may contain variables with DISCARD or higher.  Eliminate them,
; replacing them with new variables with id REPLACE instead.
(defun subst-away (pat e id discard replace)
   (cond ((atom pat) (values pat e))
         ((is-uvar pat)
          (let ((p (uvar-lookup (uvar-name pat) id e)))
             (cond ((and p (varbdg-val p))
                    (let ((val (varbdg-val p)))
                       (cond ((expclo-id val)
                              (subst-away (expclo-skel val) e (expclo-id val)
                                          discard replace))
                             (t (values (expclo-skel val) e)))))
                   ((= id replace)
                    (values pat e))
                   ((< id replace)
                    (multiple-value-bind (x e)
					 (new-dummy-var
                                            replace
                                            pat
                                            id
                                            e)
                       (values x e)))
                   (t
                    (let ((newvar (new-var-sym)))
                       (let ((e (cons (make-varbdg
					    (uvar-name pat) id
					    (make-expclo
						  (make-uvar newvar)
						  replace))
                                      (cons (make-varbdg
                                                  newvar replace nil)
                                            e))))
                          (values (make-uvar newvar)
                                  e)))))))
         (t
          (multiple-value-bind (a e1)
			       (subst-away (car pat) e id discard replace)
             (multiple-value-bind (d e2)
				  (subst-away (cdr pat) e1 id discard replace)
                (values (cons-if-new a d pat)
                        e2))))))

(defun conj-flatten (c)
   (cond ((and (consp c) (eq (car c) 'AND))
	  (mapcan #'conj-flatten (cdr c)))
	 (t (list c))))

(defun has-qvars (x)
   (cond ((qvar-p x) t)
	 ((atom x) nil)
	 (t (some #'has-qvars x))))

(defun has-uvars (x id e)
   (cond ((qvar-p x)
	  (let ((b (uvar-lookup (uvar-name x) id e)))
	     (cond (b
		    (let ((ec (varbdg-val b)))
		       (cond ((expclo-id ec)
			      (has-uvars (expclo-skel ec)
					 (expclo-id ec)
					 e))
			     (t nil))))
		   (t t))))
	 ((atom x) nil)
	 (t
	  (some #'(lambda (y) (has-uvars y id e))
		x))))

(defun all-uvars (x id e)
   (cond ((is-uvar x)
          (let ((b (uvar-lookup (uvar-name x) id e)))
             (cond ((and b (varbdg-val b))
		    (let ((ec (varbdg-val b)))
                       (all-uvars (expclo-skel ec)
				  (expclo-id ec)
				  e)))
                   (t
                    (list (make-expclo x id))))))
         ((atom x) '())
         (t
	  (mapcan #'(lambda (y) (all-uvars y id e))
		  x))))

; Useful for processing input formulas, with no binding history yet
(defun raw-uvars (x)
   (cond ((is-uvar x) (list (uvar-name x)))
	 ((atom x) '())
	 (t
	  (reduce #'(lambda (coll r)
		       (union coll (raw-uvars r) :test #'eq))
	          x
		  :initial-value '()))))

(defun same-expclo-var (ev1 ev2)
   (let ((v1 (expclo-skel ev1))
	 (v2 (expclo-skel ev2))
	 (id1 (expclo-id ev1))
	 (id2 (expclo-id ev2)))
      (and (eq (uvar-name v1) (uvar-name v2))
	   (cond (id1 (and id2 (= id1 id2)))
		 (t (null id2))))))

; Id to use when you're sure the pattern has no variables
(defvar ground-id* 1)
(defvar dummy-id* 1)

(defvar varno* 0)

(defun new-var-sym ()
   (setq varno* (+ varno* 1))
   (intern
	      (concatenate 'string
		 "_" 
		 (let ((*print-radix* nil))
		   (princ-to-string varno*)))))

(defun cons-if-new (x y l)
   (cond ((and (eq x (car l)) (eq y (cdr l)))
          l)
         (t (cons x y))))

