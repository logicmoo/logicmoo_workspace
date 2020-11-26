(IN-PACKAGE "PDDL")

; Index for sets of eq-tested objects that typically have large overlaps

(defstruct (eq_dtree
	      (:constructor
	          new-eq_dtree
		  (size contents)))
   (size 0 :type integer)
   (last-discrim-attempt 0) ; integer = size on last discrim attempt; 
                            ; or nil -> successfully discriminated
   (contents '())
   (discrim nil)
   (haves nil)     ; subtrees for sets containing discrim
   (have-nots nil) ; subtrees for sets not containing discrim
)

(defun make-eq_dtree (contents)
   (new-eq_dtree (length contents) contents))

(defun eq_dtree-discriminated (tr)
   (not (eq_dtree-last-discrim-attempt tr)))

(defstruct (eq_setindex
	      (:constructor
	          new-eq_setindex (tree piecefn)))
   (tree nil :type eq_dtree)
   piecefn)

(defun make-eq_setindex (piecefn)
   (new-eq_setindex (make-eq_dtree '())
		    piecefn))

(defvar eqt-rehash-thresh* 5)

(defun eq-index-fetch (x ind)
      (elts-eq-index-fetch (funcall (eq_setindex-piecefn ind) x)
                           ind))

(defun elts-eq-index-fetch (elts ind)
   (let ((pfn (eq_setindex-piecefn ind))
         (already '()))
      (labels ((walk-down (tr)
                  (cond ((member tr already)
                         (error "Hit twice"))
                        (t
                         (push tr already)))                                
		  (cond ((and (not (eq_dtree-discriminated tr))
			      (>= (- (eq_dtree-size tr)
				     (eq_dtree-last-discrim-attempt tr))
				  eqt-rehash-thresh*))
			 (setindex-try-rehash tr pfn)))
		  (cond ((eq_dtree-discriminated tr)
			 (cond ((member (eq_dtree-discrim tr) elts
					:test #'eq)
				(walk-down (eq_dtree-haves tr)))
			       (t
				(walk-down (eq_dtree-have-nots tr)))))
			(t (eq_dtree-contents tr)))))
	  (walk-down (eq_setindex-tree ind)))))

(defun eq-index-add (x ind)
   (let ((elts (funcall (eq_setindex-piecefn ind) x)))
      (labels ((walk-down (tr)
                  (cond ((eq_dtree-discriminated tr)
                         (cond ((member (eq_dtree-discrim tr)
					elts
					:test #'eq)
                                (walk-down (eq_dtree-haves tr)))
                               (t
                                (walk-down (eq_dtree-have-nots tr)))))
                        (t
                         (push x (eq_dtree-contents tr))
                         (setf (eq_dtree-size tr)
			       (+ (eq_dtree-size tr) 1))))))
         (walk-down (eq_setindex-tree ind)))))

(defun setindex-try-rehash (tr pfn)
   (multiple-value-bind
	      (ok dis)
	      (set-disting (eq_dtree-contents tr) pfn)
      (cond (ok
             (setf (eq_dtree-discrim tr) dis)
	     (setf (eq_dtree-haves tr)
		   (make-eq_dtree
		         (remove-if-not
			      #'(lambda (x)
			         (member dis (funcall pfn x)
					 :test #'eq))
			      (eq_dtree-contents tr))))
	     (setf (eq_dtree-have-nots tr)
		   (make-eq_dtree
			 (remove-if
			      #'(lambda (x)
			          (member dis (funcall pfn x)
					      :test #'eq))
			      (eq_dtree-contents tr))))
             (setf (eq_dtree-last-discrim-attempt tr) nil)
             (let ((h (eq_dtree-size (eq_dtree-haves tr)))
                   (n (eq_dtree-size (eq_dtree-have-nots tr)))
                   (s (eq_dtree-size tr)))
                ;(format t "Rehashed, haves=~s, have-nots=~s, tot=~s~%"
                ;          h n s)
                (cond ((not (= (+ h n) s))
                       (error "Rehash discrepancy ~s+~s=/=~s"))
                      ((or (= h 0) (= n 0))
                       (error "Rehash futility")))))
            (T
             (setf (eq_dtree-last-discrim-attempt tr) (eq_dtree-size tr))))
      (cond ((not (eq (not ok)
                      (not (eq_dtree-discriminated tr))))
             (error "Rehash discrepancy ok = ~s discriminated = ~s ~%"
                    ok (eq_dtree-discriminated tr))))
      ok))

; Find an object that divides the elements of SL into an approximate 
; dichotomy -- about half the sets in SL contain the object and about
; half don't.
; Typically, element of SL has <100 elements.  SL has <10 elements
(defun set-disting (sl pfn)
   (do ((sltl sl (cdr sltl))
	(found nil) (d))
       ((or found (null sltl))
	(cond (found (values t d))
	      (t (values nil nil))))
     (do ((xl (funcall pfn (car sltl)) (cdr xl))
	  (x) (haves) (have-nots))
	 ((or found (null xl)))
       (setf x (car xl))
       (setf haves 0)
       (setf have-nots 0)
       (do ((sltl1 sl (cdr sltl1))
	    ;(i 1 (+ i 1))
	   )
	   ((null sltl1))
	 (cond ((member x (funcall pfn (car sltl1))
			:test #'eq)
		(setf haves (+ haves 1)))
	       (T 
		(setf have-nots (+ have-nots 1)))))
       (cond ((and (> haves 0)
		   (> have-nots 0)
		   (let ((r (/ (float haves)
			       (float have-nots))))
		     (and (>= r 0.5)
			  (<= r 2.0))))
	      (setf d x)
	      (setf found t))))))

(defun random-ints (m n)
   (do  ((I 1 (+ i 1))
	 (S '()))
	((> i n)
	 s)
      (setf s (adjoin (RANDOM M) s))))
