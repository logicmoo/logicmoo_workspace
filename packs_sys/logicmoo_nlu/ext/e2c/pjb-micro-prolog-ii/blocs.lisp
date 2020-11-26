;;; mini-PrologII
;;; blocks.lsp
;;;

;;; I. Registers
;;;
(defconstant +bottom-of-frozen-goals-stack+ 1)
(defconstant +bottom-of-global-stack+ (+ +bottom-of-frozen-goals-stack+  3000))
(defconstant +bottom-of-local-stack+  (+ +bottom-of-global-stack+        3000))
(defconstant +bottom-of-trail+        (+ +bottom-of-local-stack+        14000))
(defconstant +max-of-trail+           (+ +bottom-of-trail+               2000))
(defconstant +memory-size+            (+ +max-of-trail+                    50))

(defparameter *memory* (make-array +memory-size+ :initial-element 0))
(defparameter *top-of-frozen-goals-stack*    nil)
(defparameter *top-of-trail*                 nil)
(defparameter *top-of-local-stack*           nil)
(defparameter *top-of-global-stack*          nil)
(defparameter *current-continuation*         nil)
(defparameter *cl*                           nil) 
(defparameter *specific-cut-point*           nil)
(defparameter *awakened-goals*               nil)
(defparameter *backtracking-local-register*  nil)
(defparameter *backtracking-global-register* nil)
(defparameter *current*                      nil)
(defparameter *current-environment*          nil)
(defparameter *current-goal*                 nil)
(defparameter *not-done*                     nil)

(defmacro vset (v i x) `(setf (svref ,v ,i) ,x))

;;; II. Local Stack
;;;
;;;deterministic block [*cl* *current-continuation* *top-of-global-stack* Cut E]
;;;
(defmacro cl                   (b) `(svref *memory* ,b))
(defmacro current-continuation (b) `(svref *memory* (1+ ,b)))
(defmacro top-of-global-stack  (b) `(svref *memory* (+ ,b 2)))
(defmacro cut                  (b) `(svref *memory* (+ ,b 3)))
(defmacro e                    (b) `(+ ,b 4))            

(defmacro push_cont ()
  "saves continuation"
  `(progn
     (vset *memory* *top-of-local-stack* *cl*)
     (vset *memory* (1+ *top-of-local-stack*) *current-continuation*)))
  
(defmacro push_E (n)
  "allocates local env"
  `(let ((top (+ *top-of-local-stack* 4 ,n)))	
     (when (>= top +bottom-of-trail+)
       (throw 'debord (progn(terpri) (princ "Local Stack Overflow") (terpri))))
     (vset *memory* (+ *top-of-local-stack* 3) *specific-cut-point*)
     (dotimes (i ,n top)                ; n local free variables
       (vset *memory* (decf top) (cons 'LIBRE +bottom-of-global-stack+)))))
 
(defmacro maj_L (nl)
  "updates top of local stack"
  `(incf *top-of-local-stack* (+ 4 ,nl)))


;;;choice-point : [a1 .. an +max-of-trail+ *top-of-frozen-goals-stack* BCP BCL *backtracking-global-register* *backtracking-local-register* BP *top-of-trail*]
;;;
(defmacro top-of-trail                 (b) `(svref *memory* (1- ,b)))
(defmacro BP                           (b) `(svref *memory* (- ,b 2)))
(defmacro backtracking-local-register  (b) `(svref *memory* (- ,b 3)))
(defmacro backtracking-global-register (b) `(svref *memory* (- ,b 4)))
(defmacro BCL                          (b) `(svref *memory* (- ,b 5)))
(defmacro BCP                          (b) `(svref *memory* (- ,b 6)))
(defmacro top-of-frozen-goals-stack    (b) `(svref *memory* (- ,b 7)))
(defmacro max-of-trail                 (b) `(svref *memory* (- ,b 8)))

(defun save_args ()
  "save args of current goal"
  (dotimes (i (svref *memory* +max-of-trail+)
              (vset *memory* (incf *top-of-local-stack* i) i))
    (vset *memory* (+ *top-of-local-stack* i) (svref *memory* (+ +max-of-trail+ i 1)))))

(defun push_choix ()
  "allocates a choice point"
  (save_args)                           ; goal args
  (vset *memory* (incf *top-of-local-stack*) *top-of-frozen-goals-stack*) ; top of f.g. stack
  (vset *memory* (incf *top-of-local-stack*) *current-continuation*) ; continuation
  (vset *memory* (incf *top-of-local-stack*) *cl*)
  (vset *memory* (incf *top-of-local-stack*) *top-of-global-stack*) ; top of global stack
  (vset *memory* (incf *top-of-local-stack*) *backtracking-local-register*) ; last choice point
  (vset *memory* (incf *top-of-local-stack* 2) *top-of-trail*) ; top of trail
  (setq *backtracking-local-register*  (incf *top-of-local-stack*)
        *backtracking-global-register* *top-of-global-stack*)) ; new last choice point
        
(defun push_bpr (reste)
  (vset *memory* (- *backtracking-local-register* 2) reste))
    
(defmacro size_C (b) `(+ 8 (max-of-trail ,b)))

(defun pop_choix ()		       
  (setq *top-of-local-stack* (- *backtracking-local-register*
                                (size_C *backtracking-local-register*)) ; pops the block
        *backtracking-local-register* (backtracking-local-register *backtracking-local-register*) ; updates backtrack reg.
        *backtracking-global-register* (if (zerop *backtracking-local-register*)
                                           +bottom-of-global-stack+
                                           (backtracking-global-register *backtracking-local-register*))))

;;; III. Global Stack
;;;
(defmacro push_G (n)
  `(let ((top (+ *top-of-global-stack* ,n)))
     (when (>= top +bottom-of-local-stack+) 
         (throw 'debord (progn (terpri) (princ "Global Stack Overflow") (terpri))w))
     (dotimes (i ,n
                 (vset *memory* (+ *top-of-local-stack* 2) *top-of-global-stack*)) ; n free global vars
       (vset *memory* (decf top) (cons 'LIBRE +bottom-of-global-stack+)))))


(defmacro maj_G (n) `(incf *top-of-global-stack* ,n))

;;;IV. Trail
;;;
(defmacro fgblock (x) `(cdr (svref *memory* ,x)))

(defun pushtrail (x)
  (when (>= *top-of-trail* +max-of-trail+)
    (throw 'debord  (progn (terpri) (princ "Trail Overflow") (terpri)) ))
  (vset *memory* *top-of-trail* x)
  (incf *top-of-trail*))

(defun poptrail (top)	       
  (do ()
      ((= *top-of-trail* top))
    (let ((x (svref *memory* (decf *top-of-trail*))))
      (if (numberp x)                   ; classical var else frozen
          (vset *memory* x       (cons 'LIBRE +bottom-of-global-stack+))
          (vset *memory* (car x) (cons 'LIBRE (cdr x)))))))

;;; V. Frozen Goals Stack
;;;
(defmacro FGvar   (x) `(svref *memory* ,x))
(defmacro FGtail  (x) `(svref *memory* (1+ ,x)))
(defmacro FGgoal  (x) `(svref *memory* (+ 2 ,x)))
(defmacro FGenv   (x) `(svref *memory* (+ 3 ,x)))
(defmacro frozen? (x) `(< (fgblock ,x) +bottom-of-global-stack+))

(defmacro push_fg (v b eb r)            ; allocates a frozen block
  `(if (>= (+ *top-of-frozen-goals-stack* 3) +bottom-of-global-stack+)
       (throw 'debord  (progn (terpri) (princ "Frozen Goals Stack Overflow") (terpri)) )
       (progn (vset *memory* *top-of-frozen-goals-stack* ,v)
              (vset *memory* (incf *top-of-frozen-goals-stack*) ,r)
              (vset *memory* (incf *top-of-frozen-goals-stack*) ,b)
              (vset *memory* (incf *top-of-frozen-goals-stack*) ,eb)
              (incf *top-of-frozen-goals-stack*))))

;;;; THE END ;;;;
