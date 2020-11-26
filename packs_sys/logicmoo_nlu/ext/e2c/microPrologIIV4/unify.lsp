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

