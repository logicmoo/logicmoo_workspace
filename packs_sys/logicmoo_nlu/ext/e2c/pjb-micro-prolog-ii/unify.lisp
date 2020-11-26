;;; mini-PrologII
;;; unify.lsp
;;;

(defmacro bind (x sq e xt)              ; binds x to (sq,e)
  `(progn
     (when (or (and (> ,x +bottom-of-local-stack+) (< ,x *backtracking-local-register*))
               (< ,x *backtracking-global-register*)) 
       (pushtrail ,xt))                 ; if post-bound trail it
     (rplaca (svref *memory* ,x) ,sq)
     (rplacd (svref *memory* ,x) ,e)))

(defun bindte (x sq e)                  ; binds x to (sq,e)
  (if (frozen? x)
      (let ((y (fgblock x)))
        (push y *awakened-goals*)       ; awakes the delayed goals
        (bind x sq e (cons x y)))       ; to trail the old value
      (bind x sq e x)))

(defun bindfg (x b eb r)                ; binds x to the frozen goal b
  (bind x 'LIBRE *top-of-frozen-goals-stack* (if (frozen? x)
                                                 (cons x r)
                                                 x))
  (push_fg x b eb r))                   ; allocates a new frozen block

(defun unify_with (largs el eg)         ; unifies head of clause
  (catch 'impossible                    ; with registers Ai
    (dotimes (i (svref *memory* +max-of-trail+))
      (unif
       (let ((te (svref *memory* (+ +max-of-trail+ 1 i))))
         (val (car te) (cdr te)))
       (ultimate (pop largs) el eg)))))

