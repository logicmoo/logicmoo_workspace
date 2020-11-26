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
