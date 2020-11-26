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
