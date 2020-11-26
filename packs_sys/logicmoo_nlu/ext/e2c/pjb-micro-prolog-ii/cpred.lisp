;;; mini-Cprolog & mini-PrologII
;;; cpred.lsp
;;;

(defmacro value1 (x) `(car (ultimate ,x *current-environment* *current-goal*)))

(defun uni (x y)
  (catch 'impossible
    (unif (ultimate x *current-environment* *current-goal*)
          (ultimate y *current-environment* *current-goal*))))


(define-prolog-function |write| (x)
  "
write/1 (?term)
"
  (write1 (ultimate x *current-environment* *current-goal*)))

(defun write1 (te)                      ; depending on te
  (let ((x (car te)) (e (cdr te)))
    (cond 
      ((null x) (format t "[]"))
      ((atom x) (format t "~A" x))
      ((var? x) (format t "X~A" (adr x e)))
      ((list? x) (format t "[")
       (writesl (val (cadr x) e) (val (caddr x) e))
       (format t "]"))
      ((writesf (functor (des x)) (largs x) e)))))

(defun writesl (te r)                   ; for a list
  (write1 te)
  (let ((q (car r)) (e (cdr r)))
    (cond
      ((null q))
      ((var? q) (format t "|X~A" (adr q e)))
      (t (format t ",") 
         (writesl (val (cadr q) e) (val (caddr q) e))))))

(defun writesf (fct largs e)            ; for a functional term
  (format t "~A(" fct)
  (write1 (val (car largs) e))
  (mapc (lambda (x) (format t ",") (write1 (val x e))) 
        (cdr largs))
  (format t ")"))

                                        
(define-prolog-function |nl| ()
  "
nl/0
"
  (terpri))


(define-prolog-function |tab| (x)
  "
tab/1 (+int)
"
  (dotimes (i (value1 x)) (format t " ")))


(define-prolog-function |read| (x)
  "
read/1 (?term)
"
  (let ((te (read_terme)))              ; gets the term
    (catch 'impossible 
      (unif (ultimate x *current-environment* *current-goal*) 
            (cons (cdr te) (push1_g (car te)))))))

(defun read_terme ()
  (let ((*lvarloc*  nil)
        (*lvarglob* nil))
    (let ((te (read_term (rchnsep) 2)))
      (rchnsep) (cons (length *lvarglob*) (c te)))))

(defun push1_g (n)
  (if (>= (+ *top-of-global-stack* n) +bottom-of-local-stack+) ; allocates a global env
      (throw 'debord (print "Global Stack Overflow")))
  (dotimes (i n (- *top-of-global-stack* n)) 
    (vset *memory* *top-of-global-stack* (cons 'LIBRE +bottom-of-global-stack+)) 
    (incf *top-of-global-stack*)))


(define-prolog-function |get| (x)
  "
get/1 (?char)
"
  (uni x (char-int (rchnsep))))


(define-prolog-function |get0| (x)
  "
get0/1 (?char)
"
  (uni x (char-int (read-char))))


(define-prolog-function |var| (x)
  "
var/1 (?term)
"
  (unless (var? (value1 x))
    'fail))


(define-prolog-function |nonvar| (x)
  "
nonvar/1 (?term)
"
  (when (var? (value1 x))
    'fail))


(define-prolog-function |atomic| (x)
  "
atomic/1 (?term)
"
  (when (listp (value1 x))
      'fail))


(define-prolog-function |atom| (x)
  "
atom/1 (?term)
"
  (unless (symbolp (value1 x)) 'fail))


(define-prolog-function |number| (x)
  "
number/1 (?term)
"
  (unless (numberp (value1 x)) 'fail))


(define-prolog-function |fail| ()
  "
fail/0
"
  'fail)


(define-prolog-function |true| ()
  "true/0"
  (values))


(define-prolog-function |divi| (x y z)
  "
divi/3 (+int,+int,?int)
"
  (uni z (floor (value1 x) (value1 y))))


(define-prolog-function |mod| (x y z)
  "
mod/3 (+int,+int,?int)
"
  (uni z (rem (value1 x) (value1 y))))


(define-prolog-function |plus| (x y z)
  "
plus/3 (+int,+int,?int)
"
  (uni z (+ (value1 x) (value1 y))))

(define-prolog-function |minus| (x y z)
  "
minus/3 (+int,+int,?int)
"
  (uni z (- (value1 x) (value1 y))))

(define-prolog-function |mult| (x y z)
  "
mult/3 (+int,+int,?int)
"
  (uni z (* (value1 x) (value1 y))))


(define-prolog-function |le| (x y)
  "
le/2 (+int,+int)
"
  (if (> (value1 x) (value1 y)) 'fail))


(define-prolog-function |lt| (x y)
  "
lt/2 (+int,+int)
"
  (if (>= (value1 x) (value1 y)) 'fail))


(define-prolog-function |name| (x y)
  "
name/2 (?atom,?list)
"
  (let ((b (value1 x)))
    (if (var? b) 
        (uni x (impl (undo_l (ultimate y *current-environment* *current-goal*))))
        (uni y (do_l (expl b))))))


(defun undo_l (te)
  (let ((x (car te)) (e (cdr te)))
    (if (atom x) 
        x
        (cons (undo_l (val (cadr x) e)) (undo_l (val (caddr x) e))))))

(defun do_l (x)
  (if (atom x) x (list '(\. 2) (car x) (do_l (cdr x)))))

(defun impl (l)
  (intern (map 'string (function code-char) l)))

(defun expl (at)
  (map 'list (function char-code) (string at)))


(define-prolog-function |consult| (f)
  "
consult/1 (+atom)
"
  (format t "~A~%" (load (value1 f))))


(define-prolog-function |abolish| (p)
  "
abolish/1 (+ atom)
"
  (mapc (lambda (x) (setf (get p x) nil))
        '(atom empty list fonct def)))


(define-prolog-function |cputime| (x)
  "
cputime/1 (? int)
"
  (uni x (float (/ (get-internal-run-time) 
                   internal-time-units-per-second))))
