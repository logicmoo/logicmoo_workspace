; mini-Cprolog & mini-PrologII
; cparser.lsp
;

(defvar *lvarloc nil)                   ; list of local vars
(defvar *lvarglob nil)                  ; list of global vars
(set-macro-character #\% (get-macro-character #\;))

(defun rch ()                           ; skips Newline
  (do ((ch (read-char) (read-char)))
      ((char/= ch #\Newline) ch)))
(defun rchnsep ()                       ; skips Newline and Space
  (do ((ch (rch) (rch)))
      ((char/= ch #\space) ch)))

(defun special (ch) (char= ch '#\_))
(defun alphanum (ch)                    ; symbol normal constituant
  (or (alphanumericp ch) (special ch)))
(defun valdigit (ch) (digit-char-p ch)) 
 
(defun read_number (ch)                 ; next integer
  (do ((v (valdigit ch) (+ (* v 10) (valdigit (read-char)))))
      ((not (digit-char-p (peek-char))) v)))

(defun implode (lch) (intern (map 'string #'identity lch)))

(defun read_atom (ch)                   ; next normal symbol
  (do ((lch (list ch) (push (read-char) lch)))
      ((not (alphanum (peek-char))) (implode (reverse lch)))))

(defun read_at (ch)                     ; next special symbol
  (do ((lch (list ch) (push (read-char) lch)))
      ((char= (peek-char) #\') (read-char) (implode (reverse lch)))))

(defun read_string (ch)                 ; Prolog list of ascii codes
  (do ((lch (list (char-int ch)) (push (char-int (read-char)) lch)))
      ((char= (peek-char) #\") (read-char) (do_l (reverse lch)))))

(defun read_var (ch n)                  ; next variable
  (status (read_atom ch) n))

(defun status (nom n)                   ; n is the term's depth
  (if (= n 1)                           ; local if not yet global
      (unless (member nom *lvarglob) (pushnew nom *lvarloc))
    (progn (if (member nom *lvarloc)    ; else global
               (setq *lvarloc (delete nom *lvarloc)))
           (pushnew nom *lvarglob)))
  nom)

(defun read_simple (ch n)               ; next simple term
  (cond
   ((or (upper-case-p ch) (special ch)) (read_var ch n))
   ((digit-char-p ch) (read_number ch))
   ((char= ch #\") (read_string (read-char)))
   ((char= ch #\') (read_at (read-char)))
   (t (read_atom ch))))

(defun read_fct (ch n)                  ; next functional term
  (let ((fct (read_simple ch n)) (c (rchnsep)))
    (if (char= c #\()                   ; reads its arguments
        (let ((la (read_args (rchnsep) (1+ n))))
          (cons (list fct (length la)) la)) ; adds its descriptor
      (progn (unread-char c) fct))))    ; else atom
                
(defun read_args (ch n)                 ; args of a functional term
  (let ((arg (read_term ch n)))
    (if (char= (rchnsep) #\,)
        (cons arg (read_args (rchnsep) n))
      (list arg))))
   
(defun read_list (ch n)                 ; next list
  (if (char= ch #\])                    ; ending with the empty list
      ()
    (let ((te (read_term ch n)))        ; gets the head
      (case (rchnsep)
            (#\, (list '(\.  2) te (read_list (rchnsep) n)))
            (#\| (prog1                 ; dotted pair
                     (list '(\. 2) te (read_term (rchnsep) n)) 
                   (rchnsep))) 
            (#\] (list '(\. 2) te nil))))))

(defun read_term (ch n)                 ; next term
  (if (char= ch #\[) (read_list (rchnsep) (1+ n)) (read_fct ch n))) 

(defun read_tail (ch)                   ; read the body of a rule
  (let ((tete (read_pred ch)))          ; gets the first goal
    (if (char= (rchnsep) #\.)
        (list tete)
      (cons tete (read_tail (rchnsep))))))

(defun read_clause (ch)                 ; reads a clause
  (let ((tete (read_pred ch)))
    (if (char= (rchnsep) #\.)
        (list tete)
      (progn (read-char) (cons tete (read_tail (rchnsep)))))))

(defun c (l)                            ; codes once read
  (if (atom l)
      (if (member l *lvarloc)
          (cons 'L (position l *lvarloc))
        (if (member l *lvarglob) (cons 'G (position l *lvarglob)) l))
    (if (eq (car l) '!)                 ; the cut with its intern
        (list '! (length *lvarloc))     ; argument
      (cons (c (car l)) (c (cdr l))))))
