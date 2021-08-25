(setq *consult-motel-string* "['/usr/local/knowRep/motel/motel'].") ; Prolog Befehl um Motel zu konsulten.
(setq *prolog-executable* "/usr/local/languages/sicstus2.1/sicstus")  ; Ausfuehrbares Prolog Program mit Pfad
(setq *int_dot_pl* "/HG/hiwis/timm/lucid/int.pl")           ; Pfad zu int.pl, iny.o muss im gleichen
                                                            ; Verzeichnis stehen.


(defun start-prolog (   )
  (multiple-value-bind
   (i e stat p)
   (run-program "/usr/local/sicstus2.1/sicstus" :input :stream :output :stream :error-output :stream :wait nil)
   (read-line e)
   (write-line (concatenate 'string "['" *int_dot_pl* "'].") i)
   (force-output i)
   (read-line e)
   (read-line e)
   (read-line e)
   (read-line e)
   (setq *prolog-lisp-i/o-stream* i)
   (setq *prolog-lisp-err-i/o-stream* e)
   (setq *prolog-lisp-stat-message* stat)
   (setq *prolog-process-number* p)
   (values i e stat p)))

(defun reset-prolog (&optional (i/o-stream *prolog-lisp-i/o-stream*) (err-i/o-stream *prolog-lisp-err-i/o-stream*) (process-number *prolog-process-number*))
  (run-program "kill" :arguments (cons "-INT" (list (format nil "~a" process-number))))
  (write-line "a" i/o-stream)
  (force-output i/o-stream)
  (read-line err-i/o-stream))


(defun start-motel (   )
(multiple-value-bind
 (i e stat p)
 (run-program *prolog-executable* :input :stream :output :stream :error-output :stream :wait nil)
 (read-line e)
 (write-line *consult-motel-string* i)
 (force-output i) 
 (read-line e)
 (read-line e)
 (read-line e)
 (read-line e)
 (write-line (concatenate 'string "['" *int_dot_pl* "'].") i)
 (force-output i)
 (read-line e)
 (read-line e)
 (read-line e)
 (read-line e)
 (setq *prolog-lisp-i/o-stream* i)
 (setq *prolog-lisp-err-i/o-stream* e)
 (setq *prolog-lisp-stat-message* stat)
 (setq *prolog-process-number* p)
 (values i e stat p)))

(defun prolog-goal ( prolog-goal-list &optional (i/o-stream *prolog-lisp-i/o-stream*) (err-i/o-stream *prolog-lisp-err-i/o-stream*) (process-number *prolog-process-number*))  

  (write-line "" i/o-stream)      ;;; Return abschicken
  (force-output i/o-stream)
  (if (listen err-i/o-stream) (read-line err-i/o-stream))                 
  (if (listen err-i/o-stream) (read-line err-i/o-stream))                 
  (if (listen err-i/o-stream) (read-line err-i/o-stream))                 
  (if (listen err-i/o-stream) (read-line err-i/o-stream))                 
  (if (listen err-i/o-stream) (read-line err-i/o-stream))                 
  (do ()
      ((not (listen err-i/o-stream)) nil)
      (read-line err-i/o-stream))

  (setq prolog-goal-listtt (variables-list prolog-goal-list))
                                         

  (cond 
   ((null (cdr prolog-goal-list))        
    (write-line (concatenate 'string (convert-to-prolog (car prolog-goal-list)) ".") i/o-stream))
   (t
    (write-line (concatenate 'string
			     (do* ((string (convert-to-prolog (car prolog-goal-list)))
				   (liste (cdr prolog-goal-list)))
				  ((null liste) string)
				  (setq string (concatenate 'string string "," (convert-to-prolog (car liste))))
				  (setq liste (cdr liste)))
			     ".") i/o-stream)))
  
  (force-output i/o-stream)
  (read-line err-i/o-stream)
  (cond 
   ((= 0 (length prolog-goal-listtt))
    (if (equal "yes" (read-line err-i/o-stream))
	(values (do ((input-line nil)
		   (printed-output ""))
		  ((not (listen i/o-stream)) printed-output)
		  (if (listen i/o-stream) (setq printed-output (concatenate 'string printed-output (string #\nl) (read-line i/o-stream))))) 'last)
      (values (do ((input-line nil)
		   (printed-output ""))
		  ((not (listen i/o-stream)) printed-output)
		  (if (listen i/o-stream) (setq printed-output (concatenate 'string printed-output (string #\nl) (read-line i/o-stream))))) nil)))
   (t
    (let ((input-line "")
	  )
      
      (do ((liste prolog-goal-listtt)
	   )
	  ((null liste) nil)
	  (set (car liste) (car liste))
	  (setq liste (cdr liste)))
      (values (do ((input-line nil)
		   (printed-output ""))
		  ((not (listen i/o-stream)) printed-output)
		  (if (listen i/o-stream) (setq printed-output (concatenate 'string printed-output (string #\nl) (read-line i/o-stream)))))
		
	      (do ((result nil)
		   (input-line "")
		   (ready nil))
		  ;;	  ((not (listen err-i/o-stream)) result)
		  ( ready result)
		  (setq input-line (string-trim '(#\space) (special-read-line err-i/o-stream)))
		  (cond
		   ((equal input-line "no")
		    (setq result nil)
		    (setq ready t))
		   ((equal input-line "yes")
		    (setq result t)
		    (setq ready t))
		   ((or 
		     (equal #\, (char input-line (1- (length input-line))))
		     (equal #\? (char input-line (1- (length input-line))))
		     )
		    (if (equal #\? (char input-line (1- (length input-line)))) (setq ready t))
		    (setq result t)
		    (setq input-line (subseq input-line 0 (1- (length input-line))))
		    (cond
		     ((and (equal #\? (elt (convert-to-lisp (subseq input-line (1+ (position #\= input-line)))) 0))
			   (atom (read-from-string (convert-to-lisp (subseq input-line (1+ (position #\= input-line)))))))
		      (set (read-from-string (concatenate 'string (string #\?) (string-trim '(#\space) (subseq input-line 0 (position #\space input-line))))) (read-from-string (convert-to-lisp (subseq input-line (1+ (position #\= input-line))))))
		      (set (read-from-string (convert-to-lisp (subseq input-line (1+ (position #\= input-line))))) (read-from-string (concatenate 'string (string #\?) (string-trim '(#\space) (subseq input-line 0 (position #\space input-line))))))
		      )
		     (t
		      (set (read-from-string (concatenate 'string (string #\?) (string-trim '(#\space) (subseq input-line 0 (position #\space input-line))))) (read-from-string (convert-to-lisp (subseq input-line (1+ (position #\= input-line))))))))))))))))
		  




(defun prolog-next (&optional (i/o-stream *prolog-lisp-i/o-stream*) (err-i/o-stream *prolog-lisp-err-i/o-stream*) (process-number *prolog-process-number*))
  (write-line ";" i/o-stream)
  (force-output i/o-stream)
  (read-line err-i/o-stream)
  (let ((input-line "")
	)
    
    (do ((liste prolog-goal-listtt)
	 )
	((null liste) nil)
	(set (car liste) (car liste))
	(setq liste (cdr liste)))
    (values (do ((input-line nil)
		 (printed-output ""))
		((not (listen i/o-stream)) printed-output)
		(if (listen i/o-stream) (setq printed-output (concatenate 'string printed-output (string #\nl) (read-line i/o-stream)))))
	    (do ((result nil)
		 (input-line "")
		 (last-line nil))
		(last-line result)
		(setq input-line (string-trim '(#\space) (special-read-line err-i/o-stream)))
		(if (and (equal #\? (char input-line (1- (length input-line))))
			 (equal #\space (char input-line (1- (1- (length input-line))))))
		    (setq last-line t))
		(cond
		 ((equal input-line "no")
		  (setq result nil)
		  (setq last-line t))
		 ((equal input-line "yes")
		  (setq last-line t)
		  (setq result 'last))
		 ((or 
		   (equal #\, (char input-line (1- (length input-line))))
		   (equal #\? (char input-line (1- (length input-line))))
		   )
		  (setq result t)
		  (setq input-line (subseq input-line 0 (1- (length input-line))))
		  (cond
		   ((and (equal #\? (elt (convert-to-lisp (subseq input-line (1+ (position #\= input-line)))) 0))
			 (atom (read-from-string (convert-to-lisp (subseq input-line (1+ (position #\= input-line)))))))
		    (set (read-from-string (concatenate 'string (string #\?) (string-trim '(#\space) (subseq input-line 0 (position #\space input-line))))) (read-from-string (convert-to-lisp (subseq input-line (1+ (position #\= input-line))))))
		    (set (read-from-string (convert-to-lisp (subseq input-line (1+ (position #\= input-line))))) (read-from-string (concatenate 'string (string #\?) (string-trim '(#\space) (subseq input-line 0 (position #\space input-line))))))
		    )
		   (t
		    (set (read-from-string (concatenate 'string (string #\?) (string-trim '(#\space) (subseq input-line 0 (position #\space input-line))))) (read-from-string (convert-to-lisp (subseq input-line (1+ (position #\= input-line))))))))))))))
	 






(defmacro do-prolog (prolog vars end-test &body body)
  
					; This macro allows to call Prolog and to process the Prolog answers in the same way as the Lisp DO macro.
					; prolog is a list of Prolog goal clauses. The variables are Lisp symbols prefixed with ?.
					; The rest is like the DO macro.
					; vars is a list of variable declarations, either a variable name or a list (variable init) or al list
					; (variable init step). The evaluation of init determines the initial value of the variable, the evaluation
					; of step (for example (incf variable) determines the next values of the variable.
					; end-test is a list (test . forms).
					; If test returns T then the iteration is stopped and forms are evlaluated with an implicit PROGN.
					; The vlause of do-prolog is the value of the last form.
					; body is the body of the do form.
					; 
					; The Prolog query is submitted to prolog and the variable names are bound to the result of the Prolog evaluation. 
					; Then the DO-loop is entered and at each iteration, the new bindings of the ?..-variables are computed in Prolog.
					; The loop stops if either Prolog says "no" or if the end-test becomes true.
  
  (labels ((is-var (expression)
		   (and (not (eq `:list expression)) (symbolp expression) (equal #\? (elt (symbol-name expression) 0))))
	   (vars (expression)
		 (cond((consp expression)
		       (cond ((consp (car expression))
			      (append (vars (car expression)) (vars (cdr expression))))
			     ((is-var (car expression)) (cons (car expression) (vars (cdr expression))))
			     (t (vars (cdr expression)))))
		      ((is-var expression) (list expression)))))
	  (let ((pvars (delete-duplicates(vars prolog))))
	    `(let (prolog-resulttt ,@pvars)
	       (declare (special ,@pvars))
	       (cond ((setq prolog-resulttt (prolog-goal (quote ,prolog)))
		      (do ,vars 
			  ((or (null prolog-resulttt) ,(car end-test)) ,@(cdr end-test))
			  ,@body
			  (cond((eq prolog-resulttt 'last) (setq prolog-resulttt nil))
			       (t (setq prolog-resulttt (prolog-next)))))))))))

(defun kill-prolog (&optional (i/o-stream *prolog-lisp-i/o-stream*) (err-i/o-stream *prolog-lisp-err-i/o-stream*) (process-number *prolog-process-number*))
  (let ((arg '()))
    (setq arg (cons "-9" (list (format nil "~a" process-number))))
    (run-program "kill" :arguments arg)))

(defun convert-to-prolog ( expression )
  (cond ((null expression) "")
	((and (atom expression)
	      (symbolp expression)
	      (equal '#\? (elt (symbol-name expression) 0)))
	 (format nil "~:(~a~)" (subseq (symbol-name expression) 1)))
	((and (atom expression)
	      (not (symbolp expression))) ; Zahl
	 (format nil "~a" expression))
	((atom expression) 
	 (concatenate 'string "'" (do ((symbol-string (symbol-name expression))
				       (return-string ""))
				      ((equal "" symbol-string) return-string)
				      (cond 
				       ((equal #\_ (elt symbol-string 0))
					(cond 
					 ((equal #\_ (elt symbol-string 1))
					  (setq return-string (concatenate 'string return-string "_"))
					  (setq symbol-string (subseq symbol-string 2)))
					 (t
					  (setq return-string (concatenate 'string return-string (format nil "~:@(~a~)" (string (elt symbol-string 1)))))
					  (setq symbol-string (subseq symbol-string 2)))))
				       (t
					(setq return-string (concatenate 'string return-string (format nil "~(~a~)" (string (elt symbol-string 0)))))
					(setq symbol-string (subseq symbol-string 1))))) "'"))
	
	((equal ':list (car expression)) 
	 (concatenate 'string "[" (convert-to-prolog (cadr expression))
		      (do ((arg-list (cddr expression))
			   (give-back ""))
			  ((null arg-list) give-back)
			  (setq give-back (concatenate 'string give-back 
						       ", "
						       (convert-to-prolog (car arg-list))))
			  (setq arg-list (cdr arg-list)))
		      "]"))
	((equal '^ (car expression))
	 (concatenate 'string (do ((arg-list (cadr expression))
				   (give-back ""))
				  ((null arg-list) give-back)
				  (setq give-back (concatenate 'string give-back
							       (convert-to-prolog (car arg-list))
							       "^"))
				  (setq arg-list (cdr arg-list)))
		      (convert-to-prolog (caddr expression))))
	
	(t
	 (concatenate 'string (convert-to-prolog (car expression)) "(" 
		      (convert-to-prolog (cadr expression))
		      (do ((arg-list (cddr expression))
			   (give-back ""))
			  ((null arg-list) give-back)
			  (setq give-back (concatenate 'string give-back 
						       ", "
						       (convert-to-prolog (car arg-list))))
			  (setq arg-list (cdr arg-list)))
		      ")"))))

(defun double-underscores (string)
  (concatenate 'string (subseq string 0 (if (position #\_ string) (position #\_ string) (length string))) (if (position #\_ string) "__" "") (if (position #\_ string) (double-underscores (subseq string (1+ (position #\_ string)))) "")))


(defun convert-to-lisp ( expr-string )
  (setq expr-string (double-underscores expr-string))
  (setq expr-string (remove #\   expr-string))                                 
  (setq expr-string (remove #\'  expr-string))                                 
  (let* (
	 (open-paren    (position #\( expr-string))
	 (open-bracket  (position #\[ expr-string))
	 (close-paren (if open-paren 
			  (do ((counter 1)
			       (pos-open nil)
			       (pos-close nil)
			       (pos open-paren))
			      ((equal '0 counter) pos-close)
			      (setq pos-close (position #\) expr-string :start (+ 1 pos)))
			      (setq pos-open (position #\( expr-string  :start (+ 1 pos)))
			      (if (or (not pos-open) (> pos-open pos-close)) (setq counter (- counter 1)) (setq counter (+ counter 1)))
			      (if (or (not pos-open) (> pos-open pos-close)) (setq pos pos-close) (setq pos pos-open))
			      )
			nil))
	 
	 (close-bracket (if open-bracket
			    (do ((counter 1)
				 (pos-open nil)
				 (pos-close nil)
				 (pos open-bracket))
				((equal '0 counter) pos-close)
				(setq pos-close (position #\] expr-string :start (+ 1 pos)))
				(setq pos-open (position #\[ expr-string  :start (+ 1 pos)))
				(if (or (not pos-open) (> pos-open pos-close)) (setq counter (- counter 1)) (setq counter (+ counter 1)))
				(if (or (not pos-open) (> pos-open pos-close)) (setq pos pos-close) (setq pos pos-open))
				)
			  nil))
	 (comma         (position #\, expr-string))
	 (bar           (position #\| expr-string))
	 )
    (cond 
     ((equal 0 (length expr-string)) "")                                        
     
     
     ((and comma
	   (if open-bracket (if (< open-bracket comma) (< close-bracket comma) t) t)
	   (if open-paren   (if (< open-paren   comma) (< close-paren   comma) t) t))
      (concatenate 'string (convert-to-lisp (subseq expr-string 0 comma)) " " (convert-to-lisp (subseq expr-string (+ 1 comma)))))
     
     ((and open-paren (or (not open-bracket) (< open-paren open-bracket)) (or (not comma) (< open-paren comma)))
      (concatenate 'string "(" (subseq expr-string 0 open-paren) " "            
		   (convert-to-lisp (subseq expr-string (+ 1 open-paren) close-paren))
		   ")" (convert-to-lisp (subseq expr-string (+ 1 close-paren)))))
     
     ((equal 0 comma) 
      (convert-to-lisp (subseq expr-string 1)))                                 
     
     
     ((and open-bracket (or (not comma) (< open-bracket comma)) (or (not open-paren) (< open-bracket open-paren))
	   (or (not bar) (and bar (< close-bracket bar))))
      (concatenate 'string "(:list " (convert-to-lisp (subseq expr-string (+ 1 open-bracket) close-bracket)) ")"))
     
     
     
     ((and open-bracket (or (not comma) (< open-bracket comma)) (or (not open-paren) (< open-bracket open-paren))
	   (and bar (< bar close-bracket)))
      (concatenate 'string  "(:openlist (" (convert-to-lisp (subseq expr-string (+ 1 open-bracket) bar)) ") "
		   (convert-to-lisp (subseq expr-string (+ 1 bar) close-bracket)) ")" ))
     
     ((equal #\? (elt expr-string 0))                   
      expr-string)
     
     ((upper-case-p (elt expr-string 0))                
      (concatenate 'string "?" expr-string))
     
     (t
      expr-string)
     )))

(defun variables-list ( expression )   
  (if (atom expression)
      nil
    (let (
	  (liste nil)
	  (head (pop expression))
	  )
      (if (atom head)
	  (if (and 
	       (symbolp head)
	       (equal #\? (elt (symbol-name head) 0))
	       (not (equal ':openlist head))
	       (not (equal ':list head)))
	      (setq liste (adjoin head liste)) ())
	(setq liste (union (variables-list head) liste)))
      (if expression (setq liste (union liste (variables-list expression))) () )
      liste
      )
    ))


(defmacro do-prolog-with-streams (s1 s2 p call)
  `(let ((*prolog-lisp-i/o-stream* ,s1)(*prolog-lisp-err-i/o-stream* ,s2)(*prolog-process-number* ,p))
     (declare (special *prolog-lisp-i/o-stream* *prolog-lisp-err-i/o-stream* *prolog-process-number*))
     ,call))


(defun special-read-line (stream)
  (do ((line-string "")
       (character nil)
       (look-ahead-char nil)
       (line-read nil))
      (line-read line-string)
      (setq character (read-char-no-hang stream))
      (cond 
       ((equal character nil) 
	nil)
       ((equal character #\Newline) 
	(setq line-read t))
       ((equal character #\?)
	(setq look-ahead-char (read-char-no-hang stream))
	(cond 
	 ((and (equal look-ahead-char #\space) (not (listen stream)))
	  (setq line-read t)
	  (setq line-string (concatenate 'string line-string "?")))
	 (t
	  (unread-char look-ahead-char stream)
	  (setq line-string (concatenate 'string line-string "?")))))
       (t
	(setq line-string (concatenate 'string line-string (string character)))))))


