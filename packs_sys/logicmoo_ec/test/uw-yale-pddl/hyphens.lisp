; Treat a-b as a - b, etc.
(defmacro with-pddl-syntax-macros (&rest body)
   `(unwind-protect
      (progn
         (set-macro-character #\- #'pddl-pm nil pddl-read-table*)
         (set-macro-character #\+ #'pddl-pm nil pddl-read-table*)
         (let ((*readtable* pddl-read-table*))
            ,@body))
     (set-syntax-from-char #\- #\- pddl-read-table*)
     (set-syntax-from-char #\+ #\+ pddl-read-table*)))

; Handle + or - as token, unless followed by a digit
(defun pddl-pm (stream char)
   (let ((c (peek-char nil stream)))
      (cond ((digit-char-p c)
             (let ((n 
                    (cond ((member ':harlequin-pc-lisp *features*)
                           (harlequin-workaround c stream))
                          (t (read stream)))))
               (cond ((char= char #\-) (- n))
                     (t n))))
            (t
             (cond ((eq char #\-) '-)
                   (t '+))))))

; Harlequin bug: peek-char = read-char in some situations.
(defun harlequin-workaround (c stream)
   (declare (ignore c))
   (let ((chars '()))
      (loop 
         (let ((d (read-char stream)))
            (cond ((member d '(#\space #\tab #\newline))
                   (return (read-from-string
                              (coerce (reverse chars) 'string)))))
            (push d chars)))))
