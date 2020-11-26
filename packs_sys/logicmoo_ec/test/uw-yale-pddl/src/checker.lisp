(in-package "PDDL")

(defvar flag-count*)

(defvar when-to-dump* 'always
   "When to write out syntax-checked expressions: always, never, or when-flagged")

(defvar print-pretty-patch* t
  "Hack to trick Harlequin into pretty-printing")

(defvar strict* nil
  "If t, enforce severe syntactic restrictions: just one item per file;
   no addenda;
   and fields must occur in the order specified in the manual.")


; Main entry point for file syntax checking
(defun pddl-file-syncheck (in-file &optional (out-file nil))
  (setq in-file (pathname in-file))
  (with-open-file (insrm in-file :direction ':input)
                (cond ((and (not out-file)
                            (not (eq when-to-dump* 'never)))
                       (setq out-file
                             (pathname-new-extension in-file "chk"))))
                (cond (out-file
                       (with-open-file (outsrm out-file
                                               :direction ':output
                                               :if-exists ':supersede
                                               :if-does-not-exist ':create)
                          (syncheck-and-dump insrm outsrm)))
                      (t
                       (syncheck-and-dump insrm nil)))))

(defun syncheck-and-dump (insrm outsrm)
         (let ((total-flag-count 0))
	    (let ((r nil) (found-nontrivial nil))
	       (loop
		  (setq r (read insrm nil 'end-of-file))
		  (cond ((eq r 'end-of-file)
			 (return)))
		  (multiple-value-bind (n nontrivial)
				       (expand-and-dump r outsrm)
                     (setq total-flag-count
			   (+ total-flag-count n))
		     (cond (nontrivial
			    (cond ((and found-nontrivial strict*)
				   (flag-strictness-violation
				      "Just one expression allowed per file in strict subset"
				      outsrm)))
			    (setq found-nontrivial t))))))
            `(flagged ,total-flag-count expressions)))

(defun flag-strictness-violation (message outsrm)
   (setq flag-count* (+ flag-count* 1))
   (cond ((not (eq when-to-dump* 'never))
	  (form-dump
	      (flagexp message "__")
	      outsrm))))

; First val is number of flagged subexpressions; second is t if form was
; nontrivial.
(defun expand-and-dump (r out-file)
   (let ((flag-count* 0)
	 (nontrivial nil))
      (let ((mh (pddl-form-handler r)))
	 (cond ((consp mh)
                (case (car mh)
                   (macro
		    (multiple-value-bind (stuff nonsense)
					 (funcall (cadr mh) r)
                                         (declare (ignore nonsense))
		       (dolist (e stuff)
			  (multiple-value-bind (n nontriv)
					       (expand-and-dump e out-file)
			     (setq flag-count*
				   (+ flag-count* n))
			     (if nontriv (setq nontrivial t))))))
                   (top-level
		    (setq nontrivial t)
		    (maybe-dump (funcall (cadr mh) r)
				out-file))))
               (t
                (maybe-dump r out-file)))
         (values flag-count* nontrivial))))
               
(defun maybe-dump (e out-file)
   (cond ((or (eq when-to-dump* 'always)
              (and (eq when-to-dump* 'when-flagged)
                   (> flag-count* 0)))
	  (form-dump e out-file))))

(defun form-dump (e out-file)
          (let ((*print-pretty* t)
	        (*print-level* nil)
	        (*print-length* nil)
	        (print-pretty-patch* t))
            (print e out-file)))

(def-pddl-form-handler ^^ macro (e)
    (values (list (cadr e)) nil))

(def-pddl-form-handler achieve macro (e)
    (values (list `(in-context (--) :precondition ,(cadr e)))
            nil))

(defmacro define (&rest stuff)
   `(pddl-define ',stuff))

(defun pddl-define (stuff)
   (let ((flag-count* 0))
      (cond ((and (consp (car stuff))
                  (symbolp (caar stuff)))
             (let ((h (pddl-form-handler (car stuff))))
                (cond ((and h (eq (car h) 'definer))
                       (let ((ex (funcall (cadr h) `(define ,@stuff))))
                          (cond ((> flag-count* 0)
				 (cerror "I'll proceed"
					 "Flagged ~s subexpression(s) in define ~s"
					 flag-count* (car stuff))
                                 ex)
                                (t (caar stuff)))))
                      (t
                       (cerror "I'll ignore it"
                               "No way to define: ~s"
                               stuff)
                       nil))))
            (t
             (cerror "I'll ignore it"
                     "Unintelligible define ~s"
                     stuff)
             nil))))

