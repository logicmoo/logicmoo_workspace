(defpackage "EXPDT"
  (:use "COMMON-LISP"))

(in-package "EXPDT")

;; Wheresyms behave something like gensyms that are interned when printed.
(defstruct (wheresym
               (:print-function
		   (lambda (y srm k)
		       (declare (ignore k))
		       (let ((sym (where->here y)))
		          (format srm "~s" sym))))
	       (:constructor
		   make-wheresym (name))
	       (:predicate is-wheresym))
   (name "" :type string)
   (sym nil)
   (plist '()))

(defun wheresym-get (s ind)
  (let ((p (assoc IND (wheresym-plist s) :test #'eq)))
    (cond (p (cdr p))
	  (t nil))))

(defun wheresym-put (s ind val)
  (let ((p (assoc IND (wheresym-plist s) :test #'eq)))
    (cond ((null p)
	   (setf P (cons ind val))
	   (push p (wheresym-plist s)))
	  (t
	   (setf (cdr p) val)))
    val))

(defsetf wheresym-get wheresym-put)

(defun where->here (wh)
   (let ((s (wheresym-sym wh)))
      (cond ((null s)
	     (setf s (intern (concatenate 'string
					  "!:" (wheresym-name wh))))
	     (cond ((get 's 'wheresym)
		    (cerror "Will make up another symbol"
			    "Duplicate wheresym ~s" S)
		    (setf s (gensym))))
	     (setf (get s 'wheresym) wh)
	     (setf (wheresym-sym wh) s)))
      s   ))

(defun string->wheresym (s)
   (let ((sym (intern (concatenate 'string "!:" s))))
      (or (get sym 'wheresym)
	  (error "Unknown symboid ~s" sym)))) 

(defun is-symboid (s) (or (symbolp s) (is-wheresym s)))

(defun symboid-name (s)
      (cond ((is-wheresym s) (wheresym-name s))
	    (t (symbol-name s))))

(defun symboid-get (s ind)
      (cond ((is-wheresym s)
	     (wheresym-get s ind))
	    (t
	     (get s ind))))

(defun symboid-put (s ind val)
     (cond ((is-wheresym s)
	    (setf (wheresym-get s ind) val))
	   (t
	    (setf (get s ind) val))))

