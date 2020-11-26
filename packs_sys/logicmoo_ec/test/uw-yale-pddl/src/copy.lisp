(defun copy-tab ()
   (list '()))

(defun thing-copy (x tab initializer filler)
   (let ((p (assoc x (car tab) :test #'eq)))
       (cond (p (cadr p))
	     (t
	      (let ((new (funcall initializer x)))
		 (setf (car tab)
		       (push (list x new) (car tab)))
		 (funcall filler new x tab)
		 new)))))
