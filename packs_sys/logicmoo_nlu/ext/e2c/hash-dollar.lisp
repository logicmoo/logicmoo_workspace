;;; FILE: case.lisp


;;; KM package is now the current package
(eval-when
 (:compile-toplevel :load-toplevel :execute)
 (defconstant *km-package* *package*))


;;; File: case.lisp
;;; Author: Peter Clark
;;; Purpose: Case-sensitive handling for KM

;;; ======================================================================
;;;			READING
;;; ======================================================================
;;; Thanks to Brian Mastenbrook for info on the usage of eval-when, which
;;; avoids pre-loading km.lisp before compiling!

;;; New version, thanks to Sunil Mishra (SRI)
;;; This version uses unwind-protect to ensure that the readtable-case gets reset,
;;; and cerror to allow resuming km from the entered debugger with a :cont.

;;; New version, extended to add a #t construct - thanks to Francis Leboutte
;;; The #t construct (dispatch macro-character)
;;; Francis Leboutte, 20Jul2005

#|
Reader macro documentation:
Example:

(km '#$(every Car has
              (wheel-count (4))
              (parts ((a Engine) (a Chassis)))))

(km '#$(a Car))

To get the parts of a Car instance, below the various ways to write
the call to km.
Notice:
 in this example, the current package is the "USER" package and the km symbol
 accessible in the "USER" package

1. without using the #$ construct:

(let* ((car-instance
        (first (km '(km::|the| km::|all-instances| km::|of| km::|Car|)))))
  (km `((km::|the| km::|parts| km::|of| ,USER::CAR-INSTANCE))))

2. with the #$ construct:

(let ((car-instance (first (km '#$(the all-instances of Car)))))
  (km `(#$the #$parts #$of ,car-instance)))

3. with the #$ construct, other way
Notice in the second call to km, car-instance must be package qualified
and in majuscules:

(let ((car-instance (first (km '#$(the all-instances of Car)))))
  (km `#$(the parts of ,USER::CAR-INSTANCE)))

4. with the #$ and #t constructs.
Just write the km requests as they would be written at the KM prompt
and prefix any lisp variables with #t. The case of letters of these
variables is unimportant:

(let ((car-instance (first (km '#$(the all-instances of Car)))))
  (km `#$(the parts of ,#tcar-instance)))


For another example of how to use the #t construct, see the
property-mult-property and property-div-property functions.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *t-readtable* (copy-readtable *readtable*))
  ;; standard CL mode:
; (setf (readtable-case *t-readtable*) :upcase)
  (defvar *t-package* nil))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun hash-t-reader (stream subchar arg)
    (declare (ignore subchar arg))
    (let (;; bind *package* to the package that was in effect outside the
          ;; form prefixed by #$
          (*package* *t-package*)
          (*readtable* *t-readtable*))
      (read stream t nil t))))

;;; (get-dispatch-macro-character #\# #\t)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\t #'hash-t-reader))


;;; UPDATED DEFINITIONS
;;; *******************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun case-sensitive-read-km (&optional stream (eof-err-p t) eof-val rec-p)
    ;; FLE 29Jul2005
    ;; bind *t-package* to the current package, to be used in the #t construct
    ;; It doesn't hurt if *package* is already bound to *km-package*
    ;; (which is :km or a "current" package, usually :user).
    ;; BTW, I think KM should always be packaged (:km package)
    (let ((*t-package* *package*)
          (*package* *km-package*))
      (case-sensitive-read stream eof-err-p eof-val rec-p))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun case-sensitive-read (&optional stream (eof-err-p t) eof-val rec-p)
    (let ((old-readtable-case (readtable-case *readtable*)))
      (loop
       (handler-case
           (unwind-protect
               (progn
                 (setf (readtable-case *readtable*) :preserve)
                 (return (read stream eof-err-p eof-val rec-p)))
             (setf (readtable-case *readtable*) old-readtable-case))
         (error
          (error)
          ;; FLE 25Jul2005: more understandable error message (typep and ~a)
          (cerror "Ignore error and return."
                  (if (typep error 'end-of-file)
                      "During case-sensitive-read, certainly a premature end-of-file:~%~a"
                    "During case-sensitive-read:~%~a")
                  error)))))))

;;; ======================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *km-readtable* (copy-readtable *readtable*)))

(eval-when
 (:compile-toplevel :load-toplevel :execute)
 (defun hash-dollar-reader (stream subchar arg)
   (declare (ignore subchar arg))
   (convert-to-cycl (case-sensitive-read-km stream t nil t))))

;; (case-sensitive-read-km *standard-input* t nil t)
(defun convert-to-cycl (s) (if (not (fboundp 'cyc::find-constant)) s
            (cyc::find-or-create-constant (string s))))

(eval-when
 (:compile-toplevel :load-toplevel :execute)
 (set-dispatch-macro-character #\# #\$ #'hash-dollar-reader))

;;; ======================================================================
;;;			WRITING
;;; ======================================================================

#|
This version of format *doesn't* put || around symbols, but *does* put
"" around strings. This is impossible to do with the normal format, as
|| and "" can only be suppressed in unison (via the *print-escape*
variable). There's no other way round that I can see besides the below.

> ([km-]format t "~a" (case-sensitive-read))
(The BIG big "car" 2)

produces:
  *case-sensitivity*  *print-case*	format ~a		km-format ~a		format ~s			km-format ~s
	t		:upcase		(The BIG big car 2)	(The BIG big "car" 2)   (|The| BIG |big| "car" 2)	(|The| BIG |big| "\"car\"" 2)
	t		:downcase	(the big big car 2)	(The BIG big "car" 2)
[	nil		:upcase		(THE BIG BIG car 2)	(THE BIG BIG "car" 2)]
[	nil		:downcase	(the big big car 2)	(the big big "car" 2)]

(defun test (x)
  (setq *print-case* :upcase)
  (km-format t "km-format: ~a~%" x)
  (format t "format:   ~a~%" x)
  (setq *print-case* :downcase)
  (km-format t "km-format: ~a~%" x)
  (format t "format:   ~a~%" x))
|#

(defun km-format (stream string &rest args)
  (let ( (old-print-case *print-case*) )
    (prog2
	(setq *print-case* :upcase)	; :upcase really means "case-sensitively"
	(apply #'format (cons stream
			      (cons string (mapcar #'add-quotes args))))
      (setq *print-case* old-print-case))))

;;; For prettiness, we normally remove || when printing. But, this has the side-effect of also
;;; removing quotes, so we must add those back in -- and also add back in || if the symbol
;;; contains special characters "() ,;:".
;;; (the "cat") -> (the "\"cat\"")

(defun add-quotes (obj)
  (cond ((null obj) nil)
	((aconsp obj) (cons (add-quotes (first obj)) (add-quotes (rest obj))))
	((listp obj) (mapcar #'add-quotes obj))
	((stringp obj) (format nil "~s" obj)) 		; (concat "\"" obj "\"") <- Insufficient for "a\"b"
	((and (symbolp obj)
	      (let ( (chars (explode (symbol-name obj))) )
		(or (intersection chars '(#\( #\) #\  #\, #\; #\: #\' #\"))
		    (not (set-difference chars '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))))	;  e.g. |1943|, the symbol
	 (concat "|" (symbol-name obj) "|"))
	((keywordp obj) (concat ":" (symbol-name obj)))		; better!
	(t obj)))

;;; ======================================================================
;;;		BETTER FORMATTING
;;; ======================================================================

;;; (write-km-vals '#$(:seq _Car2 |the Dog| (baz . bar) #,(the #'dog)))
;;; -> (:seq _Car2 #|"mike" "joe"|# |the Dog| (baz . bar) #,(the #'dog))
;;; (write-km-vals '#$(:seq _Car2 #|"mike" "joe"|# |the Dog| (foo baz . bar))) will give an error though
;;;	[(length '(a b . c)) generates an error - ignore this case for now].
(defun write-km-vals (vals &optional (stream t))
  (let ( (old-print-case *print-case*) )
    (prog2
	(setq *print-case* :upcase)	; :upcase really means "case-sensitively"
	(write-km-vals2 vals stream)
      (setq *print-case* old-print-case))))

(defun write-km-vals2 (vals &optional (stream t))
  (cond ((null vals) (format stream "~a" nil))
	((and (pairp vals)
	      (symbolp (first vals))
	      (assoc (first vals) *special-symbol-alist*))
	 (let ( (special-symbol-str (second (assoc (first vals) *special-symbol-alist*))) )
	   (format stream "~a" special-symbol-str)
	   (write-km-vals2 (second vals) stream)))
	((listp vals) (write-km-list vals stream))
	((stringp vals) (format stream "~s" vals))
	((keywordp vals) (format stream ":~a" vals))
	((and (symbolp vals)
	      (intersection (explode (symbol-name vals)) '(#\( #\) #\  #\, #\; #\:)))
	 (format stream "|~a|" vals))
	((anonymous-instancep vals)
	 (format stream "~a" vals)
	 (let ( (tags (remove-constraints (append (get-vals vals '|called| :situation *global-situation*)
						  (get-vals vals '|uniquely-called| :situation *global-situation*)))) )
	   (cond (tags (tag-write tags))
		 (t (let* ( (classes (immediate-classes vals))
			    (skolem-root (skolem-root (symbol-name vals)))
			    (name-class-str (cond ((starts-with skolem-root "_Proto") (subseq skolem-root 6 (length skolem-root)))
						  ((starts-with skolem-root "_Some") (subseq skolem-root 5 (length skolem-root)))
						  (t (butfirst-char skolem-root))))
			    (name-class (intern name-class-str *km-package*)) )
		      (cond ((or (>= (length classes) 2)
				 (neq name-class (first classes)))
			     (let ( (new-tag (concat-list (cons "a " (commaed-list (mapcar #'symbol-name classes) "&")))) )
			       (tag-write (list new-tag) stream)))))))))
	(t (format stream "~a" vals))))

(defun write-km-list (list &optional (stream t) (first-time-through t))
  (cond ((null list) (format stream ")"))
	(t (cond (first-time-through (format stream "("))
		 (t (format stream " ")))
	   (cond ((aconsp list) (write-km-vals2 (first list) stream)
		  (format stream " . ") (write-km-vals2 (rest list) stream) (format stream ")"))
		 (t (write-km-vals2 (first list) stream)
		    (write-km-list (rest list) stream nil))))))		; i.e. first-time-through = nil

(defun tag-write (tags &optional (stream t) (first-time-through t))
  (cond ((null tags) (format stream "|#"))
	(t (cond (first-time-through (format stream " #|"))
		 (t (format stream " ")))
	   (format stream "~s" (first tags))
	   (tag-write (rest tags) stream nil))))			; i.e. first-time-through = nil

;;; "_Car23" -> "_Car"
(defun skolem-root (string)
  (cond
   ((string= string ""))
   ((member (last-char string) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :test #'char=)
    (skolem-root (butlast-char string)))
   (t string)))

;;; ======================================================================

;;; "Tool" -> |Tool| (case-sensitivity on); [|TOOL| (case-sensitivity off)]
(defun string-to-frame (string)
  (cond ((string= string "") nil)
	(t (intern string *km-package*))))

;;; Inverse suffix must obey case-sensitive restrictions
(defparameter *inverse-suffix* "-of")
(defparameter *length-of-inverse-suffix* (length *inverse-suffix*))

#|
======================================================================
		UNQUOTING: KM's own mechanism
		=============================
This isn't very elegant, I'd rather use the traditional `, Lisp syntax, but
this will have to do**. Note the complication that #, always returns a
LIST of instances, so we have to be careful to splice them in appropriately.
Added #@ to do splicing. (a #@b) = (a . #,b)

However, we need to make it a reader macro so that KM will respond to
embedded #, which would otherwise be unprocessed, eg. a handler for ","
won't even reach the embedded unit in:
	KM> (Pete has (owns (`(a Car with (age ,(the Number))))))
	but a macro character will:
	KM> (Pete has (owns ('(a Car with (age #,(the Number))))))

** The mechanism needs to be vendor-independent, but the handling of `, is
vendor-specific. Allegro names these two symbols as excl:backquote and
excl:bq-comma; Harlequin preprocesses the expressions in the reader, so
that `(a b ,c) is pre-converted to (list 'a 'b c).

======================================================================

This *doesn't* require pairing with backquote `.
Usage:
KM> (:set (a Car) (a Car))
(_Car13 _Car14)

KM> '(:set (a Car) (a Car))
('(:set (a Car) (a Car)))

KM> '(:set (a Car) #,(a Car))
('(:set (a Car) (_Car16)))		<= note undesirable () around _Car16

KM> '(:set (a Car) . #,(a Car))		<= use . #, to slice item at end of list
('(:set (a Car) _Car17))
|#

;;; Thanks to Brian Mastenbrook for info on the usage of eval-when, which
;;; avoids pre-loading km.lisp before compiling!
(eval-when
 (:compile-toplevel :load-toplevel :execute)
 (defun hash-comma-reader (stream subchar arg)
   (declare (ignore subchar arg))
   (list 'unquote (case-sensitive-read-km stream t nil t))))

(eval-when
 (:compile-toplevel :load-toplevel :execute)
 (set-dispatch-macro-character #\# #\, #'hash-comma-reader))


