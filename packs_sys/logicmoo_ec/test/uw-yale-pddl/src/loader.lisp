
  
;;;; DEPENDS-ON

;;; The function DEPENDS-ON is used in subsequent files to state dependencies
;;; between files.  The current definition just loads the required files,
;;; assumming they match the pathname specified in *PDDL-DIRECTORY*.
;;; You should change that to match where you have stored the files.
;;; A more sophisticated DEPENDS-ON would only load it if it has not yet
;;; been loaded, and would search in different directories if needed.

(defun depends-on (&rest files)
  "The arguments are files that are required to run an application."
  (mapc #'load-pddl-file files))

(defvar *pddl-files*
  '("setindex" "types" "basics" "typecheck" "action"
    "parsers" "definers" "checker" "builtins"))

(defvar *syntax-checker-files*
  '("setindex" "types" "basics" "typecheck" "action"
    "parsers" "definers" "checker" "builtins"))

(defvar *solution-checker-files*
  '("symboid" "expdt" "index" "deduction" "unify" "checkutils" 
     "checksearch" "copy"))



(defparameter *pddl-directory*
  (make-pathname :name nil :type nil
		 :defaults
                 ;(or (and (boundp '*load-truename*)
                  ;        *load-truename*)
                 (truename "M:/prog/pddl/pddl/"))
                          ;;"/home/ipp/wettbewerb-aips98/pddl/"
                          ;;??? Maybe Change this
  "The location of the source files for this book.  If things don't work,
  change it to reflect the location of the files on your computer.")

(defparameter *pddl-source* 
  (make-pathname
     :name nil :type "lisp" ;;???  Maybe Change this
     :directory (append
		   (pathname-directory
		    *pddl-directory*)
		   '("src"))
     :defaults *pddl-directory*))


(defparameter *pddl-binary*
  (make-pathname
   :name nil
   :type (first (list #+LCL (first *load-binary-pathname-types*)
		      #+Lispworks system::*binary-file-type*
		      #+MCL "fasl"
		      #+Allegro excl:*fasl-default-type*
		      #+(or AKCL KCL) "o"
		      #+CMU "sparcf"
		      #+CLISP "fas"
		      "bin"))  ;;???  Maybe Change this
   :directory (pathname-directory (truename "M:/prog/pddl/bin/"))
            ;;(append (pathname-directory *pddl-source*) '("bin"))
   :defaults *pddl-directory*))

(defun pddl-pathname (name &optional (type :lisp))
  (make-pathname :name name 
		 :defaults (ecase type
			     ((:lisp :source) *pddl-source*)
			     ((:binary :bin) *pddl-binary*))))

(defun compile-all-pddl-files ()
  (mapc #'compile-pddl-file *pddl-files*))

(defun compile-syntax ()
  (mapc #'compile-pddl-file *syntax-checker-files*))

(defun compile-solution ()
  (mapc #'compile-pddl-file *solution-checker-files*))


(defun compile-pddl-file (name)
  (let ((path (pddl-pathname name :lisp)))
    (load path)
    (compile-file path :output-file (pddl-pathname name :binary))))

(defun load-syntax ()
  (mapc #'load-pddl-file *syntax-checker-files*))

;(defun load-syntax ()
;   (load  "/home/ipp/wettbewerb-aips98/pddl/bin/setindex.fasl")
;   (load  "/home/ipp/wettbewerb-aips98/pddl/bin/types.fasl")
;   (load   "/home/ipp/wettbewerb-aips98/pddl/bin/basics.fasl" )
;   (load      "/home/ipp/wettbewerb-aips98/pddl/bin/typecheck.fasl" )
;   (load      "/home/ipp/wettbewerb-aips98/pddl/bin/action.fasl" )
;   (load      "/home/ipp/wettbewerb-aips98/pddl/bin/parsers.fasl" )
;   (load      "/home/ipp/wettbewerb-aips98/pddl/bin/definers.fasl")
;   (load      "/home/ipp/wettbewerb-aips98/pddl/bin/checker.fasl" )
;   (load   "/home/ipp/wettbewerb-aips98/pddl/bin/builtins.fasl" ))


(defun load-pddl-file (file)
  "Load the binary file if it exists and is newer, else load the source."
  (let* ((src (pddl-pathname file :lisp))
	 (src-date (file-write-date src))
	 (bin (pddl-pathname file :binary))
	 (bin-date (file-write-date bin)))
    (load (if (and (probe-file bin) src-date bin-date (>= bin-date src-date))
	      bin
	    src))))



(defpackage "PDDL"
   #+(or :ansi-cl :cltl2) (:USE "COMMON-LISP" "CL-USER")
   #-(or :ansi-cl :cltl2) (:USE "LISP" "CL-USER")
   (:import-from "CL-USER" CL-USER::PDDL-READ-TABLE* CL-USER::GO-PDDL) 
   (:EXPORT "DEFINE" "PDDL-FILE-SYNCHECK" "^^" 
            "REQUIREMENT" "DOMAIN" "ADDENDUM" "PROBLEM" "SITUATION"
            "ACHIEVE" "CHANGE" "WHEN" "FORALL" "EXISTS" 
            "OBJECT" "NUMBER"
            "IN-CONTEXT" "CHOICE" "FORSOME" "FOREACH" "SERIES" "PARALLEL"
            "TAG" "CONSTRAINED"
            "TEST" "BOUNDED-INT" ))


(defparameter pddl-read-table* (copy-readtable nil))

(defun go-pddl ()
   (in-package "PDDL")
   (setq *readtable* pddl-read-table*))

