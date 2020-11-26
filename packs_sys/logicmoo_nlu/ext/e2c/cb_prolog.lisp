;;Saved into a file called common.lisp

;; (load "cynd/cb_prolog.lisp")

(define define-anonymous-function (arguments body) 
   (clet ((name (gensym "LAMBDA-"))) (eval `(define ,name ,arguments (ret (progn ,@body)))) (ret (symbol-function name))))

(defmacro cl-lambda (arguments &body body)  (ret (define-anonymous-function arguments body)))
(define string-concat (&rest list)(ret (apply #'cconcatenate (cons "" (mapcar #'(lambda (x)(ret (fif (stringp x) x (coerce x 'string) ))) list)))))


(DEFINE COERCE (VALUE RESULT-TYPE)
  (CLET ((LEN VALUE)
         (VTYPE (TYPE-OF VALUE))
         (CLTYPE RESULT-TYPE))
        (PWHEN (EQUAL RESULT-TYPE VTYPE)
               (RET VALUE))
        (FUNLESS (CAND (CONSP CLTYPE)
                      (CSETQ LEN (SECOND CLTYPE))
                      (CSETQ CLTYPE (CAR CLTYPE)))
          (FIF
           (CONSP VALUE)
           (CSETQ LEN (LENGTH VALUE))))
        (PCASE CLTYPE ('T (RET VALUE))
               ('SEQUENCE
                (FIF
                 (SEQUENCEP VALUE)
                 (RET (COPY-SEQ VALUE))
                 (CSETQ VALUE (WRITE-TO-STRING VALUE)))
                (CSETQ CLTYPE (MAKE-VECTOR LEN))
                (CDO ((IDX 0 (+ 1 IDX)))
                     ((= IDX LEN)
                      (RET CLTYPE ))
                     (SET-AREF CLTYPE IDX (ELT VALUE IDX))))
               ('CHARACTER
                (PCOND
                 ((CHARACTERP VALUE)
                  (RET VALUE))
                 ((NUMBERP VALUE)
                  (RET (CODE-CHAR VALUE)))
                 ((STRINGP VALUE)
                  (RET (CHAR VALUE 0)))
                 (T (RET (CHAR (COERCE VALUE 'STRING ) 0)))))
               ('NUMBER
                (PCOND
                 ((NUMBERP VALUE)
                  (RET VALUE))
                 ((CHARACTERP VALUE)
                  (RET (CHAR-CODE VALUE)))
                 ((STRINGP VALUE)
                  (RET (STRING-TO-NUMBER VALUE)))
                 (T (RET (STRING-TO-NUMBER (WRITE-TO-STRING VALUE))))))
               ('INTEGER (RET (ROUND (COERCE VALUE 'NUMBER))))
               ('FIXNUM (RET (ROUND (COERCE VALUE 'NUMBER))))
               ('FLOAT (RET (FLOAT (COERCE VALUE 'NUMBER))))
               ('REAL (RET (FLOAT (COERCE VALUE 'NUMBER))))
               ('FLONUM (RET (FLOAT (COERCE VALUE 'NUMBER))))
               ('STRING
                (PCOND
                 ((STRINGP VALUE)
                  (RET VALUE))
                 ((CHARACTERP VALUE)
                  (RET (MAKE-STRING 1 VALUE)))
                 ((SEQUENCEP VALUE)
                  (CSETQ CLTYPE (MAKE-STRING LEN))
                  (CDO ((IDX 0 (+ 1 IDX)))
                       ((= IDX LEN)
                        (RET CLTYPE ))
                       (SET-AREF CLTYPE IDX (COERCE (ELT VALUE IDX) 'CHARACTER))))
                 (T (RET (WRITE-TO-STRING VALUE)))))
               ('LIST
                (PCOND
                 ((LISTP VALUE)
                  (RET LIST))
                 ((SEQUENCEP VALUE)
                  (CSETQ CLTYPE NIL)
                  (CDO ((IDX LEN (- IDX 1)))
                       ((= IDX 0)
                        (RET CLTYPE ))
                       (CSETQ CLTYPE (CONS (ELT VALUE IDX) CLTYPE))))
                 (T (CSETQ CLTYPE NIL)
                    (CSETQ VALUE (WRITE-TO-STRING VALUE))
                    (CDO ((IDX LEN (- IDX 1)))
                         ((= IDX 0)
                          (RET CLTYPE ))
                         (CSETQ CLTYPE (CONS (ELT VALUE IDX) CLTYPE))))))
               ('CONS
                (PCOND
                 ((LISTP VALUE)
                  (RET LIST))
                 ((SEQUENCEP VALUE)
                  (CSETQ CLTYPE NIL)
                  (CDO ((IDX LEN (- IDX 1)))
                       ((= IDX 0)
                        (RET CLTYPE ))
                       (CSETQ CLTYPE (CONS (ELT VALUE IDX) CLTYPE))))
                 (T (CSETQ CLTYPE NIL)
                    (CSETQ VALUE (WRITE-TO-STRING VALUE))
                    (CDO ((IDX LEN (- IDX 1)))
                         ((= IDX 0)
                          (RET CLTYPE ))
                         (CSETQ CLTYPE (CONS (ELT VALUE IDX) CLTYPE))))))
               ('KEYPAIR
                (PCOND
                 ((ATOM VALUE)
                  (RET LIST VALUE))
                 (T (RET (COERCE VALUE 'CONS)))))
               ('ALIST (CSETQ CLTYPE (CSETQ CLTYPE NIL))
                (FIF
                 (SEQUENCEP VALUE) T (CSETQ VALUE (COERCE VALUE 'SEQUENCE)))
                (CDO ((IDX 0 (+ 1 IDX)))
                     ((= IDX LEN)
                      (RET CLTYPE))
                     (CSETQ RESULT-TYPE (COERCE (ELT VALUE IDX) 'CONS))
                     (CSETQ CLTYPE (ACONS (CAR RESULT-TYPE)
                                          (CDR RESULT-TYPE) CLTYPE)))
                (RET CLTYPE))
               ('HASH-TABLE
                (FIF
                 (HASH-TABLE-P VALUE)
                 (RET VALUE))
                (CSETQ CLTYPE (MAKE-HASH-TABLE LEN))
                (FIF
                 (SEQUENCEP VALUE) T (CSETQ VALUE (COERCE VALUE 'SEQUENCE)))
                (CDO ((IDX 0 (+ 1 IDX)))
                     ((= IDX LEN)
                      (RET CLTYPE))
                     (PRINT (LIST 'COERCE VALUE RESULT-TYPE CLTYPE LEN (ELT VALUE IDX)))
                     (CSETQ RESULT-TYPE (COERCE (ELT VALUE IDX) 'KEYPAIR))
                     (SETHASH (CAR RESULT-TYPE) CLTYPE (CDR RESULT-TYPE))))
               (OTHERWISE (RET VALUE)))
        (THROW :COERCE (LIST VALUE RESULT-TYPE)))
  (RET VALUE))

'(
  
  (DEFCONSTANT *DTP-DELAY* 'DELAY)
  
  (DEFINE DELAY-P (SUBLISP::OBJECT)
    (RET (CAND (SUBLISP::_STRUCTURES-BAG-P SUBLISP::OBJECT)
               (EQ (SUBLISP::_STRUCTURE-SLOT SUBLISP::OBJECT 1) *DTP-DELAY*))))
  
  (DEFINE DELAY-VALUE (SUBLISP::OBJECT)
    (CHECK-TYPE SUBLISP::OBJECT DELAY-P)
    (RET (SUBLISP::_STRUCTURE-SLOT SUBLISP::OBJECT 2)))
  
  (DEFINE DELAY-FUNCTION (SUBLISP::OBJECT)
    (CHECK-TYPE SUBLISP::OBJECT DELAY-P)
    (RET (SUBLISP::_STRUCTURE-SLOT SUBLISP::OBJECT 3)))
  
  (DEFINE _CSETF-DELAY-VALUE (SUBLISP::OBJECT SUBLISP::VALUE)
    (CHECK-TYPE SUBLISP::OBJECT DELAY-P)
    (RET (SUBLISP::_SET-STRUCTURE-SLOT SUBLISP::OBJECT 2 SUBLISP::VALUE)))
  
  (DEFINE _CSETF-DELAY-FUNCTION (SUBLISP::OBJECT SUBLISP::VALUE)
    (CHECK-TYPE SUBLISP::OBJECT DELAY-P)
    (RET (SUBLISP::_SET-STRUCTURE-SLOT SUBLISP::OBJECT 3 SUBLISP::VALUE)))
  (SUBLISP::_DEF-CSETF 'DELAY-VALUE '_CSETF-DELAY-VALUE)
  (SUBLISP::_DEF-CSETF 'DELAY-FUNCTION '_CSETF-DELAY-FUNCTION)
  
  (DEFINE MAKE-DELAY (&OPTIONAL SUBLISP::ARGLIST)
    (CLET ((SUBLISP::NEW (SUBLISP::_NEW-STRUCTURE *DTP-STRUCTURES-BAG* 2)))
          (SUBLISP::_CLEAR-SUB-STRUCTURE SUBLISP::NEW 2 *DTP-DELAY*)
          (CLET ((#:NEXT SUBLISP::ARGLIST))
                (LOOP
                  (FIF
                   #:NEXT
                   
                   (CLET ((#:CURRENT-ARG (CAR #:NEXT))
                          (#:CURRENT-VALUE (CADR #:NEXT)))
                         (PCASE #:CURRENT-ARG
                                (:VALUE (_CSETF-DELAY-VALUE SUBLISP::NEW #:CURRENT-VALUE))
                                (:FUNCTION (_CSETF-DELAY-FUNCTION SUBLISP::NEW #:CURRENT-VALUE))
                                (OTHERWISE (ERROR (FORMAT NIL "INVALID SLOT ~S FOR CONSTRUCTION FUNCTION" #:CURRENT-ARG))))
                         (CSETQ #:NEXT (CDDR #:NEXT)))
                   (RET SUBLISP::NEW))))
          (RET SUBLISP::NEW)))
  (IDENTITY 'DELAY))





(DEFMACRO CATCH (TAG &BODY BODY)
  (RET `(APPLY #'VALUES
               (CLET ((*THROWN* :UNTHROWN)
                      (*RESULT* :UNEVALED))(TERPRI)
                     (CCATCH ,TAG *THROWN* (CSETQ *RESULT* (MULTIPLE-VALUE-LIST (PROGN ,@BODY))))
                     (PCOND
                      ((EQUAL *RESULT* :UNEVALED)
                       (LIST *THROWN*))
                      (T *RESULT*))))))



(DEFMACRO CL-LOOP (&REST EXPS)
  ;;"SUPPORTS BOTH ANSI AND SIMPLE LOOP.  WARNING: NOT EVERY LOOP KEYWORD IS SUPPORTED."
  (FORMAT T "~%~S~%" `(LOOP ,@EXPS))(FORCE-OUTPUT)
  (PUNLESS (MEMBER-IF #'SYMBOLP EXPS) (RET `(LOOP ,@EXPS))) 
  (PCASE (CAR EXPS) 
      ((UNTIL WHILE) (RET EXPS))
      (FOR (BREAK "CL-LOOP"))
      (REPEAT (BREAK "CL-LOOP"))
      (OTHERWISE (RET `(LOOP ,@EXPS)))))

;;(DEFMACRO CL-LOOP (&REST FORMS) (RET `(LOOP ,@FORMS)))

(DEFINE BREAK-STRING-AT (STRING BREAK-CHAR) 
  (FUNLESS (STRING= STRING "") 
    (CLET ((CHARAT (POSITION BREAK-CHAR STRING)))(RET (FIF CHARAT (CONS (SUBSEQ STRING 0 CHARAT) 
      (BREAK-STRING-AT (SUBSEQ STRING (+ 1 CHARAT)) BREAK-CHAR)) (LIST STRING))))))

(DEFINE BINDINGS-FOR (PATTERN)
  (CLET ((COLLECT ()))
   (DOLIST (VAR (VARIABLES-IN PATTERN))
	 (SETQ COLLECT (APPEND COLLECT (LIST (CDR (ASSOC VAR *BINDINGS*))))))
    (RET COLLECT)))

(DEFINE COMPILE-RULES (RULES VAR)
 ;; "A RULES IS OF THE FORM (PAT CODE) WHERE CODE MAY REFERENCE VARS IN PAT."
 (CLET ((COLLECT ()))
  (DOLIST (PATTERN+CONSEQUENT RULES)
    (CSETQ COLLECT (APPEND COLLECT (LIST (COMPILE-RULE (FIRST PATTERN+CONSEQUENT)(SECOND PATTERN+CONSEQUENT) VAR)))))
  (RET (CREDUCE #'MERGE-CODE COLLECT))))

(DEFINE EXPLODE (STRING) 
   (FUNLESS (STRING= STRING "")  
     (CLET ((RESULT ())(LEN (LENGTH STRING)))
      (CDO ((NDX (- LEN 1) (- NDX 1))(RESULT (CONS (CHAR STRING NDX) RESULT)(CONS (CHAR STRING NDX) RESULT))) ((= NDX 0)(RET RESULT))))))

(DEFMACRO CONCATENATE (CLTYPE &BODY ARGS)
  (RET `(COERCE (CCONCATENATE ,@ARGS) ,CLTYPE)))

(DEFINE MAP-SEQUENCES (FUNCTION SEQUENCES)
  (RET (FIF
        (MEMBER () SEQUENCES) ()
        (CONS (APPLY FUNCTION (MAPCAR #'CAR SEQUENCES))
              (MAP-SEQUENCES FUNCTION (MAPCAR #'CDR SEQUENCES))))))

(DEFINE MAP (RESULT-TYPE FUNCTION &BODY SEQUENCES)
  (RET (FIF
        RESULT-TYPE (COERCE (MAP-SEQUENCES FUNCTION SEQUENCES) RESULT-TYPE)
        (PROGN (MAP-SEQUENCES FUNCTION SEQUENCES) NIL))))



(DEFMACRO DESTRUCTURING-BIND (ARGS DATUM &BODY BODY)(RET `(CDESTRUCTURING-BIND ,ARGS ,DATUM ,@BODY)))
(DEFMACRO MULTIPLE-VALUE-BIND (ARGS DATUM &BODY BODY)(RET `(CMULTIPLE-VALUE-BIND ,ARGS ,DATUM ,@BODY)))
;;(DEFMACRO CMULTIPLE-VALUE-LIST (VALUE &REST IGNORE)(RET `(MULTIPLE-VALUE-LIST ,VALUE)))

(DEFINE CONCAT (&REST LIST)
  (RET (APPLY #'CCONCATENATE (CONS "" (MAPCAR #'(LAMBDA (X)(RET (FIF (STRINGP X) X (COERCE X 'STRING) ))) LIST)))))

(DEFMACRO CL-DEFSTRUCT (NAME &REST REST)
  (CLET ((SLOTS (MAPCAR #'(LAMBDA(X)(RET (FIF
                                          (ATOM X) X (CAR X)))) REST)))
        (RET `(DEFSTRUCT (,NAME) ,@SLOTS))))

(DEFINE CL-MAKE-STRING (&REST REST)
  (RET (MAKE-STRING (FIND 'NUMBERP REST #'FUNCALL)
                    (FIND #'CHARACTERP REST 'FUNCALL))))

(DEFINE CL-MAKE-HASH-TABLE (&REST REST)
  (CLET ((SIZE (FIND 'NUMBERP REST #'FUNCALL)))
        (RET (MAKE-HASH-TABLE
              (FIF
               SIZE SIZE 64)
              (FIND #'FUNCTIONP REST 'FUNCALL)))))

(DEFINE MAKE-ARRAY (&REST REST)
  (CLET ((SIZE (FIND 'NUMBERP REST #'FUNCALL)))
        (RET (MAKE-VECTOR
              (FIF
               SIZE SIZE 64)
              (FIND #'FUNCTIONP REST 'FUNCALL)))))

(DEFMACRO CASE (TEST &REST FORMS) (RET `(PCASE ,TEST ,(MAPCAR #'(LAMBDA (X) (RET `(,(CAR X)(TRACE-PROGN ,(CDR X))) )) FORMS))))
(DEFMACRO CASE (TEST &REST FORMS) (RET `(PCASE ,TEST ,@FORMS)))

(DEFMACRO COND (&REST FORMS) (RET (CONS 'PCOND (MAPCAR #'(LAMBDA (X) (RET `(,(CAR X)(TRACE-PROGN ,(CDR X))) )) FORMS) )))
(DEFMACRO COND (&REST FORMS) (RET `(PCOND ,@FORMS)))

(DEFMACRO OR (&rest forms) 
 (ret (fif forms (fif (cdr forms) 
        `(PCOND ((TRACE-LISP ,(car forms))) ((OR ,@(cdr forms))))
        `(TRACE-LISP ,(car forms))))))

(DEFMACRO AND (&rest forms) 
 (ret (fif forms (fif (cdr forms) 
        `(PWHEN (TRACE-LISP ,(car forms)) (AND ,@(cdr forms)))
        `(TRACE-LISP ,(car forms))))))


(DEFMACRO CL-READ (&REST FORMS)             
            (terpri)
            (RET (CONS 'READ FORMS)))


(DEFMACRO COND (&REST FORMS) (RET (CONS 'PCOND FORMS)))
(DEFMACRO CASE (&REST FORMS) (RET (CONS 'PCASE FORMS)))

(DEFVAR PLEVEL ())
(CSETQ PLEVEL ())

(DEFMACRO TRACE-FORMAT (STRING &REST REST) 
   (IF *FIRST-ERROR* 
     (RET `(PROGN 
          (FORMAT T "~%;;~A;; " (reverse PLEVEL))
          (FORMAT T ,STRING ,@REST)
          (FORCE-OUTPUT))
          (RET NIL))))

(DEFMACRO TRACE-DEFUN (NAME ARGS &rest FORMS)  
 (ret
   `(progn 
     (clet ((*FIRST-ERROR* T))
      (TRACE-FORMAT "FUNCALL: ~S" (CONS ,NAME (MAPCAR #'(LAMBDA (X) (RET (list 'QUOTE X))) ',ARGS))))
      (clet ((PLEVEL (cons ,NAME PLEVEL))) (TRACE-PROGN ,@FORMS)))))

(DEFMACRO TRACE-PROGN (&REST FORMS) 
  (RET (CONS 'PROGN (MAPCAR #'(LAMBDA (X) (RET `(TRACE-LISP ,X ))) FORMS) )))

(DEFMACRO ERROR-HANDLER-FOR (CODE)
  (RET `(FUNCTION (LAMBDA () 
                    (WITH-ERROR-HANDLER 
                        #'(LAMBDA () (TRACE-FORMAT "ERROR ~S DURRING HANDLER~%" *ERROR-MESSAGE*))
                      (PROGN 
                          (FUNLESS *FIRST-ERROR* (CSETQ *FIRST-ERROR* (list *ERROR-MESSAGE* ',CODE)))
                          (CSETQ *LAST-ERROR* (list *ERROR-MESSAGE* ',CODE))
                          (CINC *ERROR-NUMBER*)                      
                          (TRACE-FORMAT "ERROR ~S DURRING: ~S ~%" *ERROR-MESSAGE* ',CODE)
                          (PWHEN (> *ERROR-NUMBER* 0) (RET (BREAK "ERROR-HANDLER-FOR")))(RET NIL)))))))

(DEFVAR *ERROR-NUMBER* 0)
(CSETQ *ERROR-NUMBER* 1)

(DEFMACRO TRACE-LISP (CODE)
  (RET 
   `(progn 
      (WITH-ERROR-HANDLER 
        #'(LAMBDA () (TRACE-FORMAT "   ERROR ~S DURRING DEBUG~%" *ERROR-MESSAGE*))
          (TRACE-FORMAT "DEBUGGING: ~S => " ',CODE))
      (WITH-ERROR-HANDLER
        (ERROR-HANDLER-FOR ',CODE)
        (CLET ((RESULT ,CODE)) (TRACE-FORMAT " ~S~%" RESULT) RESULT)))))

(DEFVAR *FIRST-ERROR* NIL)
(DEFVAR *LAST-ERROR* NIL)
(CSETQ *FIRST-ERROR* T)
(CSETQ *LAST-ERROR* T)


(DEFMACRO PROG1 (BODY1 &BODY BODY) (RET `(CLET ((PROG1RES ,BODY1)) ,@BODY PROG1RES)))
(DEFMACRO PROG2 (BODY1 BODY2 &BODY BODY) (RET `(CLET ((PROG1RES ,BODY1)(PROG2RES ,BODY2)) ,@BODY PROG2RES)))
(DEFMACRO PROG3 (BODY1 BODY2 BODY3 &BODY BODY) (RET `(CLET ((PROG1RES ,BODY1)(PROG2RES ,BODY2)(PROG3RES ,BODY3)) ,@BODY PROG3RES)))
(DEFMACRO MEMQ (ITEM MY-LIST) `(MEMBER ,ITEM ,MY-LIST :TEST #'EQ))

(DEFVAR INTERNAL-TIME-UNITS-PER-SECOND *INTERNAL-TIME-UNITS-PER-SECOND*)

(DEFMACRO PUSH (ITEM PLACE) (RET `(PROGN (CPUSH ,ITEM ,PLACE) ,PLACE)))
(DEFMACRO PUSH (ITEM PLACE) (RET (LIST 'CPUSH ITEM PLACE)))
(defmacro POP (place) (ret `(CLET ((f1rst (CAR ,place))) (CPOP ,place) f1rst)))

(DEFMACRO GETLKEY (KEY &OPTIONAL DEFAULT) 
  (RET 
   `(CSETQ ,KEY 
     (PCOND 
        ((CAR (CDR (MEMBER-IF #'(LAMBDA (X)(RET (CAND (SYMBOLP X)(SYMBOLP ,KEY)(EQUAL (SYMBOL-NAME X) (SYMBOL-NAME ,KEY))))) LKEYS))))
        (T ,DEFAULT)))))

(DEFINE CL-MEMBER (ITEM LIST &REST LKEYS)
  (TRACE-DEFUN 'CL-MEMBER (ITEM LIST '&REST LKEYS)())
  (CLET (TEST KEY TEST-NOT)
        (GETLKEY TEST)(GETLKEY KEY)(GETLKEY TEST-NOT)     
        (FUNLESS TEST (CSETQ TEST #'EQL))
        (FUNLESS KEY (CSETQ KEY #'IDENTITY))
        (PWHEN TEST-NOT (CSETQ TEST #'(LAMBDA (X Y)(RET (CNOT (FUNCALL TEST-NOT X Y))))))
        (RET (MEMBER ITEM LIST TEST KEY))))

(DEFINE CL-INTERSECTION (LIST-1 LIST-2 &REST LKEYS)
  (TRACE-DEFUN 'CL-INTERSECTION (LIST-1 LIST-2 '&REST LKEYS)())
  (CLET (TEST KEY TEST-NOT)
        (GETLKEY TEST)(GETLKEY KEY)(GETLKEY TEST-NOT)     
        (FUNLESS KEY (CSETQ KEY #'IDENTITY))
        (FUNLESS TEST (CSETQ TEST #'EQL))
        (PWHEN TEST-NOT (CSETQ TEST #'(LAMBDA (X Y)(RET (CNOT (FUNCALL TEST-NOT X Y))))))
        (RET (INTERSECTION LIST-1 LIST-2 TEST KEY ))))

(DEFINE CL-REMOVE (ITEM LIST &REST LKEYS )
  (TRACE-DEFUN 'CL-REMOVE (ITEM LIST '&REST LKEYS)())
  (CLET (TEST FROM-END TEST-NOT START END COUNT KEY)
        (GETLKEY TEST)(GETLKEY KEY)(GETLKEY TEST-NOT)(GETLKEY FROM-END)(GETLKEY START)(GETLKEY END)(GETLKEY COUNT)
        (FUNLESS KEY (CSETQ KEY #'IDENTITY))
        (FUNLESS TEST (CSETQ TEST #'EQL))
        (PWHEN TEST-NOT (CSETQ TEST #'(LAMBDA (X Y)(RET (CNOT (FUNCALL TEST-NOT X Y))))))
        (FUNLESS START (CSETQ START 0))
        (PWHEN FROM-END (RET (REVERSE (REMOVE ITEM (REVERSE LIST) TEST KEY START END COUNT))))
        (RET (REMOVE ITEM LIST TEST KEY START END COUNT))))

(DEFINE CL-REMOVE-DUPLICATES (LIST &REST LKEYS)
  (TRACE-DEFUN 'CL-REMOVE-DUPLICATES (LIST '&REST LKEYS) ())
  (CLET (TEST FROM-END TEST-NOT START END COUNT KEY)
        (GETLKEY TEST)(GETLKEY KEY)(GETLKEY TEST-NOT)(GETLKEY FROM-END)(GETLKEY START)(GETLKEY END)(GETLKEY COUNT)
        (FUNLESS KEY (CSETQ KEY #'IDENTITY))
        (FUNLESS TEST (CSETQ TEST #'EQL))
        (PWHEN TEST-NOT (CSETQ TEST #'(LAMBDA (X Y)(RET (CNOT (FUNCALL TEST-NOT X Y))))))
        (FUNLESS START (CSETQ START 0))
        (PWHEN FROM-END (RET (REVERSE (REMOVE-DUPLICATES (REVERSE LIST) TEST KEY START END))))
        (RET (REMOVE-DUPLICATES LIST TEST KEY START END))))

(DEFINE CL-DELETE-DUPLICATES (LIST &REST LKEYS)    
  (TRACE-DEFUN 'CL-DELETE-DUPLICATES (LIST '&REST LKEYS)())
  (CLET (TEST FROM-END TEST-NOT START END COUNT KEY)
        (GETLKEY TEST)(GETLKEY KEY)(GETLKEY TEST-NOT)(GETLKEY FROM-END)(GETLKEY START)(GETLKEY END)(GETLKEY COUNT)
        (FUNLESS KEY (CSETQ KEY #'IDENTITY))
        (FUNLESS TEST (CSETQ TEST #'EQL))
        (PWHEN TEST-NOT (CSETQ TEST #'(LAMBDA (X Y)(RET (CNOT (FUNCALL TEST-NOT X Y))))))
        (FUNLESS START (CSETQ START 0))
        (PWHEN FROM-END (RET (REVERSE (DELETE-DUPLICATES (REVERSE LIST) TEST KEY START END))))
        (RET (DELETE-DUPLICATES LIST TEST KEY START END))))

(DEFINE CL-SUBSETP (LIST LIST2 &REST LKEYS)
  (TRACE-DEFUN 'CL-SUBSETP (LIST LIST2 '&REST LKEYS)())
  (CLET (TEST KEY TEST-NOT)
        (GETLKEY TEST)(GETLKEY KEY)(GETLKEY TEST-NOT)     
        (FUNLESS KEY (CSETQ KEY #'IDENTITY))
        (FUNLESS TEST (CSETQ TEST #'EQL))
        (PWHEN TEST-NOT (CSETQ TEST #'(LAMBDA (X Y)(RET (CNOT (FUNCALL TEST-NOT X Y))))))
        (RET (SUBSETP LIST LIST2 TEST KEY))))

(DEFMACRO IF (COND TRUE &OPTIONAL FALSE)
    (RET `(FIF ,COND ,TRUE ,FALSE)))


;;(DEFMACRO HANDLER-CASE (FORM &REST CASES) (PRINT (LIST 'HANDLER-CASE FORM CASES)) (RET `,FORM))
(DEFMACRO HANDLER-CASE (FORM &REST CASES) (RET FORM))
;;(DEFMACRO AND (&REST REST) (RET `(TRACE-LISP ,(CONS 'CAND REST))))
;;(DEFMACRO LET (&REST REST) (RET `(TRACE-LISP ,(CONS 'CLET REST))))
;;(DEFMACRO SETF (&REST REST) (RET `(TRACE-LISP ,(CONS 'CSETF REST))))
;;(DEFMACRO SETQ (&REST REST) (RET `(TRACE-LISP ,(CONS 'CSETQ REST))))
;;(DEFMACRO UNLESS (&REST REST) (RET `(TRACE-LISP ,(CONS 'FUNLESS REST))))
;;(DEFMACRO UNWIND-PROTECT (&REST REST) (RET `(TRACE-LISP ,(CONS 'CUNWIND-PROTECT REST))))

;;(DEFINE CL-GET (SYM PROP);; (PRINT `(GET ,SYM ,PROP ,(GET SYM PROP)))
;;                        (RET `(GET ,SYM ,PROP)))
;;(DEFINE CL-SET (SYM PROP VAL) (PRINT `(SET ,SYM ,PROP ,VAL))(RET `(SET ,SYM ,PROP ,VAL)))
;;(DEFINE CL-GET (SYM PROP) (RET `(GET ,SYM ,PROP )))
;;(DEFINE CL-SET (SYM PROP VAL) (RET `(SET ,SYM ,PROP ,VAL)))


;;(DEFMACRO UNLESS (&REST REST) (RET `(RET (PROGN (PRINT ',FORM) ,FORM))))
;;(DEFMACRO SUBLISP-INITVAR (VAR VALUE) (RET `(FUNLESS ,VAR (CSETQ ,VAR ,VALUE))))
(DEFMACRO SUBLISP-INITVAR (VAR VALUE) (RET `(FUNLESS ,VAR (CSETQ ,VAR ,VALUE))))

(DEFINE CL-STRING-DOWNCASE (STR)
  (RET (PCOND
        ((SYMBOLP STR)
         (STRING-DOWNCASE (SYMBOL-NAME STR)))
        (T (STRING-DOWNCASE STR)))))

#|
(DEFVAR STREAM *STANDARD-INPUT*) 
(DEFVAR EOF-VAL :EOF) 
(DEFVAR REC-P T) 
(DEFVAR EOF-ERR-P NIL) 
(DEFMACRO USER-WARNING (STRING &REST ARGS)
    (RET `(FORMAT T ,STRING ,@ARGS)))
(DEFMACRO USER-ERROR (STRING &REST ARGS)
    (RET `(FORMAT T ,STRING ,@ARGS)))
|#



(DEFINE CL-EQUAL (&REST REST)
  (TRACE-FORMAT "~S~%"  `(EQUAL ,@REST))
  (RET (EQUAL (CAR REST) (CAR (CDR REST)))))

(DEFINE MY-CONCAT (&REST REST) (RET (APPLY #'CONCAT REST)))
(DEFINE CONCAT (&REST REST) (RET (APPLY #'CCONCATENATE (CONS "" (FLATTEN REST)))))


(PRINT "LOADED COMMON-LISP.LISP!")
(define KMP () 
 (CSETF (READTABLE-CASE *READTABLE*) :UPCASE)
  (load "common_lisp.lisp")
  (load "kmp.subl")
  (load "common_lisp.lisp")
 ;;(CSETF (READTABLE-CASE *READTABLE*) :PRESERVE)
  )

#|
 CREATE-INSTANCE 
 ISA 
 ALL-INSTANCES 
 COMMENT 
 LOAD-KB 
 FLATTEN 
 ASSOC-EQUAL 
 ORDERED-SET-DIFFERENCE 
 ORDERED-INTERSECTION 
 QUOTIFY 
 PERMUTE 
 TRIM-WHITESPACE 
 FIRST-CHAR 
 LAST-CHAR 
 ENDS-WITH 
 STARTS-WITH 
 STRING-TO-NUMBER 
|#




;; Save the original defmacro: should actually be (macro-function 'defmacro)
;;(cpushnew :COMMON-LISP *features*)

#-COMMON-LISP
(progn
  (defmacro defun (name pattern &body body) (ret `(define ,name ,pattern (ret (progn ,@body)))))
  (defun myprint (stuff) (prin1 stuff)(princ " ")(force-output)stuff)
  (myprint "this is not CL!")
  (defvar *original-defmacro* (symbol-function 'defmacro))
  (defmacro defmacro-cl (name pattern &body body) (ret `(defmacro ,name ,pattern (ret (progn ,@body)))))
  
  
  (defmacro prog1 (body1 &body body) (ret `(let ((prog1res ,body1)) ,@body prog1res)))
  
  ;; transliterations
  (defmacro let (&body body) (ret `( clet ,@body)))
  (defmacro let* (&body body) (ret `( clet ,@body)))
  (defmacro dotimes (&body body) (ret `(cdotimes ,@body)))
  (defmacro case (&body body) (ret `( pcase ,@body)))
  (defmacro if (&body body) (ret `(fif ,@body)))
  (defmacro do (&body body) (ret `( cdo ,@body)))
  (defmacro not (&body body) (ret `(cnot ,@body)))
  (defmacro or (&body body) (ret `(cor ,@body)))
  (defmacro cond (&body body) (ret `( pcond ,@body)))
  (defmacro and (&body body) (ret `(cand ,@body)))
  (defmacro unless (&body body) (ret `(funless ,@body)))
  (defmacro when (&body body) (ret `(pwhen ,@body)))
  (defmacro setq (&body body) (ret `( csetq ,@body)))
  (defmacro setf (&body body) (ret `(csetf ,@body)))
  (defmacro pushnew (item place) (ret `(progn (cpushnew ,item ,place) ,place)))
  (defmacro push (&body body) (ret `(cpush ,@body)))
  (defmacro pop (place) 
    (ret `(let ((f1rst (elt ,place 0))) (CPOP) f1rst)))
  (define make-array (size &optional :initial-element init ) (ret (make-vector size  init)))

  (defmacro concatenate (type &body args) (ret `(coerce type (cconcatenate ,@args type))))
  
  ;;(defmacro until (test &body body)"Repeatedly evaluate BODY until TEST is true."(ret `(do ()(,test) ,@body)))
  
  ;; dont know if this is correct
  (defmacro return (&body body) (ret `(ret ,@body)))
  (defmacro return-from (tag value) (ret `(ret ,value)))
  
  (defmacro svref (array idx) (ret `(aref ,array ,idx)))
  ;;(defmacro incf (arg1 &body body) (ret `(fif (null body) (cincf arg1) (progn (cincf ,@body) ,@body)))
  (defmacro incf (&body body) (ret `(cinc ,@body)))
  (defmacro decf (&body body) (ret `(cdec ,@body)))
  
  (defmacro unwind-protect (protected-form &body body) (ret `(cunwind-protect ,protected-form ,@body)))
  (defmacro destructuring-bind (pattern datum &body body) (ret `(cdestructuring-bind ,pattern ,datum  ,@body)))
  (defmacro multiple-value-bind (pattern datum &body body) (ret `(cmultiple-value-bind  ,pattern ,datum  ,@body)))
  
  (defmacro catch (tag &body body)
    (ret 
     `(apply #'values  
        (let ((*thrown* :UNTHROWN) (*result* :UNEVALED))
          ;;(print (list 'eval (cons 'catch (cons ',tag  ',body))))(terpri)
          (ccatch ,tag *thrown* (setq *result* (multiple-value-list (progn ,@body))))
          (cond
           ((equal *result* :UNEVALED) (list *thrown*))
           (t *result*))))))
  
  
  (define map-sequences (function sequences)
    (ret (fif (member () sequences) () (cons (apply function (mapcar #'car sequences)) (map-sequences function (mapcar #'cdr sequences))))))
  
  (define map (result-type function &body sequences)
    (let ((result (map-sequences function sequences))) 
      (ret (fif result-type (coerce result result-type) nil))))
  
  ;;are hashtables supposed ot be coercable back and forth from alists? 
  (define coerce (value result-type)
    (let ((len value)(type result-type))
      (unless (cand (consp type) (setq len (second type)) (setq type (car type)) ) 
        (if (consp value) (setq len (length value))))
      ;;     (print (list 'coerce value result-type type len))
      (case type
        ('t (ret value))
        ('sequence
         (if (sequencep value) (ret (copy-seq value)) (setq value (write-to-string value)))
         (setq type (make-vector len))
         (do ((idx 0 (+ 1 idx))) ((= idx len) (ret  type )) (set-aref type idx (elt value idx))))
        ('character
         (cond
          ((characterp value) (ret value))
          ((numberp value) (ret (code-char value)))
          ((stringp value) (ret (char value 0)))
          (t (ret (char (coerce value 'string ) 0)))))
        ('number
         (cond
          ((numberp value) (ret value))
          ((characterp value) (ret (char-code value)))
          ((stringp value) (ret (string-to-number value)))
          ;; not like CL
          (t (ret (string-to-number (write-to-string value))))))
        ('integer
         (ret (round (coerce value 'number))))
        ('fixnum
         (ret (round (coerce value 'number))))
        ('float
         (ret (float (coerce value 'number))))
        ('real
         (ret (float (coerce value 'number))))
        ('flonum
         (ret (float (coerce value 'number))))
        ('string 
         (cond
          ((stringp value) (ret value))
          ((characterp value) (ret (make-string 1 value)))
          ((sequencep value) (setq type (make-string len))
           (do ((idx 0 (+ 1 idx))) ((= idx len) (ret type )) (set-aref type idx (coerce (elt value idx) 'character))))
          (t (ret (write-to-string value)))))
        ('list 
         (cond
          ((listp value) (ret list))
          ((sequencep value)
           (setq type nil)
           (do ((idx len (- idx 1))) ((= idx 0) (ret  type )) (setq type (cons (elt value idx) type))))
          (t 
           (setq type nil)
           (setq value (write-to-string value))
           (do ((idx len (- idx 1))) ((= idx 0) (ret  type )) (setq type (cons (elt value idx) type))))))
        ('cons 
         (cond
          ((listp value) (ret list))
          ((sequencep value)
           (setq type nil)
           (do ((idx len (- idx 1))) ((= idx 0) (ret  type )) (setq type (cons (elt value idx) type))))
          (t 
           (setq type nil)
           (setq value (write-to-string value))
           (do ((idx len (- idx 1))) ((= idx 0) (ret  type )) (setq type (cons (elt value idx) type))))))
        ;; not finished
        ('keypair
         (cond
          ((atom value) (ret list value))
          (t (ret (coerce value 'cons)))))
        ;; not finished
        ('alist
         ;;(if (hash-table-p value) (ret value))
         (setq type (setq type nil))
         (if (sequencep value) t (setq value (coerce value 'sequence)))
         (do ((idx 0 (+ 1 idx))) ((= idx len) (ret type)) 
           (setq result-type (coerce (elt value idx) 'cons))
           (setq type (acons (car result-type) (cdr result-type) type)))
         (ret type))
        ;; not finished
        ('hash-table
         (if (hash-table-p value) (ret value))
         (setq type (make-hash-table len))
         (if (sequencep value) t (setq value (coerce value 'sequence)))
         (do ((idx 0 (+ 1 idx))) ((= idx len) (ret type)) 
           (print (list 'coerce value result-type type len (elt value idx)))
           (setq result-type (coerce (elt value idx) 'keypair))
           (sethash (car result-type) type (cdr result-type))))
        ;; not like CL
        (otherwise (ret value)))
      (throw :coerce (list value result-type))))
  
  ;; not finished
  (defmacro defstruct-cl (name &body clslots)
    (let ((options nil)(slots nil)(body nil)(constuctor nil))
      (ret `(defstruct (`,name ,options) ,body ,slots))))
  
  (myprint "this is RCyc!")
  ;;(load "cb_smartworld.lisp")
  (defmacro defstub (feat symb)
    (ret `(defun ,symb (&rest body) 
            (let ((closure (cons ',feat (cons ',symb body))))
              (prin1 (prolog-to-string closure))(terpri)(force-output)
              (apply #'values body))))
    ))

#+COMMON-LISP
(progn
  (defun myprint (stuff) (prin1 stuff)(princ " ")(force-output)stuff)
  (myprint "this is NOT RCyc!")
  
  
  ;;(defmacro ret (&body body) `(return ,@body))
  (defmacro RET (value) `,value)
  (defun SET-CHAR (str idx ch) `(setf (aref ,str ,idx) ,ch))
  (defmacro DEFINE (name pattern &body body) `(defun ,name ,pattern ,@body))
  (defmacro THROW-UNEVALUATABLE-ON-ERROR (&body body) `(progn ,@body))
  (defvar *WHITESPACE-CHARS* '(#\Space #\Tab #\Return #\Newline))
  (defvar *HTML-STREAM* *TERMINAL-IO*)
  
  
  (defmacro defstub (feat symb)
    `(defun ,symb (&rest body) 
       (let ((closure (cons ',feat (cons ',symb body))))
         (prin1 (prolog-to-string closure))(terpri)(force-output)
         (apply #'values body))))
  
  
  (defstub :SL2C ask-template)
  (defstub :SL2C assertion-el-ist-formula )
  (defstub :SL2C assertion-p) 
  (defstub :SL2C constant-internal-id )
  (defstub :SL2C constant-name )
  (defstub :SL2C constant-name-spec-p )
  (defstub :SL2C constant-p) 
  (defstub :SL2C correct-variable  )
  (defstub :SL2C cyc-query )
  (defstub :SL2C cyc-var? )
  (defstub :SL2C define-html-handler)
  (defstub :SL2C dotted-list-p )
  (defstub :SL2C el-var-name-without-prefix )
  (defstub :SL2C el-variable-p )
  (defstub :SL2C fi-unassert )
  (defstub :SL2C find-assertions-cycl)
  (defstub :SL2C find-or-create-constant)
  (defstub :SL2C fort-p )
  (defstub :SL2C hl-explanation-of-why-not-wff)
  (defstub :SL2C hl-variable-p )
  (defstub :SL2C ke-assert-now)
  (defstub :SL2C make-el-var )
  (defstub :SL2C mt? )
  (defstub :SL2C nart-el-formula )
  (defstub :SL2C nart-p )
  (defstub :SL2C predicate? )
  (defstub :SL2C prolog-list-to-cons) 
  (defstub :SL2C substring )
  (defstub :SL2C weak-string-equal )
  )

(myprint "Done loading the common.lisp")
(force-output)


;; Version 3
;; boot.lsp
;;
(TRACE-LISP (DEFINE MLG3 NIL (trace-defun 'MLG3 NIL (RET (TRACE-PROGN (BANNER)   (MYLOOP (READ_PROMPT)))))))
(TRACE-LISP (DEFINE READ_PROMPT NIL (trace-defun 'READ_PROMPT NIL (RET (TRACE-PROGN (TERPRI) (FORMAT T "| ?- ")  (READ_CODE_TAIL))))))
(TRACE-LISP
 (DEFINE BANNER NIL
  (trace-defun 'BANNER NIL (RET (TRACE-PROGN (CDOTIMES (I 2) (TERPRI)) (FORMAT T "Micro_Log3 pour vous servir~%") (CDOTIMES (I 2) (TERPRI)))))))
(TRACE-LISP (DEFINE L NIL (trace-defun 'L NIL (RET (TRACE-PROGN (FORMAT T "Back to MicroLog3 top-level~%") (MYLOOP (READ_PROMPT)))))))
;; clecteur.lsp
;;
(TRACE-LISP (DEFVAR *LVARLOC NIL))
(TRACE-LISP (DEFVAR *LVARGLOB NIL))
(TRACE-LISP (SET-MACRO-CHARACTER #\% (GET-MACRO-CHARACTER #\;)))
(TRACE-LISP (DEFINE RCH NIL (trace-defun 'RCH NIL (RET (CDO ((CH (READ-CHAR) (READ-CHAR))) ((CHAR/= CH #\Newline) CH))))))
(TRACE-LISP (DEFINE RCHNSEP NIL (trace-defun 'RCHNSEP NIL (RET (CDO ((CH (RCH) (RCH))) ((CHAR/= CH #\Space) CH))))))
(TRACE-LISP (DEFINE SPECIAL-PLP (CH) (trace-defun 'SPECIAL-PLP (CH) (RET (CHAR= CH '#\_)))))
(TRACE-LISP (DEFINE ALPHANUM (CH) (trace-defun 'ALPHANUM (CH) (RET (OR (ALPHANUMERICP CH) (SPECIAL-PLP CH))))))
(TRACE-LISP (DEFINE VALDIGIT (CH) (trace-defun 'VALDIGIT (CH) (RET (DIGIT-CHAR-P CH)))))
(TRACE-LISP
 (DEFINE READ_NUMBER (CH)
  (trace-defun 'READ_NUMBER (CH) (RET (CDO ((V (VALDIGIT CH) (+ (* V 10) (VALDIGIT (READ-CHAR))))) ((CNOT (DIGIT-CHAR-P (PEEK-CHAR))) V))))))
(TRACE-LISP (DEFINE IMPLODE (LCH) (trace-defun 'IMPLODE (LCH) (RET (INTERN (MAP 'STRING #'IDENTITY LCH))))))
(TRACE-LISP
 (DEFINE READ_ATOM (CH)
  (trace-defun 'READ_ATOM (CH) (RET (CDO ((LCH (LIST CH) (PUSH (READ-CHAR) LCH))) ((CNOT (ALPHANUM (PEEK-CHAR))) (IMPLODE (REVERSE LCH))))))))
(TRACE-LISP
 (DEFINE READ_AT (CH)
  (trace-defun 'READ_AT (CH)
   (RET (CDO ((LCH (LIST CH) (PUSH (READ-CHAR) LCH))) ((CHAR= (PEEK-CHAR) #\') (READ-CHAR) (IMPLODE (REVERSE LCH))))))))
(TRACE-LISP
 (DEFINE READ_STRING (CH)
  (trace-defun 'REfAD_STRING (CH)
   (RET (CDO ((LCH (LIST (CHAR-INT CH)) (PUSH (CHAR-INT (READ-CHAR)) LCH))) ((CHAR= (PEEK-CHAR) #\") (READ-CHAR) (DO_L (REVERSE LCH))))))))
(TRACE-LISP (DEFINE READ_VAR (CH N) (trace-defun 'READ_VAR (CH N) (RET (STATUS (READ_ATOM CH) N)))))
(TRACE-LISP
 (DEFINE STATUS (NOM N)
  (trace-defun 'STATUS (NOM N)
   (RET
    (TRACE-PROGN
     (IF (= N 1) (FUNLESS (CL-MEMBER NOM *LVARGLOB) (PUSHNEW NOM *LVARLOC))
      (TRACE-PROGN (IF (CL-MEMBER NOM *LVARLOC) (CSETQ *LVARLOC (DELETE NOM *LVARLOC))) (PUSHNEW NOM *LVARGLOB)))
     NOM)))))
(TRACE-LISP
 (DEFINE READ_SIMPLE (CH N)
  (trace-defun 'READ_SIMPLE (CH N)
   (RET
    (COND ((OR (UPPER-CASE-P CH) (SPECIAL-PLP CH)) (READ_VAR CH N)) ((DIGIT-CHAR-P CH) (READ_NUMBER CH)) ((CHAR= CH #\") (READ_STRING (READ-CHAR)))
     ((CHAR= CH #\') (READ_AT (READ-CHAR))) (T (READ_ATOM CH)))))))
(TRACE-LISP
 (DEFINE READ_FCT (CH N)
  (trace-defun 'READ_FCT (CH N)
   (RET
    (CLET ((FCT (READ_SIMPLE CH N)) (C (RCHNSEP)))
     (IF (CHAR= C #\() (CLET ((LA (READ_ARGS (RCHNSEP) (1+ N)))) (CONS (LIST FCT (LENGTH LA)) LA)) (TRACE-PROGN (UNREAD-CHAR C) FCT)))))))
(TRACE-LISP
 (DEFINE READ_ARGS (CH N)
  (trace-defun 'READ_ARGS (CH N)
   (RET (CLET ((ARG (READ_TERM CH N))) (IF (CHAR= (RCHNSEP) #\,) (CONS ARG (READ_ARGS (RCHNSEP) N)) (LIST ARG)))))))
(TRACE-LISP
 (DEFINE READ_LIST (CH N)
  (trace-defun 'READ_LIST (CH N)
   (RET
    (IF (CHAR= CH #\]) NIL
     (CLET ((TE (READ_TERM CH N)))
      (CASE (RCHNSEP) (#\, (LIST '(|.| 2) TE (READ_LIST (RCHNSEP) N))) (#\| (PROG1 (LIST '(|.| 2) TE (READ_TERM (RCHNSEP) N)) (RCHNSEP)))
       (#\] (LIST '(|.| 2) TE NIL)))))))))
(TRACE-LISP (DEFINE READ_TERM (CH N) (trace-defun 'READ_TERM (CH N) (RET (IF (CHAR= CH #\[) (READ_LIST (RCHNSEP) (1+ N)) (READ_FCT CH N))))))
(TRACE-LISP
 (DEFINE READ_TAIL (CH)
  (trace-defun 'READ_TAIL (CH) (RET (CLET ((TETE (READ_PRED CH))) (IF (CHAR= (RCHNSEP) #\.) (LIST TETE) (CONS TETE (READ_TAIL (RCHNSEP)))))))))
(TRACE-LISP
 (DEFINE READ_CLAUSE (CH)
  (trace-defun 'READ_CLAUSE (CH)
   (RET (CLET ((TETE (READ_PRED CH))) (IF (CHAR= (RCHNSEP) #\.) (LIST TETE) (TRACE-PROGN (READ-CHAR) (CONS TETE (READ_TAIL (RCHNSEP))))))))))
(TRACE-LISP
 (DEFINE C (L)
  (trace-defun 'C (L)
   (RET
    (IF (ATOM L) (IF (CL-MEMBER L *LVARLOC) (CONS 'L (POSITION L *LVARLOC)) (IF (CL-MEMBER L *LVARGLOB) (CONS 'G (POSITION L *LVARGLOB)) L))
     (IF (EQ (CAR L) '!) (LIST '! (LENGTH *LVARLOC)) (CONS (C (CAR L)) (C (CDR L)))))))))
;; Version 3
;; lecteur.lsp
;;
(TRACE-LISP
 (DEFINE READ_CODE_CL NIL
  (trace-defun 'READ_CODE_CL NIL
   (RET
    (CLET ((*LVARLOC NIL) (*LVARGLOB NIL))
     (CLET ((X (READ_CLAUSE (RCHNSEP)))) (MAJ_LOCGLOB (CAR X) (CAR (LAST X))) (CONS (CONS (LENGTH *LVARLOC) (LENGTH *LVARGLOB)) (C X))))))))
(TRACE-LISP
 (DEFINE READ_CODE_TAIL NIL
  (trace-defun 'READ_CODE_TAIL NIL
   (RET
    (TRACE-PROGN (CSETQ *LVARLOC NIL *LVARGLOB NIL)
     (CLET ((X (READ_TAIL (RCHNSEP)))) (CONS (CONS (LENGTH *LVARLOC) (LENGTH *LVARGLOB)) (APPEND (C X) (LIST '(|true|))))))))))
(TRACE-LISP
 (DEFINE READ_PRED (CH)
  (trace-defun 'READ_PRED (CH)
   (RET
    (CLET ((NOM (READ_ATOM CH)) (C (RCHNSEP)))
     (IF (CHAR= C #\() (CONS NOM (READ_ARGS (RCHNSEP) (IF (MEMBER NOM '(|dif| |freeze|)) 2 1))) (TRACE-PROGN (UNREAD-CHAR C) (LIST NOM))))))))
(TRACE-LISP (DEFINE UNSAFE? (X H Q) (trace-defun 'UNSAFE? (X H Q) (RET (AND (CL-MEMBER X Q) (CNOT (CL-MEMBER X H)))))))
(TRACE-LISP
 (DEFINE MAJ_LOCGLOB (H Q)
  (trace-defun 'MAJ_LOCGLOB (H Q)
   (RET
    (MAPC #'(LAMBDA (X) (trace-defun '#:G10466 (X) (RET (WHEN (UNSAFE? X H Q) (CSETQ *LVARLOC (DELETE X *LVARLOC)) (PUSH X *LVARGLOB)))))
     *LVARLOC)))))
;; Version 3
;; blocs.lsp
;;
;; I. Registres
;;
(TRACE-LISP (DEFCONSTANT BOTTOMFR 1))
(TRACE-LISP (DEFCONSTANT BOTTOMG 3000))
(TRACE-LISP (DEFCONSTANT BOTTOML 6000))
(TRACE-LISP (DEFCONSTANT BOTTOMTR 10000))
(TRACE-LISP (DEFCONSTANT A 12000))
(TRACE-LISP (DEFVAR MEM (MAKE-ARRAY 12050 :INITIAL-ELEMENT 0)))
(TRACE-LISP (DEFVAR FR))
(TRACE-LISP (DEFVAR TR))
(TRACE-LISP (DEFVAR L))
(TRACE-LISP (DEFVAR G))
(TRACE-LISP (DEFVAR CP))
(TRACE-LISP (DEFVAR CL))
(TRACE-LISP (DEFVAR CUT_PT))
(TRACE-LISP (DEFVAR FRCP))
(TRACE-LISP (DEFVAR BL))
(TRACE-LISP (DEFVAR BG))
(TRACE-LISP (DEFVAR PC))
(TRACE-LISP (DEFVAR PCE))
(TRACE-LISP (DEFVAR PCG))
(TRACE-LISP (DEFVAR DUBOULOT))
(TRACE-LISP (DEFMACRO VSET (V I X) (RET `(CSETF (AREF ,V ,I) ,X))))
;; II. Local Stack
;;
;; WAM notion of environment [CL CP G Cut E]
;;
(TRACE-LISP (DEFMACRO CL (B) (RET `(AREF MEM ,B))))
(TRACE-LISP (DEFMACRO CP (B) (RET `(AREF MEM (1+ ,B)))))
(TRACE-LISP (DEFMACRO G (B) (RET `(AREF MEM (+ ,B 2)))))
(TRACE-LISP (DEFMACRO CUT (B) (RET `(AREF MEM (+ ,B 3)))))
(TRACE-LISP (DEFMACRO E (B) (RET `(+ ,B 4))))
(TRACE-LISP (DEFMACRO PUSH_CONT NIL (RET `(TRACE-PROGN (VSET MEM L CL) (VSET MEM (1+ L) CP)))))
(TRACE-LISP
 (DEFMACRO PUSH_E (N)
  (RET
   `(CLET ((TOP (+ L 4 ,N))) (IF (>= TOP BOTTOMTR) (THROW 'DEBORD (PRINT "Local Stack Overflow"))) (VSET MEM (+ L 3) CUT_PT)
     (CDOTIMES (I ,N TOP) (VSET MEM (CDEC TOP) (CONS 'LIBRE BOTTOMG)))))))
(TRACE-LISP (DEFMACRO MAJ_L (NL) (RET `(INCF L (+ 4 ,NL)))))
;;choice-point : [a1 .. an A FR BCP BCL BG BL BP TR]
;;
(TRACE-LISP (DEFMACRO TR (B) (RET `(AREF MEM (1- ,B)))))
(TRACE-LISP (DEFMACRO BP (B) (RET `(AREF MEM (- ,B 2)))))
(TRACE-LISP (DEFMACRO BL (B) (RET `(AREF MEM (- ,B 3)))))
(TRACE-LISP (DEFMACRO BG (B) (RET `(AREF MEM (- ,B 4)))))
(TRACE-LISP (DEFMACRO BCL (B) (RET `(AREF MEM (- ,B 5)))))
(TRACE-LISP (DEFMACRO BCP (B) (RET `(AREF MEM (- ,B 6)))))
(TRACE-LISP (DEFMACRO FR (B) (RET `(AREF MEM (- ,B 7)))))
(TRACE-LISP (DEFMACRO A (B) (RET `(AREF MEM (- ,B 8)))))
(TRACE-LISP
 (DEFINE SAVE_ARGS NIL
  (trace-defun 'SAVE_ARGS NIL (RET (CDOTIMES (I (AREF MEM A) (VSET MEM (INCF L I) I)) (VSET MEM (+ L I) (AREF MEM (+ A I 1))))))))
(TRACE-LISP
 (DEFINE PUSH_CHOIX NIL
  (trace-defun 'PUSH_CHOIX NIL
   (RET
    (TRACE-PROGN (SAVE_ARGS) (VSET MEM (INCF L) FR) (VSET MEM (INCF L) CP) (VSET MEM (INCF L) CL) (VSET MEM (INCF L) G) (VSET MEM (INCF L) BL)
     (VSET MEM (INCF L 2) TR) (CSETQ BL (INCF L) BG G))))))
(TRACE-LISP (DEFINE PUSH_BPR (RESTE) (trace-defun 'PUSH_BPR (RESTE) (RET (VSET MEM (- BL 2) RESTE)))))
(TRACE-LISP (DEFMACRO SIZE_C (B) (RET `(+ 8 (A ,B)))))
(TRACE-LISP
 (DEFINE POP_CHOIX NIL (trace-defun 'POP_CHOIX NIL (RET (CSETQ L (- BL (SIZE_C BL)) BL (BL BL) BG (IF (ZEROP BL) BOTTOMG (BG BL)))))))
;; III. Global Stack
;;
(TRACE-LISP
 (DEFMACRO PUSH_G (N)
  (RET
   `(CLET ((TOP (+ G ,N))) (IF (>= TOP BOTTOML) (THROW 'DEBORD (PRINT "Global Stack Overflow")))
     (CDOTIMES (I ,N (VSET MEM (+ L 2) G)) (VSET MEM (CDEC TOP) (CONS 'LIBRE BOTTOMG)))))))
(TRACE-LISP (DEFMACRO MAJ_G (N) (RET `(INCF G ,N))))
;;IV. Trail
;;
(TRACE-LISP (DEFMACRO FGBLOCK (X) (RET `(CDR (AREF MEM ,X)))))
(TRACE-LISP
 (DEFINE PUSHTRAIL (X)
  (trace-defun 'PUSHTRAIL (X) (RET (TRACE-PROGN (IF (>= TR A) (THROW 'DEBORD (PRINT "Trail Overflow"))) (VSET MEM TR X) (INCF TR))))))
(TRACE-LISP
 (DEFINE POPTRAIL (TOP)
  (trace-defun 'POPTRAIL (TOP)
   (RET
    (CDO NIL ((= TR TOP))
     (CLET ((X (AREF MEM (CDEC TR)))) (IF (NUMBERP X) (VSET MEM X (CONS 'LIBRE BOTTOMG)) (VSET MEM (CAR X) (CONS 'LIBRE (CDR X))))))))))
;; V. Frozen Goals Stack
;;
(TRACE-LISP (DEFMACRO FGVAR (X) (RET `(AREF MEM ,X))))
(TRACE-LISP (DEFMACRO FGTAIL (X) (RET `(AREF MEM (1+ ,X)))))
(TRACE-LISP (DEFMACRO FGGOAL (X) (RET `(AREF MEM (+ 2 ,X)))))
(TRACE-LISP (DEFMACRO FGENV (X) (RET `(AREF MEM (+ 3 ,X)))))
(TRACE-LISP (DEFMACRO FROZEN? (X) (RET `(< (FGBLOCK ,X) BOTTOMG))))
(TRACE-LISP
 (DEFMACRO PUSH_FG (V B EB R)
  (RET
   `(IF (>= (+ FR 3) BOTTOMG) (THROW 'DEBORD (PRINT "Frozen Goals Stack Overflow"))
     (TRACE-PROGN (VSET MEM FR ,V) (VSET MEM (INCF FR) ,R) (VSET MEM (INCF FR) ,B) (VSET MEM (INCF FR) ,EB) (INCF FR))))))
;; cutili.lsp
;;
(TRACE-LISP (DEFMACRO NLOC (C) (RET `(CAAR ,C))))
(TRACE-LISP (DEFMACRO NGLOB (C) (RET `(CDAR ,C))))
(TRACE-LISP (DEFMACRO HEAD (C) (RET `(CADR ,C))))
(TRACE-LISP (DEFMACRO TAIL (C) (RET `(CDDR ,C))))
(TRACE-LISP (DEFMACRO PRED (G) (RET `(CAR ,G))))
(TRACE-LISP (DEFMACRO LARGS (G) (RET `(CDR ,G))))
(TRACE-LISP (DEFMACRO FUNCTOR (DES) (RET `(CAR ,DES))))
(TRACE-LISP (DEFMACRO CL-ARITY (DES) (RET `(CADR ,DES))))
(TRACE-LISP (DEFMACRO DES (TE) (RET `(CAR ,TE))))
(TRACE-LISP (DEFMACRO VAR? (V) (RET `(AND (CONSP ,V) (NUMBERP (CDR ,V))))))
(TRACE-LISP (DEFMACRO LIST? (X) (RET `(EQ (FUNCTOR (DES ,X)) '|.|))))
(TRACE-LISP (DEFMACRO USER? (G) (RET `(GET (PRED ,G) 'DEF))))
(TRACE-LISP (DEFMACRO BUILTIN? (G) (RET `(GET (PRED ,G) 'EVALUABLE))))
(TRACE-LISP (DEFMACRO DEF_OF (G) (RET `(GET (PRED ,G) (IF (LARGS ,G) (NATURE (CAR (ULTIMATE (CAR (LARGS ,G)) PCE PCG))) 'DEF)))))
(TRACE-LISP
 (DEFINE NATURE (TE)
  (trace-defun 'NATURE (TE) (RET (COND ((VAR? TE) 'DEF) ((NULL TE) 'EMPTY) ((ATOM TE) 'ATOM) ((LIST? TE) 'LIST) (T 'FONCT))))))
(TRACE-LISP (DEFINE ADD_CL (PRED C IND) (trace-defun 'ADD_CL (PRED C IND) (RET (CSETF (GET PRED IND) (APPEND (GET PRED IND) (LIST C)))))))
(TRACE-LISP
 (SET-MACRO-CHARACTER #\$
  #'(LAMBDA (STREAM CHAR)
     (trace-defun '#:G10467 (STREAM CHAR)
      (RET
       (TRACE-PROGN
        (CLET ((*STANDARD-INPUT* STREAM) (C (READ_CODE_CL))) (ADD_CL (PRED (HEAD C)) C 'DEF)
         (IF (LARGS (HEAD C))
          (CLET ((B (NATURE (CAR (LARGS (HEAD C))))))
           (IF (EQ B 'DEF) (MAPC #'(LAMBDA (X) (trace-defun '#:G10468 (X) (RET (ADD_CL (PRED (HEAD C)) C X)))) '(ATOM EMPTY LIST FONCT))
            (ADD_CL (PRED (HEAD C)) C B)))))
        (VALUES)))))))
(TRACE-LISP
 (DEFINE ANSWER NIL
  (trace-defun 'ANSWER NIL
   (RET
    (TRACE-PROGN (PRINTVAR)
     (IF (ZEROP BL) (CSETQ DUBOULOT NIL) (IF (AND (PRINC "More : ") (CL-MEMBER (CL-READ) '(O Y))) (BACKTRACK) (CSETQ DUBOULOT NIL))))))))
(TRACE-LISP
 (DEFINE PRINTVAR NIL
  (trace-defun 'PRINTVAR NIL
   (RET
    (IF (AND (NULL *LVARLOC) (NULL *LVARGLOB)) (FORMAT T "Yes ~%")
     (CLET ((NL -1) (NG -1))
      (MAPC
       #'(LAMBDA (X)
          (trace-defun '#:G10469 (X) (RET (TRACE-PROGN (FORMAT T "~A = " X) (WRITE1 (ULT (CONS 'L (INCF NL)) (E BOTTOML))) (TERPRI)))))
       *LVARLOC)
      (MAPC
       #'(LAMBDA (X) (trace-defun '#:G10470 (X) (RET (TRACE-PROGN (FORMAT T "~A = " X) (WRITE1 (ULT (CONS 'G (INCF NG)) BOTTOMG)) (TERPRI)))))
       *LVARGLOB)))))))
;; Version 3
;; unify.lsp
;;
(TRACE-LISP
 (DEFMACRO BIND (X SQ E XT)
  (RET `(TRACE-PROGN (IF (OR (AND (> ,X BOTTOML) (< ,X BL)) (< ,X BG)) (PUSHTRAIL ,XT)) (RPLACA (AREF MEM ,X) ,SQ) (RPLACD (AREF MEM ,X) ,E)))))
(TRACE-LISP
 (DEFINE BINDTE (X SQ E)
  (trace-defun 'BINDTE (X SQ E) (RET (IF (FROZEN? X) (CLET ((Y (FGBLOCK X))) (PUSH Y FRCP) (BIND X SQ E (CONS X Y))) (BIND X SQ E X))))))
;;(defun bindf0 (x b eb r)
;;   (pushtrail (cons x (fgblock x)))
;;   (rplacd (svref Mem x) FR)
;;   (push_fg b eb r))
(TRACE-LISP
 (DEFINE BINDFG (X B EB R)
  (trace-defun 'BINDFG (X B EB R) (RET (TRACE-PROGN (BIND X 'LIBRE FR (IF (FROZEN? X) (CONS X R) X)) (PUSH_FG X B EB R))))))
(TRACE-LISP
 (DEFINE UNIFY_WITH (LARGS EL EG)
  (trace-defun 'UNIFY_WITH (LARGS EL EG)
   (RET
    (CATCH 'IMPOSSIBLE (CDOTIMES (I (AREF MEM A)) (UNIF (CLET ((TE (AREF MEM (+ A 1 I)))) (VAL (CAR TE) (CDR TE))) (ULTIMATE (POP LARGS) EL EG))))))))
;; cunify.lsp
;;
(TRACE-LISP (DEFMACRO ADR (V E) (RET `(+ (CDR ,V) ,E))))
(TRACE-LISP (DEFMACRO VALUE (V E) (RET `(AREF MEM (ADR ,V ,E)))))
(TRACE-LISP
 (DEFINE ULT (V E)
  (trace-defun 'ULT (V E)
   (RET (CLET ((TE (VALUE V E))) (COND ((EQ (CAR TE) 'LIBRE) (CONS V E)) ((VAR? (CAR TE)) (ULT (CAR TE) (CDR TE))) (TE)))))))
(TRACE-LISP (DEFINE VAL (X E) (trace-defun 'VAL (X E) (RET (IF (VAR? X) (ULT X E) (CONS X E))))))
(TRACE-LISP
 (DEFINE ULTIMATE (X EL EG) (trace-defun 'ULTIMATE (X EL EG) (RET (IF (VAR? X) (IF (EQ (CAR X) 'L) (ULT X EL) (ULT X EG)) (CONS X EG))))))
(TRACE-LISP (DEFMACRO BINDV (X EX Y EY) (RET `(CLET ((AX (ADR ,X ,EX)) (AY (ADR ,Y ,EY))) (IF (< AX AY) (BINDTE AY ,X ,EX) (BINDTE AX ,Y ,EY))))))
(TRACE-LISP
 (DEFINE UNIF (T1 T2)
  (trace-defun 'UNIF (T1 T2)
   (RET
    (CLET ((X (CAR T1)) (EX (CDR T1)) (Y (CAR T2)) (EY (CDR T2)))
     (COND ((VAR? Y) (IF (VAR? X) (IF (= (ADR X EX) (ADR Y EY)) T (BINDV Y EY X EX)) (BINDTE (ADR Y EY) X EX))) ((VAR? X) (BINDTE (ADR X EX) Y EY))
      ((AND (ATOM X) (ATOM Y)) (IF (EQL X Y) T (THROW 'IMPOSSIBLE 'FAIL))) ((OR (ATOM X) (ATOM Y)) (THROW 'IMPOSSIBLE 'FAIL))
      ((CLET ((DX (POP X)) (DY (POP Y)))
        (IF (AND (EQ (FUNCTOR DX) (FUNCTOR DY)) (= (CL-ARITY DX) (CL-ARITY DY))) (CDO NIL ((NULL X)) (UNIF (VAL (POP X) EX) (VAL (POP Y) EY)))
         (THROW 'IMPOSSIBLE 'FAIL))))))))))
;; Version 3
;; resol.lsp
;;
(TRACE-LISP
 (DEFINE FORWARD NIL
  (trace-defun 'FORWARD NIL
   (RET
    (CDO NIL ((NULL DUBOULOT) (FORMAT T "no More~%"))
     (COND ((AND (NULL CP) (NULL FRCP)) (ANSWER))
      ((LOAD_PC)
       (COND ((USER? PC) (CLET ((D (DEF_OF PC))) (IF D (PR2 D) (BACKTRACK))))
        ((BUILTIN? PC) (IF (EQ (APPLY (CAR PC) (CDR PC)) 'FAIL) (BACKTRACK) (CONT_EVAL))) ((BACKTRACK))))))))))
(TRACE-LISP
 (DEFINE LOAD_A (LARGS EL EG)
  (trace-defun 'LOAD_A (LARGS EL EG) (RET (CDOTIMES (I (LENGTH LARGS) (VSET MEM A I)) (VSET MEM (+ A I 1) (ULTIMATE (POP LARGS) EL EG)))))))
(TRACE-LISP
 (DEFINE LOAD_PC NIL
  (trace-defun 'LOAD_PC NIL
   (RET
    (TRACE-PROGN (IF FRCP (CLET ((X NIL)) (CDO NIL ((NULL FRCP)) (CSETQ X (ADD_FG (POP FRCP) X))) (CDO NIL ((NULL X)) (CREATE_BLOCK (ABS (POP X))))))
     (CSETQ PC (POP CP) PCE (E CL) PCG (G CL) CUT_PT BL))))))
;;  (if dbg (dbg PC) t))
(TRACE-LISP (DEFINE OTHER_FG (B R) (trace-defun 'OTHER_FG (B R) (RET (IF (< (FGTAIL B) BOTTOMG) (ADD_FG (FGTAIL B) R) R)))))
(TRACE-LISP
 (DEFINE ADD_FG (B R)
  (trace-defun 'ADD_FG (B R)
   (RET
    (CLET ((B1 (IF (NUMBERP (FGGOAL B)) (FGGOAL B) B)))
     (IF (EQ (PRED (FGGOAL B1)) '|dif|) (INSERT (- B1) (OTHER_FG B R))
      (CLET ((V (AREF MEM (FGVAR B1))) (TE (VAL (CAR V) (CDR V))))
       (IF (VAR? (CAR TE)) (CLET ((Y (ADR (CAR TE) (CDR TE)))) (BINDFG Y B1 NIL (FGBLOCK Y)) (OTHER_FG B R)) (INSERT B1 (OTHER_FG B R))))))))))
(TRACE-LISP
 (DEFINE INSERT (B L) (trace-defun 'INSERT (B L) (RET (IF (OR (NULL L) (> B (CAR L))) (CONS B L) (CONS (CAR L) (INSERT B (CDR L))))))))
(TRACE-LISP (DEFMACRO DEC_GOAL (X) (RET `(IF (ATOM ,X) (LIST ,X) (CONS (CAAR ,X) (CDR ,X))))))
(TRACE-LISP
 (DEFINE CREATE_BLOCK (B)
  (trace-defun 'CREATE_BLOCK (B)
   (RET (TRACE-PROGN (PUSH_CONT) (VSET MEM (+ L 2) (FGENV B)) (VSET MEM (+ L 3) CUT_PT) (CSETQ CP (LIST (FGGOAL B)) CL L) (MAJ_L 0))))))
(TRACE-LISP
 (DEFINE PR2 (PAQ)
  (trace-defun 'PR2 (PAQ)
   (RET (TRACE-PROGN (LOAD_A (LARGS PC) PCE PCG) (IF CP (PR PAQ) (TRACE-PROGN (IF (<= BL CL) (CSETQ L CL)) (CSETQ CP (CP CL) CL (CL CL)) (PR PAQ))))))))
(TRACE-LISP (DEFINE CONT_EVAL NIL (trace-defun 'CONT_EVAL NIL (RET (FUNLESS CP (IF (<= BL CL) (CSETQ L CL)) (CSETQ CP (CP CL) CL (CL CL)))))))
(TRACE-LISP (DEFINE PR (PAQ) (trace-defun 'PR (PAQ) (RET (IF (CDR PAQ) (TRACE-PROGN (PUSH_CHOIX) (PR_CHOICE PAQ)) (PR_DET (CAR PAQ)))))))
(TRACE-LISP
 (DEFINE PR_DET (C)
  (trace-defun 'PR_DET (C)
   (RET
    (IF (EQ (UNIFY_WITH (LARGS (HEAD C)) (PUSH_E (NLOC C)) (PUSH_G (NGLOB C))) 'FAIL) (BACKTRACK)
     (TRACE-PROGN (MAJ_G (NGLOB C)) (WHEN (TAIL C) (PUSH_CONT) (CSETQ CP (TAIL C) CL L) (MAJ_L (NLOC C)))))))))
(TRACE-LISP
 (DEFINE PR_CHOICE (PAQ)
  (trace-defun 'PR_CHOICE (PAQ)
   (RET
    (CLET ((RESU (SHALLOW_BACKTRACK PAQ)) (C (CAR RESU)) (R (CDR RESU)))
     (COND ((NULL R) (POP_CHOIX) (PR_DET C)) ((PUSH_BPR R) (MAJ_G (NGLOB C)) (WHEN (TAIL C) (PUSH_CONT) (CSETQ CP (TAIL C) CL L) (MAJ_L (NLOC C))))))))))
(TRACE-LISP
 (DEFINE SHALLOW_BACKTRACK (PAQ)
  (trace-defun 'SHALLOW_BACKTRACK (PAQ)
   (RET
    (IF (AND (CDR PAQ) (EQ (UNIFY_WITH (LARGS (HEAD (CAR PAQ))) (PUSH_E (NLOC (CAR PAQ))) (PUSH_G (NGLOB (CAR PAQ)))) 'FAIL))
     (TRACE-PROGN (CSETQ FRCP NIL FR (FR BL)) (POPTRAIL (TR BL)) (SHALLOW_BACKTRACK (CDR PAQ))) PAQ)))))
(TRACE-LISP
 (DEFINE BACKTRACK NIL
  (trace-defun 'BACKTRACK NIL
   (RET
    (IF (ZEROP BL) (CSETQ DUBOULOT NIL)
     (TRACE-PROGN (CSETQ L BL G BG FR (FR L) FRCP NIL CUT_PT (BL BL) CP (BCP L) CL (BCL L) CUT_PT (BL BL)) (LOAD_A2) (POPTRAIL (TR BL))
      (PR_CHOICE (BP L))))))))
(TRACE-LISP
 (DEFINE LOAD_A2 NIL
  (trace-defun 'LOAD_A2 NIL
   (RET (CLET ((DEB (- L (SIZE_C L)))) (CDOTIMES (I (A L) (VSET MEM A I)) (VSET MEM (+ A I 1) (AREF MEM (+ DEB I)))))))))
(TRACE-LISP
 (DEFINE MYLOOP (C)
  (trace-defun 'MYLOOP (C)
   (RET
    (TRACE-PROGN (CSETQ FR BOTTOMFR G BOTTOMG L BOTTOML TR BOTTOMTR CUT_PT 0 CP NIL CL 0 BL 0 BG BOTTOMG FRCP NIL DUBOULOT T) (PUSH_CONT)
     (PUSH_E (NLOC C)) (PUSH_G (NGLOB C)) (CSETQ CP (CDR C) CL L) (MAJ_L (NLOC C)) (MAJ_G (NGLOB C)) (READ-CHAR) (CATCH 'DEBORD (FORWARD))
     (MYLOOP (READ_PROMPT)))))))
;; Version 3
;; pred.lsp
;;
(TRACE-LISP
 (DEFVAR OB_MICRO_LOG
  '(|write| |nl| |tab| |read| |get| |get0| |var| |nonvar| |atomic| |atom| |number| !! |fail| |true| |divi| |mod| |plus| |minus| |mult| |le| |lt| |name|
    |consult| |abolish| |cputime| |statistics| |call| |freeze| |dif| |frozen_goals|)))
(TRACE-LISP (MAPC #'(LAMBDA (X) (trace-defun '#:G10471 (X) (RET (CSETF (GET X 'EVALUABLE) T)))) OB_MICRO_LOG))
;; !/0
(TRACE-LISP (DEFINE !! (N) (trace-defun '!! (N) (RET (CSETQ BL (CUT CL) BG (IF (ZEROP BL) BOTTOMG (BG BL)) L (+ CL 4 N))))))
;; call/1 (+term)
(TRACE-LISP
 (DEFINE |call| (X)
  (trace-defun '|call| (X)
   (RET
    (IF (VAR? X)
     (CLET ((TE (ULTIMATE X PCE PCG))) (FUNLESS CP (IF (<= BL CL) (CSETQ L CL)) (CSETQ CP (CP CL) CL (CL CL))) (PUSH_CONT) (VSET MEM (+ L 2) (CDR TE))
      (VSET MEM (+ L 3) CUT_PT) (CSETQ CP (LIST (DEC_GOAL (CAR TE))) CL L) (MAJ_L 0))
     (PUSH (DEC_GOAL X) CP))))))
;; freeze/2 (?var,+term)
(TRACE-LISP
 (DEFINE |freeze| (X P)
  (trace-defun '|freeze| (X P)
   (RET
    (CLET ((XTE (ULTIMATE X PCE PCG)))
     (IF (VAR? (CAR XTE)) (CLET ((Y (ADR (CAR XTE) (CDR XTE))) (PTE (ULTIMATE P PCE PCG))) (BINDFG Y (DEC_GOAL (CAR PTE)) (CDR PTE) (FGBLOCK Y)))
      (|call| P)))))))
;; dif/2 (?term,?term)
(TRACE-LISP
 (DEFINE |dif| (X Y)
  (trace-defun '|dif| (X Y)
   (RET
    (CLET ((BL L) (BG G) (STR TR) (FRCP NIL))
     (IF (EQ (UNI X Y) 'FAIL) (POPTRAIL STR)
      (IF (/= TR STR) (CLET ((XV (AREF MEM (1- TR))) (V (IF (NUMBERP XV) XV (CAR XV)))) (POPTRAIL STR) (BINDFG V PC PCG (FGBLOCK V))) 'FAIL)))))))
;; statistics/0 
(TRACE-LISP
 (DEFINE |statistics| NIL
  (trace-defun '|statistics| NIL
   (RET
    (TRACE-PROGN (FORMAT T " local stack : ~A (~A used)~%" (- BOTTOMTR BOTTOML) (- L BOTTOML))
     (FORMAT T " global stack : ~A (~A used)~%" (- BOTTOML BOTTOMG) (- G BOTTOMG)) (FORMAT T " trail : ~A (~A used)~%" (- A BOTTOMTR) (- TR BOTTOMTR))
     (FORMAT T " frozen-goals stack : ~A (~A used)~%" BOTTOMG (- FR BOTTOMFR)))))))
;; frozen_goals/0
(TRACE-LISP
 (DEFINE |frozen_goals| NIL
  (trace-defun '|frozen_goals| NIL
   (RET
    (CDO ((I (- FR 4) (- I 4))) ((< I 0))
     (IF (EQ (CAR (AREF MEM (FGVAR I))) 'LIBRE)
      (CLET ((B (IF (NUMBERP (FGGOAL I)) (FGGOAL I) I))) (WRITESF (PRED (FGGOAL B)) (LARGS (FGGOAL B)) (FGENV B))
       (FORMAT T " frozen upon X~A~%" (FGVAR I)))))))))
;; cpred.lsp
;;
(TRACE-LISP (DEFMACRO VALUE1 (X) (RET `(CAR (ULTIMATE ,X PCE PCG)))))
(TRACE-LISP (DEFINE UNI (X Y) (trace-defun 'UNI (X Y) (RET (CATCH 'IMPOSSIBLE (UNIF (ULTIMATE X PCE PCG) (ULTIMATE Y PCE PCG)))))))
;;write/1 (?term)
(TRACE-LISP (DEFINE |write| (X) (trace-defun '|write| (X) (RET (WRITE1 (ULTIMATE X PCE PCG))))))
(TRACE-LISP
 (DEFINE WRITE1 (TE)
  (trace-defun 'WRITE1 (TE)
   (RET
    (CLET ((X (CAR TE)) (E (CDR TE)))
     (COND ((NULL X) (FORMAT T "[]")) ((ATOM X) (FORMAT T "~A" X)) ((VAR? X) (FORMAT T "X~A" (ADR X E)))
      ((LIST? X) (FORMAT T "[") (WRITESL (VAL (CADR X) E) (VAL (CADDR X) E)) (FORMAT T "]")) ((WRITESF (FUNCTOR (DES X)) (LARGS X) E))))))))
(TRACE-LISP
 (DEFINE WRITESL (TE R)
  (trace-defun 'WRITESL (TE R)
   (RET
    (TRACE-PROGN (WRITE1 TE)
     (CLET ((Q (CAR R)) (E (CDR R)))
      (COND ((NULL Q)) ((VAR? Q) (FORMAT T "|X~A" (ADR Q E))) (T (FORMAT T ",") (WRITESL (VAL (CADR Q) E) (VAL (CADDR Q) E))))))))))
(TRACE-LISP
 (DEFINE WRITESF (FCT LARGS E)
  (trace-defun 'WRITESF (FCT LARGS E)
   (RET
    (TRACE-PROGN (FORMAT T "~A(" FCT) (WRITE1 (VAL (CAR LARGS) E))
     (MAPC #'(LAMBDA (X) (trace-defun '#:G10472 (X) (RET (TRACE-PROGN (FORMAT T ",") (WRITE1 (VAL X E)))))) (CDR LARGS)) (FORMAT T ")"))))))
;;nl/0
(TRACE-LISP (DEFINE |nl| NIL (trace-defun '|nl| NIL (RET (TERPRI)))))
;;tab/1 (+int)
(TRACE-LISP (DEFINE |tab| (X) (trace-defun '|tab| (X) (RET (CDOTIMES (I (VALUE1 X)) (FORMAT T " "))))))
;;read/1 (?term)
(TRACE-LISP
 (DEFINE |read| (X)
  (trace-defun '|read| (X) (RET (CLET ((TE (READ_TERME))) (CATCH 'IMPOSSIBLE (UNIF (ULTIMATE X PCE PCG) (CONS (CDR TE) (PUSH1_G (CAR TE))))))))))
(TRACE-LISP
 (DEFINE READ_TERME NIL
  (trace-defun 'READ_TERME NIL
   (RET (CLET ((*LVARLOC NIL) (*LVARGLOB NIL)) (CLET ((TE (READ_TERM (RCHNSEP) 2))) (RCHNSEP) (CONS (LENGTH *LVARGLOB) (C TE))))))))
(TRACE-LISP
 (DEFINE PUSH1_G (N)
  (trace-defun 'PUSH1_G (N)
   (RET
    (TRACE-PROGN (IF (>= (+ G N) BOTTOML) (THROW 'DEBORD (PRINT "Global Stack Overflow")))
     (CDOTIMES (I N (- G N)) (VSET MEM G (CONS 'LIBRE BOTTOMG)) (INCF G)))))))
;;get/1 (?car)
(TRACE-LISP (DEFINE |get| (X) (trace-defun '|get| (X) (RET (UNI X (CHAR-INT (RCHNSEP)))))))
;;get0/1 (?car)
(TRACE-LISP (DEFINE |get0| (X) (trace-defun '|get0| (X) (RET (UNI X (CHAR-INT (READ-CHAR)))))))
;;var/1 (?term)
(TRACE-LISP (DEFINE |var| (X) (trace-defun '|var| (X) (RET (FUNLESS (VAR? (VALUE1 X)) 'FAIL)))))
;;nonvar/1 (?term)
(TRACE-LISP (DEFINE |nonvar| (X) (trace-defun '|nonvar| (X) (RET (IF (VAR? (VALUE1 X)) 'FAIL)))))
;;atomic/1 (?term)
(TRACE-LISP (DEFINE |atomic| (X) (trace-defun '|atomic| (X) (RET (IF (LISTP (VALUE1 X)) 'FAIL)))))
;;atom/1 (?term)
(TRACE-LISP (DEFINE |atom| (X) (trace-defun '|atom| (X) (RET (FUNLESS (SYMBOLP (VALUE1 X)) 'FAIL)))))
;;number/1 (?term)
(TRACE-LISP (DEFINE |number| (X) (trace-defun '|number| (X) (RET (FUNLESS (NUMBERP (VALUE1 X)) 'FAIL)))))
;;fail/0
(TRACE-LISP (DEFINE |fail| NIL (trace-defun '|fail| NIL (RET 'FAIL))))
;;true/0
(TRACE-LISP (DEFINE |true| NIL (trace-defun '|true| NIL)))
;;divi/3 (+int,+int,?int)
(TRACE-LISP (DEFINE |divi| (X Y Z) (trace-defun '|divi| (X Y Z) (RET (UNI Z (FLOOR (VALUE1 X) (VALUE1 Y)))))))
;;mod/3 (+int,+int,?int)
(TRACE-LISP (DEFINE |mod| (X Y Z) (trace-defun '|mod| (X Y Z) (RET (UNI Z (REM (VALUE1 X) (VALUE1 Y)))))))
;;plus/3 (+int,+int,?int)
(TRACE-LISP (DEFINE |plus| (X Y Z) (trace-defun '|plus| (X Y Z) (RET (UNI Z (+ (VALUE1 X) (VALUE1 Y)))))))
;;minus/3 (+int,+int,?int)
(TRACE-LISP (DEFINE |minus| (X Y Z) (trace-defun '|minus| (X Y Z) (RET (UNI Z (- (VALUE1 X) (VALUE1 Y)))))))
;;mult/3 (+int,+int,?int)
(TRACE-LISP (DEFINE |mult| (X Y Z) (trace-defun '|mult| (X Y Z) (RET (UNI Z (* (VALUE1 X) (VALUE1 Y)))))))
;;le/2 (+int,+int)
(TRACE-LISP (DEFINE |le| (X Y) (trace-defun '|le| (X Y) (RET (IF (> (VALUE1 X) (VALUE1 Y)) 'FAIL)))))
;;lt/2 (+int,+int)
(TRACE-LISP (DEFINE |lt| (X Y) (trace-defun '|lt| (X Y) (RET (IF (>= (VALUE1 X) (VALUE1 Y)) 'FAIL)))))
;;name/2 (?atom,?list)
(TRACE-LISP
 (DEFINE |name| (X Y)
  (trace-defun '|name| (X Y) (RET (CLET ((B (VALUE1 X))) (IF (VAR? B) (UNI X (IMPL (UNDO_L (ULTIMATE Y PCE PCG)))) (UNI Y (DO_L (EXPL B)))))))))
(TRACE-LISP
 (DEFINE UNDO_L (TE)
  (trace-defun 'UNDO_L (TE)
   (RET (CLET ((X (CAR TE)) (E (CDR TE))) (IF (ATOM X) X (CONS (UNDO_L (VAL (CADR X) E)) (UNDO_L (VAL (CADDR X) E)))))))))
(TRACE-LISP (DEFINE DO_L (X) (trace-defun 'DO_L (X) (RET (IF (ATOM X) X (LIST '(|.| 2) (CAR X) (DO_L (CDR X))))))))
(TRACE-LISP (DEFINE IMPL (L) (trace-defun 'IMPL (L) (RET (INTERN (MAP 'STRING #'INT-CHAR L))))))
(TRACE-LISP (DEFINE EXPL (AT) (trace-defun 'EXPL (AT) (RET (MAP 'LIST #'CHAR-INT (STRING AT))))))
;;consult/1 (+atom)
(TRACE-LISP (DEFINE |consult| (F) (trace-defun '|consult| (F) (RET (FORMAT T "~A~%" (LOAD (VALUE1 F)))))))
;; abolish/1
(TRACE-LISP
 (DEFINE |abolish| (P)
  (trace-defun '|abolish| (P)
   (RET (MAPC #'(LAMBDA (X) (trace-defun '#:G10473 (X) (RET (CSETF (GET P X) NIL)))) '(ATOM EMPTY LIST FONCT DEF))))))
;; cputime/1
(TRACE-LISP
 (DEFINE |cputime| (X) (trace-defun '|cputime| (X) (RET (UNI X (FLOAT (/ (GET-INTERNAL-RUN-TIME) INTERNAL-TIME-UNITS-PER-SECOND)))))))
(TRACE-LISP (CSETF (GET '|eq| 'DEF) '(((1 . 0) (|eq| (L . 0) (L . 0))))))
(TRACE-LISP (CSETF (GET '|or| 'DEF) '(((2 . 0) (|or| (L . 1) (L . 0)) (|call| (L . 1))) ((2 . 0) (|or| (L . 1) (L . 0)) (|call| (L . 0))))))
#|    

%
% Startup MicroPrologII
%
$ eq(X,X).
$ neq(X,X):- !!, fail.
$ neq(X,Y).

$ not(X) :- call(X), !!, fail.
$ not(X).

$ repeat.
$ repeat :- repeat.

$ differents(L) :- freeze(L,differents1(L)).
$ differents1([]).
$ differents1([TXQ]) :- hors_de(T,Q), differents(Q).
$ hors_de(X,L) :- freeze(L,hors_de1(X,L)).
$ hors_de1(X,[]).
$ hors_de1(X,[TXQ]) :- dif(X,T), hors_de(X,Q).

$ freeze2_ou(X,Y,B) :- freeze(X,une_fois(B,V)),
                       freeze(Y,une_fois(B,V)).
$ une_fois(B,V) :- var(V), !!, call(B), eq(V,deja_fait).
$ une_fois(B,V).

$ or(X,_):-call(X).
$ or(_,X):-call(X).


$ conc([],X,X) .
$ conc([TXQ],L,[TXR]) :- conc(Q,L,R).
$ element(X,[XXY]).
$ element(X,[YXZ]) :- element(X,Z).
$ del(X,[XXY],Y).
$ del(X,[YXZ],[YXR]) :- del(X,Z,R).

$ wb(X) :- write(X), write(' ').
$ wf(X) :- write(X), nl, fail.



(mlg3)|#


;; Prolog Adapted by Douglas R. Miles (dmiles@users.sourceforge.net) from Common Lisp to SubLisp
;; from book "The Implementation of Prolog" 
;; Patrice Boizumault, Princeton University Press, 1993. 
;; Translated by Ara M. Djamboulian and Jamal Fattouh. ISBN 0-691-08757-1, 357 pages
;; see http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/prolog/impl/prolog/boiz_pl/0.html
;;(load "common.lisp")

(defstub :cb-prolog prolog-call)
(defstub :cb-prolog prolog-asserta)
(defstub :cb-prolog prolog-retract)
(defstub :cb-prolog prolog-retractall)
(defstub :cb-prolog prolog_repl )


(defun foc (str) (find-or-create-constant str))

;; TODO - make prolog reader use it's own copy
(defvar *lvarloc nil)
(defvar *lvarglob nil)
(defvar *prologMt* (foc "PrologDataMt"))
(defvar *theList* (foc "TheList"))
(defvar *entailedBy* (foc "prolog:entailedBy"))
(defvar *prologFn* (foc "prolog:fn"))
(defvar *prologPredicate* (foc "prolog:ProgrammingPredicate"))
(defvar *prologCons* (list *prologFn* "."))
;;(defvar *prologNil* (foc "TheEmptyList"))
(defvar *prologNil* ())
(defvar *lispNil* (list *prologFn* "[]"))

(defvar *xfy* ())
(defvar *fx* ())
(defvar *xfx* ())
(defvar *fy* ())

(setq *xfy* '(";" ","))
(setq *xfx* '(":-" "->" "+" "-" "/" "*" "=" "==" ":"))
(setq *fx* '("+" "-"))
(setq *fy* '(":-"))

(defun string-remove-segment ( str strtstr endstr )
  (let ((start (search-obey-escapes strtstr str)))
    (if (null start) str
      (let ((end (search-obey-escapes endstr str #'eql #'identity 0 nil start)))
        (if (not (and (numberp start)(numberp end)(> end start))) str
          (string-remove-segment (string-concat (SUBSEQ str 0 start) (SUBSEQ str (+ end (length endstr)))) strtstr endstr))))))

;;STRING-RIGHT-TRIM STRING-RIGHT-TRIM
#|
e
 (remove-comments ";; lisp comment threre")
c
 %% prolog comment
 b
 /* c comment */
 ok
")
  (load "cb_prolog.lisp")
|#
(defun remove-comments (str)
  #|  (setq str (string-remove-segment (string-concat str (string #\Newline)) "%" (string #\Newline)))
  (setq str (string-remove-segment (string-concat str (string #\Newline)) "/*" "*/"))
  (setq str (string-remove-segment (string-concat str (string #\Newline)) (string-concat (string #\Newline) ";") (string #\Newline)))
  (setq str (string-remove-segment (string-concat str (string #\Newline)) "#|" "|#"))
  (setq str (string-trim *WHITESPACE-CHARS* str))
  str)|#
  (string-trim *WHITESPACE-CHARS* (string-remove-segment (string-concat (string-remove-segment (string-concat (string-remove-segment (string-concat (string-remove-segment (string-concat str (string #\Newline)) "%" (string #\Newline)) (string #\Newline)) "/*" "*/") (string #\Newline)) (string-concat (string #\Newline) ";") (string #\Newline)) (string #\Newline)) "#|" "|#")))

(defun string-to-term (str &optional (fun  #'read_term)) 
  (setq str (remove-comments str))
  (let ((len (length str)))
    (if (= len 0) (return-from string-to-term :EOF)
      (let ((*read-sofar* 0)
            (*standard-input* (MAKE-STRING-INPUT-STREAM str))
            (res (funcall fun (rch))))
        (return-from string-to-term (values res *read-sofar* (substring str *read-sofar*)))))))

;; TODO - make prolog reader
(defun prolog-read (stream varinfo)
  (THROW-UNEVALUATABLE-ON-ERROR 
   (if (stringp stream) (string-to-term stream)
     (let ((*standard-input* (if (streamp stream) stream *standard-input*))) (list (read_term (rch)) varinfo)))))

(defun search-obey-escapes (seq1 str &optional (test #'eql) (key #'identity) (start1 0) end1 (start2 0) end2) 
  (let ((start (search seq1 str test key start1 end1 start2 end2)))
    (if (or (not (numberp start))(= start 0)) start
      (if (not (member (nth (- start 1) str) '(#\\ #\' #\`) #'char=)) start
        (search-obey-escapes seq1 str test key start1 end1 (+ 1 start) end2)))))

;(defun impl (l) (intern (map 'string #'code-char l)))
(defun implstring (l) (let ((codelist (mapcar #'code-char l))(code (make-string (length l)))) 
                        (do ((trm 0 (+ 1 trm))) ((= trm (length l)) (return-from implstring code )) (set-char code trm (nth trm codelist)))))
(defun impl (l) (intern (implstring l)))

;(defun implode (lch) (intern (map 'string #'identity lch)))
(defun implode (l) (let ((codelist (mapcar #'identity l))(code (make-string (length l)))) 
                     (do ((trm 0 (+ 1 trm))) ((= trm (length l)) (return-from implode (intern code ))) (set-char code trm (nth trm codelist)))))

;(defun expl (at) (map 'list #'char-code (string at)))
(defun expl (l) (let ((l (string l))(codelist ()))
                  (do ((trm 0 (+ 1 trm))) ((= trm (length l)) (return-from expl codelist)) (setq codelist (append codelist (list (char-code (char l trm))))))))

#|(defun read_prompt (&optional varinfo) 
  (terpri)
  (format t "| ?- ") (force-output) ;(gc)
  (let ((retval (read_code_tail)))
    (myprint retval) 
    retval))

(defun read_code_tail ()
  (let ((trm (read_comma_list (rch) #\.)))
    (cons (cons (length *lvarloc) (length *lvarglob)) (append (ccc trm) (list '(|true|))))))
(defun banner () (terpri)(format t "Prolog REPL~%")(terpri)(force-output) T)

(defun l () (format t "Back to Prolog top-level~%")(force-output)(prolog_repl))
|#

(defvar *saved-standard-input* *standard-input*)
(defvar *saved-standard-output* *standard-output*)
(defvar *saved-terminal-io* *terminal-io*)



(defvar *read-sofar* 0)

;;(defun chars-ready (stream) (let (read-char-no-hang stream))
;; end-of-stream exits true of there are no chars to peek at
(defun end-of-stream ()
  (null (peek-char nil *standard-input* nil)))

;; peek-char that can emit #\Null
(defun pch ()
  ;;(read-char-no-hang)
  (if (end-of-stream) (code-char 0)(peek-char)))

;; unread-char that accepts #\Null
(defun uch (c)
  (if (char= c #\Null) (ret NIL))
  (decf *read-sofar*)(unread-char c)(return-from uch t))

;; read-char that can emit #\Null
(defun nch ()
  (if (end-of-stream) (return-from nch (code-char 0)))
  (incf *read-sofar*)(return-from nch (read-char)))
;; read-char (non-white) that can emit #\Null
(defun rch ()
  (if (end-of-stream) (return-from rch (code-char 0)))
  (let ((ch (nch))) 
    (return-from rch (if (member ch *WHITESPACE-CHARS*) (rch) ch ))))


(defun charvarp (ch) (ret (or (upper-case-p ch) (char= ch #\_) (char= ch #\?) )))
(defun alphanum (ch) (ret (or (alphanumericp ch) (charvarp ch))))
(defun valdigit (ch) (ret (digit-char-p ch)))

;;(defun read_number (ch)(return-from read_number (do ((v (valdigit ch) (+ (* v 10) (valdigit (nch)))))((not (digit-char-p (pch))) v))))
(defun read_number (ch)(uch ch)(read))

(defun read_atom (ch)
  (if (char= ch #\Null ) (ret :EOF))
  (let ((op (infix-op ch))) (if op (ret op)))
  (return-from read_atom 
    (do ((lch (list ch) (push (nch) lch)))
        ((not (alphanum (pch))) (implode (reverse lch))))))

(defun read_qatom (ch)
  (return-from read_qatom (do ((lch (list ch) (push (nch) lch)))
                              ((char= (pch) #\') (nch) (implode (reverse lch))))))


(defun do_char_list (trm)
  (return-from do_char_list (if (atom trm) trm (prolog-make-cons (car trm) (do_char_list (cdr trm))))))
#|(defun read_string (ch)
  (return-from read_string (do ((lch (list (char-code ch)) 
                                  (push (char-code (nch)) lch)))
                               ((char= (pch) #\" ) (nch ) (do_char_list (reverse lch)))))) 
|#
(defun read_string (ch)
  (uch ch)
  (read))

(defun skipto (char &optional prestr)
  (if (null prestr) (setq prestr ""))
  (let ((ch (nch)))
    (if (or (char= ch char)(char= ch #\Null)) prestr (string-concat (string ch) (skipto char prestr)))))

(defun make_var_ref (trm)
  (unless (CYC-VAR? trm) (setq trm (MAKE-EL-VAR trm)))
  (setq trm (corRECT-VARIABLE trm ))
  ;; (myprint (list :make_var_ref trm))
  (unless (member trm *lvarglob)(pushnew trm *lvarloc))
  (ret trm))

#|
countSuccess(Call,_):-flag(Call,_,0),fail.
countSuccess(Call,_):-call(Call),flag(Call,N,N+1),fail.
countSuccess(Call,N):-flag(Call,N,0).
|#

(defun read_atomic (ch)
  (if (char= ch #\Null) (return-from read_atomic :EOF))
  ;;operators
  (let ((op (infix-op ch))) (if op (return-from read_atomic op)))
  (cond
   ;;comments
   ((char= ch #\% ) (skipto #\Newline "%")(return-from read_atomic (read_atomic (rch))))
   ((and (char= ch #\/ ) (char= (pch) #\* )) (skipto #\/ "/")(return-from read_atomic (read_atomic (rch))))
   ;; Prolog lists [ ... ]
   ((char= ch #\[ ) (return-from read_atomic (read_list (rch))))
   ;; Prolog vectors { ... }
   ((char= ch #\{) (return-from read_atomic (cons '|{}| (read_comma_list (rch) #\}))))
   ;; Prolog chars `A
   ((char= ch #\` )(return-from read_atomic (read-char ch)))
   ;; Prolog quoted atoms 'Abc'
   ((char= ch #\' ) (return-from read_atomic (read_qatom (nch))))
   ;; Prolog variables Abc
   ((charvarp ch) (return-from read_atomic (make_var_ref (read_atom ch))))
   ;; Prolog Numbers
   ((valdigit ch) (return-from read_atomic (read_number ch)))
   ;;((char= ch #\() (return-from read_atomic (cons '|,| (read_comma_list (rch) #\)))))
   ;;((char= ch #\%) (readto \#Newline)(return-from read_atomic (read_atomic (rch))))
   ((char= ch #\" ) (return-from read_atomic (prolog-make-string (read_string (nch)))))
   ((and (char= ch #\# )(member (pch) '(#\S ))) (read-char ch)(return-from read_atomic (read)))
   ;;((and (char= ch #\# )(member (pch) '(#\$ #\\ #\( ))) (unread-char ch)(return-from read_atomic (read)))
   ((char= ch #\# ) (uch ch)(return-from read_atomic (read)))
   (t (return-from read_atomic (read_atom ch)))))

(defun prolog-make-string (str)(ret str))

;; worng one
(defun read_term (ch)
  (let ((trm (read_atomic ch)) (c (rch)))
    ;;(myprint c)
    (if (member c '(#\Null #\.)) (return-from read_term trm))
    (if (char= c #\( ) 
        (let ( (args (read_comma_list (rch) #\) ) ))
          (setq trm (ret (cons *prologFn* (cons (prolog-make-pred trm) (prolog-make-cdr args)))))
          (setq c (rch))))
    (if (member c '(#\Null #\.)) (return-from read_term trm))
    (let ((op (infix-op c))) 
      (if op (return-from read_term (list op trm (read_term (rch) )))))
    ;;(myprint c)
    (uch c) 
    (return-from read_term trm)
    ))


#|
CYC(65): (string-to-term "member(ITEM,[X|REST])")
(prolog:clause (prolog:FunctorFn "member" 2) ?ITEM (|.| ?X ?REST))
CYC(66): (string-to-term "a,b")
(|,| |a| |b|)
CYC(67): (string-to-term "a:-b")
(:- |a| |b|)
CYC(68): (string-to-term "a(X):-b(X)")
(:- (prolog:clause (prolog:FunctorFn "a" 1) ?X) (prolog:clause (prolog:FunctorFn "b" 1) ?X))
CYC(69): (string-to-term "a([X]):-b(X)")
(:- (prolog:clause (prolog:FunctorFn "a" 1) (|.| ?X NIL)) (prolog:clause (prolog:FunctorFn "b" 1) ?X))
CYC(70): (string-to-term "a([X|B])")
(prolog:clause (prolog:FunctorFn "a" 1) (|.| ?X ?B))
CYC(71): (string-to-term "a([X|B]):-ture")
(prolog:clause (prolog:FunctorFn "a" 1) (|.| ?X ?B))
CYC(71): (string-to-term "[X|B]:-ture")
CYC(83): (string-to-term "")
:EOF
(string-to-term "[]") => NIL
(string-to-term "[1]") => (|.| 1 NIL)

|#

(defun charstring (&rest chars) 
  ;; (print chars)(force-output)
  (ret (implstring (mapcar #'char-code chars))))

(defun infix-op (c &optional list)
  (if (null list) (setq list (append *xfy* *xfx* *fx* *fy*)))
  (let ((opstr ""))
    #|(let ((nc (nch)))
 (setq opstr (charstring c nc (pch)))
 (if (member opstr list #'equal) (progn (nch)(ret (intern opstr))))
 (uch nc))|#
    (setq opstr (charstring c (pch)))
    (if (member opstr list #'equal) (progn (nch)(ret (intern opstr))))
    (setq opstr (charstring c))
    (if (member opstr list #'equal) (ret (intern opstr)))))

;; (load "prolog_reader.lisp")(read_term (rch))  
;; (load "prolog_reader.lisp")(prolog-read)
(defun read_comma_list (ch lchar)
  (if (member ch (cons lchar '(#\Null #\.))) (ret ()))
  (let ((*xfy* (remove "," *xfy* #'equal))(*xfx* (remove "," *xfx* #'equal))
        (trm (read_term ch)) (c (rch)))
    (if (char= c lchar) (ret (list trm)))
    (if (char= c #\,) (ret (cons trm (read_comma_list (rch) lchar))))
    (uch c) (ret (list trm))))


#|
CYC(91): (string-to-term "[A,B]")
(|.| ?A (|.| ?B NIL))
(string-to-term "[1.0,2.0 ]")
(|.| 1.0 (|.| 2.0 NIL))
|#
(defun read_list (ch)
  (if (char= ch #\])
      *prologNil*
    (let ((*xfy* (remove "," *xfy* #'equal))(*xfx* (remove "," *xfx* #'equal))(trm (read_term ch)))
      (case (rch)
        (#\, (prolog-make-cons trm (read_list (rch))))
        (#\| (prog1 (prolog-make-cons trm (read_term (rch))) (rch))) 
        (#\] (prolog-make-cons trm *prologNil*))))))


(defun ccc (l)
  (ret 
   (if (atom l) 
       (if (member l *lvarloc) 
           (cons 'L (position l *lvarloc))
         (if (member l *lvarglob) 
             (cons 'G (position l *lvarglob)) l))
     (if (eq (car l) '! ) (list '! (length *lvarloc))
       (cons (ccc (car l)) (ccc (cdr l)))))))

; Version 3
; lecteur.lsp
;

(defun unsafe? (trm h q) (ret (and (member trm q) (not (member trm h)))))

(defun maj_locglob (h q)
  (ret (mapc #'(lambda (trm) 
                 (when (unsafe? trm h q)
                   (setq *lvarloc (delete trm *lvarloc))
                   (push trm *lvarglob))) 
         *lvarloc)))

(defun read_code_cl ()
  (let ((*lvarloc ()) (*lvarglob ()))
    (ret (let ((trm (read_term (rch))))
           (maj_locglob (car trm) (car (last trm)))
           (cons (cons (length *lvarloc) (length *lvarglob)) (ccc trm))))))


(defun prolog-assert-now (sent &optional (mt *prologmt*)) (prolog-assert-now-real sent mt))

(defun prolog-assert-now-real (sent &optional (mt *prologmt*))
  (let (
        (this (list 'ke-assert-now (list 'quote sent) (list 'quote mt) :monotonic :FORWARD ))
        (retval (car (FIND-ASSERTIONS-CYCL sent mt))))
    (print this)(force-output)
    (if (assertion-p retval) (ret retval))
    (let ((res (eval this))(retval (car (FIND-ASSERTIONS-CYCL sent mt))))
      (print retval)(if (equal res t) (ret retval))
      (print (list :ERROR (HL-EXPLANATION-OF-WHY-NOT-WFF sent mt)))
      (force-output))))


(prolog-assert-now (list (foc "isa") *prologMt* (foc "DataMicrotheory")) (foc "BaseKB"))
(prolog-assert-now (list (foc "isa") *prologMt* (foc "ApplicationContext")) (foc "BaseKB"))
(prolog-assert-now (list (foc "comment") *prologMt* "The #$DataMicrotheory that holds a the prolog program used by <a href=\"cg?cb-prolog&mt=PrologDataMt&prolog=Listing&code=true.\" target=\"cyc-main\">Listing of #$PrologDataMt</a>") *prologMt*)


(prolog-assert-now (list (foc "isa") *prologPredicate* (foc "Collection")) *prologMt*)
(prolog-assert-now (list (foc "genls") *prologPredicate* (foc "Relation")) *prologMt*)
;;(prolog-assert-now (list (foc "genls") *prologPredicate* (foc "Predicate")) *prologMt* )
(PROLOG-ASSERT-NOW
 (LIST (FOC "implies") (LIST (FOC "isa") '?PRED *PROLOGPREDICATE*)
   (LIST (FOC "and") 
     (LIST (FOC "completeExtentEnumerable") '?PRED)
     ;; (LIST (FOC "argsQuotedIsa") '?PRED (FOC "CycLIndexedTerm")) 
     (LIST (FOC "argsIsa") '?PRED (FOC "Thing"))
     ;; (LIST (FOC "argFormat") '?PRED 2 (FOC "openEntryFormatInArgs"))
     (LIST (FOC "argFormat") '?PRED 1 (FOC "openEntryFormatInArgs"))
     ;;(LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "LeaveSomeTermsAtEL"))
     (LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "DontReOrderCommutativeTerms"))
     ;;(LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "AllowKeywordVariables"))
     (LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "LeaveSomeTermsAtELAndAllowKeywordVariables"))
     (LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "AllowGenericArgVariables"))
     (LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "LeaveVariablesAtEL"))
     ))
 *PROLOGMT*)

(defvar *prologSource* (foc "prolog:sourceCode"))
(defvar *ist* (foc "ist"))

(prolog-assert-now (list (foc "isa") *prologSource* (foc "BinaryPredicate")) *prologMt*)
(prolog-assert-now (list (foc "isa") *prologSource* (foc "StrictlyFunctionalSlot")) *prologMt*)
(prolog-assert-now (list (foc "isa") *prologSource* *prologPredicate*) *prologMt*)
(prolog-assert-now (list (foc "arg1Isa") *prologSource* (foc "Individual")) *prologMt*)
(prolog-assert-now (list (foc "arg2Isa") *prologSource* (foc "List")) *prologMt*)
(prolog-assert-now (list (foc "argFormat") *prologSource* 2 (foc "openEntryFormatInArgs")) *prologMt*)


(prolog-assert-now (list (foc "isa") *entailedBy* (foc "TransitiveBinaryPredicate")) *prologMt*)
(prolog-assert-now (list (foc "isa") *entailedBy* (foc "AsymmetricBinaryPredicate")) *prologMt*)
(prolog-assert-now (list (foc "isa") *entailedBy* *prologPredicate*) *prologMt*)
;;(prolog-assert-now (list (foc "argsQuotedIsa") *entailedBy* (foc "CycLSentence")) *prologMt*)
(prolog-assert-now (list (foc "comment") *entailedBy* "The prolog neck (:-) predicate - ?arg1 :- ?arg2, ?arg3, ... .") *prologMt*)

;;(prolog-assert-now (list (foc "argsQuotedIsa") *prologClause* (foc "CycLExpression")) *prologMt*)
;;(prolog-assert-now (list (foc "isa") *prologFn* (foc "IndividualDenotingFunction")) *prologMt*)
;;(prolog-assert-now (list (foc "isa") *prologFn* (foc "TotalFunction")) *prologMt*)
(prolog-assert-now (list (foc "isa") *prologFn* (foc "VariableArityPredicate")) *prologMt*)
;;(prolog-assert-now (list (foc "isa") *prologFn* (foc "UnreifiableFunction")) *prologMt*)
(prolog-assert-now (list (foc "comment") *prologFn* "A prolog predicate clause - ?arg1(?arg2, ?arg3, ...).") *prologMt*)
;;(prolog-assert-now (list (foc "resultIsa") *prologFn* (foc "Thing")) *prologMt*)

(prolog-assert-now (list (foc "isa") *prologFn* *prologPredicate*) *prologMt*)

#|
(prolog-assert-now (list (foc "arity") (list *functorFn* '?STRING '?ARITY) '?ARITY) *prologMt* )
(prolog-assert-now 
 (list (foc "implies")
   (list (foc "isa") (list *functorFn* '?STRING '?ARITY) (foc "Relation"))
   (list (foc "arity") (list *functorFn* '?STRING '?ARITY) '?ARITY)) *prologMt* )
(prolog-assert-now 
 (list (foc "implies")
   (list (foc "and")
     (list (foc "isa") (list *functorFn* '?STRING '?ARITY) (foc "Relation"))
     (list (foc "resultIsa") *functorFn* '?TYPE))
   (list (foc "isa") (list *functorFn* '?STRING '?ARITY) '?TYPE)) *prologMt* )

(prolog-assert-now (list (foc "isa") *prologNil* *prologPredicate*) *prologMt* )
(prolog-assert-now (list (foc "isa") *prologCons* *prologPredicate*) *prologMt* )
(prolog-assert-now (list (foc "isa") *prologCons* (foc "ProgramFunction")) *prologMt* )
|#
;;(prolog-assert-now (list (foc "isa") (list *functorFn* "member" 2) (foc "Thing")) *prologMt* )
;; TODO - implement prolog writer


(defmacro defprolog (name farity args &rest body)
  (let ((arity (length `,args))
        (fname (concatenate 'string `,name "/" (write-to-string `,farity))))
    (let ((sym (intern `,fname)))
      (let ((fort (prolog-make-cycl `,fname `,arity)))
        (setf (get `,sym 'evaluable) t)
        (setf (get `,sym 'prolog_name) `,name)
        (setf (get `,sym 'prolog_arity) `,farity)
        (setf (get `,sym 'lisp_arity) `,arity)
        (prolog-assert-now (list (foc "arity") `,fort `,arity ) *prologMt* ) 
        (prolog-assert-now (list (foc "evaluationDefn") `,fort (list (foc "SubLQuoteFn") `,sym)) *prologMt* ) 
        `(defun-evaluation-defn ,sym ,args (ret (progn . ,body)))))))

(defun prolog-sentences-to-string (trms )
  (ret (if (null trms) " " (string-concat (prolog-to-string (car trms)) "." (make-string 2 #\Newline) (prolog-sentences-to-string (cdr trms))))))

(defun prolog-need-quote (str) (let ((ch (char str 0)))
                                 (if (member ch '(#\: #\' #\` #\#) #'char=) (ret nil))
                                 (ret (or (not (lower-case-p ch)) (find #\: str)))))


(defun prolog-list-to-string (fun trm &optional (begstr "") (endstr "") (nullstr "")) 
  (if (null trm ) (ret (string-concat begstr endstr)))
  (if (atom trm ) (ret (funcall fun trm)))
  (ret (string-concat begstr (funcall fun (car trm)) (prolog-cdr-to-string fun (cdr trm) "," "|" nullstr) endstr )))

(defun prolog-cdr-to-string (fun trm &optional (commastr ",")(barstr "|") (nullstr "")) 
  (cond
   ((null trm) (ret nullstr))
   ((consp trm) (ret (string-concat commastr (funcall fun (car trm)) 
                       (prolog-cdr-to-string fun (cdr trm) commastr barstr nullstr))))
   (t (ret (string-concat barstr (funcall fun trm))))))


(defun prolog-to-name (trm)
  (cond
   ((stringp trm) (ret (if (prolog-need-quote trm) (string-concat "'" trm "'") trm)))
   ((constant-p trm) (ret (string-concat "#$" (constant-name trm) "")))
   (t (prolog-to-name (prolog-to-string trm)))))


(defun prolog-to-string (trm )
  (cond 
   ((nart-p trm) (ret (string-concat "nart(" (prolog-to-string (nart-el-formula trm)) ")")))
   ((assertion-p trm) (ret (prolog-to-string (assertion-el-ist-formula trm))))
   ((equal trm *prologNil*) (ret "[]"))
   ((null trm) (ret "[]"))
   ((el-variable-p trm) (ret (SUBSTITUTE #\_ #\- (EL-VAR-NAME-WITHOUT-PREFIX trm))))
   ((hl-variable-p trm) (ret (SUBSTITUTE #\_ #\- (EL-VAR-NAME-WITHOUT-PREFIX trm))))
   ((constant-p trm) (ret (string-concat "#$" (constant-name trm) "")))
   ((stringp trm) (ret (write-to-string trm)))
   ((numberp trm) (ret (write-to-string trm)))
   ((symbolp trm) (ret (prolog-to-name (symbol-name trm))))
   ((not (consp trm)) (ret (prolog-to-name (write-to-string trm))))
   (t 
    (let ((pred (elt trm 0))(trm (cdr trm)))
      (if (equal pred *ist*) (ret (prolog-to-string (cdr trm))))
      (if (equal pred *prologFn*) 
          (progn 
            (setq pred (car trm))(setq trm (cdr trm))
        (if (equal pred ".") (ret (string-concat "[" (prolog-to-string (first trm)) "|" (prolog-to-string (second trm)) "]" )))
        (if (member pred *xfx* #'equal) 
            (ret (string-concat (prolog-to-string (car trm)) " " (prolog-to-name pred) " " 
                (prolog-list-to-string #'prolog-to-string (cdr trm) "" "" ))))
        (ret (string-concat (prolog-to-name pred) (prolog-list-to-string #'prolog-to-string trm "(" ")")))
        ))(ret (prolog-list-to-string #'prolog-to-string trm "[" "]"))))))



(defun prolog-make-cycl (trm &optional (mt *prologmt*))
  (cond 
   ((assertion-p trm) (ret trm))
   ((null trm) (ret *prologNil*))
   ((el-variable-p trm) (ret trm))
   ((hl-variable-p trm) (ret trm))
   ((constant-p trm) (ret trm))
   ((fort-p trm) (ret trm))
   ((stringp trm) (ret trm))
   ((numberp trm) (ret trm))
   ((prolog-predicatep trm) (ret trm))
   ((nart-p trm) (ret trm))
   ((atom trm) (ret (list *prologFn* trm)))
   (t (let ((pred (car trm))(trm (cdr trm)))
        (if (equal pred *ist*) (ret (prolog-make-cycl (elt trm 2) (elt trm 1))))
        (if (member pred '("." '|.| *prologCons*)) (ret (prolog-make-cons (prolog-make-cycl (car trm) mt)(prolog-make-cycl (cdr trm) mt))))
        (if (not (equal pred *prologFn*) ) (ret (prolog-make-cons (prolog-make-cycl pred)(prolog-make-cycl trm))))
        (setq pred (car trm) trm (cdr trm))
        (if (member pred '("." '|.| *prologCons*)) (ret (prolog-make-cons (prolog-make-cycl (car trm) mt)(prolog-make-cycl (cdr trm) mt))))
        (ret (cons *prologFn* (cons (prolog-make-pred pred) (prolog-make-cdr trm))))))))

(defun prolog-make-cdr (trm &optional (mt *prologmt*))
  (if 
      (atom trm) 
      (if trm (prolog-make-cycl trm mt) trm)
    (cons (prolog-make-cycl (car trm) mt) (prolog-make-cdr (cdr trm) mt))))

(defun prolog-make-cons (car cdr) (ret (cons car cdr)))

(defun prolog-car (trm)
  (ret (if (prolog-consp trm) (second trm) (third trm))))

(defun prolog-cdr (trm)
  (ret (if (prolog-consp trm) (third trm) (fourth trm))))

(defun prolog-consp (trm)
  (and (consp trm) (if (equal (car trm) *prologfn*) (equal (second trm) *prologCons*) t)))

(defun prolog-predicatep (fort)
  (ret (and (fort-p fort) 
         (or (predicate? fort)(cyc-query (list (foc "isa") fort *prologPredicate*) *prologMt*)))))



(defun prolog-make-op (trm)
  (cond
   ((stringp trm) (ret (list (foc "OperatorFn") trm)))
   ((el-variable-p trm) (ret trm))
   ((hl-variable-p trm) (ret trm))
   ((fort-p trm) (ret trm))
   ((nart-p trm) (ret trm))
   ((constant-p trm) (ret trm))
   ((symbolp trm) (ret (list (foc "OperatorFn") (symbol-name trm))))
   (t (ret trm))))

(defun prolog-make-pred (trm &optional arity)
  ;; (print (list 'prolog-make-pred (list 'quote trm)))
  (cond
   ((el-variable-p trm) (ret trm))
   ((prolog-predicatep trm) (ret trm))
   ((constant-p trm) (ret trm))
   ((symbolp trm) (ret (prolog-make-pred (symbol-name trm))))
   ((stringp trm) (ret trm))
   ((atom trm) (ret trm))
   ((dotted-list-p trm) (ret (prolog-make-pred (car trm) '??)))
   (t
    (let ((pred (elt trm 0)))
      (if (equal *prologFn* pred) (ret (prolog-make-pred (cdr trm))))
      (if (equal '|:-| pred) (ret (prolog-make-pred (elt trm 1))))
      (if (equal *ist* pred) (ret (prolog-make-pred (elt trm 2))))
      ;;(if (equal *entailedBy* pred) (ret (prolog-make-pred (elt trm 1))))
      (if (null arity) (setq arity (- (length trm) 1)))
      (ret (prolog-make-pred pred arity))))))



#|
(print (prolog-make-cycl '("member" ?item (?item . ?rest))))
(print (prolog-make-pred '("member" ?item (?item . ?rest))))
(print (prolog-make-cycl '(|:-| ("member" ?item (?x . ?rest)) ("member" ?item ?rest))))
(print (prolog-make-pred '(|:-| ("member" ?item (?x . ?rest)) ("member" ?item ?rest))))
(load "prolog_to_cycl.lisp")
(print (prolog-make-cycl 1))
|#

 
(defun first-answer (ansrs)
  (ret (if (consp ansrs) (car ansrs))))


(defun prolog-pred-source (pred &optional (mt *prologmt*))
  (first-answer (ask-template '?cycl (list *prologSource* (prolog-make-op pred) (cons *theList* '?cycl)) mt)))

(defun prolog-update-code (pred new &optional old (mt *prologMt*))
  (setq pred (prolog-make-op pred))
  (if (null old) (setq old (prolog-pred-source pred mt)))
  (if old (fi-unassert (list *prologSource* pred (cons *theList* old)) mt))
  (prolog-assert-now (list *prologSource* pred (cons *theList* new)) mt))

(defun prolog-assertz (sent &optional pred (mt *prologmt*))
  (if (null pred)(setq pred (prolog-make-pred sent)))
  (let ((code (prolog-pred-source pred))
        (nxt (prolog-make-cycl sent mt))
        (new (append code (list nxt))))
    ;;(print (list code '=> new))(force-output)
    ;;(print (list *prologSource* pred (cons *theList* new)))
    ;;(prolog-assert-now (list *prologSource* pred (cons *theList* new)) mt)))
    (prolog-update-code pred new code mt)))


;;(fi-assert (list *prologSource* pred (cons *theList* new)) mt)))
;; (if (null nxt) (throw :UNEVALUATABLE (list :ERROR "unassertable" nxt mt)))
;;


;;(prolog-make-cycl '("member" ?item (?item . ??rest)) *prologMt*)


;; TODO - dump mt to FOPL
(defun prolog-listing (mt) 
  (let ((res nil))
    (if (mt? mt) (setq mt (ask-template '?pred (list *prologSource* '?pred '?first) mt)))
    (if (atom mt) (setq mt (list mt)))
    (mapcar #'(lambda (x) (setq res (append res (prolog-pred-source x)))) mt)
    (ret res)))


;;(prolog-make-cycl '(|member| ?item (?item . ??rest)) *prologMt*)
(if (prolog-pred-source '|member|) ()
  (progn 
    ;;(prolog-make-cycl '(|:-| ("member" ?item (??skip . ?rest)) ("member" ?item ?rest)) *prologMt*)
    (prolog-assertz '(|member| ?item (?item . ??rest)) *prologMt*)
    (prolog-assertz '(|:-| (|member| ?item (??skip . ?rest)) (|member| ?item ?rest)) *prologMt*)
    ))

#|

CODE 
CODESTR 
HTTPVARSIN 
MT 
MTSTRING 
PROLOGCMDSTR 


|#




(defun prolog-readmacro-star (stream char)
  (let ((*standard-input* (if (streamp stream) stream *standard-input*))(red (read_term (rch))))
    (princ "; Read: ")(princ red)(terpri)(force-output)
    (ret (values red))))


(set-macro-character #\% (get-macro-character #\;))
(set-macro-character #\$ #'prolog-readmacro-star)

(myprint "loaded")


(defun main (mainloop))



(defun fix-httpvars (httpvars)
  (if (null httpvars) ()
    (let ((item (car httpvars))(httpvars (cdr httpvars)))
      (if (and (consp item) (stringp (second item)) (stringp (car httpvars)))
          (fix-httpvars (cons (list (car item) (string-concat (second item) "|" (car httpvars))) (cdr httpvars)))
        (cons item (fix-httpvars httpvars))))))

(defun httpvar (name httpvars) 
  (let ((retval ()))
    (and httpvars name 
      (mapcar 
          #'(lambda (trm) 
              (cond 
               ((and (atom trm) (equal name trm)) (setq retval (cons t retval)))
               ((and (consp trm) (equal name (car trm))) (setq retval (cons (if (consp (cdr trm)) (cadr trm)(cdr trm)) retval)))
               (t nil)))
        httpvars ))
    retval))

;; todo my appoligies for using format - just quick way to maintain html insead of cyc's better html interface
;; test with 
#+CYC-BROWSER
(define-html-handler cb-prolog (httpvarsIn)
  (defun prolog-assert-now (sent &optional (mt *prologmt*)) )
  ;;(load "cb_prolog.lisp")
  (defun prolog-assert-now (sent &optional (mt *prologmt*)) (prolog-assert-now-real sent mt))
  (let ((*standard-output* *html-stream*)(read-error NIL)(httpvars (fix-httpvars httpvarsIn))
        (prologcmdstr (car (httpvar "prolog" httpvars)))
        (mtstring (car (httpvar "mt" httpvars)))
        (codestr (remove-comments (car (httpvar "code" httpvars))))
        (code :UNREAD)
        (mt NIL))
    (unwind-protect (progn (if (and (stringp mtstring) (CONSTANT-NAME-SPEC-P mtstring)) (foc mtstring))) t)
    (if (null mt) (setq mt *prologMt*))
    (setq mtstring (constant-name mt))
    (format t "<HTML>
 	<HEAD>
 		<TITLE>Prolog In Cyc</TITLE>
 		<META NAME='GENERATOR' Content='Microsoft Visual Studio .NET 8.0'>
 	</HEAD>
 	<BODY>
 		<FORM method='get' action='cg?cb-prolog' ID='prolog_form'>
 			<INPUT id='cb-prolog' type='hidden' name='cb-prolog'>
 			<BR>
 			Interpret in&nbsp;Mt &nbsp; <INPUT id='mt' type='text' name='mt' value='~a'> 
 			<INPUT id='listing' type='submit' value='Listing' name='prolog'> &nbsp; <a href='cg?cb-cf&c~a'>Examine ~a</a><BR>
 			<BR>
 			<TEXTAREA wrap=off id='code' name='code' rows='16' cols='100%'>" 
      mtstring (constant-internal-id mt) mtstring )
    (force-output)
    (unwind-protect
        (cond
         ((weak-string-equal prologcmdstr "Listing") 
          (setq code (prolog-listing mt))
          (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
          (princ (prolog-sentences-to-string code))
          )
         ;;(unwind-protect (ccatch :UNEVALUATABLE read-error (setq code (prolog-read codestr))) t)
         ((weak-string-equal prologcmdstr "call (?-)") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- ")(princ (prolog-to-string code))(princ ".")(terpri)(terpri)(force-output)
            (prolog-call code mt)))
         ((weak-string-equal prologcmdstr "assert(z)") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- assert(")(princ (prolog-to-string code))(princ ").")(terpri)(terpri)(force-output)
            (prolog-assertz code mt)))
         ((weak-string-equal prologcmdstr "asserta") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- asserta(")(princ (prolog-to-string code))(princ ").")(terpri)(terpri)(force-output)
            (prolog-asserta code mt)))
         ((weak-string-equal prologcmdstr "retract") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- retract(")(princ (prolog-to-string code))(princ ").")(terpri)(terpri)(force-output)
            (prolog-retract code mt)))
         ((weak-string-equal prologcmdstr "retractall") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- retractall(")(princ (prolog-to-string code))(princ ").")(terpri)(terpri)(force-output)
            (prolog-retractall code mt)))
         ((weak-string-equal prologcmdstr "listing") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- listing(")(princ (prolog-to-string code))(princ ").")(terpri)(terpri)(force-output)
            (prolog-retractall code mt)))
         (t (princ codestr)))
      
      (format t "</TEXTAREA><BR>
 			<BR>
 			<INPUT id='call' type='submit' value='call (?-)' name='prolog'> &nbsp; 
  <INPUT id='assert' type='submit' value='assert(z)' name='prolog'> &nbsp;
  <INPUT id='asserta' type='submit' value='asserta' name='prolog'> &nbsp;
  <INPUT id='retract' type='submit' value='retract' name='prolog'>
  <INPUT id='retractall' type='submit' value='retractall' name='prolog'>
  <INPUT id='eval-subLisp' type='submit' value='eval_SubLisp' name='prolog'>
  <INPUT id='eval-subProlog' type='submit' value='eval_SubProlog' name='prolog'>
  <INPUT id='Listing' type='submit' value='Listing' name='prolog'>
 			<BR>
 		</FORM>
 		<BR>
 		<FORM method='post' action='cg?cb-prolog-upload' ID='upload_form'>
 			<INPUT id='cb-prolog-upload' type='hidden' name='cb-prolog'>
 			Upload Prolog Source Code<BR>
 			<INPUT id='filename' type='file' name='filename'> &nbsp;&nbsp; <INPUT id='upload' type='submit' value='upload' name='upload'>
 			<P>
 			</P>
 		</FORM>
  DebugInfo:
 <pre>HTTPVARS = ~s</pre>
 	</BODY>
 </HTML>"
        httpvars)) 
    (force-output)))


(print "Done loading file")
(DEFINE MLG () (CUNWIND-PROTECT (progn (CSETF (READTABLE-CASE *READTABLE*) :PRESERVE) (MLG3)) (CSETF (READTABLE-CASE *READTABLE*) :UPCASE)))
