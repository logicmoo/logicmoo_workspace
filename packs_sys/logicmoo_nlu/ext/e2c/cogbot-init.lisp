;;;{{{DOC
;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Copyright (c) 2002 - 2010 Douglas R. Miles.  All rights reserved.
;;;
;;; @module COGBOT-INIT
;;; @features :COGBOT-NL
;;;
;;; @author dmiles
;;; @owner dmiles
;;;
;;; @created 2010/01/10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tools & utilities for Cogbot/NLParsing
;;
;; !!!!!!!!!!!!!!!
;; IMPORTANT NOTES:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External interface:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Depends on interface: 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;}}}EDOC

(in-package "CYC")



;; (load "e2c/cogbot-init.lisp")
;; DMiles likes to watch
(define FORCE-PRINT (string) (print string) (force-output))
(force-print ";; start loading e2c/cogbot-init.lisp")
(csetq *silent-progress?* NIL) ;this adds some printouts
(csetq *dump-verbose* T) ;this adds some printouts
#|
(csetq *recan-verbose?* T)
(csetq *tm-load-verbose?* T)
(csetq *sunit-verbose* T)
(csetq *sme-lexwiz-verbose?* T)
(csetq *verbose-print-pph-phrases?* T)
(csetq *it-failing-verbose* T)
(csetq *it-verbose* T)
(csetq *psp-verbose?* T)
(csetq *quote-char* #\")

(csetq *TASK-PROCESSOR-VERBOSITY* 1) ;;9
(csetq *SBHL-TRACE-LEVEL* 1) ;; 9
(csetq *ALLOW-EXTERNAL-INFERENCE* nil)
(csetq *EVALUATABLE-BACKCHAIN-ENABLED* t)
(csetq *ALLOW-FORWARD-SKOLEMIZATION* t)
(csetq *UNBOUND-RULE-BACKCHAIN-ENABLED* t)
(csetq *SUSPEND-SBHL-TYPE-CHECKING?* t)
(csetq *REPORT-DEFAULT-METHOD-CALLS?* t)
(csetq *EVAL-IN-API-TRACED-FNS* '(T EVAL))
(csetq *EVAL-IN-API-LEVEL* -1)
(csetq *DEFAULT-TRANSFORMATION-MAX* 10000) ;; normal 1000
(csetq *DEFAULT-RECURSION-LIMIT* 10000) ;; normal 1000
(csetq *ALLOW-EXPANDED-RULES-AS-OWL-EXPORT-CANDIDATES?* t)
(csetq *ALLOW-BACKWARD-GAFS* t)
(csetq *HL-FAILURE-BACKCHAINING* t)
(csetq *PERFORM-EQUALS-UNIFICATION* t)
(csetq *READ-REQUIRE-CONSTANT-EXISTS* t)
(csetq *PERFORM-UNIFICATION-OCCURS-CHECK* t)
(csetq *READ-REQUIRE-CONSTANT-EXISTS* T)
(csetq *SUSPEND-SBHL-TYPE-CHECKING?* T)

(csetq *EVALUATABLE-BACKCHAIN-ENABLED* nil)
(csetq *ALLOW-FORWARD-SKOLEMIZATION* nil)
(csetq *UNBOUND-RULE-BACKCHAIN-ENABLED* nil)
(csetq *REPORT-DEFAULT-METHOD-CALLS?* nil)
(csetq *ALLOW-EXPANDED-RULES-AS-OWL-EXPORT-CANDIDATES?* nil)
(csetq *ALLOW-BACKWARD-GAFS* nil)
(csetq *HL-FAILURE-BACKCHAINING* nil)
(csetq *PERFORM-EQUALS-UNIFICATION* nil)
|#

(define foc (string) (ret (find-or-create-constant string)))

;; makes it more like common lisp .. (but leaks functions)
;; (defmacro lambda (args &rest body) (ret `#'(define ,(gensym "LAMBDA") ,args ,@body)))

(in-package :SUBLISP)
#+Allegro
(defmacro sl:define (name arglist &body body &environment env)
  (let ((*top-level-environment* env))
    (must (valid-function-definition-symbol name) "~S ~S is not written in valid SubL." 'sl:define name)
   ;; (must (valid-define-arglist arglist) "~S arglist inappropriate --- ~S." name arglist)
   (must (valid-function-body name body) "~S body is not written in valid SubL." name)
   (unless (tree-member 'sl:ret body)
     (warn "~%Function ~S will not return a value." name))
   (multiple-value-bind (body declarations doc-string)
    (ex::strip-declarations-and-doc-string body)
    `(progn
      (when *translator-loaded*
        (funcall *translator-arglist-change-callback* ',name ',arglist))
      (defun ,name ,arglist
        ,@(when declarations
            `((declare ,@declarations)))
        ,@(when doc-string (list doc-string))
        (macrolet ((sl:ret (value)
                    `(return-from ,',name ,value)))
          ,@(when sl::*call-profiling-enabled?*
             `((sl::possibly-note-function-entry ',name)))
          ,@body
          nil))))))

(in-package :CYC)

(define cbnl-assert (sent &rest flags)
     (csetq flags (flatten (cons flags *VocabularyMt*)))
     (clet  ((mt (car (member-if #'MT? flags)))
             (cmd `(fast-assert-int ,(list 'quote sent) ,mt ,(fif (member :default flags) :default :monotonic) ,(fif (member :BACKWARD flags) :backward :forward))))
      (clet ((res (eval  cmd)))
       (punless res
        (format t "~&~s~& ;; HL ERROR: ~s~&" cmd (HL-EXPLANATION-OF-WHY-NOT-WFF sent Mt))
        (format t "~&;; EL ERROR: ~s~&~&" (WHY-NOT-WFF sent Mt))
        (force-output)
        (ret (values nil (WHY-NOT-WFF sent Mt))))
       (clet ((as (find-assertion-cycl sent mt)))
           (cyc-assert `(#$myReviewer ,as ,*COGBOTUSER*) #$BookkeepingMt)
          (ret (values as res))))))

(define cbnl-retract (sent &rest mts)
 (clet (askable template)
   (csetq mts (member-if #'mt? (flatten (cons mts (list *SMT*)))))
   (csetq sent (fif (equal (car sent) #$ist) sent (list #$ist '?Mt sent)))
   (csetq askable (fif (equal (car sent) #$ist) sent (list #$ist '?Mt sent)))
   (force-print `(ask-template ',sent ',askable ,*SMT*))
   (csetq mts (ask-template sent askable *SMT*))
   (print (length mts))
   (cdolist (sent mts) (ke-unassert-now (third sent)(second sent)))
   (ret mts)))

(csetq *SMT* #$EverythingPSC)
(define sentence-ids (sent &rest mts)
 (clet (result askable template)
   (csetq mts (member-if #'mt? (flatten (cons mts (list *SMT*)))))
   (csetq sent (fif (equal (car sent) #$ist) sent (list #$ist '?Mt sent)))
   (csetq askable (fif (equal (car sent) #$ist) sent (list #$ist '?Mt sent)))
   ;;(force-print `(ask-template ',sent ',askable ,*SMT*))
   (csetq mts (ask-template sent askable *SMT*))
   (cdolist (sent mts) 
    (clet ((call `(find-gaf ',(third sent) ',(canonicalize-hlmt (second sent))))(inm (force-print call))(gaf (eval call)))
     (pwhen gaf (cpushnew (assertion-id gaf ) result))))
   (ret result)))

'(sentence-ids '(#$isa ?CITY #$USCity))

(define sentence-clauses (sent &rest mts)
 (clet (result askable template)
   (csetq mts (member-if #'mt? (flatten (cons mts (list *SMT*)))))
   (csetq sent (fif (equal (car sent) #$ist) sent (list #$ist '?Mt sent)))
   (csetq askable (fif (equal (car sent) #$ist) sent (list #$ist '?Mt sent)))
   ;;(force-print `(ask-template ',sent ',askable ,*SMT*))
   (csetq mts (ask-template sent  askable *SMT*)) ;;4 nil nil 3000
  (ret mts)))

'(sentence-clauses '(#$isa ?CITY #$USCity))


(set-the-cyclist "CycAdministrator")

(force-print ";; done loading e2c/cogbot-init.lisp")

(in-package :CYC)
