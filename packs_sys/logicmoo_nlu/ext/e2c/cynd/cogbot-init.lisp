;; (load "cynd/cogbot-init.lisp")
;; DMiles likes to watch
(in-package "CYC")
(define FORCE-PRINT (string) (print string) (force-output))
(force-print ";; start loading cynd/cogbot-init.lisp")
(csetq *silent-progress?* NIL) ;this adds some printouts
(csetq *dump-verbose* T) ;this adds some printouts
(csetq *recan-verbose?* T)
(csetq *tm-load-verbose?* T)
(csetq *sunit-verbose* T)
(csetq *sme-lexwiz-verbose?* T)
(csetq *verbose-print-pph-phrases?* T)
(csetq *it-failing-verbose* T)
(csetq *it-verbose* T)
(csetq *psp-verbose?* T)

(define foc (string) (ret (find-or-create-constant string)))

;; makes it more like common lisp .. (but leaks functions)
(defmacro lambda (args &rest body) (ret `#'(define ,(gensym "LAMBDA") ,args ,@body)))

(in-package :SLI)
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
     (cmd `(ke-assert-now ,(list 'quote sent) ,mt ,(fif (member :default flags) :default :monotonic) ,(fif (member :BACKWARD flags) :backward :forward))))
      (punless (eval  cmd)
        (format t "~&~s~& ; ERROR: ~s~&" cmd (HL-EXPLANATION-OF-WHY-NOT-WFF sent Mt))
        (force-output))))

(define cbnl-retract (sent &rest mts)
 (clet (askable template)
   (csetq mts (member-if #'mt? (flatten (cons mts (list *SMT*)))))
   (csetq sent (fif (equal (car sent) #$ist) sent (list #$ist '?sim-retract-Mt sent)))
   (csetq askable (fif (equal (car sent) #$ist) sent (list #$ist '?sim-retract-Mt sent)))
   (force-print `(ask-template ',sent ',askable ,*SMT*))
   (csetq mts (ask-template sent askable *SMT*))
   (print (length mts))
   (cdolist (sent mts) (ke-unassert-now (third sent)(second sent)))
   (ret mts)))

(set-the-cyclist "CycAdministrator")

(force-print ";; done loading cynd/cogbot-init.lisp")
