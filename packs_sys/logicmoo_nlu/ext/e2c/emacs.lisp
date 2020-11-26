;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-

;;; -*- Package: CYC; Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package "CYC")

(SL::DEFVAR CYC::*LOADER-PACKAGE* *PACKAGE*)
(IN-PACKAGE "CYC")
(initialize-transcript-handling)
(csetq *thesaurus-subdirectories* '("init" "applications"))
(csetq *thesaurus-filename* "gw-thesaurus-init")
(csetq *thesaurus-filename-extension* "lisp")
(initialize-agenda)

(SL::LOAD "init/parameters.lisp")
(SL::LOAD "init/port-init.lisp")
(SL::LOAD "init/parameters.lisp")
(print '(SL::LOAD "setup/my-cyc-init.lisp"))


;;; This file should be the first thing LOADed when a cyc
;;; image is started.
(csetq *DEFAULT-CYCLIST-NAME* "CycAdministrator")
(SL::DEFVAR *init-emacs-LOADed* 'T)
(csetq *gc-reports* t)
(SL::DEFVAR *emacs-tcp-port* 4005)
(csetq *gc-reports* nil)
;; LISP parameters
(LOAD-system-parameters)

;;(IN-PACKAGE "SUBLISP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Intitally setup packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant lambda-list-keywords '(&optional &rest &key &allow-other-keys &aux &whole &BODY &environment))
(export 'lambda-list-keywords *PACKAGE*)

(define force-format (strm &rest BODY)(clet ((res (apply #'format (cons strm BODY))))(pif (streamp strm) (output-stream-p strm) (force-output))(ret res)))
(define force-princ (&rest BODY)(clet ((res (princ BODY)))(force-output)(ret res)))
(define force-print (&rest BODY) (clet ((res (print BODY)))(force-output)(ret res)))
(SL::DEFVAR *sticky-symbols* (append '( &BODY NIL) lambda-list-keywords))


(SL::DEFVAR KEYWORD-PACKAGE (find-package :KEYWORD))
;; This package is the common lisp implmentation of the cyc LISP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(SL::DEFMACRO trace-defun (&rest body) (ret
  `(ret (progn ,@(cddr body)))))

(SL::DEFVAR *LISP-PACKAGE* (make-package "COMMON-LISP" '("SUBLISP" "CYC") '("LISP" "EXT" "SYSTEM" "IMPL" "SYS" "INT" "CL" "INTERNAL")))
(SL::DEFVAR *SYSTEM-PACKAGE* *LISP-PACKAGE*)
(SL::DEFVAR *COMMON-LISP-PACKAGE* *LISP-PACKAGE*)
(SL::DEFVAR *default-package-use* '("CL" "SYS" "EXT" "CYC" "SL"))
(SL::DEFVAR *COMMON-LISP-USER-PACKAGE* (make-package "COMMON-LISP-USER" *default-package-use* '("USER")))

(SL::DEFVAR SYS::*SUBLISP-DEFMACRO* (find-symbol "DEFMACRO" :SUBLISP))
(SL::DEFVAR SYS::*SUBLISP-DEFINE* (find-symbol "DEFINE" :SUBLISP))
(SL::DEFVAR SYS::*SUBLISP-LAMBDA* (find-symbol "LAMBDA" :SUBLISP))
(SL::DEFVAR SYS::*LISP-DEFINE* (SL::intern (SL::make-symbol "DEFINE") :SYSTEM))
(SL::DEFVAR SYS::*LISP-DEFMACRO* (SL::intern (SL::make-symbol "DEFMACRO") :SYSTEM))

#|
(SL::IMPORT 'SL::DEFVAR :SYS)
(SL::IMPORT 'SL::INTERN :SYS)


(import (find-symbol "DEFMACRO" :SYS)  :CYC))

(SL::DEFMACRO SYS::DEFMACRO (symbol pattern &rest body)
  (csetq symbol (eval `,symbol))(force-print symbol pattern body)
  (ret (cons SYS::*SUBLISP-DEFMACRO* (cons symbol (cons pattern `((ret (progn ,@body))))))))

(SL::DEFMACRO SYS::DEFINE (symbol pattern &rest body)
  (csetq symbol (eval `,symbol))(force-print symbol pattern body)
  (ret (cons SYS::*SUBLISP-DEFINE* (cons symbol (cons pattern `((ret (progn ,@body))))))))

(SL::IN-PACKAGE :SYSTEM)
(SL::EXPORT SYS::*LISP-DEFMACRO* :CYC)
(SL::IMPORT SYS::*LISP-DEFMACRO* :CYC)

(SYS::DEFMACRO SYS::*LISP-DEFMACRO* (symbol pattern SL::&rest body)
  (SL::ret (SL::cons (symbol-value SYS::*SUBLISP-DEFMACRO*) `(,symbol ,pattern (SL::ret (trace-defun ,symbol ,pattern ,@body))))))
(SL::IN-PACKAGE (package-name CYC::*LOADER-PACKAGE*))
(SL::IMPORT SYS::*LISP-DEFMACRO* :CYC)
(print (list (symbol-package (find-symbol "DEFMACRO" )) (fboundp (find-symbol "DEFMACRO" :SYSTEM ))))

(SL::IN-PACKAGE :SYSTEM)
(SL::EXPORT SYS::*LISP-DEFINE* :CYC)
(SL::IMPORT SYS::*LISP-DEFINE* :CYC)

(SYS::DEFMACRO SYS::*LISP-DEFINE* (symbol pattern SL::&rest body)
  (SL::ret (SL::cons (symbol-value SYS::*SUBLISP-DEFINE*) `(,symbol ,pattern (SL::ret (trace-defun ,symbol ,pattern ,@body))))))
(SL::IN-PACKAGE (package-name CYC::*LOADER-PACKAGE*))
(SL::IMPORT SYS::*LISP-DEFMACRO* :CYC)
(print (list (symbol-package (find-symbol "DEFINE" )) (fboundp (find-symbol "DEFINE" :SYSTEM ))))
|#
  
(DEFMACRO ALTER-DEFINE (OLDSAVE ORGIPACKAGE NEWSAVE DEF-STR VARS &REST BODY)(RET 
  `(SL::PROGN (DEFVAR ,OLDSAVE (SL::FIND-SYMBOL ,DEF-STR ,ORGIPACKAGE))
    (SL::IN-PACKAGE :SYSTEM)
    (SL::DEFVAR ,NEWSAVE (SL::INTERN (SL::MAKE-SYMBOL ,DEF-STR) :SYSTEM))
    (SL::EXPORT ,NEWSAVE :CYC)
    (SL::IMPORT ',NEWSAVE :CYC)
    (SYS::DEFMACRO ,NEWSAVE ,VARS (SL::RET (SL::PROGN ,@BODY)))
    (SL::IN-PACKAGE (package-name CYC::*LOADER-PACKAGE*))
    (SL::IMPORT ',NEWSAVE :CYC)
    (SL::PRINT (SL::LIST (SL::SYMBOL-PACKAGE (SL::FIND-SYMBOL ,DEF-STR )) (SL::FBOUNDP (SL::FIND-SYMBOL ,DEF-STR :SYSTEM )))))))

(define import-symbol (name from &optional (to *PACKAGE*))
   (clet ((old  (find-symbol name TO)))
    ;;(pwhen (eq (symbol-package old) from) (ret (find-symbol name TO)))
    (SL::INTERN NAME TO)
    (with-error-handler #'(LAMBDA ()())(sl::unINTERN (find-symbol name TO) TO))
    (with-error-handler #'(LAMBDA ()())(sl::unINTERN (find-symbol name TO) TO))
    (sl::unINTERN 'NIL *CYC-PACKAGE*)
    (ret (values-list (list (SL::IMPORT '(find-symbol NAME FROM)) old)))))

;;;;;;;;;;;;;;;;;;------------7--6--5--4--3--2--1
(csetq *symbol-worths* (list '(NIL) '(NIL) '(NIL) '(NIL) '(NIL) '(NIL) '(NIL)))
(define symbol-worth (sym)
 (clet ((n (symbol-name sym)))
  (pcond
  ((cor (null sym) (keywordp sym)) (ret 0))
  ((fboundp sym) 
     (pwhen (boundp sym) (fif (symbol-value sym)(ret 7) (ret 6)))
     (ret 5))
  ((boundp sym) (fif (symbol-value sym)(ret 4) (ret 3)))
  ((member-if #'(lambda (a) (ret (search a n))) '("&" "#" "@" "%" "*" "_" ))(ret 2))
  (t (ret 1)))))

(define import-symbols (&optional (to *PACKAGE*))
    (clet ((packages (remove *KEYWORD-PACKAGE* (LIST-ALL-PACKAGES))))
     (cdo-all-symbols (s) 
       (clet ((f (symbol-package s))(n (symbol-name s))(w (symbol-worth s)))
        (pwhen (> w 1)
           (export s f) 
         (cdolist (p packages)
         (pwhen (> w (symbol-worth (find-symbol n p)))
           (import-symbol n f p))))))
    (force-FORMAT t "~& ;; done importing symbols to ~&")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages completed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(IN-PACKAGE :IMPL)
;;(lock-package :IMPL)
;;(cdo-symbols (s *PACKAGE*) (export s :IMPL))
;;(IN-PACKAGE (PACKAGE-NAME CYC::*LOADER-PACKAGE*))


(define reLOAD-planner ()
  (clet ((directory "./")
         (files '("action-planner" "planner-workarounds" "html-action-planner")))
    (cdolist (file files)
      (clet ((filename (format nil "~A~A~A" directory file ".lisp")))
        (SL::LOAD filename))))
  (csetq *forward-propagate-from-negations* nil)
  (ret nil))
;; (reLOAD-planner)
 



(define-html-handler cb-echo (httpvars)
 (clet ((*standard-output* *html-stream*))
  (ret (html-echo-args httpvars))))


(DEFINE-CB-LINK-METHOD :current-cb-echo (&optional linktext)
  (punless linktext
    (csetq linktext "Echo the http request"))
  (frame-link 
   (html-princ "cb-echo")
   (html-princ linktext))
  (ret nil))

(DECLARE-CB-TOOL :current-cb-echo "Echo the http request" "Echo the http request" "Echo the http request")
(SL::IN-PACKAGE "CYC")

(define current-cb-echo (&optional linktext) (cb-link :current-cb-echo linktext) (ret nil))

(define-html-handler cb-smartworld (httpvars)
   (clet ((*standard-output* *html-stream*)) 
    (format t 
     "<html>
        <head>
        <title>SmartWorld: A teeny-tiny SmallTalk interpreter with Cyc connections</title>
        </head>
        <BODY bgcolor='#00ffff'>
        <h1>SmartWorld: A teeny-tiny SmallTalk interpreter adapted to Cyc written in Java</h1>
        <applet width='300' height='200' codebase='http://www.daxtron.com'
          code='SmartWorldWeb.class' name='SmartWorldWeb' 
          archive='SmartWorld.zip,lib/cyc-common.jar,lib/OpenCyc.jar,lib/cyc-framework.jar,lib/jug.jar,lib/ViolinStrings.jar'>
        <b>Sorry, your browser doesn't appear to support applets.</b>
        </applet>
        </BODY>
        </html>"
        )))

(DEFINE-CB-LINK-METHOD :current-cb-smartworld (&optional linktext)
  (punless linktext
    (csetq linktext "Run SmartWorld"))
  (frame-link 
   (html-princ "cb-smartworld")
   (html-princ linktext))
  (ret nil))

(DECLARE-CB-TOOL :current-cb-smartworld "SmartWorld" "Run SmartWorld" "Current SmartWorld VM")

(define current-cb-smartworld (&optional linktext) (cb-link :current-cb-smartworld linktext) (ret nil))


(define-html-handler cb-factory (httpvars)
   (clet ((*standard-output* *html-stream*)) 
    (format t 
     "<?xml version='1.0' encoding='iso-8859-1'?>
<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN'
   'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>
<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en'>

<head><title>FACTory</title></head><BODY>
<p>
<!-- The deal here is, FireFox and other browsers will SL::LOAD the outer
	     object, and IE will SL::LOAD the inner one.  The different classids
	     tell IE to SL::LOAD the Sun JRE (if installed), and other browsers
     what file to SL::LOAD.  -->

<!--[if !IE]>-->
<object classid='java:com/cyc/project/butler/quickQuestion/ui/QuickQuestionApplet' archive='QuickQuestion-v2.jar' height='520' width='760'>
  <param name='type' value='application/x-java-applet;'/>
  <param name='code' value='com.cyc.project.butler.quickQuestion.ui.QuickQuestionApplet'/>
  <param name='codebase' value='http://207.207.9.186/java'/>

  <param name='questionServerMachine' value='webgame.cyc.com'/>
  <param name='questionServerPort' value='21212'/>
  <param name='traceCommands' value='true'/>
  <!--<![endif]-->
      <object classid='clsid:8AD9C840-044E-11D1-B3E9-00805F499D93' height='520' width='760'>
         <param name='type' value='application/x-java-applet;'/>
         <param name='code' value='com.cyc.project.butler.quickQuestion.ui.QuickQuestionApplet'/>
         <param name='archive' value='QuickQuestion-v2.jar'/>
         <param name='codebase' value='http://207.207.9.186/java'/>

         <param name='questionServerMachine' value='webgame.cyc.com'/>
         <param name='questionServerPort' value='21212'/>
         <param name='traceCommands' value='true'/>
      </object>
      <!-- end inner object -->
<!--[if !IE]>-->
</object>
<!--<![endif]-->
</p>
<p>
<i>&nbsp;&nbsp;<b>Beta version 1.2.4.</b> Copyright Cycorp, Inc. 2005 &ndash; 2006.<br/>

&nbsp;&nbsp;Use of this site indicates acceptance of our <a href='ToS.html'>Terms of Service</a>.</i>
</p>
</BODY></html>
")))

(DEFINE-CB-LINK-METHOD :current-cb-factory (&optional linktext)
  (punless linktext
    (csetq linktext "Run FACTory"))
  (frame-link 
   (html-princ "cb-factory")
   (html-princ linktext))
  (ret nil))

(DECLARE-CB-TOOL :current-cb-factory "FACTory" "Run FACTory" "Current FACTory Game")

(define current-cb-factory (&optional linktext) (cb-link :current-cb-factory linktext) (ret nil))


;; (EXPLODE  (STRING (IMPL '(32 96 106))))
;; (IMPL (EXPL  (STRING (IMPL '(32 96 106)))))
(DEFINE KMP () 
 (CSETF (READTABLE-CASE *READTABLE*) :UPCASE)
  (SL::LOAD "common.lisp")
  (SL::LOAD "kmp.subl")
  (SL::LOAD "common.lisp")
 ;;(CSETF (READTABLE-CASE *READTABLE*) :PRESERVE)
  )
(DEFINE V4 () 
 (CSETF (READTABLE-CASE *READTABLE*) :UPCASE)
  ;;(SL::LOAD "cb-prolog.lisp")
  (SL::LOAD "prologc.lisp")
 ;; (MLG3)
 ;;(CSETF (READTABLE-CASE *READTABLE*) :PRESERVE)
  )

(DEFINE MLG () (CUNWIND-PROTECT (progn (CSETF (READTABLE-CASE *READTABLE*) :PRESERVE) (MLG3)) (CSETF (READTABLE-CASE *READTABLE*) :UPCASE)))
(SL::IN-PACKAGE "CYC")
(INITIALIZE-POWERLOOM-HANDLING)

;;(EBMT-CREATE-INDEX-FOR-RELEASE  "httpd/htdocs/java/fet/lib/cyc-common.jar:/cyc/java/lib/xercesImpl-2.2.1.jar:/cyc/java/lib/lucene-1.4-final.jar" "/departments/nl/corpora/ebmt-release/" 8002 #$EBMTTrainingExamplesFromQLMt)

(define d3 ()(SL::LOAD "cynd/doom.lisp"))

'(cyc-build-world "units/7117/" "world/kb-7117-RH-ES3-x86_32-local.SL::LOAD")

;;(SL::DEFMACRO let (vars &rest BODY) (ret `(clet ,vars (prog-t ,@BODY))))
;;(SL::DEFMACRO prog-t (&rest BODY) (ret (fif BODY (fif (cdr BODY) `(progn (trace-warn ,(car BODY)) (prog-t ,@(cdr BODY)))`,(car BODY)))))
;;(define other-package (pack) (ret (fif (eq pack *CYC-PACKAGE*)  SYS::*SUBLISP-PACKAGE*  *CYC-PACKAGE*)))


;; This package export publically all symbols from the cyc impl,netnat

(IN-PACKAGE (PACKAGE-NAME CYC::*LOADER-PACKAGE*))

(pwhen (probe-file "subl-grep.lisp")  
  (SL::LOAD "subl-grep.lisp")
 (EXPORT-HANDLER 'CB-HL-MODULE-SUMMARY))
(pwhen (probe-file "kif-loader.lisp")  (SL::LOAD "kif-loader.lisp"))
'(pwhen (probe-file "trace.lisp")  (SL::LOAD "trace.lisp"))
'(pwhen (probe-file "common.lisp") (SL::LOAD "common.lisp"))


(SL::IN-PACKAGE "CYC")
(force-output)

#|
(INITIALIZE-REFORMULATOR)
|#

(CYC::IMPORT-SYMBOLS)




