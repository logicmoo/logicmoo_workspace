(punless (fboundp 'force-print) 
  (define force-print (&rest body) (clet ((body (fif (equal 1 (length body)) (car body) body))(res (print body)))(force-output)(ret res))))
(pwhen (probe-file "subl-grep.lisp")  (SL::LOAD "subl-grep.lisp") (EXPORT-HANDLER 'CB-HL-MODULE-SUMMARY))

#|

;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-

;;; -*- Package: CYC; Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package "CYC")


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

'(cyc-build-world "units/7117/" "world/kb-7117-RH-ES3-x86_32-local.LOAD")

;;(SL::DEFMACRO let (vars &rest BODY) (ret `(clet ,vars (prog-t ,@BODY))))
;;(SL::DEFMACRO prog-t (&rest BODY) (ret (fif BODY (fif (cdr BODY) `(progn (trace-warn ,(car BODY)) (prog-t ,@(cdr BODY)))`,(car BODY)))))
;;(define other-package (pack) (ret (fif (eq pack *CYC-PACKAGE*)  SYS::*SUBLISP-PACKAGE*  *CYC-PACKAGE*)))


;; This package export publically all symbols from the cyc impl,netnat



'(pwhen (probe-file "trace.lisp")  (SL::LOAD "trace.lisp")) 



(force-output)
#|
(INITIALIZE-REFORMULATOR)
|#
(define trace-warn (&rest code)
  (ret `(with-error-handler #'(lambda ()
   (force-format t "~%;; ERRROR: ~a~%;; code: ~a~%" *error-message* ',code)) (sl::progn ,@code))))

(defparameter *CURRENT-FILE-STREAM* ())

'(pwhen (probe-file "common.lisp")
 (punless  (MEMBER :CYC-COMMON-LISP sl::*features*)
  (csetq *CURRENT-FILE-STREAM* (OPEN-TEXT "common.lisp" :INPUT))
  (clet (expr)
  (cunwind-protect 
  (cdo () ((equal expr :EOF))
    (csetq expr (SL::read *CURRENT-FILE-STREAM* nil :EOF))
    (punless (equal expr :EOF)
      (trace-warn (force-print 'line (file-position *CURRENT-FILE-STREAM*)) (eval (force-print expr)))))
   (SL::close *CURRENT-FILE-STREAM*)))
   (csetq *CURRENT-FILE-STREAM* ())))
    
|#
(SL::IN-PACKAGE "CYC")

(csetq *USE-TRANSCRIPT-SERVER* nil)
(csetq *MASTER-TRANSCRIPT-LOCK-HOST* "transcript-server.cyc.com")
(pwhen (probe-file "kif-loader.lisp")  (SL::LOAD "kif-loader.lisp"))

