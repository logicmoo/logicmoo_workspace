(punless (fboundp 'force-print) 
  (define force-print (&rest body) (clet ((body (fif (equal 1 (length body)) (car body) body))(res (print body)))(force-output)(ret res))))
    ;;http://gothapedia.com/openCycCGI/cg?cb-subl-grep=&SEARCH=PARS&ARGS=&DO=GREP&STARTCOUNT=0&MAXDISPLAYCOUNT=1600&FUNCTIONS=FUNCTIONS&MODULES=MODULES
    ;;http://gothapedia.com/openCycCGI/cg?cb-start|cb-subloop-class&CLASS=PARSER
    ;;http://gothapedia.com/openCycCGI/cg?cb-start|cb-subloop-class&CLASS=PARSER
;;(csetq *ERROR-MESSSAGE* "No error message")
(defmacro onerror-warn (message &rest forms) (ret
    `(with-error-handler
        #'(lambda (&rest r) 
           (format t "<PRE><font color=red>~a ~a</font>~% in ~a ~a~&</PRE>" ,message *ERROR-MESSSAGE* ',forms r)
           (force-output))
        (progn ,@forms))))


(defmacro or (&rest body) (ret (fif body (fif (cdr body) `(pcond (,(car body)) ((or ,@(cdr body))))`,(car body)))))
(defmacro and (&rest body) (ret (fif body (fif (cdr body) `(pwhen ,(car body) (and ,@(cdr body)))`,(car body)))))
(defmacro not (&rest body) (ret `(cnot ,@body)))

(define sym< (a b) (ret (cand a b (string-lessp (write-to-string a)(write-to-string b)))))

(define insert-sorted (item sortedset)
  (punless item (ret sortedset))
  (punless sortedset (ret (values (list item) (list item))))
 (clet ((e ()) c)
  (cdo ((d sortedset e)(c (car sortedset)(car d))(e (cdr d)(cdr d))) (())
    (pwhen (equal item c) (ret sortedset))
    (pwhen (sym< item c)
        (rplacd d (cons c e))
        (rplaca d item)
        (ret (values sortedset d)))
    (pwhen (null e) (rplacd d (list item))(ret (values sortedset (cdr d) ))))))

(insert-sorted 'A '(B C))

(insert-sorted 'C '(A B C))

(insert-sorted '1 '(A B C))

(insert-sorted 'D '(A B C))

(defmacro push-sorted (item place) (ret 
 `(fif (null ,place) 
    (csetq ,place (list ,item))
    (pwhen ,item (second (multiple-value-list (csetq ,place (insert-sorted ,item ,place))))))))

#|
(csetq lotus-image (NEW-REMOTE-IMAGE "lotus" 3614))
(csetq lotus-connection (NEW-REMOTE-IMAGE-CONNECTION  lotus-image))
(REMOTE-IMAGE-CONNECTION-PORT lotus-connection)
(OPEN-REMOTE-IMAGE-CONNECTION lotus-connection) 
( REMOTE-IMAGE-CONNECTION-IMAGE  lotus-connection) 
(csetq lotus-INTERSECTION (NEW-KB-INTERSECTION  lotus-IMAGE))
(csetq lotus-INTERSECTION-result  (KB-INTERSECTION-COMPUTE  lotus-INTERSECTION))
(csetq lotus-DIFFERENCE-result (csetq lotus-DIFFERENCE (NEW-KB-DIFFERENCE    lotus-IMAGE)))

(csetq lotus-INTERSECTION-result (COMPUTE-REMOTE-IMAGE-KB-INTERSECTION "lotus" 3614 ))

(csetq lotus-DIFFERENCE-result (COMPUTE-REMOTE-IMAGE-KB-DIFFERENCE "lotus" 3614 ))

;;COMPUTE-REMOTE-IMAGE-KB-DIFFERENCE  

LOAD-JANUS-TRANSCRIPT  

SAVE-KBS-SPLICING-PARTITION  


(define save-big-cpart (&optional ( filename (cconcatenate (KB-VERSION-STRING) ".cpart")))
    (CB-SET-PARTITION-SAVE-TERMS (all-constants))
    (CB-PERFORM-PARTITION-SAVE  filename part-save )))

(defvar part-save ())
(define save-big-cpart (&optional ( filename (cconcatenate (KB-VERSION-STRING) ".cpart")))
  (pwhen (CB-PARTITION-SAVE-P part-save) (ret parts-save))
  (csetq part-save (MAKE-CB-PARTITION-SAVE))
  (DO-FORTS (c) (CB-ADD-PARTITION-SAVE-TERM part-save c))
  (CB-PERFORM-PARTITION-SAVE filename part-save))

(csetq *fboundps* ())(cdo-all-symbols (sym) (pwhen (sl::fboundp sym) (cpushnew sym *fboundps*)))(length *fboundps*)
   (CB-PERFORM-PARTITION-SAVE "allmts" part-save)
    (with-error-handler
        #'(lambda (&rest r) 
           (format t "<PRE><font color=red>~a </font></PRE>" *ERROR-MESSSAGE* )
           (force-output))
        (load-partition "titan-7116.cpart"))

(defvar part-save ())
(punless (CB-PARTITION-SAVE-P part-save) (csetq part-save (MAKE-CB-PARTITION-SAVE)))
(cdolist (mt (remove-duplicates (ask-template '?MT `(#$and (#$genls ?C #$Microtheory)(#$nearestIsa ?MT ?C) ) #$EverythingPSC)))
  (CB-ADD-PARTITION-SAVE-TERM part-save mt))


(defvar part-save ())
(punless (CB-PARTITION-SAVE-P part-save) (csetq part-save (MAKE-CB-PARTITION-SAVE)))
  (cdolist (mt (remove-duplicates (all-instances #$Microtheory))) (CB-ADD-PARTITION-SAVE-TERM part-save mt))


 (CB-ADD-PARTITION-SAVE-TERM part-save "all-mts.cpart")

(define save-big-cpart (&optional ( filename (cconcatenate (KB-VERSION-STRING) ".cpart")))
  (clet ((part-save (MAKE-CB-PARTITION-SAVE )))
   (cdolist (mt (ask-template '?MT `(#$and (#$isa ?MT ?C) (#$genls ?C #$Microtheory)) #$EverythingPSC))
      (CB-ADD-PARTITION-SAVE-TERM part-save mt))
   (CB-PERFORM-PARTITION-SAVE  filename part-save )))


(defvar part-save ())
(punless (CB-PARTITION-SAVE-P part-save) (csetq part-save (MAKE-CB-PARTITION-SAVE)))
(cdolist (mt '(#$isa #$genls #$comment #$genlMt #$Collection #$resultIsa #$termOfUnit)) (CB-ADD-PARTITION-SAVE-TERM part-save mt))
(cdolist (mt (remove-duplicates (all-instances #$Microtheory))) (CB-ADD-PARTITION-SAVE-TERM part-save mt))
(CB-PERFORM-PARTITION-SAVE 
  (cconcatenate (fif (SL::GETENV "COMPUTERNAME")  (SL::GETENV "COMPUTERNAME") (SL::GETENV "HOSTNAME") ) "-" (KB-VERSION-STRING) ".cpart")  
   part-save )


;;(assertion-count) 2700397

2700396

(get-assertion-dependants
DO-ASSERTION-DEPENDENTS
   
   

(ke-assert-now `(#$implies (#$and (#$isa ?MT ?MTTYPE)(#$isa ?MTTYPE #$MicrotheoryType))(#$isa ?MT #$Microtheory)) #$BaseKB  :monotonic :FORWARD)
(ke-assert-now `(#$isa ,(foc "MicrotheoryFunction") #$Collection) #$BaseKB)
(ke-assert-now `(#$genls ,(foc "MicrotheoryFunction") #$IndividualDenotingFunction) #$BaseKB)
(ke-assert-now `(#$implies (#$genls ?MTTYPE #$Microtheory)(#$isa ?MTTYPE #$MicrotheoryType)) #$BaseKB  :default  :forward)
(ke-assert-now `(#$implies (#$and (#$resultIsa ?FUNCT ?MTTYPE)(#$isa ?MTTYPE #$MicrotheoryType))(#$isa ?FUNCT #$MicrotheoryFunction))#$BaseKB :default  :forward)
(ke-assert-now `(#$implies (#$isa ?FUNCT #$MicrotheoryFunction) (#$resultIsa ?FUNCT #$Microtheory)) #$BaseKB :default  :forward)

(defvar part-save ())
(punless (CB-PARTITION-SAVE-P part-save) (csetq part-save (MAKE-CB-PARTITION-SAVE)))
(length (all-instances #$Microtheory))
(cdolist (mt '(#$isa #$genls #$comment #$genlMt #$Collection #$resultIsa #$termOfUnit)) (CB-ADD-PARTITION-SAVE-TERM part-save mt))
(DO-ASSERTION-DEPENDENT-ASSERTIONS (deps (find-assertion-by-id 2700396))
     (CB-ADD-PARTITION-SAVE-TERM part-save (second (assertion-el-formula deps))))


(DO-ASSERTION-DEPENDENT-ASSERTIONS (deps (find-assertion-by-id 2700396))
     (CB-ADD-PARTITION-SAVE-TERM  (second (assertion-el-formula deps))))

(cdolist (mt (remove-duplicates (all-instances #$Microtheory))) (CB-ADD-PARTITION-SAVE-TERM part-save mt))
(CB-PERFORM-PARTITION-SAVE 
  (cconcatenate (fif (SL::GETENV "COMPUTERNAME")  (SL::GETENV "COMPUTERNAME") (SL::GETENV "HOSTNAME") ) "-" (KB-VERSION-STRING) ".cpart")  
   part-save )


(define translate-cfasl (&optional (infile "units/7116/sbhl-modules.cfasl") ( outfile (cconcatenate infile ".text")))
     ( WITH-CFASL-STREAM-EXTENSIONS   
   (clet ((istream (OPEN-BINARY infile :input))(ostream (fif (stringp outfile)(OPEN-TEXT outfile :output)(fif (streamp outfile) outfile *STANDARD-OUTPUT*))))
   (cdo () ()
       (print (CFASL-INPUT istream) ostream)))))


 (translate-cfasl "units/7116/sbhl-modules.cfasl" nil)
clet ((*fstream* (OPEN-TEXT filestring :input))(kifterm nil)(CYCL nil)(*the-date* (the-date)))
  


|#

(define better-symbol (suggest current) (ret
   (punless suggest (ret current))
   (punless current (ret suggest))
   (pwhen (eql suggest current) (ret current))
   (pwhen (> (symbol-priority suggest)(symbol-priority current))
      (ret suggest))
   (ret current)))

(define symbol-priority (sym &optional (start 1))
  (pwhen (cor (null sym)(keywordp sym)) (ret 0))
  (pwhen (fboundp sym) (cinc start 5))
  (pwhen (boundp sym) (cinc start 3))
  (pwhen (member-if #'(lambda (a) (ret (search a (symbol-name sym)))) '("&" "#" "@" "%" "*" "_"))(cinc start 1))
  (ret start))

(define xfer-symbol (sym topack &optional (report (eq topack *CYC-PACKAGE*)))
    (punless sym (ret sym))
    (pwhen (keywordp sym) (ret sym))
    (punless (eq topack (symbol-package sym))
        (clet ((status (multiple-value-list (find-symbol (symbol-name sym) topack)))
              (found (car status)))
              (fif found
                (punless (eq sym found)
                  (pwhen (cor (fboundp sym)(boundp sym))
                  (clet ((foundp (symbol-package found)))
                    (pwhen (eq foundp topack)
                      (fif (cor (fboundp found)(boundp found))
                      (progn (terpri)(terpri) (force-print `(in  ,topack the symbol ,found from ,foundp is hiding ,sym from ,(symbol-package sym ))))
                      (progn 
                      (pwhen (equal (second status) ':external) 
                              (terpri)(terpri)(force-print `(unexport ,found ,foundp))
                              (unexport found foundp))
                      (force-print `(inside  ,topack the symbol ,found from ,foundp is being replaced with ,sym from ,(symbol-package sym )))
                      (csetq *greppable-symbols* (remove found *greppable-symbols*))
                      (with-error-handler #'(lambda ()) (unintern found topack))
                      (import sym topack)
                      (import sym topack)
                      (terpri)(force-print `(now ,(symbol-package (find-symbol (symbol-name sym) topack))))))))))
                (progn (pwhen report (force-print `(importing ,sym)) )
                       (import sym topack)(import sym topack))))))

(punless (find-package "EXPORTING") (make-package "EXPORTING"))
(define sublisp::test1 (a)
  (ret `(test1 ,a)))

(define cyc::test1 (a)
  (ret `(test1 ,a)))


(defvar *total-greppable-symbols* 0)
(defvar *new-greppable-symbols* :unknown)
(defvar *uninterned-greppable-symbols* ())
(defvar *greppable-symbols* '(() T ))


(define CHECK-ON-greppable-SYMBOLS (&rest any)
  (csetq *ERROR-MESSSAGE* NIL)
  (fresh-line)(format t "known symbols ~a ~%" (length *greppable-symbols*))(force-output)
    (CDO-ALL-SYMBOLS (sym)
      (xfer-symbol sym (fif (eq *CYC-PACKAGE* (symbol-package sym)) *SUBLISP-PACKAGE* *CYC-PACKAGE*))
      (cpushnew sym *greppable-symbols*))
    (fresh-line)(format t "sorting symbols ~a ~%" (length *greppable-symbols*))(force-output)
    (csetq *greppable-symbols* (sort *greppable-symbols* #'SYM<))
    (csetq *total-greppable-symbols* (length *greppable-symbols*))
    (fresh-line)(format t "found symbols ~a ~%" *total-greppable-symbols*)(force-output))
#|
(punless
  (equal 0 *new-greppable-symbols*)
  (format t "~% ;; searching for new *greppable-symbols* bacause ~s was increased by ~s ~%" *total-greppable-symbols* *new-greppable-symbols* )(force-output)
  (csetq *new-greppable-symbols*  0)
  (CDO-ALL-SYMBOLS (sym) (punless (keywordp sym) (pwhen (second (multiple-value-list (insert-sorted sym *greppable-symbols*)))(cinc *new-greppable-symbols*))))
  (punless (equal 0 *new-greppable-symbols*)
      (format t "~% ;; (re)sorting ~A *greppable-symbols* after finding ~s symbol(s) ~%" 
       (+ *new-greppable-symbols* *total-greppable-symbols*) *new-greppable-symbols*))
  (force-output)
  (csetq *total-greppable-symbols* (length *greppable-symbols*))
  (format t "~% ;; symbol count is now: ~s ~%" *total-greppable-symbols*)(force-output)))
  |#
 

(define read-from-vstring (str)
  (ret (and (stringp str) (cnot (empty-string-p str)) (read-from-string str))))

(defmacro HTML-LABELED-CHECKBOX (KEYWORDS)
  (clet ((named (format nil "~a" KEYWORDS))) (ret
    `(progn (csetq ,KEYWORDS (html-extract-input ,named httpvars))
    (HTML-CHECKBOX-INPUT ,named ,named ,KEYWORDS (format nil "id=~s" ,named))
    (format t "<label for=\"~a\"> ~a  </label>" ,named ,named)))))

(csetq STARTCOUNT 0)
(csetq FOUNDCOUNT 0)
(csetq MAXDISPLAYCOUNT 500)

(define cb-subl-grep-any (httpvars)
 (clet 
   ((*standard-output* *html-stream*)
    (sym ())
    (search (or (html-extract-input "SEARCH" httpvars) "CB-")) 
    (args (or (html-extract-input "ARGS" httpvars) ""))
    (STARTCOUNT (or (read-from-vstring (html-extract-input "STARTCOUNT" httpvars)) STARTCOUNT))
    (MAXDISPLAYCOUNT (or (read-from-vstring (html-extract-input "MAXDISPLAYCOUNT" httpvars)) MAXDISPLAYCOUNT))
    (KEYWORDS (or (html-extract-input "KEYWORDS" httpvars) NIL))
    (DETAILED (or (html-extract-input "DETAILED" httpvars) NIL))
    (symbols (or (html-extract-input "SYMBOLS" httpvars) KEYWORDS))
    (functions (or (html-extract-input "FUNCTIONS" httpvars) KEYWORDS))
    (handlers (or (html-extract-input "HANDLER" httpvars) KEYWORDS))
    (modules (or (html-extract-input "MODULES" httpvars) KEYWORDS))
    (STARTDISPLAYCOUNT (- STARTCOUNT MAXDISPLAYCOUNT))
    (DISPLAYCOUNT 0)
    (FOUNDCOUNT 0))
   (format t 
     "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.0 Transitional//EN'>
        <html>
            <head>
                <title></title>
                <meta name='GENERATOR' content='Microsoft Visual Studio .NET 7.1'>
                <meta name='vs_targetSchema' content='http://schemas.microsoft.com/intellisense/ie5'>
         <style>
        textarea { 
        scrollbar-face-color:#fff; 
        scrollbar-arrow-color:#fff; 
        scrollbar-highlight-color:#fff; 
        scrollbar-shadow-color:#fff; 
        scrollbar-3dlight-color:#fff; 
        scrollbar-darkshadow-color:#fff; 
        scrollbar-track-color:#fff; 
        } 
        </style>
        </HEAD><BODY>
        <table border=\"0\"  bordercolorlight=\"#3FFFFF\" bordercolordark=\"#FFFFFF\" align=\"left\" cellspacing=\"0\" cellpadding=\"0\">
         <tr><th  align=\"left\">")
    (format t "<FORM method='get' action='cg?cb-subl-grep'><INPUT id='cb-subl-grep' type='hidden' name='cb-subl-grep'>")
    (format t "search: <input type=text name='SEARCH' value=\"~a\">" search)
    (format t "args: <input type=text name='ARGS' value=\"~a\">" args)
    (format t "<INPUT id='DO' type='submit' value='GREP' name='DO'>")
    (format t " start: <input type=text name='STARTCOUNT' size=3 value=\"~a\">" (fif (< STARTDISPLAYCOUNT 0) 0 STARTDISPLAYCOUNT))
    (format t " max: <input type=text name='MAXDISPLAYCOUNT' size=4 value=\"~a\"><pre>HTTPVARS = ~s<BR>" MAXDISPLAYCOUNT httpvars)
    (HTML-LABELED-CHECKBOX functions)(HTML-LABELED-CHECKBOX handlers)
    (HTML-LABELED-CHECKBOX MODULES)(HTML-LABELED-CHECKBOX DETAILED)(HTML-LABELED-CHECKBOX symbols)(HTML-LABELED-CHECKBOX KEYWORDS)
    (format t "</form>")
    (force-output)
    (pwhen (cor (empty-string-p search)(cnot (stringp search))) (csetq string nil))
    (pwhen (cor (empty-string-p args)(cnot (stringp args))) (csetq args nil))
    (pwhen modules
     (DO-HL-MODULES (mod)
         (clet ((sym (HL-MODULE-NAME mod)))
          (cand (cor    (null search)
                  (clet ((sname (symbol-name sym))(ssname (cconcatenate " " sname " ")))
                    (cand  (cor (null search)(search sname search)(search search ssname))
                            (cor (null args)(clet ((ssname (write-to-string (MODULE-GET-NAME mod))))
                                (cor (search sname args) (search args ssname)))))))
           (format t " <a href='cg?cb-hl-module&:~a'>~s</a>~%" sym sym)))))
   (format t "</pre></th></tr>
    <tr><td><table border=\"0\"  bordercolorlight='#FF6FFF' bordercolordark='#FFFF5F' align=left cellspacing='0' cellpadding='0'>
    <tr><th align=\"left\" width=\"80px\"><i>Function</i></th><th align=\"left\" width=\"430px\"><i>Symbol</i></th><th>
     <p align=\"center\"><i>Arguments</i></p></th></tr>")
  (force-output)
  (pwhen (listp *greppable-symbols*)
    (cdolist  (sym *greppable-symbols*)
     (punless (cor (>= DISPLAYCOUNT MAXDISPLAYCOUNT)(member sym '(*greppable-symbols*)))
      (clet ((sname (symbol-name sym))(ssname (cconcatenate " " sname " ")))
       (cand 
         (cor
           (cand functions (fboundp sym)
                (cor (null search)(search search ssname))    
                (cor (null args)(search sname args)(search args ssname)))
           (cand handlers (HTML-HANDLER-FUNCTION? sym)
                (cor (null search)(search search ssname))    
                (cor (null args)(search sname args)(search args ssname)))
           (cand keywords 
                (cor (null search)(search search ssname))
                (cor (null args)(search sname args)(search args ssname)))
           (cand symbols (boundp sym) (cnot (keywordp sym))
                (cor (onerror-warn "unkownn"  (pwhen (symbol-value sym) (csetq ssname (write-to-string (symbol-value sym))))) T)
                (cor (null search)(search search ssname))
                (cor (null args)(search sname args)(search args ssname))))
        (onerror-warn "unkownn"  (cinc FOUNDCOUNT))
        (pwhen (> FOUNDCOUNT STARTCOUNT)
          (cinc DISPLAYCOUNT)
          (onerror-warn "unkownn" (html-symbol-apropos sym detailed))))))))
;;   (pwhen (onerror-warn "unkownn"  (<= (CDEC STARTCOUNT) 0)) (pwhen  (onerror-warn "unkownn"  (< 0 MAXDISPLAYCOUNT))(onerror-warn "unkownn"  (cdec MAXDISPLAYCOUNT))))))
    (format t " </table></td></tr><tr><td>")
    (format t "<FORM method='get' action='cg?cb-subl-grep'><INPUT id='cb-subl-grep' type='hidden' name='cb-subl-grep'>")
    (format t "search: <input type=text name='SEARCH' value=\"~a\">" (fif search search ""))
    (format t "args: <input type=text name='ARGS' value=\"~a\">" (fif  args  args ""))
    (format t "<INPUT id='DO' type='submit' value='GREP' name='DO'>")
    (format t " start: <input type=text name='STARTCOUNT' size=3 value=\"~a\">"  FOUNDCOUNT )
    (format t " max: <input type=text name='MAXDISPLAYCOUNT' size=4 value=\"~a\"><pre><BR>" MAXDISPLAYCOUNT)
    (HTML-LABELED-CHECKBOX functions)(HTML-LABELED-CHECKBOX handlers)
    (HTML-LABELED-CHECKBOX MODULES)(HTML-LABELED-CHECKBOX DETAILED)(HTML-LABELED-CHECKBOX symbols)(HTML-LABELED-CHECKBOX KEYWORDS)
    (format t "</form>")
    (format t "</td></tr></table></body></html>")
    (force-output)(ret httpvars)))

(define safe-mapcar (fun list)
  (pwhen (cand list fun)
     (cdolist (e list) 
       (funcall fun e))))

(define html-object-details (obj &optional (class (type-of obj)) depth)
 (punless obj (ret class))
   (pwhen (symbolp obj)
      (html-object-details (CLASSES-RETRIEVE-CLASS obj))
      (ret (html-symbol-apropos obj nil)))
   (pwhen (CLASS-P obj)
     (clet ((class obj)(obj (CLASS-NAME class)))
           (format t "<TR><TD><pre>CLASS:~%~a</pre></TD><TD><PRE>" obj)
           (format t "</PRE></TD><TD><PRE>")
           (display-class class)
           (format t "</PRE></TD></TR>")
           (format t "<TR><TD><pre>SUBCLASSES</pre></TD><TD><PRE>~a" obj)
           (format t "</PRE></TD><TD>")
           (cdolist (sclass (sort (CLASSES-FIND-ALL-SUBCLASSES class nil) #'sym<))
              (format t "<a href='cg?cb-subloop-class&CLASS=~s'>~s</a> " sclass sclass))
           (format t "</TD></TR>")))
   (ret nil))



     
(define html-symbol-apropos (sym &optional detailed)
 (pwhen detailed (ret (html-object-details sym)))
 (clet ((shown nil))
  (pwhen (boundp sym) (html-form-statement-cgi 'CSETQ sym (quotify (symbol-value sym)))
     (csetq shown 'CSETQ))
  (pwhen (HTML-HANDLER-FUNCTION? sym)
     (html-form-statement-cgi 'HTTPY sym (FUNCTION-symbol-ARGLIST sym))
     (csetq shown 'HTTPY))
  (pwhen (fboundp sym) 
    (punless (eq shown 'HTTPY) 
     (html-form-statement-cgi 'APPLY sym (FUNCTION-symbol-ARGLIST sym))
     (csetq shown 'APPLY)))
  (punless shown
     (html-form-statement-cgi 'SYM sym (cons (SYMBOL-PACKAGE sym) (multiple-value-list (find-symbol (symbol-name sym) (SYMBOL-PACKAGE sym))))))
  (ret shown)))

(define html-form-statement-cgi (id sym infoS)
  (clet ((sname (write-to-string sym)))
  (format t 
    "<FORM method='get' action='cg?cb-subl-grep'>
    <INPUT id='cb-subl-grep' type='hidden' name='cb-subl-grep' value='~a'>
    <INPUT id='cb-subl-grep' type='hidden' name='~a' value='~a'>
    <tr><td>(~a</td><td>~a</td><td>"
    id id sym id sname)
     (pcase id
       ((APPLY FUNCALL HTTPY)
           (onerror-warn (format nil "(html-form-statement-cgi ~a ~a ...)" id sym) (html-value-caller-form-element sym infoS)))
       (otherwise
           (onerror-warn (format nil "(html-form-statement-cgi ~a ~a ...)" id sym) (html-value-setter-form-element sym infoS))))
     (format t ")</td></tr></form>")))

(define html-value-caller-form-element (sym infoS)
  (pwhen (consp infoS) 
     (csetq infoS (remove '&OPTIONAL infoS))
     (csetq infoS (remove '&REST infoS))     
     (csetq infoS (write-to-string infoS))
     (csetq infoS (substring infoS 1 (- (length infoS) 1))))
  (format t "<INPUT type=text style='WIDTH: 800px' NAME=\"~a\" value=\"~a\">"  sym infoS))

(define html-value-setter-form-element (sym infoS)
   (pcond 
     ((equal T infoS) ;; True/False
       (format t "<SELECT style='WIDTH: 800px' id=\"~a\"  name=\"~a\" size='1'>" sym sym)
       (format t "<OPTION value='T'>T</OPTION><option value='NIL'>NIL</option></SELECT>"))
     ((stringp infoS) ;; Strings
        (format t "<input style='WIDTH: 800px'  type=text  NAME=\"~a\" id=\"~a\" value=\"~a\">" sym sym (HTML-ENCODE-TEXT (write-to-string infoS))))
     ((unreadable-p infoS) ;; Ugly objects (offer T/NIL)
       (format t "<SELECT style='WIDTH: 800px' id=\"~a\"  name=\"~a\" size='1'>" sym sym)
       (format t "<option value=\"~a\" selected>~a</option><option value='NIL'>NIL</option><OPTION value='T'>T</OPTION></SELECT>" (HTML-ENCODE-TEXT (write-to-string infoS)) (HTML-ENCODE-TEXT (write-to-string infoS))))
     (t  ;; Re-readables
        (format t "<input style='WIDTH: 800px' type=text  NAME=\"~a\" id=\"~a\" value=\"~a\">" sym sym (HTML-ENCODE-TEXT (write-to-string infoS))))))

;;(ret (equal obj (read-from-string (write-to-string obj)))))
(define unREADABLE-P (obj &optional (depth 10))
  (pwhen (cand (consp obj) (> depth 0))
    (ret (cor (unREADABLE-P (car obj) (- depth 1))(unREADABLE-P (cdr obj) (- depth 1)))))
  (ret (cnot (readable-p obj))))
        

 ;;(setq unre '(1 . 2)) (rplaca unre unre )


;;http://subl.cycipedia.com/openCycCGI/cg?cb-start|cb-subl-grep&search=PARSER
(define-cb-link-method :current-cb-subl-grep (&optional linktext)
  (punless linktext (csetq linktext "Grep for SubL Code"))
  (frame-link (html-princ "cb-subl-grep") (html-princ linktext))
  (ret nil))

(declare-cb-tool :current-cb-subl-grep "Grep for SubL Code" "All httpd" "Grep for SubL")


(define my-apply (fn rest)
  (punless rest
   (ret (funcall (function CMDCALL))))
  (clet ((argv (FUNCTION-symbol-ARGLIST cmdcall))(argc (length argv)))
     (pcond
        ((onerror-warn "unkownn"  (= argc 1)) (ret (funcall (function CMDCALL) (list rest))))
        ((onerror-warn "unkownn"  (= argc 1)) (ret (funcall (function CMDCALL) (list rest))))
        ((onerror-warn "unkownn"  (= argc 2))(ret (funcall (function CMDCALL) (list rest))))
        (t  (ret (funcall (function CMDCALL) rest))))))




(define export-handler (cmdname)
 (clet 
  ((cmdcall cmdname)
   (WRAPPER (intern (make-symbol (format nil "~s-WRAPPERX" CMDCALL))))
   (WRAPPER-KW (make-keyword (format nil "~s-WRAPPERX" CMDCALL)))   
   (HANDLER (intern (make-symbol (format nil "~s-HANDLERX" CMDCALL))))
   (argStr (list CMDCALL 'httpvars)))
   (pwhen (fboundp CMDCALL) (csetq argStr (cons CMDCALL (FUNCTION-symbol-ARGLIST CMDCALL))))
 (eval `(progn
   (print (list ',CMDCALL ',WRAPPER ',WRAPPER-KW ',HANDLER ))
    (define-html-handler ,HANDLER (rest)
       (clet ((*standard-output* *html-stream*))
        (format t 
         "<html><head><title>~s</title></head><body><h1>~s</h1>" ,(format nil "Running ~s" CMDCALL),(format nil "Running ~s" CMDCALL))
        (with-error-handler 
              #'(lambda (&rest errinfo)
                 (format t "<pre>ERROR: ~s ~s ~s ~%</pre>" *ERROR-MESSAGE* ,(quotify argStr) errinfo )
                 (force-output)(ret (car errinfo)))
             (print (my-apply ,(quotify CMDCALL) rest)))
        (format t "</body>
            </html>")))    
    (define-cb-link-method ,WRAPPER-KW (&optional linktext) (punless linktext (csetq linktext ,(format nil "~s" WRAPPER-KW))) (frame-link (html-princ ,(format nil "~s" HANDLER)) (html-princ linktext))(ret nil))    
    (declare-cb-tool ,WRAPPER-KW ,(format nil "~s" CMDCALL) ,(format nil "~s" CMDCALL) ,(format nil "Call ~s" argStr))
    (define ,WRAPPER (&optional linktext) (cb-link ,WRAPPER-KW linktext) (ret nil))))))


'(progn (macroexpand-1 '(EXPORT-HANDLER CB-HL-MODULE-SUMMARY)))
;;#$expansion #$ParserMadLibsFn (#$EvaluateSubLFn (#$ExpandSubLFn (NIL) (RTP-GENERATE-MADLIBS ':ARG1 ':ARG2)))):#$TemplateParsingMt

;; '(clet ((sym ())(pack ()))
;;     (cdolist (pack (LIST-ALL-PACKAGES))
;;        (CDO-all-SYMBOLS (sym)
;;           (pwhen (fboundp sym)
;;            (punless
;;             (search "ERX" (symbol-name sym))
;;             (EXPORT-HANDLER sym)
;;             (format t "<a href='cg?~s-HANDLERX'>~s</a><br>"  sym (cons sym (FUNCTION-symbol-ARGLIST sym  )))
;;             (export sym pack))))))


(define-html-handler cb-subl-grep (httpvars)
 (pwhen (probe-file "/cyc/top/subl-grep.lisp")(load "/cyc/top/subl-grep.lisp"))
 (cb-subl-grep-any httpvars))

(define-html-handler cb-any (httpvars)
 (cb-subl-grep-any httpvars))
 
#|
Warning: A SYMBOL at 0x402c2434 is not a structurep.
Warning: #$isa is not a valid sbhl-predicate-p
Warning: A SYMBOL at 0x4001001c is not a structurep.
Warning: #$isa is not a valid sbhl-predicate-p
Warning: A SYMBOL at 0x4001001c is not a structurep.
Warning: #$isa is not a valid sbhl-predicate-p
Warning: A SYMBOL at 0x4001001c is not a structurep.
Warning: #$isa is not a valid sbhl-predicate-p
Warning: A SYMBOL at 0x4001001c is not a structurep.
Warning: #$genlPreds is not a valid sbhl-predicate-p
Warning: A SYMBOL at 0x4001001c is not a structurep.
Warning: #$genlPreds is not a valid sbhl-predicate-p
Warning: A SYMBOL at 0x4001001c is not a structurep.
|#


(declare-cb-tool :current-cb-subloop-class "Subloop Interface" "subloop" "Current subloop Interface")

(define-cb-link-method :current-cb-subloop-class (&optional linktext)
 (punless linktext
      (csetq linktext "subloop"))
 (frame-link
  (html-princ "cb-subloop-class")
  (html-princ linktext))
 (ret nil))

(cpushnew '("cb-start|cb-subloop-class" . "subloop") *MAIN-MENU-LIST* #'EQUAL )
(define current-cb-subloop-class (&optional linktext) (cb-link :current-cb-smartworld linktext) (ret nil))

(csetq *OBJECT-CLASS* (CLASSES-RETRIEVE-CLASS 'object))

(define mexpand (code)
    (csetq code (macroexpand code))
    (pwhen (cand (consp code)(consp (cdr code))) (csetq code (cons (car code)(safe-mapcar #'mexpand (cdr code)))))
    (ret code))

#|
(defvar *all-struct-types* ())
(define all-struct-types ()
    (cdo-all-symbols (sym)   
        (clet (sv(sn (symbol-name sym))(l (length sn)))
         (pwhen (onerror-warn "unkownn"  (cand (> l 6)
           (boundp sym)
           (char= (char sn (- l 1)) #\*)
           (equal "*DTP-" (substring sn 0 5)))
           (csetq sn (substring sn 5 (- l 1)))
           (csetq sv (symbol-value sym))
           (fif (numberp sv)
                (cpushnew sym *all-struct-types*)
               (fif (cand (symbolp sv)(equal sn (symbol-name sv)))
                 (cpushnew sym *all-struct-types*))))))
         (ret *all-struct-types*))
                 ;;(cpushnew (cons sn sym) *all-struct-types* #'equal)))))))

(ALL-STRUCT-TYPES)
|#
                                         
(define-html-handler cb-subloop-class (httpvars)
 ;;(load " (load "/cyc/top/cb-prolog.lisp")
  (clet ((*standard-output* *html-stream*)(read-error NIL)
         (class (html-extract-input "CLASS" httpvars)))
   (pwhen (stringp class) (csetq class (read-from-string class)))
   (pwhen  (symbolp class) (CLASSES-RETRIEVE-CLASS class) (csetq class (CLASSES-RETRIEVE-CLASS class)))
   (punless (class-p class) (csetq class *OBJECT-CLASS*))
   (format t "<HTML><HEAD><TITLE>SUBLOOP CLASS:~s</TITLE><META NAME='GENERATOR' Content='Microsoft Visual Studio .NET 8.0'></HEAD><BODY>" class)

   (format t "<PRE>~%")
   (cdolist (sclass (safe-mapcar #'CLASSES-RETRIEVE-CLASS (cons (class-parent class) (sort (CLASSES-FIND-ALL-SUBCLASSES class nil) #'sym<))))
    (pwhen (class-p sclass)
      (format t "<a href='cg?cb-subloop-class&CLASS=~s'>~s</a>&nbsp " (class-name sclass)(class-name sclass))))
   (format t "~%")
   (display-class class)
   (format t "</PRE>")
   (format t "DebugInfo:<pre>HTTPVARS = ~s</pre></BODY></HTML>"httpvars)))

;;(lisp)
(csetq *AVAILABLE-CYCLIFICATION-PARSERS* (GET-CYCLIFICATION-PARSERS))
(pwhen (fboundp 'CHECK-ON-GREPPABLE-SYMBOLS) (defvar *CHECK-ON-GREPPABLE-SYMBOLS-PROCESS* (print (MAKE-PROCESS-WITH-ARGS "CHECK-ON-GREPPABLE-SYMBOLS" #'CHECK-ON-GREPPABLE-SYMBOLS))))

(force-print '(load "/cyc/top/subl-grep.lisp"))


