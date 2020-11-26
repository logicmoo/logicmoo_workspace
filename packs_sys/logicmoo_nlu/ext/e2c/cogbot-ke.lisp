;;;{{{DOC
;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Copyright (c) 2002 - 2010 Douglas R. Miles.  All rights reserved.
;;;
;;; @module COGBOT-KE
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
;; IMPORTANT NOTES:  !!!!!!!!! THIS IS POST KB LOAD !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External interface:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Depends on interface: COGBOT-INIT
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;}}}EDOC

(in-package "CYC")


;; (load "e2c/cogbot-ke.lisp")
(in-package "CYC")
(load "e2c/cogbot-init.lisp")
(force-print ";; start loading e2c/cogbot-ke.lisp")
;;;;==================================================
;;;; MICROTHEORY SETUP
;;;;==================================================
(in-package "CYC")
(csetq *SMT* #$EverythingPSC)
(define foc (string) (ret (find-or-create-constant string)))
(define FORCE-PRINT (string) (print string) (force-output))
(defparameter *UVMt* (foc "UniversalVocabularyMt"))
(defparameter *BASEKB* (foc "BaseKB"))
(defparameter *VocabularyMt* (foc "SimVocabularyMt"))

(csetq *UVMt* (foc "UniversalVocabularyMt"))
(csetq *BASEKB* (foc "BaseKB"))
(csetq *VocabularyMt* (foc "SimVocabularyMt"))

(cyc-assert `(#$isa ,*VocabularyMt*  #$VocabularyMicrotheory) *UVMt*)
(defparameter *COGBOTUSER* (foc "CogbotProgramCyclist"))
(csetq *COGBOTUSER* (foc "CogbotProgramCyclist"))
(cyc-assert `(#$isa ,*COGBOTUSER* #$Cyclist) #$UniversalVocabularyMt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WORK AROUNDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unknown-instance-indexed-expression? (exp) (ret exp))

#+Logicmoo
(cbnl-assert
`(#$implies 
       (#$termOfUnit 
           (#$WebPageCWFn ?STR) 
           (#$WebPageCWFn ?STR)) 
       (#$webPageURL 
           (#$AISForFn 
               (#$WebPageCWFn ?STR)) (#$URLFn ?STR))) #$BaseKB)


;; [Time: 269.468 secs] this one takes a while and might not be needed (creates info stores for each webpage reffed)
#+Ignore
(cbnl-assert
`(#$implies 
       (#$urlOfCW 
           (#$WebPageCWFn ?STR) (#$URLFn ?STR))
       (#$webPageURL 
           (#$AISForFn 
               (#$WebPageCWFn ?STR)) (#$URLFn ?STR))) #$BaseKB)

#+Logicmoo
(cbnl-assert
`(#$implies 
       (#$webPageURL 
           (#$AISForFn 
               (#$WebPageCWFn ?STR)) (#$URLFn ?STR))
       (#$urlOfCW 
           (#$WebPageCWFn ?STR) (#$URLFn ?STR))) #$BaseKB)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LEVERAGE SOME PARSERS (should we use viableProposedMeaning instead of proposedMeaning?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cbnl-assert 
 `(#$implies 
  (#$and 
    (#$parserDefinedForNLPhraseType ?PARSER ?PHRASE-TYPE) 
    (#$syntacticNodeCategory ?NODE ?PHRASE-TYPE) 
    (#$syntacticNodeString ?NODE ?STRING) 
    (#$parserRunToSpecificationWithInputStringReturnsCyclification ?PARSER 
      (#$DefaultParameterSpecificationFn ?PARSER) ?STRING ?CYCL)) 
  (#$proposedMeaning ?NODE ?CYCL))
        #$ParseTreeRepresentationVocabularyMt)


(cbnl-assert `(#$genlMt #$ParseTreeRepresentationVocabularyMt #$CycorpProductsMt) #$BaseKB)

#+CYC-EBMT
(cbnl-assert `(#$parserDefinedForNLPhraseType #$EBMTCyclifier #$NLSentence) #$CycorpProductsMt)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; logicmoo rkf reader (wrapper for RTP)  (should we use viableProposedMeaning instead of proposedMeaning?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cbnl-assert
`(#$implies
  (#$and
    (#$nlPhraseTypeForTemplateCategory ?PHRASE-TYPE ?RTP-TYPE)
    (#$syntacticNodeCategory ?NODE ?PHRASE-TYPE)
    (#$syntacticNodeString ?NODE ?STRING)
    (#$memberOfList ?PARSE ?PARSES)
        (#$evaluate ?PARSES
               (#$EvaluateSubLFn
                   (#$ExpandSubLFn
                       (?STRING ?RTP-TYPE)
                       (MAKE-EL-LIST  (logicmoo-rkf-reader ?STRING ?RTP-TYPE))))))
  (#$proposedMeaning ?NODE ?PARSE))
        #$ParseTreeRepresentationVocabularyMt)


(define-public logicmoo-rkf-test (text &optional (templ #$NPTemplate)(parsing-mt #$RKFParsingMt) (domain-mt #$RKFParsingMt))
  "@param TEXT string
   @param TEMPL TemplateType 
   @param PARSING-MT microtheory
   @param DOMAIN-MT microtheory
   @return list; a list of dates that can be parsed from TEXT"
  (clet (result)
      (csetq result (logicmoo-rkf-term-reader text parsing-mt domain-mt templ))
    (ret result)))

(define-public logicmoo-rkf-term-reader (text parsing-mt domain-mt &optional parse-specification)
  "@param PARSE-SPECIFICATION either a template category or an isa for what the term should be
          in cases where PARSE-SPECIFICATION is an arg-constraint, that arg-constraint will be
          used to determine what template category should be used during parsing, but that constraint
          will not be used to filter outputs"
  (clet ((template-categories (template-category-from-parse-spec parse-specification parsing-mt))
	 result)
   (with-full-spans-only-returned 
      (with-fully-resolved-parses
	(without-nltags-returned
	  (with-parsers-ordered-by-quality
	    (csetq result (logicmoo-rkf-phrase-reader text parsing-mt domain-mt template-categories)))
	    )))
    (pwhen result (ret result))
    ;;(with-full-spans-only-returned 
      ;;(with-fully-resolved-parses
	;;(without-nltags-returned
	  (with-parsers-ordered-by-quality
	    (csetq result (logicmoo-rkf-phrase-reader text parsing-mt domain-mt template-categories)))
	    ;;)))
	(pwhen result  (cdolist (var result)(print `(:SKIPPING ,template-categories :for ,var))))
    (ret nil)))


(define-public logicmoo-rkf-phrase-reader (text parsing-mt domain-mt
				       &optional parse-templates)
  (pwhen (null parse-templates)
    (csetq parse-templates *rkf-term-reader-default-templates*))
  (clet ((results ()))
    (clet ((*parsing-domain-mt* domain-mt))
      (cdolist (parse-template parse-templates)
	(csetq results
	       (rkf-uttrdr-merge-parse-results 
		results
		(rkf-utterance-reader text parse-template parsing-mt domain-mt)))))
    ;;insert code here to make sure we're using full-spans-only
    (pwhen (rkf-uttrdr-full-spans-only?)
      (csetq results (rkf-uttrdr-remove-sub-parses results text)))
    (ret results)))

;;(logicmoo-rkf-reader "the year of Starwar's production is 1979" #$STemplate)

(define-public logicmoo-rkf-reader (text &optional (templ #$STemplate)(parse-tree-mt :save)(parsing-mt #$RKFParsingMt) (domain-mt #$RKFParsingMt))
  "@param TEXT string
   @param TEMPL TemplateType 
   @param PARSING-MT microtheory
   @param DOMAIN-MT microtheory
   @return list; a list of dates that can be parsed from TEXT"
  (clet (result)
   (while-saving-rtp-parse-into-kb
    (with-fully-resolved-parses
      (csetq result (top-level-parses (logicmoo-rkf-term-reader text parsing-mt domain-mt templ)))))
    (ret result)))

(csetq *psp-max-edges-per-chart* 1000000) ;; x10
(csetq *psp-max-edges-per-span* 102120) ;; x10

(define try-rkf-reader (string &optional (parsing-mt #$RKFParsingMt) (domain-mt #$RKFParsingMt))
;; (try-rkf-reader "see two books")
;;(try-rkf-reader "have not see two books")
  (clet (answers)
    (cdolist
       (temp (query-template '?T '(#$genls ?T #$ParsingTemplateCategory) #$UniversalVocabularyMt))
      (punless  (eq #$StringTemplate temp)
       (clet ((res (logicmoo-rkf-reader string temp :save parsing-mt domain-mt)))
         (pwhen res (cdolist (r res)
	 (cpush (list temp r) answers))))))
     (ret answers)))

(define parse (string &optional (parsing-mt #$RKFParsingMt) (domain-mt #$RKFParsingMt))
  (clet ((res (try-rkf-reader string parsing-mt domain-mt)))
    (cdolist (p res)
      (print `(:found ,p)))
    (ret res)))


(define parsert (string temp) (ret (logicmoo-rkf-reader string temp)))

;;(GET-CONTENTMODEL-MT "I see")
;;(GET-PARSE-MT "I see two books sitting on a shelf")
;;(GET-CONTENTMODEL-MT "The country is nominally a democracy and is formally named the Republic of Angola (Portuguese: Republica de Angola).")
;;(GET-CONTENTMODEL-MT  "Bob likes summer storms.")
(define get-contentmodel-mt (sent)
  (clet ((cm (car (ask-template '?cm `(#$contentModelForString ?cm ,sent) #$EverythingPSC))))
   (pwhen cm (ret cm))
  (ret (car (ask-template '?cm 
     `(#$and 
       (#$genlMt ?mt (#$ContentMtOfLinkageFn (#$NthLinkParserLinkageFn ,sent ?N)))
       (#$genlMt ?cm ?mt)
       ;;(#$genlMt ?te ?cm)
       ;;(#$isa ?te #$TextualEntailmentProblemSolvingContext)
       (#$isa ?cm #$ContentModelMicrotheory)
       (#$isa ?mt #$ParseTreeRepresentationMicrotheory)) #$BaseKB)))))

;; (FOCPARSE "Bob likes summer storms")
;; (FOCPARSE "I see two books sitting on a shelf")
;; (logicmoo-rkf-reader "I see two books sitting on a shelf" #$STemplate)
(define focparse (sent) 
  (clet (restof assrt (cm (get-contentmodel-mt sent)))
    (punless cm (prep-cyclify sent) (ret (focparse sent)))
      (clet ((res (logicmoo-rkf-reader sent #$STemplate)))
      (punless res (ret cm))
      (csetq restof (expand-parse (cons #$and res)))
      (csetq assrt (cbnl-assert restof cm))
      (pwhen assrt (ret restof))
            
     (cdolist (p res)
       (csetq p (expand-parse p))
       (print `(:found ,p))         
        (cbnl-assert p cm)
	;;(cbnl-assert `(#$allParseResultsForContentModel ,cm ,(make-el-list (list p))) cm)
        
       )
       ;;(cbnl-assert `(#$allParseResultsForContentModel ,cm ,(make-el-list res)) cm)
    (ret res))))

;;(expand-parse '(ret (cdr (make-el-list p))))
(define expand-parse (p)
  (clet (new)
  (pwhen (consp p)
	(csetq p (cdr (make-el-list p))))
  (csetq p (expand-parse-n p))
  (csetq new (cyc-substitute-formula '?THESENTSUBJ #$TheSentenceSubject p))
  (punless (equal new p)
    (csetq p `(#$thereExists ?THESENTSUBJ ,new)))
 (ret p)))

;;(expand-parse-n '(:foo (#$SubstituteFormulaFn 1 2 (1 2 3))))
(define expand-parse-n (p)
  (pwhen (consp p)
    (clet ((a (car p))(d (cdr p)))
     (pcase a
       (#$SubstituteFormulaFn
         (ret (expand-parse-n (apply #'cyc-substitute-formula d))))
	           
       (t ))
     (ret (cons (expand-parse-n a)(expand-parse-n d)))))
  (pwhen (EL-VARIABLE-P p) (ret p))
  (pwhen (numberp p) (ret p))
  (pwhen (keywordp p) (ret (intern (cconcatenate "?" (symbol-name p)))))
  (ret p))

#|
(

  (clet ((res (try-rkf-reader string parsing-mt domain-mt)))
    (cdolist (p res)
      (print `(:found ,p)))
    (ret res)))
|#


;;(try-rkf-reader "I see two books sitting on a shelf")
;; example (prep-cyclify "Bill Clinton likes cars")
;; example (prep-cyclify "I see you")
(define cycl-parse-tree (sent) 
   (ret (first 
    (ask-template '?Linkage `(#$genlMt (#$ContentMtOfParseTreeFn  ?Linkage)  
     ,(second (multiple-value-list (assert-linkage sent T)))) #$UniversalVocabularyMt))))

;;(ask-template `?S `(#$syntacticNodePennTag ?S #$S-PennTag) `(#$ContentMtOfParseTreeFn #$LinkageCycParseTree-35))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SENTENCE BREAKERS FOR PARAGRAPHS for #$stringSentenceList
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define first-sent-pos (remaining &optional (start 0))
 (clet (
   (pos (search ". " (SUBSTITUTE  #\. #\! (SUBSTITUTE  #\. #\? remaining)) #'eql #'identity 0 nil start nil)))
    (pwhen (null pos) (ret nil))
    (pwhen (zerop pos) (ret nil))
    (pwhen (UPPER-CASE-P (nth (1- pos) remaining)) (ret (first-sent-pos remaining (1+ pos))))
    (ret pos)))

;; (split-string-sentences "One b c! Two A. b? Three 3 3. Four four. Five five.")
(define split-string-sentences (string  &optional (delimiter ". "))
  "Splits STRING based on DELIMITER.
   @param STRING stringp; the string to split.
   @param DELIMITER stringp; the string to split on;
   @returns listp
   @note This is the inverse of join-strings.
   @owner dmiles"
  (clet ((ans '())
         (snum 0)
	 (remaining string)
	 (pos (first-sent-pos string)))
    (while pos
      (cinc snum)
      (cpush (string-trim ". " (substring remaining 0 (+ pos 1))) ans)
      (csetq remaining (substring remaining (+ pos (length delimiter))))
      (csetq pos (first-sent-pos remaining)))
    (cinc snum)
    (cpush (string-trim ". " remaining) ans)
    (csetq remaining nil)
    (ret  (MAKE-EL-LIST (nreverse ans)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; stringSentenceList/3 (uses the above #'split-string-sentences)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cbnl-assert `(#$isa ,(foc "stringSentenceList") #$BinaryPredicate) #$UniversalVocabularyMt)
(cbnl-assert `(#$isa #$stringSentenceList #$FunctionalPredicate) #$UniversalVocabularyMt)
(cbnl-assert '(#$and
(#$argIsa #$stringSentenceList 1 #$CharacterString)
(#$argIsa #$stringSentenceList 2 #$List)
(#$functionalInArgs #$stringSentenceList 2)
) #$BaseKB)


(cbnl-assert `(#$implies (#$isa ?TEXT #$CharacterString)
(#$stringSentenceList ?TEXT (#$EvaluateSubLFn (#$ExpandSubLFn (?TEXT) (split-string-sentences ?TEXT))))
) #$BaseKB)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; nthInList (functionCorrespondingPredicate to NthInListFn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(foc "nthInList")
(cbnl-assert '(#$and (#$isa #$nthInList #$TernaryPredicate) (#$isa #$nthInList #$FunctionalPredicate)) #$UniversalVocabularyMt)

(cbnl-assert '(#$and
(#$isa #$nthInList #$FunctionalPredicate)
(#$arity #$nthInList 3)
(#$functionalInArgs #$nthInList 3)
(#$argIsa #$nthInList 1 #$List)
(#$argIsa #$nthInList 2 #$PositiveInteger)
(#$argIsa #$nthInList 3 #$Thing)
(#$backchainEncouraged #$nthInList)
(#$functionCorrespondingPredicate-Canonical #$NthInListFn #$nthInList 3)
) #$UniversalVocabularyMt)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; subSentenceOfAIS/5 (uses paragraph->senetences breaker)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cbnl-assert `(#$isa ,(foc "subSentenceOfAIS") #$Predicate) #$UniversalVocabularyMt)

(cbnl-assert '(#$and
;;(#$backchainRequired #$subSentenceOfAIS)
(#$arity #$subSentenceOfAIS 6)
(#$argIsa #$subSentenceOfAIS 1 #$LinguisticExpressionPeg-CompleteUtterance)
(#$argIsa #$subSentenceOfAIS 2 #$PositiveInteger)
(#$argIsa #$subSentenceOfAIS 3 #$PositiveInteger)
(#$argIsa #$subSentenceOfAIS 4 #$CharacterString)
(#$argIsa #$subSentenceOfAIS 5 #$Microtheory)
(#$argIsa #$subSentenceOfAIS 6 #$ParseTree)
) #$UniversalVocabularyMt)

;; read out some PEGs
(cbnl-assert '(#$implies
(#$and 
  (#$isa ?PEG #$LinguisticExpressionPeg-CompleteUtterance) 
  (#$ist ?CMT (#$originalPhrase ?PEG ?TEXT)) 
  (#$occurrenceNOfStringInStructure ?PEG 1 ?TEXT2 (#$NthOccurrenceOfStructureTypeInStructureFn ?PN #$NLParagraph ?AIS))
  (#$nthInList (#$EvaluateSubLFn (#$ExpandSubLFn (?TEXT) (split-string-sentences ?TEXT))) ?SN ?SENT))
(#$subSentenceOfAIS ?PEG ?PN ?SN ?SENT ?CMT ?AIS))
#$BaseKB)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PREP-CYCLIFY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (prep-cyclify "In the room, I see two books sitting on a shelf")
;; (prep-cyclify "While the dulcimer may not have a large fan base in young people, many music teachers consider them to be especially educational")
#|

 creates genlMts:

  CyclificationCollectorMt-* [TextualEntailmentProblemSolvingContext]
    -> CyclifierContentModelMt-* [ContentModelMicrotheory]
        -> (ContentMtOfParseTreeFn LinkageCycParseTree-*)  [ParseTreeRepresentationMicrotheory]
          -> (ContentMtOfLinkageFn (NthLinkParserLinkageFn ?TEXT 0)) ;; Holds the actual Link parser output

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define prep-cyclify (sent0)
  (clet 
     ((sent (SUBSTITUTE  #\Space  #\, (SUBSTITUTE  #\[  #\( (SUBSTITUTE  #\]  #\) sent0))))
     (tm (cycl-parse-tree sent))
      (cmmt (get-cyclifier-interps-for-string sent))) ;;could be instead create-cyclifier-content-model
    (cbnl-assert `(#$genlMt ,cmmt (#$ContentMtOfParseTreeFn ,tm)) #$BaseKB)
    (cbnl-assert `(#$genlMt (#$ContentMtOfParseTreeFn ,tm) #$ParseTreeRepresentationVocabularyMt) #$BaseKB)
    (cbnl-assert `(#$genlMt ,cmmt #$ParseTreeRepresentationVocabularyMt) #$BaseKB)
    (ret (list cmmt tm))))

;;(get-cyclifier-interps-for-string "I see two books sitting on a shelf")
;;(get-cyclification-parsers)
;;(get-cycls-for-sentence  "I see two books sitting on a shelf")
;;(create-cyclifier-content-model "I see two books sitting on a shelf")
;; (tep-premise-context-from-string "I see two books sitting on a shelf")
;;(tep-sentence-content-from-string "I see two books")
;;(get-cycls-for-sentence "Bill Clinton likes cars")
;; (prep-cyclify "I see two books sitting on a shelf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DOCUMENTS-CYCLIFY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (l2r-read-document '(#$SentenceDescribingObjectFn #$BillClinton "Bill Clinton likes cars."))
;;(cbnl-assert '(#$isa (#$ResourcePCWAtURIFn "file://NLTest.txt") #$WorldWideWebPage-PCW ) #$BaseKB)
;; (isa-in-any-mt? '(#$ResourcePCWAtURIFn "file://NLTest.txt") #$WorldWideWebPage-PCW)
;; (l2r-read-document '(#$WebPageCWFn "http://en.wikipedia.org/wiki/Appalachian_dulcimer"))
;; (l2r-read-document '(#$WebPageCWFn "http://en.wikipedia.org/wiki/U.S._Securities_and_Exchange_Commission"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; afterAdding #$originalPhrase -> subSentenceOfAIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public ADD-ORIGINALPHRASE (type assertion &rest rest) 
  (intern-originalPhrase assertion))

(cbnl-assert '(#$afterAdding #$originalPhrase 
  (#$SubLQuoteFn ADD-ORIGINALPHRASE)) #$BaseKB)

(defvar *peg-originalPhrase-lock* (make-lock "*peg-originalPhrase-lock*"))
(defvar *peg-originalPhrase* (make-hash-table 50))

(define-public intern-originalPhrase (assertion)   
 (clet ((formula  (assertion-formula assertion))
	(sn 0)
        (mt  (assertion-mt assertion))
	(peg  (second formula))
	(string  (third formula)))
    ;;(punless (stringp string)
    (with-lock-held (*peg-originalPhrase-lock*) 
         ;; already doing
	(pwhen (gethash peg *peg-originalPhrase* nil) 
	  (print `(:already-have ,peg))
	   (ret t))
	(sethash peg *peg-originalPhrase* peg))

    ;; not a sentence 
    (punless (isa-in-any-mt? peg #$LinguisticExpressionPeg-CompleteUtterance) (ret nil))

    ;; already done 
    (pwhen (query-template 
	  `(?sn ?str) `(#$subSentenceOfAIS ,peg ?sn ?str ?a1 ?a2) mt
           ;;Transitivity is implemented via forward rule:
           '(:max-transformation-depth 0 :answer-language :hl)) (ret t))


	(print `(intern-originalPhrase ,peg ,string ,mt))  
	(cdolist (var (cdr (split-string-sentences string)))
	  (cinc sn)
	  (clet (
	     (cp (prep-cyclify var))
	     (cmmt (first cp))
	     (pt (second cp))
	     (sent `(#$and (#$subSentenceOfAIS ,peg 0 ,sn ,var ,cmmt ,pt) (#$genlMt ,cmmt ,mt )(#$genlMt ,mt ,cmmt ))))
	  (print sent)
	  (cbnl-assert sent mt)))
	  ))

  

 ;; (cbnl-assert '(#$originalPhrase #$L2R-SentenceLevelPeg-2 "The Ap dulcimer manufacture is often conducted by small, family-run businesses located in the American South and particularly in Appalachia. John Bailey's book provides instructions for constructing a dulcimer:") '(#$ContentModelForPegFn #$L2R-SentenceLevelPeg-2) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CB-ECHO WEBSERVER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-html-handler cb-echo (httpvars)
 (clet ((*standard-output* *html-stream*))
  (ret (html2-echo-args httpvars))))


(define-cb-link-method :current-cb-echo (&optional linktext)
  (punless linktext
    (csetq linktext "Echo the http request"))
  (frame-link 
   (html-princ "cb-echo")
   (html-princ linktext))
  (ret nil))

(define-html-handler html2-echo-args (args)  
  (html-simple-document
   ("Echo of ARGS 2")
   (html-dummy-form
    (cb-back-button :self "Back2")
    (html-pre
     (cdolist (arg args)
       (html-prin1 arg)
       (html-terpri)))))
  (ret nil))


(declare-cb-tool :current-cb-echo "Echo the http request" "Echo the http request" "Echo the http request")

(define current-cb-echo (&optional linktext) (cb-link :current-cb-echo linktext) (ret nil))


(define reload-cogbot-parser () (load "e2c/cogbot-parser.lisp") )

'(defvar *run-once-reload-cb* (make-process "Reload Cogbot Parser" #'reload-cogbot-parser))


(force-print ";; done loading e2c/cogbot-ke.lisp")









#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EXTRA STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cbnl-assert '(#$implies
(#$and
  (#$isa ?PEG #$LinguisticExpressionPeg-CompleteUtterance) 
  (#$originalPhrase ?PEG ?TEXT)
  (#$stringSentenceList ?TEXT ?LIST)
  (#$nthInList ?LIST ?SN ?SENT))
  (#$subSentenceOfAIS ?PEG ?SN ?SENT ))
#$BaseKB)



(#$stringSentenceList ?SENT  (#$EvaluateSubLFn (#$ExpandSubLFn (?TEXT) (split-string-sentences ?TEXT))))

(cbnl-assert '(#$implies
(#$and 
  (#$isa ?PEG #$LinguisticExpressionPeg-CompleteUtterance) 
  (#$occurrenceNOfStringInStructure ?PEG 1 ?TEXT ??OCUR)
  (#$stringSentenceList ?TEXT ?LIST)
  (#$nthInList ?LIST ?SN ?SENT))
(#$subSentenceOfAIS ?PEG ?SN ?SENT)
)
#$UniversalVocabularyMt)



(cbnl-assert '(#$implies
(#$and 
  (#$isa ?PEG #$LinguisticExpressionPeg-CompleteUtterance) 
  (#$ist ?CMT (#$originalPhrase ?PEG ?TEXT)) 
  (#$occurrenceNOfStringInStructure ?PEG 1 ?TEXT2 
     (#$NthOccurrenceOfStructureTypeInStructureFn ?PN #$NLParagraph ?AIS))
  (#$nthInList (#$EvaluateSubLFn (#$ExpandSubLFn (?TEXT) (split-string-sentences ?TEXT))) ?SN ?SENT))
(#$subSentenceOfAIS ?PEG ?CMT ?PN ?SN ?SENT ?AIS))
#$BaseKB)



(#$NthOccurrenceOfStructureTypeInStructureFn ?SN #$NLSentence
  (#$NthOccurrenceOfStructureTypeInStructureFn ?PN #$NLParagraph ?AIS))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SubPegOfPegFn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(foc "SubPegOfPegFn")
(cbnl-assert '(#$and
(#$isa #$SubPegOfPegFn #$TernaryFunction)
(#$arity #$SubPegOfPegFn 3)
(#$argIsa #$SubPegOfPegFn 1 #$LinguisticExpressionPeg)
(#$argIsa #$SubPegOfPegFn 2 #$Microtheory)
(#$argIsa #$SubPegOfPegFn 3 #$Thing)
(#$resultIsa #$SubPegOfPegFn #$LinguisticExpressionPeg)

) #$UniversalVocabularyMt)


;; MAYBE NOT
(cbnl-assert '(#$implies (#$and (#$isa ?PEG #$LinguisticExpressionPeg-CompleteUtterance) 
(#$occurrenceNOfStringInStructure ?PEG 1 ?TEXT 
  (#$NthOccurrenceOfStructureTypeInStructureFn ?NUM #$NLParagraph ?W))
(#$originalPhrase ?PEG ?TEXT)) #$BaseKB)



;;(#$SentenceDescribingObjectFn #$BillClinton "Bill Clinton likes cars")
;;(csetq document (new-document-from-string "Bill Clinton likes cars"))



(#$and 
  (#$isa ?PEG #$LinguisticExpressionPeg-CompleteUtterance) 
  (#$ist ?CMT (#$originalPhrase ?PEG ?TEXT))
(#$occurrenceNOfStringInStructure ?PEG 1 ?TEXT2
       (#$NthOccurrenceOfStructureTypeInStructureFn ?PN #$NLParagraph 
           ?AIS))

    )

(#$implies (#$and (#$isa ?PEG #$LinguisticExpressionPeg-CompleteUtterance) 
(#$occurrenceNOfStringInStructure ?PEG 1 ?TEXT 
  (#$NthOccurrenceOfStructureTypeInStructureFn ?NUM #$NLParagraph ?W)))
(#$originalPhrase ?PEG ?TEXT))




(and 
   (isa ?PEG LinguisticExpressionPeg-CompleteUtterance) 
   (ist ?CMT 
       (originalPhrase ?PEG ?TEXT)) 
   (occurrenceNOfStringInStructure ?PEG 1 ?TEXT2 
       (NthOccurrenceOfStructureTypeInStructureFn ?PARANUM NLParagraph ?AIS)))


	(equals ?NLSENT (ParagraphSentenceFn PARAGRAPH (SentenceNumberFn ?SN)))



(equals ?WHAT (NthOccurrenceOfStructureTypeInStructureFn 1 NLSentence
   (NthOccurrenceOfStructureTypeInStructureFn 5 NLParagraph 
   (AISForFn 
       (WebPageCWFn "http://en.wikipedia.org/wiki/Appalachian_dulcimer")))))
(orignalText

Constant: SentenceDescribingObjectFn.

In Mt: UniversalVocabularyMt.
Direction: :forward.
isa: IndividualDenotingFunction.
Direction: :forward.
isa: BinaryFunction.
Direction: :forward.
isa: ReifiableFunction.

In Mt: BaseKB.
Direction: :forward.
quotedIsa: ForwardReifiableCycLFunctor.
Direction: :forward.
resultIsa: PropositionalConceptualWork.

In Mt: UniversalVocabularyMt.

Truth Value: :default.
Direction: :forward.
arity: 2.

In Mt: BaseKB.

Truth Value: :default.
Direction: :forward.
argIsa: (1 Thing).
argIsa: (2 CharacterString).
Direction: :forward.
arg2Isa: CharacterString.
Direction: :forward.
comment: "A binary function that returns an instance of #$PropositionalConceptualWork when given a #$PropositionalConceptualWork and a #$CharacterString. (#$SentenceOfCWFn PCW STRING) denotes the #$PropositionalConceptualWork that is a subwork of PCW that is derived from the sentence STRING.".

Truth Value: :monotonic.
Direction: :forward.
f: (skolemizeForward SentenceDescribingObjectFn).
f: (completeTextOfWork 
     (SentenceDescribingObjectFn ?OBJ ?TEXT) ?TEXT).

f: (pcwInformationAbout (SentenceDescribingObjectFn ?OBJ ?TEXT) ?OBJ 
       (PCWInformationAboutFn (SentenceDescribingObjectFn ?OBJ ?TEXT) ?OBJ)).


(objectsDescribed ?MT ?OBJ)

(get-cyclifier-interps-for-string "In the room I see two books sitting on a shelf")
       

  (#$webPageURL (#$AISForFn (#$WebPageCWFn "http://en.wikipedia.org/wiki/Appalachian_dulcimer") "http://en.wikipedia.org/wiki/Appalachian_dulcimer")

# <(#$AISForFn #<(#$WebPageCWFn "http://en.wikipedia.org/wiki/Appalachian_dulcimer")>)>

(load-ke-text-file #$CycAdministrator “C:/my.ketext.txt” :agenda t)

|#


