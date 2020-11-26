;;;{{{DOC
;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Copyright (c) 2002 - 2010 Douglas R. Miles.  All rights reserved.
;;;
;;; @module COGBOT-PARSER
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
;;; Depends on interface: COGBOT-KE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;}}}EDOC

(in-package "CYC")

'(progn 
(load "cycl/rtp/rtp-datastructures.lisp")
(load "cycl/rtp/rtp-type-checkers.lisp")
(load "cycl/rtp/recognition.lisp")
)


#+Allegro
( progn
;; (sys:resize-areas :verbose t :new 500000000  :old 5000000000)
 (gc :tenure) (gc t))

(csetq *ignore-musts?* t)

;; maybe  (sys:resize-areas :verbose t :new 15000000000  :old 15000000000)
;; (load "cynd/cogbot-parser.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RTP/ITP SETUP    (excl:dumplisp :name "cogbot-start.dxl")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "cynd/cogbot-ke.lisp")
(print ";;;;;;;; Loading cogbot-parser.lisp")(force-output)


(csetq *psp-verbose?* nil)
(csetq *it-verbose?* nil)

(csetq *verbose-print-pph-phrases?* nil)
(csetq *it-failing-verbose* nil)
(csetq *it-verbose* nil)
(csetq *psp-verbose?* nil)


;; x 10 normal
(csetq *psp-max-edges-per-chart* 1000000) 
(csetq *psp-max-edges-per-span* 102120)

;;(load-ke-text-file #$CycAdministrator "cynd/cogbot-parser.ke" :agenda t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; SET SemTransPreds to forward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define set-rule-forward-cons-pred (term)
  (cdolist (ass (all-term-assertions term))
     (pwhen (backward-rule? ass)
      (clet ((cons  (third (assertion-el-formula ass))))
       (pwhen (consp cons)
       (pwhen (eq term (first cons))
         (print ass)
		 (tms-change-direction ass :FORWARD)
		 ))))))

(define set-rule-forward-antecedant-pred (term)
  (cdolist (ass (all-term-assertions term))
     (pwhen (backward-rule? ass)
      (clet ((cons  (second (assertion-el-formula ass))))
       (pwhen (consp cons)
       (pwhen (eq term (first cons))
         (print ass)
		 (tms-change-direction ass :FORWARD)
		 ))))))

'(cdolist (v (query-template '?X `(#$isa ?X #$SemTransPred) #$EverythingPSC))
  (set-rule-forward-cons-pred v))

'(cdolist (v (query-template '?X `(#$isa ?X #$SemTransPred) #$EverythingPSC))
  (set-rule-forward-antecedant-pred v))



(csetq *TMPL-STORE* #$EnglishTemplateMt)
(csetq *RULEKB* #$BaseKB) 

#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; TODOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(csetq *rkf-uttrdr-full-spans-only?* t)

(parse "He saw my dog")

(cbnl-assert `
  (#$termTemplate #$ConstraintTemplateFn 
     (#$NLPatternList
           (#$NLPattern-Agr :INF #$infinitive))
       (#$Simply :INF)) *TMPL-STORE*)


(cbnl-assert `
  (#$termTemplate #$InfinitivalVBarTemplate 
     (#$NLPatternList
           (#$NLPattern-Agr :INF #$infinitive))
       (#$Simply :INF)) *TMPL-STORE*)


(logicmoo-rkf-reader "run" #$ActiveVBarTemplate-NPGap)
(logicmoo-rkf-reader "colour" #$InfinitivalVBarTemplate)
(logicmoo-rkf-reader "for 5 minutes" #$PPTemporalDurationTemplate )


(set-rule-forward-cons-pred #$nounSemTrans)
(set-rule-forward-cons-pred #$verbSemTrans)
(set-rule-forward-cons-pred #$compoundSemTrans)

(logicmoo-rkf-reader "two books" #$NPTemplate)
(logicmoo-rkf-reader "I see you" #$STemplate)
(logicmoo-rkf-reader "I see two books" #$STemplate)
(logicmoo-rkf-reader "I see two books sitting on a shelf" #$STemplate)
(logicmoo-rkf-reader "In the room, I see two books sitting on a shelf" #$STemplate)

Mt : EnglishMt

(implies 
    (and 
      (isa ?SLOT FamilyRelationSlot) 
      (denotation ?WORD CountNoun ?SN ?SLOT)) 
    (nounSemTrans ?WORD ?SN GenitiveFrame 
      (?SLOT :POSSESSOR :NOUN)))
(implies 
    (and 
      (isa ?SLOT InterAnimalRelationPredicate) 
      (denotation ?WORD CountNoun ?SN ?SLOT)) 
    (nounSemTrans ?WORD ?SN GenitiveFrame 
      (?SLOT :POSSESSOR :NOUN)))


(term-template-assert 
 (#$isa ?PLACE #$GeopoliticalEntity) 
  
  (#$termTemplate #$GeopoliticalEntityTemplate 
   (#$NLPatternList 
    (#$NLPattern-Term ?PLACE #$nameString)) ?PLACE))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; CREATE NEW TEMPLATE CATEGORIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  VPModSubject - A verbphrase that attempts to complete a Sentence after a Subject
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define add-template-cat (str)
(clet ((pc (find-or-create-constant str)))
	(cbnl-assert ` (#$isa ,pc #$KBDependentCollection) #$UniversalVocabularyMt :DEFAULT :FORWARD) 
	(cbnl-assert ` (#$not (#$isa ,pc #$CycKBSubsetCollection)) #$UniversalVocabularyMt :MONOTONIC :FORWARD) 
	(cbnl-assert ` (#$isa ,pc #$LinguisticObjectType) #$UniversalVocabularyMt :MONOTONIC :FORWARD) 
	(cbnl-assert ` (#$quotedIsa ,pc #$CycNLTemplateParsingConstant) #$UniversalVocabularyMt :DEFAULT :FORWARD) 
	(cbnl-assert ` (#$quotedIsa ,pc #$ResearchCycConstant-NotFullyReviewed) #$BookkeepingMt :DEFAULT :FORWARD) 
	(cbnl-assert ` (#$quotedIsa ,pc #$CycSecureFORT) #$BookkeepingMt :DEFAULT :FORWARD) 
	(cbnl-assert ` (#$genls ,pc #$ParsingTemplateCategory) #$UniversalVocabularyMt :MONOTONIC :FORWARD)
 (ret pc)))

;; Used to re-escape VPPhrases
(csetq *VPPHRASE* (add-template-cat "VPModSubject"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; HACK RTP TO VISIT EVERYTHING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deflexical *logicmoo-default-template-types* '((#$termTemplate) (#$termTemplate-Reln) (#$assertTemplate-Reln #$TemporalModifierTemplate)
 (#$commandTemplate) (#$rewriteTemplate) (#$assertTemplate) (#$queryTemplate) (#$metaStatementTemplate-Reln) (#$commandTemplate-Reln) (#$queryTemplate-Reln) (#$assertTemplate-Reln) (#$termTemplate-Test) (#$queryTemplate-Test) (#$assertTemplate-Test)))

(csetq *perform-vp-parser-expansion* t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; EVEN MORE MTs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( cbnl-assert `(#$implies (#$nearestGenlMt ?mt #$RKFParsingMt)(#$isa ?mt #$TemplateParsingMicrotheory)) *RULEKB* )
( cbnl-assert `(#$implies (#$nearestGenlMt ?mt #$TemplateParsingMt)(#$isa ?mt #$TemplateParsingMicrotheory)) *RULEKB* )
( cbnl-assert `(#$implies (#$genlMt ?mt #$RKFParsingMt)(#$isa ?mt #$TemplateParsingMicrotheory)) *RULEKB* )
( cbnl-assert `(#$implies (#$isa ?mt #$TemplateParsingMicrotheory)(#$genlMt ?mt #$TemplateParsingMt)) *RULEKB* )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; LOCAL ASSERTION MACRO/IterativeTemplateParserCyclist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro term-template-assert (ante cons) 
   (ret `(make-process (string ',ante) #'(lambda () (cbnl-assert '(#$implies ,ante (#$ist ,*TMPL-STORE* ,cons)) ,*RULEKB* )))))


;;TODO HELP! need to figure out how to make this mark antecdants to let use know a deduction was made
(cbnl-assert `(#$isa ,(find-or-create-constant "IterativeTemplateParserCyclist") #$Cyclist) #$UniversalVocabularyMt)
;;(#$ist #$BookkeepingMt (#$myReviewer ante #$IterativeTemplateParserCyclist))
(defmacro term-template-assert (ante cons) 
   (ret (cbnl-assert 
   `(#$implies (#$and ,ante (#$equals ,ante ?sent)(#$ist-Asserted ?mt ?sent))
     (#$and
        (#$myReviewer ?sent #$IterativeTemplateParserCyclist)
	(#$ist ,*TMPL-STORE* ,cons))) *RULEKB*)))


(defmacro term-template-assert (ante cons &optional (tomt  *RULEKB*)) 
   (clet ((as (cbnl-assert `(#$implies ,ante (#$ist ,*TMPL-STORE* ,cons)) tomt :backward)))    
	(cdolist (var (ask-template cons `(#$and ,ante (#$unknownSentence ,cons)) #$InferencePSC))
	  (extend-rtp-with-template-formula var as))
	 (ke-assert-now `(#$myReviewer ,as #$IterativeTemplateParserCyclist) #$BookkeepingMt)
	 (ret as)))
	

(defmacro term-template-assert (ante cons &optional (tomt  *RULEKB*)) 
   (clet ((ar (arity (car cons)))(as (cbnl-assert `(#$implies ,ante (#$ist ,*TMPL-STORE* ,cons)) tomt :backward)))    
	(cdolist (var (ask-template cons `(#$and ,ante (#$unknownSentence ,cons)) #$InferencePSC))
	  (extend-rtp-with-template-formula var as))
	 (ke-assert-now `(#$myReviewer ,as #$IterativeTemplateParserCyclist) #$BookkeepingMt)
	 (ret as)))


(defmacro term-template-assert (ante formula &optional (tomt  *RULEKB*)) 
   (clet ((assertion (cbnl-assert `(#$implies ,ante (#$ist ,*TMPL-STORE* ,formula)) tomt :backward)))
	 (ke-assert-now `(#$myReviewer ,assertion #$IterativeTemplateParserCyclist) #$BookkeepingMt)
  (clet ((mt (assertion-mt assertion))
	 (meaning  (nth 0 formula))
         (category (nth 1 formula))
         template expansion sem-test violator)
    (pcase (arity meaning)
      (3
       (csetq template (nth 2 formula)
              expansion (nth 3 formula)))
      (4         
       (csetq template (nth 3 formula)
              expansion (nth 4 formula)))
      (5
       (csetq template (nth 3 formula)
	      expansion (nth 4 formula)
	      sem-test (nth 5 formula)))
      )
      (clet ((tt (cbnl-assert `(#$termTemplate-Test ,category ,category ,template ,expansion ,ante) *TMPL-STORE*)))
         (ke-assert-now `(#$myReviewer ,tt #$IterativeTemplateParserCyclist) #$BookkeepingMt)

	 (ret assertion)))))


;;;;;;;;;;;;;;;;; OLD STYLE FORWARD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro term-template-assert (ante cons &optional (tomt  *RULEKB*)) 
   (clet ((as (cbnl-assert `(#$implies ,ante (#$ist ,*TMPL-STORE* ,cons)) tomt)))
    (ke-assert-now `(#$myReviewer ,as #$IterativeTemplateParserCyclist) #$BookkeepingMt)
	(ret as)))


(defmacro term-template-assert-fw (ante cons &optional (tomt  *RULEKB*)) 
   (clet ((as (cbnl-assert `(#$implies ,ante (#$ist ,*TMPL-STORE* ,cons)) tomt)))
    (ke-assert-now `(#$myReviewer ,as #$IterativeTemplateParserCyclist) #$BookkeepingMt)
	(ret as)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; ADD RTP RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from cycl/rtp/rtp-datastructures.lisp %% extend-rtp-with-template-assertion
(define-public extend-rtp-with-template-logicmoo (meaning category template expansion sem-test mt &optional (assertion *template-rule-default-assertion*))
  (clet ((*pph-language-mt* (first-of *pph-language-mt* #$CyclishParaphraseMt))
	 (*pph-domain-mt* (first-of *pph-domain-mt* #$DataForNLMt)))
  (ensure-rtp-rules)
  (clet (violator)
    (pwhen (consp expansion)
      (pcase (first expansion) 
	(#$ExpandSubLFn
	 (csetq expansion (third expansion)))
	(#$SubLQuoteFn
	 (csetq expansion (second expansion)))))
    (csetf template (strip-wrapper-functions-from template))
    ;;(pwhen (rtp-syntactic-constraint template)
    (ccatch :recursive-template violator
      (clet (existing-tp-rules) ;;was (find-rtp-rules-by-assertion assertion)
	(punless existing-tp-rules
	  (clet ((tp-rule (new-template-rule category template expansion sem-test assertion meaning)))
	    ;;(print `(new-template-rule ,category ,template ,expansion ,sem-test ,mt ,meaning))
	    (print-template-rule tp-rule *STANDARD-OUTPUT* nil)
	    (add-rtp-rule tp-rule)
	    (csetq existing-tp-rules (list tp-rule))))
	(cdolist (tp-rule existing-tp-rules)
	  (extend-categorized-template-rule-set mt tp-rule)))) ;)
    (pwhen violator
      (warn "Expansion ~S specifies a left-recursive template~% ~S~%"
            expansion template ))
  (ret violator))))


(define-public extend-rtp-with-template-formula (formula assertion)
  (clet ((*pph-language-mt* (first-of *pph-language-mt* #$CyclishParaphraseMt))
	 (*pph-domain-mt* (first-of *pph-domain-mt* #$DataForNLMt)))
  (ensure-rtp-rules)
  ;; (print `(extend-rtp-with-template-formula ,formula))
  (clet ((mt (assertion-mt assertion))
	 (meaning  (nth 0 formula))
         (category (nth 1 formula))
         template expansion sem-test violator)
    (pcase (arity meaning)
      (3
       (csetq template (nth 2 formula)
              expansion (nth 3 formula)))
      (4         
       (csetq template (nth 3 formula)
              expansion (nth 4 formula)))
      (5
       (csetq template (nth 3 formula)
	      expansion (nth 4 formula)
	      sem-test (nth 5 formula)))
      )
    (pwhen (consp expansion)
      (pcase (first expansion) 
	(#$ExpandSubLFn
	 (csetq expansion (third expansion)))
	(#$SubLQuoteFn
	 (csetq expansion (second expansion)))))
    (csetf template (strip-wrapper-functions-from template))
    ;;(pwhen (rtp-syntactic-constraint template)
    (ccatch :recursive-template violator
      (clet ((existing-tp-rules (find-rtp-rules-by-assertion assertion)))
	(punless existing-tp-rules
	  (clet ((tp-rule (new-template-rule category template expansion sem-test assertion meaning)))
	    (print `(new-template-rule ,category ,template ,expansion ,sem-test ,mt ,meaning))
	    (add-rtp-rule tp-rule)
	    (csetq existing-tp-rules (list tp-rule))))
	(cdolist (tp-rule existing-tp-rules)
	  (extend-categorized-template-rule-set mt tp-rule)))) ;)
    (pwhen violator
      (warn "Assertion ~D specifies a left-recursive template~% ~S~%"
            (assertion-id assertion) formula))))
  (ret assertion))


#|

(term-template-assert 
(#$massNounSemTrans ?Elite-TheWord ??LOGICMOO #$RegularNounFrame ?CYCL)  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$OptionalOne "a" "an" "the")
    (#$NLPattern-Word ?Elite-TheWord #$MassNoun))
   (#$TheOneOf :NOUN ?CYCL)))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; TESTS TO MAKE SURE RTP IS ACCEPTING NEW CATEGORIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(cbnl-assert 
 `(#$and
  (#$assertTemplate #$VPModSubject 
   (#$NLPatternList 
    (#$NLPattern-Exact "avpt"))
   (#$Simply "avpt"))

  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Exact "ast"))
   (#$Simply "ast"))

  (#$assertTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Exact "anp"))
   (#$Simply "anp"))

  (#$assertTemplate #$VPModSubject 
   (#$NLPatternList 
    (#$NLPattern-Exact "tvpt"))
   (#$Simply "tvpt"))

  (#$termTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Exact "tst"))
   (#$Simply "tst"))

  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Exact "tnp"))
   (#$Simply "tnp"))

 (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :ARG1) 
    (#$NLPattern-Template #$VPModSubject :ARG2)
    (#$NLPattern-Template #$VPModSubject :ARG3))
    (#$TheList :ARG1 :ARG2 :ARG3))

 (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Exact "tstvp") 
    (#$NLPattern-Template #$VPModSubject :ARG2))
    (#$TheList :ARG1 :ARG2))) *TMPL-STORE*)


|#
#|

(cbnl-assert `
  (#$termTemplate #$VPTemplate
   (#$NLPatternList 
    (#$NLPattern-Exact "vptemplate"))
   (#$The #$TheSentenceSubject)) #$GenFormatTemplateMt)

(cbnl-assert `
  (#$assertTemplate #$VPTemplate
   (#$NLPatternList 
    (#$NLPattern-Exact "vptemplate"))
   (#$The #$TheSentenceSubject)) #$GenFormatTemplateMt)

 (logicmoo-rkf-reader "vptemplate" #$VPTemplate)
 (logicmoo-rkf-reader "he vptemplate" #$STemplate)

 (logicmoo-rkf-reader "tst" #$STemplate)

 (logicmoo-rkf-reader "tnp tvpt" #$STemplate)


 (logicmoo-rkf-reader "tvpt" #$VPModSubject)

 (logicmoo-rkf-reader "avpt" #$VPModSubject)


 (logicmoo-rkf-reader "anp avpt tvpt" #$STemplate)
 ;; ==> ((#$TheList "anp" "avpt" "tvpt") (#$TheList #$AtrialNatriureticPeptide "avpt" "tvpt"))

 (logicmoo-rkf-reader "tstvp avpt" #$STemplate)
 ;; ==> ((#$TheList :ARG1 "avpt")

 (logicmoo-rkf-reader "man that tvpt" #$NPTemplate)

 (logicmoo-rkf-reader "anp" #$NPTemplate)

 (logicmoo-rkf-reader "tnp" #$NPTemplate)
 ;; => ("tnp")

(parse "tstvp avpt")



(add-template-cat "VPModSubjectTest")

(cbnl-assert 
  `(#$termTemplate-Reln #$VPModSubjectTest #$Simply
   (#$NLPatternList 
    (#$NLPattern-Exact "tvpt"))
   (#$Simply "tvpt")) *TMPL-STORE*)

(add-template-cat "VPModSubjectTest")

(cbnl-assert 
  `(#$termTemplate-Reln #$InfinitivalVPTemplate #$Simply
   (#$NLPatternList 
    (#$NLPattern-Exact "ivt"))
   (#$Simply "ivt found")) *TMPL-STORE*)

(cbnl-assert 
  `(#$assertTemplate-Reln #$InfinitivalVPTemplate #$Simply
   (#$NLPatternList 
    (#$NLPattern-Exact "ivt"))
   (#$Simply "ivt found")) *TMPL-STORE*)

(cbnl-assert 
  `(#$assertTemplate-Reln #$NPTemplate #$Simply
   (#$NLPatternList 
    (#$NLPattern-Exact "ivt"))
   (#$Simply "ivt found")) *TMPL-STORE*)

(cbnl-assert 
  `(#$and
	(#$posPredForTemplateCategory #$wordStrings #$VPModSubjectTest)
	(#$posForTemplateCategory #$NLWordForm #$VPModSubjectTest)
	(#$nlPhraseTypeForTemplateCategory #$NLSentence #$VPModSubjectTest)
	(#$nlPhraseTypeForTemplateCategory (#$PhraseFn #$NLSentence) #$VPModSubjectTest)
         
		) *BASEKB*)

(logicmoo-rkf-reader "good" #$StringTemplate)



(cpush  #$VPModSubjectTest *rkf-term-reader-default-templates*)

(logicmoo-rkf-reader "tvpt" #$VPModSubjectTest)


(logicmoo-rkf-reader "good" #$VPModSubjectTest)

(parse "Fred")

(parse "Freddy")

(parse "Bobby")

(parse "between 1 and two")

(logicmoo-rkf-reader "good" #$NPTemplate)
(logicmoo-rkf-reader "fly" #$VPTemplate)
(logicmoo-rkf-reader "fly" #$NPTemplate)

(logicmoo-rkf-reader "Ivt" #$InfinitivalVPTemplate)

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; #$VPTemplate Mining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cbnl-assert `(#$isa ,(foc "forwardTemplateAssertions") #$BinaryPredicate) *UVMT*)
(cbnl-assert `(#$argsGenl #$forwardTemplateAssertions #$ParsingTemplateCategory) *UVMT*)
(cbnl-assert `(#$comment #$forwardTemplateAssertions "this is mainly becasue #$VPTemplate seems to be missing capabilities of #$VPModSubject") *RULEKB*)
(cbnl-assert `
 (#$implies 
  (#$forwardTemplateAssertions ?FROM ?TO)
     (#$and 
	(#$implies (#$commandTemplate-Reln ?FROM ?LOGICMOO ?PATTERN ?CYCL) (#$ist ,*TMPL-STORE* (#$commandTemplate-Reln ?TO ?LOGICMOO ?PATTERN ?CYCL)))
	(#$implies (#$commandTemplate ?FROM ?PATTERN ?CYCL) (#$ist ,*TMPL-STORE*  (#$commandTemplate ?TO ?PATTERN ?CYCL)))
	(#$implies (#$metaStatementTemplate-Reln ?FROM ?LOGICMOO ?PATTERN ?CYCL) (#$ist ,*TMPL-STORE* (#$metaStatementTemplate-Reln ?TO ?LOGICMOO ?PATTERN ?CYCL)))
	(#$implies (#$rewriteTemplate ?FROM ?PATTERN ?CYCL) (#$ist ,*TMPL-STORE* (#$rewriteTemplate ?TO ?PATTERN ?CYCL)))
	(#$implies (#$assertTemplate-Reln ?FROM ?LOGICMOO ?PATTERN ?CYCL) (#$ist ,*TMPL-STORE*  (#$assertTemplate-Reln ?TO ?LOGICMOO ?PATTERN ?CYCL)))
	(#$implies (#$assertTemplate ?FROM ?PATTERN ?CYCL) (#$ist ,*TMPL-STORE* (#$assertTemplate ?TO ?PATTERN ?CYCL)))
	(#$implies (#$termTemplate ?FROM ?PATTERN ?CYCL) (#$ist ,*TMPL-STORE* (#$termTemplate ?TO ?PATTERN ?CYCL)))
	(#$implies (#$termTemplate-Reln ?FROM ?LOGICMOO ?PATTERN ?CYCL) (#$ist ,*TMPL-STORE* (#$termTemplate-Reln ?TO ?LOGICMOO ?PATTERN ?CYCL)))
	(#$implies (#$queryTemplate ?FROM ?PATTERN ?CYCL) (#$ist ,*TMPL-STORE* (#$queryTemplate ?TO ?PATTERN ?CYCL)))	
	(#$implies (#$queryTemplate-Reln ?FROM ?LOGICMOO ?PATTERN ?CYCL) (#$ist ,*TMPL-STORE* (#$queryTemplate-Reln ?TO ?LOGICMOO ?PATTERN ?CYCL)))
	)) *RULEKB*)


'(cbnl-assert `(#$forwardTemplateAssertions #$VPTemplate #$VPModSubject) *RULEKB*)

(define TREE-SUBSTITUTE-2 (before from to) (ret (append (list (first before) (second before)) (TREE-SUBSTITUTE (cddr before) from to))))

(define copy-cyc-constant-term (from to)
  (cdolist (var (all-term-assertions from))
    (clet ((before (assertion-el-formula var)))
       (punless (eq (second before) from)
                      (cbnl-assert  (TREE-SUBSTITUTE-2 before from to) (assertion-mt var))))))

(define copy-cyc-constant-term (from to)
  (cdolist (var (all-term-assertions from))
    (clet ((before (assertion-el-formula var)))
       (punless ()
         (cbnl-assert  (TREE-SUBSTITUTE before from to) (assertion-mt var))))))

'(copy-cyc-constant-term #$VPTemplate #$VPModSubject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; #$consumesTemplateAssertions Mining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cbnl-assert `(#$isa ,(foc "consumesTemplateAssertions") #$BinaryPredicate) *UVMT*)
(cbnl-assert `(#$quotedIsa #$SharedNoteOnRTPVBarTemplates #$SharedNote) *RULEKB*)
(cbnl-assert `(#$isa #$SharedNoteOnRTPVBarTemplates #$Individual) *RULEKB*)
  
(cbnl-assert `
(#$implies 
  (#$and 
   (#$sharedNotes ?TEMPCAT #$SharedNoteOnRTPVBarTemplates)
   (#$genls ?TEMPCAT #$ParsingTemplateCategory))
   (#$consumesTemplateAssertions #$VPModSubject ?TEMPCAT)) *RULEKB*)

(cbnl-assert 
`(#$implies
 (#$consumesTemplateAssertions ?TO ?FROM)
 (#$and 
  (#$assertTemplate-Reln ?TO ?FROM (#$NLPatternList (#$NLPattern-Template ?FROM :VBAR-CONT)) (#$Simply :VBAR-CONT))
  (#$termTemplate-Reln ?TO ?FROM (#$NLPatternList (#$NLPattern-Template ?FROM :VBAR-CONT)) (#$Simply :VBAR-CONT))
  ;; (#$queryTemplate-Reln ?TO ?FROM (#$NLPatternList (#$NLPattern-Template ?FROM :VBAR-CONT)) (#$Simply :VBAR-CONT))
    ))  *RULEKB*)
       


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; #$Situation Mining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
'(term-template-assert 
	(#$and 
          (#$nlPhraseTypeForTemplateCategory (#$PhraseFn-Bar1 #$Verb) ?InfinitivalVBarTemplate)
	  (#$denotation ?Restore-TheWord #$Verb ??N0 ?RestoringAnObject)
	  (#$genls ?RestoringAnObject #$Situation)
          ;; (#$isa ??N0 #$Integer)
	  (#$posForTemplateCategory #$Verb ?InfinitivalVBarTemplate)
	  (#$posPredForTemplateCategory ?infinitive ?InfinitivalVBarTemplate))

	(#$termTemplate-Reln ?InfinitivalVBarTemplate ?InfinitivalVBarTemplate
	     (#$NLPatternList
		(#$NLPattern-Word ?Restore-TheWord ?infinitive))
	    (#$Simply ?RestoringAnObject)))
  
'(term-template-assert 
	(#$and 
          (#$nlPhraseTypeForTemplateCategory (#$PhraseFn-Bar1 #$Verb) ?InfinitivalVBarTemplate)
	  (#$denotation ?Restore-TheWord #$Verb ??N0 ?RestoringAnObject)
	  (#$genls ?RestoringAnObject #$Situation)
          ;; (#$isa ??N0 #$Integer)
	  )

	(#$termTemplate ?InfinitivalVBarTemplate
	     (#$NLPatternList
		(#$NLPattern-Word ?Restore-TheWord #$Verb))
	    (#$Simply ?RestoringAnObject)))

'(term-template-assert 
	(#$and 
	  (#$denotation ?Restore-TheWord #$Verb ??N0 ?RestoringAnObject)
	  (#$genls ?RestoringAnObject #$Situation)
          ;; (#$isa ??N0 #$Integer)
	  )

	(#$termTemplate #$InfinitivalVBarTemplate
	     (#$NLPatternList
		(#$NLPattern-Word ?Restore-TheWord #$Verb))
	    (#$Simply ?RestoringAnObject)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; VPBars Tensed Mining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(term-template-assert 
 (#$and 
   (#$nlPhraseTypeForTemplateCategory (#$PhraseFn-Bar1 #$Verb) ?InfinitivalVBarTemplate)
   (#$posPredForTemplateCategory ?infinitive ?InfinitivalVBarTemplate))
 (#$termTemplate ?InfinitivalVBarTemplate 
    (#$NLPatternList
           (#$NLPattern-Agr :INF ?infinitive))
       (#$Simply :INF)))

;;; the previous effectively does:
(cbnl-assert `
  (#$termTemplate #$InfinitivalVBarTemplate 
     (#$NLPatternList
           (#$NLPattern-Agr :INF #$infinitive))
       (#$Simply :INF)) *TMPL-STORE*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STemplate-Action
;; Used to convert STemplate to an action term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cbnl-assert `(#$comment ,(add-template-cat "STemplate-Action")  "Used to convert STemplate to an action term" ))
(cbnl-assert `
  (#$termTemplate-Reln #$STemplate-Action #$STemplate
   (#$NLPatternList
    (#$NLPattern-Template #$STemplate :EXP))
  (#$SubstituteFromListFn (#$TheOneOf :ACTION :EXP) (#$TheList 
     (#$TheList ?S #$TheSentenceSubject) 
     (#$TheList ?A :ACTION)
     (#$TheList ?S :SUBJECT)
     (#$TheList ?OO :OBLIQUE-OBJECT)
     (#$TheList ?O :OBJECT) ))
     ) *TMPL-STORE*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STemplate-Clause
;; Used to convert STemplate to an action term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(logicmoo-rkf-reader "Joe thinks that fish are wet" #$STemplate)
(cbnl-assert `(#$comment ,(add-template-cat "STemplate-Clause")  "Used to convert STemplate to an action term" ))
(cbnl-assert `
  (#$termTemplate-Reln #$STemplate-Clause #$STemplate
   (#$NLPatternList
    (#$NLPattern-Template #$STemplate :EXP))
  (#$SubstituteFromListFn :EXP 
   (#$TheList 
     (#$TheList ?S #$TheSentenceSubject) 
     (#$TheList ?A :ACTION)
     (#$TheList ?S :SUBJECT)
     (#$TheList ?OO :OBLIQUE-OBJECT)
     (#$TheList ?O :OBJECT) ))) *TMPL-STORE*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; VPModSubjectTrans (a VBAR Template)
;; Basically a VBarTemplate: x <#$VPModSubjectTrans> y.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cbnl-assert `(#$comment ,(add-template-cat "VPModSubjectTrans") "Basically a VBarTemplate: x <#$VPModSubjectTrans> y." ))
(cbnl-assert `
  (#$termTemplate-Reln #$VPModSubject #$VPModSubjectTrans
   (#$NLPatternList
    (#$NLPattern-Template #$VPModSubjectTrans :ACTION)
    (#$NLPattern-Template #$NPTemplate :POST-OBJECT))
  (#$SubstituteFormulaFn :OBJECT :POST-OBJECT :ACTION)) *TMPL-STORE*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPInfTemplate
;; Used to broker Infinitive clauses matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-template-cat "NPInfTemplate")
(cbnl-assert `
  (#$termTemplate-Reln #$VPModSubject #$NPInfTemplate
   (#$NLPatternList
    (#$NLPattern-Template #$NPInfTemplate :ACTION))
  (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT :ACTION)) *TMPL-STORE*)

;; (parse "saw it")
;; (logicmoo-rkf-reader "saw it" #$VPModSubject)
;; (logicmoo-rkf-reader "saw it" #$InfinitivalVPTemplate)

(cbnl-assert `
  (#$termTemplate-Reln #$VPModSubject #$Simply
   (#$NLPatternList
    (#$NLPattern-Template #$InfinitivalVPTemplate :ACTION))
  (#$Simply :ACTION)) *TMPL-STORE*)

(extend-rtp-from-mt nil (list (list #$termTemplate-Reln #$VPModSubject)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Generalized STemplates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cbnl-assert  `(#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJ) 
    (#$NLPattern-Template #$VPModSubject :VPHRASE))
        (#$and (#$equals :SUBJ #$TheSentenceSubject) :VPHRASE))
	   *TMPL-STORE*)

(cbnl-assert  
  `(#$assertTemplate #$STemplate 
       (#$NLPatternList 
           (#$NLPattern-Template #$NPTemplate :SUBJ) 
           (#$NLPattern-Template #$VPModSubject :PRED)) 
       (#$joinTemplateComponents 
           (#$TheList #$NPTemplate #$VPTemplate) 
           (#$TheList :SUBJ :PRED)))
	    *TMPL-STORE*)


;; (parse "I saw it")
'(cbnl-assert  `(#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJ) 
    (#$NLPattern-Template #$InfinitivalVPTemplate :VPHRASE))
        (#$and (#$equals :SUBJ #$TheSentenceSubject)(#$playsActiveSubjectRole :SUBJ :VPHRASE)))
	   *TMPL-STORE*)

(cbnl-assert  
  `(#$assertTemplate #$STemplate 
       (#$NLPatternList 
           (#$NLPattern-Template #$NPTemplate :SUBJ) 
           (#$NLPattern-Template #$InfinitivalVPTemplate :PRED)) 
       (#$joinTemplateComponents 
           (#$TheList #$NPTemplate #$VPTemplate) 
           (#$TheList :SUBJ :PRED)))
	    *TMPL-STORE*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  subordinatingConjunctionSemTrans Phrases 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+SEE_COMMENT
(term-template-assert 
 (#$subordinatingConjunctionSemTrans ?Because-TheWord ??LOGICMOO #$VerbPhraseModifyingFrame (?CYCL ?SUBORD-CLAUSE ?CLAUSE))
  (#$assertTemplate-Reln #$STemplate #$VerbPhraseModifyingFrame
   (#$NLPatternList 
   (#$NLPattern-Template #$STemplate-Clause :CLAUSE)
    (#$NLPattern-Word ?Because-TheWord #$SubordinatingConjunction)
    (#$NLPattern-Template #$STemplate-Clause :SUBORD-CLAUSE))
  (?CYCL ?SUBORD-CLAUSE ?CLAUSE)))

#|
TODOTODOTODOTODOTODOTODO  TODOTODOTODO TODO TODO

where this is correct:
(subordinatingConjunctionSemTrans Before-TheWord 1 VerbPhraseModifyingFrame 
       (startsAfterEndingOf :SUBORD-ACTION :ACTION))

These two are backwards
(subordinatingConjunctionSemTrans Since-TheWord 2 VerbPhraseModifyingFrame 
       (startsAfterEndingOf :ACTION :SUBORD-ACTION))
(subordinatingConjunctionSemTrans After-TheWord 1 VerbPhraseModifyingFrame 
       (startsAfterEndingOf :ACTION :SUBORD-ACTION))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DATE, RATIO AND PERCENT PARSING - @WARNING RETRACTS!!! -
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-TooPromiscuous
#+DontNeedThem
(cbnl-retract `
(#$termTemplate-Reln #$NPTemplate #$RatioTemplate 
       (#$NLPatternList 
           (#$NLPattern-Template #$RatioTemplate :NUM)) 
       (#$Simply :NUM)) #$EnglishGrammarTemplateMt)

#-TooPromiscuous
#+DontNeedThem
(cbnl-retract `
 (#$termTemplate #$NPTemplate 
       (#$NLPatternList 
           (#$NLPattern-Template #$DateTemplate :DATE)) 
       (#$Simply :DATE)) #$EnglishTemplateMt)

#-TooPromiscuous
#+DontNeedThem
(cbnl-retract `
 (#$termTemplate-Reln #$NPTemplate #$Percent 
       (#$NLPatternList 
           (#$NLPattern-Template #$PercentTemplate :PERC)) 
       (#$Simply :PERC)) #$EnglishTemplateMt)


;; (parse "I saw him right now")
#+TooPromiscuous
(cbnl-assert  `
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Template #$VPModSubject :VPHRASE)
    (#$NLPattern-Template #$DateTemplate :DATE))
        (#$holdsIn :DATE :VPHRASE))
	   #$EnglishTemplateMt)


;; (parse "right now, I saw him")
#+TooPromiscuous
(cbnl-assert  `(#$assertTemplate #$STemplate
  (#$NLPatternList 
    (#$NLPattern-Template #$DateTemplate :DATE)
    (#$OptionalOne ",")
    (#$NLPattern-Template #$STemplate-Clause :CLAUSE))
  (#$holdsIn :DATE :CLAUSE))
	   *TMPL-STORE*)

;; (parsert "right now" #$DateTemplate)

#|

#-TooPromiscuous
(cbnl-assert `
(#$termTemplate-Reln #$NPTemplate #$RatioTemplate 
       (#$NLPatternList 
           (#$NLPattern-Template #$RatioTemplate :NUM)) 
       (#$Simply :NUM)) #$EnglishGrammarTemplateMt)

#-TooPromiscuous
(cbnl-assert `
 (#$termTemplate #$NPTemplate 
       (#$NLPatternList 
           (#$NLPattern-Template #$DateTemplate :DATE)) 
       (#$Simply :DATE)) #$EnglishTemplateMt)

#-TooPromiscuous
(cbnl-assert `
 (#$termTemplate-Reln #$NPTemplate #$Percent 
       (#$NLPatternList 
           (#$NLPattern-Template #$PercentTemplate :PERC)) 
       (#$Simply :PERC)) #$EnglishTemplateMt)


(cbnl-retract `
 (#$termTemplate-Reln #$NPTemplate #$Percent 
       (#$NLPatternList 
           (#$NLPattern-Template #$PercentTemplate :PERC)) 
       (#$Simply :PERC)) #$EnglishTemplateMt)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; X IS A Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cbnl-assert
`(#$assertTemplate-Reln #$STemplate #$is-Underspecified 
  (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :ARG1) 
    (#$NLPattern-Word #$Be-TheWord #$Verb) 
    (#$NLPattern-Exact "a") 
    (#$NLPattern-Template #$NPTemplate :ARG2)) 
  (#$is-Underspecified :ARG1 :ARG2)) *TMPL-STORE*)

;; (parse "all dogs are cats")
(cbnl-assert
`(#$assertTemplate-Reln #$STemplate #$genls
  (#$NLPatternList
    (#$NLPattern-Word #$All-TheWord #$Determiner-Central)
    (#$NLPattern-Agr :ARG1 #$plural)
    (#$NLPattern-Exact "are") 
    (#$NLPattern-POS :ARG2 #$CommonNoun))
  (#$genls :ARG1 :ARG2)) *TMPL-STORE*)

;;----------------------------
#|
(cbnl-assert
`(#$assertTemplate #$STemplate #$genls
  (#$NLPatternList
    (#$NLPattern-Word #$All-TheWord #$Determiner-Central)
    (#$NLPattern-Agr :ARG1 #$plural)
    (#$NLPattern-Exact "are") 
    (#$NLPattern-POS :ARG2 #$CommonNoun))
  (#$genls :ARG1 :ARG2)) *TMPL-STORE*)

(cbnl-assert 
 `(#$termTemplate-Reln #$NPTemplate #$NPTemplate
  (#$NLPatternList 
    (#$NLPattern-Agr ?DENOT (#$Kappa (?W ?S) (#$and (#$denotation ?W ?POS ??SN ?COL)(#$termStrings ?W ?S)))))
  (#$TheList ?COL ?DENOT ?POS ?S ?W))
 #$EnglishTemplateMt)

(parse "doggy")

(cbnl-assert 
 `(#$termTemplate-Test #$NPTemplate #$NPTemplate
  (#$NLPatternList 
    (#$NLPattern-Agr ?DENOT (#$Kappa (?W ?S) (#$and (#$denotation ?W ?POS ??SN ?COL)(#$termStrings ?W ?S)))))
  (#$TheList ?COL ?DENOT ?POS ?S ?W) #$True)
 #$EnglishTemplateMt)


(cbnl-assert 
 `(#$termTemplate-Test #$NPTemplate #$NPTemplate
  (#$NLPatternList 
    (#$NLPattern-Agr ?DENOT (#$Kappa (?W ?S) (#$and (#$denotation ?W ?POS ??SN ?COL)(#$termStrings ?W ?S)))))
  (#$TheList ?COL ?DENOT ?POS ?S ?W)
  (#$denotation ?W ?POS ??SN ?COL))
 #$EnglishTemplateMt)

|#
;;----------------------------


;; (parse "each dog is a mammal")
'(cbnl-assert
`(#$assertTemplate-Reln #$STemplate #$genls
  (#$NLPatternList
    (#$NLPattern-Term #$Every-NLAttr #$Determiner-Central)
    (#$NLPattern-POS :ARG1 #$Noun)
    (#$NLPattern-Word #$Be-TheWord #$Verb)
    (#$OptionalOne "a" "an")
    (#$NLPattern-POS :ARG2 #$Noun))
  (#$genls :ARG1 :ARG2)) *TMPL-STORE*)


;; (parse "all dogs are good")
(cbnl-assert
`(#$assertTemplate-Reln #$STemplate #$relationAllInstance
  (#$NLPatternList
    (#$NLPattern-Word #$All-TheWord #$Determiner-Central)
    (#$NLPattern-Agr :ARG1 #$plural)
    (#$NLPattern-Exact "are") 
    (#$NLPattern-POS :ARG2 #$Adjective))
  (#$relationAllInstance #$hasNonAspectualQuantity :ARG1 :ARG2)) *TMPL-STORE*) ;; maybe #$hasEvaluativeQuantity 



;;(load "cynd/cogbot-parser-template.lisp")
(csetq *rtp-default-template-types* *logicmoo-default-template-types*)
(print ";;;;;;;; Done cogbot-parser.lisp")(force-output)

;;(parse "it is a symptom associated with exposure to heat")
;; TODO maybe use extend-rtp-with-template-assertion
;;(extend-rtp-from-mt nil *rtp-default-template-types*)
;;  (extend-rtp-from-mt nil *logicmoo-default-template-types*)
;; (parse "he has placed an obstacle to it")

 ;; (logicmoo-rkf-reader "Austin" #$GeopoliticalEntityTemplate)
 ;; (isa-in-any-mt? #$nameString #$TermPhrasesConstraint ) 
;; (logicmoo-rkf-reader  "You are standing in the doorway of the elevator")
;;
;; (parse "there")
;; (logicmoo-rkf-reader  "You can see a picture hanging on the wall")
;; (logicmoo-rkf-reader  "You are standing in the doorway of the elevator")
;; ((#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) 
;; (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) ...)
;; (logicmoo-rkf-reader  "think that i can" #$VPModSubject)
;; (logicmoo-rkf-reader  "think" #$InfinitivalVPTemplate)
;;(cbnl-assert `(#$posForTemplateCategory #$Verb #$VPModSubject) #$TemplateParsingMt)
;;(cbnl-retract `(#$posPredForTemplateCategory #$verbStrings #$VPModSubject) #$TemplateParsingMt)
;;(cbnl-retract `(#$posPredForTemplateCategory #$regularAdverb #$VPModSubject) #$TemplateParsingMt)
;; (FI-MERGE #$VPModSubject #$InfinitivalVPTemplate)
;; (FI-MERGE #$VPTemplate #$VPModSubject)

;; save as #$InfinitivalVPTemplate
;; TODO (parse "the book of mine is here")
;;     ( kb-node-analyze-category '(#$PhraseFn-Bar1 #$Noun) #$EverythingPSC)
;; ( kb-node-analyze-category #$Noun #$EverythingPSC)

'(add-template-assertion :TRUE-DEF (cbnl-assert '
(#$termTemplate-Test #$NPTemplate  #$InstanceNamedFn (#$NLPatternList
           (#$NLPattern-Template #$StringTemplate :VAR))
       (#$InstanceNamedFn ?W :VAR)
 (#$TheOneOf ?W (#$termStrings ?W :VAR)))  #$EnglishTemplateMt))

;;(parse "I see two books on a shelf")
;;(parse "You re standing on the edge")
;; (parse "You are standing on the edge")
;; (parse "I am standing above three new cars")
;; assertions 6570590


;; (ITP-SEQUEL "An actor that can see two books on the shelf" #$NPTemplate)
