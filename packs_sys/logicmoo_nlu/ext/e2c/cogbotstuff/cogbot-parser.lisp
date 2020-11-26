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


'(cdolist (v (query-template '?X `(#$isa ?X #$SemTransPred) #$EverythingPSC)) (set-rule-forward-cons-pred v))


(csetq *TMPL-STORE* #$EnglishTemplateMt)
(csetq *RULEKB* #$BaseKB) 

#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; TODOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(csetq *rkf-uttrdr-full-spans-only?* t)

(parse "He saw my dog")



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
	(ke-assert-now ` (#$isa ,pc #$KBDependentCollection) #$UniversalVocabularyMt :DEFAULT :FORWARD) 
	(ke-assert-now ` (#$not (#$isa ,pc #$CycKBSubsetCollection)) #$UniversalVocabularyMt :MONOTONIC :FORWARD) 
	(ke-assert-now ` (#$isa ,pc #$LinguisticObjectType) #$UniversalVocabularyMt :MONOTONIC :FORWARD) 
	(ke-assert-now ` (#$quotedIsa ,pc #$CycNLTemplateParsingConstant) #$UniversalVocabularyMt :DEFAULT :FORWARD) 
	(ke-assert-now ` (#$quotedIsa ,pc #$ResearchCycConstant-NotFullyReviewed) #$BookkeepingMt :DEFAULT :FORWARD) 
	(ke-assert-now ` (#$quotedIsa ,pc #$CycSecureFORT) #$BookkeepingMt :DEFAULT :FORWARD) 
	(ke-assert-now ` (#$genls ,pc #$ParsingTemplateCategory) #$UniversalVocabularyMt :MONOTONIC :FORWARD)
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


(defmacro term-template-assert-fw (ante cons &optional (tomt  *RULEKB*)) 
   (clet ((as (cbnl-assert `(#$implies ,ante (#$ist ,*TMPL-STORE* ,cons)) tomt)))
    (ke-assert-now `(#$myReviewer ,as #$IterativeTemplateParserCyclist) #$BookkeepingMt)
	(ret `'(ask-template ',cons '(#$and ,ante (#$unknownSentence ,cons)) #$InferencePSC))
	))


(defmacro term-template-assert (ante cons &optional (tomt  *RULEKB*)) 
   (clet ((as (cbnl-assert `(#$implies ,ante (#$ist ,*TMPL-STORE* ,cons)) tomt :backward)))
    (ke-assert-now `(#$myReviewer ,as #$IterativeTemplateParserCyclist) #$BookkeepingMt)
	(ret `'(ask-template ',cons '(#$and ,ante (#$unknownSentence ,cons)) #$InferencePSC))
	))

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
(ke-assert-now `(#$isa ,(foc "forwardTemplateAssertions") #$BinaryPredicate) *UVMT*)
(ke-assert-now `(#$argsGenl #$forwardTemplateAssertions #$ParsingTemplateCategory) *UVMT*)
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
                      (ke-assert-now  (TREE-SUBSTITUTE-2 before from to) (assertion-mt var))))))

'(copy-cyc-constant-term #$VPTemplate #$VPModSubject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; #$consumesTemplateAssertions Mining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ke-assert-now `(#$isa ,(foc "consumesTemplateAssertions") #$BinaryPredicate) *UVMT*)
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
       

;;(load "cynd/cogbot-parser-template.lisp")
(csetq *rtp-default-template-types* *logicmoo-default-template-types*)
(print ";;;;;;;; Done cogbot-parser.lisp")(force-output)

;;(parse "it is a symptom associated with exposure to heat")
;; TODO maybe use extend-rtp-with-template-assertion
(extend-rtp-from-mt nil *rtp-default-template-types*)
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


