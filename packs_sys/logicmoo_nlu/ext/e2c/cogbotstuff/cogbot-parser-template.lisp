;;;{{{DOC
;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Copyright (c) 2002 - 2010 Douglas R. Miles.  All rights reserved.
;;;
;;; @module COGBOT-PARSER-TEMPLATE
;;; @features :COGBOT-NL
;;;
;;; @author dmiles
;;; @owner dmiles
;;;
;;; @created 2010/01/10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tools & utilities for Cogbot/NLParsing
;;
;; This will mine the GenTemplate/NLGeneration and try to produce RTP visible assertions
;;
;; !!!!!!!!!!!!!!!
;; IMPORTANT NOTES:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External interface:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Depends on interface: COGBOT-PARSER
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;}}}EDOC

(in-package "CYC")


;; (load "cynd/cogbot-parser-template.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Mining #$genTemplate into ITP/SEQUEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print ";;;;;;;; Loading cogbot-parser-template.lisp")(force-output)

(csetq *verbose-print-pph-phrases?* nil)
(csetq *it-failing-verbose* nil)
(csetq *it-verbose* nil)
(csetq *psp-verbose?* nil)

;; from cycl/rtp/rtp-datastructures.lisp %% extend-rtp-with-template-assertion
(define-public extend-rtp-with-template-logicmoo (meaning category template expansion sem-test mt)
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
	  (clet ((tp-rule (new-template-rule category template expansion sem-test *template-rule-default-assertion* meaning)))
	    (add-rtp-rule tp-rule)
	    (csetq existing-tp-rules (list tp-rule))))
	(cdolist (tp-rule existing-tp-rules)
	  (extend-categorized-template-rule-set mt tp-rule)))) ;)
    (pwhen violator
      (warn "Expansion ~S specifies a left-recursive template~% ~S~%"
            expansion template ))
  (ret violator))))



(cbnl-assert '(#$resultIsa #$BasicTransitive-PassiveSentenceFn  #$SententialConstituent) *UVMT*)
(cbnl-assert '(#$resultIsa #$BasicTransitiveSentenceFn  #$SententialConstituent) *UVMT*)

;;; getTemplate
(term-template-assert 
  (#$genTemplate ?carriesInfectionType 
	    (#$BasicTransitive-PassiveSentenceFn 
	   (#$TermParaphraseFn :ARG1) ?Carry-TheWord 
	   (#$TermParaphraseFn :ARG2)))
  (#$assertTemplate-Reln #$VPModSubject ?carriesInfectionType
   (#$NLPatternList 
    (#$NLPattern-Word ?Carry-TheWord #$passiveParticiple)
    (#$NLPattern-Template #$NPTemplate :ARG2))
  (?carriesInfectionType #$TheSentenceSubject :ARG2 )))

(term-template-assert 
	(#$genTemplate ?carriesInfectionType 
	       (#$BasicTransitive-PassiveSentenceFn 
		   (#$TermParaphraseFn :ARG2) ?Carry-TheWord 
		   (#$TermParaphraseFn :ARG1)))
  (#$assertTemplate-Reln #$VPModSubject ?carriesInfectionType
   (#$NLPatternList 
    (#$NLPattern-Word ?Carry-TheWord #$passiveParticiple) 
    (#$NLPattern-Template #$NPTemplate :ARG1))
  (?carriesInfectionType  :ARG1 #$TheSentenceSubject )))


#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; TODOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(genFormat-Precise ambientVisibility "the ambient visibility of the region ~a is ~a" TheEmptyList)

(genFormat-Precise partTypes "~a has at least one ~a as a part" 
       (TheList 
           (TheList 1 :SINGULAR-GENERIC :EVERY) 
           (TheList 2 :SINGULAR-GENERIC)))


(genFormat-ArgFixed RelationAllExistsFn 2 grandmothers "~a grandmothers" 
       (TheList 
           (TheList 1 :POSSESSIVE)))

(genFormat-ArgFixed InstanceWithRelationToFn 2 originallyFromRegion "the ~A that comes from ~A" 
       (TheList 
           (TheList 1 :SINGULAR) 
           (TheList 3 :SINGULAR)))

(genFormat-NP productionQuotaDuring "~a production quota during ~a" 
       (TheList 
           (TheList 2 :MASS-NUMBER :SINGULAR) 3))

(genFormat-NP supplyThroughAmountDuring "~a supply via ~a during ~a" 
       (TheList 
           (TheList 2 :MASS-NUMBER :SINGULAR) 3 4))

(genFormat c4AgentInMission "~a is responsible for providing command, control, communication, and computer support for the primary agent assigned to the mission described in ~a." 
       (TheList 2 1))

(genFormat agentPermitsActionType "~a permits ~a to ~a" 
       (TheList 1 2 
           (TheList 3 :INFINITIVE)))

(genTemplate characterInCW 
       (NPIsXP-NLSentenceFn 
           (TermParaphraseFn-NP :ARG1) 
           (IndefiniteNounPPFn Character-TheWord "in" 
               (TermParaphraseFn :ARG2))))

(genFormat permittedArea "in ~a access is allowed to ~a" TheEmptyList)

(genTemplate-Constrained typePrimaryFunction 
    (genlPreds :ARG3 instrumentalRole) 
    (GenTemplateRecipeOmitsArgFn :ARG3 
      (PhraseFormFn NLSentence 
        (ConcatenatePhrasesFn 
          (BestDetNbarFn 
            (TermParaphraseFn Every-NLAttr) 
            (TermParaphraseFn :ARG1)) 
          (BestHeadVerbForInitialSubjectFn Be-TheWord) 
          (BestNLPhraseOfStringFn "designed for") 
          (TermParaphraseFn-Constrained gerund :ARG2)))))


(genTemplate commonCondimentFor 
       (ConcatenatePhrasesFn 
           (TermParaphraseFn-NP :ARG1) 
           (BestHeadVerbForInitialSubjectFn Go-TheWord) 
           (BestNLPhraseOfStringFn "well") 
           (BestPPFn With-TheWord 
               (TermParaphraseFn-NP :ARG2))))


(genTemplate soleMakerOfProductType 
       (ConcatenatePhrasesFn 
           (TermParaphraseFn-NP :ARG1) 
           (PhraseFormFn presentTense-Generic 
               (BestHeadVerbForInitialSubjectFn Be-TheWord)) 
           (BestNLPhraseOfStringFn "the maker of") 
           (TermParaphraseFn-NP :ARG2)))

(genTemplate possesses 
       (ConcatenatePhrasesFn 
           (TermParaphraseFn-NP :ARG1) 
           (BestNLPhraseOfStringFn "possessed") 
           (TermParaphraseFn-NP :ARG2)))

      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Mining #$ConcatenatePhrasesFn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gplist-to-template (phrases)
  (ret (mapcar #'gphrase-to-template phrases)))

(define join-e (list last) (ret (append list (list last))))

(define gphrase-to-template (phrase)
  (clet ((a (car phrase))(d (cdr phrase)))
  (pcase a
     (#$ConcatenatePhrasesFn
       (ret (cons #$NLPatternList (gplist-to-template d))))
    ;;; (def-gen-rtp (#$BestHeadVerbForInitialSubjectFn ?Occur-TheWord) (#$NLPattern-Word ?Occur-TheWord #$Verb))
     (#$BestHeadVerbForInitialSubjectFn
       (ret (cons #$NLPattern-Word (join-e d #$Verb))))
    ;;; (def-gen-rtp (#$TermParaphraseFn-NP ?ARG1) (#$NLPattern-Template #$NPTemplate ?ARG1))
     (#$TermParaphraseFn-NP
       (ret (cons #$NLPattern-Template (cons #$NPTemplate d))))
    ;;; (def-gen-rtp (#$TermParaphraseFn-Constrained ?nonPlural-Generic ?ARG1) (#$NLPattern-Word ?ARG1 ?nonPlural-Generic))
     (#$TermParaphraseFn-Constrained
       (ret (cons #$NLPattern-Word (join-e (cdr d) (car d)))))
;;;;;;;; TODOs;;;;;;;; TODOs;;;;;;;; TODOs
;; (def-gen-rtp (PhraseFormFn ?presentTense-Generic (BestHeadVerbForInitialSubjectFn ?Be-TheWord)) (#$NLPattern-Word ?Be-TheWord ?presentTense-Generic))

     (t (ret (warn "Cannot figure out how to translate ~S to termTemplate" phrase))))))


 BasicDatumParaphraseFn   BasicTransitive-PassiveSentenceFn   BasicTransitiveSentenceFn   BestBindingsPhraseFn   BestChemicalFormulaFn   BestCoefficientItemPhraseFn-Constrained   BestCycLPhraseFn   BestDetNbarFn   BestDetNbarFn-Definite   BestDetNbarFn-Indefinite   BestEvaluatedIfEvaluatablePhraseFn-Constrained   BestHeadVerbForInitialSubjectFn   BestListParaphraseWithSeparatorAndCoefficientsFn-Constrained   BestListParaphraseWithSeparatorFn-Constrained   BestMathFormulaOperandFn   BestNLPhraseOfStringFn   BestNLWordFormOfLexemeFn   BestNLWordFormOfLexemeFn-Constrained   BestNumberedListParaphraseFn-Constrained   BestPPFn   BestParentheticalPhraseFn   BestPluralOfLexemeFn   BestSetParaphraseWithSeparatorAndCoefficientsFn-Constrained   BestStringOfNLPhraseFn   BestSymbolPhraseFn   BestVerbFormForSubjectFn   ConcatenatePhrasesFn   ConcatenatePhrasesFn-NoSpaces   ConditionalPhraseFn   CurrentListItemFn   DefiniteNbarPPFn   DefiniteNounPPFn   ExplicitIfCollection-NLPhraseFn   GenTemplateRecipeOmitsArgFn   GenValueParaphraseFn   GenerateGKEGlossWrtMtFn   GeneratePhraseFn   HeadWordOfPhraseFn   IndefiniteNbarPPFn   IndefiniteNounPPFn   LatitudeOrLongitudeParaphraseFn   NDecimalPlaceParaphraseFn   NLConjunctionFn   NLDisjunctionFn   NLSimpleBinaryConjunctionFn   NPIsXP-NLSentenceFn   NbarHeadedByNounFormFn   NbarWithPluralHeadFn   NthPhraseAgrFn   NthPhraseFn   PercentParaphraseFn   PhraseCycLFn   PhraseFormFn   PluralParaphraseFn   PluralParaphraseFn-PP   Postmodifier_EdFormFn   PreciseParaphraseFn   QuotedParaphraseFn   RepeatForSubsequentArgsFn   StringMentionFn   StructuredParaphraseFn   SubParaphraseFn   TensedPhraseFn   TensedPhraseFn-DefaultPast   TermParaphraseFn   TermParaphraseFn-CityWithCountryAbbreviation   TermParaphraseFn-CityWithCountryName   TermParaphraseFn-CityWithStateOrProvinceAbbreviation   TermParaphraseFn-CityWithStateOrProvinceName   TermParaphraseFn-Constrained   TermParaphraseFn-CountyWithStateOrProvinceAbbreviation   TermParaphraseFn-CountyWithStateOrProvinceName   TermParaphraseFn-NP   TermParaphraseFn-NP-QuaType   TermParaphraseFn-NPAndNP   TermParaphraseFn-PP   TermParaphraseFn-PP-Constrained   TermParaphraseFn-PhysicalLocation   TermParaphraseFn-Possessive   TermParaphraseFn-Possessive-Constrained   TermParaphraseFn-TemporalLocation   TermParaphraseFn-TemporalLocation-Date   TerseParaphraseFn   ThePrototypicalNLGenerationFunction   TwoDigitYearParaphraseFn   TypeClarifyingPhraseFn   UnlinkedSubParaphraseFn   UnlinkedSubParaphraseWithHTMLFn   XActsAsAYInZ-NLSentenceFn   XHasYAsAZ-NLSentenceFn   


(term-template-assert 
 (#$genTemplate ?occurredDuringMonth 
       (#$ConcatenatePhrasesFn 
           (#$TermParaphraseFn-NP :ARG1)
           (#$BestHeadVerbForInitialSubjectFn ?Occur-TheWord) 
           (#$BestNLPhraseOfStringFn ?duringthemonthof) 
           (#$TermParaphraseFn-NP ?ARG2)))

  (#$assertTemplate-Reln #$VPModSubject ?occurredDuringMonth 
   (#$NLPatternList
    (#$NLPattern-Word ?Occur-TheWord #$Verb) 
    (#$SubstituteFormulaFn #$NLPattern-Exact #$TheList (#$StringTokenizeFn ?duringthemonthof (#$TheList " ")))
    (#$NLPattern-Template #$NPTemplate ?ARG2))
   (?occurredDuringMonth #$TheSentenceSubject ?ARG2)))




|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; TODO Fix #$StringToStringListFn (but might not need it)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+Ignore?
(progn 

(cbnl-assert `(#$isa ,(find-or-create-constant "StringToStringListFn") #$UnaryFunction) #$UniversalVocabularyMt)
(ke-assert-now `(#$implies 
   (#$evaluate (#$TheList . ?toks) (#$StringTokenizeFn ?duringthemonthof (#$TheList " ")))
   (#$evaluate ?toks (#$StringToStringListFn ?duringthemonthof))) #$BaseKB)
(cbnl-assert 
 `(#$and
   (#$isa #$StringToStringListFn #$EvaluatableFunction)
   (#$isa #$StringToStringListFn #$IndividualDenotingFunction)
   ) #$UniversalVocabularyMt)

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; #$NPPreSubject a Pre-NounPhraseModifyingFrame 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-template-cat "NPPreMod")

(cbnl-assert  
 `(#$termTemplate-Reln #$NPTemplate #$NPPreMod
   (#$NLPatternList     
    (#$NLPattern-Template #$NPPreMod :VPHRASE)
    (#$NLPattern-Template #$NPTemplate :SUBJ))
        (#$SubstituteFormulaFn ?NEW-REF #$TheSentenceSubject 
	  (#$TheOneOf :VPHRASE 
	   (#$equals :SUBJ #$TheSentenceSubject))))
	   #$EnglishTemplateMt)

;; (logicmoo-rkf-reader "the port number for machine 666" #$NPTemplate)
(term-template-assert 
 (#$genTemplate ?portNumberForSKS 
       (#$NPIsXP-NLSentenceFn 
           (#$PhraseFormFn #$singular 
               (#$ConcatenatePhrasesFn 
                   (#$BestNLPhraseOfStringFn ?theportnumberfor) 
                   (#$TermParaphraseFn :ARG1))) 
           (#$TermParaphraseFn :ARG2)))
 (#$termTemplate-Reln #$NPTemplate #$NPPreMod
   (#$NLPatternList 
    (#$SubstituteFormulaFn #$NLPattern-Exact #$TheList (#$StringTokenizeFn ?theportnumberfor (#$TheList " ")))
    (#$NLPattern-Template #$NPTemplate :ARG1))
    (#$TheOneOf #$TheSentenceSubject (?portNumberForSKS :ARG1 #$TheSentenceSubject))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; #$NPPostMod a Post-NounPhraseModifyingFrame 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-template-cat "NPPostMod")

(cbnl-assert  
 `(#$termTemplate-Reln #$NPTemplate #$NPPostMod
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJ) 
    (#$NLPattern-Template #$NPPostMod :VPHRASE))
        (#$SubstituteFormulaFn ?NEW-REF #$TheSentenceSubject 
	  (#$TheOneOf :VPHRASE 
	   (#$equals :SUBJ #$TheSentenceSubject))))
	   #$EnglishTemplateMt)



;;  (parse "alex is addicted to it")
;;  (parse "alex who is addicted to it")
;;  (parse "alex is topologically inside sea")
;;  (parse "addicted to it")
(cbnl-assert  
 `(#$assertTemplate-Reln #$STemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJ)
    (#$NLPattern-Word #$Be-TheWord #$Verb) 
    (#$NLPattern-Template #$NPPostMod :VPHRASE))
        (#$SubstituteFormulaFn ?NEW-REF #$TheSentenceSubject 
	  (#$equals :SUBJ :VPHRASE)))
	   #$EnglishTemplateMt)



;;  (logicmoo-rkf-reader "alex topologically inside joe" #$NPTemplate)

(term-template-assert 
 (#$genTemplate ?topologicallyInsideSpaceRegion 
       (#$NPIsXP-NLSentenceFn 
           (#$TermParaphraseFn-NP :ARG1) 
           (#$ConcatenatePhrasesFn 
               (#$BestNLPhraseOfStringFn ?topologicallyinside) 
               (#$TermParaphraseFn-NP :ARG2))))
  (#$termTemplate-Reln #$NPPostMod ?topologicallyInsideSpaceRegion 
   (#$NLPatternList
    (#$SubstituteFormulaFn #$NLPattern-Exact #$TheList (#$StringTokenizeFn ?topologicallyinside (#$TheList " ")))
    (#$NLPattern-Template #$NPTemplate :ARG2))
   (#$TheOneOf #$TheSentenceSubject (?topologicallyInsideSpaceRegion #$TheSentenceSubject :ARG2))))


;; (logicmoo-rkf-reader "joe supervised by alex" #$NPTemplate)

(term-template-assert 
 (#$genTemplate ?supervisesInAction 
       (#$NPIsXP-NLSentenceFn 
           (#$TermParaphraseFn-Constrained #$nonPlural-Generic :ARG2) 
           (#$ConcatenatePhrasesFn 
               (#$BestNLPhraseOfStringFn ?supervised) 
               (#$BestPPFn ?By-TheWord 
                   (#$TermParaphraseFn-NP :ARG1)))))
  (#$termTemplate-Reln #$NPPostMod ?supervisesInAction 
  (#$NLPatternList  
    (#$SubstituteFormulaFn #$NLPattern-Exact #$TheList (#$StringTokenizeFn ?supervised (#$TheList " ")))
    (#$NLPattern-Word ?By-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :ARG1)) 
   (#$TheOneOf #$TheSentenceSubject (?supervisesInAction :ARG1 #$TheSentenceSubject))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; #$VPModSubject
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TODO (parse "joe threatened to fire on it during then month of May")
(term-template-assert 
 (#$genTemplate ?occurredDuringMonth 
       (#$ConcatenatePhrasesFn 
           (#$TermParaphraseFn-NP :ARG1)
           (#$BestHeadVerbForInitialSubjectFn ?Occur-TheWord) 
           (#$BestNLPhraseOfStringFn ?duringthemonthof) 
           (#$TermParaphraseFn-Constrained #$nonPlural-Generic :ARG2)))

  (#$assertTemplate-Reln #$VPModSubject ?occurredDuringMonth 
   (#$NLPatternList
    (#$NLPattern-Word ?Occur-TheWord #$Verb) 
    (#$SubstituteFormulaFn #$NLPattern-Exact #$TheList (#$StringTokenizeFn ?duringthemonthof (#$TheList " ")))
    (#$NLPattern-Template #$NPTemplate :ARG2))
   (?occurredDuringMonth #$TheSentenceSubject :ARG2)))



;; (parse "joe threatened to fire on it") => (#$unitThreaten-TFT ?joe ?it)
(term-template-assert 
 (#$genTemplate ?occurredDuringMonth 
       (#$ConcatenatePhrasesFn 
           (#$TermParaphraseFn-NP :ARG1)
           (#$BestHeadVerbForInitialSubjectFn ?Occur-TheWord) 
           (#$BestNLPhraseOfStringFn ?duringthemonthof) 
           (#$TermParaphraseFn-NP ?ARG2)))

  (#$assertTemplate-Reln #$VPModSubject ?occurredDuringMonth 
   (#$NLPatternList
    (#$NLPattern-Word ?Occur-TheWord #$Verb) 
    (#$SubstituteFormulaFn #$NLPattern-Exact #$TheList (#$StringTokenizeFn ?duringthemonthof (#$TheList " ")))
    (#$NLPattern-Template #$NPTemplate ?ARG2))
   (?occurredDuringMonth #$TheSentenceSubject ?ARG2)))

(term-template-assert 
 (#$genTemplate ?possesses 
       (#$ConcatenatePhrasesFn 
           (#$TermParaphraseFn-NP :ARG1)
           (#$BestNLPhraseOfStringFn ?possessesstr) 
           (#$TermParaphraseFn-NP ?ARG2)))

  (#$assertTemplate-Reln #$VPModSubject ?possesses
   (#$NLPatternList
    (#$SubstituteFormulaFn #$NLPattern-Exact #$TheList (#$StringTokenizeFn ?possessesstr (#$TheList " ")))
    (#$NLPattern-Template #$NPTemplate ?ARG2))
   (?possesses #$TheSentenceSubject ?ARG2)))


(term-template-assert 
 (#$genTemplate ?possesses 
  (#$PhraseFormFn #$NLSentence 
       (#$ConcatenatePhrasesFn 
           (#$TermParaphraseFn-NP :ARG1)
           (#$BestHeadVerbForInitialSubjectFn ?Possess-TheWord)
           (#$TermParaphraseFn-NP ?ARG2))))

  (#$assertTemplate-Reln #$VPModSubject ?possesses
   (#$NLPatternList
    (#$NLPattern-Word ?Possess-TheWord #$Verb)
    (#$NLPattern-Template #$NPTemplate ?ARG2))
   (?possesses #$TheSentenceSubject ?ARG2)) #$EnglishParaphraseMt)


(term-template-assert 
 (#$genTemplate ?restrictsMovement 
       (#$ConcatenatePhrasesFn 
           (#$TermParaphraseFn-NP :ARG1)
           (#$BestHeadVerbForInitialSubjectFn ?Restrict-TheWord)
           (#$BestNLPhraseOfStringFn ?themovement) 
           (#$BestPPFn ?Of-TheWord  (#$TermParaphraseFn-NP ?ARG2))))

  (#$assertTemplate-Reln #$VPModSubject ?restrictsMovement
   (#$NLPatternList
    (#$NLPattern-Word ?Restrict-TheWord #$Verb)
    (#$SubstituteFormulaFn #$NLPattern-Exact #$TheList (#$StringTokenizeFn ?themovement (#$TheList " ")))
    (#$NLPattern-Word ?Of-TheWord #$Preposition)
    (#$NLPattern-Template #$NPTemplate ?ARG2))
   (?restrictsMovement #$TheSentenceSubject ?ARG2)))






;; (parse "joe is southwest of alex") => (#$unitThreaten-TFT ?joe ?it)
(term-template-assert 
 (#$genTemplate ?southwest 
       (#$ConcatenatePhrasesFn 
           (#$TermParaphraseFn-NP :ARG1) 
           (#$PhraseFormFn #$presentTense-Generic 
               (#$BestHeadVerbForInitialSubjectFn ?Be-TheWord)) 
           (#$BestNLPhraseOfStringFn ?southwestof) 
           (#$TermParaphraseFn-NP :ARG2)))
  (#$assertTemplate-Reln #$VPModSubject ?southwest 
   (#$NLPatternList
    (#$NLPattern-Word ?Be-TheWord #$Verb) 
    (#$SubstituteFormulaFn #$NLPattern-Exact #$TheList (#$StringTokenizeFn ?southwestof (#$TheList " ")))
    (#$NLPattern-Template #$NPTemplate :ARG2))
   (?southwest #$TheSentenceSubject :ARG2)))


(print ";;;;;;;; Done cogbot-parser-template.lisp")(force-output)
;;(extend-rtp-from-mt nil *rtp-default-template-types*)




;;(parse "You are standing at the portal")

