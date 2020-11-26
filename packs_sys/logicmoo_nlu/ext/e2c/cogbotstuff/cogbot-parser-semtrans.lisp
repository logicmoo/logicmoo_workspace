;;;{{{DOC
;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Copyright (c) 2002 - 2010 Douglas R. Miles.  All rights reserved.
;;;
;;; @module COGBOT-PARSER-SEMTRANS
;;; @features :COGBOT-NL
;;;
;;; @author dmiles
;;; @owner dmiles
;;;
;;; @created 2010/01/10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tools & utilities for Cogbot/NLParsing
;;  
;; This will mine the #$SemTransPred(s) and try to produce RTP visible assertions
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


(define set-rule-forward-cons-pred (term)
  (cdolist (ass (all-term-assertions term))
     (pwhen (backward-rule? ass)
      (clet ((cons  (third (assertion-el-formula ass))))
       (pwhen (consp cons)
       (pwhen (eq term (first cons))
         (print ass)
		 (tms-change-direction ass :FORWARD)
		 ))))))

(cdolist (v (query-template '?X `(#$isa ?X #$SemTransPred) #$EverythingPSC))
  (set-rule-forward-cons-pred v))


;; (load "cynd/cogbot-parser-semtrans.lisp")

;; Prequesite
 (load "cynd/cogbot-parser.lisp")

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
;; Used to convert STemplate to action terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-template-cat "STemplate-Action")
(cbnl-assert `
  (#$termTemplate-Reln #$STemplate-Action #$STemplate
   (#$NLPatternList
    (#$NLPattern-Template #$STemplate :ACTION))
  (#$AchieveFn :ACTION)) *TMPL-STORE*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STemplate-Clause
;; Used to convert STemplate to action terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-template-cat "STemplate-Clause")
(cbnl-assert `
  (#$termTemplate-Reln #$STemplate-Clause #$STemplate
   (#$NLPatternList
    (#$NLPattern-Template #$STemplate :ACTION))
  (#$Simply :ACTION)) *TMPL-STORE*)

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
'(term-template-assert 
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

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; X IS A Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ke-assert-now
`(#$assertTemplate-Reln #$STemplate #$is-Underspecified 
  (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :ARG1) 
    (#$NLPattern-Word #$Be-TheWord #$Verb) 
    (#$NLPattern-Exact "a") 
    (#$NLPattern-Template #$NPTemplate :ARG2)) 
  (#$is-Underspecified :ARG1 :ARG2)) *TMPL-STORE*)

;; (parse "all dogs are cats")
(ke-assert-now
`(#$assertTemplate-Reln #$STemplate #$genls
  (#$NLPatternList
    (#$NLPattern-Word #$All-TheWord #$Determiner-Central)
    (#$NLPattern-Agr :ARG1 #$plural)
    (#$NLPattern-Exact "are") 
    (#$NLPattern-POS :ARG2 #$CommonNoun))
  (#$genls :ARG1 :ARG2)) *TMPL-STORE*)

;; (parse "all dogs are good")
'(ke-assert-now
`(#$assertTemplate-Reln #$STemplate #$relationAllInstance
  (#$NLPatternList
    (#$NLPattern-Word #$All-TheWord #$Determiner-Central)
    (#$NLPattern-Agr :ARG1 #$plural)
    (#$NLPattern-Exact "are") 
    (#$NLPattern-POS :ARG2 #$Adjective))
  (#$relationAllInstance #$isa :ARG1 :ARG2)) *TMPL-STORE*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  #$prepSemTrans / #$VerbPhraseModifyingFrame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(with-UnderspecifiedAgent :ACTION :OBJECT)
(term-template-assert 
 (#$prepSemTrans ?With-TheWord ??LOGICMOO #$VerbPhraseModifyingFrame ?CYCL) 
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Template #$VPModSubject :VPHRASE)
    (#$NLPattern-Word ?With-TheWord #$Preposition)
    (#$NLPattern-Template #$NPTemplate :OBJECT))
        (#$and ?CYCL :VPHRASE)))

#|
;;;;;;;;;;;;;;;; PREP SEM TRANS
;; (logicmoo-rkf-reader "the ship" #$NPTemplate)
;; (logicmoo-rkf-reader "the ship" #$STemplate)
;; (logicmoo-rkf-reader "onboard ship" #$NPPostMod)
;; (logicmoo-rkf-reader "alex onboard the ship" #$NPTemplate)
'(term-template-assert 
  (#$prepSemTrans ?On-TheWord ?LOGICMOO #$Post-NounPhraseModifyingFrame ?CYCL)
  (#$termTemplate #$NPPostMod 
   (#$NLPatternList  
    (#$NLPattern-Word ?On-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :OBJECT)) 
   (#$TheOneOf :NOUN ?CYCL)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  #$prepSemTrans / #$Post-NounPhraseModifyingFrame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO why not working?
(term-template-assert 
(#$and (#$subcatFrame ?With-TheWord #$Preposition ??LOGICMOO #$Post-NounPhraseModifyingFrame) 
   (#$prepSemTrans ?With-TheWord ??LOGICMOO #$Post-NounPhraseModifyingFrame ?CYCL))
  (#$termTemplate-Reln #$NPTemplate #$Post-NounPhraseModifyingFrame
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :NOUN)
    (#$NLPattern-Word ?With-TheWord #$Preposition)
    (#$NLPattern-Template #$NPTemplate :OBJECT))
        (#$TheOneOf :NOUN ?CYCL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  #$adverbSemTrans / #$VPModSubject
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (parse "he ate the apple gladly")
;; he gladly ate, he ate gladly, 
(term-template-assert 
(#$adverbSemTrans ?Glad-TheWord ??LOGICMOO #$VerbPhraseModifyingFrame ?CYCL) 
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
      (#$RequireOne
	(#$WordSequence 
	    (#$NLPattern-Template #$VPModSubject :VPHRASE)
	    (#$NLPattern-Word ?Glad-TheWord #$Adverb))
        (#$WordSequence 
	    (#$NLPattern-Word ?Glad-TheWord #$Adverb)
	    (#$NLPattern-Template #$VPModSubject :VPHRASE))
    ))
    (#$and ?CYCL :VPHRASE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; #$VPModSubject for assertTemplates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (logicmoo-rkf-reader "joe likes fish" #$STemplate)
(term-template-assert 
 (#$and 
  (#$isa ?LIKES-GENERIC #$AgentPredicate) 
  (#$isa ?LIKES-GENERIC #$BinaryPredicate) 
  (#$denotation ?LIKE-THEWORD #$Verb ??N ?LIKES-GENERIC)) 
  
  (#$assertTemplate-Reln #$VPModSubject ?LIKES-GENERIC 
   (#$NLPatternList  
    (#$NLPattern-Word ?LIKE-THEWORD #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT)) 
   (?LIKES-GENERIC #$TheSentenceSubject :OBJECT)))


;; (logicmoo-rkf-reader "joe suspected him" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Suspect-TheWord ??LOGICMOO #$TransitiveNPFrame ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Suspect-TheWord #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))


;; (logicmoo-rkf-reader "joe jumped" #$STemplate)
;; (logicmoo-rkf-reader "jumped" #$VPModSubject)
(term-template-assert 
 (#$verbSemTrans ?Jumped-TheWord ??LOGICMOO #$IntransitiveVerbFrame ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Jumped-TheWord #$Verb))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;; (logicmoo-rkf-reader "joe jumped" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Jumped-TheWord ??LOGICMOO #$MiddleVoiceFrame ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Jumped-TheWord #$Verb))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))


(term-template-assert 
  (#$verbPrep-TransitiveTemplate ?Paying ?To-TheWord ?CYCL)
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Term ?Paying #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT)
    (#$NLPattern-Word ?To-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$SubstituteFormulaFn ?Paying :DENOTS ?CYCL))))

;; (logicmoo-rkf-reader "joe gnawed on fish" #$STemplate) ==> (#$and (#$isa :ACTION #$Chewing) (#$objectActedOn :ACTION #$AvrahamFish) (#$performedBy :ACTION #$JosephCorneli))
(term-template-assert 
 (#$verbSemTrans ?Gnaw-TheWord ??LOGICMOO (#$PPCompFrameFn #$TransitivePPFrameType ?On-TheWord) ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Gnaw-TheWord #$Verb) 
    (#$NLPattern-Word ?On-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;;  (logicmoo-rkf-reader "joe opened fire on fish" #$STemplate) ==> (#$and (#$intendedAttackTargets :ACTION #$AvrahamFish) (#$isa :ACTION #$ShootingAProjectileWeapon) (#$performedBy :ACTION #$JosephCorneli)) 
;; TODO  (logicmoo-rkf-reader "joe opened fire on a fish" #$STemplate)
(term-template-assert 
 (#$compoundSemTrans ?Open-TheWord (#$TheList . ?fire) #$Verb (#$PPCompFrameFn #$TransitivePPFrameType ?On-TheWord) ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Open-TheWord #$Verb) 
	(#$NLPattern-Exact . ?fire)
    (#$NLPattern-Word ?On-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;; (logicmoo-rkf-reader "Joe smelled himself" #$STemplate) ==> (#$smells #$JosephCorneli (#$PronounFn #$ThirdPerson-NLAttr #$Singular-NLAttr #$Masculine-NLAttr #$ReflexivePronoun))
;; (logicmoo-rkf-reader "Joe smells" #$STemplate) ==> (#$objectEmitsOdor #$JosephCorneli #$FoulOdor)
;;works (logicmoo-rkf-reader "Joe smells like fish" #$STemplate) ==> (#$objectEmitsOdor #$JosephC (#$OdorFn #$Fish))
;;TODO (logicmoo-rkf-reader "Joe smells like Joe" #$STemplate) 
(term-template-assert 
 (#$compoundSemTrans ?Smells-TheWord (#$TheList . ?like) #$Verb #$TransitiveNPFrame ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Smells-TheWord #$Verb) 
	(#$NLPattern-Exact . ?like)
    (#$NLPattern-Template #$NPTemplate :OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

(term-template-assert 
 (#$verbSemTrans ?Put-TheWord ??LOGICMOO (#$PPCompFrameFn #$DitransitivePPFrameType ?In-TheWord) ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Put-TheWord #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT)
    (#$NLPattern-Word ?In-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;;works (logicmoo-rkf-reader "I cut back him" #$STemplate)
;;doesnt (logicmoo-rkf-reader "I cut back the trees" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Cut-TheWord ??LOGICMOO (#$ParticleCompFrameFn #$TransitiveParticleNPFrameType ?Back-TheWord) ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Cut-TheWord #$Verb) 
    (#$NLPattern-Word ?Back-TheWord #$VerbParticle) 
    (#$NLPattern-Template #$NPTemplate :OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))


(term-template-assert 
 (#$verbSemTrans ?Cut-TheWord ??LOGICMOO (#$ParticleCompFrameFn #$TransitiveParticleNPFrameType ?Back-TheWord) ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Cut-TheWord #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT)
    (#$NLPattern-Word ?Back-TheWord #$VerbParticle))   
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clause Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;That
;; "I hope that he was there"
;;TODO (logicmoo-rkf-reader "Joe hopes that he was there" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Hope-TheWord ??LOGICMOO #$TransitiveThatClauseFrame ?CYCL)
  
  (#$termTemplate #$NPInfTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Hope-TheWord #$Verb) 
    (#$NLPattern-Word #$That-TheWord #$Determiner-Definite) 
    (#$NLPattern-Template #$STemplate-Clause :CLAUSE))
  (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;;;;;;;;;;;;;;;;;;;;WHWord
;;TODO (logicmoo-rkf-reader "Joe asked who was there" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Ask-TheWord ??LOGICMOO #$TransitiveWhClauseFrame ?CYCL)
  
  (#$termTemplate #$NPInfTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Ask-TheWord #$Verb)
    (#$NLPattern-POS :WH-WORD #$WHWord) 
    (#$NLPattern-Template #$STemplate-Clause :CLAUSE))
   ?CYCL))

;;;;;;;;;;;;;;;;;;;;For
;;TODO (logicmoo-rkf-reader "Joe waits for sue to eat" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Wait-TheWord ??LOGICMOO #$TransitiveForNPInfinitivePhraseFrame ?CYCL)
  
  (#$termTemplate #$NPInfTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Wait-TheWord #$Verb) 
    (#$NLPattern-Word #$For-TheWord #$Preposition)
    (#$NLPattern-Template #$STemplate-Clause :OBJECT))
  ?CYCL))


;;;;;;;;;;;;;;;;;;;;Any
;;TODO (logicmoo-rkf-reader "Joe was able to buy bread" #$STemplate)
(term-template-assert 
 (#$compoundSemTrans ?Be-TheWord (#$TheList . ?able) #$Verb #$TransitiveInfinitivePhraseFrame ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Be-TheWord #$Verb)
	(#$NLPattern-Exact . ?able)
    (#$NLPattern-Template #$STemplate-Clause :CLAUSE))
  ?CYCL))


;;;;;;;;;;;;;;;;;;;;Any + optional That
;;(logicmoo-rkf-reader "Joe thinks [that] he likes Joe" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Think-TheWord ??LOGICMOO #$TransitiveInfinitivePhraseFrame ?CYCL)
   
  (#$termTemplate #$NPInfTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Think-TheWord #$Verb)
    (#$OptionalOne (#$NLPattern-Word #$That-TheWord #$Determiner-Definite))
    (#$NLPattern-Template #$STemplate-Clause :CLAUSE))
  (#$SubstituteFormulaFn :CLAUSE :INF-COMP ?CYCL)))

;;;;;;;;;;;;;;;;Yeild a #$VPModSubject
(cbnl-assert `
  (#$termTemplate-Reln #$VPModSubject #$NPInfTemplate
   (#$NLPatternList
    (#$NLPattern-Template #$NPInfTemplate :ACTION))
  (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT :ACTION)) *TMPL-STORE*)

;;;;;;;;;;;;;;;;Yeild a NP
(cbnl-assert `
 (#$assertTemplate-Reln #$NPTemplate #$NPInfTemplate 
       (#$NLPatternList 
           (#$NLPattern-Word #$The-TheWord  #$Determiner-Definite) 
	    (#$NLPattern-Template #$NPInfTemplate :ACTION))
	   ;; remove the previous Sentence subject
	 (#$SubstituteFormulaFn ??SOME-SUBJ #$TheSentenceSubject :ACTION)) *TMPL-STORE*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intransitive Verb Phrases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(logicmoo-rkf-reader "Joe broke out" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Jack-TheWord ??LOGICMOO (#$ParticleCompFrameFn #$IntransitiveParticleFrameType ?Off-TheWord) ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
      (#$NLPattern-Word ?Jack-TheWord #$Verb) 
    (#$NLPattern-Word ?Off-TheWord #$VerbParticle))
     (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT 
      (#$and (#$situationConstituents :ACTION :SUBJECT) ?CYCL))))


;;(logicmoo-rkf-reader "Joe sat down" #$STemplate)
(term-template-assert 
(#$compoundSemTrans ?Sat-TheWord (#$TheList . ?down) #$Verb #$IntransitiveVerbFrame ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Sat-TheWord #$Verb) 
    (#$NLPattern-Exact . ?down))
  (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;; (logicmoo-rkf-reader "Joe sat down" #$STemplate)
(term-template-assert 
(#$compoundSemTrans ?Sat-TheWord (#$TheList . ?down) #$Verb #$MiddleVoiceFrame ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Sat-TheWord #$Verb) 
    (#$NLPattern-Exact . ?down))
  (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STemplate/semTransLiteralForPrepAndFrame #$STemplate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(semTransLiteralForPrepAndFrame In-TheWord VNSubcatFrame-NP-PPInducedActionwithoutaccompaniedmotionpath-PP 
;;       (eventOccursAt :ACTION :OBLIQUE-OBJECT))
(term-template-assert 
(#$semTransLiteralForPrepAndFrame ?In-TheWord #$VNSubcatFrame-NP-PPInducedActionwithoutaccompaniedmotionpath-PP  ?CYCL)  
  (#$assertTemplate #$STemplate
   (#$NLPatternList
    (#$NLPattern-Template #$STemplate-Action :ACTION)
    (#$NLPattern-Word ?In-TheWord #$Preposition) 
     (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
  ?CYCL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STemplate/compoundString #$Verb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (logicmoo-rkf-reader "Joe made a facial expression" #$STemplate)
(term-template-assert-fw 
 (#$compoundString #$Make-TheWord (#$TheList . ?expression) #$Verb ?MakingFacialExpression)
 (#$compoundSemTrans #$Make-TheWord (#$TheList . ?expression) #$Verb #$MiddleVoiceFrame 
  (#$and (#$isa :ACTION ?MakingFacialExpression)(#$doneBy :ACTION :SUBJECT))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/compoundString #$CountNoun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (logicmoo-rkf-reader "chatroom for sports fans" #$NPTemplate)
(term-template-assert-fw 
 (#$compoundString ?Chatroom-TheWord (#$TheList . ?expression) #$CountNoun ?SportsChatRoom)
 (#$compoundSemTrans ?Chatroom-TheWord (#$TheList . ?expression) #$CountNoun #$RegularNounFrame 
   (#$TheOneOf :NOUN (#$isa :NOUN ?SportsChatRoom))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/denotation #$CountNoun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+Very-slow
(term-template-assert 
 (#$denotation ?Menage-TheWord #$CountNoun ??LOGICMOO ?CohabitationUnit)
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Menage-TheWord #$CountNoun))
   (#$TheOneOf :NOUN (#$isa :NOUN ?CohabitationUnit))))

;; Questionable
(cbnl-assert `(#$implies
  (#$compoundString ?The-TheWord  (#$TheList . ?battle-for-hue) #$Determiner-Definite ?BattleForHue)
 (#$compoundString ?The-TheWord (#$TheList . ?battle-for-hue) #$ProperNoun ?BattleForHue)) ;; not count noun?
 *RULEKB*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/denotation #$CountNoun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(logicmoo-rkf-reader "purplish book" #$NPTemplate)
(term-template-assert 
 (#$adjSemTrans ?Purplish-TheWord ??LOGICMOO #$RegularAdjFrame ?CYCL)
 (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Purplish-TheWord #$Adjective-Gradable)
     (#$NLPattern-Template #$NPTemplate :NOUN))
   (#$TheOneOf :NOUN ?CYCL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/#$NounSemTransPred
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; must exclude 

#+REAL-SLOW-BUT-MAYBE-PREPLACES-STUFF
(progn 

(term-template-assert 
 (#$and 
  (#$isa ?nounSemTrans #$NounSemTransPred )
  (#$semTransPredForPOS ?MassNoun ?nounSemTrans)
  (?nounSemTrans ?Allege-TheWord ??LOGICMOO #$TransitiveThatClauseFrame ?CYCL))
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Allege-TheWord ?MassNoun) 
    (#$NLPattern-Word #$That-TheWord #$Determiner-Definite))
    (#$TheOneOf :NOUN ?CYCL)))


(term-template-assert 
 (#$and 
  (#$isa ?nounSemTrans #$NounSemTransPred )
  (#$semTransPredForPOS ?MassNoun ?nounSemTrans)
  (#$not (#$equals ?MassNoun #$WHWord))
  (?nounSemTrans ?Idiot-TheWord ??LOGICMOO #$RegularNounFrame ?CYCL))
 (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$OptionalOne "a" "an" "the")
    (#$NLPattern-Word ?Idiot-TheWord ?MassNoun))
   (#$TheOneOf :NOUN ?CYCL)))

;;(logicmoo-rkf-reader "the litterer" #$NPTemplate)
(term-template-assert 
 (#$and 
  (#$isa ?nounSemTrans #$NounSemTransPred )
  (#$semTransPredForPOS ?MassNoun ?nounSemTrans)
  (#$not (#$equals ?MassNoun #$WHWord))
   (?nounSemTrans ?Litter-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL))
  (#$termTemplate #$NPTemplate
   (#$NLPatternList
      (#$OptionalOne "a" "an" "the")
    (#$NLPattern-Word ?Litter-TheWord #$AgentiveNoun))
   (#$TheOneOf :POSSESSOR ?CYCL)))


(term-template-assert 
 (#$and 
  (#$isa ?nounSemTrans #$NounSemTransPred )
  (#$semTransPredForPOS ?MassNoun ?nounSemTrans)
  (?nounSemTrans ?Shock-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL))
 (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun)
    (#$NLPattern-Word #$Of-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :POSSESSOR))   
    (#$TheOneOf :NOUN ?CYCL)))

(term-template-assert 
 (#$and 
  (#$isa ?nounSemTrans #$NounSemTransPred )
  (#$semTransPredForPOS ?MassNoun ?nounSemTrans)
  (?nounSemTrans ?Shock-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL))
  (#$termTemplate #$NPTemplate
   (#$NLPatternList
    (#$NLPattern-Template #$PossessiveTemplate :POSSESSOR) 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun))
    (#$TheOneOf :NOUN ?CYCL)))






)

(cbnl-assert `
 (#$termTemplate #$NPTemplate
       (#$NLPatternList 
           (#$NLPattern-Word #$The-TheWord  #$Determiner-Definite) 
	    (#$NLPattern-Template #$NBarTemplate :SUBJECT))
	   ;; remove the previous Sentence subject
	 (#$The :SUBJECT)) *TMPL-STORE*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/massNounSemTrans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;; #$RegularNounFrame ;;;;;;;;;;;;;;;;;;;;;
;;; (logicmoo-rkf-reader "elite" #$NPTemplate)
;; (logicmoo-rkf-reader "the elitist" #$NPTemplate)
(term-template-assert 
(#$massNounSemTrans ?Elite-TheWord ??LOGICMOO #$RegularNounFrame ?CYCL)  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$OptionalOne "a" "an" "the")
    (#$NLPattern-Word ?Elite-TheWord #$MassNoun))
   (#$TheOneOf :NOUN ?CYCL)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/nounSemTrans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;; #$NumberFrame ;;;;;;;;;;;;;;;;;;;;;
;;BAD STILL (logicmoo-rkf-reader "one mark" #$NPTemplate)
(term-template-assert 
  (#$nounSemTrans ?Mark-TheWord ??LOGICMOO #$NumberFrame ?CYCL)
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NumberTemplate :NUMBER) 
    (#$NLPattern-Word ?Mark-TheWord #$CountNoun))
   (#$TheOneOf :NOUN ?CYCL)))

;;;;;;;;;;; #$RegularNounFrame ;;;;;;;;;;;;;;;;;;;;;
;;; (logicmoo-rkf-reader "idiot" #$NPTemplate)
;; (logicmoo-rkf-reader "2 idiots" #$NPTemplate)
(term-template-assert 
(#$nounSemTrans ?Idiot-TheWord ??LOGICMOO #$RegularNounFrame ?CYCL)  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$OptionalOne "a" "an" "the")
    (#$NLPattern-Word ?Idiot-TheWord #$Noun))
   (#$TheOneOf :NOUN ?CYCL)))

;;;;;;;;;;;; #$GenitiveFrame + OF ;;;;;;;;;;;;;;
;; (logicmoo-rkf-reader "shocker of Joe" #$NPTemplate)
(term-template-assert 
(#$nounSemTrans ?Shock-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL)  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun)
    (#$NLPattern-Word #$Of-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :POSSESSOR) 
    )
    (#$TheOneOf :NOUN ?CYCL)))


;;;;;;;;;;;; #$GenitiveFrame + 'S ;;;;;;;;;;;;;;
;;BAD STILL (logicmoo-rkf-reader "Joe's shocker" #$NPTemplate)
(term-template-assert 
(#$nounSemTrans ?Shock-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL)  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$PossessiveTemplate :POSSESSOR) 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun))
    (#$TheOneOf :NOUN ?CYCL)))


;;;;;;;;;;;; #$GenitiveFrame + PLAIN ;;;;;;;;;;;;;;
(logicmoo-rkf-reader "the litterer" #$NPTemplate)
(term-template-assert 
(#$nounSemTrans ?Litter-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL)  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Litter-TheWord #$AgentiveNoun))
   (#$TheOneOf :POSSESSOR ?CYCL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/nounPrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (logicmoo-rkf-reader "border of France" #$NPTemplate)
(term-template-assert 
(#$nounPrep ?Border-TheWord ?Of-TheWord ?CYCL)  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Border-TheWord #$Noun)
    (#$NLPattern-Word ?Of-TheWord #$Preposition)
    (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
   (#$TheOneOf :NOUN ?CYCL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/multiWordString
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(term-template-assert 
  (#$multiWordString 
       (#$TheList . ?stormy) ?Locate-TheWord #$CountNoun ?ModerateThunderstormArea)
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Exact . ?stormy)
    (#$NLPattern-Word ?Locate-TheWord #$Noun))
   (#$TheOneOf :NOUN (#$isa :NOUN ?ModerateThunderstormArea))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/compoundSemTrans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(term-template-assert 
(#$compoundSemTrans ?City-TheWord (#$TheList . ?slums) ?CountNoun #$RegularNounFrame ?CYCL)
  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?City-TheWord ?CountNoun)
	(#$NLPattern-Exact . ?slums))
   (#$TheOneOf :NOUN ?CYCL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/agentiveNounSemTrans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;; #$RegularNounFrame ;;;;;;;;;;;;;;;;;;;;;
(term-template-assert 
(#$agentiveNounSemTrans ?Shock-TheWord ??LOGICMOO #$RegularNounFrame ?CYCL)  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun))
    (#$TheOneOf :NOUN ?CYCL)))

;;;;;;;;;;; #$GenitiveFrame OF ;;;;;;;;;;;;;;;;;;;;;
;; (logicmoo-rkf-reader "supervisor of Joe" #$NPTemplate)
(term-template-assert 
(#$agentiveNounSemTrans ?Shock-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL)  
 (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun)
    (#$NLPattern-Word #$Of-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :POSSESSOR))    
  (#$TheOneOf :NOUN ?CYCL)))

;;;;;;;;;;;; #$GenitiveFrame 'S ;;;;;;;;;;;;;;
;; (logicmoo-rkf-reader "Joe's" #$PossessiveTemplate)
;;BAD STILL (logicmoo-rkf-reader "Joe's shocker" #$NPTemplate)
(term-template-assert 
(#$agentiveNounSemTrans ?Shock-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL)  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$PossessiveTemplate :POSSESSOR) 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun))
   (#$TheOneOf :NOUN ?CYCL)))



;; (logicmoo-rkf-reader "foot of Joe" #$NBarTemplate)
;; (logicmoo-rkf-reader "the foot of Joe" #$NPTemplate)
#+Redundant?
(term-template-assert 
(#$and 
  (#$elementOf ?POS
    (#$TheSet #$CountNoun #$MassNoun)) 
  (#$isa ?BODY-PART #$AnimalBodyPartType) 
  (#$denotation ?WORD ?POS ?SN ?BODY-PART))
 (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?WORD ?POS)
    (#$NLPattern-Word #$Of-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :POSSESSOR))
	(#$TheOneOf :NOUN (#$and 
        (#$anatomicalParts :POSSESSOR :NOUN) 
        (#$isa :NOUN ?BODY-PART)))))


