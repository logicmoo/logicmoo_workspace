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


;; (load "e2c/cogbot-parser-semtrans.lisp")

;; Prequesite
 (load "e2c/cogbot-parser.lisp")
 (print ";;;;;;;; Loading cogbot-parser-semtrans.lisp")(force-output)


(cpushnew :COGBOT_SLOW_BY_NEEDED *FEATURES*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  #$prepSemTrans / #$VerbPhraseModifyingFrame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(with-UnderspecifiedAgent :ACTION :OBJECT)
(term-template-assert 
 (#$prepSemTrans ?With-TheWord ??LOGICMOO #$VerbPhraseModifyingFrame ?CYCL) 
  (#$termTemplate #$VPModSubjectTrans
   (#$NLPatternList 
    (#$NLPattern-Template #$VPModSubject :VPHRASE)
    (#$NLPattern-Word ?With-TheWord #$Preposition))
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


;; needs  (define CYCL-SENTENCE-ASSERTIBLE? (&rest any) (ret t)) (define CYCL-EXPRESSION? (&rest any) (ret t))
;;from (adverbSemTrans At-Most-MWW 0 DeterminerModifyingFrame  (IntervalMaxFn :DET))
(term-template-assert 
(#$and (#$subcatFrame ?Approximate-TheWord #$Adverb ??LOGICMOO #$DeterminerModifyingFrame) 
   (#$adverbSemTrans ?Approximate-TheWord ??LOGICMOO #$DeterminerModifyingFrame ?CYCL))
  (#$termTemplate-Reln #$NPTemplate #$DeterminerModifyingFrame
   (#$NLPatternList 
    (#$NLPattern-Word ?Approximate-TheWord #$Adverb)
    (#$NLPattern-Template #$NPTemplate :DET))
        ?CYCL))

(term-template-assert 
 (#$and (#$subcatFrame ?Only-TheWord #$Adverb ??LOGICMOO #$Pre-NounPhraseModifyingFrame) 
   (#$adverbSemTrans ?Only-TheWord ??LOGICMOO #$Pre-NounPhraseModifyingFrame ?CYCL))
   (#$termTemplate-Reln #$NPTemplate #$Pre-NounPhraseModifyingFrame
   (#$NLPatternList 
    (#$NLPattern-Word ?Only-TheWord #$Adverb)
    (#$NLPattern-Template #$NPTemplate :OBJECT))
        ?CYCL))


(term-template-assert 
(#$and (#$subcatFrame ?Daily-TheWord #$Adverb ??LOGICMOO #$ClauseModifyingFrame) 
   (#$adverbSemTrans ?Daily-TheWord ??LOGICMOO #$ClauseModifyingFrame ?CYCL))
  (#$termTemplate-Reln #$NPTemplate #$ClauseModifyingFrame
   (#$NLPatternList 
    (#$NLPattern-Word ?Daily-TheWord #$Adverb)
    (#$NLPattern-Template #$NPTemplate :SCOPE)) ?CYCL))


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
;;;;  #$modalVerbSemTrans / #$VPModSubject
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(term-template-assert 
  (#$modalVerbSemTrans ?Should-TheWord ??LOGICMOO #$TransitiveBareInfinitiveFrame  ?CYCL)
  (#$termTemplate #$VPModSubject 
   (#$NLPatternList  
    (#$NLPattern-Word ?Should-TheWord #$Modal) 
    (#$NLPattern-Template #$VPModSubject :CLAUSE))
   ?CYCL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  #$nonDavidsonianPredTemplate / #$VPModSubject / #$TransitiveNPFrame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+ALREADY_COVERED
(term-template-assert 
 (#$nonDavidsonianPredTemplate ?Pruning #$TransitiveNPFrame ??LOGICMOO ?CYCL)  
  (#$termTemplate #$VPModSubjectTrans
   (#$NLPatternList 
    (#$NLPattern-Term ?Pruning #$Verb))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; #$VPModSubject for assertTemplates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (logicmoo-rkf-reader "joe likes fish" #$STemplate)
(term-template-assert 
 (#$and 
  (#$isa ?LIKES-GENERIC #$AgentPredicate) 
  (#$isa ?LIKES-GENERIC #$BinaryPredicate) 
  (#$denotation ?LIKE-THEWORD #$Verb ??N ?LIKES-GENERIC)) 
  
 (#$termTemplate-Reln #$VPModSubjectTrans ?LIKES-GENERIC 
   (#$NLPatternList  
    (#$NLPattern-Word ?LIKE-THEWORD #$Verb) 
    ) 
   (?LIKES-GENERIC #$TheSentenceSubject :OBJECT)))


;; (logicmoo-rkf-reader "joe suspected him" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Suspect-TheWord ??LOGICMOO #$TransitiveNPFrame ?CYCL)
  
  (#$termTemplate #$VPModSubjectTrans
   (#$NLPatternList 
    (#$NLPattern-Word ?Suspect-TheWord #$Verb))
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
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$SubstituteFormulaFn ?Paying :DENOTS (#$SubstituteFormulaFn ?Paying :DENOT ?CYCL)))))

;; (logicmoo-rkf-reader "joe gnawed on fish" #$STemplate) ==> (#$and (#$isa :ACTION #$Chewing) (#$objectActedOn :ACTION #$AvrahamFish) (#$performedBy :ACTION #$JosephCorneli))
;; Dependents : 1047
#+COGBOT_SLOW_BY_NEEDED
(term-template-assert 
 (#$verbSemTrans ?Gnaw-TheWord ??LOGICMOO (#$PPCompFrameFn #$TransitivePPFrameType ?On-TheWord) ?CYCL)
  
  (#$assertTemplate #$VPModSubjectTrans
   (#$NLPatternList 
    (#$NLPattern-Word ?Gnaw-TheWord #$Verb) 
    (#$NLPattern-Word ?On-TheWord #$Preposition) 
    )
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT 
      (#$SubstituteFormulaFn :OBJECT :OBLIQUE-OBJECT 
       (#$and ?CYCL (#$situationConstituents :ACTION :SUBJECT) (#$situationConstituents :ACTION :OBJECT ))))))

;;  (logicmoo-rkf-reader "joe opened fire on fish" #$STemplate) ==> (#$and (#$intendedAttackTargets :ACTION #$AvrahamFish) (#$isa :ACTION #$ShootingAProjectileWeapon) (#$performedBy :ACTION #$JosephCorneli)) 
;; TODO  (logicmoo-rkf-reader "joe opened fire on a fish" #$STemplate)
(term-template-assert 
 (#$compoundSemTrans ?Open-TheWord (#$TheList . ?fire) #$Verb (#$PPCompFrameFn #$TransitivePPFrameType ?On-TheWord) ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Open-TheWord #$Verb) 
	(#$NLPattern-Exact . ?fire)
    (#$NLPattern-Word ?On-TheWord #$Preposition) 
    )
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$SubstituteFormulaFn :OBJECT :OBLIQUE-OBJECT ?CYCL))))

;; (logicmoo-rkf-reader "Joe smelled himself" #$STemplate) ==> (#$smells #$JosephCorneli (#$PronounFn #$ThirdPerson-NLAttr #$Singular-NLAttr #$Masculine-NLAttr #$ReflexivePronoun))
;; (logicmoo-rkf-reader "Joe smells" #$STemplate) ==> (#$objectEmitsOdor #$JosephCorneli #$FoulOdor)
;;works (logicmoo-rkf-reader "Joe smells like fish" #$STemplate) ==> (#$objectEmitsOdor #$JosephC (#$OdorFn #$Fish))
;;TODO (logicmoo-rkf-reader "Joe smells like Joe" #$STemplate) 
(term-template-assert 
 (#$compoundSemTrans ?Smells-TheWord (#$TheList . ?like) #$Verb #$TransitiveNPFrame ?CYCL)
  
  (#$termTemplate #$VPModSubjectTrans
   (#$NLPatternList 
    (#$NLPattern-Word ?Smells-TheWord #$Verb) 
	(#$NLPattern-Exact . ?like)
     )
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

#+COGBOT_SLOW_BY_NEEDED
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



(term-template-assert-fw 
 (#$verbSemTrans ?Cut-TheWord ??LOGICMOO (#$ParticleCompFrameFn #$TransitiveParticleNPFrameType ?Back-TheWord) ?CYCL)
  
  (#$termTemplate #$VPModSubjectTrans
   (#$NLPatternList 
    (#$NLPattern-Word ?Cut-TheWord #$Verb) 
    (#$NLPattern-Word ?Back-TheWord #$VerbParticle) 
     )
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))


(term-template-assert 
 (#$verbSemTrans ?Cut-TheWord ??LOGICMOO (#$ParticleCompFrameFn #$TransitiveParticleNPFrameType ?Back-TheWord) ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Cut-TheWord #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT)
    (#$NLPattern-Word ?Back-TheWord #$VerbParticle))   
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))


;; (parse "I feel good")
(term-template-assert-fw 
 (#$verbSemTrans ?Feel-TheWord ??LOGICMOO #$TransitiveADJPFrame ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Feel-TheWord #$Verb) 
    (#$NLPattern-Template #$AdjPTemplate :OBJECT))   
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
;;  (parse "he asked who liked it")
(term-template-assert 
 (#$verbSemTrans ?Ask-TheWord ??LOGICMOO #$TransitiveWhClauseFrame ?CYCL)
  
  (#$termTemplate #$NPInfTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Ask-TheWord #$Verb)   
  (#$NLPattern-POS :WH-WORD #$WHWord) ;; This is sinkTemplate really
    (#$NLPattern-Template #$STemplate-Clause :VPHRASE))
   (#$and ?CYCL 
                   (#$equals :CLAUSE 
                       (#$SubstituteFormulaFn :WH-WORD #$TheSentenceSubject :VPHRASE)))))

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
;;(logicmoo-rkf-reader "Joe thinks that he likes Joe" #$STemplate)
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

;;(logicmoo-rkf-reader "Joe jacked off" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Jack-TheWord ??LOGICMOO (#$ParticleCompFrameFn #$IntransitiveParticleFrameType ?Off-TheWord) ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
      (#$NLPattern-Word ?Jack-TheWord #$Verb) 
    (#$NLPattern-Word ?Off-TheWord #$VerbParticle))
     (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT 
      (#$and (#$situationConstituents :ACTION :SUBJECT) ?CYCL))))


;;(logicmoo-rkf-reader "Joe sat down" #$STemplate)
(term-template-assert-fw
(#$compoundSemTrans ?Sat-TheWord (#$TheList . ?down) ?Verb #$IntransitiveVerbFrame ?CYCL)
  
  (#$assertTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Sat-TheWord ?Verb) 
    (#$NLPattern-Exact . ?down))
  (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;;
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
;; (parse "he was good in the mud")
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

;;(logicmoo-rkf-reader "organic peas" #$NPTemplate)
(term-template-assert 
 (#$adjSemTrans-Restricted ?Organic-TheWord ??LOGICMOO ?RegularAdjFrame ?Food ?CYCL)
 (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Organic-TheWord #$Adjective-Gradable)
     (#$NLPattern-Template #$NPTemplate :NOUN))
   (#$TheOneOf :NOUN (#$and (#$isa :NOUN ?Food) ?CYCL))))




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
;;(logicmoo-rkf-reader "the litterer" #$NPTemplate)
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

(print ";;;;;;;; Done loading cogbot-parser-semtrans.lisp")(force-output)
