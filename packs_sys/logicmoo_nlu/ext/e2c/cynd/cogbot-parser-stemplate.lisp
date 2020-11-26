;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RTP/ITP SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print ";;;;;;;; Loading cogbot-parser.lisp")(force-output)

(csetq *verbose-print-pph-phrases?* nil)
(csetq *it-failing-verbose* nil)
(csetq *it-verbose* nil)
(csetq *psp-verbose?* nil)

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

(cdolist (v (query-template '?X `(#$isa ?X #$SemTransPred) #$EverythingPSC))
  (set-rule-forward-cons-pred v))


(csetq *TMPL-STORE* #$EnglishTemplateMt)
(csetq *RULEKB* #$BaseKB)

(csetq *rtp-default-template-types* '((#$termTemplate) (#$termTemplate-Reln) (#$assertTemplate-Reln #$TemporalModifierTemplate)
 (#$commandTemplate) (#$rewriteTemplate) (#$assertTemplate) (#$queryTemplate) (#$metaStatementTemplate-Reln) (#$commandTemplate-Reln) (#$queryTemplate-Reln) (#$assertTemplate-Reln) (#$termTemplate-Test) (#$queryTemplate-Test) (#$assertTemplate-Test)))

( cbnl-assert `(#$implies (#$nearestGenlMt ?mt #$RKFParsingMt)(#$isa ?mt #$TemplateParsingMicrotheory)) *RULEKB* )
( cbnl-assert `(#$implies (#$genlMt ?mt #$RKFParsingMt)(#$isa ?mt #$TemplateParsingMicrotheory)) *RULEKB* )
( cbnl-assert `(#$implies (#$isa ?mt #$TemplateParsingMicrotheory)(#$genlMt ?mt #$TemplateParsingMt)) *RULEKB* )

(defmacro term-template-assert (ante cons) 
   (ret `(make-process (string ',ante) #'(lambda () (cbnl-assert '(#$implies ,ante (#$ist ,*TMPL-STORE* ,cons)) ,*RULEKB* )))))

(defmacro term-template-assert (ante cons) 
   (ret `(cbnl-assert '(#$implies ,ante (#$ist ,*TMPL-STORE* ,cons)) ,*RULEKB*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STemplate/assertTemplate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (logicmoo-rkf-reader "joe likes fish" #$STemplate)
(term-template-assert 
 (#$and 
  (#$isa ?LIKES-GENERIC #$AgentPredicate) 
  (#$isa ?LIKES-GENERIC #$BinaryPredicate) 
  (#$denotation ?LIKE-THEWORD #$Verb ??N ?LIKES-GENERIC)) 
  
  (#$assertTemplate-Reln #$STemplate ?LIKES-GENERIC 
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :ARG1) 
    (#$NLPattern-Word ?LIKE-THEWORD #$Verb) 
    (#$NLPattern-Template #$NPTemplate :ARG2)) 
   (?LIKES-GENERIC :ARG1 :ARG2)))

;; (logicmoo-rkf-reader "joe suspected him" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Suspect-TheWord ??LOGICMOO #$TransitiveNPFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Suspect-TheWord #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT))
   ?CYCL))


(term-template-assert 
 (#$verbSemTrans ?Suspect-TheWord ??LOGICMOO #$TransitiveNPFrame ?CYCL)  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Suspect-TheWord #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT))
   ?CYCL))

;; (logicmoo-rkf-reader "joe jumped" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Jumped-TheWord ??LOGICMOO #$IntransitiveVerbFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Jumped-TheWord #$Verb))
   ?CYCL))

;; (logicmoo-rkf-reader "joe jumped" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Jumped-TheWord ??LOGICMOO #$MiddleVoiceFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Jumped-TheWord #$Verb))
   ?CYCL))


;; (logicmoo-rkf-reader "joe gnawed on fish" #$STemplate) ==> (#$and (#$isa :ACTION #$Chewing) (#$objectActedOn :ACTION #$AvrahamFish) (#$performedBy :ACTION #$JosephCorneli))
(term-template-assert 
 (#$verbSemTrans ?Gnaw-TheWord ??LOGICMOO (#$PPCompFrameFn #$TransitivePPFrameType ?On-TheWord) ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Gnaw-TheWord #$Verb) 
    (#$NLPattern-Word ?On-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
   ?CYCL))

;;  (logicmoo-rkf-reader "joe opened fire on fish" #$STemplate) ==> (#$and (#$intendedAttackTargets :ACTION #$AvrahamFish) (#$isa :ACTION #$ShootingAProjectileWeapon) (#$performedBy :ACTION #$JosephCorneli)) 
;; TODO  (logicmoo-rkf-reader "joe opened fire on a fish" #$STemplate)
(term-template-assert 
 (#$compoundSemTrans ?Open-TheWord (#$TheList . ?fire) #$Verb (#$PPCompFrameFn #$TransitivePPFrameType ?On-TheWord) ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Open-TheWord #$Verb) 
	(#$NLPattern-Exact . ?fire)
    (#$NLPattern-Word ?On-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
   ?CYCL))
;; (logicmoo-rkf-reader "Joe smelled himself" #$STemplate) ==> (#$smells #$JosephCorneli (#$PronounFn #$ThirdPerson-NLAttr #$Singular-NLAttr #$Masculine-NLAttr #$ReflexivePronoun))
;; (logicmoo-rkf-reader "Joe smells" #$STemplate) ==> (#$objectEmitsOdor #$JosephCorneli #$FoulOdor)
;;works (logicmoo-rkf-reader "Joe smells like fish" #$STemplate) ==> (#$objectEmitsOdor #$JosephC (#$OdorFn #$Fish))
;;TODO (logicmoo-rkf-reader "Joe smells like Joe" #$STemplate) 
(term-template-assert 
 (#$compoundSemTrans ?Smells-TheWord (#$TheList . ?like) #$Verb #$TransitiveNPFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Smells-TheWord #$Verb) 
	(#$NLPattern-Exact . ?like)
    (#$NLPattern-Template #$NPTemplate :OBJECT))
   ?CYCL))

(term-template-assert 
 (#$verbSemTrans ?Put-TheWord ??LOGICMOO (#$PPCompFrameFn #$DitransitivePPFrameType ?In-TheWord) ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Put-TheWord #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT)
    (#$NLPattern-Word ?In-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
   ?CYCL))

;;works (logicmoo-rkf-reader "I cut back him" #$STemplate)
;;doesnt (logicmoo-rkf-reader "I cut back the trees" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Cut-TheWord ??LOGICMOO (#$ParticleCompFrameFn #$TransitiveParticleNPFrameType ?Back-TheWord) ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Cut-TheWord #$Verb) 
    (#$NLPattern-Word ?Back-TheWord #$VerbParticle) 
    (#$NLPattern-Template #$NPTemplate :OBJECT))
   ?CYCL))

;;TODO (logicmoo-rkf-reader "Joe hopes that he was there" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Hope-TheWord ??LOGICMOO #$TransitiveThatClauseFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Hope-TheWord #$Verb) 
    (#$NLPattern-Word #$That-TheWord #$Determiner-Definite) 
    (#$NLPattern-Template #$STemplate :CLAUSE))
   ?CYCL))

;;TODO (logicmoo-rkf-reader "Joe asked who was there" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Ask-TheWord ??LOGICMOO #$TransitiveWhClauseFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Ask-TheWord #$Verb) 
    (#$NLPattern-Template #$STemplate :CLAUSE))
   ?CYCL))

;;TODO (logicmoo-rkf-reader "Joe waits for it" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Wait-TheWord ??LOGICMOO #$TransitiveForNPInfinitivePhraseFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Wait-TheWord #$Verb) 
	(#$NLPattern-Word #$For-TheWord #$Preposition)
    (#$NLPattern-Template #$STemplate :OBJECT))
   ?CYCL))


;;TODO (logicmoo-rkf-reader "Joe was able to buy bread" #$STemplate)
(term-template-assert 
 (#$compoundSemTrans ?Be-TheWord (#$TheList . ?able) #$Verb #$TransitiveInfinitivePhraseFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Be-TheWord #$Verb)
	(#$NLPattern-Exact . ?able)
    (#$NLPattern-Template #$STemplate :CLAUSE))
   ?CYCL))


;;(logicmoo-rkf-reader "Joe thinks that he likes Joe" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Think-TheWord ??LOGICMOO #$TransitiveInfinitivePhraseFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Think-TheWord #$Verb)
	(#$NLPattern-Word #$That-TheWord #$Determiner-Definite)
    (#$NLPattern-Template #$STemplate :CLAUSE))
   ?CYCL))


;;(logicmoo-rkf-reader "Joe thinks he likes Joe" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Think-TheWord ??LOGICMOO #$TransitiveInfinitivePhraseFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Think-TheWord #$Verb) 
    (#$NLPattern-Template #$STemplate :CLAUSE))
   ?CYCL))


;;(logicmoo-rkf-reader "Joe jacked off" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Jack-TheWord ??LOGICMOO (#$ParticleCompFrameFn #$IntransitiveParticleFrameType ?Off-TheWord) ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Jack-TheWord #$Verb) 
    (#$NLPattern-Word ?Off-TheWord #$VerbParticle))
   (#$and (#$situationConstituents :ACTION :SUBJECT) ?CYCL)))


;;(logicmoo-rkf-reader "Joe sat down" #$STemplate)
(term-template-assert 
(#$compoundSemTrans ?Sat-TheWord (#$TheList . ?down) #$Verb #$IntransitiveVerbFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Sat-TheWord #$Verb) 
    (#$NLPattern-Exact . ?down))
   ?CYCL))

;; (logicmoo-rkf-reader "Joe sat down" #$STemplate)
(term-template-assert 
(#$compoundSemTrans ?Sat-TheWord (#$TheList . ?down) #$Verb #$MiddleVoiceFrame ?CYCL)
  
  (#$assertTemplate #$STemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$NPTemplate :SUBJECT) 
    (#$NLPattern-Word ?Sat-TheWord #$Verb) 
    (#$NLPattern-Exact . ?down))
   ?CYCL))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STemplate/compoundString
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (logicmoo-rkf-reader "Joe made a facial expression" #$STemplate)
(term-template-assert 
 (#$compoundString #$Make-TheWord (#$TheList . ?expression) #$Verb ?MakingFacialExpression)
 (#$compoundSemTrans #$Make-TheWord (#$TheList . ?expression) #$Verb #$MiddleVoiceFrame 
  (#$and (#$isa :ACTION ?MakingFacialExpression)(#$doneBy :ACTION :SUBJECT))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/compoundString
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (logicmoo-rkf-reader "chatroom for sports fans" #$NPTemplate)
(term-template-assert 
 (#$compoundString ?Chatroom-TheWord (#$TheList . ?expression) #$CountNoun ?SportsChatRoom)
 (#$compoundSemTrans ?Chatroom-TheWord (#$TheList . ?expression) #$CountNoun #$RegularNounFrame 
   (#$TheOneOf :NOUN (#$isa :NOUN ?SportsChatRoom))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/denotation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+Very-slow
(term-template-assert 
 (#$denotation ?Menage-TheWord #$CountNoun ??LOGICMOO ?CohabitationUnit)
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Menage-TheWord #$CountNoun))
   (#$TheOneOf :NOUN (#$isa :NOUN ?CohabitationUnit))))

;; Questionable
(term-template-assert 
  (#$compoundString ?The-TheWord 
       (#$TheList . ?battle-for-hue) #$Determiner-Definite ?BattleForHue)
 (#$compoundString ?The-TheWord (TheList . ?battle-for-hue) #$CountNoun #$RegularNounFrame ?BattleForHue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/nounSemTrans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (logicmoo-rkf-reader "idiot" #$NPTemplate)
;; (logicmoo-rkf-reader "2 idiots" #$NPTemplate)
(term-template-assert 
(#$nounSemTrans ?Idiot-TheWord ??LOGICMOO #$RegularNounFrame ?CYCL)  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$OptionalOne "a" "an" "the")
    (#$NLPattern-Word ?Idiot-TheWord #$Noun))
   (#$TheOneOf :NOUN ?CYCL)))

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
;;;; NPTemplate/nounSemTrans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;BAD STILL (logicmoo-rkf-reader "Joe's shocker" #$NPTemplate)
(term-template-assert 
(#$nounSemTrans ?Shock-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL)
  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$PossessiveTemplate :POSSESSOR) 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun))
    (#$TheOneOf :NOUN ?CYCL)))

;; (logicmoo-rkf-reader "Joe's" #$PossessiveTemplate)
;;BAD STILL (logicmoo-rkf-reader "Joe's shocker" #$NPTemplate)
(term-template-assert 
(#$agentiveNounSemTrans ?Shock-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL)
  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Template #$PossessiveTemplate :POSSESSOR) 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun))
   (#$TheOneOf :NOUN ?CYCL)))


(term-template-assert 
(#$agentiveNounSemTrans ?Shock-TheWord ??LOGICMOO #$RegularNounFrame ?CYCL)
  
  (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun))
    (#$TheOneOf :NOUN ?CYCL)))



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

;; (logicmoo-rkf-reader "supervisor of Joe" #$NPTemplate)
(term-template-assert 
(#$agentiveNounSemTrans ?Shock-TheWord ??LOGICMOO #$GenitiveFrame ?CYCL)
  
 (#$termTemplate #$NPTemplate
   (#$NLPatternList 
    (#$NLPattern-Word ?Shock-TheWord #$AgentiveNoun)
    (#$NLPattern-Word #$Of-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :POSSESSOR))    
  (#$TheOneOf :NOUN ?CYCL)))

;;(logicmoo-rkf-reader "foot of Joe" #$NPTemplate)
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





#|



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; TODOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


(termTemplate-Reln VPTemplate typePrimaryFunction 
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



(term-template-assert 
 (#$isa ?PLACE #$GeopoliticalEntity) 
  
  (#$termTemplate #$GeopoliticalEntityTemplate 
   (#$NLPatternList 
    (#$NLPattern-Term ?PLACE #$nameString)) ?PLACE))

|#

(print ";;;;;;;; Done cogbot-parser.lisp")(force-output)
(extend-rtp-from-mt nil *rtp-default-template-types*)
 ;; (logicmoo-rkf-reader "Austin" #$GeopoliticalEntityTemplate)
 ;; (isa-in-any-mt? #$nameString #$TermPhrasesConstraint ) 
