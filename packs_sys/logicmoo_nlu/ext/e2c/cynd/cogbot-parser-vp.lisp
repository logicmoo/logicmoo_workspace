
;; (load "cynd/cogbot-parser.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RTP/ITP SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "cynd/cogbot-ke.lisp")
(print ";;;;;;;; Loading cogbot-parser.lisp")(force-output)


(csetq *psp-verbose?* nil)
(csetq *it-verbose?* nil)

(csetq *verbose-print-pph-phrases?* nil)
(csetq *it-failing-verbose* nil)
(csetq *it-verbose* t)
(csetq *psp-verbose?* t)


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
(csetq *rtp-default-template-types* '((#$termTemplate) (#$termTemplate-Reln) (#$assertTemplate-Reln #$TemporalModifierTemplate)
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


(defmacro term-template-assert (ante cons &optional (tomt  *RULEKB*)) 
   (ret (cbnl-assert `(#$implies ,ante (#$ist ,*TMPL-STORE* ,cons)) tomt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; TESTS TO MAKE SURE RTP IS ACCEPTING NEW CATEGORIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(cbnl-assert 
 `(#$and
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject 
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

  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject 
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

 (logicmoo-rkf-reader "tst" #$STemplate)

 (logicmoo-rkf-reader "tnp tvpt" #$STemplate)

 (logicmoo-rkf-reader "tvpt" #$VPModSubject)

 (logicmoo-rkf-reader "avpt" #$VPModSubject)

 (logicmoo-rkf-reader "anp avpt tvpt" #$STemplate)

 (logicmoo-rkf-reader "tstvp avpt" #$STemplate)

 (logicmoo-rkf-reader "man that tvpt" #$NPTemplate)

 (logicmoo-rkf-reader "tnp" #$NPTemplate)

(parse "tstvp avpt")
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

(define copy-cyc-constant-term (from to)
  (cdolist (var (all-term-assertions from))
    (clet ((before (assertion-el-formula var)))
                      (ke-assert-now  (TREE-SUBSTITUTE before from to) (assertion-mt var)))))

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

(term-template-assert 
 (#$consumesTemplateAssertions ?TO ?FROM)
 (#$and 
  (#$assertTemplate-Reln ?TO ?FROM (#$NLPatternList (#$NLPattern-Template ?FROM :VBAR-CONT)) (#$Simply :VBAR-CONT))
  (#$termTemplate-Reln ?TO ?FROM (#$NLPatternList (#$NLPattern-Template ?FROM :VBAR-CONT)) (#$Simply :VBAR-CONT))
  ;; (#$queryTemplate-Reln ?TO ?FROM (#$NLPatternList (#$NLPattern-Template ?FROM :VBAR-CONT)) (#$Simply :VBAR-CONT))
    ))
       
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
'(cbnl-assert `
  (#$termTemplate #$VPModSubject
   (#$NLPatternList
    (#$NLPattern-Template #$InfinitivalVPTemplate :ACTION))
  (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT :ACTION)) *TMPL-STORE*)

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
(cbnl-retract `
(#$termTemplate-Reln #$NPTemplate #$RatioTemplate 
       (#$NLPatternList 
           (#$NLPattern-Template #$RatioTemplate :NUM)) 
       (#$Simply :NUM)) #$EnglishGrammarTemplateMt)

#-TooPromiscuous
(cbnl-retract `
 (#$termTemplate #$NPTemplate 
       (#$NLPatternList 
           (#$NLPattern-Template #$DateTemplate :DATE)) 
       (#$Simply :DATE)) #$EnglishTemplateMt)

#-TooPromiscuous
(cbnl-retract `
 (#$termTemplate-Reln #$NPTemplate #$Percent 
       (#$NLPatternList 
           (#$NLPattern-Template #$PercentTemplate :PERC)) 
       (#$Simply :PERC)) #$EnglishTemplateMt)


;; (parse "I saw him right now")
#+TooPromiscuous
(cbnl-assert  `
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  #$prepSemTrans / #$VerbPhraseModifyingFrame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(with-UnderspecifiedAgent :ACTION :OBJECT)
(term-template-assert 
 (#$prepSemTrans ?With-TheWord ??LOGICMOO #$VerbPhraseModifyingFrame ?CYCL) 
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
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
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
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
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Suspect-TheWord #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))


;; (logicmoo-rkf-reader "joe jumped" #$STemplate)
;; (logicmoo-rkf-reader "jumped" #$VPModSubject)
(term-template-assert 
 (#$verbSemTrans ?Jumped-TheWord ??LOGICMOO #$IntransitiveVerbFrame ?CYCL)
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Jumped-TheWord #$Verb))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;; (logicmoo-rkf-reader "joe jumped" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Jumped-TheWord ??LOGICMOO #$MiddleVoiceFrame ?CYCL)
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Jumped-TheWord #$Verb))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))


(term-template-assert 
  (#$verbPrep-TransitiveTemplate ?Paying ?To-TheWord ?CYCL)
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Term ?Paying #$Verb) 
    (#$NLPattern-Template #$NPTemplate :OBJECT)
    (#$NLPattern-Word ?To-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$SubstituteFormulaFn ?Paying :DENOTS ?CYCL))))

;; (logicmoo-rkf-reader "joe gnawed on fish" #$STemplate) ==> (#$and (#$isa :ACTION #$Chewing) (#$objectActedOn :ACTION #$AvrahamFish) (#$performedBy :ACTION #$JosephCorneli))
(term-template-assert 
 (#$verbSemTrans ?Gnaw-TheWord ??LOGICMOO (#$PPCompFrameFn #$TransitivePPFrameType ?On-TheWord) ?CYCL)
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Gnaw-TheWord #$Verb) 
    (#$NLPattern-Word ?On-TheWord #$Preposition) 
    (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;;  (logicmoo-rkf-reader "joe opened fire on fish" #$STemplate) ==> (#$and (#$intendedAttackTargets :ACTION #$AvrahamFish) (#$isa :ACTION #$ShootingAProjectileWeapon) (#$performedBy :ACTION #$JosephCorneli)) 
;; TODO  (logicmoo-rkf-reader "joe opened fire on a fish" #$STemplate)
(term-template-assert 
 (#$compoundSemTrans ?Open-TheWord (#$TheList . ?fire) #$Verb (#$PPCompFrameFn #$TransitivePPFrameType ?On-TheWord) ?CYCL)
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
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
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Smells-TheWord #$Verb) 
	(#$NLPattern-Exact . ?like)
    (#$NLPattern-Template #$NPTemplate :OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

(term-template-assert 
 (#$verbSemTrans ?Put-TheWord ??LOGICMOO (#$PPCompFrameFn #$DitransitivePPFrameType ?In-TheWord) ?CYCL)
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
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
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Cut-TheWord #$Verb) 
    (#$NLPattern-Word ?Back-TheWord #$VerbParticle) 
    (#$NLPattern-Template #$NPTemplate :OBJECT))
   (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))


(term-template-assert 
 (#$verbSemTrans ?Cut-TheWord ??LOGICMOO (#$ParticleCompFrameFn #$TransitiveParticleNPFrameType ?Back-TheWord) ?CYCL)
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
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
  
  (#$assertTemplate-Reln #$NPInfTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Hope-TheWord #$Verb) 
    (#$NLPattern-Word #$That-TheWord #$Determiner-Definite) 
    (#$NLPattern-Template #$STemplate-Clause :CLAUSE))
  (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;;;;;;;;;;;;;;;;;;;;WHWord
;;TODO (logicmoo-rkf-reader "Joe asked who was there" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Ask-TheWord ??LOGICMOO #$TransitiveWhClauseFrame ?CYCL)
  
  (#$assertTemplate-Reln #$NPInfTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Ask-TheWord #$Verb)
    (#$NLPattern-POS :WH-WORD #$WHWord) 
    (#$NLPattern-Template #$STemplate-Clause :CLAUSE))
   ?CYCL))

;;;;;;;;;;;;;;;;;;;;For
;;TODO (logicmoo-rkf-reader "Joe waits for sue to eat" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Wait-TheWord ??LOGICMOO #$TransitiveForNPInfinitivePhraseFrame ?CYCL)
  
  (#$assertTemplate-Reln #$NPInfTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Wait-TheWord #$Verb) 
    (#$NLPattern-Word #$For-TheWord #$Preposition)
    (#$NLPattern-Template #$STemplate-Clause :OBJECT))
  ?CYCL))


;;;;;;;;;;;;;;;;;;;;Any
;;TODO (logicmoo-rkf-reader "Joe was able to buy bread" #$STemplate)
(term-template-assert 
 (#$compoundSemTrans ?Be-TheWord (#$TheList . ?able) #$Verb #$TransitiveInfinitivePhraseFrame ?CYCL)
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Be-TheWord #$Verb)
	(#$NLPattern-Exact . ?able)
    (#$NLPattern-Template #$STemplate-Clause :CLAUSE))
  ?CYCL))


;;;;;;;;;;;;;;;;;;;;Any + optional That
;;(logicmoo-rkf-reader "Joe thinks [that] he likes Joe" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Think-TheWord ??LOGICMOO #$TransitiveInfinitivePhraseFrame ?CYCL)
   
  (#$assertTemplate-Reln #$NPInfTemplate #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Think-TheWord #$Verb)
    (#$OptionalOne (#$NLPattern-Word #$That-TheWord #$Determiner-Definite))
    (#$NLPattern-Template #$STemplate-Clause :CLAUSE))
  ?CYCL))

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
	 (#$SubstituteFormulaFn :SOME-SUBJ #$TheSentenceSubject :ACTION)) *TMPL-STORE*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intransitive Verb Phrases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(logicmoo-rkf-reader "Joe broke out" #$STemplate)
(term-template-assert 
 (#$verbSemTrans ?Jack-TheWord ??LOGICMOO (#$ParticleCompFrameFn #$IntransitiveParticleFrameType ?Off-TheWord) ?CYCL)
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
   (#$NLPatternList 
      (#$NLPattern-Word ?Jack-TheWord #$Verb) 
    (#$NLPattern-Word ?Off-TheWord #$VerbParticle))
     (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT 
      (#$and (#$situationConstituents :ACTION :SUBJECT) ?CYCL))))


;;(logicmoo-rkf-reader "Joe sat down" #$STemplate)
(term-template-assert 
(#$compoundSemTrans ?Sat-TheWord (#$TheList . ?down) #$Verb #$IntransitiveVerbFrame ?CYCL)
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
   (#$NLPatternList 
    (#$NLPattern-Word ?Sat-TheWord #$Verb) 
    (#$NLPattern-Exact . ?down))
  (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT ?CYCL)))

;; (logicmoo-rkf-reader "Joe sat down" #$STemplate)
(term-template-assert 
(#$compoundSemTrans ?Sat-TheWord (#$TheList . ?down) #$Verb #$MiddleVoiceFrame ?CYCL)
  
  (#$assertTemplate-Reln #$VPModSubject #$VPModSubject
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
  (#$assertTemplate-Reln #$STemplate #$STemplate
   (#$NLPatternList
    (#$NLPattern-Template #$STemplate-Action :ACTION)
    (#$NLPattern-Word ?In-TheWord #$Preposition) 
     (#$NLPattern-Template #$NPTemplate :OBLIQUE-OBJECT))
  ?CYCL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STemplate/compoundString #$Verb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (logicmoo-rkf-reader "Joe made a facial expression" #$STemplate)
(term-template-assert 
 (#$compoundString #$Make-TheWord (#$TheList . ?expression) #$Verb ?MakingFacialExpression)
 (#$compoundSemTrans #$Make-TheWord (#$TheList . ?expression) #$Verb #$MiddleVoiceFrame 
  (#$and (#$isa :ACTION ?MakingFacialExpression)(#$doneBy :ACTION :SUBJECT))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NPTemplate/compoundString #$CountNoun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (logicmoo-rkf-reader "chatroom for sports fans" #$NPTemplate)
(term-template-assert 
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
(term-template-assert 
  (#$compoundString ?The-TheWord 
       (#$TheList . ?battle-for-hue) #$Determiner-Definite ?BattleForHue)
 (#$compoundString ?The-TheWord (TheList . ?battle-for-hue) #$CountNoun #$RegularNounFrame ?BattleForHue))


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

;;(load "cynd/cogbot-parser-template.lisp")
(print ";;;;;;;; Done cogbot-parser.lisp")(force-output)
(extend-rtp-from-mt nil *rtp-default-template-types*)
 ;; (logicmoo-rkf-reader "Austin" #$GeopoliticalEntityTemplate)
 ;; (isa-in-any-mt? #$nameString #$TermPhrasesConstraint ) 
;; (logicmoo-rkf-reader  "You are standing in the doorway of the elevator")
;; (logicmoo-rkf-reader  "You can see a picture hanging on the wall")
;; (logicmoo-rkf-reader  "You are standing in the doorway of the elevator")
;; ((#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) (#$on-Physical (#$and (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT (#$and # # # #)) (#$equals #$TheSentenceSubject (#$PronounFn #$SecondPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$SubjectPronoun))) (#$SubstituteFormulaFn #$TheSentenceSubject :SUBJECT #$Wall-GenericBarrier)) ...)
;; (logicmoo-rkf-reader  "think that i can" #$VPModSubject)
;; (logicmoo-rkf-reader  "think" #$InfinitivalVPTemplate)
(cbnl-assert `(#$posForTemplateCategory #$Verb #$VPModSubject) #$TemplateParsingMt)
(cbnl-retract `(#$posPredForTemplateCategory #$verbStrings #$VPModSubject) #$TemplateParsingMt)
(cbnl-retract `(#$posPredForTemplateCategory #$regularAdverb #$VPModSubject) #$TemplateParsingMt)
;; (FI-MERGE #$VPModSubject #$InfinitivalVPTemplate)
;; (FI-MERGE #$InfinitivalVPTemplate #$VPModSubject)

;; save as #$InfinitivalVPTemplate
