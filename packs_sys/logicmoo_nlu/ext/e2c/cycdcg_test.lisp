
(s2t-test #$isa
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "the") 
      (#$BestNLPhraseOfStringFn "onset") 
      (#$BestNLPhraseOfStringFn "of") 
      (#$TermParaphraseFn-Constrained #$nonSingular-Generic :arg1) 
      (#$ConcatenatePhrasesFn
      (#$BestNLPhraseOfStringFn "is") 
      (#$BestNLPhraseOfStringFn "typically") 
      (#$TermParaphraseFn-Constrained #$singular-Generic :arg3) )
      (#$BestNLPhraseOfStringFn "when") 
      (#$BestNLPhraseOfStringFn "caused") 
      (#$BestPPFn #$By-TheWord 
        (#$TermParaphraseFn-Constrained #$nonSingular-Generic :arg2))))

(s2t-test  #$postEvents 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Happen-TheWord) 
      (#$BestNLPhraseOfStringFn "after #$and #$is #$related #$to") 
      (#$TermParaphraseFn :arg1)))

(s2t-test  #$performsWorkOfType 
    (#$PhraseFormFn #$NLSentence 
      (#$ConcatenatePhrasesFn 
        (#$TermParaphraseFn-NP :arg1) 
        (#$BestHeadVerbForInitialSubjectFn #$Perform-TheWord) 
        (#$TermParaphraseFn-NP :arg2) 
        (#$BestNLPhraseOfStringFn "professionally"))))

(s2t-test  #$BiographyOfTypeFn 
    (#$PhraseFormFn
      (#$PhraseFn-Bar1 #$Noun) 
      (#$ConcatenatePhrasesFn 
        (#$HeadWordOfPhraseFn 
          (#$BestNLWordFormOfLexemeFn-Constrained #$CountNoun #$Biography-TheWord)) 
        (#$BestPPFn #$Of-TheWord 
          (#$TermParaphraseFn-Constrained #$nonSingular-Generic :arg1)))))

(s2t-test  #$symptomOnsetRateInConditionType 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "the") 
      (#$BestNLPhraseOfStringFn "onset") 
      (#$BestNLPhraseOfStringFn "of") 
      (#$TermParaphraseFn-Constrained #$nonSingular-Generic :arg1) 
      (#$BestNLPhraseOfStringFn "is") 
      (#$BestNLPhraseOfStringFn "typically") 
      (#$TermParaphraseFn-Constrained #$singular-Generic :arg3) 
      (#$BestNLPhraseOfStringFn "when") 
      (#$BestNLPhraseOfStringFn "caused") 
      (#$BestPPFn #$By-TheWord 
        (#$TermParaphraseFn-Constrained #$nonSingular-Generic :arg2))))

(s2t-test  #$uninstallProgramType 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestNLPhraseOfStringFn "uninstalls") 
      (#$BestDetNbarFn-Indefinite 
        (#$PhraseFormFn #$nonPlural-Generic 
          (#$TermParaphraseFn :arg2))) 
      (#$BestNLPhraseOfStringFn "from") 
      (#$TermParaphraseFn-NP :arg3)))
(s2t-test  #$effectiveDispersalToolForBioAgent 
    (#$NPIsXP-NLSentenceFn 
      (#$TermParaphraseFn-Constrained #$nonPlural-Generic :arg3) 
      (#$ConcatenatePhrasesFn 
        (#$HeadWordOfPhraseFn 
          (#$BestDetNbarFn-Indefinite 
            (#$TermParaphraseFn #$BiologicalAgentStuff))) 
        (#$BestNLPhraseOfStringFn "that #$can #$be #$effectively #$dispersed #$by") 
        (#$TermParaphraseFn-NP :arg1) 
        (#$BestPPFn #$With-TheWord 
          (#$BestDetNbarFn-Indefinite 
            (#$TermParaphraseFn-Constrained #$singular :arg2))))))

(s2t-test  #$SouthEasternRegionFn 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "Southeast") 
      (#$TermParaphraseFn :arg1)))
(s2t-test  #$animalHasBloodType 
    (#$NPIsXP-NLSentenceFn 
      (#$BestDetNbarFn 
        (#$TermParaphraseFn-Possessive :arg1) 
        (#$TermParaphraseFn #$BloodType)) 
      (#$TermParaphraseFn :arg2)))
(s2t-test  #$minimize 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "by") 
      (#$BestNLPhraseOfStringFn "default, #$it #$is #$not #$the #$case #$that") 
      (#$TermParaphraseFn-NP :arg1)))
(s2t-test  #$ZipCodeToIntegerFn 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "the #$five #$digit #$portion #$of #$the #$zip #$code") 
      (#$TermParaphraseFn :arg1)))
(s2t-test  #$productionRateOfRegion 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Produce-TheWord) 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestNLPhraseOfStringFn "at") 
      (#$BestNLPhraseOfStringFn "the #$rate #$of") 
      (#$TermParaphraseFn-NP :arg3)))
(s2t-test  #$typicalInfectionHost 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-Constrained #$plural-Generic :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "typical #$infection #$hosts #$for") 
      (#$TermParaphraseFn-Constrained #$plural-Generic :arg1)))
(s2t-test  #$organismExposedToHarmfulSubstance 
    (#$TensedPhraseFn-DefaultPast :arg1 
      (#$ConcatenatePhrasesFn 
        (#$TermParaphraseFn-NP :arg2) 
        (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
        (#$BestNLPhraseOfStringFn "exposed #$to #$some #$harmful #$substance #$in") 
        (#$TermParaphraseFn :arg1))))
(s2t-test  #$in-ContFullOf 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "full #$of") 
      (#$TermParaphraseFn :arg1)))
(s2t-test  #$TourDayFn 
    (#$GenTemplateRecipeOmitsArgFn :arg1 
      (#$ConcatenatePhrasesFn 
        (#$BestNLPhraseOfStringFn "Day") 
        (#$TermParaphraseFn :arg2))))
(s2t-test  #$instancesAssociatedWithMETT-TCPrinciple 
    (#$ConcatenatePhrasesFn 
      (#$PhraseFormFn #$NounPhrase 
        (#$ConcatenatePhrasesFn 
          (#$HeadWordOfPhraseFn 
            (#$BestNLWordFormOfLexemeFn-Constrained #$massNumber #$Know-TheWord)) 
          (#$BestPPFn #$About-TheWord 
            (#$TermParaphraseFn-NP :arg1)))) 
      (#$BestHeadVerbForInitialSubjectFn #$Fall-TheWord) 
      (#$BestNLPhraseOfStringFn "under #$the #$METT-TC #$principle") 
      (#$TermParaphraseFn-NP :arg2)))

(s2t-test #$equipmentOfUnit-Operational 
    (#$NPIsXP-NLSentenceFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$ConcatenatePhrasesFn 
        (#$BestNLPhraseOfStringFn "equipped #$with") 
        (#$BestDetNbarFn 
          (#$TermParaphraseFn :arg3) 
          (#$ConditionalPhraseFn 
            (#$equals :arg3 1) 
            (#$TermParaphraseFn-Constrained #$nonPlural-Generic :arg2) 
            (#$TermParaphraseFn-Constrained #$plural-Generic :arg2))))))
(s2t-test #$functionalInArgs 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "functional #$in #$argument") 
      (#$TermParaphraseFn :arg2)))
(s2t-test #$placeInCity 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "located #$in") 
      (#$TermParaphraseFn :arg2)))
(s2t-test #$threeWayJunctionInSystem 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "a #$three-way #$junction #$in #$the #$path #$system") 
      (#$TermParaphraseFn-NP :arg2)))
(s2t-test #$vehicleCargoCapacity-Volume 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "the") 
      (#$BestNLPhraseOfStringFn "volume #$of") 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestNLPhraseOfStringFn "cargo") 
      (#$BestNLPhraseOfStringFn "capacity #$is") 
      (#$TermParaphraseFn-NP :arg2)))
(s2t-test #$leadSinger 
    (#$NPIsXP-NLSentenceFn 
      (#$TermParaphraseFn :arg2) 
      (#$BestDetNbarFn-Definite 
        (#$ConcatenatePhrasesFn 
          (#$BestNLPhraseOfStringFn "lead") 
          (#$HeadWordOfPhraseFn 
            (#$BestNLWordFormOfLexemeFn-Constrained #$AgentiveNoun #$Sing-TheWord)) 
          (#$BestPPFn #$Of-TheWord 
            (#$TermParaphraseFn-NP :arg1))))))
(s2t-test #$OnlineActivityTypeFn 
    (#$PhraseFormFn 
      (#$PhraseFn-Bar1 #$Noun) 
      (#$ConcatenatePhrasesFn 
        (#$BestNLPhraseOfStringFn "online") 
        (#$TermParaphraseFn :arg1))))
(s2t-test #$headOfStateOf 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "head #$of #$state #$of") 
      (#$TermParaphraseFn :arg2)))
(s2t-test #$LeftwardsFn 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "leftwards #$for") 
      (#$TermParaphraseFn :arg1)))
(s2t-test #$spatialExtent 
    (#$NPIsXP-NLSentenceFn 
      (#$BestDetNbarFn 
        (#$TermParaphraseFn-Possessive :arg1) 
        (#$PhraseFormFn #$nonPlural-Generic 
          (#$BestNLPhraseOfStringFn "spatial #$extent"))) 
      (#$TermParaphraseFn-Constrained #$nonPlural-Generic :arg2)))
(s2t-test #$performedByPart 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "performed #$by #$some #$member (#$or #$members) #$of") 
      (#$TermParaphraseFn-NP :arg2)))
(s2t-test #$keSuggestionApplies 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Apply-TheWord) 
      (#$BestPPFn #$To-TheWord 
        (#$TermParaphraseFn :arg1))))
(s2t-test #$Escudo-CapeVerde 
    (#$BestDetNbarFn 
      (#$ConcatenatePhrasesFn 
        (#$ConditionalPhraseFn 
          (#$formulaArity :SELF 2) 
          (#$ConcatenatePhrasesFn 
            (#$BestNLWordFormOfLexemeFn #$Between-TheWord) 
            (#$TermParaphraseFn-NPAndNP :arg1 :arg2)) 
          (#$HeadWordOfPhraseFn 
            (#$TermParaphraseFn :arg1)))) 
      (#$TermParaphraseFn :ARG0)))

(s2t-test #$MillionBarrelsPerDay 
    (#$BestDetNbarFn 
      (#$ConcatenatePhrasesFn 
        (#$ConditionalPhraseFn 
          (#$formulaArity :SELF 2) 
          (#$ConcatenatePhrasesFn 
            (#$BestNLWordFormOfLexemeFn #$Between-TheWord) 
            (#$TermParaphraseFn-NPAndNP :arg1 :arg2)) 
          (#$HeadWordOfPhraseFn 
            (#$TermParaphraseFn :arg1)))) 
      (#$TermParaphraseFn :ARG0)))

(s2t-test #$mentionForMentionHypothesis 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "the #$textual #$reference #$corresponding #$to") 
      (#$TermParaphraseFn :arg1)))
(s2t-test #$detects 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Detect-TheWord) 
      (#$TermParaphraseFn-NP :arg2)))
(s2t-test #$relativeVarietyOn 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$TermParaphraseFn-NP :arg2) 
      (#$HeadWordOfPhraseFn 
        (#$BestVerbFormForSubjectFn #$Fall-TheWord 
          (#$NthPhraseFn 2))) 
      (#$BestNLPhraseOfStringFn "into") 
      (#$BestNLPhraseOfStringFn "a") 
      (#$TermParaphraseFn-NP :ARG4) 
      (#$TermParaphraseFn-NP :arg3) 
      (#$BestNLPhraseOfStringFn ",") 
      (#$BestNLPhraseOfStringFn "when") 
      (#$BestNLPhraseOfStringFn "compared") 
      (#$BestNLPhraseOfStringFn "with") 
      (#$BestNLPhraseOfStringFn "other") 
      (#$TermParaphraseFn-NP :ARG5)))
(s2t-test #$tenureInUnitPosition 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "in") 
      (#$BestNLPhraseOfStringFn "the #$time #$frame #$of #$the #$assertion,") 
      (#$TermParaphraseFn-NP :arg3) 
      (#$PhraseFormFn #$presentTense-Generic 
        (#$HeadWordOfPhraseFn 
          (#$BestVerbFormForSubjectFn #$Have-TheWord 
            (#$NthPhraseFn 3)))) 
      (#$BestNLPhraseOfStringFn "held #$the #$position #$of") 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestNLPhraseOfStringFn "in") 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestNLPhraseOfStringFn "for") 
      (#$TermParaphraseFn-NP :ARG4)))
(s2t-test #$groupMembers 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Have-TheWord) 
      (#$TermParaphraseFn-Constrained #$nonSingular-Generic :arg2) 
      (#$BestNLPhraseOfStringFn "as #$a #$group #$member")))
(s2t-test #$coaAddressesMissionEssentialTask 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Address-TheWord) 
      (#$BestNLPhraseOfStringFn "the") 
      (#$BestNLPhraseOfStringFn "mission") 
      (#$BestNLPhraseOfStringFn "essential") 
      (#$BestNLPhraseOfStringFn "task") 
      (#$BestNLPhraseOfStringFn "specified") 
      (#$BestPPFn #$By-TheWord 
        (#$TermParaphraseFn-NP :arg2))))
(s2t-test #$collectionSubsetsByCardinality 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn :arg3) 
      (#$BestNLPhraseOfStringFn "is #$the #$collection #$of #$subsets #$of") 
      (#$TermParaphraseFn :arg1) 
      (#$BestNLPhraseOfStringFn "with #$cardinality") 
      (#$TermParaphraseFn :arg2)))
(s2t-test #$subWorks 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Contain-TheWord) 
      (#$TermParaphraseFn-NP :arg2)))

(s2t-test 
    (#$Milli #$Bar-UnitOfPressure) 
    (#$BestDetNbarFn 
      (#$ConcatenatePhrasesFn 
        (#$ConditionalPhraseFn 
          (#$formulaArity :SELF 2) 
          (#$ConcatenatePhrasesFn 
            (#$BestNLWordFormOfLexemeFn #$Between-TheWord) 
            (#$TermParaphraseFn-NPAndNP :arg1 :arg2)) 
          (#$HeadWordOfPhraseFn 
            (#$TermParaphraseFn :arg1)))) 
      (#$TermParaphraseFn :ARG0)))

(s2t-test #$componentFields 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "the #$value #$of #$the #$field") 
      (#$TermParaphraseFn :arg1) 
      (#$BestNLPhraseOfStringFn "is #$formed #$partly #$from #$the #$value #$of #$the #$field") 
      (#$TermParaphraseFn :arg2)))

(s2t-test #$cloudinessOfRegion 
    (#$PhraseFormFn #$NLSentence 
      (#$ConcatenatePhrasesFn 
        (#$TermParaphraseFn-NP :arg1) 
        (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
        (#$TermParaphraseFn-Constrained #$regularDegree :arg2))))

(s2t-test #$PhysicalPortionsOfFn 
    (#$NbarHeadedByNounFormFn #$Portion-TheWord #$plural 
      (#$TermParaphraseFn-PP #$Of-TheWord :arg1)))
(s2t-test #$erects 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Erect-TheWord) 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestNLPhraseOfStringFn "of") 
      (#$TermParaphraseFn-NP :arg3) 
      (#$BestNLPhraseOfStringFn "in #$or #$at") 
      (#$TermParaphraseFn :ARG4)))
(s2t-test #$TroopMovementAlongFn 
    (#$PhraseFormFn 
      (#$PhraseFn-Bar1 #$Noun) 
      (#$ConcatenatePhrasesFn 
        (#$BestNLPhraseOfStringFn "troop") 
        (#$HeadWordOfPhraseFn 
          (#$BestNLWordFormOfLexemeFn-Constrained #$MassNoun #$Movement-TheWord)) 
        (#$BestPPFn #$Along-TheWord 
          (#$TermParaphraseFn-Constrained #$nonSingular-Generic :arg1)))))
(s2t-test #$SelfInjectableFn 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "self-injectable") 
      (#$TermParaphraseFn-NP :arg1)))
(s2t-test #$vulnerableToAgent 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "vulnerable #$to #$harm #$by") 
      (#$TermParaphraseFn-Constrained #$nonSingular-Generic :arg2)))
(s2t-test #$cotemporal 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Have-TheWord) 
      (#$BestNLPhraseOfStringFn "exactly #$the #$same #$temporal #$extent #$as") 
      (#$TermParaphraseFn :arg2)))
(s2t-test #$soleMakerOfProductType 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "the #$sole #$maker #$of") 
      (#$TermParaphraseFn-NP :arg1)))
(s2t-test 
    (#$ComplexActionFn 
      (#$NonDavidsonianPredFn 
        (#$AggregateSituationTypeByRoleFn #$GainingUserRights #$objectOfPossessionTransfer) 
        (#$TheList #$toPossessor 
          (#$AggregateRoleFn #$objectOfPossessionTransfer)))) 
    (#$PhraseFormFn #$NLSentence 
      (#$ConcatenatePhrasesFn 
        (#$BestNLPhraseOfStringFn "the #$roles") 
        (#$TermParaphraseFn 
          (#$TheList #$toPossessor 
            (#$AggregateRoleFn #$objectOfPossessionTransfer))) 
        (#$BestNLPhraseOfStringFn "are #$played #$by") 
        (#$NLConjunctionFn 
          (#$RepeatForSubsequentArgsFn 1 
            (#$TermParaphraseFn-NP :arg1))) 
        (#$BestNLPhraseOfStringFn "in") 
        (#$BestDetNbarFn-Indefinite 
          (#$TermParaphraseFn 
            (#$AggregateSituationTypeByRoleFn #$GainingUserRights #$objectOfPossessionTransfer))))))
(s2t-test #$conceivableEnemies 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestNLPhraseOfStringFn "and") 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestNLPhraseOfStringFn "are #$conceivably #$enemies")))
(s2t-test #$SecondsDuration 
    (#$BestDetNbarFn 
      (#$ConcatenatePhrasesFn 
        (#$ConditionalPhraseFn 
          (#$formulaArity :SELF 2) 
          (#$ConcatenatePhrasesFn 
            (#$BestNLWordFormOfLexemeFn #$Between-TheWord) 
            (#$TermParaphraseFn-NPAndNP :arg1 :arg2)) 
          (#$HeadWordOfPhraseFn 
            (#$TermParaphraseFn :arg1)))) 
      (#$TermParaphraseFn :ARG0)))
(s2t-test #$pathFollowedInTask 
    (#$NPIsXP-NLSentenceFn 
      (#$TermParaphraseFn :arg1) 
      (#$DefiniteNounPPFn #$Path-TheWord "followed #$in" 
        (#$TermParaphraseFn :arg2))))
(s2t-test #$electricalResistanceOfObject 
    (#$BasicTransitive-PassiveSentenceFn 
      (#$TermParaphraseFn :arg1) #$Have-TheWord 
      (#$TermParaphraseFn :arg2)))
(s2t-test #$interCommunicators 
    (#$PhraseFormFn #$NLSentence 
      (#$ConcatenatePhrasesFn 
        (#$NLSimpleBinaryConjunctionFn 
          (#$TermParaphraseFn-NP :arg2) 
          (#$TermParaphraseFn-NP :arg3)) 
        (#$ConcatenatePhrasesFn 
          (#$BestHeadVerbForInitialSubjectFn #$Communicate-TheWord) 
          (#$BestPPFn #$With-TheWord 
            (#$BestNLPhraseOfStringFn "each #$other")) 
          (#$BestPPFn #$In-TheWord 
            (#$TermParaphraseFn :arg1))))))
(s2t-test #$sinner 
    (#$TensedPhraseFn-DefaultPast :arg1 
      (#$ConcatenatePhrasesFn 
        (#$TermParaphraseFn-NP :arg2) 
        (#$BestHeadVerbForInitialSubjectFn #$Sin-TheWord) 
        (#$BestPPFn #$During-TheWord 
          (#$TermParaphraseFn-NP :arg1)))))
(s2t-test #$UniquePartFn 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "the") 
      (#$TermParaphraseFn :arg2) 
      (#$BestPPFn #$Of-TheWord 
        (#$TermParaphraseFn :arg1))))
(s2t-test #$temporallySubsumes 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestNLPhraseOfStringFn "temporally") 
      (#$BestHeadVerbForInitialSubjectFn #$Subsume-TheWord) 
      (#$TermParaphraseFn-NP :arg2)))
(s2t-test #$earliestDateOfFact 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "the #$earliest-starting #$date #$at #$which #$it #$is #$known #$to #$be #$true #$that") 
      (#$TermParaphraseFn :arg1)))
(s2t-test #$Milligram 
    (#$BestDetNbarFn 
      (#$ConcatenatePhrasesFn 
        (#$ConditionalPhraseFn 
          (#$formulaArity :SELF 2) 
          (#$ConcatenatePhrasesFn 
            (#$BestNLWordFormOfLexemeFn #$Between-TheWord) 
            (#$TermParaphraseFn-NPAndNP :arg1 :arg2)) 
          (#$HeadWordOfPhraseFn 
            (#$TermParaphraseFn :arg1)))) 
      (#$TermParaphraseFn :ARG0)))
(s2t-test #$obstacleToFrom-Place 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "an #$obstacle #$for") 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestNLPhraseOfStringFn "moving #$into #$the #$area #$of") 
      (#$TermParaphraseFn-NP :arg3)))
(s2t-test #$isotopeOf 
    (#$NPIsXP-NLSentenceFn 
      (#$TermParaphraseFn :arg1) 
      (#$BestDetNbarFn-Indefinite 
        (#$ConcatenatePhrasesFn 
          (#$HeadWordOfPhraseFn 
            (#$BestNLWordFormOfLexemeFn-Constrained #$singular #$Isotope-TheWord)) 
          (#$BestPPFn #$Of-TheWord 
            (#$TermParaphraseFn-Constrained #$nonPlural-Generic :arg2))))))
(s2t-test #$suspendingFluid 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "the #$suspending #$fluid #$in") 
      (#$TermParaphraseFn :arg1)))
(s2t-test #$hasMembers 
    (#$XHasYAsAZ-NLSentenceFn 
      (#$TermParaphraseFn :arg1) 
      (#$TermParaphraseFn :arg2) 
      (#$ConcatenatePhrasesFn 
        (#$HeadWordOfPhraseFn 
          (#$BestNLWordFormOfLexemeFn-Constrained #$simpleNounStrings #$Member-TheWord)))))
(s2t-test #$uniformTypeComponentType 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "a #$component #$of") 
      (#$TermParaphraseFn :arg1)))
(s2t-test #$BillionDollars 
    (#$BestDetNbarFn 
      (#$ConcatenatePhrasesFn 
        (#$ConditionalPhraseFn 
          (#$formulaArity :SELF 2) 
          (#$ConcatenatePhrasesFn 
            (#$BestNLWordFormOfLexemeFn #$Between-TheWord) 
            (#$TermParaphraseFn-NPAndNP :arg1 :arg2)) 
          (#$HeadWordOfPhraseFn 
            (#$TermParaphraseFn :arg1)))) 
      (#$TermParaphraseFn :ARG0)))
(s2t-test #$cultureBeliefs 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "a #$characteristic #$belief #$of #$culture") 
      (#$TermParaphraseFn :arg1)))
(s2t-test 
    (#$OfDateFn #$CalendarSecond) 
    (#$NPIsXP-NLSentenceFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestDetNbarFn-Definite 
        (#$NbarHeadedByNounFormFn #$Second-TheWord #$CountNoun 
          (#$BestPPFn #$Of-TheWord 
            (#$TermParaphraseFn-NP :arg2))))))
(s2t-test #$LineBetweenFn 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "the #$line #$between") 
      (#$TermParaphraseFn :arg1) 
      (#$BestNLPhraseOfStringFn "and") 
      (#$TermParaphraseFn :arg2)))
(s2t-test 
    (#$NonDavidsonianPredFn 
      (#$AggregateSituationTypeByRoleFn #$GainingUserRights #$objectOfPossessionTransfer) 
      (#$TheList #$toPossessor 
        (#$AggregateRoleFn #$objectOfPossessionTransfer))) 
    (#$PhraseFormFn #$NLSentence 
      (#$ConcatenatePhrasesFn 
        (#$BestNLPhraseOfStringFn "the #$roles") 
        (#$TermParaphraseFn 
          (#$TheList #$toPossessor 
            (#$AggregateRoleFn #$objectOfPossessionTransfer))) 
        (#$BestNLPhraseOfStringFn "are #$played #$by") 
        (#$NLConjunctionFn 
          (#$RepeatForSubsequentArgsFn 1 
            (#$TermParaphraseFn-NP :arg1))) 
        (#$BestNLPhraseOfStringFn "in") 
        (#$BestDetNbarFn-Indefinite 
          (#$TermParaphraseFn 
            (#$AggregateSituationTypeByRoleFn #$GainingUserRights #$objectOfPossessionTransfer))))))
(s2t-test #$reportsTo 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Report-TheWord) 
      (#$BestNLPhraseOfStringFn "to") 
      (#$TermParaphraseFn :arg2)))
(s2t-test #$agentEstimatedRemainingWorkForSpec 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Estimate-TheWord) 
      (#$BestNLPhraseOfStringFn "that #$there #$is") 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestNLPhraseOfStringFn "of #$work #$left #$on") 
      (#$TermParaphraseFn-NP :arg3)))
(s2t-test #$Knot-Speed 
    (#$BestDetNbarFn 
      (#$ConcatenatePhrasesFn 
        (#$ConditionalPhraseFn 
          (#$formulaArity :SELF 2) 
          (#$ConcatenatePhrasesFn 
            (#$BestNLWordFormOfLexemeFn #$Between-TheWord) 
            (#$TermParaphraseFn-NPAndNP :arg1 :arg2)) 
          (#$HeadWordOfPhraseFn 
            (#$TermParaphraseFn :arg1)))) 
      (#$TermParaphraseFn :ARG0)))
(s2t-test #$distanceTranslated 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "the #$distance #$moved #$by") 
      (#$TermParaphraseFn :arg1)))
(s2t-test #$objectAdheredTo 
    (#$TensedPhraseFn-DefaultPast :arg1 
      (#$ConcatenatePhrasesFn 
        (#$TermParaphraseFn-NP :arg2) 
        (#$BestHeadVerbForInitialSubjectFn #$Have-TheWord) 
        (#$BestNLPhraseOfStringFn "something #$attached #$to #$it #$in") 
        (#$TermParaphraseFn :arg1))))
(s2t-test #$CollectionSubsetFn 
    (#$ConcatenatePhrasesFn 
      (#$HeadWordOfPhraseFn 
        (#$TermParaphraseFn-Constrained #$Noun :arg1)) 
      (#$BestNLPhraseOfStringFn "which") 
      (#$BestVerbFormForSubjectFn #$Be-TheWord 
        (#$NthPhraseFn 1)) 
      (#$PhraseFormFn 
        (#$NthPhraseAgrFn 1) 
        (#$BestDetNbarFn-Indefinite 
          (#$BestNLWordFormOfLexemeFn-Constrained #$simpleNounStrings #$Element-TheWord))) 
      (#$BestPPFn #$Of-TheWord 
        (#$TermParaphraseFn-NP :arg2))))
(s2t-test #$LocalAdministratorAccountFn 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "local") 
      (#$BestNLWordFormOfLexemeFn-Constrained #$agentive-Sg #$Administrate-TheWord) 
      (#$HeadWordOfPhraseFn 
        (#$BestNLWordFormOfLexemeFn-Constrained #$CountNoun #$Account-TheWord)) 
      (#$BestPPFn #$On-TheWord 
        (#$TermParaphraseFn :arg1))))
(s2t-test #$lethalitySummaryForTypeWRTOrgType 
    (#$GenTemplateRecipeOmitsArgFn :arg2 
      (#$ConcatenatePhrasesFn 
        (#$BestNLWordFormOfLexemeFn-Constrained #$massNumber 
          (#$WordWithSuffixFn #$Lethal-TheWord #$Ity-TheSuffix)) 
        (#$BestNLWordFormOfLexemeFn-Constrained #$nonSingular-Generic #$Data-TheWord) 
        (#$BestPPFn #$For-TheWord 
          (#$TermParaphraseFn-Constrained #$nonSingular-Generic :arg1)) 
        (#$BestNLPhraseOfStringFn ":") 
        (#$TermParaphraseFn-Constrained #$NLSentence :arg3))))
(s2t-test #$doUninstallProgram 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn :arg1) 
      (#$BestNLPhraseOfStringFn "uninstalls") 
      (#$TermParaphraseFn :arg2) 
      (#$BestPPFn #$From-TheWord 
        (#$TermParaphraseFn :arg3))))
(s2t-test #$oldTownOf 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg2) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "the #$historic #$area #$of") 
      (#$TermParaphraseFn :arg1)))
(s2t-test #$ventralTo 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "ventral #$to") 
      (#$TermParaphraseFn :arg2)))
(s2t-test #$solubleIn 
    (#$ConcatenatePhrasesFn 
      (#$TermParaphraseFn-NP :arg1) 
      (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
      (#$BestNLPhraseOfStringFn "soluble") 
      (#$BestPPFn #$In-TheWord 
        (#$TermParaphraseFn-NP :arg2))))
(s2t-test #$cycProblemArgumentLinks 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "the") 
      (#$BestNLPhraseOfStringFn "inference #$problem") 
      (#$TermParaphraseFn-Constrained #$singular-Generic :arg1) 
      (#$PhraseFormFn #$presentTense-Generic 
        (#$HeadWordOfPhraseFn 
          (#$BestVerbFormForSubjectFn #$Have-TheWord 
            (#$NthPhraseFn 3)))) 
      (#$TermParaphraseFn-Constrained #$singular-Generic :arg2) 
      (#$BestNLPhraseOfStringFn "as") 
      (#$BestNLPhraseOfStringFn "an #$inference #$argument #$link")))
(s2t-test #$BBCCountryProfileFn 
    (#$BestDetNbarFn-Definite
      (#$ConcatenatePhrasesFn 
        (#$BestNLPhraseOfStringFn "BBC #$Country") 
        (#$HeadWordOfPhraseFn 
          (#$BestNLWordFormOfLexemeFn-Constrained #$singular #$Profile-TheWord)) 
        (#$BestNLPhraseOfStringFn "for") 
        (#$QuotedParaphraseFn :arg1))))
(s2t-test #$superLocationType 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "every") 
      (#$TermParaphraseFn-NP :arg1) 
      (#$PhraseFormFn #$presentTense-Generic 
        (#$HeadWordOfPhraseFn 
          (#$BestVerbFormForSubjectFn #$Be-TheWord 
            (#$NthPhraseFn 2)))) 
      (#$BestNLPhraseOfStringFn "spatially #$contained #$in") 
      (#$BestDetNbarFn-Indefinite 
        (#$TermParaphraseFn :arg2))))

(s2t-test #$formulaSubstitutionFor 
    (#$ConcatenatePhrasesFn 
      (#$BestNLPhraseOfStringFn "substituting") 
      (#$QuotedParaphraseFn 
        (#$TermParaphraseFn-Constrained #$nonPlural-Generic :arg1)) 
      (#$BestNLPhraseOfStringFn "for") 
      (#$QuotedParaphraseFn 
        (#$TermParaphraseFn-Constrained #$nonPlural-Generic :arg2)) 
      (#$BestNLPhraseOfStringFn "in") 
      (#$BestNLPhraseOfStringFn "the #$formula") 
      (#$QuotedParaphraseFn 
        (#$TermParaphraseFn-Constrained #$nonPlural-Generic :arg4)) 
      (#$PhraseFormFn #$plural 
        (#$HeadWordOfPhraseFn 
          (#$BestVerbFormForSubjectFn #$Produce-TheWord 
            (#$NthPhraseFn 7)))) 
      (#$BestNLPhraseOfStringFn "the #$formula") 
      (#$QuotedParaphraseFn 
        (#$TermParaphraseFn-Constrained #$nonPlural-Generic :arg3))))

(s2t-test #$financialState 
    (#$PhraseFormFn #$NLSentence 
      (#$ConcatenatePhrasesFn 
        (#$TermParaphraseFn-NP :arg1) 
        (#$BestHeadVerbForInitialSubjectFn #$Be-TheWord) 
        (#$TermParaphraseFn-Constrained #$regularDegree :arg2))))


