:- set_prolog_flag(double_quotes, string).
:- install_constant_renamer_until_eof.
:- set_prolog_flag(do_renames_sumo,never).

%textCached('Are-TheWord', denotation, [ 'CountNoun', 'Are-UnitOfArea']).
%textCached('Be-TheWord', denotation, [ 'BeAux', 'is-Underspecified']).
%textCached('My-TheWord', denotation, [ 'PossessivePronoun-Pre', 'PronounFn'('FirstPerson-NLAttr', 'Singular-NLAttr', 'Ungendered-NLAttr', 'PossessivePronoun-Pre')]).
%textCached('Name-TheWord', denotation, [ 'CountNoun', 'Name']).
%textCached('Name-TheWord', denotation, [ 'CountNoun', nameString]).
%textCached('Name-TheWord', denotation, [ 'Verb', 'NamingSomething']).
%textCached('You-TheWord', denotation, [ 'ObjectPronoun', 'PronounFn'('SecondPerson-NLAttr', 'Plural-NLAttr', 'Ungendered-NLAttr', 'ObjectPronoun')]).
%textCached('You-TheWord', denotation, [ 'ObjectPronoun', 'PronounFn'('SecondPerson-NLAttr', 'Singular-NLAttr', 'Ungendered-NLAttr', 'ObjectPronoun')]).
%textCached('You-TheWord', denotation, [ 'SubjectPronoun', 'PronounFn'('SecondPerson-NLAttr', 'Plural-NLAttr', 'Ungendered-NLAttr', 'SubjectPronoun')]).
%textCached('You-TheWord', denotation, [ 'SubjectPronoun', 'PronounFn'('SecondPerson-NLAttr', 'Singular-NLAttr', 'Ungendered-NLAttr', 'SubjectPronoun')]).
:- multifile(textCached/3).
:- dynamic(textCached/3).
:- style_check(-singleton).

textCached('A-TheWord',frame, ['A-TheWord', 'Determiner', 'QuantifierFrame', thereExists(':NOUN', and(':RESTR', ':SCOPE')), determinerSemTrans]).
textCached('About-TheWord',frame, ['About-TheWord', 'Adverb', 'DeterminerModifyingFrame', 'ApproximatelyFn'(':DET'), adverbSemTrans]).
textCached('About-TheWord',frame, ['About-TheWord', 'AdverbOfFrequency', 'DeterminerModifyingFrame', 'ApproximatelyFn'(':DET'), adverbSemTrans]).
textCached('About-TheWord',frame, ['About-TheWord', 'AdverbOfManner', 'DeterminerModifyingFrame', 'ApproximatelyFn'(':DET'), adverbSemTrans]).
textCached('About-TheWord',frame, ['About-TheWord', 'AdverbOfPlace', 'DeterminerModifyingFrame', 'ApproximatelyFn'(':DET'), adverbSemTrans]).
textCached('About-TheWord',frame, ['About-TheWord', 'AdverbOfTime', 'DeterminerModifyingFrame', 'ApproximatelyFn'(':DET'), adverbSemTrans]).
textCached('About-TheWord',frame, ['About-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', implies(and(isa(':NOUN', 'InformationBearingObject'), isa(':OBLIQUE-OBJECT', 'Thing')), containsInformationAbout(':NOUN', ':OBLIQUE-OBJECT')), 'prepReln-Object']).
textCached('About-TheWord',frame, ['About-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', implies(and(isa(':NOUN', 'OneToManyCommunication'), isa(':OBLIQUE-OBJECT', 'Thing')), and(isa(':NOUN', 'AnnouncingSomething'), topicOfInfoTransfer(':NOUN', ':OBLIQUE-OBJECT'))), 'prepReln-Object']).
textCached('About-TheWord',frame, ['About-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', implies(and(isa(':NOUN', 'PropositionalInformationThing'), isa(':OBLIQUE-OBJECT', 'Thing')), propositionalInfoAbout(':NOUN', ':OBLIQUE-OBJECT')), 'prepReln-Object']).
textCached('About-TheWord',frame, ['About-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', subjectOfInfo(':NOUN', ':OBJECT'), prepSemTrans]).
textCached('About-TheWord',frame, ['About-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', topicOfInfoTransfer(':ACTION', ':OBLIQUE-OBJECT'), prepSemTrans]).
textCached('Absent-TheWord',frame, ['Absent-TheWord', 'Verb', 'PPCompFrameFn'('DitransitivePPFrameType', 'From-TheWord'), not(socialParticipants(':OBJECT', ':SUBJECT')), verbSemTrans]).
textCached('Access-TheWord',frame, ['Access-TheWord', 'CountNoun', 'GenitiveFrame', and(isa(':NOUN', 'AccessingAnIBT'), informationOrigin(':NOUN', ':POSSESSOR')), nounSemTrans]).
textCached('Access-TheWord',frame, ['Access-TheWord', 'Noun', 'GenitiveFrame', and(isa(':NOUN', 'AccessingAnIBT'), informationOrigin(':NOUN', ':POSSESSOR')), nounSemTrans]).
textCached('Access-TheWord',frame, ['Access-TheWord', 'Verb', 'TransitiveNPFrame', and(isa(':ACTION', 'AccessingAnIBT'), performedBy(':ACTION', ':SUBJECT'), informationOrigin(':ACTION', ':OBJECT')), verbSemTrans]).
textCached('Access-TheWord',frame, ['Access-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', and(isa(':ACTION', 'AccessingAnIBT'), situationConstituents(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT'))), verbSemTransPartial]).
textCached('Access-TheWord',frame, ['Access-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', and(situationConstituents(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT'), isa(':ACTION', 'AccessingAnIBT'))), verbSemTransPartial]).
textCached('Admit-TheWord',frame, ['Admit-TheWord', 'Verb', 'DitransitiveNP-InfinitivePhraseFrame', and(isa(':ACTION', 'AdmitToMembership'), performedBy(':ACTION', ':SUBJECT'), recipientOfInfo(':ACTION', ':OBJECT'), infoTransferred(':ACTION', A), 'ist-Information'(A, performedBy(':INF-COMP', ':OBJECT'))), verbSemTrans]).
textCached('Admit-TheWord',frame, ['Admit-TheWord', 'Verb', 'DitransitiveNP-InfinitivePhraseFrame', and(isa(':ACTION', 'PermittingEntrance'), performedBy(':ACTION', ':SUBJECT'), recipientOfInfo(':ACTION', ':OBJECT'), infoTransferred(':ACTION', A), 'ist-Information'(A, performedBy(':INF-COMP', ':OBJECT'))), verbSemTrans]).
textCached('Admit-TheWord',frame, ['Admit-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', and(isa(':ACTION', 'AdmitToMembership'), situationConstituents(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT'))), verbSemTransPartial]).
textCached('Admit-TheWord',frame, ['Admit-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', and(isa(':ACTION', 'PermittingEntrance'), situationConstituents(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT'))), verbSemTransPartial]).
textCached('Admit-TheWord',frame, ['Admit-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', and(situationConstituents(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT'), isa(':ACTION', 'AdmitToMembership'))), verbSemTransPartial]).
textCached('Admit-TheWord',frame, ['Admit-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', and(situationConstituents(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT'), isa(':ACTION', 'PermittingEntrance'))), verbSemTransPartial]).
textCached('All-TheWord',frame, ['All-TheWord', 'Determiner', 'QuantifierFrame', forAll(':NOUN', implies(':RESTR', ':SCOPE')), determinerSemTrans]).
textCached('Along-TheWord',frame, ['Along-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', 'motionPathway-Partial'(':NOUN', ':OBJECT'), prepSemTrans]).
textCached('Along-TheWord',frame, ['Along-TheWord', 'Preposition', 'VerbPhraseModifyingFrame', 'motionPathway-Partial'(':EVENT', ':OBJECT'), prepSemTrans]).
textCached('An-TheWord',frame, ['An-TheWord', 'Determiner', 'QuantifierFrame', thereExists(':NOUN', and(':RESTR', ':SCOPE')), determinerSemTrans]).
textCached('Ancient-TheWord',frame, ['Ancient-TheWord', 'Adjective', 'RegularAdjFrame', startsDuring(':NOUN', 'Antiquity'), adjSemTrans]).
textCached('Anger-TheWord',frame, ['Anger-TheWord', 'Adjective', 'RegularAdjFrame', feelsEmotion(':NOUN', 'MediumToVeryHighAmountFn'('Anger')), adjSemTrans]).
textCached('Anger-TheWord',frame, ['Anger-TheWord', 'Adjective', 'RegularAdjFrame', feelsEmotionTypeAtLevel(':NOUN', 'MediumToVeryHighAmountFn'('Anger')), adjSemTrans]).
textCached('Arabian-TheWord',frame, ['Arabian-TheWord', 'Adjective', 'RegularAdjFrame', conceptuallyRelated(':NOUN', 'ArabianPeninsula'), adjSemTrans]).
textCached('Are-TheWord', completeSemTrans,[]).
textCached('Are-TheWord',frame, ['Are-TheWord', 'CountNoun', 'NumberFrame', equals(':NOUN', 'Are-UnitOfArea'(':NUMBER')), nounSemTrans]).
textCached('Are-TheWord',frame, ['Are-TheWord', 'Noun', 'NumberFrame', equals(':NOUN', 'Are-UnitOfArea'(':NUMBER')), nounSemTrans]).
textCached('Arrive-TheWord',frame, ['Arrive-TheWord', 'CountNoun', 'GenitiveFrame', and(isa(':NOUN', 'ArrivingAtAPlace'), doneBy(':NOUN', ':POSSESSOR')), nounSemTrans]).
textCached('Arrive-TheWord',frame, ['Arrive-TheWord', 'Noun', 'GenitiveFrame', and(isa(':NOUN', 'ArrivingAtAPlace'), doneBy(':NOUN', ':POSSESSOR')), nounSemTrans]).
textCached('Arrive-TheWord',frame, ['Arrive-TheWord', 'Verb', 'IntransitiveVerbFrame', and(isa(':ACTION', 'ArrivingAtAPlace'), doneBy(':ACTION', ':SUBJECT')), verbSemTrans]).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', 'at-UnderspecifiedLandmark'(':NOUN', ':OBJECT'), prepSemTrans]).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', implies(and(isa(':NOUN', 'Agent-PartiallyTangible'), isa(':OBLIQUE-OBJECT', 'Event')), spectators(':OBLIQUE-OBJECT', ':NOUN')), 'prepReln-Object']).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', implies(and(isa(':NOUN', 'Event'), isa(':OBLIQUE-OBJECT', 'Place')), eventOccursAt(':NOUN', ':OBLIQUE-OBJECT')), 'prepReln-Object']).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', implies(and(isa(':NOUN', 'PartiallyTangible'), isa(':OBLIQUE-OBJECT', 'SpatialThing-Localized')), objectFoundInLocation(':NOUN', ':OBLIQUE-OBJECT')), 'prepReln-Object']).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'Post-NounPhraseModifyingFrame', objectFoundInLocation(':NOUN', ':OBJECT'), prepSemTrans]).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'VerbPhraseModifyingFrame', 'at-UnderspecifiedLandmark'(':ACTION', ':OBJECT'), prepSemTrans]).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'VerbPhraseModifyingFrame', eventOccursAt(':ACTION', ':OBJECT'), prepSemTrans]).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'VerbPhraseModifyingFrame', implies(and(isa(':ACTION', 'Event'), isa(':OBLIQUE-OBJECT', 'SpatialThing')), eventOccursAt(':ACTION', ':OBLIQUE-OBJECT')), 'prepReln-Action']).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'VerbPhraseModifyingFrame', implies(and(isa(':ACTION', 'Event'), isa(':OBLIQUE-OBJECT', 'TimeOfDay')), temporallySubsumes(':OBLIQUE-OBJECT', 'StartFn'(':ACTION'))), 'prepReln-Action']).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'VerbPhraseModifyingFrame', implies(and(isa(':ACTION', 'GeneralizedTransfer'), isa(':OBLIQUE-OBJECT', 'PartiallyTangible')), target(':ACTION', ':OBLIQUE-OBJECT')), 'prepReln-Action']).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'VerbPhraseModifyingFrame', implies(and(isa(':ACTION', 'MakingAnOralSound'), isa(':OBLIQUE-OBJECT', 'PartiallyTangible')), communicationTarget(':ACTION', ':OBLIQUE-OBJECT')), 'prepReln-Action']).
textCached('At-TheWord',frame, ['At-TheWord', 'Preposition', 'VerbPhraseModifyingFrame', temporallyIntersects(':OBJECT', 'StartFn'(':ACTION')), prepSemTrans]).
textCached('Attend-TheWord',frame, ['Attend-TheWord', 'Verb', 'TransitiveNPFrame', socialParticipants(':OBJECT', ':SUBJECT'), verbSemTrans]).
textCached('Attend-TheWord',frame, ['Attend-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', holdsIn(':ACTION', socialParticipants(':SUBJECT', ':OBJECT'))), denotationRelatedTo]).
textCached('Babysit-TheWord',frame, ['Babysit-TheWord', 'CountNoun', 'GenitiveFrame', and(isa(':NOUN', 'Babysitting'), beneficiary(':NOUN', ':POSSESSOR')), nounSemTrans]).
textCached('Babysit-TheWord',frame, ['Babysit-TheWord', 'Noun', 'GenitiveFrame', and(isa(':NOUN', 'Babysitting'), beneficiary(':NOUN', ':POSSESSOR')), nounSemTrans]).
textCached('Babysit-TheWord',frame, ['Babysit-TheWord', 'Verb', 'TransitiveNPFrame', and(isa(':ACTION', 'Babysitting'), beneficiary(':ACTION', ':OBJECT'), performedBy(':ACTION', ':SUBJECT')), verbSemTrans]).
textCached('Bad-TheWord',frame, ['Bad-TheWord', 'Adjective', 'RegularAdjFrame', hasEvaluativeQuantity(':NOUN', 'MediumToVeryHighAmountFn'('Badness-Generic')), adjSemTrans]).
textCached('Bargain-TheWord',frame, ['Bargain-TheWord', 'Verb', 'PPCompFrameFn'('TransitivePPFrameType', 'With-TheWord'), and(socialParticipants(':ACTION', ':OBLIQUE-OBJECT'), socialParticipants(':ACTION', ':SUBJECT'), isa(':ACTION', 'Bargaining')), verbSemTrans]).
textCached('Bargain-TheWord',frame, ['Bargain-TheWord', 'Verb', 'UnderstoodReciprocalObjectFrame', and(socialParticipants(':ACTION', ':SUBJECT'), isa(':ACTION', 'Bargaining')), verbSemTrans]).
textCached('Base-TheWord',frame, ['Base-TheWord', 'Verb', 'PPCompFrameFn'('TransitivePPFrameType', 'On-TheWord'), 'relyOn-Generic'(':SUBJECT', ':OBLIQUE-OBJECT'), verbSemTrans]).
textCached('Base-TheWord',wsframe, ['Base-TheWord', 'TheList'(string("of"), string("operations")), 'GenitiveFrame', headquarters(':POSSESSOR', ':NOUN'), 'CountNoun']).
textCached('Bat-TheWord',frame, ['Bat-TheWord', 'Verb', 'IntransitiveVerbFrame', thereExists(':ACTION', and(bodilyDoer(':SUBJECT', ':ACTION'), isa(':ACTION', 'SportsEvent'), possible(isa(':SUBJECT', 'BaseballBatter')))), performsInstancesAsPartOfJob]).
textCached('Bat-TheWord',frame, ['Bat-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', and(bodilyDoer(':SUBJECT', ':ACTION'), isa(':ACTION', 'SportsEvent'), possible(isa(':SUBJECT', 'BaseballBatter')))), performsInstancesAsPartOfJob]).
textCached('Bat-TheWord',frame, ['Bat-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', and(isa(':ACTION', 'BaseballSwing'), situationConstituents(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT'))), verbSemTransPartial]).
textCached('Bat-TheWord',frame, ['Bat-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', and(situationConstituents(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT'), isa(':ACTION', 'BaseballSwing'))), verbSemTransPartial]).
textCached('Be-TheWord', completeSemTrans,[]).
textCached('Be-TheWord',frame, ['Be-TheWord', 'AuxVerb', 'CopulaGenericFrame', ':COMPLEMENT', auxVerbSemTrans]).
textCached('Be-TheWord',frame, ['Be-TheWord', 'AuxVerb', 'TransitiveNPFrame', equals(':OBJECT', ':SUBJECT'), auxVerbSemTrans]).
textCached('Be-TheWord',frame, ['Be-TheWord', 'Verb', 'TransitiveNPFrame', thereExists(':ACTION', holdsIn(':ACTION', 'is-Underspecified'(':SUBJECT', ':OBJECT'))), denotation]).
textCached('Be-TheWord',wsframe, ['Be-TheWord', 'TheList'(string("able")), 'TransitiveInfinitivePhraseFrame', capableOf(':SUBJECT', ':ACTION', performedBy), 'Verb']).
textCached('Be-TheWord',wsframe, ['Be-TheWord', 'TheList'(string("made")), 'PPCompFrameFn'('TransitivePPFrameType', 'Of-TheWord'), constituents(':SUBJECT', ':OBLIQUE-OBJECT'), 'Verb']).
textCached('Be-TheWord',wsframe, ['Be-TheWord', 'TheList'(string('"able"')), 'TransitiveInfinitivePhraseFrame', capableOf(':SUBJECT', ':ACTION', performedBy), 'Verb']).
textCached('Be-TheWord',wsframe, ['Be-TheWord', 'TheList'(string('"made"')), 'PPCompFrameFn'('TransitivePPFrameType', 'Of-TheWord'), constituents(':SUBJECT', ':OBLIQUE-OBJECT'), 'Verb']).


:- include('posm_cached_data2.nldata').
