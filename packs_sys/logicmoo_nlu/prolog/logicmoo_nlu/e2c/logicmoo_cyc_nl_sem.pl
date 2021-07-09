%<?
% ===================================================================
% File 'e2c_sem.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2005/06/06 15:43:15 $
% from Bratko chapter 17 page 455. This comes from Pereira and Warren paper, AI journal, 1980
/*
[14:52] <kinoc> http://turing.cs.washington.edu/papers/yates_dissertation.pdf
[14:54] <kinoc> this was the one I was looking for  http://www.cs.washington.edu/homes/soderlan/aaaiSymposium2007.pdf
[15:01] <kinoc> http://www.pat2pdf.org/patents/pat20080243479.pdf  while it lasts
% http://danielmclaren.net/2007/05/11/getting-started-with-opennlp-natural-language-processing/
% http://opennlp.sourceforge.net
% http://www.cs.washington.edu/research/textrunner/index.html
*/
% ===================================================================


/*
:-module(e2c_sem, [
	e2c_sem/1,
	e2c_sem/2,
	testE2C/0]).


(SIMPLIFY-CYCL-SENTENCE-SYNTAX ' (#$and
    (#$and
      (#$isa ?THEY-2
          (#$PronounFn #$ThirdPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$ObjectPronoun)))
    (#$and
      (#$and
        (#$isa ?THEY-2 Agent-Generic)
        (#$and
          (#$preActors ?G20477 ?THEY-2)
          (#$knows ?THEY-2 ?G57625)))
      (#$and
        (#$preActors ?G20477 ?THEY-2)
        (#$knows ?THEY-2 ?G57625)))))

*/

:- use_module(library(logicmoo_plarkc)).

simplifyCycL(In, Out):- catch((cyclify(In, Mid), evalSubl('SIMPLIFY-CYCL-SENTENCE-SYNTAX'(Mid), Out)), _, fail), !.
simplifyCycL(In, In).

% throwOnFailure/1 is like Java/C's assert/1
throwOnFailure(X):-X, !.
throwOnFailure(X):-trace, not(X).

% imports sentencePos/2, bposToPos/2
%:- style_check(+singleton).
:- style_check(-discontiguous).
:- style_check(-atom).
:- set_prolog_flag(double_quotes, string).
:-set_prolog_flag(double_quotes, string).

:- if(exists_source(cyc)).
:-use_module(cyc).
:-setCycOption(query(time), 2).
:-setCycOption(query(time), 2).
:- cyc:startCycAPIServer.
:- endif.

% imports sentencePos/2, bposToPos/2
:- style_check(-singleton).
:- style_check(-discontiguous).
:- style_check(-atom).
:- set_prolog_flag(double_quotes, string).
:-set_prolog_flag(double_quotes, string).
:- install_constant_renamer_until_eof.
:- set_prolog_flag(do_renames_sumo, never).
:-set_prolog_flag(double_quotes, string).
:-multifile(user:currentParsingPhrase/1).
:-multifile(user:recentParses/1).
:-dynamic(user:recentParses/1).
:-dynamic(user:currentParsingPhrase/1).

:- if(exists_source(cyc)).
:-(ensure_loaded('freq.pdat.txt')).
:-(ensure_loaded('colloc.pdat.txt')).
:-(ensure_loaded('pt-grammar.prul2.txt')).
:-(ensure_loaded('BRN_WSJ_LEXICON.txt')).
:-(consult(textCached)).
:-(consult(recentParses)).
:-(ensure_loaded(logicmoo_nl_pos)).
:-multi_transparent(ac/1).
:-multi_transparent(ac/2).
:-multi_transparent(ac/3).
:-multi_transparent(ac/4).
:-multi_transparent(ac/5).
:-multi_transparent(ac/6).
:-multi_transparent(ac/7).
:-multi_transparent(ac/8).

:-ensure_loaded(owl_parser).
:- endif.


todo(X):-notrace(noteFmt('~q', todo(Agent,X))).


printAndThrow(F, A):-sformat(S, F, A), write(user, S), nl(user), flush_output(user), trace, throw(error(representation_error(S), context(printAndThrow/2, S))).

throwOnFailure(A, _B):-call(A), !.
throwOnFailure(A, B):-printAndThrow('~n ;; Must ~q when ~q ~n', [A, B]).

throwOnFailure(A):-catch(A, E, (printAndThrow('~n ;; ~q Must ~q ~n', [E, A]), !, fail)), !.
throwOnFailure(A):-printAndThrow('~n ;; Must ~q ~n', [A]).

% ===================================================================
% Caches
% ===================================================================

:-dynamic(recentParses/1).
:-dynamic(textCached/2).
saveCaches:-tell(textCached), listing(textCached), told, tell(recentParses), listing(recentParses), told.
:-at_halt(saveCaches).

:-use_module(library(make)), redefine_system_predicate(make:list_undefined).
%:-abolish(make:list_undefined/0).
%make:list_undefined.

%:-abolish(make:list_undefined/1).
%make:list_undefined(_).

%:-abolish(make:list_undefined_/2).
%make:list_undefined_(_Informational, _Local).

%:-abolish(list_undefined_/2).
%list_undefined_(_Informational, _Local).


%:-module_transparent(processRequestHook/1).
%:-multifile(processRequestHook/1).
%:-dynamic(processRequestHook/1).

cmember(H, T):-!, member(H, T).
cmember(H, T):-trace, nonvar(T), T=[H|_].
cmember(H, [_|T]):-nonvar(T), cmember(H, T).

processRequestHook(ARGS):- member(file='english.moo', ARGS), !,
      ignore(cmember(english=English, ARGS)),
      ignore(English=''),
      cyc:writeHTMLStdHeader('English Parser'),
      fmt('
      <form method="GET">
	<p><textarea rows="2" name="english" cols="80">~w</textarea><br>
             <input type="submit" value="Parse Normally" name="submit">&nbsp;<input type="submit" value="Parse with idiomatics" name="submit">
                <input type="checkbox" value="CHECKED" name="findall">&nbspShow All<br>
        </p>
       </form>
      <pre>Please wait..
<textarea rows="10" name="debug" cols="80" wrap=off>', [English]),
      flush_output,
      ignore(once(notrace(e2c_sem(English, CycL)))),
      fmt('
      </textarea><br>CycL/KIF<br>
<textarea rows="15" name="cycl" cols="80" wrap=off>', []),
      flush_output,
      once(writeCycL(CycL)),
%      notrace(once((cmember(findall='CHECKED', ARGS), ignore(catch(dotimes(34, (e2c_sem(W), nl, nl)), E, writeq(E)))) ; ignore(once(catch(dotimes(1, (e2c_sem(W), nl, nl)), E, writeq(E)))))),
      flush_output,
      fmt('
      </textarea>
      ..Done
      </pre>'),
      listRecentParses,
      flush_output,
      cyc:writeHTMLStdFooter, flush_output, !.


dotimes(N, Goal):- flag('$dotimes', _, N-1),
      Goal, flag('$dotimes', D, D-1), D<1.

listRecentParses:-recentParses(Eng), once(listRecentParses(Eng)), fail.
listRecentParses.

listRecentParses(Eng):-
      concat_atom(Eng, ' ', EngA),
      fmt('<a href="english.moo?english=~w">~w</a><br>', [EngA, EngA]), nl.

% ===================================================================
% Cyc Database normalizer
% ===================================================================
:- if(exists_source(cyc)).
:-use_module(cyc).
%cycQueryV(Vars, CycL):-free_variables(Vars, Free), cyc:cycQueryReal(CycL, 'EverythingPSC', Free, Backchains, Number, Time, Depth).
cycQueryA(CycL):-cycQuery(CycL).
:- endif.

/*
assertion(Result):-assertion(_, _PRED, _, _MT, [Pred|ARGS], _TRUE, _FORWARD, _DEFAULT, _NIL1, _NIL2, _ASSERTEDTRUEDEF, _DATE, _NIL3),
         once(assertRefactor(Pred, ARGS, Result)).
*/

assertRefactor(implies, [ARG1, ARG2], Result):-litRefactor([implies, ARG1, ARG2], [implies, LARG1, [Pred|LARG2]]), Result=..[ac, pl_implied, Pred, LARG2, LARG1], !.
assertRefactor(Pred, R1, Out):-litRefactor(R1, R2), Out=..[ac, Pred|R2].

litRefactor(Subj, Subj):- (var(Subj);atom(Subj);number(Subj)), !.
litRefactor([H|T], [HH|TT]):-litRefactor(H, HH), litRefactor(T, TT), !.
litRefactor(var(B, _), B):-!.
litRefactor(B, string(A)):-string(B), string_to_atom(B, S), catch(getWordTokens(S, A), _E, A=B), !.
litRefactor(B, A):-compound(B), B=..BL, litRefactor(BL, AL), A=..AL, !.
litRefactor(B, B).

tas:-tell(a), ignore((assertion(R), fmt('~q.~n', [R]), fail)), told.


:- if(exists_source(el_holds)).
:-ensure_loaded(el_holds).
:-retractall(ac(X, Y, zzzzzzzzzzzzzzzzz)).
:-retractall(ac(X, zzzzzzzzzzzzzzzzz, Y)).
:- endif.



writeCycL(CycL):-
      once((toCycApiExpression(CycL, CycL9),
      indentNls(CycL9, CycLOutS),
      fmt('~w~n', [CycLOutS]), flush_output)), !.


atom_junct(X, Y):-concat_atom(Y, ' ', X).


indentNls(X, S):-string_to_list(X, M), indentNls(0, M, Y), string_to_list(S, Y).


nSpaces(N, []):-N<1, !.
nSpaces(N, [32|More]):-NN is N-1, nSpaces(NN, More), !.

indentNls(_, [], []).
indentNls(N, [40|More], CycLOutS):-NN is N+2, indentNls(NN, More, CycL9), nSpaces(N, Spaces), append([10|Spaces], [40|CycL9], CycLOutS).
indentNls(N, [41|More], [41|CycLOutS]):-NN is N-2, indentNls(NN, More, CycLOutS).
indentNls(N, [X|More], [X|CycLOutS]):-indentNls(N, More, CycLOutS).

noteFmt(F, A):-
        dmsg(F, A), !,
        %% next two lines if writing to a file will write some debug
        current_output(X), !,
        (stream_property(X, alias(user_output)) -> true ; (write(X, ' %% DEBUG '), fmt(X, F, A))), !.


sentenceUnbreaker([], []).
sentenceUnbreaker([[W|Ws]|Tagged], [R|ForLGP]):-concat_atom([W|Ws], '_', R), sentenceUnbreaker(Tagged, ForLGP).
sentenceUnbreaker([W|Tagged], [R|ForLGP]):-concat_atom([W], '_', R), sentenceUnbreaker(Tagged, ForLGP).

%getWordTokens("this is English", _Eng).
% string_to_atom(S, 'this is English'), getWordTokens(S, E).
%getWordTokens('this is English', Eng).

% ===================================================================
% Semantic Interpretation
%   when using utterance_sem we need to pass 3 arguments,
%   the first will match CycL in the head of the DGC clause
%   the second is the list containing the words in the utterance_sem
%   the third is the empty list.
% ===================================================================
/*
?- e2c_sem("The cat in the hat sees a bat").

(thereExists ?cat65
       (and (isa ?cat65 (OneOfFn Cat DomesticCat ) )
         (and (and (isa ?sees3 Event ) (thereExists ?bat26
         (and (isa ?bat26 (OneOfFn BaseballBat BaseballSwing Bat-Mammal ) )
         (or (awareOf ?cat65 ?bat26 ) (and (isa ?sees3 VisualPerception )
         (performedBy ?sees3 ?cat65 ) (perceivedThings ?sees3 ?bat26 ) ) ) ) ) )
         (thereExists ?hat27 (and (isa ?hat27 Hat ) (in-UnderspecifiedContainer ?cat65 ?hat27 ) ) ) ) ) )

*/

e2c_sem([]):-!.
e2c_sem(''):-!.
e2c_sem(English):-
      e2c_sem(English, CycL),
      notrace(writeCycL(CycL)), !,
      notrace(fmt('~n For sentence: "~w".~n~n', [English])).

e2c_sem(English, CycL9):-nonvar(English),
     notrace((noteFmt('~nEnglish: ~q.~n~n', [English]))),
     notrace(throwOnFailure(getWordTokens(English, Eng))),
     notrace(asserta_if_new(recentParses(Eng))),
     e2c_sem(Event, Eng, CycL9), !.

e2c_sem(Event, [], []):-!.
e2c_sem(Event, English, CycL9):-
     notrace(noteFmt('~nEng: ~q.~n~n', [English])),
     %concat_atom(English, ' ', Text), fmt('~n?- e2c_sem("~w").~n~n', [Text]), flush_output,
      notrace(sentenceTagger(English, IsTagged)),
      linkParse(English, Links, Tree), writeCycL(Links),
      addLinkTags(IsTagged, Links, Tagged),
      notrace(dumpList(Tagged)), !,
      % time(ignore(catch(e2c_sem(Event, English, Tagged, CycL9), E, (writeCycL(E), ignore(CycL9=error(E)))))),
      e2c_sem(Event, English, Tagged, CycL9),
      flush_output,
      ignore(CycL9=not_parsed(Tagged)),
      saveCaches.


p2c(English, CycL):-
     notrace(noteFmt('~nEng: ~q.~n~n', [English])),
     %concat_atom(English, ' ', Text), fmt('~n?- e2c_sem("~w").~n~n', [Text]), flush_output,
      notrace(sentenceTagger(English, IsTagged)),
      linkParse(English, Links, Tree), writeCycL(Links),
      saveCaches,
      addLinkTags(IsTagged, Links, Tagged),
      notrace(dumpList(Tagged)), !,
      phrase(CycL, Tagged, []),
      notrace(writeCycL(CycL)),
      notrace(fmt('~n For sentence: "~w".~n~n', [English])).


addLinkTags(Pre, ['S'|Links], Post):-!, addLinkTags(Pre, Links, Post).
%addLinkTags(Pre, Links, Post):-openLists(Pre, Post), !.
addLinkTags(Pre, Links, Post):-Post=Pre, !.
%%addLinkTags(Pre, ['NP'|Links], Post):-getWordSegment(Links, Pre, Segs), addToSegs().

e2c_sem(Event, English, Tagged, CycL):- englishCtx(Event, CycL, Tagged, []).
%e2c_sem(Event, English, Tagged, CycL):- chunkParseCycL(Event, Subj, Tagged, CycL).
%e2c_sem(Event, English, Tagged, no_e2c(English)):-!.
e2c_sem(Event, English, Tagged, no_e2c(List)):-dcgMapCar(Var, theWord(Var), List, Tagged, []).

dcgWordList(List)-->dcgMapCar(Var, theWord(Var), List).

dumpList([]):-nl.
dumpList([B|Tagged]):-dumpList1(B), dumpList(Tagged), !.
dumpList1(Tagged):-writeq(Tagged), nl, flush_output.

%:-      setCycOption(cycServer, '10.1.1.3':13701).

cyclifyTest(String, cyc(Result)):-cyc:evalSubL('cyclify'(string(String)), Result).
%cyclifyTest(String, 'cyc-assert'(Result)):-cyc:sublTransaction('10.1.1.3':13701, 'parse-a-sentence-completely'(string(String), '#$RKFParsingMt'), Result).
%cyclifyTest(String, 'cyc-query'(Result)):-cyc:sublTransaction('10.1.1.3':13701, 'parse-a-question-completely'(string(String), '#$RKFParsingMt'), Result).


firstNth1(_Ns, [], []).
firstNth1(Ns, [H|Src], [H|Res]):-Ns>0, N2 is Ns-1, firstNth1(N2, Src, Res).
firstNth1(Ns, Src, []).


lowerOnly([], []):-!.
lowerOnly([[S|H]|T], [[S|H]|TT]):-atom(H), name(H, [A1|_]), is_lower(A1), lowerOnly(T, TT).
lowerOnly([[S|H]|T], TT):-lowerOnly(T, TT).


%sameString(CYCSTRING, String):-var(String), var(CYCSTRING), !, CYCSTRING=String.
%sameString(CYCSTRING, String):-var(CYCSTRING), !, CYCSTRING=String.

reformatStrings(X, X):- (var(X);number(X)), !.
reformatStrings(string(X), S):-string(X), string_to_atom(X, A), reformatStrings(string(A), S).
reformatStrings(string(A), string(S)):-atom(A), !, concat_atom(S, ' ', A).
reformatStrings(X, string(S)):-string(X), string_to_atom(X, S), !.
reformatStrings([], []):-!.
reformatStrings([H|T], [HH|TT]):-!, reformatStrings(H, HH), reformatStrings(T, TT), !.
reformatStrings(X, P):-compound(X), X=..LIST, reformatStrings(LIST, DL), P=..DL, !.
reformatStrings(X, X):-not(atom(X)), !.
reformatStrings(B, A):-atom_concat('#$', A, B), !.
reformatStrings(B, B):-!.

%:-setCycOption(query(time), 2).

% =======================================================
% Text to word info
% =======================================================

harvestCycConstants(DM, DM):-number(DM), !, fail.
harvestCycConstants(Words, DM):-evalSubL(mapcar('#\'cdr', 'denotation-mapper'(string(Words), quote(['#$middleName', '#$alias', '#$initialismString', '#$middleNameInitial', '#$nicknames', '#$givenNames', '#$firstName', '#$abbreviationString-PN']), ':greedy')), List, _), leastOne(List), !, reformatStrings(List, DM), !.
harvestCycConstants(Words, DM):-evalSubL(mapcar('#\'cdr', 'denotation-mapper'(string(Words), quote(['#$middleName', '#$alias', '#$initialismString', '#$middleNameInitial', '#$nicknames', '#$givenNames', '#$firstName', '#$abbreviationString-PN']), ':diligent')), List, _), !, reformatStrings(List, DM), !.

learnTexts([]).
learnTexts([A|B]):-learnText(A), learnTexts(B).

learnText(Atom):-atom(Atom), concat_atom(List, '_', Atom), !, learnText(List).
learnText(X):-textCached(X, [txt|X]), !.
learnText(W):-
      saveText(W, [txt|W]),
      cyc:fmt(learnText(W)),
      ignore((harvestCycConstants(W, CycL), collectionInfo(CycL, COLINFO), saveList(W, COLINFO))),
      ignore(makePosInfo(W)),
      %denotationInfo(POSINFO, DENOTESINFO),
      saveTemplatesForString(W),
      ignore((mwStringsForString(W, MWStrings), saveList(W, MWStrings))),
      saveCaches.
learnText(W):-!. %true.

saveList(String, []):-!.
saveList(String, [A|List]):-saveText(String, A), saveList(String, List).

saveText(String, A):-asserta_if_new(textCached(String, A)).

appendLists([List], List):-!.
appendLists([L|List], Res):-appendLists(List, AL), append(L, AL, Res), !.

%(#$and (#$speechPartPreds ?Pos ?Pred)(?Pred ?Word ?STRING)
asserta_if_new(X):-retractall(X), asserta(X), !.
asserta_if_new(X):-catch(X, _, fail), !.
asserta_if_new(X):-assertz(X), !.

denotationInfo([], []).
denotationInfo([COL|INFO], [[COL|COLINFO]|MORE]):-
      denotationPOS(COL, COLINFO),
      denotationInfo(INFO, MORE).

mwStringsForString([N], []):-number(N), !.
mwStringsForString(N, []):-number(N), !.
mwStringsForString(String, []):-!.
mwStringsForString(String, List):-
   findall([mwsem, PreText, Word, Pos, THING],
        cyc:cycQueryReal(thereExists(Pred,
        and(wordForms(Word, Pred, string(String)), speechPartPreds(Pos, Pred),
        multiWordString(PreText, Word, Pos, THING))), '#$EnglishMt', [PreText, Word, Pos, THING], 3, 'NIL', 10, 30), List).

wordTerms(Word, List):-
   findall([wformulas, Word, Forms],
        cyc:cycQueryReal(termFormulas(Word, Forms), '#$EverythingPSC', [Word, Forms], 3, 'NIL', 10, 30), List).


saveTemplatesForString(String):-
      wordForString(String, Word, Pos),
      saveTemplatesForWord(Word), fail.
saveTemplatesForString(String):-!.


saveTemplatesForWord(Word):-var(Word), !, trace, fail.
saveTemplatesForWord(Word):- textCached(Word, completeSemTrans), !.
saveTemplatesForWord(Word):-
      findall([frame, Word, Pos, FRAME, implies(PreRequ, CYCL), Pred],
                      cycQueryV([Word, Pos, FRAME, CYCL, Pred, PreRequ],
                          wordSemTrans(Word, _Num, FRAME, CYCL, Pos, Pred, PreRequ)), List), saveList(Word, List), fail.

saveTemplatesForWord(Word):-findall([frame, Word, Pos, FRAME, CYCL, Pred],
        cycQueryV([Word, Pos, FRAME, CYCL, Pred], and(isa(Pred, 'SemTransPred'),
                semTransPredForPOS(Pos, Pred), arity(Pred, 4), [Pred, Word, _Num, FRAME, CYCL])), List), once(saveList(Word, List)), fail.

saveTemplatesForWord(Word):-findall([frame, Word, 'Verb', FRAME, CYCL, verbSemTrans], cycQueryV([Word, FRAME, CYCL], verbSemTrans(Word, _Num, FRAME, CYCL)), List), once(saveList(Word, List)), fail.
saveTemplatesForWord(Word):-findall([frame, Word, 'Adjective', FRAME, CYCL, adjSemTrans], cycQueryV([Word, FRAME, CYCL], adjSemTrans(Word, _Num, FRAME, CYCL)), List), once(saveList(Word, List)), fail.


saveTemplatesForWord(Word):-
      findall([swframe, PreText, Word, FRAME, CYCL, Pos], cycQuery(multiWordSemTrans(PreText, Word, Pos, FRAME, CYCL)), List), saveList(Word, List), fail.

%(denotation Automobile-TheWord CountNoun 0 Automobile)
saveTemplatesForWord(Word):-
      findall([denotation, Pos, CYCL], cycQuery(denotation(Word, Pos, _Num, CYCL)), List), saveList(Word, List), fail.

saveTemplatesForWord(Word):-
      findall([wsframe, Word, PreText, FRAME, CYCL, Pos], cycQuery(compoundSemTrans(Word, PreText, Pos, FRAME, CYCL)), List), saveList(Word, List), fail.

saveTemplatesForWord(Word):-asserta(textCached(Word, completeSemTrans)).


denotationPOS([lex, COL|Pos], denote(COL)):-!.


/* in POS FILE
wordForString(String, Word, Pos):-atom(String), !, wordForString([String], Word, Pos).
wordForString(String, Word, Pos):-findall(Word, textCached(String, [lex, Word|_]), Words), sort(Words, Words9), !, cmember(Word, Words9).
*/


abbreviationForLexicalWord(S, Pos, Word):-cycQueryV([Pos, Word], abbreviationForLexicalWord(Word, Pos, string(S))).


%getPos(W, Pos, Word):-cycQuery(partOfSpeech(Word, Pos, string(W)), 'GeneralEnglishMt').
%getPos(W, Pred, Word):-cycQueryV([Pred, Word], and(speechPartPreds(Pos, Pred), [Pred, Word, string(W)])).
%      sformat(S, '(remove-duplicates (ask-template \'(?Pos  ?Word) \'(#$and (#$speechPartPreds ?Pos ?Pred)(?Pred ?Word "~w")) #$EverythingPSC) #\'TREE-EQUAL)', [W]),
%      evalSubL(S, R:_).

makePosInfo(String):-getPos(String, Pos, Word), saveText(String, [lex, Word, Pos]), fail.
makePosInfo(String):-!.

%posInfo([W], RR):-atom_concat(NW, '\'s', W), !, wordAllInfoRestart(NW, R), append(R, [[pos, possr_s]], RR).
posInfo(String, [[lex, 'UNKNOWN-Word', 'UNKNOWN-Pos']]):-!.

posInfo2(String, RESFLAT):-findall([Word, Pos], getPos(String, Pos, Word), RES),
         findall(Word, cmember([Word, Pos], RES), WRODS), sort(WRODS, WORDS),
         posInfo(WORDS, RES, RESFLAT), !.

posInfo([], RES, []):-!.
posInfo([W|WORDS], RES, [[lex, W|POSESS]|RESFLAT]):-findall(Pos, cmember([_Word, Pos], RES), POSES), =(POSES, POSESS),
      posInfo(WORDS, RES, RESFLAT).


cycAllIsa([Fort], COLS):-!, cycAllIsa(Fort, COLS).
cycAllIsa(nart(Fort), COLS):-!, cycAllIsa(Fort, COLS).
cycAllIsa(Fort, COLS):-number(Fort), !, findall(Type, numberTypes(Fort, Type), COLS), !.
cycAllIsa(Fort, COLS):-copy_term(Fort, FFort), numbervars(FFort, 0, _), termCyclify(Fort, CycL),
  findall(MEMBER, cycQuery('#$or'(nearestIsa(CycL, MEMBER), and(isa(CycL, MEMBER), memberOfList(MEMBER, ['TheList',
  'StuffType', 'TemporalObjectType', 'QuantityType', 'GameTypeExceptions',
    'SpatialThing-Localized', 'ClarifyingCollectionType', 'Collection', 'Individual', 'Event',
      'TemporalStuffType', 'DurativeEventType', 'SituationPredicate', 'ObjectPredicate',
    'TruthFunction', 'BinaryRelation', 'UnaryRelation', 'TernaryRelation', 'ObjectPredicate', 'Function-Denotational',
    'CollectionType', 'FacetingCollectionType', 'Agent-Generic'])))), COLS).

%cycAllIsa(Fort, COLS):-is_list(Fort), termCyclify(Fort, CycL), cycQueryA(isa(CycL, List)), List, _), reformatStrings(List, COLS).
%cycAllIsa(Fort, COLS):-termCyclify(Fort, CycL), evalSubL('ALL-ISA'((CycL), '#$InferencePSC'), List, _), reformatStrings(List, COLS).
numberTypes(N, 'Integer'):-integer(N).
numberTypes(N, 'Numeral'):-integer(N), N>=0, N<10.
numberTypes(N, 'RealNumber'):-not(integer(N)).
numberTypes(N, 'NegativeNumber'):-N<0.
numberTypes(N, 'PositiveNumber'):-N>0.
numberTypes(N, 'Number-General').

collectionInfo([], []).
collectionInfo([COL|INFO], [[denotation, Pos, COL|COLINFO]|MORE]):-
      cycAllIsa(COL, COLINFO),
      collectionInfo(INFO, MORE).

% ?- A is rationalize(0.999999999999999), rational(A).
% A = 1000799917193442 rdiv 1000799917193443



subcatFrame(Word, Pos, INT, CAT):-cycQueryA(subcatFrame(Word, Pos, INT, CAT)).

vetodenoteMapper('is', '#$Israel').



theSTemplate(TempType) --> dcgTemplate(TempType, _VarName, _CycL).

theVar(VarName, Subj) --> [_|_], theVar(VarName, Subj), {true}.
theVar(VarName, Subj) --> [].


tellall(X, W):- X, numbervars(X, 0, _), (format('~q.~n', [W])), fail.
tellall(_X, _W):-flush_output.


argName(string(N), Name):-!, atom_concat(':ARG', N, Name).
argName(N, Name):-atom_concat(':ARG', N, Name).
makeArgNameLen(0, []):-!.
makeArgNameLen(N, ArgsS):- N2 is N-1, makeArgNameLen(N2, Args), argName(N, Named), append(Args, [Named], ArgsS).


/*
(genFormat nearestTransitiveNeighbor "~a nearest generalization by transitivity of ~a is ~a"
       (TheList
           (TheList 2 :EQUALS :POSSESSIVE)
           (TheList 1 :EQUALS)
           (TheList 3 :EQUALS)))
*/
    %tell(pp), rehold, told.
rehold:-between(1, 12, A), functor(P, ac, A),
         catch(P, _, fail), once((litRefactor(P, PP), fmt('~q.~n', [PP]))), fail.
rehold.

%getWordTokens(string([X|Y]), [X|Y]):-atom(X), !.
%getWordTokens([X|Y], [X|Y]):-atom(X), !.
%getWordTokens(X, Y):-getCycLTokens(X, Y), !.


% =======================================================
% sentence constituent breaker
% =======================================================
%sentenceChunker(Ws, []):-!.

%linkParse(String, Fourth, P:M):-evalSubL('link-parse'(string(String)), [[P, M, _, string(FourthS)]], _), getSurfaceFromChars(FourthS, Fourth, _).
linkParse(A, XX, B):- linkParse0(A, B, string(Y)), concat_atom(['', S, ''], '"', Y), getSurfaceFromChars(S, XX, YY).
linkParse0(String, FourthS, Fourth):-evalSubL('link-parse'(string(String)), FourthS, _),
      append(_, [string(FourthAtom)], FourthS), getSurfaceFromChars(FourthAtom, Fourth, _).
      %linkParse(Text, Info, _), !,

sentenceChunker([], []).
sentenceChunker([W|Ws], [New|Out]):-
      notrace(isTagChunk(W, Constit)),
      gatherChunk(Constit, Ws, NW, Rest),
      createChunk(Constit, [W|NW], New),
      sentenceChunker(Rest, Out).

createChunk(Constit, WNW, New):-
      notrace(getLoc(WNW, Loc)),
      notrace(getText(WNW, Text)),
      Constit=..List,
      append([seg|List], [Text, WNW], Out), !,
      New=..Out.

chunkHead(W, Type, RA):-cmember([txt|Text], W), cmember(RA:NGE, W), cmember(1.0-Type, W), !.

gatherChunk(Constit, [], [], []):-!.
gatherChunk(Constit, [W|Ws], [W|SWords], Rest):-
      notrace(isTagChunk(W, New)),
      chunkCompat(Constit, New), !,
      gatherChunk(Constit, Ws, SWords, Rest), !.
gatherChunk(Constit, Rest, [], Rest).

chunkCompat(Start, End):-functor(Start, C, _), functor(End, C, _).

%isTagChunk(W, np('IN')):-getText(W, [of]).
%isTagChunk(W, np('IN')):-getText(W, [for]).
isTagChunk(W, np('PRONOUN')):-getText(W, ['I']).
isTagChunk(W, in('IN')):-isTag(W, 'Preposition'), !.
isTagChunk(W, vp('Modal')):-isTag(W, 'Modal'), !.
isTagChunk(W, vp('AuxVerb')):-isTag(W, 'AuxVerb'), !.
isTagChunk(W, vp('AuxVerb')):-isTag(W, 'BeAux'), !.
isTagChunk(W, vp('AuxVerb')):-isTag(W, 'HaveAux'), !.
isTagChunk(W, wh('WHDeterminer')):-isTag(W, 'WHDeterminer'), !.
isTagChunk(W, wh('WHAdverb, ')):-isTag(W, 'WHAdverb'), !.
isTagChunk(W, cc('CoordinatingConjunction')):-isTag(W, 'cc'), !.
isTagChunk(W, cc('CoordinatingConjunction')):-isTag(W, 'CoordinatingConjunction'), !.
isTagChunk(W, cc('SubordinatingConjunction')):-isTag(W, 'SubordinatingConjunction'), !.
isTagChunk(W, np('POSS')):-isTag(W, 'Possessive'), !.
isTagChunk(W, np('POSS')):-isTag(W, 'PossessivePronoun'), !.
isTagChunk(W, np('PRONOUN')):-isTag(W, 'Pronoun'), !.
isTagChunk(W, np('PROPER')):-isTag(W, 'ProperNoun'), !.
isTagChunk(W, np('DET')):-isTag(W, 'Determiner'), !.
isTagChunk(W, np('Adjective')):-isTag(W, 'Adjective'), !.
isTagChunk(W, np('QUANT')):-isTag(W, 'Quantifier'), !.
isTagChunk(W, vp('Verb')):-isTag(W, 'Verb'), !.
isTagChunk(W, sym('Interjection-SpeechPart')):-isTag(W, 'Interjection-SpeechPart'), !.
isTagChunk(W, sym('SYM')):-isTag(W, '.'), !.
isTagChunk(W, sym('SYM')):-isTag(W, '?'), !.
isTagChunk(W, sym('SYM')):-isTag(W, '!'), !.
isTagChunk(W, sym('SYM')):-isTag(W, '.'), !.
isTagChunk(W, np('COMMON')):-isTag(W, 'nn'), !.
isTagChunk(W, vp('Adverb')):-isTag(W, 'Adverb'), !.
isTagChunk(W, np('OTHER')):-!.

%sentenceParse(Event, Types, Joined):-joinTypes(Types, Joined).

%joinTypes(Types, Joined):-     %most of the features i keep off not to violate copywrites .. since the system was designed arround closed src software i had to emulate.. but i hate writting docs which makes me code arround other peoples manuals



/*
TODO These two forms are totally identical?

Mt : EnglishParaphraseMt
(genFormat-Precise hasStoredInside "~a is stored in ~a when not in use"
       (TheList 2 1))
genTemplate :  (NPIsXP-NLSentenceFn
   (TermParaphraseFn-NP :ARG2)
   (ConcatenatePhrasesFn
       (BestNLPhraseOfStringFn "stored")
       (BestPPFn In-TheWord
           (TermParaphraseFn-NP :ARG1))))


*/

% =======================================================
% Mining genFormat
% =======================================================

/*


 S = '(#$headMedialString "the head-medial string containing ~a, ~a, and ~a is ~a and denotes ~a" (#$TheList 1 (#$TheList 2 :|equals|) 3 (#$TheList 4 :A-THE-WORD) (#$TheList 5 :|equals|)))',
 getWordTokens(S, W).




 atom_codes("(#$headMedialString \"the head-medial string containing ~a, ~a, and ~a is ~a and denotes ~a\" (#$TheList 1 (#$TheList 2 :|equals|) 3 (#$TheList 4 :A-THE-WORD) (#$TheList 5 :|equals|)))", C), getSurfaceFromChars(C, F, A).

 atom_codes("(#$headMedialString \"th ~a\" )", C), tokenize3(C, Ts).


 OLD

genFormatH(W1, W2, Reln, [W1, '~a', W2|REst], ArgListL):-cmember(GenFormat, ['genFormat-Precise', 'genFormat']), holdsWithQuery(GenFormat, Reln, String, ArgListL), stringUnify(String, [W1, '~a', W2|REst]).
genFormatH(W1, W2, Reln, ['~a', W1, W2|REst], ArgListL):-cmember(GenFormat, ['genFormat-Precise', 'genFormat']), holdsWithQuery(GenFormat, Reln, String, ArgListL), stringUnify(String, ['~a', W1, W2|REst]).
genFormatH(W1, W2, Reln, [W1, W2|REst], ArgListL):-cmember(GenFormat, ['genFormat-Precise', 'genFormat']), holdsWithQuery(GenFormat, Reln, String, ArgListL), stringUnify(String, [W1, W2|REst]).

genFormatU([W1, W2], DCG, [Reln|Args]):-
        genFormatH(W1, W2, Reln, DCGPre, ArgListLC), notrace(cyc:decyclify(ArgListLC, ArgListL)), !,
         reformList(ArgListL, ArgList),
        % noteFmt('~nArgList: ~q -> ~q:~q ~n', [Reln, DCGPre, ArgList]),
         once(dcgRedressGenFormat(0, DCGPre, ArgList, DCG, Vars)), Vars=Vars,
         %catch((once(dcgRedressGenFormat(1, DCGPre, ArgList, DCG, Vars)), sort(Vars, VarsS)), E, noteFmt('Error: ~q ~n', [DCGPre])),
         once(((getArity(Reln, Arity);length(ArgList, Arity)), number(Arity), makeArgNameLen(Arity, Args))).
*/

genFormatH(Reln, [S|Plit], ArgListL):-cmember(GenFormat, ['genFormat-Precise', 'genFormat']),
     holdsWithQuery(GenFormat, Reln, String, ArgListL),
      nonvar(String), %%writeq(holdsWithQuery(GenFormat, Reln, String, ArgListL)), nl,
     throwOnFailure(nonvar(String)),
     once(notrace(stringUnify(String, [S|Plit]))), ignore((cmember('~A', [S|Plit]), trace, writeq(String), stringUnify(String, [_S|_Plit]))).

genFormatU(DCG, [Reln|Args]):-
        genFormatH(Reln, DCGPre, ArgListL),
         once(reformList(ArgListL, ArgList)),
        % noteFmt('~nArgList: ~q -> ~q:~q ~n', [Reln, DCGPre, ArgList]),
         once(dcgRedressGenFormat(0, DCGPre, ArgList, DCG, Vars)), Vars=Vars,
         %catch((once(dcgRedressGenFormat(1, DCGPre, ArgList, DCG, Vars)), sort(Vars, VarsS)), E, noteFmt('Error: ~q ~n', [DCGPre])),
         once(((getArity(Reln, Arity);length(ArgList, Arity)), number(Arity), makeArgNameLen(Arity, Args))).



%dcgRedressGenFormat([DC|GPre], ArgList, DCG, Vars).
replGenFormat(N, varError(N)):-not(ground(N)), !, trace.
replGenFormat([string(['[', ']']), string(['s'])], dcgTrue).
replGenFormat([string(['is']), string(['are'])], dcgTrue).
replGenFormat([string(N)|How], New):-replGenFormat([N|How], New).
replGenFormat([N|How], theGen(Name, How)):-argName(N, Name). %, unSvar(SHow, How).
replGenFormat(N, theGen(Name, [])):-argName(N, Name). %, unSvar(SHow, How).
replGenFormat(N, N):-trace.

% utils

reformList(X, X):-var(X), !.
reformList('TheEmptyList', []):-!.
reformList(X, X):-not(compound(X)), !.
reformList([TheList|ArgList], X):- TheList=='TheList', !, reformList(ArgList, X), !.
reformList([H|T], [HH|TT]):-!, reformList(H, HH), !, reformList(T, TT), !.
reformList(svar(_, B), B):-!.
reformList(B, A):-functor(B, 'TheList', _), B=..BL, reformList(BL, A), !.
reformList(B, A):-B=..BL, reformList(BL, AL), A=..AL, !.


cleanForA(S, [], S):-!.
cleanForA(S, [B-A], SA):-concat_atom(List, B, S), concat_atom(List, A, SA), !.
cleanForA(S, [B-A|More], SA1):-
  cleanForA(S, [B-A], SA), cleanForA(SA, More, SA1), !.

stringUnify(S, S):-var(S), trace, !, fail.
stringUnify(S, SU):-var(SU), S=SU, !.
stringUnify([S|H], U):-concat_atom([S|H], ' ', A), !, stringUnify(A, U), !.
stringUnify(string(S), U):-!, stringUnify(S, U), !.
stringUnify(S, U):-atom(S), atom_concat('"', Right, S), atom_concat(New, '"', Right), !, stringUnify(New, U), !.
stringUnify(S, U):-string(S), !, string_to_atom(S, A), !, stringUnify(A, U), !.
stringUnify(S, U):-not(atom(S)), !, trace, fail.
stringUnify(S, U):-cleanForA(S, [
   '<br>'-' ',
      '\n'-' ',
      '\t'-' ',
      '~A'-'~a',
      '~a'-' ~a ',
      (', ')-' , ',
      ('.')-' . ',
      (':')-' : ',
      ('!')-' ! ',
                            '('-' ( ',
                            ')'-' ) ',
                            '['-' [ ',
                            ']'-' ] ',
                            '{'-' { ',
                            ('}')-(' } '),
                            ('\`')-(' \` '),
                            ('$')-' $ ',
                            '%'-' % ',
                            '"'-' " ',
                            '?'-' ? ',
                            '\''-' \' ',
                            % this one breaks it? bug in concat_atom '\\'-' \\ ',
   '  '-' ',
   '  '-' ',
   '# $ '-'#$'

   ], SA), concat_atom(U, ' ', SA), !.
stringUnify(S, U):-concat_atom(U, ' ', S), !.


dcgRedressGenFormat(N, A2, ArgList, DCG, varError(N)):-not(ground(ArgList:A2)), !, trace.
dcgRedressGenFormat(N, ['~a'|DCGPre], [How|ArgList], [D|DCG], Vars):-replGenFormat(How, D), N2 is N+1, dcgRedressGenFormat(N2, DCGPre, ArgList, DCG, Vars).
dcgRedressGenFormat(N, ['~a'|DCGPre], ArgList, [theGen(SVAR, [])|DCG], Vars):-N2 is N+1, argName(N2, SVAR), dcgRedressGenFormat(N2, DCGPre, ArgList, DCG, Vars).
%dcgRedressGenFormat(N, ['~a'|DCGPre], [['TheList', Num, Type]|ArgList], [repl(SVAR, Type)|DCG], Vars):-argName(Num, SVAR), dcgRedressGenFormat(N, DCGPre, ArgList, DCG, Vars).
dcgRedressGenFormat(N, [W|DCGPre], ArgList, [theGText([W])|DCG], Vars):-dcgRedressGenFormat(N, DCGPre, ArgList, DCG, Vars).
dcgRedressGenFormat(N, DCG, ArgList, DCG, Vars).

%writeq(ac(comment, 'ScientificNumberFn', string(['(', '#$ScientificNumberFn', 'SIGNIFICAND', 'EXPONENT', ')', denotes, a, number, in, scientific, notation, which, has, 'SIGNIFICAND', with, an, implicit, decimal, point, after, its, first, digit, as, its, significand, and, 'EXPONENT', as, its, exponent, '.', 'So', (', '), e, '.', g, '.', (', '), '(', '#$ScientificNumberFn', 314159, 0, ')', denotes, '3.14159.', 'Likewise', (', '), '(', '#$ScientificNumberFn', 23648762469238472354, 32, ')', denotes, 2.36487624692e+032, or, 2.36487624692, *, 10, ^, 32, '.']))).

genTemplateU(Reln, DCGPre, [Reln|Args]):-
         holdsWithQuery(genTemplate, Reln, DCGPre),
         once((getArity(Reln, Arity), number(Arity), makeArgNameLen(Arity, Args))).

/*
%todo
getGenFormat(PredType, _Template, ARGS):-
   holdsWithQuery(genFormat, Pred, string(Str), How), %getPredType(Pred, PredType, Arity),
   genArgList(1, PredType, Arity, How, Str, StrFmt, ARGS).

%genArgList(N, How, Str, StrFmt, ARGS):-
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

getPredType(Pred, 'Predicate', 2):-holdsWithQuery(isa, Pred, 'BinaryPredicate'), !.
getPredType(Pred, 'Function', 2):-holdsWithQuery(isa, Pred, 'BinaryFunction'), !.
getPredType(Pred, 'Function', A):-atom(Pred), atom_codes(Pred, [C|_]), is_upper(C), getArity(Pred, A), number(A), !.
getPredType(Pred, 'Predicate', A):-atom(Pred), atom_codes(Pred, [C|_]), is_lower(C), getArity(Pred, A), number(A), !.

getArity(Pred, A):-holdsWithQuery(arity, Pred, A), (number(A) -> ! ; true).
getArity(Pred, Out):-holdsWithQuery(arityMin, Pred, A), (number(A) -> (! , Out is A + 0 ); Out=A).
getArity(Pred, Out):-(noteFmt('CANT get arity of ~q ~n', [Pred])), !, atom(Pred), Out=1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
holdsWithQuery(Reln, DCGPre, CycLPre):-Reln == expansion, catch(cycQuery([Reln, DCGPre, CycLPre], 'EverythingPSC'), _, fail).
holdsWithQuery(Reln, DCGPre, CycLPre):-ac(Reln, DCGPre, CycLPre).
holdsWithQuery(Reln, DCGPre, CycLPre):-Reln \== expansion, catch(cycQuery([Reln, DCGPre, CycLPre], 'EverythingPSC'), _, fail).
holdsWithQuery(Reln, TempType, DCGPre, CycLPre):-ac(Reln, TempType, DCGPre, CycLPre).
holdsWithQuery(Reln, TempType, DCGPre, CycLPre):-cycQuery([Reln, TempType, DCGPre, CycLPre], 'EverythingPSC').
holdsWithQuery(Reln, TempType, Name, DCGPre, CycLPre):-ac(Reln, TempType, Name, DCGPre, CycLPre).
holdsWithQuery(Reln, TempType, Name, DCGPre, CycLPre):-cycQuery([Reln, TempType, Name, DCGPre, CycLPre], 'EverythingPSC').
holdsWithQuery(Reln, TempType, Name, DCGPre, CycLPre, Test):-ac(Reln, TempType, Name, DCGPre, CycLPre, Test).
holdsWithQuery(Reln, TempType, Name, DCGPre, CycLPre, Test):-cycQuery([Reln, TempType, Name, DCGPre, CycLPre, Test], 'EverythingPSC').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

% =======================================================
% Mine out Cyc idiomatic DCGs
% =======================================================

:-dynamic(cache_dcgRedressStart/4).

list_dcgRedressStart :- cache_dcgRedressStart(Reln, DCGPre, DCG, Vars),
      numbervars(cache_dcgRedressStart(Reln, DCGPre, DCG, Vars), 0, _),
      noteFmt('?- ~q. ~n ', [dcgRedressStart(Reln, DCGPre, DCG, Vars)]), fail.
list_dcgRedressStart :- retractall(cache_dcgRedressStart(_, _, _, _)).

dcgRedressStart(Reln, DCGPre, DCG, VarsO):-
      nonvar(DCGPre),
      asserta(cache_dcgRedressStart(Reln, DCGPre, DCG, Vars)),
      catch((dcgRedress(DCGPre, DCG, Vars), sort(Vars, VarsO)), E,
               (printAndThrow('Error: ~q in ~q/~q ~q ~n', [E, DCG, Vars, DCGPre]), list_dcgRedressStart, trace)),
      retractall(cache_dcgRedressStart(_, _, _, _)).

% todo fix ac('genTemplate-Constrained', subsetOf, [and, [isa, svar(_G276, ':ARG1'), 'Collection'], [isa, svar(_G291, ':ARG2'), 'Collection']], ['NPIsNP-NLSentenceFn', ['PhraseFormFn', singular, ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G276, ':ARG1')]]], ['IndefiniteNounPPFn', 'Subset-TheWord', string([of]), ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G291, ':ARG2')]]]]).
dcgt :- dcgRedress( ['NPIsNP-NLSentenceFn', ['PhraseFormFn', singular, ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G276, ':ARG1')]]], ['IndefiniteNounPPFn', 'Subset-TheWord', string([of]), ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G291, ':ARG2')]]]], DCG, Vars).
dcgt(DCG, Vars) :- dcgRedress( ['NPIsNP-NLSentenceFn', ['PhraseFormFn', singular, ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G276, ':ARG1')]]], ['IndefiniteNounPPFn', 'Subset-TheWord', string([of]), ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G291, ':ARG2')]]]], DCG, Vars).

mustNonvar(Vars, Call):-call(Call), once(nonvar(Vars);(noteFmt('mustNonvar ~q~n', [Call]), flush, trace)).

appliedTemplate_mine('GenerationPhrase', Reln, DCG, CycL9):-
         genFormatU(DCGPre, [Reln|Args]),
         dcgRedressStart([Reln|Args], DCGPre, DCG, VarsS),
         once(cyclRedress([Reln|Args], CycL)), substEach(CycL, VarsS, CycL9).

appliedTemplate_mine(TempType, Reln, DCG, CycL9):-
         cmember(Reln, ['assertTemplate', 'rewriteTemplate', 'termTemplate', 'queryTemplate', 'commandTemplate']),
         holdsWithQuery(Reln, TempType, DCGPre, CycLPre),
         dcgRedressStart(CycLPre, DCGPre, DCG, VarsS),
         once(cyclRedress(CycLPre, CycL)), substEach(CycL, VarsS, CycL9).

appliedTemplate_mine(TempType, Reln:Name, DCG, CycL9):-
         cmember(Reln, ['assertTemplate-Reln', 'metaStatementTemplate-Reln', 'termTemplate-Reln', 'queryTemplate-Reln', 'commandTemplate-Reln']),
         holdsWithQuery(Reln, TempType, Name, DCGPre, CycLPre),
         dcgRedressStart(Name-CycLPre, DCGPre, DCG, VarsS),
         once(cyclRedress(CycLPre, CycL)), substEach(CycL, VarsS, CycL9).

appliedTemplate_mine(TempType, Reln:Name, DCG, CycL9):-
         cmember(Reln, ['assertTemplate-Test', 'termTemplate-Test', 'queryTemplate-Test']),
         holdsWithQuery(Reln, TempType, Name, DCGPre, CycLPre, Test),
         dcgRedressStart(implies(Test, CycLPre), DCGPre, DCG, VarsS),
         once(cyclRedress(implies(Test, CycLPre), CycL)), substEach(CycL, VarsS, CycL9).

appliedTemplate_mine('GenerationTemplateConstrained', Reln, DCG, CycL9):-
         holdsWithQuery('genTemplate-Constrained', Reln, Constr, DCGPre), nonvar(DCGPre),
         once((getArity(Reln, Arity), number(Arity), makeArgNameLen(Arity, Args))),
         dcgRedressStart([Reln|Args], DCGPre, DCG, VarsS),
         once(cyclRedress(implies(Constr, [Reln|Args]), CycL)), substEach(CycL, VarsS, CycL9).

appliedTemplate_mine('GenerationTemplate', Reln, DCG, CycL9):-
         genTemplateU(Reln, DCGPre, [Reln|Args]), nonvar(DCGPre), CycLPre=[Reln|Args],
         dcgRedressStart([Reln|Args], DCGPre, DCG, VarsS),
         once(cyclRedress(CycLPre, CycL)), substEach(CycL, VarsS, CycL9).


tl:-told,
  tell('dcgTermTemplate.pl'),
  tellall(appliedTemplate_mine(TempType, Reln, DCG, CycL9), appliedTemplate(TempType, Reln, DCG, CycL9)), told.

:- if(exists_source(dcgTermTemplate)).
:- ensure_loaded(dcgTermTemplate).
:- endif.
:- if(exists_source(dcgTermTemplate2)).
:- ensure_loaded(dcgTermTemplate2).
:- endif.

isCycKeyword(KW):-atom(KW), atom_concat(':', _, KW).
getKeyVariable(KW, KW):-isCycKeyword(KW), !.
getKeyVariable(KW, KW):-atom(KW), !, atom_concat('?', _, KW), !.
getKeyVariable(SVAR, KW):-nonvar(SVAR), SVAR = svar(_, KW), !.
getKeyVariable(SVAR, KW):-nonvar(SVAR), !, SVAR = [svar, _, KW].
getKeyVariable(SVAR, KW):-sformat(S, ':VAR~w', [SVAR]), string_to_atom(S, KW), !, noteFmt(';; ~q.~n', [getKeyVariable(SVAR, KW)]), list_dcgRedressStart. %, ignore(KW=SVAR).

cyclRedress(V, V):-var(V), !.
cyclRedress(svar(_, VarName), VarName):-!.
cyclRedress([Cyc|L], [CycO|LO]):-cyclRedress(Cyc, CycO), cyclRedress(L, LO), !.
cyclRedress(CycL, CycL9):-compound(CycL), !, CycL=..[Cyc|L], cyclRedress([Cyc|L], CycLL), CycL9=..CycLL, !.
cyclRedress(CycL, CycL):-!.


juntifyList(Pred, List, Res):-!, delete(List, theGText(['']), List1), delete(List1, theGText(''), List2), juntifyList0(Pred, List2, Res), !.

juntifyList0(Pred, [], Pred):-!.
juntifyList0(dcgSeq, [List], List):-!.
%juntifyList0(Pred, [List], Res):-!, Res=..[Pred, List], !.
juntifyList0(Pred, [H|List], Res):-length([H|List], Len), Res=..[Pred, Len, [H|List]], !.
juntifyList0(Pred, [H|List], Res):-juntifyList0(Pred, List, PRES), Res=..[Pred, H, PRES], !.


dcgRedressJunctify(DcgSeq, List, DCG, Vars):- throwOnFailure((dcgRedressL(List, RList, Vars), juntifyList(DcgSeq, RList, DCG))),
      throwOnFailure(nonvar(DCG), dcgRedressJunctify(DcgSeq, List, DCG, Vars)), !.

dcgRedressL(I, O, V):-debugOnFailure(dcgRedressL0(I, O, V)), throwOnFailure(nonvar(O), dcgRedressJunctify(DcgSeq, List, DCG, Vars)), !.
dcgRedressL0([], [], []):-!.
dcgRedressL0([H|T], HHTT, Vars):-dcgRedressSafe(H, HH, Var1), !, dcgRedressL0(T, TT, Var2), append(Var1, Var2, Vars), !, HHTT=[HH|TT].

dcgRedressSafe(H, HH, Var1):-throwOnFailure((dcgRedress(H, HH, Var1))), throwOnFailure((nonvar(HH), nonvar(Var1)), dcgRedressSafe(H, HH, Var1)).

% dcgRedress(A, B, C):-trace, throw(dcgRedress(A, B, C)).


dcgRedress(V, theVar(KW, V), []):-var(V), !, getKeyVariable(V, KW), !, list_dcgRedressStart.
dcgRedress(Var, A, B):-var(Var), trace, !, printAndThrow('dcgRedress got var in arg1 ~q', [dcgRedress(Var, A, B)]).
dcgRedress(A, NonVar, B):-nonvar(NonVar), trace, !, printAndThrow('dcgRedress got nonvar in arg2 ~q ', [dcgRedress(A, NonVar, B)]).
dcgRedress(A, B, NonVar):-nonvar(NonVar), trace, !, printAndThrow('dcgRedress got nonvar in arg3 ~q ', [dcgRedress(A, B, NonVar)]).
dcgRedress(V, varError(V), []):-var(V), !, trace, !. %, throw(error(dcgRedress/3, 'Arguments are not sufficiently instantiated'))


dcgRedress(X, S, Vars):-string(X), string_to_atom(X, A), dcgRedress_string(A, S, Vars).
dcgRedress(DCG, theGText(DCG), []):-number(DCG), !.

dcgRedress([Var|Rest], varError([Var|Rest]), []):-
         var(Var),
         noteFmt('Var in head of list: ~w~n', [[Var|Rest]]),
         list_dcgRedressStart, trace.

dcgRedress(string(S), A, Vars):-!, dcgRedress_string(S, A, Vars).
dcgRedress([string, S], A, Vars):-!, dcgRedress_string(S, A, Vars).

   dcgRedress_string((string(String)), DCG, Var):-!, dcgRedress_string(String, DCG, Var).
   dcgRedress_string([string, String], DCG, Var):-!, dcgRedress_string(String, DCG, Var).
   dcgRedress_string([string|String], DCG, Var):-!, dcgRedress_string(String, DCG, Var).
   dcgRedress_string(X, S, Vars):-string(X), string_to_atom(X, A), dcgRedress_string(A, S, Vars).
   dcgRedress_string(([DCG]), theGText(DCG), []):-!.
   dcgRedress_string((""), dcgNone, []):-!.
   dcgRedress_string((''), dcgNone, []):-!.
   dcgRedress_string((String), DCG, Vars):-atom(String), atom_concat('"', Right, String), atom_concat(New, '"', Right), dcgRedress_string(New, DCG, Vars), !.
   dcgRedress_string((String), DCG, Vars):-atom(String), concat_atom([H, T|List], ' ', String), !, dcgRedress_string([H, T|List], DCG, Vars), !.
   dcgRedress_string((String), theGText(A), []):-atom(String), cleanEachAtomList(String, A), !, (atom(A)->throwOnFailure(atom_codes(A, [_|_]));throwOnFailure(A=[_|_])).
   dcgRedress_string(([D|CG]), theText(DCG), []):-throwOnFailure(cleanEachAtomList([D|CG], DCG)), noteFmt('~q.~n', theText(DCG)), !, list_dcgRedressStart.

dcgRedress(D, DCG, Vars):-string(D), !, dcgRedress(string(D), DCG, Vars), !.
dcgRedress(String, DCG, Var):-atom(String), atom_concat('"', _, String), !, dcgRedress(string(String), DCG, Var), !.
dcgRedress(TemplateMarker, DCG, Vars):-templateMarkerRepresentation(TemplateMarker, Char), !, dcgRedress(string(Char), DCG, Vars), !.
dcgRedress([TemplateMarker], DCG, Vars):-templateMarkerRepresentation(TemplateMarker, Char), !, dcgRedress(string(Char), DCG, Vars), !.


cleanEachAtomList(B, AA):-cleanEachAtom(B, A), (is_list(A)->delete(A, '', AA);AA=A).

cleanEachAtom([], []):-!.
cleanEachAtom([svar|S], [svar|S]):-!, trace.
cleanEachAtom(['\'', OneTwo|CG], ['\''|CG2]):-atom(OneTwo), atom_length(OneTwo, N), N>0, N<3, !,
   cleanEachAtom([OneTwo|CG], CG2), !.
cleanEachAtom([D|CG], CG2):- cleanEachAtom(D, D2), D2='\'', !, cleanEachAtomList(CG, CG2), !.
cleanEachAtom([D|CG], [D2|CG2]):-!,
   cleanEachAtom(D, D2), !,
   cleanEachAtomList(CG, CG2), !.
cleanEachAtom(D, A):-string(D), string_to_atom(D, M), !, cleanEachAtom(M, A), !.
cleanEachAtom(D, A):-not(atom(D)), !, (D=A->true;trace).
cleanEachAtom(D, D):-atom_codes(D, [_]), !.
cleanEachAtom(D, A):-atom_concat('\\"', M, D), M\='', cleanEachAtom(M, A), !.
cleanEachAtom(D, A):-atom_concat(M, '\\"', D), M\='', cleanEachAtom(M, A), !.
cleanEachAtom(D, A):-atom_concat(M, '\\', D), cleanEachAtom(M, A), !.
cleanEachAtom(D, A):-atom_concat('\\', M, D), cleanEachAtom(M, A), !.
cleanEachAtom(D, A):-atom_concat('\'', M, D), cleanEachAtom(M, A), !.
cleanEachAtom(D, A):-atom_concat(M, '\'', D), cleanEachAtom(M, A), !.
cleanEachAtom(D, A):-atom_concat('"', M, D), cleanEachAtom(M, A), !.
cleanEachAtom(D, A):-atom_concat(M, '"', D), cleanEachAtom(M, A), !.
cleanEachAtom(A, A).

dcgRedress(['theGText'([Word])], DCG, Vars):-dcgRedress_string(Word, DCG, Vars), !.
dcgRedress(['theGText'(Word)], DCG, Vars):-dcgRedress_string(Word, DCG, Vars), !.
dcgRedress(['theGText'|Word], DCG, Vars):-dcgRedress_string(Word, DCG, Vars), !.
dcgRedress(['theGen', VarName, []], dcgTemplate('AnyTemplate', VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.
dcgRedress(['theGen', VarName, Details], dcgTemplateKW(Details, VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.

dcgRedress(svar(Subj, VarName), theVar(VarName, Subj), [VarName-Subj]):-!, noteFmt('~q~n', [theVar(VarName, Subj)]), list_dcgRedressStart.

dcgRedress(['OptionalSome'|List], dcgOptionalSome(RList), Vars):-dcgRedressL(List, RList, Vars), !.
dcgRedress(['OPTIONALSOME'|List], dcgOptionalSome(RList), Vars):-dcgRedressL(List, RList, Vars), !.
dcgRedress(['OptionalOne'|List], dcgOptionalOne(RList), Vars):-dcgRedressL(List, RList, Vars), !.
dcgRedress(['NLPatternList'|List], DCG, Vars):-!, dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.
dcgRedress(['NLPattern-Exact'|List], DCG, Vars):-!, dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.
dcgRedress(['RequireOne'|List], DCG, Vars):-dcgRedressJunctify(dcgOr, List, DCG, Vars), !.
dcgRedress(['RequireSome'|List], DCG, Vars):-dcgRedressJunctify(dcgRequireSome, List, DCG, Vars), !.
dcgRedress(['WordSequence'], dcgNone, []):-!.
dcgRedress('WordSequence', dcgNone, []):-!.
dcgRedress(['WordSequence'|List], DCG, Vars):-dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.
dcgRedress(['NLPattern-Word', Word, Pos], thePOS(Word, Pos), []):-!.
dcgRedress(['NLPattern-Term', Term, Pos], dcgAnd(thePOS(Pos), theTerm(Term)), []):-!.
dcgRedress(['TermPOSPair', Term, Pos], dcgAnd(thePOS(Pos), theTerm(Term)), []):-!.
dcgRedress(['NLPattern-TermPred', Term, Pos], theTermPred(Term, Pos), []):-!.
dcgRedress(['NLPattern-Template', TempType, VarName], dcgTemplate(TempType, VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.
dcgRedress(['NLPattern-POS', VarName, Pos], dcgPosVar(Pos, VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.
dcgRedress(['NLPattern-Agr', VarName, Pos], dcgPosVar(Pos, VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.

dcgRedress(['WordWithSuffixFn', Word, Suffix], theWord(['WordWithSuffixFn', Word, Suffix]), []):-!.

dcgRedress(['BestNLPhraseOfStringFn', VarName], dcgTemplate('AnyTemplate', VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.
dcgRedress(['BestNLPhraseOfStringFn', VarName], dcgTemplate('AnyTemplate', VarName, Subj), [VarName-Subj]):-var(VarName), trace, !.
dcgRedress(['BestNLPhraseOfStringFn', string(String)], DCG, Var):-!, dcgRedress(string(String), DCG, Var), !.
dcgRedress(['BestNLPhraseOfStringFn', String], DCG, Var):-!, dcgRedress(string(String), DCG, Var), !.
dcgRedress(['BestNLWordFormOfLexemeFn', Word], theWord(Word), []):-!.
dcgRedress(['BestHeadVerbForInitialSubjectFn', Word], theWord(Word), []):-!.
dcgRedress(['HeadWordOfPhraseFn', Word], Out, Vars):-!, dcgRedress(Word, Out, Vars), !.
dcgRedress(['BestSymbolPhraseFn', Word], dcgOr(theWord(Word), theTermStrings(Word)), []):-!.

dcgRedress(['ConcatenatePhrasesFn-NoSpaces'|List], dcgNoSpaces(DCG), Vars):-dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.

dcgRedress(['ConcatenatePhrasesFn'|List], DCG, Vars):-dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.
dcgRedress(['TermParaphraseFn-NP', VarName], dcgTemplate('NPTemplate', VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.

% this makes a new arg
dcgRedress(['TermParaphraseFn-NP', Constr], dcgAnd(dcgTemplate('NPTemplate', VarNameO, Subj), isPOS(Constr)), [VarNameO-VarName]):-getKeyVariable(VarName, VarNameO), !.

dcgRedress(['StringMentionFn', VarName], dcgTemplate('StringTemplate', VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.
dcgRedress(['PercentParaphraseFn', VarName], dcgTemplate('PercentTemplate', VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.
dcgRedress(['TermParaphraseFn-Possessive', VarName], dcgTemplate('PossessiveTemplate', VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.

dcgRedress(['QuotedParaphraseFn'|List], dcgMaybeQuoted(DCG), Vars):-dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.


%% TODO these need more work
dcgRedress(['TermParaphraseFn', VarName], dcgTemplate('AnyTemplate', VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.
dcgRedress(['TermParaphraseFn', [KW|List]], DCG, Vars):-getKeyVariable(KW, _VarNameO), dcgRedressJunctify(dcgSeq, [['TermParaphraseFn', _VarNameO]|List], DCG, Vars), !.
dcgRedress(['TermParaphraseFn'], varError('TermParaphraseFn'), []):- trace, !.
dcgRedress(['TermParaphraseFn' |List], DCG, Vars):- dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.
%dcgRedress(['TermParaphraseFn', VarName], dcgInverse(VarName), []):-!.

dcgRedress(['BestCycLPhraseFn', VarNameO], dcgCycL(Subj, VarNameO), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.

dcgRedress(['TermParaphraseFn-Constrained', Pos, VarName], dcgPosVar(Pos, VarNameO, Subj), [VarNameO-Subj]):-atom( Pos), getKeyVariable(VarName, VarNameO), !.
dcgRedress(['TermParaphraseFn-Constrained', Constr, List], dcgConstraintBindings(DCG, Constr), Vars):-dcgRedress(List, DCG, Vars), !.
dcgRedress(['BestNLWordFormOfLexemeFn-Constrained', Pos, Word], thePOS(Word, Pos), []):-atom( Pos), atom(Word), !.
dcgRedress(['ConditionalPhraseFn', Constr|List], dcgConstraint(DCG, Constr), Vars):-!, dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.

% dcgSeqReinterp
dcgRedress(['BestBindingsPhraseFn', Constr, List], dcgConstraintBindings(DCG, Constr), Vars):-nonvar(List), dcgRedress(List, DCG, Vars), !.

dcgRedress(['BestBindingsPhraseFn'|ConstrList], Foo, Bar):-true, !, ignore(Foo=dcgConstraintBindings(ConstrList)), ignore(Bar=[]).


dcgRedress(['RepeatForSubsequentArgsFn', Num, List], dcgRepeatForSubsequentArgsFn(Num, DCG), Vars):-dcgRedress(List, DCG, Vars), !.

dcgRedress(['BestChemicalFormulaFn', List, Counts], dcgBestChemicalFormulaFn(DCG, Counts), Vars):-dcgRedress(List, DCG, Vars), !.




dcgRedress(VarName, dcgTemplate('AnyTemplate', VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.



dcgRedress(['BestDetNbarFn'|List], DCG, Vars):-!, dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.

dcgRedress(['BestDetNbarFn-Indefinite'|List], dcgPhraseType('BestDetNbarFn-Indefinite', DCG), Vars):-!, dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.
dcgRedress(['BestDetNbarFn-Definite'|List], dcgPhraseType('BestDetNbarFn-Definite', DCG), Vars):-!, dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.


dcgRedress(['TypeClarifyingPhraseFn', NP, COMP], dcgSeq(dcgOptional(dcgOptional(theWord('The-TheWord')), DCG1, dcgOptional(theWord('Of-TheWord'))), DCG2), Vars):-!,
   dcgRedress(NP, DCG1, Vars1), dcgRedress(COMP, DCG2, Vars2), append(Vars1, Vars2, Vars), !.



dcgRedress(['BestPPFn', A, B], dcgSeq(theWord(A, 'Preposition'), DCG2), Vars):-!, dcgRedress(B, DCG2, Vars), !.

dcgRedress(['BestPPFn'|List], DCG, Vars):-!, dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.


dcgRedress(['NPIsXP-NLSentenceFn', NP, COMP], dcgSeq(DCG1, theWord('Be-TheWord'), DCG2), Vars):-!,
   dcgRedress(NP, DCG1, Vars1), dcgRedress(COMP, DCG2, Vars2), append(Vars1, Vars2, Vars), !.

% ['DefiniteNounPPFn', 'Government-TheWord', '"of"', ['TermParaphraseFn-NP', ':ARG1']]
dcgRedress(['DefiniteNounPPFn'|List], dcgPhraseType('DefiniteNounPPFn', DCG), Vars):-!, dcgRedressJunctify(dcgSeq, List, DCG, Vars), !.

dcgRedress(['PhraseFormFn', Pos, Template], dcgAnd(thePOS(Pos), DCG), Vars):-!, dcgRedress(Template, DCG, Vars), !.
dcgRedress(['BestVerbFormForSubjectFn', Word, NOTE], dcgAnd(thePOS(Word, 'Verb'), dcgNote(NOTE)), []):-!.

dcgRedress(['GenTemplateRecipeOmitsArgFn', Arg, Template], dcgAnd(DCG, dcgNote('GenTemplateRecipeOmitsArgFn'(Arg))), Vars):-dcgRedress(Template, DCG, Vars), !.


dcgRedress(['NPTemplate', VarName], dcgTemplate('NPTemplate', VarName, Subj), [VarName-Subj]):-!.
dcgRedress(['PossessiveTemplate', VarName], dcgTemplate('PossessiveTemplate', VarName, Subj), [VarName-Subj]):-!.
dcgRedress(['NBarTemplate', VarName], dcgTemplate('NBarTemplate', VarName, Subj), [VarName-Subj]):-!.
dcgRedress([TemplateType, VarName], dcgTemplate(TemplateType, VarName, Subj), [VarName-Subj]):-
   atom(TemplateType), debugOnError(cycQuery(genls(TemplateType, 'ParsingTemplateCategory'))), !.


dcgRedress(['NLConjunctionFn'|List], DCG, Vars):-!, throwOnFailure((dcgRedressL(List, RList, Vars), juntifyList(dcgConjWithWord, ['And-TheWord', RList], DCG))), !.

dcgRedress(['NLDisjunctionFn'|List], DCG, Vars):-!, throwOnFailure((dcgRedressL(List, RList, Vars), juntifyList(dcgConjWithWord, ['Or-TheWord', RList], DCG))), !.



dcgRedress(THEWORD, theWord(THEWORD), []):-atom(THEWORD), atom_concat(_, '-TheWord', THEWORD), !.


isNLFunction(NL):-not(atom(NL)), !, fail.
isNLFunction(NL):-concat_atom([_, 'NL', _], '', NL), !.
isNLFunction('NPIsNP-NLSentenceFn'):-!.
isNLFunction(NL):-cycQueryIsaCache(NL, 'NLFunction').

:-dynamic(notIsaIC/2).
cycQueryIsaCache(I, C):-notIsaIC(I, C), !, fail.
cycQueryIsaCache(I, C):-catch(holdsWithQuery(isa, I, C), _, fail), !.
cycQueryIsaCache(I, C):-asserta(notIsaIC(I, C)), !, fail.

% Term to List
dcgRedress(DCG, DOIT, Vars):-not(DCG=[_|_]), compound(DCG), pterm_to_sterm(DCG, DCGL), (nonvar(DCGL);trace), !, dcgRedress(DCGL, DOIT, Vars), !.
%dcgRedress(DCG, Out, Vars):-notrace(once(cyc:decyclify(DCG, UDCG))), (DCG\== UDCG), !, throwOnFailure(dcgRedress(UDCG, Out, Vars)), !.


isSomeFn(X):-nonvar(X), cmember(X, ['SomeFn', 'AnyFn', 'GovernmentFn', 'EveryFn', 'ManyFn', 'CollectionSubsetFn', 'FormulaArgFn', 'SpecsFn']).
isSomeFn(X):-atom(X), atom_concat('SubcollectionOf', _, X).
isSomeFn(X):-atom(X), concat_atom(['Collection', _, 'Fn'], '', X).


dcgRedress([SomeFn, Var|Rest], dcgAnd(dcgReverse([SomeFn, VarNameO|Rest]), theVar(Var, Subj)), [VarNameO-Subj]):-isSomeFn(SomeFn), getKeyVariable(Var, VarNameO), !.
dcgRedress([SomeFn|Rest], dcgReverse([SomeFn|Rest]), []):-isSomeFn(SomeFn), !.

dcgRedress(['PhraseCycLFn', Var, Rest], dcgAnd(DCG, theVar(VarNameO, Subj)), [VarNameO-Subj|Vars]):-getKeyVariable(Var, VarNameO), dcgRedress(Rest, DCG, Vars), !.

isLexicalWord(THEWORD):-not(atom(THEWORD)), !, fail.
isLexicalWord([]):-!, fail.
isLexicalWord(THEWORD):-atom_length(THEWORD, N), N<3, !, fail.
isLexicalWord(THEWORD):-atom_concat(_, '-TheWord', THEWORD), !.
isLexicalWord(THEWORD):-atom_concat(_, '-MWS', THEWORD).
isLexicalWord(THEWORD):-atom_concat(_, '-MWW', THEWORD).
isLexicalWord(THEWORD):-cycQueryIsaCache(THEWORD, 'LexicalWord').

dcgRedress([THEWORD, POS], theWord(THEWORD, POS), []):-isLexicalWord(THEWORD), !.
dcgRedress(['svar', P, OS], DCG, Vars):-dcgRedress('svar'(P, OS), DCG, Vars), !.
dcgRedress([THEWORD|POS], dcgReverse([THEWORD|POS]), []):-isLowercaseAtom(THEWORD), noteFmt('dcgReverse: ~q.~n', [[THEWORD|POS]]), list_dcgRedressStart.

isLowercaseAtom(THEWORD):-not(atom(THEWORD)), !, fail.
isLowercaseAtom([]):-!, fail.
isLowercaseAtom(THEWORD):-atom_codes(THEWORD, [C|_]), !, char_type(C, lower).

dcgRedress(DCG, Out, Vars):-notrace(once(cyc:decyclify(DCG, UDCG))), (DCG\== UDCG), !, throwOnFailure(dcgRedress(UDCG, Out, Vars)), !.

dcgRedress([TerseParaphraseFn, Phrase], dcgAnd(DCG, dcgNote(TerseParaphraseFn)), Vars):-
      cmember(TerseParaphraseFn, ['TerseParaphraseFn', 'PreciseParaphraseFn']),
      dcgRedress(Phrase, DCG, Vars), !.



dcgRedress([D|CG], dcgDressed(DCG), Vars):-compound(D), throwOnFailure((dcgRedressL([D|CG], RList, Vars), juntifyList(dcgSeq, RList, DCG))), !.
dcgRedress([D|CG], dcgDressedString(DCG), Vars):-atom(D), atom_concat('"', _, D), throwOnFailure((dcgRedressL([D|CG], RList, Vars), juntifyList(dcgSeq, RList, DCG))), !.


isNLFunctionNonExpandedArgs('NDecimalPlaceParaphraseFn').
dcgRedress([REDRESS, KW|ARGS], dcgNonExpandedVar(REDRESS, VarNameO, ARGS, Subj), [VarNameO-Subj]):-isNLFunctionNonExpandedArgs(REDRESS), getKeyVariable(KW, VarNameO), !.
dcgRedress([REDRESS|ARGS], dcgNonExpanded(REDRESS, ARGS), []):-isNLFunctionNonExpandedArgs(REDRESS), !.

isNLFunctionNonExpanded(A):-not(atom(A)), !, fail.
isNLFunctionNonExpanded([]):-!, fail.
isNLFunctionNonExpanded(NP, 'NounPhrase'):-holdsWithQuery(resultIsa, NP, 'NounPhrase'), !.
isNLFunctionNonExpanded('NPIsNP-NLSentenceFn', 'NLSentence'):-!.
isNLFunctionNonExpanded('NPIsXP-NLSentenceFn', 'NLSentence'):-!.
isNLFunctionNonExpanded(NP, RealType):-holdsWithQuery(resultIsa, NP, 'NLPhrase'), catch(cycQuery(and(resultIsa(NP, RealType), genls(RealType, 'NLPhrase'))), _, RealType='NLPhrase'), !,
   noteFmt('nl: ~q.~n', [isNLFunctionNonExpanded(NP, RealType)]), !, list_dcgRedressStart.

dcgRedress([REDRESS|ARGS], dcgNonExpandedFrom(RI, REDRESS, DCG), Vars):- nonvar(REDRESS), isNLFunctionNonExpanded(REDRESS, RI), !,
   % noteFmt('isNLFunctionNonExpanded: ~q.~n', [[REDRESS|ARGS]]), !, list_dcgRedressStart,
   % trace,
   dcgRedressL(ARGS, DCG, Vars), !.



%use expansion
dcgRedress([REDRESS|ARGS], Out, Vars):-fail,
   isNLFunction(REDRESS),
   holdsWithQuery(expansion, REDRESS, NEW),
   cycUseExpansion([REDRESS|ARGS], NEW2),
   dcgRedress(NEW2, Out, Vars), !,
   noteFmt('~q -> ~q -> ~q ~n', [[REDRESS|ARGS], NEW2, Out]),
   list_dcgRedressStart.

cycUseExpansion([REDRESS|ARGS], NEW2):-holdsWithQuery(expansion, REDRESS, NEW),
   V = varHolder(NEW),
   ignore((
      nth0(N, [REDRESS|ARGS], ARG), N>0,
      arg(1, V, Value),
      atom_concat(':ARG', N, ARGNAME),
      subst(Value, ARGNAME, ARG, NewValue),
      nb_setarg(1, V, NewValue),
      fail)),
   arg(1, V, NEW2), !.



   /*

%use expansion
dcgRedress([REDRESS, ARG1], Out, Vars):-isNLFunction(REDRESS), holdsWithQuery(expansion, REDRESS, NEW),
   substEach(NEW, [':ARG1'-ARG1], NEW2), dcgRedress(NEW2, Out, Vars), !, list_dcgRedressStart.
dcgRedress([REDRESS, ARG1, ARG2], Out, Vars):- isNLFunction(REDRESS), holdsWithQuery(expansion, REDRESS, NEW),
   substEach(NEW, [':ARG1'-ARG1, ':ARG2'-ARG2], NEW2), dcgRedress(NEW2, Out, Vars), !, list_dcgRedressStart.
dcgRedress([REDRESS, ARG1, ARG2, ARG3], Out, Vars):- isNLFunction(REDRESS), holdsWithQuery(expansion, REDRESS, NEW),
   substEach(NEW, [':ARG1'-ARG1, ':ARG2'-ARG2, ':ARG3'-ARG3], NEW2), dcgRedress(NEW2, Out, Vars), !, list_dcgRedressStart.
dcgRedress([REDRESS, ARG1, ARG2, ARG3, ARG4], Out, Vars):- isNLFunction(REDRESS), holdsWithQuery(expansion, REDRESS, NEW),
   substEach(NEW, [':ARG1'-ARG1, ':ARG2'-ARG2, ':ARG3'-ARG3, ':ARG4'-ARG4], NEW2), dcgRedress(NEW2, Out, Vars), !, list_dcgRedressStart.
dcgRedress([REDRESS, ARG1, ARG2, ARG3, ARG4, ARG5], Out, Vars):- isNLFunction(REDRESS), holdsWithQuery(expansion, REDRESS, NEW),
   substEach(NEW, [':ARG1'-ARG1, ':ARG2'-ARG2, ':ARG3'-ARG3, ':ARG4'-ARG4, ':ARG5'-ARG5], NEW2), dcgRedress(NEW2, Out, Vars), !, list_dcgRedressStart.
*/
dcgRedress([REDRESS|ARGS], DCG, Vars):-isNLFunction(REDRESS),
   noteFmt('isNLFunction: ~q.~n', [[REDRESS|ARGS]]), !, list_dcgRedressStart,
   %trace,
   dcgRedressL(ARGS, DCG, Vars), !.


%TODO comment out unless for file testing
dcgRedress([D|CG], dcgSeqReinterp([D|CG]), []):-noteFmt('dcgSeqReinterp: ~q.~n', [[D|CG]]), list_dcgRedressStart, !.

dcgRedress([D|CG], dcgSeqReinterp(DCG), Vars):-noteFmt('dcgSeqReinterp: ~q.~n', [[D|CG]]), list_dcgRedressStart,
      throwOnFailure((dcgRedressL([D|CG], RList, Vars), juntifyList(dcgSeq, RList, DCG))), !.


dcgRedress('\\', dcgNone, []):-!.
dcgRedress(THEWORD, theWord(THEWORD), []):-isLexicalWord(THEWORD), !.
dcgRedress(VarName, dcgTemplate('AnyTemplate', VarNameO, Subj), [VarNameO-Subj]):-getKeyVariable(VarName, VarNameO), !.


dcgRedress(DCG, dcgReinterp(DCG), []):-noteFmt('dcgReinterp: ~q.~n', [DCG]), !, list_dcgRedressStart.


%holdsWithQuery(genTemplate, geographicalSubRegions, ['ConcatenatePhrasesFn', ['TermParaphraseFn-DO', svar(A, ':ARG2')], ['BestHeadVerbForInitialSubjectFn', 'Be-TheWord'], ['BestNLPhraseOfStringFn', string([a])], ['BestNLPhraseOfStringFn', string([geographical])], ['BestNLPhraseOfStringFn', string([subregion])], ['BestPPFn', 'Of-TheWord', ['TermParaphraseFn-DO', svar(B, ':ARG1')]]]).


%templateMarkerRepresentation(TemplateMarker, Char):-assertion(templateMarkerRepresentation, TemplateMarker, [(Char)]).
templateMarkerRepresentation('TemplateHyphenMarker', "-").
templateMarkerRepresentation('TemplateOpenBracketMarker', "[").
templateMarkerRepresentation('TemplateCloseBracketMarker', "]").
templateMarkerRepresentation('TemplateCloseParenMarker', "]").
templateMarkerRepresentation('TemplateDoubleQuoteMarker', "'").
templateMarkerRepresentation('TemplateSingleQuoteMarker', "'").
templateMarkerRepresentation('TemplateExclamationMarkMarker', "!").
templateMarkerRepresentation('TemplateQuestionMarkMarker', "?").
templateMarkerRepresentation('TemplateCommaMarker', ", ").
templateMarkerRepresentation('TemplateSemiColonMarker', ";").
templateMarkerRepresentation('TemplatePeriodMarker', ".").
templateMarkerRepresentation('TemplateColonMarker', ":").


toText([], []):-!.
toText(S, T):-cmember([txt|T], S), !.
toText([S|SS], TTT):-toText(S, T), toText(SS, TT), flatten([T|TT], TTT).


% =======================================================
% DCG Helpers/Operators (and some Terminals)
% =======================================================


%memberData(M, [W|Ws]):-textCached([W|Ws], M).
%memberData(M, [W|Ws]):-textCached([_, W|Ws], M), !.
%memberData(M, [W|Ws]):-textCached([W|_], M), !.
%memberData(M, W):-true, concat_atom(WList, '_', W), !, memberData(M, WList).


% Text
theText(_, [], _):- !, fail.
theText([S]) --> {!}, theGText(S).
theText([S|Text]) --> {nonvar(Text)}, [Data], {cmember([txt|[S|Text]], Data), !}.
theText([S|Text]) --> {nonvar(Text), !}, [Data], {cmember([txt, S], Data)}, theText(Text).
theText(S) --> {atom(S), atom_concat('"', Right, S), atom_concat(New, '"', Right), !}, theText(New).
theText(S) --> {atom(S), concat_atom([W1, W2|List], ' ', S), !}, theText([W1, W2|List]).
theText(S) --> theGText(S).

% Looser text test?
theGText(_, [], _):- !, fail.
theGText(S) --> {!}, [Data], {member([txt, S], Data)}.

theTerm(Term) --> theText([S|TEXT]), {textCached([S|TEXT], [denotation, _Pos, Term|_])}.

% bposToPos/2 is from [cyc_pos].
%thePOS(Pos) --> [Data], {once((memberchk([tag, _|POSs], Data), cmember(CPos, POSs), (Pos=CPos;bposToPos(CPos, Pos))))}.
thePOS(Pos) --> [Data], {notrace(isTag(Data, Pos))/* , ! */}.
thePOS('NLSentence') --> dcgZeroOrMore(dcgAny).
thePOS(Word, Pos) --> dcgAnd(thePOS(Pos), theWord(Word)).

theWord(_, [], _):- !, fail.
theWord(Word) --> theText([S|Text]), {once(textCached([S|Text], [lex, Word|_])), !}.
theWord('WordFn'(string([S|Text]))) --> theText([S|Text]), {!}.
%theWord(Word) --> theText([A]), {once(memberData([lex, Word|_], [A, B]))}, theText([B]).

theWord(_, _, [], _):- !, fail.
theWord(Word, Pos) --> {var(Word), !}, dcgAnd(thePOS(Pos), theWord(Word)).
theWord(Word, Pos) --> dcgAnd(theWord(Word), thePOS(Pos)).


theForm(PosForm)--> theText([S|Text]), {(textCached([S|Text], [lex, Word, PosForm|_]))}.

dcgPosVar(Pos, VarName, Subj)-->dcgAnd(thePOS(Pos), theVar(VarName, Subj)).

theName(Var, S, _) :-getText(S, Text), suggestVar(Text, Var), !.


theTense(Time) --> theForm(PosForm), {once(timeLookup(PosForm, Time))}.
thePerson(Pers) --> theForm(PosForm), {once(personLookup(PosForm, Pers))}.
theCount(Num)  --> theForm(PosForm), {once(countLookup(PosForm, Num))}.
theAgreement(Pers, Num) --> theForm(When), {personLookup(When, Pers), countLookup(When, Num)}.

timeLookup(Atom, 'Now'):-atom(Atom), atom_concat(_, 'Present', Atom).
timeLookup(Atom, 'Past'):-atom(Atom), atom_concat(_, 'Past', Atom).
timeLookup(Atom, 'Future'):-atom(Atom), atom_concat(_, 'Future', Atom).
timeLookup(Atom, 'Now'):-atom(Atom), atom_concat('present', _, Atom).
timeLookup(Atom, 'Past'):-atom(Atom), atom_concat('past', _, Atom).
timeLookup(Atom, 'Future'):-atom(Atom), atom_concat('future', _, Atom).

countLookup(Atom, 'Plural'):-atom(Atom), atom_concat('plural', _, Atom).
countLookup(Atom, 'Plural'):-atom(Atom), atom_concat(_, 'Plural', Atom).
countLookup(Atom, 'Plural'):-atom(Atom), concat_atom([_, 'nonSing', _], Atom).
countLookup(Atom, 'Singular'):-atom(Atom), atom_concat('singular', _, Atom).
countLookup(Atom, 'Singular'):-atom(Atom), atom_concat(_, 'Singular', Atom).
countLookup(Atom, 'Singular'):-atom(Atom), concat_atom([_, 'Sg', _], Atom).

personLookup(Atom, 'First'):-atom(Atom), atom_concat('first', _, Atom).
personLookup(Atom, 'Second'):-atom(Atom), atom_concat('second', _, Atom).
personLookup(Atom, 'Third'):-atom(Atom), atom_concat('third', _, Atom).






theFrame(Event, FrameType, Pos, Template) --> theWord(Word), {getFrame(Word, Event, FrameType, Pos, Template), once(suggestVar(SText, Event))}.

%textCached('Lead-TheWord', [frame, 'Lead-TheWord', 'Verb', 'TransitiveNPFrame', and(isa(':ACTION', 'GuidingAMovingObject'), directingAgent(':ACTION', ':SUBJECT'), primaryObjectMoving(':ACTION', ':OBJECT')), verbSemTrans]).

%  getFrame(Word, Event, 'ParticleCompFrameFn'('IntransitiveParticleFrameType', Prep), 'Verb', Template).
getFrame(Word, Event, FrameType, Pos, Template):-ground(FrameType), !, findall(CycL, (textCached(_, [frame, Word, Pos, FrameType, CycL, Pred])), Some), Some=[Lest|One], joinCols(or, [Lest|One], Template).
getFrame(Word, Event, FrameType, Pos, CycL):-textCached(_, [frame, Word, Pos, FrameType, CycL, Pred]).
getFrame(Word, Event, FrameType, Pos, CycL):- cycQuery(wordSemTrans(Word, Num, FrameType, CycL, Pos, Pred, Precons)).


templateConstaint('True', Template, Template):-!.
templateConstaint(Constraints, Template, implies(Constraints, Template)).

framePredForPos(Pos, Pred):-holdsWithQuery(semTransPredForPOS, Pos, Pred).
%framePredForPos('Preposition', 'prepReln-Action').
%framePredForPos('Preposition', 'prepReln-Object').
framePredForPos('Adjective', 'adjSemTrans-Restricted').

frameAdd:-cycQuery('adjSemTrans-Restricted'(WORD, NUM, FRAME, COL, CYCL), '#$EverythingPSC'), cycAssert(wordSemTrans(WORD, 'Adjective', NUM, FRAME, implies(isa(':NOUN', COL), CYCL), 'adjSemTrans-Restricted', _PreRequ), '#$EnglishMt'), fail.
frameAdd:-cycQuery('nounPrep'(WORD, PREP, CYCL), '#$EverythingPSC'), cycAssert(wordSemTrans(WORD, _Num, 'PPCompFrameFn'('TransitivePPFrameType', PREP), CYCL, 'Noun', 'nounPrep', _PreRequ), '#$EnglishMt'), fail.
%frameAdd:-cycQuery('prepReln-Action'(ACTION, OBJECT, WORD, CYCL), '#$EverythingPSC'), cycAssert(wordSemTrans(WORD, _Num, 'Post-VerbPhraseModifyingFrame', implies(and(isa(':ACTION', ACTION), isa(':OBLIQUE-OBJECT', OBJECT)), CYCL), 'Preposition', 'prepReln-Action', _PreRequ), '#$EnglishMt'), fail.
%frameAdd:-cycQuery('prepReln-Object'(NOUN, OBJECT, WORD, CYCL), '#$EverythingPSC'), cycAssert(wordSemTrans(WORD, _Num, 'Post-NounPhraseModifyingFrame', implies(and(isa(':NOUN', NOUN), isa(':OBLIQUE-OBJECT', OBJECT)), CYCL), 'Preposition', 'prepReln-Object', _PreRequ), '#$EnglishMt'), fail.

frameAdd(WORDS):-cycQuery('compoundSemTrans'(WORDS, WLIST, POS, FRAME, CYCL), '#$EverythingPSC'),
         stringToWords(WLIST, WORDS).

stringToWords([], []).
stringToWords(TheList, Words):-functor(TheList, 'TheList', _), TheList=..[_|List], !, stringToWords(List, Words).
stringToWords(string(S), Words):-!, stringToWords(S, Words).
stringToWords(['TheList'|List], Words):-!, stringToWords(List, Words).
stringToWords([S|Tring], [W|Words]):-stringToWord(S, W), stringToWords(Tring, Words).

stringToWord([S], W):-!, textCached([S], [lex, W|_]).
stringToWord(S, W):-textCached([S], [lex, W|_]).



suggestVar(Subj, Subj):-var(Subj), !.
suggestVar(Subj, Subj2):-var(Subj), !.
suggestVar([W|ORDS], Subj):-!, ignore(notrace(once((nonvar(ORDS), concat_atom(['?'|[W|ORDS]], '', Suj), gensym(Suj, SubjSl), cyc:toUppercase(SubjSl, SubjS), ignore(SubjS=Subj))))), !.
suggestVar([], Subj):-!.%suggestVar([A], Subj), !.
suggestVar(A, Subj):-suggestVar([A], Subj), !.

suggestVarI([W|ORDS], Subj):-!, concat_atom(['?'|[W|ORDS]], '', Suj), cyc:toUppercase(Suj, SubjS), ignore(SubjS=Subj), !.
suggestVarI(A, Subj):-suggestVarI([A], Subj).

theIsa(Subj, COLS, ColType) --> theOneOfFn(Subj, COLS, ColType).

theOneOfFn(Subj, COLS, ColType)-->theWord(Word), {suggestVar(SText, Subj),
    meetsRequirement(Word, ColType, COLSS), joinCols('OneOfFn', COLSS, COLS)}.

meetsRequirement(Word, ColType, COLSS):-
       notrace(findall(COL, (textCached(Word, [denotation, _Pos, COL|DPROPS]), anyIsa([COL|DPROPS], ColType)), COLSS)), !, leastOne(COLSS).

leastOne([CO|LSS]).

anyIsa([COL|DPROPS], ColType):-member(One, [COL|DPROPS]), cycQuery(isa(One, ColType)), !.

%e2c_sem("the singer sang a song").

joinCols(JOINER, [CO|LS1], COLS):-list_to_set([CO|LS1], COLS1S), ([COLS]=COLS1S -> true; COLS=nart([JOINER|COLS1S])), !.

%constraintsOnIsa([_, _, [EQ|ColTypes]|_], EQ, nart(['CollectionIntersectionFn'|ColTypes])).
%constraintsOnIsa([_, _, [EQ]|_], EQ, 'Thing').


:-flag(dcg_template_depth, _, 0).
:-dynamic(current_dcg_template/2).
% current_dcg_template(?Template, ?Depth)


textMathesTemplate(T, TempType):-not(atom(TempType)), !, fail.
textMathesTemplate(T, TempType):- cmember(T, ['X', 'Y', 'Z']).
textMathesTemplate(Txt, TempType):- atom(Txt), concat_atom([_|_], Txt, TempType).

disable_dcgTemplate(TempType, VarName, cycForTemplate(VarName, TempType, TempType1), [TempTypeData|E], E):-
     getText(TempTypeData, [TempType1]), textMathesTemplate(TempType1, TempType), !.

dcgTemplate(TempType, VarName, CycL, S, E):- TempType == 'AnyTemplate', !, dcgTemplate('NPTemplate', VarName, CycL, S, E).
dcgTemplate(TempType, VarName, CycL, S, E):-
            (current_dcg_template(TempType, Prev)->(fail);true),
            flag(dcg_template_depth, D, D+1),
            asserta(current_dcg_template(TempType, D)),
            (D<40 -> true ; fail),
            %startsTemplateType(S, TempType),
            appliedTemplate(TempType, _Pred, DCG, CycL),
            throwOnFailure(callable(DCG)),
            phrase(DCG, S, E),
            flag(dcg_template_depth, DN, DN-1),
            retractall(current_dcg_template(TempType, D)).



dcgNote(X)--> dcgConsumeData(True), {noteFmt('dcgNote(~q) for ~q )', [X, True]), !}.

dcgTemplateKW(KWs, VarName, Subj)-->{cmember(TempType, KWs)}, dcgTemplate(TempType, VarName, Subj).

dcgDressedString(X)-->dcgDressed(X).
dcgDressed(X)-->{throwOnFailure(nonvar(X)->callable(X);true)}, X.

startsTemplateType([A|B], VPTemplate, Type):-cmember([lex, _|POSVerb], A), !, cmember(Verb, POSVerb), holdsWithQuery(posForTemplateCategory, Verb, VPTemplate).
startsTemplateType([A|B], VPTemplate, Type).


dcgReinterp(Term) --> theTerm(Term).
theTermStrings(Term) --> theWord(Term).
theTermStrings(Term) --> theTerm(Term).

dcgOptionalOne([])-->[].
dcgOptionalOne([X|_])-->dcgDressed(X), (!).
dcgOptionalOne([_|Xs])-->dcgOptionalOne(Xs).


dcgStartsWith(TheType, SMORE, SMORE) :- phrase(TheType, SMORE, _).
dcgStartsWith1(TheType, [S|MORE], [S|MORE]) :- phrase(TheType, [S], []).

dcgAndRest(TheType, TODO, [S|MORE], []) :- phrase(TheType, [S], []), phrase(TheType, [S|MORE], []).

dcgConsumeRest(_, []).
dcgPreTest(DCG, SE, SE):-phrase(DCG, SE, []).
dcgContainsPreTest(DCG)-->dcgPreTest((dcgMapOne(DCG), dcgConsumeRest)).


% same dcgNoConsumeStarts(DCG, SE, SE):-dcgNoConsume((DCG, dcgConsumeRest), SE, SE).
dcgNoConsumeStarts(DCG, SE, SE):-phrase(DCG, SE, _).



:-dynamic(posOkForPhraseType(DefiniteNounPPFn, Pos)).

dcgPhraseType(DefiniteNounPPFn, DCG)--> dcgAnd((dcgStartsWith(thePOS(Pos)), {posOkForPhraseType(DefiniteNounPPFn, Pos)}), dcgDressed(X)).

dcgConstraintBindings(DCG, Constr) --> dcgDressed(DCG), {todo(cycQuery(Constr))}.
dcgConstraint(DCG, Constr) --> dcgDressed(DCG), {todo(cycQuery(Constr))}.

dcgCycL(DCG, Constr) --> {todo(getGenerationTemplateFor(dcgCycL(DCG, Constr, Template))), fail}, Template.

dcgMaybeQuoted(X)-->dcgDressed(X).

dcgConjWithWord(Num, [Word, ConjList]) --> dcgSeq(Num, ConjList).

dcgRepeatForSubsequentArgsFn(Num, Each)--> dcgDressed(Each).
dcgNoSpaces(X)-->dcgDressed(X).

dcgReverse(Rev) --> {todo(getGenerationTemplateFor(dcgReverse(Rev, Template))), fail}, Template.
dcgSeqReinterp(Rev) --> {todo(getGenerationTemplateFor(dcgReverse(Rev, Template))), fail}, Template.

dcgOptionalSome(X)-->dcgOptionalCount(X, _N).

dcgOptionalCount([], 0)-->[], {!}.
dcgOptionalCount([X|Xs], N)-->dcgDressed(X), {!}, dcgOptionalCount(Xs, N1), {N is N1 + 1}.
dcgOptionalCount([_X|Xs], N)-->dcgOptionalCount(Xs, N).

dcgRequireSome(_Num, X)-->dcgOptionalCount(X, N), {!, N>0}.

% Num + List of Items
dcgSeq(Num, List, B, E):-number(Num),
   %is_list(List),
   !, dcgSeq_phrase(List, B, E).
% Item1 + Item2
dcgSeq(X, Y, [S0, S1|SS], E):-phrase((X, Y), [S0, S1|SS], E).
dcgSeq(X, Y, Z, [S0, S1|SS], E):-phrase((X, Y, Z), [S0, S1|SS], E).

dcgSeq_phrase([], B, B) :- !.
dcgSeq_phrase([Dcg|List], B, E) :- catch(phrase(Dcg, B, M), E, (writeq(E+phrase(Dcg, B, M)), nl, fail)), dcgSeq_phrase(List, M, E).


dcgBoth(DCG1, DCG2, S, R) :- append(L, R, S), phrase(DCG1, L, []), once(phrase(DCG2, L, [])).

dcgAnd(DCG1, DCG2, DCG3, DCG4, S, E) :- phrase(DCG1, S, E), phrase(DCG2, S, E), phrase(DCG3, S, E), phrase(DCG4, S, E).
dcgAnd(DCG1, DCG2, DCG3, S, E) :- phrase(DCG1, S, E), phrase(DCG2, S, E), phrase(DCG3, S, E).
dcgAnd(DCG1, DCG2, [S|TART], E) :- phrase(DCG1, [S|TART], E), phrase(DCG2, [S|TART], E).
dcgOr(DCG1, DCG2, DCG3, DCG4, DCG5, S, E) :- phrase(DCG1, S, E);phrase(DCG2, S, E);phrase(DCG3, S, E);phrase(DCG4, S, E);phrase(DCG5, S, E).
dcgOr(DCG1, DCG2, DCG3, DCG4, S, E) :- phrase(DCG1, S, E);phrase(DCG2, S, E);phrase(DCG3, S, E);phrase(DCG4, S, E).
dcgOr(DCG1, DCG2, DCG3, S, E) :- phrase(DCG1, S, E);phrase(DCG2, S, E);phrase(DCG3, S, E).
dcgOr(Num, DCG2, S, E) :- number(Num), !, member(DCG, DCG2), phrase(DCG, S, E).
dcgOr(DCG1, DCG2, S, E) :- phrase(DCG1, S, E);phrase(DCG2, S, E).
dcgOr([D|CG1], S, E) :- member(One, [D|CG1]), phrase(One, S, E).
dcgNot(DCG2, S, E) :- not(phrase(DCG2, S, E)).
dcgIgnore(DCG2, S, E) :- ignore(phrase(DCG2, S, E)).
dcgOnce(DCG2, S, E) :- once(phrase(DCG2, S, E)).

dcgWhile(True, Frag)-->dcgAnd(dcgOneOrMore(True), Frag).

dcgOneOrMore(True) --> True, dcgZeroOrMore(True), {!}.

dcgZeroOrMore(True) --> True, {!}, dcgZeroOrMore(True), {!}.
dcgZeroOrMore(True) --> [].

dcgMapCar(Var, DCG, [Var|List])-->{copy_term(Var+DCG, Var2+DCG2)}, DCG, {!}, dcgMapCar(Var2, DCG2, List).
dcgMapCar(Var, DCG, [])-->[].

dcgMapSome(Var, DCG, [Var|List])-->{copy_term(Var+DCG, Var2+DCG2)}, DCG, {!}, dcgMapSome(Var2, DCG2, List).
dcgMapSome(Var, DCG, List)--> [_], {!}, dcgMapSome(Var, DCG, List).
dcgMapSome(Var, DCG, [])-->[].

dcgMapOne(Var, DCG)--> DCG, {!}.
dcgMapOne(Var, DCG)--> [_], dcgMapOne(Var, DCG).

dcgConsumeData([]) --> [].
dcgConsumeData([Data|More]) --> [Data], {!}, dcgConsumeData(More), {!}.

dcgLeftOf(Mid, [Left|T], S, [MidT|RightT]):-append([Left|T], [MidT|RightT], S), phrase(Mid, MidT), phrase([Left|T], LeftT).

dcgMid(Mid, Left, Right) --> dcgLeftOf(Mid, Left), Right.

scanDcgUntil(_, _, [], __):-!, fail.
scanDcgUntil(DCG, [], [Found|Rest], [Found|Rest]):-phrase(DCG, [Found], []), !.
scanDcgUntil(DCG, [Skipped|Before], [Skipped|More], [Found|Rest]):-scanDcgUntil(DCG, Before, More, [Found|Rest]).

dcgNone --> [].
dcgAny --> [_].

% Matches the rightmost DCG item
dcgLast(DCG, S, Left):-append(Left, [Last], S), phrase(DCG, [Last]).

dcgIfThenElse(DCG, True, False, S, E) :- phrase(DCG, S, M) -> phrase(True, M, E) ; phrase(False, S, E).
dcgIfThenOr(DCG, True, False, S, E) :- (phrase(DCG, S, M) , phrase(True, M, E)) ; phrase(False, S, E).

dcgOptional(A)-->dcgOnce(dcgOr(A, dcgNone)).

%throwOnFailure(X):-once(X;(trace, X)).
debugOnError(X):-catch(X, E, (writeq(E), trace, X)).

dcgNonExpandedFrom(Pos, GenValueParaphraseFn, ConjList) -->  dcgSeq_phrase(ConjList).
dcgNonExpandedVar(NDecimalPlaceParaphraseFn, VARG1 , [Num], Var) -->
   dcgAnd(theVar(VARG1, Var), dcgNote(dcgNonExpandedVar(NDecimalPlaceParaphraseFn, VARG1 , [Num], Var))).


capitalized([W|Text]) --> theText([W|Text]), {atom_codes(W, [C|Odes]), is_upper(C)}.

substAll(B, [], R, B):-!.
substAll(B, [F|L], R, A):-subst(B, F, R, M), substAll(M, L, R, A).

substEach(B, [], B):-!.
substEach(B, [F-R|L], A):-subst(B, F, R, M), substEach(M, L, A).

% =======================================================
% utterance_sem(Event, CycL, [every, man, that, paints, likes, monet], [])
% =======================================================

%utterance_sem(Event, CycL) -->utterance_sem(Event, CycL1), {substEach(CycL1, [':POSSESSOR'-Pos], CycL), suggestVar('POSSESSOR', Pos)}.

englishCtx(Event, thereExists(Event, CycL)) --> utterance_sem(Event, CycL).

%utterance_sem(Event, 'NothingSaid') -->[].
utterance_sem(Event, CycL) -->['S'|Form], {!, throwOnFailure(phrase(utterance_sem(Event, CycL), Form))}.
utterance_sem(Event, template(TempType, CycL)) --> {cmember(TempType, ['STemplate'])}, dcgTemplate(TempType, Event, CycL).
utterance_sem(Event, template(TempType, CycL)) --> dcgTemplate(TempType, Event, CycL).
%utterance_sem(Event, CycL) --> noun_unit(Type, Event, Subj, holdsIn('Now', Subj), CycL).
%utterance_sem(Event, CycL, S, []) :- once((ground(S), toText(S, T))), cyclifyTest(string(T), CycL).
utterance_sem(Event, forAll(Subj, forAll(Obj, implies(isa(Subj, SType), isa(Obj, SObj))))) --> universal_word(Form), dcgAnd(theIsa(Subj, SType, 'Collection'), theForm(Form)), thePOS('BeAux'), theIsa(Obj, SObj, 'Collection').
utterance_sem(Event, CycL) --> declaritive_sentence(Subj, Event, Obj, CycL), nonQMarkPunct.
utterance_sem(Event, CycL) --> imparitive_sentence(Event, CycL), nonQMarkPunct.
utterance_sem(Event, 'CYC-QUERY'(CycL)) --> inquiry(Event, CycL).


%utterance_sem(Event, and(CycL1, CycL2)) -->utterance_sem(Event, CycL1), utterance_sem(Event, CycL2).

nonQMarkPunct --> dcgAnd(thePOS('Punctuation-SP'), dcgNot(theWord('QuestionMark-TheSymbol'))).
nonQMarkPunct --> [].


imparitive_sentence(Event, CycL) --> thePOS('Interjection-SpeechPart'), {!}, imparitive_sentence(Event, CycL).
imparitive_sentence(Event, utters('?SPEAKER', Word, Means)) --> dcgAnd(thePOS(Word, 'Interjection-SpeechPart'), dcgIgnore(theTerm(Means))).
imparitive_sentence(Event, CycL) --> theWord('Please-TheWord'), {!}, imparitive_sentence(Event, CycL).
imparitive_sentence(Event, CycL) --> verb_phrase(Time, Subj, Obj, Event, CycL), {suggestVarI('TargetAgent', Subj), suggestVar('ImparitiveEvent', Event)}.


chunkParseCycL(Event, Subj, Tagged, CycL9):-
        sentenceChunker(Tagged, Chunks), nl,
        parseCycLChunk(and(isa(Event, 'Situation'), situationConstituents(Event, Subj)), Chunks, Event, Subj, CycL9).



parseCycLChunk(_CycLIn, Chunks, _Event, Subj, _):-once(dumpList(Chunks)), nl, true, fail.

% tail closer
parseCycLChunk(CycLIn, [], Event, Subj, CycLIn):-!, suggestVar('SENTENCE', Event), suggestVar('SUBJECT', Subj).

parseCycLChunk(CycLIn, [seg(cc, _, _, _)|Const], Event, Subj, CycL) :-
         parseCycLChunk(CycLIn, Const, Event, Subj, CycL).

parseCycLChunk(CycLIn, [seg(cs, _, _, _)|Const], Event, Subj, CycL) :-
         parseCycLChunk(CycLIn, Const, Event, Subj, CycL).

parseCycLChunk(CycLIn, [seg(sym, _, [X], _)|Const], Event, Subj, CycL):- cmember(X, [('.'), ('!'), (', ')]), !,
         parseCycLChunk(CycLIn, Const, Event, Subj, CycL).

parseCycLChunk(CycLIn, Const, Event, Subj, 'CYC-ASSERT'(CycL)) :- append(Const1, [seg(sym, _, [X], _)], Const), cmember(X, [('.'), ('!'), (', ')]), !,
         parseCycLChunk(CycLIn, Const1, Event, Subj, CycL).

parseCycLChunk(CycLIn, Const, Event, Subj, 'CYC-QUERY'(CycL)) :- append(Const1, [seg(sym, _, [X], _)], Const), cmember(X, [('?')]), !,
         parseCycLChunk(CycLIn, Const1, Event, Subj, CycL).

objTypeCompat(ObjType2, ObjType1):-var(ObjType2), var(ObjType1), !.
objTypeCompat(ObjType2, ObjType1):-(var(ObjType2);var(ObjType1)), !, fail.
objTypeCompat(ObjType1, ObjType1).



isConjoined(seg(in, _, [with], _), with).
isConjoined(seg(cc, _, [Conj], _), Conj).

% theObject and theObject -> theObject
parseCycLChunk(CycLIn, Const, Event, SubjThru, CycLO) :-
    append(ConstL, [theObject(ObjType1, Subj1, W1), WITH, theObject(ObjType2, Subj2, W2)|ConstR], Const),
    isConjoined(WITH, Conj), objTypeCompat(ObjType2, ObjType1), !,
         append(W1, [Conj|W2], W),
         append(ConstL, [theObject(ObjType1, Subj, W)|ConstR], NConst),
         parseCycLChunk(and(memberOfList(Subj, 'TheList'(Subj1, Subj2)), CycLIn), NConst, Event, SubjThru, CycLO), !,
         ignore(SubjThru=Subj),
         suggestVar(W, Subj), !.

% Convert PN -> NPs
parseCycLChunk(CycLIn, Const, Event, SubjThru, CycLO) :-
   append(ConstL, [seg(pn, T1, W1, D1)|ConstR], Const),
         append(ConstL, [seg(np, T1, W1, D1)|ConstR], NConst),
         parseCycLChunk(CycLIn, NConst, Event, SubjThru, CycLO).

% First  NP -> theObject
%parseCycLChunk(CycLIn, [seg(np, T1, W1, D1)|ConstR], Event, Subj, CycL) :-
%         phrase(noun_unit(T1, Event, Subj, CycLO, CycL), D1),
%         parseCycLChunk(CycLIn, [theObject(ObjType, Subj, W1)|ConstR], Event, Subj, CycLO).

% NP + OF + NP
parseCycLChunk(CycLIn, Const, EventThru, SubjThru, CycLOut):-
   append(ConstL, [seg(np, T1, W1, D1), seg(in, 'IN', [of], [PTAG]), seg(np, T2, W2, D2)|ConstR], Const),
         append(W1, [of|W2], W), append(D1, [PTAG|D2], D),
         append(ConstL, [seg(np, T2, W, D)|ConstR], NConst), %!,
         parseCycLChunk(CycLIn, NConst, EventThru, SubjThru, CycLOut).

% NP + AT + NP
no_parseCycLChunk(CycLIn, Const, EventThru, SubjThru, CycLOut):-
   append(ConstL, [seg(np, T1, W1, D1), seg(in, 'IN', [at], [PTAG]), seg(np, T2, W2, D2)|ConstR], Const),
         append(W1, [at|W2], W), append(D1, [PTAG|D2], D),
         append(ConstL, [seg(np, T2, W, D)|ConstR], NConst), %!,
         parseCycLChunk(CycLIn, NConst, EventThru, SubjThru, CycLOut).


% NP -> theObject
parseCycLChunk(CycLIn, Const, Event, SubjThru, CycL) :-
   append(ConstL, [seg(np, T1, W1, D1)|ConstR], Const),
         asNounUnit(ObjType, T1, D1, W1, Event, Subj, CycLO, CycL),
         append(ConstL, [theObject(ObjType, Subj, W1)|ConstR], NConst),
         parseCycLChunk(CycLIn, NConst, Event, SubjThru, CycLO), suggestVar(W1, Subj).


% theObject + VP + theObject + IN + theObject -> theEvent
parseCycLChunk(CycLIn, Const, EventThru, SubjThru, CycLOut):-
   append(ConstL, [theObject(ObjType1, Subj, SWords), seg(vp, Type, TXT, VTAG), theObject(ObjType2, Obj, OWords), seg(in, PType, PTXT, PTAG), theObject(ObjType3, Target, TWords)|ConstR], Const),
         append(ConstL, [theObject('Action', Event, TXT), theObject(ObjType1, Subj, SWords)|ConstR], NConst),
         phrase(verb_prep(Event, Subj, Obj, PTAG, Target, CycL), VTAG),
         parseCycLChunk(and(CycL, CycLIn), NConst, EventThru, SubjThru, CycLOut), suggestVar(TXT, Event).

% theObject + VP + theObject + IN + theObject -> theEvent
parseCycLChunk(CycLIn, Const, EventThru, SubjThru, CycLOut):-
   append(ConstL, [theObject(ObjType1, Subj, SWords), seg(vp, Type, TXT, VTAG), seg(in, PType, PTXT, PTAG), theObject(ObjType3, Target, TWords)|ConstR], Const),
         append(ConstL, [theObject('Action', Event, TXT), theObject(ObjType1, Subj, SWords)|ConstR], NConst),
         phrase(verb_prep(Event, Subj, Target, PTAG, Target, CycL), VTAG),
         parseCycLChunk(and(CycL, CycLIn), NConst, EventThru, SubjThru, CycLOut), suggestVar(TXT, Event).

% theObject + VP + IN + theObject -> theEvent
parseCycLChunk(CycLIn, Const, EventThru, SubjThru, CycLOut):-
   append(ConstL, [theObject(ObjType1, Subj, SWords), seg(vp, Type, TXT, VTAG), seg(in, PType, PTXT, PTAG), theObject(ObjType2, Target, TWords)|ConstR], Const),
         append(ConstL, [theObject('Action', Event, TXT), theObject(ObjType1, Subj, SWords)|ConstR], NConst),
         append(VTAG, PTAG, TAGGED),
         phrase(verb2(Event, Subj, Target, CycL), TAGGED),
         parseCycLChunk(and(CycL, CycLIn), NConst, EventThru, SubjThru, CycLOut), suggestVar(TXT, Event).

% theObject + VP + theObject -> theEvent
parseCycLChunk(CycLIn, Const, EventThru, SubjThru, CycLOut):-
   append(ConstL, [theObject(ObjType1, Subj, SWords), seg(vp, Type, TXT, VTAG), theObject(ObjType2, Obj, OWords)|ConstR], Const),
         append(ConstL, [theObject('Action', Event, TXT), theObject(ObjType1, Subj, SWords)|ConstR], NConst),
         phrase(verb2(Event, Subj, Obj, CycL), VTAG),
         parseCycLChunk(and(CycL, CycLIn), NConst, EventThru, SubjThru, CycLOut), suggestVar(TXT, Event).

% theObject + VP  -> theEvent
parseCycLChunk(CycLIn, Const, EventThru, SubjThru, CycLOut):-
   append(ConstL, [theObject(ObjType1, Subj, SWords), seg(vp, Type, TXT, VTAG)|ConstR], Const),
         append(ConstL, [theObject('Action', Event, TXT), theObject(ObjType1, Subj, SWords)|ConstR], NConst),
         phrase(verb1(Event, Subj, CycL), VTAG),
         parseCycLChunk(and(CycL, CycLIn), NConst, EventThru, SubjThru, CycLOut), suggestVar(TXT, Event).

% Convert IN -> VP
parseCycLChunk(CycLIn, Const, Event, SubjThru, CycLO) :-
    append(ConstL, [seg(in, T1, W1, D1)|ConstR], Const),
         append(ConstL, [seg(vp, T1, W1, D1)|ConstR], NConst), !,
         parseCycLChunk(CycLIn, NConst, Event, SubjThru, CycLO).

% Skipovers
parseCycLChunk(CycLIn, [theObject(Action, Var, Words)|Rest], EventThru, SubjThru, and('subEvents'(EventThru, Var), CycL)) :-
    Action == 'Action', !, suggestVar(Var, Words),
         parseCycLChunk(CycLIn, Rest, EventThru, SubjThru, CycL).

parseCycLChunk(CycLIn, [theObject(ObjType, Var, Words)|Rest], EventThru, SubjThru, and('doom:descriptionStrings'(Var, string(Words)), CycL)) :-!, suggestVar(Var, Words),
         ignore(Var=SubjThru), parseCycLChunk(CycLIn, Rest, EventThru, SubjThru, CycL).

parseCycLChunk(CycLIn, [seg(PC, Type, Words, Tagged)|Rest], EventThru, SubjThru, and('doom:descriptionStrings'(Subj, string(Words), PC), CycL)) :- !,
         parseCycLChunk(CycLIn, Rest, EventThru, SubjThru, CycL), suggestVar(Subj, Words).

parseCycLChunk(CycLIn, [Err|Rest], Event, Subj, and('doom:descriptionErrorStrings'(Subj, string(Words)), CycL)) :- true,
         parseCycLChunk(CycLIn, Rest, Event, SubjNext, CycL).

verb1(Event, Subj, and(actors(Event, Subj), CycL))-->verb_unit(Type, Time, Subj, Obj, Target, Event, CycL).
verb2(Event, Subj, Obj, CycL)-->verb_unit(Type, Time, Subj, Obj, Target, Event, CycL).
skip(string(C))-->[X], {getText(X, C)}.

% =======================================================
% utterance_sem(Event, CycL, [every, man, that, paints, likes, monet], [])
% =======================================================

sent(Event, Subj, 'CYC-ASSERT'(CycL9)) -->dcgLast(theText([X])), {member(X, [('.'), ('!'), (', ')]), !}, sent(Event, Subj, CycL9).
sent(Event, Subj, CycL) -->(theText([X])), {member(X, [('.'), ('!'), (', ')]), !}, sent(Event, Subj, CycL).
sent(Event, Subj, CycL) -->dcgLast(theText([X])), {member(X, [('?')]), !}, sent(Event, Subj, CycL).
sent(Event, Subj, 'CYC-QUERY'(CycL9))-->thePOS('Modal'), !, sent(Event, Subj, CycL9).
sent(Event, Subj, verbSubjObj(Event, Subj, Obj, CycL9)) --> noun_unit(Type, Event, Subj, VerbIn, CycL9), vp(Event, Subj, Obj, VerbIn).
sent(Event, Subj, verbSubjObj(Event, '?TargetAgent', Obj, CycL9)) --> vp(Event, Subj, Obj, CycL9).
sent(Event, Subj, CycL9) --> noun_unit(Type, Event, Subj, the(Subj), CycL9).
sent(Event, Subj, and(isa(Event, 'Event'), CycL9)) --> rel_clause_sem(Event, Subj, CycL9).


%You could apply chineese room to human physical neurons.. but does that make us unintelligent?


joinNP(N1, no(noun), N1):-!.
joinNP('NounFn'(N1), 'NounFn'(N2), 'NounFn'(N1, N2)):-!.
joinNP(N1, N2, 'JoinFn'(N1, N2)).


/*
vp(Event, Subj, Obj1, and(CycL5, CycL9)) -->vp1(Event, Subj, Obj1, CycL5), conj(CC), vp1(Event, Subj, Obj2, CycL9).
vp(Event, Subj, Obj, CycL9) -->vp1(Event, Subj, Obj, CycL9).

compressed is :
*/
vp(Event, Subj, Obj, CycLO) --> vp1(Event, Subj, Obj, CycL5),
   dcgIfThenElse((conj(CC), vp1(Event, Subj, Obj2, CycL9)), {CycLO=and(CycL5, CycL9)}, {CycLO=CycL5}).


/*
vp1(Event, Subj, Obj, and(hasAttribute(Event, AVP), CycL9)) --> verb_unit(Type, Time, Subj, Obj, Target, Event, CycL5), object_prep_phrase(Event, Obj, CycL5, CycL9), adv(AVP).
vp1(Event, Subj, Obj, CycL9) --> verb_unit(Type, Time, Subj, Obj, Target, Event, CycL5), object_prep_phrase(Event, Obj, CycL5, CycL9).

compressed is :
*/
vp1(Event, Subj, Obj, holdsIn(Time, CycL9))-->verb_phrase(Time, Subj, Obj, Event, CycL9).
vp1(Event, Subj, Obj, CycLO) --> verb_unit(Type, Time, Subj, Obj, Target, Event, CycL5), object_prep_phrase(Event, Obj, CycL5, CycL9),
   dcgIfThenElse(adv(AVP), {CycLO=and(hasAttribute(Event, AVP), CycL9)}, {CycLO=CycL9}).


object_prep_phrase(Event, Obj, CycL0, CycL9) --> noun_unit(Type, Event, Obj, CycL0, CycL5),
            thePrep(Event2, P), noun_unit(Type, Event2, Subj, thereExists(Event2, and(isa(Event2, 'Situation'), actorIn(Event2, Subj))), CycL9), {!}.
object_prep_phrase(Event, Obj, CycL0, CycL9) --> noun_unit(Type, Event, Obj, CycL0, CycL9).
object_prep_phrase(Event, Obj, CycL0, and(CycL0, CycL)) --> sent(Event, Obj, CycL).
object_prep_phrase(Event, Obj, CycL0, CycL0)-->[].



% :-ignore((predicate_property(aux(_, _, _), PP), writeq(PP), nl, fail)).
auxV(_, [], _):-!, fail.
auxV(AVP) --> thePOS(AVP, 'Modal').
auxV(AVP) --> thePOS(AVP, 'AuxVerb').


adv(_, [], _):-!, fail.
adv('Adverb2Fn'(AVP, AVP1))-->thePOS(AVP, 'Adverb'), conj(CC), thePOS(AVP1, 'Adverb'), {!}.
adv('AdverbFn'(AVP))-->thePOS(AVP, 'Adverb').


thePrep(_, _, [], _):-!, fail.
thePrep(Event2, P)-->conj(CC), {!}, thePrep(Event2, P).
thePrep(Event2, 'PrepAFn'(P, V))-->auxV(V), {!}, thePrep(Event2, P).
thePrep(Event2, 'PrepJoinFn'(P, P2))-->dcgBoth((thePOS(P, 'Preposition'), thePOS(P2, 'Preposition')), theName(Event2)).
thePrep(Event2, 'PrepositionFn'(P))-->dcgBoth((thePOS(P, 'Preposition')), theName(Event2)).

conj(_, [], _):-!, fail.
conj(CC)-->theWord('As-TheWord'), theWord('Well-TheWord'), theWord('As-TheWord').
conj(CC)-->conj1(_), conj1(_), conj1(CC).
conj(CC)-->conj1(_), conj1(CC).
conj(CC)-->conj1(CC).

conj1(CC)-->theText([(', ')]).
conj1(CC)-->thePOS(CC, 'CoordinatingConjunction').




declaritive_sentence(Subj, Event, Obj, and(CycL, HowDoes)) --> theFrame(Event, 'Pre-NounPhraseModifyingFrame', _, Template),
    declaritive_sentence(Subj, Event, Obj, CycL), {substEach(Template, [':SUBJECT'-Subj, ':NOUN'-Subj, ':ACTION'-Event, ':OBJECT'-Obj], HowDoes)}.

declaritive_sentence(Subj, Event, Obj, CycL9) -->  theWord('From-TheWord'), {!}, noun_unit(Type, Event, Target, and(CycL1, 'from-Underspecified'(Target, Obj)), CycL9), theText([(, )]),
                 declaritive_sentence(Subj, Event, Obj, CycL1).

declaritive_sentence(Subj, Event, Obj, CycL) --> noun_unit(Type, Event, Subj, CycL1, CycL), verb_phrase(Time, Subj, Obj, Event, CycL1).
%declaritive_sentence(Subj, Event, Obj, CycL) --> noun_unit(Type, Event, Subj, and(CycL1, CycL2), CycL),
%declaritive_sentence(Subj, Event, Obj, CycL) --> theWord('The-TheWord'), trans2_verb_unit(Type, Subj, Event, Obj, VProp), possible_prep, noun_unit(Type, Event, Obj, VProp, CycL1), theWord('Be-TheWord'), noun_unit(Type, Event, Subj, CycL1, CycL).
%declaritive_sentence(Subj, Event, Obj, CycL) --> noun_unit(Type, Event, Obj, VProp, CycL1), theWord('The-TheWord'), trans2_verb_unit(Type, Subj, Event, Obj, VProp), possible_prep, noun_unit(Type, Event, Obj, VProp, CycL1), theWord('Be-TheWord'), noun_unit(Type, Event, Subj, CycL1, CycL).

declaritive_sentence(Subj, Event, Obj, CycL) -->{!}, sent(Event, Subj, CycL).

possible_prep-->[of];[].

inquiry(Event, 'thereExists'(Actor, CycL)) --> wh_pronoun(Actor), verb_phrase(Time, Actor, Obj, Event, CycL), theText([?]), {suggestVar('Event', Event)}.
inquiry(Event, CycL) --> inv_sentence(Event, CycL), theText([?]).
%inquiry(CycL) --> declaritive_sentence(Subj, Event, Obj, CycL), theText([?]).

inv_sentence(Event, holdsIn(Time, CycL)) --> verbal_aux(Time), utterance_sem(Event, CycL).

verbal_aux(Time) --> aux_phrase(Time, Subj, Obj, Event, CycL).

%WHAdverb aux_phrase(Time, Subj, Obj, Event, CycL),
wh_pronoun('?Who') --> theWord('Who-TheWord').
wh_pronoun('?What') --> theWord('What-TheWord').


% =======================================================
% DCG Tester
% =======================================================
testPhrase(Dcg, English):-
         sentenceTagger(English, Tagged), dumpList(Tagged),
         phrase(Dcg, Tagged, Left),
         nl, nl, writeq(left),
         nl, dumpList(Left).

% =======================================================
% Rel Clauses
%  see http://en.wikipedia.org/wiki/Relative_clause
% =======================================================

whomThatWhich(Word)-->dcgAnd(dcgOr([theTerm('Backreference-ClassB-NLAttr'), theWord('Who-TheWord'), thePOS('ExpletivePronoun'),
                            theWord('Whom-TheWord'), theWord('Which-TheWord'), theWord('Whose-TheWord'), theWord('That-TheWord')]), theWord(Word)), {!}.

% kill driving
notPresentParticiple --> dcgAnd(thePOS('Verb'), (theForm('presentParticiple'))), {!, fail}.
% drove
notPresentParticiple --> dcgAnd(thePOS('Verb'), dcgNot(theForm('presentParticiple'))).
% is driving
notPresentParticiple --> thePOS('BeAux'), presentParticiple.
% can drive
notPresentParticiple --> thePOS('Modal'), notPresentParticiple.
% the driver
notPresentParticiple --> thePOS('Determiner-Central'), thePOS('AgentiveNoun').
notPresentParticiple --> thePOS('regularAdverb'), notPresentParticiple.

% kill drove
presentParticiple --> dcgAnd(thePOS('Verb'), dcgNot(theForm('presentParticiple'))), {!, fail}.
% driving
presentParticiple --> dcgAnd(thePOS('Verb'), theForm('presentParticiple')).
% that ..
presentParticiple --> whomThatWhich(Word), {!, true}.
% that [ drove | is driving ]
presentParticiple --> whomThatWhich(Word), notPresentParticiple.
% quickly driving
presentParticiple --> thePOS('regularAdverb'), presentParticiple.
% from Nantucket
presentParticiple --> thePOS('Proposition').

% DCG circuit
relationl_clause_head(_, [], _):-!, fail.

% driving
% that drove
% that is driving
% that can drive
% who is driving
% whom drives
% the driver
% who is the driver
% that is from dover
relationl_clause_head --> presentParticiple.
% named Bilbo
%  ... todo ...
% whose daughter is ill
% than whom I am smaller
% to whom I have written


% DCG circuit
rel_clause_sem(Event, Subj, SubjObjectCycL, [], _):-!, fail.

% of stuff
% on a shelf with a ball
rel_clause_sem(Event, Subj, SubjObjectCycL) -->
    theFrame(Event, 'Post-NounPhraseModifyingFrame', _, Template),
      noun_unit(Type, Event, Obj, HowDoes, SubjObjectCycL),
   {substEach(Template, [':SUBJECT'-Subj, ':NOUN'-Subj, ':ACTION'-Event, ':OBJECT'-Obj, ':OBLIQUE-OBJECT'-Obj], HowDoes)}.

% that Verb Phrase
rel_clause_sem(Event, Subj, CycL9) --> theTerm('Backreference-ClassB-NLAttr'), verb_phrase(Time, Subj, Obj, Event, CycL9).

% who Verb Phrase
rel_clause_sem(Event, Subj, More) --> dcgAnd(thePOS('wps'), pronoun(Subj, CycL9, More)), verb_phrase(Time, Subj, Obj, Event, CycL9).
% named Benji
rel_clause_sem(Event, Subj, 'and'(IsaCycL, ProperNameCycL)) --> theWord('Name-TheWord'), proper_name(Subj, ProperNameCycL).

% on the shelf
%rel_clause_sem(Event, Subj, SubjIsaCycL, 'rpand'(SubjIsaCycL, preactors(Event, Obj))) --> prepositional_noun_phrase(Event, Subj, Obj, SubjIsaCycL, Prep).

% needs p2c("kissed it", (verb_phrase(X, Y, Z, A, B), dcgWordList(List))).
% needs p2c("driving southward", (verb_phrase(X, Y, Z, A, B), dcgWordList(List))).

% sitting on the shelf / kissed it
rel_clause_sem(Event, Subj, SubjIsaCycL, 'vpand'(SubjIsaCycL, VPCycL, timeOf(Event, Time))) -->dcgStartsWith(theForm('presentParticiple')),
      verb_phrase(Time, Subj, Obj, Event, VPCycL).

% of Dover
%rel_clause_sem(Event, Subj, CycL9)-->theWord('Of-TheWord'), noun_unit(Type, Event2, Subj, thereExists(Event2, and(isa(Event2, 'Situation'), actorIn(Event2, Subj))), CycL9), {!}.
rel_clause_sem(Event, Subj, 'QueryFn'(That, CycL2))-->thePOS(That, 'WHAdverb'), {!}, sent(Event, Subj, CycL2), {!}.
%rel_clause_sem(Event, Subj, CycL0, whp(DE), V) -->thePOS(DET, 'WHPronoun'), {!}, verb_unit(Type, V).
%rel_clause_sem(Event, Subj, 'WhDeterminerFn'(Subj, Obj, CycL9))-->thePOS(That, 'Determiner'), vp1(Event, Subj, Obj, CycL9), {!}.
%rel_clause_sem(Event, Subj, and(isa(Event2, That), 'ThatFn'(That, Subj, Obj, CycL9)))-->thePrep(Event2, That), vp1(Event2, Subj, Obj, CycL9), {!}.


%verb_intransitive(Time, Subj, Obj, Event, 'and'('bodilyDoer'(Subj, Event), 'isa'(Event, actOf(paint)))) --> [paints].

%rel_clause_sem(Event, Subj, CycL0, 'and'(CycL0, HowDoes)) --> theWord('That-TheWord'), verb_phrase_premods(Time, Subj, Obj, Event, Does, HowDoes), verb_phrase(Time, Subj, Obj, Event, Does).

% =======================================================
% Nouns Phrases
% =======================================================

theRestText(Text) -->theText(Text1), theRestText1(Text2), {flatten([Text1, Text2], Text), !}.
theRestText1(Text) -->theText(Text1), theRestText1(Text2), {flatten([Text1, Text2], Text), !}.
theRestText1([]) -->[].

% =======================================================
% Nouns Units Each
% =======================================================

% p2c("$ 1", noun_unit(Type, Event, Subj, CycLIn, CycLOut)).
% p2c("my $ 1", noun_unit(Type, Event, Subj, CycLIn, CycLOut)).
% p2c("my car", noun_unit(Type, Event, Subj, CycLIn, CycLOut)).
% p2c("I", noun_unit(Type, Event, Subj, CycLIn, CycLOut)).
% p2c("me", noun_unit(Type, Event, Subj, CycLIn, CycLOut)).
% p2c("large number of birds", noun_unit(X, Y, Z, A, B)).
% p2c("United States of America", noun_unit(X, Y, Z, A, B)).
% p2c("United States of Americana", noun_unit(X, Y, Z, A, B)).
% p2c("the United States of America", noun_unit(X, Y, Z, A, B)).

%textCached([fond], [frame, adjSemTrans, 'PPCompFrameFn'('TransitivePPFrameType', 'Of-TheWord'), 'Adjective', feelsTowardsObject(':NOUN', ':OBLIQUE-OBJECT', 'Affection', positiveAmountOf)]).


%textCached([fond], [frame, adjSemTrans, 'PPCompFrameFn'('TransitivePPFrameType', 'Of-TheWord'), 'Adjective', feelsTowardsObject(':NOUN', ':OBLIQUE-OBJECT', 'Affection', positiveAmountOf)]).

%textCached([of], [frame, prepSemTrans, 'Post-NounPhraseModifyingFrame', 'Preposition', possessiveRelation(':OBJECT', ':NOUN')]).

%asNounUnit(ObjType, T1, D1, W1, Event, Subj, CycLO, CycL):-append([A|Djs], [Noun], D1),
%         asNounQualifiers([A|Djs], Event, Subj, CycLQ),
%         asNoun([Noun], Event, Subj, CycLN).

asNounUnit(ObjType, T1, D1, W1, Event, Subj, CycLO, CycL):-phrase(noun_unit(T1, Event, Subj, CycLO, CycL), D1), !.
asNounUnit(ObjType, T1, D1, W1, Event, Subj, CycLO, and(CycLO, 'doom:descriptionStrings'(Subj, string(W1)))):-!.

noun_unit(Adjective, Event, Subj, CycLIn, CycLO, [], _) :-!, fail.

% this prefers to cut noun units shorter than "offered" at least once "not presentParticiple" because we want relatinal clauses
noun_unit(Type, Event, Subj, CycLIn, CycLO, S, [Verb|Rest]) :-
   scanDcgUntil(dcgAnd( thePOS('Verb'), dcgNot(theForm('presentParticiple'))), Before, S, [Verb|Rest]),
   phrase(noun_unit(Type, Event, Subj, CycLIn, CycLO1), Before, []).

% p2c("at least three cars drove south", (noun_unit(X, Y, Z, A, B), dcgWordList(List))).
% p2c("at least three cars driving south", (noun_unit(X, Y, Z, A, B), dcgWordList(List))).
% p2c("the boy than whom I am smaller", (noun_unit(X, Y, Z, A, B), dcgWordList(List))).
% p2c("the boy whose daughter is ill", (noun_unit(X, Y, Z, A, B), dcgWordList(List))).
% p2c("the boy to whom I have written", (noun_unit(X, Y, Z, A, B), dcgWordList(List))).
noun_unit(Type, Event, Subj, CycLIn, CycLO, [S, T|ART], E) :-
   scanDcgUntil(relationl_clause_head, Before, [S, T|ART], [Verb|Rest]),
   phrase(noun_unit(Type, Event, Subj, CycLIn, CycLO1), Before, []),
   (rel_clause_sem(Event, Subj, CycLO2, [Verb|Rest], E)
     ->
     CycLO = and(CycLO1, CycLO2)
     ;
     (CycLO = CycLO1, E=[Verb|Rest])).

noun_unit(Type, Event, Subj, CycLIn, CycLO, S, E) :-
   scanDcgUntil(thePOS('Preposition'), Before, S, [Verb|Rest]),
   phrase(noun_unit(Type, Event, Subj, CycLIn, CycLO1), Before, []),
   (rel_clause_sem(Event, Subj, CycLO2, [Verb|Rest], E)
     ->
     CycLO = and(CycLO1, CycLO2)
     ;
     (CycLO = CycLO1, E=[Verb|Rest])).

noun_unit('Adjective', Event, Subj, CycLIn, CycLO) -->
         theFrame(Event, 'PPCompFrameFn'('TransitivePPFrameType', Prep), 'Adjective', Template), thePrepWord(Prep),
         {substEach(Template, [':SUBJECT'-Subj, ':NOUN'-Subj, ':ACTION'-Event, ':OBJECT'-Obj, ':INF-COMP'-Obj, ':OBLIQUE-OBJECT'-Obj], VerbCycL)},
         noun_unit(Type, Event, Obj, and(CycLIn, VerbCycL), CycLO).

% p2c("two books on a shelf", (noun_unit(X, Y, Z, A, B), dcgWordList(List))).
noun_unit(Type, Event, Subj, CycLIn, CycLO, S, E) :-
   scanDcgUntil(thePOS('Preposition'), Before, S, [Prep|Rest]),
   phrase(noun_unit(Type, Event, Subj, CycLIn, CycLO1), Before, []),
   (prepositional_noun_phrase(Event, Subj, Target, CycLO2, PrepWord, [Prep|Rest], E)
     ->
     CycLO = and(CycLO1, CycLO2)
     ;
     (CycLO = CycLO1, E=[Prep|Rest])).


%noun_unit('DET', Event, Subj, CycLIn, CycLO) -->


%noun_unit(Type, Event, Subj, CycL5, and(possessiveRelation(DET, Subj), CycL9)) -->thePOS(DET, 'Possessive'), {!}, noun_unit(Type, Event, Subj, CycL5, CycL9).

possessor_phrase(Owner, Owned, CycL5, CycL) --> dcgStartsWith(thePOS('Possessive')), subject_noun(Event, Owner, CycL5, CycL).
possessor_phrase(Owner, Owned, CycL5, CycL) --> subject_noun(Event, Owner, CycL5, CycL), theGText('\''), theGText('s').

noun_unit(Type, Event, Subj, CycL5, and(possessiveRelation(Owner, Subj), CycL9)) -->possessor_phrase(Owner, Subj, CycL8, CycL9), noun_unit(Type, Event, Subj, CycL5, CycL8).


% $100
noun_unit(Type, Event, Subj, CycLIn, and(equals(Subj, 'DollarFn'(Num)), CycLIn)) --> dollarSign, decimalNumber(Num).
% non number pronoun "I"
noun_unit(Type, Event, Subj, CycLIn, CycL9, [S|Data], E) :- phrase(thePOS('Pronoun'), [S]), not(phrase(theIsa(Subj, _, 'Number-General'), [S])), pronoun_unit(Type, Event, Subj, CycLIn, CycL9, [S|Data], E), !.
% det noun
noun_unit(Type, Event, Subj, CycLIn, CycL9, [S|Data], E) :- /*phrase(thePOS('Determiner'), [S]), */det_noun_unit(Type, Event, Subj, CycLIn, CycL9, [S|Data], E), !.
% he/BillClinton/joe's her
noun_unit(Type, Event, Subj, CycLIn, implies(CycL, CycLIn)) --> nondet_noun_unit(Type, Event, Subj, CycL).
% green thingy-like substance
noun_unit(Type, Event, Subj, CycLIn, implies(CycL, CycLIn)) --> adjective_noun_unit(Type, Event, Subj, CycL).

%noun_unit(Type, Event, Subj, CycLIn, and(equals(Subj, 'DollarFn'(Num)), CycLIn)) --> {!, fail}.

%noun_unit(Type, Event, Subj, CycL5, whp(DET, CycL9)) -->thePOS(DET, 'WHPronoun'), {!}, vp(Event, Subj, Obj, CycL5, CycL9).
%& TODO noun_unit(Type, Event, Subj, CycL5, implies(thereExists(Event2, and(actorsIn(Event2, DET, Subj), CycL9)), CycL5)) -->thePOS(DET, 'WHPronoun'), vp(Event2, Subj, Obj, CycL9).
%noun_unit(Type, Event, Subj, CycL5, and(isa(Subj, 'QuantifierFn'(DET)), CycL9)) -->thePOS(DET, 'Quantifier'), {!}, noun_unit(Type, Event, Subj, CycL5, CycL9).
noun_unit(Type, Event, Subj, CycL5, forAll(Subj, CycL9)) -->thePOS(DET, 'Quantifier'), noun_unit(Type, Event, Subj, CycL5, CycL9).
%noun_unit(Type, Event, Subj, CycL5, and(isa(Subj, 'NumberFn'(DET)), CycL9)) -->thePOS(DET, 'Number'), {!}, noun_unit(Type, Event, Subj, CycL5, CycL9).
noun_unit(Type, Event, Subj, CycL5, thereExistExactly(DET, Subj, CycL9)) -->thePOS(DET, 'Number'), noun_unit(Type, Event, Subj, CycL5, CycL9).
%noun_unit(Type, Event, Subj, CycL5, and(isa(Subj, 'DeterminerFn'(DET)), CycL9)) -->thePOS(DET, 'Determiner'), {!}, noun_unit(Type, Event, Subj, CycL5, CycL9).
noun_unit(Type, Event, Subj, CycL5, thereExists(Subj, CycL9)) -->thePOS(DET, 'Determiner'), noun_unit(Type, Event, Subj, CycL5, CycL9).
%noun_unit(Type, Event, Subj, CycL5, and(isa(Subj, 'AdjectiveFn'(DET)), CycL9)) -->thePOS(DET, 'Adjective'), {!}, noun_unit(Type, Event, Subj, CycL5, CycL9).
noun_unit(Type, Event, Subj, CycL5, and('isa'(Subj, 'AdjectiveFn'(DET)), CycL9)) -->thePOS(DET, 'Adjective'), noun_unit(Type, Event, Subj, CycL5, CycL9).
noun_unit(Type, Event, Subj, CycL5, CycL) -->subject_noun(Event, Subj, CycL5, CycL).

%noun_unit(Type, Event, Subj, CycLIn, CycL9) --> dcgAnd(scanNounUnit, noun_phrase_units(Event, Subj, CycL0, CycL9)), {!}, rel_clause_sem(Event, Subj, CycLIn, CycL0), {!}.

%subject_noun(Event, Subj, CycL5, and(isa(Subj, 'PronounFn'(DET)), CycL5)) -->thePOS(DET, 'Pronoun'), {!}.
%subject_noun(Event, Subj, CycL5, and(isa(Subj, 'ProperNounFn'(DET)), CycL9)) -->thePOS(DET, 'ProperNoun'), {!}, noun_unit(Type, Event, Subj, CycL5, CycL9).
subject_noun(Event, Subj, CycL5, and(isa(Subj, 'ProperNounFn'(DET, PN)), CycL5)) -->thePOS(DET, 'ProperNoun'), dcgAnd(thePOS(PN, 'Noun'), theName(Subj)).
%subject_noun(Event, Subj, CycL5, and(isa(Subj, 'NounFn'(PN)), CycL5)) -->thePOS(PN, 'Noun').

subject_noun(Event, Subj, CycL5, np_and(CycL, CycL5)) -->dcgAnd(dcgTemplate('NPTemplate', VarName, CycL), theName(Subj)).

% last resort
subject_noun(Event, Subj, CycL5, and(isa(Subj, 'NounFn'(PN)), CycL5)) -->dcgBoth(thePOS(PN, 'Noun'), theName(Subj)).


% =======================================================
% Proper Noun Phrases as nondet_noun_unit
% =======================================================

% his thing
nondet_noun_unit(Type, Event, Subj, CycL9) -->
         dcgSeq(dcgAnd(thePOS('Possessive'), theName(HIS)), noun_unit(Type, Event, Subj, possesiveRelation(HIS, Subj), CycL9)).

% President Clinton
nondet_noun_unit(Type, Event, Subj, and(CycL1, CycL2)) --> person_title(Subj, CycL1), proper_name(Subj, CycL2).

% Clinton
nondet_noun_unit(Type, Event, Subj, CycL) --> proper_name(Subj, CycL).

% =======================================================
% Proper Noun Phrases as proper_name
% =======================================================

% Bill Clinton
proper_name(Subj, equals(Subj, 'BillClinton')) --> theTerm('BillClinton').

% Texas
proper_name(Subj, equals(Subj, CycLTerm)) -->theIsa(Subj, CycLTerm, 'Agent-Generic').

% Adjectives that are capitalized
proper_name(Subj, _)-->thePOS('Adjective'), {!, fail}.

% Van Morison
proper_name(Subj, properNameString(Subj, string(Name))) --> dcgSeq(capitalized(Text1), capitalized(Text2)), {suggestVar(Text1, Subj), flatten([Text1, Text2], Name)}.
% Fido
proper_name(Subj, properNameString(Subj, string(Name))) --> capitalized(Name), {suggestVar(Name, Subj)}.

% president
person_title(Subj, Term)--> theIsa(Subj, Term, 'PersonTypeByActivity').

% =======================================================
% Pronoun Phrases as noun_unit
% =======================================================

% Her His Your Our Their
pronoun_unit(Type, Event, Subj, CycL, and(controls(HIS, Subj), CycL9)) --> %                   {true}
      dcgSeq(dcgAnd(thePOS('Possessive'), thePOS('Pronoun'), pronoun(HIS, CycLI, CycL9)), noun_unit(Type, Event, Subj, CycL, CycLI)).
% My
pronoun_unit(Type, Event, Subj, CycL, and(controls(HIS, Subj), CycL9)) --> %                   {true}
      dcgSeq(dcgAnd(theWord('My-TheWord'), pronoun(HIS, CycLI, CycL9)), noun_unit(Type, Event, Subj, CycL, CycLI)).

pronoun_unit(Type, Event, Subj, CycLIn, CycL9, [S|Data], E) :- not(phrase(thePOS('Postdeterminer'), [S])), !, nonpostdet_pronoun_unit(Type, Event, Subj, CycLIn, CycL9, [S|Data], E).

% Him She They
nonpostdet_pronoun_unit(Type, Event, Subj, CycL, thereExists(Subj, CycL9)) --> pronoun(Subj, CycL, CycL9).


pronoun_unit(Type, Event, Subj, CycL, thereExists(Subj, CycL9)) --> pronoun(Subj, CycL, CycL9).

pronoun(Subj, CycL, and(isa(Subj, 'Person'), CycL))  --> theWord('I-TheWord'), {suggestVarI('Speaker', Subj)}.
pronoun(Subj, CycL, and(isa(Subj, 'Person'), CycL))  --> theWord('You-TheWord'), {suggestVarI('TargetAgent', Subj)}.
%pronoun(Subj, CycL, and(isa(Subj, 'Person'), CycL))  --> theWord('They-TheWord'), {suggestVarI('TargetAgent', Subj)}.
pronoun(Subj, CycL, and(isa(Subj, 'Male'), CycL))  --> theWord('He-TheWord'), {suggestVarI('he', Subj)}.
pronoun(Subj, CycL, and(isa(Subj, 'Female'), CycL))  --> theWord('She-TheWord'), {suggestVarI('she', Subj)}.
pronoun(Subj, CycL, and(isa(Subj, 'Artifact-NonAgentive'), CycL))  --> theWord('It-TheWord'), {suggestVarI('TargetThing', Subj)}.
%pronoun(Subj, CycL, and(Constraints, CycL)) --> dcgAnd(thePOS(Word, 'IndefinitePronoun'), theConstraints(Subj, Constraints)).
pronoun(Subj, CycL, and(Constraints, CycL)) -->  theText([Text]), {pronounConstraints([Text], Subj, Constraints)}.

pronounConstraints(Text, Subj, equals(Subj, nart(Eq))):-textCached(Text, [denotation, Pos, nart(Eq)|Types]), suggestVarI(Text, Subj), !.
pronounConstraints(Text, Subj, CycL):-constraintsOnIsa(Text, Subj, CycL), !.

theConstraints(Subj, isa(Subj, ColType)) --> theText([Text]), {constraintsOnIsa([Text], Subj, ColType)}.

constraintsOnIsa(Text, Subj, CycL):-
      suggestVarI(Text, Subj),
      findall(and(equals(Subj, Eq), isa(Subj, Cols)), (textCached(Text, [denotation, Pos, Eq|Types]), joinCols('CollectionUnionFn', Types, Cols)), [IS|AS]),
      CycL=..['#$or'|[IS|AS]].


% =======================================================
% Quantities (DET Phrases)
% =======================================================
decimalNumber(Num) --> wholeNumber(Subj, Num1), dotSign, wholeNumber(Subj, Num2), {concat_atom([Num1, '.', Num2], Atom), atom_number(Atom, Num)}.
decimalNumber(Num) --> wholeNumber(Subj, Num).
wholeNumber(Subj, Num) --> theText([Num]), {number(Num), !}.
wholeNumber(Subj, 2) --> theText([two]).
wholeNumber(Subj, 2) --> theText([two]).
wholeNumber(Subj, 1) --> theText([one]).
wholeNumber(Subj, 1) --> theText([a]).
wholeNumber(Subj, Num) --> dcgOr(theIsa(Subj, Num, 'Numeral'), theIsa(Subj, Num, 'Number-General')), {throwOnFailure(number(Num))}.
dollarSign --> thePOS('$').
dotSign --> thePOS('.').

% =======================================================
% Quantification (DET Phrases)
%
% p2c("all cars", noun_unit(X, Y, Z, A, B)).
% p2c("many cars", noun_unit(X, Y, Z, A, B)).
% p2c("large numbers of cars", noun_unit(X, Y, Z, A, B)).
% p2c("a large number of cars", noun_unit(X, Y, Z, A, B)).
% p2c("two cars", noun_unit(X, Y, Z, A, B)).
% p2c("at least two cars", noun_unit(X, Y, Z, A, B)).
% p2c("all", quant_phrase(Subj, Prop1, CycL1, CycL)).
%
% TODO Negations (no fewer than)
% =======================================================
%quant_phrase(Subj, Pre, Post, CycL) --> quant_phrase1(Subj, Pre, Mid, CycL), quant_phrase(Subj, Mid, Post, CycL).




quant_phrase(Subj, Restr, Scope, CycL9)--> dcgOr(dcgOr(existential_words, universal_words(_)), dcgNone), quant_phraseN(Subj, Restr, Scope, CycL9).



quant_phrase(Subj, Restr, Scope, 'thereExists'(Subj, 'and'(Restr , Scope))) --> existential_words.

quant_phrase(Subj, Restr, Scope, 'forAll'(Subj, 'implies'(Restr , Scope))) --> universal_words(_).


quant_phrase(Subj, Restr, Scope, CycL9) --> theFrame(_, 'QuantifierFrame', _, Template),
      {substEach(Template, [':SCOPE'-Scope, ':NOUN'-Subj, ':RESTR'-Restr], CycL9)}.

quant_phrase(Subj, Restr, Scope, and(Restr, Scope)) --> [].

quant_phraseN(Subj, Restr, Scope, 'thereExistExactly'(Num, Subj, 'and'(Restr , Scope))) -->  wholeNumber(Subj, Num).


quant_phraseN(Subj, Restr, Scope, 'thereExistAtLeast'(Num, Subj, 'and'(Restr , Scope))) -->
         dcgSeq(at_least_words(N), wholeNumber(Subj, Num1)), {Num is N+Num1}.

%p2c("large number of", quant_phrase(Subj, Prop1, CycL1, CycL)).
quant_phraseN(Subj, Restr, Scope, and(largeNumber(Restr), Scope)) --> theText('large'), theWord('Number-TheWord'), theText('of').

% at least
at_least_words(0) --> theWord('At-TheWord'), theWord('Little-TheWord').
at_least_words(1) --> theWord('More-Than-MWW').
at_least_words(1) --> theWord('Greater-Than-MWW').

quant_phraseN(Subj, Restr, CycL1, 'thereExistAtMost'(Num, Subj, 'and'(Restr , CycL1))) -->
         dcgSeq(at_most_words(N), wholeNumber(Subj, Num1)), {Num is N+Num1}.

at_most_words(0) --> dcgSeq(theWord('At-TheWord'), dcgOr(theWord('Most-TheWord'), theTerm('Most-NLAttr'))).
at_most_words(-1) --> theWord('Less-Than-MWW').
at_most_words(-1) --> theWord('Few-TheWord'), theWord('Than-TheWord').


existential_words --> existential_word, existential_word.
existential_words --> existential_word.
existential_word --> theWord('A-TheWord');theWord('An-TheWord');theWord('The-TheWord');theWord('Some-TheWord').
% there is, there are
existential_word --> theWord('There-TheWord'), theWord('Be-TheWord').
existential_word --> theWord('There-TheWord'), theWord('Exist-TheWord'). % there exists

universal_words(S) --> universal_word(_), universal_word(S).
universal_words(S) --> universal_word(S).

universal_word(plural) --> theWord('All-TheWord'), dcgOptional(existential_word).
% every
universal_word(singular) --> theTerm('Every-NLAttr').
universal_word(singular) --> theWord('Every-TheWord');theWord('Each-TheWord').
universal_word(plural) --> theWord('For-TheWord'), theWord('All-TheWord').
universal_word(plural) --> theText([forAll]).
universal_word(plural) --> theText([forall]).

% =======================================================
% Count and Mass Nouns as det_noun_unit
% =======================================================


/*


% det_noun_unit(Type, Event, Subj, CycL, thereExists(Subj, and(isa(Subj, CycL0), CycL))) --> dcgAnd(scanNounUnit, theIsa(Subj, CycL0, 'SpatialThing-Localized'), theIsa(Subj, CycLTerm, 'Collection')).

% the green party
det_noun_unit(Type, Event, Subj, equals(Subj, CycLTerm)) --> dcgOptional(theWord('The-TheWord')), proper_noun(CycLTerm).
% the usa, the green party
proper_noun(CycLTerm)--> dcgAnd(scanNounUnit, theIsa(Subj, CycLTerm, 'SpatialThing-Localized'), theIsa(Subj, CycLTerm, 'Individual')).
% the President
det_noun_unit(Type, Event, Subj, isa(Subj, CycLTerm)) --> dcgOptional(theWord('The-TheWord')), person_title(Subj, CycLTerm).

%% dog, person    AuxVerb
det_noun_unit(Type, Event, Subj, CycL, and(CycL, isa(Subj, Type))) --> theIsa(Subj, Type, 'StuffType').

*/

%det_noun_unit(Type, Event, Subj, CycLVerbInfo, ('thereExists'(Subj, 'and'(AttribIsa, CycLVerbInfo)))) -->
%   adjectives_phrase(Event, Subj, AttribIsa1, AttribIsa), collection_noun(Event, Subj, CycL0), adjectives_phrase(Event, Subj, CycL0, AttribIsa1).
%det_noun_unit(Type, Event, Subj, CycL1, CycL) -->  quant_phrase(Subj, Prop12, CycL1, CycL), collection_noun(Event, Subj, Prop1), rel_clause_sem(Event, Subj, Prop1, Prop12).
%det_noun_unit(Type, Event, Subj, CycL1, CycL) -->  quant_phrase(Subj, Prop1, CycL1, CycL), collection_noun(Event, Subj, Prop1).

det_noun_unit(Type, Event, Subj, CycLIn, CycL9) --> %{trace},
         quant_phrase(Subj, AttribIsa, CycLIn, CycL9), {!},
         adjective_noun_unit(Type, Event, Subj, AttribIsa).

%adjective_noun_unit(Type, Event, Subj, AttribIsa) --> collection_noun(Event, Subj, IsaInfo), thePOS('Preposition-Of'), noun_unit(Type, Event, Subj, IsaInfo, AttribIsa).

adjective_noun_unit(Type, Event, Subj, AttribIsa) --> {true},
            adjectives_phrase(Event, Subj, IsaInfo, AttribIsa),
            collection_noun(Event, Subj, IsaInfo).

% =======================================================
% Adjective Phrases
% =======================================================

conjuntive_and --> theText([and]).
conjuntive_and --> theText([(', ')]).

adjectives_phrase(Event, Subj, CycL, CycL9) --> thePOS('Preposition'), {!, fail}.
adjectives_phrase(Event, Subj, CycL, CycL9) --> dcgAnd(thePOS('Adjective'), adjective_word(Subj, CycL, CycLO1)), conjuntive_and, adjective_word(Subj, CycLO1, CycL9).
adjectives_phrase(Event, Subj, CycL, CycL9) --> adjective_word(Subj, CycL, CycL1), adjectives_phrase(Event, Subj, CycL1, CycL9).
adjectives_phrase(Event, Subj, CycL, CycL) --> [].

%adjective_word(Subj, CycL, and(CycL, equals(':POSSESSOR', PronounIsa), controls(PronounIsa, Subj))) --> dcgAnd(dcgAnd(thePOS('PossessivePronoun'), thePOS('Pronoun')), theIsa(Pro, PronounIsa, 'Individual')).
adjective_word(Subj, CycL, and(CycL, isa(Subj, AttribProp))) --> theIsa(Subj, AttribProp, 'ChromaticColor').
adjective_word(Subj, CycL, and(CycL, isa(Subj, AttribProp))) --> theIsa(Subj, AttribProp, 'FirstOrderCollection').
adjective_word(Subj, CycL, and(CycL, NPTemplate)) --> theFrame(_, _, 'Adjective', Template), {substEach(Template, [':NOUN'-Subj, ':REPLACE'-Subj, ':SUBJECT'-Subj], NPTemplate)}.
adjective_word(Subj, CycL, and(CycL, controls('?Speaker', Subj))) --> theWord('My-TheWord').
adjective_word(Subj, CycL, and(CycL, controls(PronounIsa, Subj))) --> dcgAnd(thePOS('PossessivePronoun'), theIsa(Pro, PronounIsa, 'Individual')).
adjective_word(Subj, CycL, and(CycL, isa(Subj, AttribProp))) --> dcgAnd(thePOS('Possessive'), theIsa(Subj, AttribProp, 'Collection')).
adjective_word(Subj, CycL, and(CycL, isa(Subj, AttribProp))) --> dcgAnd(thePOS('Adjective'), theIsa(Subj, AttribProp, 'Collection')).


% =======================================================
% Qualified Noun
% =======================================================

% dont chase prepositions
collection_noun(Event, Subj, CycL) --> thePOS('Preposition'), {!, fail}.

%':POSSESSOR'
% the eaters of the dead
collection_noun(Event, Subj, and(HowDoes, occursBefore(PreEvent, Event))) --> dcgAnd(theFrame(Subj, 'GenitiveFrame', 'AgentiveNoun', Template), theName(Subj)),
  {substEach(Template, [':SUBJECT'-Subj, ':NOUN'-Subj, ':ACTION'-Event], HowDoes), suggestVar(['GenitiveFrame'], PreEvent),
     ignore(sub_term(performedBy(PreEvent, Subj), HowDoes))}.

% the jugglers
collection_noun(Event, Subj, NPTemplate) --> theFrame(Subj, 'RegularNounFrame', WHPronoun, Template),
    {subst(Template, ':NOUN', Subj, NPTemplate)}. %numbervars(NPTemplate, _, _)

%collection_noun(Event, Subj, Event, 'isa'(Subj, CycLCollection)) --> dcgAnd(collection_type(Subj, CycLCollection), scanNounUnit).

% the eaters of the dead
collection_noun(Event, Subj, CycLO) -->
    theFrame(Subj, 'GenitiveFrame', 'CountNoun', Template),
   {substEach(Template, [':SUBJECT'-Subj, ':NOUN'-Subj, ':ACTION'-Event, ':POSSESSOR'-Subj2], HowDoes)},
   theWord('Of-TheWord'),
   noun_unit(Type, Event, Subj2, HowDoes, CycLO).

% the men
collection_noun(Event, Subj, and(isa(Subj, CycLCollection1), isa(Subj, CycLCollection2))) -->
%      dcgSeq(dcgAnd(collection_type(Subj, CycLCollection1), theName(Subj)), collection_type(Subj, CycLCollection2)).
      dcgSeq(collection_type(Subj, CycLCollection1), collection_type(Subj, CycLCollection2)).

collection_noun(Event, Subj, isa(Subj, CycLCollection)) --> dcgAnd(collection_type(Subj, CycLCollection), theName(Subj)), {!}.
collection_noun(Event, Subj, equals(Subj, Type)) --> theTerm(Type).

collection_type(Subj, Type)--> theIsa(Subj, Type, 'StuffType').
collection_type(Subj, Type)--> theIsa(Subj, Type, 'ClarifyingCollectionType').
collection_type(Subj, Type)--> theIsa(Subj, Type, 'SentenceSubjectIndexical').
collection_type(Subj, 'Person') --> theText([person]).
collection_type(Subj, Type)--> theIsa(Subj, Type, 'Collection').
collection_type(Subj, 'NounFn'(Word)) --> theWord(Word, 'Noun'), {!}.
collection_type(Subj, 'AdjectiveFn'(Word)) --> theWord(Word, 'Adjective'), {!}.
collection_type(Subj, 'WordFn'(Word, POS)) --> theWord(Word, POS), {!}.
collection_type(Subj, 'Thing') --> [].
                             %   Individual
%collection_type(Subj, 'InstanceNamedFn'(string(Word), 'WnNoun')) --> thePOS(Word, 'WnNoun').
%collection_type(Subj, 'InstanceNamedFn'(string(SType), 'Collection')) --> [Type], {flatten([Type], SType)}.

%phraseNoun_each(Eng, CycL):-posMeans(Eng, 'SimpleNoun', Form, CycL).
%phraseNoun_each(Eng, CycL):-posMeans(Eng, 'MassNoun', Form, CycL).
%phraseNoun_each(Eng, CycL):-posMeans(Eng, 'AgentiveNoun', Form, CycL).
%phraseNoun_each(Eng, CycL):-posMeans(Eng, 'Noun', Form, CycL).
%phraseNoun_each(Eng, CycL):-posMeans(Eng, 'QuantifyingIndexical', _, CycL).


% =======================================================
% Verb Phrase
% =======================================================
verb_phrase(Time, Subj, Obj, Event, CycL9, [], _):-!, fail.
verb_phrase(Time, Subj, Obj, Event, CycL9) --> [['DO'|More]], {throwOnFailure(phrase(verb_phrase(Time, Subj, Obj, Event, CycL9), More)), !}.

verb_phrase(Time, Subj, Obj, Event, CycLO)--> dcgAnd(thePOS('BeAux'), dcgIgnore(theTense(Time))),
   dcgStartsWith(theForm('presentParticiple')), verb_phrase(Time, Subj, Obj, Event, CycLO).

verb_phrase(Time, Subj, Obj, Event, CycLO)--> dcgAnd(theWord('Do-TheWord'), dcgIgnore(theTense(Time))),
   dcgStartsWith(dcgAnd(thePOS('Verb'), dcgIgnore(theTense(Time)))), verb_phrase(Time, Subj, Obj, Event, CycLO).

%   verb_phrase(Time, Subj, Obj, Event1, CycL1), theText([(, )]), verb_phrase(Time, Subj, Obj, Event, CycL2).
%verb_phrase(Time, Subj, Obj, Event, adv(Mod, CycL9)) -->[['ADVP'|Mod]], verb_phrase(Time, Subj, Obj, Event, mod(Mod, CycL9)).

% p2c("is good", (verb_phrase(X, Y, Z, A, B), dcgWordList(List))).
verb_phrase(Time, Subj, Obj, Event, CycL9) --> dcgAnd(thePOS('BeAux'), dcgIgnore(theTense(Time))), adjectives_phrase(Event, Subj, occursDurring(Event, Time), CycL9), {!}.


verb_phrase(Time, Subj, Obj, Event, CycLO)-->verb_expression(Time, Subj, Obj, Event, CycLO).
verb_phrase(Time, Subj, Obj, Event, CycLO)-->verb_unit(Type, Time, Subj, Obj, Target, Event, CycLO).

%textCached([notice], [frame, verbSemTrans, 'TransitiveFiniteClauseFrame', 'Verb', and(':CLAUSE', notices(':SUBJECT', ':CLAUSE'))]).


% =======================================================
% Verb PRE Modifiers
% =======================================================

verb_phrase_premods(Time, Subj, Obj, Event, ScopeIn, VerbCycL) --> theFrame(Pred, 'VerbPhraseModifyingFrame', Adverb, Template),
   {substEach(Template, [':SUBJECT'-Subj, ':SCOPE'-Scope, ':ACTION'-Event, ':NOUN'-Subj, ':OBJECT'-Obj], VerbCycL)},
   verb_phrase_premods(Time, Subj, Obj, Event, ScopeIn, Scope).

verb_phrase_premods(Time, Subj, Obj, Event, ScopeIn, not(Scope)) --> theWord('Not-TheWord'), {!},
   verb_phrase_premods(Time, Subj, Obj, Event, ScopeIn, Scope).

verb_phrase_premods(Time, Subj, Obj, Event, ScopeIn, and(VerbCycL, Scope)) --> aux_phrase(Time, Subj, Obj, Event, VerbCycL), {!},
   verb_phrase_premods(Time, Subj, Obj, Event, ScopeIn, Scope).

verb_phrase_premods(Time, Subj, Obj, Event, Scope, Scope)-->[].


% =======================================================
% AuxVerbs & Adverbs
% =======================================================
aux_phrase(Time, Subj, Obj, Event, occursDurring(Event, Time)) --> dcgAnd(theWord('Have-TheWord'), dcgIgnore(theTense(Time))).
aux_phrase(Time, Subj, Obj, Event, occursDurring(Event, Time)) --> dcgAnd(thePOS('BeAux'), dcgIgnore(theTense(Time))).
aux_phrase(Time, Subj, Obj, Event, occursDurring(Event, Time)) --> dcgAnd(theWord('Be-TheWord'), dcgIgnore(theTense(Time))).
aux_phrase(Time, Subj, Obj, Event, and(occursDurring(Event, Time), bodilyDoer(Subj, Event))) --> dcgAnd(theWord('Do-TheWord'), dcgIgnore(theTense(Time))).
aux_phrase(Time, Subj, Obj, Event, behavourCapable(Subj, Event)) -->  dcgAnd(theWord('Can-TheModal'), dcgIgnore(theTense(Time))).
%throwOnFailure be modal: aux_phrase(Time, Subj, Obj, Event, behavourCapable(Subj, Event)) -->  dcgAnd(theWord('Can-TheWord'), dcgIgnore(theTense(Time))).
aux_phrase('Past', Subj, Obj, Event, holdsIn('Past', behavourCapable(Subj, Event))) -->  theWord('Could-TheWord').
aux_phrase(Time, Subj, Obj, Event, behavourCapable(Subj, Event)) -->  dcgAnd(thePOS('Modal'), dcgIgnore(theTense(Time))).
%textCached([hardly], [denotation, Pos, 'AlmostNever', 'NonNumericQuantity', 'Frequency', 'Individual']).
aux_phrase(Time, Subj, Obj, Event, isa(Event, Term)) -->dcgAnd(thePOS('Adverb'), theIsa(Event, Term, 'Individual')).
aux_phrase(Time, Subj, Obj, Event, isa(Event, 'AdverbFn'(Word))) --> dcgAnd(thePOS('Adverb'), theWord(Word)).

verb_phrase_postmods(Event, CycL, and(Truth, CycL)) -->aux_phrase(Time, Subj, Obj, Event, Truth).
verb_phrase_postmods(Event, CycL, implies(occursDuring(Event, Mod), holdsIn(Event, CycL))) --> time_phrase(Event, Mod).
verb_phrase_postmods(Event, CycL, CycL) --> [].

verb_postmods(Event, CycL, CycLO)-->verb_phrase_postmods(Event, CycL, CycLO).
%verb_postmods(Event, CycL, CycLO)-->dcgAnd(dcgNot(thePOS('Determiner')), verb_phrase_postmods(Event, CycL, CycLO)).

% Today
time_phrase(Event, Mod) --> theIsa(Event, Mod, 'TimePoint').
% Monday
time_phrase(Event, Mod) --> theIsa(Event, Mod, 'CalendarDay').
time_phrase(Event, Mod) --> theIsa(Event, Mod, 'TemporalObjectType').
time_phrase(Event, Mod) --> theIsa(Event, Mod, 'TimeInterval').


%time_phrase(Event, Mod) -->

% =======================================================
% Particles
% =======================================================
particle_np(Event, Subj, Obj, VerbCycL, VerbObjectCycL, Prep) --> partical_expression(Prep), {!}, noun_unit(Type, Event, Obj, VerbCycL, VerbObjectCycL).
particle_np(Event, Subj, Obj, VerbCycL, VerbObjectCycL, Prep) --> noun_unit(Type, Event, Obj, VerbCycL, VerbObjectCycL), partical_expression(Prep).
%e2c_sem("she took off clothing").

partical_expression(Prep) --> [['PRT'|FORM]], {(phrase(partical_expression_sub(Prep), FORM)), !}.
partical_expression(Prep) --> partical_expression_sub(Prep).

partical_expression_sub(Prep) --> thePrepWord(Prep).

% =======================================================
% Preposition
% =======================================================
prepositional_noun_phrase(Event, Obj, Target, NPCycL, Prep) -->[['PP'|SText]], {throwOnFailure(phrase(prepositional_noun_phrase(Event, Obj, Target, NPCycL, Prep), SText))}.

prepositional_noun_phrase(Event, Obj, Target, NPCycL, Prep) -->
      prepositional(Event, Obj, Target, PrepCycL, Prep),
      noun_unit(Type, Event, Target, PrepCycL, NPCycL).

%textCached([about], [frame, prepSemTrans, 'Post-NounPhraseModifyingFrame', 'Preposition', topicOfInfoTransfer(':ACTION', ':OBLIQUE-OBJECT')]).
prepositional(Event, Obj, Target, PrepCycL, Prep) --> %dcgAnd(thePOS(Prep, 'Preposition'), theFrame(Pred, _, 'Preposition', Template)),
    dcgAnd(thePrepWord(Prep), theFrame(Pred, _, 'Preposition', Template)),
   {substEach(Template, [':SUBJECT'-Obj, ':NOUN'-Obj, ':ACTION'-Event, ':OBJECT'-Target, ':OBLIQUE-OBJECT'-Target], PrepCycL)}.

thePrep(Event2, Word) --> dcgAnd(thePOS('Preposition'), theWord(Word)).
thePrepWord(Prep)-->dcgAnd(thePOS('Preposition'), theWord(Prep)).

% =======================================================
% Verbs/Verb Phrases
% =======================================================


% Two
verb_prep(Event, Subj, Obj, PTAG, Target, VerbObjectCycLO)-->
        theFrame(Event, 'PPCompFrameFn'('TransitivePPFrameType', Prep), 'Verb', Template),
      {phrase(thePrepWord(Prep), PTAG)},
     verb_postmods(Event, VerbCycL, VerbObjectCycLO),
     {substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event, ':OBJECT'-Obj, ':INF-COMP'-Target, ':OBLIQUE-OBJECT'-Target], VerbCycL)}.

% Three
verb_prep(Event, Subj, Obj, PTAG, Target, VerbObjectCycLO)-->
        {phrase(thePrepWord(Prep), PTAG)},
     theFrame(Event, 'PPCompFrameFn'('DitransitivePPFrameType', Prep), 'Verb', Template), %{true},
     verb_postmods(Event, VerbCycL, VerbObjectCycLO),
     {substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event, ':OBJECT'-Obj, ':INF-COMP'-Target, ':OBLIQUE-OBJECT'-Target], VerbCycL)}.


% Three
verb_prep(Event, Subj, Obj, PTAG, Target, and(CycL, VerbCycL))-->
        theFrame(Event, 'DitransitiveNP-InfinitivePhraseFrame', 'Verb', Template), %{true},
        verb_postmods(Event, VerbObjectCycL, VerbObjectCycLO),
     {phrase(thePrepWord(Prep), PTAG)},
        {substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event, ':OBJECT'-Obj, ':INF-COMP'-Target, ':OBLIQUE-OBJECT'-Target], VerbCycL)}.

% Three
verb_expression_resorted(Time, Subj, Obj, Event, CycLO) -->
        theFrame(Event, 'DitransitiveNP-NPFrame', 'Verb', Template), %{true},
        verb_postmods(Event, CycL, CycLO),
        noun_unit(Type, Event, Obj, VerbObjectCycL, CycL),
        dcgOptional(thePOS('Preposition')),
        noun_unit(Type, Event, Target, VerbCycL, VerbObjectCycL),
        {substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event, ':OBJECT'-Obj, ':INF-COMP'-Target, ':OBLIQUE-OBJECT'-Target], VerbCycL)}.

% more frames UnderstoodReciprocalObjectFrame MiddleVoiceFrame

% =======================================================
% Intransitive Verbs + verb_unit
% =======================================================
verb_expression_never(Time, Subj, Obj, Event, and(VerbObjectCycL, CycL)) -->
     verb_intransitive(Time, Subj, Obj, Event, CycL),
     optionalVerbGlue(_AND_OR),
     verb_unit(Type, Time, Subj, Target, Obj, Event, PrepCycL),
     noun_unit(Type, Event, Obj, PrepCycL, VerbObjectCycL).

% Intransitive Verbs One
verb_expression(Time, Subj, Obj, Event, CycLO) -->
     verb_intransitive(Time, Subj, Obj, Event, CycL),
        verb_postmods(Event, CycL9, CycLO),
     intrans_modifiers(CycL, CycL9).

intrans_modifiers(CycL, CycL) --> [].


verb_intransitive(Time, Subj, Obj, Event, and(preActors(Event, Subj), CycL)) -->
      theFrame(Event, 'MiddleVoiceFrame', 'Verb', Template),
      {substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event], CycL)}.

verb_intransitive(Time, Subj, Obj, Event, and(preActors(Event, Subj), CycL)) -->
      theFrame(Event, 'ParticleCompFrameFn'('IntransitiveParticleFrameType', Prep), Verb, Template),
      thePrepWord(Prep), {!, substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event], CycL)}.

verb_intransitive(Time, Subj, Obj, Event, and(preActors(Event, Subj), CycL)) -->
      theFrame(Event, 'IntransitiveVerbFrame', 'Verb', Template),
      {!, substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event], CycL)}.


% =======================================================
% Verbs Expression Resorts
% =======================================================

% Verb resort Two
verb_expression(Time, Subj, Obj, Event, PREPL) -->
   verb_unit(Type, Time, Subj, Obj, Target, Event, PrepCycL),
   dcgOptional(thePOS(Prep, 'Preposition')),
   noun_unit(Type, Event, Obj, PrepCycL, VerbObjectCycL),
   {prepl(Event, VerbObjectCycL, Prep, PREPL)}.


% Verb resort Three
verb_expression(Time, Subj, Obj, Event, PREPL) -->
   verb_unit(Type, Time, Subj, Obj, Target, Event, PrepCycL),
   noun_unit(Type, Event, Obj, VerbTargetCycL, VerbObjectCycL),
   dcgOptional(thePOS(Prep, 'Preposition')),
   noun_unit(Type, Event, Target, PrepCycL, VerbTargetCycL),
   {prepl(Event, VerbObjectCycL, Prep, PREPL)}.

% Verb resort One
verb_expression(Time, Subj, Obj, Event, PREPL) -->
   verb_unit(Type, Time, Subj, Obj, Target, Event, PrepCycL),
   dcgOptional(thePOS(Prep, 'Preposition')),
   {prepl(Event, PrepCycL, Prep, PREPL)}.

prepl(Event, VerbObjectCycL, Prep, and(isa(Event, 'PrepositionFn'(Prep, Event)), VerbObjectCycL)):-nonvar(Prep), !.
prepl(Event, VerbObjectCycL, Prep, VerbObjectCycL).


% =======================================================
% Verbs Resorts
% =======================================================

%verb_unit(Type, Time, Subj, Obj, Target, Event, and(hasVAttributes(Event, AVP), CycL5)) --> auxV(AVP), verb_unit(Type, Time, Subj, Obj, Target, Event, CycL5).
%verb_unit(Type, Time, Subj, Obj, Target, Event, and(hasVAttributes(Event, AVP), CycL5)) --> adv(AVP), verb_unit(Type, Time, Subj, Obj, Target, Event, CycL5).

verb_unit(Type, Time, Subj, Obj, Target, Event, Scope) -->
       verb_unit1(Type, Time, Subj, Obj, Target, Event, Scope).

verb_unit(Type, Time, Subj, Obj, Target, Event, Scope) -->
      verb_phrase_premods(Time, Subj, Obj, Event, ScopeIn, Scope),
       verb_unit1(Type, Time, Subj, Obj, Target, Event, ScopeIn).



% eaten by
verb_unit1(Type, Time, Subj, Obj, Target, Event, CycLO)-->
            verb_resort1(Time, Obj, Subj, Target, Event, CycL), theWord('By-TheWord'), {!},
                    verb_postmods(Event, CycL, CycLO).

verb_unit1(Type, Time, Subj, Obj, Target, Event, CycLO)-->
            verb_resort1(Time, Subj, Obj, Target, Event, CycL),
                    verb_postmods(Event, CycL, CycLO).

verb_unit1(Type, Time, Subj, Obj, Target, Event, CycLO)-->
            verb_resort1(Time, Subj, Obj, Target, Event, CycL1),
            verb_unit(Type, Time, Subj, Obj, Target, Event, CycL2),
                    verb_postmods(Event, and(CycL1, CycL2), CycLO).


verb_unit1(Type, Time, Subj, Obj, Target, Event, isa(Event, Isa)) --> dcgBoth((event_verb(Event, Isa), thePOS('BeAux')), theName(Event)).
verb_unit1(Type, Time, Subj, Obj, Target, Event, isa(Event, Isa)) --> dcgBoth(event_verb(Event, Isa), theName(Event)).

event_verb(Event, 'VerbAPFn'(V, P))--> thePOS(V, 'Adverb'), thePOS(P, 'Preposition'), {!}.
event_verb(Event, 'VerbVPFn'(V, P))--> thePOS(V, 'Verb'), thePOS(P, 'Preposition').
event_verb(Event, 'VerbFn'(V))-->thePOS(V, 'Verb'), {!}.

verb_resort1(Time, Subj, Obj, Target, Event, VerbCycL) --> theWord('To-TheWord'),
      verb_resort1(Time, Subj, Obj, Target, Event, VerbCycL).

% Two
verb_resort1(Time, Subj, Obj, Target, Event, VerbCycL) -->
           theFrame(Event, 'ParticleCompFrameFn'('TransitiveParticleNPFrameType', Prep), 'Verb', Template),
           thePrepWord(Prep),
     verb_postmods(Event, VerbCycL, VerbObjectCycLO),
     {substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event, ':OBJECT'-Obj, ':INF-COMP'-Target, ':OBLIQUE-OBJECT'-Target], VerbCycL)}.

% Three
verb_resort1(Time, Subj, Obj, Target, Event, VerbCycL) -->
        theFrame(Event, 'DitransitiveNP-NPFrame', 'Verb', Template), %{true},
        {substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event, ':OBJECT'-Obj, ':INF-COMP'-Target, ':OBLIQUE-OBJECT'-Target], VerbCycL)}.

% Two
verb_resort1(Time, Subj, Obj, Target, Event, and(isa(Event, 'Event'), VerbCycL)) -->
     theFrame(Event, 'TransitiveNPFrame', 'Verb', Template),
     {substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event, ':OBJECT'-Obj, ':INF-COMP'-Target, ':OBLIQUE-OBJECT'-Target], VerbCycL)}.


% Three
verb_resort1(Time, Subj, Obj, Target, Event, VerbCycL) -->
        theFrame(Event, _, 'Verb', Template), %{true},
        {substEach(Template, [':SUBJECT'-Subj, ':ACTION'-Event, ':OBJECT'-Obj, ':INF-COMP'-Target, ':OBLIQUE-OBJECT'-Target], VerbCycL)}.


verb_resort1(Time, Subj, Obj, Target, Event, CycL) -->verb_intransitive(Time, Subj, Obj, Event, CycL).

verb_resort1(Time, Subj, Obj, Target, Event, 'is-Underspecified'(Subj, Obj)) --> dcgOr(thePOS('BeAux'), theWord('Am-TheWord'), theWord('Be-TheWord')).

verb_resort1(Time, Subj, Obj, Target, Event, and(isa(Event, EventType), occursDuring(Event, When), preActors(Event, Subj), actors(Event, Obj))) -->
      dcgAnd(theIsa(Event, EventType, 'DurativeEventType'), theTense(When), theName(Event)).

verb_resort1(Time, Subj, Obj, Target, Event, and(occursDuring(Event, When), holdsIn(Event, [Pred, Subj, Obj]))) -->
      dcgAnd(theIsa(Event, Pred, 'TruthFunction'), theTense(When), theName(Event)).

verb_resort1(Time, Subj, Obj, Target, Event, holdsIn(Event, PrepCycL)) --> prepositional(Event, Subj, Obj, PrepCycL, Prep).


verb_resort1(Time, Subj, Obj, Target, Event, implies(isa(Event, 'VerbFn'(Word)), eventSOT(Event, Subj, Obj, Time))) -->
     dcgAnd(thePOS(Word, 'Verb'), theName(Event), theTense(Time)).

optionalVerbGlue(and) --> theText([and]).
optionalVerbGlue(or) --> theText([or]).
optionalVerbGlue(and) --> theText([, ]).
%optionalVerbGlue(and) --> [].

:-set_prolog_flag(double_quotes, string).
:- if(exists_source(logicmoo_nl_testing)).
:-include(logicmoo_nl_testing).
:- endif.
         /*
           , cycAssert(wordSemTrans(WORD, 33, 'Post-NounPhraseModifyingFrame', CYCL, 'Preposition', 'prepReln-Object', and(isa(':NOUN', NOUN), isa(':OBLIQUE-OBJECT', OBJECT))), '#$EnglishMt'), fail.

           (implies (and (compoundSemTrans ?WORD  (TheList ?S1)  ?POS TransitiveInfinitivePhraseFrame   ?CYCL)
           (wordStrings ?WORDW ?S1) )
           (wordSemTrans ?WORD 30 (PPCompFrameFn TransitiveParticleNPFrameType ?WORDW) ?CYCL ?POS compoundSemTrans True))
             */

           %wordSemTrans(Word, SenseNum, Frame, CycL, Pos
           %(resultIsa FrameRestrictionFn SubcategorizationFrame)
           %  ?SubcategorizationFrame


            /*

           (cyc-assert ' #$UniversalVocabularyMt)

           (isa wordSemTrans NLSemanticPredicate)
           (arity wordSemTrans 7)

           (argIsa wordSemTrans 1 LexicalWord)
           (argIsa wordSemTrans 2 Thing)
           (argIsa wordSemTrans 3 Thing)
           (argIsa wordSemTrans 4 Thing)
           (argIsa wordSemTrans 5 LinguisticObjectType)
           (argIsa wordSemTrans 6 Predicate)
           (argIsa wordSemTrans 7 Thing)
           (#$comment   #$wordSemTrans "(#$wordSemTrans #$LexicalWord #$Integer #$SubcategorizationFrame #$NLTemplateExpression #$LinguisticObjectType #$Predicate #$NLTemplateExpression)")


           (and



           (implies (clauseSemTrans ?WORD ?NUM ?FRAME  ?CYCL) (wordSemTrans  ?WORD ?NUM ?FRAME ?CYCL Conjunction clauseSemTrans True))
           (implies (nounSemTrans ?WORD ?NUM ?FRAME  ?CYCL) (wordSemTrans  ?WORD ?NUM ?FRAME ?CYCL Noun nounSemTrans True))

           (implies
                  (and (denotation ?WORD ?POS ?NUM ?COL)(isa ?COL Collection)(genls ?POS Noun))
                          (wordSemTrans ?WORD ?NUM RegularNounFrame (isa :NOUN ?COL) ?POS denotation True))
           (implies
                  (and (denotation ?WORD ?POS ?NUM ?COL)(isa ?COL Event)(genls ?POS Verb))
                          (wordSemTrans ?WORD ?NUM RegularNounFrame (and (situationConstituents :ACTION :SUBJECT)(isa :ACTION ?COL)) ?POS denotation True))

           (implies (and (compoundSemTrans ?WORD  (TheList ?S1)  ?POS IntransitiveVerbFrame ?CYCL)
           (partOfSpeech ?WORDW ?POSW ?S1) )
           (wordSemTrans ?WORD 30 (PPCompFrameFn TransitiveParticleNPFrameType ?WORDW) ?CYCL ?POS compoundSemTrans True))

           (implies (and (compoundSemTrans ?WORD  (TheList ?S1)  ?POS TransitiveNPFrame  ?CYCL)
           (partOfSpeech ?WORDW ?POSW ?S1) )
           (wordSemTrans ?WORD 30 (PPCompFrameFn TransitiveParticleNPFrameType ?WORDW) ?CYCL ?POS compoundSemTrans True))


           (implies (and (compoundSemTrans ?WORD  (TheList ?S1)  ?POS TransitiveInfinitivePhraseFrame   ?CYCL)
           (wordStrings ?WORDW ?S1) )
           (wordSemTrans ?WORD 30 (PPCompFrameFn TransitiveParticleNPFrameType ?WORDW) ?CYCL ?POS compoundSemTrans True))

           (implies
                  (prepSemTrans-New ?WORD ?POS ?FRAME ?CYCL)
                  (wordSemTrans ?WORD 30 ?FRAME ?CYCL ?POS prepSemTrans-New True))

           (implies
                  (and
                      (nounPrep ?WORD ?PREP ?CYCL)
                      (termOfUnit ?PPCOMPFRAMEFN
                          (PPCompFrameFn TransitivePPFrameType ?PREP)))
                  (wordSemTrans ?WORD 30 ?PPCOMPFRAMEFN ?CYCL Noun nounPrep True))

           (implies
                  (prepReln-Action ?ACTION ?OBJECT ?WORD ?CYCL)
                  (wordSemTrans ?WORD 30 VerbPhraseModifyingFrame ?CYCL Preposition prepReln-Action
                      (and
                          (isa :ACTION ?ACTION)
                          (isa :OBLIQUE-OBJECT ?OBJECT))))
           (implies
                  (prepReln-Object ?NOUN ?OBJECT ?WORD ?CYCL)
                  (wordSemTrans ?WORD 30 Post-NounPhraseModifyingFrame ?CYCL Preposition prepReln-Object
                      (and
                          (isa :NOUN ?NOUN)
                          (isa :OBLIQUE-OBJECT ?OBJECT))))
           (implies
                  (and
                      (semTransPredForPOS ?POS ?Pred)
                      (?Pred ?WORD ?NUM ?FRAME ?CYCL))
                        (wordSemTrans ?WORD ?NUM ?FRAME ?CYCL ?POS ?Pred True))

           (implies
                        (adjSemTrans-Restricted ?WORD ?NUM ?FRAME ?COL ?CYCL)
                        (wordSemTrans ?WORD ?NUM ?FRAME ?CYCL Adjective adjSemTrans-Restricted (isa :NOUN ?COL)))

           (implies
                        (nonCompositionalVerbSemTrans ?WORD ?COL ?CYCL)
                        (wordSemTrans ?WORD 666 VerbPhraseModifyingFrame ?CYCL Verb nonCompositionalVerbSemTrans (isa :OBJECT ?COL)))
           (implies
                        (lightVerb-TransitiveSemTrans ?WORD ?COL ?CYCL)
                        (wordSemTrans ?WORD 667 VerbPhraseModifyingFrame ?CYCL Verb lightVerb-TransitiveSemTrans (isa :OBJECT ?COL)))

           (implies
                        (verbSemTransPartial  ?WORD ?NUM ?FRAME ?CYCL)
                        (wordSemTrans ?WORD ?NUM ?FRAME ?CYCL Verb verbSemTransPartial True))





           (implies
                  (and
                      (isa ?OBJ ?COL)
                      (adjSemTransTemplate ?COL ?FRAME ?CYCL)
                      (denotation ?WORD Adjective ?NUM ?OBJ)
                      (evaluate ?TRANS
                          (SubstituteFormulaFn ?OBJ :DENOT ?CYCL)))
                  (wordSemTrans  ?WORD ?NUM ?FRAME ?TRANS Adjective adjSemTrans True ))

           (implies
                        (and
                            (genls ?SPEC ?COL)
                            (verbSemTransTemplate ?COL ?FRAME ?CYCL)
                            (denotation ?WORD Verb ?NUM ?SPEC)
                            (evaluate ?TRANS
                                (SubstituteFormulaFn ?SPEC :DENOT ?CYCL)))
                  (wordSemTrans  ?WORD ?NUM ?FRAME ?TRANS Verb verbSemTrans True ))
                )


(implies
       (and
           (relationInstanceAll performsInstancesAsPartOfJob ?REFINISHING ?REFINISHER)
           (subcatFrame ?RENOVATE-THEWORD Verb ?NUM ?TRANSITIVENPFRAME)
           (denotation ?RENOVATE-THEWORD AgentiveNoun ?NUM2 ?REFINISHER))
       (wordSemTrans ?RENOVATE-THEWORD Verb ?TRANSITIVENPFRAME
           (thereExists :ACTION
               (and
                   (bodilyDoer :SUBJECT :ACTION)
                   (isa :ACTION ?REFINISHING)
                   (possible
                       (isa :SUBJECT ?REFINISHER)))) performsInstancesAsPartOfJob))

(implies
       (and
           (different ?WORD1 ?WORD2)
           (semTransPredForPOS ?POS ?PRED)
           (denotesOpposite ?WORD1 ?POS ?NUM1 ?CONCEPT)
           (denotation ?WORD2 ?POS ?NUM2 ?CONCEPT)
           (?PRED ?WORD2 ?NUM2 ?FRAME ?FORMULA))
       (?PRED ?WORD1 ?NUM1 ?FRAME
           (not ?FORMULA)))


(implies
       (and
           (isa ?BELIEFS BinaryPredicate)
           (denotesOpposite ?DISBELIEVE-THEWORD Verb ?NUM ?BELIEFS))
       (wordSemTrans ?DISBELIEVE-THEWORD Verb TransitiveNPFrame
           (thereExists :ACTION
               (not
                   (holdsIn :ACTION
                       (?BELIEFS :SUBJECT :OBJECT)))) denotesOpposite))

(implies
       (and
           (isa ?BELIEFS BinaryPredicate)
           (denotation ?BELIEVE-THEWORD Verb ?NUM ?BELIEFS))
       (wordSemTrans ?BELIEVE-THEWORD Verb TransitiveNPFrame
           (thereExists :ACTION
                   (holdsIn :ACTION
                       (?BELIEFS :SUBJECT :OBJECT))) denotation))
(implies
       (and
           (isa ?BELIEFS BinaryPredicate)
           (denotationRelatedTo ?BELIEVE-THEWORD Verb ?NUM ?BELIEFS))
       (wordSemTrans ?BELIEVE-THEWORD Verb TransitiveNPFrame
           (thereExists :ACTION
                   (holdsIn :ACTION
                       (?BELIEFS :SUBJECT :OBJECT))) denotationRelatedTo))



            */
/*
Kino's numspecs
"
(cyc-assert '
(#$implies
  (#$and
  (#$genls ?C #$Thing)
    (#$evaluate ?R (#$EvaluateSubLFn
        (#$ExpandSubLFn
          (?C)
          (LENGTH
            (REMOVE-DUPLICATES
              (ALL-SPECS ?C)))))))
  (#$ist #$BaseKB     (#$numspecs ?C ?R))) #$BaseKB) "*/

dm1:-e2c_sem("I see two books sitting on a shelf").
dm2:-e2c_sem("AnyTemplate1 affects the NPTemplate2").
dm3:-e2c_sem("AnyTemplate1 at AnyTemplate2").

%% ABCL tests: (progn (defconstant CHAR-CODE-LIMIT 256) (load "doit.lsp"))
% ?>
setopt:-set_prolog_flag(toplevel_print_options, [quoted(true), portray(true), max_depth(0), attributes(portray)]).

:-setopt.
