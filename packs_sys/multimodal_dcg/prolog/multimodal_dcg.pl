/* Part of LogicMOO Base An Implementation a MUD server in SWI-Prolog
% ===================================================================
% File 'multimodal_dcg.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_bugger.pl' 1.0.0
% Revision:  $Revision: 1.1 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
:- module(multimodal_dcg,[
         do_dcg_util_tests/0,
         isVar/1,
         isQVar/1,
         isVarOrVAR/1,


         dcgOneOrMore//1,
         dcgOptional//1,
         dcgZeroOrMore//1,
         dcgOptionalGreedy//1,
         dcgAnd//2,
         dcgAnd//3,
         dcgAnd//4,
         dcgMust//1,
         % dumpList/1,
         dcgSeqLen//1,
         dcgOr//2,
         dcgNot//1,
         theString//1,
         theString//2,
         theText//1,
         theCode//1,         
         dcgLenBetween/4,

         % unit test functions
         do_dcgTest/3,
         do_dcgTest_startsWith/3,
         decl_dcgTest_startsWith/2,
         decl_dcgTest_startsWith/3,
         decl_dcgTest/2,
         decl_dcgTest/3,
         dcgReorder/4
	 ]).

:- set_module(class(library)).

:- use_module(library(logicmoo_common)).
:- reexport(library(logicmoo/dcg_meta)).

end_of_file.


% :- ensure_loaded(library(logicmoo_utils)).
% :- ensure_loaded(library(logicmoo_util_strings)).

 
:- meta_predicate dcgLeftOfMid(?,//,?,?).
:- meta_predicate dcgLeftMidRight(//,//,//,?,?).

:- meta_predicate dcgAnd(//,//,//,//,?,?).
:- meta_predicate dcgAnd(//,//,//,?,?).
:- meta_predicate dcgAnd(//,//,?,?).
:- meta_predicate dcgAndRest(//,*,*,*).
:- meta_predicate dcgBoth(//,//,*,*).
:- meta_predicate dcgIgnore(//,?,?).
:- meta_predicate dcgLeftOf(//,*,*,*).
:- meta_predicate dcgMust(//,?,?).
:- meta_predicate dcgMidLeft(//,*,//,*,?).
:- meta_predicate dcgNot(//,?,?).
:- meta_predicate dcgOnce(//,?,?).
:- meta_predicate dcgOnceOr(//,//,?,?).
:- meta_predicate dcgOneOrMore(//,?,*).
:- meta_predicate dcgOptional(//,?,?).
:- meta_predicate dcgOptionalGreedy(//,?,?).
:- meta_predicate dcgOr(//,//,//,//,//,?,?).
:- meta_predicate dcgOr(//,//,//,//,?,?).
:- meta_predicate dcgOr(//,//,//,?,?).
:- meta_predicate dcgOr(//,//,?,?).
:- meta_predicate dcgReorder(//,//,?,?).
:- meta_predicate dcgStartsWith(//,?,?).
:- meta_predicate dcgStartsWith0(//,?,*).
:- meta_predicate dcgStartsWith1(//,?,?).
:- meta_predicate dcgTraceOnFailure(0).
:- meta_predicate dcgWhile(?,//,?,?).
:- meta_predicate dcgZeroOrMore(//,?,*).
:- meta_predicate decl_dcgTest(?,?).
:- meta_predicate decl_dcgTest(?,?,?).
:- meta_predicate decl_dcgTest_startsWith(?,?,?).
:- meta_predicate do_dcgTest(*,//,0).
:- meta_predicate do_dcgTest_startsWith(?,//,?).
:- meta_predicate suggestVar(2,*,?).
:- meta_predicate theAll(//,?,?).
:- meta_predicate theCode(?,?,?).

% :- meta_predicate decl_dcgTest(?,//).
% :- meta_predicate decl_dcgTest_startsWith(?,*,0).
% :- meta_predicate decl_dcgTest(?,//,0).
% :- meta_predicate theCode(0,*,*).

% this is a backwards compatablity block for SWI-Prolog 6.6.6

:- if(current_prolog_flag(dialect,swi)).
:- dynamic(double_quotes_was_in_dcg/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_in_dcg(WAS)).
:- retract(double_quotes_was_in_dcg(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_in_dcg(WAS)).
:- set_prolog_flag(double_quotes,string).
:- endif.

isVarOrVAR(V):-var(V),!.
isVarOrVAR('$VAR'(_)).
isVar(V):- (isVarOrVAR(V);isQVar(V)),!.
isQVar(Cvar):-atom(Cvar),atom_concat('?',_,Cvar).

:- dynamic 
   decl_dcgTest/2,
   decl_dcgTest/3,
   decl_dcgTest_startsWith/2,
   decl_dcgTest_startsWith/3.


decl_dcgTest(X,Y):- nonvar(Y),!,do_dcgTest(X,Y,true).
decl_dcgTest(X,Y,Z):- nonvar(Y),!,do_dcgTest(X,Y,Z).
decl_dcgTest_startsWith(X,Y):- nonvar(Y),!,do_dcgTest(X,dcgStartsWith(Y),true).
decl_dcgTest_startsWith(X,Y,Z):- nonvar(Y),!,do_dcgTest(X,dcgStartsWith(Y),Z).
% ========================================================================
% getWhatnot helpers
% ========================================================================

getText([],[]).
getText(L,Txt):-member([txt|Txt],L),!.
getText([L|List],Text):-getText(L,Text1),getText(List,Text2),append(Text1,Text2,Text),!.
getText(F,S):-functor(F,_,3),arg(2,F,S),!.
getText(S,S).


% ========================================================================
% theWhatnot helpers  (parses contents of terminals)
% ========================================================================
:- style_check(-discontiguous).



equals_text(S,Data):- is_list(Data),member([txt,S0],Data),!,equals_text(S,S0).
equals_text(S,S):- !.
%equals_text(S,S0):- var(S),var(S0),!,S=S0.
equals_text(S,S0):- var(S0),text_to_string(S,S0),!.
equals_text(S,S0):- var(S),text_to_string(S0,S),!.
equals_text(S,S0):- text_to_string(S,SS),text_to_string(S0,SS).

decl_dcgTest("this is text",theText([this,is,text])).

% Text

theText(Text) --> {Text==[],!},[].
theText([S|Text]) --> {nonvar(S),!},theText0(S),!,theText(Text).

theText([S|Text]) --> theText0(S),theText(Text).
theText([]) --> [].

%theText([S|Text],[S0|TextData],More):- member([txt,S0|TextData],Data),equals_text(S,S0),append(Text,More,TextData).

%  dtrace, do_dcgTest_startsWith("this is text", multimodal_dcg:dcgStartsWith1(theText(["this"])), true) .

% Looser text test?
theText0(_,W,_):- W==[],!,fail.
theText0(S) --> {atomic(S),atom_concat('"',Right,S),atom_concat(New,'"',Right),!},theText(New).
theText0(S) --> {atomic(S),concat_atom([W1,W2|List],' ',S),!},theText([W1,W2|List]).
theText0(S) --> {!}, [Data],{equals_text(S,Data)}.




decl_dcgTest("this is a string",theString("this is a string")).
theString(String) --> theString(String, " ").

atomic_to_string(S,S):- string(S),!.
atomic_to_string(S,Str):-sformat(Str,'~w',[S]).

atomics_to_string_str(L,S,A):-catch(atomics_to_string(L,S,A),_,fail).
atomics_to_string_str(L,S,A):-atomics_to_string_str0(L,S,A).

atomics_to_string_str0([],_Sep,""):-!.
atomics_to_string_str0([S],_Sep,String):-atom(S),!,string_to_atom(String,S).
atomics_to_string_str0([S],_Sep,S):- string(S),!.
atomics_to_string_str0([S|Text],Sep,String):-
   atomic_to_string(S,StrL),
   atomics_to_string_str0(Text,Sep,StrR),!,   
   new_a2s([StrL,StrR],Sep,String).

% theString(String,Sep) --> [S|Text], {atomic_list_concat_catch([S|Text],Sep,String),!}.
theString(String,Sep) --> [S|Text], {atomics_to_string_str([S|Text],Sep,String),!}.

decl_dcgTest_startsWith([a,b|_],theCode(X=1),X==1).
decl_dcgTest_startsWith("anything",theCode(X=1),X==1).
decl_dcgTest("",theCode(X=1),X==1).
theCode(Code) --> [],{Code}.

 
decl_dcgTest([a,b|C],theAll([a,b|C])).
% theAll(X)--> X.
theAll(X, B, C) :- var(X),X=B,C=[],!.
theAll(X, B, C) :- phrase(X, B, C).

decl_dcgTest([a,b|C],theRest(X),X==[a,b|C]).
theRest(X, X, []).



theName(Var,S,_) :-getText(S,Text),suggestVar(=,Text,Var),!.

%suggestVar(_,Subj,Subj2):-makeName(Subj,Subj2),!.

suggestVar(_Gensym,Subj,Subj):-var(Subj),!.%,true,!.
suggestVar(_Gensym,Subj,_Subj2):-var(Subj),!.%,true,!.
suggestVar(Gensym,[W|ORDS],Subj):-!,ignore((once((nonvar(ORDS),toPropercase([W|ORDS],Proper),concat_atom(['Hypothetic'|Proper],'-',Suj),call(Gensym,Suj,SubjSl),ignore(SubjSl=Subj))))),!.
%suggestVar(Gensym,[W|ORDS],Subj):-!,ignore(notrace(once((nonvar(ORDS),concat_atom(['?'|[W|ORDS]],'',Suj),call(Gensym,Suj,SubjSl),toUppercase(SubjSl,SubjS),ignore(SubjS=Subj))))),!.
suggestVar(_Gensym,[],_):-!.%suggestVar(gensym,[A],Subj),!.
suggestVar(Gensym,A,Subj):-suggestVar(Gensym,[A],Subj),!.



%makeName(Subj,Subj2):-toCreate(Subj,hypotheticDenotation(Subj,_,string(Words))),!,makeName(Words,Subj2),!.
makeName(A,A):-!.
makeName(Subj,Subj2):-var(Subj),!,term_to_atom(Subj,Atom),makeName(['Hypothetic',Atom],Subj2),!.
makeName([],Subj2):-!,makeName(_Subj,Subj2),!.
makeName(Subj,Subj2):-atom(Subj),atom_concat('?',Sub2,Subj),!,makeName(Sub2,Subj2),!.
makeName(A,Subj):-atom(A),!,makeName([A],Subj),!.
makeName([W|ORDS],Subj):-nonvar(ORDS),!,toPropercase([W|ORDS],PCASE),concat_atom(['Hypothetic'|PCASE],'-',Suj),gensym(Suj,Subj),!.

leastOne([_CO|_LSS]).

% ========================================================================
% dcgWhatnot helpers  (meta interprets)
% ========================================================================

% TODO: when using the DCG to generate instead of test it will move the C before the P
% dcgReorder(P,C) --> P, C.
:- export(dcgReorder//2).
dcgReorder(P, C, B, E):- phrase(P, B, D), phrase(C, D, E).

:- export(dcgSeq//2).
dcgSeq(X,Y,[S0,S1|SS],E):-phrase((X,Y),[S0,S1|SS],E).

:- export(dcgBoth//2).
dcgBoth(DCG1,DCG2,S,R) :- append(L,R,S),phrase(DCG1,L,[]),once(phrase(DCG2,L,[])).

dcgAnd(DCG1,DCG2,DCG3,DCG4,S,E) :- phrase(DCG1,S,E),phrase(DCG2,S,E),phrase(DCG3,S,E),phrase(DCG4,S,E).
dcgAnd(DCG1,DCG2,DCG3,S,E) :- phrase(DCG1,S,E),phrase(DCG2,S,E),phrase(DCG3,S,E).
dcgAnd(DCG1,DCG2,S,E) :- phrase(DCG1,S,E),phrase(DCG2,S,E).
dcgOr(DCG1,DCG2,DCG3,DCG4,DCG5,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E);phrase(DCG3,S,E);phrase(DCG4,S,E);phrase(DCG5,S,E).
dcgOr(DCG1,DCG2,DCG3,DCG4,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E);phrase(DCG3,S,E);phrase(DCG4,S,E).
dcgOr(DCG1,DCG2,DCG3,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E);phrase(DCG3,S,E).
dcgOr(DCG1,DCG2,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E).
dcgOnceOr(DCG1,DCG2,S,E) :- phrase(DCG1,S,E)->true;phrase(DCG2,S,E).
dcgNot(DCG2,S,E) :- not(phrase(DCG2,S,E)).
dcgIgnore(DCG2,S,E) :- ignore(phrase(DCG2,S,E)).
dcgOnce(DCG2,S,E) :- once(phrase(DCG2,S,E)).

dcgWhile(True,Frag)-->dcgAnd(dcgOneOrMore(True),Frag).

dcgMust((DCG1,List),S,E) :- is_list(List),!,must((phrase(DCG1,S,SE),phrase(List,SE,E))).
dcgMust(DCG1,S,E) :- must(phrase(DCG1,S,E)).

dcgSeqLen(Len, FB, END) :-
        length(CD, Len),
        '$append'(CD, END, FB).


% addtext a sofa is in here
% dcgLenBetween(_,_) --> [_].
dcgLenBetween(Start,Start) --> {!}, dcgSeqLen(Start),{!}.
dcgLenBetween(Start,End, FB, END) :- FB==[],!, ((Start>End -> between(End,Start,0) ; between(Start,End,0))),must(END=[]).
dcgLenBetween(Start,End) --> dcgOnceOr(dcgSeqLen(Start),({(Start>End -> Next is Start-1 ; Next is Start+1)},dcgLenBetween(Next,End))).
dcgLenBetween(Len, Start, End, FB, END) :-
       ( length(CD, Start),
        '$append'(CD, END, FB)) -> ignore(End=Start) ;
        (
            (Start>End -> Next is Start-1 ; Next is Start+1),
            dcgLenBetween(Len, Next, End, FB, END)
        ).




dcgOneOrMore(True) --> True,dcgZeroOrMore(True),{!}.

dcgZeroOrMore(True) --> True,{!},dcgZeroOrMore(True),{!}.
dcgZeroOrMore(_True) -->[].

dcgLeftOf(Mid,[Left|T],S,[MidT|RightT]):-append([Left|T],[MidT|RightT],S),phrase(Mid,MidT),phrase([Left|T],_LeftT).

dcgLeftOfMid([Left|T],Mid,S,[MidT|RightT]):-append([Left|T],[MidT|RightT],S),phrase(Mid,MidT),phrase([Left|T],_LeftT).

dcgLeftMidRight(Left,Mid,Right) --> dcgLeftOfMid(LeftL,Mid),{phrase(Left,LeftL,[])},Right.

dcgMidLeft(Mid,Left,Right) --> dcgLeftOf(Mid,Left),Right.

dcgNone --> [].

dcgOptional(A)--> dcgOnce(dcgOr(A,dcgNone)).

dcgOptionalGreedy(A)--> dcgOnce(dcgOr(A,dcgNone)).

dcgTraceOnFailure(X):-once(X;(dtrace(X))).

:- export(capitalized//1).
capitalized([W|Text]) --> theText([W|Text]),{atom_codes(W,[C|_Odes]),is_upper(C)}.

substAll(B,[],_R,B):-!.
substAll(B,[F|L],R,A):-subst(B,F,R,M),substAll(M,L,R,A).
   
substEach(B,[],B):-!.
substEach(B,[F-R|L],A):-subst(B,F,R,M),substEach(M,L,A).

dcgAndRest(TheType,_TODO,[S|MORE],[]) :- phrase(TheType,[S],[]),phrase(TheType,[S|MORE],[]).

% =======================================================
% look ahead but ...
% =======================================================

% 1) must be first in list 
% 2) doesnt consume
% 3) sees as many items as needed
dcgStartsWith(TheType,SMORE,SMORE) :- phrase(TheType,SMORE,_).

% tests for the above
decl_dcgTest_startsWith("this is text",dcgStartsWith(theText(["this","is"]))).


:- export(dcgStartsWith1//1).
% 1) must be first in list 
% 2) doesnt consume
% 3) sees only 1 item
dcgStartsWith1(TheType,[S|MORE],[S|MORE]) :- phrase(TheType,[S],[]).

% tests for the above
decl_dcgTest_startsWith("this is text",dcgStartsWith1(theText(["this"]))).


% 1) must be first in list 
% 2) consumes like theRest(...)
% 3) sees as many items as needed
dcgStartsWith0(TheType,SMORE,[]) :- phrase(TheType,SMORE,_).

% tests for the above
decl_dcgTest("this is text",dcgStartsWith0(theText(["this",is]))).

% =======================================================
% DCG Tester
% =======================================================

:- export(do_dcg_util_tests/0).
do_dcg_util_tests:-
   forall(decl_dcgTest(List,Phrase,Call),'@'((do_dcgTest(List,Phrase,Call)),multimodal_dcg)),
   forall(decl_dcgTest_startsWith(List,Phrase,Call),'@'((do_dcgTest_startsWith(List,Phrase,Call)),multimodal_dcg)).


do_dcgTest(Input,DCG,Call):- to_word_list(Input,List),OTEST=do_dcgTest(Input,DCG,Call),copy_term(DCG:OTEST,CDCG:TEST),
   once((phrase(DCG,List,Slack),Call,(Slack==[]->dmsg(passed(CDCG,TEST,OTEST));dmsg(warn(Slack,OTEST))))).
do_dcgTest(Input,DCG,Call):- dmsg(warn(failed(DCG, do_dcgTest(Input,DCG,Call)))).


do_dcgTest_startsWith(Input,DCG,Call):- to_word_list(Input,List),OTEST=do_dcgTest(Input,DCG,Call),copy_term(DCG:OTEST,CDCG:TEST),
   once((phrase(DCG,List,Slack),Call,(Slack==[]->wdmsg(warn(CDCG,TEST,OTEST));dmsg(passed(CDCG,TEST,OTEST))))).
do_dcgTest_startsWith(Input,DCG,Call):- wdmsg(warn(failed(DCG, do_dcgTest_startsWith(Input,DCG,Call)))).


decl_dcgTest(List,Phrase,true):-decl_dcgTest(List,Phrase).
decl_dcgTest_startsWith(List,Phrase,true):-decl_dcgTest_startsWith(List,Phrase).



% :-source_location(File,_Line),module_property(M,file(File)),!,forall(current_predicate(M:F/A),M:export(F/A)).

     


dumpList(B):- currentContext(dumpList,Ctx),dumpList(Ctx,B).
dumpList(_,AB):-dmsg(dumpList(AB)),!.

dumpList(_,[]):-!.
%dumpList(Ctx,[A|B]):-!,fmt(Ctx,A),dumpList(Ctx,B),!.
%dumpList(Ctx,B):-fmt(Ctx,dumpList(B)).

% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- if(current_prolog_flag(dialect,swi)).
:- retract(double_quotes_was_in_dcg(WAS)),set_prolog_flag(double_quotes,WAS).
:- endif.


:- all_source_file_predicates_are_transparent.

end_of_file.

sentenceTagger(English,Tagged).





testPhrase(Dcg,English):-
         sentenceTagger(English,Tagged),dumpList(Tagged),
         phrase(Dcg,Tagged,Left),
         nl,nl,writeq(left),         
         nl,dumpList(Left).




