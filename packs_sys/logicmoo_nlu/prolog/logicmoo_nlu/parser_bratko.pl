% ===================================================================
% File 'parser_bratko.pl'
% Purpose: English to KIF conversions from SWI-Prolog  
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_bratko.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2012/06/06 15:43:15 $
% ===================================================================

 /*                                 
  From Bratko chapter 17 page 455.
   This comes from Pereira and Warren paper, AI journal, 1980

   To see the non-toy version: https://github.com/logicmoo/logicmoo_nlu/blob/master/prolog/logicmoo_nlu/parser_bratko.pl 

   This version goes straight from Text to Deep Logical-Form :) 

   */

:-module(parser_bratko,[]).

:- op(500,xfy,&).
:- op(50,xfx,+).
:- op(510,xfy,=>).
:- op(1200,xfx,-->).
:- op(100,fx,'`').
% exampels 
test_bratko("bertrand wrote principia").
test_bratko("an author wrote principia").
test_bratko("is bertrand an author").
test_bratko("bertrand is an author").
test_bratko("bertrand is a writer").
test_bratko("is bertrand an author").
test_bratko("every author is a programmer").
test_bratko("is bertrand an programmer").
test_bratko("what did bertrand write").
test_bratko("what is a book").
test_bratko("what is a author").
test_bratko("principia is a book").
% test_bratko("bertrand is bertrand").
test_bratko("shrdlu halts").
test_bratko("every student wrote a program").
test_bratko("terry writes a program that halts").

test_bratko("an author of every book wrote a program").

test_bratko("bertand wrote a book about gottlob").
test_bratko("bertand wrote about gottlob").
test_bratko("bertand wrote nothing about gottlob").
test_bratko("bertand wrote to alfred about gottlob").
test_bratko("bertand wrote a letter to alfred about gottlob").

test_bratko("bertand wrote alfred a letter about gottlob").

test_bratko("bertand wrote alfred").

test_bratko("bertand wrote alfred a letter").
%              BY     wrote     TO      OBJECT

test_bratko("bertand wrote a letter to alfred").
test_bratko("bertand wrote to alfred a letter").
test_bratko("to alfred bertand wrote a letter").
test_bratko("bertand wrote a letter"). 

"without a paddle for fun Bertand wrote."
PREP + SLOT

test_bratko("what did alfred give to bertrand").
test_bratko("alfred gave ").
test_bratko("who did alfred give a book to").




prep_list([by,$write,to,object]).
prep_list([by,$jump,over]).
prep_list([at_time,from,$gave,to,object]).

secure, ((s:_\np)/np)/np, ['Beneficiary','Theme','Cause'

"alfred gave for fun joe a book "
I bet you $100 i will win.
by $bet, to, worth, object


I wrote you "i will win."



%            BY     jump  OVER
test_bratko("alfred jumped ").
up the creek 
without a paddle



"up the creek for fun alfred jumped  without a paddle"

"up the creek  without a paddle for fun alfred jumped "


:- throw(module(parser_e2c)).

:-export(t3/0).
system:t3:- make, parser_bratko:forall(test_bratko(Sent),bratko(Sent)).

baseKB:sanity_test:- t3.

m :- bratko.

:- thread_local(t_l:into_form_code/0).
:- asserta(t_l:into_form_code).

:-export(bratko/0).
bratko :- locally(tracing80,
             with_no_assertions(lmconf:use_cyc_database,
                  locally(t_l:usePlBratko, (told, repeat, prompt_read('BRATKO> ',U),  
                            into_lexical_segs(U,WL),(WL==[bye];WL==[end,'_',of,'_',file];bratko(WL)))))).

:-export(bratko/1).
bratko(Sentence):- into_lexical_segs(Sentence,Words),!,dmsg(sent_in_bratko(Words)),bratko(Words,Reply),  print_reply(Reply).

:-export(bratko/2).
bratko(Sentence,Reply) :-
   show_call(bratko_parse(Sentence,LF,Type)),
   show_call(bratko_clausify(LF,Clause,FreeVars)),!,   
   bratko_reply(Type,FreeVars,Clause,Reply).

bratko(Sentence,error('too difficult'(Sentence))).

% bratko_reply a question
bratko_reply(query,FreeVars, (answer(Answer) :- Condition), Reply) :-  
 Query = FreeVars^satisfy(Condition),
 fmt(query(Answer,Query)),
((baseKB:setof(Answer,Query,Answers)
 -> Reply = answer(Answers)
 ; (Answer == yes -> Reply = answer([no]) ; Reply = answer([none])))),!.
 
% bratko_reply an assertion
bratko_reply(assertion,_,Assertion,asserted(Assertion)) :-  baseKB:ain(Assertion),  !.
bratko_reply(_,_,_,error('unknown type')).


print_reply(Other) :-  fmt(Other).


bratko_parse(Sentence,LF,Type):- \+ is_list(Sentence),!,into_lexical_segs(Sentence,WL),!,bratko_parse(WL,LF,Type).  
bratko_parse(Sentence,LF,query)     :-  question(LF,Sentence,[]).
bratko_parse(Sentence,LF,assertion) :-  declarative(LF,nogap,Sentence,[]).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%% CLAUSIFY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Universals
bratko_clausify(all(X,F0),F,[X|V]) :-  bratko_clausify(F0,F,V).

% Implications 
bratko_clausify('=>'(A0 , C0) ,(C:-A),V) :-  clausify_literal(C0,C),  clausify_antecedent(A0,A,V).

% Literals
bratko_clausify(C0,C,[]):-  clausify_literal(C0,C).


% Literals
clausify_antecedent(L0,L,[]):-  clausify_literal(L0,L).

% Conjunctions
clausify_antecedent(E0&F0,(E,F),V) :-  clausify_antecedent(E0,E,V0),  clausify_antecedent(F0,F,V1),  conc(V0,V1,V).
 
% Existentials
clausify_antecedent(exists(X,F0),F,[X|V]) :-  clausify_antecedent(F0,F,V).

clausify_literal(L,L).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%% Grammar %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Questions

optionalText1(X) --> { length(X,L),L > 0, L < 33 } , X.
optionalText1(_) --> [].

question(Q             ) --> interogative(Q), optionalText1([?]).
question(S => answer(S)) --> sentence(S,nogap),[?].


% Interogative sentences
interogative(S => answer(X))   -->  whpron,verb_phrase(_Tense+fin,X^S,nogap).
interogative(S => answer(X))   -->  whpron,sentence_inv(S,gap(noun_phrase,X)). 
interogative(S => answer(yes)) -->  sentence_inv(S,nogap).
interogative(S => answer(yes)) -->  copula_is_does, noun_phrase((X^SO)^S, nogap),   noun_phrase((X^true)^exists(X,SO & true),nogap).

          
% Declarative sentences
declarative(S,GapInfo) --> sentence(S,GapInfo), optionalText1([.]).

sentence(S,GapInfo) -->   noun_phrase(VP^S,nogap),   verb_phrase(_Tense+fin,VP,GapInfo).

% Inverted sentences
sentence_inv(S,GapInfo) --> 
 aux(_Tense+fin/Form,VP1^VP2),  noun_phrase(VP2^S,nogap),  verb_phrase(Form,VP1,GapInfo).
 
% Noun Phrases
noun_phrase(NP,nogap)       -->  det(N2^NP),common_noun(N1),relative_clause(N1^N2).
% noun_phrase(NP,nogap)     -->  proper_noun(N2^NP),relative_clause(N2).
noun_phrase(NP,nogap)       --> proper_noun(NP).
noun_phrase((X^S)^S,gap(noun_phrase,X)) --> [].


% Verb phrases
verb_phrase(Form2,VP2,GapInfo) --> v_p(Form2,VP2,GapInfo).

v_p(Form1,VP2,GapInfo) -->  v_p_unit(Form1,VP2,GapInfo).

% @TODO Prep
% v_p(Form1,VP1&VP2,GapInfo) -->  v_p_unit(Form1,VP1,nogap), p_p(VP2,GapInfo).
% v_p(Form1,VP1&VP2,GapInfo) -->  v_p_unit(Form1,VP1,GapInfo), p_p(VP2,nogap).

v_p(Form1,VP2,GapInfo) -->  aux(Form1/Form2,   VP1^VP2),             v_p(Form2,VP1,GapInfo).
v_p(Form1,VP2,GapInfo) -->  v_p_rovn1(Form1,VP2,GapInfo,Form2,VP1),  v_p(Form2,VP1,nogap).
v_p(Form2,VP2,GapInfo) -->  v_p_rovn1(Form1,VP2, nogap ,Form2,VP1),  v_p(Form1,VP1,GapInfo).
v_p(_Tense+fin,X^S,GapInfo) --> copula_is_does,  noun_phrase((X^P)^exists(X,S&P),GapInfo).

v_p_unit(Form,X^S,GapInfo) -->  bratko_tv(Form,X^VP),  noun_phrase(VP^S,GapInfo).
v_p_unit(Form,VP,  nogap ) -->  bratko_iv(Form,VP).

p_p(X^S,GapInfo) -->  bratko_pp(X^VP),  noun_phrase(VP^S,GapInfo).

v_p_rovn1(Form1,VP2, GapInfo,Form2,VP1) --> rov(Form1/Form2,NP,VP1,VP2), noun_phrase(NP,GapInfo).

% relative clauses
relative_clause((X^S1)^(X^(S1&S2))) -->  relpron,verb_phrase(_Tense+fin,X^S2,nogap).
relative_clause((X^S1)^(X^(S1&S2))) -->  relpron,sentence(S2,gap(noun_phrase,X)).
% an author of every book
relative_clause((X^S1)^(X^(S1&(z(of, X,Y)&S2)))) -->  [of], noun_phrase(Y^S2,nogap).
% a book about gottlob @TODO
% relative_clause((X^S1)^(X^(S1&(z(about, X,Y)&S2)))) -->  [about], noun_phrase(Y^S2,nogap).

relative_clause(N^N) --> [].


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%% Terminals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

det(LF) --> [D],{det_lf(D,LF)}.
common_noun(LF) --> [N],{noun_lf(N,LF)}.
proper_noun((E^S)^S) --> [PN],{pn_lf(PN,E)}.

aux(Form,LF) --> [Aux],{aux_lf(Aux,Form,LF)}.
relpron --> [RP],{relpron(RP)}.
whpron --> [WH], {whpron(WH)}.

copula_is_does --> [C], {copula_is_does(C)}.

bratko_iv(nonfinite, LF) --> [IV],{bratko_iv_lf(IV,_,_,_,_,LF)}.
bratko_iv(pres+fin,  LF) --> [IV],{bratko_iv_lf(_,IV,_,_,_,LF)}.
bratko_iv(past+fin,  LF) --> [IV],{bratko_iv_lf(_,_,IV,_,_,LF)}.
bratko_iv(past+part, LF) --> [IV],{bratko_iv_lf(_,_,_,IV,_,LF)}.
bratko_iv(pres+part, LF) --> [IV],{bratko_iv_lf(_,_,_,_,IV,LF)}.

bratko_tv(nonfinite, LF) --> [TV],{bratko_tv_lf(TV,_,_,_,_,LF)}.
bratko_tv(pres+fin,  LF) --> [TV],{bratko_tv_lf(_,TV,_,_,_,LF)}.
bratko_tv(past+fin,  LF) --> [TV],{bratko_tv_lf(_,_,TV,_,_,LF)}.
bratko_tv(past+part, LF) --> [TV],{bratko_tv_lf(_,_,_,TV,_,LF)}.
bratko_tv(pres+part, LF) --> [TV],{bratko_tv_lf(_,_,_,_,TV,LF)}.

bratko_pp(X^Y^z(PP,X,Y)) --> [PP],{prep(PP)}.

rov(nonfinite /Requires,LF) --> [ROV], {rov_lf(ROV,_,_,_,_,LF,Requires)}.
rov(pres+fin  /Requires,LF) --> [ROV], {rov_lf(_,ROV,_,_,_,LF,Requires)}.
rov(past+fin  /Requires,LF) --> [ROV], {rov_lf(_,_,ROV,_,_,LF,Requires)}.
rov(past+part /Requires,LF) --> [ROV], {rov_lf(_,_,_,ROV,_,LF,Requires)}.
rov(pres+part /Requires,LF) --> [ROV], {rov_lf(_,_,_,_,ROV,LF,Requires)}.


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%% Lexical Items / Dictionary %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prep(X):- talk_db(preposition, X).

relpron(that).
relpron(who).
relpron(whom).

whpron(who).
whpron(whom).
whpron(what).
whpron(Which):- talk_db(pronoun,Which).

copula_is_does(is).
copula_is_does(does).


det_lf(every, (X^S1)^(X^S2)^ all(X, =>(S1,S2))).
det_lf(an, (X^S1)^(X^S2)^ exists(X,S1& S2)).
det_lf(a, (X^S1)^(X^S2)^ exists(X,S1& S2)).
det_lf(some, (X^S1)^(X^S2)^ exists(X,S1& S2)).

noun_lf1(author).
noun_lf1(book).
noun_lf1(professor).
noun_lf1(program).
noun_lf1(programmer).
noun_lf1(student).

into_isa3(X,Y,isa(X,Y)).

noun_lf(Sing,  X^ISA) :- noun_lf1(Sing),into_isa3(X,Sing,ISA).
noun_lf(Word,  X^ISA) :- (Word=Sing;Word=Plur),talk_db(noun1,Sing,Plur),into_isa3(X,Sing,ISA).
noun_lf(Mass,  X^ISA) :- talk_db(noun2,Mass),into_isa3(X,Mass,ISA).

 adj_lf(Sing,  X^ISA) :- (adj_db(Sing,restr);talk_db(adj,Sing)),into_isa3(X,adjFn(Sing),ISA).


pn_lf(Name, Value) :- pn_dict(Name), Name = Value.

pn_dict(begriffsschrift ).
pn_dict(bertrand ).
pn_dict(bill ).
pn_dict(gottlob ).
pn_dict(alfred ).
pn_dict(lunar ).
pn_dict(principia ).
pn_dict(shrdlu ).
pn_dict(terry ).
pn_dict(Name ):- name_template_db(Name,_).


%           nonfinite, pres+fin, past+fin,  past+part,  pres+part,  LF
bratko_iv_lf( halt,      halts,    halted,    halted,     halting,    X^z(doing,X,halt)).

bratko_iv_lf( Write,     Writes,   Wrote,     Written,    Writing,    X^z(doing,X,Writes)) :- 
   talk_db(intransitive,Write,Writes,Wrote,Writing,Written).

%           nonfinite, pres+fin, past+fin,  past+part,  pres+part,  LF
bratko_tv_lf( write,     writes,   wrote,     written,    writing,    X^Y^z(writes,X,Y)). 
bratko_tv_lf( meet,      meets,    met,       met,        meeting,    X^Y^z(meets,X,Y)).
bratko_tv_lf( concern, concerns, concerned, concerned, concerning,   X^Y^z(concerns,X,Y)).
bratko_tv_lf( run,  runs, ran, run,  running,                    X^Y^z(runs,X,Y)).

bratko_tv_lf( Write,     Writes,   Wrote,     Written,    Writing,    X^Y^z(Writes,X,Y)) :- 
  talk_db(transitive,Write,Writes,Wrote,Writing,Written).

rov_lf(want, wants, wanted, wanted, wanting,  ((X^z(want,Y,X,Comp))^S) ^(X^Comp) ^Y ^S,infinitival).

%semantics is partially execution of 
% NP ^VP ^Y ^NP(X want(Y,X,VP(X)))
%((X^ '`'(want(Y,X,Comp)))^S) ^(X^Comp) ^Y ^S, % form of VP required:
%infinitival).

aux_lf(to ,  infinitival/nonfinite , VP^VP).
aux_lf(does , _Tense+fin/nonfinite ,  VP^VP).
aux_lf(did ,  _Tense+fin/nonfinite ,  VP^VP).
aux_lf(to ,  _/_ , VP^VP).

conc([],L,L).
conc([H|T],L,[H|R]) :- conc(T,L,R).


:- retract(t_l:into_form_code).


:- context_module(CM),module_predicates_are_exported(CM).

:- context_module(CM),module_meta_predicates_are_transparent(CM).
% :- context_module(CM),module_property(CM, exports(List)),moo_hide_show_childs(List).


