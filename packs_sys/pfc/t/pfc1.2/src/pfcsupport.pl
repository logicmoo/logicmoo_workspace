%%
%%
%% predicates for manipulating support relationships
%%
:- module(pfcsupport,[ifNotDMiles/1,ifNotDMiles/2]).


:- use_module(library(pfc_pack_xform)).

:- if(false).
% NON-DMILES
:- dynamic support1/3.
:- dynamic support2/3.
:- dynamic support3/3.
ifNotDMiles(G):- G.
ifNotDMiles(G,_):- G.

:- else.

% DMILES
:- dynamic spft/3.
support1(P,Fact,Trigger):-umt(spft(P,Fact,Trigger)).
support2(Fact,Trigger,P):-umt(spft(P,Fact,Trigger)).
support3(Trigger,P,Fact):-umt(spft(P,Fact,Trigger)).

ifNotDMiles(_).
ifNotDMiles(_,G):- G.

:- endif.


%% pfcAddSupport(+Fact,+Support)

pfcAddSupport(P,(Fact,Trigger)) :- umt(clause_asserted(spft(P,Fact,Trigger))),!,dmsg(tWICE_pfcAddSupport(P,(Fact,Trigger))),!.
pfcAddSupport(P,(Fact,Trigger)) :-
  show_call(assert_u(spft(P,Fact,Trigger))),!,
  nop(ifNotDMiles(assert(support2(Fact,Trigger,P)))),
  nop(ifNotDMiles(assert(support3(Trigger,P,Fact)))),!.

pfcGetSupport(P,(F,T)):- !, umt((spft(P,F,T))).

pfcGetSupport(P,FT):- umt((pfcGetSupport0(P,FT))).

pfcGetSupport(P,(Fact,Trigger)) :-
   nonvar(P)         -> support1(P,Fact,Trigger) 
   ; nonvar(Fact)    -> support2(Fact,Trigger,P) 
   ; nonvar(Trigger) -> support3(Trigger,P,Fact) 
   ; otherwise       -> support1(P,Fact,Trigger).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(P),
  !,
  pfcRetractOrWarn(spft(P,Fact,Trigger)),
  ifNotDMiles(pfcRetractOrWarn(support2(Fact,Trigger,P))),
  ifNotDMiles(pfcRetractOrWarn(support3(Trigger,P,Fact))).


pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(Fact),
  !,
  ifNotDMiles(pfcRetractOrWarn(support2(Fact,Trigger,P)),support2(Fact,Trigger,P)),
  pfcRetractOrWarn(spft(P,Fact,Trigger)),
  ifNotDMiles(pfcRetractOrWarn(support3(Trigger,P,Fact))).

pfcRemSupport(P,(Fact,Trigger)) :-
  ifNotDMiles(pfcRetractOrWarn(support3(Trigger,P,Fact)),support3(Trigger,P,Fact)),
  pfcRetractOrWarn(spft(P,Fact,Trigger)),
  ifNotDMiles(pfcRetractOrWarn(support2(Fact,Trigger,P))).


pfc_collect_supports(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

pfc_support_relation((P,F,T)) :-
  umt((support1(P,F,T))).

pfc_make_supports((P,S1,S2)) :- 
  pfcAddSupport(P,(S1,S2)),
  (pfcAdd(P); true),
  !.

%% pfcTriggerKey(+Trigger,-Key) 
%%
%% Arg1 is a trigger.  Key is the best term to index it on.

pfcTriggerKey(pt(Key,_),Key).
% unused? pfcTriggerKey(pt(Key,_,_),Key).
pfcTriggerKey(nt(Key,_,_),Key).
pfcTriggerKey(Key,Key).


%%^L
%% Get a key from the trigger that will be used as the first argument of
%% the trigger base clause that stores the trigger.
%%

pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).

:- fixup_exports.
