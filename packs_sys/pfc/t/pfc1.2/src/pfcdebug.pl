
%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

:- module(pfcdebug, []).
:- use_module(library(pfc_pack_xform)).

:- dynamic pfcTraced/1.
:- dynamic pfcSpied/2.
:- dynamic pfcTraceExecution/0.
:- dynamic   pfcWarnings/1.


:- initialization(pfcDefault(pfcWarnings(_), pfcWarnings(true))).

%% predicates to examine the state of pfc

pfcQueue :- umt(( listing(pfcQueue/2))).

pfcPrintDB :-
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
  pfcPrintSupports.

%% pfcPrintFacts ...

pfcPrintFacts :- pfcPrintFacts(_,true).

pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern,true).

pfcPrintFacts(P,C) :-
  pfcFacts(P,C,L),
  pfcClassifyFacts(L,User,Pfc,_Rule),
  ansi_format([underline],"~N~nUser added facts: ",[]),
  pfcPrintitems(User),
  ansi_format([underline],"~N~nPfc added facts: ",[]),
  pfcPrintitems(Pfc).



pfcPrintitems(List):- \+ \+ umt((pfcPrintitems0(List))).
%- printitems0 clobbers it's arguments - beware!
pfcPrintitems0([]).
pfcPrintitems0([H|T]) :-
  numbervars(H,0,_),
  ansi_format([bold],"~N  ~p",[H]),
  pfcPrintitems0(T).

pfcClassifyFacts([],[],[],[]).

pfcClassifyFacts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],[H|User],Pfc,Rule) :-
  pfcGetSupport(H,UU),
  get_first_real_user_reason(H,UU),
  is_axiom_support(UU),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],User,[H|Pfc],Rule) :-
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcPrintRules :-
  bagof((P==>Q),clause((P==>Q),true),R1),
  pfcPrintitems(R1),
  bagof((P<==>Q),clause((P<==>Q),true),R2),
  pfcPrintitems(R2),
  bagof((P<-Q),clause((P<-Q),true),R3),
  pfcPrintitems(R3).

pfcPrintTriggers :-
  ansi_format([underline],"~NPositive triggers...~n",[]),
  bagof(pt(T,B),pfcGetTrigger(pt(T,B)),Pts),
  pfcPrintitems(Pts),
  ansi_format([underline],"~NNegative triggers...~n",[]),
  bagof(nt(A,B,C),pfcGetTrigger(nt(A,B,C)),Nts),
  pfcPrintitems(Nts),
  ansi_format([underline],"~NGoal triggers...~n",[]),
  bagof(bct(A,B),pfcGetTrigger(bct(A,B)),Bts),
  pfcPrintitems(Bts).

pfcPrintSupports :- 
  % temporary hack.
  setof((S > P), pfcGetSupport(P,S),L),
  pfcPrintitems(L).

%% pfcFact(P) is true if fact P was asserted into the database via add.

pfcFact(P) :- pfcFact(P,true).

%% pfcFact(P,C) is true if fact P was asserted into the database via
%% add and contdition C is satisfied.  For example, we might do:
%% 
%%  pfcFact(X,pfcUserFact(X))
%%

pfcFact(P,C) :- 
  pfcGetSupport(P,_),
  pfcType(P,fact),
  call(C).

%% pfcFacts(-ListofPfcFacts) returns a list of facts added.

pfcFacts(L) :- pfcFacts(_,true,L).

pfcFacts(P,L) :- pfcFacts(P,true,L).

%% pfcFacts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfcFacts(P,C,L) :- setof(P,pfcFact(P,C),L).

brake(X) :-  pfc(X), break.

%%
%%
%% predicates providing a simple tracing facility
%%

pfcTraceAdd(P) :- 
  % this is here for upward compat. - should go away eventually.
  pfcTraceAdd(P,(o,o)).

pfcTraceAdd(pt(_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfcTraceAdd(nt(_,_,_),_) :-
  % hack for now - never trace triggers.
  !.

pfcTraceAdd(P,S) :-
   pfcTraceAddPrint(P,S),
   pfcTraceBreak(P,S).
   

pfcTraceAddPrint(P,S) :-
  umt(pfcTraced(P)),
  !,
  copy_term(P,Pcopy),
  numbervars(Pcopy,0,_),
  (pfcCurrentUserSupport(S)
       -> ansi_format([fg(green)],"~NAdding (u) ~p~n",[Pcopy])
        ; ansi_format([fg(green)],"~NAdding ~p~n",[Pcopy])).

pfcTraceAddPrint(_,_).


pfcTraceBreak(P,_S) :-
  umt(pfcSpied(P,add)) -> 
   (copy_term(P,Pcopy),
    numbervars(Pcopy,0,_),
    ansi_format([fg(yellow)],"~N~nBreaking on add(~p)~n",[Pcopy]),
    break)
   ; true.

pfcTraceRem(pt(_,_)) :-
  % hack for now - never trace triggers.
  !.
pfcTraceRem(nt(_,_,_)) :-
  % hack for now - never trace triggers.
  !.

pfcTraceRem(P) :-
  (umt(pfcTraced(P))
     -> ansi_format([fg(cyan)],'~NRemoving ~p.',[P])
      ; true),
  (umt(pfcSpied(P,rem))
   -> (ansi_format([fg(yellow)],"~NBreaking on rem(~p)",[P]),
       break)
   ; true).


pfcTrace :- pfcTrace(_).

pfcTrace(Form) :-
  assert(pfcTraced(Form)).

pfcTrace(Form,Condition) :- 
  assert((pfcTraced(Form) :- umt(Condition))).

pfcSpy(Form) :- pfcSpy(Form,[add,rem],true).

pfcSpy(Form,Modes) :- pfcSpy(Form,Modes,true).

pfcSpy(Form,[add,rem],Condition) :-
  !,
  pfcSpy1(Form,add,Condition),
  pfcSpy1(Form,rem,Condition).

pfcSpy(Form,Mode,Condition) :-
  pfcSpy1(Form,Mode,Condition).

pfcSpy1(Form,Mode,Condition) :-
  assert((pfcSpied(Form,Mode) :- umt(Condition))).

pfcNospy :- pfcNospy(_,_,_).

pfcNospy(Form) :- pfcNospy(Form,_,_).

pfcNospy(Form,Mode,Condition) :- 
  clause(pfcSpied(Form,Mode), umt(Condition), Ref),
  erase(Ref),
  fail.
pfcNospy(_,_,_).

pfcNoTrace :- pfcUntrace.
pfcUntrace :- pfcUntrace(_).
pfcUntrace(Form) :- retractall(pfcTraced(Form)).

% needed:  pfcTraceRule(Name)  ...


pfc_trace_msg(Msg) :- pfc_trace_msg('TRACE:','~N~p~N', Msg),!.
% if the correct flag is set, trace exection of Pfc
pfc_trace_msg(Msg,Args) :- pfc_trace_msg('       TRACE:',Msg,Args).
pfc_trace_msg(PreMsg,Msg,Args) :-
    umt(pfcTraceExecution),
    !,
    ansi_format([fg(green)], '~N~n', []),!,
    ansi_format([fg(green)], PreMsg, []),!,
    ansi_format([fg(yellow)], Msg, Args),!.
pfc_trace_msg(_PreMsg,_Msg,_Args).


mpred_notrace_exec:- pfcNoWatch.

mpred_trace_exec:- pfcWatch.

pfcWatch :- assert(pfcTraceExecution).

pfcNoWatch :-  retractall(pfcTraceExecution).

pfcError(Msg) :-  pfcError(Msg,[]).

pfcError(Msg,Args) :- 
  ansi_format([fg(red)],"~N~nERROR/Pfc: ",[]),
  ansi_format([fg(red),bold],Msg,Args),
  ansi_format([underline],"~N",[]).


%%
%% These control whether or not warnings are printed at all.
%%   pfcWarn.
%%   nopfcWarn.
%%
%% These print a warning message if the flag pfcWarnings is set.
%%   pfcWarn(+Message)
%%   pfcWarn(+Message,+ListOfArguments)
%%

pfcWarn :- 
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

nopfcWarn :-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(false)).
 
pfcWarn(Msg) :-  pfcWarn('~p',[Msg]).

pfcWarn(Msg,Args) :- 
  umt(pfcWarnings(true)),
  !,
  ansi_format([fg(red)],"~N~nWARNING/Pfc: ",[]),
  ansi_format([fg(yellow)],Msg,Args),
  ansi_format([underline],"~N",[]).
pfcWarn(_,_).

%%
%% pfcWarnings/0 sets flag to cause pfc warning messages to print.
%% pfcNoWarnings/0 sets flag to cause pfc warning messages not to print.
%%

pfcWarnings :- 
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

pfcNoWarnings :- 
  retractall(pfcWarnings(_)).

:- fixup_exports.
