%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.


:- module(pfccore, []).

:- use_module(library(pfc_pack_xform)).

:- use_module(library(lists)).


:- dynamic ('<-')/2.
:- dynamic ('==>')/2.
:- dynamic ('::::')/2.
%:- dynamic '<==>'/2.
:- dynamic 'pt'/2.
:- dynamic 'nt'/3.
:- dynamic 'bct'/2.
:- dynamic fcUndoMethod/2.
:- dynamic fcAction/2.
:- dynamic fcTmsMode/1.
:- dynamic pfcQueue/1.
:- dynamic pfcDatabase/1.
:- dynamic pfcHaltSignal/1.
:- dynamic pfcDebugging/0.
:- dynamic pfcSelect/1.
:- dynamic pfcSearch/1.
:- dynamic pfcCurrentDb/1.

%%% initialization of global assertons 

%% pfcDefault/1 initialized a global assertion.
%%  pfcDefault(P,Q) - if there is any fact unifying with P, then do 
%%  nothing, else assert Q.

:- export(pfcDefault/2).
:- module_transparent(pfcDefault/2).
pfcDefault(GeneralTerm,Default) :-
  umt((clause(GeneralTerm,true) -> true ; assert(Default))).

%% fcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- initialization(baseKB:pfcDefault(fcTmsMode(_), fcTmsMode(cycles))).

% Pfc Search strategy. pfcSearch(X) where X is one of {direct,depth,breadth}
:- initialization(baseKB:pfcDefault(pfcSearch(_), pfcSearch(direct))).


% 

%% add/2 and post/2 are the main ways to assert new clauses into the
%% database and have forward reasoning done.

%% add(P,S) asserts P into the dataBase with support from S.

add(M:P) :- b_setval(defaultQueryMt,M),b_setval(defaultAssertMt,M),!,
  pfcCurrentUserSupport(UU),
  M:add(P,UU).
add(P) :-  pfcCurrentUserSupport(UU),add(P,UU).

add((==>(P)),S) :- add(P,S).

add(P,S) :- 
  post(P,S),
  pfcRun.

%add(_,_).
%add(P,S) :- pfcWarn("add(~p,~p) failed",[P,S]).


% post(+Ps,+S) tries to add a fact or set of fact to the database.  For
% each fact (or the singelton) post1 is called. It always succeeds.

post([H|T],S) :-
  !,
  post1(H,S),
  post(T,S).
post([],_) :- !.
post(P,S) :- post1(P,S).


% post1(+P,+S) tries to add a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.

post1(M:'==>'(P),S) :-!, post1_(M:P,S).
post1('==>'(P),S) :-!, post1_(P,S).
post1(P,S) :- post1_(P,S),!.

post1_(PIn,S) :- 
  defaultAssertMt(M),
  %% db 
  (pfcAddDbToHead(PIn,P) -> true ; P = PIn),
  % old vesrion 
  nop(pfcRemoveOldVersion(P)),
  pfcAddSupport(P,S),
  pfcUnique(P),
  assert(P),
  pfcTraceAdd(P,S),
  !,
  pfcEnqueue(M,P,S),
  !.

post1_(_,_).
%%post1_(P,S) :-  pfcWarn("add(~p,~p) failed",[P,S]).

%%  pfcAddDbToHead(+P,-NewP) is semidet.
% talkes a fact P or a conditioned fact
% (P:-C) and adds the Db context.
%

pfcAddDbToHead(P,NewP) :-
  umt(pfcCurrentDb(Db)),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   otherwise      -> NewP = (P :- Db)).


%% pfcUnique(X) is det.
% 
% is true if there is no assertion X in the prolog db.
%

pfcUnique((Head:-Tail)) :- 
  !, 
  \+ clause(Head,Tail).
pfcUnique(P) :-
  !,
  \+ clause(P,true).



%% pfcEnqueue(P,Q) is det.
% 
% Enqueu according to settings
%
/*
pfcEnqueue(P,S):- strip_module(P,M,PP),
 % defaultAssertMt(M),
  pfcEnqueue(M,PP,S).
*/
get_pfcSearch(Mode):- umt(pfcSearch(Mode0)),!,Mode0=Mode.
get_pfcSearch(direct).
pfcEnqueue(M,P,S) :-
  must(get_pfcSearch(Mode)) 
    -> (Mode=direct  -> fc(P) ;
	Mode=depth   -> pfcAsserta(pfcQueue(M,P),S) ;
	Mode=breadth -> pfcAssert(pfcQueue(M,P),S) ;
	nop(otherwise)         -> pfcWarn("Unrecognized pfcSearch mode: ~p", Mode))
     ; pfcWarn("No pfcSearch mode").


%% pfcRemoveOldVersion(+Rule) is det.
%
% if there is a rule of the form Identifier ::: Rule then delete it.

pfcRemoveOldVersion((Identifier::::Body)) :-
  % this should never happen.
  (var(Identifier)
  ->
  pfcWarn("variable used as an  rule name in ~p :::: ~p",
          [Identifier,Body]);
  umt((pfcRemoveOldVersion0(Identifier::::Body)))).

  
pfcRemoveOldVersion0((Identifier::::Body)) :-
  nonvar(Identifier),
  clause((Identifier::::OldBody),_),
  \+(Body=OldBody),
  pfc_withdraw((Identifier::::OldBody)),
  !.
pfcRemoveOldVersion0(_).



% 

% pfcRun compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfcQueue mechanism.

pfcRun :-
  (\+ get_pfcSearch(direct)),
  pfcStep,
  pfcRun.
pfcRun.


% pfcStep removes one entry from the pfcQueue and reasons from it.


pfcStep :-  
  % if pfcHaltSignal is true, reset it and fail, thereby stopping inferencing.
  pfcRetract(pfcHaltSignal(Msg)),
  pfc_trace_msg(removing(pfcHaltSignal(Msg))),
  !, 
  fail.

pfcStep :-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P),
  ignore(fc(P)),
  !.

get_next_fact(P) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P) :- 
  umt((
    pfcRetract(pfcQueue(MM,P)),sanity((strip_module(P,M,PP),MM=M),
  must(pfc_remove_supports_quietly(pfcQueue(MM,PP)))))),
  !.
remove_selection(P) :-
  brake(format("~Npfc:get_next_fact - selected fact not on Queue: ~p",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.  
% It tries the user defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact(M:P) :- 
  umt((pfcSelect(M,P))),
  !.  
select_next_fact(P) :- 
  defaultpfcSelect(P),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultpfcSelect(M:PP) :- umt((pfcQueue(MM,P),sanity((strip_module(P,M,PP),MM=M)))),!.

% pfcHalt stops the forward chaining.
pfcHalt :-  pfcHalt("",[]).

pfcHalt(Format) :- pfcHalt(Format,[]).

pfcHalt(Format,Args) :- 
  format(Format,Args),
  umt((pfcHaltSignal -> 
       pfcWarn("pfcHalt finds pfcHaltSignal already set")
     ; assert(pfcHaltSignal))).


%%
%%
%% predicates for manipulating triggers
%%

pfcAddTrigger(pt(Trigger,Body),Support) :-
  !,
  deterministically_must(pfc_trace_msg('      Adding positive trigger: ','~p~n',
		[pt(Trigger,Body)])),
  deterministically_must(pfcAssert(pt(Trigger,Body),Support)),
  copy_term(pt(Trigger,Body),Tcopy),
  deterministically_must(pfc(Trigger)),
  deterministically_must(fcEvalLHS(Body,(Trigger,Tcopy))),
  fail.


pfcAddTrigger(nt(Trigger,Test,Body),Support) :-
  !,
  pfc_trace_msg('      Adding negative trigger: ','~p~n       test: ~p~n       body: ~p~n',
		[Trigger,Test,Body]),
  copy_term(Trigger,TriggerCopy),
  pfcAssert(nt(TriggerCopy,Test,Body),Support),
  \+ call(Test),
  fcEvalLHS(Body,((\+Trigger),nt(TriggerCopy,Test,Body))).

pfcAddTrigger(bct(Trigger,Body),Support) :-
  !,
  pfcAssert(bct(Trigger,Body),Support),
  pfcBtPtCombine(Trigger,Body,Support).

pfcAddTrigger(X,_Support) :-
  pfcWarn("Unrecognized trigger to pfcAddtrigger: ~p",[X]).


pfcBtPtCombine(Head,Body,Support) :- 
  %% a backward trigger (bct) was just added with head and Body and support Support
  %% find any pt's with unifying heads and add the instantied bct body.
  pfcGetTriggerQuick(pt(Head,_PtBody)),
  fcEvalLHS(Body,Support),
  fail.
pfcBtPtCombine(_,_,_) :- !.

pfcGetTriggerQuick(Trigger) :-  umt(clause(Trigger,true)).
% pfcGetTriggerQuick(Trigger) :-  umt(Trigger).

%%
%%
%% predicates for manipulating action traces.
%%

pfcAddActionTrace(Action,Support) :- 
  % adds an action trace and it's support.
  umt((pfcAddSupport(pfcAction(Action),Support))).

pfcRemActionTrace(pfcAction(A)) :-
  umt((fcUndoMethod(A,M),
  M)),
  !.


%%
%% predicates to remove pfc facts, triggers, action traces, and queue items
%% from the database.
%%

pfcRetract(X) :- 
  %% retract an arbitrary thing.
  pfcType(X,Type),
  pfcRetractType(Type,X),
  !.

pfcRetractType(fact,X) :-   
  %% db 
  pfcAddDbToHead(X,X2)-> retract(X2) ; retract(X).

pfcRetractType(rule,X) :- 
  %% db  
  pfcAddDbToHead(X,X2) ->  retract(X2) ; retract(X).

pfcRetractType(trigger,X) :- 
  retract(X)
    -> unFc(X)
     ; pfcWarn("Trigger not found to retract: ~p",[X]).

pfcRetractType(action,X) :- pfcRemActionTrace(X).
  

%% pfcAdd(X) adds item X to some database

pfcAdd(X) :-
  % what type of X do we have?
  pfcType(X,Type),
  % call the appropriate predicate.
  pfcAddType(Type,X).

pfcAddType(fact,X) :- 
  pfcUnique(X), 
  assert(X),!.
pfcAddType(rule,X) :- 
  pfcUnique(X), 
  assert(X),!.
pfcAddType(trigger,X) :- 
  assert(X).
pfcAddType(action,_Action) :- !.


  

%% pfc_withdraw(P,S) removes support S from P and checks to see if P is still supported.
%% If it is not, then the fact is retreactred from the database and any support
%% relationships it participated in removed.

pfc_withdraw(P) :- 
  pfcCurrentUserSupport(UU),
  % iterate down the list of facts to be pfc_withdraw'ed.
  (is_list(P)->
  pfc_withdraw_list(P,UU);
    % pfc_withdraw/1 is the user's interface - it withdraws user support for P.
  pfc_withdraw(P,UU)).
  
  
pfc_withdraw_list(P) :- 
  pfcCurrentUserSupport(UU),
  pfc_withdraw_list(P,UU).

pfc_withdraw_list([H|T],UU) :-
  % pfc_withdraw each element in the list.
  pfc_withdraw(H,UU),
  pfc_withdraw_list(T,UU).

pfc_withdraw(P,S) :-
  % pfcDebug(format("~Nremoving support ~p from ~p",[S,P])),
  (pfc_trace_msg('    Removing support: ','~p~n',[S]),
     pfc_trace_msg('     Which was: ','~p~n',[P])),
  
  ((pfcRemSupport(P,S)
     -> removeIfUnsupported(P)
      ; pfcWarn("pfc_withdraw/2 Could not find support ~p to remove from fact ~p",
                [S,P]))).

%%
%% pfc_remove2 is like pfc_withdraw, but if P is still in the DB after removing the
%% user's support, it is retracted by more forceful means (e.g. remove).
%%

pfc_remove2(P) :-  freeze(UU,pfcCurrentUserSupport(UU)),
  % pfc_remove2/1 is the user's interface - it withdraws user support for P.
  pfc_remove2(P,UU).

pfc_remove2(P,S) :-
  pfc_withdraw(P,S),
  pfc(P)
     -> remove(P) 
      ; true.

%%
%% remove(+F) retracts fact F from the DB and removes any dependent facts */
%%

remove(F) :- 
  pfcRemoveSupports(F),
  fcUndo(F).


% removes any remaining supports for fact F, complaining as it goes.

pfcRemoveSupports(F) :- 
  pfcRemSupport(F,S),
  pfcWarn("~p was still supported by ~p",[F,S]),
  fail.
pfcRemoveSupports(_).

pfc_remove_supports_quietly(F) :- 
  pfcRemSupport(F,_),
  fail.
pfc_remove_supports_quietly(_).

% fcUndo(X) undoes X.


fcUndo(pfcAction(A)) :-  
  % undo an action by finding a method and successfully executing it.
  !,
  pfcRemActionTrace(pfcAction(A)).

fcUndo(pt(Head,Body)) :-  
  % undo a positive trigger.
  %
  !,
  (retract(pt(Head,Body))
    -> unFc(pt(Head,Body))
     ; pfcWarn("Trigger not found to retract: ~p",[pt(Head,Body)])).

fcUndo(nt(Head,Condition,Body)) :-  
  % undo a negative trigger.
  !,
  (retract(nt(Head,Condition,Body))
    -> unFc(nt(Head,Condition,Body))
     ; pfcWarn("Trigger not found to retract: ~p",[nt(Head,Condition,Body)])).

fcUndo(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  retract(Fact),
  pfcTraceRem(Fact),
  unFc1(Fact).
  

%% unFc(P) is det.
%
% unFc(P) "un-forward-chains" from fact f.  That is, fact F has just
% been removed from the database, so remove all support relations it
% participates in and check the things that they support to see if they
% should stayu in the database or should also be removed.


unFc(F) :- 
  pfcRetractSupportRelations(F),
  unFc1(F).

unFc1(F) :-
  pfcUnFcCheckTriggers(F),
  % is this really the right place for pfcRun<?
  pfcRun.

%pfcUnFcCheckTriggers(F):- umt((pfcUnFcCheckTriggers(F))).
pfcUnFcCheckTriggers(F) :-
  pfcType(F,fact),
  copy_term(F,Fcopy),
  umt(nt(Fcopy,Condition,Action)),
  (\+ umt(Condition)),
  fcEvalLHS(Action,((\+F),nt(F,Condition,Action))),
  fail.
pfcUnFcCheckTriggers(_).

pfcRetractSupportRelations(Fact) :-
  pfcType(Fact,Type),
  (Type=trigger -> pfcRemSupport(P,(_,Fact))),
  removeIfUnsupported(P),
  fail.
pfcRetractSupportRelations(Fact) :-
  % pfcType(Fact,Type),
  pfcRemSupport(P,(Fact,_)),
  removeIfUnsupported(P),
  fail.
pfcRetractSupportRelations(_).



%% removeIfUnsupported(+P) checks to see if P is supported and removes
%% it from the DB if it is not.

removeIfUnsupported(P) :- 
   fcSupported(P) -> true ;  fcUndo(P).


%% fcSupported(+P) succeeds if P is "supported". What this means
%% depends on the TMS mode selected.

fcSupported(P) :- 
  must(umt(fcTmsMode(Mode));Mode=cycles),
  pfcSupported(Mode,P).

pfcSupported(local,P) :- !, pfcGetSupport(P,_).
pfcSupported(cycles,P) :-  !, wellFounded(P).
pfcSupported(_,_P) :- true.


%%
%% a fact is well founded if it is supported by the user
%% or by a set of facts and a rules, all of which are well founded.
%%

wellFounded(Fact) :- wf(Fact,[]).

wf(F,_) :-
  % supported by user (axiom) or an "absent" fact (assumption).
  (axiom(F) ; assumption(F)),
  !.

wf(F,Descendants) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supports(F,Supporters),
  % all of whose members are well founded.
  wflist(Supporters,[F|Descendants]),
  !.

%% wflist(L) simply maps wf over the list.

wflist([],_).
wflist([X|Rest],L) :-
  wf(X,L),
  wflist(Rest,L).



% supports(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [user].

supports(F,[Fact|MoreFacts]) :-
  pfcGetSupport(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(U,[]) :- pfcCurrentUserSupport((_,U)), !.
triggerSupports(Trigger,[Fact|MoreFacts]) :-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).


%%
%%
%% fc(X) forward chains from a fact or a list of facts X.
%%


fc([H|T]) :- !, fc1(H), fc(T).
fc([]) :- !.
fc(P) :- fc1(P).

% fc1(+P) forward chains for a single fact.

fc1(Fact) :-
  fc_rule_check(Fact),
  copy_term(Fact,F),
  % check positive triggers
  fcpt(Fact,F),
  % check negative triggers
  fcnt(Fact,F).


%%
%% fc_rule_check(P) does some special, built in forward chaining if P is 
%% a rule.
%% 

fc_rule_check((P==>Q)) :-  
  !,  
  processRule(P,Q,(P==>Q)).
fc_rule_check((Name::::P==>Q)) :- 
  !,  
  processRule(P,Q,(Name::::P==>Q)).
fc_rule_check((P<==>Q)) :- 
  !, 
  processRule(P,Q,(P<==>Q)), 
  processRule(Q,P,(P<==>Q)).
fc_rule_check((Name::::P<==>Q)) :- 
  !, 
  processRule(P,Q,((Name::::P<==>Q))), 
  processRule(Q,P,((Name::::P<==>Q))).

fc_rule_check(('<-'(P,Q))) :-
  !,
  pfcDefineBcRule(P,Q,('<-'(P,Q))).

fc_rule_check(_).


fcpt(Fact,F) :- 
  pfcGetTriggerQuick(pt(F,Body)),
  pfc_trace_msg('      Found positive trigger: ','~p~n       body: ~p~n',
		[F,Body]),
  fcEvalLHS(Body,(Fact,pt(F,Body))),
  fail.

%fcpt(Fact,F) :- 
%  pfcGetTriggerQuick(pt(presently(F),Body)),
%  fcEvalLHS(Body,(presently(Fact),pt(presently(F),Body))),
%  fail.

fcpt(_,_).

fcnt(_Fact,F) :-
  support3(nt(F,Condition,Body),X,_),
  call(Condition),
  pfc_withdraw(X,(_,nt(F,Condition,Body))),
  fail.
fcnt(_,_).


%%
%% pfcDefineBcRule(+Head,+Body,+ParentRule) - defines a backeard
%% chaining rule and adds the corresponding bct triggers to the database.
%%

pfcDefineBcRule(Head,_Body,ParentRule) :-
  (\+ pfcAtom(Head)),
  pfcWarn("Malformed backward chaining rule.  ~p not atomic.",[Head]),
  pfcWarn("rule: ~p",[ParentRule]),
  !,
  fail.

pfcDefineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  pfcCurrentUserSupport((_,U)),
  buildRhs(Head,Rhs),
  forall(pfc_nf(Body,Lhs),
          ignore((buildTrigger(Lhs,rhs(Rhs),Trigger),
           add(bct(Head,Trigger),(ParentRuleCopy,U))))).
 


%%
%%
%% eval something on the LHS of a rule.
%%

 
fcEvalLHS((Test->Body),Support) :-  
  !, 
  (call(Test) -> fcEvalLHS(Body,Support)),
  !.

fcEvalLHS(rhs(X),Support) :-
  !,
  pfc_eval_rhs(X,Support),
  !.

fcEvalLHS(X,Support) :-
  pfcType(X,trigger),
  !,
  pfcAddTrigger(X,Support),
  !.

%fcEvalLHS(snip(X),Support) :- 
%  snip(Support),
%  fcEvalLHS(X,Support).

fcEvalLHS(X,_) :-
  pfcWarn("Unrecognized item found in trigger body, namely ~p.",[X]).


%%
%% eval something on the RHS of a rule.
%%

pfc_eval_rhs([],_) :- !.
pfc_eval_rhs([Head|Tail],Support) :- 
  pfc_eval_rhs1(Head,Support),
  pfc_eval_rhs(Tail,Support).


pfc_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 fcEvalAction(Action,Support).

pfc_eval_rhs1(P,_Support) :-
 % predicate to remove.
 pfcNegatedLiteral(P),
 !,
 pfc_withdraw(P).

pfc_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 pfc_eval_rhs([X|Xrest],Support).

pfc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 post1(Assertion,Support).


pfc_eval_rhs1(X,_) :-
  pfcWarn("Malformed rhs of a rule: ~p",[X]).


%%
%% evaluate an action found on the rhs of a rule.
%%

fcEvalAction(Action,Support) :-
  umt(Action), 
  (undoable(Action) 
     -> pfcAddActionTrace(Action,Support) 
      ; true).


%%
%% 
%%

trigger_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  pfc(Trigger),
%  fcEvalLHS(Body,(presently(Trigger),pt(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  pfc(Trigger),
  fcEvalLHS(Body,(Trigger,pt(TriggerCopy,Body))),
  fail.



%% pfc(F) is nondet.
%
% pfc(F) is true iff F is a fact available for forward chaining.
% Note that this has the side effect of catching unsupported facts and
% assigning them support from God.
%

pfc(P) :-
  % trigger any bc rules.
  pfcGetTriggerQuick(bct(P,Trigger)),
  pfcGetSupport(bct(P,Trigger),S),
  fcEvalLHS(Trigger,S),
  fail.

pfc(F) :- ground(F),!,pfc0(F),!.
pfc(F) :- pfc0(F).
pfc(F) :-
  %- this is probably not advisable due to extreme inefficiency.
  var(F)    ->  pfcFact(F) ;
  otherwise ->  clause(F,Condition),call(Condition).

%- pfc(F) :- 
%-  %% we really need to check for system predicates as well.
%-  % current_predicate(_,F) -> call(F).
%-  clause(F,Condition),call(Condition).


pfc0(F) :- !,
  %- this is probably not advisable due to extreme inefficiency.
  (var(F)    ->  pfcFact(F) ;
  otherwise -> findall(F-C,clause(F,C),List),member(F-C,List),umt(C)).



% an action is undoable if there exists a method for undoing it.
undoable(A) :- umt(fcUndoMethod(A,_)).



%%
%%
%% defining fc rules 
%%

%% pfc_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form 
%% Out.  It also does certain optimizations.  Backtracking into this
%% predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  mpred_nf1(LHS,List2),
  pfc_nf_negations(List2,List).


%% mpred_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
%% Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

mpred_nf1(P,[P]) :- is_ftVar(P), !.


mpred_nf1(P/Cond,[(\+P)/Cond]):- mpred_negated_literal(P), !, dmsg(warn(mpred_nf1(P/Cond,[(\+P)/Cond]))).

mpred_nf1(P/Cond,[P/Cond]):- var(P),!.
mpred_nf1(P/Cond,[P/Cond]):- ((mpred_db_type(P,trigger);mpred_literal_nonvar(P))), !.


% these next two rules are here for upward compatibility and will go 
% away eventually when the P/Condition form is no longer used anywhere.

mpred_nf1(P/Cond,[(\+P)/Cond]) :- pfcNegatedLiteral(P), !.

mpred_nf1(P/Cond,[P/Cond]) :-  pfcAtom(P), !.

%% handle a negated form

mpred_nf1(NegTerm,NF) :-
  pfc_negation(NegTerm,Term),
  !,
  pfc_nf1_negation(Term,NF).

%% disjunction.

mpred_nf1((P;Q),NF) :- 
  !,
  (mpred_nf1(P,NF) ;   mpred_nf1(Q,NF)).


%% conjunction.

mpred_nf1((P,Q),NF) :-
  !,
  mpred_nf1(P,NF1),
  mpred_nf1(Q,NF2),
  append(NF1,NF2,NF).


% prolog_clause mpred_nf1
mpred_nf1((H :- B)  , [(H :- B)]):-  
  mpred_positive_literal(H),!.


%% handle a random atom.

mpred_nf1(P,[P]) :- 
  pfcAtom(P), 
  !.

%%% shouln't we have something to catch the rest as errors?
mpred_nf1(Term,[Term]) :-
  pfcWarn("pfc_nf doesn't know how to normalize ~p",[Term]).


%% pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
pfc_nf1_negation((P/Cond),[(\+(P))/Cond]) :- !.

pfc_nf1_negation((P;Q),NF) :-
  !,
  pfc_nf1_negation(P,NFp),
  pfc_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

pfc_nf1_negation((P,Q),NF) :- 
  % this code is not correct! twf.
  !,
  pfc_nf1_negation(P,NF) 
  ;
  (mpred_nf1(P,Pnf),
   pfc_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

pfc_nf1_negation(P,[\+P]).


%% pfc_nf_negations(List2,List) sweeps through List2 to produce List,
%% changing ~{...} to {\+...}
%%% ? is this still needed? twf 3/16/90

pfc_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

pfc_nf_negations([],[]).

pfc_nf_negations([H1|T1],[H2|T2]) :-
  pfc_nf_negation(H1,H2),
  pfc_nf_negations(T1,T2).

pfc_nf_negation(Form,{\+ X}) :- 
  nonvar(Form),
  Form=(~({X})),
  !.
pfc_nf_negation(X,X).




     %% mpred_unnegate(+N, ?P) is semidet.
     %
     %  is true if N is a negated term and P is the term
     %  with the negation operator stripped.  (not Logical ~ negation however)
     %
     mpred_unnegate(P,_):- is_ftVar(P),!,fail.
     mpred_unnegate((\+(P)),P).
     mpred_unnegate((-P),P).



     %% mpred_negated_literal(+P) is semidet.
     %
     % PFC Negated Literal.
     %
     mpred_negated_literal(P):-
       mpred_unnegate(P,Q),
       mpred_positive_literal(Q).

     orig_2_0_mpred_literal(X):- is_ftVar(X),!.
     orig_2_0_mpred_literal(X):- mpred_negated_literal(X),!.
     orig_2_0_mpred_literal(X):- mpred_positive_literal(X),!.

     mpred_is_trigger(X):-   mpred_db_type(X,trigger).

     mpred_positive_fact(X):-  mpred_positive_literal(X), X \= ~(_), mpred_db_type(X,fact(_FT)), \+ mpred_db_type(X,trigger).

     mpred_positive_literal(X):-
       is_ftNonvar(X),
       \+ mpred_db_type(X,rule(_RT)),
       get_functor(X,F,_),
       \+ mpred_neg_connective(F),
       !.


     mpred_connective(Var):-var(Var),!,fail.
     mpred_connective(';').
     mpred_connective(',').
     mpred_connective('/').
     mpred_connective('{}').
     mpred_connective('|').
     mpred_connective(('==>')).
     mpred_connective(('<-')).
     mpred_connective('==>').
     mpred_connective('-').
     % mpred_connective('~').
     mpred_connective(('\\+')).


     mpred_neg_connective('-').
     % mpred_neg_connective('~').
     mpred_neg_connective('\\+').

     is_simple_lhs(ActN):- is_ftVar(ActN),!,fail.
     is_simple_lhs( \+ _ ):-!,fail.
     is_simple_lhs( ~ _ ):-!,fail.
     is_simple_lhs( _  / _ ):-!,fail.
     is_simple_lhs((Lhs1,Lhs2)):- !,is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs((Lhs1;Lhs2)):- !,is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs(ActN):- is_active_lhs(ActN),!,fail.
     is_simple_lhs((Lhs1/Lhs2)):- !,fail, is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs(_).


     is_active_lhs(ActN):- var(ActN),!,fail.
     is_active_lhs(!).
     is_active_lhs(cut_c).
     is_active_lhs(actn(_Act)).
     is_active_lhs('{}'(_Act)).
     is_active_lhs((Lhs1/Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).
     is_active_lhs((Lhs1,Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).
     is_active_lhs((Lhs1;Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).


     add_lhs_cond(Lhs1/Cond,Lhs2,Lhs1/(Cond,Lhs2)):-!.
     add_lhs_cond(Lhs1,Lhs2,Lhs1/Lhs2).


     %% constrain_meta(+Lhs, ?Guard) is semidet.
     %
     % Creates a somewhat sane Guard.
     %
     % To turn this feature off...
     % ?- set_prolog_flag(constrain_meta,false).  
     %
     %
     constrain_meta(_,_):- current_prolog_flag(constrain_meta,false),!,fail.
     % FACT
     constrain_meta(P,mpred_positive_fact(P)):- is_ftVar(P),!.
     % NEG chaining
     constrain_meta(~ P, CP):- !,  constrain_meta(P,CP).
     constrain_meta(\+ P, CP):- !,  constrain_meta(P,CP).
     % FWD chaining
     constrain_meta((_==>Q),nonvar(Q)):- !, is_ftVar(Q).
     % EQV chaining
     constrain_meta((P<==>Q),(nonvar(Q);nonvar(P))):- (is_ftVar(Q);is_ftVar(P)),!.
     % BWD chaining
     constrain_meta((Q <- _),mpred_literal(Q)):- is_ftVar(Q),!.
     constrain_meta((Q <- _),CQ):- !, constrain_meta(Q,CQ).
     % CWC chaining
     constrain_meta((Q :- _),mpred_literal(Q)):- is_ftVar(Q),!.
     constrain_meta((Q :- _),CQ):- !, constrain_meta(Q,CQ).



%% mpred_db_type(+VALUE1, ?Type) is semidet.
%
% PFC Database Type.
%
%  simple typeing for Pfc objects
%
mpred_db_type(Var,Type):- var(Var),!, Type=fact(_FT).
mpred_db_type(_:X,Type):- !, mpred_db_type(X,Type).
mpred_db_type(~_,Type):- !, Type=fact(_FT).
mpred_db_type(('==>'(_,_)),Type):- !, Type=rule(fwd).
mpred_db_type(('==>'(_,_)),Type):- !, Type=rule(==>).
mpred_db_type(('<-'(_,_)),Type):- !, Type=rule(bwc).
mpred_db_type((':-'(_,_)),Type):- !, Type=rule(cwc).
mpred_db_type(pt(_,_,_),Type):- !, Type=trigger.
mpred_db_type(pt(_,_),Type):- !, Type=trigger.
mpred_db_type(nt(_,_,_),Type):- !,  Type=trigger.
mpred_db_type(bct(_,_),Type):- !,  Type=trigger.
mpred_db_type(actn(_),Type):- !, Type=action.
mpred_db_type((('::::'(_,X))),Type):- !, mpred_db_type(X,Type).
mpred_db_type(_,fact(_FT)):-
  %  if it''s not one of the above, it must_ex be a fact!
  !.



%%
%% buildRhs(+Conjunction,-Rhs)
%%

buildRhs(X,[X]) :- 
  var(X),
  !.

buildRhs((A,B),[A2|Rest]) :- 
  !, 
  pfcCompileRhsTerm(A,A2),
  buildRhs(B,Rest).

buildRhs(X,[X2]) :-
   pfcCompileRhsTerm(X,X2).

pfcCompileRhsTerm((P/C),((P:-C))) :- !.

pfcCompileRhsTerm(P,P).


%% pfc_negation(N,P) is true if N is a negated term and P is the term
%% with the negation operator stripped.

pfc_negation(P,_):- is_ftVar(P),!,fail.
pfc_negation((~P),P).
pfc_negation((-P),P).
pfc_negation((\+(P)),P).

pfcNegatedLiteral(P) :- 
  callable(P),
  pfc_negation(P,Q),
  pfcPositiveAtom(Q).

pfcAtom(X) :- pfcNegatedLiteral(X).
pfcAtom(X) :- pfcPositiveAtom(X).

pfcPositiveAtom(X) :-  
  callable(X),
  functor(X,F,_), 
  \+ pfcConnective(F).

pfcConnective(';').
pfcConnective(',').
pfcConnective('/').
pfcConnective('|').
pfcConnective(('==>')).
pfcConnective(('<-')).
pfcConnective('<==>').

pfcConnective(('==>')).
pfcConnective(('<-=')).
pfcConnective('==>').

pfcConnective('-').
%pfcConnective('~').
pfcConnective(( \+ )).

processRule(Lhs,Rhs,ParentRule) :-
 pfcCurrentUserSupport((_,U)),
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs(Rhs,Rhs2),
  forall(pfc_nf(Lhs,Lhs2), 
          ignore(buildRule(Lhs2,rhs(Rhs2),(ParentRuleCopy,U)))).

buildRule(Lhs,Rhs,Support) :-
  buildTrigger(Lhs,Rhs,Trigger),
  fcEvalLHS(Trigger,Support).

buildTrigger([],Consequent,Consequent).

buildTrigger([V|Triggers],Consequent,pt(V,X)) :-
  var(V),
  !, 
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1/Test)|Triggers],Consequent,nt(T2,Test2,X)) :-
  pfc_negation(T1,T2),
  !, 
  buildNtTest(T2,Test,Test2),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1)|Triggers],Consequent,nt(T2,Test,X)) :-
  pfc_negation(T1,T2),
  !,
  buildNtTest(T2,true,Test),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  buildTrigger(Triggers,Consequent,X).

buildTrigger([T/Test|Triggers],Consequent,pt(T,X)) :-
  !, 
  buildTest(Test,Test2),
  buildTrigger([{Test2}|Triggers],Consequent,X).


%buildTrigger([snip|Triggers],Consequent,snip(X)) :-
%  !,
%  buildTrigger(Triggers,Consequent,X).

buildTrigger([T|Triggers],Consequent,pt(T,X)) :-
  !, 
  buildTrigger(Triggers,Consequent,X).

%%
%% buildNtTest(+,+,-).
%%
%% builds the test used in a negative trigger (nt/3).  This test is a
%% conjunction of the check than no matching facts are in the db and any
%% additional test specified in the rule attached to this ~ term.
%%

buildNtTest(T,Testin,Testout) :-
  buildTest(Testin,Testmid),
  pfcConjoin((pfc(T)),Testmid,Testout).

  
% this just strips away any currly brackets.

buildTest({Test},Test) :- !.
buildTest(Test,Test).

%%



%% simple typeing for pfc objects

pfcType(('==>'(_,_)),Type) :- !, Type=rule.
pfcType(('<==>'(_,_)),Type) :- !, Type=rule.
pfcType(('<-'(_,_)),Type) :- !, Type=rule.
pfcType(pt(_,_,_),Type) :- !, Type=trigger.
pfcType(pt(_,_),Type) :- !, Type=trigger.
pfcType(nt(_,_,_),Type) :- !,  Type=trigger.
pfcType(bct(_,_),Type) :- !,  Type=trigger.
pfcType(pfcAction(_),Type) :- !, Type=action.
pfcType((('::::'(_,X))),Type) :- !, pfcType(X,Type).
pfcType(_,fact) :-
  %% if it's not one of the above, it must be a fact!
  !.

pfcAssert(P,Support) :- 
  (pfc_clause(P) ; assert(P)),
  !,
  pfcAddSupport(P,Support).

pfcAsserta(P,Support) :-
  (pfc_clause(P) ; asserta(P)),
  !,
  pfcAddSupport(P,Support).

pfcAssertz(P,Support) :-
  (pfc_clause(P) ; assertz(P)),
  !,
  pfcAddSupport(P,Support).

pfc_clause((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  clause(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

pfc_clause(Head) :-
  % find a unit clause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  clause(Head_copy,true),
  variant(Head,Head_copy).

pfcForEach(Binder,Body) :- umt(( Binder,pfcdo(Body))),fail.
pfcForEach(_,_).

% pfcdo(X) executes X once and always succeeds.
pfcdo(X) :- umt((X)),!.
pfcdo(_).


%% pfcUnion(L1,L2,L3) - true if set L3 is the result of appending sets
%% L1 and L2 where sets are represented as simple lists.

pfcUnion([],L,L).
pfcUnion([Head|Tail],L,Tail2) :-  
  memberchk(Head,L),
  !,
  pfcUnion(Tail,L,Tail2).
pfcUnion([Head|Tail],L,[Head|Tail2]) :-  
  pfcUnion(Tail,L,Tail2).


%% pfcConjoin(+Conjunct1,+Conjunct2,?Conjunction).
%% arg3 is a simplified expression representing the conjunction of
%% args 1 and 2.

pfcConjoin(C1,C2,C12):- 
  C1 == true -> C12 = C2;
  C2 == true -> C12 = C1;
  otherwise -> C12 = (C1,C2).

/*
pfcConjoin(true,X,X) :- !.
pfcConjoin(X,true,X) :- !.
pfcConjoin(C1,C2,(C1,C2)).
*/

:- fixup_exports.


