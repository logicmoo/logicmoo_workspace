%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure
% :- module(baseKB,[add_PFC/1, add_PFC/2]).

pfcVersion(1.2).

only_support1:- fail.
use_old_names.

    :- volatile(pfctmp:module_dialect_pfc/5).
:- thread_local(pfctmp:module_dialect_pfc/5).

:- meta_predicate(call_SYS(*)).
:- meta_predicate(call_u(*)).
:- meta_predicate(sys_assert(:)).
:- meta_predicate(sys_clause(:,?)).
:- meta_predicate(sys_clause_0(:,?)).
:- meta_predicate(sys_clause(:,?,-)).
:- meta_predicate(sys_asserta(:)).
:- meta_predicate(sys_assertz(:)).
:- meta_predicate(sys_retract(:)).
:- meta_predicate(sys_retractall(:)).
call_u(H):- call(H).
call_SYS(H):- call(H).
dynamic_SYS(MP):- notrace((strip_module(MP,M,P),(P=(F/A)->true;functor(P,F,A)), M:dynamic(F/A))).
sys_clause_0(H,B):- clause(H,B).
sys_clause(H,B):- notrace(predicate_property(H,number_of_clauses(_))), clause(H,B).
sys_clause(H,B,R):- clause(H,B,R).
sys_asserta(H):- asserta(H).
sys_assertz(H):- assertz(H).
sys_assert(H):- assert(H).
sys_retract(H):- retract(H).
sys_retractall(H):- retractall(H).
bagof_PFC(T,C,L):- bagof(T,C,L)*->true;L=[].

%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

/*
:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).
:- if(use_old_names).
:- op(1050,xfx,('=>')).
:- op(1050,xfx,'<=>').
:- op(1050,xfx,('<=')).
:- op(1100,fx,('=>')).
:- endif.
*/
:- module_transparent('term_expansion_PFC'/2).

is_external_directive(module(_,_)).
is_external_directive(encoding(_)).
is_external_directive(trace).
is_exernal_term(begin_of_file).
is_exernal_term(end_of_file).
%term_expansion_PFC('==>'(P,Q),(:- add_PFC(('<-'(Q,P))))).  % speed-up attempt
term_expansion_PFC(MP, O):- strip_module(MP,M,P), term_expansion_PFC(M,P,O).
term_expansion_PFC(_, P, _):- is_exernal_term(P), !, fail.
term_expansion_PFC(M, Term,(:- M:add_PFC(Term))):- pfcType(Term,rule(_)),!.


term_expansion_PFC(M, _, _):- \+ pfctmp:module_dialect_pfc(_,_,M,_,_), !, fail.
term_expansion_PFC(_, _, _):- \+ prolog_load_context(dialect, pfc), !, fail.
term_expansion_PFC(M, (:- P),(:- M:call_PFC(P))):- !, \+ is_external_directive(P).
term_expansion_PFC(M, P, (:- M:add_PFC(P))).
%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.

:- use_module(library(lists)).

%:- dynamic ('==>')/2.
%:- dynamic ('::::')/2.
%:- dynamic '<==>'/2.
'==>'(P,Q):- throw(illegal_PFC('==>'(P,Q))).
'<==>'(P,Q):- throw(illegal_PFC('<==>'(P,Q))).
'<-'(P,Q):- throw(illegal_PFC('<-'(P,Q))).
'::::'(P,Q):- throw(illegal_PFC('::::'(P,Q))).
'==>'(P):- throw(illegal_PFC('==>'(P))).

:- dynamic 'pt'/2.
:- dynamic 'nt'/3.
:- dynamic 'bt'/2.
:- dynamic fcUndoMethod/2.
:- dynamic fcAction/2.
:- dynamic fcTmsMode/1.
:- dynamic pfcQueue/1.
:- dynamic pfcDatabase/1.
:- dynamic pfcHaltSignal/0.
:- dynamic pfcDebugging/0.
:- dynamic pfcSelect/1.
:- dynamic pfcSearch/1.

%=% initialization of global assertons 

%= pfcDefault/1 initialized a global assertion.
%=  pfcDefault(P,Q) - if there is any fact unifying with P, then do 
%=  nothing, else assert Q.

pfcDefault(GeneralTerm,Default) :-
  sys_clause(GeneralTerm,true) -> true ; sys_assert(Default).

%= fcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- pfcDefault(fcTmsMode(_), fcTmsMode(cycles)).

% Pfc Search strategy. pfcSearch(X) where X is one of {direct,depth,breadth}
:- pfcDefault(pfcSearch(_), pfcSearch(direct)).


% 
:- if(use_old_names).
add(X) :- add_PFC(X).
:- endif.
%= add_PFC/2 and post_PFC/2 are the main ways to sys_assert new clauses into the
%= database and have forward reasoning done.

%= add_PFC(P,S) asserts P into the dataBase with support from S.

add_PFC(P) :- add_PFC(P,(user,user)).

:- if(use_old_names).
add_PFC(('=>'(P)),S) :- add_PFC(P,S).
:- endif.
add_PFC('==>'(P),S) :- add_PFC(P,S).

add_PFC(P,S) :- 
  post_PFC(P,S),
  pfcRun.

%add_PFC(_,_).
%add_PFC(P,S) :- pfcWarn("add_PFC(~q,~q) failed",[P,S]).


% post_PFC(+Ps,+S) tries to add a fact or set of fact to the database.  For
% each fact (or the singelton) post1 is called. It always succeeds.

:- if(use_old_names).
post(P,S) :- post_PFC(P,S).
:- endif.
post_PFC([H|T],S) :-
  !,
  post1_PFC(H,S),
  post_PFC(T,S).
post_PFC([],_) :- !.
post_PFC(P,S) :- post1_PFC(P,S).


:- if(use_old_names).
post1(X) :- post1_PFC(X).
:- endif.
% post1_PFC(+P,+S) tries to add a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.

:- if(use_old_names).
post1(P,S) :- post1_PFC(P,S).
:- endif.
post1_PFC(P,S) :- 
  %= db pfcAddDbToHead(P,P2),
  % removeOldVersion_PFC(P),
  dynamic_SYS(P),
  pfcAddSupport(P,S),
  pfcUnique(P),
  sys_assert(P),
  pfcTraceAdd(P,S),
  !,
  pfcEnqueue(P,S),
  !.

post1_PFC(_,_).
%=post1_PFC(P,S) :-  pfcWarn("add_PFC(~q,~q) failed",[P,S]).

%=
%= pfcAddDbToHead(+P,-NewP) talkes a fact P or a conditioned fact
%= (P:-C) and adds the Db context.
%=

:- dynamic(pfcCurrentDb/1).
pfcAddDbToHead(P,NewP) :-
  pfcCurrentDb(Db),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   true   -> NewP = (P :- Db)).
% was otherwise

% pfcUnique(X) is true if there is no sys_assertion X in the prolog db.

pfcUnique((Head:-Tail)) :- 
  !, 
  \+ sys_clause(Head,Tail).
pfcUnique(P) :-
  !,
  \+ sys_clause(P,true).


pfcEnqueue(P,S) :-
  pfcSearch(Mode) 
    -> (Mode=direct  -> fc_PFC(P) ;
	Mode=depth   -> pfcAsserta(pfcQueue(P),S) ;
	Mode=breadth -> pfcAssert(pfcQueue(P),S) ;
	true/*otehrwise*/  -> pfcWarn("Unrecognized pfcSearch mode: ~q", Mode))
     ; pfcWarn("No pfcSearch mode").


% if there is a rule of the form Identifier ::: Rule then delete it.

removeOldVersion_PFC('::::'(Identifier,Body)) :-
  % this should never happen.
  var(identifier),
  !,
  pfcWarn("variable used as an  rule name in ~q :::: ~q",
          [Identifier,Body]).

  
removeOldVersion_PFC('::::'(Identifier,Body)) :-
  nonvar(Identifier),
  sys_clause('::::'(Identifier,OldBody),_),
  \+(Body=OldBody),
  rem_PFC('::::'(Identifier,OldBody)),
  !.
removeOldVersion_PFC(_).



% 

% pfcRun compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfcQueue mechanism.

pfcRun :-
  (\+ pfcSearch(direct)),
  pfcStep,
  pfcRun.
pfcRun.


% pfcStep removes one entry from the pfcQueue and reasons from it.


pfcStep :-  
  % if pfcHaltSignal is true, reset it and fail, thereby stopping inferencing.
  pfcRetract(pfcHaltSignal),
  !, 
  fail.

pfcStep :-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact_PFC(P),
  pfcdo(fc_PFC(P)),
  !.

get_next_fact_PFC(P) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact_PFC(P),
  remove_selection_PFC(P).

remove_selection_PFC(P) :- 
  pfcRetract(pfcQueue(P)),
  rem_PFCoveSupportsQuietly(pfcQueue(P)),
  !.
remove_selection_PFC(P) :-
  brake_PFC(format("~Npfc:get_next_fact - selected fact not on Queue: ~q",
               [P])).


% select_next_fact_PFC(P) identifies the next fact to reason from.  
% It tries the user defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact_PFC(P) :- 
  pfcSelect(P),
  !.  
select_next_fact_PFC(P) :- 
  defaultpfcSelect(P),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultpfcSelect(P) :- pfcQueue(P),!.

% pfcHalt stops the forward chaining.
pfcHalt :-  pfcHalt("",[]).

pfcHalt(Format) :- pfcHalt(Format,[]).

pfcHalt(Format,Args) :- 
  format(Format,Args),
  pfcHaltSignal -> 
       pfcWarn("pfcHalt finds pfcHaltSignal already set")
     ; sys_assert(pfcHaltSignal).


%=
%=
%= predicates for manipulating triggers
%=


pfcAddTrigger(pt(Trigger,Body),Support) :-
  !,
  pfc_trace_msg('~n      Adding positive trigger ~q~n',
		[pt(Trigger,Body)]),
  pfcAssert(pt(Trigger,Body),Support),
  copy_term(pt(Trigger,Body),Tcopy),
  call_PFC(Trigger),
  fcEvalLHS(Body,(Trigger,Tcopy)),
  fail.


pfcAddTrigger(nt(Trigger,Test,Body),Support) :-
  !,
  pfc_trace_msg('~n      Adding negative trigger: ~q~n       test: ~q~n       body: ~q~n',
		[Trigger,Test,Body]),
  copy_term(Trigger,TriggerCopy),
  pfcAssert(nt(TriggerCopy,Test,Body),Support),
  \+Test,
  fcEvalLHS(Body,((\+Trigger),nt(TriggerCopy,Test,Body))).

pfcAddTrigger(bt(Trigger,Body),Support) :-
  !,
  pfcAssert(bt(Trigger,Body),Support),
  pfcBtPtCombine(Trigger,Body,Support).

pfcAddTrigger(X,_Support) :-
  pfcWarn("Unrecognized trigger to pfcAddtrigger: ~q",[X]).


pfcBtPtCombine(Head,Body,Support) :- 
  %= a backward trigger (bt) was just added with head and Body and support Support
  %= find any pt's with unifying heads and add the instantied bt body.
  pfcGetTriggerQuick(pt(Head,_PtBody)),
  fcEvalLHS(Body,Support),
  fail.
pfcBtPtCombine(_,_,_) :- !.

pfcGetTriggerQuick(Trigger) :-  sys_clause_0(Trigger,true).

%=
%=
%= predicates for manipulating action traces.
%=

pfcAddActionTrace(Action,Support) :- 
  % adds an action trace and it's support.
  pfcAddSupport(pfcAction(Action),Support).

pfcRemActionTrace(pfcAction(A)) :-
  fcUndoMethod(A,M),
  M,
  !.


%=
%= predicates to remove pfc facts, triggers, action traces, and queue items
%= from the database.
%=

pfcRetract(X) :- 
  %= retract an arbitrary thing.
  pfcType(X,Type),
  pfcRetractType(Type,X),
  !.

pfcRetractType(fact,X) :-   
  %= db pfcAddDbToHead(X,X2), sys_retract(X2). 
  sys_retract(X).

pfcRetractType(rule(_),X) :- 
  %= db  pfcAddDbToHead(X,X2),  sys_retract(X2).
  sys_retract(X).
pfcRetractType(trigger,X) :- 
  sys_retract(X)
    -> unFc(X)
     ; pfcWarn("Trigger not found to sys_retract: ~q",[X]).

pfcRetractType(action,X) :- pfcRemActionTrace(X).
  

%= pfcAdd(X) adds item X to some database

pfcAdd(X) :-
  % what type of X do we have?
  pfcType(X,Type),
  % call the appropriate predicate.
  pfcAddType(Type,X).

pfcAddType(fact,X) :- 
  pfcUnique(X), 
  sys_assert(X),!.
pfcAddType(rule(_),X) :- 
  pfcUnique(X), 
  sys_assert(X),!.
pfcAddType(trigger,X) :- 
  sys_assert(X).
pfcAddType(action,_Action) :- !.


  

%= rem_PFC(P,S) removes support S from P and checks to see if P is still supported.
%= If it is not, then the fact is retreactred from the database and any support
%= relationships it participated in removed.
:- if(use_old_names).
rem(X) :- rem_PFC(X).
:- endif.

rem_PFC(List) :- 
  % iterate down the list of facts to be rem_PFC'ed.
  nonvar(List),
  List=[_|_],
  remlist_PFC(List).
  
rem_PFC(P) :- 
  % rem_PFC/1 is the user's interface - it withdraws user support for P.
  rem_PFC(P,(user,user)).

:- if(use_old_names).
remlist(X) :- remlist_PFC(X).
:- endif.
remlist_PFC([H|T]) :-
  % rem each element in the list.
  rem_PFC(H,(user,user)),
  remlist_PFC(T).

:- if(use_old_names).
rem(P,S) :- rem_PFC(P,S).
:- endif.
rem_PFC(P,S) :-
  % pfcDebug(format("~Nremoving support ~q from ~q",[S,P])),
  pfc_trace_msg('~n    Removing support: ~q from ~q~n',[S,P]),
  pfcRemSupport(P,S)
     -> removeIfUnsupported_PFC(P)
      ; pfcWarn("rem_PFC/2 Could not find support ~q to remove from fact ~q",
                [S,P]).

%=
%= rem2 is like rem_PFC, but if P is still in the DB after removing the
%= user's support, it is sys_retracted by more forceful means (e.g. remove_PFC).
%=

:- if(use_old_names).
rem2(X) :- rem2_PFC(X).
:- endif.
rem2_PFC(P) :- 
  % rem2_PFC/1 is the user's interface - it withdraws user support for P.
  rem2_PFC(P,(user,user)).

:- if(use_old_names).
rem2(P,S) :- rem2_PFC(P,S).
:- endif.
rem2_PFC(P,S) :-
  rem_PFC(P,S),
  call_PFC(P)
     -> remove_PFC(P) 
      ; true.

%=
%= remove_PFC(+F) sys_retracts fact F from the DB and removes any dependent facts */
%=

:- if(use_old_names).
remove(X) :- remove_PFC(X).
:- endif.
remove_PFC(F) :- 
  pfcRemoveSupports(F),
  fcUndo(F).


% removes any remaining supports for fact F, complaining as it goes.

pfcRemoveSupports(F) :- 
  pfcRemSupport(F,S),
  pfcWarn("~q was still supported by ~q",[F,S]),
  fail.
pfcRemoveSupports(_).

rem_PFCoveSupportsQuietly(F) :- 
  pfcRemSupport(F,_),
  fail.
rem_PFCoveSupportsQuietly(_).

% fcUndo(X) undoes X.


fcUndo(pfcAction(A)) :-  
  % undo an action by finding a method and successfully executing it.
  !,
  pfcRemActionTrace(pfcAction(A)).

fcUndo(pt(Key,Head,Body)) :-  
  % undo a positive trigger.
  %
  !,
  (sys_retract(pt(Key,Head,Body))
    -> unFc(pt(Head,Body))
     ; pfcWarn("Trigger not found to sys_retract: ~q",[pt(Head,Body)])).

fcUndo(nt(Head,Condition,Body)) :-  
  % undo a negative trigger.
  !,
  (sys_retract(nt(Head,Condition,Body))
    -> unFc(nt(Head,Condition,Body))
     ; pfcWarn("Trigger not found to sys_retract: ~q",[nt(Head,Condition,Body)])).

fcUndo(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  sys_retract(Fact),
  pfcTraceRem(Fact),
  unFc1(Fact).
  


%= unFc(P) "un-forward-chains" from fact f.  That is, fact F has just
%= been removed from the database, so remove all support relations it
%= participates in and check the things that they support to see if they
%= should stayu in the database or should also be removed.


unFc(F) :- 
  pfcRetractSupportRelations(F),
  unFc1(F).

unFc1(F) :-
  pfcUnFcCheckTriggers(F),
  % is this really the right place for pfcRun<?
  pfcRun.


pfcUnFcCheckTriggers(F) :-
  pfcType(F,fact),
  copy_term(F,Fcopy),
  nt(Fcopy,Condition,Action),
  (\+ Condition),
  fcEvalLHS(Action,((\+F),nt(F,Condition,Action))),
  fail.
pfcUnFcCheckTriggers(_).

pfcRetractSupportRelations(Fact) :-
  pfcType(Fact,Type),
  (Type=trigger -> pfcRemSupport(P,(_,Fact))
                ; pfcRemSupport(P,(Fact,_))),
  removeIfUnsupported_PFC(P),
  fail.
pfcRetractSupportRelations(_).



%= removeIfUnsupported_PFC(+P) checks to see if P is supported and removes
%= it from the DB if it is not.

:- if(use_old_names).
removeIfUnsupported(P) :- removeIfUnsupported_PFC(P).
:- endif.

removeIfUnsupported_PFC(P) :- 
   fcSupported(P) -> true ;  fcUndo(P).


%= fcSupported(+P) succeeds if P is "supported". What this means
%= depends on the TMS mode selected.

fcSupported(P) :- 
  fcTmsMode(Mode),
  pfcSupported(Mode,P).

pfcSupported(local,P) :- !, pfcGetSupport(P,_).
pfcSupported(cycles,P) :-  !, wellFounded_PFC(P).
pfcSupported(_,_P) :- true.


%=
%= a fact is well founded if it is supported by the user
%= or by a set of facts and a rules, all of which are well founded.
%=

wellFounded_PFC(Fact) :- wf_PFC(Fact,[]).

:- if(use_old_names).
wf(X) :- wf_PFC(X).
:- endif.
wf_PFC(F,_) :-
  % supported by user (axiom_PFC) or an "absent" fact (assumption_PFC).
  (axiom_PFC(F) ; assumption_PFC(F)),
  !.

wf_PFC(F,Descendants) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supports_PFC(F,Supporters),
  % all of whose members are well founded.
  wflist_PFC(Supporters,[F|Descendants]),
  !.

%= wflist_PFC(L) simply maps wf over the list.

wflist_PFC([],_).
wflist_PFC([X|Rest],L) :-
  wf_PFC(X,L),
  wflist_PFC(Rest,L).



% supports_PFC(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [user].

supports_PFC(F,[Fact|MoreFacts]) :-
  pfcGetSupport(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(user,[]) :- !.
triggerSupports(Trigger,[Fact|MoreFacts]) :-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).


%=
%=
%= fc_PFC(X) forward chains from a fact or a list of facts X.
%=

:- if(use_old_names).
fc(X) :- fc_PFC(X).
:- endif.

fc_PFC([H|T]) :- !, fc1_PFC(H), fc_PFC(T).
fc_PFC([]) :- !.
fc_PFC(P) :- fc1_PFC(P).

% fc1(+P) forward chains for a single fact.

:- if(use_old_names).
fc1(X) :- fc1_PFC(X).
:- endif.

fc1_PFC(Fact) :-
  fc_rule_check(Fact),
  copy_term(Fact,F),
  % check positive triggers
  fcpt(Fact,F),
  % check negative triggers
  fcnt(Fact,F).


%=
%= fc_rule_check(P) does some special, built in forward chaining if P is 
%= a rule.
%= 

fc_rule_check('==>'(P,Q)) :-  
  !,  
  processRule_PFC(P,Q,'==>'(P,Q)).
fc_rule_check('::::'(Name,'==>'(P,Q))) :- 
  !,  
  processRule_PFC(P,Q,'::::'(Name,'==>'(P,Q))).
fc_rule_check('<==>'(P,Q)) :- 
  !, 
  processRule_PFC(P,Q,'<==>'(P,Q)), 
  processRule_PFC(Q,P,'<==>'(P,Q)).
fc_rule_check('::::'(Name,'<==>'(P,Q))) :- 
  !, 
  processRule_PFC(P,Q,('::::'(Name,'<==>'(P,Q)))), 
  processRule_PFC(Q,P,('::::'(Name,'<==>'(P,Q)))).

fc_rule_check(('<-'(P,Q))) :-
  !,
  pfcDefineBcRule(P,Q,('<-'(P,Q))).

fc_rule_check(_).


fcpt(Fact,F) :- 
  pfcGetTriggerQuick(pt(F,Body)),
  pfc_trace_msg('~n      Found positive trigger: ~q~n       body: ~q~n',
		[F,Body]),
  fcEvalLHS(Body,(Fact,pt(F,Body))),
  fail.
fcpt(_,_).

%fcpt(Fact,F) :- 
%  pfcGetTriggerQuick(pt(presently(F),Body)),
%  fcEvalLHS(Body,(presently(Fact),pt(presently(F),Body))),
%  fail.

fcnt1(_Fact,F) :-
  support1(X,_,nt(F,Condition,Body)),
  call_u(Condition),
  rem_PFC(X,(_,nt(F,Condition,Body))),
  fail.
fcnt1(_,_).
fcnt(Fact,F):- only_support1, !, fcnt1(Fact,F).
:- if( \+ only_support1 ).
fcnt(_Fact,F) :-
  support3(nt(F,Condition,Body),X,_),
  Condition,
  rem_PFC(X,(_,nt(F,Condition,Body))),
  fail.
:- endif.
fcnt(_,_).


%=
%= pfcDefineBcRule(+Head,+Body,+ParentRule) - defines a backeard
%= chaining rule and adds the corresponding bt triggers to the database.
%=

pfcDefineBcRule(Head,_Body,ParentRule) :-
  (\+ pfcLiteral(Head)),
  pfcWarn("Malformed backward chaining rule.  ~q not atomic.",[Head]),
  pfcWarn("rule: ~q",[ParentRule]),
  !,
  fail.

pfcDefineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs_PFC(Head,Rhs),
  foreach_PFC(pfc_nf(Body,Lhs),
          (buildTrigger_PFC(Lhs,rhs(Rhs),Trigger),
           add_PFC(bt(Head,Trigger),(ParentRuleCopy,user)))).
 


%=
%=
%= eval something on the LHS of a rule.
%=

 
fcEvalLHS((Test->Body),Support) :-  
  !, 
  (call_u(Test) -> fcEvalLHS(Body,Support)),
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
  pfcWarn("Unrecognized item found in trigger body, namely ~q.",[X]).


%=
%= eval something on the RHS of a rule.
%=

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
 rem_PFC(P).

pfc_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 pfc_eval_rhs([X|Xrest],Support).

pfc_eval_rhs1(Assertion,Support) :-
 % an sys_assertion to be added.
 post1_PFC(Assertion,Support).


pfc_eval_rhs1(X,_) :-
  pfcWarn("Malformed rhs of a rule: ~q",[X]).


%=
%= evaluate an action found on the rhs of a rule.
%=

fcEvalAction(Action,Support) :-
  call_u(Action), 
  (undoable_PFC(Action) 
     -> pfcAddActionTrace(Action,Support) 
      ; true).


%=
%= 
%=

trigger_trigger_PFC(Trigger,Body,_Support) :-
 trigger_trigger_PFC1(Trigger,Body).
trigger_trigger_PFC(_,_,_).


%trigger_trigger_PFC1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  call_PFC(Trigger),
%  fcEvalLHS(Body,(presently(Trigger),pt(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger_PFC1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  call_PFC(Trigger),
  fcEvalLHS(Body,(Trigger,pt(TriggerCopy,Body))),
  fail.



%=
%= call_PFC(F) is true iff F is a fact available for forward chaining.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%=
:- if(use_old_names).
pfc(X) :- call_PFC(X).
:- endif.

call_PFC(P) :-
  % trigger any bc rules.
  bt(P,Trigger),
  pfcGetSupport(bt(P,Trigger),S),
  fcEvalLHS(Trigger,S),
  fail. 
  % keep going ...
call_PFC(P) :- predicate_property(P, built_in), !, call_SYS(P).
call_PFC(F) :-
  %= this is probably not advisable due to extreme inefficiency.
  var(F)    ->  pfcFact(F) ;
  /*otherwise*/ true ->  sys_clause(F,Condition),call_u(Condition).

%=call_PFC(F) :- 
%=  %= we really need to check for system predicates as well.
%=  % current_predicate(_,F) -> call_u(F).
%=  sys_clause(F,Condition),call_u(Condition).


% an action is undoable if there exists a method for undoing it.
undoable_PFC(A) :- fcUndoMethod(A,_).



%=
%=
%= defining fc rules 
%=

%= pfc_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form 
%= Out.  It also does certain optimizations.  Backtracking into this
%= predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  pfc_nf1(LHS,List2),
  pfc_nf_negations(List2,List).


%= pfc_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
%= Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

pfc_nf1(P,[P]) :- var(P), !.

% these next two rules are here for upward compatibility and will go 
% away eventually when the P/Condition form is no longer used anywhere.

pfc_nf1(P/Cond,[(\+P)/Cond]) :- pfcNegatedLiteral(P), !.

pfc_nf1(P/Cond,[P/Cond]) :-  pfcLiteral(P), !.

%= handle a negated form

pfc_nf1(NegTerm,NF) :-
  pfc_negation(NegTerm,Term),
  !,
  pfc_nf1_negation(Term,NF).

%= disjunction.

pfc_nf1((P;Q),NF) :- 
  !,
  (pfc_nf1(P,NF) ;   pfc_nf1(Q,NF)).


%= conjunction.

pfc_nf1((P,Q),NF) :-
  !,
  pfc_nf1(P,NF1),
  pfc_nf1(Q,NF2),
  append(NF1,NF2,NF).

%= handle a random atomic literal.

pfc_nf1(P,[P]) :- 
  pfcLiteral(P), 
  !.

%=% shouln't we have something to catch the rest as errors?
pfc_nf1(Term,[Term]) :-
  pfcWarn("pfc doesn't know how to normalize ~q",[Term]).


%= pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
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
  (pfc_nf1(P,Pnf),
   pfc_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

pfc_nf1_negation(P,[\+P]).


%= pfc_nf_negations(List2,List) sweeps through List2 to produce List,
%= changing ~{...} to {\+...}
%=% ? is this still needed? twf 3/16/90

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


%=
%= buildRhs_PFC(+Conjunction,-Rhs)
%=

buildRhs_PFC(X,[X]) :- 
  var(X),
  !.

buildRhs_PFC((A,B),[A2|Rest]) :- 
  !, 
  pfcCompileRhsTerm(A,A2),
  buildRhs_PFC(B,Rest).

buildRhs_PFC(X,[X2]) :-
   pfcCompileRhsTerm(X,X2).

pfcCompileRhsTerm((P/C),((P:-C))) :- !.

pfcCompileRhsTerm(P,P).


%= pfc_negation(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

pfc_negation('~'(P),P).
pfc_negation((-P),P).
pfc_negation((\+(P)),P).

pfcNegatedLiteral(P) :- 
  pfc_negation(P,Q),
  pfcPostiveLiteral(Q).

:- if(use_old_names).
pfcAtom(X) :- pfcLiteral(X).
:- endif. 
pfcLiteral(X) :- pfcNegatedLiteral(X).
pfcLiteral(X) :- pfcPostiveLiteral(X).

pfcPostiveLiteral(X) :-  
  functor(X,F,_), 
  \+ pfcConnective(F).

pfcConnective(';').
pfcConnective(',').
pfcConnective('/').
pfcConnective('|').
pfcConnective(('==>')).
pfcConnective(('<-')).
pfcConnective('<==>').

pfcConnective('-').
pfcConnective('~').
pfcConnective('\\+').

processRule_PFC(Lhs,Rhs,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs_PFC(Rhs,Rhs2),
  foreach_PFC(pfc_nf(Lhs,Lhs2), 
          buildRule_PFC(Lhs2,rhs(Rhs2),(ParentRuleCopy,user))).

buildRule_PFC(Lhs,Rhs,Support) :-
  buildTrigger_PFC(Lhs,Rhs,Trigger),
  fcEvalLHS(Trigger,Support).

buildTrigger_PFC([],Consequent,Consequent).

buildTrigger_PFC([V|Triggers],Consequent,pt(V,X)) :-
  var(V),
  !, 
  buildTrigger_PFC(Triggers,Consequent,X).

buildTrigger_PFC([(T1/Test)|Triggers],Consequent,nt(T2,Test2,X)) :-
  pfc_negation(T1,T2),
  !, 
  buildNtTest_PFC(T2,Test,Test2),
  buildTrigger_PFC(Triggers,Consequent,X).

buildTrigger_PFC([(T1)|Triggers],Consequent,nt(T2,Test,X)) :-
  pfc_negation(T1,T2),
  !,
  buildNtTest_PFC(T2,true,Test),
  buildTrigger_PFC(Triggers,Consequent,X).

buildTrigger_PFC([{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  buildTrigger_PFC(Triggers,Consequent,X).

buildTrigger_PFC([T/Test|Triggers],Consequent,pt(T,X)) :-
  !, 
  buildTest_PFC(Test,Test2),
  buildTrigger_PFC([{Test2}|Triggers],Consequent,X).


%buildTrigger_PFC([snip|Triggers],Consequent,snip(X)) :-
%  !,
%  buildTrigger_PFC(Triggers,Consequent,X).

buildTrigger_PFC([T|Triggers],Consequent,pt(T,X)) :-
  !, 
  buildTrigger_PFC(Triggers,Consequent,X).

%=
%= buildNtTest_PFC(+,+,-).
%=
%= pfc_builds the test used in a negative trigger (nt/3).  This test is a
%= conjunction of the check than no matching facts are in the db and any
%= additional test specified in the rule attached to this ~ term.
%=

buildNtTest_PFC(T,Testin,Testout) :-
  buildTest_PFC(Testin,Testmid),
  pfcConjoin((call_PFC(T)),Testmid,Testout).

  
% this just strips away any currly brackets.

buildTest_PFC({Test},Test) :- !.
buildTest_PFC(Test,Test).

%=



%= simple typeing for pfc objects
pfcType(P, Type):- pfcType_0(P, VType), !, VType=Type.

pfcType_0(('==>'(_, _)), rule(_)).
pfcType_0(('<==>'(_, _)), rule(_)).
pfcType_0(('<-'(_, _)), rule(_)).
pfcType_0(pt(_, _, _), trigger).
pfcType_0(pt(_, _), trigger).
pfcType_0(nt(_, _, _), trigger).
pfcType_0(bt(_, _), trigger).
pfcType_0(pfcAction(_), action).
pfcType_0((('::::'(_, X))), Type) :- !,  pfcType_0(X, Type).
pfcType_0(_, fact) :-
  %= if it's not one of the above, it must be a fact!
  !.

pfcAssert(P,Support) :- 
  (pfc_clause(P) ; sys_assert(P)),
  !,
  pfcAddSupport(P,Support).

pfcAsserta(P,Support) :-
  (pfc_clause(P) ; sys_asserta(P)),
  !,
  pfcAddSupport(P,Support).

pfcAssertz(P,Support) :-
  (pfc_clause(P) ; sys_assertz(P)),
  !,
  pfcAddSupport(P,Support).

pfc_clause((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  sys_clause(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

pfc_clause(Head) :-
  % find a unit sys_clause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  sys_clause(Head_copy,true),
  variant(Head,Head_copy).

foreach_PFC(Binder,Body) :- call_u(Binder),pfcdo(Body),fail.
foreach_PFC(_,_).

% pfcdo(X) executes X once and always succeeds.
pfcdo(X) :- X,!.
pfcdo(_).


%= pfcUnion(L1,L2,L3) - true if set L3 is the result of appending sets
%= L1 and L2 where sets are represented as simple lists.

pfcUnion([],L,L).
pfcUnion([Head|Tail],L,Tail2) :-  
  memberchk(Head,L),
  !,
  pfcUnion(Tail,L,Tail2).
pfcUnion([Head|Tail],L,[Head|Tail2]) :-  
  pfcUnion(Tail,L,Tail2).


%= pfcConjoin(+Conjunct1,+Conjunct2,?Conjunction).
%= arg3 is a simplified expression representing the conjunction of
%= args 1 and 2.

pfcConjoin(true,X,X) :- !.
pfcConjoin(X,true,X) :- !.
pfcConjoin(C1,C2,(C1,C2)).


%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%=	restore, reset, etc.0

% pfcDatabaseTerm(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

pfcDatabaseTerm(support1/3).
:- if( \+ only_support1 ).
pfcDatabaseTerm(support2/3).
pfcDatabaseTerm(support3/3).
:- endif.
pfcDatabaseTerm(pt/3).
pfcDatabaseTerm(bt/3).
pfcDatabaseTerm(nt/4).
pfcDatabaseTerm('==>'/2).
pfcDatabaseTerm('<==>'/2).
pfcDatabaseTerm('<-'/2).
pfcDatabaseTerm(pfcQueue/1).

% removes all forward chaining rules and justifications from db.

pfcReset :-
  sys_clause(support1(P,F,Trigger),true),
  pfcRetractOrWarn(P),
  pfcRetractOrWarn(support1(P,F,Trigger)),
  pfcRetractOrWarn(support2(F,Trigger,P)),
  pfcRetractOrWarn(support3(Trigger,P,F)),
  fail.
pfcReset :-
  pfcDatabaseItem(T),
  pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n",[T]).
pfcReset.

% true if there is some pfc crud still in the database.
pfcDatabaseItem(Term) :-
  pfcDatabaseTerm(P/A),
  functor(Term,P,A),
  sys_clause(Term,_).

pfcRetractOrWarn(X) :-  sys_retract(X), !.
pfcRetractOrWarn(X) :- 
  pfcWarn("Couldn't sys_retract ~p.",[X]).



%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

:- dynamic pfcTraced/1.
:- dynamic pfcSpied/2.
:- dynamic pfcTraceExecution/0.
:- dynamic   pfcWarnings/1.

:- pfcDefault(pfcWarnings(_), pfcWarnings(true)).

%= predicates to examine the state of pfc

pfcQueue :- listing(pfcQueue/1).

pfcPrintDB :-
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
  pfcPrintSupports.

%= pfcPrintFacts ...

pfcPrintFacts :- pfcPrintFacts(_,true).

pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern,true).

pfcPrintFacts(P,C) :-
  pfcFacts(P,C,L),
  pfcClassifyFacts(L,User,Pfc,_Rule),
  format("~n~nUser added facts:",[]),
  pfcPrintitems(User),
  format("~n~nPfc added facts:",[]),
  pfcPrintitems(Pfc).


%= printitems clobbers it's arguments - beware!

pfcPrintitems([]).
pfcPrintitems([H|T]) :-
  \+ \+ (numbervars(H,0,_), format("~n  ~q",[H])),
  pfcPrintitems(T).

pfcClassifyFacts([],[],[],[]).

pfcClassifyFacts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule(_)),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],[H|User],Pfc,Rule) :-
  pfcGetSupport(H,(user,user)),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],User,[H|Pfc],Rule) :-
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcPrintRules :-
  bagof_PFC('==>'(P,Q),sys_clause('==>'(P,Q),true),R1),
  pfcPrintitems(R1),
  bagof_PFC('<==>'(P,Q),sys_clause('<==>'(P,Q),true),R2),
  pfcPrintitems(R2),
  bagof_PFC('<-'(P,Q),sys_clause('<-'(P,Q),true),R3),
  pfcPrintitems(R3).

pfcPrintTriggers :-
  format("Positive triggers...~n",[]),
  bagof_PFC(pt(T,B),pfcGetTrigger(pt(T,B)),Pts),
  pfcPrintitems(Pts),
  format("Negative triggers...~n",[]),
  bagof_PFC(nt(A,B,C),pfcGetTrigger(nt(A,B,C)),Nts),
  pfcPrintitems(Nts),
  format("Goal triggers...~n",[]),
  bagof_PFC(bt(A,B),pfcGetTrigger(bt(A,B)),Bts),
  pfcPrintitems(Bts).

pfcGetTrigger(Trigger):- call_PFC(Trigger).

pfcPrintSupports :- 
  % temporary hack.
  setof((S > P), pfcGetSupport(P,S),L),
  pfcPrintitems(L).

%= pfcFact(P) is true if fact P was sys_asserted into the database via add_PFC.

pfcFact(P) :- pfcFact(P,true).

%= pfcFact(P,C) is true if fact P was sys_asserted into the database via
%= add and contdition C is satisfied.  For example, we might do:
%= 
%=  pfcFact(X,pfcUserFact(X))
%=

pfcFact(P,C) :- 
  pfcGetSupport(P,_),
  pfcType(P,fact),
  call_u(C).

%= pfcFacts(-ListofPfcFacts) returns a list of facts added.

pfcFacts(L) :- pfcFacts(_,true,L).

pfcFacts(P,L) :- pfcFacts(P,true,L).

%= pfcFacts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfcFacts(P,C,L) :- setof(P,pfcFact(P,C),L).

:- if(use_old_names).
brake(X) :- brake_PFC(X).
:- endif.
brake_PFC(X) :-  X, break.

%=
%=
%= predicates providing a simple tracing facility
%=

pfcTraceAdd(P) :- 
  % this is here for upward compat. - should go away eventually.
  pfcTraceAdd(P,(o,o)).

pfcTraceAdd(pt(_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfcTraceAdd(nt(_,_),_) :-
  % hack for now - never trace triggers.
  !.

pfcTraceAdd(P,S) :-
   pfcTraceAddPrint(P,S),
   pfcTraceBreak(P,S).
   

pfcTraceAddPrint(P,S) :-
  pfcTraced(P),
  !,
  copy_term(P,Pcopy),
  numbervars(Pcopy,0,_),
  (S=(user,user)
       -> format("~nAdding (u) ~q",[Pcopy])
        ; format("~nAdding ~q",[Pcopy])).

pfcTraceAddPrint(_,_).


pfcTraceBreak(P,_S) :-
  pfcSpied(P,add) -> 
   (copy_term(P,Pcopy),
    numbervars(Pcopy,0,_),
    format("~nBreaking on add(~q)",[Pcopy]),
    break)
   ; true.

pfcTraceRem(pt(_,_)) :-
  % hack for now - never trace triggers.
  !.
pfcTraceRem(nt(_,_)) :-
  % hack for now - never trace triggers.
  !.

pfcTraceRem(P) :-
  (pfcTraced(P) 
     -> format('~nRemoving ~q.',[P])
      ; true),
  (pfcSpied(P,rem)
   -> (format("~nBreaking on rem(~q)",[P]),
       break)
   ; true).


pfcTrace :- pfcTrace(_).

pfcTrace(Form) :-
  sys_assert(pfcTraced(Form)).

pfcTrace(Form,Condition) :- 
  sys_assert((pfcTraced(Form) :- call_SYS(Condition))).

pfcSpy(Form) :- pfcSpy(Form,[add,rem],true).

pfcSpy(Form,Modes) :- pfcSpy(Form,Modes,true).

pfcSpy(Form,[add,rem],Condition) :-
  !,
  pfcSpy1(Form,add,Condition),
  pfcSpy1(Form,rem,Condition).

pfcSpy(Form,Mode,Condition) :-
  pfcSpy1(Form,Mode,Condition).

pfcSpy1(Form,Mode,Condition) :-
  sys_assert((pfcSpied(Form,Mode) :- call_SYS(Condition))).

pfcNospy :- pfcNospy(_,_,_).

pfcNospy(Form) :- pfcNospy(Form,_,_).

pfcNospy(Form,Mode,Condition) :- 
  sys_clause(pfcSpied(Form,Mode), call_SYS(Condition), Ref),
  erase(Ref),
  fail.
pfcNospy(_,_,_).

pfcNoTrace :- pfcUntrace.
pfcUntrace :- pfcUntrace(_).
pfcUntrace(Form) :- sys_retractall(pfcTraced(Form)).

% needed:  pfcTraceRule(Name)  ...


% if the correct flag is set, trace exection of Pfc
pfc_trace_msg(Msg,Args) :-
    pfcTraceExecution,
    !,
    format(user_output, Msg, Args).
pfc_trace_msg(_Msg,_Args).

pfcWatch :- sys_assert(pfcTraceExecution).

pfcNoWatch :-  sys_retractall(pfcTraceExecution).

pfcError(Msg) :-  pfcError(Msg,[]).

pfcError(Msg,Args) :- 
  format("~nERROR/Pfc: ",[]),
  format(Msg,Args).


%=
%= These control whether or not warnings are printed at all.
%=   pfcWarn.
%=   nopfcWarn.
%=
%= These print a warning message if the flag pfcWarnings is set.
%=   pfcWarn(+Message)
%=   pfcWarn(+Message,+ListOfArguments)
%=

pfcWarn :- 
  sys_retractall(pfcWarnings(_)),
  sys_assert(pfcWarnings(true)).

nopfcWarn :-
  sys_retractall(pfcWarnings(_)),
  sys_assert(pfcWarnings(false)).
 
pfcWarn(Msg) :-  pfcWarn(Msg,[]).

pfcWarn(Msg,Args) :- 
  pfcWarnings(true),
  !,
  format("~nWARNING/Pfc: ",[]),
  format(Msg,Args).
pfcWarn(_,_).

%=
%= pfcWarnings/0 sets flag to cause pfc warning messages to print.
%= pfcNoWarnings/0 sets flag to cause pfc warning messages not to print.
%=

pfcWarnings :- 
  sys_retractall(pfcWarnings(_)),
  sys_assert(pfcWarnings(true)).

pfcNoWarnings :- 
  sys_retractall(pfcWarnings(_)).

%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a fact *****


:- use_module(library(lists)).

justification_PFC(F,J) :- supports_PFC(F,J).

justifications_PFC(F,Js) :- bagof_PFC(J,justification_PFC(F,J),Js).



%= base_PFC(P,L) - is true iff L is a list of "base" facts which, taken
%= together, allows us to deduce P.  A base fact is an axiom (a fact 
%= added by the user or a raw Prolog fact (i.e. one w/o any support))
%= or an assumption.

base_PFC(F,[F]) :- (axiom_PFC(F) ; assumption_PFC(F)),!.

base_PFC(F,L) :-
  % i.e. (reduce 'append (map 'base (justification f)))
  justification_PFC(F,Js),
  bases_PFC(Js,L).


%= bases_PFC(L1,L2) is true if list L2 represents the union of all of the 
%= facts on which some conclusion in list L1 is based.

bases_PFC([],[]).
bases_PFC([X|Rest],L) :-
  base_PFC(X,Bx),
  bases_PFC(Rest,Br),
  pfcUnion(Bx,Br,L).
	
axiom_PFC(F) :- 
  pfcGetSupport(F,(user,user)); 
  pfcGetSupport(F,(god,god)).

%= an assumption is a failed goal, i.e. were assuming that our failure to 
%= prove P is a proof of not(P)

assumption_PFC(P) :- pfc_negation(P,_).
   
%= assumptions_PFC(X,As) if As is a set of assumptions which underly X.

assumptions_PFC(X,[X]) :- assumption_PFC(X).
assumptions_PFC(X,[]) :- axiom_PFC(X).
assumptions_PFC(X,L) :-
  justification_PFC(X,Js),
  assumptions_PFC1(Js,L).

assumptions_PFC1([],[]).
assumptions_PFC1([X|Rest],L) :-
  assumptions_PFC(X,Bx),
  assumptions_PFC1(Rest,Br),
  pfcUnion(Bx,Br,L).  


%= pfcProofTree(P,T) the proof tree for P is T where a proof tree is
%= of the form
%=
%=     [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%=          ^                         and has the form of
%=          [J11, J12,... J1n]      a list of proof trees.


% pfcChild(P,Q) is true iff P is an immediate justifier for Q.
% mode: pfcChild(+,?)

pfcChild(P,Q) :-
  pfcGetSupport(Q,(P,_)).

pfcChild(P,Q) :-
  pfcGetSupport(Q,(_,Trig)),
  pfcType(Trig,trigger),
  pfcChild(P,Trig).

pfcChildren(P,L) :- bagof_PFC(C,pfcChild(P,C),L).

% pfcDescendant(P,Q) is true iff P is a justifier for Q.

pfcDescendant(P,Q) :- 
   pfcDescendant1(P,Q,[]).

pfcDescendant1(P,Q,Seen) :-
  pfcChild(X,Q),
  (\+ member(X,Seen)),
  (P=X ; pfcDescendant1(P,X,[X|Seen])).
  
pfcDescendants(P,L) :- 
  bagof_PFC(Q,pfcDescendant1(P,Q,[]),L).


%=
%=
%= predicates for manipulating support relationships
%=

%= pfcAddSupport(+Fact,+Support)
pfcAddSupport(P,(Fact,Trigger)) :- only_support1, !, sys_assert(support1(P,Fact,Trigger)).
:- if( \+ only_support1 ).
pfcAddSupport(P,(Fact,Trigger)) :-
  sys_assert(support1(P,Fact,Trigger)),
  sys_assert(support2(Fact,Trigger,P)),
  sys_assert(support3(Trigger,P,Fact)).
:- endif.

pfcGetSupport(P,(Fact,Trigger)) :- only_support1, !, support1(P,Fact,Trigger).
:- if( \+ only_support1 ).
pfcGetSupport(P,(Fact,Trigger)) :-
   nonvar(P)         -> support1(P,Fact,Trigger) 
   ; nonvar(Fact)    -> support2(Fact,Trigger,P) 
   ; nonvar(Trigger) -> support3(Trigger,P,Fact) 
   ; true /*otherwise*/  -> support1(P,Fact,Trigger).
:- endif.


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.
pfcRemSupport(P,(Fact,Trigger)) :- only_support1, !, pfcRetractOrWarn(support1(P,Fact,Trigger)).

:- if( \+ only_support1 ).
pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(P),
  !,
  pfcRetractOrWarn(support1(P,Fact,Trigger)),
  pfcRetractOrWarn(support2(Fact,Trigger,P)),
  pfcRetractOrWarn(support3(Trigger,P,Fact)).

pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(Fact),
  !,
  pfcRetractOrWarn(support2(Fact,Trigger,P)),
  pfcRetractOrWarn(support1(P,Fact,Trigger)),
  pfcRetractOrWarn(support3(Trigger,P,Fact)).

pfcRemSupport(P,(Fact,Trigger)) :-
  pfcRetractOrWarn(support3(Trigger,P,Fact)),
  pfcRetractOrWarn(support1(P,Fact,Trigger)),
  pfcRetractOrWarn(support2(Fact,Trigger,P)).
:- endif.


pfc_collect_supports(Tripples) :-
  bagof_PFC(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

pfc_support_relation((P,F,T)) :-
  call_u(support1(P,F,T)).

pfc_make_supports((P,S1,S2)) :- 
  pfcAddSupport(P,(S1,S2)),
  (pfcAdd(P); true),
  !.

%= pfcTriggerKey(+Trigger,-Key) 
%=
%= Arg1 is a trigger.  Key is the best term to index it on.

pfcTriggerKey(pt(Key,_),Key).
pfcTriggerKey(pt(Key,_,_),Key).
pfcTriggerKey(nt(Key,_,_),Key).
pfcTriggerKey(Key,Key).


%=^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger base clause that stores the trigger.
%=

pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).

%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:- use_module(library(lists)).

:- thread_local(whymemory_PFC/2).

:- dynamic(support1/3).
:- if( \+ only_support1 ).
:- dynamic(support2/3).
:- dynamic(support3/3).
:- endif.

pfcWhy :- 
  whymemory_PFC(P,_),
  pfcWhy(P).

pfcWhy(N) :-
  number(N),
  !,
  whymemory_PFC(P,Js),
  pfcWhyCommand(N,P,Js).

pfcWhy(P) :-
  justifications_PFC(P,Js),
  sys_retractall(whymemory_PFC(_,_)),
  sys_assert(whymemory_PFC(P,Js)),
  pfcWhyBrouse(P,Js).

pfcWhy1(P) :-
  justifications_PFC(P,Js),
  pfcWhyBrouse(P,Js).

pfcWhyBrouse(P,Js) :-
  pfcShowJustifications(P,Js),
  pfcAsk(' >> ',Answer),
  pfcWhyCommand(Answer,P,Js).

pfcWhyCommand(q,_,_) :- !.
pfcWhyCommand(h,_,_) :- 
  !,
  format("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",[]).

pfcWhyCommand(N,_P,Js) :-
  float(N),
  !,
  pfcSelectJustificationNode(Js,N,Node),
  pfcWhy1(Node).

pfcWhyCommand(u,_,_) :-
  % u=up
  !.

pfcCommand(N,_,_) :-
  integer(N),
  !,
  format("~n~w is a yet unimplemented command.",[N]),
  fail.

pfcCommand(X,_,_) :-
 format("~n~w is an unrecognized command, enter h. for help.",[X]),
 fail.

why_PFC(P):- 
  justifications_PFC(P,Js),
  pfcShowJustifications(P,Js), !.

pfcShowJustifications(P,Js) :-
  format("~nJustifications for ~q:",[P]),
  pfcShowJustification1(Js,1).

pfcShowJustification1([],_).

pfcShowJustification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  pfcShowJustifications2(J,N,1),
  N2 is N+1,
  pfcShowJustification1(Js,N2).

pfcShowJustifications2([],_,_).

pfcShowJustifications2([C|Rest],JustNo,StepNo) :- 
  copy_term(C,CCopy),
  numbervars(CCopy,0,_),
  format("~n    ~q.~q ~q",[JustNo,StepNo,CCopy]),
  StepNext is 1+StepNo,
  pfcShowJustifications2(Rest,JustNo,StepNext).

pfcAsk(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

pfcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth0(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth0(StepNo,Justification,Step).
 

:-source_location(S,_),prolog_load_context(module,FM),
 forall(source_file(M:H,S),
  ignore((functor(H,F,A),
   \+ atom_concat('$',_,F),
      M:export(M:F/A),
   \+ predicate_property(M:H,transparent),
%    dra_w(M:H),
   % (format(user_error,'~N~q.~n',[FM:module_transparent(M:F/A)]),
   \+ atom_concat('__aux',_,F), FM:module_transparent(M:F/A)))).


:- multifile('term_expansion'/2).
:- module_transparent('term_expansion'/2).
term_expansion(I, PosIn, O, PosOut):- notrace(nonvar(I)), term_expansion_PFC(I,O), PosOut=PosIn.


