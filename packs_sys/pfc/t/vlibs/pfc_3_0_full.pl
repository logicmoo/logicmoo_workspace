/*   
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/
:- if(current_prolog_flag(xref,true)).  % XREF
                                      
:- module(pfc_full,[]).

:- else. % XREF
:- prolog_load_context(file,File),unload_file(File).

:- if(('$current_typein_module'(TM),'$current_source_module'(SM),'context_module'(CM),
   Info = (baseKB:'using_pfc'(TM,SM,CM,pfc_full)),
   dmsg(Info),
   asserta(Info))).  
:- endif.


:- if((use_module(library(logicmoo_utils)))). 
:- endif.
:- if(use_module(library(make))). 
:- endif.
:- if((reexport(library(logicmoo_utils)))). 
:- endif.
 
:- if((prolog_load_context(source,File),prolog_load_context(file,File))).
:- prolog_load_context(file,File),unload_file(File).
:- endif.

:- endif. % \+ XREF

begin_pfc.

mpred_positive_literal(P):- pfcPositiveLiteral(P).

:- ifprolog:import(date:day_of_the_week/2).
:- ifprolog:import(date:day_of_the_year/2).
:- '$messages':import(wfs:delays_residual_program/2).
:- '$messages':import(prolog_listing:portray_clause/3).

:- 'pldoc':export(pldoc:doc_browser/0).
:- 'pldoc':export(pldoc:doc_server/0).
:- '$toplevel':import(pldoc:doc_browser/0).
:- '$toplevel':import(pldoc:doc_server/0).

:- '$tabling':import(reeval/1).
:- '$toplevel':import(prolog_dump_runtime_variables/0).
:- '$toplevel':import(window_title/1).
:- '$toplevel':import(win_registry_get_value/3).


==>(_).


/*
*/
:- dynamic(baseKB:pfc_is_tracing_exec/0).
:- export(baseKB:pfc_is_tracing_exec/0).

pfc_database_term_syntax(do_and_undo,2,rule(_)).

pfc_database_term_syntax(('::::'),2,rule(_)).
pfc_database_term_syntax((<-),2,rule(_)).
pfc_database_term_syntax((<==>),2,rule(_)).
pfc_database_term_syntax((==>),2,rule(_)).

pfc_database_term_syntax(mdefault,1,fact(_)).
pfc_database_term_syntax((==>),1,fact(_)).
pfc_database_term_syntax((~),1,fact(_)).


pfc_database_term(F,A,syntaxic(T)):- pfc_database_term_syntax(F,A,T).
pfc_database_term(F,A,T):- pfc_core_database_term(F,A,T).

pfc_core_database_term(genlPreds,2,fact(_)).
% pfc_core_database_term(rtArgsVerbatum,1,fact(_)).

% forward,backward chaining database
pfc_core_database_term(spft,3,support).

pfc_core_database_term(nt,3,trigger(pt)).
pfc_core_database_term(pt,2,trigger(nt)).
pfc_core_database_term(bt,2,trigger(bt)).

% transient state
pfc_core_database_term(pfcAction,1,state).
pfc_core_database_term(pfcQueue,1,state).
pfc_core_database_term(pfcHaltSignal,1,state).




% forward,backward settings
pfc_core_database_term(pfcCurrentDb,1,setting).
pfc_core_database_term(pfcSelect,1,setting).
pfc_core_database_term(fcTmsMode,1,setting).
pfc_core_database_term(fcMode,1,setting).

% debug settings
pfc_core_database_term(pfcTraceExecution,0,debug).
%pfc_core_database_term(lmcache:pfc_is_spying_pred,2,debug).
pfc_core_database_term(pfcWarnings,1,debug).
% pfc_core_database_term(t_l:whybuffer,2,debug).

pfc_core_database_term(mpred_prop,4,fact(_)).

pfc_core_database_term(predicateConventionMt,2,fact(_)).
% pfc_core_database_term(genlMt,2,fact(_)).
%pfc_core_database_term(arity,2,fact(_)).
%pfc_core_database_term(rtArgsVerbatum,1,fact(_)).


declare_pfc_support(Dyn,M):- forall(pfc_database_term(F,A,_),M:call(Dyn,M:F/A)).
declare_pfc_support(M):- declare_pfc_support(dynamic,M).
export_pfc_support(M):- declare_pfc_support(export,M).
import_pfc_support(M,Into):- declare_pfc_support(Into:import,M).

import_everywhere(BaseKB):-   
   declare_pfc_support(BaseKB),
   export_pfc_support(BaseKB),
   import_pfc_support(BaseKB,system).

:- thread_local(t_l:whybuffer/2).
% :- dynamic(baseKB:que/2).

%:- meta_predicate show_pfc_success(*,*).
%show_pfc_success(Type,G):- G*->pfc_trace_msg(success(Type,G)) ; fail.

% :- ensure_loaded(library(logicmoo_utils)).


tilded_negation.

bagof_PFC(T,G,L):- bagof(T,G,L)*->true;L=[].
setof_or_nil(T,G,L):- setof(T,G,L)*->true;L=[].

add(X):- pfcAdd(X).
pfcAtom(X):- pfcLiteral(X).
rem(X):- pfcWithdraw(X).
rem2(X):- pfcRemove(X).
remove(X):- pfcBlast(X).

% :- mpred_ain_in_thread.
% :- current_thread_pool(ain_pool)->true;thread_pool_create(ain_pool,20,[]).
:- multifile thread_pool:create_pool/1.
:- dynamic thread_pool:create_pool/1.
thread_pool:create_pool(ain_pool) :-
    thread_pool_create(ain_pool, 50, [detached(true)] ).

%:- use_module(library(http/thread_httpd)).
:- use_module(library(thread_pool)).

is_ain_pool_empty:- thread_pool_property(ain_pool,running(N)),!,N==0.
is_ain_pool_empty.

show_ain_pool:- forall(thread_pool_property(ain_pool,PP),fmt(show_ain_pool(PP))).

await_ain_pool:- is_ain_pool_empty->true;(repeat, sleep(0.005), is_ain_pool_empty).

ain_in_thread(MAIN):- strip_module(MAIN,M,AIN), call_in_thread(M:pfcAdd(AIN)).

call_in_thread(MG):- strip_module(MG,M,G), notrace((copy_term(M:G,GG,_),numbervars(GG,0,_),term_to_atom(GG,TN))), 
 call_in_thread(TN,M,G),
  dmsg_pretty(call_in_thread(TN,M,G)).

call_in_thread(TN,M,G):- thread_property(_,alias(TN)),!,dmsg_pretty(already_queued(M,G)).
call_in_thread(TN,M,G):- must(current_why(Why)), thread_create_in_pool(ain_pool,call_in_thread_code(M,G,Why,TN),_Id,[alias(TN)]).

call_in_thread_code(M,G,Why,TN):- 
 with_only_current_why(Why,
   catch(( M:G-> nop(dmsg_pretty(suceeded(exit,TN)));dmsg_pretty(failed(exit,TN))),E, dmsg_pretty(error(E-->TN)))).

:- call_in_thread(wdmsg(call_in_thread)).
% why_dmsg(Why,Msg):- with_current_why(Why,dmsg_pretty(Msg)).

%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure

pfcVersion(3.0).

/*
pfcFile('pfcsyntax').	% operator declarations.
pfcFile('pfccore').	% core of Pfc.
pfcFile('pfcsupport').	% support maintenance
pfcFile('pfcdb').	% predicates to manipulate database.
pfcFile('pfcdebug').	% debugging aids (e.g. tracing).
pfcFile('pfcjust').	% predicates to manipulate justifications.
pfcFile('pfcwhy').	% interactive exploration of justifications.

pfcLoad :- pfcFile(F), ensure_loaded(F), fail.
pfcLoad.
*/

%pfcFcompile :- pfcFile(F), compile(F), fail.
%pfcFcompile.

%:- pfcLoad.

%   File   : pfccompile.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%   Purpose: compile system file for Pfc
/*
:- compile(pfcsyntax).
:- compile(pfccore).
:- compile(pfcdb).
:- compile(pfcjust).
:- compile(pfcwhy).
:- compile(pfcdebug).
*/

%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.


:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

:- include(pfc_expansion).


:- dynamic(pfctmp:knows_will_table_as/2).

will_table_as(Stuff,As):- pfctmp:knows_will_table_as(Stuff,As),!.
will_table_as(Stuff,As):- assert(pfctmp:knows_will_table_as(Stuff,As)),
   must(react_tabling(Stuff,As)),!,fail.

react_tabling(Stuff,_):- dynamic(Stuff).

:- dynamic(lmconf:is_treated_like_pfc_file/1).
:- dynamic(lmconf:is_treated_like_pfc_module/1).
if_pfc_indicated :- source_location(F,_),(sub_string(F, _, _, _, '.pfc')->true;lmconf:is_treated_like_pfc_file(F)),!.
if_pfc_indicated :- prolog_load_context(module, M),is_pfc_module(M),!.

pfc_term_expansion((:- table Stuff as Type), [:- pfcAdd(tabled_as(Stuff,Type)),(:- table Stuff as Type)]):- !, if_pfc_indicated, \+ will_table_as(Stuff, Type).
pfc_term_expansion((:- table Stuff ), [:- pfcAdd(tabled_as(Stuff,incremental)),(:- table Stuff as incremental)]):- if_pfc_indicated, \+ will_table_as(Stuff,incremental).
pfc_term_expansion((:- _),_):- !, fail.
pfc_term_expansion((P==>Q),(:- pfcAdd((P==>Q)))).
%term_expansion((P==>Q),(:- pfcAdd(('<-'(Q,P))))).  % speed-up attempt
pfc_term_expansion(('<-'(P,Q)),(:- pfcAdd(('<-'(P,Q))))).
pfc_term_expansion((P<==>Q),(:- pfcAdd((P<==>Q)))).
pfc_term_expansion((RuleName :::: Rule),(:- pfcAdd((RuleName :::: Rule)))).
pfc_term_expansion((==>P),(:- pfcAdd(P))).
pfc_term_expansion(I,I):- (I == end_of_file; I=begin_of_file),!.
pfc_term_expansion(( M:H :- B) ,(:- M:pfcAdd(H :- B))):- is_pfc_module(M).
pfc_term_expansion( M:P ,(:- M:pfcAdd(P))):- is_pfc_module(M).
pfc_term_expansion( P ,(:- pfcAdd(P))):- if_pfc_indicated.

term_subst(P,O):- term_subst(clause,P,O),!.

term_subst(_, P,O):- \+ compound(P),!,O=P.

term_subst(tilded_negation,P,O):- !, term_subst(
  [(not)-(~),
   (=>)-(==>),
   (<=>)-(<==>),
   (<=)-(<-)],P,O).

term_subst(Subst,P,O):- 
 compound_name_arguments(P,F,Args),
 maplist(term_subst(Subst),Args,ArgsL),
 termf_subst(Subst,F,F2),
 compound_name_arguments(O,F2,ArgsL).

termf_subst(Subst,F,F2):-member(F-F2,Subst)->true;F=F2.


%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.

:- use_module(library(lists)).


:- multifile ('<-')/2.
:- dynamic ('<-')/2.
:- discontiguous(('<-')/2).

/*
:- multifile ('==>')/2.
:- dynamic ('==>')/2.
:- discontiguous(('==>')/2).

%:- multifile ('==>')/2.
:- dynamic ('::::')/2.
%:- dynamic '<==>'/2.
:- dynamic 'pt'/2.
:- dynamic 'nt'/3.
:- dynamic 'bt'/2.
:- dynamic do_and_undo/2.
:- dynamic fcAction/2.
:- dynamic fcTmsMode/1.
:- dynamic pfcQueue/1.
:- dynamic pfcCurrentDb/1.
:- dynamic pfcHaltSignal/1.
:- dynamic pfcDebugging/0.
:- dynamic pfcSelect/1.
:- dynamic fcMode/1.
:- dynamic spft/3.
*/

:- thread_local(t_l:pfcSearchTL/1).


% %%% initialization of global assertons 

pfcSetVal(Stuff):- 
   duplicate_term(Stuff,DStuff),
   functor(DStuff,_,N),
   setarg(N,DStuff,_),
   retractall(DStuff),
   assert(Stuff).

% %% pfcDefault/1 initialized a global assertion.
% %%  pfcDefault(P,Q) - if there is any fact unifying with P, then do 
% %%  nothing, else assert Q.

pfcDefault(GeneralTerm,Default) :-
  clause(GeneralTerm,true) -> true ; assert(Default).

% %% fcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- pfcDefault(fcTmsMode(_), fcTmsMode(cycles)).

% Pfc Search strategy. fcMode(X) where X is one of {direct,depth,breadth}
:- pfcDefault(fcMode(_), fcMode(direct)).


% 

% %% pfcAdd/2 and pfcPost/2 are the main ways to assert new clauses into the
% %% database and have forward reasoning done.

% %% pfcAdd(P,S) asserts P into the dataBase with support from S.

pfcAdd(P) :-  must(current_why_UU(UU)), pfcAdd(P, UU).

pfcAdd((==>P),S) :- !, pfcAdd(P,S).

pfcAdd(P,S) :- 
  pfcPost(P,S),
  pfcRun,!.

%pfcAdd(_,_).
pfcAdd(P,S) :- pfcWarn("pfcAdd(~p,~p) failed",[P,S]).


% pfcPost(+Ps,+S) tries to add a fact or set of fact to the database.  For
% each fact (or the singelton) pfcPost1 is called. It always succeeds.

pfcPost(List,S):- pfcPost_rev(S,List).

pfcPost_rev(S,Term) :-  
  is_list(Term) 
  -> maplist(pfcPost_rev(S),Term)
  ; pfcPost0(Term,S).

pfcPost0(P,S):- 
 quietly(must(full_transform(pfcPost0,P,PP))),
 pfcPost1(PP,S).

ain_fast(P) :-  must(current_why_UU(UU)), pfcPost1(P, UU).

% pfcPost1(+P,+S) tries to add a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.

pfcPost1((P1,P2),S) :- !, pfcPost1(P1,S),pfcPost1(P2,S).
pfcPost1(P,S) :- 
  %% db pfcAddDbToHead(P,P2),
  % pfcRemoveOldVersion(P),
  must(pfcAddSupport(P,S)),
  pfcUnique(post, P),
  must(assert(P)),
  must(pfcTraceAdd(P,S)),
  !,
  must(pfcEnqueue(P,S)),
  !.

pfcPost1(_,_).
%pfcPost1(P,S) :-  
 %pfcWarn("pfcPost1: ~p\n (support: ~p) failed",[P,S]).

% %%  pfcAddDbToHead(+P,-NewP) is semidet.
% talkes a fact P or a conditioned fact
% (P:-C) and adds the Db context.
%

pfcAddDbToHead(P,NewP) :-
  pfcCallSystem(pfcCurrentDb(Db)),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   true      -> NewP = (P :- Db)).

:- dynamic(pfcCurrentDb/1).
pfcCurrentDb(true).

% %% pfcUnique(X) is det.
% 
% is true if there is no assertion X in the prolog db.
%

pfcUnique(_Type,(Head:-Tail)) :- 
  !, 
  \+ clause(Head,Tail).
pfcUnique(_Type, P) :-
  \+ clause(P,true).


% %% pfcEnqueue(P,Q) is det.
% 
% Enqueu according to settings
%
pfcSetSearch(Mode):- pfcSetVal(fcMode(Mode)).

pfcGetSearch(Mode):- (t_l:pfcSearchTL(ModeT)->true;fcMode(ModeT))->Mode=ModeT.

pfcEnqueue(P,S) :-
  pfcGetSearch(Mode)
    -> (Mode=direct  -> pfcFwd(P) ;
	Mode=thread   -> pfcThreadFwd(P,S) ;
    Mode=depth   -> pfcAsserta(pfcQueue(P),S) ;
	Mode=breadth -> pfcAssert(pfcQueue(P),S) ;
	true         -> pfcWarn("Unrecognized fcMode mode: ~p", Mode))
     ; pfcWarn("No fcMode mode").



% %% pfcRemoveOldVersion(+Rule) is det.
%
% if there is a rule of the form Identifier ::: Rule then delete it.

pfcRemoveOldVersion((Identifier::::Body)) :-
  % this should never happen.
  (var(Identifier)
  ->
  pfcWarn("variable used as an  rule name in ~p :::: ~p",
          [Identifier,Body]);
  pfcRemoveOldVersion0(Identifier::::Body)).

  
pfcRemoveOldVersion0((Identifier::::Body)) :-
  nonvar(Identifier),
  clause((Identifier::::OldBody),_),
  \+(Body=OldBody),
  pfcWithdraw((Identifier::::OldBody)),
  !.
pfcRemoveOldVersion0(_).


% %% with_fc_mode(+Mode,:Goal) is semidet.
% 
% Temporariliy changes to forward chaining propagation mode while running the Goal
%
with_fc_mode(Mode,Goal):- locally(t_l:pfcSearchTL(Mode),Goal).


pfcThreadFwd(S,P):- 
      with_only_current_why(S,
       % maybe keep `thread` mode?
        call_in_thread(with_fc_mode(thread, (pfcFwd(P))))).

% in_fc_call(Goal):- with_fc_mode( thread, Goal).
%in_fc_call(Goal):- with_fc_mode( direct, Goal).
% in_fc_call(Goal):- !, pfcCallSystem(Goal).




% 

% pfcRun compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfcQueue mechanism.

pfcRun :-
  (\+ pfcGetSearch(direct)),
  pfcStep,
  pfcRun.
pfcRun.


% pfcStep removes one entry from the pfcQueue and reasons from it.


pfcStep :-  
  % if pfcHaltSignal(Msg) is true, reset it and fail, thereby stopping inferencing.
  pfcRetract(pfcHaltSignal(Msg)),
  pfcTraceMsg(removing(pfcHaltSignal(Msg))),
  !, 
  fail.

pfcStep :-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P),
  pfcdo(pfcFwd(P)),
  !.

get_next_fact(P) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P) :- 
  pfcRetract(pfcQueue(P)),
  pfcRemoveSupportsQuietly(pfcQueue(P)),
  !.
remove_selection(P) :-
  brake(pfcPrintf("pfc:get_next_fact - selected fact not on Queue: ~p",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.  
% It tries the user defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact(P) :- 
  pfcSelect(P),
  !.  
select_next_fact(P) :- 
  defaultpfcSelect(P),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultpfcSelect(P) :- pfcCallSystem(pfcQueue(P)),!.

% pfcHalt stops the forward chaining.
pfcHalt :-  pfcHalt("unknown_reason",[]).

pfcHalt(Format) :- pfcHalt(Format,[]).

pfcHalt(Format,Args) :- 
  format(string(Msg),Format,Args),
  (pfcHaltSignal(Msg) -> 
       pfcWarn("pfcHalt finds pfcHaltSignal(~w) already set",[Msg])
     ; assert(pfcHaltSignal(Msg))).


% %%
% %%
% %% predicates for manipulating triggers
% %%

pfcAddTrigger(pt(Trigger,Body),Support) :-
  !,
  pfcTraceMsg('      Adding positive trigger ~p~n',
		[pt(Trigger,Body)]),
  pfcAssert(pt(Trigger,Body),Support),
  copy_term(pt(Trigger,Body),Tcopy),
  pfc_call(Trigger),
  fcEvalLHS(Body,(Trigger,Tcopy)),
  fail.


pfcAddTrigger(nt(Trigger,Test,Body),Support) :-
  !,
  pfcTraceMsg('      Adding negative trigger: ~p~n       test: ~p~n       body: ~p~n',
		[Trigger,Test,Body]),
  copy_term(Trigger,TriggerCopy),
  pfcAssert(nt(TriggerCopy,Test,Body),Support),
  \+ pfc_call(Test),
  fcEvalLHS(Body,((\+Trigger),nt(TriggerCopy,Test,Body))).

pfcAddTrigger(bt(Trigger,Body),Support) :-
  !,
  pfcAssert(bt(Trigger,Body),Support),
  pfcBtPtCombine(Trigger,Body,Support).

pfcAddTrigger(X,_Support) :-
  pfcWarn("Unrecognized trigger to pfcAddtrigger: ~p",[X]).


pfcBtPtCombine(Head,Body,Support) :- 
  %% a backward trigger (bt) was just added with head and Body and support Support
  %% find any pt's with unifying heads and add the instantied bt body.
  pfcGetTriggerQuick(pt(Head,_PtBody)),
  fcEvalLHS(Body,Support),
  fail.
pfcBtPtCombine(_,_,_) :- !.

pfcGetTriggerQuick(Trigger) :-  clause(Trigger,true).
pfcCallSystem(Trigger) :-  call(Trigger).

% %%
% %%
% %% predicates for manipulating action traces.
% %%

pfcAddActionTrace(Action,Support) :- 
  % adds an action trace and it's support.
  pfcAddSupport(pfcAction(Action),Support).

pfcRemActionTrace(pfcAction(A)) :-
  do_and_undo(A,UndoMethod),
  pfcCallSystem(UndoMethod),
  !.


% %%
% %% predicates to remove pfc facts, triggers, action traces, and queue items
% %% from the database.
% %%

pfcRetract(X) :- 
  %% retract an arbitrary thing.
  pfcType(X,Type),
  pfcRetractType(Type,X),
  !.                       

pfcRetractType(fact(_),X) :-   
  %% db 
  pfcAddDbToHead(X,X2)-> retract(X2) ; retract(X).

pfcRetractType(rule(_),X) :- 
  %% db  
  pfcAddDbToHead(X,X2) ->  retract(X2) ; retract(X).

pfcRetractType(trigger,X) :- 
  retract(X)
    -> unFc(X)
     ; pfcWarn("Trigger not found to retract: ~p",[X]).

pfcRetractType(action,X) :- pfcRemActionTrace(X).
  

% %% pfcAddType1(X) adds item X to some database

pfcAddType1(X) :-
  % what type of X do we have?
  pfcType(X,Type),
  pfcAddDbToHead(X,X2),
  % call the appropriate predicate.
  pfcAddType(Type,X2).

pfcAddType(fact(Type),X) :- 
  pfcUnique(fact(Type),X), 
  assert(X),!.
pfcAddType(rule(Type),X) :- 
  pfcUnique(rule(Type),X), 
  assert(X),!.
pfcAddType(trigger,X) :- 
  pfcUnique(trigger,X) -> assert(X) ; 
   (pfcWarn(not_pfcUnique(X)),assert(X)).
   
pfcAddType(action,_Action) :- !.


 

% pfcWithdraw/1  withdraws any "direct" support for P.
% If a list, iterates down the list
pfcWithdraw(P) :- is_list(P),!,maplist(pfcWithdraw,P).
pfcWithdraw(P) :- matches_why_UU(UU), pfcWithdraw(P,UU).
% %% pfcWithdraw(P,S) removes support S from P and checks to see if P is still supported.
% %% If it is not, then the fact is retractred from the database and any support
% %% relationships it participated in removed.
pfcWithdraw(P,S) :-
  % pfcDebug(pfcPrintf("removing support ~p from ~p",[S,P])),
  pfcGetSupport(P,S),
  matterialize_support_term(S,Sup),
  pfcTraceMsg('    Withdrawing direct support: ~p   \n   From: ~p~n',[Sup,P]),
   (pfcRemOneSupportOrQuietlyFail(P,S)
      -> pfcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P]) 
      ; pfcWarn("pfcRemOneSupport/2 Could not find support ~p thus\n    Did not pfcRemOneSupport: ~p",
                 [Sup,P])),
   removeIfUnsupported(P).

pfcWithdraw(P,S) :-
  matterialize_support_term(S,Sup),
  pfcTraceMsg('    No support matching: ~p   \n   For: ~p~n',[Sup,P]),!,
  removeIfUnsupported(P).

% pfcRetractAll/1  withdraws any "direct" and "indirect" support for P.
% If a list, iterates down the list
pfcRetractAll(P) :- is_list(P),!,maplist(pfcRetractAll,P).
pfcRetractAll(P) :- matches_why_UU(UU), pfcRetractAll(P,UU).

% %% pfcRetractAll(P,S) removes support S from P and checks to see if P is still supported.
% %% If it is not, then the fact is retreactred from the database and any support
% %% relationships it participated in removed.
pfcRetractAll(P,S) :-
  \+ \+ pfcWithdraw(P,S),
  fail.
pfcRetractAll(P,S) :-  
  pfcGetSupport(P,(P2,_)),
  pfcType(P2,fact(_)),
  pfcSupportedBy(P2,S,_How),
   pfcRetractAll(P2),   
    \+ fcSupported(P),!,
    fcUndo(P).
pfcRetractAll(P,S) :-
  pfcGetSupport( P,(_,T)),
    pfcGetSupport(T,(P2,_)),
    pfcSupportedBy(P2,S,_How),
    pfcType(P2,fact(_)),
   pfcRetractAll(P2),
    \+ fcSupported(P),!,
    fcUndo(P).
pfcRetractAll(P,S) :-
  fcSupported(P),
  pfcGetSupport(P,(P2,_)),
  pfcSupportedBy(P2,S,_How),
  pfcType(P2,rule(_)),
   pfcRetractAll(P2),
    \+ fcSupported(P),
    fcUndo(P),!.
pfcRetractAll(P,_S0) :- 
  removeIfUnsupported(P),
  fail.
pfcRetractAll(_,_).


pfcSupportedBy(P,S,How):- 
   pfcGetSupport(P,(F,T)),
   (pfcSupportedBy(F,S,_)->How=F;
   pfcSupportedBy(T,S,How)).
   
pfcSupportedBy(P,S,How):-P=S,How=S.  

pfcRetractAll_v2(P,S0) :-
  \+ \+ pfcWithdraw(P,S0),
  pfcGetSupport(P,(S,RemoveIfTrigger)),
  % pfcDebug(pfcPrintf("removing support ~p from ~p",[S,P])),
  matterialize_support_term((S,RemoveIfTrigger),Sup),
  pfcTraceMsg('    Removing support: ~p   \n   From: ~p~n',[Sup,P]),
  (pfcRemOneSupportOrQuietlyFail(P,(S,RemoveIfTrigger))
     -> pfcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P]) 
     ; (pfcWarn("pfcRemOneSupport/2 Could not find support ~p thus\n    Did not yet pfcRetractAll_v2: ~p",
                [Sup,P]))),
  pfcRetractAll_v2(S, S0),
  fail.

pfcRetractAll_v2(P,_):- removeIfUnsupported(P).

% pfcRemove/1 is the user's interface - it withdraws user support for P.
%
% pfcRemove is like pfcRetractAll, but if P is still in the DB after removing the
% user's support, it is retracted by more forceful means (e.g. pfcBlast).
%
pfcRemove(P) :-
  pfcRetractAll(P),
  pfc_call(P)
     -> pfcBlast(P) 
      ; true.


% %% pfcBlast(+F) is det
%
% retracts fact F from the DB and removes any dependent facts 
%

pfcBlast(F) :- 
  pfcRemoveSupports(F),
  fcUndo(F).


% removes any remaining supports for fact F, complaining as it goes.

pfcRemoveSupports(F) :- 
  pfcRemOneSupport(F,S),
  pfcWarn("~p was still supported by ~p (but no longer)",[F,S]),
  fail.
pfcRemoveSupports(_).

pfcRemoveSupportsQuietly(F) :- 
  pfcRemOneSupport(F,_),
  fail.
pfcRemoveSupportsQuietly(_).

% fcUndo(X) undoes X.


fcUndo(pfcAction(A)) :-  
  % undo an action by finding a method and successfully executing it.
  !,
  pfcRemActionTrace(pfcAction(A)).

fcUndo(pt(/*Key,*/Head,Body)) :-  
  % undo a positive trigger.
  %
  !,
  (retract(pt(/*Key,*/Head,Body))
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
  unFc(Fact).
  

% %% unFc(P) is det.
%
% unFc(P) "un-forward-chains" from fact f.  That is, fact F has just
% been removed from the database, so remove all dependant relations it
% participates in and check the things that they support to see if they
% should stayu in the database or should also be removed.


unFc(F) :- 
  pfcRetractDependantRelations(F),
  unFc1(F).

unFc1(F) :-
  pfcUnFcCheckTriggers(F),
  % is this really the right place for pfcRun<?
  pfcRun.


pfcUnFcCheckTriggers(F) :-
  pfcType(F,fact(_)),
  copy_term(F,Fcopy),
  pfcCallSystem(nt(Fcopy,Condition,Action)),
  (\+ pfcCallSystem(Condition)),
  fcEvalLHS(Action,((\+F),nt(F,Condition,Action))),
  fail.
pfcUnFcCheckTriggers(_).

pfcRetractDependantRelations(Fact) :-
  pfcType(Fact,Type),
  (Type=trigger -> pfcRemOneSupport(P,(_,Fact))
                ; pfcRemOneSupportOrQuietlyFail(P,(Fact,_))),
  removeIfUnsupported(P),
  fail.
pfcRetractDependantRelations(_).



% %% removeIfUnsupported(+P) checks to see if P is supported and removes
% %% it from the DB if it is not.

removeIfUnsupported(P) :- 
   fcSupported(P) -> nop(pfcTraceMsg(fcSupported(P))) ;  fcUndo(P).


% %% fcSupported(+P) succeeds if P is "supported". What this means
% %% depends on the TMS mode selected.

fcSupported(P) :- 
  must(fcTmsMode(Mode)),
  supported(Mode,P).

supported(local,P) :- !, pfcGetSupport(P,_).
supported(cycles,P) :-  !, wellFounded(P).
supported(_,_P) :- true.


% %%
% %% a fact is well founded if it is supported by the user
% %% or by a set of facts and a rules, all of which are well founded.
% %%

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

% %% wflist(L) simply maps wf over the list.

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

triggerSupports(U,[]) :- axiomatic_supporter(U),!.
triggerSupports(Trigger,[Fact|MoreFacts]) :-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).


axiomatic_supporter(Var):-is_ftVar(Var),!,fail.
axiomatic_supporter(is_ftVar(_)).
axiomatic_supporter(clause_u(_)).
axiomatic_supporter(user(_)).
axiomatic_supporter(U):- is_file_ref(U),!.
axiomatic_supporter(ax):-!.

is_file_ref(A):-compound(A),A=mfl4(_VarNameZ,_,_,_).

triggerSupports(_,Var,[is_ftVar(Var)]):-is_ftVar(Var),!.
triggerSupports(_,U,[]):- axiomatic_supporter(U),!.
triggerSupports(FactIn,Trigger,OUT):-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger))*->
  (triggerSupports(Fact,AnotherTrigger,MoreFacts),OUT=[Fact|MoreFacts]);
  triggerSupports1(FactIn,Trigger,OUT).

triggerSupports1(_,X,[X]):- may_cheat.
may_cheat:- fail.



% %%
% %%
% %% pfcFwd(X) forward chains from a fact or a list of facts X.
% %%
pfcFwd(Fact) :- is_list(List)->maplist(pfcFwd1,List);pfcFwd1(Fact).

% fc1(+P) forward chains for a single fact.
pfcFwd1(Fact) :-
  fc_rule_check(Fact),
  copy_term(Fact,F),
  % check positive triggers
  fcpt(Fact,F),
  % check negative triggers
  fcnt(Fact,F).


% %%
% %% fc_rule_check(P) does some special, built in forward chaining if P is 
% %% a rule.
% %% 

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
  pfcTraceMsg('      Found positive trigger: ~p~n       body: ~p~n',
		[F,Body]),
  fcEvalLHS(Body,(Fact,pt(F,Body))),
  fail.

%fcpt(Fact,F) :- 
%  pfcGetTriggerQuick(pt(presently(F),Body)),
%  fcEvalLHS(Body,(presently(Fact),pt(presently(F),Body))),
%  fail.

fcpt(_,_).

fcnt(_Fact,F) :-
  spft(X,_,nt(F,Condition,Body)),
  pfcCallSystem(Condition),
  pfcRem_S(X,(_,nt(F,Condition,Body))),
  fail.
fcnt(_,_).


% %% pfcRem_S(P,S) removes support S from P and checks to see if P is still supported.
% %% If it is not, then the fact is retreactred from the database and any support
% %% relationships it participated in removed.
pfcRem_S(P,S) :-
  % pfcDebug(pfcPrintf("removing support ~p from ~p",[S,P])),
  pfcTraceMsg('    Removing support: ~p from ~p~n',[S,P]),
  pfcRemOneSupport(P,S)
     -> removeIfUnsupported(P)
      ; pfcWarn("pfcRem_S/2 Could not find support ~p to remove from fact ~p",
                [S,P]).



% %% pfcDefineBcRule(+Head,+Body,+ParentRule) 
%
% defines a backward
% chaining rule and adds the corresponding bt triggers to the database.
%

pfcDefineBcRule(Head,_Body,ParentRule) :-
  (\+ pfcLiteral(Head)),
  pfcWarn("Malformed backward chaining rule.  ~p not atomic literal.",[Head]),
  pfcError("caused by rule: ~p",[ParentRule]),
  !,
  fail.

pfcDefineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs(Head,Rhs),
  current_why_U(USER), % @TODO REVIEW _U
  pfcForEach(pfc_nf(Body,Lhs),
          (buildTrigger(Lhs,rhs(Rhs),Trigger),
           pfcAdd(bt(Head,Trigger),(ParentRuleCopy,USER)))).
get_bc_clause(Head,(HeadC:- BodyC)):- get_bc_clause(Head,HeadC,BodyC).

get_bc_clause(HeadIn, ~HeadC, Body):- compound(HeadIn), HeadIn = ~Head,!,
     Body = ( awc, 
            ( nonvar(HeadC)-> (HeadC = Head,!) ; (HeadC = Head)), 
              pfc_bc_and_with_pfc(~Head)).
get_bc_clause(Head, Head, Body):-  % % :- is_ftNonvar(Head).
     Body = ( awc, !, pfc_bc_and_with_pfc(Head)).

:- thread_initialization(nb_setval('$pfc_current_choice',[])).

push_current_choice:- current_prolog_flag(pfc_support_cut,false),!.
push_current_choice:- prolog_current_choice(CP),push_current_choice(CP),!.
push_current_choice(CP):- nb_current('$pfc_current_choice',Was)->b_setval('$pfc_current_choice',[CP|Was]);b_setval('$pfc_current_choice',[CP]).
 
cut_c:- current_prolog_flag(pfc_support_cut,false),!.
cut_c:- must(nb_current('$pfc_current_choice',[CP|_WAS])),prolog_cut_to(CP).


% %%
% %%
% %% eval something on the LHS of a rule.
% %%

 
fcEvalLHS((Test->Body),Support) :-  
  !, 
  (pfcCallSystem(Test) -> fcEvalLHS(Body,Support)),
  !.

fcEvalLHS((Test*->Body),Support) :-  
  !, 
  (pfcCallSystem(Test) *-> fcEvalLHS(Body,Support)).

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


% %%
% %% eval something on the RHS of a rule.
% %%

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
 pfcWithdraw(P).

pfc_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 pfc_eval_rhs([X|Xrest],Support).

pfc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 must(pfcPost0(Assertion,Support)).


pfc_eval_rhs1(X,_) :-
  pfcWarn("Malformed rhs of a rule: ~p",[X]).


% %%
% %% evaluate an action found on the rhs of a rule.
% %%

fcEvalAction(Action,Support) :-
  pfcCallSystem(Action), 
  (undoable(Action) 
     -> pfcAddActionTrace(Action,Support) 
      ; true).


% %%
% %% 
% %%

trigger_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  pfc_call(Trigger),
%  fcEvalLHS(Body,(presently(Trigger),pt(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  pfc_call(Trigger),
  fcEvalLHS(Body,(Trigger,pt(TriggerCopy,Body))),
  fail.


% %% pfc_call(F) is nondet.
%
% pfc_call(F) is true iff F is a fact available for forward chaining.
% Note that this has the side effect of catching unsupported facts and
% assigning them support from God.
%

pfc_call(F) :- var(F), !, pfc_call_var(F).
pfc_call((A,B)) :-!, pfc_call(A),pfc_call(B).
pfc_call((A;B)) :-!, pfc_call(A);pfc_call(B).
pfc_call((A->B)) :-!, pfc_call(A)->pfc_call(B).
pfc_call((A->B;C)) :-!, pfc_call(A)->pfc_call(B);pfc_call(C).
pfc_call((A*->B)) :-!, pfc_call(A)*->pfc_call(B).
pfc_call((A*->B;C)) :-!, pfc_call(A)*->pfc_call(B);pfc_call(C).
pfc_call(\+ (A)) :-!, \+ pfc_call(A).
pfc_call((A is B)) :-!, A is B.
pfc_call(P):- pfc_call_var(P).

pfc_cache_bc(P) :-
  % trigger any bc rules.
  forall(bt(P,Trigger),
  forall(pfcGetSupport(bt(P,Trigger),S),
  % @TODO REVIEW _U
  fcEvalLHS(Trigger,S))).


% we really need to check for system predicates as well.
pfc_call_var(P) :- callable(P), (predicate_property(P,imported_from(system));predicate_property(P,built_in)), !, call(P).
% this is probably not advisable due to extreme inefficiency.
pfc_call_var(P) :-
  % trigger any bc rules.
  bt(P,Trigger),
  pfcGetSupport(bt(P,Trigger),S),
  % @TODO REVIEW _U
  fcEvalLHS(Trigger,S),
  fail.
pfc_call_var(P) :- var(P), !, pfcFact(P).
pfc_call_var(P) :- clause(P,true)*-> true ;
  (clause(P,Condition), Condition\==true,
     pfcCallSystem(Condition)).


% an action is undoable if there exists a method for undoing it.
undoable(A) :- do_and_undo(A,_).



% %%
% %%
% %% defining fc rules 
% %%

% %% pfc_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form 
% %% Out.  It also does certain optimizations.  Backtracking into this
% %% predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  pfc_nf1(LHS,List2),
  pfc_nf_negations(List2,List).


% %% pfc_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
% %% Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

pfc_nf1(P,[P]) :- var(P), !.

% these next two rules are here for upward compatibility and will go 
% away eventually when the P/Condition form is no longer used anywhere.

pfc_nf1(P/Cond,[(\+P)/Cond]) :- pfcNegatedLiteral(P), !.

pfc_nf1(P/Cond,[P/Cond]) :-  (pfcLiteral(P);var(P)), !.

% %% handle a negated form

pfc_nf1(NegTerm,NF) :-
  pfc_unnegate(NegTerm,Term),
  !,
  pfc_nf1_negation(Term,NF).

% %% disjunction.

pfc_nf1((P;Q),NF) :- 
  !,
  (pfc_nf1(P,NF) ;   pfc_nf1(Q,NF)).


% %% conjunction.

pfc_nf1((P,Q),NF) :-
  !,
  pfc_nf1(P,NF1),
  pfc_nf1(Q,NF2),
  append(NF1,NF2,NF).

% %% handle a random atom.

pfc_nf1(P,[P]) :-
  pfcLiteral(P), 
  !.

% %%% shouln't we have something to catch the rest as errors?
pfc_nf1(Term,[Term]) :-
  pfcWarn("pfc_nf doesn't know how to normalize ~p",[Term]).


% %% pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
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


% %% pfc_nf_negations(List2,List) sweeps through List2 to produce List,
% %% changing ~{...} to {\+...}
% %%% ? is this still needed? twf 3/16/90

pfc_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

pfc_nf_negations([],[]).

pfc_nf_negations([H1|T1],[H2|T2]) :-
  pfc_nf_negation(H1,H2),
  pfc_nf_negations(T1,T2).

% Maybe \+ tilded_negation ?

pfc_nf_negation(Form,{\+ X}) :-
  nonvar(Form),
  Form=(~({X})),
  !.
pfc_nf_negation(Form,{\+ X}) :- tilded_negation, 
  nonvar(Form),
  Form=(-({X})),
  !.
pfc_nf_negation(Form,{\+ X}) :- tilded_negation, 
  nonvar(Form),
  Form=( \+ ({X})),
  !.
pfc_nf_negation(X,X).



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



     pfc_positive_fact(X):-  pfcPositiveLiteral(X), X \= ~(_), 
        pfcType(X,fact(_FT)), \+ pfcType(X,trigger).



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
     is_active_lhs(pfcAction(_Act)).
     is_active_lhs('{}'(_Act)).
     is_active_lhs((Lhs1/Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).
     is_active_lhs((Lhs1,Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).
     is_active_lhs((Lhs1;Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).


     add_lhs_cond(Lhs1/Cond,Lhs2,Lhs1/(Cond,Lhs2)):-!.
     add_lhs_cond(Lhs1,Lhs2,Lhs1/Lhs2).



% %%
% %% buildRhs(+Conjunction,-Rhs)
% %%

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


% %% pfc_unnegate(N,P) is true if N is a negated term and P is the term
% %% with the negation operator stripped.

pfc_unnegate(P,_):- var(P),!,fail.
pfc_unnegate((~P),P):-  \+ tilded_negation.
pfc_unnegate((-P),P).
pfc_unnegate((\+(P)),P).

pfcNegatedLiteral(P) :- 
  callable(P),
  pfc_unnegate(P,Q),
  pfcPositiveLiteral(Q).

pfcLiteral(X) :- pfcNegatedLiteral(X).
pfcLiteral(X) :- pfcPositiveLiteral(X).

pfcPositiveLiteral(X) :-  
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

pfcConnective('-').
pfcConnective('~'):- \+ tilded_negation.
pfcConnective(( \+ )).

is_implicitly_prolog(Callable):- \+ callable(Callable),!, fail.
is_implicitly_prolog(_ is _).

processRule(Lhs,Rhs,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs(Rhs,Rhs2),
  current_why_U(USER), % @TODO REVIEW _U
  pfcForEach(pfc_nf(Lhs,Lhs2), 
          buildRule(Lhs2,rhs(Rhs2),(ParentRuleCopy,USER))).

buildRule(Lhs,Rhs,Support) :-
  buildTrigger(Lhs,Rhs,Trigger),
  fcEvalLHS(Trigger,Support).

buildTrigger([],Consequent,Consequent).

buildTrigger([Test|Triggers],Consequent,(Test->X)) :- is_implicitly_prolog(Test),
  !,
  buildTrigger(Triggers,Consequent,X).

buildTrigger([V|Triggers],Consequent,pt(V,X)) :-
  var(V),
  !, 
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1/Test)|Triggers],Consequent,nt(T2,Test2,X)) :-
  pfc_unnegate(T1,T2),
  !, 
  buildNtTest(T2,Test,Test2),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1)|Triggers],Consequent,nt(T2,Test,X)) :-
  pfc_unnegate(T1,T2),
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

% %%
% %% buildNtTest(+,+,-).
% %%
% %% builds the test used in a negative trigger (nt/3).  This test is a
% %% conjunction of the check than no matching facts are in the db and any
% %% additional test specified in the rule attached to this ~ term.
% %%
     %  tilded_negation.
buildNtTest(T,Testin,Testout) :-
  buildTest(Testin,Testmid),
  pfcConjoin((pfc_call(T)),Testmid,Testout).

  
% this just strips away any currly brackets.

buildTest({Test},Test) :- !.
buildTest(Test,Test).

% %%


% %% pfcType(+VALUE1, ?Type) is semidet.
%
% PFC Database Type.
%
%  simple typeing for Pfc objects
%


pfcType(Var,Type):- var(Var),!, Type=fact(_FT).
pfcType(_:X,Type):- !, pfcType(X,Type).
pfcType(~_,Type):- !, Type=fact(_FT).
pfcType(('==>'(_,_)),Type):- !, Type=rule(fwd).
pfcType( '==>'(X),Type):- !, pfcType(X,Type), pfcWarn(pfcType( '==>'(X), Type)).
pfcType(('<==>'(_,_)),Type):- !, Type=rule(<==>).
pfcType(('<-'(_,_)),Type):- !, Type=rule(bwc).
pfcType((':-'(_,_)),Type):- !, Type=rule(cwc).
pfcType(pt(_,_,_),Type):- !, Type=trigger.
pfcType(pt(_,_),Type):- !, Type=trigger.
pfcType(nt(_,_,_),Type):- !,  Type=trigger.
pfcType(bt(_,_),Type):- !,  Type=trigger.
pfcType(pfcAction(_),Type):- !, Type=action.
pfcType((('::::'(_,X))),Type):- !, pfcType(X,Type).
pfcType(_,fact(_FT)):-
  %  if it''s not one of the above, it must_ex be a fact!
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

pfcForEach(Binder,Body) :- Binder,pfcdo(Body),fail.
pfcForEach(_,_).

% pfcdo(X) executes X once and always succeeds.
pfcdo(X) :- X,!.
pfcdo(_).


% %% pfcUnion(L1,L2,L3) - true if set L3 is the result of appending sets
% %% L1 and L2 where sets are represented as simple lists.

pfcUnion([],L,L).
pfcUnion([Head|Tail],L,Tail2) :-  
  memberchk(Head,L),
  !,
  pfcUnion(Tail,L,Tail2).
pfcUnion([Head|Tail],L,[Head|Tail2]) :-  
  pfcUnion(Tail,L,Tail2).


% %% pfcConjoin(+Conjunct1,+Conjunct2,?Conjunction).
% %% arg3 is a simplified expression representing the conjunction of
% %% args 1 and 2.

pfcConjoin(true,X,X) :- !.
pfcConjoin(X,true,X) :- !.
pfcConjoin(C1,C2,(C1,C2)).


%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
% %%	restore, reset, etc.0

% pfcDatabaseTerm(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

pfcDatabaseTerm(spft/3).
pfcDatabaseTerm(pt/2).
pfcDatabaseTerm(bt/2).
pfcDatabaseTerm(nt/3).
pfcDatabaseTerm('==>'/2).
pfcDatabaseTerm('<==>'/2).
pfcDatabaseTerm('<-'/2).
pfcDatabaseTerm(pfcQueue/1).

% removes all forward chaining rules and justifications from db.

pfcReset :-
  clause(spft(P,F,Trigger),true),
  pfcRetractOrWarn(P),
  pfcRetractOrWarn(spft(P,F,Trigger)),
  fail.
pfcReset :-
  (pfcDatabaseItem(T)*->
   (pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n",[T]),fail)
    ; true).


% true if there is some pfc crud still in the database.
pfcDatabaseItem(Term:-Body) :-
  pfcDatabaseTerm(P/A),
  functor(Term,P,A),
  clause(Term,Body).

pfcRetractOrWarn(X) :-  retract(X), !.
pfcRetractOrWarn(X) :- 
  pfcWarn("Couldn't retract ~p.",[X]),dumpST,pfcWarn("Couldn't retract ~p.",[X]),!.

pfcRetractOrQuietlyFail(X) :-  retract(X), !.
pfcRetractOrQuietlyFail(X) :- 
  nop((pfcTraceMsg("Trace: Couldn't retract ~p.",[X]),dumpST,pfcWarn("Couldn't retract ~p.",[X]))),
  !,fail.



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

% %% predicates to examine the state of pfc

pfcQueue :- listing(pfcQueue/1).

pfcPrintDB :-
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
  pfcPrintSupports,!.

printLine:- ansi_format([underline],"~N=========================================~n",[]).

% %% pfcPrintFacts ...

pfcPrintFacts :- pfcPrintFacts(_,true).


pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern,true).

pfcPrintFacts(P,C) :-  
  pfcFacts(P,C,L),
  pfcClassifyFacts(L,User,Pfc,_Rule),
  printLine,
  pfcPrintf("User added facts:~n",[]),
  pfcPrintitems(User),
  printLine,
  pfcPrintf("Pfc added facts:~n",[]),
  pfcPrintitems(Pfc),
  printLine,!.


% %% printitems clobbers it's arguments - beware!

pfcPrintitems([]).
pfcPrintitems([H|T]) :-
  % numbervars(H,0,_),
  %format('~N ~p.',[H]),
  \+ \+ ( pretty_numbervars(H,H1),format(" ",[]),portray_clause_w_vars(H1)),
  pfcPrintitems(T).

pfcClassifyFacts([],[],[],[]).

pfcClassifyFacts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],[H|User],Pfc,Rule) :-
  matches_why_UU(UU),
  pfcGetSupport(H,UU),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],User,[H|Pfc],Rule) :-
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcPrintRules :-
  printLine,
  pfcPrintf("Rules:...~n",[]),
  bagof_PFC((P==>Q),clause((P==>Q),true),R1),
  pfcPrintitems(R1),
  bagof_PFC((P<==>Q),clause((P<==>Q),true),R2),
  pfcPrintitems(R2),
  bagof_PFC((P<-Q),clause((P<-Q),true),R3),
  pfcPrintitems(R3),
  printLine.

pfcGetTrigger(Trigger):- pfc_call(Trigger).


% %%  pfcPrintTriggers is semidet.
%
% Pretty Print Triggers.
%
pfcPrintTriggers :-
     pp_mask("Positive trigger", pt(_,_)),
     pp_mask("Negative trigger", nt(_,_,_)),
     pp_mask("Goal trigger",bt(_,_)).

pp_triggers:-pfcPrintTriggers.
%= 	 	 

% %% pfcPrintSupports is semidet.
%
% Pretty Print Supports.
%
pfcPrintSupports :-
  % temporary hack.
  draw_line,
  fmt("Supports ...~n",[]), 
  setof_or_nil((P =< S), (pfcGetSupport(P,S), \+ pp_filtered(P)),L),
  pp_items('Support',L),
  draw_line,!.
pp_supports:- pfcPrintSupports.

pp_filtered(P):-var(P),!,fail.
pp_filtered(_:P):- !, pp_filtered(P).
pp_filtered(P):- safe_functor(P,F,A),F\==(/),!,pp_filtered(F/A).
pp_filtered(F/_):-F==mpred_prop.



pfcFact(P) :- pfcFact(P,true).

% %% pfcFact(P,C) is true if fact P was asserted into the database via
% %% pfcAdd and contdition C is satisfied.  For example, we might do:
% %% 
% %%  pfcFact(X,pfcUserFact(X))
% %%

pfcFact(P,C) :- 
  pfcGetSupport(P,_),
  pfcType(P,fact(_)),
  pfcCallSystem(C).

% %% pfcFacts(-ListofPfcFacts) returns a list of facts added.

pfcFacts(L) :- pfcFacts(_,true,L).

pfcFacts(P,L) :- pfcFacts(P,true,L).

% %% pfcFacts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfcFacts(P,C,L) :- setof_or_nil(P,pfcFact(P,C),L).

brake(X) :-  X, break.

% %%
% %%
% %% predicates providing a simple tracing facility
% %%

pfcTraceAdd(P) :- 
  % this is here for upward compat. - should go away eventually.
  pfcTraceAdd(P,(upward_compat,o)).

pfcTraceAdd(pt(_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfcTraceAdd(nt(_,_),_) :-
  % hack for now - never trace triggers.
  !.

pfcTraceAdd(P,S) :-
   quietly(pfcTraceAddPrint(P,S)),
   quietly(pfcTraceBreak(P,S)).
   

pfcTraceAddPrint(P,S) :-
  pfcIsTraced(P),
  !, 
  pretty_numbervars(P,Pcopy),
  % numbervars(Pcopy,0,_),
  matches_why_UU(UU),
  (S=UU
       -> pfcPrintf("Adding (u) ~@",[fmt_cl(Pcopy)])
        ; pfcPrintf("Adding ~@",[fmt_cl(Pcopy)])).

pfcTraceAddPrint(_,_).


pfcTraceBreak(P,_S) :-
  pfcSpied(P,+) -> 
   (pretty_numbervars(P,Pcopy),
    % numbervars(Pcopy,0,_),
    pfcPrintf("Breaking on pfcAdd(~p)",[Pcopy]),
    break)
   ; true.

pfcTraceRem(pt(_,_)) :-
  % hack for now - never trace triggers.
  !.
pfcTraceRem(nt(_,_)) :-
  % hack for now - never trace triggers.
  !.

pfcTraceRem(P) :-
  (pfcIsTraced(P) 
     -> pfcPrintf("Removing: ~p.",[P])
      ; true),
  (pfcSpied(P,-)
   -> (pfcPrintf("Breaking on pfcRem(~p)",[P]),
       break)
   ; true).

pfcIsTraced(P):- pfcTraced(P).

mpred_trace_exec:- pfcTrace, pfcWatch.

pfcTrace :- pfcTrace(_).

pfcTrace(Form) :-
  assert(pfcTraced(Form)).

pfcTrace(Form,Condition) :- 
  assert((pfcTraced(Form) :- Condition)).

pfcSpy(Form) :- pfcSpy(Form,[+,-],true).

pfcSpy(Form,Modes) :- pfcSpy(Form,Modes,true).

pfcSpy(Form,[H|T],Condition) :-
  !,
  pfcSpy1(Form,H,Condition),
  pfcSpy(Form,T,Condition).

pfcSpy(Form,Mode,Condition) :-
  pfcSpy1(Form,Mode,Condition).

pfcSpy1(Form,Mode,Condition) :-
  assert((pfcSpied(Form,Mode) :- Condition)).

pfcNospy :- pfcNospy(_,_,_).

pfcNospy(Form) :- pfcNospy(Form,_,_).

pfcNospy(Form,Mode,Condition) :- 
  clause(pfcSpied(Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
pfcNospy(_,_,_).

pfcNoTrace :- pfcUntrace.
pfcUntrace :- pfcUntrace(_).
pfcUntrace(Form) :- retractall(pfcTraced(Form)).

% needed:  pfcTraceRule(Name)  ...


% if the correct flag is set, trace exection of Pfc

pfcTraceMsg(Msg):- pfcTraceMsg('~p',[Msg]).
pfcTraceMsg(Msg,Args) :-
    pfcTraceExecution,
    !,
    pfcPrintf(user_output, Msg, Args).
pfcTraceMsg(_Msg,_Args).


pfcPrintf(Msg,Args) :- 
  pfcPrintf(user_output, Msg,Args).

pfcPrintf(Where,Msg,Args) :- 
  format(Where,'~N',[]),
  format(Where,Msg,Args),
  format(Where,'~N',[]),!.



pfcWatch :- assert(pfcTraceExecution).

pfcNoWatch :-  retractall(pfcTraceExecution).

pfcError(Msg) :-  pfcError(Msg,[]).

pfcError(Msg,Args) :- 
  ansi_format([underline,fg(red)],"~N===============nERROR/Pfc=================~n",[]),
  ansi_format([fg(yellow)],Msg,Args),
  printLine,
  (clause_b(breakOnWarnings)->break;true).

% %%
% %% These control whether or not warnings are printed at all.
% %%   pfcWarn.
% %%   nopfcWarn.
% %%
% %% These print a warning message if the flag pfcWarnings is set.
% %%   pfcWarn(+Message)
% %%   pfcWarn(+Message,+ListOfArguments)
% %%

pfcWarn :- 
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

nopfcWarn :-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(false)).
 
pfcWarn(Msg) :-  pfcWarn('~p',[Msg]).

pfcWarn(Msg,Args) :- 
  (pfcWarnings(true);clause_b(breakOnWarnings)),
  !,
   ansi_format([underline,fg(red)],"~N==============WARNING/Pfc================~n",[]),
   ansi_format([fg(yellow)],Msg,Args),
  printLine,
  (clause_b(breakOnWarnings)->break;true).
pfcWarn(_,_).

% %%
% %% pfcWarnings/0 sets flag to cause pfc warning messages to print.
% %% pfcNoWarnings/0 sets flag to cause pfc warning messages not to print.
% %%

pfcWarnings :- 
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

pfcNoWarnings :- 
  retractall(pfcWarnings(_)).

%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a fact *****


%:- use_module(library(lists)).

justification(F,J) :- supports(F,J).

justifications(F,Js) :- bagof(J,justification(F,J),Js).



% %% base(P,L) - is true iff L is a list of "base" facts which, taken
% %% together, allows us to deduce P.  A base fact is an axiom (a fact 
% %% added by the user or a raw Prolog fact (i.e. one w/o any support))
% %% or an assumption.

base(F,[F]) :- (axiom(F) ; assumption(F)),!.

base(F,L) :-
  % i.e. (reduce 'append (map 'base (justification f)))
  justification(F,Js),
  bases(Js,L).


% %% bases(L1,L2) is true if list L2 represents the union of all of the 
% %% facts on which some conclusion in list L1 is based.

bases([],[]).
bases([X|Rest],L) :-
  base(X,Bx),
  bases(Rest,Br),
  pfcUnion(Bx,Br,L).
	
axiom(F) :- 
  matches_why_UU(UU),
  pfcGetSupport(F,UU); 
  pfcGetSupport(F,(god,god)).

% %% an assumption is a failed goal, i.e. were assuming that our failure to 
% %% prove P is a proof of not(P)

assumption(P) :- pfc_unnegate(P,_).
   
% %% assumptions(X,As) if As is a set of assumptions which underly X.

assumptions(X,[X]) :- assumption(X).
assumptions(X,[]) :- axiom(X).
assumptions(X,L) :-
  justification(X,Js),
  assumptions1(Js,L).

assumptions1([],[]).
assumptions1([X|Rest],L) :-
  assumptions(X,Bx),
  assumptions1(Rest,Br),
  pfcUnion(Bx,Br,L).  


% %% pfcProofTree(P,T) the proof tree for P is T where a proof tree is
% %% of the form
% %%
% %%     [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
% %%          ^                         and has the form of
% %%          [J11, J12,... J1n]      a list of proof trees.


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



/*
current_why_U(U):- must(current_why(Why)), U = user(Why).
current_why_UU(UU):- current_why_U(U), UU= (U,U).
matches_why_U(U):-  freeze(U,U=user(_)).
matches_why_UU(UU):- matches_why_U(U1),matches_why_U(U2), freeze(UU,UU=(U1,U2)).
*/
current_why_U(U):-  get_why_uu((U,_)).% must(current_why(Why)), U = user(Why).
current_why_UU(UU):- quietly(get_why_uu(UU)). % current_why_U(U), UU= (U,U).
matches_why_U(U):-  nop((current_why_U(Y), freeze(U,\+ \+ (U=Y;true)))).
matches_why_UU(UU):- nop(only_is_user_reason(UU)). % matches_why_U(U1),matches_why_U(U2),freeze(UU,UU=(U1,U2)).


matterialize_support_term(S,Sup):- term_attvars(S,Atts), Atts\==[] -> copy_term(S,_,Goals),Sup= S+Goals,!.
matterialize_support_term(SS,SS).

% %%
% %%
% %% predicates for manipulating support relationships
% %%

% %% pfcAddSupport(+Fact,+Support)

pfcAddSupport(P,(Fact,Trigger)) :- assert(spft(P,Fact,Trigger)).

pfcGetSupport(P,(Fact,Trigger)) :- pfcCallSystem(spft(P,Fact,Trigger)).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

pfcRemOneSupport(P,(Fact,Trigger)) :-
  must(callable(P);callable(Fact);callable(Trigger)),
  pfcRetractOrWarn(spft(P,Fact,Trigger)).

pfcRemOneSupportOrQuietlyFail(P,(Fact,Trigger)) :-
  must(callable(P);callable(Fact);callable(Trigger)),
  pfcRetractOrQuietlyFail(spft(P,Fact,Trigger)).


pfc_collect_supports(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

pfc_support_relation((P,F,T)) :-
  spft(P,F,T).

pfc_make_supports((P,S1,S2)) :- 
  pfcAddSupport(P,(S1,S2)),
  (pfcAddType1(P); true),
  !.

% %% pfcTriggerKey(+Trigger,-Key) 
% %%
% %% Arg1 is a trigger.  Key is the best term to index it on.

pfcTriggerKey(pt(Key,_),Key).
pfcTriggerKey(pt(Key,_,_),Key).
pfcTriggerKey(nt(Key,_,_),Key).
pfcTriggerKey(Key,Key).


% %%^L
% %% Get a key from the trigger that will be used as the first argument of
% %% the trigger base clause that stores the trigger.
% %%

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

%:- use_module(library(lists)).

:- dynamic(t_l:whybuffer/2).


/*
mpred_test(\+ X):- !, mpred_test(~ X).
mpred_test(~(~(X))):- !, mpred_test(X).
mpred_test(X):- full_transform(pfcWhy,X,G), pfc_call(G),pfcTF(G).
*/

pfcWhy :- 
  t_l:whybuffer(P,_),
  pfcWhy(P).


pfcTF(KR):- full_transform(pfcWhy,KR,P),(pfc_call(P)*->foreach(pfcTF1(P),true);pfcTF1(P)).
pfcTF1(P):- 
   ansi_format([underline],"~N=========================================",[]),
   (ignore(pfcWhy(P))),
   ignore(pfcWhy(~P)),
   printLine.
   

pfcWhy(N) :-
  number(N),
  !,
  t_l:whybuffer(P,Js),
  pfcWhyCommand(N,P,Js).

pfcWhy(KR) :- 
  full_transform(pfcWhy,KR,P),
  justifications(P,Js),
  retractall(t_l:whybuffer(_,_)),
  assert(t_l:whybuffer(P,Js)),
  pfcWhyBrouse(P,Js).

system:pfcWhy(P):- pfcWhy(P).


pfcWhy1(P) :-
  justifications(P,Js),
  pfcWhyBrouse(P,Js).

pfcWhyBrouse(P,Js) :-
  % rtrace(pfc_pp_db_justifications(P,Js)),
  pfcShowJustifications(P,Js),
  nop((pfcAsk(' >> ',Answer),
  pfcWhyCommand(Answer,P,Js))).

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
  pfcPrintf("~p is a yet unimplemented command.",[N]),
  fail.

pfcCommand(X,_,_) :-
 pfcPrintf("~p is an unrecognized command, enter h. for help.",[X]),
 fail.
  
pfcShowJustifications(P,Js) :-
  show_current_source_location,
  reset_shown_justs,
  %color_line(yellow,1),
  format("~N~nJustifications for ",[]),
  ansi_format([fg(green)],'~p',[P]),
  format(" :~n",[]),

  pfcShowJustification1(Js,1),!.

pfcShowJustification1([],_):-!.
pfcShowJustification1([J|Js],N) :- !,
  % show one justification and recurse.    
  reset_shown_justs,
  pfcShowSingleJust(N,step(1),J),!,
  N2 is N+1,  
  pfcShowJustification1(Js,N2).

pfcShowJustification1(J,N) :- 
  reset_shown_justs, % nl,
  pfcShowSingleJust(N,step(1),J),!.

incrStep(StepNo,Step):-arg(1,StepNo,Step),X is Step+1,nb_setarg(1,StepNo,X).

pfcShowSingleJust(JustNo,StepNo,C):- is_ftVar(C),!,incrStep(StepNo,Step),
  ansi_format([fg(cyan)],"~N    ~w.~w ~w ",[JustNo,Step,C]),!.
pfcShowSingleJust(_JustNo,_StepNo,[]):-!.
pfcShowSingleJust(JustNo,StepNo,(P,T)):-!, 
  pfcShowSingleJust(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,T).
pfcShowSingleJust(JustNo,StepNo,(P,F,T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,F),
  pfcShowSingleJust1(JustNo,StepNo,T).
pfcShowSingleJust(JustNo,StepNo,(P*->T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),format('      *-> ',[]),
  pfcShowSingleJust1(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,(P:-T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),format(':- ~p.',[T]).
 
pfcShowSingleJust(JustNo,StepNo,(P : -T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),format('      :- ',[]),
  pfcShowSingleJust(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,(P :- T) ):- !, 
  pfcShowSingleJust1(JustNo,StepNo,call(T)),  
  pfcShowSingleJust1(JustNo,StepNo,P).


pfcShowSingleJust(JustNo,StepNo,[P|T]):-!, 
  pfcShowSingleJust(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,pt(P,Body)):- !, 
  pfcShowSingleJust1(JustNo,StepNo,pt(P)),  
  pfcShowSingleJust(JustNo,StepNo,Body).

pfcShowSingleJust(JustNo,StepNo,C):- 
 pfcShowSingleJust1(JustNo,StepNo,C).

system:fmt_cl(P):- \+ \+ (pretty_numbervars(P,PP),numbervars(PP,126,_,[attvar(skip),singletons(true)]),write_term(PP,[portray(true)])).

unwrap_litr(C,CCC+VS):- copy_term(C,CC,VS),
  numbervars(CC+VS,0,_),
  unwrap_litr0(CC,CCC),!.
unwrap_litr0(call(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(pt(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(body(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(head(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(C,C).

:- thread_local(t_l:shown_why/1).

pfcShowSingleJust1(JustNo,StepNo,C):- unwrap_litr(C,CC),!,pfcShowSingleJust4(JustNo,StepNo,C,CC).
pfcShowSingleJust4(_,_,_,CC):- t_l:shown_why(C),C=@=CC,!.
pfcShowSingleJust4(JustNo,StepNo,C,CC):- assert(t_l:shown_why(CC)),!,
   incrStep(StepNo,Step),
   ansi_format([fg(cyan)],"~N    ~w.~w ~@ ",[JustNo,Step,fmt_cl(C)]),   
   pfcShowSingleJust_C(C),!.

pfcShowSingleJust_C(C):-is_file_ref(C),!.
pfcShowSingleJust_C(C):-find_mfl(C,MFL),assert(t_l:shown_why(MFL)),!,pfcShowSingleJust_MFL(MFL).
pfcShowSingleJust_C(_):-ansi_format([hfg(black)]," % [no_mfl] ",[]),!.

short_filename(F,FN):- atomic_list_concat([_,FN],'/pack/',F),!.
short_filename(F,FN):- atomic_list_concat([_,FN],swipl,F),!.
short_filename(F,FN):- F=FN,!.

pfcShowSingleJust_MFL(MFL):- MFL=mfl4(VarNameZ,_M,F,L),atom(F),short_filename(F,FN),!,varnames_load_context(VarNameZ),
   ansi_format([hfg(black)]," % [~w:~w] ",[FN,L]).
pfcShowSingleJust_MFL(MFL):- ansi_format([hfg(black)]," % [~w] ",[MFL]),!.

pfcAsk(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

pfcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).






















:- set_prolog_flag(expect_pfc_file,unknown).

% =======================================================
/* 
%
%= predicates to examine the state of pfc 
% interactively exploring Pfc justifications.
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/pfc_list_triggers.pl
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
pfc_listing_module:- nop( module(pfc_listing,
          [ draw_line/0,
            loop_check_just/1,
            pinfo/1,
            pp_items/2,
            pp_item/2,
            pp_filtered/1,
            pp_facts/2,
            pp_facts/1,
            pp_facts/0,
            pfc_list_triggers_types/1,
            pfc_list_triggers_nlc/1,
            pfc_list_triggers_1/1,
            pfc_list_triggers_0/1,
            pfc_list_triggers/1,
            pfc_contains_term/2,
            pfc_classify_facts/4,
            lqu/0,
            get_clause_vars_for_print/2,
            %pfcWhyBrouse/2,
            %pfcWhy1/1,
            %pfcWhy/1,
            %pfcWhy/0,
            pp_rules/0,
            pfcPrintSupports/0,
             pfcPrintTriggers/0,            
            print_db_items/1,
            print_db_items/2,
            print_db_items/3,
            print_db_items/4,
            print_db_items_and_neg/3,
            show_pred_info/1,
            show_pred_info_0/1,
            pfc_listing_file/0
          ])).

%:- include('pfc_header.pi').

:- endif.

% :- use_module(logicmoo(util/logicmoo_util_preddefs)).



:- multifile((
              user:portray/1,
  	user:prolog_list_goal/1,
  	user:prolog_predicate_name/2,
  	user:prolog_clause_name/2)).

:- dynamic
  	user:portray/1.

% :- dynamic(whybuffer/2).



%= 	 	 

% %% lqu is semidet.
%
% Lqu.
%
lqu :- listing(que/2).


 

%= 	 	 

% %% pp_facts is semidet.
%
% Pretty Print Facts.
%
pp_facts :- pp_facts(_,true).


%= 	 	 

% %% pp_facts( ?Pattern) is semidet.
%
% Pretty Print Facts.
%
pp_facts(Pattern) :- pp_facts(Pattern,true).


%= 	 	 

% %% pp_facts( ?P, ?C) is semidet.
%
% Pretty Print Facts.
%
pp_facts(P,C) :-
  pfcFacts(P,C,L),
  pfc_classify_facts(L,User,Pfc,_Rule),
  draw_line,
  fmt("User added facts:",[]),
  pp_items(user,User),
  draw_line,
  draw_line,
  fmt("Pfc added facts:",[]),
  pp_items(system,Pfc),
  draw_line.



%= 	 	 

% %% pp_items( ?Type, :TermH) is semidet.
%
% Pretty Print Items.
%
pp_items(_Type,[]):-!.
pp_items(Type,[H|T]) :-
  ignore(pp_item(Type,H)),!,
  pp_items(Type,T).
pp_items(Type,H) :- ignore(pp_item(Type,H)).

:- thread_local(t_l:print_mode/1).

%= 	 	 

% %% pp_item( ?MM, :TermH) is semidet.
%
% Pretty Print Item.
%
pp_item(_M,H):-pp_filtered(H),!.
pp_item(MM,(H:-B)):- B ==true,pp_item(MM,H).
pp_item(MM,H):- flag(show_asserions_offered,X,X+1),find_and_call(get_print_mode(html)), ( \+ \+ if_defined(pp_item_html(MM,H))),!.


pp_item(MM,spft(W0,U,ax)):- W = (_KB:W0),!,pp_item(MM,U:W).
pp_item(MM,spft(W0,F,U)):- W = (_KB:W0),atom(U),!,    fmt('~N%~n',[]),pp_item(MM,U:W), fmt('rule: ~p~n~n', [F]),!.
pp_item(MM,spft(W0,F,U)):- W = (_KB:W0),         !,   fmt('~w~nd:       ~p~nformat:    ~p~n', [MM,W,F]),pp_item(MM,U).
pp_item(MM,nt(Trigger0,Test,Body)) :- Trigger = (_KB:Trigger0), !, fmt('~w n-trigger: ~p~ntest: ~p~nbody: ~p~n', [MM,Trigger,Test,Body]).
pp_item(MM,pt(F0,Body)):- F = (_KB:F0),             !,fmt('~w p-trigger:~n', [MM]), pp_item('',(F:-Body)).
pp_item(MM,bt(F0,Body)):- F = (_KB:F0),             !,fmt('~w b-trigger:~n', [MM]), pp_item('',(F:-Body)).


pp_item(MM,U:W):- !,format(string(S),'~w  ~w:',[MM,U]),!, pp_item(S,W).
pp_item(MM,H):- \+ \+ (( get_clause_vars_for_print(H,HH),fmt("~w ~p~N",[MM,HH]))).


%= 	 	 

% %% get_clause_vars_for_print( ?HB, ?HB) is semidet.
%
% Get Clause Variables For Print.
%
get_clause_vars_for_print(HB,HB):- ground(HB),!.
get_clause_vars_for_print(I,I):- is_listing_hidden(skipVarnames),!.
get_clause_vars_for_print(H0,MHB):- get_clause_vars_copy(H0,MHB),!.
get_clause_vars_for_print(HB,HB).

%= 	 	 

% %% pfc_classify_facts( :TermH, ?User, :TermPfc, ?H) is semidet.
%
% Managed Predicate Classify Facts.
%
pfc_classify_facts([],[],[],[]).

pfc_classify_facts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule),
  !,
  pfc_classify_facts(T,User,Pfc,Rule).

pfc_classify_facts([H|T],[H|User],Pfc,Rule) :-
  pfcGetSupport(H,(mfl4(_VarNameZ,_,_,_),ax)),
  !,
  pfc_classify_facts(T,User,Pfc,Rule).

pfc_classify_facts([H|T],User,[H|Pfc],Rule) :-
  pfc_classify_facts(T,User,Pfc,Rule).



%= 	 	 

% %% print_db_items( ?T, ?I) is semidet.
%
% Print Database Items.
%
print_db_items(T, I):- 
    draw_line, 
    fmt("~N~w ...~n",[T]),
    print_db_items(I),
    draw_line,!.


%= 	 	 

% %% print_db_items( ?I) is semidet.
%
% Print Database Items.
%
print_db_items(F/A):-number(A),!,safe_functor(P,F,A),!,print_db_items(P).
print_db_items(H):- bagof(H,clause(H,true),R1),pp_items((:),R1),R1\==[],!.
print_db_items(H):- \+ current_predicate(_,H),!. 
print_db_items(H):- catch( ('$find_predicate'(H,_),call_u(listing(H))),_,true),!,nl,nl.


%= 	 	 

% %% pp_rules is semidet.
%
% Pretty Print Rules.
%
pp_rules :-
   pp_mask("Forward Rules",(_ ==> _)),
   pp_mask("Bidirectional Rules",(_ <==> _)), 
   pp_mask("Implication Rules",=>(_ , _)),
   pp_mask("Bi-conditional Rules",<=>(_ , _)),
   pp_mask("Backchaining Rules",(_ <- _)),
   pp_mask("Positive Facts",(==>(_))),
   pp_mask("Negative Facts",(~(_))).
   


%= 	 	 


% %% draw_line is semidet.
%
% Draw Line.
%
draw_line:- \+ thread_self_main,!.
draw_line:- printLine,!.
draw_line:- (t_l:print_mode(H)->true;H=unknown),fmt("~N%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]),H=H.

 :- meta_predicate loop_check_just(0).

%= 	 	 

% %% loop_check_just( :GoalG) is semidet.
%
% Loop Check Justification.
%
loop_check_just(G):-loop_check(G,ignore(arg(1,G,[]))).


%= 	 	 

% %% show_pred_info( ?F) is semidet.
%
% Show Predicate Info.
%
/*
show_pred_info(PI):-
   ((
       pi_to_head_l(PI,Head),      
       % doall(show_call(why,call_u(isa(Head,_)))),
        safe_functor(Head,F,_),
        doall(show_call(why,call_u(isa(F,_)))),
       ((current_predicate(_,M:Head), (\+ predicate_property(M:Head,imported_from(_))))
          -> show_pred_info_0(M:Head); 
             wdmsg_pretty(cannot_show_pred_info(Head))))),!.
*/

%= 	 	 

% %% show_pred_info_0( ?Head) is semidet.
%
% show Predicate info  Primary Helper.
%
show_pred_info_0(Head):- 
        doall(show_call(why,predicate_property(Head,_))),
        (has_cl(Head)->doall((show_call(why,clause(Head,_))));quietly((listing(Head)))),!.


% ===================================================
% Pretty Print Formula
% ===================================================



%= 	 	 

% %% print_db_items( ?Title, ?Mask, ?What) is semidet.
%
% Print Database Items.
%
print_db_items(Title,Mask,What):-print_db_items(Title,Mask,Mask,What).

%= 	 	 

% %% print_db_items( ?Title, ?Mask, ?SHOW, ?What0) is semidet.
%
% Print Database Items.
%
print_db_items(Title,Mask,SHOW,What0):-
     get_pi(Mask,H),get_pi(What0,What),
     format(atom(Showing),'~p for ~p...',[Title,What]),
     statistics(cputime,Now),Max is Now + 2,!,
       gripe_time(1.0,
         doall((once(statistics(cputime,NewNow)),NewNow<Max,clause_or_call(H,B),
             quietly(pfc_contains_term(What,(H:-B))),
             flag(print_db_items,LI,LI+1),
             ignore(quietly(pp_item(Showing,SHOW)))))),
     ignore(pp_item(Showing,done)),!.


%= 	 	 

% %% pfc_contains_term( ?What, ?VALUE2) is semidet.
%
% Managed Predicate Contains Term.
%
pfc_contains_term(What,_):-is_ftVar(What),!.
pfc_contains_term(What,Inside):- compound(What),!,(\+ \+ ((copy_term_nat(Inside,Inside0),snumbervars(Inside0),occurs:contains_term(What,Inside0)))),!.
pfc_contains_term(What,Inside):- (\+ \+ once((subst(Inside,What,foundZadooksy,Diff),Diff \=@= Inside ))),!.



%= 	 	 

% %% hook_pfc_listing( ?What) is semidet.
%
% Hook To [baseKB:hook_pfc_listing/1] For Module Mpred_listing.
% Hook Managed Predicate Listing.
%
baseKB:hook_pfc_listing(What):- on_x_debug(pfc_list_triggers(What)).

:- thread_local t_l:pfc_list_triggers_disabled.
% listing(L):-locally(t_l:pfc_list_triggers_disabled,listing(L)).


%= 	 	 

% %% pfc_list_triggers( ?What) is semidet.
%
% Managed Predicate List Triggers.
%
pfc_list_triggers(_):-t_l:pfc_list_triggers_disabled,!.
pfc_list_triggers(What):-loop_check(pfc_list_triggers_nlc(What)).

:- meta_predicate(pfc_list_triggers_nlc(?)).


%= 	 	 

% %% pfc_list_triggers_nlc( ?What) is semidet.
%
% Managed Predicate List Triggers Nlc.
%
pfc_list_triggers_nlc(MM:What):-atom(MM),!,MM:pfc_list_triggers(What).
pfc_list_triggers_nlc(What):-loop_check(pfc_list_triggers_0(What),true).


%= 	 	 

% %% pfc_list_triggers_0( ?What) is semidet.
%
% Managed Predicate list triggers  Primary Helper.
%
pfc_list_triggers_0(What):-get_pi(What,PI),PI\=@=What,pfc_list_triggers(PI).
pfc_list_triggers_0(What):-nonvar(What),What= ~(Then),!, \+ \+ pfc_list_triggers_1(Then), \+ \+ pfc_list_triggers_1(What).
pfc_list_triggers_0(What):- \+ \+  pfc_list_triggers_1(~(What)), \+ \+ pfc_list_triggers_1(What).


%= 	 	 

% %% pfc_list_triggers_types( ?VALUE1) is semidet.
%
% Managed Predicate list triggers  Types.
%
pfc_list_triggers_types('Triggers').
pfc_list_triggers_types('Instances').
pfc_list_triggers_types('Subclasses').
pfc_list_triggers_types('ArgTypes').
pfc_list_triggers_types('Arity').
pfc_list_triggers_types('Forward').
pfc_list_triggers_types('Bidirectional').
pfc_list_triggers_types('Backchaining').
pfc_list_triggers_types('Negative').
pfc_list_triggers_types('Sources').
pfc_list_triggers_types('Supports').
pfc_list_triggers_types('Edits').

% print_db_items_and_neg(Title,Fact,What):-nonvar(Fact),Fact= ~(_),!,fail.

%= 	 	 

% %% print_db_items_and_neg( ?Title, ?Fact, ?What) is semidet.
%
% Print Database Items And Negated.
%
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,Fact,What).
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,~(Fact),What).


%= 	 	 

% %% pfc_list_triggers_1( ?What) is semidet.
%
% Managed Predicate list triggers  Secondary Helper.
%
pfc_list_triggers_1(~(What)):-var(What),!.
pfc_list_triggers_1(~(_What)):-!.
pfc_list_triggers_1(What):-var(What),!.
pfc_list_triggers_1(What):- 
   print_db_items('Supports User',spft_precanonical(P,mfl4(VarNameZ,_,_,_),ax),spft(P,mfl4(VarNameZ,_,_,_),ax),What),
   print_db_items('Forward Facts',(nesc(F)),F,What),
   print_db_items('Forward Rules',(_==>_),What),
 ignore((What\= ~(_),safe_functor(What,IWhat,_),
   print_db_items_and_neg('Instance Of',isa(IWhat,_),IWhat),
   print_db_items_and_neg('Instances: ',isa(_,IWhat),IWhat),
   print_db_items_and_neg('Subclass Of',genls(IWhat,_),IWhat),
   print_db_items_and_neg('Subclasses: ',genls(_,IWhat),IWhat))),
   forall(suggest_m(M),print_db_items('PFC Watches', mpred_prop(M,_,_,_),What)),
   print_db_items('Triggers Negative', nt(_,_,_,_),What),
   print_db_items('Triggers Goal',bt(_,_,_),What),
   print_db_items('Triggers Positive',pt(_,_,_),What),
   print_db_items('Bidirectional Rules',(_<==>_),What), 
   dif(A,B),print_db_items('Supports Deduced',spft_precanonical(P,A,B),spft(P,A,B),What),
   dif(G,ax),print_db_items('Supports Nonuser',spft_precanonical(P,G,G),spft(P,G,G),What),
   print_db_items('Backchaining Rules',(_<-_),What),
   % print_db_items('Edits',is_disabled_clause(_),What),
   print_db_items('Edits',is_edited_clause(_,_,_),What),
   print_db_items('Instances',isa(_,_),What),
   print_db_items('Subclasses',genls(_,_),What),
   print_db_items('Negative Facts',~(_),What),

   print_db_items('ArgTypes',argGenls(_,_,_),What),
   print_db_items('ArgTypes',argIsa(_,_,_),What),
   print_db_items('ArgTypes',argQuotedIsa(_,_,_),What),
   print_db_items('ArgTypes',meta_argtypes(_),What),
   print_db_items('ArgTypes',predicate_property(G,meta_predicate(G)),What),
   print_db_items('ArgTypes',resultGenls(_,_),What),
   print_db_items('ArgTypes',resultIsa(_,_),What),
   print_db_items('Arity',arity(_,_),What),
   print_db_items('Arity',current_predicate(_),What),
   print_db_items('MetaFacts Predicate',predicate_property(_,_),What),
   print_db_items('Sources',module_property(_,_),What),
   print_db_items('Sources',predicateConventionMt(_,_),What),
   print_db_items('Sources',source_file(_,_),What),
   print_db_items('Sources',_:man_index(_,_,_,_,_),What),
   print_db_items('Sources',_:'$pldoc'(_,_,_,_),What),
   print_db_items('Sources',_:'$pred_option'(_,_,_,_),What),
   print_db_items('Sources',_:'$mode'(_,_),What),
   !.     


pinfo(F/A):- listing(F/A),safe_functor(P,F,A),findall(Prop,predicate_property(P,Prop),List),wdmsg_pretty(pinfo(F/A)==List),!.



% %% pp_DB is semidet.
%
% Pretty Print All.
%
%pp_DB:- defaultAssertMt(M),clause_b(mtHybrid(M)),!,pp_DB(M).
%pp_DB:- forall(clause_b(mtHybrid(M)),pp_DB(M)).

pp_DB:- prolog_load_context(module,M),pp_DB(M).

with_exact_kb(M,G):- M:call(G).

pp_DB(M):-
 with_exact_kb(M,
 M:must_det_l((
  pp_db_facts,
  pp_db_rules,
  pp_db_triggers,
  pp_db_supports))).

pp_db_facts:- context_module(M), pp_db_facts(M).
pp_db_rules:- context_module(M), pp_db_rules(M).
pp_db_triggers:- context_module(M), pp_db_triggers(M).
pp_db_supports:- context_module(M), pp_db_supports(M).


:- system:import(pp_DB/0).
:- system:export(pp_DB/0).

%  pp_db_facts ...

pp_db_facts(MM):- ignore(pp_db_facts(MM,_,true)).

pp_db_facts(MM,Pattern):- pp_db_facts(MM,Pattern,true).

pp_db_facts(MM,P,C):-
  pfc_facts_in_kb(MM,P,C,L),
  pfc_classifyFacts(L,User,Pfc,_ZRule),
  length(User,UserSize),length(Pfc,PfcSize),
  format("~N~nUser added facts in [~w]: ~w",[MM,UserSize]),
  pp_db_items(User),
  format("~N~nSystem added facts in [~w]: ~w",[MM,PfcSize]),
  pp_db_items(Pfc).

%  printitems clobbers it''s arguments - beware!


pp_db_items(Var):-var(Var),!,format("~N  ~p",[Var]).
pp_db_items([]):-!.
pp_db_items([H|T]):- !,
  % numbervars(H,0,_),
  format("~N  ~p",[H]),
  nonvar(T),pp_db_items(T).

pp_db_items((P >= FT)):- is_hidden_pft(P,FT),!.
  
pp_db_items(Var):-
  format("~N  ~p",[Var]).


is_hidden_pft(_,(mfl4(_VarNameZ,baseKB,_,_),ax)).
is_hidden_pft(_,(why_marked(_),ax)).


pp_mask(Type,MMMask):- strip_module(MMMask,MM,Mask),pp_4_mask(Type,MM,Mask,Mask).
pp_mask(Type,MMMask,Template):- strip_module(MMMask,MM,Mask),pp_4_mask(Type,MM,Mask,Template).

pp_4_mask(Type,MM,Mask,Template):-   
  bagof_PFC(Template,Mask^lookup_kb(MM,Mask),Nts),
  list_to_set_variant(Nts,NtsSet),!,
  pp__mask_list(Type,MM,NtsSet).

pp__mask_list(Type,MM,[]):- !,
  format("~N~nNo ~ws in [~w]...~n",[Type,MM]).
pp__mask_list(Type,MM,NtsSet):- length(NtsSet,Size), !,
  format("~N~n~ws (~w) in [~w]...~n",[Type,Size,MM]),
  pp_db_items(NtsSet).

pfc_classifyFacts([],[],[],[]).

pfc_classifyFacts([H|T],User,Pfc,[H|Rule]):-
  pfcType(H,rule(_)),
  !,
  pfc_classifyFacts(T,User,Pfc,Rule).

pfc_classifyFacts([H|T],[H|User],Pfc,Rule):-
  % get_source_uu(UU),
  get_first_user_reason(H,_UU),
  !,
  pfc_classifyFacts(T,User,Pfc,Rule).

pfc_classifyFacts([H|T],User,[H|Pfc],Rule):-
  pfc_classifyFacts(T,User,Pfc,Rule).


pp_db_rules(MM):- 
   pp_mask("Forward Rule",MM:'==>'(_,_)),
   pp_mask("Bidirectional Rule",MM:'<==>'(_,_)),
   pp_mask("Backchaining Rule",MM:'<-'(_,_)),
   pp_mask("Implication Rule",MM:'=>'(_,_)),
   pp_mask("Bi-conditional Rule",MM:'<=>'(_,_)),
   pp_mask("Negative Fact",MM:('~'(_))),
   pp_mask("Prolog Rule",MM:spft((H:-B),_,_),(H:-B)),
  % pp_mask("Material-impl Rule",MM:<=(_,_)),
 % pp_mask("Prolog Rule",MM::-(_,_)),
 !.


pp_db_triggers(MM):- 
 pp_mask("Positive trigger",MM:pt(_,_)),
 pp_mask("Negative trigger",MM:nt(_,_,_)),
 pp_mask("Goal trigger",MM:bt(_,_)),!.

pp_db_supports(MM):-
  % temporary hack.
  format("~N~nSupports in [~w]...~n",[MM]),
  with_exact_kb(MM, bagof_PFC((P >= S), pfcGetSupport(P,S),L)),
  list_to_set_variant(L,LS),
  pp_db_items(LS),!.


list_to_set_variant(List, Unique) :-
    list_unique_1(List, [], Unique),!.

list_unique_1([], _, []).
list_unique_1([X|Xs], So_far, Us) :-
    memberchk_variant(X, So_far),!,
    list_unique_1(Xs, So_far, Us).
list_unique_1([X|Xs], So_far, [X|Us]) :-
    list_unique_1(Xs, [X|So_far], Us).


% %%	memberchk_variant(+Val, +List)
%
%	Deterministic check of membership using =@= rather than
%	unification.

memberchk_variant(X, [Y|Ys]) :-
   (   X =@= Y
   ->  true
   ;   memberchk_variant(X, Ys)
   ).

lookup_kb(MM,MHB):- strip_module(MHB,M,HB),
     expand_to_hb(HB,H,B),
      (MM:clause(M:H,B,Ref)*->true; M:clause(MM:H,B,Ref)),
      %clause_ref_module(Ref),
      clause_property(Ref,module(MM)).


% %% has_cl( +H) is semidet.
%
% Has Clause.
%
has_cl(H):-predicate_property(H,number_of_clauses(_)).



% %% clause_or_call( +H, ?B) is semidet.
%
% Clause Or Call.
%

% PFC2.0 clause_or_call(M:H,B):-is_ftVar(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
% PFC2.0 clause_or_call(isa(I,C),true):-!,call_u(isa_asserted(I,C)).
% PFC2.0 clause_or_call(genls(I,C),true):-!,on_x_log_throw(call_u(genls(I,C))).
clause_or_call(H,B):- clause(src_edit(_Before,H),B).
clause_or_call(H,B):- predicate_property(H,number_of_clauses(C)),predicate_property(H,number_of_rules(R)),((R*2<C) -> (clause(H,B)*->!;fail) ; clause(H,B)).
% PFC2.0 clause_or_call(H,true):- call_u(should_call_for_facts(H)),no_repeats(on_x_log_throw(H)).

  /*



% as opposed to simply using clause(H,true).

% %% should_call_for_facts( +H) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(H):- get_functor(H,F,A),call_u(should_call_for_facts(H,F,A)).

% %% should_call_for_facts( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(_,F,_):- a(prologSideEffects,F),!,fail.
should_call_for_facts(H,_,_):- modulize_head(H,HH), \+ predicate_property(HH,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- clause_b(mpred_prop(_M,F,A,pfcRHS)),!,fail.
should_call_for_facts(_,F,A):- clause_b(mpred_prop(_M,F,A,pfcMustFC)),!,fail.
should_call_for_facts(_,F,_):- a(prologDynamic,F),!.
should_call_for_facts(_,F,_):- \+ a(pfcControlled,F),!.

       */

% %% no_side_effects( +P) is semidet.
%
% No Side Effects.
%
%no_side_effects(P):-  (\+ is_side_effect_disabled->true;(get_functor(P,F,_),a(prologSideEffects,F))).

pfc_facts_in_kb(MM,P,C,L):- with_exact_kb(MM,setof_or_nil(P,pfcFact(P,C),L)).


lookup_spft(P,F,T):-pfcGetSupport(P,(F,T)).
% why_dmsg(Why,Msg):- with_current_why(Why,dmsg_pretty(Msg)).

u_to_uu(U,(U,ax)):- var(U),!.
u_to_uu(U,U):- nonvar(U),U=(_,_),!.
u_to_uu([U|More],UU):-list_to_conjuncts([U|More],C),!,u_to_uu(C,UU).
u_to_uu(U,(U,ax)):-!.

% %% get_source_uu( :TermU) is det.
%
% Get Source Ref (Current file or User)
%
:- module_transparent((get_source_uu)/1).
get_source_uu(UU):- must(((get_source_ref1(U),u_to_uu(U,UU)))),!.

get_source_ref1(U):- quietly_ex(((current_why(U),nonvar(U)));ground(U)),!.
get_source_ref1(U):- quietly_ex(((get_source_mfl(U)))),!.


:- module_transparent((get_why_uu)/1).
get_why_uu(UU):- findall(U,current_why(U),Whys),Whys\==[],!,u_to_uu(Whys,UU).
get_why_uu(UU):- get_source_uu(UU),!.


get_startup_uu(UU):-u_to_uu((isRuntime,mfl4(VarNameZ,baseKB, user_input, _)),UU),varnames_load_context(VarNameZ).

is_user_reason((_,U)):-atomic(U).
only_is_user_reason((U1,U2)):- freeze(U2,is_user_reason((U1,U2))).

is_user_fact(P):-get_first_user_reason(P,UU),is_user_reason(UU).


get_first_real_user_reason(P,UU):- nonvar(P), UU=(F,T),
  quietly_ex((  ((((lookup_spft(P,F,T))),is_user_reason(UU))*-> true;
    ((((lookup_spft(P,F,T))), \+ is_user_reason(UU))*-> (!,fail) ; fail)))).

get_first_user_reason(P,(F,T)):-
  UU=(F,T),
  ((((lookup_spft(P,F,T))),is_user_reason(UU))*-> true;
    ((((lookup_spft(P,F,T))), \+ is_user_reason(UU))*-> (!,fail) ;
       (clause_asserted(P),get_source_uu(UU),is_user_reason(UU)))),!.
get_first_user_reason(_,UU):- get_why_uu(UU),is_user_reason(UU),!.
get_first_user_reason(_,UU):- get_why_uu(UU),!.
get_first_user_reason(P,UU):- must_ex(ignore(((get_first_user_reason0(P,UU))))),!.
get_first_user_reason0(_,(M,ax)):-get_source_mfl(M).

%get_first_user_reason(_,UU):- get_source_uu(UU),\+is_user_reason(UU). % ignore(get_source_uu(UU)).

%:- export(pfc_at_box:defaultAssertMt/1).
%:- system:import(defaultAssertMt/1).
%:- import(pfc_at_box:defaultAssertMt/1).

:- module_transparent((get_source_mfl)/1).
get_source_mfl(M):- current_why(M), nonvar(M) , M =mfl4(_VarNameZ,_,_,_).
get_source_mfl(mfl4(VarNameZ,M,F,L)):- defaultAssertMt(M), current_source_location(F,L),varnames_load_context(VarNameZ).

get_source_mfl(mfl4(VarNameZ,M,F,L)):- defaultAssertMt(M), current_source_file(F:L),varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ,M,F,_L)):- defaultAssertMt(M), current_source_file(F),varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ,M,_F,_L)):- defaultAssertMt(M), varnames_load_context(VarNameZ).
%get_source_mfl(M):- (defaultAssertMt(M)->true;(atom(M)->(module_property(M,class(_)),!);(var(M),module_property(M,class(_))))).
get_source_mfl(M):- fail,dtrace,
 ((defaultAssertMt(M) -> !;
 (atom(M)->(module_property(M,class(_)),!);
    pfcError(no_source_ref(M))))).

is_source_ref1(_).

% defaultAssertMt(M):- prolog_load_context(module, M).



pfc_pp_db_justifications(P,Js):-
 show_current_source_location, 
 must_ex(quietly_ex(( format("~NJustifications for ~p:",[P]),
  pfc_pp_db_justification1('',Js,1)))).

pfc_pp_db_justification1(_Prefix,[],_).

pfc_pp_db_justification1(Prefix,[J|Js],N):-
  % show one justification and recurse.
  nl,  
  pfc_pp_db_justifications2(Prefix,J,N,1),
  reset_shown_justs,
  N2 is N+1,
  pfc_pp_db_justification1(Prefix,Js,N2).

pfc_pp_db_justifications2(_Prefix,[],_,_).

pfc_pp_db_justifications2(Prefix,[C|Rest],JustNo,StepNo):-
(nb_hasval('$last_printed',C)-> dmsg_pretty(chasVal(C)) ;
(
 (StepNo==1->fmt('~N~n',[]);true),
  backward_compatibility:sformat(LP,' ~w.~p.~p',[Prefix,JustNo,StepNo]),
  nb_pushval('$last_printed',LP),
  format("~N  ~w ~p",[LP,C]),
  ignore(loop_check(pfcWhy_sub_sub(C))),
  StepNext is 1+StepNo,
  pfc_pp_db_justifications2(Prefix,Rest,JustNo,StepNext))).


pfcWhy_sub_sub(P):-
  justifications(P,Js),
  clear_proofs,
  % retractall(t_l:whybuffer(_,_)),
  (nb_hasval('$last_printed',P)-> dmsg_pretty(hasVal(P)) ;
   ((
  assertz(t_l:whybuffer(P,Js)),
   nb_getval('$last_printed',LP),
   ((pfc_pp_db_justification1(LP,Js,1),fmt('~N~n',[])))))).

nb_pushval(Name,Value):-nb_current(Name,Before)->nb_setval(Name,[Value|Before]);nb_setval(Name,[Value]).
nb_peekval(Name,Value):-nb_current(Name,[Value|_Before]).
nb_hasval(Name,Value):-nb_current(Name,List),member(Value,List).
nb_popval(Name,Value):-nb_current(Name,[Value|Before])->nb_setval(Name,Before).

reset_shown_justs:- retractall(t_l:shown_why(_)),nop(color_line(red,1)).
clear_proofs:- retractall(t_l:whybuffer(_P,_Js)),nop(color_line(cyan,1)).


lookup_spft_match(A,B,C):- copy_term(A,AA),lookup_spft(A,B,C),A=@=AA.

lookup_spft_match_deeper(H,Fact,Trigger):- 
  copy_term(H,HH),
  lookup_spft((H:- _B),Fact,Trigger),
  H=@=HH.

lookup_spft_match_first(A,B,C):- nonvar(A),!, 
  no_repeats(((lookup_spft_match(A,B,C);lookup_spft(A,B,C)))).

lookup_spft_match_first(A,B,C):- lookup_spft(A,B,C).


% %% pfc_is_info( :TermC) is semidet.
%
% PFC If Is A Info.
%
pfc_is_info((CWC,Info)):- (atom(CWC),is_a_info(CWC));pfc_is_info(Info).
pfc_is_info(pfc_bc_only(C)):-is_ftNonvar(C),!.
pfc_is_info(infoF(C)):-is_ftNonvar(C),!.
pfc_is_info(inherit_above(_,_)).


is_a_info(fail).
is_a_info(CWC):- is_pfc_chained(CWC).

is_pfc_chained(cwc).
is_pfc_chained(awc).
is_pfc_chained(zwc).
is_pfc_chained(fwc).
is_pfc_chained(bwc).
is_pfc_chained(wac).

% %% cwc is det.
%
% Cwc.
%
cwc:-true.

% %% fwc is det.
%
% Fwc.
%
fwc:-true.

% %% bwc is semidet.
%
% Bwc.
%
bwc:-true.

% %% wac is semidet.
%
% Wac.
%
wac:-true.

awc:-true.
zwc:-true.

reserved_body(B):-var(B),!,fail.
reserved_body(attr_bind(_)).
reserved_body(attr_bind(_,_)).
reserved_body(B):-reserved_body_helper(B).

reserved_body_helper(B):- \+ compound(B),!,fail.
reserved_body_helper((ZAWC,_)):- atom(ZAWC),is_pfc_chained(ZAWC).

call_only_based_mfl(H,mfl4(_VarNameZ,M,F,L)):- 
  ignore(predicate_property(H,imported_from(M));predicate_property(H,module(M))),
  ignore(predicate_property(H,line_count(L))),
  ignore(source_file(M:H,F);predicate_property(H,file(F));(predicate_property(H,foreign),F=foreign)).

uses_call_only(H):- predicate_property(H,foreign),!.
uses_call_only(H):- predicate_property(H,_), \+ predicate_property(H,interpreted),!.

clause_match(H,_B,uses_call_only(H)):- uses_call_only(H),!.
clause_match(H,B,Ref):- clause_asserted(H,B,Ref),!.
clause_match(H,B,Ref):- ((copy_term(H,HH),clause(H,B,Ref),H=@=HH)*->true;clause(H,B,Ref)), \+ reserved_body_helper(B).

find_mfl(C,MFL):- lookup_spft_match(C,MFL,ax).
find_mfl(C,MFL):- unwrap_litr0(C,UC) -> C\==UC -> find_mfl(UC,MFL).
find_mfl(C,MFL):- expand_to_hb(C,H,B),
   find_hb_mfl(H,B,_Ref,MFL)->true; (clause_match(H,B,Ref),find_hb_mfl(H,B,Ref,MFL)).

find_hb_mfl(_H,_B,Ref,mfl4(_VarNameZ,M,F,L)):- atomic(Ref),clause_property(Ref,line_count(L)),
 clause_property(Ref,file(F)),clause_property(Ref,module(M)). 
find_hb_mfl(H,B,_,mfl4(VarNameZ,M,F,L)):- lookup_spft_match_first( (H:-B),mfl4(VarNameZ,M,F,L),_),!.
find_hb_mfl(H,B,_Ref,mfl4(VarNameZ,M,F,L)):- lookup_spft_match_first(H,mfl4(VarNameZ,M,F,L),_),ground(B).
find_hb_mfl(H,_B,uses_call_only(H),MFL):- !,call_only_based_mfl(H,MFL).

% %% pfc_facts_and_universe( +P) is semidet.
%
% PFC Facts And Universe.
%
pfc_facts_and_universe(P):- (is_ftVar(P)->pred_head_all(P);true),call_u(P). % (meta_wrapper_rule(P)->call_u(P) ; call_u(P)).


% %% repropagate( :TermP) is semidet.
%
% Repropagate.
%
repropagate(_):-  check_context_module,fail.
%repropagate(P):-  check_real_context_module,fail.

repropagate(P):-  is_ftVar(P),!.
repropagate(==>P):- !,repropagate(P).
repropagate(P):-  meta_wrapper_rule(P),!,call_u(repropagate_meta_wrapper_rule(P)).
repropagate(P):-  \+ predicate_property(P,_),'$find_predicate'(P,PP),PP\=[],!,forall(member(M:F/A,PP),
                                                          must((functor(Q,F,A),repropagate_1(M:Q)))).
repropagate(F/A):- is_ftNameArity(F,A),!,functor(P,F,A),!,repropagate(P).
repropagate(F/A):- atom(F),is_ftVar(A),!,repropagate(F).

repropagate(P):-  \+ predicate_property(_:P,_),dmsg_pretty(undefined_repropagate(P)),dumpST,dtrace,!,fail.
repropagate(P):-  call_u(repropagate_0(P)).


predicate_to_goal(P,Goal):-atom(P),get_arity(P,F,A),functor(Goal,F,A).
predicate_to_goal(PF/A,Goal):-atom(PF),get_arity(PF,F,A),functor(Goal,F,A).
predicate_to_goal(G,G):-compound(G),!.

% %% repropagate_0( +P) is semidet.
%
% repropagate  Primary Helper.
%
repropagate_0(P):- loop_check(call_u(repropagate_1(P)),true).

:- thread_local t_l:is_repropagating/1.


% %% repropagate_1( +P) is semidet.
%
% repropagate  Secondary Helper.
%
repropagate_1(P):- is_ftVar(P),!.
repropagate_1(==>P):- !,repropagate_1(P).
repropagate_1(USER:P):- USER==user,!,repropagate_1(P).
%repropagate_1((P/_)):-!,repropagate_1(P).

repropagate_1(P):- call_u(repropagate_2(P)).

:- export(repropagate_2/1).
:- module_transparent(repropagate_2/1).

% %% repropagate_2( +P) is semidet.
%
% repropagate  Extended Helper.
%
repropagate_2(P):-
 call_u(doall((no_repeats((pfc_facts_and_universe(P))),
    locally_tl(is_repropagating(P),ignore((once(show_failure(fwd_ok(P))),show_call(pfcFwd(P)))))))).

% repropagate_meta_wrapper_rule(P==>_):- !, repropagate(P).

% %% repropagate_meta_wrapper_rule( +P) is semidet.
%
% Repropagate Meta Wrapper Rule.
%
repropagate_meta_wrapper_rule(P):-repropagate_1(P).


% %% fwd_ok( :TermP) is semidet.
%
% Forward Repropigated Ok.
%
fwd_ok(_):-!.
fwd_ok(P):-ground(P),!.
fwd_ok(if_missing1(_,_)).
fwd_ok(idForTest(_,_)).
fwd_ok(clif(_)).
fwd_ok(pfclog(_)).
fwd_ok(X):-compound(X),get_assertion_head_arg(_,X,E),compound(E),functor(E,(:-),_),!.
% fwd_ok(P):-must(ground(P)),!.

/*
has_body_atom(WAC,P,Rest):- call(WAC==P -> Rest = true ; (is_ftCompound(P),functor(P,F,A),is_atom_body_pfa(WAC,P,F,A,Rest))).
is_atom_body_pfa(WAC,P,F,2,Rest):-get_assertion_head_arg(1,P,E),E==WAC,get_assertion_head_arg(2,P,Rest),!.
is_atom_body_pfa(WAC,P,F,2,Rest):-get_assertion_head_arg(2,P,E),E==WAC,get_assertion_head_arg(1,P,Rest),!.
*/

:- module_transparent( (get_assertion_head_arg)/3).
get_assertion_head_arg(N,P,E):-get_assertion_head_unnegated(P,PP),!,arg(N,PP,E).

same_functors(Head1,Head2):-must_det(get_unnegated_functor(Head1,F1,A1)),must_det(get_unnegated_functor(Head2,F2,A2)),!,F1=F2,A1=A2.



% %% pfc_facts_only( +P) is semidet.
%
% PFC Facts Only.
%
pfc_facts_only(P):- (is_ftVar(P)->(pred_head_all(P),\+ meta_wrapper_rule(P));true),no_repeats(P).



:- thread_local(t_l:user_abox/1).

check_context_module.









is_pfc_file(M,F,TF):- (module_property(M,file(F)),is_pfc_file(F)) *-> TF=true ; 
  (module_property(M,file(F))*->TF=false ; (F= (-), TF=false)).

maybe_ensure_abox(M):- is_pfc_file(M,F,_), (F \== (-)), !,   
  (is_pfc_file(F)->show_call(is_pfc_file(F),ensure_abox(M));dmsg_pretty(not_is_pfc_file(M,F))).
maybe_ensure_abox(M):- show_call(not_is_pfc_file,ensure_abox(M)).


:- module_transparent((ensure_abox)/1).
ensure_abox(_):-!.
ensure_abox(M):- 
  ignore(((M==user;M==baseKB)->true;nop(add_import_module(M,pfc_lib,end)))),
  dynamic(M:defaultTBoxMt/1),
  must(ensure_abox_support(M,baseKB)),!.
:- module_transparent((ensure_abox_support)/2).
ensure_abox_support(M,TBox):- clause_b(M:defaultTBoxMt(TBox)),!.
ensure_abox_support(M,TBox):- 
	% asserta(M:defaultTBoxMt(TBox)),
   set_prolog_flag(M:unknown,error),  
  must(forall(pfc_database_term(F,A,_Type),
           kb_shared(M:F/A))),
  must(M:ain(TBox:mtHybrid(M))),   
  must(system:add_import_module(M,system,end)),
  (M\==user->must(ignore(system:delete_import_module(M,user)));true),!,
  must(setup_module_ops(M)),!.
  
ensure_abox_support(M,TBox):- 
       system:add_import_module(M,user,end),
       must(ignore(system:delete_import_module(M,system))),
       must(ignore(system:delete_import_module(M,baseKB))),
       system:add_import_module(M,system,end),
       retractall(M:defaultTBoxMt(TBox)),
       throw(failed_ensure_abox_support(M,TBox)).


   

setup_module_ops(M):- pfc_op_each(pfc_op_unless(M)).

pfc_op_unless(M,A,B,C):- op_safe(A,B,M:C).

pfc_op_each(OpEach):-
            call(OpEach,1199,fx,('==>')), % assert
            call(OpEach,1199,fx,('?->')), % ask
            call(OpEach,1190,xfy,('::::')), % Name something
            call(OpEach,1180,xfx,('==>')), % Forward chaining
            call(OpEach,1170,xfx,('<==>')), % Forward and backward chaining
            call(OpEach,1160,xfx,('<==')), % backward chain PFC sytle
            call(OpEach,1160,xfx,('<-')), % backward chain PTTP sytle (currely really PFC)
            call(OpEach,1160,xfx,('<=')), % backward chain DRA sytle
            call(OpEach,1150,xfx,('=>')), % Logical implication
            call(OpEach,1130,xfx,('<=>')), % Logical bi-implication
            call(OpEach,600,yfx,('&')), 
            call(OpEach,600,yfx,('v')),
            call(OpEach,400,fx,('~')),
            % call(OpEach,300,fx,('-')),
            call(OpEach,350,xfx,('xor')),
            % replicate user:op/3s in case we remove inheritance
            forall(current_op(X,Y,user:Z),
              call(OpEach,X,Y,Z)).


in_dialect_pfc:- is_pfc_file. % \+ current_prolog_flag(dialect_pfc,cwc),!.

is_pfc_module(SM):- clause_b(using_pfc(SM,_, SM, pfc_toplevel)),!.
is_pfc_module(SM):- clause_b(using_pfc(SM,_, SM, pfc_mod)),!,clause_b(mtCanAssert(SM)).
is_pfc_module(SM):- clause_b(mtHybrid(SM)).

can_extreme_debug :- \+ in_pengines.

%is_pfc_file:- can_extreme_debug, current_prolog_flag(expect_pfc_file,always),!,(is_pfc_file_notrace  ; (nop((dumpST,sleep(1),break,rtrace(is_pfc_file_notrace),break)),fail)),!.
%is_pfc_file:- can_extreme_debug, current_prolog_flag(expect_pfc_file,never),!,(\+is_pfc_file_notrace->fail;nop((dumpST,sleep(1),break,rtrace(\+is_pfc_file_notrace),break))),!.
is_pfc_file:- quietly(is_pfc_file_notrace),!.

:- export(is_pfc_file/0).
:- system:import(is_pfc_file/0).
%:- header_sane:import(is_pfc_file/0).

:- export(is_pfc_file_notrace/0).
is_pfc_file_notrace:- notrace(( prolog_load_context(source, SFile), 
                       (prolog_load_context(file,File);source_location(File,_W)))),
              is_pfc_filename(File,SFile),!.

is_pfc_file_notrace:- current_source_file(FileL),(FileL=File:_),!,is_pfc_file(File),!.

 %is_pfc_file_notrace:- \+ , prolog_load_context(module, M),M\==baseKB,is_pfc_module(M),!,clause_b(mtHybrid(M)).
:- system:import(is_pfc_file_notrace/0).

is_pfc_file(File):- is_pfc_filename(File,File),!.
is_pfc_file(M):- is_pfc_file(M,F,TF),!, (F \== (-)), TF = true.
% First checks to confirm there is nothing inhibiting
is_pfc_filename(File,_):- (atom_concat(_,'.pfc.pl',File);atom_concat(_,'.clif',File);atom_concat(_,'.plmoo',File);atom_concat(_,'.pfc',File)),!.
%is_pfc_filename(File,_):- call(call,lmcache:mpred_directive_value(File, language, Lang)),!,(Lang==pfc;Lang==clif;Lang==fwd).
%is_pfc_filename(File,_):- check_how_virtualize_file(false,File),!,fail.
is_pfc_filename(File,_):- baseKB:how_virtualize_file(heads,File,0),!.
is_pfc_filename(File,File):-!,fail.
%is_pfc_filename(_,File):- call(call,lmcache:mpred_directive_value(File, language, Lang)),!,(Lang==pfc;Lang==clif;Lang==fwd).
is_pfc_filename(_,File):- check_how_virtualize_file(heads,File),!.
is_pfc_filename(_,File):- check_how_virtualize_file(false,File),!,fail.
%is_pfc_filename(_,File):- atom_concat(_,'.pfc.pl',File);atom_concat(_,'.clif',File);atom_concat(_,'.plmoo',File);atom_concat(_,'.pfc',File),!.






:- multifile(lmcache:has_pfc_database_preds/1).
:- dynamic(lmcache:has_pfc_database_preds/1).


% %% assert_setting01( ?X) is semidet.
% :- srtrace.
assert_setting01(M:P):-safe_functor(P,_,A),dupe_term(P,DP),setarg(A,DP,_),system:retractall(M:DP),system:asserta(M:P).

% :- break.

% %% which_file( ?F) is semidet.
%
% Which File.
%
which_file(F):- prolog_load_context(source,F) -> true; once(loading_source_file(F)).

% :- '$hide'(defaultAssertMt(_)).


% %% get_file_type_local( ?File, ?Type) is det.
%
% Get File Type.
%
get_file_type_local(File,Type):-var(File),!,quietly_must(loading_source_file(File)),get_file_type_local(File,Type),!.
get_file_type_local(File,pfc):-file_name_extension(_,'.pfc.pl',File),!.
get_file_type_local(File,pfc):-file_name_extension(_,'.clif',File),!.
get_file_type_local(File,Type):-file_name_extension(_,Type,File),!.
get_file_type_local(File,Type):-clause_b(lmcache:pfc_directive_value(File,language,Type)).


mtCanAssert(Module):- clause_b(mtNonAssertable(Module)),!,fail.
mtCanAssert(ABox):- \+ \+ (ABox=abox),!,trace_or_throw(mtCanAssert(ABox)),fail.
mtCanAssert(Module):- clause_b(mtHybrid(Module)).
mtCanAssert(user):-  is_user_pfc.
% mtCanAssert(Module):- clause_b(mtExact(Module)).
mtCanAssert(Module):-  module_property(Module,file(_)),!,fail.
mtCanAssert(Module):- (loading_source_file(File),get_file_type_local(File,pfc),prolog_load_context(module,Module)).
mtCanAssert(Module):- clause_b(mtProlog(Module)),!,fail.
mtCanAssert(Module):- \+ is_code_module(Module),!.

is_user_pfc:- clause_b(mtHybrid(user)).



% %% fileAssertMt(-ABox) is det.
%
% Gets ABox is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules

fileAssertMt(M):- nonvar(M), fileAssertMt(ABoxVar),!,M=@=ABoxVar.
fileAssertMt(M):- loading_source_file(File),clause_b(baseKB:file_to_module(File,M)),!.
fileAssertMt(M):- loading_source_file(File),clause_b(lmcache:pfc_directive_value(File,module,M)),!.
fileAssertMt(M):- fileAssertMt0(M), (source_location(_,_)->show_call(set_fileAssertMt(M));true).

fileAssertMt0(M):- prolog_load_context(module,M),mtCanAssert(M),!.
fileAssertMt0(M):- '$current_typein_module'(M),mtCanAssert(M),!.
fileAssertMt0(M):- 'strip_module'(module,M,module),mtCanAssert(M),!.
fileAssertMt0(M):- must(get_fallBackAssertMt(M)),!.


:- initialization(fix_baseKB_imports,now).


% %% set_fileAssertMt( ABox) is semidet.
%
% Sets the File''s Module.
%

% set_fileAssertMt(M):- '$current_source_module'(M),!.
set_fileAssertMt(M):-
 ensure_abox(M),
  ignore(show_failure(clause_b(mtCanAssert(M)))),
 % sanity(mtCanAssert(M)),
  
  must(which_file(File)),
  assert_setting(baseKB:file_to_module(File,M)),
  assert_setting(lmcache:pfc_directive_value(File,module,M)),
  asserta_until_eof(t_l:current_defaultAssertMt(M)),!,
  ((is_pfc_file) -> must(set_current_modules_until_eof(M)) ; true).


%:- import(is_pfc_file/0).
% :- '$hide'(set_fileAssertMt(_)).


set_current_modules_until_eof(M):- 
 '$current_typein_module'(CM),'$set_typein_module'(M),call_on_eof('$set_typein_module'(CM)),
 '$current_source_module'(SM),'$set_source_module'(M),call_on_eof('$set_source_module'(SM)).


% %% set_defaultAssertMt( ?M) is semidet.
%
% Sets Current Module.
%
set_defaultAssertMt(M):-
  ignore(show_failure(mtCanAssert(M))),
   ensure_abox(M),!,
   % assert_setting(t_l:current_defaultAssertMt(M)),
   asserta_until_eof(t_l:current_defaultAssertMt(M)),
  (source_location(_,_)-> ((fileAssertMt(M) -> true; set_fileAssertMt(M)))  ;set_current_modules_until_eof(M)).

% :- '$hide'(set_defaultAssertMt(_)).



% %% defaultAssertMt(-Ctx) is det.
%
% M is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules

defaultAssertMt(M):- nonvar(M), defaultAssertMt(ABoxVar),!,M=@=ABoxVar.
defaultAssertMt(M):- quietly(defaultAssertMt0(M)),!.

defaultAssertMt0(M):- t_l:current_defaultAssertMt(M).
defaultAssertMt0(M):- get_fallBackAssertMt(M),!.

get_fallBackAssertMt(M):- loading_source_file(File),clause_b(baseKB:file_to_module(File,M)).
get_fallBackAssertMt(M):- loading_source_file(File),clause_b(lmcache:pfc_directive_value(File,module,M)).
get_fallBackAssertMt(M):- guess_maybe_assertMt(M),clause_b(mtHybrid(M)),!.
get_fallBackAssertMt(M):- guess_maybe_assertMt(M),mtCanAssert(M),!.
get_fallBackAssertMt(M):- guess_maybe_assertMt(M).

guess_maybe_assertMt(M):- '$current_source_module'(M).
guess_maybe_assertMt(M):- context_module(M).
guess_maybe_assertMt(M):- loading_source_file(File),clause_b(baseKB:file_to_module(File,M)).
guess_maybe_assertMt(M):- loading_source_file(File),clause_b(lmcache:pfc_directive_value(File,module,M)).
guess_maybe_assertMt(M):-  which_file(File)->current_module(M),module_property(M,file(File)),File\==M.
guess_maybe_assertMt(M):- '$current_typein_module'(M).
guess_maybe_assertMt(M):- nb_current(defaultQueryMt,M),!.
guess_maybe_assertMt(M):- which_file(File)->make_module_name_local(File,M),current_module(M),File\==M.   
guess_maybe_assertMt(M):- (loading_source_file(File),get_file_type_local(File,pfc)),prolog_load_context(module,M).



make_module_name_local(A,B):- make_module_name_local0(A,B), \+ exists_file(B),!.

make_module_name_local0(Source,KB):- clause_b(mtProlog(Source)),t_l:current_defaultAssertMt(KB),!.
make_module_name_local0(Source,KB):- clause_b(mtGlobal(Source)),t_l:current_defaultAssertMt(KB),!.
make_module_name_local0(Source,SetName):- clause_b(baseKB:file_to_module(Source,SetName)),!.
make_module_name_local0(Source,Source):- lmcache:has_pfc_database_preds(Source).
make_module_name_local0(Source,Source):- clause_b(mtHybrid(Source)),!.
make_module_name_local0(user,KB):- t_l:current_defaultAssertMt(KB),!.
make_module_name_local0(user,baseKB):-!.
make_module_name_local0(Source,GetName):- make_module_name00(Source,GetName).

:- baseKB:import(is_code_module/1).
is_code_module(system).
is_code_module(user).
is_code_module(baseKB):-!,fail.
is_code_module(pfc_lib).
is_code_module(M):- clause_b(mtProlog(M)),!,fail.
is_code_module(M):- module_property(M,class(system)).
is_code_module(M):- module_property(M,class(library)).
is_code_module(baseKB):-!,fail.
is_code_module(Mt):- clause_b(mtHybrid(Mt)),!,fail.
is_code_module(M):- module_property(M,file(_)).
%call_u_mp(user, P1 ):- !,  call_u_mp(baseKB,P1).



defaultQueryMt(M):- nonvar(M), defaultQueryMt(ABoxVar),!,M=@=ABoxVar.
defaultQueryMt(M):- nb_current(defaultQueryMt,M)->true;(defaultQueryMt0(M)->nb_setval(defaultQueryMt,M)),!.


defaultQueryMt0(M):- 'strip_module'(module,M,module),clause_b(mtHybrid(M)),!.
defaultQueryMt0(M):- prolog_load_context(module,M),clause_b(mtHybrid(M)),!.
defaultQueryMt0(M):- '$current_typein_module'(M),clause_b(mtHybrid(M)),!.
defaultQueryMt0(M):- guess_maybe_assertMt(M),clause_b(mtHybrid(M)),!.
defaultQueryMt0(M):- guess_maybe_assertMt(M),mtCanAssert(M),!.
defaultQueryMt0(M):- guess_maybe_assertMt(M).


scan_missed_source:-!.
pfc_ain(G):- pfcAdd(G).
mpred_ain(G):- pfcAdd(G).
pfc_unload_file:-!.
file_begin(_).
% %% neg_in_code( +G) is semidet.
%
% Negated In Code.
%
:- meta_predicate neg_in_code(*).
:- export(neg_in_code/1).
neg_in_code(G):- no_repeats(loop_check(neg_in_code0(G))).

:- kb_shared(baseKB:proven_neg/1).

:- meta_predicate neg_in_code0(*).
:- export(neg_in_code0/1).
/*
neg_in_code0(G):- cwc, loop_check(proven_neg(G)).
neg_in_code0(G):- cwc, var(G),!,loop_check(lookup_u(~ G)).
neg_in_code0(call_u(G)):- !,neg_in_code0(G).
neg_in_code0(~(G)):- nonvar(G),!,  \+ loop_check(~G) ,!.
neg_in_code0(G):-  is_ftNonvar(G), a(prologSingleValued,G),
      must((if_missing_mask(G,R,Test),nonvar(R),nonvar(Test))),call_u(R),!,call_u(Test).
neg_in_code0(G):- cwc, clause(~G,Call)*-> call_u(Call).
*/
neg_in_code0(G):- loop_check(neg_may_naf(G)), \+ loop_check(G),!.
% neg_in_code0(_:G):-!,baseKB:neg_in_code0(G).


:- meta_predicate neg_may_naf(*).
:- module_transparent(neg_may_naf/1).
:- export(neg_may_naf/1).

% %% neg_may_naf( :GoalP) is semidet.
%
% Negated May Negation-by-faliure.
%
neg_may_naf(P):- pfc_non_neg_literal(P),get_functor(P,F),clause_b(prologNegByFailure(F)),!.
neg_may_naf(P):- is_ftCompound(P),is_never_pfc(P).



% %% if_missing_mask( +Q, ?R, ?Test) is semidet.
%
% If Missing Mask.
%

if_missing_mask(M:Q,M:R,M:Test):- nonvar(Q),!,if_missing_mask(Q,R,Test).
if_missing_mask(Q,~Q,\+Q):- \+ is_ftCompound(Q),!.

%if_missing_mask(ISA, ~ ISA, \+ ISA):- functor(ISA,F,1),(F==tSwim;call_u(functorDeclares(F))),!.
if_missing_mask(HB,RO,TestO):- once(pfc_rule_hb(HB,H,B)),B\==true,HB\==H,!,
     if_missing_mask(H,R,TestO),subst(HB,H,R,RO).

if_missing_mask(ISA, ISA, \+ ISA):- functor(ISA, _F,1),!.% (F==tSwim;call_u(functorDeclares(F))),!.

if_missing_mask(Q,R,Test):-
   which_missing_argnum(Q,N),
   if_missing_n_mask(Q,N,R,Test),!.

if_missing_mask(ISA, ~ ISA, \+ ISA).

% %% if_missing_n_mask( +Q, ?N, ?R, ?Test) is semidet.
%
% If Missing Mask.
%
if_missing_n_mask(Q,N,R,Test):-
  get_assertion_head_arg(N,Q,Was),
  (nonvar(R)-> (which_missing_argnum(R,RN),get_assertion_head_arg(RN,R,NEW));replace_arg(Q,N,NEW,R)),!,
   Test=dif:dif(Was,NEW).

/*
Old version
if_missing_mask(Q,N,R,dif:dif(Was,NEW)):- 
 must((is_ftNonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A))),
  (singleValuedInArg(F,N) -> 
    (get_assertion_head_arg(N,Q,Was),replace_arg(Q,N,NEW,R));
    ((get_assertion_head_arg(N,Q,Was),is_ftNonvar(Was)) -> replace_arg(Q,N,NEW,R);
        (N=A,get_assertion_head_arg(N,Q,Was),replace_arg(Q,N,NEW,R)))).
*/


% %% which_missing_argnum( +VALUE1, ?VALUE2) is semidet.
%
% Which Missing Argnum.
%
which_missing_argnum(Q,N):- compound(Q),\+ compound_name_arity(Q,_,0),
 must((acyclic_term(Q),is_ftCompound(Q),get_functor(Q,F,A))),
 F\=t,
  (pfc_call(singleValuedInArg(F,N)) -> true; which_missing_argnum(Q,F,A,N)).

which_missing_argnum(_,_,1,_):-!,fail.
which_missing_argnum(Q,_F,A,N):- between(A,1,N),get_assertion_head_arg(N,Q,Was),is_ftNonvar(Was).


% %% is_reprop( +X) is semidet.
%
% If Is A Reprop.
%
is_reprop(X):- compound(X),is_reprop_0(X).

% %% is_reprop_0( +X) is semidet.
%
% If Is A reprop  Primary Helper.
%
is_reprop_0(~(X)):-!,is_reprop(X).
is_reprop_0(X):-get_functor(X,repropagate,_).


% %% pfc_non_neg_literal( +X) is semidet.
%
% PFC Not Negated Literal.
%
pfc_non_neg_literal(X):- is_reprop(X),!,fail.
pfc_non_neg_literal(X):- atom(X),!.
pfc_non_neg_literal(X):- sanity(stack_check),
    pfcPositiveLiteral(X), X \= ~(_), X \= mpred_prop(_,_,_,_), X \= conflict(_).


% module prefixed clauses for sure should be non pfc?
is_never_pfc(Var):- \+ callable(Var),!.
%is_never_pfc(P):- always_pfc_p(P),!,fail.
%is_never_pfc(_):- prolog_load_context(file,F),\+ prolog_load_context(source,F),atom_concat(_,'.pl',F),\+ atom_concat(_,'pfc.pl',F).
is_never_pfc(':-'(_)).
is_never_pfc(':-'(C,_)):- !, is_never_pfc(C).
is_never_pfc(M:P):- cna_functor_safe(P,F,A),clause_b(mpred_prop(M,F,A,prologBuiltin)),!.
is_never_pfc(M:_):- M==system,!.
is_never_pfc(_:C):- !, is_never_pfc(C).

is_never_pfc(goal_expansion(_,_,_,_)).
is_never_pfc(module(_,_)).
is_never_pfc(proven_helper(_)).
is_never_pfc(begin_of_file).
is_never_pfc('?-'(_)).
is_never_pfc('-->'(_,_)):-!.
is_never_pfc('==>>'(_,_)):-!.
is_never_pfc(attr_unify_hook(_,_)):-!.

is_never_pfc(P):- is_never_pfc_sys(P), (\+ can_extreme_debug -> true ;  (\+ is_pfc_file_notrace->true;(dumpST,rtrace(is_never_pfc_sys(P))))), !.

is_never_pfc_sys(P):- notrace(predicate_property(P,static)),predicate_property(P,static).
%is_never_pfc_sys(P):- predicate_property(P,built_in).
is_never_pfc_sys(P):- predicate_property(P,system),cna_functor_safe(P,F,2),current_op(Pri,xfx,F),Pri<1000.

cna_functor_safe(P,F,A):- compound(P) -> compound_name_arity(P,F,A) ; functor(P,F,A).


/** <module> pfc_terms
% Provides a common set of operators in translation between the several logical languages
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


:- export(any_to_number/2).
% %% any_to_value( ?Var, ?Var) is semidet.
%
% Any Converted To Value.
%
any_to_value(Var,Var):-var(Var),!.
any_to_value(V,Term):-atom(V),!,atom_to_value(V,Term).
any_to_value(A,V):-any_to_number(A,V).
any_to_value(A,A).


% %% any_to_number( :TermN, ?N) is semidet.
%
% Any Converted To Number.
%
any_to_number(N,N):- number(N),!.
any_to_number(ftDiceFn(A,B,C),N):- ground(A),if_defined(roll_dice(A,B,C,N)),!.
any_to_number(A,N):-atom(A),atom_to_value(A,V),A\=V,any_to_number(V,N).
any_to_number(A,N):- catch(number_string(N,A),_,fail).

% %% atom_to_value( ?V, :TermTerm) is semidet.
%
% Atom Converted To Value.
%
atom_to_value(V,Term):-not(atom(V)),!,any_to_value(V,Term).
% 56
atom_to_value(V,Term):- catch((read_term_from_atom(V,Term,[variable_names([])])),_,fail),!.
% 18d18+4000
atom_to_value(V,ftDiceFn(T1,T2,+T3)):- atomic_list_concat_safe([D1,'d',D2,'+',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.
atom_to_value(V,ftDiceFn(T1,T2,-T3)):- atomic_list_concat_safe([D1,'d',D2,'-',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.



% %% is_ftText( ?Arg) is semidet.
%
% If Is A Format Type Text.
%
is_ftText(Arg):- string(Arg),!.
is_ftText(Arg):- \+ compound(Arg),!,fail.
is_ftText(Arg):- safe_functor(Arg,s,_),!.
is_ftText([Arg|_]):-string(Arg),!.
is_ftText(Arg):- is_ftVar(Arg),!,fail.
is_ftText(Arg):- text_to_string_safe(Arg,_),!.
is_ftText(Arg):- safe_functor(Arg,S,_), ereq(resultIsa(S,ftText)).

:- kb_global(baseKB:ftText/1).
:-prolog_load_context(module,M),
  ain(baseKB:(ftText(A):- !, if_defined(term_is_ft(A, ftText),M:is_ftText(A)),!)).

% =======================================================
% term utils
% =======================================================

:- was_export(inverse_args/2).



% %% inverse_args( ?AR, ?GS) is semidet.
%
% Inverse Arguments.
%
inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.

:- was_export(same_vars/2).



% %% same_vars( ?T1, ?T2) is semidet.
%
% Same Variables.
%
same_vars(T1,T2):-term_variables(T1,V1),term_variables(T2,V2),!,V1==V2.




% %% replace_arg( ?C, :PRED3A, ?VAR, ?CC) is semidet.
%
% Replace Argument.
%
replace_arg(C,A,_VAR,_CC):-sanity((is_ftCompound(C),integer(A))),fail.
replace_arg((C:-B),A,VAR,(CC:-B)):-!,replace_arg(C,A,VAR,CC).
replace_arg(~ (C),A,VAR,~(CC)):-!,replace_arg(C,A,VAR,CC).
replace_arg( \+ (C),A,VAR,~(CC)):-!,replace_arg(C,A,VAR,CC).
replace_arg(M:(C),A,VAR,M:(CC)):-!,replace_arg(C,A,VAR,CC).
replace_arg(C,0,VAR,CC):-!, C=..[_|ARGS],CC=..[VAR|ARGS].
replace_arg(C,1,VAR,CC):-!, C=..[F,_|ARGS],CC=..[F,VAR|ARGS].
replace_arg(C,2,VAR,CC):-!, C=..[F,A,_|ARGS],CC=..[F,A,VAR|ARGS].
replace_arg(C,3,VAR,CC):-!, C=..[F,A,B,_|ARGS],CC=..[F,A,B,VAR|ARGS].
% replace_arg(C,A,VAR,CO):- dupe_term(C,CC),setarg(A,CC,VAR),!,CC=CO.
replace_arg(C,A,VAR,CC):- C=..FARGS,replace_nth_arglist(FARGS,A,VAR,FARGO),!,CC=..FARGO.

% :- pfc_trace_nochilds(replace_arg/4).

% %% replace_nth_arglist(+List, +Index, +Element, -NewList) is det[private]
% Replace the Nth (1-based) element of a list.
% :- pfc_trace_nochilds(replace_nth_arglist/4).



% %% replace_nth_arglist( :TermARG1, ?VALUE2, ?VAR, :TermVAR) is semidet.
%
% Replace Nth Arglist.
%
replace_nth_arglist([],_,_,[]):- !.
replace_nth_arglist([_|ARGO],0,VAR,[VAR|ARGO]):- !.
replace_nth_arglist([T|FARGS],A,VAR,[T|FARGO]):- 
    A2 is A-1,replace_nth_arglist(FARGS,A2,VAR,FARGO).





% %% replace_nth_ref( :TermARG1, ?N, ?OldVar, ?NewVar, :TermARG5) is semidet.
%
% Replace Nth Ref.
%
replace_nth_ref([],_N,_OldVar,_NewVar,[]):- !,trace_or_throw(missed_the_boat).
replace_nth_ref([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):- !.
replace_nth_ref([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):- 
 Which1 is Which-1,
 replace_nth_ref(ARGS,Which1,OldVar,NewVar,NEWARGS),!.


% :- pfc_trace_nochilds(update_value/3).



% %% update_value( ?OLD, ?NEW, ?NEXT) is semidet.
%
% Update Value.
%
update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(OLD,NEW,NEWV):- var(OLD),!,compute_value_no_dice(NEW,NEWV).
update_value(OLD,X,NEW):- is_list(OLD),!,list_update_op(OLD,X,NEW),!.
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLDI,X,NEW):- number(X),X<0,compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_,NEW,NEWV):- compute_value_no_dice(NEW,NEWV),!.




% %% flatten_append( ?First, ?Last, ?Out) is semidet.
%
% Flatten Append.
%
flatten_append(First,Last,Out):-flatten([First],FirstF),flatten([Last],LastF),append(FirstF,LastF,Out),!.




% %% list_update_op( ?OLDI, :TermX, ?NEW) is semidet.
%
% List Update Oper..
%
list_update_op(OLDI,+X,NEW):-flatten_append(OLDI,X,NEW),!.
list_update_op(OLDI,-X,NEW):-flatten([OLDI],OLD),flatten([X],XX),!,list_difference_eq(OLD,XX,NEW),!.




% %% compute_value_no_dice( ?NEW, ?NEW) is semidet.
%
% Compute Value No Dice.
%
compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDiceFn,_),!.
compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDice,_),!.
compute_value_no_dice(NEW,NEWV):-compute_value(NEW,NEWV).




% %% compute_value( ?NEW, ?NEWV) is semidet.
%
% Compute Value.
%
compute_value(NEW,NEWV):-catch(NEWV is NEW,_,fail),!.
compute_value(NEW,NEWV):-catch(any_to_value(NEW,NEWV),_,fail),!.
compute_value(NEW,NEW).




% %% insert_into( :TermARGS, ?VALUE2, ?Insert, :TermInsert) is semidet.
%
% Insert Converted To.
%
insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).



% ========================================
% is_holds_true/is_holds_false
% ========================================


:- was_export(into_plist/2).

%= 	 	 

% %% into_plist( ?In, ?Out) is semidet.
%
% Converted To Plist.
%
into_plist(In,Out):-into_plist_arities(2,12,In,Out).

:- was_export(into_plist_arities/4).

%= 	 	 

% %% into_plist_arities( ?Min, ?Max, ?PLIST, ?PLISTO) is semidet.
%
% Converted To Plist Arities.
%
into_plist_arities(Min,Max,PLIST,PLISTO):- var(PLIST),!,between(Min,Max,X),length(PLIST,X),PLISTO=PLIST.
into_plist_arities(_,_,[P|LIST],[P|LIST]):-var(P),!.
into_plist_arities(_,_,[(t)|PLIST],PLIST):-!.  % t is our versuion of '$holds' or call/N
into_plist_arities(_,_,plist(P,LIST),[P|LIST]):-!.
into_plist_arities(_,_,Call,PLIST):- Call=..PLIST. % finally the fallthrue



%= 	 	 

% %% never_pfc_tcall( ?VALUE1) is semidet.
%
% Never Managed Predicate Managed Predicate.
%

never_pfc_tcall(mpred_prop).
never_pfc_tcall(isa).
never_pfc_tcall(arity).


local_qh_pfc_prop(M,F,A,C):- call_u(mpred_prop(M,F,A,C)).


% :- setup_pfc_ops.


                   
%= 	 	 

:- meta_predicate(if_result(0,0)).

%= 	 	 

% %% if_result( :GoalTF, :Goal) is semidet.
%
% If Result.
%
if_result(TF,Call):-(TF->Call;true).



:- baseKB:forall(between(1,11,A),kb_local(t/A)).
:- baseKB:forall(between(5,7,A),kb_local(mpred_f/A)).


%= 	 	 

% %% pfc_plist_t( ?P, :TermLIST) is semidet.
%
% Managed Predicate Plist True Stucture.
%

/*
% pfc_plist_t(P,[]):-!,t(P). 
pfc_plist_t(P,LIST):-var(P),!,is_list(LIST),CALL=..[t,P|LIST],on_x_debug((CALL)).
pfc_plist_t(t,[P|LIST]):-!, pfc_plist_t(P,LIST).
% %pfc_plist_t(pfc_isa,[C,_A,I]):-!,ground(I:C),local_qh_pfc_isa(C,I).
pfc_plist_t(isa,[I,C]):-!,call(call,t,C,I).
pfc_plist_t(P,_):-never_pfc_tcall(P),!,fail.
pfc_plist_t(P,[L|IST]):-is_holds_true(P),!,pfc_plist_t(L,IST).
pfc_plist_t(P,LIST):-is_holds_false(P),!,call_u(mpred_f(LIST)).
pfc_plist_t(P,LIST):- CALL=..[t,P|LIST],on_x_debug(CALL).
*/

:- meta_predicate(pfc_fa_call(?,?,0)).



%= 	 	 

% %% pfc_fa_call( ?F, ?UPARAM2, :Goal) is semidet.
%
% Managed Predicate Functor-arity Call.
%
pfc_fa_call(F,A,Call):- var(F),!,
 no_repeats(F,(clause_b(support_hilog(F,A));clause_b(arity(F,A)))), 
   once((F\==t, 
   \+ a(rtNotForUnboundPredicates,F),current_predicate(F,M:_OtherCall))),
    on_x_debug(M:Call).
pfc_fa_call(M:F,A,Call):- nonvar(M),!,pfc_fa_call(F,A,M:Call).
pfc_fa_call(F,_,Call):-F\==t,current_predicate(F,M:_OtherCall),!,M:Call.


%= 	 	 

% %% pfc_fact_arity( ?VALUE1, ?VALUE2) is semidet.
%
% Managed Predicate Fact Arity.
%
pfc_fact_arity(F,A):- pfc_call(arity(F,A)),
  suggest_m(M),
  once(local_qh_pfc_prop(M,F,A,prologHybrid);
     local_qh_pfc_prop(M,F,A,pfcControlled);
     local_qh_pfc_prop(M,F,A,prologPTTP);
     local_qh_pfc_prop(M,F,A,prologKIF)),!.


%= 	 	 

% %% prologHybridFact( ?G) is semidet.
%
% Prolog Hybrid Fact.
%
prologHybridFact(G):- (var(G)->(pfc_fact_arity(F,A),safe_functor(G,F,A));true),into_pfc_form(G,M),!,no_repeats(call_u(M)).




:- dynamic prolog:make_hook/2.
:- multifile prolog:make_hook/2.

prolog:make_hook(before, _C) :- nodebug(codewalk).
prolog:make_hook(after, _C) :- nodebug(codewalk).

:- nodebug(codewalk).
:- nodebug(codewalk(trace)).


% if the correct flag is set, dtrace exection of Pfc
pfc_trace_msg(_):- current_prolog_flag(set_pfc_silent,true).
pfc_trace_msg(Info):- not_not_ignore_quietly_ex(((((clause_asserted(pfc_is_tracing_exec);tracing)->(show_wdmsg(Info));true)))).
pfc_trace_msg(Format,Args):- not_not_ignore_quietly_ex((((clause_asserted(pfc_is_tracing_exec);tracing)-> (show_wdmsg(Format,Args))))),!.
% pfc_trace_msg(Format,Args):- not_not_ignore_quietly_ex((((format_to_message(Format,Args,Info),pfc_trace_msg(Info))))).

show_wdmsg(A,B):- current_prolog_flag(set_pfc_silent,true)-> true; wdmsg_pretty(A,B).
show_wdmsg(A):- current_prolog_flag(set_pfc_silent,true)-> true; wdmsg_pretty(A).


:- use_module(library(pfc_test)).
:- dynamic(lmcache:pfc_is_spying_pred/2).

% %%  pfc_set_warnings(+TF) is det.
%   true = sets flag to cause Pfc warning messages to print.
%   false = sets flag to cause Pfc warning messages not to print.
%
pfc_set_warnings(False):- False == false, !,
  retractall(pfcWarnings(_)).
pfc_set_warnings(True):-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(True)).


pfc_trace_all:- pfc_trace_exec,pfcTrace,pfc_set_warnings(true),set_pfc_silent(false).
pfc_notrace_all:- pfc_notrace_exec,pfc_notrace,pfc_set_warnings(false).
pfc_notrace_exec:- retractall(pfc_is_tracing_exec).


% %% pfc_is_silent is det.
%
% If Is A Silient.
%
pfc_is_silent :- t_l:hide_pfc_trace_exec,!, \+ tracing.
pfc_is_silent :- quietly_ex(( \+ t_l:pfc_debug_local, \+ lookup_u(pfc_is_tracing_exec), \+ lookup_u(lmcache:pfc_is_spying_pred(_,_)),
  current_prolog_flag(debug,false), is_release)) ,!.

oinfo(O):- xlisting((O, - spft, - ( ==> ), - pt , - nt , - bt , - mdefault, - lmcache)).


/*
pfc_load_term(:- module(_,L)):-!, call_u_no_bc(maplist(export,L)).
pfc_load_term(:- TermO):- call_u_no_bc(TermO).
pfc_load_term(TermO):-pfc_ain_object(TermO).

*/


% not_not_ignore_quietly_ex(G):- ignore(quietly(\+ \+ G)).
% not_not_ignore_quietly_ex(G):- ignore( \+ (G)).
not_not_ignore_quietly_ex(G):- notrace(ignore(quietly_ex(\+ \+ G))).

% needed:  pfc_trace_rule(Name)  ...

set_pfc_silent(TF):-set_prolog_flag(set_pfc_silent,TF).


pfc_nowatch:-  pfc_notrace_exec.

pfc_trace_exec:- assert(pfc_is_tracing_exec),set_pfc_silent(false).
pfc_notrace:- pfc_untrace.
pfc_untrace:- pfc_untrace(_).
pfc_untrace(Form0):- get_head_term(Form0,Form), retractall(lmcache:pfc_is_spying_pred(Form,print)).




:- thread_local(t_l:hide_pfc_trace_exec/0).


set_file_abox_module(User):- '$set_typein_module'(User), '$set_source_module'(User),
  set_fileAssertMt(User).

set_file_abox_module_wa(User):- set_file_abox_module(User),set_defaultAssertMt(User).


:- thread_local(t_l:pfc_debug_local).

  
% %% get_pfc_is_tracing(:PRED) is semidet.
%
% PFC If Is A Tracing.
%
get_pfc_is_tracing(_):-!,fail.
get_pfc_is_tracing(Form0):- get_head_term(Form0,Form), t_l:hide_pfc_trace_exec,!,
  \+ \+ ((quietly_ex(call(lmcache:pfc_is_spying_pred(Form,print))))).
get_pfc_is_tracing(Form0):- get_head_term(Form0,Form),
  once(t_l:pfc_debug_local ; tracing ; clause_asserted(pfc_is_tracing_exec) ;
     call(lmcache:pfc_is_spying_pred(Form,print))).

:- prolog_load_context(module,M),declare_pfc_support(M).
%:- import_everywhere(BaseKB).

pfc_load_lib:- consult(library('pfclib/system_autoexec_3_0.pfc')).
:- fixup_exports.


:- export(pfc_term_expansion/2).
:- system:import(pfc_term_expansion/2).
:- multifile(system:term_expansion/4).
:- dynamic(system:term_expansion/4).
:- module_transparent(system:term_expansion/4).
system:term_expansion(I,S0,O,S1):-
  notrace( \+ current_prolog_flag(xref, true) -> prolog_load_context(term,T)->T==I)->pfc_term_expansion(I,O)->I\=@=O->S0=S1.





/*

% fix_mp(Why,Unassertable,_,_):- Why = clause(_,_), unassertable(Unassertable),!,trace_or_throw(unassertable_fix_mp(Why,Unassertable)).

*/
system_between(A,B,C):-call(call,between,A,B,C).

pfc_truth_value(Call,vTrue,vAsserted):-clause_b(Call),!.
pfc_truth_value(Call,vTrue,vDeduced):-call_u(Call),!.
pfc_truth_value(_Call,vUnknown,vFailed).

convention_to_mt(From,Why,F,A,RealMt):-convention_to_symbolic_mt_ec(From,Why,F,A,Mt),to_real_mt(Why,Mt,RealMt).

get_unnegated_mfa(M:G,M,F,A):-!,get_unnegated_functor(G,F,A).
get_unnegated_mfa(G,M,F,A):- strip_module(G,M0,_),get_unnegated_functor(G,F,A),
                 convention_to_mt(M0,get_unnegated_mfa(G,M,F,A),F,A,M).

get_unnegated_functor(G,F,A):- strip_module(G,_,GO),
   get_assertion_head_unnegated(GO,Unwrap),
   nonvar(Unwrap),
   safe_functor(Unwrap,F,A),
   ignore(show_failure(\+ bad_head_pred(F))),!.
   

:- module_transparent( (get_assertion_head_unnegated)/2).

get_assertion_head_unnegated(Head,Unwrap):-
  get_assertion_head(Head,Mid),
  maybe_unnegated(Mid,Unwrap).

   
maybe_unnegated(Head,Head):- \+ compound(Head),!.
maybe_unnegated(~ Head,Unwrap):- \+ is_ftVar(Head),!, get_assertion_head(Head,Unwrap).
maybe_unnegated( \+ Head,Unwrap):- \+ is_ftVar(Head),!, get_assertion_head(Head,Unwrap).
maybe_unnegated(Head,Unwrap):- get_assertion_head(Head,Unwrap).


get_assertion_head(Head,Head):- \+ compound(Head),!.
get_assertion_head(Head,Unwrap):- is_ftVar(Head),!,Head=Unwrap.
get_assertion_head( ( Head :- _ ),Unwrap):- nonvar(Head), !, get_assertion_head(Head,Unwrap).
get_assertion_head(Head,Unwrap):- strip_module(Head,_,HeadM),Head\=@=HeadM,!,get_assertion_head(HeadM,Unwrap).
% Should?
get_assertion_head( ( _,Head),Unwrap):- \+ is_ftVar(Head),!, get_assertion_head(Head,Unwrap).
% Should?
get_assertion_head((P/_),PP):- \+ is_ftVar(P),!,get_assertion_head(P,PP).
% Should?
% NOOOO get_assertion_head((P<-_),PP):-compound(P),!,get_assertion_head(P,PP).
% disabled
get_assertion_head( Head,UnwrapO):- fail, pfc_rule_hb(Head,Unwrap,_),nonvar(Unwrap),
  Head \=@= Unwrap,!,get_assertion_head(Unwrap,UnwrapO).
get_assertion_head(P,P).


get_head_term(Form,Form):-var(Form),!.
get_head_term(F/A,Form):- integer(A),safe_functor(Form,F,A),!.
get_head_term(Form0,Form):- get_assertion_head_unnegated(Form0,Form).

bad_head_pred([]).
bad_head_pred('[]').
bad_head_pred((.)).
bad_head_pred('{}').
bad_head_pred('[|]').
bad_head_pred(',').
bad_head_pred(':').
bad_head_pred('/').
bad_head_pred(':-').
bad_head_pred(';').
bad_head_pred( \+ ).
bad_head_pred_neg('~').

% bad_head_pred('=>').
% bad_head_pred('<-').
% bad_head_pred('==>').
% Probably bad_head_pred('==>').

% the next line transforms to pfc_lib:convention_to_symbolic_mt(_From,_Why,A, _, B) :- call(ereq, predicateConventionMt(A, B)), !.

convention_to_symbolic_mt_ec(From,Why,F,A,Mt):-convention_to_symbolic_mt(From,Why,F,A,Mt).

/*convention_to_symbolic_mt(_From,_Why,predicateConventionMt,2,baseKB):-!.
convention_to_symbolic_mt(_From,_Why,genlMt,2,baseKB):-!.
convention_to_symbolic_mt(_From,_Why,mtNonAssertable,1,baseKB):-!.
convention_to_symbolic_mt(_From,_Why,mtProlog,1,baseKB):-!.
convention_to_symbolic_mt(_From,_Why,functorDeclares,1,baseKB):-!.
convention_to_symbolic_mt(_From,_Why,functorIsMacro,1,baseKB):-!.
*/

convention_to_symbolic_mt(_From,_Why,mtHybrid,1,baseKB):-!.
convention_to_symbolic_mt(From,_Why,F,_,Mt):-  clause_b(From:predicateConventionMt(F,Mt)),!.
convention_to_symbolic_mt(_From,_Why,F,A,M):- lmcache:already_decl(kb_global,M,F,A),!.




% convention_to_symbolic_mt(From,Why,F,A,Error):- bad_head_pred(F),!,dumpST,dmsg_pretty(bad_head_pred(F)),break,trace_or_throw_ex(error_convention_to_symbolic_mt(From,Why,F,A,Error)).
convention_to_symbolic_mt(_From,_Why,F,A,M):- lmcache:already_decl(kb_global,M,F,A),!.
convention_to_symbolic_mt(_From,_Why,F,A,abox):- pfc_database_term_syntax(F,A,_).
convention_to_symbolic_mt(_From,_Why,F,A,abox):- lmcache:already_decl(kb_shared,_,F,A),!.
convention_to_symbolic_mt(_From,_Why,F,A,abox):- lmcache:already_decl(kb_local,_,F,A),!.

convention_to_symbolic_mt(_From,_Why,F,A,Mt):-  safe_functor(P,F,A),show_success(predicate_property(P,imported_from(Mt))),!.
convention_to_symbolic_mt(_From,_Why,F,A,   M):- lmcache:already_decl(kb_global,M,F,A),!.
convention_to_symbolic_mt(_From,_Why,F,A,abox):- pfc_database_term(F,A,_).
convention_to_symbolic_mt(_From,_Why,F,A,abox):- clause_b(safe_wrap(_M,F,A,ereq)).


convention_to_symbolic_mt(From,Why,F,A,Error):- bad_head_pred(F),!,Error = From,
  if_interactive((
   dumpST,dmsg_pretty(bad_head_pred(F)),break,trace_or_throw(error_convention_to_symbolic_mt(From,Why,F,A,Error)))).


% convention_to_symbolic_mt(_From,_Why,_,_,M):- atom(M),!.

full_transform_warn_if_changed(_,MH,MHH):-!,MH=MHH.
full_transform_warn_if_changed(Why,MH,MHH):- full_transform(Why,MH,MHH),!,sanity(MH=@=MHH).
full_transform_warn_if_same(Why,MH,MHH):- full_transform(Why,MH,MHH),!,sanity(MH \=@= MHH).

%% pred_head( :PRED1Type, ?P) is semidet.
%
% Predicate Head.
%
pred_head(Type,P):- no_repeats(P,(call(Type,P),\+ nonfact_metawrapper(P),is_ftCompound(P))).


%% pred_head_all( +P) is semidet.
%
% Predicate Head All.
%
pred_head_all(P):- pred_head(pred_all,P).


%% nonfact_metawrapper( :TermP) is semidet.
%
% Nonfact Metawrapper.
%
nonfact_metawrapper(~(_)).
nonfact_metawrapper(pt(_,_)).
nonfact_metawrapper(bt(_,_,_)).
nonfact_metawrapper(nt(_,_)).
nonfact_metawrapper(spft(_,_,_)).
nonfact_metawrapper(added(_)).
% we use the arity 1 forms is why 
nonfact_metawrapper(term_expansion(_,_)).
nonfact_metawrapper(P):- \+ current_predicate(_,P).
nonfact_metawrapper(M:P):-atom(M),!,nonfact_metawrapper(P).
nonfact_metawrapper(P):- get_functor(P,F,_), 
   (a(prologSideEffects,F);a(rtNotForUnboundPredicates,F)).
nonfact_metawrapper(P):-rewritten_metawrapper(P).


%% rewritten_metawrapper( +C) is semidet.
%
% Rewritten Metawrapper.
%
rewritten_metawrapper(_):-!,fail.
%rewritten_metawrapper(isa(_,_)).
rewritten_metawrapper(C):-is_ftCompound(C),functor(C,t,_).


%% meta_wrapper_rule( :TermARG1) is semidet.
%
% Meta Wrapper Rule.
%
meta_wrapper_rule((_<-_)).
meta_wrapper_rule((_<==>_)).
meta_wrapper_rule((_==>_)).
meta_wrapper_rule((_:-_)).



%% pred_all( +P) is semidet.
%
% Predicate All.
%
pred_all(P):-pred_u0(P).
pred_all(P):-pred_t0(P).
pred_all(P):-pred_r0(P).


%% pred_u0( +P) is semidet.
%
% Predicate For User Code Primary Helper.
%
pred_u0(P):-pred_u1(P),has_db_clauses(P).
pred_u0(P):-pred_u2(P).

%% pred_u1( +VALUE1) is semidet.
%
% Predicate For User Code Secondary Helper.
%
pred_u1(P):-a(pfcControlled,F),arity_no_bc(F,A),functor(P,F,A).
pred_u1(P):-a(prologHybrid,F),arity_no_bc(F,A),functor(P,F,A).
pred_u1(P):-a(prologDynamic,F),arity_no_bc(F,A),functor(P,F,A).

%% pred_u2( +P) is semidet.
%
% Predicate For User Code Extended Helper.
%
pred_u2(P):- compound(P),functor(P,F,A),sanity(no_repeats(arity_no_bc(F,A))),!,has_db_clauses(P).
pred_u2(P):- no_repeats(arity_no_bc(F,A)),functor(P,F,A),has_db_clauses(P).



%% has_db_clauses( +PI) is semidet.
%
% Has Database Clauses.
%
has_db_clauses(PI):-modulize_head(PI,P),
   predicate_property(P,number_of_clauses(NC)),\+ predicate_property(P,number_of_rules(NC)), \+ \+ clause(P,true).



%% pred_t0(+ ?P) is semidet.
%
% Predicate True Stucture Primary Helper.
%
pred_t0(P):-mreq('==>'(P)).
pred_t0(P):-mreq(pt(P,_)).
pred_t0(P):-mreq(bt(P,_)).
pred_t0(P):-mreq(nt(P,_,_)).
pred_t0(P):-mreq(spft(P,_,_)).

%pred_r0(-(P)):- call_u(-(P)).
%pred_r0(~(P)):- mreq(~(P)).


%% pred_r0( :TermP) is semidet.
%
% Predicate R Primary Helper.
%
pred_r0(P==>Q):- mreq(P==>Q).
pred_r0(P<==>Q):- mreq(P<==>Q).
pred_r0(P<-Q):- mreq(P<-Q).

mreq(G):- clause_b(G).


:- user:use_module(library(clpfd),['#='/2]).
%% get_arity( :TermTerm, ?F, ?A) is semidet.
%
% Get Arity.
%
get_arity(Term,F,A):- atom(Term),F=Term,!,ensure_arity(F,A).
get_arity(F/A,F,A):-!,atom(F),ensure_arity(F,A),!,(A>0).
get_arity(F // A,F,A2):- must(integer(A)),!, atom(F), is(A2 , A+2), ensure_arity(F,A2),!,(A2>0).
get_arity(F // A,F,A2):- use_module(library(clpfd),['#='/2]),!, atom(F), clpfd:call(#=(A2 , A+2)), ensure_arity(F,A2),!,(A2>0).
get_arity(M:FA,F,A):-atom(M),!,get_arity(FA,F,A).
get_arity(FA,F,A):- get_functor(FA,F,A),must(A>0).

% arity_no_bc(F,A):- call_u(arity(F,A)).
arity_no_bc(F,A):- clause_b(arity(F,A)).
arity_no_bc(F,A):- clause_b(support_hilog(F,A)).
arity_no_bc(F,A):- clause_b(functorDeclares(F)),!,A=1.
arity_no_bc(completeExtentAsserted,1).
arity_no_bc(home,2).
arity_no_bc(record,2).
arity_no_bc(F,A):- suggest_m(M),clause_b(mpred_prop(M,F,AA,_)),nonvar(AA),A=AA.
%arity_no_bc(F,A):- current_predicate(F/A)
% arity_no_bc(F,A):- current_predicate(_:F/A),\+(current_predicate(_:F/AA),AA\=A). =

%% ensure_arity( ?VALUE1, ?VALUE2) is semidet.
%
% Ensure Arity.
%
ensure_arity(F,A):- 
 one_must(
   arity_no_bc(F,A),
   one_must(
    (current_predicate(F/A),(A>0),assert_arity(F,A)),
    (ground(F:A),(A>0),assert_arity(F,A)))),
  !.


%=

%% assert_arity( ?F, :PRED2A) is semidet.
%
% Assert Arity.
%

assert_arity(F,A):- sanity(\+ ((bad_arity(F,A), trace_or_throw(assert_arity(F,A))))), arity_no_bc(F,A),!.
assert_arity(F,A):- arity_no_bc(F,AA), A\=AA,dmsg_pretty(assert_additional_arity(F,AA->A)),!,ain_fast(arity(F,A)).
assert_arity(F,A):- ain_fast(arity(F,A)),!.

bad_arity(F,_):- \+ atom(F).
bad_arity(_,A):- \+ integer(A).
bad_arity('[|]',_).
bad_arity(typeProps,0).
bad_arity(argIsa,2).
bad_arity(isEach,_).
bad_arity(_,0).
bad_arity(prologDynamic,2).
bad_arity(F,A):- \+ good_pred_relation_name(F,A).


%=

%% good_pred_relation_name( ?F, ?A) is semidet.
%
% Good Predicate Relation Name.
%
good_pred_relation_name(F,A):- \+ bad_pred_relation_name0(F,A).


%=

%% bad_pred_relation_name0( ?V, ?VALUE2) is semidet.
%
% Bad Predicate Relation Name Primary Helper.
%
bad_pred_relation_name0(V,_):- \+ atom(V),!.
bad_pred_relation_name0('[]',_).
bad_pred_relation_name0('',_).
bad_pred_relation_name0('!',_).
bad_pred_relation_name0('{}',_).
bad_pred_relation_name0(',',_).
bad_pred_relation_name0('[|]',_).

%=

%% bad_pred_relation_name1( ?X, ?Y) is semidet.
%
% Bad Predicate Relation Name Secondary Helper.
%
bad_pred_relation_name1(X,Y):-bad_pred_relation_name0(X,Y).
bad_pred_relation_name1(F,A):-must_det((atom_codes(F,[C|_]),to_upper(C,U))),!, U == C, A>1.
bad_pred_relation_name1(F,A):-arity_no_bc(F,AO), A \= AO.

% :-after_boot(writeq("Seen Mpred_props at start!\n")),!.

%=

%% functor_check_univ( ?G1, ?F, ?List) is semidet.
%
% Functor Check Univ.
%
functor_check_univ(M:G1,F,List):-atom(M),member(M,[dbase,user]),!,functor_check_univ(G1,F,List),!.
functor_check_univ(G1,F,List):-must_det(compound(G1)),must_det(G1 \= _:_),must_det(G1 \= _/_),G1=..[F|List],!.

full_transform(Why,MH,MHH):-
 must_det(fully_expand_real(change(assert,Why),MH,MHH)),!,
 nop(sanity(on_f_debug(same_modules(MH,MHH)))).

same_modules(MH,MHH):- strip_module(MH,HM,_),strip_module(MHH,HHM,_),!,
   HM==HHM.

:- system:import(full_transform/3).


%% sub_term_eq( +H, ?HH) is semidet.
%
% Sub Term Using (==/2) (or =@=/2) ).
%
sub_term_eq(H,HH):-H==HH,!.
sub_term_eq(H,HH):-each_subterm(HH,ST),ST==H,!.


%% sub_term_v( +H, ?HH) is semidet.
%
% Sub Term V.
%
sub_term_v(H,HH):-H=@=HH,!.
sub_term_v(H,HH):-each_subterm(HH,ST),ST=@=H,!.

%% all_different_head_vals(+Clause) is det.
%
% Enforces All Different Head Vals.
%
all_different_head_vals(HB):- (\+ compound(HB) ; ground(HB)),!.
all_different_head_vals(HB):- 
  pfc_rule_hb(HB,H,B),
  term_slots(H,Slots),  
  (Slots==[]->
     all_different_head_vals(B);
    (lock_vars(Slots),all_different_head_vals_2(H,Slots),unlock_vars(Slots))),!.
  

all_different_head_vals_2(_H,[]):-!.
all_different_head_vals_2(H,[A,R|EST]):-get_assertion_head_arg(_,H,E1),E1 ==A,dif(A,E2),get_assertion_head_arg(_,H,E2),\+ occurs:contains_var(A,E2),all_different_vals(dif_matrix,[A,E2,R|EST]),!.
all_different_head_vals_2(_H,[A,B|C]):-all_different_vals(dif_matrix,[A,B|C]),!.
all_different_head_vals_2(HB,_):- \+ compound(HB),!.
all_different_head_vals_2(H,[A]):-get_assertion_head_arg(_,H,E1),E1 ==A, H=..[_|ARGS], all_different_vals(dif_matrix,ARGS),!.
all_different_head_vals_2(H,[A]):-get_assertion_head_arg(_,H,E1),E1 ==A,  get_assertion_head_arg(_,H,E2), A\==E2, \+ occurs:contains_var(A,E2), dif(A,E2),!.
all_different_head_vals_2(H,[A]):-get_assertion_head_arg(_,H,E1),E1\==A, compound(E1), occurs:contains_var(A,E1), all_different_head_vals_2(E1,[A]),!.
all_different_head_vals_2(_,_).
   	 

%% pfc_rule_hb( +Outcome, ?OutcomeO, ?AnteO) is semidet.
%
% Calculate PFC Rule Head+body.
%
pfc_rule_hb(Outcome,OutcomeO,Body):- nonvar(OutcomeO),!,pfc_rule_hb(Outcome,OutcomeN,Body),must(OutcomeO=OutcomeN).
pfc_rule_hb(Outcome,OutcomeO,BodyO):- nonvar(BodyO),!,pfc_rule_hb(Outcome,OutcomeO,BodyN),must(BodyN=BodyO).
pfc_rule_hb(Outcome,OutcomeO,AnteO):- 
  quietly((pfc_rule_hb_0(Outcome,OutcomeO,Ante),
  pfc_rule_hb_0(Ante,AnteO,_))).
% :-pfc_trace_nochilds(pfc_rule_hb/3).


%% pfc_rule_hb_0( +Rule, -Head, -Body) is nondet.
%
% Calculate PFC rule Head+Body  Primary Helper.
%


pfc_rule_hb_0(Outcome,OutcomeO,true):-is_ftVar(Outcome),!,OutcomeO=Outcome.
pfc_rule_hb_0(Outcome,OutcomeO,true):- \+compound(Outcome),!,OutcomeO=Outcome.
pfc_rule_hb_0((Outcome1,Outcome2),OutcomeO,AnteO):- pfc_rule_hb(Outcome1,Outcome1O,Ante1),pfc_rule_hb(Outcome2,Outcome2O,Ante2),
                   conjoin(Outcome1O,Outcome2O,OutcomeO),
                   conjoin(Ante1,Ante2,AnteO).
pfc_rule_hb_0((Ante1==>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(=>(Ante1,Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0((Ante1->Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0((Ante1*->Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
% pfc_rule_hb_0((Outcome/Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(rhs([Outcome]),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
% pfc_rule_hb_0(rhs([OutcomeH|OutcomeT]),OutcomeO,Ante2):- !, pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0({Outcome},OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0((Outcome<-Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(&(Ante1 , Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0((Ante1 , Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0((Outcome<==>Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0((Ante1<==>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(_::::Outcome,OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb_0(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(bt(Outcome,Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(pt(Ante1,Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(pk(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(nt(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(spft(Outcome,Ante1a,Ante1b),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true),pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0(que(Outcome,_),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
% pfc_rule_hb_0(pfc Default(Outcome),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), pfc_rule_hb(Outcome,OutcomeO,Ante2).
pfc_rule_hb_0((Outcome:-Ante),Outcome,Ante):-(nonvar(Outcome)-> ! ; true).
pfc_rule_hb_0(Outcome,Outcome,true).


%:- multifile(get_current_default_tbox/1).
%:- dynamic(get_current_default_tbox/1).
%get_current_default_tbox(baseKB).
:- if(current_predicate(get_current_default_tbox/1)).
:- redefine_system_predicate(get_current_default_tbox/1).
:- endif.
:- module_transparent(get_current_default_tbox/1).
get_current_default_tbox(TBox):- defaultAssertMt(ABox)->current_module(ABox)->clause(ABox:defaultTBoxMt(TBox),B),call(B),!.
get_current_default_tbox(baseKB).
:- sexport(get_current_default_tbox/1).


to_real_mt(_Why,abox,ABOX):- defaultAssertMt(ABOX),!.
to_real_mt(_Why,tbox,TBOX):- get_current_default_tbox(TBOX),!.
to_real_mt(_Why,BOX,BOX).

