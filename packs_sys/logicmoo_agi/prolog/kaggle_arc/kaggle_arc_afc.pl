/*   
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/
:- if( \+ current_predicate(set_fileAssertMt/1)).

:- set_prolog_flag(afc_shared_module,user).
%:- set_prolog_flag(afc_shared_module,baseKB).

control_arg_types(A,B):- once(control_arg_types1([],A,B)),A\==B,!.

control_arg_types1(_,A,B):- \+ compound(A),!,A=B.
control_arg_types1(_,A,B):- (current_predicate(check_args/2)->check_args(A,B)->A\=@=B),!.
control_arg_types1(Pre,A,B):- 
 compound_name_arguments(A,F,AA),
 length(AA,N),
 do_control_arg_types1(F/N,1,Pre,AA,BB),
 compound_name_arguments(B,F,BB).

do_control_arg_types1(_FofN,_ArgNp1,_Pre,[],[]):-!.
do_control_arg_types1(FofN,ArgN,Pre,[A|AA],[B|BB]):- 
  do_control_1arg_type(FofN,ArgN,Pre,A,B), 
  ArgNp1 is ArgN+1,
  do_control_arg_types1(FofN,ArgNp1,Pre,AA,BB).

do_control_1arg_type(_FN,_N,_Pre,A,B):- var(A),!,B=A.
do_control_1arg_type(F/_, N,_Pre,A,B):- arg_n_isa(F,N,ISA),into_type(ISA,A,B),!.
do_control_1arg_type(FofN,_,Pre,A,B):- control_arg_types1([FofN|Pre],A,B).


arg_n_isa(_F,_N,_ISA):- fail.
arg_n_isa(F,N,ISA):- clause_b(argIsa(F,N,ISA)).

save_afc_state:-
  %tell(afcState),
  forall((afcStateTerm(F/A),current_predicate(F/A)),listing(F/A)),
  %told.
  !.

afcDoAll(Goal):- forall(call(Goal),true).

afcStateTerm(F/A):- afcDatabaseTerm(F/A).
afcStateTerm(F/A):-
 member((F/A),[
     fcUndoMethod/2,
     fcAction/2,
     fcTmsMode/1,
     afcQueue/1,
     afcCurrentDb/1,
     afcHaltSignal/1,
     afcDebugging/0,
     afcSelect/1,
     afcSearch/1]).



:- if(( current_prolog_flag(xref,true) ;
   ('$current_source_module'(SM),'context_module'(M),'$current_typein_module'(CM),
     current_prolog_flag(afc_shared_module,BaseKB),asserta(BaseKB:'wusing_afc'(M,CM,SM,afc_rt))))).
:- endif.
:- if(current_prolog_flag(xref,true)).
%:- module(afc_rt,[]).
:- endif.
:- if((prolog_load_context(source,File),prolog_load_context(file,File))).
:- prolog_load_context(file,File),unload_file(File).
:- use_module(library(logicmoo_utils)).
:- endif.
%:- afc_lib:use_module(afc_lib).
:- if( \+  current_prolog_flag(xref,true)).
:- current_prolog_flag(afc_shared_module,BaseKB),
   must(retract(BaseKB:'wusing_afc'(M,CM,SM,afc_rt))),
   nop(wdmsg(BaseKB:'chusing_afc'(M,CM,SM,afc_rt))),
   (M==SM -> 
     (nop(maybe_ensure_abox(SM)),nop((M:ain(genlMt(SM,BaseKB)))));
     nop(wdmsg(BaseKB:'lusing_afc'(M,CM,SM,afc_rt)))),
   assert(BaseKB:'$using_afc'(M,CM,SM,afc_rt)),
   asserta(SM:'$does_use_afc_mod'(M,CM,SM,afc_rt)).
   %backtrace(200).

/*
:- multifile '$exported_op'/3. 
:- dynamic '$exported_op'/3. 
:- discontiguous '$exported_op'/3. 
'$exported_op'(_,_,_):- fail.
*/

:- multifile '$pldoc'/4. 
:- dynamic '$pldoc'/4. 
:- discontiguous '$pldoc'/4. 
'$pldoc'(_,_,_,_):- fail.

:- multifile '$autoload'/3. 
:- discontiguous '$autoload'/3.
:- dynamic '$autoload'/3.
'$autoload'(_,_,_):- fail.

:- system:use_module(library(make)).
%:- set_prolog_flag(retry_undefined, kb_shared).
%:- set_prolog_flag(afc_ready, true).
:- set_prolog_flag(expect_afc_file,unknown).
:- endif.

:- ifprolog:import(date:day_of_the_week/2).
:- ifprolog:import(date:day_of_the_year/2).


tilded_negation.

bagof_or_nil(T,G,L):- bagof(T,G,L)*->true;L=[].
setof_or_nil(T,G,L):- setof(T,G,L)*->true;L=[].

call_u(G):- afcCallSystem(G).
clause_u(H,B):- clause(H,B).

afc_ain(P):- arc_assert(P).
arc_assert(P:-True):- True==true,!,arc_assert_r(P).
arc_assert(P):-  % wdmsg(arc_assert(P)), 
  arc_assert_r(P).

arc_assert_r(P):- !, asserta_if_new(P).
arc_assert_r(P):-
  must(current_why_UU(UU)),nop(wdmsg(afcAdd(P, UU))),!, afcAdd(P, UU),asserta_if_new(P).

afc_retract(P):- wdmsg(afc_retract(P)),afcRetract(P).
afc_retractall(P):- wdmsg(afc_retractall(P)),afcRetractAll(P).

:- dynamic((~)/1).
~(_):- fail.
must_ex(X):-must(X).
quietly_ex(X):-call(X).

add(X):- afcAdd(X).


afc_test(call_u(X)):- nonvar(X),!,afcCallSystem(X),afcWhy(X).
afc_test(\+ call_u(X)):- nonvar(X),!, (call_u(X)-> (dmsg(warn(failed(afc_test(\+ call_u(X))))),afc_test_why(X)); afc_test_why(~(X))).
afc_test(X):- (afc_test_why(X) *-> true ; afc_test_why(~(X))).

:- thread_local t_l:shown_child/1.
:- thread_local t_l:shown_dep/2.

afc_info(X):- afc_info(X).
afc_info(X):-
 retractall(t_l:shown_child(_)),
 retractall(t_l:shown_dep(_,_)),
 ignore((
  forall(afc_test_why(X),true),
  forall(afc_child_info(X),true))).

afc_child_info(P):- 
  retractall(t_l:shown_child(_)),
  show_child_info(P),!,
  printLine.

show_child_info(P):- 
  afcChildren(P,L),
  show_child_info(P,L),!.

show_child_info(P,_):- t_l:shown_child(Q),P=@=Q,!.
show_child_info(P,_):- asserta(t_l:shown_child(P)),fail.
show_child_info(_,[]):-!.
show_child_info(P,L):- list_to_set(L,S),
  format("~N~nChildren for ",[]),  
  ansi_format([fg(green)],'~@',[pp(P)]),
  format(" :~n",[]),
  forall((member(D,S), \+ t_l:shown_dep(P,D)),(asserta(t_l:shown_dep(P,D)),ansi_format([fg(yellow)],'~N ~@. ~n',[pp(D)]))),
  maplist(show_child_info,S).

afc_why(X):- afc_test_why(X).

afc_test_why(X):- 
  afcCallSystem(X)*->afcTF1(X);afcTF1(X).

afc_literal(X):- afcLiteral(X).
afc_positive_literal(X):- afcPositiveLiteral(X).
afcAtom(X):- afcLiteral(X).
rem(X):- afcWithdraw(X).
rem2(X):- afcRemove(X).
remove(X):- afcBlast(X).

% :- afc_ain_in_thread.
% :- current_thread_pool(ain_pool)->true;thread_pool_create(ain_pool,20,[]).
:- multifile thread_pool:create_pool/1.
:- dynamic thread_pool:create_pool/1.
thread_pool:create_pool(ain_pool) :-
    thread_pool_create(ain_pool, 50, [detached(true)] ).

:- use_module(library(http/thread_httpd)).
:- use_module(library(thread_pool)).

is_ain_pool_empty:- thread_pool_property(ain_pool,running(N)),!,N==0.
is_ain_pool_empty.

show_ain_pool:- forall(thread_pool_property(ain_pool,PP),fmt(show_ain_pool(PP))).

await_ain_pool:- is_ain_pool_empty->true;(repeat, sleep(0.005), is_ain_pool_empty).

ain_in_thread(MAIN):- strip_module(MAIN,M,AIN), call_in_thread(M:afcAdd(AIN)).

call_in_thread(MG):- strip_module(MG,M,G), notrace((copy_term(M:G,GG,_),numbervars(GG,0,_,[attvar(skip),singletons(true)]),term_to_atom(GG,TN))), 
 call_in_thread(TN,M,G),
  dmsg_pretty(call_in_thread(TN,M,G)).

call_in_thread(TN,M,G):- thread_property(_,alias(TN)),!,dmsg_pretty(already_queued(M,G)).
call_in_thread(TN,M,G):- must(current_why(Why)), thread_create_in_pool(ain_pool,call_in_thread_code(M,G,Why,TN),_Id,[alias(TN)]).

call_in_thread_code(M,G,Why,TN):- 
 with_only_current_why(Why,
   catch(( M:G-> nop(dmsg_pretty(suceeded(exit,TN)));dmsg_pretty(failed(exit,TN))),E, dmsg_pretty(error(E-->TN)))).

:- call_in_thread(wdmsg(call_in_thread)).
% why_dmsg(Why,Msg):- with_current_why(Why,dmsg_pretty(Msg)).

%   File   : afc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure

afcVersion(3.0).

/*
afcFile('afcsyntax').	% operator declarations.
afcFile('afccore').	% core of Pfc.
afcFile('afcsupport').	% support maintenance
afcFile('afcdb').	% predicates to manipulate database.
afcFile('afcdebug').	% debugging aids (e.g. tracing).
afcFile('afcjust').	% predicates to manipulate justifications.
afcFile('afcwhy').	% interactive exploration of justifications.

afcLoad :- afcFile(F), ensure_loaded(F), fail.
afcLoad.
*/

%afcFcompile :- afcFile(F), compile(F), fail.
%afcFcompile.

%:- afcLoad.

%   File   : afccompile.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%   Purpose: compile system file for Pfc
/*
:- compile(afcsyntax).
:- compile(afccore).
:- compile(afcdb).
:- compile(afcjust).
:- compile(afcwhy).
:- compile(afcdebug).
*/

%   File   : afcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).


:- dynamic(afctmp:knows_will_table_as/2).

will_table_as(Stuff,As):- afctmp:knows_will_table_as(Stuff,As),!.
will_table_as(Stuff,As):- assert(afctmp:knows_will_table_as(Stuff,As)),
   must(react_tabling(Stuff,As)),!,fail.

react_tabling(Stuff,_):- dynamic(Stuff).

:- dynamic(lmconf:is_treated_like_afc_file/1).
:- dynamic(lmconf:is_afc_module/1).
if_afc_indicated :- source_location(F,_),(sub_string(F, _, _, _, '.afc')->true;lmconf:is_treated_like_afc_file(F)),!.
if_afc_indicated :- prolog_load_context(module, M),lmconf:is_afc_module(M),!.

skip_afc_term_expansion(Var):- var(Var),!.
skip_afc_term_expansion(begin_of_file).
skip_afc_term_expansion(end_of_file).

:- export(afc_term_expansion/2).
:- system:import(afc_term_expansion/2).
afc_term_expansion(I,O):- skip_afc_term_expansion(I),!, I=O.
afc_term_expansion((:- table Stuff as Type), [:- afcAdd(tabled_as(Stuff,Type)),(:- table Stuff as Type)]):- nonvar(Stuff), !, if_afc_indicated, \+ will_table_as(Stuff, Type).
afc_term_expansion((:- table Stuff ), [:- afcAdd(tabled_as(Stuff,incremental)),(:- table Stuff as incremental)]):- if_afc_indicated, \+ will_table_as(Stuff,incremental).
afc_term_expansion((:- _),_):- !, fail.
afc_term_expansion((P==>Q),(:- afcAdd((P==>Q)))).
%term_expansion((P==>Q),(:- afcAdd(('<-'(Q,P))))).  % speed-up attempt
afc_term_expansion(('<-'(P,Q)),(:- afcAdd(('<-'(P,Q))))).
afc_term_expansion((P<==>Q),(:- afcAdd((P<==>Q)))).
afc_term_expansion((RuleName :::: Rule),(:- afcAdd((RuleName :::: Rule)))).
afc_term_expansion((==>P),(:- afcAdd(P))).
afc_term_expansion(I,I):- I == end_of_file,!.
afc_term_expansion( P ,(:- afcAdd(P))):- if_afc_indicated.

%use_afc_term_expansion:- current_prolog_flag(afc_term_expansion,false),!,fail.
% maybe switch to prolog_load_context(file,...)?
%use_afc_term_expansion:- source_location(File,_), atom_concat(_,'.afc.pl',File).

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


%   File   : afccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.

:- use_module(library(lists)).


%==>(_).

% ==>(G):- arc_assert(G).

%:- multifile ('<-')/2.
%:- dynamic ('<-')/2.
%:- discontiguous(('<-')/2).
%'<-'(_,_).

%:- multifile ('==>')/2.
%:- dynamic ('==>')/2.
%:- discontiguous(('==>')/2).
%'==>'(_,_).

%:- multifile ('==>')/2.
%:- dynamic ('::::')/2.
%:- dynamic '<==>'/2.
:- dynamic '$pt$'/2.
:- dynamic '$nt$'/3.
:- dynamic '$bt$'/2.
:- dynamic fcUndoMethod/2.
:- dynamic fcAction/2.
:- dynamic fcTmsMode/1.
:- dynamic afcQueue/1.
:- dynamic afcCurrentDb/1.
:- dynamic afcHaltSignal/1.
:- dynamic afcDebugging/0.
:- dynamic afcSelect/1.
:- dynamic afcSearch/1.

:- thread_local(t_l:afcSearchTL/1).

:- dynamic '$spft$'/3.

% % % initialization of global assertons 

afcSetVal(Stuff):- 
   duplicate_term(Stuff,DStuff),
   functor(DStuff,_,N),
   setarg(N,DStuff,_),
   retractall(DStuff),
   assert(Stuff).

% %  afcDefault/1 initialized a global assertion.
% %   afcDefault(P,Q) - if there is any fact unifying with P, then do 
% %   nothing, else assert Q.

afcDefault(GeneralTerm,Default) :-
  clause(GeneralTerm,true) -> true ; assert(Default).

% %  fcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- afcDefault(fcTmsMode(_), fcTmsMode(cycles)).

% Pfc Search strategy. afcSearch(X) where X is one of {direct,depth,breadth}
:- afcDefault(afcSearch(_), afcSearch(direct)).


% 

% %  afcAdd/2 and afcPost/2 are the main ways to assert new clauses into the
% %  database and have forward reasoning done.

% %  afcAdd(P,S) asserts P into the dataBase with support from S.

afcAdd(P) :-  must(current_why_UU(UU)), with_current_why(P, afcAdd(P, UU)).

afcAdd((==>P),S) :- !, afcAdd(P,S).

afcAdd(P,S) :- 
  afcPost(P,S),
  afcRun,!.

%afcAdd(_,_).
afcAdd(P,S) :- afcWarn("afcAdd(~p,~p) failed",[P,S]).


% afcPost(+Ps,+S) tries to add a fact or set of fact to the database.  For
% each fact (or the singelton) afcPost1 is called. It always succeeds.

afcPost(List,S):- afcPost_rev(S,List).

afcPost_rev(S,Term) :-  
  is_list(Term) 
  -> maplist(afcPost_rev(S),Term)
  ; afcPost1(Term,S).


% afcPost1(+P,+S) tries to add a fact to the database, and, if it succeeded,
% adds an entry to the afc queue for subsequent forward chaining.
% It always succeeds.

afcPost1(Fact,S) :- control_arg_types(Fact,Fixed),!,afcPost1(Fixed,S).

afcPost1(P,S) :- 
  % %  db afcAddDbToHead(P,P2),
  % afcRemoveOldVersion(P),
  must(afcAddSupport(P,S)),
  (afcUnique(post, P)-> afcPost2(P,S) ; true).

afcPost2(P,S):- 
  must(assert(P)),
  must(afcTraceAdd(P,S)),
  !,
  must(afcEnqueue(P,S)),
  !.

%afcPost1(_,_).
%afcPost1(P,S) :-  
 %afcWarn("afcPost1: ~p\n (support: ~p) failed",[P,S]).

% %   afcAddDbToHead(+P,-NewP) is semidet.
% talkes a fact P or a conditioned fact
% (P:-C) and adds the Db context.
%

afcAddDbToHead(P,NewP) :-
  afcCallSystem(afcCurrentDb(Db)),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   true      -> NewP = (P :- Db)).

:- dynamic(afcCurrentDb/1).
afcCurrentDb(true).

% %  afcUnique(X) is det.
% 
% is true if there is no assertion X in the prolog db.
%

afcUnique(_Type,(Head:-Tail)) :- 
  !, 
  \+ clause(Head,Tail).
afcUnique(_Type, P) :-
  \+ clause(P,true).


% %  afcEnqueue(P,Q) is det.
% 
% Enqueu according to settings
%
afcSetSearch(Mode):- afcSetVal(afcSearch(Mode)).

afcGetSearch(Mode):- (t_l:afcSearchTL(ModeT)->true;afcSearch(ModeT))->Mode=ModeT.

afcEnqueue(P,S) :-
  afcGetSearch(Mode)
    -> (Mode=direct  -> afcFwd(P) ;
	Mode=thread   -> afcThreadFwd(P,S) ;
    Mode=depth   -> afcAsserta(afcQueue(P),S) ;
	Mode=breadth -> afcAssert(afcQueue(P),S) ;
	true         -> afcWarn("Unrecognized afcSearch mode: ~p", Mode))
     ; afcWarn("No afcSearch mode").



% %  afcRemoveOldVersion(+Rule) is det.
%
% if there is a rule of the form Identifier ::: Rule then delete it.

afcRemoveOldVersion((Identifier::::Body)) :-
  % this should never happen.
  (var(Identifier)
  ->
  afcWarn("variable used as an  rule name in ~p :::: ~p",
          [Identifier,Body]);
  afcRemoveOldVersion0(Identifier::::Body)).

  
afcRemoveOldVersion0((Identifier::::Body)) :-
  nonvar(Identifier),
  clause((Identifier::::OldBody),_),
  \+(Body=OldBody),
  afcWithdraw((Identifier::::OldBody)),
  !.
afcRemoveOldVersion0(_).


% %  with_fc_mode(+Mode,:Goal) is semidet.
% 
% Temporariliy changes to forward chaining propagation mode while running the Goal
%
with_fc_mode(Mode,Goal):- locally(t_l:afcSearchTL(Mode),Goal).


afcThreadFwd(S,P):- 
      with_only_current_why(S,
       % maybe keep `thread` mode?
        call_in_thread(with_fc_mode(thread, (afcFwd(P))))).

% in_fc_call(Goal):- with_fc_mode( thread, Goal).
%in_fc_call(Goal):- with_fc_mode( direct, Goal).
% in_fc_call(Goal):- !, afcCallSystem(Goal).




% 

% afcRun compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the afcQueue mechanism.

afcRun :-
  (\+ afcGetSearch(direct)),
  afcStep,
  afcRun.
afcRun.


% afcStep removes one entry from the afcQueue and reasons from it.


afcStep :-  
  % if afcHaltSignal(Msg) is true, reset it and fail, thereby stopping inferencing.
  afcRetract(afcHaltSignal(Msg)),
  afcTraceMsg(removing(afcHaltSignal(Msg))),
  !, 
  fail.

afcStep :-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P),
  afcdo(afcFwd(P)),
  !.

get_next_fact(P) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P) :- 
  afcRetract(afcQueue(P)),
  afcRemoveSupportsQuietly(afcQueue(P)),
  !.
remove_selection(P) :-
  brake(afcPrintf("afc:get_next_fact - selected fact not on Queue: ~p",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.  
% It tries the user defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact(P) :- 
  afcSelect(P),
  !.  
select_next_fact(P) :- 
  defaultafcSelect(P),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultafcSelect(P) :- afcCallSystem(afcQueue(P)),!.

% afcHalt stops the forward chaining.
afcHalt :-  afcHalt("unknown_reason",[]).

afcHalt(Format) :- afcHalt(Format,[]).

afcHalt(Format,Args) :- 
  format(string(Msg),Format,Args),
  (afcHaltSignal(Msg) -> 
       afcWarn("afcHalt finds afcHaltSignal(~w) already set",[Msg])
     ; assert(afcHaltSignal(Msg))).


% % 
% % 
% %  predicates for manipulating triggers
% % 

afcAddTrigger('$pt$'(Trigger,Body),Support) :-
  !,
  afcTraceMsg('      Adding positive trigger(+) ~p~n',
		['$pt$'(Trigger,Body)]),
  afcAssert('$pt$'(Trigger,Body),Support),
  copy_term('$pt$'(Trigger,Body),Tcopy),
  afc_call(Trigger),
  with_current_why(Trigger,fcEvalLHS(Body,(Trigger,Tcopy))),
  fail.


afcAddTrigger('$nt$'(Trigger,Test,Body),Support) :-
  !,
  afcTraceMsg('      Adding negative trigger(-): ~p~n       test: ~p~n       body: ~p~n',
		[Trigger,Test,Body]),
  copy_term(Trigger,TriggerCopy),
  afcAssert('$nt$'(TriggerCopy,Test,Body),Support),
  \+ afc_call(Test),
  with_current_why(\+ afc_call(Test), fcEvalLHS(Body,((\+Trigger),'$nt$'(TriggerCopy,Test,Body)))).

afcAddTrigger('$bt$'(Trigger,Body),Support) :-
  !,
  afcAssert('$bt$'(Trigger,Body),Support),
  afcBtPtCombine(Trigger,Body,Support).

afcAddTrigger(X,_Support) :-
  afcWarn("Unrecognized trigger(?) to afcAddtrigger: ~p",[X]).


afcBtPtCombine(Head,Body,Support) :- 
  % %  a backward trigger(?) ('$bt$') was just added with head and Body and support Support
  % %  find any '$pt$'(s) with unifying heads and add the instantied '$bt$' body.
  afcGetTriggerQuick('$pt$'(Head,_PtBody)),
  fcEvalLHS(Body,Support),
  fail.
afcBtPtCombine(_,_,_) :- !.

afcGetTriggerQuick(Trigger) :-  clause(Trigger,true)*->true;afc_call(Trigger).
afcCallSystem(Trigger) :-  afc_call(Trigger).

% % 
% % 
% %  predicates for manipulating action traces.
% % 

afcAddActionTrace(Action,Support) :- 
  % adds an action trace and it's support.
  afcAddSupport(afcAction(Action),Support).

afcRemActionTrace(afcAction(A)) :-
  fcUndoMethod(A,UndoMethod),
  afcCallSystem(UndoMethod),
  !.


% % 
% %  predicates to remove afc facts, triggers, action traces, and queue items
% %  from the database.
% % 

afcRetract(X) :- 
  % %  retract an arbitrary thing.
  afcType(X,Type),
  afcRetractType(Type,X),
  !.                       

afcRetractType(fact(_),X) :-   
  % %  db 
  afcAddDbToHead(X,X2)-> retract(X2) ; retract(X).

afcRetractType(rule(_),X) :- 
  % %  db  
  afcAddDbToHead(X,X2) ->  retract(X2) ; retract(X).

afcRetractType(trigger(Pos),X) :- 
  retract(X)
    -> unFc(X)
     ; afcWarn("Trigger(~p) not found to retract: ~p",[Pos,X]).

afcRetractType(action,X) :- afcRemActionTrace(X).
  

% %  afcAddType1(X) adds item X to some database

afcAddType1(X) :-
  % what type of X do we have?
  afcType(X,Type),
  afcAddDbToHead(X,X2),
  % call the appropriate predicate.
  afcAddType(Type,X2).

afcAddType(fact(Type),X) :- 
  afcUnique(fact(Type),X), 
  assert(X),!.
afcAddType(rule(Type),X) :- 
  afcUnique(rule(Type),X), 
  assert(X),!.
afcAddType(trigger(Pos),X) :- 
  afcUnique(trigger(Pos),X) -> assert(X) ; 
   (afcWarn(not_afcUnique(X)),assert(X)).
   
afcAddType(action,_Action) :- !.


 

% afcWithdraw/1  withdraws any "direct" support for P.
% If a list, iterates down the list
afcWithdraw(P) :- is_list(P),!,maplist(afcWithdraw,P).
afcWithdraw(P) :- matches_why_UU(UU), afcWithdraw(P,UU).
% %  afcWithdraw(P,S) removes support S from P and checks to see if P is still supported.
% %  If it is not, then the fact is retractred from the database and any support
% %  relationships it participated in removed.
afcWithdraw(P,S) :-
  % afcDebug(afcPrintf("removing support ~p from ~p",[S,P])),
  afcGetSupport(P,S),
  matterialize_support_term(S,Sup),
  afcTraceMsg('    Withdrawing direct support: ~p   \n   From: ~p~n',[Sup,P]),
   (afcRemOneSupportOrQuietlyFail(P,S)
      -> afcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P]) 
      ; afcWarn("afcRemOneSupport/2 Could not find support ~p thus\n    Did not afcRemOneSupport: ~p",
                 [Sup,P])),
   removeIfUnsupported(P).

afcWithdraw(P,S) :-
  matterialize_support_term(S,Sup),
  afcTraceMsg('    No support matching: ~p   \n   For: ~p~n',[Sup,P]),!,
  removeIfUnsupported(P).

% afcRetractAll/1  withdraws any "direct" and "indirect" support for P.
% If a list, iterates down the list
afcRetractAll(P) :- is_list(P),!,maplist(afcRetractAll,P).
afcRetractAll(P) :- matches_why_UU(UU), afcRetractAll(P,UU).

% %  afcRetractAll(P,S) removes support S from P and checks to see if P is still supported.
% %  If it is not, then the fact is retreactred from the database and any support
% %  relationships it participated in removed.

afcRetractAll(Fact,S) :- control_arg_types(Fact,Fixed),!,afcRetractAll(Fixed,S).
afcRetractAll(P,S) :-
  \+ \+ afcWithdraw(P,S),
  fail.
afcRetractAll(P,S) :-  
  afcGetSupport(P,(P2,_)),
  afcType(P2,fact(_)),
  afcSupportedBy(P2,S,_How),
   afcRetractAll(P2),   
    \+ fcSupported(P),!,
    fcUndo(P).
afcRetractAll(P,S) :-
  afcGetSupport( P,(_,T)),
    afcGetSupport(T,(P2,_)),
    afcSupportedBy(P2,S,_How),
    afcType(P2,fact(_)),
   afcRetractAll(P2),
    \+ fcSupported(P),!,
    fcUndo(P).
afcRetractAll(P,S) :-
  fcSupported(P),
  afcGetSupport(P,(P2,_)),
  afcSupportedBy(P2,S,_How),
  afcType(P2,rule(_)),
   afcRetractAll(P2),
    \+ fcSupported(P),
    fcUndo(P),!.
afcRetractAll(P,_S0) :- 
  removeIfUnsupported(P),
  fail.
afcRetractAll(_,_).


afcSupportedBy(P,S,How):- 
   afcGetSupport(P,(F,T)),
   (afcSupportedBy(F,S,_)->How=F;
   afcSupportedBy(T,S,How)).
   
afcSupportedBy(P,S,How):-P=S,How=S.  

afcRetractAll_v2(P,S0) :-
  \+ \+ afcWithdraw(P,S0),
  afcGetSupport(P,(S,RemoveIfTrigger)),
  % afcDebug(afcPrintf("removing support ~p from ~p",[S,P])),
  matterialize_support_term((S,RemoveIfTrigger),Sup),
  afcTraceMsg('    Removing support: ~p   \n   From: ~p~n',[Sup,P]),
  (afcRemOneSupportOrQuietlyFail(P,(S,RemoveIfTrigger))
     -> afcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P]) 
     ; (afcWarn("afcRemOneSupport/2 Could not find support ~p thus\n    Did not yet afcRetractAll_v2: ~p",
                [Sup,P]))),
  afcRetractAll_v2(S, S0),
  fail.

afcRetractAll_v2(P,_):- removeIfUnsupported(P).

% afcRemove/1 is the user's interface - it withdraws user support for P.
%
% afcRemove is like afcRetractAll, but if P is still in the DB after removing the
% user's support, it is retracted by more forceful means (e.g. afcBlast).
%
afcRemove(Fact) :- control_arg_types(Fact,Fixed),!,afcRemove(Fixed).
afcRemove(P) :-
  afcRetractAll(P),
  afc_call(P)
     -> afcBlast(P) 
      ; true.


% %  afcBlast(+F) is det
%
% retracts fact F from the DB and removes any dependent facts 
%

afcBlast(F) :- 
  afcRemoveSupports(F),
  fcUndo(F).


% removes any remaining supports for fact F, complaining as it goes.

afcRemoveSupports(F) :- 
  afcRemOneSupport(F,S),
  afcWarn("~p was still supported by ~p (but no longer)",[F,S]),
  fail.
afcRemoveSupports(_).

afcRemoveSupportsQuietly(F) :- 
  afcRemOneSupport(F,_),
  fail.
afcRemoveSupportsQuietly(_).

% fcUndo(X) undoes X.


fcUndo(afcAction(A)) :-  
  % undo an action by finding a method and successfully executing it.
  !,
  afcRemActionTrace(afcAction(A)).

fcUndo('$pt$'(/*Key,*/Head,Body)) :-  
  % undo a positive trigger(+).
  %
  !,
  (retract('$pt$'(/*Key,*/Head,Body))
    -> unFc('$pt$'(Head,Body))
     ; afcWarn("Trigger not found to retract: ~p",['$pt$'(Head,Body)])).

fcUndo('$nt$'(Head,Condition,Body)) :-  
  % undo a negative trigger(-).
  !,
  (retract('$nt$'(Head,Condition,Body))
    -> unFc('$nt$'(Head,Condition,Body))
     ; afcWarn("Trigger not found to retract: ~p",['$nt$'(Head,Condition,Body)])).

fcUndo(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  retract(Fact),
  afcTraceRem(Fact),
  unFc(Fact).
  

% %  unFc(P) is det.
%
% unFc(P) "un-forward-chains" from fact f.  That is, fact F has just
% been removed from the database, so remove all dependant relations it
% participates in and check the things that they support to see if they
% should stayu in the database or should also be removed.


unFc(F) :- 
  afcRetractDependantRelations(F),
  unFc1(F).

unFc1(F) :-
  afcUnFcCheckTriggers(F),
  % is this really the right place for afcRun<?
  afcRun.


afcUnFcCheckTriggers(F) :-
  afcType(F,fact(_)),
  copy_term(F,Fcopy),
  afcCallSystem('$nt$'(Fcopy,Condition,Action)),
  (\+ afcCallSystem(Condition)),
  fcEvalLHS(Action,((\+F),'$nt$'(F,Condition,Action))),
  fail.
afcUnFcCheckTriggers(_).

afcRetractDependantRelations(Fact) :-
  afcType(Fact,Type),
  (Type=trigger(_Pos) -> afcRemOneSupport(P,(_,Fact))
                ; afcRemOneSupportOrQuietlyFail(P,(Fact,_))),
  removeIfUnsupported(P),
  fail.
afcRetractDependantRelations(_).



% %  removeIfUnsupported(+P) checks to see if P is supported and removes
% %  it from the DB if it is not.

removeIfUnsupported(P) :- 
   fcSupported(P) -> afcTraceMsg(fcSupported(P)) ;  fcUndo(P).


% %  fcSupported(+P) succeeds if P is "supported". What this means
% %  depends on the TMS mode selected.

fcSupported(P) :- 
  must(fcTmsMode(Mode)),
  supported(Mode,P).

supported(local,P) :- !, afcGetSupport(P,_).
supported(cycles,P) :-  !, wellFounded(P).
supported(_,_P) :- true.


% % 
% %  a fact is well founded if it is supported by the user
% %  or by a set of facts and a rules, all of which are well founded.
% % 

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

% %  wflist(L) simply maps wf over the list.

wflist([],_).
wflist([X|Rest],L) :-
  wf(X,L),
  wflist(Rest,L).



% supports(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [user].

supports(F,[Fact|MoreFacts]) :-
  afcGetSupport(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(U,[]) :- axiomatic_supporter(U),!.

triggerSupports(Trigger,AllSupport):- 
  triggerSupports1(Trigger,AllSupport)*->true;triggerSupports2(Trigger,AllSupport).

triggerSupports1(Trigger,AllSupport) :-
  afcGetSupport(Trigger,(Fact,AnotherTrigger)),
  (triggerSupports(AnotherTrigger,MoreFacts)*->true;MoreFacts=[AnotherTrigger]),
  [Fact|MoreFacts] = AllSupport.

triggerSupports2(Trigger,AllSupport) :- fail,
  afcGetSupport(Trigger,(Fact,AnotherTrigger)),
  (triggerSupports(AnotherTrigger,MoreFacts)*->true;MoreFacts=[AnotherTrigger]),
  [Fact|MoreFacts] = AllSupport.

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
  afcGetSupport(Trigger,(Fact,AnotherTrigger))*->
  (triggerSupports(Fact,AnotherTrigger,MoreFacts),OUT=[Fact|MoreFacts]);
  triggerSupports1(FactIn,Trigger,OUT).

triggerSupports1(_,X,[X]):- may_cheat.
may_cheat:- fail.



% % 
% % 
% %  afcFwd(X) forward chains from a fact or a list of facts X.
% % 
afcFwd(Fact) :- control_arg_types(Fact,Fixed),!,afcFwd(Fixed).
afcFwd(Fact) :- is_list(List)->maplist(afcFwd1,List);afcFwd1(Fact).

% fc1(+P) forward chains for a single fact.


afcFwd1(Fact) :-
  (fc_rule_check(Fact)*->true;true),
  copy_term(Fact,F),
  % check positive triggers
  ignore(fcpt(Fact,F)),
  % check negative triggers
  ignore(fcnt(Fact,F)).


% % 
% %  fc_rule_check(P) does some special, built in forward chaining if P is 
% %  a rule.
% %  

fc_rule_check((Name::::P==>Q)) :- 
  !,  
  processRule(P,Q,(Name::::P==>Q)).
fc_rule_check((Name::::P<==>Q)) :- 
  !, 
  processRule(P,Q,((Name::::P<==>Q))), 
  processRule(Q,P,((Name::::P<==>Q))).



fc_rule_check((P==>Q)) :-  
  !,  
  processRule(P,Q,(P==>Q)).
fc_rule_check((P<==>Q)) :- 
  !, 
  processRule(P,Q,(P<==>Q)), 
  processRule(Q,P,(P<==>Q)).

fc_rule_check(('<-'(P,Q))) :-
  !,
  afcDefineBcRule(P,Q,('<-'(P,Q))).

fc_rule_check(_).


fcpt(Fact,F) :- 
  afcGetTriggerQuick('$pt$'(F,Body)),
  afcTraceMsg('      Found positive trigger(+): ~p~n       body: ~p~n',
		[F,Body]),
  afcGetSupport('$pt$'(F,Body),Support), %wdmsg(afcGetSupport('$pt$'(F,Body),Support)),
  with_current_why(Support,with_current_why(Fact,fcEvalLHS(Body,(Fact,'$pt$'(F,Body))))),
  fail.

%fcpt(Fact,F) :- 
%  afcGetTriggerQuick('$pt$'(presently(F),Body)),
%  fcEvalLHS(Body,(presently(Fact),'$pt$'(presently(F),Body))),
%  fail.

fcpt(_,_).

fcnt(_Fact,F) :-
  afc_spft(X,_,'$nt$'(F,Condition,Body)),
  afcCallSystem(Condition),
  afcRem_S(X,(_,'$nt$'(F,Condition,Body))),
  fail.
fcnt(_,_).


% %  afcRem_S(P,S) removes support S from P and checks to see if P is still supported.
% %  If it is not, then the fact is retreactred from the database and any support
% %  relationships it participated in removed.
afcRem_S(P,S) :-
  % afcDebug(afcPrintf("removing support ~p from ~p",[S,P])),
  afcTraceMsg('    Removing support: ~p from ~p~n',[S,P]),
  afcRemOneSupport(P,S)
     -> removeIfUnsupported(P)
      ; afcWarn("afcRem_S/2 Could not find support ~p to remove from fact ~p",
                [S,P]).



% %  afcDefineBcRule(+Head,+Body,+ParentRule) 
%
% defines a backward
% chaining rule and adds the corresponding '$bt$' triggers to the database.
%

afcDefineBcRule(Head,_Body,ParentRule) :-
  (\+ afcLiteral(Head)),
  afcWarn("Malformed backward chaining rule.  ~p not atomic literal.",[Head]),
  afcError("caused by rule: ~p",[ParentRule]),
  !,
  fail.

afcDefineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs(Head,Rhs),
  current_why_U(USER), % @TODO REVIEW _U
  afcForEach(afc_nf(Body,Lhs),
          (buildTrigger(Lhs,rhs(Rhs),Trigger),
           afcAdd('$bt$'(Head,Trigger),(ParentRuleCopy,USER)))).
get_bc_clause(Head,(HeadC:- BodyC)):- get_bc_clause(Head,HeadC,BodyC).

get_bc_clause(HeadIn, ~HeadC, Body):- compound(HeadIn), HeadIn = ~Head,!,
     Body = ( awc, 
            ( nonvar(HeadC)-> (HeadC = Head,!) ; (HeadC = Head)), 
              afc_bc_and_with_afc(~Head)).
get_bc_clause(Head, Head, Body):-  % % :- is_ftNonvar(Head).
     Body = ( awc, !, afc_bc_and_with_afc(Head)).

:- thread_initialization(nb_setval('$afc_current_choice',[])).

push_current_choice:- current_prolog_flag(afc_support_cut,false),!.
push_current_choice:- prolog_current_choice(CP),push_current_choice(CP),!.
push_current_choice(CP):- nb_current('$afc_current_choice',Was)->b_setval('$afc_current_choice',[CP|Was]);b_setval('$afc_current_choice',[CP]).
 
cut_c:- current_prolog_flag(afc_support_cut,false),!.
cut_c:- must(nb_current('$afc_current_choice',[CP|_WAS])),prolog_cut_to(CP).


% % 
% % 
% %  eval something on the LHS of a rule.
% % 

 
fcEvalLHS((Test->Body),Support) :-  
  !, 
  afcDoAll(afcCallSystem(Test) -> (fcEvalLHS(Body,Support))),
  !.

fcEvalLHS((Test*->Body),Support) :-  
  !, 
  afcDoAll(afcCallSystem(Test) *-> (fcEvalLHS(Body,Support))).

fcEvalLHS(rhs(X),Support) :-
  !,
  afcDoAll(afc_eval_rhs(X,Support)),
  !.

fcEvalLHS(X,Support) :-
  afcType(X,trigger(_Pos)),
  !,
  afcAddTrigger(X,Support),
  !.

%fcEvalLHS(snip(X),Support) :- 
%  snip(Support),
%  fcEvalLHS(X,Support).

fcEvalLHS(X,_) :-
  afcWarn("Unrecognized item found in trigger body, namely ~p.",[X]).


% % 
% %  eval something on the RHS of a rule.
% % 

afc_eval_rhs([],_) :- !.
afc_eval_rhs([Head|Tail],Support) :- 
  afc_eval_rhs1(Head,Support),
  afc_eval_rhs(Tail,Support).


afc_eval_rhs1(Fact,S) :- control_arg_types(Fact,Fixed),!,afc_eval_rhs1(Fixed,S).

afc_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 fcEvalAction(Action,Support).

afc_eval_rhs1(P,_Support) :-
 % predicate to remove.
 afcNegatedLiteral(P),
 !,
 afcWithdraw(P).

afc_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 afc_eval_rhs([X|Xrest],Support).

afc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 (must(afcPost1(Assertion,Support))*->true ; afcWarn("Malformed rhs of a rule: ~p",[Assertion])).


% % 
% %  evaluate an action found on the rhs of a rule.
% % 

fcEvalAction(Action,Support) :-
  afcCallSystem(Action), 
  (undoable(Action) 
     -> afcAddActionTrace(Action,Support) 
      ; true).


% % 
% %  
% % 

trigger_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  afc_call(Trigger),
%  fcEvalLHS(Body,(presently(Trigger),'$pt$'(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  afc_call(Trigger),
  with_current_why(Trigger,fcEvalLHS(Body,(Trigger,'$pt$'(TriggerCopy,Body)))),
  fail.


% %  afc_call(F) is nondet.
%
% afc_call(F) is true iff F is a fact available for forward chaining.
% Note that this has the side effect of catching unsupported facts and
% assigning them support from God.
%

%afc_call(F) :- var(F), !, afc_call(F).
afc_call(P) :- var(P), !, afcFact(P).
afc_call(P) :- \+ callable(P), throw(afc_call(P)).
afc_call((!)) :-!,cut_c.
afc_call(true):-!.
afc_call((A->B;C)) :-!, afc_call(A)->afc_call(B);afc_call(C).
afc_call((A*->B;C)) :-!, afc_call(A)*->afc_call(B);afc_call(C).
afc_call((A->B)) :-!, afc_call(A)->afc_call(B).
afc_call((A*->B)) :-!, afc_call(A)*->afc_call(B).
afc_call((A,B)) :-!, afc_call(A),afc_call(B).
afc_call((A;B)) :-!, afc_call(A);afc_call(B).
afc_call(\+ (A)) :-!, \+ afc_call(A).
afc_call((A is B)) :-!, A is B.
afc_call(clause(A,B)) :-!, clause(A,B).
afc_call(clause(A,B,Ref)) :-!, clause(A,B,Ref).
% we really need to check for system predicates as well.
% this is probably not advisable due to extreme inefficiency.
afc_call(P) :-
  % trigger(?) any bc rules.
  '$bt$'(P,Trigger),
  afcGetSupport('$bt$'(P,Trigger),S),
  % @TODO REVIEW _U
  fcEvalLHS(Trigger,S),
  fail.
%afc_call(P) :- var(P), !, afcFact(P).
afc_call(P) :- predicate_property(P,imported_from(system)), !, call(P).
afc_call(P) :- predicate_property(P,built_in), !, call(P).
afc_call(P) :- \+ predicate_property(P,_), functor(P,F,A), dynamic(F/A), !, call(P).
afc_call(P) :- \+ predicate_property(P,number_of_clauses(_)), !, call(P).
afc_call(P) :- 
  setup_call_cleanup(
    nb_current('$afc_current_choice',Was),
    (prolog_current_choice(CP), push_current_choice(CP), clause(P,Condition), afc_call(Condition)),
    nb_setval('$afc_current_choice',Was)).
     
/*
afc_call(P) :- 
  clause(P,true)*-> true ; (clause(P,Condition), Condition\==true,
     afc_call(Condition)).
*/

% an action is undoable if there exists a method for undoing it.
undoable(A) :- fcUndoMethod(A,_).

afc_cache_bc(P) :-
  % trigger(?) any bc rules.
  forall('$bt$'(P,Trigger),
  forall(afcGetSupport('$bt$'(P,Trigger),S),
  % @TODO REVIEW _U
  fcEvalLHS(Trigger,S))).


% % 
% % 
% %  defining fc rules 
% % 

% %  afc_nf(+In,-Out) maps the LHR of a afc rule In to one normal form 
% %  Out.  It also does certain optimizations.  Backtracking into this
% %  predicate will produce additional clauses.


afc_nf(LHS,List) :-
  afc_nf1(LHS,List2),
  afc_nf_negations(List2,List).


% %  afc_nf1(+In,-Out) maps the LHR of a afc rule In to one normal form
% %  Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

afc_nf1(P,[P]) :- var(P), !.

% these next two rules are here for upward compatibility and will go 
% away eventually when the P/Condition form is no longer used anywhere.

afc_nf1(P/Cond,[( \+P )/Cond]) :- afcNegatedLiteral(P), !.

afc_nf1(P/Cond,[P/Cond]) :-  afcLiteral(P), !.

% %  handle a negated form

afc_nf1(NegTerm,NF) :-
  afc_unnegate(NegTerm,Term),
  !,
  afc_nf1_negation(Term,NF).

% %  disjunction.

afc_nf1((P;Q),NF) :- 
  !,
  (afc_nf1(P,NF) ;   afc_nf1(Q,NF)).


% %  conjunction.

afc_nf1((P,Q),NF) :-
  !,
  afc_nf1(P,NF1),
  afc_nf1(Q,NF2),
  append(NF1,NF2,NF).

% %  handle a random atom.

afc_nf1(P,[P]) :-
  afcLiteral(P), 
  !.

% % % shouln't we have something to catch the rest as errors?
afc_nf1(Term,[Term]) :-
  afcWarn("afc_nf doesn't know how to normalize ~p (accepting though)",[Term]).


% %  afc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
afc_nf1_negation((P/Cond),[(\+(P))/Cond]) :- !.

afc_nf1_negation((P;Q),NF) :-
  !,
  afc_nf1_negation(P,NFp),
  afc_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

afc_nf1_negation((P,Q),NF) :- 
  % this code is not correct! twf.
  !,
  afc_nf1_negation(P,NF) 
  ;
  (afc_nf1(P,Pnf),
   afc_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

afc_nf1_negation(P,[\+P]).


% %  afc_nf_negations(List2,List) sweeps through List2 to produce List,
% %  changing ~{...} to {\+...}
% % % ? is this still needed? twf 3/16/90

afc_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

afc_nf_negations([],[]).

afc_nf_negations([H1|T1],[H2|T2]) :-
  afc_nf_negation(H1,H2),
  afc_nf_negations(T1,T2).

% Maybe \+ tilded_negation ?

afc_nf_negation(Form,{\+ X}) :-
  nonvar(Form),
  Form=(~({X})),
  !.
afc_nf_negation(Form,{\+ X}) :- tilded_negation, 
  nonvar(Form),
  Form=(-({X})),
  !.
afc_nf_negation(Form,{\+ X}) :- tilded_negation, 
  nonvar(Form),
  Form=( \+ ({X})),
  !.
afc_nf_negation(X,X).



     % %  constrain_meta(+Lhs, ?Guard) is semidet.
     %
     % Creates a somewhat sane Guard.
     %
     % To turn this feature off...
     % ?- set_prolog_flag(constrain_meta,false).  
     %
     %
     constrain_meta(_,_):- current_prolog_flag(constrain_meta,false),!,fail.
     % FACT
     constrain_meta(P,afc_positive_fact(P)):- is_ftVar(P),!.
     % NEG chaining
     constrain_meta(~ P, CP):- !,  constrain_meta(P,CP).
     constrain_meta(\+ P, CP):- !,  constrain_meta(P,CP).
     % FWD chaining
     constrain_meta((_==>Q),nonvar(Q)):- !, is_ftVar(Q).
     % EQV chaining
     constrain_meta((P<==>Q),(nonvar(Q);nonvar(P))):- (is_ftVar(Q);is_ftVar(P)),!.
     % BWD chaining
     constrain_meta((Q <- _),afc_literal(Q)):- is_ftVar(Q),!.
     constrain_meta((Q <- _),CQ):- !, constrain_meta(Q,CQ).
     % CWC chaining
     constrain_meta((Q :- _),afc_literal(Q)):- is_ftVar(Q),!.
     constrain_meta((Q :- _),CQ):- !, constrain_meta(Q,CQ).





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



% % 
% %  buildRhs(+Conjunction,-Rhs)
% % 

buildRhs(X,[X]) :- 
  var(X),
  !.

buildRhs((A,B),[A2|Rest]) :- 
  !, 
  afcCompileRhsTerm(A,A2),
  buildRhs(B,Rest).

buildRhs(X,[X2]) :-
   afcCompileRhsTerm(X,X2).

afcCompileRhsTerm((P/C),((P:-C))) :- !.

afcCompileRhsTerm(P,P).


% %  afc_unnegate(N,P) is true if N is a negated term and P is the term
% %  with the negation operator stripped.

afc_unnegate(P,_):- var(P),!,fail.
afc_unnegate((~P),P):-  \+ tilded_negation.
afc_unnegate((-P),P).
afc_unnegate((\+(P)),P).

afcNegatedLiteral(P) :- 
  callable(P),
  afc_unnegate(P,Q),
  afcPositiveLiteral(Q).

afcLiteral(X) :- afcNegatedLiteral(X).
afcLiteral(X) :- afcPositiveLiteral(X).

afcPositiveLiteral(X) :-  
  callable(X),
  functor(X,F,_), 
  \+ afcConnective(F).

afcConnective(';').
afcConnective(',').
afcConnective('/').
afcConnective('|').
afcConnective(('==>')).
afcConnective(('<-')).
afcConnective('<==>').

afcConnective('-').
afcConnective('~'):- \+ tilded_negation.
afcConnective(( \+ )).

is_implicitly_prolog(Callable):- \+ callable(Callable),!, fail.
is_implicitly_prolog(_ is _).

processRule(Lhs,Rhs,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs(Rhs,Rhs2),
  current_why_U(USER), % @TODO REVIEW _U
  afcForEach(afc_nf(Lhs,Lhs2), 
          buildRule(Lhs2,rhs(Rhs2),(ParentRuleCopy,USER))).

buildRule(Lhs,Rhs,Support) :-
  buildTrigger(Lhs,Rhs,Trigger),
  fcEvalLHS(Trigger,Support).

buildTrigger([],Consequent,Consequent).

buildTrigger([Test|Triggers],Consequent,(Test *-> X)) :- is_implicitly_prolog(Test),
  !,
  buildTrigger(Triggers,Consequent,X).

buildTrigger([V|Triggers],Consequent,'$pt$'(V,X)) :-
  var(V),
  !, 
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1/Test)|Triggers],Consequent,'$nt$'(T2,Test2,X)) :-
  afc_unnegate(T1,T2),
  !, 
  buildNtTest(T2,Test,Test2),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1)|Triggers],Consequent,'$nt$'(T2,Test,X)) :-
  afc_unnegate(T1,T2),
  !,
  buildNtTest(T2,true,Test),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([{Test}|Triggers],Consequent,(Test *-> X)) :-
  !,
  buildTrigger(Triggers,Consequent,X).

buildTrigger([T/Test|Triggers],Consequent,'$pt$'(T,X)) :-
  !, 
  buildTest(Test,Test2),
  buildTrigger([{Test2}|Triggers],Consequent,X).


%buildTrigger([snip|Triggers],Consequent,snip(X)) :-
%  !,
%  buildTrigger(Triggers,Consequent,X).

buildTrigger([T|Triggers],Consequent,'$pt$'(T,X)) :-
  !, 
  buildTrigger(Triggers,Consequent,X).

% % 
% %  buildNtTest(+,+,-).
% % 
% %  builds the test used in a negative trigger(-) ('$nt$'/3).  This test is a
% %  conjunction of the check than no matching facts are in the db and any
% %  additional test specified in the rule attached to this ~ term.
% % 
     %  tilded_negation.
buildNtTest(T,Testin,Testout) :-
  buildTest(Testin,Testmid),
  afcConjoin((afc_call(T)),Testmid,Testout).

  
% this just strips away any currly brackets.

buildTest({Test},Test) :- !.
buildTest(Test,Test).

% % 


% %  afcType(+VALUE1, ?Type) is semidet.
%
% PFC Database Type.
%
%  simple typeing for Pfc objects
%


afcType(Var,Type):- var(Var),!, Type=fact(_FT).
afcType(_:X,Type):- !, afcType(X,Type).
afcType(~_,Type):- !, Type=fact(_FT).
afcType(('==>'(_,_)),Type):- !, Type=rule(fwd).
afcType( '==>'(X),Type):- !, afcType(X,Type), afcWarn(afcType( '==>'(X), Type)).
afcType(('<==>'(_,_)),Type):- !, Type=rule(<==>).
afcType(('<-'(_,_)),Type):- !, Type=rule(bwc).
afcType((':-'(_,_)),Type):- !, Type=rule(cwc).
afcType('$pt$'(_,_,_),Type):- !, Type=trigger(+).
afcType('$pt$'(_,_),Type):- !, Type=trigger(+).
afcType('$nt$'(_,_,_),Type):- !,  Type=trigger(-).
afcType('$bt$'(_,_),Type):- !,  Type=trigger(?).
afcType(afcAction(_),Type):- !, Type=action.
afcType((('::::'(_,X))),Type):- !, afcType(X,Type).
afcType(_,fact(_FT)):-
  %  if it''s not one of the above, it must_ex be a fact!
  !.

afcAssert(P,Support) :- 
  (afc_clause(P) ; assert(P)),
  !,
  afcAddSupport(P,Support).

afcAsserta(P,Support) :-
  (afc_clause(P) ; asserta(P)),
  !,
  afcAddSupport(P,Support).

afcAssertz(P,Support) :-
  (afc_clause(P) ; assertz(P)),
  !,
  afcAddSupport(P,Support).

afc_clause((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  clause(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

afc_clause(Head) :-
  % find a unit clause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  clause(Head_copy,true),
  variant(Head,Head_copy).

afcForEach(Binder,Body) :- Binder,afcdo(Body),fail.
afcForEach(_,_).

% afcdo(X) executes X once and always succeeds.
afcdo(X) :- X,!.
afcdo(_).


% %  afcUnion(L1,L2,L3) - true if set L3 is the result of appending sets
% %  L1 and L2 where sets are represented as simple lists.

afcUnion([],L,L).
afcUnion([Head|Tail],L,Tail2) :-  
  memberchk(Head,L),
  !,
  afcUnion(Tail,L,Tail2).
afcUnion([Head|Tail],L,[Head|Tail2]) :-  
  afcUnion(Tail,L,Tail2).


% %  afcConjoin(+Conjunct1,+Conjunct2,?Conjunction).
% %  arg3 is a simplified expression representing the conjunction of
% %  args 1 and 2.

afcConjoin(true,X,X) :- !.
afcConjoin(X,true,X) :- !.
afcConjoin(C1,C2,(C1,C2)).


%   File   : afcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a afc database (e.g. save,
% % 	restore, reset, etc.0

% afcDatabaseTerm(P/A) is true iff P/A is something that afc adds to
% the database and should not be present in an empty afc database

afcDatabaseTerm('$spft$'/3).
afcDatabaseTerm('$pt$'/2).
afcDatabaseTerm('$bt$'/2).
afcDatabaseTerm('$nt$'/3).
afcDatabaseTerm('==>'/2).
afcDatabaseTerm('<==>'/2).
afcDatabaseTerm('<-'/2).
afcDatabaseTerm(afcQueue/1).

% removes all forward chaining rules and justifications from db.

afcReset :-
  afc_spft(P,F,Trigger),
  afcRetractOrWarn(P),
  afcRetractOrWarn('$spft$'(P,F,Trigger)),
  fail.
afcReset :-
  (afcDatabaseItem(T)*->
   (afcError("Pfc database not empty after afcReset, e.g., ~p.~n",[T]),fail)
    ; true).


% true if there is some afc crud still in the database.
afcDatabaseItem(Term:-Body) :-
  afcDatabaseTerm(P/A),
  functor(Term,P,A),
  clause(Term,Body).

afcRetractOrWarn(X) :-  retract(X), !.
afcRetractOrWarn(X) :- 
  afcWarn("Couldn't retract ~p.",[X]),dumpST,afcWarn("Couldn't retract ~p.",[X]),!.

afcRetractOrQuietlyFail(X) :-  retract(X), !.
afcRetractOrQuietlyFail(X) :- 
  nop((afcTraceMsg("Trace: Couldn't retract ~p.",[X]),dumpST,afcWarn("Couldn't retract ~p.",[X]))),
  !,fail.



%   File   : afcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

:- dynamic afcTraced/1.
:- dynamic afcSpied/2.
:- dynamic afcTraceExecution/0.
:- dynamic   afcWarnings/1.

:- afcDefault(afcWarnings(_), afcWarnings(true)).

% %  predicates to examine the state of afc

afcQueue :- listing(afcQueue/1).

afcPrintDB :-
  afcPrintFacts,
  afcPrintRules,
  afcPrintTriggers,
  afcPrintSupports,!.

printLine:- ansi_format([underline],"~N=========================================~n",[]).

% % afcPrintFacts ...

afcPrintFacts :- afcPrintFacts(_,true).


afcPrintFacts(Pattern) :- afcPrintFacts(Pattern,true).

afcPrintFacts(P,C) :-  
  afcFacts(P,C,L),
  afcClassifyFacts(L,User,Pfc,_Rule),
  printLine,
  afcPrintf("User added facts:~n",[]),
  afcPrintitems(User),
  printLine,
  afcPrintf("Pfc added facts:~n",[]),
  afcPrintitems(Pfc),
  printLine,!.


% %  printitems clobbers it's arguments - beware!

afcPrintitems([]).
afcPrintitems([H|T]) :-
  % numbervars(H,0,_),
  %format('~N ~p.',[H]),
  \+ \+ ( pretty_numbervars(H,H1),format(" ",[]),portray_clause_w_vars(H1)),
  afcPrintitems(T).

afcClassifyFacts([],[],[],[]).

afcClassifyFacts([H|T],User,Pfc,[H|Rule]) :-
  afcType(H,rule),
  !,
  afcClassifyFacts(T,User,Pfc,Rule).

afcClassifyFacts([H|T],[H|User],Pfc,Rule) :-
  matches_why_UU(UU),
  afcGetSupport(H,UU),
  !,
  afcClassifyFacts(T,User,Pfc,Rule).

afcClassifyFacts([H|T],User,[H|Pfc],Rule) :-
  afcClassifyFacts(T,User,Pfc,Rule).

afcPrintRules :-
  printLine,
  afcPrintf("Rules:...~n",[]),
  bagof_or_nil((P==>Q),clause((P==>Q),true),R1),
  afcPrintitems(R1),
  bagof_or_nil((P<==>Q),clause((P<==>Q),true),R2),
  afcPrintitems(R2),
  bagof_or_nil((P<-Q),clause((P<-Q),true),R3),
  afcPrintitems(R3),
  printLine.

afcGetTrigger(Trigger):- afc_call(Trigger).


% %   afcPrintTriggers is semidet.
%
% Pretty Print Triggers.
%
afcPrintTriggers :-
     print_afc_items("Positive triggers", '$pt$'(_,_)),
     print_afc_items("Negative triggers", '$nt$'(_,_,_)),
     print_afc_items("Goal triggers",'$bt$'(_,_)).

pp_triggers:-afcPrintTriggers.
%= 	 	 

% %  afcPrintSupports is semidet.
%
% Pretty Print Supports.
%
afcPrintSupports :-
  % temporary hack.
  draw_line,
  fmt("Supports ...~n",[]), 
  setof_or_nil((P =< S), (afcGetSupport(P,S), \+ pp_filtered(P)),L),
  pp_items('Support',L),
  draw_line,!.
pp_supports:- afcPrintSupports.

pp_filtered(P):-var(P),!,fail.
pp_filtered(_:P):- !, pp_filtered(P).
pp_filtered(P):- safe_functor(P,F,A),F\==(/),!,pp_filtered(F/A).
pp_filtered(F/_):-F==afc_prop.



afcFact(P) :- afcFact(P,true).

% %  afcFact(P,C) is true if fact P was asserted into the database via
% %  afcAdd and contdition C is satisfied.  For example, we might do:
% %  
% %   afcFact(X,afcUserFact(X))
% % 

afcFact(P,C) :- 
  afcGetSupport(P,_),
  afcType(P,fact(_)),
  afcCallSystem(C).

% %  afcFacts(-ListofPfcFacts) returns a list of facts added.

afcFacts(L) :- afcFacts(_,true,L).

afcFacts(P,L) :- afcFacts(P,true,L).

% %  afcFacts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

afcFacts(P,C,L) :- setof_or_nil(P,afcFact(P,C),L).

brake(X) :-  X, break.

% % 
% % 
% %  predicates providing a simple tracing facility
% % 

afcTraceAdd(P) :- 
  % this is here for upward compat. - should go away eventually.
  afcTraceAdd(P,(o,o)).

afcTraceAdd('$pt$'(_,_),_) :-
  % hack for now - never trace triggers.
  !.
afcTraceAdd('$nt$'(_,_),_) :-
  % hack for now - never trace triggers.
  !.

afcTraceAdd(P,S) :-
   afcTraceAddPrint(P,S),
   afcTraceBreak(P,S).
   

afcTraceAddPrint(P,S) :-
  afcIsTraced(P),
  !, 
  pretty_numbervars(P,Pcopy),
  % numbervars(Pcopy,0,_),
  matches_why_UU(UU),
  (S=UU
       -> afcPrintf("Adding (u) ~@",[fmt_cl(Pcopy)])
        ; afcPrintf("Adding ~@",[fmt_cl(Pcopy)])).

afcTraceAddPrint(_,_).


afcTraceBreak(P,_S) :-
  afcSpied(P,+) -> 
   (pretty_numbervars(P,Pcopy),
    % numbervars(Pcopy,0,_),
    afcPrintf("Breaking on afcAdd(~p)",[Pcopy]),
    break)
   ; true.

afcTraceRem('$pt$'(_,_)) :-
  % hack for now - never trace triggers.
  !.
afcTraceRem('$nt$'(_,_)) :-
  % hack for now - never trace triggers.
  !.

afcTraceRem(P) :-
  (afcIsTraced(P) 
     -> afcPrintf("Removing: ~p.",[P])
      ; true),
  (afcSpied(P,-)
   -> (afcPrintf("Breaking on afcRem(~p)",[P]),
       break)
   ; true).

afcIsTraced(P):- afcTraced(P).

afc_trace_exec:- afcWatch,afcTrace.
afc_notrace_exec:- afcNoTrace,afcNoWatch.

afcTrace :- afcTrace(_).

afcTrace(Form) :-
  assert(afcTraced(Form)).

afcTrace(Form,Condition) :- 
  assert((afcTraced(Form) :- Condition)).

afcSpy(Form) :- afcSpy(Form,[+,-],true).

afcSpy(Form,Modes) :- afcSpy(Form,Modes,true).

afcSpy(Form,[H|T],Condition) :-
  !,
  afcSpy1(Form,H,Condition),
  afcSpy(Form,T,Condition).

afcSpy(Form,Mode,Condition) :-
  afcSpy1(Form,Mode,Condition).

afcSpy1(Form,Mode,Condition) :-
  assert((afcSpied(Form,Mode) :- Condition)).

afcNospy :- afcNospy(_,_,_).

afcNospy(Form) :- afcNospy(Form,_,_).

afcNospy(Form,Mode,Condition) :- 
  clause(afcSpied(Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
afcNospy(_,_,_).

afcNoTrace :- afcUntrace.
afcUntrace :- afcUntrace(_).
afcUntrace(Form) :- retractall(afcTraced(Form)).

% needed:  afcTraceRule(Name)  ...


% if the correct flag is set, trace exection of Pfc

afcTraceMsg(Msg):- afcTraceMsg('~p',[Msg]).
afcTraceMsg(Msg,Args) :-
    afcTraceExecution,
    !,
    afcPrintf(user_output, Msg, Args).
afcTraceMsg(_Msg,_Args).


afcPrintf(Msg,Args) :- 
  afcPrintf(user_output, Msg,Args).

afcPrintf(Where,Msg,Args) :- 
  format(Where,'~N',[]),
  format(Where,Msg,Args).



afcWatch :- assert(afcTraceExecution).

afcNoWatch :-  retractall(afcTraceExecution).

afcError(Msg) :-  afcError(Msg,[]).

afcError(Msg,Args) :- 
  format("~N~nERROR/Pfc: ",[]),
  format(Msg,Args).

% % 
% %  These control whether or not warnings are printed at all.
% %    afcWarn.
% %    noafcWarn.
% % 
% %  These print a warning message if the flag afcWarnings is set.
% %    afcWarn(+Message)
% %    afcWarn(+Message,+ListOfArguments)
% % 

afcWarn :- 
  retractall(afcWarnings(_)),
  assert(afcWarnings(true)).

noafcWarn :-
  retractall(afcWarnings(_)),
  assert(afcWarnings(false)).
 
afcWarn(Msg) :-  afcWarn('~p',[Msg]).

afcWarn(Msg,Args) :- 
  afcWarnings(true),
  !,
  ansi_format([underline,fg(red)],"~N==============WARNING/Pfc================~n",[]),
  ansi_format([fg(yellow)],Msg,Args),
  printLine.
afcWarn(_,_).

% % 
% %  afcWarnings/0 sets flag to cause afc warning messages to print.
% %  afcNoWarnings/0 sets flag to cause afc warning messages not to print.
% % 

afcWarnings :- 
  retractall(afcWarnings(_)),
  assert(afcWarnings(true)).

afcNoWarnings :- 
  retractall(afcWarnings(_)).

%   File   : afcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a fact *****


:- use_module(library(lists)).

justification(F,J) :- supports(F,J).

justifications(F,Js) :- bagof(J,justification(F,J),Js).



% %  base(P,L) - is true iff L is a list of "base" facts which, taken
% %  together, allows us to deduce P.  A base fact is an axiom (a fact 
% %  added by the user or a raw Prolog fact (i.e. one w/o any support))
% %  or an assumption.

base(F,[F]) :- (axiom(F) ; assumption(F)),!.

base(F,L) :-
  % i.e. (reduce 'append (map 'base (justification f)))
  justification(F,Js),
  bases(Js,L).


% %  bases(L1,L2) is true if list L2 represents the union of all of the 
% %  facts on which some conclusion in list L1 is based.

bases([],[]).
bases([X|Rest],L) :-
  base(X,Bx),
  bases(Rest,Br),
  afcUnion(Bx,Br,L).
	
axiom(F) :- 
  matches_why_UU(UU),
  afcGetSupport(F,UU); 
  afcGetSupport(F,(god,god)).

% %  an assumption is a failed goal, i.e. were assuming that our failure to 
% %  prove P is a proof of not(P)

assumption(P) :- afc_unnegate(P,_).
   
% %  assumptions(X,As) if As is a set of assumptions which underly X.

assumptions(X,[X]) :- assumption(X).
assumptions(X,[]) :- axiom(X).
assumptions(X,L) :-
  justification(X,Js),
  assumptions1(Js,L).

assumptions1([],[]).
assumptions1([X|Rest],L) :-
  assumptions(X,Bx),
  assumptions1(Rest,Br),
  afcUnion(Bx,Br,L).  


% %  afcProofTree(P,T) the proof tree for P is T where a proof tree is
% %  of the form
% % 
% %      [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
% %           ^                         and has the form of
% %           [J11, J12,... J1n]      a list of proof trees.


% afcChild(P,Q) is true iff P is an immediate justifier for Q.
% mode: afcChild(+,?)

afcChild(P,Q) :-
  afcGetSupport(Q,(P,_)).

afcChild(P,Q) :-
  afcGetSupport(Q,(_,Trig)),
  afcType(Trig,trigger(_Pos)),
  afcChild(P,Trig).

afcChildren(P,L) :- bagof_or_nil(C,afcChild(P,C),L).

% afcDescendant(P,Q) is true iff P is a justifier for Q.

afcDescendant(P,Q) :- 
   afcDescendant1(P,Q,[]).

afcDescendant1(P,Q,Seen) :-
  afcChild(X,Q),
  (\+ member(X,Seen)),
  (P=X ; afcDescendant1(P,X,[X|Seen])).
  
afcDescendants(P,L) :- 
  bagof_or_nil(Q,afcDescendant1(P,Q,[]),L).



/*
current_why_U(U):- must(current_why(Why)), U = user(Why).
current_why_UU(UU):- current_why_U(U), UU= (U,U).
matches_why_U(U):-  freeze(U,U=user(_)).
matches_why_UU(UU):- matches_why_U(U1),matches_why_U(U2), freeze(UU,UU=(U1,U2)).
*/
current_why_U(U):-  get_why_uu((U,_)).% must(current_why(Why)), U = user(Why).
current_why_UU(UU):- get_why_uu(UU). % current_why_U(U), UU= (U,U).
matches_why_U(U):-  nop((current_why_U(Y), freeze(U,\+ \+ (U=Y;true)))).
matches_why_UU(UU):- nop(only_is_user_reason(UU)). % matches_why_U(U1),matches_why_U(U2),freeze(UU,UU=(U1,U2)).


matterialize_support_term(S,Sup):- term_attvars(S,Atts), Atts\==[] -> copy_term(S,_,Goals),Sup= S+Goals,!.
matterialize_support_term(SS,SS).

% % 
% % 
% %  predicates for manipulating support relationships
% % 

% %  afcAddSupport(+Fact,+Support)

afcAddSupport(P,(Fact,Trigger)) :- assert('$spft$'(P,Fact,Trigger)).

afcGetSupport(P,(Fact,Trigger)) :- afc_spft(P,Fact,Trigger).

afc_spft(P,F,T) :- afcCallSystem('$spft$'(P,F,T)).

% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

afcRemOneSupport(P,(Fact,Trigger)) :-
  must(callable(P);callable(Fact);callable(Trigger)),
  afcRetractOrWarn('$spft$'(P,Fact,Trigger)).

afcRemOneSupportOrQuietlyFail(P,(Fact,Trigger)) :-
  must(callable(P);callable(Fact);callable(Trigger)),
  afcRetractOrQuietlyFail('$spft$'(P,Fact,Trigger)).


afc_collect_supports(Tripples) :-
  bagof(Tripple, afc_support_relation(Tripple), Tripples),
  !.
afc_collect_supports([]).

afc_support_relation((P,F,T)) :-
  afc_spft(P,F,T).



afc_make_supports((P,S1,S2)) :- 
  afcAddSupport(P,(S1,S2)),
  (afcAddType1(P); true),
  !.

% %  afcTriggerKey(+Trigger,-Key) 
% % 
% %  Arg1 is a trigger.  Key is the best term to index it on.

afcTriggerKey('$pt$'(Key,_),Key).
afcTriggerKey('$pt$'(Key,_,_),Key).
afcTriggerKey('$nt$'(Key,_,_),Key).
afcTriggerKey(Key,Key).


% % ^L
% %  Get a key from the trigger that will be used as the first argument of
% %  the trigger base clause that stores the trigger.
% % 

afc_trigger_key(X,X) :- var(X), !.
afc_trigger_key(chart(word(W),_L),W) :- !.
afc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
afc_trigger_key(chart(Concept,_L),Concept) :- !.
afc_trigger_key(X,X).

%   File   : afcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:- use_module(library(lists)).

:- dynamic(t_l:whybuffer/2).



afcWhy :- 
  t_l:whybuffer(P,_),
  afcWhy(P).

afcTF(P):- afc_call(P)*->foreach(afcTF1(P),true);afcTF1(P).
afcTF1(P):- 
   ansi_format([underline],"~N=========================================",[]),
   (ignore(afcWhy(P))), ignore(afcWhy(~P)),
   printLine.
   

afcWhy(N) :-
  number(N),
  !,
  t_l:whybuffer(P,Js),
  afcWhyCommand(N,P,Js).

afcWhy(P) :-
  justifications(P,Js),
  retractall(t_l:whybuffer(_,_)),
  assert(t_l:whybuffer(P,Js)),
  afcWhyBrouse(P,Js).

afcWhy1(P) :-
  justifications(P,Js),
  afcWhyBrouse(P,Js).

afcWhy2(P,N) :-
  justifications(P,Js), afcShowJustification1(Js,N).

afcWhyBrouse(P,Js) :-
  % rtrace(afc_pp_afc_justifications(P,Js)),
  afcShowJustifications(P,Js),
  nop((afcAsk(' >> ',Answer),
  afcWhyCommand(Answer,P,Js))).

afcWhyCommand(q,_,_) :- !.
afcWhyCommand(h,_,_) :- 
  !,
  format("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",[]).

afcWhyCommand(N,_P,Js) :-
  float(N),
  !,
  afcSelectJustificationNode(Js,N,Node),
  afcWhy1(Node).

afcWhyCommand(u,_,_) :-
  % u=up
  !.

afcCommand(N,_,_) :-
  integer(N),
  !,
  afcPrintf("~p is a yet unimplemented command.",[N]),
  fail.

afcCommand(X,_,_) :-
 afcPrintf("~p is an unrecognized command, enter h. for help.",[X]),
 fail.
  
afcShowJustifications(P,Js) :-
  show_current_source_location,
  reset_shown_justs,
  %color_line(yellow,1),
  format("~N~nJustifications for ",[]),
  ansi_format([fg(green)],'~@',[pp(P)]),
  format(" :~n",[]),
  afcShowJustification1(Js,1),!,
  printLine.

afcShowJustification1([],_):-!.
afcShowJustification1([J|Js],N) :- !,
  % show one justification and recurse.    
  %reset_shown_justs,
  afcShowSingleJust(N,step(1),J),!,
  N2 is N+1,  
  afcShowJustification1(Js,N2).

afcShowJustification1(J,N) :- 
  %reset_shown_justs, % nl,
  afcShowSingleJust(N,step(1),J),!.

incrStep(StepNo,Step):-arg(1,StepNo,Step),X is Step+1,nb_setarg(1,StepNo,X).

afcShowSingleJust(JustNo,StepNo,C):- is_ftVar(C),!,incrStep(StepNo,Step),
  ansi_format([fg(cyan)],"~N    ~w.~w ~w ",[JustNo,Step,C]),!, maybe_more_c(C).
afcShowSingleJust(_JustNo,_StepNo,[]):-!.
afcShowSingleJust(JustNo,StepNo,(P,T)):-!, 
  afcShowSingleJust(JustNo,StepNo,P),
  afcShowSingleJust(JustNo,StepNo,T).
afcShowSingleJust(JustNo,StepNo,(P,F,T)):-!, 
  afcShowSingleJust1(JustNo,StepNo,P),
  afcShowSingleJust(JustNo,StepNo,F),
  afcShowSingleJust1(JustNo,StepNo,T).
afcShowSingleJust(JustNo,StepNo,(P*->T)):-!, 
  afcShowSingleJust1(JustNo,StepNo,P),format('      *-> ',[]),
  afcShowSingleJust1(JustNo,StepNo,T).

afcShowSingleJust(JustNo,StepNo,(P:-T)):-!, 
  afcShowSingleJust1(JustNo,StepNo,P),format(':- ~p.',[T]).
 
afcShowSingleJust(JustNo,StepNo,(P : -T)):-!, 
  afcShowSingleJust1(JustNo,StepNo,P),format('      :- ',[]),
  afcShowSingleJust(JustNo,StepNo,T).

afcShowSingleJust(JustNo,StepNo,(P :- T) ):- !, 
  afcShowSingleJust1(JustNo,StepNo,call(T)),  
  afcShowSingleJust1(JustNo,StepNo,P).


afcShowSingleJust(JustNo,StepNo,[P|T]):-!, 
  afcShowSingleJust(JustNo,StepNo,P),
  afcShowSingleJust(JustNo,StepNo,T).

afcShowSingleJust(JustNo,StepNo,'$pt$'(P,Body)):- !, 
  afcShowSingleJust1(JustNo,StepNo,'$pt$'(P)),  
  afcShowSingleJust(JustNo,StepNo,Body).

afcShowSingleJust(JustNo,StepNo,C):- 
 afcShowSingleJust1(JustNo,StepNo,C).

fmt_cl(P):- \+ \+ (pretty_numbervars(P,PP),numbervars(PP,126,_,[attvar(skip),singletons(true)]), write_term(PP,[portray(true),portray_goal(fmt_cl)])),write('.').
fmt_cl(S,_):- term_is_ansi(S), !, write_keeping_ansi(S).
fmt_cl(G,_):- is_grid(G),write('"'),user:print_grid(G),write('"'),!.
% fmt_cl(P,_):- catch(arc_portray(P),_,fail),!.
fmt_cl(P,_):- is_list(P),catch(print_tree_nl(P),_,fail),!.
%ptg(PP,Opts):- is_list(PP),select(portray_goal(ptg),Opts,Never),write_term(PP,Never). 

unwrap_litr(C,CCC+VS):- copy_term(C,CC,VS),
  numbervars(CC+VS,0,_),
  unwrap_litr0(CC,CCC),!.
unwrap_litr0(call(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0('$pt$'(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(body(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(head(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(C,C).

:- thread_local t_l:shown_why/1.

afcShowSingleJust1(_,_,MFL):- is_mfl(MFL),!.
afcShowSingleJust1(JustNo,StepNo,C):- unwrap_litr(C,CC),!,afcShowSingleJust4(JustNo,StepNo,C,CC).
afcShowSingleJust4(_,_,_,MFL):- is_mfl(MFL),!.
afcShowSingleJust4(_,_,_,CC):- t_l:shown_why(C),C=@=CC,!.
afcShowSingleJust4(JustNo,StepNo,C,CC):- assert(t_l:shown_why(CC)),!,
   incrStep(StepNo,Step),
   ansi_format([fg(cyan)],"~N    ~w.~w ~@ ",[JustNo,Step,user:fmt_cl(C)]),  
   afcShowSingleJust_C(C),!,
   format('~N'),
   ignore((maybe_more_c(C))),
   format('~N'),!.

is_mfl(MFL):- compound(MFL), MFL = mfl4(_,_,_,_).

maybe_more_c(MFL):- is_mfl(MFL),!.
maybe_more_c(_):- t_l:shown_why(no_recurse).
maybe_more_c(C):- t_l:shown_why(more(C)),!.
maybe_more_c(C):- t_l:shown_why((C)),!.
maybe_more_c(C):- assert(t_l:shown_why(more(C))),assert(t_l:shown_why((C))), 
 locally(t_l:shown_why(no_recurse),
  locally(t_l:shown_why((C)),locally(t_l:shown_why(more(C)),
   ignore(catch(afcWhy2(C,1.1),E,wdmsg(E)))))),!.

afcShowSingleJust_C(C):-is_file_ref(C),!.
afcShowSingleJust_C(C):-find_mfl(C,MFL),assert(t_l:shown_why(MFL)),!,afcShowSingleJust_MFL(MFL).
afcShowSingleJust_C(_):-ansi_format([hfg(black)]," % [no_mfl] ",[]),!.

short_filename(F,FN):- atomic_list_concat([_,FN],'/pack/',F),!.
short_filename(F,FN):- atomic_list_concat([_,FN],swipl,F),!.
short_filename(F,FN):- F=FN,!.

afcShowSingleJust_MFL(MFL):- MFL=mfl4(VarNameZ,_M,F,L),atom(F),short_filename(F,FN),!,varnames_load_context(VarNameZ),
   ansi_format([hfg(black)]," % [~w:~w] ",[FN,L]).

afcShowSingleJust_MFL(MFL):- MFL=mfl4(V,M,F,L),maplist(var,[V,M,F,L]),!.
afcShowSingleJust_MFL(MFL):- ansi_format([hfg(black)]," % [~w] ",[MFL]),!.

afcAsk(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

afcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).






















:- set_prolog_flag(expect_afc_file,unknown).

% =======================================================
/* 
%
%= predicates to examine the state of afc 
% interactively exploring Pfc justifications.
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/afc/afc_list_triggers.pl
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
afc_listing_module:- nop( module(afc_listing,
          [ draw_line/0,
            loop_check_just/1,
            pinfo/1,
            pp_items/2,
            pp_item/2,
            pp_filtered/1,
            pp_facts/2,
            pp_facts/1,
            pp_facts/0,
            afc_list_triggers_types/1,
            afc_list_triggers_nlc/1,
            afc_list_triggers_1/1,
            afc_list_triggers_0/1,
            afc_list_triggers/1,
            afc_contains_term/2,
            afc_classify_facts/4,
            lqu/0,
            get_clause_vars_for_print/2,
            %afcWhyBrouse/2,
            %afcWhy1/1,
            %afcWhy/1,
            %afcWhy/0,
            pp_rules/0,
            afcPrintSupports/0,
             afcPrintTriggers/0,            
            print_afc_items/1,
            print_afc_items/2,
            print_afc_items/3,
            print_afc_items/4,
            print_afc_items_and_neg/3,
            show_pred_info/1,
            show_pred_info_0/1,
            afc_listing_file/0
          ])).

%:- include('afc_header.pi').

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

% %  lqu is semidet.
%
% Lqu.
%
lqu :- listing(que/2).


 

%= 	 	 

% %  pp_facts is semidet.
%
% Pretty Print Facts.
%
pp_facts :- pp_facts(_,true).


%= 	 	 

% %  pp_facts( ?Pattern) is semidet.
%
% Pretty Print Facts.
%
pp_facts(Pattern) :- pp_facts(Pattern,true).


%= 	 	 

% %  pp_facts( ?P, ?C) is semidet.
%
% Pretty Print Facts.
%
pp_facts(P,C) :-
  afcFacts(P,C,L),
  afc_classify_facts(L,User,Pfc,_Rule),
  draw_line,
  fmt("User added facts:",[]),
  pp_items(user,User),
  draw_line,
  draw_line,
  fmt("Pfc added facts:",[]),
  pp_items(system,Pfc),
  draw_line.



%= 	 	 

% %  pp_items( ?Type, :TermH) is semidet.
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

% %  pp_item( ?MM, :TermH) is semidet.
%
% Pretty Print Item.
%
pp_item(_M,H):-pp_filtered(H),!.
pp_item(MM,(H:-B)):- B ==true,pp_item(MM,H).
pp_item(MM,H):- flag(show_asserions_offered,X,X+1),find_and_call(get_print_mode(html)), ( \+ \+ if_defined(pp_item_html(MM,H))),!.


pp_item(MM,'$spft$'(W0,U,ax)):- W = (_KB:W0),!,pp_item(MM,U:W).
pp_item(MM,'$spft$'(W0,F,U)):- W = (_KB:W0),atom(U),!,    fmt('~N%~n',[]),pp_item(MM,U:W), fmt('rule: ~p~n~n', [F]),!.
pp_item(MM,'$spft$'(W0,F,U)):- W = (_KB:W0),         !,   fmt('~w~nd:       ~p~nformat:    ~p~n', [MM,W,F]),pp_item(MM,U).
pp_item(MM,'$nt$'(Trigger0,Test,Body)) :- Trigger = (_KB:Trigger0), !, fmt('~w n-trigger(-): ~p~ntest: ~p~nbody: ~p~n', [MM,Trigger,Test,Body]).
pp_item(MM,'$pt$'(F0,Body)):- F = (_KB:F0),             !,fmt('~w p-trigger(+):~n', [MM]), pp_item('',(F:-Body)).
pp_item(MM,'$bt$'(F0,Body)):- F = (_KB:F0),             !,fmt('~w b-trigger(?):~n', [MM]), pp_item('',(F:-Body)).


pp_item(MM,U:W):- !,format(string(S),'~w  ~w:',[MM,U]),!, pp_item(S,W).
pp_item(MM,H):- \+ \+ (( get_clause_vars_for_print(H,HH),fmt("~w ~p~N",[MM,HH]))).


%= 	 	 

% %  get_clause_vars_for_print( ?HB, ?HB) is semidet.
%
% Get Clause Variables For Print.
%
get_clause_vars_for_print(HB,HB):- ground(HB),!.
get_clause_vars_for_print(I,I):- is_listing_hidden(skipVarnames),!.
get_clause_vars_for_print(H0,MHB):- get_clause_vars_copy(H0,MHB),!.
get_clause_vars_for_print(HB,HB).

%= 	 	 

% %  afc_classify_facts( :TermH, ?User, :TermPfc, ?H) is semidet.
%
% Managed Predicate Classify Facts.
%
afc_classify_facts([],[],[],[]).

afc_classify_facts([H|T],User,Pfc,[H|Rule]) :-
  afcType(H,rule),
  !,
  afc_classify_facts(T,User,Pfc,Rule).

afc_classify_facts([H|T],[H|User],Pfc,Rule) :-
  afcGetSupport(H,(mfl4(_VarNameZ,_,_,_),ax)),
  !,
  afc_classify_facts(T,User,Pfc,Rule).

afc_classify_facts([H|T],User,[H|Pfc],Rule) :-
  afc_classify_facts(T,User,Pfc,Rule).



%= 	 	 

% %  print_afc_items( ?T, ?I) is semidet.
%
% Print Database Items.
%
print_afc_items(T, I):- 
    draw_line, 
    fmt("~N~w ...~n",[T]),
    print_afc_items(I),
    draw_line,!.


%= 	 	 

% %  print_afc_items( ?I) is semidet.
%
% Print Database Items.
%
print_afc_items(F/A):-number(A),!,safe_functor(P,F,A),!,print_afc_items(P).
print_afc_items(H):- bagof(H,clause(H,true),R1),pp_items((:),R1),R1\==[],!.
print_afc_items(H):- \+ current_predicate(_,H),!. 
print_afc_items(H):- catch( ('$find_predicate'(H,_),call_u(listing(H))),_,true),!,nl,nl.


%= 	 	 

% %  pp_rules is semidet.
%
% Pretty Print Rules.
%
pp_rules :-
   print_afc_items("Forward Rules",(_ ==> _)),
   print_afc_items("Bidirectional Rules",(_ <==> _)), 
   print_afc_items("Implication Rules",=>(_ , _)),
   print_afc_items("Bi-conditional Rules",<=>(_ , _)),
   print_afc_items("Backchaining Rules",(_ <- _)),
   print_afc_items("Positive Facts",(==>(_))),
   print_afc_items("Negative Facts",(~(_))).


%= 	 	 


% %  draw_line is semidet.
%
% Draw Line.
%
draw_line:- \+ thread_self_main,!.
draw_line:- printLine,!.
draw_line:- (t_l:print_mode(H)->true;H=unknown),fmt("~N% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %~n",[]),H=H.

 :- meta_predicate loop_check_just(0).

%= 	 	 

% %  loop_check_just( :GoalG) is semidet.
%
% Loop Check Justification.
%
loop_check_just(G):-loop_check(G,ignore(arg(1,G,[]))).


%= 	 	 

% %  show_pred_info( ?F) is semidet.
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

% %  show_pred_info_0( ?Head) is semidet.
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

% %  print_afc_items( ?Title, ?Mask, ?What) is semidet.
%
% Print Database Items.
%
print_afc_items(Title,Mask,What):-print_afc_items(Title,Mask,Mask,What).

%= 	 	 

% %  print_afc_items( ?Title, ?Mask, ?SHOW, ?What0) is semidet.
%
% Print Database Items.
%
print_afc_items(Title,Mask,SHOW,What0):-
     get_pi(Mask,H),get_pi(What0,What),
     format(atom(Showing),'~p for ~p...',[Title,What]),
     statistics(cputime,Now),Max is Now + 2,!,
       gripe_time(1.0,
         doall((once(statistics(cputime,NewNow)),NewNow<Max,clause_or_call(H,B),
             quietly(afc_contains_term(What,(H:-B))),
             flag(print_afc_items,LI,LI+1),
             ignore(quietly(pp_item(Showing,SHOW)))))),
     ignore(pp_item(Showing,done)),!.


%= 	 	 

% %  afc_contains_term( ?What, ?VALUE2) is semidet.
%
% Managed Predicate Contains Term.
%
afc_contains_term(What,_):-is_ftVar(What),!.
afc_contains_term(What,Inside):- compound(What),!,(\+ \+ ((copy_term_nat(Inside,Inside0),snumbervars(Inside0),occurs:contains_term(What,Inside0)))),!.
afc_contains_term(What,Inside):- (\+ \+ once((subst(Inside,What,foundZadooksy,Diff),Diff \=@= Inside ))),!.



%= 	 	 

% %  hook_afc_listing( ?What) is semidet.
%
% Hook To [baseKB:hook_afc_listing/1] For Module Mpred_listing.
% Hook Managed Predicate Listing.
%
:- current_prolog_flag(afc_shared_module,BaseKB),
 assert_if_new((BaseKB:hook_afc_listing(What):- on_x_debug(afc_list_triggers(What)))).

:- thread_local t_l:afc_list_triggers_disabled/0.
% listing(L):-locally(t_l:afc_list_triggers_disabled,listing(L)).


%= 	 	 

% %  afc_list_triggers( ?What) is semidet.
%
% Managed Predicate List Triggers.
%
afc_list_triggers(_):-t_l:afc_list_triggers_disabled,!.
afc_list_triggers(What):-loop_check(afc_list_triggers_nlc(What)).

:- meta_predicate(afc_list_triggers_nlc(?)).


%= 	 	 

% %  afc_list_triggers_nlc( ?What) is semidet.
%
% Managed Predicate List Triggers Nlc.
%
afc_list_triggers_nlc(MM:What):-atom(MM),!,MM:afc_list_triggers(What).
afc_list_triggers_nlc(What):-loop_check(afc_list_triggers_0(What),true).


%= 	 	 

% %  afc_list_triggers_0( ?What) is semidet.
%
% Managed Predicate list triggers  Primary Helper.
%
afc_list_triggers_0(What):-get_pi(What,PI),PI\=@=What,afc_list_triggers(PI).
afc_list_triggers_0(What):-nonvar(What),What= ~(Then),!, \+ \+ afc_list_triggers_1(Then), \+ \+ afc_list_triggers_1(What).
afc_list_triggers_0(What):- \+ \+  afc_list_triggers_1(~(What)), \+ \+ afc_list_triggers_1(What).


%= 	 	 

% %  afc_list_triggers_types( ?VALUE1) is semidet.
%
% Managed Predicate list triggers  Types.
%
afc_list_triggers_types('Triggers').
afc_list_triggers_types('Instances').
afc_list_triggers_types('Subclasses').
afc_list_triggers_types('ArgTypes').
afc_list_triggers_types('Arity').
afc_list_triggers_types('Forward').
afc_list_triggers_types('Bidirectional').
afc_list_triggers_types('Backchaining').
afc_list_triggers_types('Negative').
afc_list_triggers_types('Sources').
afc_list_triggers_types('Supports').
afc_list_triggers_types('Edits').

% print_afc_items_and_neg(Title,Fact,What):-nonvar(Fact),Fact= ~(_),!,fail.

%= 	 	 

% %  print_afc_items_and_neg( ?Title, ?Fact, ?What) is semidet.
%
% Print Database Items And Negated.
%
print_afc_items_and_neg(Title,Fact,What):-print_afc_items(Title,Fact,What).
print_afc_items_and_neg(Title,Fact,What):-print_afc_items(Title,~(Fact),What).


%= 	 	 

% %  afc_list_triggers_1( ?What) is semidet.
%
% Managed Predicate list triggers  Secondary Helper.
%
afc_list_triggers_1(What):-var(What),!.
afc_list_triggers_1(~(What)):- var(What),!.
afc_list_triggers_1(~(_What)):-!.
afc_list_triggers_1(What):- 
   print_afc_items('Supports User',spft_precanonical(P,mfl4(VarNameZ,_,_,_),ax),'$spft$'(P,mfl4(VarNameZ,_,_,_),ax),What),
   print_afc_items('Forward Facts',(nesc(F)),F,What),
   print_afc_items('Forward Rules',(_==>_),What),
 ignore((What\= ~(_),safe_functor(What,IWhat,_),
   print_afc_items_and_neg('Instance Of',isa(IWhat,_),IWhat),
   print_afc_items_and_neg('Instances: ',isa(_,IWhat),IWhat),
   print_afc_items_and_neg('Subclass Of',genls(IWhat,_),IWhat),
   print_afc_items_and_neg('Subclasses: ',genls(_,IWhat),IWhat))),
   forall(suggest_m(M),print_afc_items('PFC Watches', afc_prop(M,_,_,_),What)),
   print_afc_items('Triggers Negative', '$nt$'(_,_,_,_),What),
   print_afc_items('Triggers Goal','$bt$'(_,_,_),What),
   print_afc_items('Triggers Positive','$pt$'(_,_,_),What),
   print_afc_items('Bidirectional Rules',(_<==>_),What), 
   dif(A,B),print_afc_items('Supports Deduced',spft_precanonical(P,A,B),'$spft$'(P,A,B),What),
   dif(G,ax),print_afc_items('Supports Nonuser',spft_precanonical(P,G,G),'$spft$'(P,G,G),What),
   print_afc_items('Backchaining Rules',(_<-_),What),
   % print_afc_items('Edits',is_disabled_clause(_),What),
   print_afc_items('Edits',is_edited_clause(_,_,_),What),
   print_afc_items('Instances',isa(_,_),What),
   print_afc_items('Subclasses',genls(_,_),What),
   print_afc_items('Negative Facts',~(_),What),

   print_afc_items('ArgTypes',argGenls(_,_,_),What),
   print_afc_items('ArgTypes',argIsa(_,_,_),What),
   print_afc_items('ArgTypes',argQuotedIsa(_,_,_),What),
   print_afc_items('ArgTypes',meta_argtypes(_),What),
   print_afc_items('ArgTypes',predicate_property(G,meta_predicate(G)),What),
   print_afc_items('ArgTypes',resultGenls(_,_),What),
   print_afc_items('ArgTypes',resultIsa(_,_),What),
   print_afc_items('Arity',arity(_,_),What),
   print_afc_items('Arity',current_predicate(_),What),
   print_afc_items('MetaFacts Predicate',predicate_property(_,_),What),
   print_afc_items('Sources',module_property(_,_),What),
   print_afc_items('Sources',predicateConventionMt(_,_),What),
   print_afc_items('Sources',source_file(_,_),What),
   print_afc_items('Sources',_:man_index(_,_,_,_,_),What),
   print_afc_items('Sources',_:'$pldoc'(_,_,_,_),What),
   print_afc_items('Sources',_:'$pred_option'(_,_,_,_),What),
   print_afc_items('Sources',_:'$mode'(_,_),What),
   !.     


pinfo(F/A):- listing(F/A),safe_functor(P,F,A),findall(Prop,predicate_property(P,Prop),List),wdmsg_pretty(pinfo(F/A)==List),!.



% %  pp_DB is semidet.
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
  pp_afc_facts,
  pp_afc_rules,
  pp_afc_triggers,
  pp_afc_supports))).

pp_afc_facts:- context_module(M), pp_afc_facts(M).
pp_afc_rules:- context_module(M), pp_afc_rules(M).
pp_afc_triggers:- context_module(M), pp_afc_triggers(M).
pp_afc_supports:- context_module(M), pp_afc_supports(M).


:- system:import(pp_DB/0).
:- system:export(pp_DB/0).

%  pp_afc_facts ...

pp_afc_facts(MM):- ignore(pp_afc_facts(MM,_,true)).

pp_afc_facts(MM,Pattern):- pp_afc_facts(MM,Pattern,true).

pp_afc_facts(MM,P,C):-
  afc_facts_in_kb(MM,P,C,L),
  afc_classifyFacts(L,User,Pfc,_ZRule),
  length(User,UserSize),length(Pfc,PfcSize),
  format("~N~nUser added facts in [~w]: ~w",[MM,UserSize]),
  pp_afc_items(User),
  format("~N~nSystem added facts in [~w]: ~w",[MM,PfcSize]),
  pp_afc_items(Pfc).

%  printitems clobbers it''s arguments - beware!


pp_afc_items(Var):-var(Var),!,format("~N  ~p",[Var]).
pp_afc_items([]):-!.
pp_afc_items([H|T]):- !,
  % numbervars(H,0,_),
  format("~N  ~p",[H]),
  nonvar(T),pp_afc_items(T).

pp_afc_items((P >= FT)):- is_hidden_pft(P,FT),!.
  
pp_afc_items(Var):-
  format("~N  ~p",[Var]).


is_hidden_pft(_,(mfl4(_VarNameZ,BaseKB,_,_),ax)):- current_prolog_flag(afc_shared_module,BaseKB),!.
is_hidden_pft(_,(why_marked(_),ax)).


pp_mask(Type,MM,Mask):-   
  bagof_or_nil(Mask,lookup_kb(MM,Mask),Nts),
  list_to_set_variant(Nts,NtsSet),!,
  pp_mask_list(Type,MM,NtsSet).

pp_mask_list(Type,MM,[]):- !,
  format("~N~nNo ~ws in [~w]...~n",[Type,MM]).
pp_mask_list(Type,MM,NtsSet):- length(NtsSet,Size), !,
  format("~N~n~ws (~w) in [~w]...~n",[Type,Size,MM]),
  pp_afc_items(NtsSet).

afc_classifyFacts([],[],[],[]).

afc_classifyFacts([H|T],User,Pfc,[H|Rule]):-
  afcType(H,rule(_)),
  !,
  afc_classifyFacts(T,User,Pfc,Rule).

afc_classifyFacts([H|T],[H|User],Pfc,Rule):-
  % get_source_uu(UU),
  get_first_user_reason(H,_UU),
  !,
  afc_classifyFacts(T,User,Pfc,Rule).

afc_classifyFacts([H|T],User,[H|Pfc],Rule):-
  afc_classifyFacts(T,User,Pfc,Rule).


pp_afc_rules(MM):- 
   pp_mask("Forward Rule",MM,==>(_,_)),
   pp_mask("Bidirectional Rule",MM,<==>(_,_)),
   pp_mask("Backchaining Rule",MM,<-(_,_)),
   pp_mask("Implication Rule",MM,=>(_,_)),
   pp_mask("Bi-conditional Rule",MM,<=>(_,_)),
   pp_mask("Negative Fact",MM,(~(_))),
  % pp_mask("Material-impl Rule",MM,<=(_,_)),
 % pp_mask("Prolog Rule",MM,:-(_,_)),
 !.


pp_afc_triggers(MM):- 
 pp_mask("Positive trigger(+)",MM,'$pt$'(_,_)),
 pp_mask("Negative trigger(-)",MM,'$nt$'(_,_,_)),
 pp_mask("Goal trigger(?)",MM,'$bt$'(_,_)),!.

pp_afc_supports(MM):-
  % temporary hack.
  format("~N~nSupports in [~w]...~n",[MM]),
  with_exact_kb(MM, bagof_or_nil((P >= S), afcGetSupport(P,S),L)),
  list_to_set_variant(L,LS),
  pp_afc_items(LS),!.


list_to_set_variant(List, Unique) :-
    list_unique_1(List, [], Unique),!.

list_unique_1([], _, []).
list_unique_1([X|Xs], So_far, Us) :-
    memberchk_variant(X, So_far),!,
    list_unique_1(Xs, So_far, Us).
list_unique_1([X|Xs], So_far, [X|Us]) :-
    list_unique_1(Xs, [X|So_far], Us).


% % 	memberchk_variant(+Val, +List)
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


% %  has_cl( +H) is semidet.
%
% Has Clause.
%
has_cl(H):-predicate_property(H,number_of_clauses(_)).



% %  clause_or_call( +H, ?B) is semidet.
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

% %  should_call_for_facts( +H) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(H):- get_functor(H,F,A),call_u(should_call_for_facts(H,F,A)).

% %  should_call_for_facts( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(_,F,_):- a(prologSideEffects,F),!,fail.
should_call_for_facts(H,_,_):- modulize_head(H,HH), \+ predicate_property(HH,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- clause_b(afc_prop(_M,F,A,afcRHS)),!,fail.
should_call_for_facts(_,F,A):- clause_b(afc_prop(_M,F,A,afcMustFC)),!,fail.
should_call_for_facts(_,F,_):- a(prologDynamic,F),!.
should_call_for_facts(_,F,_):- \+ a(afcControlled,F),!.

       */

% %  no_side_effects( +P) is semidet.
%
% No Side Effects.
%
%no_side_effects(P):-  (\+ is_side_effect_disabled->true;(get_functor(P,F,_),a(prologSideEffects,F))).

afc_facts_in_kb(MM,P,C,L):- with_exact_kb(MM,setof_or_nil(P,afcFact(P,C),L)).

lookup_spft(P,F,T):-afcGetSupport(P,(F,T)).
% why_dmsg(Why,Msg):- with_current_why(Why,dmsg_pretty(Msg)).

u_to_uu(U,(U,ax)):- var(U),!.
u_to_uu(U,U):- nonvar(U),U=(_,_),!.
u_to_uu([U|More],UU):-list_to_conjuncts([U|More],C),!,u_to_uu(C,UU).
u_to_uu(U,(U,ax)):-!.

% %  get_source_uu( :TermU) is det.
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


get_startup_uu(UU):-
  prolog_load_context(module,CM),
  u_to_uu((isRuntime,mfl4(VarNameZ,CM, user_input, _)),UU),varnames_load_context(VarNameZ).

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

%:- export(afc_at_box:defaultAssertMt/1).
%:- system:import(defaultAssertMt/1).
%:- afc_lib:import(afc_at_box:defaultAssertMt/1).

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
    afcError(no_source_ref(M))))).

is_source_ref1(_).

defaultAssertMt(M):- prolog_load_context(module, M).



afc_pp_afc_justifications(P,Js):-
 show_current_source_location, 
 must_ex(quietly_ex(( format("~NJustifications for ~p:",[P]),
  afc_pp_afc_justification1('',Js,1)))).

afc_pp_afc_justification1(_Prefix,[],_).

afc_pp_afc_justification1(Prefix,[J|Js],N):-
  % show one justification and recurse.
  nl,  
  afc_pp_afc_justifications2(Prefix,J,N,1),
  %reset_shown_justs,
  N2 is N+1,
  afc_pp_afc_justification1(Prefix,Js,N2).

afc_pp_afc_justifications2(_Prefix,[],_,_).

afc_pp_afc_justifications2(Prefix,[C|Rest],JustNo,StepNo):-
(nb_hasval('$last_printed',C)-> dmsg_pretty(chasVal(C)) ;
(
 (StepNo==1->fmt('~N~n',[]);true),
  backward_compatibility:sformat(LP,' ~w.~p.~p',[Prefix,JustNo,StepNo]),
  nb_pushval('$last_printed',LP),
  format("~N  ~w ~p",[LP,C]),
  ignore(loop_check(afcWhy_sub_sub(C))),
  StepNext is 1+StepNo,
  afc_pp_afc_justifications2(Prefix,Rest,JustNo,StepNext))).


afcWhy_sub_sub(P):-
  justifications(P,Js),
  clear_proofs,
  % retractall_u(t_l:whybuffer(_,_)),
  (nb_hasval('$last_printed',P)-> dmsg_pretty(hasVal(P)) ;
   ((
  assertz(t_l:whybuffer(P,Js)),
   nb_getval('$last_printed',LP),
   ((afc_pp_afc_justification1(LP,Js,1),fmt('~N~n',[])))))).

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


% %  afc_is_info( :TermC) is semidet.
%
% PFC If Is A Info.
%
afc_is_info((CWC,Info)):- (atom(CWC),is_a_info(CWC));afc_is_info(Info).
afc_is_info(afc_bc_only(C)):-is_ftNonvar(C),!.
afc_is_info(infoF(C)):-is_ftNonvar(C),!.
afc_is_info(inherit_above(_,_)).


is_a_info(fail).
is_a_info(CWC):- is_afc_chained(CWC).

is_afc_chained(cwc).
is_afc_chained(awc).
is_afc_chained(zwc).
is_afc_chained(fwc).
is_afc_chained(bwc).
is_afc_chained(wac).

:- forall(is_afc_chained(Op),assert_if_new(Op)).

reserved_body(B):-var(B),!,fail.
reserved_body(attr_bind(_)).
reserved_body(attr_bind(_,_)).
reserved_body(B):-reserved_body_helper(B).

reserved_body_helper(B):- \+ compound(B),!,fail.
reserved_body_helper((ZAWC,_)):- atom(ZAWC),is_afc_chained(ZAWC).

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

:- fixup_exports.
%:- current_prolog_flag(afc_shared_module,BaseKB),fixup_module_exports_into(BaseKB).
:- fixup_module_exports_into(system).

afc_rule_hb(C,_):- \+ compound(C),!,fail.
afc_rule_hb((H:-B),H,B):- !.
afc_rule_hb((H<-B),H,B):- !.
afc_rule_hb((B==>H),H,B):- !.
afc_rule_hb((==>H),H,true):- !.
afc_rule_hb((HB1<==>HB2),(H1,H2),(B1,B2)):- !, (afc_rule_hb((HB1==>HB2),H2,B2);afc_rule_hb((HB2==>HB1),H1,B1)).

:- module_transparent( (get_assertion_head_arg)/3).
get_assertion_head_arg(N,P,E):-get_assertion_head_unnegated(P,PP),!,arg(N,PP,E).

get_assertion_head_unnegated(P,PP):- afc_rule_hb(P,H,_), (afc_unnegate(H,PP)->true;H==PP). 
replace_arg(Q,N,NEW,R):- duplicate_term(Q,R),Q=R,nb_setarg(N,R,NEW).

%% if_missing_mask( +Q, ?R, ?Test) is semidet.
%
% If Missing Mask.
%

if_missing_mask(M:Q,M:R,M:Test):- nonvar(Q),!,if_missing_mask(Q,R,Test).
if_missing_mask(Q,~Q,\+Q):- \+ is_ftCompound(Q),!.

%if_missing_mask(ISA, ~ ISA, \+ ISA):- functor(ISA,F,1),(F==tSwim;call_u(functorDeclares(F))),!.
if_missing_mask(HB,RO,TestO):- once(afc_rule_hb(HB,H,B)),B\==true,HB\==H,!,
     if_missing_mask(H,R,TestO),subst(HB,H,R,RO).

if_missing_mask(ISA, ISA, \+ ISA):- functor(ISA, _F,1),!.% (F==tSwim;call_u(functorDeclares(F))),!.

if_missing_mask(Q,R,Test):-
   which_missing_argnum(Q,N),
   if_missing_n_mask(Q,N,R,Test),!.

if_missing_mask(ISA, ~ ISA, \+ ISA).

%% if_missing_n_mask( +Q, ?N, ?R, ?Test) is semidet.
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


%% which_missing_argnum( +VALUE1, ?VALUE2) is semidet.
%
% Which Missing Argnum.
%
which_missing_argnum(Q,N):- compound(Q),\+ compound_name_arity(Q,_,0),
 must((acyclic_term(Q),is_ftCompound(Q),get_functor(Q,F,A))),
 F\=t,
  (call_u(singleValuedInArg(F,N)) -> true; which_missing_argnum(Q,F,A,N)).

which_missing_argnum(_,_,1,_):-!,fail.
which_missing_argnum(Q,_F,A,N):- between(A,1,N),get_assertion_head_arg(N,Q,Was),is_ftNonvar(Was).


:- multifile(system:term_expansion/4).
system:term_expansion(I,S0,O,S1):- %use_afc_term_expansion, % trace,
 ( \+ current_prolog_flag(afc_term_expansion,false),
  ( \+ \+ (source_location(File,_), atom_concat(_,'.afc.pl',File)) ; current_prolog_flag(afc_term_expansion,true))) ->
 prolog_load_context('term',T)->(T==I->afc_term_expansion(I,O)->I\=@=O->S0=S1, wdmsg(I-->O)).


:- endif.


end_of_file.





















%% is_fc_body( +P) is semidet.
%
% If Is A Forward Chaining Body.
%
is_fc_body(P):- has_body_atom(fwc,P).

%% is_bc_body( +P) is semidet.
%
% If Is A Backchaining Body.
%
is_bc_body(P):- has_body_atom(bwc,P).

%% is_action_body( +P) is semidet.
%
% If Is A Action Body.
%
is_action_body(P):- has_body_atom(wac,P).



%% has_body_atom( +WAC, ?P) is semidet.
%
% Has Body Atom.
%
has_body_atom(WAC,P):- call(
   WAC==P -> true ; (is_ftCompound(P),get_assertion_head_arg(1,P,E),has_body_atom(WAC,E))),!.

/*
has_body_atom(WAC,P,Rest):- call(WAC==P -> Rest = true ; (is_ftCompound(P),functor(P,F,A),is_atom_body_pfa(WAC,P,F,A,Rest))).
is_atom_body_pfa(WAC,P,F,2,Rest):-get_assertion_head_arg(1,P,E),E==WAC,get_assertion_head_arg(2,P,Rest),!.
is_atom_body_pfa(WAC,P,F,2,Rest):-get_assertion_head_arg(2,P,E),E==WAC,get_assertion_head_arg(1,P,Rest),!.
*/


same_functors(Head1,Head2):-must_det(get_unnegated_functor(Head1,F1,A1)),must_det(get_unnegated_functor(Head2,F2,A2)),!,F1=F2,A1=A2.

%% afc_update_literal( +P, ?N, ?Q, ?R) is semidet.
%
% PFC Update Literal.
%
afc_update_literal(P,N,Q,R):-
    get_assertion_head_arg(N,P,UPDATE),call(replace_arg(P,N,Q_SLOT,Q)),
    must(call_u(Q)),update_value(Q_SLOT,UPDATE,NEW), 
    replace_arg(Q,N,NEW,R).


% '$spft'(MZ,5,5,5).

%% update_single_valued_arg(+Module, +P, ?N) is semidet. 
%
% Update Single Valued Argument.
%
:- module_transparent( (update_single_valued_arg)/3).

update_single_valued_arg(M,M:Pred,N):-!,update_single_valued_arg(M,Pred,N).
update_single_valued_arg(_,M:Pred,N):-!,update_single_valued_arg(M,Pred,N).

update_single_valued_arg(world,P,N):- !, current_prolog_flag(afc_shared_module,BaseKB), update_single_valued_arg(BaseKB,P,N).
update_single_valued_arg(M,P,N):- break, \+ clause_b(mtHybrid(M)), trace, clause_b(mtHybrid(M2)),!,
   update_single_valued_arg(M2,P,N).

update_single_valued_arg(M,P,N):- 
  get_assertion_head_arg(N,P,UPDATE),
  is_relative(UPDATE),!,
  dtrace,
  break,
  replace_arg(P,N,OLD,Q),
  must_det_l((clause_u(Q),update_value(OLD,UPDATE,NEW),\+ is_relative(NEW), replace_arg(Q,N,NEW,R))),!,
  update_single_valued_arg(M,R,N).


update_single_valued_arg(M,P,N):- 
 call_u((must_det_l((

  call_u(mtHybrid(M)),
  afc_type_args \= M,
  afc_kb_ops \= M,
  get_assertion_head_arg(N,P,UPDATE),
  replace_arg(P,N,Q_SLOT,Q),
  var(Q_SLOT),
  same_functors(P,Q),
  % current_why(U),
  must_det_l((
     % rtrace(attvar_op(assert_if_new,M:'$spft'(MZ,P,U,ax))),
     % (call_u(P)->true;(assertz_mu(P))),
     assertz(M:P),
     doall((
          lookup_u(M:Q,E),
          UPDATE \== Q_SLOT,
          erase(E),
          afc_unfwc1(M:Q))))))))).

% ======================= 
% utils
% ======================= 

%% map_literals( +P, ?G) is semidet.
%
% Map Literals.
%
map_literals(P,G):-map_literals(P,G,[]).


%% map_literals( +VALUE1, :TermH, ?VALUE3) is semidet.
%
% Map Literals.
%
map_literals(_,H,_):-is_ftVar(H),!. % skip over it
map_literals(_,[],_) :- !.
map_literals(Pred,(H,T),S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,[H|T],S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,H,S):- afc_literal(H),must(apply(Pred,[H|S])),!.
map_literals(_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_literals(Pred,H,S):-H=..List,!,map_literals(Pred,List,S),!.



%% map_unless( :PRED1Test, ?Pred, ?H, ?S) is semidet.
%
% Map Unless.
%
map_unless(Test,Pred,H,S):- call(Test,H),ignore(apply(Pred,[H|S])),!.
map_unless(_Test,_,[],_) :- !.
map_unless(_Test,_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_unless(Test,Pred,(H,T),S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,[H|T],S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,H,S):-H=..List,!,map_unless(Test,Pred,List,S),!.


:- meta_predicate(map_first_arg(*,+)).
%% map_first_arg( +Pred, ?List) is semidet.
%
% PFC Maptree.
%
map_first_arg(CMPred,List):- strip_module(CMPred,CM,Pred), map_first_arg(CM,Pred,List,[]).

:- meta_predicate(map_first_arg(+,*,+,+)).
%% map_first_arg( +Pred, :TermH, ?S) is semidet.
%
% PFC Maptree.
%
map_first_arg(CM,Pred,H,S):-is_ftVar(H),!,CM:apply(Pred,[H|S]).
map_first_arg(_,_,[],_) :- !.
map_first_arg(CM,Pred,(H,T),S):-!, map_first_arg(CM,Pred,H,S), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,(H;T),S):-!, map_first_arg(CM,Pred,H,S) ; map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,[H|T],S):-!, CM:apply(Pred,[H|S]), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,H,S):- CM:apply(Pred,[H|S]). 

%:- fixup_exports.

% % :- ensure_loaded(logicmoo(util/rec_lambda)).

%example afcVerifyMissing(afc_isa(I,D), afc_isa(I,C), ((afc_isa(I,C), {D==C});-afc_isa(I,C))). 
%example afcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});-mudColor(I,C))). 


%% afcVerifyMissing( +GC, ?GO, ?GO) is semidet.
%
% Prolog Forward Chaining Verify Missing.
%
afcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example afc_freeLastArg(afc_isa(I,C),~(afc_isa(I,C))):-is_ftNonvar(C),!.
%example afc_freeLastArg(afc_isa(I,C),(afc_isa(I,F),C\=F)):-!.

%% afc_freeLastArg( +G, ?GG) is semidet.
%
% PFC Free Last Argument.
%
afc_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
afc_freeLastArg(_G,false).


%% afc_current_op_support( +VALUE1) is semidet.
%
% PFC Current Oper. Support.
%
afc_current_op_support((p,p)):-!.


%% afcVersion( +VALUE1) is semidet.
%
% Prolog Forward Chaining Version.
%
%afcVersion(6.6).


% % :- '$set_source_module'(afc_kb_ops).

%% correctify_support( +S, ?S) is semidet.
%
% Correctify Support.
%
correctify_support(U,(U,ax)):-var(U),!.
correctify_support((U,U),(U,ax)):-!.
correctify_support((S,T),(S,T)):-!.
correctify_support((U,_UU),(U,ax)):-!.
correctify_support([U],S):-correctify_support(U,S).
correctify_support(U,(U,ax)).


%% clause_asserted_local( :TermABOX) is semidet.
%
% Clause Asserted Local. 
%
clause_asserted_local(MCL):-
  strip_mz(MCL,MZ,CL),
  must(CL='$spft'(MZ,P,Fact,Trigger )),!,
  clause_u('$spft'(MZ,P,Fact,Trigger),true,Ref),
  clause_u('$spft'(MZ,UP,UFact,UTrigger),true,Ref),
  (((UP=@=P,UFact=@=Fact,UTrigger=@=Trigger))).



%% is_already_supported( +P, ?S, ?UU) is semidet.
%
% If Is A Already Supported.
%
is_already_supported(P,(S,T),(S,T)):- clause_asserted_local('$spft'(_MZ,P,S,T)),!.
is_already_supported(P,_S,UU):- clause_asserted_local('$spft'(_MZ,P,US,UT)),must(get_source_uu(UU)),UU=(US,UT).

% TOO UNSAFE 
% is_already_supported(P,_S):- copy_term_and_varnames(P,PC),sp ftY(PC,_,_),P=@=PC,!.


if_missing1(Q):- afc_literal_nv(Q), call_u( \+ ~ Q), if_missing_mask(Q,R,Test),!, lookup_u(R), Test.


afc_run_pause:- asserta(t_l:afc_run_paused).
afc_run_resume:- retractall(t_l:afc_run_paused).

without_running(G):- (t_l:afc_run_paused->G;locally_tl(afc_run_pause,G)).


