
:- module(ec_common,[]).

is_sicstus:- \+ current_prolog_flag(version_data,swi(_,_,_,_)).

?- is_sicstus -> prolog_flag(single_var_warnings,_,off) ; true.

?- is_sicstus -> ensure_loaded(ec_common_sicstus) ; ensure_loaded(ec_common_swi).

:- style_check(-singleton).
/* Emulates the writeNoln(1) function */

% =========================================
% Axiom Access
% =========================================

:- module_transparent(local_database/1).
:- export(local_database/1).
:- system:import(local_database/1).

:- module_transparent(ec_current_domain_db1/1).
:- export(ec_current_domain_db1/1).
:- system:import(ec_current_domain_db1/1).

:- module_transparent(ec_current_domain_db_name/1).
:- dynamic(ec_current_domain_db_name/1).
:- export(ec_current_domain_db_name/1).
:- system:import(ec_current_domain_db_name/1).

:- module_transparent(user:ec_current_domain_db2/2).
:- dynamic(user:ec_current_domain_db2/2).
:- export(user:ec_current_domain_db2/2).
:- system:import(user:ec_current_domain_db2/2).


:- reexport((ec_loader)).
:- use_module((ec_loader)).

 

% =========================================
% Test Decls
% =========================================

fail_solve_goal(G,R):- \+ abdemo_solve(G,R).

:- export(ec_prove/1).
ec_prove(G):-
  abdemo_solve(G,_).

:- export(abdemo_solve/2).
abdemo_solve(Gs,R):- abdemo_solve(Gs,R,1,20).
:- export(abdemo_solve/4).
abdemo_solve(Gss,R,H,L):- 
/*  When = now,
  must(fix_goal(When,Gs,Gs0)), !,
  must(fix_time_args(When,Gs0,Gss)), !,  
  dbginfo(all, [nl,realGoal=Gss,nl]),*/
  abdemo_special(depth(H,L), Gss, R).


:- meta_predicate test_body(*,0,*,*).
test_body(N,(Was=G,Body),Info,Vs):- Was==N,!, copy_term(G,NewN),!,Was=G, test_body(NewN,(Body),Info,Vs).
test_body(N,true,Info,Vs):- !, test_body(N,abdemo_solve(N,R),Info,['R'=R|Vs]).
test_body(N,Body,Info,Vs):-
   dbginfo(verbose, nl(2)),
   dbginfo(verbose, [Info,nl(1)]),
   %dbginfo(all,['body'=Body,nl(2)]),
   % copy_term(Body,BodyA),
   maybe_nl, write('START OUTPUT of '), write(N), write(' >>>>>'),
   maybe_nl, 
   ticks(Z1),   
   (call(Body)-> (Passed = true) ; Passed = '?!?!??!FAILED!?!?!?'),
   ticks(Z2), TotalTime is (Z2-Z1)/1000, 
   maybe_nl, write('<<<<< ENDOF OUTPUT of '), write(N),nl,
   dbginfo(verbose, nl(2)),
   % dbginfo(all,['bodyAgain'=BodyA, nl, nl, Vs]),
   dbginfo(all,Vs),
   maybe_nl, nl, 
   (Passed == true -> ansi_format(fg(cyan),'!!!PASSED!!! ~w time=~w',[N,TotalTime]) ;
     (ansi_format(hfg(red),'~p ~w time=~w',[Passed,N,TotalTime]),sleep(1.0))),
   nl,
   dbginfo(verbose, nl(2)).

do_tests:- 
  clause_w_names(do_test(N),Body,_Ref,File,Vs),
  once(test_body(N,Body,File,Vs)),
  fail.
do_tests:- current_prolog_flag(debug,false) -> halt(7) ; true.
:- export(do_tests/0).

% =========================================
% Debug Info
% =========================================

:- use_module('./ec_reader').

:- set_ec_option(verbose, all).
:- set_ec_option(extreme, false).
:- set_ec_option(debug, failure).

is_dbginfo(N):- var(N),!, fail.
is_dbginfo(N=V):- !, etmp:ec_option(N, V).
is_dbginfo(not(N)):- !, \+ is_dbginfo(N).
is_dbginfo(N):- is_list(N), !, maplist(is_dbginfo,N).
is_dbginfo(N):- etmp:ec_option(N, false),!,fail.
is_dbginfo(N):- etmp:ec_option(verbose, N),!.


maybe_nl:- notrace(format('~N',[])).

dbginfo(NV, G):- notrace(tracing), !,notrace, dbginfo(NV, G), trace.
dbginfo(NV, G):- \+ is_dbginfo(NV) -> true ; dbginfo(G). 
:- export(dbginfo/1).
dbginfo_else(NV,G,E):- is_dbginfo(NV) -> dbginfo(G); dbginfo(E).

:- meta_predicate catch_ignore(0).
catch_ignore(G):- ignore(catch(G,E,wdmsg(E))),!.

dbginfo(G):- notrace(tracing),!,notrace,dbginfo(G),trace.
dbginfo(Var):- var(Var),!, maybe_nl, format('ListVAR = ~p~n',[Var]).
dbginfo([]):- !, maybe_nl. 
dbginfo(call(G)):- !, catch_ignore(G).
dbginfo({G}):- !, maybe_nl, catch_ignore(G).
dbginfo([A|B]):- !, maybe_nl, dbginfo(A), maybe_nl, dbginfo(B),!.
dbginfo(N=V):- \+compound(V),!, maybe_nl, catch_ignore(portray_clause(N=V)).
dbginfo(N=V):- !, maybe_nl, catch_ignore(portray_clause(N:-V)).
dbginfo(nl):- !, maybe_nl, nl.
dbginfo(nl(N)):- !, maybe_nl, catch_ignore(forall(between(0,N,_),nl)).
dbginfo(fmt(F,A)):- !, catch_ignore(format(F,A)).
dbginfo(afmt(Ansi,F,A)):- !, catch_ignore(ansi_format(Ansi,F,A)).
dbginfo(NV):- catch_ignore(portray_clause(:- NV)), !.
:- export(dbginfo/1).


% =========================================
% Plan Portrayal
% =========================================

write_plan_len(A,B):- length(A,AL), length(B,BL),write('Lengths of '), write_plan(AL,BL).
write_plan(HA,BA):- write('Plan: '), write(HA), write('-'), write(BA), write('    ').
/* Emulates the writeNoln(1) function */

% writeNoln(A) :- \+ atom(A),!,writeYesln(A).
writeNoln(A) :- fresh_line, write(A).
% writeNoln(_).

% writeYesln(A):- atom(A),!,writeNoln(A).
writeYesln(A):- write(A),fresh_line.

writenl(A):- write(A),fresh_line.

user:file_search_path(ec,D):- catch( (findall(D,relative_from_ec(D),L),dedupe_files(L,S),member(D,S)),_,fail).
relative_from_ec(F):- nb_current('$ec_input_file', F), atom(F), F \== [].
relative_from_ec(D):- catch( (expand_file_search_path(library('ec_planner'),D)),_,fail),exists_directory(D).
relative_from_ec(D):- catch( (expand_file_search_path(library('ec_planner/../../ext/ec_sources'),D)),_,fail),exists_directory(D).
relative_from_ec(D):- catch( (expand_file_search_path(library('../test/ec_planner/'),D)),_,fail),exists_directory(D).
relative_from_ec(D):- catch( (expand_file_search_path(library('../test/ec_planner/ectest'),D)),_,fail),exists_directory(D).


ec_predicate_template(axiom(_,_)).
ec_predicate_template(predicate(_)).
ec_predicate_template(fluent(_)).
ec_predicate_template(event(_)).
ec_predicate_template(action(_)).
ec_predicate_template(function(_,_)).
ec_predicate_template(Var):- local_database(predicate(Pred)), functor(Pred,F,A), functor(Var,F,A).

local_database(Var):- notrace(var(Var)),!,ec_predicate_template(Var),ec_current_domain_db1(Var).
local_database(G):- ec_current_domain_db1(G).

%local_database(Var):- ec_current_domain_db1(Var).

system:show_ec_current_domain_db:- 
   pprint_ecp_cmt(yellow,showing_ec_current_domain_db),
   forall(ec_current_domain_db1(G),pprint_ecp(yellow,G)),
   pprint_ecp_cmt(yellow,shown_ec_current_domain_db).

   listing(user:ec_current_domain_db2/2).



user_ec_current_domain_db(axiom(G,Gs)):- nonvar(G), !, ec_current_domain_axiom(G,Gs).
user_ec_current_domain_db(G):- user:ec_current_domain_db2(G,_).

ec_current_domain_axiom(call(G), []):- nonvar(G),!, call(G).
ec_current_domain_axiom(G, Gs):- user:ec_current_domain_db2(axiom(G,Gs),_).
ec_current_domain_axiom(holds(G,Zero), Gs):-  once(get_zero(Zero)), ec_current_domain_axiom(initially(G),Gs).
%ec_current_domain_axiom(initially(G), Gs):-  once(get_zero(Zero)), ec_current_domain_axiom(holds(G,Zero),Gs).


ec_current_domain_db1(G):- user_ec_current_domain_db(G).
ec_current_domain_db1(G):- lps_current_domain_db(G).
ec_current_domain_db1(G):- var(G), !, fail.
ec_current_domain_db1(event(G)):- lps_current_domain_db(action(G)).
ec_current_domain_db1(G):- G \= axiom(_,_),!, ec_current_domain_db1(axiom(G,B)), B==[].
%ec_current_domain_db1(Var):- notrace(var(Var)),!, throw(ec_current_domain_var(Var)).
%ec_current_domain_db1(axiom(G,Gs)):- !, axiom(G,Gs).
%ec_current_domain_db1(event(G)):- var(G),!, ec_current_domain_db1(predicate(Ax)), functor(Ax,F,A), functor(G,F,A).


:- lock_predicate(ec_current_domain_db1/1).


% :- use_module(library(lps_corner)). % ,[u_call_lps/1]).

:- multifile(u_call_contrib/2).
:- dynamic(u_call_contrib/2).
:- module_transparent(u_call_contrib/2).
u_call_contrib(M,G):- user:ec_current_domain_db2(G,M).


%lps_call(G):- current_predicate(_,G), !, call(G).
lps_call(G):- catch(interpreter:u_call_lps(G),_,fail).
lps_current_domain_db(event(F)):- lps_call(events(List)),member(F,List).
lps_current_domain_db(action(F)):- lps_call(actions(List)),member(F,List). %lps_call(action(F)).
lps_current_domain_db(action(F)):- lps_call(action(F)).
lps_current_domain_db(axiom(Head, Body)):- lps_axiom(Head,Body).


lps_axiom(happens(Head,T),[/*holds(true,T)*/]):- lps_call(observe(Heads, T)), member(Head,Heads).
lps_axiom( initiates(Action,Fluent, T), Pre):- lps_call(initiated(happens(Action, T, _T2), Fluent, Pre)).
lps_axiom(terminates(Action,Fluent, T), Pre):-   lps_call(terminated(happens(Action, T, _T2), Fluent, Pre)).

lps_axiom(holds(G,Zero), Gs):-  once(get_zero(Zero)), lps_axiom(initially(G),Gs).
lps_axiom(initially(F), []):- lps_call(initial_state(List)),member(F,List).

lps_axiom(Head,Body):- lps_call(reactive_rule(Body,Heads)), member(Head,Heads).
lps_axiom(Head,Body):- lps_call(l_timeless(Head, Body)).
lps_axiom(Head,Body):- lps_call(l_int(Head, Body)).
lps_axiom(Head,Body):- lps_call(l_events(Head, Body)). % Head=happens(_Event, _T1, _T2),
lps_axiom(Head,Body):- lps_d_axiom(Head,Body).


%ec_current_domain_db1(axiom(Head, Body)):- fail, lps_call(d_pre(Cond)), select(Item,Conds,Body), opposite(Item,Head).
% Possible Blockers
lps_d_axiom(Head,[BlockerCond|Body]):- 
   lps_call(d_pre(Conds)), 
   select(Head,Conds,Rest), term_variables(Head,HeadVars), 
   select(Blocker,Rest,Body), 
   (functor(Head,happens,_)-> \+ functor(Blocker,happens,_) ; true),
   d_pre_w_vars(HeadVars,Blocker),
   opposite(Blocker,BlockerCond).

d_pre_w_vars([],_Blocker):-!.
d_pre_w_vars(HeadVars,Blocker):- term_variables(Blocker,BlockerVars),
   member(BV,BlockerVars),member(HV,HeadVars), HV==BV,!.






% ec_current_domain_db1(predicate(F)):- lps_call(ext_prolog_predicates(List)),member(F,List).

/*
ec_current_domain_db1(axiom(..)):-  lps_call(l_events(happens(eat_food(A), B, _), [holds(hunger(A, 20), B)]).

fluents([left(A), right(A), searching(_)]).

l_int(holds(found(A), B), [holds(left(A), B), holds(right(A), B)]).
l_events(happens(eat_food(A), B, _), [holds(hunger(A, 20), B)]).
l_events(happens(do_sample, A, B), [holds(left(C), A), holds(right(D), _), E is (D+C)div 2, happens(sample(E), _, B)]).
d_pre([happens(collect_wood(A, _, _, _), B, C), happens(collect_wood(A, _, _, _), B, C)]).
d_pre([happens(collect_wood(A, _, _, _), B, C), happens(change_animal(A, _, _, _, _, _, _), B, C)]).
d_pre([happens(break_tree(A, _, _), B, C), happens(turn_hit_shelter(A, _, _, _, _, _, _, _, _), B, C)]).
d_pre([happens(break_tree(A, _, _), B, C), happens(turn_hit(A, _, _, _, _, _, _), B, C)]).
d_pre([happens(break_tree(A, _, _), B, C), happens(hit_shelter(A, _, _, _, _, _), B, C)]).
d_pre([happens(break_tree(A, _, _), B, C), happens(hit_from(A, _, _, _), B, C)]).
d_pre([happens(break_tree(A, _, _), B, C), happens(eat(A, _, _, _, _, _, _), B, C)]).
d_pre([happens(break_tree(A, _, _), B, C), happens(collect_wood(A, _, _, _), B, C)]).
d_pre([happens(break_tree(A, _, _), B, C), happens(collect_food(A, _, _, _, _, _, _, _, _), B, C)]).
d_pre([happens(break_tree(A, _, _), B, C), happens(change_animal(A, _, _, _, _, _, _), B, C)]).
*/

:- fixup_exports.


