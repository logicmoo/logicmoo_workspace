
:- module(ec_common,[]).

is_sicstus:- \+ current_prolog_flag(version_data,swi(_,_,_,_)).

?- is_sicstus -> prolog_flag(single_var_warnings,_,off) ; true.

?- is_sicstus -> ensure_loaded(ec_common_sicstus) ; ensure_loaded(ec_common_swi).

:- style_check(-singleton).
/* Emulates the writeNoln(1) function */

% =========================================
% Axiom Access
% =========================================

:- module_transparent(ec_current_domain/1).
:- export(ec_current_domain/1).
:- system:import(ec_current_domain/1).

:- module_transparent(ec_current_domain_bi/1).
:- export(ec_current_domain_bi/1).
:- system:import(ec_current_domain_bi/1).

:- module_transparent(ec_current_domain_db/1).
:- export(ec_current_domain_db/1).
:- system:import(ec_current_domain_db/1).

:- module_transparent(user:ec_current_domain_db/2).
:- dynamic(user:ec_current_domain_db/2).
:- export(user:ec_current_domain_db/2).
:- system:import(user:ec_current_domain_db/2).


:- reexport(library(ec_planner/ec_loader)).
:- use_module(library(ec_planner/ec_loader)).

ec_predicate_template(Var):- ec_current_domain(predicate(Pred)), functor(Pred,F,A), functor(Var,F,A).

ec_current_domain(Var):- notrace(var(Var)),!,ec_predicate_template(Var),ec_current_domain(Var).
ec_current_domain(Var):- ec_current_domain_bi(Var).
%ec_current_domain(Var):- ec_current_domain_db(Var).

ec_current_domain_bi(Var):- notrace(var(Var)),!, throw(ec_current_domain_var(Var)).
%ec_current_domain_bi(axiom(G,Gs)):- !, axiom(G,Gs).
ec_current_domain_bi(G):- ec_current_domain_db(G).
ec_current_domain_bi(G):- ec_current_domain_db(axiom(G,B)), B==[].
ec_current_domain_bi(executable(G)):- var(G), ec_current_domain_bi(event(Ax)), functor(Ax,F,A), functor(G,F,A).
ec_current_domain_bi(executable(G)):- compound(G), functor(G,F,A), functor(Ax,F,A), ec_current_domain_bi(event(Ax)).

ec_current_domain_db(axiom(call(G), [])):- nonvar(G),rtrace(G).
ec_current_domain_db(G):- user:ec_current_domain_db(G, _REF).
:- lock_predicate(ec_current_domain_db/1).
 

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

run_tests:- 
  clause_w_names(do_test(N),Body,_Ref,File,Vs),
  once(test_body(N,Body,File,Vs)),
  fail.
run_tests:- current_prolog_flag(debug,false) -> halt(7) ; true.
:- export(run_tests/0).

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

write_plan_len(A,B):- length(A,AL), length(B,BL),write_plan(AL,BL).
write_plan(HA,BA):- write('Plan: '), write(HA), write('-'), write(BA), write('    ').
/* Emulates the writeNoln(1) function */

% writeNoln(A) :- \+ atom(A),!,writeYesln(A).
writeNoln(A) :- fresh_line, write(A).
% writeNoln(_).

% writeYesln(A):- atom(A),!,writeNoln(A).
writeYesln(A):- write(A),fresh_line.

writenl(A):- write(A),fresh_line.

:- fixup_exports.

