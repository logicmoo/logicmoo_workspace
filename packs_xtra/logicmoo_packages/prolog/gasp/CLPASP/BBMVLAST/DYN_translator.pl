%%% Translator AUGUST, 05th, 2010
%%%% Version with only positive fluent literals
%%%% No static laws are allowed
%%%% - but we added: non_inertial(_,_)

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(between)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% wrapping (08-08-2010)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_executable(Act) :-
    once(executable(Act,_)). %better that findall
%%%% Amsterdam, 22072010
%    findall(X,executable(Act,X),[_|_]).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wrap_action(Act) :- action(Act), is_executable(Act).
wrap_causes(Act,F,L) :- causes(Act,F,L), is_executable(Act).
wrap_executable(Act,C) :- executable(Act,C).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% wrapping 21062010  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%wrap_action(Act) :-       is_executable(Act), action(Act).
%wrap_causes(Act,F,L) :-   is_executable(Act), causes(Act,F,L).
%wrap_executable(Act,C) :- executable(Act,C).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(Input,Len,Output) :-
    format("~n% Reading from file: ~w~n",[Input]),
    format("% Appending to file: ~w~n~n",[Output]),
    tell(Output),
    compile(Input),
    translate(Len),
    seen,told.

main(Input,Len) :-
    integer(Len), Len>0,
    name(Input,IList),
    name(Len,LenList),
    append(["trDFP_",LenList,"moves_",IList],Ilist1),
    name(Output,Ilist1),
    main(Input,Len,Output).

translate(Len) :-
    holds,nl, 
    dump_fluents,nl,
    dump_actions,nl,
    dump_initially,nl,
    make_time(Len),nl,
    axioms(Len),nl,
    make_exec,nl,
    make_causes,nl,
    make_goal(Len),nl,
    ending.

dump_fluents :-
    setof(F,fluent(F),List),
    sub_dump_list(List,fluent).

dump_actions :-
    setof(A,wrap_action(A),List),
    sub_dump_list(List,action).

dump_initially :-
    (setof(F,(initially(F),fluent(F)),List) ; List=[]),!, %%% check
    sub_dump_list(List,initially).

sub_dump_list([],_).
sub_dump_list([X|Xs],Pred):-
    format("~q(~q).~n",[Pred,X]),
    sub_dump_list(Xs,Pred).

make_time(N) :-
    format("time(0..~q).~n",[N]).

axioms(N) :-
    format("1{occ(Act,T):action(Act)}1 :- time(T), T < ~q.~n",[N]),
    format(":- occ(Act,T), action(Act), time(T), not exec(Act,T).~n",[]).

ending :-
    format("#hide.~n #show occ(A,B).~n",[]).

holds :-
    format("hold(Fl,0) :- initially(Fl).~n",[]),
    format("hold(Fl,T+1) :- time(T), fluent(Fl), occ(Act,T), causes(Act,Fl), possible(Act,Fl,T), exec(Act,T).~n",[]),
    format("hold(Fl,T+1) :- time(T), fluent(Fl), hold(Fl,T), not non_inertial(Fl,T+1).~n",[]),
    format("non_inertial(Fl,T+1):-  time(T), fluent(Fl), occ(Act,T), causes(Act,neg(Fl)), possible(Act,neg(Fl),T), exec(Act,T).~n",[]).
    %%% format(":-  hold(Fl,T+1), time(T), fluent(Fl), occ(Act,T), causes(Act,neg(Fl)), possible(Act,neg(Fl),T), exec(Act,T).~n",[]).

%%%%%%%%%%%%%%%%
    
make_exec :-
    setof(A,wrap_action(A),List),
    sub_make_exec(List).
sub_make_exec([]).
sub_make_exec([A|B]) :-
    sub_make_exec_oneAction(A),
    sub_make_exec(B).
sub_make_exec_oneAction(A) :-
    wrap_executable(A,List),
    once((format("exec(~q,T) :- time(T)",[A]),
          sub_submake_exec(List),
          format(".~n",[])) ),
    fail.
sub_make_exec_oneAction(_).

sub_submake_exec([]).
sub_submake_exec([neg(A)|B]) :-
    !,
    format(", not hold(~q,T) ",[A]),
    sub_submake_exec(B).
sub_submake_exec([A|B]) :-
    format(",hold(~q,T) ",[A]),
    sub_submake_exec(B).

make_causes :-
    wrap_causes(Act,Flu,Conds),
    format("causes(~q,~q).~n",[Act,Flu]),
    format("possible(~q,~q,T) :- time(T)",[Act,Flu]),
    sub_make_causes(Conds), fail.
make_causes.
    

sub_make_causes([]) :-
    format(".~n",[]).
sub_make_causes([neg(A)|B]) :-
    !,
    format(", not hold(~q,T)", [A]),
    sub_make_causes(B).
sub_make_causes([A|B]) :-
    format(", hold(~q,T)", [A]),
    sub_make_causes(B).
    
make_goal(N) :-
    (setof(G, goal(G), Fluents ) ; Fluents=[]),!,
    format(":- not goal.~n",[]),
    format("goal :- 1==1 ",[]),
    sub_make_goal(N,Fluents).
sub_make_goal(_N,[]) :-
    format(".~n",[]).
sub_make_goal(N,[neg(A) |B]) :- !,
    format(", not hold(~q,~q)",[A,N]),
    sub_make_goal(N,B).
sub_make_goal(N,[A|B]) :-
    format(", hold(~q,~q)",[A,N]),
    sub_make_goal(N,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neq(A,B) :- A \== B.
diff(A,B) :- A \== B.
diff(A,B,C) :- A \== B, A \== C, C \== B.
interval(In,Min,Max) :- between(Min,Max,In).
diffpair(X1,_,X2,_) :- diff(X1,X2).
diffpair(X,Y1,X,Y2) :- diff(Y1,Y2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
