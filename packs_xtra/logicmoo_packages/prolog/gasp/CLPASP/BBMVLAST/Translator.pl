%%% Translator DECEMBER 2012
%%% Last tested in B Prolog 
%%% Comments for SICStus and SWI 
%%% Added handling of non_inertial fluents (like reachability in sokoban)

%%%% For SICStus Prolog:
%:- use_module(library(lists)).
%:- use_module(library(system)).
%:- use_module(library(between)).
%:- prolog_flag(unknown,_,fail).

%%% For B Prolog
:- set_prolog_flag(unknown,fail).
:- set_prolog_flag(warning,off).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wrap_action(Act) :- action(Act), is_executable(Act).
wrap_causes(Act,F,L) :- causes(Act,F,L), is_executable(Act).
wrap_caused(C1,C2) :- caused(C1,C2).
wrap_executable(Act,C) :- executable(Act,C).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(Input,Len,Output) :-
    format("~n% Reading from file: ~w~n",[Input]),
    format("% Appending to file: ~w~n~n",[Output]),
    tell(Output),
    %% SISCTUS/SWI
    %compile(Input),
    %% BProlog
    cl(Input),
    translate(Len),
    seen,told.

%%% Default output name:

main(Input,Len) :-
    integer(Len), Len>0,
    name(Input,IList),
    name(Len,LenList),
    append(["trDFP_",LenList,"moves_",IList],Ilist1),
    name(Output,Ilist1),
    main(Input,Len,Output).

%%% Main predicate

translate(Len) :-
    dump_fluents,nl,
    dump_inertial,nl,
    dump_actions,nl,
    dump_initially,nl,
    make_time(Len),nl,
    axioms(Len),nl,
    holds,nl,
    make_exec,nl,
    make_causes,nl,
    make_caused,nl,
    make_goal(Len),nl,
    ending.

dump_fluents :-
    setof(F,fluent(F),List),
    sub_dump_list(List,fluent).

dump_inertial :-
    (non_inertial(_),!,
     setof(NI,non_inertial(NI),List);
     List=[]),
     sub_dump_list(List,non_inertial).
    
dump_actions :-
    setof(A,wrap_action(A),List),
    sub_dump_list(List,action).

dump_initially :-
    setof(F,initially(F),List),
    sub_dump_list(List,initially).

sub_dump_list([],_).
sub_dump_list([X|Xs],Pred):-
    format("~q(~q).~n",[Pred,X]),
    sub_dump_list(Xs,Pred).

make_time(N) :-
    format("time(0..~q).~n",[N]).


axioms(N) :-
    format("literal(Fl) :- fluent(Fl).~n",[]),
    format("literal(neg(Fl)) :- fluent(Fl).~n",[]),
    %%%% NEW - february 7, 2012
    format("inertial(L) :- literal(L), not non_inertial(L).~n",[]),
    format("non_inertial(neg(F)) :- fluent(F), non_inertial(F).~n",[]),
    %%%%
    format("1{occ(Act,Ti):action(Act)}1 :- time(Ti), Ti < ~q.~n",[N]),
    format("opposite(Fl,neg(Fl)) :- fluent(Fl).~n",[]),
    format("opposite(neg(Fl),Fl) :- fluent(Fl).~n",[]),
    format(":- time(Ti), fluent(Fl), hold(Fl,Ti), hold(neg(Fl),Ti).~n",[]),
    format(":- occ(Act,Ti), action(Act), time(Ti), not exec(Act,Ti).~n",[]).

 
ending :-
    format("#hide exec(U,V).~n #hide initially(U).~n #hide literal(V).~n",[]),
    format("#hide fluent(V).~n #hide inertial(V).~n #hide non_inertial(V).~n #hide action(V). ~n #hide opposite(V,U). ~n",[]),
    format("#hide causes(V,U).~n #hide possible(U,V,W). ~n #hide time(U).~n",[]),
    format("#hide caused(U,V). ~n #hide hold(U,V).~n #hide goal.~n",[]).
    
holds :-
    initial_hold,
    regular_hold.

initial_hold :-
    format("hold(Fl,0) :- initially(Fl).~n",[]).

regular_hold :-
    format("hold(Fl,Ti+1) :- time(Ti), literal(Fl), occ(Act,Ti), causes(Act,Fl), possible(Act,Fl,Ti), exec(Act,Ti).~n",[]),
    format("hold(Fl,Ti) :- time(Ti), Ti>0, literal(Fl), caused(Ti,Fl).~n",[]),
    %%% CHANGED HERE
    format("hold(Li,Ti+1) :- time(Ti), literal(Li), inertial(Li), hold(Li,Ti), opposite(Li,Lu), not hold(Lu,Ti+1).~n",[]),
    format("hold(neg(F),Ti) :- time(Ti), Ti>0, fluent(F), non_inertial(F), hold(Li,Ti), literal(Li), not hold(F,Ti).~n",[]).


make_exec :-
    setof(A,wrap_action(A),List),
    sub_make_exec(List).
sub_make_exec([]).
sub_make_exec([A|B]) :-
    sub_make_exec_oneAction(A),
    sub_make_exec(B).
sub_make_exec_oneAction(A) :-
    wrap_executable(A,List),
    once((format("exec(~q,Ti) :- time(Ti)",[A]),
          sub_submake_exec(List),
          format(".~n",[])) ),
    fail.
sub_make_exec_oneAction(_).


sub_submake_exec([]).
sub_submake_exec([A|B]) :-
    format(",hold(~q,Ti) ",[A]),
    sub_submake_exec(B).


make_causes :-
    wrap_causes(Act,Flu,Conds),
    format("causes(~q,~q).~n",[Act,Flu]),
    format("possible(~q,~q,Ti) :- time(Ti)",[Act,Flu]),
    sub_make_causes(Conds), fail.
make_causes.

sub_make_causes([]) :-
    format(".~n",[]).
sub_make_causes([A|B]) :-
    format(", hold(~q,Ti)", [A]),
    sub_make_causes(B).

make_caused :-
    wrap_caused(Cond,Fl),
    format("caused(Ti,~q) :- time(Ti) ",[Fl]),
    sub_make_caused(Cond), fail.
make_caused.

sub_make_caused([]) :-
    format(".~n",[]).
sub_make_caused([A|B]) :-
    format(", hold(~q,Ti)",[A]),
    sub_make_caused(B).


make_goal(N) :-
    setof(G, goal(G), [Fluent|Fluents]),
    format(":- not goal.~n",[]),
    format("goal :- hold(~q,~q)",[Fluent,N]),
    sub_make_goal(N,Fluents).
sub_make_goal(_N,[]) :-
    format(".~n",[]).
sub_make_goal(N,[A|B]) :-
    format(", hold(~q,~q)",[A,N]),
    sub_make_goal(N,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% wrapping (08-08-2010)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_executable(Act) :-
    once(executable(Act,_)). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neq(A,B) :- A \== B.
diff(A,B) :- A \== B.
diff(A,B,C) :- A \== B, A \== C, C \== B.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interval(In,Min,Max) :- between(Min,Max,In).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% BINARY APPEND (built-in in some Prolog)

append([],[]).
append([A],A).
append([A,B],C) :-  !, append(A,B,C).
append([A,B|R],S) :-    append(A,B,T),   append([T|R],S).

%%%

