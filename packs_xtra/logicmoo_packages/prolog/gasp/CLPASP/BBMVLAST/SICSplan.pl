%%% Interpreter for the B language
%%% Updated JULY 30th 2010
%%% Faster than the TPLP version (~10x)

:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(terms)).
:-use_module(library(between)).
:-use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%  W  A  R  N  I  N  G  !  !  !       %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% In case no STATIC CAUSAL LAWS (caused) are defined in
%%% the Action Description loaded below, uncomment:
% :- dynamic(caused/2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% LOAD THE ACTION DESCRIPTION

%:-compile(
%'peg_solitaire.txt' %%% ASP COMPETITION 2009
%'15puzzle.txt'   %%% ASP competition 2009
%'hanoi.txt'     %%% ASP competition 2009
%'trucks.txt'  %%% International Planning Competition
%'hydraulic.txt' %%% ASP competition 2009
%'barrels.txt'
%'tangram.txt'
%).

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
wrap_caused(C1,C2) :- caused(C1,C2).
wrap_executable(Act,C) :- executable(Act,C).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
% wrapping (21-06-2010)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%wrap_action(Act) :- action(Act),  executable(Act,_).
%wrap_causes(Act,F,L) :- causes(Act,F,L), executable(Act,_).
%wrap_caused(C1,C2) :- caused(C1,C2).
%wrap_executable(Act,C) :- executable(Act,C).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


sicsplan(N) :-
    M is N + 1,
    main(M,_,_),!.
sicsplan(_) :-
    statistics(runtime,[_,Time]),
    write('No solutions: RunTime: '),write(Time), write('ms '),nl.

main(N) :-
   main(N,_,_),!.
main(_) :-
    statistics(runtime,[_,Time]),
    write('No solutions: RunTime: '),write(Time),write('ms '),nl.

main(N, Actionsocc,States):-
    statistics(_,_),                                       %%%%%%%%%%%%%%%%%%%%%
    setof(F, fluent(F), Lf), setof(A, wrap_action(A), La), %%% Problem Input %%%
    setof(F, initially(F), Init), setof(F, goal(F), Goal), %%%%%%%%%%%%%%%%%%%%%
    make_states(N,Lf,States),
    make_action_occurrences(N,La,Actionsocc),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    (set_initial(Init,States); %%% A.T. DEBUG
      write('Inconsistent initial state'),nl,!,fail),
    write('Initial State:'),nl, States=[S|_],write_state(S),nl,
    (set_goal(Goal,States); %%% A.T.  DEBUG
      write('Inconsistent final state'),nl,!,fail),
    write('Final State:'),nl, append(_,[Last],States),write_state(Last),
    flush_output,!,
    set_transitions(Actionsocc,States),
    write('****transitions   set****'),nl,
    set_executability(Actionsocc,States),    %%% Faster if here !!!!!!!!
    write('****executability set****'),nl,
    get_all_actions(Actionsocc, AllActions),
    length(AllActions,L),
    write('we label '),write(L),write(' variables'),nl,
    sel_vars(Actionsocc,Varsocc),
    statistics(runtime,[_,PT]),
    write('Constraints added in ms: '),write(PT),nl,!,
    my_labeling(Varsocc,States),
    statistics(runtime,[_,Time]),
    write('*************************************************************************'),nl,
    %%% write_state(Last),nl,nl,
    dump_result(Actionsocc,States),
    write('Solution found in Time: '),write(Time),nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meta(MIN) :-
   statistics(runtime,[Ts,_]),
   meta_up(MIN),
   statistics(runtime,[Te,_]),
   T is (Te-Ts)/1000,
   format("***Total time: ~2f s ~n",[T]).

meta_up(N) :-
  (metafail(N),! ;
   format("***Not found Plan of length ~d~n",[N]),
   M is N + 1, meta_up(M)).
metafail(N) :-
    M is N + 1,  main(M,_,_),
    format("***Found Minimal Plan of length ~d~n",[N]),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


my_labeling(Varsocc,States) :-
    %%%no_loop_constraint(States),%%% NEW: added June 14, 2010
    my_labeling(Varsocc,States,1).
    %%% my_labeling_bin(Varsocc).
my_labeling([],_,_) :- !.
my_labeling([CurrVars|Vars], States,I) :-
    %%%random_permutation(CurrVars,RanCurrVars),
    %%%labeling([down],CurrVars),  %%% Change options here. 
    labeling([ffc,down],CurrVars), %%% <= GOOD FOR PUZZLE
    %%%labeling([ffc],CurrVars),   %%% <= GOOD FOR HANOI and TRUCKS
    %%% useless after no_loop_ constraint: 
    no_loop(States,I),
    I1 is I + 1,
    my_labeling(Vars, States,I1).

my_labeling_bin(Varsocc) :-
    term_variables(Varsocc,VARS),
    labeling([ffc,down],VARS).

%%% no_loop check (after instantiation) that the new
%%% state is different from all previous ones

no_loop(States,A) :-
    state_select(A,States,StateA,Firsts),
    nonmember(StateA,Firsts).
    
%%% no_loop_constraint: for each pair of states
%%% s_i, s_j a constraint c_ij is set to 1 if they are
%%% equal (namely the values of the same fluents
%%% is the same in the two states). 
%%% We force all those constraints to be false.

no_loop_constraint(States) :-
   no_loop_constraint(States,C),
   bool_or(C,0).
   
no_loop_constraint([],[]) :- !.
no_loop_constraint([_],[]) :- !.
no_loop_constraint([S|Tates],C) :-
   no_loop_constraint(S,Tates,C1),
   no_loop_constraint(Tates,C2),
   append(C1,C2,C).
   
no_loop_constraint(_,[],[]). 
no_loop_constraint(S,[T|Ates],[Cflag|C2]) :-
    all_equal(S,T,C1),
    bool_and(C1,Cflag),
    no_loop_constraint(S,Ates,C2).
   
all_equal([],[],[]).
all_equal([fluent(A,V)|R],[fluent(A,W)|S],[C1|C]) :-
    C1 #<=> V #= W, %%% reified
    all_equal(R,S,C).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The domain of Each Fluent Variable is set to {0,1}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_states(0,_,[]):-!.
make_states(N,List,[S|STATES]) :-
    N1 is N-1, make_states(N1,List,STATES),
    make_one_state(List,S).
make_one_state([],[]).
make_one_state([F|Fluents],[fluent(F,VarF)|VarFluents]) :-
    make_one_state(Fluents,VarFluents),
    fd_domain_bool(VarF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The domain of each action (occurrence) variable is set to {0,1}
%%% In each state transition exactly one action occur (sum constraint)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_action_occurrences(1,_,[]):-!.
make_action_occurrences(N,List,[Act|ActionsOcc]) :-
    N1 is N-1, make_action_occurrences(N1,List,ActionsOcc),
    make_one_action_occurrences(List,Act),
    get_action_list(Act,AList),
    fd_only_one(AList).
make_one_action_occurrences([],[]).
make_one_action_occurrences([A|Actions],[ action(A,OccA)|OccActions]) :-
    make_one_action_occurrences(Actions,OccActions),
    fd_domain_bool(OccA).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initial state info are set here.
%%% Static causal rules are then applied by "complete_state" and aux
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_initial(List,[InitialState|_]) :-
    set_state(List,InitialState),
    complete_state(InitialState,InitialState).

complete_state([],_).
complete_state( [fluent(Fluent,EV)| Fluents],InitialState) :-
    ( integer(EV), ! ;
    set_one_static_fluent(Fluent, EV, InitialState) ),
    complete_state(  Fluents ,InitialState).

set_one_static_fluent(Name, EV, State) :-
    findall(Po1, wrap_caused(Po1,Name), StatPos),
    static(StatPos, State, PStatPos,EV,p),
    bool_or(PStatPos,PosFired),
    PosFired   #=< EV,  %%% i.e.,  PosFired #=>    EV,
    findall(Ne1, wrap_caused(Ne1,neg(Name)), StatNeg),
    static(StatNeg, State, PStatNeg,EV,n),
    bool_or(PStatNeg,NegFired),
    NegFired + EV #< 2. %%% i.e.,  NegFired #=> #\ EV.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Final state info are set here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_goal(List,States) :-
     last(States,FinalState),
     set_state(List,FinalState).
set_state([],_).
set_state([Fluent|Rest],State) :-
     (Fluent=neg(F),!,member(fluent(F,0),State);
      member(fluent(Fluent,1),State)),
     set_state(Rest,State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% constraints on transitions are set here. First each tranisition
%%% is selected. Then every fluent is analyzed and its new value
%%% is constrained using its previous value and the dynamic and static
%%% rules applied in that state transition.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_transitions(Occurrences,States) :-
   set_transitions(Occurrences,States,FLAGS),
   table(FLAGS, [[0,0,0,0], [0,0,1,0], [0,0,1,1], 
                 [1,0,0,1], [1,1,0,0],[1,1,0,1]]).

set_transitions(_Occurrences,[_States],[]) :- !.
set_transitions([O|Occurrences],[S1,S2|Rest],FLAGS) :-
    set_transition(O,S1,S2,S1,S2,FL),
    set_transitions(Occurrences,[S2|Rest],AGS),
    append(FL,AGS,FLAGS).

set_transition(_Occ,[],[],_,_,[]).
set_transition(Occ,[fluent(Fluent,IV)|R1],[fluent(Fluent,EV)|R2],FromState,ToState,[F|LAGS]):-
    set_one_fluent(Fluent,IV,EV,Occ,FromState,ToState,F),
    set_transition(Occ, R1, R2,FromState,ToState,LAGS).

set_one_fluent(Name,IV,EV, Occurrence,FromSt,ToSt,[EV,PosFired,NegFired,IV]) :-
    findall([X,L],wrap_causes(X,Name,L),DynPos), %%% POSITIVE EFFECTS
    dynamic(DynPos, Occurrence, FromSt,PFormula,EV,p),
    findall(Po1, wrap_caused(Po1,Name), StatPos),  
    static(StatPos, ToSt, PStatPos,EV,p),
    bool_or(PFormula,PStatPos, PosFired),
    findall([Y,M],wrap_causes(Y,neg(Name),M),DynNeg), %% NEGATIVE EFFECTS
    dynamic(DynNeg, Occurrence, FromSt,NFormula,EV,n),
    findall(Ne1, wrap_caused(Ne1,neg(Name)), StatNeg),  
    static(StatNeg, ToSt, PStatNeg,EV,n),
    bool_or(NFormula,PStatNeg, NegFired).
%%% The list [EV,PosFired,NegFired,IV] is passed outside to add the 
%%% following constraints as a table for the new value of the fluent 
%    PosFired * NegFired #= 0,
%    EV #<=>  PosFired #\/ (#\ NegFired  #/\ IV ).
                   

dynamic([],_,_, [],_,_).
dynamic([[Name,Prec]|Rest],Occurrence, State,[ Flag |PF1],EV,Mode):-
    member(action(Name,VA),Occurrence),
    get_precondition_vars(Prec,State,ListPV),
    bool_and([VA|ListPV],Flag), % (VA  #/\ ListPV ) #<=> Flag
    dynamic(Rest,Occurrence, State,PF1,EV,Mode).

static([],_,[],_,_).
static([ Cond | Others], State, [Flag |Fo],EV,Mode) :-
    get_precondition_vars(Cond,State,ListPV),
    bool_and(ListPV,Flag), % Flag #<=> ListPV 
    static(Others,State,Fo,EV,Mode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The executability of each action is then related to the
%%% values of the fluents in the previous state. "formula"
%%% store the disjunction of all executability conditions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_executability(ActionsOcc,States) :-
    findall([Act,C],wrap_executable(Act,C),Conds),
    group_cond(Conds,GroupedConds),
    set_executability(ActionsOcc,States,GroupedConds).


set_executability([],[_],_).
set_executability([ActionsOcc|ARest],[State|States],Conds) :-
    set_executability_sub(Conds,ActionsOcc,State),
    set_executability(ARest,States,Conds).


set_executability_sub([],_,_).
set_executability_sub([[Act,C]|CA],ActionsOcc,State) :-
    member(action(Act,VA),ActionsOcc),
    preconditions_flags(C, State,Flags), 
    bool_or(Flags,F),
    VA #=< F, %%% i.e.,  VA #=> F,
    set_executability_sub(CA,ActionsOcc,State).

preconditions_flags([],_,[]).
preconditions_flags([C|R],State,[Flag|Flags]) :-
      get_precondition_vars(C,State,ListPV),
      bool_and(ListPV,Flag), %% ListPV  #<=> Flag,
      preconditions_flags(R,State,Flags).

%%%%%% AUXILIARY predicates

state_select(0,[State|_],State,[]) :- !.
state_select(N,[State1|States],State,[State1|States1]) :-
    N1 is N - 1,
    state_select(N1, States,State, States1).

group_cond([],[]).
group_cond([[Action,C]|R],[[Action,[C|Cs]]|S]) :-
     findall(L,(member([Action,L],R)),Cs),
     filter(Action,R,Others),
     group_cond(Others,S).
filter(_,[],[]).
filter(A,[[A,_]|R],S) :-
      !, filter(A,R,S).
filter(A,[C|R],[C|S]) :-
      !, filter(A,R,S).

get_precondition_vars([],_,[]).
get_precondition_vars([P1|Rest],State,[F|LR]) :-
    get_precondition_vars(Rest,State,LR),
    (P1 = neg(FluentName),!,
     member(fluent(FluentName,A),State), F #= 1-A;
     member(fluent(P1,F),State)).
get_all_actions([],[]).
get_all_actions([A|B],List) :-
    get_action_list(A,List1),
    get_all_actions(B,List2),
    append(List1,List2,List).
get_action_list([],[]).
get_action_list([action(_,V)|Rest],[V|MRest]) :-
    get_action_list(Rest,MRest).

sel_vars([],[]).
sel_vars([A|B],[C|D]) :-
    term_variables(A,C),
    sel_vars(B,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%      FD auxiliary predicates       %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% (some of them built-in in other Prolog
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fd_domain_bool(X) :- X in 0..1.
sum(X,Res) :- sum(X,#=,Res).
fd_only_one(X) :- sum(X,#=,1).
labelling(AllActions) :-
    labeling([ff,min],AllActions).

bool_or(A,B,C) :- %%% C is the "or" of all elements of A and of B
    append(A,B,D),
    bool_or(D,C).

bool_or([],0) :-!.
bool_or(List,Flag) :-  %%% Flag <-> 1 in List
    maximum(Flag,List).%%%    

bool_and([],1):-!.
bool_and(List,Flag) :- %%% Flag <-> 
    minimum(Flag,List).%%% Lists = [1,1,1...1] 

%%% END of MAIN CODE %%%%%%%

%%% B primitives

neq(A,B) :- A \== B.
diff(A,B) :- A \== B.
diff(A,B,C) :- A \== B, A \== C, C \== B.

interval(In,Min,Max) :- between(Min,Max,In).

diffpair(X1,_,X2,_) :- neq(X1,X2).
diffpair(X,Y1,X,Y2) :- neq(Y1,Y2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dump_result(A,S) :-
    state_plan(A),
    nl,nl,
    dump_result1(A,S).

state_plan([]) :- write('stop.'),nl.
state_plan([A|As]) :-
    find_action(A,Name),
    format("~q -> ",[Name]),
    state_plan(As).

dump_result1([],[S]) :-
    write_state(S).
dump_result1([A|B],[S|Rest]) :-
    write_state(S),
    write_action(A),
    dump_result1(B,Rest).

write_state([]) :- nl.
write_state([fluent(Name,Value)|Rest]) :-
    ( fd_var(Value) -> true; %%% format("~q: unknown  ",[Name]);
      Value == 1 ->  format("~q  ",[Name]);
      true ),
    write_state(Rest).

write_action(A) :-
    find_action(A,Name),
    format(" ---->>   ~q ",[Name]),nl.

find_action([],unknown).
find_action([action(Name,Value)|_], Name) :-
    Value == 1,!.
find_action([_|Rest],Name) :-
    find_action(Rest,Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
