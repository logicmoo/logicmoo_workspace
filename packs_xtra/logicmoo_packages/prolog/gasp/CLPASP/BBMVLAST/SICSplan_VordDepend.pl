%
%%% Interpreter for the B language%
%
%   Updated AUGUST 8th 2010
%	Seconda versione di riordino
%	statico delle variabili per il labeling.
%	Un ordine distinto per ogni passo.
%	L'ordine dipende dal peso delle azioni
%       Il peso e' calcolato in base all'overlapping
%       tra causes di una azione e goal della theory.
%       Ad ogni azione Act viene associato un ordine diverso
%       da essere utilizzato nel labeling per il
%       passo che segue Act. L'ordine associato ad una
%       azione Act, privilegia le Act' che hanno tra le
%       causes-precondition fluenti-lits presenti tra gli 
%       effetti di Act. 
%       Tutti gli ordini sono precalcolati e memorizzati
%       in un log-array.
%
%
%

%%% Updated JUNE 24th 2010

%invocare prima di consult: prolog_flag(compiling,_,profiledcode).
%:-use_module(library(gauge)).

:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(terms)).
:-use_module(library(ordsets)).
:-use_module(library(between)).
:-use_module(library(logarr)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%  W  A  R  N  I  N  G  !  !  !       %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% In case no STATIC CAUSAL LAWS (caused) are defined in
%%% the Action Description loaded below, uncomment:
%:- dynamic(caused/2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% LOAD THE ACTION DESCRIPTION

%:-compile(
%'act.txt'
%'hanoi.txt'     %%% ASP competition 2009
%'hydraulic.txt' %%% ASP competition 2009
%'15puzzle.txt'
%'15puzzle_rev.txt'
%'pipes_B.lp'  %%% From IPC4: too large!
%'encoding.lp'
%'barrels.txt'
%'barrelsVoid.txt'
%'puzzle_9.txt'
%'puzzle_9_zero.txt'
%'orderedBlocks.txt'
%'capracavolo.txt'
%'peg_solitaire.txt'
%'langford.txt'
%'traffic_jam.txt'
%'game_light.txt'
%'saw.pl'
%'trucks_p02.txt'
%'trucks.txt'
%'tangram_B.pl'
%'tangram280710.txt'
%).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_executable(Act) :-
    once(executable(Act,_)). %better that findall
%%%% Amsterdam, 22072010
%    findall(X,executable(Act,X),[_|_]).
%
%%%
% wrapping (21-06-2010)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wrap_action(Act) :- action(Act), is_executable(Act).
wrap_causes(Act,F,L) :- causes(Act,F,L), is_executable(Act).
wrap_caused(C1,C2) :- caused(C1,C2).
wrap_executable(Act,C) :- executable(Act,C).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%
% % wrapping (21-06-2010)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% wrap_action(Act) :- action(Act),  executable(Act,_).
% wrap_causes(Act,F,L) :- causes(Act,F,L), executable(Act,_).
% wrap_caused(C1,C2) :- caused(C1,C2).
% wrap_executable(Act,C) :- executable(Act,C).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
    statistics(_,_),
    mysetof(F, fluent(F), Lf),            %%%%%%%%%%%%%%%%%%%%%
    mysetof(A, wrap_action(A), SortLa),   %%% Problem Input %%%
    mysetof(F, initially(F), Init),       %%%%%%%%%%%%%%%%%%%%%
    mysetof(F, goal(F), SortGoal),
    length(SortLa,NumActs),
    length(Lf,NumFlus),
    %SortLa, SortGoal assumed to be sorted in what follows

    format("Action theory:~n    fluents: ~w ~n    actions: ~w ~n",[NumFlus,NumActs]),
    statistics(runtime,[_,ReadTime]),
    format("Read in ~w ms~n",[ReadTime]),
    format("Looking for a trajectory with ~w states~n",[N]),

    statistics(runtime,[StartSAnal,_]),
    
    %%% ATTIVARE UNA RIGA:
    %%% Con analisi statica e riordino delle variabili, [leftmost,down]-labeling:
    static_anal(SortLa,SortGoal, ReorderedLa, MultiOrdersWithVars), StatAnal = statanal,
    %%% Senza analisi statica e riordino delle variabili:
     %  ReorderedLa = SortLa, StatAnal = nostatanal, MultiOrdersWithVars=[],
    
    statistics(runtime,[EndSAnal,_]),
    TSA is EndSAnal-StartSAnal,
    format("Time for static analysis:~w ms~n",[TSA]),

    make_states(N,Lf,States),
    make_action_occurrences(N,ReorderedLa,Actionsocc,ActIdxOfSteps),
    %ActIdxOfSteps e' una lista di var/interi, uno per ogni transizione,
    %l'I-esimo intero vale N se al passo I stata eseguita la N-th azione
    %nella lista ReorderedLa
 
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    (set_initial(Init,States); %%% A.T. DEBUG
      write('Inconsistent initial state'),nl,!,fail),
    write('Initial State:'),nl, States=[S|_],write_state(S),nl,
    (set_goal(SortGoal,States); %%% A.T.  DEBUG
      write('Inconsistent final state'),nl,!,fail),
    write('Final State:'),nl, append(_,[Last],States),write_state(Last),
    !,
    set_transitions(Actionsocc,States),
    write('****transitions   set****'),nl,
    set_executability(Actionsocc,States),    %%% Faster if here !!!!!!!!
    write('****executability set****'),nl,
    get_all_actions(Actionsocc, AllActions),
    length(AllActions,L),
    write('We label '),write(L),write(' variables'),nl,
    statistics(runtime,[_,PT]),
    write('Constraints added in ms: '),write(PT),nl,!,
    sel_vars(Actionsocc,Varsocc, StatAnal),

    do_my_labeling(StatAnal,Varsocc,States,ActIdxOfSteps,MultiOrdersWithVars),

    statistics(runtime,[_,Time]),
    write('*************************************************************************'),nl,
    %%% write_state(Last),nl,nl,
    dump_result(Actionsocc,States),
    write('Solution found in Time: '),write(Time),nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%a setof not failing if no solution exists
mysetof(A,B,C) :-
	(setof(A, B, C) ; C=[]), !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meta(MIN) :-
   statistics(runtime,[Ts,_]),
   meta_up(MIN),
   statistics(runtime,[Te,_]),
   T is (Te-Ts)/1000,
   format("***Total time: ~2f s ~n~n~n",[T]).

meta_up(N) :-
  (metafail(N),! ;
   format("***Not found Plan of length ~d~n~n~n",[N]),
   M is N + 1, meta_up(M)).
metafail(N) :-
    M is N + 1,  main(M,_,_),
    format("***Found Minimal Plan of length ~d~n~n~n",[N]),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


do_my_labeling(nostatanal,Varsocc,States,ActIdxOfSteps,_) :-
    %no_loop_constraint(States),%%% NEW: added June 14, 2010
    my_labeling(Varsocc,States,1,ActIdxOfSteps).
    %%% my_labeling_bin(Varsocc).
do_my_labeling(statanal,Varsocc,States,ActIdxOfSteps,ArrayVarPerm) :-
    %%%no_loop_constraint(States),%%% NEW: added June 14, 2010
    my_labeling_orderedvars(Varsocc, States, 1, ActIdxOfSteps, ArrayVarPerm).
    
my_labeling([],_,_,_) :- !.
my_labeling([CurrVars|Vars], States, I, [_ThisActIdx|ActIdxOfSteps]) :-
    %%labeling([down],CurrVars),  %%% Change options here. 
    %%labeling([ffc,down],CurrVars),
    %%labeling([ffc],CurrVars),
    labeling([leftmost,down],CurrVars),
    %%% useless after no_loop_ constraint: 
    no_loop(States,I),
    I1 is I + 1, 
    my_labeling(Vars, States,I1,ActIdxOfSteps).



my_labeling_orderedvars([], _States, _, [], _ArrayVarPerm).
my_labeling_orderedvars([FirstStepVars|Varsocc], States, I, ActIdxOfSteps, ArrayVarPerm) :-
    labeling([leftmost,down],FirstStepVars),
    I1 is I+1,
    my_labeling_orderedvars_aux(Varsocc, States, I1, ActIdxOfSteps, ArrayVarPerm).

my_labeling_orderedvars_aux([],_,_,[_],_).
my_labeling_orderedvars_aux([ThisTrans|TransList], States, I, [LastActIdx|ActIdxOfSteps], ArrayVarPerm) :-
    aref(LastActIdx, ArrayVarPerm, VARSorig-ORDVARSorig),
    copy_term(VARSorig-ORDVARSorig,ThisTrans-ORDVARS),
    labeling([leftmost,down],ORDVARS),
    %labeling([ffc,down],CurrVars),
    %labeling([ffc],CurrVars),
    %%% useless after no_loop_ constraint: 
    no_loop(States,I),
    I1 is I+1,
    my_labeling_orderedvars_aux(TransList, States, I1, ActIdxOfSteps, ArrayVarPerm).



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

make_action_occurrences(1,_,[],[]):-!.
make_action_occurrences(N,List,[Act|ActionsOcc],[ActIdxOfThisStep|Idxs]) :-
    make_one_action_occurrences(List,Act,AList),
    %get_action_list(Act,AList),
    fd_only_one(AList),
    length(List,NumActs),
    domain([ActIdxOfThisStep],1,NumActs),
    element(ActIdxOfThisStep,AList,1),
    N1 is N-1, make_action_occurrences(N1,List,ActionsOcc,Idxs),
    true.
make_one_action_occurrences([],[],[]).
make_one_action_occurrences([A|Actions],[ action(A,OccA)|OccActions],[OccA|OccAs]) :-
    fd_domain_bool(OccA),
    make_one_action_occurrences(Actions,OccActions,OccAs).

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
   format("Setting transitions constraints: ",[]),
   set_transitions(States,Occurrences,FLAGS),
   table(FLAGS, [[0,0,0,0], [0,0,1,0], [0,0,1,1], 
                 [1,0,0,1], [1,1,0,0],[1,1,0,1]]).

set_transitions([_States],_Occurrences,[]) :- !, format(" done~n",[]).
set_transitions([S1,S2|Rest],[O|Occurrences],FLAGS) :-
    %format("Setting next transition~n",[]),
    write('.'),
    set_transition(S1,S2,O,S1,S2,FL),
    set_transitions([S2|Rest],Occurrences,AGS),
    append(FL,AGS,FLAGS).

set_transition([], [], _Occ, _, _, []).
set_transition([fluent(Fluent,IV)|R1], [fluent(Fluent,EV)|R2], Occ, FromState, ToState, [F|LAGS]):-
    set_one_fluent(Fluent, IV, EV, Occ, FromState, ToState, F),
    set_transition(R1, R2, Occ, FromState, ToState, LAGS).

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
static([Cond|Others], State, [Flag|Fo], EV, Mode) :-
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

state_select(0,[State|_],State) :- !.
state_select(N,[_State1|States],State) :-
    N1 is N - 1,
    state_select(N1, States,State).

state_select(0,[State|_],State,[]) :- !.
state_select(N,[State1|States],State,[State1|States1]) :-
    N1 is N - 1,
    state_select(N1, States,State, States1).

group_cond([],[]).
group_cond([[Action,C]|R],[[Action,[C|Cs]]|S]) :-
     findall(L,(member([Action,L],R)),Cs),
     filter(R,Action,Others),
     group_cond(Others,S).
filter([],_,[]).
filter([[A,_]|R],A,S) :-
      !, filter(R,A,S).
filter([C|R],A,[C|S]) :-
      !, filter(R,A,S).

get_precondition_vars([],_,[]).
get_precondition_vars([P1|Rest],State,[F|LR]) :-
    (P1 = neg(FluentName),!,
     member(fluent(FluentName,A),State), F #= 1-A;
     member(fluent(P1,F),State)),
    get_precondition_vars(Rest,State,LR).
get_all_actions([],[]).
get_all_actions([A|B],List) :-
    get_all_actions(B,List2),
    get_action_list(A,List1),
    append(List1,List2,List).
get_action_list([],[]).
get_action_list([action(_,V)|Rest],[V|MRest]) :-
    get_action_list(Rest,MRest).

sel_vars([],[], _StatAnal).
sel_vars([A|B],[C|D], StatAnal) :-
    (( StatAnal == statanal ) -> get_action_list(A,C) ;  term_variables_bag(A,C) ), %;  term_variables(A,C) ),
    sel_vars(B,D,StatAnal).

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
%interval(A,A,_).
%interval(X,A,B) :-
%    A < B, C is A + 1,
%    interval(X,C,B).

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
%%% Static analysis of dynamic laws
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

static_anal(SortActionIds, SortGoalLits, ReorderedActionIds, MultiOrdersWithVars) :-
    write('****Static analysis...'),

    statistics(runtime,[StgetDependentActs,_]),
    getDependentActs(SortActionIds,SRevOuts,ActActsList),!,
    statistics(runtime,[EndgetDependentActs,_]),
    TNDe is EndgetDependentActs-StgetDependentActs,
    format("Time for getDependentActs:~w ms~n",[TNDe]),
  %format("        ActActsList:~w ~n",[ActActsList]),
  %format(" SOuts:~w ~n",[SRevOuts]),

    statistics(runtime,[StgatherAffected,_]),
    gatherAffected(SRevOuts, SortGoalLits, Was),
    keysort(Was, WasOrd),
    keys_and_values(WasOrd, _, RevReorderedActionIds),
    reverse(RevReorderedActionIds, ReorderedActionIds),
    statistics(runtime,[EndgatherAffected,_]),
    TNG is EndgatherAffected-StgatherAffected,
    format("Time for gatherAffected:~w ms~n",[TNG]),
  %format("                Was:~w ~n",[Was]),
  %format(" ReorderedActionIds:~w ~n",[ReorderedActionIds]),

    statistics(runtime,[Stsplitting,_]),
    splitter(ActActsList,ReorderedActionIds, MultiOrdersNames),
%   format("MultiOrdersNames:~n",[]),
%   listOut(MultiOrdersNames),
    statistics(runtime,[Endsplitting,_]),
    TNS is Endsplitting-Stsplitting,
    format("Time for splitting:~w ms~n",[TNS]),
%   format("ReorderedActionIds:~n~w~n",[ReorderedActionIds]),

    %format("MultiOrdersNames:~w ~n",[MultiOrdersNames]),
    %format("         ReorderedLa:~w ~n",[ReorderedLa]),
    new_array(Empty),
    length(ReorderedActionIds,NumActs),
    statistics(runtime,[StmkArray,_]),
    mkMultiOrdersWithNewVars(MultiOrdersNames, ReorderedActionIds, NumActs, Empty, MultiOrdersWithVars),
    statistics(runtime,[EndmkArray,_]),
    TNmkA is EndmkArray-StmkArray,
    format("Time for mkorders:~w ms~n",[TNmkA]),
    %MultiOrdersWithVars e' logarray di Vars
    %format("MultiOrdersWithVars :~w ~n",[MultiOrdersWithVars]),
    %outArray(0, NumActs,MultiOrdersWithVars),
   write('...done****'),nl.


% partendo da ActActsList (vedi getDependentActs), genera una lista MultiOrdersNames
% di coppie Act-OrdActs dove OrdActs indica un ordine da usare per il labeling 
% delle azioni nel passo successivo alla occorrenza di Act.
% (Si divide in due parti l'ordine dato da gatherAffected posizionando
% leftmost le azioni "influenzabili" da Act secondo il risultato di getDependentActs)
%Da migliorare nell'efficienza....
splitter([],_ReorderedActionIds, []).
splitter([Act-Acts|ActActsList], AllActsWsorted, [PositionAct-OrderedNextActNames|Rest]) :-
        extract(AllActsWsorted, Acts, FirstAs, SecondAs),!,
        append(FirstAs, SecondAs, OrderedNextActNames),
	nth1(PositionAct, AllActsWsorted, Act),
	%        assert(giumbo(PositionAct, Act, OrderedNextActNames)),!,
        splitter(ActActsList, AllActsWsorted, Rest).


%non utilizzabile dinamicamente. da usare per costruire logarr-actocc
%applySamePermutation(ActNamesOrig, ActNamesReord, ActVarsOrig, ActVarsReord) :-
%	keys_and_values(ActNamesVarsOrig, ActNamesOrig, ActVarsOrig),
%	keys_and_values(ActNamesVarsReord, ActNamesReord, ActVarsReord),
%	permutation(ActNamesVarsOrig, ActNamesVarsReord).

applySamePermutation([], _PermTermsList, [], _PermVars).
applySamePermutation([Term|Terms], PermTermsList, [Var|Vars], PermVars) :-
	%nth1(Pos, PermTermsList, Term),!,
	%nth1(Pos, PermVars, Var),!,
	correspond(Term, PermTermsList, PermVars, Var),!,
	applySamePermutation(Terms, PermTermsList, Vars, PermVars).


extract([], _Prior,  [], []).
extract([A|As], Prior,  FirstAs, SecondAs) :-
        (ord_member(A,Prior) -> (FirstAs=[A|F1], SecondAs=S1) ; (FirstAs=F1 ,SecondAs=[A|S1])),
        extract(As, Prior,  F1, S1).


%listOut([]).
%listOut([A|AS]) :-
%	format("    A:~w~n  ",[A]),
%	listOut(AS).


% in futuro... idx invece che action names
%numerize([], _,[]).
%numerize([A-E-Pl|AcEPl], SortIds, [PosA-E-Pl|AEPl]) :-
%	nth1(PosA,SortIds,A),!,
%	numerize(AcEPl, SortIds, AEPl).

% getDependentActs determina una lista di coppie Action-Actions
% dove Actions sono tutte le azioni che hanno nelle precondition
% almeno un fluentliteral che figura tra gli effetti di Action
% (pensato per influenzare l'ordine delle variabili/azioni nel labeling).
% (Attualmente non tiene conto delle caused...forse avrebbe senso farlo
% ma potrebbe risultarne un "tutto-influenza-tutto"...)
getDependentActs(SortActionIds, SRevOuts, ActActsListWithWidows) :-
	mysetof(A-E-Pl,wrap_causes(A,E,Pl),AEPl),
%	numerize(AcEPl, SortActionIds, AEPl),!,
%format("~n~nSortActionIds:~n ~w~n ",[SortActionIds]),
%format("AEPl:~n ~w~n ",[AEPl]),
	mkedges(AEPl, RevOuts, Outs, Insls), !,
	sort(RevOuts, SRevOuts), 
	append(Insls,Ins),
	sort(Ins,SIns),
	sort(Outs,SOuts),
	mkpairs(SOuts,SIns,ActActList),!,
	sort(ActActList,ActActListS),
	keyclumped(ActActListS,ActActsList),
	length(ActActsList,ActActsListLen),
        format("~nStatic analysis reordered ~w actions~n",[ActActsListLen]),
	addWidows(SortActionIds,ActActsList,ActActsListWithWidows).

addWidows([],L,L).
addWidows([W|Ws],L,Lout) :-
	memberchk(W-_,L) -> addWidows(Ws,L,Lout) ; addWidows(Ws,[W-[]|L],Lout).


mkpairs([], _SIns, []) :-!.
mkpairs(_SOuts, [], []) :-!.
mkpairs([F1-_A1|SOuts], [F2-A2|SIns], ActActList) :-
	F1 @< F2, !,
	mkpairs(SOuts, [F2-A2|SIns], ActActList).
mkpairs([F1-A1|SOuts], [F2-_A2|SIns], ActActList) :-
	F1 @> F2, !,
	mkpairs([F1-A1|SOuts], SIns, ActActList).
mkpairs([F1-A1|SOuts], [F2-A2|SIns], ActActList) :-
	F1 == F2, !,
	mkpairs_aux(SOuts, F2, A2, Res1),
	mkpairs([F1-A1|SOuts], SIns, Res2),
	append([A1-A2|Res1], Res2, ActActList).

%mkpairs_aux([], _F12, _A2, []) :- !.
mkpairs_aux([F12-A3|SOuts], F12, A2, [A3-A2|Res]) :- !,
	mkpairs_aux(SOuts, F12, A2, Res).
mkpairs_aux(_SOuts, _F12, _A2, []).



mkedges([], [], [], []).
mkedges([A-E-Pl|AEPl], [A-E|RevOuts], [E-A|Outs], [PlA_s|Insl]) :-
	findall(P-A, member(P,Pl), PlA_s),!,
	mkedges(AEPl, RevOuts, Outs, Insl).



% gatherAffected  determina il "peso" di ogni azione in base
% all'overlap tra i suoi effetti immediati e il goal
% (Verra' utilizzato per ordinare le variabili/azioni nel labeling.
% ...poco vantaggioso per le prime azioni, puo' essere rilevante
% per le ultime azioni del piano)
gatherAffected(SRevOuts, SortGoalLits, Was) :-
	keyclumped(SRevOuts, A_Fs_list),
	evalWeight(A_Fs_list, SortGoalLits, Was).

evalWeight([], _SortGoalLits, []).
evalWeight([Act-Flist|A_Fs_list], SortGoalLits, [WEIGTH-Act|WAs]) :-
	evalOneWeight(Flist, SortGoalLits, WEIGTH),
	evalWeight(A_Fs_list, SortGoalLits, WAs).

evalOneWeight(Fs, SortGoalLits, WEIGTH) :- 
	evalOneWeight(Fs, SortGoalLits, GPu, GNu, NoGPu, NoGNu),
	sort(GPu,GP), length(GP,NumGP),
	sort(GNu,GN), length(GN,NumGN),
	sort(NoGPu,NoGP), length(NoGP,NumNoGP),
	sort(NoGNu,NoGN), length(NoGN,NumNoGN), 
	%format("ACT:~w~nGP:~w~nGN:~w~nNoGP:~w~nNoGN:~w~n~n", [Act,GP,GN,NoGP,NoGN]),
	%format("ACT-NumGP-NumGN-NumNoGP-NumNoGN: ~w~n", [Act-NumGP-NumGN-NumNoGP-NumNoGN]),
%alternative per la stima del peso della azione:
	WEIGTH is 2*NumGP + 2*NumGN - NumNoGP - NumNoGN.  %(sembra meglio in generale) 
%	WEIGTH is NumGP + NumGN.   %(sembra la migliore per tangram)
%	WEIGTH is NumGP + NumGN - NumNoGP - NumNoGN. %(sembra la peggiore per tangram)


evalOneWeight([], _SortGoalLits, [], [], [], []).
evalOneWeight([neg(F)|Fs], SortGoalLits, GPu, NewGNu, NoGPu, NewNoGNu) :- !,
	(ord_member(neg(F),SortGoalLits) -> (NewGNu = [F|GNu]) ; NewGNu=GNu),
	(ord_member(F,SortGoalLits) -> (NewNoGNu = [F|NoGNu]) ; NewNoGNu=NoGNu),
	evalOneWeight(Fs, SortGoalLits, GPu, GNu, NoGPu, NoGNu).
evalOneWeight([F|Fs], SortGoalLits, NewGPu, GNu, NewNoGPu, NoGNu) :-
	(ord_member(F,SortGoalLits) -> (NewGPu = [F|GPu]) ; NewGPu=GPu),
	(ord_member(neg(F),SortGoalLits) -> (NewNoGPu = [F|NoGPu]) ; NewNoGPu=NoGPu),
	evalOneWeight(Fs, SortGoalLits, GPu, GNu, NoGPu, NoGNu).



mkMultiOrdersWithNewVars([], _, _, Array, Array).
mkMultiOrdersWithNewVars([ActNum-Acts|ActNumActs], OrigOrder, N, OldArray, NewArray) :- 
	length(VARS,N),
	length(ORDVARS,N),
	applySamePermutation(OrigOrder, Acts, VARS, ORDVARS), !,
	aset(ActNum, OldArray, VARS-ORDVARS, TempArray),
	mkMultiOrdersWithNewVars(ActNumActs, OrigOrder, N, TempArray, NewArray).

outArray(I,Max,Ar):- I<Max, !, I1 is I+1,
              (aref(I1,Ar, Elem) -> format("Element ~w : ~w~n",[I1,Elem]) ; true),!,
	      outArray(I1,Max,Ar).
outArray(_,_,_).



