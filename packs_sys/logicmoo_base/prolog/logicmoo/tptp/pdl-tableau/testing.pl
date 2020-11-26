%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2003, Renate Schmidt, University of Manchester
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% testing(+ProblemClassId)
%
%     ProblemClassId is e.g. routine, routine_w_test, difficult; see
%     definition of problems. 

testing(ProblemClassId) :-
    problems(ProblemClassId, ProblemList),
    create_test_output_file,
    test_problem_list(test_output, ProblemList),
    close(test_output).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_problem_list(+OutputStream, +ProblemList)
%
%     ProblemList is a list of problem identifiers (see problems.pl)

test_problem_list(_, []).

test_problem_list(Output, [Problem|List]) :-
    test_problem(Output, Problem),
    test_problem_list(Output, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_problem(+ProblemIdentifier)

test_problem(ProblemId) :-
    current_output(Output),
    test_problem(Output, ProblemId).

test_problem(Output, ProblemId) :-
    nl(Output),
    write(Output, 'pdl-tableau Version 1.1'), nl(Output),
    write('Testing on '), ttyflush,
    unix(system(hostname)), ttyflush,
    write(Output, 'Problem '), print(Output, ProblemId), nl(Output),
    write('Problem '), print(ProblemId),
    getRuntime(T0),
    problem(ProblemId, Expected, Result),
    getRuntime(T1),
    T is T1 - T0,
    write(Output, 'Result   '), print(Output, Result), nl(Output),
    write('Result   '), print(Result), nl,
    get_proof_steps_counter(StepsNumber),
    writef('Inference steps required: %4r', [StepsNumber]), nl,
    format('Runtime                 : ~3f~n', [T/1000]),
    get_sat_reuse_counter(SatReuseNumber),
    predicate_property(consistent(_,_,_),number_of_clauses(SatStoreNumber)),
    writef('Sat   sets stored: %4r', [SatStoreNumber]), nl,
    writef('Sat   set  reuse : %4r times', [SatReuseNumber]), nl,
    get_unsat_reuse_counter(UnsatReuseNumber),
    predicate_property(inconsistent(_),number_of_clauses(UnsatStoreNumber)),
    writef('Unsat sets stored: %4r', [UnsatStoreNumber]), nl,
    writef('Unsat set  reuse : %4r times', [UnsatReuseNumber]), nl,
    write(Output, 'Expected '), 
    get_negate_first(Flag),
    (Flag = yes ->
         write(Output, 'unknown  --  ')
         ;
         true
    ),
    print(Output, Expected), 
    validate(Output, Flag, Result, Expected),
    nl(Output), nl(Output),
    write('Expected '), 
    get_negate_first(Flag),
    (Flag = yes ->
         write('unknown  --  ')
         ;
         true
    ),
    print(Expected), 
    validate(Flag, Result, Expected),
    nl, nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate(Output, yes, unsatisfiable, unsatisfiable) :- !,
    write(Output, ', WARNING conflict !').

validate(Output, no, unsatisfiable, satisfiable) :- !,
    write(Output, ', WARNING conflict !').

validate(Output, no, satisfiable, unsatisfiable) :- !,
    write(Output, ', WARNING conflict !').

validate(_, _, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate(yes, unsatisfiable, unsatisfiable) :- !,
    write(', WARNING conflict !').

validate(no, unsatisfiable, satisfiable) :- !,
    write(', WARNING conflict !').

validate(no, satisfiable, unsatisfiable) :- !,
    write(', WARNING conflict !').

validate(_, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_formula(Formula) :-
    write('pdl-tableau 1.1'), nl,
    getRuntime(T0),
    satisfiable(Formula,Result),
    getRuntime(T1),
    T is T1 - T0,
    write('Result   '), print(Result), nl,
    get_proof_steps_counter(StepsNumber),
    writef('Inference steps required: %4r', [StepsNumber]), nl,
    format('Runtime                 : ~3f~n', [T/1000]),
    get_sat_reuse_counter(SatReuseNumber),
    predicate_property(consistent(_,_,_),number_of_clauses(SatStoreNumber)),
    writef('Sat   sets stored: %4r', [SatStoreNumber]), nl,
    writef('Sat   set  reuse : %4r times', [SatReuseNumber]), nl,
    get_unsat_reuse_counter(UnsatReuseNumber),
    predicate_property(inconsistent(_),number_of_clauses(UnsatStoreNumber)),
    writef('Unsat sets stored: %4r', [UnsatStoreNumber]), nl,
    writef('Unsat set  reuse : %4r times', [UnsatReuseNumber]), nl,
    nl, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_file(File) :-
    write('pdl-tableau 1.1'), nl,
    see(File),
    read(Formula),
    seen,
    getRuntime(T0),
    satisfiable(Formula,Result),
    getRuntime(T1),
    T is T1 - T0,
    write('Result   '), print(Result), nl,
    get_proof_steps_counter(StepsNumber),
    writef('Inference steps required: %4r', [StepsNumber]), nl,
    format('Runtime                 : ~3f~n', [T/1000]),
    get_sat_reuse_counter(SatReuseNumber),
    predicate_property(consistent(_,_,_),number_of_clauses(SatStoreNumber)),
    writef('Sat   sets stored: %4r', [SatStoreNumber]), nl,
    writef('Sat   set  reuse : %4r times', [SatReuseNumber]), nl,
    get_unsat_reuse_counter(UnsatReuseNumber),
    predicate_property(inconsistent(_),number_of_clauses(UnsatStoreNumber)),
    writef('Unsat sets stored: %4r', [UnsatStoreNumber]), nl,
    writef('Unsat set  reuse : %4r times', [UnsatReuseNumber]), nl,
    nl, !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_difference([], _, []).

list_difference(List, [], List).

list_difference(ListA, [Eliminate|ListB], ListDiff) :-
    select(Eliminate, ListA, ListAminus), !,
    list_difference(ListAminus,ListB, ListDiff).
    
list_difference(ListA, [_|ListB], ListDiff) :-
    list_difference(ListA, ListB, ListDiff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getRuntime(-RT)
% RT is the runtime used so far by the process

getRuntime(RT) :-
        system:statistics(cputime,RT0),
        RT is integer(RT0*1000).




