%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright 2009-10, Ullrich Hustadt, University of Liverpool
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(system)).
:- ensure_loaded([
       problems
   ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% testing(+ProblemClassId)
%
%     ProblemClassId is e.g. routine, routine_w_test, difficult; see
%     definition of problems. 

testing(ProblemClassId) :-
    problems(ProblemClassId, ProblemList),
    test_problem_list(ProblemList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_problem_list(+OutputStream, +ProblemList)
%
%     ProblemList is a list of problem identifiers (see problems.pl)

test_problem_list([]).

test_problem_list([Problem|List]) :-
    test_problem(Problem),
    test_problem_list(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_problem(+ProblemIdentifier)

test_problem(ProblemId) :-
    write('Testing PDLProver (tree) on '), ttyflush,
    shell(hostname), ttyflush,
    write('Problem '), print( ProblemId), nl,
    ttyflush,
    problem(ProblemId, Expected, _Result),
    write('Expected Result   '), print(Expected), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate(yes, unsatisfiable, unsatisfiable) :- !,
    write(', WARNING conflict !').

validate(no, unsatisfiable, satisfiable) :- !,
    write(', WARNING conflict !').

validate(no, satisfiable, unsatisfiable) :- !,
    write(', WARNING conflict !').

validate( _, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

systemCall(CallPredicate,Command) :-
        once(systemCall(Command,[],CallPredicate)),
        !.
systemCall(_CallPredicate,Command) :-
        write('SystemCall of '),
        write(Command),
        write(' failed.'),
        nl,
        !.

systemCall([],CommandString,_) :-
        atom_chars(Command,CommandString),
        !,
        shell(Command).
systemCall([],CommandString,shell(Result)) :-
        atom_chars(Command,CommandString),
        !,
        shell(Command,Result).
systemCall([A|L],CommandString1,CallPredicate) :-
        (number(A) ->
            number_chars(A,AC)
        ;
            (atomic(A) ->
                atom_chars(A,AC)
            ;
                AC = A
            )
        ),
        append(CommandString1,AC,CommandString2),
        systemCall(L,CommandString2,CallPredicate).

generateSourceFilename(ProblemId,File) :-
        generateAtom(['/var/tmp/','pdl-',ProblemId],[],File).

generateFilename(Base,Extension,ProblemId,File) :-
        generateAtom([Base,ProblemId,Extension],[],File).

generateAtom([],AtomString,Atom) :-
        atom_chars(Atom,AtomString).
generateAtom([A|L],AtomString1,Atom) :-
        (number(A) ->
            number_chars(A,AC)
        ;
            (atomic(A) ->
                atom_chars(A,AC)
            ;
                AC = A
            )
        ),
        append(AtomString1,AC,AtomString2),
        generateAtom(L,AtomString2,Atom).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prove([],Formula,Result) :-
	prove(Formula,Result).
prove([_|_],_,_) :-
	write('Problem uses axioms.'), nl.

prove(Formula,Result) :-
	current_prolog_flag(pid,ProblemId),
        generateFilename('/var/tmp/pdlProver','.in', ProblemId,PDLProver_Input_File),
        generateFilename('/var/tmp/pdlProver','.out',ProblemId,PDLProver_Output_File),
	write('Writing PDLProver output to '), write(PDLProver_Output_File), nl,
        tell(PDLProver_Input_File),
        printFormulaPDLProver(Formula), 
	nl,
        told,
        !,
        systemCall(exec,['ulimit -t 1100; time cat ',PDLProver_Input_File,' | /users/lect/ullrich/provers/pdlProver/pdl tree verbose | tee ',PDLProver_Output_File]),
%        systemCall(system,['cat ',PDLProver_Output_File]),
	Result = unknown,
        ttyflush.

printFormulaPDLProver(dia(P,A)) :-
        write('< '),
	printProgramPDLProver(P),
	write(' > '),
        printFormulaPDLProver(A).
printFormulaPDLProver(box(P,A)) :-
        write('[ '),
	printProgramPDLProver(P),
	write(' ] '),
        printFormulaPDLProver(A).
printFormulaPDLProver(and(A,B)) :-
        write('('),
        printFormulasPDLProver(' & ',[A,B]),
        write(')').
printFormulaPDLProver(equiv(A,B)) :-
        write('('),
        printFormulasPDLProver(' <=> ',[A,B]),
        write(')').
printFormulaPDLProver(implies(A,B)) :-
        write('('),
        printFormulasPDLProver(' => ',[A,B]),
        write(')').
printFormulaPDLProver(or(A,B)) :-
        write('('),
        printFormulasPDLProver(' | ',[A,B]),
        write(')').
printFormulaPDLProver(not(A)) :-
        write('~('),
        printFormulaPDLProver(A),
        write(')').
printFormulaPDLProver(true) :-
        write('True').
printFormulaPDLProver(false) :-
        write('False').
printFormulaPDLProver(A) :-
        atomic(A),
        write(A).

printFormulasPDLProver(_Op,[T1]) :-
        printFormulaPDLProver(T1).
printFormulasPDLProver(Op,[T1,T2|TL]) :-
        printFormulaPDLProver(T1),
        write(' '),
        write(Op),
        write(' '),
        printFormulasPDLProver(Op,[T2|TL]).

printProgramPDLProver(star(P)) :-
	write(' * ('),
	printProgramPDLProver(P),
	write(')').
printProgramPDLProver(plus(P)) :-
	printProgramPDLProver(comp(P,star(P))).
printProgramPDLProver(test(A)) :-
	write(' ? ('),
	printFormulaPDLProver(A),
	write(')').
printProgramPDLProver(comp(P,Q)) :-
	write('('),
	printProgramPDLProver(P),
	write(' ; '),
	printProgramPDLProver(Q),
	write(')').
printProgramPDLProver(or(P,Q)) :-
	write('('),
	printProgramPDLProver(P),
	write(' + '),
	printProgramPDLProver(Q),
	write(')').
printProgramPDLProver(id) :-
	write('? True').
printProgramPDLProver(P) :-
	atomic(P),
	write(P).
