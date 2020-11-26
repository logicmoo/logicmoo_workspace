
% I re-implement the old findall_constraints that was implemented in
% SICStus 3. I have looked at the source code of the new CHR library
findall_constraints(C,L):-
    findall(C,'$enumerate_constraints'(C),L).

% This version has N^2 complexity,
% but retains the names of the variables, ie., if two CHR constraints
% share a variable, the list will have two terms sharing the same variable.
findall_constraints_nsquare(C,L):-
    findall_constraints_nsquare(C,[],L).
findall_constraints_nsquare(C,Lin,Lout):-
    copy_term(C,C1),
    '$enumerate_constraints'(C1),
    not_member_eq(C1,Lin),!,
    findall_constraints_nsquare(C,[C1|Lin],Lout).
findall_constraints_nsquare(_,L,L).

not_member_eq(_,[]).
not_member_eq(X,[Y|T]):- \+(X==Y), not_member_eq(X,T).
    

max_constraints(C,Max):-
    assert('$n_constraints'(0)),
    ('$enumerate_constraints'(C), retract('$n_constraints'(N)),
        (N<Max -> N1 is N+1, assert('$n_constraints'(N1)), fail
            ;   !,retract('$n_constraints'(_)), fail
        )
    ; retractall('$n_constraints'(_))
    ).

print_chr_list([],_).
print_chr_list([X|R],Sep):-
    portray(X), write(Sep),
    print_chr_list(R,Sep).
