%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2003,    Renate Schmidt, University of Manchester
%  Modified 2009-10, Ullrich Hustadt, University of Liverpool
%
% Uses
%       X_{<a*>p}
%   (X)-----------
%       p | <a>X
%
%         A v B
%   (v) -------------
%         A | B
%
% 'nd' is short for `not disjoint' branches
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(pdl_dGM_nd, 
    [
        eventualities/2,
        simplify_X/2,
        reduce_local/7
    ]).

:- dynamic
        eventualities/2,
        simplify_X/2,
        reduce_local/7.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eventualities([], []).

eventualities([pr(_,FmlList)|Branch], Eventualities) :-
    eventualities_fml(FmlList, Eventualities1), !,
    eventualities(Branch, Eventualities2),
    sort(Eventualities1, SortedEventualities1),
    sort(Eventualities2, SortedEventualities2),
    merge_set(SortedEventualities1, SortedEventualities2, Eventualities).

eventualities_fml([], []).

eventualities_fml([Fml|FmlList], Eventualities) :-
    eventualities_fml(Fml, Eventualities1), !,
    eventualities_fml(FmlList, Eventualities2),
    sort(Eventualities1, SortedEventualities1),
    sort(Eventualities2, SortedEventualities2),
    merge_set(SortedEventualities1, SortedEventualities2, Eventualities).
    
eventualities_fml(x(X,Fml), [x(X,Fml)|Eventualities]) :-
    eventualities_fml(Fml, Eventualities).

eventualities_fml(and(A,B), Eventualities) :-
    eventualities_fml(A, EventualitiesA),
    eventualities_fml(B, EventualitiesB),
    sort(EventualitiesA, SortedEventualitiesA),
    sort(EventualitiesB, SortedEventualitiesB),
    merge_set(SortedEventualitiesA, SortedEventualitiesB, Eventualities).

eventualities_fml(or(A,B), Eventualities) :-
    eventualities_fml(A, EventualitiesA),
    eventualities_fml(B, EventualitiesB),
    sort(EventualitiesA, SortedEventualitiesA),
    sort(EventualitiesB, SortedEventualitiesB),
    merge_set(SortedEventualitiesA, SortedEventualitiesB, Eventualities).

eventualities_fml(not(A), Eventualities) :-
    eventualities_fml(A, Eventualities).

eventualities_fml(test(A), Eventualities) :-
    eventualities_fml(A, Eventualities).

eventualities_fml(star(R), Eventualities) :-
    eventualities_fml(R, Eventualities).

eventualities_fml(comp(R,S), Eventualities) :-
    eventualities_fml(R, EventualitiesR),
    eventualities_fml(S, EventualitiesS),
    sort(EventualitiesR, SortedEventualitiesR),
    sort(EventualitiesS, SortedEventualitiesS),
    merge_set(SortedEventualitiesR, SortedEventualitiesS, Eventualities).

eventualities_fml(dia(R,A), Eventualities) :-
    eventualities_fml(R, EventualitiesR),
    eventualities_fml(A, EventualitiesA),
    sort(EventualitiesR, SortedEventualitiesR),
    sort(EventualitiesA, SortedEventualitiesA),
    merge_set(SortedEventualitiesR, SortedEventualitiesA, Eventualities).

eventualities_fml(box(R,A), Eventualities) :-
    eventualities_fml(R, EventualitiesR),
    eventualities_fml(A, EventualitiesA),
    sort(EventualitiesR, SortedEventualitiesR),
    sort(EventualitiesA, SortedEventualitiesA),
    merge_set(SortedEventualitiesR, SortedEventualitiesA, Eventualities).

eventualities_fml(_,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
simplify_X([], []).

simplify_X([Fml|FmlList], [SimplifiedFml|SimplifiedFmlList]) :-
    simplify_X_fml(Fml, SimplifiedFml), !,
    simplify_X(FmlList, SimplifiedFmlList).

simplify_X_fml(x(_,Fml), x(SimplifiedFml)) :-
    simplify_X_fml(Fml, SimplifiedFml).

simplify_X_fml(and(A,B), and(SimplifiedA,SimplifiedB)) :-
    simplify_X_fml(A, SimplifiedA),
    simplify_X_fml(B, SimplifiedB).

simplify_X_fml(or(A,B), or(SimplifiedA,SimplifiedB)) :-
    simplify_X_fml(A, SimplifiedA),
    simplify_X_fml(B, SimplifiedB).

simplify_X_fml(not(A), not(SimplifiedA)) :-
    simplify_X_fml(A, SimplifiedA).

simplify_X_fml(dia(R,A), dia(SimplifiedR,SimplifiedA)) :-
    simplify_X_fml(R, SimplifiedR),
    simplify_X_fml(A, SimplifiedA).

simplify_X_fml(box(R,A), box(SimplifiedR,SimplifiedA)) :-
    simplify_X_fml(R, SimplifiedR),
    simplify_X_fml(A, SimplifiedA).

simplify_X_fml(comp(R,S), comp(SimplifiedR,SimplifiedS)) :-
    simplify_X_fml(R, SimplifiedR),
    simplify_X_fml(S, SimplifiedS).

simplify_X_fml(test(R), test(SimplifiedR)) :-
    simplify_X_fml(R, SimplifiedR).

simplify_X_fml(A,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% reduce_local(+Unexpanded, +State, +Workedoff, -NewWorkedoff,
%        -Universal, -Existential, -Eventualities)
%
% - Applies the local reduction rules of the tableau calculus;
%   all except existential, universal and theory rules
% - Note that -Eventualities is the list of eventualities <A*> F
%   such that the formula F has just been added to the branch; thus
%   <A*>F is fulfilled.

reduce_local([], _, Workedoff, Workedoff, [], [], []).

reduce_local([true|Unexpanded], X, Workedoff, NewWorkedoff, 
        Universal, Existential, Eventualities) :- !,
    extend_unexpanded([true], Workedoff, Unexpanded, NewUnexpanded, X, true),
    reduce_local(NewUnexpanded, X, [true|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([or(A,B)|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, Eventualities) :- 
% Note: no Prolog cut at this point ->
%       The disjunction or(A,B) creates two branches in our search
%       space / branches in the tableau tree.
%       We first explore the branch on which A is true; this is done by the
%       remaining literals of this rule.
%       If the first branch does not result in a model, then Prolog
%       backtracking will take us back to this point and then to the next
%       rule below. 
    get_branch_point_counter(Count),
    pdl_on_write('In reduce_local, LEFT branch of OR, reduce_local: '), pdl_on_write(Count), pdl_on_nl,
    increment_branch_point_counter(1),
    extend_unexpanded([A], Workedoff, Unexpanded, NewUnexpanded, 
                 X, ['or-LEFT',or(A,B)]), 
    reduce_local(NewUnexpanded, X, [or(A,B)|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([or(A,B)|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, Eventualities) :- 
% Note: We are still dealing with the same disjunction or(A,B) as the
%       previous rule. We only get to this point if the branch on which
%       A is true did not result in a model. So, we now explore the branch
%       of the search space / tableau tree on which B is true.
    get_branch_point_counter(Count),
    pdl_on_write('In reduce_local, RIGHT branch of OR, reduce_local: '), pdl_on_write(Count), pdl_on_nl,
    increment_branch_point_counter(-1),
    extend_unexpanded([B], Workedoff, Unexpanded, NewUnexpanded, 
                 X, ['or-RIGHT',or(A,B)]), 
    reduce_local(NewUnexpanded, X, [or(A,B)|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

% Condition 4 in Definition 23 in (De Giacomo and Massacci, 2000):
% If both [A|S] and [B|S] are \bot-sets, then [or(A,B)|S] is a \bot-set
reduce_local([or(A,B)|Unexpanded], _X, Workedoff, _NewWorkedoff,
             _Universal, _Existential, _Eventualities) :- 
    !,
%    write('both branches of an or have been closed; recording inconsistency'), nl,
    append([or(A,B)|Unexpanded],Workedoff,Fmls),
    store_inconsistent(Fmls),
    fail.

reduce_local([not(or(A,B))|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, Eventualities) :- !,
    complement(A, NotA),
    complement(B, NotB),
    extend_unexpanded([NotA,NotB], Workedoff, Unexpanded, NewUnexpanded, 
                 X, ['and',not(or(A,B))]), 
    reduce_local(NewUnexpanded, X, [not(or(A,B))|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([dia(or(R,S),A)|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, Eventualities) :- % no cut
    get_branch_point_counter(Count),
    pdl_on_write('In reduce_local, LEFT branch of DIA-OR, reduce_local: '), pdl_on_write(Count), pdl_on_nl,
    increment_branch_point_counter(1),
    extend_unexpanded([dia(R,A)], Workedoff, Unexpanded, NewUnexpanded, 
                 X, ['dia-or-LEFT',dia(or(R,S),A)]), 
    reduce_local(NewUnexpanded, X, [dia(or(R,S),A)|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([dia(or(R,S),A)|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, Eventualities) :- !,
    % Expand the right branch only when the left one is closed
%    is_unsatisfiable(_),
%    complement(dia(R,A), NotDiaRA),
    get_branch_point_counter(Count),
    pdl_on_write('In reduce_local, RIGHT branch of DIA-OR, reduce_local: '), pdl_on_write(Count), pdl_on_nl,
    increment_branch_point_counter(-1),
    extend_unexpanded([dia(S,A)], Workedoff, Unexpanded, NewUnexpanded, 
                X, ['dia-or-RIGHT',dia(or(R,S),A)]), 
    reduce_local(NewUnexpanded, X, [dia(or(R,S))|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([dia(comp(R,S),A)|Unexpanded], X, Workedoff, NewWorkedoff, 
        Universal, Existential, Eventualities) :- !,
    extend_unexpanded([dia(R,dia(S,A))], Workedoff, Unexpanded, NewUnexpanded, 
                X, ['dia-comp',dia(comp(R,S),A)]), 
    reduce_local(NewUnexpanded, X, [dia(comp(R,S),A)|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([dia(star(R),A)|Unexpanded], X, Workedoff, NewWorkedoff, 
        Universal, Existential, Eventualities) :- !,
    extend_unexpanded([x(X,dia(star(R),A))], Workedoff, Unexpanded, NewUnexpanded, 
                X, ['dia-star',dia(star(R),A)]), 
    reduce_local(NewUnexpanded, X, [dia(star(R),A)|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Note that (de Giacomo and Massacci, 2001) uses complement splitting when
% dealing with <A*>F. Thus, on the right branch created for <A*>F, (not F)
% is true, making it impossible for the current state/world to be the one
% fulfilling <A*>F. On the other hand, on the left branch, we make F true
% and <A*>F is fulfilled. We record this by adding fulfilled(...) to the
% list of (fulfilled) eventualities.
reduce_local([x(Y,dia(star(R),A))|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, 
        [fulfilled(X, x(Y,dia(star(R),A)))|Eventualities]) :- % no cut
    get_branch_point_counter(Count),
    pdl_on_write('In reduce_local, LEFT branch of X, reduce_local: '), pdl_on_write(Count), pdl_on_nl,
    increment_branch_point_counter(1),
    extend_unexpanded([A], Workedoff, Unexpanded, NewUnexpanded, 
                X, ['X-LEFT',x(Y,dia(star(R),A))]), 
    reduce_local(NewUnexpanded, X, [x(Y,dia(star(R),A))|Workedoff],
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([x(Y,dia(star(R),A))|Unexpanded], X, Workedoff, NewWorkedoff, 
        Universal, Existential, Eventualities) :- !,
%    is_unsatisfiable(_),
% Remember that (de Giacomo and Massacci, 2001) uses complement splitting at
% this point. This variant of the calculus does not.
    get_branch_point_counter(Count),
    pdl_on_write('In reduce_local, RIGHT branch of X, reduce_local: '), pdl_on_write(Count), pdl_on_nl,
    increment_branch_point_counter(-1),
    extend_unexpanded([dia(R,x(Y,dia(star(R),A)))], Workedoff, 
                Unexpanded, NewUnexpanded, 
                X, ['X-RIGHT',x(Y,dia(star(R),A))]), 
    reduce_local(NewUnexpanded, X, [x(Y,dia(star(R),A))|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([dia(test(A),B)|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, Eventualities) :- !,
    extend_unexpanded([A,B], Workedoff, Unexpanded, NewUnexpanded, 
                X, ['dia-test',dia(test(A),B)]), 
    reduce_local(NewUnexpanded, X, [dia(test(A),B)|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([dia(R,A)|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, [dia(R,A)|Existential], Eventualities) :- !,
    atom(R),  % important
    reduce_local(Unexpanded, X, [dia(R,A)|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([not(dia(or(R,S),A))|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, Eventualities) :- !,
    extend_unexpanded([not(dia(R,A)),not(dia(S,A))], Workedoff, Unexpanded, NewUnexpanded, 
                X, ['box-or',not(dia(or(R,S),A))]), 
    reduce_local(NewUnexpanded, X, [not(dia(or(R,S),A))|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([not(dia(comp(R,S),A))|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, Eventualities) :- !,
    extend_unexpanded([not(dia(R,dia(S,A)))], Workedoff, Unexpanded, NewUnexpanded, 
                X, ['box-comp',not(dia(comp(R,S),A))]), 
    reduce_local(NewUnexpanded, X, [not(dia(comp(R,S),A))|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([not(dia(star(R),A))|Unexpanded], X, Workedoff, NewWorkedoff, 
        Universal, Existential, Eventualities) :- !,
    complement(A, NotA),
    extend_unexpanded([NotA,not(dia(R,dia(star(R),A)))], Workedoff, Unexpanded, NewUnexpanded, 
                X, ['box-star',not(dia(star(R),A))]), 
    reduce_local(NewUnexpanded, X, [not(dia(star(R),A))|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([not(dia(test(A),B))|Unexpanded], X, Workedoff, NewWorkedoff, 
        Universal, Existential, Eventualities) :- % no cut
    complement(A, NotA),
    get_branch_point_counter(Count),
    pdl_on_write('In reduce_local, LEFT branch of BOX-TEST, reduce_local: '), pdl_on_write(Count), pdl_on_nl,
    increment_branch_point_counter(1),
    extend_unexpanded([NotA], Workedoff, Unexpanded, NewUnexpanded, 
                X, ['box-test-LEFT',not(dia(test(A),B))]), 
    reduce_local(NewUnexpanded, X, [not(dia(test(A),B))|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([not(dia(test(A),B))|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, Eventualities) :- !,
%    is_unsatisfiable(_),
    complement(B, NotB),
    get_branch_point_counter(Count),
    pdl_on_write('In reduce_local, RIGHT branch of BOX-TEST, reduce_local: '), pdl_on_write(Count), pdl_on_nl,
    increment_branch_point_counter(-1),
    extend_unexpanded([NotB], Workedoff, Unexpanded, NewUnexpanded, 
                X, ['box-test-RIGHT',not(dia(test(A),B))]), 
    reduce_local(NewUnexpanded, X, [not(dia(test(A),B))|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([not(dia(R,A))|Unexpanded], X, Workedoff, NewWorkedoff, 
        [not(dia(R,A))|Universal], Existential, Eventualities) :- !,
    atom(R),  % important
    reduce_local(Unexpanded, X, [not(dia(R,A))|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

reduce_local([Literal|Unexpanded], X, Workedoff, NewWorkedoff,
        Universal, Existential, Eventualities) :- !,
    reduce_local(Unexpanded, X, [Literal|Workedoff], 
            NewWorkedoff, Universal, Existential, Eventualities).

