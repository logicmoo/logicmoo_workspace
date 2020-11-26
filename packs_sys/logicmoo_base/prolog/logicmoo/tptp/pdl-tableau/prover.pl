%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2003-2010, Renate Schmidt,  University of Manchester
%           2009-2010, Ullrich Hustadt, University of Liverpool
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prove(Fml, Result) :-
    provable([], Fml, Result).

provable(Fml, Result) :-
    provable([], Fml, Result).

provable(Axioms, FmlInput, unprovable) :-
    retractall(consistent(_,_,_)),
    retractall(inconsistent(_)),
    retractall(reduced(_,_,_)),
    % Axioms is a set of non-logical axioms, i.e. a background theory
    create_search_output_file,

    negated_formula(FmlInput, Fml),

    nl,
    pdl_write('** Input: '), pdl_write(Fml), pdl_nl, 

    % Transform Axioms and Fml into negation normal form
    pdl_write('** NFAxioms:   '), pdl_nl,
    normalise(Axioms, NFAxioms),
    normalise(Fml, [], NFFml, Paths),

    pdl_write('** NFFml:   '), pdl_write(NFFml), pdl_nl, 
    pdl_write('Paths = '), pdl_write(Paths), pdl_nl,

    (get_output_format(latex) -> 
         latex_output(begin)
         ;
         true),

    % Search for a proof or a model
    search(NFAxioms, NFFml), !,

    (get_output_format(latex) ->
         latex_output(end)
         ;
         true).

provable(_, _, provable) :-
    print_result('Refutation proof found'),
    (get_output_format(latex) ->
         latex_output(end)
         ;
         true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

satisfiable(Fml, Result) :-
    satisfiable([], Fml, Result).

satisfiable(Axioms, FmlInput, satisfiable) :-
    retractall(consistent(_,_,_)),
    retractall(inconsistent(_)),
    % Axioms is a set of non-logical axioms, i.e. a background theory
    create_search_output_file,

    FmlInput = Fml,

    nl,
    pdl_write('** Input: '), pdl_write(Fml), pdl_nl, 

    % Transform Axioms and Fml into negation normal form
    pdl_write('** NFAxioms:   '), pdl_nl,
    normalise(Axioms, NFAxioms),
    normalise(Fml, [], NFFml, Paths),

    pdl_write('** NFFml:   '), pdl_write(NFFml), pdl_nl, 
    pdl_write('Paths = '), pdl_write(Paths), pdl_nl,

    (get_output_format(latex) -> 
         latex_output(begin)
         ;
         true),

    % Search for a proof or a model
    search(NFAxioms, NFFml), !,

    (get_output_format(latex) ->
         latex_output(end)
         ;
         true).

satisfiable(_, _, unsatisfiable) :-
    print_result('Refutation found'),
    (get_output_format(latex) ->
         latex_output(end)
         ;
         true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

negated_formula(Fml, not(Fml)) :-
    get_negate_first(yes), !.

negated_formula(Fml, Fml).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% search(+Axioms, +Formula)
%
%     Formula is a term including the formula to be tested for
%     satisfiability and encodings of the positions of all subformulae
%     The formula is in negation normal form.

search(Axioms, Fml) :-
    set_satisfiable_flag(-1),
    set_states_counter(0),
    set_proof_steps_counter(-1),
    set_branch_point_counter(0),
    set_sat_reuse_counter(0),
    set_unsat_reuse_counter(0),

    print_proof_step(0, Axioms, '[theory]'),
    print_proof_step(0, Fml, '[given]'),

    search_state(Axioms, [Fml], 0, [], NewBranch, [], NewEventualities, [], _),
    pdl_write('In search, State : '), pdl_write(0), pdl_nl,
    pdl_write('    Axioms : '),       pdl_write(Axioms), pdl_nl,
    pdl_write('    NewBranch : '),    pdl_write(NewBranch), pdl_nl,
    pdl_write('    NewEventualities : '), pdl_write(NewEventualities), pdl_nl,
    test_ignorability(NewBranch, NewEventualities),
%    write('Remembering consistent set - state shown consistent'), write(FmlList), nl,
%    print_proof_step(State, consistent_set(FmlList), '[consistent set]'),
%    store_consistent(FmlList,Branch,NewBranch,Eventualities,NewEventualities)
    print_result('Satisfiable branch found').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% search_state(+Axioms, +Unexpanded, +State, +Branch, -NewBranch, 
%              +Eventualities, -NewEventualities)
%
%   Test whether a model for < State : Unexpanded > can be constructed.
%   The formulae in Unexpanded are assumed to be in negation normal form.
%   * +Branch is the branch constructed so far (i.e. the segment from the
%     root state/world to the parent state/world of +State)
%   * -NewBranch will be a \pi-completed branch extending +Branch
%   * +Eventualities will be the eventualities fulfilled so far on +Branch
%   * -NewEventualities will be eventualities fulfilled on -NewBranch
%   Note: The predicate search_state is also used in reduce_existentials.

search_state(Axioms, Unexpanded, State, Branch, NewBranch, 
        Eventualities, NewEventualities, IDT, ODT) :-
    pdl_write('In search_state, State : '), pdl_write(State),  pdl_nl,
    pdl_write('    Unexpanded : '),         pdl_write(Branch), pdl_nl,

    print_proof_step(State, Axioms, '[theory-inst]'),
    append(Unexpanded, Axioms, UnexpandedPlusAxioms),
    % Remember: +LocalEventualities in the following call to reduce_local are
    % the eventualities fulfilled in the current state +State
    reduce_local(UnexpandedPlusAxioms, State, [], Workedoff, 
                 Universal, Existential, LocalEventualities, IDT, ODT_RL),

    eliminate_duplicates(Workedoff, SimplifiedWorkedoff),
    eliminate_duplicates(Universal, SimplifiedUniversal),
    eliminate_duplicates(Existential, SimplifiedExistential),

    pdl_write('    Branch : '), pdl_write(Branch), pdl_nl,
    pdl_write('    SimplifiedWorkedoff : '), pdl_write(SimplifiedWorkedoff), pdl_nl,
    pdl_write('    SimplifiedUniversal : '), pdl_write(SimplifiedUniversal), pdl_nl,
    pdl_write('    SimplifiedExistential : '), pdl_write(SimplifiedExistential), pdl_nl,
    pdl_write('    LocalEventualities : '), pdl_write(LocalEventualities), pdl_nl,

    test_unsatisfiability(State, SimplifiedWorkedoff, SimplifiedWorkedoff),

    merge_set(SimplifiedWorkedoff, SimplifiedExistential, Aux),
    merge_set(Aux, SimplifiedUniversal, FmlList),

    pdl_write('    FmlList : '), pdl_write(FmlList), pdl_nl,

    prefix_to_list(State,StateList),
    assert(reduced(ODT_RL, StateList, FmlList)),
    (known_consistent(FmlList,BranchForFmlList,EventualitiesForFmlList) ->
	pdl_write('    is known to be consistent'), pdl_nl,
	append(BranchForFmlList,Branch,NewBranch),
	append(EventualitiesForFmlList,Eventualities,NewEventualities)
    ;
	extend_branch(State, FmlList, Branch, ExtendedBranch),
	pdl_write('    ExtendedBranch : '), pdl_write(ExtendedBranch), pdl_nl,

	merge_set(Eventualities, LocalEventualities, ExtendedEventualities),
	pdl_write('    ExtendedEventualities : '), pdl_write(ExtendedEventualities), pdl_nl,

	search_state_aux(Axioms, SimplifiedExistential,
                         SimplifiedUniversal, Unexpanded, State, ExtendedBranch, NewBranch,
                         ExtendedEventualities, NewEventualities, ODT_RL, ODT)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 4th argument not used.
search_state_aux(_, _, _, _, State, 
            [pr(State,FmlList)|Branch], [EqualStates,pr(State,FmlList)|Branch],
            ExtendedEventualities, ExtendedEventualities, IDT, IDT) :-
    pdl_write_ln('In search_state_aux (1)....'),
    pdl_write('    State : '),   pdl_write(State),   pdl_nl,
    pdl_write('    FmlList : '), pdl_write(FmlList), pdl_nl,
    pdl_write('    Branch : '),  pdl_write(Branch),  pdl_nl,
    is_copy(FmlList, State, Branch, EqualStates), !.

search_state_aux(Axioms, SimplifiedExistential, SimplifiedUniversal, 
            _, State, ExtendedBranch, NewBranch,
            ExtendedEventualities, NewEventualities, IDT, ODT) :-
%   pdl_write_ln('In search_state_aux (2)....'),
    reduce_existentials(Axioms, SimplifiedExistential, State,
                       SimplifiedUniversal, ExtendedBranch, NewBranch,
                       ExtendedEventualities, NewEventualities, IDT, ODT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% extend_branch(+State, +FmlList, +Branch, -ExtendedBranch)

extend_branch(State, FmlList, Branch, [pr(State, FmlList)|Branch]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    The key proof_steps_counter counts the proof steps

set_proof_steps_counter(Number) :- !,
    pdl_write('set_proof_steps_counter to '), pdl_write(Number), pdl_nl,
    flag(proof_steps_counter, _, Number).

increment_proof_steps_counter(Increment) :- !,
    flag(proof_steps_counter, Old, Old + Increment).

get_proof_steps_counter(Number) :- !,
    flag(proof_steps_counter, Number, Number).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    The key states_counter counts the states created

set_states_counter(Number) :- !,
    pdl_write('set_states_counter to '), pdl_write(Number), pdl_nl,
    flag(states_counter, _, Number).

increment_states_counter(Increment) :- !,
    flag(states_counter, Old, Old + Increment).

get_states_counter(Number) :- !,
    flag(states_counter, Number, Number).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    The key branch_point_counter counts the branch_points

set_branch_point_counter(Number) :- !,
    pdl_write('set_branch_point_counter to '), pdl_write(Number), pdl_nl,
    flag(branch_point_counter, _, Number).

increment_branch_point_counter(Increment) :- !,
    flag(branch_point_counter, Old, Old + Increment).

get_branch_point_counter(Number) :- !,
    flag(branch_point_counter, Number, Number).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    The key sat_reuse_counter counts the number of times a stored
%    satisfiable set is re-used

set_sat_reuse_counter(Number) :- !,
    pdl_write('set_sat_reuse_counter to '), pdl_write(Number), pdl_nl,
    flag(sat_reuse_counter, _, Number).

increment_sat_reuse_counter(Increment) :- !,
    flag(sat_reuse_counter, Old, Old + Increment).

get_sat_reuse_counter(Number) :- !,
    flag(sat_reuse_counter, Number, Number).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    The key unsat_reuse_counter counts the number of times a stored
%    unsatisfiable set is re-used

set_unsat_reuse_counter(Number) :- !,
    pdl_write('set_unsat_reuse_counter to '), pdl_write(Number), pdl_nl,
    flag(unsat_reuse_counter, _, Number).

increment_unsat_reuse_counter(Increment) :- !,
    flag(unsat_reuse_counter, Old, Old + Increment).

get_unsat_reuse_counter(Number) :- !,
    flag(unsat_reuse_counter, Number, Number).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%     The key satisfiable is used to track satisfiability on the current
%     branch.  
%     -1 means undefined, 0 means unsatisfiable, 1 means satisfiable

set_satisfiable_flag(Value) :-
    flag(satisfiable, _, Value).

get_satisfiability_status(Value) :-
    flag(satisfiable, Value, Value).

is_satisfiable(_) :-
    get_satisfiability_status(1).

is_unsatisfiable(_) :-
    get_satisfiability_status(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% test_ignorability(+Branch,+FulfilledEventualities)
% According to Definition 7 in (De Giacomo and Massacci, 2001),
% a \pi-completed branch B is ignorable iff there is an eventuality X which
% is not fulfilled. A branch is \pi-completed if (i) all prefixes are
% reduced, i.e. all rules except <A>-rules have been applied, and
% (ii) for every s1 which is not fully reduced there is a pair (s0,S0)
% shorter than (s1,B) s.t. s0 is fully reduced in the segment S0 and s1 is
% a copy of s0 in B.
% The procedure only uses +Branch to determine all the eventualities
% present on the branch. Whether these are fulfilled is then checked against
% +FulfilledEventualities, a list of all eventualities currently fulfilled
% on the branch.
% Note that since distinct X_i and X_j can represent the
% same eventuality, this check requires more than just comparing, say, the
% number of syntactically distinct X_i present on the branch with the number
% of syntactically distinct X_i in +FulfilledEventualities.
% Note also, that we obtain +FulfilledEventualities from reduce_local.
% +FulfilledEventualities will only be correct, as long complement splitting
% is used to deal with X_i = <A*>F and as long rules are applied even if one
% of the consequents is present (i.e. the formula F should not subsume <A*>F).

test_ignorability(Branch, FulfilledEventualities) :-
    eventualities(Branch, Eventualities),
    collectCopies(Branch,Copies),
    pdl_write_ln('In test_ignorability... '), 
    pdl_write('    Branch :    '), pdl_write(Branch), pdl_nl,
    pdl_write('    FulfilledEventualities : '), pdl_write(FulfilledEventualities), pdl_nl,
    pdl_write('    Eventualities :    '), pdl_write(Eventualities), pdl_nl,
    test_ignorability_loop(Eventualities, FulfilledEventualities, Copies, Branch, _NewEventualities, _NewFulfilledEventualities).


test_ignorability_loop(Eventualities, FulfilledEventualities, Copies, Branch, NewEventualities, NewFulfilledEventualities) :-
	test_ignorability_aux(Eventualities, FulfilledEventualities, Copies, 
	                      NewEventualities1, NewFulfilledEventualities1),
        (NewEventualities1 == [] ->
	    NewEventualities = NewEventualities1,
	    NewFulfilledEventualities = NewFulfilledEventualities1
	;
	    (NewFulfilledEventualities1 == FulfilledEventualities ->
		print_unfulfilled_eventualities(NewEventualities1),
		compute_inconsistent_sets(Eventualities,Branch,Copies),
		!,
		fail
	    ;
		test_ignorability_loop(NewEventualities1, NewFulfilledEventualities1, Copies, Branch, NewEventualities, NewFulfilledEventualities)
	    )
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% print_unfulfilled_eventualities(+Eventualities)
%
%   print a list of eventualities that are not fulfilled on the current branch

print_unfulfilled_eventualities([]) :-
	!.
print_unfulfilled_eventualities([ev(State,x(Y,Fml))|Eventualities]) :-
	pdl_write('** Ignorable ** - '), pdl_write(ev(State,x(Y,Fml))), pdl_write(' unfulfilled'), pdl_nl,
	print_proof_step('post', '** Ignorable **', ['unfulfilled',ev(State,x(Y,Fml))]), 
%       print_proof_step('post', '** Ignorable/Unsatisfiable **', ['unfulfilled',x(Y,Fml)]), 
	!,
	print_unfulfilled_eventualities(Eventualities).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% compute_inconsistent_sets(+Eventualities, +Branch, +Copies)
%

compute_inconsistent_sets([], _, _) :-
	!.

compute_inconsistent_sets([ev(State,x(_Y,_Fml))|Eventualities], Branch, Copies) :-
	get_formulae_for_state(State,Branch,FmlList),
	memberchk(isCopyOf(State,_),Copies),
	store_inconsistent(FmlList),
	!,
	compute_inconsistent_sets(Eventualities, Branch, Copies).

get_formulae_for_state(State1,[isCopyOf(_,_)|Branch],FmlList) :-
	get_formulae_for_state(State1,Branch,FmlList),	
	!.
get_formulae_for_state(State1,[pr(State1,FmlList)|_Branch],FmlList) :-
	!.
get_formulae_for_state(State1,[pr(State2,_FmlList)|Branch],FmlList) :-
	State1 \= State2,
	get_formulae_for_state(State1,Branch,FmlList),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_ignorability_aux(+Eventualities, +FulfilledEventualities, +Copies,
%                       -NewEventualities, -NewFulfilledEventualities)
%   computes -NewEventualities and -NewFulfilledEventualities from 
%   +Eventualities and +FulfilledEventualities as follows:
%   -NewFulfilledEventualities contains all eventualities in +FulfilledEventualities
%   plus any eventuality in +Eventualities that can be shown to be fulfilled with
%   respect to +FulfilledEventualities and +Copies; 
%   -NewEventualities contains all eventualities in +Eventualities that cannot be
%   shown to be fulfilled with respect to +FulfilledEventualties and +Copies.

test_ignorability_aux([], FulfilledEventualities, _Copies, [], FulfilledEventualities).

test_ignorability_aux([ev(State,Eventuality)|Eventualities], FulfilledEventualities, Copies, NewEventualities, NewFulfilledEventualities) :-
    (test_ignorability_aux_fml(ev(State,Eventuality), FulfilledEventualities, Copies) ->
	append(FulfilledEventualities,[fulfilled(State,Eventuality)],NewFulfilledEventualities1),
	test_ignorability_aux(Eventualities, NewFulfilledEventualities1, Copies, NewEventualities, NewFulfilledEventualities)
    ;
	test_ignorability_aux(Eventualities, FulfilledEventualities, Copies, NewEventualities1, NewFulfilledEventualities),
	NewEventualities = [ev(State,Eventuality)|NewEventualities1]
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_ignorability_aux_fml(+Eventuality, +FulfilledEventualities, +Copies)
%
% succeeds if +Eventuality is a fulfilled eventuality. This in turn is the case
% if (a) among +FulfilledEventualities is a syntactically identical eventuality
% which is already recorded as being fulfilled on the current branch, or
% (b) among +FulfilledEventualities is an eventuality Eventuality2, recorded to
% be fulfilled at a state State2 such that the state State1 at which +Eventuality
% occurs collapsed to State2.

test_ignorability_aux_fml(_, [], _) :-
	fail.

test_ignorability_aux_fml(ev(State1,x(Y,Fml)), [fulfilled(State2, x(X,Fml))|_], Copies) :-
	(X = Y ->
	    print_proof_step('post', fulfilled_by(ev(State1,x(Y,Fml)), ev(State2,x(X,Fml))), '[fulfilled]')
	;
	    collapses(State1,State2,Copies),
	    print_proof_step('post', fulfilled_by(ev(State1,x(Y,Fml)), ev(State2,x(X,Fml))), '[fulfilled]')
	).

test_ignorability_aux_fml(Event, [_|FulfilledEventualities], Copies) :-
    test_ignorability_aux_fml(Event, FulfilledEventualities, Copies).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% collectCopies(+Branch, -Copies)
%  
% whenever a state State1 is identified as a copy of a State2, we record that
% fact by adding `isCopyOf(State1,State2)' on a branch. The predice collectCopies
% retrieves all such facts from +Branch and returns it as a list -Copies.

collectCopies([isCopyOf(State1,State2)|Branch],[isCopyOf(State1,State2)|Copies]) :-
	!,
	collectCopies(Branch,Copies).
collectCopies([_|Branch],Copies) :-
	collectCopies(Branch,Copies),
	!.
collectCopies([],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% collapses(+State1, +State2, +Copies)
% 
% succeeds if +State1 is a copy of +State2 with respect to the information on
% which states are copies of each other contained in +Copies.

collapses(X, Y, Copies) :-
	memberchk(isCopyOf(X,Y),Copies).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_unsatisfiability(+State, +FmlList, +CompleteFmlList)
%
%   test for inconsistency of the list of formulae FmlList;
%   - The first rule states that the empty set/list of formulae is not
%     inconsistent
%   - The second rule covers conditions 1 and 2 of Definition 23 in %
%     (De Giacomo and Massacci, 2000) via a inf_closure
%   - The third rule  covers condition 3 of Definition 23 in
%     (De Giacomo and Massacci, 2000)

test_unsatisfiability(_, [], _) :- !.
% The following rule uses condition 3 of Definition 23 in
% (De Giacomo and Massacci, 2000):
% A superset of a \bot-set is a \bot-set.
test_unsatisfiability(State, Fml, _) :-
    list_to_ord_set(Fml,OrdFml1),
    inconsistent(OrdFml2),
    ord_subset(OrdFml2,OrdFml1),
    pdl_write('Reused unsatisfiable set '), pdl_write(OrdFml2), pdl_nl,
    print_proof_step(State, inconsistent_subset_of(OrdFml2,OrdFml1), '[inconsistent subset]'),
    increment_unsat_reuse_counter(1),
    !,
    fail.
test_unsatisfiability(State, Set, CompleteFmlList) :-
    select_fml(Given, Set, SetWithoutGiven),
    inf_closure(State, Given, SetWithoutGiven, CompleteFmlList),
    test_unsatisfiability(State, SetWithoutGiven, CompleteFmlList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

select_fml(Fml, [Fml|FmlList], FmlList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% is_copy(+FmlList, +CurrentState, +Branch). 
%
%     test if pr(CurrentState, FmlList) is a copy of an earlier state in
%     Branch.

is_copy(_,_,[],_) :-
	fail.
is_copy(FmlList, State, [isCopyOf(_,_)|Branch], EqualStates) :-
	!,
	is_copy(FmlList, State, Branch, EqualStates).
is_copy(FmlList, State, [pr(X,FmlListInX)|_],isCopyOf(State,X)) :-
	is_prefix(X, State),
	% Ignore the first argument in the introduce literals x(_,Fml)
        simplify_X(FmlList, SimplifiedFmlList),
	simplify_X(FmlListInX, SimplifiedFmlListInX),
%    pdl_write_ln('In is_copy... '), 
%    pdl_write('    SimplifiedFmlList :    '), pdl_write(SimplifiedFmlList), pdl_nl,
%    pdl_write('    SimplifiedFmlListInX : '), pdl_write(SimplifiedFmlListInX), pdl_nl,
    equal_set(SimplifiedFmlList, State, SimplifiedFmlListInX, X), !.
is_copy(FmlList, State, [pr(_,_)|Branch], EqualStates) :-
	is_copy(FmlList, State, Branch, EqualStates).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_prefix(X, X).
is_prefix(X, ((Y), _)) :-
    is_prefix(X,Y).

prefix_to_list(0,[0]) :- !.
prefix_to_list(((X), L, N),List) :-
	prefix_to_list(X,List1),
	append(List1,[L,N],List),
	!.

list_to_prefix([0],(0)) :- !.
list_to_prefix([A,B|List],(Prefix1,L,N)) :-
	append(PrefixList,[L,N],[A,B|List]),
	list_to_prefix(PrefixList,Prefix1),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

equal_set([], State, [], X) :- !,
    % X is a copy of State
    print_copies(State, X).

equal_set([Elem|Set1], State, Set2, X) :-
    delete(Set2, Elem, Set3),
    equal_set(Set1, State, Set3, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
eliminate_duplicates(List, SimplifiedList) :- !,
    sort(List, SimplifiedList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% complement(+Fml,-NotFml)
%
%     Returns the complement of Fml.

complement(not(A),A) :- !.

complement(A,not(A)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% reduce_existentials(+Axioms, +Existential, +State,
%        +Universal, +Branch, -NewBranch, 
%        +Eventualities, -NewEventualities)
%
%     - Applies existential, universal and theory rules
%

reduce_existentials(_, [], _, _, Branch, Branch, 
              Eventualities, Eventualities, IDT, IDT).

reduce_existentials(Axioms, Existential, X, Universal, Branch, NewBranch,
              Eventualities, NewEventualities, IDT, ODT) :-  !,
%    pdl_write('In reduce_existentials, X : '), pdl_write(X), pdl_nl,
%    pdl_write('    Existential : '), pdl_write(Existential), pdl_nl,
    select(Fml, Existential, ExistentialWithoutFml),
    Fml = dia(R,A),
    pdl_write('* R = '), pdl_write(R),
%    atom(R),
    pdl_write_ln('* '),
    increment_states_counter(1),
    get_states_counter(N),
    Y = (X,R,N),
    print_proof_step(Y, A, ['dia',Fml]), 
%    pdl_write('In reduce_existentials, Y : '), pdl_write(Y), pdl_nl,

    reduce_universal(Universal, Y, [], Unexpanded),

    extend_unexpanded([A], [], Unexpanded, NewUnexpanded),
    reduce_existential(Axioms, NewUnexpanded, Y, Branch, NewBranch1,
                  Eventualities, NewEventualities1, IDT, ODT_RE),
    reduce_existentials(Axioms, ExistentialWithoutFml, X, Universal, 
                  NewBranch1, NewBranch, NewEventualities1, NewEventualities, ODT_RE, ODT).


reduce_existential(Axioms, NewUnexpanded, Y, Branch, NewBranch, 
                   Eventualities, NewEventualities, IDT, ODT) :-
    search_state(Axioms, NewUnexpanded, Y, Branch, NewBranch, 
                 Eventualities, NewEventualities, IDT, ODT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reduce_universal([], _, Unexpanded, Unexpanded).

reduce_universal([not(dia(R,A))|Universal], Y, Unexpanded, NewUnexpanded) :- 
    % check if Y is an R-successor
    Y = (_,R,_), !,
    complement(A, NotA),
    extend_unexpanded([NotA], [], Unexpanded, NewUnexpanded1, 
                 Y, ['box',not(dia(R,A))]), 
    reduce_universal(Universal, Y, NewUnexpanded1, NewUnexpanded).

reduce_universal([_|Universal], Y, Unexpanded, NewUnexpanded) :- 
    reduce_universal(Universal, Y, Unexpanded, NewUnexpanded).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% extend_unexpanded(+FmlList, +Workedoff, +Unexpanded, -NewUnexpanded, 
%                   +X, +Justification)
% - Adds all non-redundant formulae in FmlList to Unexpanded; 
%   redundancy is evaluated against Workedoff
% - Keeping all expanded formulae in Workedoff has the disadvantage that
%   the performance decreases and proofs are longer.  
%
% TO DO: Consider ways of reducing this bookkeeping while preserving
% soundness and completeness.
%

extend_unexpanded([], _, Unexpanded, Unexpanded, _, _).

extend_unexpanded([Fml|Rest], Workedoff, Unexpanded, NewUnexpanded, X, Justification) :- 
    extend_unexpanded_aux(Fml, Workedoff, Unexpanded, Unexpanded1, 
                     X, Justification), !,
    extend_unexpanded(Rest, Workedoff, Unexpanded1, NewUnexpanded, X, Justification).

extend_unexpanded_aux(true, _, Unexpanded, Unexpanded,
                 X, Justification) :-
    print_proof_step(X, '-', Justification). 

extend_unexpanded_aux(Fml, Workedoff, Unexpanded, [Fml|Unexpanded],
                 X, Justification) :-
    not_redundant(Fml, Workedoff, Unexpanded),
    print_proof_step(X, Fml, Justification).

extend_unexpanded_aux(Fml, _, Unexpanded, Unexpanded, X, Justification) :-
    print_redundant_step(X, Fml, Justification).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% extend_unexpanded(+FmlList, +Workedoff, +Unexpanded, -NewUnexpanded)
%
%     Same as above, but derivation step is not documented
%

extend_unexpanded([], _, Unexpanded, Unexpanded).

extend_unexpanded([Fml|Rest], Workedoff, Unexpanded, NewUnexpanded) :- 
    extend_unexpanded_aux(Fml, Workedoff, Unexpanded, Unexpanded1), !,
    extend_unexpanded(Rest, Workedoff, Unexpanded1, NewUnexpanded).

extend_unexpanded_aux(Fml, Workedoff, Unexpanded, [Fml|Unexpanded]) :-
    not_in_set(Fml, Workedoff),
    not_in_set(Fml, Unexpanded).

extend_unexpanded_aux(_, _, Unexpanded, Unexpanded) :-
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% not_in_set(+Fml, +FmlList)
%

not_redundant(Fml, FmlList1, FmlList2) :-
    not_in_set(Fml, FmlList1),
    not_in_set(Fml, FmlList2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% not_in_set(+Fml, +FmlList)
%

not_in_set(Fml, FmlList) :-
    \+ member(Fml, FmlList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% known_consistent(Fmls)
% If Fmls is a subset of a set of formulae which is already known to be
% consistent, then Fmls is also consistent
%

known_consistent(Fmls,BranchForFmls,EventualitiesForFmls) :-
	list_to_ord_set(Fmls,OrdFmls1),
	consistent(OrdFmls2,BranchForFmls,EventualitiesForFmls),
	ord_subset(OrdFmls1,OrdFmls2),
%	write('Reused satisfiable set '), print(OrdFmls2), nl,
%	write('Reused a satisfiable set '), nl,
	print_proof_step(g, consistent_superset_of(OrdFmls2,OrdFmls1), '[consistent superset]'),
	increment_sat_reuse_counter(1),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print_copies(+State, +CopyState)
%

print_copies(State, CopyState) :- 
    print_proof_step(State, copy_of(State, CopyState), '[loop checking]').

%%print_copies(State, CopyState) :- 
%%    sprintf_state(StateString, State),
%%    string_to_atom(StateString, State1),
%%    sprintf_state(CopyStateString, CopyState),
%%    swritef(String, 'copy of %w', [CopyStateString]),
%%    string_to_atom(String, StringAtom),
%%    print_proof_step(State1, StringAtom, 'loop checking').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_result(Result) :-
    write('** '), write(Result), write_ln(' **'),
    get_search_output_filename(Filename),
    append(Filename),
    (get_output_format(latex) ->
	write('& \\text{** '), write(Result), write_ln(' **}')
    ;
	write('** '), write(Result), write_ln(' **')
    ),
    told.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_proof_step(_, [], _) :- !.

print_proof_step(State, [Fml|FmlList], Justification) :- !,
    print_proof_step(State, Fml, Justification),
    print_proof_step(State, FmlList, Justification).

print_proof_step(State, Conclusion, Justification) :-
    increment_proof_steps_counter(1),
    get_proof_steps_counter(N),
%    print_proof_step_aux(N, State, Conclusion, Justification),
    get_search_output_filename(Filename),
    append(Filename),
    (get_output_format(latex) ->
         print_proof_step_latex(N, State, Conclusion, Justification)
    ;
	print_proof_step_aux(N, State, Conclusion, Justification)
    ),
    told.

print_proof_step_aux(N, State, Conclusion, Justification) :-
    pdl_writef('%4r.  ', [N]),
    get_branch_point_counter(Count),
    pdl_tab(Count * 4),
    print_state(State), pdl_write(' : '),
    print_conclusion(Conclusion), pdl_nl, 
    pdl_write('    '), 
%    pdl_write('                        '), 
    pdl_write(Justification), pdl_nl.


print_proof_step_latex(N, State, Conclusion, Justification) :-
    pdl_write(N), pdl_write('.\\  & '),
    get_branch_point_counter(Count),
    print_quad(Count),
    print_state_latex(State), pdl_write(' {:} '),
    print_conclusion_latex(Conclusion), pdl_nl, 
    pdl_write('\\tag*{$'), 
    print_justification_latex(Justification), !, 
    pdl_write('$}'), pdl_nl,
    pdl_write('\\\\'), pdl_nl.

print_quad(0).

print_quad(Count) :-
    pdl_write('\\quad\\qquad '), 
    CountDecr is Count - 1,
    print_quad(CountDecr). 
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_redundant_step(_, [], _) :- !.

print_redundant_step(State, [Fml|FmlList], Justification) :- !,
    print_redundant_step(State, Fml, Justification),
    print_redundant_step(State, FmlList, Justification).

print_redundant_step(State, Conclusion, Justification) :-
    print_proof_step_aux('-', State, Conclusion, Justification),
    get_search_output_filename(Filename),
    append(Filename),
    (get_output_format(latex) ->
         print_proof_step_latex('-', State, Conclusion, Justification)
    ;
	print_proof_step_aux('-', State, Conclusion, Justification)
    ),
    told.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_justification_latex(String) :- 
    atom(String), !,
    rule_name_latex(String, LatexString),
    pdl_write(LatexString).

print_justification_latex([String|FmlList]) :-
    atom(String), !,
    rule_name_latex(String, LatexString),
    pdl_write(LatexString), pdl_write('{}: '), 
    print_formula_list_latex(FmlList).

print_justification_latex(Justification) :- !,
    pdl_write(Justification).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule_name_latex(true, '\\mltop').
rule_name_latex('or-LEFT', '\\text{${\\mlor}$-left}').
rule_name_latex('or-RIGHT', '\\text{${\\mlor}$-right}').
rule_name_latex('or-RIGHT1', '\\text{${\\mlor}$-right1}').
rule_name_latex('or-RIGHT2', '\\text{${\\mlor}$-right2}').
rule_name_latex('and', '\\neg{\\mlor}').
%rule_name_latex('and', '{\\mland}').
rule_name_latex('dia-or-LEFT', '\\text{$\\mldia{\\mlor}{}$-left}').
rule_name_latex('dia-or-RIGHT', '\\text{$\\mldia{\\mlor}{}$-right}').
rule_name_latex('dia-or-RIGHT1', '\\text{$\\mldia{\\mlor}{}$-right1}').
rule_name_latex('dia-or-RIGHT2', '\\text{$\\mldia{\\mlor}{}$-right2}').
rule_name_latex('dia-comp', '\\mldia{\\comp{}{}}{}').
rule_name_latex('dia-star', '\\mldia{\\ast}{}').
rule_name_latex('dia-test', '\\mldia{?}{}').
rule_name_latex('dia', '\\mldia{\\cdot}{}').
rule_name_latex('X-LEFT', '\\text{$X$-left}').
rule_name_latex('X-RIGHT', '\\text{$X$-right}').
rule_name_latex('X-RIGHT1', '\\text{$X$-right1}').
rule_name_latex('X-RIGHT2', '\\text{$X$-right2}').
rule_name_latex('box-or', '\\neg\\mldia{\\mlor}{}').
rule_name_latex('box-comp', '\\neg\\mldia{\\comp{}{}}{}').
rule_name_latex('box-star', '\\neg\\mldia{\\ast}{}').
rule_name_latex('box-test-LEFT', '\\text{$\\neg\\mldia{?}{}$-left}').
rule_name_latex('box-test-RIGHT', '\\text{$\\neg\\mldia{?}{}$-right}').
rule_name_latex('box-test-RIGHT1', '\\text{$\\neg\\mldia{?}{}$-right1}').
rule_name_latex('box-test-RIGHT2', '\\text{$\\neg\\mldia{?}{}$-right2}').
rule_name_latex('box', '\\neg\\mldia{\\cdot}{}').
%rule_name_latex('box-or', '\\mlbox{\\mlor}{}').
%rule_name_latex('box-comp', '\\mlbox{\\comp}{}').
%rule_name_latex('box-star', '\\mlbox{\\ast}{}').
%rule_name_latex('box', '\\mlbox{\\cdot}{}').
rule_name_latex('[theory]', '\\text{theory}').
rule_name_latex('[theory-inst]', '\\text{theory-inst}').
rule_name_latex('[given]', '\\text{given}').
rule_name_latex('[fulfilled]', '\\text{fulfilled}').
rule_name_latex('[loop checking]', '\\text{loop checking}').
rule_name_latex(String, LatexString) :-
    swritef(LatexString, '\\\\text{%w}', [String]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

justification_latex('[given]', 'given').
justification_latex('[fulfilled]', 'fulfilled').
justification_latex('[loop checking]', 'loop checking').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_formula_list_latex([]).

print_formula_list_latex([Fml]) :-
    print_formula_latex(Fml). 

print_formula_list_latex([Fml|List]) :-
    print_formula_latex(Fml), 
    pdl_write(', '),
    print_formula_list_latex(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_formula_latex(true) :- !,
    pdl_write('\\mltop').

print_formula_latex(false) :- !,
    pdl_write('\\mlbot').

print_formula_latex(x(Y,Fml)) :- !,
    pdl_write('X^{'), print_state(Y), pdl_write('}_{'), 
    print_formula_latex(Fml), pdl_write('}').

print_formula_latex(ev(State,x(Y,Fml))) :- !,
    print_state(State), pdl_write(' {:} '),
    pdl_write('X^{'), print_state(Y), pdl_write('}_{'), 
    print_formula_latex(Fml), pdl_write('}').
print_formula_latex(at(State,Fml)) :- !,
    print_state(State), pdl_write(' {:} '),
    print_formula_latex(Fml).

print_formula_latex(and(A,B)) :- !,
    pdl_write('('),
    print_formula_latex(A), 
    pdl_write(' \\mland '),
    print_formula_latex(B),
    pdl_write(')').

print_formula_latex(or(A,B)) :- !,
    pdl_write('('),
    print_formula_latex(A), 
    pdl_write(' \\mlor '),
    print_formula_latex(B),
    pdl_write(')').

print_formula_latex(not(A)) :- !,
    pdl_write('\\neg '),
    print_formula_latex(A).

print_formula_latex(test(A)) :- !,
    pdl_write('\\test{'),
    print_formula_latex(A),
    pdl_write('}').

print_formula_latex(comp(A,B)) :- !,
    pdl_write('(\\comp{'),
    print_formula_latex(A), 
    pdl_write('}{'),
    print_formula_latex(B),
    pdl_write('})').

print_formula_latex(star(A)) :- !,
    print_formula_latex(A),
    pdl_write('{}^*').

print_formula_latex(dia(R,A)) :- !,
    pdl_write('\\mldia{'),
    print_formula_latex(R), 
    pdl_write('}{'),
    print_formula_latex(A),
    pdl_write('}').

print_formula_latex(box(R,A)) :- !,
    pdl_write('\\mlbox{'),
    print_formula_latex(R), 
    pdl_write('}{'),
    print_formula_latex(A),
    pdl_write('}').

print_formula_latex(A) :- !,
    pdl_write('\\textit{'), pdl_write(A), pdl_write('}').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

latex_output(begin) :- !,
    get_search_output_filename(Filename),
    append(Filename),
    pdl_write_ln('\\begin{align*}'),
    told.

latex_output(end) :- !,
    get_search_output_filename(Filename),
    append(Filename),
    pdl_write_ln('\\end{align*}'),
    told.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sprintf_state(String, (X,Y)) :- !,
    sprintf_state(XString, X), 
    sprintf_state(YString, Y),
    swritef(String, '%w.%w', [XString, YString]).

sprintf_state(XString,X) :-
    string_to_atom(XString, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_state((X,Y)) :- !,
    print_state(X), pdl_write('.'), print_state(Y).

print_state(X) :- 
    pdl_write(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_state_latex((X,Y)) :- !,
    print_state_latex(X), pdl_write('.'), print_state_latex(Y).

print_state_latex(X) :- 
    number(X), !,
    pdl_write(X).

print_state_latex(X) :- !,
    pdl_write('\\textit{'), pdl_write(X), pdl_write('}').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_conclusion(fulfilled_by(Fml1,Fml2)) :- !,
    pdl_write('\\ '), pdl_write(Fml1), pdl_write(' fulfilled by '), pdl_write(Fml2).

print_conclusion(copy_of(X,Y)) :- !,
    print_state(X), pdl_write(' copy of '), print_state(Y).

print_conclusion(inconsistent_subset_of(X,Y)) :- !,
    print_state(X), pdl_write(' is an inconsistent subset of '), print_state(Y).

print_conclusion(consistent_superset_of(X,Y)) :- !,
    print_state(X), pdl_write(' is a consistent superset of '), print_state(Y).

print_conclusion(inconsistent_set(X)) :- !,
    print_state(X), pdl_write(' is an inconsistent set').

print_conclusion(consistent_set(X)) :- !,
    print_state(X), pdl_write(' is an consistent set').

print_conclusion(X) :- 
    pdl_write(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_conclusion_latex(fulfilled_by(Fml1,Fml2)) :- !,
    pdl_write(' '),
    print_formula_latex(Fml1), 
    pdl_write('\\textit{ fulfilled by }'), 
    print_formula_latex(Fml2).

print_conclusion_latex(copy_of(X,Y)) :- !,
    print_state_latex(X), 
    pdl_write('\\textit{ copy of }'), 
    print_state_latex(Y).

print_conclusion_latex(consistent_set(X)) :- !,
    pdl_write('\\begin{array}[t]{l}'), pdl_nl,
    print_formula_list_latex(X), pdl_write('\\\\ '), pdl_nl,
    pdl_write('\\textit{ is a consistent set }'), pdl_nl,
    pdl_write('\\end{array}').

print_conclusion_latex(inconsistent_set(X)) :- !,
    pdl_write('\\begin{array}[t]{l}'), pdl_nl,
    print_formula_list_latex(X), pdl_write('\\\\ '), pdl_nl,
    pdl_write('\\textit{ is an inconsistent set }'), pdl_nl,
    pdl_write('\\end{array}').

print_conclusion_latex(consistent_superset_of(X,Y)) :- !,
    pdl_write('\\begin{array}[t]{l}'), pdl_nl,
    print_formula_list_latex(X), pdl_write('\\\\ '), pdl_nl,
    pdl_write('\\textit{ is a consistent superset of }\\\\ '), pdl_nl,
    print_formula_list_latex(Y), pdl_write('\\\\ '), pdl_nl,
    pdl_write(' \\end{array}').

print_conclusion_latex(inconsistent_subset_of(X,Y)) :- !,
    pdl_write('\\begin{array}[t]{l}'), pdl_nl,
    print_formula_list_latex(X), pdl_write('\\\\ '), pdl_nl,
    pdl_write('\\textit{ is an inconsistent subset of }\\\\ '), pdl_nl,
    print_formula_list_latex(Y), pdl_write('\\\\ '), pdl_nl,
    pdl_write(' \\end{array}').

print_conclusion_latex(X) :- 
    print_formula_latex(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_list([]).

print_list([Head|List]) :-
    pdl_write(Head), 
    pdl_write(' '),
    print_list(List).
 
