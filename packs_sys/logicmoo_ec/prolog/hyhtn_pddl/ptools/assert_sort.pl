/*------------------------assert_sort.pl---------------------------

  check and assert (if don't exist): 
        primitive and non_primitive sorts
 
-------------------------------------------------------------------*/
assert_sort:-
        nl,write('assert primitive and non_primitive sorts:'),nl,
        assert_p_np_sort,
        nl,write('----------------------------------------'),nl,!.


% no non_primitive_sorts, no primitive_sorts, sytax error--no objects
assert_p_np_sort:-
        not(sorts(_,_)),
        not(objects(_,_)),
        nl,write('warning: --no objects******'),
        nl,!.

% no non_primitive_sorts, primitive_sorts are already exist
assert_p_np_sort:-
        setof(A, A^B^sorts(A,B),O),
        [primitive_sorts] = O,
        nl,
        write('no non_prim sorts, prim sorts already exist. no assert'),
        nl,!.

% non_primitive_sorts and primitive_sorts are already exist
assert_p_np_sort:-
        setof(A, A^B^sorts(A,B),O),
        member(non_primitive_sorts,O),
        member(primitive_sorts,O),
        nl,write('already exist. no assert'),nl,!.

% no non_prim sorts, assert prim sorts
assert_p_np_sort:-
        not(sorts(_,_)),
        setof(A, A^B^objects(A,B),P),
        assert(sorts(primitive_sorts,P)),
        nl,write('sorts(primitive_sorts,'),
        write(P), write(').'),
        nl,
        write('          ...prim sorts asserted.'),
        nl,!.

% prim sorts already exist, assert non_prim sorts
assert_p_np_sort:-
        setof(A, A^B^sorts(A,B),O),
        not(member(non_primitive_sorts,O)),
        member(primitive_sorts, O),
        remove_el(O,primitive_sorts,Np),
        assert(sorts(non_primitive_sorts,Np)),
        nl,write('sorts(non_primitive_sorts,'),
        write(Np), write(').'),
        nl,
        write('           ...non_primitive_sorts asserted.'),
        nl,!.

% non_prim sorts already exist, assert prim sorts
assert_p_np_sort:-
        setof(A, A^B^sorts(A,B),O),
        not(member(primitive_sorts,O)),
        member(non_primitive_sorts, O),
        setof(A, A^B^objects(A,B),P),
        assert(sorts(primitive_sorts,P)),
        nl,write('sorts(primitive_sorts,'),
        write(P), write(').'),
        nl,
        write('          ...primitive_sorts asserted.'),
        nl,!.

%  assert non_prim sorts and prim sorts
assert_p_np_sort:-
        setof(A, A^B^sorts(A,B),O),
        setof(A, A^B^objects(A,B),P),
        not(member(primitive_sorts,O)),
        not(member(non_primitive_sorts, O)),
        assert(sorts(non_primitive_sorts,O)),
        nl,write('sorts(non_primitive_sorts,'),
        write(O), write(').'),
        nl,
        write('          ...non_primitive_sorts asserted.'),
        assert(sorts(primitive_sorts,P)),
        nl,write('sorts(primitive_sorts,'),
        write(P), write(').'),
        nl,
        write('          ...primitive_sorts asserted.'),
        nl,!.

%  others, syntax errors, 
assert_p_np_sort:-
        nl,write('syntax error exist!'),nl.
