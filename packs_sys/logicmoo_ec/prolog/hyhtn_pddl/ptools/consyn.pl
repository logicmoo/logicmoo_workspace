/* --------------------consyn.pl----------------------------

 -- checks defn of sort hierarchy 
 -- checks that every predicate in the substate_classes
    also appears in the pred defn

 -- checks that every predicate in the atomic_invariants
    are defined.

 ----------------------------------------------------------*/

start_syn :-
   nl,write('checking basic syntax'),nl,
   get_sorts(Prim, Non_prim,All),
   nl,write('   **checking primitive sorts'),nl,
   crossP(Prim),
   crossP_sorts(Prim),
   nl,write('   **checking non-primitive sorts'),nl,
   cross_sorts(Non_prim,All),
   cross(Non_prim),
   nl,write('   **checking substate defn sorts**'),nl,
   cross_sort_substate(All), 
   nl,write('   **checking substate defn predicates**'),nl,
   cross_pred_defs, 
   nl,write('   **checking atomic predicates**'),nl,
   cross_atom_defs, 
   nl,write('   **finished syntax check'),nl,!.
start_syn :-
   check_complete. 

get_sorts(P,N,ALL) :-
   sorts(primitive_sorts, P),
    (sorts(non_primitive_sorts, N) ; N = [ ]),
   append(P,N,ALL),
   !.

/* primitives */

crossP_sorts([]) :- nl,!.
crossP_sorts([H|T]) :- 
   objects(H,_),
   crossP_sorts(T),!.
crossP_sorts([H|T]) :- nl,write('     warning: sort '),write(H),
                 write(' has no objects'),nl,crossP_sorts(T),!.

crossP(P) :- objects(S,_),crossP1(S,P).
crossP(_).
crossP1(S,P) :- member(S,P),!,fail.
crossP1(S,_) :- nl,write('     warning: sort '),write(S),
                write(' not in sort defn'),nl,fail.

cross_sorts([],_) :- nl,!.
cross_sorts([H|T],A) :- 
   sorts(H,Sub),
   local_subset(Sub,A),
   cross_sorts(T,A),!.
cross_sorts([H|T],A) :- 
   nl,write('     warning: sort '),write(H),
   write(' not properly defined'),nl,cross_sorts(T,A),!.

cross(P) :- sorts(S,_),cross1(S,P).
cross(_).
cross1(S,P) :- member(S,P),!,fail.
cross1(non_primitive_sorts,P) :- !,fail.
cross1(primitive_sorts,P) :- !,fail.
cross1(S,_) :- nl,write('     warning: sort '),write(S),
                write(' not in sort defn'),nl,fail.

% checking sorts in substates are defined
cross_sort_substate([]) :-!.
cross_sort_substate(All) :-
      substate_classes(Sort,_,_),
      cross_sort_substate1(Sort,All),fail.

cross_sort_substate1(Sort,All):-
      not(not(member(Sort,All))),!.
cross_sort_substate1(Sort,All) :-
      not(member(Sort,All)),
      nl, write('     warning: sorts: '),write(Sorts),
      write(' not in sort defn'),nl,!.
   
cross_pred_defs :-
      get_invariants(_, PList),
      predicates(X),
      cross_pred_defs(PList,X).
cross_pred_defs([],_) :- !. 
cross_pred_defs([H|T],X) :-
      not(not(member(H,X))),
      cross_pred_defs(T,X),!.
cross_pred_defs([H|T],X) :-
      nl, write('     warning: undefined predicate: '),write(H),nl,
      cross_pred_defs(T,X),!.
   
cross_atom_defs :-
      atomic_invariants(A),
      predicates(X),
      cross_atom_defs(A,X).

cross_atom_defs([],X).
cross_atom_defs([H|T],X) :- 
% copy
      functor(H,F,NN),
      functor(HC,F,NN),
      member(HC,X),
      cross_atoms(H,HC),
      cross_atom_defs(T,X),!.
cross_atom_defs([H|T],X) :-
      nl, write('     warning: undefined predicate: '),write(H),nl,
      cross_atom_defs(T,X),!.

cross_atoms(H,HC) :- 
      HC =.. [P|SortsL],
      H =.. [P|ObjsL],
      cross_each_obj(SortsL,ObjsL),!.

cross_each_obj([],[]) :- !.

cross_each_obj([S|T1],[O|T2]) :-
       is_of_sort(O,S),
       cross_each_obj(T1,T2),!. 

cross_each_obj([S|T1],[O|T2]) :-
      nl,write('     warning: undefined object: '),write(O), 
      write(' of sort '),write(S),nl,       
      cross_each_obj(T1,T2),!. 

local_subset([],_) :- !.
local_subset([H|T],SS) :-
    member(H,SS),
    local_subset(T,SS),!.
