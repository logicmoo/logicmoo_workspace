

/* OC precede    tlm 26 OCT 97 */
/* follows algorithm in AIJ  except is at the o-level */

:- multifile sort_info/2.
:- dynamic sort_info/2.


/************************* NEW PRECEDE CODE ***********************/

/* hmm - this removes corres vars within ss's !? */
% eg. bc: I_list = 
%  [[at(_D,_E)],[position(_B,_C),inside(_B,_A)],[position(_B,_C)]]

precede_codez(Codez) :-
     substate_codez(Codez),
     tell(user) , nl , write('Comparisons are in: new_precede_compare') ,
        nl ,
        tell(new_precede_compare) ,
        write_out_list(Codez) ,
        close(new_precede_compare) .
 


substate_codez(Codez) :-
     setof(I,S^I^substate_classes(S,_,I),I),
     append_llists(I,I_list),
     p_codez(I_list,I_list,[],Codez).

% p_codez
% -------------------------------------
p_codez([],[],C,C) .
p_codez([A|B],Ip,C_in,C_out) :-
        p_codez2(A,Ip,[],Codez),
        write('.'),
        append(C_in,Codez,C_tmp) ,
        remove_el(Ip,A,Ip1) ,
        p_codez(B,Ip1,C_tmp,C_out) .
p_codez(_,_,C,C) :- nl,write('warning - premature exit!'),
                    nl,write(C),nl.
 
% p_codez2
% -------------------------------------------------------------------------
% main processing loop:
%
% if preds are in precede_ignore then skip
% %       -- test_codez outputs the sets of codez constraints
%%       -- precede_instantiate outputs legal instan for each constraint
%p_codez2(_,[],C,C) .
%

p_codez2(_,[],C,C).
p_codez2(A,[I|I_rest],C_in,C_out) :-
        copy_term(A,A1) ,
        copy_term(I,I1) ,
        precede_codez(A1,I1,C) ,
 
        % generate the sets of codez constraints for the pairs of vars
        test_codez(C,Cs) ,
        % output the number of comparisons ....
        length(Cs,Lcs) ,
       % get instantiations of comparisons
        precede_instantiate(A1,I1,Cs,[],Instan) ,
       % get rid of any `[]' from instans resulting from
       %  impossible ne's when there are not enough objs..
        filter_list(Instan,is_not_empty_list,InstanF),
        append(C_in,InstanF,C_tmp) ,
        p_codez2(A,I_rest,C_tmp,C_out),!.


is_not_empty_list([_|_]).

precede_codez(L1,L2,CC) :-
        list_sort_vars(L1,V1) ,
        list_sort_vars(L2,V2) ,
        sorts_to_compare(V1,V2,Ss) ,
        vars_to_compare(V1,V2,Ss,C) ,
        % get x product of each list of vars
        vars_xprod(C,Codez) ,
        u_set_append_llists(Codez,CC) .

% list_sort_vars: [preds] --> [[sort,var],...]
% ------------------------------------------------------------
% assumes that sort_info/2 has been specified in state_invariants
% see sort_info_exswJ as an example
 
list_sort_vars(Preds,Vars) :-
        % copy_term(Preds,P) ,  % to get rid of shared vars between lists
        filter_list(Preds,is_a_dynamic_pred,Preds1),
        list_sort_vars(Preds1,[],Vars) .
 
list_sort_vars([],V,V) .
list_sort_vars([ne(_,_)|P_tail],V_in,V_out) :-
        list_sort_vars(P_tail,V_in,V_out),!.
list_sort_vars([P|P_tail],V_in,V_out) :-
        sort_info(P,Sorts),
        update_sort_vars(Sorts,V) ,
        uninstan_set_append(V_in,V,V_tmp) ,
        list_sort_vars(P_tail,V_tmp,V_out),!.

% test list_sort_vars([box_in(_493,_500),box_next_box(_493,_494),ne(_493,_494)],_943)
 
%  sort_info(part_of(_20065,_20066),_26417)
% update_sort_vars: [is_of_sort(Var,sort),..] * sort/var list --> updated list
% -----------------
update_sort_vars([],[]) .
update_sort_vars([is_of_sort(V,Sort)|S_tail],[[Sort,V]|V_tail]) :-
        update_sort_vars(S_tail,V_tail) .
 
sorts_to_compare(L1,L2,Ss) :-
        % get setof sorts overall in lists
        setof(S,S^V^(member([S,V],L1);member([S,V],L2)),Ss) .
 
% vars_to_compare:
% ------------------
% looks at each list of sort/vars input and returns list of
%       [sort,[vars in list1],[vars in list2]]
% and ignores any sort that doesn't have vars in both lists
vars_to_compare(_,_,[],[]) .
vars_to_compare(V1,V2,[S|S_rest],[[S,C1,C2]|C_rest]) :-
        setof(Sv1,Sv1^S^member([S,Sv1],V1),C1) ,
        setof(Sv2,Sv2^S^member([S,Sv2],V2),C2) ,
        vars_to_compare(V1,V2,S_rest,C_rest) .
% ignore vars of any sorts that aren't in both lists of preds
vars_to_compare(V1,V2,[_|S_rest],C_rest) :-
        vars_to_compare(V1,V2,S_rest,C_rest) .


% vars_xprod: [[sort,[v1],[v2]],...] --> [[sort,[v in vars1,v in v2],[ ,]],...]
% -----------
% input: list whose els are: sort, list of vars of sort sort from original
% list1, and list of vars of sort sort from original list2
% output: list whose els are: sort, codez pair.
vars_xprod([],[]) .
vars_xprod([[S,V1,V2]|S_rest],[X|X_rest]) :-
        xprod([V1,V2],X) ,
        vars_xprod(S_rest,X_rest) .
 
% test_codez
% ----------------------
% input: [[a,s],[w,e],[r,t],[g,f]] i.e. the pairs for comparison
% output: 2^n comparisons -- where n is the no of pairs
 
test_codez(Pairs,Codez) :-
        test_codez(Pairs,[[]],Codez) .
 
test_codez([],C,C) .
test_codez([P|P_rest],C_in,C_out) :-
        % for each el in C_in -- 2 new for P (ne(P) and eq(P))
        new_codez(C_in,P,C_tmp) ,
        test_codez(P_rest,C_tmp,C_out) .
 
new_codez([],_,[]) .
new_codez([C|C_rest],[P1,P2],[C1,C2|Z]) :-
        append(C,[ne(P1,P2)],C1) ,
        append(C,[eq(P1,P2)],C2) ,
        new_codez(C_rest,[P1,P2],Z) .
 
% precede_instantiate
% ----------------------------------------------------
% instantiate the 2 input pred lists according to constraints
precede_instantiate(L1,L2,[C|C_rest],I_in,I_out) :-
         not(new_constraint_ok(C)) ,
nl , write(constraint(C)),
write(' ** Impossible constraints ** skipping') , nl ,
          dots,
         precede_instantiate(L1,L2,C_rest,I_in,I_out) .

precede_instantiate(L1,L2,[],I,I) .

precede_instantiate(L1,L2,[C|C_rest],I_in,I_out) :-
        precede_instantiate(L1,L2,C,I) ,
        append(I_in,[I],I_tmp) ,
        precede_instantiate(L1,L2,C_rest,I_tmp,I_out) .
 
 
precede_instantiate(L1,L2,C,[I1,I2]) :-
        % write(constraints(C)) , nl ,
        not(not(p_instantiate(L1,L2,C))) ,
        retract(pi(I1,I2)) ,
        % nl , write(instan(I1,I2))  , nl .
        dots.
precede_instantiate(_,_,_,[]) .
%
new_constraint_ok(C) :-
%        numvars(C,1,_),
        member(ne(X,Y),C),
        chain_of_equals(C),
        X == Y,
        !,fail.
new_constraint_ok(C) :-
        !.

chain_of_equals([eq(X,Y)|CT]) :-
        X = Y,
        chain_of_equals(CT),!.
chain_of_equals([ne(_,_)|CT]) :-
        chain_of_equals(CT),!.
chain_of_equals([]).
        
% test 
% new_constraint_ok([ne(_78517,_78532),eq(_78517,_78533),eq(_78518,_78532),eq(_78518,_78533),ne(_78524,_78539)])

p_instantiate(L1,L2,Con) :-
        retractall(pi(_)) ,
        !,
        filter_list(L1,is_a_dynamic_pred,L1_d),
        filter_list(L1,is_a_static_pred,L1_s),
        filter_list(L2,is_a_dynamic_pred,L2_d),
        filter_list(L2,is_a_static_pred,L2_s),
        precede_instan(L1_d) ,
        precede_instan(L2_d) ,
        constraints_ok(Con) ,
% now check this is ok with statics ..
        append(L1_s,L2_s,LL),
        !,
        (LL = [] ;
                  ( atomic_invariants(Always),
                    listtoand(Always,AlwaysA),
                    listtoand(LL,LLA),!,
                    hold(LLA,AlwaysA) )
        ),
        assert(pi(L1,L2)) .
p_instantiate(L1,L2,Con) :-
        nl,write(L1),
        nl,write(L2),
        nl,write(Con),nl,write('** FAILED** '),nl,nl,
        ! , fail .

/* exs

precede_instan([arm_used(tom,key1)])
precede_instan([closed(door23),locked(door23)])

*/

precede_instan([]).
precede_instan([X|Y]) :- 
%         not( X = ne(_,_) ),
%         not( atomic_invariants(A),member(X,A)),
        X =.. [H|T],
        sort_info(X,S),
        get_ins_for_list(H,S,T),
        precede_instan(Y).        

get_ins_for_list(H,[],[]).
get_ins_for_list(H,[is_of_sort(_,Sort)|S],[X|Y]) :-
        var(X),
        objects(Sort,Obs),
        member(X,Obs),
        get_ins_for_list(H,S,Y).
% for hierarchical sorts
get_ins_for_list(H,[is_of_sort(_,Sort)|S],[X|Y]) :-
        var(X),
        sorts(Sort,SL),
        member(Sort_L,SL),
        get_ins_for_list(H,[is_of_sort(_,Sort_L)|S],[X|Y]).

get_ins_for_list(H,[is_of_sort(_,Sort)|S],[X|Y]) :-
        not(var(X)),
        get_ins_for_list(H,S,Y).

 
constraints_ok([]) .
constraints_ok([ne(A,B)|C_rest]) :-
        ! , not(A=B) ,
        constraints_ok(C_rest).
constraints_ok([eq(A,B)|C_rest]) :-
        ! , A=B ,
        constraints_ok(C_rest).
 
 
% u_intersection
% --------------------------------
% returns intersection -- where args are uninstantiated vars
u_intersection([],_,[]) :-
        ! .
u_intersection([A|B],Eq,[A|C]) :-
        u_mem(A,Eq) ,
        u_intersection(B,Eq,C) , !.
u_intersection([_|B],Eq,C) :-
        u_intersection(B,Eq,C) , !.
 






/******************************************************************/
%.. sort_info(next(Box1,Box2),[is_of_sort(Box1,box),is_of_sort(Box2,box)]).

produce_sort_info :-
        predicates(L),
        produce_sort_info(L),!.

produce_sort_info([]).
produce_sort_info([X|L]) :-
        generalise(X,X1),
        X =.. [H|T],
        X1 =.. [H|T1],
        produce_sort_ofs(T,T1,S),
        assert(sort_info(X1,S)),
        produce_sort_info(L).

generalise(X,X1) :-
        X =.. [H|T],
        genlist(T,T1),
        X1 =.. [H|T1],!.
genlist([],[]) :- !.
genlist([H|T],[H1|T1]) :-
        genlist(T,T1),!.

produce_sort_ofs([],[],[]).
produce_sort_ofs([T|L],[T1|L1],[is_of_sort(T1,T)|S1]) :-
        produce_sort_ofs(L,L1,S1),!.
        
        

