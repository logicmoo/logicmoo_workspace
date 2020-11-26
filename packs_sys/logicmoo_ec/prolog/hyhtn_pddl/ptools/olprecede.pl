
/* OC new "list precede"                tlm 26 OCT 97 */
/* follows algorithm in AIJ  except is at the o-level */
/* changed at April 2000   Donghong Liu for OCL V1.1  */
 
:- multifile sort_info/2.
:- dynamic sort_info/2.
  

% oprecede
% -----------------------------------------------------------------------
oprecede :-
        
	% get comparisons -- instan pairs of preds, substates or both
	precede_codez(Codez) ,	% defined in PRECEDE/new_precede
	% test for precede orders
	precede(Codez,1,[],Orders,[],Cycles) ,

	% if want to display with daVinci
	% assert_precede_as_graph(Orders) ,  


	% convert back to variables
        generalise_precede(Orders,Gen_orders) ,
	% convert back to variables
        generalise_precede(Cycles,Gen_cycles) ,

	% write orders out to file
        tell(user) , nl , write('Orders in: new_precede_orders') , nl ,
        tell('new_precede_orders') ,
        write_precede(orders,Gen_orders) ,told,
 
	% write cycles out to file
        tell(user) , nl , write('Cycles in: new_precede_cycles') , nl ,
        tell('new_precede_cycles') ,
        write_precede(cycles,Gen_cycles) ,
        told .



% precede/6
% ----------------------------------------------------------------------------
% input list of pairs of ground ss's: 
% [ [[preds1],[a]] ,[[preds2],[b]],.., [[preds3],[c]], ..]
% all args MUST be instantiated

precede([],_,P,P,Cyc,Cyc) .
precede([[A,B]|C],T,P_in,P_out,C_in,C_out) :-
	dots ,
	necessary_before(T,A,B,P_in,P_tmp,C_in,C_tmp) ,
	precede(C,T,P_tmp,P_out,C_tmp,C_out) .
precede([_|C],T,P_in,P_out,C_in,C_out) :-		
	precede(C,T,P_in,P_out,C_in,C_out) .	
	

% necessary_before  /  7
% ----------------------------------------------------------------------------
% need to test:
% 	(a) necessary_before(A,B) 
%	(b) necessary_before(B,A)
% and then the options are:		return codes:
% (1) order found if: a&~b or b&~a	<-- [A,B] or [B,A]
% (2) cycle found if: a&b		<-- [cycle,A,B] 
% (3) skip if: ~a&~b			<-- []
%


% skip if preds are the same
necessary_before(T,A,A,P,P,C,C) .	


% skip if A |= B or B |= A, where A and B are of course
% from different ss-classes.
% necessary_before(T,A,B,P,P,C,C) :-
%  	(implied_by(A,B) 		
%  	;
%  	implied_by(B,A)) .
% 

% skip if one of pair is a member of the other 
% (e.g. [in_room(robot,room1)],[in_room(robot,room1),at_dr(robot,d12)])
% NO ! not any more - we are tryig to achieve substates -
% 'one of pair is a member of the other' makes no sense. The
% two goals above are distinct

% necessary_before(T,A,B,P,P,C,C) :-
% 	length(A,Al) ,
% 	length(B,Bl) ,
% 	not(Al=Bl) ,
% 	(members(A,B) 
% 	;
% 	members(B,A)) .
% 
% otherwise check ..
necessary_before(Type,A,B,P_in,P_out,C_in,C_out) :-
	nb_pair(Type,A,B,Return_code) ,
	! ,
	update_orders(Return_code,P_in,P_out) ,
	update_cycles(Return_code,C_in,C_out) .
	
update_orders([A,B],P_in,P_out) :-
	tell(user), nl , write(order(A,B)),
	append(P_in,[[A,B]],P_out) , ! .
update_orders(_,P,P) .
update_cycles([cycle,A,B],C_in,C_out) :-
	tell(user), nl , write(cycle(A,B)),
	append(C_in,[[A,B]],C_out) , ! .
update_cycles(_,C,C) .


% nb_pair
% -------------
% test pair and return code as above
nb_pair(Type,A,B,Return_code) :-
	nb_pair_test(Type,A,B,Ret1) ,
	nb_pair_test(Type,B,A,Ret2) ,
	get_return_code(A,B,Ret1,Ret2,Return_code) .

% nb_pair_test
% ----------------
nb_pair_test(Type,A,B,order) :-
	necessary_before(Type,A,B) .
nb_pair_test(_,_,_,no_order) .

% get_return_code
% -----------------
get_return_code(A,B,order,no_order,[A,B]) .
get_return_code(A,B,no_order,order,[B,A]) .
get_return_code(_,_,no_order,no_order,[]) .
get_return_code(A,B,order,order,[cycle,A,B]) .



% necessary_before
% ---------------------------
necessary_before(1,A,B) :-
% check they are not substates pertaining to the
% same object 
        not(same_obj_substates(A,B)),
        consis(A,B) ,
	precede_achievers(A,A_ops) ,
	! ,
	necessary_before1(A_ops,A,B) .

same_obj_substates(A,B) :-
            A = [X|_],
            B = [Y|_],
            X =.. [_,Obj|_],
            Y =.. [_,Obj|_],!.

% 
% necessary_before(2,A,B) :-
%         consis(A,B) ,
% 	precede_achievers(A,A_ops) ,
% 	! ,
% 	necessary_before2(A_ops,A,B) . 
% 	


% necessary_before1: achievers * pair of instan LISTS --> bool
% ------------------------------------------------------------
% test type (i) order
necessary_before1([],_,_) .
necessary_before1([O|O1],A,B) :-
        % (i) B is not added also by A's achiever 
        % i.e. B not in A.e+
        % -----------------
		% primary_effects(O,Padd) ,

	get_all_effects(O,AE),
% can O possibly achieve B ?????
        not(member(B,AE)),

% So getting this far means A's achiever can't poss ach B ..
% NB .. if both A and B are members of the conditional
% effects of O then strictly we must test to see whether 
% their additional preconditions are consistent -
% if NOT the action wouldn't be able to add both
% at the same time (hmmmm...)

        !,necessary_before_cases(O,A,B),
        ! ,
        necessary_before1(O1,A,B) .


         % (ii) clobbers(O,B) OR inconsistent(A.pre,{B})
% NB  nec_clobbers(O,B) means under any further binding of
% O, say Oa, then Oa still clobbers B - so its the
% case that B is always changed if the precons are satisfied. 
% so in case (a) we need just to consider nec precons. 

% case (a)
necessary_before_cases(O,A,B) :-
        nec_achieves(O,A),
        !,
% cut - so we can remove condition on case (b) ..
        ( nec_clobbers(O,B) ; 
           (preconds(O,Pre),
            flatten(Pre,[],PreF), !, 
            o_inconsistent(PreF,B)) ),!.

% case (b) 'A' is achieved by a cond effect
necessary_before_cases(O,A,B) :-
% ok        not(nec_achieves(O,A)),
            find_cond_effect(A,O, [Pr,A]),
        ( ( nec_clobbers(O,B) ; is_nec_covered_by([Pr],B) ) 
        ;
          ( get_precons(O,Pre),
            flatten(Pre,[],PreF),
            append(Pr,PreF,Ext_precs),!,
            o_inconsistent(Ext_precs,B) )
        ),!.

find_cond_effect(A,operator(_,_,_,CE),[Pr,A]) :-
           member([Pr,A1],CE),
           A == A1,!.

% necessary_before2
% -----------------------------------------
% test type (ii) orders
% this expects that the inverse of predicates has been defined 
% for an example see:
%
%	sort_info_tw
%
necessary_before2([],_,_) .
necessary_before2([O|O1],P,Q) :-
	% (i) achiever for P also achieves Q 
	both_effects(O,Adds) ,
	members(Q,Adds) ,
	! ,	
	necessary_before2(O1,P,Q) .
necessary_before2([O|O1],P,Q) :-
	% (ii) Q must be true in order to achieve P
	% 	-- Q is precon of achiever for P
	% 	-- or ~Q is inconsistent with precons of achiever for P 
	precons(O,Precons) , 
	(members(Q,Precons) 
	;
	get_inverse(Q,Q_inv) , 	% returns inverse (if known) of any ~q in Q
	inconsistent(P,Q_inv)) ,
	! ,	
	necessary_before2(O1,P,Q) .
	



% get_inverse
% -------------------------
% this expects that the inverse of preds has been supplied -- see notes
% at top of this file
get_inverse([],[]) .
get_inverse([P|P_rest],[Ip|I_rest]) :-
        inverse(I) ,
        member(El,I) ,
        member(not(P),El) ,	% NOTE: looking for negation of Pred
        member(Ip,El) ,
        not(Ip=P) ,
	get_inverse(P_rest,I_rest) . 
get_inverse([_|P_rest],I_pred) :-
	get_inverse(P_rest,I_pred) .
 






% consis
% ----------------------------
consis(A,B) :-
	not(o_inconsistent(A,B)) .

% inconsistent:
% --------------
% is there an element (a,b) of IN where X in a and y in b (or vice versa) 
o_inconsistent(X,Y) :-
      append(X,Y,Z),!,
	inconsistent_constraint(I),
%      tell('freda'),nl,write(Z),nl,tell(user),
	o_inconsis(Z,I), 
%      tell('freda'),nl,
      write('IS INCONSISTENT according to '),write(I),nl,nl,
      tell(user),!.

% briefcase tests
test20 :-  o_inconsistent([at_bag(briefcase,home)],
                   [at_bag(briefcase,office)]).
test21 :- o_inconsistent([at_thing(cheque,office),
             inside(cheque,briefcase)],[fits_in(cheque,briefcase),
          at_bag(briefcase,home)]).

% Z is ground with no statics ..		
% NOT NEC .. Z may have vars ..
o_inconsis(Z,I) :-
     atomic_invariants(AI),
% need to ground any vars in Z ..
     return_numvars(Z,ZG),
     listtoand(AI,AIA),
     list_take(I,ZG,Left),
     (Left = [] ; (check_ne_ground(Left),listtoand(Left,LA),hold(LA,AIA)) ),!.

check_ne_ground([ne(x(_),_)|_]) :- !,fail.
check_ne_ground([ne(_,x(_))|_]) :- !,fail.
check_ne_ground([_|T]) :- 
       check_ne_ground(T),!.
check_ne_ground([]).

/* numbervars */
 
return_numvars(Z,ZG) :- 
       copy_term(Z,ZG),
       domain_dep_numvars(ZG,1,_),!.
% ?? domain_dep_numvars(A,B,C) :- domain_dep_numvars1(A,B,C),!.
domain_dep_numvars(x(N),N,N1) :-
        N1 is N+1.
% case with term = predicate
domain_dep_numvars(Term,N1,N2) :-
        nonvar(Term),
        functor(Term,_,N),
        N > 0,
        sort_info(Term,T),
        Term =.. [Name|TermL],
        ground_single_objects(TermL,T),
        domain_dep_numvars(0,N,Term,N1,N2).
% normal case
domain_dep_numvars(Term,N1,N2) :-
        nonvar(Term),
        functor(Term,_,N),
        domain_dep_numvars(0,N,Term,N1,N2).

domain_dep_numvars(N,N,_,N1,N1).
domain_dep_numvars(I,N,Term,N1,N3) :-
        I < N,
        I1 is I+1,
        arg(I1,Term,Arg),
        domain_dep_numvars(Arg,N1,N2),
        domain_dep_numvars(I1,N,Term,N2,N3).
% This replaces any sort var with its object if that sort only contains
% one object
ground_single_objects([HT|TT],[is_of_sort(_,S)|TS]) :-
        var(HT),
        objects(S,[ONE_OBJ]),
        HT = ONE_OBJ,
        ground_single_objects(TT,TS),!.
ground_single_objects([_|TT],[_|TS]) :-
        ground_single_objects(TT,TS),!.
ground_single_objects([],[]).
		

% local utils for inconsis
% ------------------------------------
precede_copy_pred(P_in,P_out) :-
	P_in =..[H|T] ,
	length(T) = length(T1) ,
	P_out =..[H|T1] .

instan_inconsis_el(List,El) :-
	member(El,List) .

get_inconsis_arg_pos(I,Arg_el,Arg_pos) :-
	member(ne(P,Q),I) ,
	Arg_el =..[_|T] ,
	(u_get_arg_pos(T,Arg_pos,P) 
	;
	u_get_arg_pos(T,Arg_pos,Q)) .

get_inconsis_arg(Arg_el,Arg_pos,Arg) :-
	Arg_el =..[_|T] ,
	get_arg_pos(T,Arg_pos,Arg) .


% get_same_arg_pos: [pred1(a1,a2,a3),pred2(a3)] -> [ [3], [1]]
% ----------------------------------------------------------------------
get_same_arg_pos(I,Pos) :-
	length(I,2) ,
	get_same_args(I,[],Args) ,
	get_same_pos(I,Args,1,[],Pos) .

get_same_args([],A,A) . 
get_same_args([I|J],A_in,A_out) :-
	I =.. [_|T] ,
	append(A_in,[T],A_tmp) ,
	get_same_args(J,A_tmp,A_out) .
	
get_same_pos(_,[],_,P,P) .
get_same_pos(I,[A|B],Count,P_in,P_out) :-
	member(A1,A) ,
	member(B1,B) ,
	u_mem(A1,B1) ,	
	u_get_arg_pos(A,A_pos,A1) ,
	u_get_arg_pos(B1,B_pos,A1) ,
	append(P_in,[[A_pos,B_pos]],P_tmp) ,
	C is Count + 1 , 
	get_same_pos(I,B,C,P_tmp,P_out) .	
get_same_pos(I,[_|B],Count,P_in,P_out) :-
	C is Count + 1 , 
	get_same_pos(I,B,C,P_in,P_out) .	

check_same_args(_,_,[]) .
check_same_args(X_el,Y_el,[[A,B]|C]) :-
	X_el =..[_|Tx] ,
	Y_el =..[_|Ty] ,
	get_arg_pos(Tx,A,Arg) ,
	get_arg_pos(Yx,B,Arg) ,
	check_same_args(X_el,Y_el,C) .
	




% precede_achievers 
% --------------------------------------------------------------------------
% get instantiated achievers of sstate
precede_achievers(SState,Ach) :-
	setof(O,O^all_poss_achievers(SState,O),A) ,
	ach(A,SState,Ach) .
	
ach(A,SState,Ach) :-
	ach(A,SState,[],Ach) .
ach([],_,A,A) .
ach([A|B],SState,A_in,A_out) :-
	setof(A,instan_ach(A,SState),Instan) ,
	append(A_in,Instan,A_tmp) ,
	ach(B,SState,A_tmp,A_out) .

% instan_ach/2
% ---------------
instan_ach(O,SS) :-	
        atomic_invariants(Always),
	get_all_effects(O,AE) ,
	member(SS,AE) ,
	get_all_statics(O,C) ,
	listtoand(C,CA) ,
	listtoand(Always,AlwaysA) ,
        hold(CA,AlwaysA).        /* check this instantiation is poss*/


to_vars([],[]) .
to_vars([A|B],[C|D]) :-
	pred_args_atoms_to_vars(A,C) ,
	to_vars(B,D) .

% pred_args_atoms_to_vars/4
% -------------------------
% input: [p(a1,a2,.),[p2( ..),p3(..)],..] -> [pred1(Var1,..) * pred2(...)
pred_args_atoms_to_vars([A,B],[A_out,B_out]) :-
	A =..[A_hd|A_tl] ,
	rhs_args(B,[],B_args) ,
	append(A_tl,B_args,Args) ,
	setof(Arg,Arg^member(Arg,Args),A_set) ,
	length(A_set,Len) ,
	length(New_args,Len) ,
	atoms_to_vars(A_tl,A_tl1,A_set,New_args) ,
	A_out =..[A_hd|A_tl1] ,
	reassemble_preds(B,A_set,New_args,B_out) .
	
atoms_to_vars([],[],_,_) .
atoms_to_vars([A|B],[C|D],Args1,Args2) :-		
	member_pos(A,Args1,Pos) ,
	member_pos(C,Args2,Pos) ,
	atoms_to_vars(B,D,Args1,Args2) .

rhs_args([],A,A) .
rhs_args([B|C],B_in,B_out) :-
	B =..[B_hd|B_tl] ,
	append(B_in,B_tl,B_tmp) ,
	rhs_args(C,B_tmp,B_out) .
	
reassemble_preds([],_,_,[]) .
reassemble_preds([A|B],A_set,New_args,[C|D]) :-
	A =..[A_hd|A_tl] ,
	atoms_to_vars(A_tl,A_tl1,A_set,New_args) ,
	C =..[A_hd|A_tl1] ,
	reassemble_preds(B,A_set,New_args,D) .





% generalise_precede: [pred(Const,Const),..] --> [pred(Var,Var),...]
% ----------------------------------------------------------------------------
% NOTE: assumes that A and B are lists of instantiated preds
generalise_precede([],[]) .
generalise_precede([[A,B]|C],[[A2,B2]|D]) :-
	listtoand(A,Aand) ,
        generalise(Aand,nil,A1) ,     % in: fmacgr
	andtolist(A1,A2) ,
        listtoand(B,Band) ,
        generalise(Band,nil,B1) ,     % in: fmacgr
        andtolist(B1,B2) ,
        generalise_precede(C,D) .
 
 
 

% write_precede
% ---------------------------------------------------
write_precede(orders,[[H,T]|Rest]) :-
        write('precede_before('),write(H),write(', '),write(T),write(').'),nl,
        write_precede(orders,Rest) .
write_precede(cycles,[[H,T]|Rest]) :-
        write('cycle('),write(H),write(', '),write(T),write(').'),nl,
        write_precede(cycles,Rest) .
write_precede(_,[]).


