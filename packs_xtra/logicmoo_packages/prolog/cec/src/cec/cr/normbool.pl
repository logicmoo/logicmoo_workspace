%------------------------------------------------------------------------------%
% Simplifies Contexts with the system Bool
%------------------------------------------------------------------------------%
norm_bool( C1,C2 ):-
		bool_red1( C1,C3 ),
		bool_red2( C3,C2 ),!.

bool_red1( [], [] ):-!.
bool_red1( [- -V1|L1] , L2):-	bool_red1([V1|L1],L2),!.
bool_red1( [ X |_] , ['true-bool'] ):- 	X == 'true-bool', !.
bool_red1( [ X |_] , ['true-bool'] ):-	X == -'false-bool', !.
bool_red1( [ X | R] , L ):-  	X == 'false-bool',
					bool_red1( R , L ),!.
bool_red1( [ X | R] , L ):-  	X == -'true-bool',
					bool_red1( R , L ),!.
bool_red1( [X | R] , L1 ):-		bool_red1( R , L2 ),
					( L2 == ['true-bool'], L1 = L2
					; L1 = [X|L2] ),!.

bool_red2( L , ['true-bool'] ):- L == ['true-bool'], !.
bool_red2( [] , [] ):- !.
bool_red2( [X1=X2|_] , ['true-bool'] ):- 	X1 == X2, !.
bool_red2( [-(X1=X2)|L1] , L2 ):- 	X1 == X2, 
					bool_red2(L1,L2),!.
bool_red2( [-V1|L1] , ['true-bool'] ):-	vmember( V1,L1 ),!.
bool_red2( [V1|L1] , ['true-bool'] ):-	\+ ( V1 = -(_)),
					vmember( -V1,L1 ),!.
bool_red2( [V1|L1] , L2  ):-		vmember( V1, L1),
					bool_red2(L1,L2),!.
bool_red2( [V1|L1] , L2):-		bool_red2(L1,L3),
					( L3 == ['true-bool'], L2 = L3
					; L2 = [V1|L3] ),!.

%------------------------------------------------------------------------------%
% complem_cond(L1,L2): L2 is the complem. condition in cnf list form
%                      of the elems. of L1.
%------------------------------------------------------------------------------%

complem_cond([L1|R],DNF1):-
		one_in_each_list(L1,DNF2),
		dnf_to_cnf(R,DNF2,DNF3),
		simple_cnf_list(DNF3,DNF1),!.

dnf_to_cnf([],DNF,DNF):-!.
dnf_to_cnf([L|R],DNF2,DNF1):-
		one_of_each(L,DNF2,DNF3),
		dnf_to_cnf(R,DNF3,DNF1),!.

one_of_each([],_,[]):-!.
one_of_each([-L|R],DNF1,DNF2):-!,
		put_ahead(L,DNF1,DNF3),
		one_of_each(R,DNF1,DNF4),
		vappend(DNF3,DNF4,DNF2),!.
one_of_each([L|R],DNF1,DNF2):-!,
		put_ahead(-L,DNF1,DNF3),
		one_of_each(R,DNF1,DNF4),
		vappend(DNF3,DNF4,DNF2),!.

put_ahead(_,[],[]):-!.
put_ahead(L,[A|R1],[[L|A]|R2]):-
		put_ahead(L,R1,R2),!.

one_in_each_list([],[]):-!.
one_in_each_list([-A|R1],[[A]|R2]):-!,
		one_in_each_list(R1,R2),!.
one_in_each_list([A|R1],[[-A]|R2]):-
		one_in_each_list(R1,R2),!.

%------------------------------------------------------------------------------%
% Simplifies cnf lists: [] means false, [[false]] means false
%------------------------------------------------------------------------------%

simple_cnf_list( L1,L2 ):-
		simplify_ors( L1,L3 ),
		simplify_cnf( L3,L2 ),!.

simplify_ors([],[]):-!.
simplify_ors( [Or1|Rest1], L ):-
		norm_bool(Or1,Or2),
		simplify_ors( Rest1, Rest2 ),
		( (Or2 = ['true-bool'], L = Rest2) ; L= [Or2|Rest2]),!.

%  X /\ false  			==  false 	      (true is covered above)
%  (X \/ L) /\  (-X \/ L) 	==  L		      (covers X /\ -X == false)
%  L1 /\  L2			==  L2		      where L1 includes L2
%  (X \/ L1)  /\  (-X \/ L2)	==  L1 /\  (-X \/ L2) where L1 includes L2
%  (-X \/ L1) /\  (X \/ L2)	==  L1 /\  (X \/ L2)  where L1 includes L2

simplify_cnf( [],  [] ):-!.
simplify_cnf( [Or1|ROrs],Lcnf):-
		vdelete( V, Or1, NOr1),
		vdelete( Or2,ROrs,ROrs1 ),
		vdelete( -V, Or2, NOr2),
		equal_set(NOr1,NOr2),!,
		simplify_cnf( [NOr1|ROrs1],Lcnf),!.
simplify_cnf( [Or1|ROrs],Lcnf):-
		vdelete( V1, Or1, NOr1),
		V1 = - V,
		vdelete( Or2,ROrs,ROrs1 ),
		vdelete( V, Or2, NOr2),
		equal_set(NOr1,NOr2),!,
		simplify_cnf( [NOr1|ROrs1],Lcnf),!.
simplify_cnf( [Or1|ROrs], Lcnf ):-
		vdelete( Or2, ROrs,ROrs1),
		((includes( Or1, Or2 ),!,
		  simplify_cnf( ROrs , Lcnf ))
		;
		(includes( Or2, Or1 ),!,
		 simplify_cnf( [Or1|ROrs1] , Lcnf ))),!.
simplify_cnf( [Or1|ROrs], Lcnf ):-
		vdelete( V, Or1, NOr1),
		vdelete(Or2,ROrs,ROrs1),
		vdelete( -V, Or2, NOr2),
		((includes(NOr1,NOr2),!,
		  simplify_cnf( [NOr1|ROrs], Lcnf ))
		;
		(includes(NOr2,NOr1),!,
		 simplify_cnf( [Or1,NOr2|ROrs1], Lcnf ))),!.
simplify_cnf( [Or1|ROrs], Lcnf ):-
		vdelete( V1, Or1, NOr1),
		V1 = - V,
		vdelete(Or2,ROrs,ROrs1),
		vdelete( V, Or2, NOr2),
		((includes(NOr1,NOr2),!,
		simplify_cnf( [NOr1|ROrs], Lcnf ))
		;
		(includes(NOr2,NOr1),!,
		simplify_cnf( [Or1,NOr2|ROrs1], Lcnf ))),!.
simplify_cnf( [Or1|ROrs], [Or1|ROrs1] ):-
		simplify_cnf( ROrs, ROrs1 ),!.

includes(_,[]):-!.
includes(L1,[X|R]):-
		vdelete(X,L1,L2),!,
		includes(L2,R),!.


