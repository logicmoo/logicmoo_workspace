/*
 *	file:		inout.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for reading and writing polynomial
 *	interpretations.
 *
 *	history:
 *	891128	uh	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% pol_get_polynomial(Operator,Varlist,Polynomial,Component) reads in a legal
% interpreting polynomial for Operator/Arity


pol_get_polynomial(Operator,[x,y],Polynomial,Component) :-
	pol_op_is_AC(Operator),
	!,
	write('Remember : only following polynomials are legal :'),
	nl,
	pol_prompt(Operator,[x,y]),
	write('A * x * y + B * (x + y) + C   with  A * C + B - B^2 = 0'),
	nl,
	repeat,
	pol_prompt(Operator,[x,y]),
	read(Term),
	pol_trans(Term,[x,y],Polynomial,Component,Operator,2),
	pol_check(Polynomial,Operator,2),
	pol_check_ac(Polynomial,Operator),
	!.
pol_get_polynomial(Operator,[x,y],Polynomial,Component) :-
	pol_op_is_C(Operator),
	!,
	pol_termlist([x,y],Termlist),
	write('Remember : only symmetric polynomials are legal !'),
	nl,
	repeat,
	pol_prompt(Operator,[x,y]),
	read(Term),
	pol_trans(Term,Termlist,Polynomial,Component,Operator,2),
	pol_check(Polynomial,Operator,2),
	pol_check_symmetric(Polynomial,Operator),
	!.
pol_get_polynomial(Operator,Varlist,Polynomial,Component) :-
	pol_termlist(Varlist,Termlist),
	length(Varlist,Arity),
	repeat,
	pol_prompt(Operator,Varlist),
	read(Term),
	pol_trans(Term,Termlist,Polynomial,Component,Operator,Arity),
	length(Varlist,Arity),
	pol_check(Polynomial,Operator,Arity),
	!.


pol_trans(Term,Termlist,Interpretationpolynomial,Component,_,_) :-
	pol_transform(Term,Termlist,Interpretation),
	pol_select_component(Interpretation,Component,Interpretationpolynomial),
	!.
pol_trans(Term,_,_,_,Op,Arity) :-
	sPrint("*** Cannot interpret expression (for '%' with arity %) :", [Op,Arity]),
	nl,
	write(Term),
	nl,
	fail.


pol_check(Interpretation,_,_) :-
	pol_legal_interpretation(Interpretation),
	!.
pol_check(Interpretation,Operator,Arity) :-
	sPrint("*** Interpretation is illegal for '%' with arity % :", [Operator, Arity]),
	nl,
	pol_write_polynomial(Interpretation),
	nl,
	fail.


% pol_prompt(Operator,Varlist) writes a prompt
% e.g. pol_prompt(concat,[x,y,z]) --> [ concat ] (@x,@y,@z)

pol_prompt(Operator,Varlist) :-
	write('[ '),
	write(Operator),
	write(' ] ('),
	pol_write_varlist(Varlist),
	write(') = ').


pol_write_varlist([]).
pol_write_varlist([Var1|Varlist]) :-
	write(Var1),
	pol_write_varlistrest(Varlist).

pol_write_varlistrest([]).
pol_write_varlistrest([Var1|Varlist]) :-
	write(','),
	write(Var1),
	pol_write_varlistrest(Varlist).


pol_check_ac(Polynomial,_Op) :-
	pol_has_AC_property([Polynomial]),
	!.
pol_check_ac(Polynomial, Op) :-
	sPrint("*** Polynomial has not AC-property ! (AC-Operator '%') :", [Op]),
	nl,
	pol_write_polynomial(Polynomial),
	nl,
	fail.

/*
pol_check_ac_const((A,B,C)) :- !,
	pol_integer(A),
	pol_integer(B),
	pol_integer(C),
	Checksum is A * C + B - B * B,
	pol_checksum(Checksum).
pol_check_ac_const(_) :-
	write('You have to type in something like ''1,2,2 .'' '),
	nl,
	fail.

pol_integer(I) :-
	integer(I),
	!.
pol_integer(X) :-
	write(X),
	write(' is not an integer !'),
	nl,
	fail.


pol_checksum(0) :- !.
pol_checksum(_) :-
	write('A * C + B - B^2 not equal to zero'),
	nl,
	fail.
*/

pol_make_ac_polynomial((0,0,_),[]) :- !,
%	write('*** Sorry, A and B both zero is illegal !'),
%	nl,
	fail.
pol_make_ac_polynomial((A,0,0),
	 [(q(A,1),[1,1])]) :- !.
pol_make_ac_polynomial((0,B,0),
	 [(q(B,1),[1,0]),(q(B,1),[0,1])]) :- !.
pol_make_ac_polynomial((0,B,C),
	 [(q(B,1),[1,0]),(q(B,1),[0,1]),(q(C,1),[0,0])]) :- !.
pol_make_ac_polynomial((A,0,C),
	 [(q(A,1),[1,1]),(q(C,1),[0,0])]) :- !.
pol_make_ac_polynomial((A,B,0),
	 [(q(A,1),[1,1]),(q(B,1),[1,0]),(q(B,1),[0,1])]) :- !.
pol_make_ac_polynomial((A,B,C),
	 [(q(A,1),[1,1]),(q(B,1),[1,0]),(q(B,1),[0,1]),(q(C,1),[0,0])]).


% pol_quicksort(Polynomial,SortedPolynomial) sorts the Polynomial.
% It uses pol_mgreater for ordering.

pol_quicksort([],[]).
pol_quicksort([A|R],B) :-
	pol_split(A,R,G,L),
	pol_quicksort(G,R1),
	pol_quicksort(L,R2),
	append(R1,[A|R2],B).


pol_split(_A,[],[],[]).
pol_split(A,[E|R],[E|G],L) :-
	pol_mgreater(E,A),
	!,
	pol_split(A,R,G,L).
pol_split(A,[E|R],G,[E|L]) :-
	pol_split(A,R,G,L).


% pol_invert(Polynomial,InvertedPolynomial) exchanges the variables of
% the Polynomial, if it has two, otherwise it fails.

pol_invert([],[]).
pol_invert([(Coeff,[X,Y])|Restpolynomial],[(Coeff,[Y,X])|InvertedRest]) :-
	pol_invert(Restpolynomial,InvertedRest).


% pol_check_symmetric(Polynomial,Operator) succeeds, if Polynomial has two
% variables and is symmetric.

pol_check_symmetric(Polynomial,_Operator) :-
	pol_invert(Polynomial,Inverted),
	pol_quicksort(Inverted,Polynomial),
	!.
pol_check_symmetric(Polynomial,Operator) :-
	sPrint("*** Polynomial is not symmetric ! (C-Operator '%') :", [Operator]),
	nl,
	pol_write_polynomial(Polynomial),
	nl,
	fail.


% pol_enter_prompt(Operator,Arity) :
%	writes a prompt to the user that he has to type in an interpretation

pol_enter_prompt(Operator,Arity) :-
	nl,
	sPrint("There is no interpretation of operator '%' with arity %", [Operator, Arity]),
	nl,
	pol_enter_prompt_ac(Operator, Arity).
pol_enter_prompt_ac(Operator, 2) :-
	pol_op_is_AC(Operator),
	!,
	write('The operator is associative and commutative !'),
	nl,
	write('Legal interpreting polynomials look like :'),
	nl,
	write('A * x * y + B * (x + y) + C   with  A * C + B - B^2 = 0'),
	nl.
pol_enter_prompt_ac(Operator, 2) :-
	pol_op_is_C(Operator),
	!,
	write('The operator is commutative !'),
	nl,
	write('Legal interpreting polynomials are symmetric !'),
	nl,
	write('( p(x,y) = p(y,x) )'),
	nl.
pol_enter_prompt_ac(_Operator, _).


% pol_enter_interpretation(Operator,Arity) is called if an operator
% is encountered without interpretation. It offers the user a default
% interpretation. Then he may change the interpretation.

pol_enter_interpretation(Operator,Arity) :-
	pol_print_opt_comment,
	pol_default_interpretation(Operator,Arity,Default),
	pol_enter_prompt(Operator,Arity),
	write('The default interpretation is :'),
	nl,
	pol_write_interpretation(Default),
	pol_ask_user(Operator,Arity,Default).


pol_print_opt_comment :-
	pol_comment((Comment,Args), L, R),
	!,
	sPrint(Comment,Args),
	nl,
	write('Consider the terms'),
	nl,
	write('	'),
	print('$term'(L)),
	nl,
	write('and'),
	nl,
	write('	'),
	print('$term'(R)),
	nl.
pol_print_opt_comment.


pol_ask_user(Operator,Arity,Default) :-
	write('Do you want to change it ? (if so type ''y'') '),
	pol_get_y_n(Char),
	Char = 121,          % 121 = 'y'
	!,
	pol_change_interpretation(Operator,Arity,Default,_).
pol_ask_user(Operator,Arity,Default) :-
	pol_assert(pol_op_interpretation(Operator,Arity,Default)).


pol_get_y_n(Char) :-
	repeat,
	get0(Char),
	pol_skipinput(Char),
	(	Char = 10            %  10 = <CR>
	;	Char = 110           %  110 = 'n'
	;	Char = 121           %  121 = 'y'
	;	write('Please type ''y'' for yes, ''n'' for no or <CR> for no : '),
		fail
	),
	!.

pol_get_y_n_1(Char) :-
	repeat,
	get0(Char),
	pol_skipinput(Char),
	(	Char = 10            %  10 = <CR>
	;	Char = 110           %  110 = 'n'
	;	Char = 121           %  121 = 'y'
	;	write('Please type ''y'' or <CR> for yes, ''n'' for no : '),
		fail
	),
	!.


pol_assert(pol_op_interpretation(Op,Arity,I)) :-
	pol_get_current(pol_state(N,IList,C,AC)),
	member(pol_op_interpretation(Op,Arity,_),IList),
	!,
	pol_subst(pol_op_interpretation(Op,Arity,_),
		  pol_op_interpretation(Op,Arity,I),
		  IList,NewList),
	pol_make_current(pol_state(N,NewList,C,AC)).
pol_assert(pol_op_interpretation(Op,Arity,I)) :-
	pol_get_current(pol_state(N,IList,C,AC)),
	pol_make_current(pol_state(N,
		 [pol_op_interpretation(Op,Arity,I)|IList],C,AC)).


% pol_change_interpretation(Operator,Arity,Old,New) :
%	interacts with the user to change the old interpretation
%	for Operator/Arity in a new one, new interpretation is stored

pol_change_interpretation(Operator,Arity,[Old],[New]) :-
	!,
	pol_varlist(Arity,Varlist),
	pol_change_question(121,Operator,Varlist,Old,New,1),
	pol_assert(pol_op_interpretation(Operator,Arity,[New])).
pol_change_interpretation(Operator,Arity,Old,New) :-
	write('Do you want to change it component for component ? (if so type ''y'') '),
	pol_get_y_n(Char),
	(Char = 121 ->
		pol_change_interpretation_componentwise(Operator,Arity,Old,New),
		pol_assert(pol_op_interpretation(Operator,Arity,New)) 
	;
		pol_change_whole_interpretation(Operator, Arity, New)
	).

pol_change_interpretation_componentwise(Operator,Arity,Old,New) :-
	pol_varlist(Arity,Varlist),
	pol_change_interpretationlist(1,Operator,Varlist,Old,New).


pol_change_whole_interpretation(Operator,Arity,Interpretation) :-
	!,
	pol_varlist(Arity,Varlist),
	write('Type in the new interpretation tuple'),
	nl,
	((Arity = 2, pol_op_is_AC(Operator)) ->
		write('Remember : operator has AC property'), 
		nl,
		write('only following polynomials (or tuples of them) are legal :'),
		nl,
		write('A * x * y + B * (x + y) + C   with  A * C + B - B^2 = 0'),
		nl
	;
		((Arity = 2, pol_op_is_C(Operator)) ->
			write('Remember : operator has C property'),
			nl,
			write('only symmetric polynomials (or tuples of them) are legal :') ,
			nl
		;
			true
		)
	),
	repeat,
	pol_prompt(Operator,Varlist),
	read(Term),
	pol_add_interpretation_online(Operator, Varlist, Term, Interpretation),
	!.

% pol_add_interpretation_online(Operator, VarList, InterprList) same
% as pol_add_interpretation/3 in addinterp, but the asserted interpretation
% is printed out
pol_add_interpretation_online(Operator, VarList, InterprList, I) :-
	length(VarList, Arity),
	!,
	pol_add_interpr1(Operator, VarList, Arity, InterprList,online,I).
pol_add_interpretation_online(Operator, VarList, _InterprList, _) :-
	sPrint("*** pol_add_interpretation_online : Illegal variable list for '%' : %", [Operator, VarList]),
	nl,
	fail.


pol_change_interpretationlist(_,_,_,[],[]) :- !.
pol_change_interpretationlist(N,Operator,Varlist,[Old1|Oldrest],[New1|Newrest]) :-
	N1 is N+1,
	nl,
	length(Varlist, Arity),
	sPrint("Changing component % for '%' with arity % ", [N,Operator,Arity]),
	pol_write_ac_property(Operator, Arity),
	nl,
	pol_change_polynomial(Operator,Varlist,Old1,New1,N),
	pol_change_interpretationlist(N1,Operator,Varlist,Oldrest,Newrest).


pol_change_polynomial(Operator,Varlist,Old,New,Component) :-
	write('The old interpreting polynomial is'),
	nl,
	pol_prompt(Operator,Varlist),
	pol_write_polynomial(Old),
	nl,
	write('Do you want to change it ? (if so, type ''y'') '),
	pol_get_y_n(Char),
	pol_change_question(Char,Operator,Varlist,Old,New,Component).


pol_skipinput(10) :- !.          % 10 = <CR>
pol_skipinput(_) :- skip(10).


pol_change_question(121,Operator,Varlist,_,New,Component) :- !,   % 121 = 'y'
	repeat,
	write('Type in the new interpreting polynomial'),
	length(Varlist, N),
	nl,
	pol_get_polynomial(Operator,Varlist,New,Component),
	sPrint("Resulting polynomial for '%' with arity %", [Operator, N]),
	((pol_get_tuplelength(I), I > 1) ->
		write(' (component '),
		write(Component),
		write(')') 
	;
		true
	),
	write(' :'),
	nl,
	pol_write_polynomial(New),
	nl,
	pol_confirm,
	!.
pol_change_question(_,_,_,New,New,_).

pol_confirm :-
	write('Do you accept it ? (if not, type ''n'') '),
	pol_get_y_n_1(Char),
	(Char = 110 -> 
		write('New interpretation cancelled ! Try again !'),
		nl,
		fail 
	;
		true
	).

%  Pol_write_polynomial writes a polynomial without unnecessary 0's
%  and 1's.
%  e.g. not '1 * (@x^1)' , but '@x'

pol_write_polynomial([]) :- !,write(0).
pol_write_polynomial([(q(1,1),M)|R]) :-
	pol_zerolist(M),
	!,
	write(1),
	pol_write_p(R).
pol_write_polynomial([(q(-1,1),M)|R]) :-
	pol_zerolist(M),
	!,
	write(-1),
	pol_write_p(R).
pol_write_polynomial([(q(0,_),_M)|R]) :-
	!,
	pol_write_polynomial(R).
pol_write_polynomial([(q(Z,N),M)|R]) :-
	Z1 is Z,
	Z1 > 0,
	!,
	pol_write_rat(q(Z1,N),Done),
	pol_write_m(M,Done),
	pol_write_p(R).
pol_write_polynomial([(q(Z,N),M)|R]) :-
	write('-'),
	Z1 is Z,
	ZM is -Z1,
	pol_write_rat(q(ZM,N),Done),
	pol_write_m(M,Done),
	pol_write_p(R).


% pol_write_interpretation(Interpretation) displays the interpretation.
% If it is a tuple, each component is displayed on a separate line.

pol_write_interpretation([Polynomial]) :- 
	!,
	pol_write_polynomial(Polynomial),
	nl.
pol_write_interpretation([Pol1|Rest]) :-
	write('   [ '),
	pol_write_polynomial(Pol1),
	pol_write_pol_list(Rest).


pol_write_pol_list([]) :-
	write(' ]'),
	nl.
pol_write_pol_list([Pol1|Rest]) :-
	write(' ,'),
	nl,
	tab(5),
	pol_write_polynomial(Pol1),
	pol_write_pol_list(Rest).


%  pol_write_rat(Rational,Done) writes a rational number.
%  'Done' indicates whether it has produced any output

pol_write_rat(q(Z,1),no) :- 
	Z1 is Z,
	Z1=1,
	!.
pol_write_rat(q(Z,1),no) :-
	Z1 is Z,
	(-1) = Z1,
	!.
pol_write_rat(q(Z,1),yes) :-
	!,
	Z1 is Z,
	write(Z1).
pol_write_rat(q(Z,N),yes) :-
	Z1 is Z,
	write('('),
	write(Z1),
	write('/'),
	write(N),
	write(')').


%  pol_write_m(Monomial,Sign) writes a monomial with a leading ' * '
%  e.g. pol_write_m([2,0,1],yes) -->  * (@x^2) * @z
%       pol_write_m([1,2,0,1],no) -->  @x1 * (@x2^2) * @x4

pol_write_m([],_) :- !.
pol_write_m([N],Sign) :-
	!,
	pol_write_power(x,'',N,Sign,_).
pol_write_m([N,M],Sign) :-
	!,
	pol_write_power(x,'',N,Sign,Sign1),
	pol_write_power(y,'',M,Sign1,_).
pol_write_m([N,M,K],Sign) :-
	!,
	pol_write_power(x,'',N,Sign,Sign1),
	pol_write_power(y,'',M,Sign1,Sign2),
	pol_write_power(z,'',K,Sign2,_).
pol_write_m([A|R],Sign) :-
	pol_write_power(x,1,A,Sign,Sign1),
	pol_write_rest(R,2,Sign1).


pol_write_rest([],_,_) :- !.
pol_write_rest([A|R],Ext,Sign) :-
	Extnew is Ext+1,
	pol_write_power(x,Ext,A,Sign,Signout),
	pol_write_rest(R,Extnew,Signout).


%  pol_write_power(Variable,Extension,Power,Sign1,Sign2)
%  if Sign1 = yes then a leading ' * ' is needed
%  Sign2 indicates whether a ' * ' is needed for the next output
%  e.g. pol_write_power(x,2,3,no,Sign) --> (@x2^3)    Sign = yes
%       pol_write_power(y,'',3,yes,Sign) -->  * (@y^3)    Sign = yes

pol_write_power(_,_,0,Sign,Sign) :- !.
pol_write_power(X,Ext,1,no,yes) :-
	!,
	write(X),
	write(Ext).
pol_write_power(X,Ext,1,yes,yes) :-
	!,
	write(' * '),
	write(X),
	write(Ext).
pol_write_power(X,Ext,N,no,yes) :-
	write('('),
	write(X),
	write(Ext),
	write('^'),
	write(N),
	write(')').
pol_write_power(X,Ext,N,yes,yes) :-
	write(' * '),
	write('('),
	write(X),
	write(Ext),
	write('^'),
	write(N),
	write(')').


% pol_write_p writes a polynomial with a leading ' - ' or ' + '

pol_write_p([]) :- !.
pol_write_p([(q(1,1),M)|R]) :-
	pol_zerolist(M),
	!,
	write(' + '),
	write(1),
	pol_write_p(R).
pol_write_p([(q(-1,1),M)|R]) :-
	pol_zerolist(M),
	!,
	write(' - '),
	write(1),
	pol_write_p(R).
pol_write_p([(q(0,_),_M)|R]) :-
	!,
	pol_write_p(R).
pol_write_p([(q(Z,N),M)|R]) :-
	Z1 is Z,
	Z1 > 0,
	!,
	write(' + '),
	pol_write_rat(q(Z1,N),Done),
	pol_write_m(M,Done),
	pol_write_p(R).
pol_write_p([(q(Z,N),M)|R]) :-
	write(' - '),
	Z1 is -Z,
	pol_write_rat(q(Z1,N),Done),
	pol_write_m(M,Done),
	pol_write_p(R).
