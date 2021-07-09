/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

/* Print term as a tree */

% uses system version if available
print_tree80(T) :- current_predicate(pretty_clauses:print_tree/1),pretty_clauses:print_tree(T),!.
print_tree80(T) :-
   ignore((numbervars80(T,111,_),
   pt0('','',T,0),nl, fail)).

may_tab(A):- line_position(current_output,0), tab(A),!.
may_tab(_):- write(' ').

inperent(In,TTs,T,Ts):- 
      TTs=..[In,T,Ts], 
      functor(TTsS,In,2),     
     ((nonvar(T), T=TTsS);(nonvar(Ts), Ts=TTsS)).

pt0(_,LC,A,I) :-
   as_is(A), !,
   may_tab(I), write_simple(A),write(LC), nl.

/*
pt0(In,LC,TTs,I) :- 
   inperent(In,TTs,T,Ts),
   I0 is I-3,
   pt0(In,LC,T,I0),   
   pt0(In,LC,Ts,I).
*/

pt0(In,LC,[T|Ts],I) :- !,
  may_tab(I),write('['),
  I2 is I+2,
  I1 is I+1,
   pt0(In,',',T,I1),
   format(atom(NLC),'  ]~w',[LC]),
   pt1(In,NLC,Ts,I2),!.

pt0(In,LC,q(E,V,G),I):- atom(E), !, T=..[E,V,G],!, pt0(In,LC,T,I).

pt0(_In,LC,T,I) :- T=..[F,A], !,
   may_tab(I), format('~p(',[F]),
   I0 is I+1, format(atom(LC2),')~w',[LC]),   
   pt1(F,LC2,[A],I0).

pt0(_In, LC,T,I) :-    
   T=..[F,A0,A|As], as_is(A0), append([L1|Left],[R|Rest],[A|As]), \+ is_arity_lt1(R), !,
   may_tab(I), format('~p( ',[F]),
   write_simple(A0), write_simple_each([L1|Left]), format(', '), nl,
   I0 is I+3, format(atom(LC2),')~w',[LC]),   
   pt1(F,LC2,[R|Rest],I0).


pt0(_In,LC,T,I) :- T=..[F,A,B|As], is_arity_lt1(A), !, 
   may_tab(I), format('~p( ~p,',[F,A]), nl,
   I0 is I+2, format(atom(LC2),')~w',[LC]),
   pt1(F,LC2,[B|As],I0).

pt0(In,LC,T,I) :- !,
   T=..[F|As],   
   (((In==F, F == & )
     -> (I0 is I+1,LCO='~w' )
      ; (may_tab(I), format('~p(',[F]), I0 is I+3, nl, LCO=')~w'))),
   format(atom(LC2),LCO,[LC]),
   pt1(F,LC2,As,I0).

pt1(_In,_LC,[],_) :- !.
pt1( In, LC,[A],I) :- !,
   pt0(In,LC,A,I).
pt1( In, LC,[A0,A|As],I) :- is_arity_lt1(A0), append([L1|Left],[R|Rest],[A|As]), \+ is_arity_lt1(R), !,
   may_tab(I), write_simple(A0), write_simple_each([L1|Left]), nl,
   pt0(In,',',R,I),
   pt1(In,LC,Rest,I).  
pt1( In, LC,[A|As],I) :- !,
   pt0(In,',',A,I),
   pt1(In,LC,As,I).



is_arity_lt1(A) :- \+ compound(A),!.
is_arity_lt1(A) :- compound_name_arity(A,_,0),!.
is_arity_lt1(A) :- functor(A,'$VAR',_),!.

as_is(V):- var(V).
as_is(A) :- is_arity_lt1(A), !.
as_is(A) :- is_list(A),length(A,L),L<2,!.
as_is(A) :- functor(A,F,_), simple_f(F).
as_is('_'(_)) :- !.
as_is(X) :-
   quote80(X).
as_is(A) :- A=..[_|S], maplist(is_arity_lt1,S), !.
% as_is(F):- simple_arg(F), !.

simple_f(denotableBy).
simple_f(iza).
simple_f(c).
simple_f(p).
simple_f(isa).
simple_f(HasSpace):- atom_contains(HasSpace,' ').

simple_arg(S):- (nvar(S) ; \+ compound(S)),!.
simple_arg(S):- S=[_,A], simple_arg(A), !.
simple_arg(S):- \+ (arg(_,S,Var), compound(Var), \+ nvar(Var)).

nvar(S):- \+ is_arity_lt1(S)-> functor(S,'$VAR',_); var(S).



% write_simple(A):- is_arity_lt2(A),!, format('~p',[A]).
write_simple(A):- is_arity_lt1(A),!, format('~q',[A]).
write_simple(A):- format('~p',[A]).

write_simple_each([]).
write_simple_each([A0|Left]):-  format(', '), write_simple(A0), write_simple_each(Left).


