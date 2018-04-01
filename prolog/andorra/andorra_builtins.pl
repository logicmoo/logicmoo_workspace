:- module(andorra_builtins,
          ['>_andorra'/5,
           '<_andorra'/5,
           '=<_andorra'/5,
           '>=_andorra'/5,
           '=:=_andorra'/5,
           '=\\=_andorra'/5,
           is_andorra/5,
           arithexpression_andorra/4,
           name_andorra/5,
           atom_codes_andorra/5,
           number_codes_andorra/5,
           atom_length_andorra/5,
           atom_concat_andorra/6,
           int_andorra/4,
           integer_andorra/4,
           nnegint_andorra/4,
           flt_andorra/4,
           float_andorra/4,
           num_andorra/4,
           number_andorra/4,
           atm_andorra/4,
           atom_andorra/4,
           struct_andorra/4,
           gnd_andorra/4,
           constant_andorra/4,
           atomic_andorra/4,
           callable_andorra/4,
           arg_andorra/6,
           functor_andorra/6,
           '=.._andorra'/5
          ]).

:- use_module(library(andorra/andorra_rt), [suspend_andorra/5]).

'>_andorra'(X,Y,L,L1,_):- 
 	( ground(X/Y) -> L=L1, X > Y ;
 	    L=[S|L1],
	    term_variables([X,Y],LS),
 	    suspend_andorra(S,LS,X > Y,ground(X/Y),builtin)
 	    ).

'<_andorra'(X,Y,L,L1,_):- 
	( ground(X/Y) -> L=L1, X < Y ;
	    L=[S|L1],
	    term_variables([X,Y],LS),
	    suspend_andorra(S,LS,X < Y,ground(X/Y),builtin)
	    ).

'=<_andorra'(X,Y,L,L1,_):- 
	( ground(X/Y) -> L=L1, X =< Y ;
	    L=[S|L1],
	    term_variables([X,Y],LS),
	    suspend_andorra(S,LS,X =< Y,ground(X/Y),builtin)
	    ).

'>=_andorra'(X,Y,L,L1,_):- 
	( ground(X/Y) -> L=L1, X >= Y ;
	    L=[S|L1],
	    term_variables([X,Y],LS),
	    suspend_andorra(S,LS,X >= Y,ground(X/Y),builtin)
	    ).

'=:=_andorra'(X,Y,L,L1,_):- 
	( ground(X/Y) -> L=L1, X =:= Y ;
	    L=[S|L1],
	    term_variables([X,Y],LS),
	    suspend_andorra(S,LS,X =:= Y,ground(X/Y),builtin)
	    ).

'=\\=_andorra'(X,Y,L,L1,_):- 
	( ground(X/Y) -> L=L1, X =\= Y ;
	    L=[S|L1],
	    term_variables([X,Y],LS),
	    suspend_andorra(S,LS,X =\= Y,ground(X/Y),builtin)
	    ).

is_andorra(X,Y,L,L1,_):- 
	( ground(Y) -> L = L1, X is Y ;
	  L=[S|L1],
	  term_variables(Y,LS),
          suspend_andorra(S,LS,X is Y,ground(Y),builtin)
	  ).


arithexpression_andorra(X,L,L1,_):- 
	( ground(X) -> L = L1, arithexpression(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,arithexpression(X),ground(X),builtin)
	  ).


name_andorra(X,Y,L,L1,_):-
	( (ground(X);ground(Y)) -> L = L1, name(X,Y) ;
	    L = [S|L1],
	    term_variables([X,Y],LS),
          suspend_andorra(S,LS,name(X,Y),(ground(X);ground(Y)),builtin)
	  ).

atom_codes_andorra(X,Y,L,L1,_):-
	( (ground(X);ground(Y)) -> L = L1, atom_codes(X,Y) ;
	    L = [S|L1],
	    term_variables([X,Y],LS),
          suspend_andorra(S,LS,atom_codes(X,Y),(ground(X);ground(Y)),builtin)
	  ).

number_codes_andorra(X,Y,L,L1,_):-
	( (ground(X);ground(Y)) -> L = L1, number_codes(X,Y) ;
	    L = [S|L1],
	    term_variables([X,Y],LS),
          suspend_andorra(S,LS,number_codes(X,Y),(ground(X);ground(Y)),builtin)
	  ).
/*
number_codes_andorra(X,L,L1,_,Y,Z):-
	((ground(X/Y);ground(Z/Y)) -> L = L1, number_codes(X,Y,Z) ;
	    L = [S|L1],
	    term_variables([X,Y,Z],LS),
          suspend_andorra(S,LS,number_codes(X,Y,Z),(ground(X/Y);ground(Z/Y)),builtin)
	  ).
*/

atom_length_andorra(X,Y,L,L1,_):- 
	( ground(X) -> L = L1, atom_length(X,Y);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,atom_length(X,Y),ground(X),builtin)
	  ).


atom_concat_andorra(X,Y,Z,L,L1,_):- 
	( (ground(X/Y);ground(X/Z);ground(Y/Z)) -> L = L1, atom_concat(X,Y,Z)
        ; L=[S|L1],
	  term_variables([X,Y,Z],LS),
          suspend_andorra(S,LS,atom_concat(X,Y,Z),(ground(X/Y);ground(X/Z);ground(Y/Z)),builtin)
	).

/*
sub_atom_andorra(X,L,L1,_,Y,Z,U):- 
	( ground(X/Y/Z) -> L = L1, sub_atom(X,Y,Z,U);
	  L=[S|L1],
	  term_variables([X,Y,Z],LS),
          suspend_andorra(S,LS,sub_atom(X,Y,Z,U),ground(X/Y/Z),builtin)
	  ).
*/

int_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, int(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,int(X),nonvar(X),builtin)
	  ).

integer_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, integer(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,integer(X),nonvar(X),builtin)
	  ).




nnegint_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, nnegint(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,nnegint(X),nonvar(X),builtin)
	  ).

flt_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, flt(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,flt(X),nonvar(X),builtin)
	  ).

float_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, float(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,float(X),nonvar(X),builtin)
	  ).


num_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, num(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,num(X),nonvar(X),builtin)
	  ).

number_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, number(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,number(X),nonvar(X),builtin)
	  ).





atm_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, atm(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,atm(X),nonvar(X),builtin)
	  ).

atom_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, atom(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,atom(X),nonvar(X),builtin)
	  ).


struct_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, struct(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,struct(X),nonvar(X),builtin)
	  ).

gnd_andorra(X,L,L1,_):- 
	( ground(X) -> L = L1, gnd(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,gnd(X),ground(X),builtin)
	  ).

constant_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, constant(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,constant(X),nonvar(X),builtin)
	  ).

atomic_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, atomic(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,atomic(X),nonvar(X),builtin)
	  ).




callable_andorra(X,L,L1,_):- 
	( nonvar(X) -> L = L1, callable(X);
	  L=[S|L1],
	  term_variables(X,LS),
          suspend_andorra(S,LS,callable(X),nonvar(X),builtin)
	  ).

arg_andorra(X,Y,Z,L,L1,_):- 
	( (ground(X),nonvar(Y)) -> L = L1, arg(X,Y,Z);
	  L=[S|L1],
	  term_variables([X,Y],LS),
          suspend_andorra(S,LS,arg(X,Y,Z),(ground(X),nonvar(Y)),builtin)
	  ).

functor_andorra(X,Y,Z,L,L1,_):- 
	( (nonvar(X);(nonvar(Y),nonvar(Z))) -> L = L1, functor(X,Y,Z);
	  L=[S|L1],
	  term_variables([X,Y,Z],LS),
          suspend_andorra(S,LS,functor(X,Y,Z),(nonvar(X);(nonvar(Y),nonvar(Z))),builtin)
	  ).

'=.._andorra'(X,Y,L,L1,_):- 
	( (nonvar(X);nonvar(Y)) -> L = L1, X =.. Y;
	  L=[S|L1],
	  term_variables([X,Y],LS),
          suspend_andorra(S,LS,X =.. Y,(nonvar(X);nonvar(Y)),builtin)
	  ).
