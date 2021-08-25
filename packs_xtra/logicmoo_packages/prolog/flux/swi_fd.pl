
:- use_module(library('clp/clpfd')).

:-  op(750, fy, #\+).
:-  op(700, xfx, ##).
:-  op(760, yfx, #/\).
:-  op(770, yfx, #\/).
:-  op(780, yfx, #=>).
:-  op(790, yfx, #<=>).
:-  op(800, xfx, isd).
:-  op(400, yfx, ('*`')). 

:-  op(750, fy, local).

:- meta_predicate local(:).

local(_M:Call) :- Call.

variable(Var):-nb_setval(Var,[]).
variable(Var,VAL):-nb_setval(Var,VAL).
setval(Var,VAL):-nb_setval(Var,VAL).
getval(Var,VAL):-nb_getval(Var,VAL).

% summary:"Succeeds if Term is a domain variable.
is_domain(T):-clpfd:fd_get(T, Dom, _), !, T in Dom.

setval(X):-trace,nb_setval(X,[]).

% copy_term_vars(?Vars, ?OldTerm, -NewTerm)
copy_term_vars(Vars,OldTerm,NewTerm):-copy_term(OldTerm,NewTerm),term_variables(NewTerm,Vars).

#\+(C):- #\(C).

is_predicate(F/A):-current_predicate(F/A), functor(P,F,A),predicate_property(P, visible).
