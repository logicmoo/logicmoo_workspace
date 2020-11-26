
%:- use_module(library(logicmoo_common)).

%writenl(P):- write(P),nl. 

style_check(_).

forall(P,Q) :- \+ (P, \+Q).

sub_term(N,N).
sub_term(N,T1+T2):- sub_term(N,T1);sub_term(N,T2).

ttyflush.

%writeln(P):- write(P),nl. 

ticks(Z1):-  statistics(runtime,[Z1,_]).

:- ensure_loaded('genSymPatches.pl').


?- prolog_flag(single_var_warnings,_,off).


clause_w_names(Head,Body,ClauseRef,[allVars=LocalVars,props=Props]):- 
  clause(Head,Body,ClauseRef),
  findall(Prop,clause_property(ClauseRef, Prop), Props),
  term_variables(Head+Body,LocalVars).

