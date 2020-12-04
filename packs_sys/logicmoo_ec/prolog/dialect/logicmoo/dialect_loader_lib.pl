:- module(dialect_loader_lib,[dialect_on_load/3,get_prolog_load_context/1]).

:- dynamic(setup_dialect_load_context/1).

get_prolog_load_context([variable_names=Vs,module=M,file=F,line=L,dialect=D,source=S,reload=R]):- 
  source_location(F,L),
  prolog_load_context(variable_names,Vs),
  prolog_load_context(module,M),
  prolog_load_context(dialect,D),
  prolog_load_context(source,S),
  (prolog_load_context(reload,R)->true;R=false).

:- module_transparent(dialect_on_load/3).
:- meta_predicate(dialect_on_load(1,+,:)).

dialect_on_load(P1,T,DC):-
 strip_module(DC,_,DC0),
 b_setval('$term',T),
 b_setval('$prolog_load_context',DC0),
 ignore((memberchk(variable_names=Vs,DC0),
 b_setval('$variable_names',Vs))),
 call(P1,T).                                                       



