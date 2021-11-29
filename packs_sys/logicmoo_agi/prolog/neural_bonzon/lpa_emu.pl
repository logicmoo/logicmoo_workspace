/* compiler_cleaner */
/* BNF syntax
<tree> ::= [] || <sequence> || [<alternative>]
<sequence> ::= [<instruction>|<tree>]
<alternative> ::= <branch> ||(<branch>;<alternative>)
<branch> ::=(<guard>|<tree>)
*/
:- use_module(library(apply),[maplist/3]).
:- use_module(library(logicmoo_utils)).
:- cls.
:-op(600,fx,'@').
:-op(800,xfy,'>>>').
:-op(700,xfy,'o').
:-op(901,xfy,':::').
:-op(904,xfy,'=>').

bonzon_bi(lpa_call_sense/1).
bonzon_bi(lpa_call/1). bonzon_bi(apply/2). bonzon_bi(wdmsg/1).

bonzon_bi((->)/2). bonzon_bi(!/1). bonzon_bi(('[|]')/2). bonzon_bi(('|')/2). bonzon_bi(('$VAR')/1). bonzon_bi((,)/2).
bonzon_bi((:::)/2). bonzon_bi((;)/2). bonzon_bi((=)/2).
bonzon_bi((>>>)/2). bonzon_bi((\+)/1). bonzon_bi(assert/1).
bonzon_bi(atom/1). bonzon_bi(call/1). bonzon_bi(compile/2). bonzon_bi(compileAlternative/4). bonzon_bi(compileBranch/4). 
bonzon_bi(compileInstruction/4). bonzon_bi(compileSequence/4). bonzon_bi(compileTree/4).  bonzon_bi(compound_name_arguments/3).
bonzon_bi(else/1). bonzon_bi(findall/3). bonzon_bi(for_each/3). bonzon_bi(forall/2). bonzon_bi(functor/3).  bonzon_bi(get_code/1).
bonzon_bi(if/2). bonzon_bi(if/3). bonzon_bi(if_not/2). bonzon_bi(insert/2). bonzon_bi(instance/2). bonzon_bi(instruction/1).
bonzon_bi(interrupt/1). bonzon_bi(is/2). bonzon_bi(is_list/1). bonzon_bi(is_list/2). bonzon_bi(ist/2).
bonzon_bi(length/2). bonzon_bi(append/3). bonzon_bi(lpa_functor/1). bonzon_bi(load/1).
bonzon_bi(loop/1). bonzon_bi(maplist/3). bonzon_bi(member/2). bonzon_bi(('+')/2). bonzon_bi(('=>')/2).
bonzon_bi(from/1). bonzon_bi(do/1). bonzon_bi(new/1). bonzon_bi(nth0/3). bonzon_bi(random/1). bonzon_bi(random/2).
bonzon_bi(random_e/2). bonzon_bi(react/1). bonzon_bi(read/1). bonzon_bi(reflect/1). bonzon_bi(remove/2). 
bonzon_bi(retractall/1). bonzon_bi(run/1). bonzon_bi(such_that/1). bonzon_bi(set/2).
bonzon_bi(then/1). bonzon_bi(write/1). 

bonzon_builtin(F/A):- nonvar(F),bonzon_bi(F/A),!.
bonzon_builtin(F/A):- current_predicate(system:F/A), format('%~~ ~N~q.~n',[bonzon_bi(F/A)]).

%lpa_zave_varname(N,V):- debug_var(N,V),!.
lpa_zave_varname(N,V):- V = '$VAR'(N).


lpa_implode_varnames(Vs):- (var(Vs) ; Vs==[]),!.
lpa_implode_varnames([NV|Vs]) :- lpa_implode_varnames(Vs),
    (var(NV) -> ignore((get_var_name(NV,Name),lpa_zave_varname(Name,NV))); 
  ignore((NV=(N=V),lpa_zave_varname(N,V)))).


user:portray(P):- is_list(P),writeq(P).
lpa_expansion(I,O):- \+ compound(I) ; I=(:- _),!,I=O.
%lpa_expansion(I,O):- sub_term(E,I),is_list(E), !,I=O.
lpa_expansion(I,O):- 
  prolog_load_context(variable_names,Vs),copy_term(I:Vs,II:VVs),
  lpa_implode_varnames(VVs),
  term_variables(II,TVs),
  maplist(=('$VAR'('_')),TVs),
  %prolog_load_context(variable_names,Vs),
  %pterm_to_sterm(I,O),
  %I=O,
  pterm_to_sterm(II,OO),
  unnumbervars(OO,O),
  nop(II\==OO-> wdmsg(II); true), format('~N'),pc(OO),!.

%pc(O):- print(O),writeln('.'),!.
pc(O):- prolog_listing:portray_clause(O).
pc(O):- prolog_listing:portray_body(O, 0, indent, 1199, current_output, [portray(true),quoted(true), output(current_output)]).
pc(O):- print_tree(O).

pterm_to_sterm(In,In):-  \+ compound(In),!.
pterm_to_sterm('$VAR'(In),'$VAR'(In)):-!.
pterm_to_sterm(In,Out):-  is_list(In), !, maplist(pterm_to_sterm,In,Out).
%pterm_to_sterm(In,Out):- sub_term(E,In),is_list(E), !, In=Out.
pterm_to_sterm(In,Out):- 
 compound_name_arguments(In,N,A),
 functor(In,N,Ar),
 maplist(pterm_to_sterm,A,AA),
 lis_to_pterm(Ar,[N|AA],Mid),
 maybe_fix_list(Mid,Out).

maybe_fix_list(Mid,Out):- maybe_fix_list_0(Mid,Out),ignore((Mid=[A|_],atom(A),assert_if_new(bonzon_mach:bonzon_op(A)))).
maybe_fix_list_0(Mid,Out):- \+ compound(Mid), Out= Mid.
maybe_fix_list_0('@'(T),T):- !.
maybe_fix_list_0(Mid,Out):- \+ is_list(Mid), Out= Mid.
maybe_fix_list_0(['@',T],T):- !.
maybe_fix_list_0([H|T],[H|TT]):- maybe_fix_list_0(T,TT).

maybe_var(A,_):- \+ atom(A),!,fail.
maybe_var(A,AA):- downcase_atom(A,A),!,AA=A.
maybe_var(A,AA):- AA='$VAR'(A).
lis_to_pterm(_,X,X):- \+ compound(X),!.
lis_to_pterm(_,X,X):- \+ is_list(X),!.
%lis_to_pterm(_,[N|A],Y):- maplist(pterm_to_sterm,A,AA),A\==AA,!,lis_to_pterm(_,[N|AA],Y).
lis_to_pterm(_,[N|A],Y):- atom(N),upcase_atom(N,N),downcase_atom(N,N),is_list(A),compound_name_arguments(Y,N,A).
lis_to_pterm(_,[A,@(B)],[AA|B]):-!,maybe_var(A,AA).
lis_to_pterm(_,[A,@,B],[A|B]):-!.
lis_to_pterm(_,['$VAR',A],['$VAR'(A)]):-!.
lis_to_pterm(_,[N|A],[N|A]):- atom(N),instruction(N),!.
lis_to_pterm(_,[o,A,B],NN):- !, append(A,B,NN).
lis_to_pterm(Ar,[N|A],Y):- atom(N),bonzon_builtin(N/Ar),!,assertion((atom(N),is_list(A))),compound_name_arguments(Y,N,A).
lis_to_pterm(_,[N|A],[NN|A]):- maybe_var(N,NN),!.
lis_to_pterm(_,[N|A],[N|A]):-!.
%lis_to_pterm(_,[N|A],Y):- atom(N),compound_name_arguments(Y,N,A).
%lis_to_pterm(_,NA,t(NA)).



term_expansion(I,P,O,P):- 
  notrace(current_prolog_flag(allow_variable_name_as_functor,true)),
  compound(I),
  nonvar(P),
  prolog_load_context(term,T),T==I,
  lpa_expansion(I,O),!.

:- style_check(-singleton).


