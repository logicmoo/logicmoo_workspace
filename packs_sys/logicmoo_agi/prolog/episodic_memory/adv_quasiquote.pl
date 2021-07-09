
:- '$set_source_module'(mu).

% @TODO
parse_kind(Type, String, Logic):- trace_or_throw(parse_kind(Type, String, Logic)).

flatten_ul(NewData, NewDataF):- var(NewData), !, NewDataF=NewData.
flatten_ul(NewData, NewDataF):- is_list(NewDataM), flatten(NewData, NewDataM), NewData\==NewDataM, !, flatten_ul(NewDataM, NewDataF).
flatten_ul([NewData], NewDataF):- !, flatten_ul(NewData, NewDataF).
flatten_ul(NewData, NewDataF):- NewData=NewDataF.

remove_any_elements( Ctx, Dom, Term):- is_list(Dom), must_maplist(remove_any_elements(Ctx), Dom, Term).
remove_any_elements(_Ctx, Dom, Term):- var(Dom), Term=Dom, !.
remove_any_elements(_Ctx, Dom, Term):- string(Dom), munl_call(into_text80(Dom, Term)), !.
remove_any_elements(_Ctx, Dom, Term):- atom(Dom), munl_call(into_text80(Dom, TermM)), !, (TermM=[Term] -> true; TermM=Term), !.
remove_any_elements(_Ctx, Dom, Term):- \+ compound(Dom), Term = Dom, !.
remove_any_elements( Ctx, element(M:S, [], Data), Term):-
  remove_any_elements(e(M:S, Ctx), Data, NewData),
  flatten_ul(NewData, NewDataF),
  P=..[S, NewDataF], !,
  remove_any_elements(Ctx, M:P, Term).

remove_any_elements( Ctx, element(S, [], Data), Term):-
  remove_any_elements(e(S, Ctx), Data, NewData),
  flatten_ul(NewData, NewDataF),
  P=..[S, NewDataF], !,
  remove_any_elements(Ctx, P, Term).

remove_any_elements( Ctx, Dom, Term):-
  compound_name_arguments(Dom, F, Args),
  remove_any_elements( Ctx, Args, MArgs),
  (is_list(MArgs)
    -> (length(MArgs, ML), length(Args, L),
            (ML == L
             -> compound_name_arguments(Term, F, MArgs)
               ; Term =..[F, MArgs]))
   ; (Term=MArgs)).


i7_term(Ctx, Dom):- dmsg('dont_call_directly !!!!'(i7_term(Ctx, Dom))).

%! i7_syntax(+TypInModule, +CallModule, +Vars:list, +Dict:list, -Term:list) is det
%
% called at compile time to convert the quasiquoted English to
% Prolog code
%
% We make ourselves look like the Logic To English Output
% so you can insert English in the content fed to i7//1
%
% or {|i7(Subj)||<declarative>Subj is shiny and cold!</declarative>|}
i7_syntax(TiM, M, Content, Vars, Dict, TermOut):-
    must_be(list, Dict),
    with_quasi_quotation_input(
        Content, In,
        % we cheat using SGML parser to normalize the English <tags>'s
        load_sgml(In, Dom,
                  [  dialect(html5),
                     attribute_value(string),
                     cdata(string),
                     system_entities(true),
                     space(remove),
                     syntax_errors(quiet),
                     case_preserving_attributes(true),
                     case_sensitive_attributes(false),
                  max_errors(-1)])),
      % i7_syntax_term(TiM, M, Vars, Dict, Dom, TermOut).
% i7_syntax_term(TiM, M, Vars, Dict, Dom, TermOut):-
    Ctx = v(TiM, M, Vars, Dict),
    copy_term(Ctx:Dom, CtxC:DomC),
    remove_any_elements(CtxC, DomC, TermC), flatten_ul(TermC, TermF),
    remove_any_elements(CtxC, CtxC, Out),
    TermOut = i7_term(Out, TermF).

:- use_module(library(quasi_quotations)).

%! i7(+Vars:list, +Dict:list, -Term:list) is det
%
% called at compile time to convert the quasiquoted English to
% Prolog code
%
% We make ourselves look like the Logic To English Output
% so you can insert English in the content fed to i7//1
%
% or {|i7(Subj)||<declarative>Subj is shiny and cold!</declarative>|}
:- module_transparent(i7/4).
i7(Content, Vars, Dict, Term) :-
    '$current_typein_module'(TiM),
    strip_module(_, M, _),
    i7_syntax(TiM, M, Content, Vars, Dict, Term).

:- export(i7/4).
:- system:import(i7/4).

% so we recognize {|i7(type(Subj))||.... |}
:- quasi_quotation_syntax(system:i7).


/*
testi7(a) :- Subj=7, Y={|i7(Subj)|| fooSubj |}, writeln(Y).

testi7(b) :- Z = {|i7(7)|| oogle |}, writeln(Z).

testi7(c) :- Y={|i7(7)|| fooSubj |}, writeln(Y).

:- listing(testi7/1).
*/

