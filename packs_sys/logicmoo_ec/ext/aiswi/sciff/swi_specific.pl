% Code specific to the host Prolog language
% Version for SWI Prolog

:- module(swi_specific,
    [subsumeschk/2,get_atts/2,put_atts/2,term_variables_bag/2,attribute/1, text_style/1]).

subsumeschk(General,Specific):-
    subsumes_chk(General,Specific).

% SICStus requires the definition of attributes through an 'attribute' declaration,
% while SWI raises an error. So, I define a dummy attribute/1 predicate that does nothing
attribute(_).

% Definition of the predicate get_atts, built-in in SICStus.
% This definition makes sense only for the two attributes quant/1 and restrictions/1 
get_atts(Var, AccessSpec):-
    nonvar(AccessSpec),!,
    (AccessSpec=quant(Dom) -> get_attr(Var,quant,quant(Dom)) ;
     AccessSpec=restrictions(R) -> get_attr(Var,restrictions,restrictions(R)) ;
     writeln('*** ERROR of porting SCIFF from SICStus to SWI ***'),
     writeln('*** Please contact SCIFF developers ***')).

put_atts(Var, AccessSpec):-
    nonvar(AccessSpec),!,
    (AccessSpec=quant(Dom) -> put_attr(Var,quant,quant(Dom)) ;
     AccessSpec=restrictions(R) -> put_attr(Var,restrictions,restrictions(R)) ;
     writeln('*** ERROR of porting SCIFF from SICStus to SWI ***'),
     writeln('*** Please contact SCIFF developers ***')).
% Forse questo si potrebbe direttamente sostituire, cosi` diventa piu` "standard"
term_variables_bag(Term, Variables):-
    term_variables(Term, Variables).

text_style(Num):-
    write_term('\033[',[character_escapes(true)]),
    write_term(Num,[character_escapes(true)]),
    write_term('m',[character_escapes(true)]).

