%-------------------------------------------------------------
% parser RuleML
% Marco Gavanelli
% 3 May 2006
% This is for an old version, I think it's the 0.8, but
% I'm not sure
%-------------------------------------------------------------

:- use_module(parser_utils).
:- use_module(debug).
:- use_module(library(xml)).

ruleml_parse_file(FileName,ICList):-
    read_file_to_string(FileName,FileString),
    xml_parse(FileString,XML),
    ruleml_parse(XML,ICList).

ruleml_parse(xml(_,L),ICList):- ruleml_parse(L,ICList).
ruleml_parse(element(rulebase,_,L),ICList):- ruleml_parse(L,ICList).
ruleml_parse(element(imp,_,[element(head,_,[HeadX]),element(body,_,[BodyX])]),[IC]):-
    IC = ic(Body,Head), 
    ruleml_conjunct(BodyX,Body),
    ruleml_head(HeadX,Head).
ruleml_parse(element(imp,_,[element(body,_,[BodyX]),element(head,_,[HeadX])]),[IC]):-
    IC = ic(Body,Head), 
    ruleml_conjunct(BodyX,Body),
    ruleml_head(HeadX,Head).

ruleml_parse([X|T],ICList):-
    ruleml_parse(X,IC1),
    ruleml_parse(T,IC2), append(IC1,IC2,ICList).
ruleml_parse([],[]).

ruleml_head(element(or,_,L),List):-
    ruleml_disjunctlist(L,List).
ruleml_head(RuleML,[List]):-
    ruleml_conjunct(RuleML,List).

ruleml_disjunctlist([],[]).
ruleml_disjunctlist([H|T],[A|R]):-
    ruleml_conjunct(H,A),
    ruleml_disjunctlist(T,R).

ruleml_conjunct(element(and,_,L),List):-
    ruleml_atomlist(L,List).
ruleml_conjunct(RuleML,[Atom]):-
    ruleml_atom(RuleML,Atom).


ruleml_atomlist([],[]).
ruleml_atomlist([H|T],[A|R]):-
    ruleml_atom(H,A),
    ruleml_atomlist(T,R).

ruleml_atom(element(atom,_,[element(opr,_,[element(rel,_,Functor)])|Args]),Atom):-
    ruleml_functor(Functor,Fun),
    ruleml_termlist(Args,AtomArgs),
    Atom =.. [Fun|AtomArgs].

ruleml_termlist([],[]).
ruleml_termlist([H|T],[A|R]):-
    ruleml_term(H,A),
    ruleml_termlist(T,R).

ruleml_term(element(var,_,[pcdata(String)]),VarName):-
    atom_codes(VarName,String).
ruleml_term(element(ind,_,[pcdata(String)]),Constant):-
    atom_codes(Constant,String).

ruleml_functor(Functor,Fconv):-
    Functor = [pcdata(F)],
    atom_codes(Fun,F),
    conv_fun(Fun,Fconv).

conv_fun('E',e):- !.
conv_fun('EN',en):- !.
conv_fun('H',h):- !.
conv_fun('!H',noth):- !.
conv_fun('!E',note):- !.
conv_fun('!EN',noten):- !.
conv_fun(X,X).
