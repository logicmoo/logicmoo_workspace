%-------------------------------------------------------------
% parser RuleML
% Marco Gavanelli
% 11 May 2006
% This is for RuleML 0.9
%-------------------------------------------------------------

:-module(ruleml_parser,
	 [save_ics_ruleml/1,
	 ruleml_parse_file/3,
	 ruleml_parse/3]).

:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(parser_utils).
:- use_module(debug).
:- use_module(library(xml)).


save_ics_ruleml(FileName):-
    consult(ics),
    findall(ic(Body,Head),ics(Body,Head),ICList),
    numbervars(ICList,1,_),
    ruleml_parse(XML,xml,ICList),
    ground_options(XML),
    xml_parse(FileString,XML),
    open(FileName,write,Stream),
    write_file(Stream,FileString),
    close(Stream).
    

ruleml_parse_file(FileName,ICList,Error):-
    read_file_to_string(FileName,FileString),
    xml_parse(FileString,XML),
%    xml_parse(Back,XML),
    (XML = xml(_,[X])
        ->  (X=pcdata(_)
                -> Error = no_ruleml
                ;  ruleml_parse(XML,xml,ICList), Error=[]
            )
        ;   Error = no_ruleml
    ).




ruleml_parse(xml(_,[L]),xml,ICList):- ruleml_parse(L,ruleml,ICList).
ruleml_parse(element('RuleML',_,[L]),ruleml,ICList):- ruleml_parse(L,assert,ICList).
ruleml_parse(element('Assert',_,L),assert,ICList):- ruleml_parse(L,implies_list,ICList).

ruleml_parse(element('Implies',_,[element(body,_,[BodyX]),element(head,_,[HeadX])]),implies,IC):-
    IC = ic(Body,Head), 
    ruleml_parse(BodyX,conjunct,Body),
    ruleml_parse(HeadX,head,Head),!.
ruleml_parse(element('Implies',_,[element(head,_,[HeadX]),element(body,_,[BodyX])]),implies,IC):-
    IC = ic(Body,Head), 
    ruleml_parse(BodyX,conjunct,Body),
    ruleml_parse(HeadX,head,Head).



ruleml_parse(element('Or',_,L),head,List):-
    List = [_,_|_], % OR must have at least two arguments
    ruleml_parse(L,disjunctlist,List).
ruleml_parse(RuleML,head,[List]):-
    ruleml_parse(RuleML,conjunct,List).

ruleml_parse([],disjunctlist,[]).
ruleml_parse([H|T],disjunctlist,[A|R]):-
    ruleml_parse(H,conjunct,A),
    ruleml_parse(T,disjunctlist,R).

ruleml_parse(element('And',_,L),conjunct,List):-
    List = [_,_|_], % And must have at least two arguments
    ruleml_parse(L,atomlist,List).
ruleml_parse(RuleML,conjunct,[Atom]):-
    ruleml_parse(RuleML,atom,Atom).


ruleml_parse([],atomlist,[]).
ruleml_parse([H|T],atomlist,[A|R]):-
    ruleml_parse(H,atom,A),
    ruleml_parse(T,atomlist,R).

% Explicit Negation: if an atom is requested, but it starts with <Neg>
% then, produce a negated atom
ruleml_parse(XML,atom,Atom):- % XML -> IC
    % The negated atom may be optionally surrounded by a <strong> role
    ( XML = element('Neg',_,[element('Atom',_,[Rel|Args])])
    ; XML = element(strong,_,[element('Neg',_,[element('Atom',_,[Rel|Args])])])
    ),
    nonvar(Rel),!,
    ruleml_parse(Rel,rel,Fun),
    ruleml_parse(Args,termlist,AtomArgs),
    neg_functor(Fun,NegFun),
    Atom =.. [NegFun|AtomArgs].
% Default Negation: if an atom is requested, but it starts with <Naf>
% then, produce a negated atom
ruleml_parse(XML,atom,Atom):- % XML -> IC
    % The negated atom may be optionally surrounded by a <strong> role
    ( XML = element('Naf',_,[element('Atom',_,[Rel|Args])])
    ; XML = element(weak,_,[element('Naf',_,[element('Atom',_,[Rel|Args])])])
    ),
    nonvar(Rel),!,
    ruleml_parse(Rel,rel,Fun),
    ruleml_parse(Args,termlist,AtomArgs),
    naf_functor(Fun,NegFun),
    Atom =.. [NegFun|AtomArgs].
% Positive atom
ruleml_parse(element('Atom',_,[Rel|Args]),atom,Atom):- % XML -> IC
    nonvar(Rel),!,
    ruleml_parse(Rel,rel,Fun),
    ruleml_parse(Args,termlist,AtomArgs),
    Atom =.. [Fun|AtomArgs].
% Negated atom
ruleml_parse(element('Neg',_,[element('Atom',_,[Rel|Args])]),atom,Atom):- % IC -> XML
    nonvar(Atom),
    Atom =.. [NegFun|AtomArgs],
    neg_functor(Fun,NegFun),!,
    ruleml_parse(Rel,rel,Fun),
    ruleml_parse(Args,termlist,AtomArgs).
ruleml_parse(element('Naf',_,[element('Atom',_,[Rel|Args])]),atom,Atom):- % IC -> XML
    nonvar(Atom),
    Atom =.. [NegFun|AtomArgs],
    naf_functor(Fun,NegFun),!,
    ruleml_parse(Rel,rel,Fun),
    ruleml_parse(Args,termlist,AtomArgs).
ruleml_parse(element('Atom',[],[Rel|Args]),atom,Atom):- % IC -> XML
    nonvar(Atom),
    Atom =.. [Fun|AtomArgs],
    ruleml_parse(Rel,rel,Fun),
    ruleml_parse(Args,termlist,AtomArgs).

ruleml_parse([],termlist,[]).
ruleml_parse([H|T],termlist,[A|R]):-
    ruleml_parse(H,term,A),
    ruleml_parse(T,termlist,R).

ruleml_parse(element('Var',_,[pcdata(String)]),term,VarName):- % variable
    nonvar(String),
    atom_codes(VarName,String).
ruleml_parse(element('Var',[],[pcdata(String)]),term,VarName):- % variable
    nonvar(VarName), VarName='$VAR'(N),
    number_codes(N,C2),
    atom_codes('X',C1),
    append(C1,C2,String).
ruleml_parse(element('Ind',_,[pcdata(String)]),term,Constant):- % constant
    (atomic(Constant); nonvar(String)),!,
    atom_codes(Constant,String).
ruleml_parse(element('Cterm',_,[Ctor|Args]),term,Term):- %complex term, XML -> IC
    nonvar(Ctor),
    ruleml_parse(Ctor,ctor,Fun),
    ruleml_parse(Args,termlist,AtomArgs),
    Term =.. [Fun|AtomArgs].
ruleml_parse(element('Cterm',[],[Ctor|Args]),term,Term):- %complex term, IC -> XML
    nonvar(Term), Term =.. [Fun|AtomArgs],
    ruleml_parse(Ctor,ctor,Fun),
    ruleml_parse(Args,termlist,AtomArgs).


ruleml_parse(element('Rel',_,Functor),rel,Fconv):- % the functor of an atom
    ruleml_parse(Functor,functor_rel,Fconv).
ruleml_parse(element(op,_,[element('Rel',_,Functor)]),rel,Fconv):- % the functor of an atom: op is optional
    ruleml_parse(Functor,functor_rel,Fconv).

ruleml_parse(element('Ctor',_,Functor),ctor,Fconv):- % the functor of a term
    ruleml_parse(Functor,functor,Fconv).
ruleml_parse(element(op,_,[element('Ctor',_,Functor)]),ctor,Fconv):- % the functor of a term: op is optional
    ruleml_parse(Functor,functor,Fconv).

ruleml_parse(Functor,functor_rel,Fconv):-
    Functor = [pcdata(F)],
    (nonvar(F)
       ->   atom_codes(Fun,F),
            conv_fun(Fun,Fconv)
        ;   conv_fun(Fun,Fconv),
            atom_codes(Fun,F)
    ).
            

ruleml_parse(Functor,functor,Fconv):-
    Functor = [pcdata(F)],
    atom_codes(Fconv,F).

ruleml_parse([],implies_list,[]).
ruleml_parse([X|T],implies_list,[IC1|ICList]):-
    ruleml_parse(X,implies,IC1),
    ruleml_parse(T,implies_list,ICList).



/*
ruleml_parse([],_,[]).
ruleml_parse([X|T],A,ICList):-
    ruleml_parse(X,A,IC1),
    ruleml_parse(T,A,IC2), append(IC1,IC2,ICList).
*/

ruleml_parse(comment(X),_,[]):- nonvar(X). % so it does not insert comments in the ICS->XML
ruleml_parse(namespace(_,_,L),X,ICList):- ruleml_parse(L,X,ICList).



conv_fun('E',e):- !.
conv_fun('EN',en):- !.
conv_fun('H',h):- !.
conv_fun('!H',noth):- !.
conv_fun('!E',note):- !.
conv_fun('!EN',noten):- !.
conv_fun(X,X).

% Conversion, in SCIFF internal syntax, of a functor and the
% corresponding functor for explicit negation
% If a functor is not reported here, then it cannot be used with explicit negation
neg_functor(e,note):-!.
neg_functor(en,noten):-!.

% same as neg_functor, but for negation as failure
naf_functor(h,noth).

ground_options(T):- term_variables(T,V), ground_vars(V).
ground_vars([]).
ground_vars([[]|T]):- ground_vars(T).
/*
ground_options(X):- ground(X),!.
ground_options(element(_,X,_)):- var(X),!,X=[].
ground_options([H|T]):-
*/

write_file(_,[]).
write_file(Stream,[C|T]):-
    put_code(Stream,C),
    write_file(Stream,T).
