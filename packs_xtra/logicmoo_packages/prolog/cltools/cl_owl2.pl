/* -*- Mode: Prolog -*- */

:- module(cl_owl2,
          [
          ]).

:- use_module(cl_io).
:- use_module(cl_transform).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_metamodel')).
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_util')).
:- use_module(library('thea2/owl2_from_rdf'),[expand_ns/2]).

:- multifile cl_io:parse_cltext_hook/4.
cl_io:parse_cltext_hook(File,owl,Text,Opts) :-
        import_owl(File,Text,Opts).

% ----------------------------------------
% OWL -> CL
% ----------------------------------------

% mostly direct translation from Thea.
% the main difference is that Thea uses predicates
% of fixed arity, whereas in CL we use arbitary arity.
% e.g. equivalentClasses([C1,...,Cn]) --> EquivalentClasses(C1 C2 ... Cn)

import_owl(File,SL,_Opts) :-
        owl2_io:load_axioms(File),
        findall(S,
                (   axiom(A),
                    owl_axiom_to_cl_sentence(A,S)),
                SL).

owl_axiom_to_cl_sentence(A,S) :-
        A=..[P|Args],
        thea_pred_to_cl_pred(P,P2),
        maplist(thea_term_to_cl_term,Args,Args2),
        flatten(Args2,Args2flat),
        S=..[P2|Args2flat].

thea_term_to_cl_term(T,T) :-
        atom(T),
        !.
thea_term_to_cl_term(T,T2) :-
        is_list(T),
        !,
        maplist(thea_term_to_cl_term,T,T2).
thea_term_to_cl_term(T,T2) :-
        !,
        T=..[P|Args],
        thea_pred_to_cl_pred(P,P2),
        maplist(thea_term_to_cl_term,Args,Args2),
        flatten(Args2,Args2flat),
        T2=..[P2|Args2flat].

thea_pred_to_cl_pred(P,P2) :-
        sub_atom(P,0,1,_,C1),
        sub_atom(P,1,_,0,Rest),
        upcase_atom(C1,C1u),
        concat_atom(['owl:',C1u,Rest],P2).

% ----------------------------------------
% CL -> OWL
% ----------------------------------------

:- multifile cl_io:serialize_cltext_hook/4.
cl_io:serialize_cltext_hook(File,owl,Text,Opts) :-
        export_owl(File,Text,Opts).

export_owl(File,Text,_Opts) :-
        Prefix='http://example.org#',
        forall(text_sentence(Text,S),
               cl_sentence_to_thea_owl_axiom(Prefix,S)),
        expand_namespaces,
        save_axioms(File,owl).

cl_sentence_to_thea_owl_axiom(Prefix,S) :-
        (   cl_sentence_to_thea_owl_axiom(Prefix,S,S2)
        ->  assert_axiom(S2)
        ;   print_message(error,no_translation(S))).


cl_sentence_to_thea_owl_axiom(Prefix,S,S2) :-
        S=..[P|Args],
        cvt_pred(P,P2),
        term_to_owl(Prefix,Args,Args2),
        pred_args_term(P2,Args2,S2).


term_to_owl(_Prefix,[],[]) :- !.
term_to_owl(Prefix,[H|L],[H2|L2]) :-
        !,
        term_to_owl(Prefix,H,H2),
        term_to_owl(Prefix,L,L2).

term_to_owl(_,literal(S),literal(S)) :- !.
term_to_owl(Prefix,S,S2) :-
        atom(S),
        !,
        (   expand_ns(S,S2),       % e.g. rdfs:label
            sub_atom(S2,_,_,_,':') % bit of a hack...
        ->  true
        ;   atom_concat(Prefix,S,S2)).

term_to_owl(Prefix,S,S2) :-
        S=..[P|Args],
        cvt_pred(P,P2),
        !,
        term_to_owl(Prefix,Args,Args2),
        pred_args_term(P2,Args2,S2).

term_to_owl(Prefix,S,S2) :-
        S=..[P|Args],
        !,
        term_to_owl(Prefix,Args,Args2),
        S2=..[P|Args2].

pred_args_term(P2,Args2,S2) :-
        (   pred_args(P2,[Type]),
            format(user_error,'~w->~w~n',[P2,Type]),
            (   Type=list(_)
            ;   Type=set(_))
        ->  S2=..[P2,Args2]
        ;   S2=..[P2|Args2]).



pred_args(P,Args) :- owlpredicate_arguments(P,Args).
pred_args(P,Args) :- owlpredicate_typed(P,PT),owlpredicate_arguments(PT,Args).

% by convention, terms in the owl vocabulary are preceded by
% the prefix 'owl:' (rather than the full URI).
% the CL spec is a little unclear on 'network identifiers'.
% in future we can also translate the full URI.
cvt_pred(P,P2) :-
        atom_chars(P,[o,w,l,':',C|Rest]),
        downcase_atom(C,C2),
        atom_chars(P2,[C2|Rest]).



        
        
        

