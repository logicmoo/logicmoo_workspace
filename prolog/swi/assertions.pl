:- module(assertions, []).

:- use_module(library(compound_expand)).
:- reexport(library(assertions_op)).
:- reexport(library(assertions/assrt_lib)).

:- create_prolog_flag(assrt_meta_pred, check, [type(atom)]).

:- multifile user:file_search_path/2.

user:file_search_path(assertions, library(assertions)).

% assrt_lib:nodirective_error_hook(Assr) :-
%     throw(error(context_error(nodirective, Assr), _)).
term_expansion((:- meta_predicate Assr), MPos, [(:- meta_predicate Assr)|Records],
	       [MPos|RPos]) :- !,
    current_prolog_flag(assrt_meta_pred, Status),
    MPos = term_position(DF, DT, FF, FT,
			 [term_position(_,  _,  GF, GT, [APos])]),
    % Note: MPos reassembled in the way assertion_records expect
    assertion_records(Status pred Assr,
		      term_position(DF, DT, GF, GT, [FF-FT, APos]), Records, RPos).
term_expansion((:- Decl), term_position(_, _, _, _, [DPos]), Records, RPos) :-
    assertion_records(Decl, DPos, Records, RPos).
