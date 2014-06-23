:- module(check_auto, []).

:- use_module(audit_tools(audit)).

:- multifile user:term_expansion/2.

user:term_expansion(end_of_file, end_of_file) :-
	'$set_source_module'(M, M),
	audit:checkall(M:_).
