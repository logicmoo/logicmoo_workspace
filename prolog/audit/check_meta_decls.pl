:- module(check_meta_decls, []).

:- use_module(library(prolog_metainference), []).
:- use_module(library(is_entry_point)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(infer_meta_if_required)).
:- use_module(library(audit/audit)).

:- multifile
	prolog:message//1.

prolog:message(acheck(meta_decls)) -->
    ['-----------------------------------',nl,
     'Missing Meta Predicate Declarations',nl,
     '-----------------------------------',nl,
     'The predicates below require a missing meta_predicate declaration.', nl,
     'They have been automatically inferred. Although is not required, it', nl,
     'is recommended to add them by hand or to fix the predicate in order', nl,
     'to facilitate static analysis and refactoring.', nl, nl].

prolog:message(acheck(meta_decls, (Loc/M)-Specs)) -->
    Loc,
    ['(~w):'-M],
    meta_decls(Specs).

meta_decls([]) --> [].
meta_decls([H|T]) -->
    [ '\t:- meta_predicate ~q'-[H]],
    meta_decls2(T),
    ['.'].

meta_decls2([]) --> [].
meta_decls2([H|T]) -->
    [',', nl, '\t\t~q'-[H]],
    meta_decls2(T).

% cleanup_metainference :-
%     retractall(prolog_metainference:inferred_meta_pred(_, _, _)).

% Hook to hide messages:
:- multifile hide_missing_meta_pred/1.

hide_missing_meta_pred(prolog:generated_predicate/1).

audit:check(meta_decls, Pairs, OptionL0 ) :-
    option_allchk(OptionL0, OptionL1, FileChk),
    select_option(module(M), OptionL1, _, M),
    infer_meta_if_required,
    findall(information-((Loc/M)-Spec),
	    ( prolog_metainference:inferred_meta_pred(_, M, Spec),
	      %% Only exported predicates would require qualification
	      %% of meta-arguments -- EMM after JW talk
	      is_entry_point(Spec, M),
	      functor(Spec, F, A),
	      PI = M:F/A,
	      \+ hide_missing_meta_pred(PI),
	      once(property_from(PI, _, From)), % once: only first occurrence
	      from_chk(FileChk, From),
	      from_location(From, Loc)), Pairs).

