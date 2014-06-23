:- module(check_meta_decls, []).

:- use_module(library(prolog_metainference)).
:- use_module(library(is_entry_point)).
:- use_module(library(location_utils)).

:- multifile
	prolog:message//1,
	audit:check/4.

prolog:message(acheck(meta_decls)) -->
    ['-----------------------------------',nl,
     'Missing Meta Predicate Declarations',nl,
     '-----------------------------------',nl,
     'The predicates below requires a missing meta_predicate declaration.', nl,
     'They have been automatically inferred. Although is not required, it', nl,
     'is recommented to add them by hand or to fix the predicate in order', nl,
     'to facilitate static analysis and refactoring.', nl, nl].

prolog:message(acheck(meta_decls, (Loc/M)-Specs)) -->
    Loc,
    ['(~w):'-M, nl],
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

cleanup_metainference :-
    retractall(prolog_metainference:inferred_meta_pred(_, _, _)).

% Hook to hide messages:
:- multifile hide_missing_meta_pred/1.

hide_missing_meta_pred(prolog:generated_predicate/1).

audit:check(meta_decls, MSpec, Pairs, OptionL) :-
    option_allchk(OptionL, _, FileChk),
    cleanup_metainference,
    prolog_walk_code([autoload(false)]),
    findall(information-((Loc/M)-Spec),
	    ( MSpec=M:Spec,
	      prolog_metainference:inferred_meta_pred(_, M, Spec),
	      %% Only exported predicates would require qualification
	      %% of meta-arguments -- EMM after JW talk
	      is_entry_point(M:Spec),
	      functor(Spec, F, A),
	      PI = M:F/A,
	      \+ hide_missing_meta_pred(PI),
	      property_from(PI, _, From),
	      ( from_to_file(From, File)
	      ->call(FileChk, File)
	      ; true
	      ),
	      from_location(From, Loc)), Pairs).
