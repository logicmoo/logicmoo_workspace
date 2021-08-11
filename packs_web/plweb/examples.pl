/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (C): 2020, SWI-Prolog Solutions b.v.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(examples,
          [ ex_xref/3,                  % Id,Code,XRef
            index_examples/0,
            examples//2,
            reindex_examples/0
          ]).
:- use_module(library(http/html_write)).
:- use_module(library(filesex)).
:- use_module(library(dcg/high_order)).
:- use_module(library(http/html_head)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(occurs)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(prolog_code)).
:- use_module(library(solution_sequences)).
:- use_module(library(git)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(option)).
:- use_module(library(http/http_json)).
:- use_module(library(dcg/basics)).

:- use_module(wiki).
:- use_module(messages).

user:file_search_path(examples, examples).

:- html_resource(pldoc_examples,
		 [ ordered(true),
                   requires([ jquery,
                              js('examples.js')
			    ]),
		   virtual(true)
		 ]).
:- html_resource(css('examples.css'), []).

:- multifile
    prolog:doc_object_footer//2.

prolog:doc_object_footer(Objs, Options) -->
    examples(Objs, Options).

%!  examples(+Objs, +Options)
%
%   Include examples for the predicate PI.

examples(Objs, _Options) -->
    { index_examples,
      findall(Ex-How, (member(Obj,Objs),example(Obj, Ex, How)), Refs0),
      Refs0 \== [],
      !,
      keysort(Refs0, Refs),
      group_pairs_by_key(Refs, Grouped0),
      map_list_to_pairs(ex_score, Grouped0, Scored),
      sort(1, >=, Scored, Sorted),
      pairs_values(Sorted, Grouped)
    },
    html_requires(pldoc_examples),
    html_requires(css('examples.css')),
    html(div(class('ex-list'),
             [ h4('Examples')
             | \ex_list(Grouped)
             ])).
examples(_,_) -->
    [].

ex_list([One]) -->
    { One = _File-How,
      memberchk(file, How)
    },
    !,
    ex_html(['ex-current'], One).
ex_list(ExList) -->
    !,
    sequence(ex_html([]), ExList).

ex_html(More, File-How) -->
    { best_flag(How, Flag),
      (   Flag == file
      ->  Classes = ['ex-current'|More]
      ;   Classes = More
      )
    },
    html(div(class([ex|Classes]),
             [ div(class('ex-header'),
                   [ \ex_flag(Flag),
                     \ex_title(File, How),
                     \ex_authors(File)
                   ]),
               div(class('ex-content'),
                   \ex_content(File))
             ])).

ex_title(File, _) -->
    { ex_prop(File, title, Title) }, !,
    html(span(class(title), Title)).
ex_title(File, _) -->
    { file_title(File, Title)
    },
    !,
    html(span(class(title), Title)).
ex_title(_, _) -->
    [].

ex_authors(File) -->
    { ex_prop(File, author, Authors) }, !,
    sequence(ex_author, ", ", Authors).
ex_authors(_) -->
    [].

ex_author(Author) -->
    html(span(class(author), Author)).

ex_flag(Flag) -->
    { label(Flag, Title) },
    html(span([ class(['ex-flag', Flag]),
                title(Title)
              ], '')).

ex_content(File) -->
    { ex_file_dom(File, DOM) },
    html(DOM).

%!  example(+PI, -File, -RefType) is nondet.
%
%   Get an example.

example(PI, File, How) :-
    example2(PI, File, How0),
    (   How = How0
    ;   PI = Name/Arity,
        file_base_name(File, Base),
        (   Name == Base
        ->  How = file
        ;   atom_concat(Name, Arity, Base)
        ->  How = file
        )
    ).

example2(PI, File, query) :-
    ex_code(File, _, _, XRef),
    memberchk(PI, XRef.get(query)).
example2(PI, File, called) :-
    ex_code(File, _, _, XRef),
    memberchk(PI, XRef.get(called)).
example2(PI, File, reference) :-
    ex_prop(File, reference, PI).
example2(PI, File, titleref) :-
    ex_prop(File, titleref, PI).

ex_score(_File-Flags, Score) :-
    maplist(rank, Flags, Scores),
    sum_list(Scores, Score).

best_flag(Flags, Flag) :-
    map_list_to_pairs(rank, Flags, Ranked),
    sort(1, >, Ranked, [_Rank-Flag|_]).

rank(file,     1000).
rank(titleref,  100).
rank(query,      30).
rank(called,     20).
rank(reference,   5).

label(file,      'Example file for predicate').
label(titleref,  'Mentioned in the title').
label(query,     'Used in a query').
label(called,    'Called in example').
label(reference, 'Mentioned in comment').

file_title(File, Title) :-
    file_base_name(File, Base),
    atom_codes(Base, Codes),
    (   phrase((string(Name),integer(Arity)), Codes)
    ->  documented(Name/Arity),
        format(string(Title), 'Examples for ~s/~d', [Name, Arity])
    ;   documented(Base/A1),
        documented(Base/A2),
        A1 \== A2
    ->  format(string(Title), 'Examples for ~s/N',  [Base])
    ).

:- multifile
    prolog:doc_object_summary/4.

documented(PI) :-
    prolog:doc_object_summary(PI, _Category, _Section, _Summary).


		 /*******************************
		 *              DB		*
		 *******************************/

%!  ex_code(File, N, Size, XRef)

:- dynamic
    ex_code/4,
    ex_prop/3,
    ex_done/1,
    ex_checked/1.


		 /*******************************
		 *            INDEX		*
		 *******************************/

%!  index_examples is det.
%!  index_examples(+Backlog) is det.
%!  reindex_examples is det.
%
%   Update the example index.
%
%   @tbd We only have to reprocess modified or new examples.

index_examples :-
    index_examples(60).

index_examples(Backlog) :-
    index_up_to_data(Backlog), !.
index_examples(Backlog) :-
    with_mutex(index_examples, index_examples2(Backlog)).

index_examples2(Backlog) :-
    index_up_to_data(Backlog), !.
index_examples2(_) :-
    transaction(reindex_examples).

reindex_examples :-
    clean_examples,
    do_index_examples.

do_index_examples :-
    forall(ex_file(File),
           index_example(File)),
    get_time(Now),
    assertz(ex_done(Now)),
    assertz(ex_checked(Now)).

index_up_to_data(Backlog) :-
    ex_done(Indexed),
    retract(ex_checked(Last)),
    get_time(Now),
    asserta(ex_checked(Now)),
    Now-Last > Backlog,
    (   ex_directory(Dir),
        time_file(Dir, Modified),
        Modified > Indexed
    ->  !, fail
    ;   true
    ).

clean_examples :-
    retractall(ex_done(_)),
    retractall(ex_code(_,_,_,_)),
    retractall(ex_prop(_,_,_)).

index_example(File) :-
    ex_file_dom(File, DOM),
    index_code(File, DOM),
    (   dom_property(DOM, Prop, Value),
        assertz(ex_prop(File, Prop, Value)),
        fail
    ;   true
    ).

index_code(File, DOM) :-
    (   call_nth(( dom_code(DOM, Code, _Attrs),
                   code_xref(Code, XRef)
                 ), N),
        string_length(Code, Len),
        assertz(ex_code(File, N, Len, XRef)),
        fail
    ;   true
    ).

%!  ex_xref(Id, Code, XRef) is nondet.

ex_xref(File, Code, XRef) :-
    ex_file(File),
    ex_file_dom(File, DOM),
    dom_code(DOM, Code, _Attrs),
    code_xref(Code, XRef).

%!  ex_repo(-Dir) is nondet.
%
%   True when Dir is a toplevel example directory

ex_repo(Dir) :-
    absolute_file_name(examples(.), Dir,
                       [ file_type(directory),
                         access(read),
                         solutions(all)
                       ]).


%!  ex_file(-File) is nondet.
%
%   True when File is the name of an example file

ex_file(File) :-
    ex_repo(ExDir),
    directory_member(ExDir, Path,
                     [ recursive(true),
                       extensions([md]),
                       access(read)
                     ]),
    directory_file_path(ExDir, FileEx, Path),
    file_name_extension(File, md, FileEx).

ex_directory(Path) :-
    ex_repo(ExDir),
    (   Path = ExDir
    ;   directory_member(ExDir, Path,
                         [ recursive(true),
                           file_type(directory)
                         ])
    ).


%!  ex_file_dom(+File, -DOM) is det.

ex_file_dom(File, DOM) :-
    absolute_file_name(examples(File), Path,
                       [ access(read),
                         extensions([md])
                       ]),
    wiki_file_to_dom(Path, DOM).

%!  dom_code(+DOM, -Code, -Attrs) is nondet.
%
%

dom_code(DOM, Code, Attrs) :-
    sub_term(pre(Attrs, Code), DOM).

%!  dom_property(+DOM, ?Prop, -ValueDOM) is nondet.

dom_property(DOM, Attr, Val) :-
    (   sub_term(H, DOM),
        title(H, TitleDOM0)
    ->  clean_title(TitleDOM0, TitleDOM),
        (   Attr+Val = title+TitleDOM
        ;   dom_references(TitleDOM0, Refs),
            Attr = titleref,
            member(Val, Refs)
        )
    ).
dom_property(DOM, author, AuthorDOM) :-
    (   sub_term(\tag(author, AuthorDOM), DOM)
    ->  true
    ).
dom_property(DOM, reference, Ref) :-
    dom_references(DOM, Refs),
    member(Ref, Refs).

title(h1(_, TitleDOM), TitleDOM).
title(h1(   TitleDOM), TitleDOM).

clean_title(\predref(PI), \nopredref(PI)) :-
    !.
clean_title(T0, T) :-
    compound(T0),
    !,
    compound_name_arity(T0, Name, Arity),
    compound_name_arity(T, Name, Arity),
    clean_title(1, Arity, T0, T).
clean_title(T,T).

clean_title(I, Arity, T0, T) :-
    I =< Arity,
    !,
    I2 is I+1,
    arg(I, T0, A0),
    arg(I, T, A),
    clean_title(A0, A),
    clean_title(I2, Arity, T0, T).
clean_title(_, _, _, _).

dom_references(DOM, Refs) :-
    findall(Ref, dom_reference(DOM,Ref), Refs0),
    sort(Refs0, Refs).

dom_reference(DOM, Ref) :-
    sub_term(Sub, DOM),
    el_reference(Sub, Ref).

el_reference(\predref(PI), PI).
el_reference(\file(Text, _Path), Lib) :-
    Lib = library(_),
    catch(term_string(Lib, Text),
          error(_,_), fail).

%!  code_xref(+Code, -XRef) is det.
%
%   Cross-reference a code fragment

code_xref(Code, XRef) :-
    setup_call_cleanup(
        open_string(Code, In),
        read_terms(In, Terms),
        close(In)),
    xref_terms(Terms, XRef).

read_terms(In, Terms) :-
    stream_property(In, position(Pos0)),
    catch(read_term(In, Term, []), E, true),
    (   Term == end_of_file
    ->  Terms = []
    ;   var(E)
    ->  Terms = [Term|More],
        read_terms(In, More)
    ;   set_stream_position(In, Pos0),
        skip(In, 0'\n),
        read_terms(In, Terms)
    ).

		 /*******************************
		 *	        XREF		*
		 *******************************/

%%	xref_terms(+Terms, -XRef:dict) is det.
%
%	Cross-reference a list of terms, returning a dict that contains:
%
%	  - defined:OrdSetOfPI
%	  - called:OrdSetOfPI
%	  - required:OrdSetOfPI
%	  - error:SetOfErrorTerms
%
%	Note that XRef.required is XRef.called \ built-in \XRef.defined.

xref_terms(Terms, Result) :-
    phrase(xref_terms(Terms), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    maplist(value_to_set, Grouped, GroupedSets),
    dict_pairs(Result0, xref, GroupedSets),
    (   exclude(built_in, Result0.get(called), Called),
        ord_subtract(Called, Result0.get(defined), Required),
        Required \== []
    ->  Result = Result0.put(required, Required)
    ;   Result = Result0
    ).

value_to_set(error-List, error-Set) :- !,
    variant_set(List, Set).
value_to_set(Key-HeadList, Key-PISet) :-
    maplist(pi_head, PIList, HeadList),
    sort(PIList, PISet).

variant_set(List, Set) :-
    list_to_set(List, Set1),
    remove_variants(Set1, Set).

remove_variants([], []).
remove_variants([H|T0], [H|T]) :-
    skip_variants(T0, H, T1),
    remove_variants(T1, T).

skip_variants([H|T0], V, T) :-
    H =@= V, !,
    skip_variants(T0, V, T).
skip_variants(L, _, L).


xref_terms([]) --> [].
xref_terms([(?- Query), Answer|T]) --> {is_answer(Answer)}, !, xref_query(Query), xref_terms(T).
xref_terms([H|T]) --> xref_term(H), xref_terms(T).

xref_term(Var) -->
    { var(Var) }, !.
xref_term((Head :- Body)) --> !,
    xref_head(Head),
    xref_body(Body).
xref_term((Head --> Body)) --> !,
    xref_dcg_head(Head),
    xref_dcg_body(Body).
xref_term((:- Body)) --> !,
    xref_body(Body).
xref_term((?- Query)) --> !,
    xref_query(Query).
xref_term(Head) -->
    xref_head(Head).

xref_head(Term) --> { atom(Term) }, !, [defined-Term].
xref_head(Term) --> { compound(Term), !, generalize(Term,Gen) }, [defined-Gen].
xref_head(Term) --> [ error-type_error(callable, Term) ].

xref_query(Query) -->
    xref_body(Query, query).

xref_body(Body) -->
    xref_body(Body, called).

xref_body(Term, _) --> { var(Term) }, !.
xref_body(Term, Ctx) -->
    { predicate_property(user:Term, meta_predicate(Meta)), !,
      generalize(Term, Called),
      Term =.. [_|Args],
      Meta =.. [_|Specs]
    },
    [ Ctx-Called ],
    xref_meta(Specs, Args, Ctx).
xref_body(Term, Ctx) --> { atom(Term) }, !, [Ctx-Term].
xref_body(Term, Ctx) --> { compound(Term), !, generalize(Term,Gen) }, [Ctx-Gen].
xref_body(Term, _Ctx) --> [ error-type_error(callable, Term) ].

xref_meta([], [], _) --> [].
xref_meta([S|ST], [A|AT], Ctx) -->
    xref_meta1(S, A, Ctx),
    xref_meta(ST, AT, Ctx).

xref_meta1(0, A, Ctx) --> !,
    xref_body(A, Ctx).
xref_meta1(^, A0, Ctx) --> !,
    { strip_existential(A0, A) },
    xref_body(A, Ctx).
xref_meta1(N, A0, Ctx) -->
    { integer(N), N > 0, !,
      extend(A0, N, A)
    },
    xref_body(A, Ctx).
xref_meta1(_, _, _) --> [].


xref_dcg_head(Var) -->
    { var(Var) }, !,
    [ error-instantiation_error(Var) ].
xref_dcg_head((A,B)) -->
    { is_list(B) }, !,
    xref_dcg_head(A).
xref_dcg_head(Term) -->
    { atom(Term), !,
      functor(Head, Term, 2)
    },
    [ defined-Head ].
xref_dcg_head(Term) -->
    { compound(Term), !,
      compound_name_arity(Term, Name, Arity0),
      Arity is Arity0+2,
      compound_name_arity(Gen, Name, Arity)
    },
    [ defined-Gen ].
xref_dcg_head(Term) -->
    [ error-type_error(callable, Term) ].

xref_dcg_body(Body) -->
    { var(Body) }, !.
xref_dcg_body(Body) -->
    { dcg_control(Body, Called) }, !,
    xref_dcg_body_list(Called).
xref_dcg_body(Terminal) -->
    { is_list(Terminal) ; string(Terminal) }, !.
xref_dcg_body(Term) -->
    { atom(Term), !,
      functor(Head, Term, 2)
    },
    [ called-Head ].
xref_dcg_body(Term) -->
    { compound(Term), !,
      compound_name_arity(Term, Name, Arity0),
      Arity is Arity0+2,
      compound_name_arity(Gen, Name, Arity)
    },
    [ called-Gen ].
xref_dcg_body(Term) -->
    [ error-type_error(callable, Term) ].

dcg_control((A,B), [A,B]).
dcg_control((A;B), [A,B]).
dcg_control((A->B), [A,B]).
dcg_control((A*->B), [A,B]).
dcg_control(\+(A), [A]).

xref_dcg_body_list([]) --> [].
xref_dcg_body_list([H|T]) --> xref_dcg_body(H), xref_dcg_body_list(T).

strip_existential(T0, T) :-
    (   var(T0)
    ->  T = T0
    ;   T0 = _^T1
    ->  strip_existential(T1, T)
    ;   T = T0
    ).

extend(T0, N, T) :-
    atom(T0), !,
    length(Args, N),
    T =.. [T0|Args].
extend(T0, N, T) :-
    compound(T0),
    compound_name_arguments(T0, Name, Args0),
    length(Extra, N),
    append(Args0, Extra, Args),
    compound_name_arguments(T, Name, Args).

generalize(Compound, Gen) :-
    compound_name_arity(Compound, Name, Arity),
    compound_name_arity(Gen, Name, Arity).

built_in(PI) :-
    pi_head(PI, Head),
    predicate_property(Head, built_in).

is_answer(Answer) :-
    var(Answer),
    !,
    fail.
is_answer((A;B)) :-
    !,
    is_1answer(A),
    is_answer(B).
is_answer(A) :-
    is_1answer(A).

is_1answer(X) :- var(X), !, fail.
is_1answer(true) :- !.
is_1answer(false) :- !.
is_1answer((A,B)) :-
    !,
    is_binding_or_constraint(A),
    is_1answer(B).
is_1answer(A) :-
    is_binding_or_constraint(A).

is_binding_or_constraint(Var) :-
    var(Var), !,
    fail.
is_binding_or_constraint(Var = _) :-
    !,
    var(Var).                           % often shares with query
is_binding_or_constraint(:-_) :- !, fail.
is_binding_or_constraint(?-_) :- !, fail.
is_binding_or_constraint(_).            % how to find out?


		 /*******************************
		 *            UPDATE		*
		 *******************************/

%!  pull_examples
%
%   Do a git pull on the examples and update the index.

pull_examples :-
    (   ex_repo(ExDir),
        is_git_directory(ExDir),
        git([pull], [directory(ExDir)]),
        fail
    ;   true
    ),
    index_examples(1).


		 /*******************************
		 *             HTTP		*
		 *******************************/

:- http_handler(root(examples/pull), pull_examples, []).

pull_examples(Request) :-
    (   option(method(post), Request)
    ->  http_read_json(Request, JSON),
        print_message(informational, got(JSON))
    ;   true
    ),
    call_showing_messages(pull_examples, []).
