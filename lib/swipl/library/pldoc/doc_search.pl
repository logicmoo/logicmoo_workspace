/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2018, University of Amsterdam
                              CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(pldoc_search,
          [ search_form//1,             % +Options, //
            search_reply//2,            % +Search, +Options, //
            matching_object_table//2    % +Objects, +Options, //
          ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(dcg/basics)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(atom)).
:- use_module(library(porter_stem)).

:- use_module(doc_process).
:- use_module(doc_html).
:- use_module(doc_index).
:- use_module(doc_util).
:- use_module(doc_words).
:- use_module(man_index).

:- include(hooks).

/** <module> Search form and reply

@tbd    Advanced search field

                * Limit to a directory
                * Whole-word search
*/

:- predicate_options(search_form//1, 1,
                     [ for(atom),
                       search_in(oneof([all,noapp,app,man])),
                       search_match(oneof([name,summary])),
                       search_options(boolean)
                     ]).
:- predicate_options(search_reply//2, 2,
                     [ resultFormat(oneof([summary,long])),
                       search_in(oneof([all,noapp,app,man])),
                       search_match(oneof([name,summary])),
                       header(boolean),
                       private(boolean),
                       edit(boolean),
                       page(positive_integer),
                       per_page(positive_integer),
                       pass_to(pldoc_index:doc_links//2, 2)
                     ]).

%!  search_form(+Options)//
%
%   Create  a  search  input  field.  The   input  field  points  to
%   =|/search?for=String|= on the current server.  Options:
%
%           * title(Title)

search_form(Options) -->
    { (   option(for(Value), Options)
      ->  Extra = [value(Value)]
      ;   Extra = []
      ),
      option(search_in(In), Options, all),
      option(search_match(Match), Options, summary)
    },
    html(form([ id('search-form'),
                action(location_by_id(pldoc_search))
              ],
              [ div([ \search_field([ name(for),
                                      id(for)
                                    | Extra
                                    ])
                    ]),
                \search_options(In, Match, Options)
              ])).

search_options(In, Match, Options) -->
    { option(search_options(false), Options) },
    !,
    hidden(in, In),
    hidden(match, Match).
search_options(In, Match, _Options) -->
    html(div(class('search-options'),
             [ span(class('search-in'),
                    [ \radio(in, all, 'All', In),
                      \radio(in, app, 'Application', In),
                      \radio(in, man, 'Manual', In)
                    ]),
               span(class('search-match'),
                    [ \radio(match, name, 'Name', Match),
                      \radio(match, summary, 'Summary', Match)
                    ]),
               span(class('search-help'),
                    [ a(href(location_by_id(pldoc_package)+'pldoc.html#sec:browser'),
                        'Help')
                    ])
             ])).


%!  search_field(+Options)// is det.
%
%   Hookable predicate to display the   search field. Hookability is
%   provided  to  experiment  with    auto-completion  outside  this
%   package.

search_field(Options) -->
    prolog:doc_search_field(Options),
    !.
search_field(Options) -->
    html([ input(Options, []),
           input([ id('submit-for'),
                   type(submit),
                   value('Search')
                 ])
         ]).

radio(Radio, Field, Label, In) -->
    {   Field == In
    ->  Extra = [checked]
    ;   Extra = []
    },
    html([ input([ type(radio),
                   name(Radio),
                   value(Field)
                 | Extra
                 ]),
           Label
         ]).

hidden(Name, Value) -->
    html(input([type(hidden), name(Name), value(Value)])).

%!  search_reply(+For, +Options)// is det.
%
%   Generate a reply searching for For.  Options include
%
%           * resultFormat(Format)
%           If =summary= (default), produce a summary-table.  If
%           =long=, produce full object descriptions.
%
%           * search_in(In)
%           Determine which databases to search.  One of
%           =all=, =app=, =man=
%
%           * private(Boolean)
%           If `false` (default `true`), hide private predicates
%           from results.
%
%           * search_match(Match)
%           What part of the object to match. One of =name=,
%           =summary=
%
%           * header(+Boolean)
%           If =false=, suppress the header.
%
%           * per_page(+positive_integer)
%           Number of results per page (default 25).
%
%           * page(+positive_integer)
%           Page number to show results for (default 1).

:- html_meta
    search_header(+, html, +, ?, ?).

search_reply(For, Options) -->
    { var(For) ; For == '' },
    !,
    search_header('', 'Using PlDoc search', Options),
    html([ ul( class('search-help'),
               [ li([ 'If you pause typing, the search box will display ',
                      'an auto completion list.  Selecting an object jumps ',
                      'immediately to the corresponding documentation.'
                    ]),
                 li([ 'Searching for ', i('Name/Arity'), ', ',
                      i('Name//Arity'), ', ', i('Name'), ' or ',
                      i('C-function()'), ' ensures that ',
                      'matching definitions appear first in the search ',
                      'results'
                    ]),
                 li([ 'Other searches search through the name and summary ',
                      'descriptions in the manual.'
                    ])
               ])
         ]).
search_reply(For, Options) -->
    { cached_search(For, PerCategory, Time, Options),
      PerCategory \== [],
      page_location(PerCategory, NPages, Offset, Limit, Options),
      option(resultFormat(Format), Options, summary)
    },
    !,
    search_header(For, [ 'Search results for ',
                         span(class(for), ['"', For, '"'])
                       ],
                  Options),
    { DisplayOptions = [ for(For),
                         cputime(Time),
                         page_count(NPages)
                       | Options
                       ]
    },
    indexed_matches(Format, PerCategory, Offset, Limit, DisplayOptions),
    search_pagination(DisplayOptions).
search_reply(For, Options) -->
    search_header(For, 'No matches', Options),
    html(div(class('search-no-matches'), 'No matches')).

:- dynamic
    cached_search_result/4.

cached_search(For, Result, Time, Options) :-
    option(search_in(In), Options, all),
    option(search_match(Match), Options, summary),
    cached_search_result(For, In, Match, Result),
    !,
    Time = cached.
cached_search(For, Result, Time, Options) :-
    option(search_in(In), Options, all),
    option(search_match(Match), Options, summary),
    statistics(cputime, T0),
    search_doc(For, PerCategory0, Options),
    order_matches(PerCategory0, Result),
    statistics(cputime, T1),
    Time is T1-T0,
    assertz(cached_search_result(For, In, Match, Result)),
    prune_search_cache.

prune_search_cache :-
    (   predicate_property(cached_search_result(_,_,_,_),
                           number_of_clauses(Count)),
        Del is Count - 25,
        Del > 0
    ->  forall(between(1,Del,_), retract(cached_search_result(_,_,_,_)))
    ;   true
    ).

page_location(PerCategory, NPages, Offset, Limit, Options) :-
    option(page(Page), Options, 1),
    option(per_page(Limit), Options, 25),
    Offset is (Page-1)*Limit,
    count_matches(PerCategory, Total),
    NPages is (Total+Limit-1)//Limit.

search_pagination(Options) -->
    { option(page(Page), Options, 1),
      option(page_count(NPages), Options, 1)
    },
    html(div(class(pagination),
             [ \search_prev(Page, Options),
               span(class(current), ['Page ', Page, ' of ', NPages]),
               \search_next(NPages, Page, Options)
             ])).

search_prev(Page, _) -->
    { Page =:= 1 },
    !.
search_prev(Page, Options) -->
    { Prev is Page - 1,
      page_link(Prev, Link, Options)
    },
    html(a(href(Link), '< Prev')).

search_next(NPages, Page, _) -->
    { Page =:= NPages, ! }, [].
search_next(_NPages, Page, Options) -->
    { Next is Page + 1,
      page_link(Next, Link, Options)
    },
    html(a(href(Link), 'Next >')).

page_link(Page, '?'+QueryString, Options) :-
    option(for(For), Options),
    option(search_in(In), Options, all),
    option(search_match(Match), Options, summary),
    option(resultFormat(Format), Options, summary),
    uri_query_components(QueryString,
                         [ for(For),
                           in(In),
                           match(Match),
                           resultFormat(Format),
                           page(Page)
                         ]).

search_header(_For, _Title, Options) -->
    { option(header(false), Options) },
    !,
    html_requires(pldoc).
search_header(For, Title, Options) -->
    html_requires(pldoc),
    doc_links('', [for(For)|Options]),
    html(h1(class('search-results'), Title)).

%!  matching_object_table(+Objects, +Options)// is det.
%
%   Show a list of matching objects,   similar  to a result-set from
%   search.

matching_object_table(Objects, Options) -->
    { maplist(obj_cat_sec, Objects, Pairs),
      group_hits(Pairs, Organized),
      option(format(Format), Options, summary)
    },
    indexed_matches(Format, Organized, Options).

obj_cat_sec(Object, Cat-(Section-Object)) :-
    prolog:doc_object_summary(Object, Cat, Section, _Summary).

indexed_matches(Format, PerCategory, Offset, Limit, Options) -->
    { cat_offset(Offset, _,PerCategory, PerCategory1),
      cat_limit(Limit, _, PerCategory1, PerCategory2)
    },
    (   { PerCategory2 == PerCategory }
    ->  indexed_matches(Format, PerCategory, Options)
    ;   category_counts(PerCategory,
                        [ showing('Total'),
                          link(category)
                        | Options
                        ]),
        { delete(Options, cputime(_), Options1) },
        category_counts(PerCategory2,
                        [ showing('Showing'),
                          class([showing])
                        | Options1
                        ]),
        search_pagination(Options),
        matches(Format, PerCategory2, Options)
    ).

%!  cat_offset(+Offset, -Remaining, +PerCat0, -PerCat) is det.
%!  cat_limit(+Limit, -Remaining, +PerCat0, -PerCat) is det.

cat_offset(0, 0, PerCat, PerCat) :-
    !.
cat_offset(N, R, [C-H|T], PerCat) :-
    H = [_-[_|_]|_],
    !,
    cat_offset(N, R1, H, H1),
    (   H1 == []
    ->  !, cat_offset(R1, R, T, PerCat)
    ;   PerCat = [C-H1|T]
    ).
cat_offset(N, R, [_C-L|T0], PerCat) :-
    length(L, Len),
    Left is N-Len,
    Left > 0,
    !,
    cat_offset(Left, R, T0, PerCat).
cat_offset(N, 0, [C-L0|T], [C-L|T]) :-
    !,
    length(Skip, N),
    append(Skip, L, L0).
cat_offset(N, N, Obj, Obj).

cat_limit(0, 0, _PerCat, []) :-
    !.
cat_limit(N, R, [C-H|T], PerCat) :-
    H = [_-[_|_]|_],
    !,
    cat_limit(N, R1, H, H1),
    (   R1 == 0
    ->  PerCat = [C-H1]
    ;   PerCat = [C-H|T1],
        cat_limit(R1, R, T, T1)
    ).
cat_limit(N, R, [C-L|T0], [C-L|T]) :-
    length(L, Len),
    More is N - Len,
    More >= 0,
    !,
    cat_limit(More, R, T0, T).
cat_limit(N, 0, [C-L0|_], [C-L]) :-
    !,
    length(L, N),
    append(L, _, L0).
cat_limit(N, N, [], []).


%!  order_matches(+PerCat, -Ordered) is det.
%
%   Order matches per category. Each low level   object  is of the shape
%   q(Q, Object), where Q is a number  between   0  and  1, 1 implying a
%   perfect fit.

order_matches(PerCat0, PerCat) :-
    maplist(order_category, PerCat0, PerCat).

order_category(Cat-PerSection0, Cat-PerSection) :-
    maplist(order_section, PerSection0, PerSectionTagged),
    sort(1, >=, PerSectionTagged, Ordered),
    pairs_values(Ordered, PerSection).

order_section(Section-Objects0, Q-(Section-Objects)) :-
    sort(1, >=, Objects0, Objects),
    maplist(arg(1), Objects, QList),
    join_quality(QList, Q).

join_quality([], 0).
join_quality([Q], Q).
join_quality([QH|QL], Q) :-
    join_quality(QL, QT),
    Q is 1-(1-QH)*(1-QT).


%!  indexed_matches(+Format, +PerCategory, +Options)//
%
%   Emit the matches.

indexed_matches(Format, PerCategory, Options) -->
    category_counts(PerCategory, Options),
    matches(Format, PerCategory, Options).

category_counts(PerCategory, Options) -->
    { count_matches(PerCategory, Matches),
      option(class(Classes), Options, []),
      (   PerCategory = [_]
      ->  merge_options([link(false)], Options, Options1)
      ;   Options1 = Options
      )
    },
    html([ div(class(['search-counts'|Classes]),
               [ \category_showing(Options1),
                 Matches,
                 \count_by_category(PerCategory, Options1),
                 \search_time(Options1)
               ])
         ]).

count_by_category([Cat-_PerFile], Options) -->
    !,
    html(' matches from '),
    category_link(Cat, Options).
count_by_category(PerCategory, Options) -->
    html(' matches; '),
    count_by_category_list(PerCategory, Options).

count_by_category_list([], _) -->
    [].
count_by_category_list([Cat-PerFile|T], Options) -->
    { count_category(PerFile, Count) },
    html([ \category_link(Cat, Options), ': ', Count ]),
    (   {T == []}
    ->  []
    ;   html(', '),
        count_by_category_list(T, Options)
    ).

count_matches([], 0).
count_matches([_-Cat|T], Count) :-
    count_matches(T, Count0),
    count_category(Cat, N),
    Count is Count0 + N.

count_category([], 0).
count_category([_-Objs|T], Count) :-
    count_category(T, Count0),
    length(Objs, N),
    Count is Count0 + N.

category_showing(Options) -->
    { option(showing(Showing), Options) },
    html(span(class('search-showing'), [Showing, :])).
category_showing(_) -->
    [].

search_time(Options) -->
    { option(cputime(Time), Options) },
    !,
    (   { number(Time) }
    ->  html(span(class('search-time'), '(~2f sec.)'-[Time]))
    ;   html(span(class('search-time'), '(~w)'-[Time]))
    ).
search_time(_) -->
    [].

%!  matches(+Format, +PerCategory, +Options)// is det
%
%   Display search matches according to Format.
%
%   @param PerCategory List of File-Objects

matches(long, PerCategory, Options) -->
    long_matches_by_type(PerCategory, Options).
matches(summary, PerCategory, Options) -->
    html(table(class(summary),
               \short_matches_by_type(PerCategory, 1, Options))).


long_matches_by_type([], _) -->
    [].
long_matches_by_type([Category-PerFile|T], Options) -->
    category_header(Category, Options),
    long_matches(PerFile, Options),
    long_matches_by_type(T, Options).


long_matches([], _) -->
    [].
long_matches([File-Objs|T], Options) -->
    file_header(File, Options),
    objects(Objs, Options),
    long_matches(T, Options).

category_header(Category, _Options) -->
    html(h1(class(category), \category_title(Category))).

short_matches_by_type([], _, _) -->
    [].
short_matches_by_type([Category-PerFile|T], Nth, Options) -->
    category_index_header(Category, Nth, Options),
    short_matches(PerFile, Options),
    { succ(Nth, Nth1) },
    short_matches_by_type(T, Nth1, Options).

short_matches([], _) -->
    [].
short_matches([File-Objs|T], Options) -->
    file_index_header(File, Options),
    object_summaries(Objs, File, Options),
    short_matches(T, Options).


category_index_header(Category, Nth, _Options) -->
    (   { Nth > 1 }
    ->  category_sep('category-top-sep')
    ;   []
    ),
    html(tr(th([class(category), colspan(3)],
               a(name(Category), \category_title(Category))))),
    category_sep('category-bottom-sep').

category_sep(Which) -->
    html(tr(th([class(Which), colspan(3)],
               &(nbsp)))).

category_link(Category, Options) -->
    { option(link(false), Options) },
    !,
    category_title(Category).
category_link(Category, Options) -->
    { option(link(category), Options),
      category_link(Category, HREF, Options)
    },
    !,
    html(a(href(HREF), \category_title(Category))).
category_link(Category, _Options) -->
    { atom_concat(#, Category, HREF) },
    html(a(href(HREF), \category_title(Category))).

category_link(Category, '?'+QueryString, Options) :-
    (   category_abbreviation(Category, Abbrev)
    ->  true
    ;   Abbrev = Category
    ),
    option(for(For), Options),
    option(search_match(Match), Options, summary),
    option(resultFormat(Format), Options, summary),
    uri_query_components(QueryString,
                         [ for(For),
                           in(Abbrev),
                           match(Match),
                           resultFormat(Format)
                         ]).

category_title(Category) -->
    {   prolog:doc_category(Category, _Order, Title)
    ->  true
    ;   Title = Category
    },
    html(Title).

%!  search_doc(+SearchString, -PerType:list, +Options) is det.
%
%   Return matches of SearchString as Type-PerFile tuples, where PerFile
%   is a list File-ListOfObjects.

search_doc(Search, PerType, Options) :-
    findall(Tuples, matching_object(Search, Tuples, Options), Tuples0),
    sort(Tuples0, Tuples),
    group_hits(Tuples, PerType0),
    prune_library(PerType0, PerType).

group_hits(Tuples, PerType) :-
    keysort(Tuples, Tuples1),
    group_pairs_by_key(Tuples1, PerCat0),
    key_sort_order(PerCat0, PerCat1),
    keysort(PerCat1, PerCat2),
    pairs_values(PerCat2, PerCat),
    group_by_file(PerCat, PerType).

key_sort_order([], []).
key_sort_order([Cat-ByCat|T0], [Order-(Cat-ByCat)|T]) :-
    (   prolog:doc_category(Cat, Order, _Title)
    ->  true
    ;   Order = 99
    ),
    key_sort_order(T0, T).


group_by_file([], []).
group_by_file([Type-Tuples0|T0], [Type-ByFile|T]) :-
    keysort(Tuples0, Tuples),
    group_pairs_by_key(Tuples, ByFile),
    group_by_file(T0, T).


%!  prune_library(+PerCat0, -PerCat) is det.
%
%   Prune objects from the libary that also appear in the manual.

prune_library(PerCat0, PerCat) :-
    selectchk(library-InLib0, PerCat0, library-InLib, PerCat1),
    !,
    (   cat_objects(manual, PerCat0, Manual),
        cat_objects(packages, PerCat0, Packages),
        append(Manual, Packages, Objects),
        sort(Objects, OSet0),
        maplist(arg(2), OSet0, OSet),       % get rid of q(Q,Obj)
        convlist(prune_section(OSet), InLib0, InLib),
        InLib \== []
    ->  PerCat = PerCat1
    ;   selectchk(library-_, PerCat0, PerCat)
    ).
prune_library(PerCat, PerCat).

cat_objects(Cat, PerCat, Objects) :-
    memberchk(Cat-Sections, PerCat),
    !,
    pairs_values(Sections, NestedObjects),
    append(NestedObjects, Objects).
cat_objects(_, _, []).

prune_section(Prune, Section-Objects0, Section-Objects) :-
    exclude(in_set(Prune), Objects0, Objects),
    Objects \== [].                     % specific library becomes empty

in_set(Prune, q(_Q,Obj)) :-
    memberchk(Obj, Prune),
    !.
in_set(Prune, q(_Q,_Module:Obj)) :-
    memberchk(Obj, Prune).


%!  matching_object(+SearchString, -Object, +Options) is nondet.
%
%   Object matches SearchString.  Options include
%
%     - search_in(In)
%       One of `all`, `app`, `man`.
%
%     - search_match(Match)
%       One of `name`, `summary`
%
%   @param Object   Term of the form File-Item
%   @tbd Deal with search syntax

matching_object(Search, Type-(Section-q(1,Obj)), Options) :-
    atom_concat(Function, '()', Search),
    Obj = c(Function),
    option(search_in(In), Options, all),
    prolog:doc_object_summary(Obj, Type, Section, _),
    matching_category(In, Type).
matching_object(Search, Type-(Section-q(1,Obj)), Options) :-
    (   atom_pi(Search, Obj0)
    ->  ground(Obj0)
    ;   catch(atom_to_term(Search, Obj0, _), _, fail),
        nonvar(Obj0)
    ),
    opt_qualify(Obj0, Obj),
    option(search_in(In), Options, all),
    prolog:doc_object_summary(Obj, Type, Section, _),
    matching_category(In, Type).
matching_object(Search, Match, Options) :-
    atom_codes(Search, Codes),
    phrase(search_spec(For0), Codes),
    (   For0 = not(_)
    ->  throw(error(bad_search(only_not), _))
    ;   optimise_search(For0, For),
        exec_search(For, Match, Options)
    ).

opt_qualify(Obj0, Obj) :-
    Obj0 = _:_,
    !,
    Obj = Obj0.
opt_qualify(Obj, Obj).
opt_qualify(Obj, _:Obj).


%!  optimise_search(+Spec, -Optimised)
%
%   Optimise a search specification. Currently   only deals with the
%   simple case of  first  searching  for   a  negation  and  then a
%   positive term.

optimise_search(and(not(A0), B0), and(B, not(A))) :-
    !,
    optimise_search(A0, A),
    optimise_search(B0, B).
optimise_search(A, A).


%!  exec_search(+Spec, -Match, +Options) is nondet.
%
%   Spec is one of
%
%     - and(Spec, Spec)
%       Intersection of the specification
%     - not(Spec)
%       Negation of the specification
%     - quoted(Tokens)
%       A quoted list of tokens.

exec_search(Spec, Match, Options) :-
    exec_search(Spec, Match0, Q, Options),
    add_quality(Match0, Q, Match).

add_quality(Type-(Section-Obj), Q, Type-(Section-q(Q,Obj))).

exec_search(and(A, B), Match, Q, Options) :-
    !,
    exec_search(A, Match, Q1, Options),
    exec_search(B, Match, Q2, Options),
    Q is 1-((1-Q1)*(1-Q2)).
exec_search(Search, Type-(Section-Obj), Q, Options) :-
    option(search_in(In), Options, all),
    option(search_match(Match), Options, summary),
    option(private(Public), Options, true),
    prolog:doc_object_summary(Obj, Type, Section, Summary),
    matching_category(In, Type),
    match_private(Public, Obj),
    (   Search = not(For)
    ->  State = s(0),
        \+ ( match_object(For, Obj, Summary, Match, Q),
             nb_setarg(1, State, Q)
           ),
        arg(1, State, Q)
    ;   match_object(Search, Obj, Summary, Match, Q)
    ).


matching_category(all, _).
matching_category(noapp, Category) :-
    !,
    Category \== application.
matching_category(Category, Category).
matching_category(Abbrev, Category) :-
    category_abbreviation(Category, Abbrev).

category_abbreviation(application, app).
category_abbreviation(manual,      man).
category_abbreviation(library,     lib).
category_abbreviation(packages,    pack).
category_abbreviation(wiki,        wiki).

match_private(true, _).
match_private(false, Object) :-
    (   Object = (Module:PI)
    ->  current_module(Module),
        pi_head(PI, Head),
        (   predicate_property(Module:Head, exported)
        ->  true
        ;   predicate_property(Module:Head, multifile)
        ->  true
        ;   predicate_property(Module:Head, public)
        )
    ;   true
    ).

pi_head(Name/Arity, Head) :-
    functor(Head, Name, Arity).
pi_head(Name//Arity, Head) :-
    Arity1 is Arity+ 2,
    functor(Head, Name, Arity1).

%!  search_spec(-Search)// is det.
%
%   Break a search string from the user into a logical expression.

search_spec(Spec) -->
    blanks,
    prim_search_spec(A),
    blanks,
    (   eos
    ->  { Spec = A }
    ;   search_spec(B)
    ->  { Spec = and(A,B) }
    ).

prim_search_spec(quoted(Quoted)) -->
    "\"", string(Codes), "\"",
    !,
    { tokenize_atom(Codes, Quoted)
    }.
prim_search_spec(Spec) -->
    nonblanks(Codes),
    {   Codes = [0'-,C0|Rest],
        code_type(C0, csym)
    ->  atom_codes(Word, [C0|Rest]),
        Spec = not(Word)
    ;   Codes \== [],
        atom_codes(Spec, Codes)
    }.


%!  object_summary(?Object, ?Category, ?Section, ?Summary) is nondet.
%
%   True  if  Object  is  summarised   by  Summary.  This  multifile
%   predicate can be extended  with   other  search  mechanisms. The
%   returned objects must be  handled   by  object_summaries//2  and
%   objects//2.
%
%   @param Category Atom describing the source.
%   @param Section  Reference to the context of Object.

prolog:doc_object_summary(Obj, Category, File, Summary) :-
    once(prolog_object(Obj)),
    current_prolog_flag(home, SWI),
    doc_comment(Obj0, File:_Line, Summary, _Comment),
    (   is_list(Obj0)
    ->  member(Obj, Obj0)
    ;   Obj = Obj0
    ),
    Obj \= _:module(_Title),                % HACK.  See ref_object//1
    (   sub_atom(File, 0, _, _, SWI)
    ->  Category = library
    ;   Category = application
    ).

prolog_object(Var) :- var(Var), !.
prolog_object(_/_).
prolog_object(_//_).
prolog_object(_:_/_).
prolog_object(_:_//_).
prolog_object(module(_)).


%!  doc_category(Name, SortOrder, Description) is nondet.
%
%   Describe the various  categories  of   search  results.  Used to
%   create the category headers  as  well   as  the  advanced search
%   dialog.
%
%   @param SortOrder        Ranges 0..100.  Lower values come first

prolog:doc_category(application, 20, 'Application').
prolog:doc_category(library,     80, 'System Libraries').


                 /*******************************
                 *             UTIL             *
                 *******************************/

%!  match_object(+For, +Object, +Summary, +How, -Quality) is semidet.
%
%   True when Object with summary text Summary matches For acording to
%   How.
%
%   @arg For is either a token (atom) or a term quoted(Tokens), where
%   `Tokens` is a list of atoms.
%   @arg How is one of `name` or `summary`
%   @arg Quality is a number in the range 0..1, where 1 means a strong
%   match.

match_object(For, Object, Summary, How, Quality) :-
    (   doc_object_identifier(Object, Identitier),
        identifier_match_quality(For, Identitier, Quality)
    ->  debug(search(rank), 'Rank "~w" in identifier "~w": ~q',
              [For, Identitier, Quality])
    ;   How == summary,
        summary_match_quality(For, Summary, Quality),
        debug(search(rank), 'Rank "~w" in summary "~w": ~q',
              [For, Summary, Quality])
    ).

summary_match_quality(For, Summary, Q) :-
    tokenize_atom(Summary, Tokens0),
    exclude(is_punctuation, Tokens0, Tokens),
    Tokens \== [],
    token_match_quality(summary, For, Tokens, Q0),
    Q is Q0/2.

is_punctuation(Token) :-
    atom_length(Token, 1),
    char_type(Token, punct).


identifier_match_quality(Identifier, Identifier, 1) :-
    !.
identifier_match_quality(For, Identifier, Q) :-
    dwim_match(For, Identifier, _),
    !,
    Q = 0.8.
identifier_match_quality(For, Identifier, Q) :-
    identifier_parts(Identifier, Parts),
    Parts \== [],
    token_match_quality(identifier, For, Parts, Q).

token_match_quality(_How, quoted(Tokens), Parts, Q) :-
    !,
    append(Tokens, _, All),
    append(_, All, Parts),
    Q = 1.
token_match_quality(How, For, Parts, Q) :-
    length(Parts, Len),
    (   memberchk(For, Parts)
    ->  Q0 = 1
    ;   porter_stem(For, Stem),
        member(Part, Parts),
        porter_stem(Part, Stem)
    ->  Q0 = 0.9
    ;   How == summary,
        member(Part, Parts),
        sub_atom_icasechk(Part, _, For),
        identifier_parts(Part, SubParts),
        token_match_quality(identifier, For, SubParts, Q00)
    ->  Q0 is Q00/2
    ;   How == summary,
        member(Part, Parts),
        sub_atom_icasechk(Part, 0, For),
        is_numbered_var(Part, For)
    ->  Q0 is 0.9
    ;   doc_related_word(For, Word, Distance),
        memberchk(Word, Parts)
    ->  Q0 = Distance
    ),
    Q is Q0/Len.

is_numbered_var(VarName, Search) :-
    atom_length(Search, Len),
    sub_string(VarName, Len, _, 0, NS),
    number_string(_, NS),
    sub_atom(VarName, 0, 1, _, First),
    char_type(First, prolog_var_start).
