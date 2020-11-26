/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2019, University of Amsterdam
                              VU University Amsterdam
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

:- module(pldoc_wiki,
          [ wiki_codes_to_dom/3,        % +Codes, +Args, -DOM
            wiki_lines_to_dom/3,        % +Lines, +Map, -DOM
            section_comment_header/3,   % +Lines, -Header, -RestLines
            summary_from_lines/2,       % +Lines, -Codes
            indented_lines/3,           % +Text, +PrefixChars, -Lines
            strip_leading_par/2,        % +DOM0, -DOM
            autolink_extension/2,       % ?Extension, ?Type
            autolink_file/2             % +FileName, -Type
          ]).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).

:- use_module(doc_util).


/** <module> PlDoc wiki parser

This file defines the PlDoc wiki parser,  which parses both comments and
wiki text files. The original version of this SWI-Prolog wiki format was
largely modeled after Twiki (http://twiki.org/).  The current version is
extended to take many aspects from   markdown, in particular the doxygen
refinement thereof.

@see http://www.stack.nl/~dimitri/doxygen/manual/markdown.html
*/

:- multifile
    prolog:doc_wiki_face//2,        % -Out, +VarNames
    prolog:doc_url_expansion/3,     % +Alias(Rest), -HREF, -Label
    prolog:url_expansion_hook/3,    % +Term, -Ref, -Label
    prolog:doc_autolink_extension/2.% +Extension, -Type


                 /*******************************
                 *          WIKI PARSING        *
                 *******************************/

%!  wiki_lines_to_dom(+Lines:lines, +Args:list(atom), -Term) is det
%
%   Translate a Wiki text into  an   HTML  term suitable for html//1
%   from the html_write library.

wiki_lines_to_dom(Lines, Args, HTML) :-
    tokenize_lines(Lines, Tokens0),
    normalise_indentation(Tokens0, Tokens),
    wiki_structure(Tokens, -1, Pars),
    wiki_faces(Pars, Args, HTML).


%!  wiki_codes_to_dom(+String, +Args, -DOM) is det.
%
%   Translate a plain text into a DOM term.
%
%   @param String   Plain text.  Either a string or a list of codes.

wiki_codes_to_dom(Codes, Args, DOM) :-
    indented_lines(Codes, [], Lines),
    wiki_lines_to_dom(Lines, Args, DOM).


%!  wiki_structure(+Lines:lines, +BaseIndent,
%!                 -Blocks:list(block)) is det
%
%   Get the structure in terms  of block-level elements: paragraphs,
%   lists and tables. This processing uses   a mixture of layout and
%   punctuation.

wiki_structure([], _, []) :- !.
wiki_structure([_-[]|T], BI, Pars) :-          % empty lines
    !,
    wiki_structure(T, BI, Pars).
wiki_structure(Lines, _, [\tags(Tags)]) :-
    tags(Lines, Tags),
    !.
wiki_structure(Lines, BI, [P1|PL]) :-
    take_block(Lines, BI, P1, RestLines),
    wiki_structure(RestLines, BI, PL).

%!  take_block(+Lines, +BaseIndent, ?Block, -RestLines) is semidet.
%
%   Take a block-structure from the input.  Defined block elements
%   are lists, table, hrule, section header and paragraph.

take_block([_-[]|Lines], BaseIndent, Block, Rest) :-
    !,
    take_block(Lines, BaseIndent, Block, Rest).
take_block([N-_|_], BaseIndent, _, _) :-
    N < BaseIndent,
    !,
    fail.                           % less indented
take_block(Lines, BaseIndent, List, Rest) :-
    list_item(Lines, Type, Indent, LI, LIT, Rest0),
    !,
    Indent > BaseIndent,
    rest_list(Rest0, Type, Indent, LIT, [], Rest),
    List0 =.. [Type, LI],
    (   ul_to_dl(List0, List)
    ->  true
    ;   List0 = dl(Items)
    ->  List = dl(class=wiki, Items)
    ;   List = List0
    ).
take_block([N-['|'|RL1]|LT], _, Table, Rest) :-
    phrase(row(R0), RL1),
    rest_table(LT, N, RL, Rest),
    !,
    Table = table(class=wiki, [tr(R0)|RL]).
take_block([0-[-,-|More]|LT], _, Block, LT) :-  % separation line
    maplist(=(-), More),
    !,
    Block = hr([]).
take_block([_-Line|LT], _, Block, LT) :-        % separation line
    ruler(Line),
    !,
    Block = hr([]).
take_block([_-[@|_]], _, _, _) :-              % starts @tags section
    !,
    fail.
take_block(Lines, _BaseIndent, Section, RestLines) :-
    section_header(Lines, Section, RestLines),
    !.
take_block([_-Verb|Lines], _, Verb, Lines) :-
    verbatim_term(Verb),
    !.
take_block([I-L1|LT], BaseIndent, Elem, Rest) :-
    !,
    append(L1, PT, Par),
    rest_par(LT, PT, I, BaseIndent, MaxI, Rest),
    (   MaxI >= BaseIndent+16
    ->  Elem = center(Par)
    ;   phrase(blockquote(BQ), Par)
    ->  Elem = blockquote(BQ)
    ;   Elem = p(Par)
    ).
take_block([Verb|Lines], _, Verb, Lines).

blockquote(Clean) -->
    [>, ' '],
    bq_lines(Clean).

bq_lines([' '|Par]) -->
    ['\n'], !, [>,' '],
    bq_lines(Par).
bq_lines([H|T]) -->
    [H],
    bq_lines(T).
bq_lines([]) -->
    [].


%!  ruler(+Line) is semidet.
%
%   True if Line contains 3 ruler chars and otherwise spaces.

ruler([C0|Line]) :-
    rule_char(C0),
    phrase(ruler(C0, 1), Line).

ruler(C, N) --> [C], !, { N2 is N+1 }, ruler(C, N2).
ruler(C, N) --> [' '], !, ruler(C, N).
ruler(_, N) --> { N >= 3 }.

rule_char('-').
rule_char('_').
rule_char('*').

%!  list_item(+Lines, ?Type, ?Indent, -LI0, -LIT, -RestLines) is det.
%
%   Create a list-item. Naturally this should produce a single item,
%   but DL lists produce two items, so   we create the list of items
%   as a difference list.
%
%   @tbd    Pass base-indent

list_item([Indent-Line|LT], Type, Indent, Items, ItemT, Rest) :-
    !,
    list_item_prefix(Type, Line, L1),
    (   Type == dl
    ->  split_dt(L1, DT0, DD1),
        append(DD1, LIT, DD),
        strip_ws_tokens(DT0, DT),
        Items = [dt(DT),dd(DD)|ItemT]
    ;   append(L1, LIT, LI0),
        Items = [li(LI0)|ItemT]
    ),
    rest_list_item(LT, Type, Indent, LIT, Rest).

%!  rest_list_item(+Lines, +Type, +Indent, -RestItem, -RestLines) is det
%
%   Extract the remainder (after the first line) of a list item.

rest_list_item(Lines, _Type, Indent, RestItem, RestLines) :-
    take_blocks_at_indent(Lines, Indent, Blocks, RestLines),
    (   Blocks = [p(Par)|MoreBlocks]
    ->  append(['\n'|Par], MoreBlocks, RestItem)
    ;   RestItem = Blocks
    ).

%!  take_blocks_at_indent(+Lines, +Indent, -Pars, -RestLines) is det.
%
%   Process paragraphs and verbatim blocks (==..==) in bullet-lists.

take_blocks_at_indent(Lines, _, [], Lines) :-
    skip_empty_lines(Lines, Lines1),
    section_header(Lines1, _, _),
    !.
take_blocks_at_indent(Lines, N, [Block|RestBlocks], RestLines) :-
    take_block(Lines, N, Block, Rest0),
    !,
    take_blocks_at_indent(Rest0, N, RestBlocks, RestLines).
take_blocks_at_indent(Lines, _, [], Lines).


%!  rest_list(+Lines, +Type, +Indent,
%!            -Items, -ItemTail, -RestLines) is det.

rest_list(Lines, Type, N, Items, IT, Rest) :-
    skip_empty_lines(Lines, Lines1),
    list_item(Lines1, Type, N, Items, IT0, Rest0),
    !,
    rest_list(Rest0, Type, N, IT0, IT, Rest).
rest_list(Rest, _, _, IT, IT, Rest).

%!  list_item_prefix(?Type, +Line, -Rest) is det.

list_item_prefix(ul, [*, ' '|T], T) :- !.
list_item_prefix(ul, [-, ' '|T], T) :- !.
list_item_prefix(dl, [$, ' '|T], T) :-
    split_dt(T, _, _),
    !.
list_item_prefix(ol, [w(N), '.', ' '|T], T) :-
    atom_codes(N, [D]),
    between(0'0, 0'9, D).

%!  split_dt(+LineAfterDollar, -DT, -Rest)
%
%   First see whether the entire line is the item. This allows
%   creating items holding : by using $ <tokens> :\n

split_dt(In, DT, []) :-
    append(DT, [':'], In),
    !.
split_dt(In, DT, Rest) :-
    append(DT, [':'|Rest0], In),
    (   Rest0 == []
    ->  Rest = []
    ;   Rest0 = [' '|Rest]
    ),
    !.


%!  ul_to_dl(+UL, -DL) is semidet.
%
%   Translate an UL list into a DL list   if  all entries are of the
%   form "* <term> nl, <description>" and at least one <description>
%   is   non-empty,   or    all    items     are    of    the   form
%   [[PredicateIndicator]].

ul_to_dl(ul(Items), Description) :-
    term_items(Items, DLItems, []),
    (   terms_to_predicate_includes(DLItems, Preds)
    ->  Description = dl(class(predicates), Preds)
    ;   member(dd(DD), DLItems), DD \== []
    ->  Description = dl(class(termlist), DLItems)
    ).

term_items([], T, T).
term_items([LI|LIs], DLItems, Tail) :-
    term_item(LI, DLItems, Tail1),
    term_items(LIs, Tail1, Tail).

%!  term_item(+LI, -DLItem, ?Tail) is semidet.
%
%   If LI is of the form <Term> followed  by a newline, return it as
%   dt-dd  tuple.  The  <dt>  item    contains  a  term
%
%       \term(Text, Term, Bindings).

term_item(li(Tokens),
          [ dt(class=term, \term(Text, Term, Bindings)),
            dd(Descr)
          | Tail
          ], Tail) :-
    (   (   append(TermTokens, ['\n'|Descr], Tokens)
        ->  true
        ;   TermTokens = Tokens,
            Descr = []
        )
    ->  with_output_to(string(Tmp),
                       ( forall(member(T, TermTokens),
                                write_token(T)),
                         write(' .\n'))),
        E = error(_,_),
        catch(setup_call_cleanup(
                  open_string(Tmp, In),
                  ( read_dt_term(In, Term, Bindings),
                    read_dt_term(In, end_of_file, []),
                    atom_string(Text, Tmp)
                  ),
                  close(In)),
              E, fail)
    ).

write_token(w(X)) :-
    !,
    write(X).
write_token(X) :-
    write(X).

read_dt_term(In, Term, Bindings) :-
    read_term(In, Term,
              [ variable_names(Bindings),
                module(pldoc_modes)
              ]).

terms_to_predicate_includes([], []).
terms_to_predicate_includes([dt(class=term, \term(_, [[PI]], [])), dd([])|T0],
                            [\include(PI, predicate, [])|T]) :-
    is_pi(PI),
    terms_to_predicate_includes(T0, T).

is_pi(Name/Arity) :-
    atom(Name),
    integer(Arity),
    between(0, 20, Arity).
is_pi(Name//Arity) :-
    atom(Name),
    integer(Arity),
    between(0, 20, Arity).


%!  row(-Cells)// is det.

row([C0|CL]) -->
    cell(C0),
    !,
    row(CL).
row([]) -->
    [].

cell(td(C)) -->
    face_tokens(C0),
    ['|'],
    !,
    { strip_ws_tokens(C0, C)
    }.

face_tokens([]) -->
    [].
face_tokens(Tokens) -->
    face_token(H),                          % Deal with embedded *|...|*, etc.
    token('|'),
    face_tokens(Face),
    token('|'),
    face_token(H),
    !,
    { append([[H,'|'], Face, ['|', H], Rest], Tokens) },
    face_tokens(Rest).
face_tokens([H|T]) -->
    token(H),
    face_tokens(T).

face_token(=) --> [=].
face_token(*) --> [*].
face_token('_') --> ['_'].

rest_table([N-Line|LT], N, RL, Rest) :-
    md_table_structure_line(Line),
    !,
    rest_table(LT, N, RL, Rest).
rest_table([N-['|'|RL1]|LT], N, [tr(R0)|RL], Rest) :-
    !,
    phrase(row(R0), RL1),
    rest_table(LT, N, RL, Rest).
rest_table(Rest, _, [], Rest).

%!  md_table_structure_line(+Chars)
%
%   True if Chars represents Markdown  table structure. We currently
%   ignore the structure information.

md_table_structure_line(Line) :-
    forall(member(Char, Line),
           md_table_structure_char(Char)).

md_table_structure_char(' ').
md_table_structure_char('-').
md_table_structure_char('|').
md_table_structure_char(':').

%!  rest_par(+Lines, -Par,
%!           +BaseIndent, +MaxI0, -MaxI, -RestLines) is det.
%
%   Take the rest of a paragraph. Paragraphs   are  ended by a blank
%   line or the start of a list-item.   The latter is a bit dubious.
%   Why not a general  block-level   object?  The current definition
%   allows for writing lists without a blank line between the items.

rest_par([], [], BI, MaxI0, MaxI, []) :-
    !,
    MaxI is max(BI, MaxI0).
rest_par([_-[]|Rest], [], _, MaxI, MaxI, Rest) :- !.
rest_par(Lines, [], _, MaxI, MaxI, Lines) :-
    Lines = [_-Verb|_],
    verbatim_term(Verb),
    !.
rest_par([I-L|Rest], [], _, MaxI, MaxI, [I-L|Rest]) :-
    list_item_prefix(_, L, _),
    !.
rest_par([I-L1|LT], ['\n'|Par], BI, MaxI0, MaxI, Rest) :-
    append(L1, PT, Par),
    MaxI1 is max(I, MaxI0),
    rest_par(LT, PT, BI, MaxI1, MaxI, Rest).


%!  section_header(+Lines, -Section, -RestLines) is semidet.
%
%   Get a section line from the input.

section_header([_-L1|LT], Section, LT) :-
    twiki_section_line(L1, Section),
    !.
section_header([0-L1|LT], Section, LT) :-
    md_section_line(L1, Section),
    !.
section_header([_-L1,0-L2|LT], Section, LT) :-
    md_section_line(L1, L2, Section),
    !.

%!  twiki_section_line(+Tokens, -Section) is semidet.
%
%   Extract a section using the Twiki   conventions. The section may
%   be preceeded by [Word], in which case we generate an anchor name
%   Word for the section.

twiki_section_line([-,-,-|Rest], Section) :-
    plusses(Rest, Section).

plusses([+, ' '|Rest], h1(Attrs, Content)) :-
    hdr_attributes(Rest, Attrs, Content).
plusses([+, +, ' '|Rest], h2(Attrs, Content)) :-
    hdr_attributes(Rest, Attrs, Content).
plusses([+, +, +, ' '|Rest], h3(Attrs, Content)) :-
    hdr_attributes(Rest, Attrs, Content).
plusses([+, +, +, +, ' '|Rest], h4(Attrs, Content)) :-
    hdr_attributes(Rest, Attrs, Content).

hdr_attributes(List, Attrs, Content) :-
    strip_leading_ws(List, List2),
    (   List2 = ['[',w(Name),']'|List3]
    ->  strip_ws_tokens(List3, Content),
        Attrs = [class(wiki), id(Name)]
    ;   Attrs = class(wiki),
        strip_ws_tokens(List, Content)
    ).

%!  md_section_line(+Tokens, -Section) is semidet.
%
%   Handle markdown section lines staring with #

md_section_line([#, ' '|Rest], h1(Attrs, Content)) :-
    md_section_attributes(Rest, Attrs, Content).
md_section_line([#, #, ' '|Rest], h2(Attrs, Content)) :-
    md_section_attributes(Rest, Attrs, Content).
md_section_line([#, #, #, ' '|Rest], h3(Attrs, Content)) :-
    md_section_attributes(Rest, Attrs, Content).
md_section_line([#, #, #, #, ' '|Rest], h4(Attrs, Content)) :-
    md_section_attributes(Rest, Attrs, Content).

md_section_attributes(List, Attrs, Content) :-
    phrase((tokens(Content), [' '], section_label(Label)), List),
    !,
    Attrs = [class(wiki), id(Label)].
md_section_attributes(Content, Attrs, Content) :-
    Attrs = [class(wiki)].

section_label(Label) -->
    [ '{', '#', w(Name) ],
    label_conts(Cont), ['}'],
    !,
    { atomic_list_concat([Name|Cont], Label) }.

label_conts([H|T]) --> label_cont(H), !, label_conts(T).
label_conts([]) --> [].

label_cont(-) --> [-].
label_cont(Name) --> [w(Name)].


md_section_line(Line1, Line2, Header) :-
    Line1 \== [],
    section_underline(Line2, Type),
    is_list(Line1),
    phrase(wiki_words(_), Line1),  % Should not have structure elements
    !,
    (   phrase(labeled_section_line(Title, Attrs), Line1)
    ->  true
    ;   Title = Line1,
        Attrs = []
    ),
    Header =.. [Type, [class(wiki)|Attrs], Title].

section_underline([=,=,=|T], h1) :-
    maplist(=(=), T),
    !.
section_underline([-,-,-|T], h2) :-
    maplist(=(-), T),
    !.

labeled_section_line(Title, Attrs) -->
    tokens(Title), [' '], section_label(Label),
    !,
    { Attrs = [id(Label)] }.


%!  strip_ws_tokens(+Tokens, -Stripped)
%
%   Strip leading and trailing whitespace from a token list.  Note
%   the the whitespace is already normalised.

strip_ws_tokens([' '|T0], T) :-
    !,
    strip_ws_tokens(T0, T).
strip_ws_tokens(L0, L) :-
    append(L, [' '], L0),
    !.
strip_ws_tokens(L, L).


%!  strip_leading_ws(+Tokens, -Stripped) is det.
%
%   Strip leading whitespace from a token list.

strip_leading_ws([' '|T], T) :- !.
strip_leading_ws(T, T).


                 /*******************************
                 *             TAGS             *
                 *******************************/

%!  tags(+Lines:lines, -Tags) is semidet.
%
%   If the first line is a @tag, read the remainder of the lines to
%   a list of \tag(Name, Value) terms.

tags(Lines, Tags) :-
    collect_tags(Lines, Tags0),
    keysort(Tags0, Tags1),
    pairs_values(Tags1, Tags2),
    combine_tags(Tags2, Tags).

%!  collect_tags(+IndentedLines, -Tags) is semidet
%
%   Create a list Order-tag(Tag,Tokens) for   each @tag encountered.
%   Order is the desired position as defined by tag_order/2.
%
%   @tbd Tag content is  often  poorly   aligned.  We  now  find the
%   alignment of subsequent lines  and  assume   the  first  line is
%   alligned with the remaining lines.

collect_tags([], []).
collect_tags([Indent-[@,String|L0]|Lines], [Order-tag(Tag,Value)|Tags]) :-
    tag_name(String, Tag, Order),
    !,
    strip_leading_ws(L0, L),
    rest_tag(Lines, Indent, VT, RestLines),
    normalise_indentation(VT, VT1),
    wiki_structure([0-L|VT1], -1, Value0),
    strip_leading_par(Value0, Value),
    collect_tags(RestLines, Tags).


%!  tag_name(+String, -Tag:atom, -Order:int) is semidet.
%
%   If String denotes a know tag-name,

tag_name(w(Name), Tag, Order) :-
    (   renamed_tag(Name, Tag, Level),
        tag_order(Tag, Order)
    ->  print_message(Level, pldoc(deprecated_tag(Name, Tag)))
    ;   tag_order(Name, Order)
    ->  Tag = Name
    ;   print_message(warning, pldoc(unknown_tag(Name))),
        fail
    ).


rest_tag([], _, [], []) :- !.
rest_tag(Lines, Indent, [], Lines) :-
    Lines = [Indent-[@,Word|_]|_],
    tag_name(Word, _, _),
    !.
rest_tag([L|Lines0], Indent, [L|VT], Lines) :-
    rest_tag(Lines0, Indent, VT, Lines).


%!  renamed_tag(+DeprecatedTag:atom, -Tag:atom, -Warn) is semidet.
%
%   Declaration for deprecated tags.

renamed_tag(exception, throws, warning).
renamed_tag(param,     arg,    silent).


%!  tag_order(+Tag:atom, -Order:int) is semidet.
%
%   Both declares the know tags and  their expected order. Currently
%   the tags are forced into  this   order  without  warning. Future
%   versions may issue a warning if the order is inconsistent.

:- multifile
    pldoc:tag_order/2.

tag_order(Tag, Order) :-
    pldoc:tag_order(Tag, Order),
    !.
tag_order(arg,         100).
tag_order(error,       200).            % same as throw
tag_order(throws,      300).
tag_order(author,      400).
tag_order(version,     500).
tag_order(see,         600).
tag_order(deprecated,  700).
tag_order(compat,      800).            % PlDoc extension
tag_order(copyright,   900).
tag_order(license,    1000).
tag_order(bug,        1100).
tag_order(tbd,        1200).

%!  combine_tags(+Tags:list(tag(Key, Value)), -Tags:list) is det.
%
%   Creates the final tag-list.  Tags is a list of
%
%           * \params(list(param(Name, Descr)))
%           * \tag(Name, list(Descr))
%
%   Descr is a list of tokens.

combine_tags([], []).
combine_tags([tag(arg, V1)|T0], [\args([P1|PL])|Tags]) :-
    !,
    arg_tag(V1, P1),
    arg_tags(T0, PL, T1),
    combine_tags(T1, Tags).
combine_tags([tag(Tag,V0)|T0], [\tag(Tag, [V0|Vs])|T]) :-
    same_tag(Tag, T0, T1, Vs),
    combine_tags(T1, T).

arg_tag([PT|Descr0], arg(PN, Descr)) :-
    word_of(PT, PN),
    strip_leading_ws(Descr0, Descr).

word_of(w(W), W) :- !.                  % TBD: check non-word arg
word_of(W, W).

arg_tags([tag(arg, V1)|T0], [P1|PL], T) :-
    !,
    arg_tag(V1, P1),
    arg_tags(T0, PL, T).
arg_tags(T, [], T).

same_tag(Tag, [tag(Tag, V)|T0], T, [V|Vs]) :-
    !,
    same_tag(Tag, T0, T, Vs).
same_tag(_, L, L, []).


                 /*******************************
                 *             FACES            *
                 *******************************/

%!  wiki_faces(+Structure, +ArgNames, -HTML) is det.
%
%   Given the wiki structure, analyse the content of the paragraphs,
%   list items and table cells and apply font faces and links.

wiki_faces([dt(Class, \term(Text, Term, Bindings)), dd(Descr0)|T0],
           ArgNames,
           [dt(Class, \term(Text, Term, Bindings)), dd(Descr)|T]) :-
    !,
    varnames(Bindings, VarNames, ArgNames),
    wiki_faces(Descr0, VarNames, Descr),
    wiki_faces(T0, ArgNames, T).
wiki_faces(DOM0, ArgNames, DOM) :-
    structure_term(DOM0, Functor, Content0),
    !,
    wiki_faces_list(Content0, ArgNames, Content),
    structure_term(DOM, Functor, Content).
wiki_faces(Verb, _, Verb) :-
    verbatim_term(Verb),
    !.
wiki_faces(Content0, ArgNames, Content) :-
    assertion(is_list(Content0)),
    phrase(wiki_faces(Content, ArgNames), Content0),
    !.

varnames([], List, List).
varnames([Name=_|T0], [Name|T], List) :-
    varnames(T0, T, List).

wiki_faces_list([], _, []).
wiki_faces_list([H0|T0], Args, [H|T]) :-
    wiki_faces(H0, Args, H),
    wiki_faces_list(T0, Args, T).

%!  structure_term(+Term, -Functor, -Content) is semidet.
%!  structure_term(-Term, +Functor, +Content) is det.
%
%   (Un)pack a term describing structure, so  we can process Content
%   and re-pack the structure.

structure_term(\tags(Tags), tags, [Tags]) :- !.
structure_term(\args(Params), args, [Params]) :- !.
structure_term(arg(Name,Descr), arg(Name), [Descr]) :- !.
structure_term(\tag(Name,Value), tag(Name), [Value]) :- !.
structure_term(\include(What,Type,Opts), include(What,Type,Opts), []) :- !.
structure_term(dl(Att, Args), dl(Att), [Args]) :- !.
structure_term(dt(Att, Args), dt(Att), [Args]) :- !.
structure_term(table(Att, Args), table(Att), [Args]) :- !.
structure_term(h1(Att, Args), h1(Att), [Args]) :- !.
structure_term(h2(Att, Args), h2(Att), [Args]) :- !.
structure_term(h3(Att, Args), h3(Att), [Args]) :- !.
structure_term(h4(Att, Args), h4(Att), [Args]) :- !.
structure_term(hr(Att), hr(Att), []) :- !.
structure_term(p(Args), p, [Args]) :- !.
structure_term(Term, Functor, Args) :-
    structure_term_any(Term, Functor, Args).

structure_term(Term) :-
    structure_term_any(Term, _Functor, _Args).

structure_term_any(Term, Functor, Args) :-
    functor(Term, Functor, 1),
    structure_tag(Functor),
    !,
    Term =.. [Functor|Args].

structure_tag(ul).
structure_tag(ol).
structure_tag(dl).
structure_tag(li).
structure_tag(dt).
structure_tag(dd).
structure_tag(table).
structure_tag(tr).
structure_tag(td).
structure_tag(blockquote).
structure_tag(center).


%!  verbatim_term(?Term) is det
%
%   True if Term must be passes verbatim.

verbatim_term(pre(_,_)).
verbatim_term(\term(_,_,_)).

%!  matches(:Goal, -Input, -Last)//
%
%   True when Goal runs successfully on the DCG input and Input
%   is the list of matched tokens.

:- meta_predicate matches(2, -, -, ?, ?).

matches(Goal, Input, Last, List, Rest) :-
    call(Goal, List, Rest),
    input(List, Rest, Input, Last).

input([H|T0], Rest, Input, Last) :-
    (   T0 == Rest
    ->  Input = [H],
        Last = H
    ;   Input = [H|T],
        input(T0, Rest, T, Last)
    ).


%!  wiki_faces(-WithFaces, +ArgNames)// is nondet.
%!  wiki_faces(-WithFaces, +ArgNames, +Options)// is nondet.
%
%   Apply font-changes and automatic  links   to  running  text. The
%   faces are applied after discovering   the structure (paragraphs,
%   lists, tables, keywords).
%
%   @arg Options is a dict, minimally containing `depth`

wiki_faces(WithFaces, ArgNames, List, Rest) :-
    default_faces_options(Options),
    catch(wiki_faces(WithFaces, ArgNames, Options, List, Rest),
          pldoc(depth_limit),
          failed_faces(WithFaces, List, Rest)).

default_faces_options(_{depth:5}).

failed_faces(WithFaces) -->
    { debug(markdown(overflow), 'Depth limit exceeded', []) },
    wiki_words(WithFaces).

wiki_faces([EmphTerm|T], ArgNames, Options) -->
    emphasis_seq(EmphTerm, ArgNames, Options),
    !,
    wiki_faces_int(T, ArgNames).
wiki_faces(Faces, ArgNames, Options) -->
    wiki_faces_int(Faces, ArgNames, Options).

wiki_faces_int(WithFaces, ArgNames) -->
    { default_faces_options(Options)
    },
    wiki_faces_int(WithFaces, ArgNames, Options).

wiki_faces_int([], _, _) -->
    [].
wiki_faces_int([H|T], ArgNames, Options) -->
    wiki_face(H, ArgNames, Options),
    !,
    wiki_faces(T, ArgNames, Options).
wiki_faces_int([Before,EmphTerm|T], ArgNames, Options) -->
    emphasis_before(Before),
    emphasis_seq(EmphTerm, ArgNames, Options),
    !,
    wiki_faces_int(T, ArgNames, Options).
wiki_faces_int([H|T], ArgNames, Options) -->
    wiki_face_simple(H, ArgNames, Options),
    !,
    wiki_faces_int(T, ArgNames, Options).

next_level(Options0, Options) -->
    {   succ(NewDepth, Options0.depth)
    ->  Options = Options0.put(depth, NewDepth)
    ;   throw(pldoc(depth_limit))
    }.

%!  prolog:doc_wiki_face(-Out, +VarNames)// is semidet.
%!  prolog:doc_wiki_face(-Out, +VarNames, +Options0)// is semidet.
%
%   Hook that can be  used  to   provide  additional  processing for
%   additional _inline_ wiki constructs.  The DCG list is a list of
%   tokens.  Defined tokens are:
%
%     - w(Atom)
%     Recognised word (alphanumerical)
%     - Atom
%     Single character atom representing punctuation marks or the
%     atom =|' '|= (space), representing white-space.
%
%   The  Out  variable  is  input  for    the  backends  defined  in
%   doc_latex.pl and doc_html.pl. Roughly, these   are terms similar
%   to what html//1 from library(http/html_write) accepts.

wiki_face(Out, Args, _) -->
    prolog:doc_wiki_face(Out, Args),
    !.
wiki_face(var(Arg), ArgNames, _) -->
    [w(Arg)],
    { memberchk(Arg, ArgNames)
    },
    !.
wiki_face(b(Bold), ArgNames, Options) -->
    [*,'|'], string(Tokens), ['|',*],
    !,
    { phrase(wiki_faces(Bold, ArgNames, Options), Tokens) }.
wiki_face(i(Italic), ArgNames, Options) -->
    ['_','|'], string(Tokens), ['|','_'],
    !,
    { phrase(wiki_faces(Italic, ArgNames, Options), Tokens) }.
wiki_face(code(Code), _, _) -->
    [=], eq_code_words(Words), [=],
    !,
    { atomic_list_concat(Words, Code) }.
wiki_face(code(Code), _, _) -->
    [=,'|'], wiki_words(Code), ['|',=],
    !.
wiki_face(code(Code), _, _) -->
    ['`','`'], wiki_words(Code), ['`','`'],
    !.
wiki_face(Code, _, _) -->
    (   ['`'], code_words(Words), ['`']
    ->  { atomic_list_concat(Words, Text),
          E = error(_,_),
          catch(atom_to_term(Text, Term, Vars), E, fail),
          !,
          code_face(Text, Term, Vars, Code)
        }
    ).
wiki_face(Face, _, Options) -->
    [ w(Name) ], arg_list(List),
    { atomic_list_concat([Name|List], Text),
      E = error(_,_),
      catch(atom_to_term(Text, Term, Vars), E, fail),
      term_face(Text, Term, Vars, Face, Options)
    },
    !.
wiki_face(br([]), _, _) -->
    [<,w(br),>,'\n'], !.
wiki_face(br([]), _, _) -->
    [<,w(br),/,>,'\n'], !.
        % Below this, we only do links.
wiki_face(_, _, Options) -->
    { Options.get(link) == false,
      !,
      fail
    }.
wiki_face(\predref(Name/Arity), _, _) -->
    [ w(Name), '/' ], arity(Arity),
    { functor_name(Name)
    },
    !.
wiki_face(\predref(Module:(Name/Arity)), _, _) -->
    [ w(Module), ':', w(Name), '/' ], arity(Arity),
    { functor_name(Name)
    },
    !.
wiki_face(\predref(Name/Arity), _, _) -->
    prolog_symbol_char(S0),
    symbol_string(SRest), [ '/' ], arity(Arity),
    !,
    { atom_chars(Name, [S0|SRest])
    }.
wiki_face(\predref(Name//Arity), _, _) -->
    [ w(Name), '/', '/' ], arity(Arity),
    { functor_name(Name)
    },
    !.
wiki_face(\predref(Module:(Name//Arity)), _, _) -->
    [ w(Module), ':', w(Name), '/', '/' ], arity(Arity),
    { functor_name(Name)
    },
    !.
wiki_face(\include(Name, Type, Options), _, _) -->
    ['[','['], file_name(Base, Ext), [']',']'],
    { autolink_extension(Ext, Type),
      !,
      file_name_extension(Base, Ext, Name),
      resolve_file(Name, Options, [])
    },
    !.
wiki_face(\include(Name, Type, [caption(Caption)|Options]), _, _) -->
    (   ['!','['], tokens(100, Caption), [']','(']
    ->  file_name(Base, Ext), [')'],
        { autolink_extension(Ext, Type),
          !,
          file_name_extension(Base, Ext, Name),
          resolve_file(Name, Options, [])
        }
    ),
    !.
wiki_face(Link, ArgNames, Options) -->          % TWiki: [[Label][Link]]
    (   ['[','['], wiki_label(Label, ArgNames, Options), [']','[']
    ->  wiki_link(Link, [label(Label), relative(true), end(']')]),
        [']',']'], !
    ).
wiki_face(Link, ArgNames, Options) -->          % Markdown: [Label](Link)
    (   ['['], wiki_label(Label, ArgNames, Options), [']','(']
    ->  wiki_link(Link, [label(Label), relative(true), end(')')]),
        [')'], !
    ).
wiki_face(Link, _ArgNames, _) -->
    wiki_link(Link, []),
    !.

wiki_label(Label, _ArgNames, _Options) -->
    image_label(Label).
wiki_label(Label, ArgNames, Options) -->
    next_level(Options, NOptions),
    limit(40, wiki_faces(Label, ArgNames, NOptions.put(link,false))).

%!  wiki_face_simple(-Out, +ArgNames, +Options)
%
%   Skip simple (non-markup) wiki.

wiki_face_simple(Word, _, _) -->
    [ w(Word) ],
    !.
wiki_face_simple(SpaceOrPunct, _, _) -->
    [ SpaceOrPunct ],
    { atomic(SpaceOrPunct) },
    !.
wiki_face_simple(FT, ArgNames, _) -->
    [Structure],
    { wiki_faces(Structure, ArgNames, FT)
    }.

wiki_words([]) --> [].
wiki_words([Word|T]) --> [w(Word)], !, wiki_words(T).
wiki_words([Punct|T]) --> [Punct], {atomic(Punct)}, wiki_words(T).

%!  code_words(-Words)//
%
%   True when Words is the  content   as  it  appears in =|`code`|=,
%   where =|``|= is mapped to =|`|=.

code_words([]) --> [].
code_words([Word|T]) --> [w(Word)], code_words(T).
code_words(CodeL) --> ['`','`'], {CodeL = ['`'|T]}, code_words(T).
code_words([Punct|T]) --> [Punct], {atomic(Punct)}, code_words(T).

%!  eq_code_words(-Words)//
%
%   Stuff that can be between single `=`.  This is limited to
%
%           - Start and end must be a word
%           - In between may be the following punctuation chars:
%             =|.-:/|=, notably dealing with file names and
%             identifiers in various external languages.

eq_code_words([Word]) -->
    [ w(Word) ].
eq_code_words([Word|T]) -->
    [ w(Word) ], eq_code_internals(T, [End]), [w(End)].

eq_code_internals(T, T) --> [].
eq_code_internals([H|T], Tail) -->
    eq_code_internal(H),
    eq_code_internals(T, Tail).

eq_code_internal(Word) -->
    [w(Word)].
eq_code_internal(Punct) -->
    [Punct],
    { eq_code_internal_punct(Punct) }.

eq_code_internal_punct('.').
eq_code_internal_punct('-').
eq_code_internal_punct(':').
eq_code_internal_punct('/').


%!  code_face(+Text, +Term, +Vars, -Code) is det.
%
%   Deal with =|`... code ...`|=  sequences.   Text  is  the matched
%   text, Term is the parsed Prolog term   and Code is the resulting
%   intermediate code.

code_face(Text, Var, _, Code) :-
    var(Var),
    !,
    Code = var(Text).
code_face(Text, _, _, code(Text)).


%!  emphasis_seq(-Out, +ArgNames, +Options) is semidet.
%
%   Recognise emphasis sequences

emphasis_seq(EmphTerm, ArgNames, Options) -->
    emphasis_start(C),
    next_level(Options, NOptions),
    matches(limit(100, wiki_faces(Emph, ArgNames, NOptions)), Input, Last),
    emphasis_end(C),
    { emph_markdown(Last, Input),
      emphasis_term(C, Emph, EmphTerm)
    },
    !.


%!  emphasis_term(+Emphasis, +Tokens, -Term) is det.
%!  emphasis_before(-Before)// is semidet.
%!  emphasis_start(-Emphasis)// is semidet.
%!  emphasis_end(+Emphasis)// is semidet.
%
%   Primitives for Doxygen emphasis handling.

emphasis_term('_',   Term, i(Term)).
emphasis_term('*',   Term, b(Term)).
emphasis_term('__',  Term, strong(Term)).
emphasis_term('**',  Term, strong(Term)).

emph_markdown(_, [w(_)]) :- !.
emph_markdown(Last, Tokens) :-
    \+ emphasis_after_sep(Last),
    E = error(_,_),
    catch(b_getval(pldoc_object, Obj), E, Obj = '??'),
    debug(markdown(emphasis), '~q: additionally emphasis: ~p',
          [Obj, Tokens]).

emphasis_before(Before) -->
    [Before],
    { emphasis_start_sep(Before) }.

emphasis_start_sep('\n').
emphasis_start_sep(' ').
emphasis_start_sep('<').
emphasis_start_sep('{').
emphasis_start_sep('(').
emphasis_start_sep('[').
emphasis_start_sep(',').
emphasis_start_sep(':').
emphasis_start_sep(';').

emphasis_start(Which), [w(Word)] -->
    emphasis(Which),
    [w(Word)].

emphasis(**)   --> [*, *].
emphasis(*)    --> [*].
emphasis('__') --> ['_', '_'].
emphasis('_')  --> ['_'].

emphasis_end(Which), [After] -->
    emphasis(Which),
    [ After ],
    !,
    { emphasis_close_sep(After) -> true }.
emphasis_end(Which) -->
    emphasis(Which).

% these characters should not be before a closing * or _.

emphasis_after_sep('\n').
emphasis_after_sep(' ').
emphasis_after_sep('(').
emphasis_after_sep('[').
emphasis_after_sep('<').
emphasis_after_sep('=').
emphasis_after_sep('+').
emphasis_after_sep('\\').
emphasis_after_sep('@').

emphasis_close_sep('\n').                       % white
emphasis_close_sep(' ').                        % white
emphasis_close_sep(',').                        % sentence punctuation
emphasis_close_sep('.').
emphasis_close_sep('!').
emphasis_close_sep('?').
emphasis_close_sep(':').
emphasis_close_sep(';').
emphasis_close_sep(']').                        % [**label**](link)
emphasis_close_sep(')').                        % ... _italic_)
emphasis_close_sep('}').                        % ... _italic_}
emphasis_close_sep(Token) :-
    structure_term(Token).


%!  arg_list(-Atoms) is nondet.
%
%   Atoms  is  a  token-list  for  a    Prolog   argument  list.  An
%   argument-list is a sequence of tokens '(' ... ')'.
%
%   @bug    the current implementation does not deal correctly with
%           brackets that are embedded in quoted strings.

arg_list(['('|T]) -->
    ['('], arg_list_close(T, 1).

arg_list_close(Tokens, Depth) -->
    [')'],
    !,
    (   { Depth == 1 }
    ->  { Tokens = [')'] }
    ;   { Depth > 1 }
    ->  { Tokens = [')'|More],
          NewDepth is Depth - 1
        },
        arg_list_close(More, NewDepth)
    ).
arg_list_close(['('|T], Depth) -->
    ['('], { NewDepth is Depth+1 },
    arg_list_close(T, NewDepth).
arg_list_close([H|T], Depth) -->
    [w(H)],
    !,
    arg_list_close(T, Depth).
arg_list_close([H|T], Depth) -->
    [H],
    arg_list_close(T, Depth).


%!  term_face(+Text, +Term, +Vars, -Face, +Options) is semidet.
%
%   Process embedded Prolog-terms. Currently   processes  Alias(Arg)
%   terms that refer to files.  Future   versions  will also provide
%   pretty-printing of Prolog terms.

term_face(_Text, Term, _Vars, \file(Name, FileOptions), Options) :-
    ground(Term),
    compound(Term),
    compound_name_arity(Term, Alias, 1),
    user:file_search_path(Alias, _),
    existing_file(Term, FileOptions, [], Options),
    !,
    format(atom(Name), '~q', [Term]).
term_face(Text, Term, Vars, Face, _Options) :-
    code_face(Text, Term, Vars, Face).

untag([], []).
untag([w(W)|T0], [W|T]) :-
    !,
    untag(T0, T).
untag([H|T0], [H|T]) :-
    untag(T0, T).

%!  image_label(-Label)//
%
%   Match File[;param=value[,param=value]*]

image_label(\include(Name, image, Options)) -->
    file_name(Base, Ext),
    { autolink_extension(Ext, image),
      file_name_extension(Base, Ext, Name),
      resolve_file(Name, Options, RestOptions)
    },
    file_options(RestOptions).


%!  file_options(-Options) is det.
%
%   Extracts additional processing options for  files. The format is
%   ;name="value",name2=value2,... Spaces are not allowed.

file_options(Options) -->
    [;], nv_pairs(Options),
    !.
file_options([]) -->
    [].

nv_pairs([H|T]) -->
    nv_pair(H),
    (   [',']
    ->  nv_pairs(T)
    ;   {T=[]}
    ).

nv_pair(Option) -->
    [ w(Name), =,'"'], tokens(Tokens), ['"'],
    !,
    { untag(Tokens, Atoms),
      atomic_list_concat(Atoms, Value0),
      (   atom_number(Value0, Value)
      ->  true
      ;   Value = Value0
      ),
      Option =.. [Name,Value]
    }.


%!  wiki_link(-Link, +Options)// is semidet.
%
%   True if we can find a link to a file or URL. Links are described
%   as one of:
%
%       $ filename :
%       A filename defined using autolink_file/2 or
%       autolink_extension/2
%       $ <url-protocol>://<rest-url> :
%       A fully qualified URL
%       $ '<' URL '>' :
%       Be more relaxed on the URL specification.

:- multifile
    user:url_path/2.

wiki_link(\file(Name, FileOptions), Options) -->
    file_name(Base, Ext),
    { file_name_extension(Base, Ext, Name),
      (   autolink_file(Name, _)
      ;   autolink_extension(Ext, _)
      ),
      !,
      resolve_file(Name, FileOptions, Options)
    }.
wiki_link(\file(Name, FileOptions), Options) -->
    [w(Name)],
    { autolink_file(Name, _),
      !,
      resolve_file(Name, FileOptions, Options)
    },
    !.
wiki_link(a(href(Ref), Label), Options) -->
    [ w(Prot),:,/,/], { url_protocol(Prot) },
    { option(end(End), Options, space)
    },
    tokens_no_whitespace(Rest), peek_end_url(End),
    !,
    { atomic_list_concat([Prot, :,/,/ | Rest], Ref),
      option(label(Label), Options, Ref)
    }.
wiki_link(a(href(Ref), Label), _Options) -->
    [<, w(Alias), :],
    tokens_no_whitespace(Rest), [>],
    { Term = (Alias:Rest),
      prolog:url_expansion_hook(Term, Ref, Label), !
    }.
wiki_link(a(href(Ref), Label), Options) -->
    [<, w(Alias), :],
    { user:url_path(Alias, _)
    },
    tokens_no_whitespace(Rest), [>],
    { atomic_list_concat(Rest, Local),
      (   Local == ''
      ->  Term =.. [Alias,'.']
      ;   Term =.. [Alias,Local]
      ),
      E = error(_,_),
      catch(expand_url_path(Term, Ref), E, fail),
      option(label(Label), Options, Ref)
    }.
wiki_link(a(href(Ref), Label), Options) -->
    [#, w(First)],
    { option(end(End), Options) },
    tokens_no_whitespace(Rest),
    peek_end_url(End),
    !,
    { atomic_list_concat([#,First|Rest], Ref),
      option(label(Label), Options, Ref)
    }.
wiki_link(a(href(Ref), Label), Options) -->
    [<],
    (   { option(relative(true), Options),
          Parts = Rest
        }
    ->  tokens_no_whitespace(Rest)
    ;   { Parts = [Prot, : | Rest]
        },
        [w(Prot), :], tokens_no_whitespace(Rest)
    ),
    [>],
    !,
    { atomic_list_concat(Parts, Ref),
      option(label(Label), Options, Ref)
    }.

%!  prolog:url_expansion_hook(+Term, -HREF, -Label) is semidet.
%
%   This hook is called after   recognising  =|<Alias:Rest>|=, where
%   Term is of the form Alias(Rest). If   it  succeeds, it must bind
%   HREF to an atom or string representing the link target and Label
%   to an html//1 expression for the label.

%!  file_name(-Name:atom, -Ext:atom)// is semidet.
%
%   Matches a filename.  A filename is defined as a sequence
%   <segment>{/<segment}.<ext>.

file_name(FileBase, Extension) -->
    segment(S1),
    segments(List),
    ['.'], file_extension(Extension),
    !,
    { atomic_list_concat([S1|List], '/', FileBase) }.
file_name(FileBase, Extension) -->
    [w(Alias), '('],
    { once(user:file_search_path(Alias, _)) },
    segment(S1),
    segments(List),
    [')'],
    !,
    { atomic_list_concat([S1|List], '/', Base),
      Spec =.. [Alias,Base],
      absolute_file_name(Spec, Path,
                         [ access(read),
                           extensions([pl]),
                           file_type(prolog),
                           file_errors(fail)
                         ]),
      file_name_extension(FileBase, Extension, Path)
    }.


segment(..) -->
    ['.','.'],
    !.
segment(Word) -->
    [w(Word)].
segment(Dir) -->
    [w(Word),'.',w(d)],
    { atom_concat(Word, '.d', Dir) }.

segments([H|T]) -->
    ['/'],
    !,
    segment(H),
    segments(T).
segments([]) -->
    [].

file_extension(Ext) -->
    [w(Ext)],
    { autolink_extension(Ext, _)
    }.


%!  resolve_file(+Name, -FileOptions, ?RestOptions, +Options) is det.
%
%   Find the actual file based on the pldoc_file global variable. If
%   present  and  the   file   is    resolvable,   add   an   option
%   absolute_path(Path) that reflects the current   location  of the
%   file.

resolve_file(Name, FileOptions, Rest) :-
    existing_file(Name, FileOptions, Rest, []),
    !.
resolve_file(_, Options, Options).


existing_file(Name, FileOptions, Rest, Options) :-
    \+ Options.get(link) == false,
    E = error(_,_),
    catch(existing_file_p(Name, FileOptions, Rest), E, fail).

existing_file_p(Name, FileOptions, Rest) :-
    (   nb_current(pldoc_file, RelativeTo),
        RelativeTo \== []
    ->  Extra = [relative_to(RelativeTo)|Extra1]
    ;   Extra = Extra1
    ),
    (   compound(Name)
    ->  Extra1 = [file_type(prolog)]
    ;   Extra1 = []
    ),
    absolute_file_name(Name, Path,
                       [ access(read),
                         file_errors(fail)
                       | Extra
                       ]),
    FileOptions = [ absolute_path(Path) | Rest ].

%!  arity(-Arity:int)// is semidet.
%
%   True if the next token can be  interpreted as an arity. That is,
%   refers to a non-negative integers of at most 20. Although Prolog
%   allows for higher arities, we assume 20   is  a fair maximum for
%   user-created predicates that are documented.

arity(Arity) -->
    [ w(Word) ],
    { E = error(_,_),
      catch(atom_number(Word, Arity), E, fail),
      Arity >= 0, Arity < 20
    }.

%!  symbol_string(-String)// is nondet
%
%   Accept a sequence of Prolog symbol characters, starting with the
%   shortest (empty) match.

symbol_string([]) -->
    [].
symbol_string([H|T]) -->
    [H],
    { prolog_symbol_char(H) },
    symbol_string(T).

prolog_symbol_char(C) -->
    [C],
    { prolog_symbol_char(C) }.

%!  prolog_symbol_char(?Char)
%
%   True if char is classified by Prolog as a symbol char.

prolog_symbol_char(#).
prolog_symbol_char($).
prolog_symbol_char(&).
prolog_symbol_char(*).
prolog_symbol_char(+).
prolog_symbol_char(-).
prolog_symbol_char(.).
prolog_symbol_char(/).
prolog_symbol_char(:).
prolog_symbol_char(<).
prolog_symbol_char(=).
prolog_symbol_char(>).
prolog_symbol_char(?).
prolog_symbol_char(@).
prolog_symbol_char(\).
prolog_symbol_char(^).
prolog_symbol_char(~).


functor_name(String) :-
    sub_atom(String, 0, 1, _, Char),
    char_type(Char, lower).

url_protocol(http).
url_protocol(https).
url_protocol(ftp).
url_protocol(mailto).

peek_end_url(space) -->
    peek(End),
    { space_token(End) },
    !.
peek_end_url(space, [], []) :- !.
peek_end_url(Token) -->
    peek(Token),
    !.

space_token(' ') :- !.
space_token('\r') :- !.
space_token('\n') :- !.
space_token(T) :-
    \+ atom(T),                     % high level format like p(...)
    \+ T = w(_).

%!  autolink_extension(?Ext, ?Type) is nondet.
%
%   True if Ext is a filename extensions that create automatic links
%   in the documentation.

autolink_extension(Ext, Type) :-
    prolog:doc_autolink_extension(Ext, Type),
    !.
autolink_extension(Ext, prolog) :-
    user:prolog_file_type(Ext,prolog),
    !.
autolink_extension(txt, wiki).
autolink_extension(md,  wiki).
autolink_extension(gif, image).
autolink_extension(png, image).
autolink_extension(jpg, image).
autolink_extension(jpeg, image).
autolink_extension(svg, image).

%!  autolink_file(?File, -Type) is nondet.
%
%   Files to which we automatically create links, regardless of the
%   extension.

autolink_file('README', wiki).
autolink_file('TODO', wiki).
autolink_file('ChangeLog', wiki).

                 /*******************************
                 *           SECTIONS           *
                 *******************************/

%!  section_comment_header(+Lines, -Header, -RestLines) is semidet.
%
%   Processes   /**   <section>   comments.   Header   is   a   term
%   \section(Type, Title), where  Title  is   an  atom  holding  the
%   section title and Type is an atom holding the text between <>.
%
%   @param Lines    List of Indent-Codes.
%   @param Header   DOM term of the format \section(Type, Title),
%                   where Type is an atom from <type> and Title is
%                   a string holding the type.

section_comment_header([_-Line|Lines], Header, Lines) :-
    phrase(section_line(Header), Line).

section_line(\section(Type, Title)) -->
    ws, "<", word(Codes), ">", normalise_white_space(TitleCodes),
    { atom_codes(Type, Codes),
      atom_codes(Title, TitleCodes)
    }.

                 /*******************************
                 *           TOKENIZER          *
                 *******************************/

%!  tokenize_lines(+Lines:lines, -TokenLines) is det
%
%   Convert Indent-Codes into Indent-Tokens

tokenize_lines(Lines, TokenLines) :-
    tokenize_lines(Lines, -1, TokenLines).

tokenize_lines([], _, []) :- !.
tokenize_lines(Lines, Indent, [Pre|T]) :-
    verbatim(Lines, Indent, Pre, RestLines),
    !,
    tokenize_lines(RestLines, Indent, T).
tokenize_lines([I-H0|T0], Indent0, [I-H|T]) :-
    phrase(line_tokens(H), H0),
    (   H == []
    ->  Indent = Indent0
    ;   Indent = I
    ),
    tokenize_lines(T0, Indent, T).


%!  line_tokens(-Tokens:list)// is det.
%
%   Create a list of tokens, where  is  token   is  either  a ' ' to
%   denote spaces, a  term  w(Word)  denoting   a  word  or  an atom
%   denoting a punctuation  character.   Underscores  (_)  appearing
%   inside an alphanumerical string are considered part of the word.
%   E.g., "hello_world_" tokenizes into [w(hello_world), '_'].

line_tokens([H|T]) -->
    line_token(H),
    !,
    line_tokens(T).
line_tokens([]) -->
    [].

line_token(T) -->
    [C],
    (   { code_type(C, space) }
    ->  ws,
        { T = ' ' }
    ;   { code_type(C, alnum) },
        word(Rest),
        { atom_codes(W, [C|Rest]),
          T = w(W)
        }
    ;   { char_code(T, C) }
    ).

word([C0|T]) -->
    [C0],  { code_type(C0, alnum) },
    !,
    word(T).
word([0'_, C1|T]) -->
    [0'_, C1],  { code_type(C1, alnum) },
    !,
    word(T).
word([]) -->
    [].

alphas([C0|T]) -->
    [C0],  { code_type(C0, alpha) },
    !,
    alphas(T).
alphas([]) -->
    [].

%!  verbatim(+Lines, +EnvIndent, -Pre, -RestLines) is det.
%
%   Extract a verbatim environment.  The  returned   Pre  is  of the
%   format pre(Attributes, String). The indentation   of the leading
%   fence is substracted from the indentation of the verbatim lines.
%   Two types of fences are supported:   the  traditional =|==|= and
%   the Doxygen =|~~~|= (minimum  3   =|~|=  characters), optionally
%   followed by =|{.ext}|= to indicate the language.
%
%   Verbatim environment is delimited as
%
%     ==
%       ...,
%       verbatim(Lines, Pre, Rest)
%       ...,
%     ==
%
%   In addition, a verbatim environment may  simply be indented. The
%   restrictions are described in the documentation.

verbatim(Lines, _,
         Indent-pre([class(code), ext(Ext)],Pre),
         RestLines) :-
    skip_empty_lines(Lines, [Indent-FenceLine|CodeLines]),
    verbatim_fence(FenceLine, Fence, Ext),
    verbatim_body(CodeLines, Indent, [10|PreCodes], [],
                  [Indent-Fence|RestLines]),
    !,
    atom_codes(Pre, PreCodes).
verbatim([_-[],Indent-Line|Lines], EnvIndent,
         Indent-pre(class(code),Pre),
         RestLines) :-
    EnvIndent >= 0,
    Indent >= EnvIndent+4, Indent =< EnvIndent+8,
    valid_verbatim_opening(Line),
    indented_verbatim_body([Indent-Line|Lines], Indent,
                           CodeLines, RestLines),
    !,
    lines_code_text(CodeLines, Indent, [10|PreCodes]),
    atom_codes(Pre, PreCodes).

verbatim_body(Lines, _, PreT, PreT, Lines).
verbatim_body([I-L|Lines], Indent, [10|Pre], PreT, RestLines) :-
    PreI is I - Indent,
    phrase(pre_indent(PreI), Pre, PreT0),
    verbatim_line(L, PreT0, PreT1),
    verbatim_body(Lines, Indent, PreT1, PreT, RestLines).

verbatim_fence(Line, Fence, '') :-
    Line == [0'=,0'=],
    !,
    Fence = Line.
verbatim_fence(Line, Fence, Ext) :-
    tilde_fence(Line, Fence, 0, Ext).
verbatim_fence(Line, Fence, Ext) :-
    md_fence(Line, Fence, 0, Ext).

tilde_fence([0'~|T0], [0'~|F0], C0, Ext) :-
    !,
    C1 is C0+1,
    tilde_fence(T0, F0, C1, Ext).
tilde_fence(List, [], C, Ext) :-
    C >= 3,
    (   List == []
    ->  Ext = ''
    ;   phrase(tilde_fence_ext(ExtCodes), List)
    ->  atom_codes(Ext, ExtCodes)
    ).

%!  tilde_fence_ext(-Ext)// is semidet.
%
%   Detect ```{.prolog} (Doxygen) or ```{prolog} (GitHub)

tilde_fence_ext(Ext) -->
    "{.", !, alphas(Ext), "}".
tilde_fence_ext(Ext) -->
    "{", alphas(Ext), "}".

md_fence([0'`|T0], [0'`|F0], C0, Ext) :-
    !,
    C1 is C0+1,
    md_fence(T0, F0, C1, Ext).
md_fence(List, [], C, Ext) :-
    C >= 3,
    (   List == []
    ->  Ext = ''
    ;   phrase(md_fence_ext(ExtCodes), List),
        atom_codes(Ext, ExtCodes)
    ).

% Also support Doxygen's curly bracket notation.
md_fence_ext(Ext) -->
    tilde_fence_ext(Ext),
    !.
% In Markdown language names appear without brackets.
md_fence_ext(Ext) -->
    alphas(Ext).

%!  indented_verbatim_body(+Lines, +Indent, -CodeLines, -RestLines)
%
%   Takes more verbatim lines. The input   ends  with the first line
%   that is indented less than Indent. There cannot be more than one
%   consequtive empty line in the verbatim body.

indented_verbatim_body([I-L|T0], Indent, [I-L|T], RestLines) :-
    L \== [], I >= Indent,
    !,
    indented_verbatim_body(T0, Indent, T, RestLines).
indented_verbatim_body([I0-[],I-L|T0], Indent, [I0-[],I-L|T], RestLines) :-
    I >= Indent,
    valid_verbatim_opening(L),
    indented_verbatim_body(T0, Indent, T, RestLines).
indented_verbatim_body(Lines, _, [], Lines).

%!  valid_verbatim_opening(+Line) is semidet.
%
%   Tests that line does not look like a list item or table.

valid_verbatim_opening([0'||_]) :- !, fail.
valid_verbatim_opening(Line) :-
    Line \== [],
    \+ ( phrase(line_tokens(Tokens), Line),
         list_item_prefix(_Type, Tokens, _Rest)
       ).

%!  lines_code_text(+Lines, +Indent, -Codes) is det.
%
%   Extract the actual code content from a list of line structures.

lines_code_text([], _, []).
lines_code_text([_-[]|T0], Indent, [10|T]) :-
    !,
    lines_code_text(T0, Indent, T).
lines_code_text([I-Line|T0], Indent, [10|T]) :-
    PreI is I-Indent,
    phrase(pre_indent(PreI), T, T1),
    verbatim_line(Line, T1, T2),
    lines_code_text(T0, Indent, T2).


%!  pre_indent(+Indent)// is det.
%
%   Insert Indent leading spaces.  Note we cannot use tabs as these
%   are not expanded by the HTML <pre> element.

pre_indent(N) -->
    { N > 0,
      !,
      N2 is N - 1
    }, " ",
    pre_indent(N2).
pre_indent(_) -->
    "".

verbatim_line(Line, Pre, PreT) :-
    append(Line, PreT, Pre).


                 /*******************************
                 *            SUMMARY           *
                 *******************************/

%!  summary_from_lines(+Lines:lines, -Summary:list(codes)) is det.
%
%   Produce a summary for Lines. Similar  to JavaDoc, the summary is
%   defined as the first sentence of the documentation. In addition,
%   a sentence is also ended by an  empty   line  or  the end of the
%   comment.

summary_from_lines(Lines, Sentence) :-
    skip_empty_lines(Lines, Lines1),
    summary2(Lines1, Sentence0),
    end_sentence(Sentence0, Sentence).

summary2(_, Sentence) :-
    Sentence == [],
    !.              % we finished our sentence
summary2([], []) :- !.
summary2([_-[]|_], []) :- !.            % empty line
summary2([_-[0'@|_]|_], []) :- !.       % keyword line
summary2([_-L0|Lines], Sentence) :-
    phrase(sentence(Sentence, Tail), L0, _),
    summary2(Lines, Tail).

sentence([C,End], []) -->
    [C,End],
    { \+ code_type(C, period),
      code_type(End, period)                % ., !, ?
    },
    space_or_eos,
    !.
sentence([0' |T0], T) -->
    space,
    !,
    ws,
    sentence(T0, T).
sentence([H|T0], T) -->
    [H],
    sentence(T0, T).
sentence([0' |T], T) -->                % '
    eos.

space_or_eos -->
    [C],
    !,
    {code_type(C, space)}.
space_or_eos -->
    eos.

%!  skip_empty_lines(+LinesIn, -LinesOut) is det.
%
%   Remove empty lines from the start of the input.  Note that
%   this is used both to process character and token data.

skip_empty_lines([], []).
skip_empty_lines([_-[]|Lines0], Lines) :-
    !,
    skip_empty_lines(Lines0, Lines).
skip_empty_lines(Lines, Lines).

end_sentence([], []).
end_sentence([0'\s], [0'.]) :- !.
end_sentence([H|T0], [H|T]) :-
    end_sentence(T0, T).


                 /*******************************
                 *        CREATE LINES          *
                 *******************************/

%!  indented_lines(+Text:list(codes), +Prefixes:list(codes),
%!                 -Lines:list) is det.
%
%   Extract a list of lines  without   leading  blanks or characters
%   from Prefix from Text. Each line   is a term Indent-Codes, where
%   Indent specifies the line_position of the real text of the line.

indented_lines(Comment, Prefixes, Lines) :-
    must_be(codes, Comment),
    phrase(split_lines(Prefixes, Lines), Comment),
    !.

split_lines(_, []) -->
    end_of_comment.
split_lines(Prefixes, [Indent-L1|Ls]) -->
    take_prefix(Prefixes, 0, Indent0),
    white_prefix(Indent0, Indent),
    take_line(L1),
    split_lines(Prefixes, Ls).


%!  end_of_comment//
%
%   Succeeds if we hit the end of the comment.
%
%   @bug    %*/ will be seen as the end of the comment.

end_of_comment -->
    eos.
end_of_comment -->
    ws, stars, "*/".

stars --> [].
stars --> "*", !, stars.


%!  take_prefix(+Prefixes:list(codes), +Indent0:int, -Indent:int)// is det.
%
%   Get the leading characters  from  the   input  and  compute  the
%   line-position at the end of the leading characters.

take_prefix(Prefixes, I0, I) -->
    { member(Prefix, Prefixes),
      string_codes(Prefix, PrefixCodes)
    },
    prefix(PrefixCodes),
    !,
    { string_update_linepos(PrefixCodes, I0, I) }.
take_prefix(_, I, I) -->
    [].

prefix([]) --> [].
prefix([H|T]) --> [H], prefix(T).

white_prefix(I0, I) -->
    [C],
    {  code_type(C, white),
       !,
       update_linepos(C, I0, I1)
    },
    white_prefix(I1, I).
white_prefix(I, I) -->
    [].

%!  string_update_linepos(+Codes, +Pos0, -Pos) is det.
%
%   Update line-position after adding Codes at Pos0.

string_update_linepos([], I, I).
string_update_linepos([H|T], I0, I) :-
    update_linepos(H, I0, I1),
    string_update_linepos(T, I1, I).

%!  update_linepos(+Code, +Pos0, -Pos) is det.
%
%   Update line-position after adding Code.
%
%   @tbd    Currently assumes tab-width of 8.

update_linepos(0'\t, I0, I) :-
    !,
    I is (I0\/7)+1.
update_linepos(0'\b, I0, I) :-
    !,
    I is max(0, I0-1).
update_linepos(0'\r, _, 0) :- !.
update_linepos(0'\n, _, 0) :- !.
update_linepos(_, I0, I) :-
    I is I0 + 1.

%!  take_line(-Line:codes)// is det.
%
%   Take  a  line  from  the  input.   Line  does  not  include  the
%   terminating \r or \n character(s), nor trailing whitespace.

take_line([]) -->
    "\r\n",
    !.                      % DOS file
take_line([]) -->
    "\n",
    !.                        % Unix file
take_line(Line) -->
    [H], { code_type(H, white) },
    !,
    take_white(White, WT),
    (   nl
    ->  { Line = [] }
    ;   { Line = [H|White] },
        take_line(WT)
    ).
take_line([H|T]) -->
    [H],
    !,
    take_line(T).
take_line([]) -->                       % end of string
    [].

take_white([H|T0], T) -->
    [H],  { code_type(H, white) },
    !,
    take_white(T0, T).
take_white(T, T) -->
    [].

%!  normalise_indentation(+LinesIn, -LinesOut) is det.
%
%   Re-normalise the indentation, such that the  lef-most line is at
%   zero.  Note that we skip empty lines in the computation.

normalise_indentation(Lines0, Lines) :-
    skip_empty_lines(Lines0, Lines1),
    Lines1 = [I0-_|Lines2],
    !,
    smallest_indentation(Lines2, I0, Subtract),
    (   Subtract == 0
    ->  Lines = Lines0
    ;   maplist(substract_indent(Subtract), Lines0, Lines)
    ).
normalise_indentation(Lines, Lines).

smallest_indentation([], I, I).
smallest_indentation([_-[]|T], I0, I) :-
    !,
    smallest_indentation(T, I0, I).
smallest_indentation([X-_|T], I0, I) :-
    I1 is min(I0, X),
    smallest_indentation(T, I1, I).

substract_indent(Subtract, I0-L, I-L) :-
    I is max(0,I0-Subtract).


                 /*******************************
                 *             MISC             *
                 *******************************/

%!  strip_leading_par(+Dom0, -Dom) is det.
%
%   Remove the leading paragraph for  environments where a paragraph
%   is not required.

strip_leading_par([p(C)|T], L) :-
    !,
    append(C, T, L).
strip_leading_par(L, L).


                 /*******************************
                 *           DCG BASICS         *
                 *******************************/

%!  ws// is det
%
%   Eagerly skip layout characters

ws -->
    [C], {code_type(C, space)},
    !,
    ws.
ws -->
    [].

%       space// is det
%
%       True if then next code is layout.

space -->
    [C],
    {code_type(C, space)}.

%!  nl//
%
%   Get end-of-line

nl -->
    "\r\n",
    !.
nl -->
    "\n".

%!  peek(H)//
%
%   True if next token is H without eating it.

peek(H, L, L) :-
    L = [H|_].

%!  tokens(-Tokens:list)// is nondet.
%!  tokens(+Max, -Tokens:list)// is nondet.
%
%   Defensively take tokens from the input.  Backtracking takes more
%   tokens.  Do not include structure terms.

tokens([]) --> [].
tokens([H|T]) --> token(H), tokens(T).

tokens(_, []) --> [].
tokens(C, [H|T]) --> token(H), {succ(C1, C)}, tokens(C1, T).

%!  tokens_no_whitespace(-Tokens:list(atom))// is nondet.
%
%   Defensively take tokens from the  input. Backtracking takes more
%   tokens.  Tokens  cannot  include  whitespace.  Word  tokens  are
%   returned as their represented words.

tokens_no_whitespace([]) -->
    [].
tokens_no_whitespace([Word|T]) -->
    [ w(Word) ],
    !,
    tokens_no_whitespace(T).
tokens_no_whitespace([H|T]) -->
    [H],
    { \+ space_token(H) },
    tokens_no_whitespace(T).

token(Token) -->
    [Token],
    { token(Token) }.

token(w(_)) :- !.
token(Token) :- atom(Token).

%!  limit(+Count, :Rule)//
%
%   As limit/2, but for grammar rules.

:- meta_predicate limit(+,2,?,?).

limit(Count, Rule, Input, Rest) :-
    Count > 0,
    State = count(0),
    call(Rule, Input, Rest),
    arg(1, State, N0),
    N is N0+1,
    (   N =:= Count
    ->  !
    ;   nb_setarg(1, State, N)
    ).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(pldoc(deprecated_tag(Name, Tag))) -->
    [ 'PlDoc: Deprecated tag @~w (use @~w)'-[Name, Tag]
    ].
prolog:message(pldoc(unknown_tag(Name))) -->
    [ 'PlDoc: unknown tag @~w'-[Name]
    ].
