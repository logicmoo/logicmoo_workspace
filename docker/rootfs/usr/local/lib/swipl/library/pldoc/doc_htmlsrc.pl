/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2017, University of Amsterdam
                              VU University Amsterdam
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

:- module(pldoc_htmlsrc,
          [ source_to_html/3            % +Source, +Out, +Options
          ]).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(prolog_colour)).
:- use_module(doc_colour).
:- use_module(doc_html).
:- use_module(doc_wiki).
:- use_module(doc_modes).
:- use_module(doc_process).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(prolog_xref)).

:- meta_predicate
    source_to_html(+, +, :).


/** <module> HTML source pretty-printer

This module colourises Prolog  source  using   HTML+CSS  using  the same
cross-reference based technology as used by PceEmacs.

@tbd    Create hyper-links to documentation and definitions.
@author Jan Wielemaker
*/

:- predicate_options(source_to_html/3, 3,
                     [ format_comments(boolean),
                       header(boolean),
                       skin(callable),
                       stylesheets(list),
                       title(atom)
                     ]).


:- thread_local
    lineno/0,                       % print line-no on next output
    nonl/0,                         % previous tag implies nl (block level)
    id/1.                           % Emitted ids

%!  source_to_html(+In:filename, +Out, :Options) is det.
%
%   Colourise Prolog source as HTML. The idea   is to first create a
%   sequence of fragments and  then  to   apply  these  to the code.
%   Options are:
%
%     * format_comments(+Boolean)
%     If =true= (default), use PlDoc formatting for structured
%     comments.
%
%   Other options are passed to the following predicates:
%
%     * print_html_head/2
%     * print_html_footer/2.
%     * html_fragments/6
%
%   @arg In         A filename.  Can also be an abstract name,
%                   which is subject to library(prolog_source)
%                   abstract file handling. See
%                   prolog_open_source/2.  Note that this cannot
%                   be a stream as we need to read the file three
%                   times: (1) xref, (2) assign colours and (3)
%                   generate HTML.
%   @arg Out        Term stream(Stream) or filename specification

source_to_html(Src, stream(Out), MOptions) :-
    !,
    meta_options(is_meta, MOptions, Options),
    (   option(title(_), Options)
    ->  HeadOptions = Options
    ;   file_base_name(Src, Title),
        HeadOptions = [title(Title)|Options]
    ),
    retractall(lineno),             % play safe
    retractall(nonl),               % play safe
    retractall(id(_)),
    colour_fragments(Src, Fragments),
    setup_call_cleanup(
        ( open_source(Src, In),
          asserta(user:thread_message_hook(_,_,_), Ref)
        ),
        ( print_html_head(Out, HeadOptions),
          html_fragments(Fragments, In, Out, [], State, Options),
          copy_rest(In, Out, State, State1),
          pop_state(State1, Out, In)
        ),
        ( erase(Ref),
          close(In)
        )),
    print_html_footer(Out, Options).
source_to_html(Src, FileSpec, Options) :-
    absolute_file_name(FileSpec, OutFile, [access(write)]),
    setup_call_cleanup(
        open(OutFile, write, Out, [encoding(utf8)]),
        source_to_html(Src, stream(Out), Options),
        close(Out)).

open_source(Id, Stream) :-
    prolog:xref_open_source(Id, Stream),
    !.
open_source(File, Stream) :-
    open(File, read, Stream).

is_meta(skin).

%!  print_html_head(+Out:stream, +Options) is det.
%
%   Print the =DOCTYPE= line and HTML header.  Options:
%
%           * header(Bool)
%           Only print the header if Bool is not =false=
%
%           * title(Title)
%           Title of the HTML document
%
%           * stylesheets(List)
%           Reference to the CSS style-sheets.
%
%           * format_comments(Bool)
%           If =true= (default), format structured comments.
%
%           * skin(Closure)
%           Called using call(Closure, Where, Out), where Where
%           is one of =header= or =footer=.  These calls are made
%           just after opening =body= and before closing =body=.

print_html_head(Out, Options) :-
    option(header(true), Options, true),
    !,
    option(title(Title), Options, 'Prolog source'),
    http_absolute_location(pldoc_resource('pldoc.css'), PlDocCSS, []),
    http_absolute_location(pldoc_resource('pllisting.css'), PlListingCSS, []),
    option(stylesheets(Sheets), Options, [PlListingCSS, PlDocCSS]),
    format(Out, '<!DOCTYPE html>~n', []),
    format(Out, '<html>~n', []),
    format(Out, '  <head>~n', []),
    format(Out, '    <title>~w</title>~n', [Title]),
    forall(member(Sheet, Sheets),
           format(Out, '    <link rel="stylesheet" type="text/css" href="~w">~n', [Sheet])),
    format(Out, '  </head>~n', []),
    format(Out, '<body>~n', []),
    skin_hook(Out, header, Options).
print_html_head(Out, Options) :-
    skin_hook(Out, header, Options).

print_html_footer(Out, Options) :-
    option(header(true), Options, true),
    !,
    skin_hook(Out, footer, Options),
    format(Out, '~N</body>~n', []),
    format(Out, '</html>', []).
print_html_footer(Out, Options) :-
    skin_hook(Out, footer, Options).

skin_hook(Out, Where, Options) :-
    option(skin(Skin), Options),
    call(Skin, Where, Out),
    !.
skin_hook(_, _, _).


%!  html_fragments(+Fragments, +In, +Out, +State, +Options) is det.
%
%   Copy In to Out, inserting HTML elements using Fragments.

html_fragments([], _, _, State, State, _).
html_fragments([H|T], In, Out, State0, State, Options) :-
    html_fragment(H, In, Out, State0, State1, Options),
    html_fragments(T, In, Out, State1, State, Options).

%!  html_fragment(+Fragment, +In, +Out,
%!                +StateIn, -StateOut, +Options) is det.
%
%   Print from current position upto the end of Fragment.  First
%   clause deals with structured comments.

html_fragment(fragment(Start, End, comment(structured), []),
              In, Out, State0, [], Options) :-
    option(format_comments(true), Options, true),
    !,
    copy_without_trailing_white_lines(In, Start, Out, State0, State1),
    pop_state(State1, Out, In),
    Len is End - Start,
    read_n_codes(In, Len, Comment),
    is_structured_comment(Comment, Prefix),
    indented_lines(Comment, Prefix, Lines0),
    (   section_comment_header(Lines0, Header, Lines1)
    ->  wiki_lines_to_dom(Lines1, [], DOM),
        phrase(pldoc_html:html(div(class(comment),
                                   [Header|DOM])), Tokens),
        print_html(Out, Tokens)
    ;   stream_property(In, file_name(File)),
        line_count(In, Line),
        (   xref_module(File, Module)
        ->  true
        ;   Module = user
        ),
        process_modes(Lines0, Module, File:Line, Modes, Args, Lines1),
        maplist(assert_seen_mode, Modes),
        DOM = [\pred_dt(Modes, pubdef, []), dd(class=defbody, DOM1)],
        wiki_lines_to_dom(Lines1, Args, DOM0),
        strip_leading_par(DOM0, DOM1),
        phrase(pldoc_html:html(DOM), Tokens),               % HACK
        format(Out, '<dl class="comment">~n', []),
        print_html(Out, Tokens),
        format(Out, '</dl>~n', [])
    ).
html_fragment(fragment(Start, End, structured_comment, []),
              In, Out, State0, State, _Options) :-
    !,
    copy_to(In, Start, Out, State0, State1),
    line_count(In, StartLine),
    Len is End - Start,
    read_n_codes(In, Len, Comment),
    is_structured_comment(Comment, Prefix),
    indented_lines(Comment, Prefix, Lines),
    (   section_comment_header(Lines, _Header, _RestSectionLines)
    ->  true
    ;   stream_property(In, file_name(File)),
        line_count(In, Line),
        (   xref_module(File, Module)
        ->  true
        ;   Module = user
        ),
        process_modes(Lines, Module, File:Line, Modes, _Args, _Lines1),
        maplist(mode_anchor(Out), Modes)
    ),
    start_fragment(structured_comment, In, Out, State1, State2),
    copy_codes(Comment, StartLine, Out, State2, State3),
    end_fragment(Out, In, State3, State).
html_fragment(fragment(Start, End, Class, Sub),
              In, Out, State0, State, Options) :-
    copy_to(In, Start, Out, State0, State1),
    start_fragment(Class, In, Out, State1, State2),
    html_fragments(Sub, In, Out, State2, State3, Options),
    copy_to(In, End, Out, State3, State4),  % TBD: pop-to?
    end_fragment(Out, In, State4, State).

start_fragment(atom, In, Out, State0, State) :-
    !,
    (   peek_code(In, C),
        C == 39
    ->  start_fragment(quoted_atom, In, Out, State0, State)
    ;   State = [nop|State0]
    ).
start_fragment(Class, _, Out, State, [Push|State]) :-
    element(Class, Tag, CSSClass),
    !,
    Push =.. [Tag,class(CSSClass)],
    (   anchor(Class, ID)
    ->  format(Out, '<~w id="~w" class="~w">', [Tag, ID, CSSClass])
    ;   format(Out, '<~w class="~w">', [Tag, CSSClass])
    ).
start_fragment(Class, _, Out, State, [span(class(SpanClass))|State]) :-
    functor(Class, SpanClass, _),
    format(Out, '<span class="~w">', [SpanClass]).

end_fragment(_, _, [nop|State], State) :- !.
end_fragment(Out, In, [span(class(directive))|State], State) :-
    !,
    copy_full_stop(In, Out),
    format(Out, '</span>', []),
    (   peek_code(In, 10),
        \+ nonl
    ->  assert(nonl)
    ;   true
    ).
end_fragment(Out, _, [Open|State], State) :-
    retractall(nonl),
    functor(Open, Element, _),
    format(Out, '</~w>', [Element]).

pop_state([], _, _) :- !.
pop_state(State, Out, In) :-
    end_fragment(Out, In, State, State1),
    pop_state(State1, Out, In).


%!  anchor(+Class, -Label) is semidet.
%
%   True when Label is the =id= we   must  assign to the fragment of
%   class Class. This that  the  first   definition  of  a head with
%   the id _name/arity_.

anchor(head(_, Head), Id) :-
    callable(Head),
    functor(Head, Name, Arity),
    format(atom(Id), '~w/~w', [Name, Arity]),
    (   id(Id)
    ->  fail
    ;   assertz(id(Id))
    ).

mode_anchor(Out, Mode) :-
    mode_anchor_name(Mode, Id),
    (   id(Id)
    ->  true
    ;   format(Out, '<span id="~w"><span>', [Id]),
        assertz(id(Id))
    ).

assert_seen_mode(Mode) :-
    mode_anchor_name(Mode, Id),
    (   id(Id)
    ->  true
    ;   assertz(id(Id))
    ).

%!  copy_to(+In:stream, +End:int, +Out:stream, +State) is det.
%
%   Copy data from In to Out   upto  character-position End. Inserts
%   HTML entities for HTML the reserved characters =|<&>|=. If State
%   does not include a =pre= environment,   create  one and skip all
%   leading blank lines.

copy_to(In, End, Out, State, State) :-
    member(pre(_), State),
    !,
    copy_to(In, End, Out).
copy_to(In, End, Out, State, [pre(class(listing))|State]) :-
    format(Out, '<pre class="listing">~n', []),
    line_count(In, Line0),
    read_to(In, End, Codes0),
    delete_leading_white_lines(Codes0, Codes, Line0, Line),
    assert(lineno),
    write_codes(Codes, Line, Out).

copy_codes(Codes, Line, Out, State, State) :-
    member(pre(_), State),
    !,
    write_codes(Codes, Line, Out).
copy_codes(Codes0, Line0, Out, State, State) :-
    format(Out, '<pre class="listing">~n', []),
    delete_leading_white_lines(Codes0, Codes, Line0, Line),
    assert(lineno),
    write_codes(Codes, Line, Out).


%!  copy_full_stop(+In, +Out) is det.
%
%   Copy upto and including the .

copy_full_stop(In, Out) :-
    get_code(In, C0),
    copy_full_stop(C0, In, Out).

copy_full_stop(0'., _, Out) :-
    !,
    put_code(Out, 0'.).
copy_full_stop(C, In, Out) :-
    put_code(Out, C),
    get_code(In, C2),
    copy_full_stop(C2, In, Out).


%!  delete_leading_white_lines(+CodesIn, -CodesOut, +LineIn, -Line) is det.
%
%   Delete leading white lines. Used  after structured comments. The
%   last two arguments update the  start-line   number  of the <pre>
%   block that is normally created.

delete_leading_white_lines(Codes0, Codes, Line0, Line) :-
    append(LineCodes, [10|Rest], Codes0),
    all_spaces(LineCodes),
    !,
    Line1 is Line0 + 1,
    delete_leading_white_lines(Rest, Codes, Line1, Line).
delete_leading_white_lines(Codes, Codes, Line, Line).

%!  copy_without_trailing_white_lines(+In, +End, +StateIn, -StateOut) is det.
%
%   Copy input, but skip trailing white-lines. Used to copy the text
%   leading to a structured comment.

copy_without_trailing_white_lines(In, End, Out, State, State) :-
    member(pre(_), State),
    !,
    line_count(In, Line),
    read_to(In, End, Codes0),
    delete_trailing_white_lines(Codes0, Codes),
    write_codes(Codes, Line, Out).
copy_without_trailing_white_lines(In, End, Out, State0, State) :-
    copy_to(In, End, Out, State0, State).

delete_trailing_white_lines(Codes0, []) :-
    all_spaces(Codes0),
    !.
delete_trailing_white_lines(Codes0, Codes) :-
    append(Codes, Tail, [10|Rest], Codes0),
    !,
    delete_trailing_white_lines(Rest, Tail).
delete_trailing_white_lines(Codes, Codes).

%!  append(-First, -FirstTail, ?Rest, +List) is nondet.
%
%   Split List.  First part is the difference-list First-FirstTail.

append(T, T, L, L).
append([H|T0], Tail, L, [H|T]) :-
    append(T0, Tail, L, T).

all_spaces([]).
all_spaces([H|T]) :-
    code_type(H, space),
    all_spaces(T).

copy_to(In, End, Out) :-
    line_count(In, Line),
    read_to(In, End, Codes),
    (   debugging(htmlsrc)
    ->  length(Codes, Count),
        debug(htmlsrc, 'Copy ~D chars: ~s', [Count, Codes])
    ;   true
    ),
    write_codes(Codes, Line, Out).

read_to(In, End, Codes) :-
    character_count(In, Here),
    Len is End - Here,
    read_n_codes(In, Len, Codes).

%!  write_codes(+Codes, +Line, +Out) is det.
%
%   Write codes that have been read starting at Line.

write_codes([], _, _).
write_codes([H|T], L0, Out) :-
    content_escape(H, Out, L0, L1),
    write_codes(T, L1, Out).

%!  content_escape(+Code, +Out, +Line0, -Line) is det
%
%   Write Code to Out, while taking care of.
%
%           * Use HTML entities for =|<&>|=
%           * If a line-no-tag is requested, write it
%           * On \n, post a line-no request.  If nonl/0 is set,
%             do _not_ emit a newline as it is implied by the
%             closed environment.

content_escape(_, Out, L, _) :-
    (   lineno
    ->  retractall(lineno),
        write_line_no(L, Out),
        fail
    ;   fail
    ).
content_escape(0'\n, Out, L0, L) :-
    !,
    L is L0 + 1,
    (   retract(nonl)
    ->  true
    ;   nl(Out)
    ),
    assert(lineno).
content_escape(0'<, Out, L, L) :-
    !,
    format(Out, '&lt;', []).
content_escape(0'>, Out, L, L) :-
    !,
    format(Out, '&gt;', []).
content_escape(0'&, Out, L, L) :-
    !,
    format(Out, '&amp;', []).
content_escape(C, Out, L, L) :-
    put_code(Out, C).

write_line_no(LineNo, Out) :-
    format(Out, '<span class="line-no">~|~t~d~5+</span>', [LineNo]).

%!  copy_rest(+In, +Out, +StateIn, -StateOut) is det.
%
%   Copy upto the end of the input In.

copy_rest(In, Out, State0, State) :-
    copy_to(In, -1, Out, State0, State).

%!  read_n_codes(+In, +N, -Codes)
%
%   Read the next N codes from In as a list of codes. If N < 0, read
%   upto the end of stream In.

read_n_codes(_, N, Codes) :-
    N =< 0,
    !,
    Codes = [].
read_n_codes(In, N, Codes) :-
    get_code(In, C0),
    read_n_codes(N, C0, In, Codes).

read_n_codes(_, -1, _, []) :- !.
read_n_codes(1, C, _, [C]) :- !.
read_n_codes(N, C, In, [C|T]) :-
    get_code(In, C2),
    N2 is N - 1,
    read_n_codes(N2, C2, In, T).


%!  element(+Class, -HTMLElement, -CSSClass) is nondet.
%
%   Map classified objects to an  HTML   element  and CSS class. The
%   actual  clauses  are  created   from    the   1st   argument  of
%   prolog_src_style/2.

term_expansion(element(_,_,_), Clauses) :-
    findall(C, element_clause(C), Clauses).

%element_tag(directive, div) :- !.
element_tag(_, span).

element_clause(element(Term, Tag, CSS)) :-
    span_term(Term, CSS),
    element_tag(Term, Tag).

span_term(Classification, Class) :-
    syntax_colour(Classification, _Attributes),
    css_class(Classification, Class).

css_class(Class, Class) :-
    atom(Class),
    !.
css_class(Term, Class) :-
    Term =.. [P1,A|_],
    (   var(A)
    ->  Class = P1
    ;   css_class(A, P2),
        atomic_list_concat([P1, -, P2], Class)
    ).

element(_,_,_).                         % term expanded


