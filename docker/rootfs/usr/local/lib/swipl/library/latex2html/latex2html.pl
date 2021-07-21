/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1997-2020, University of Amsterdam
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

:- module(tex,
          [ welcome/0,
            latex2html/1,               % +BaseName
            latex2html/2,               % +BaseName, +Options
            macro_expand/2,             % +In, -Out
            translate/4,                % +In, +ModeIn, -ModeOut, -Out
            translate/3,                % +In, +ModeIn, -Out
            latex2html_module/0,        % Register extension module
            tex_input_directory/1,      % +Input Dir
            step_counter/2,             % +Name, -NewValue
            ps2gif/3,                   % +PsFile, -GifFile, +Options
            translate_command/4,        % +In, +ModeIn, -ModeOut, -HTML
            translate_environment/4,    % +In, +ModeIn, -ModeOut, -HTML
            translate_reference/4,      % +Kind, +RefPrefix, +Label, -HTML
            translate_table/3,          % +Format, +BodyTokens, -HTML
            translate_section/4,        % +Level, -/*, +Title, -HTML
            translate_section/5,        % +Level, -/*, +Title, -HTML, +FileBase
            current_setting/1,          % +Type(-Value ...)
            do_float/2,                 % +Float(+Number), :Goal
            tex_load_commands/1,        % +BaseName
            add_to_index/1,             % +Term
            add_to_index/2,             % +Term, +Tag
            clean_tt/2,                 % +Raw, -Clean
            op(100, fx, #)
          ]).
:- autoload(library(apply),[maplist/3]).
:- autoload(library(backcomp),[convert_time/8]).
:- autoload(library(ctypes),
	    [is_lower/1,to_upper/2,is_alpha/1,is_upper/1,to_lower/2]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(filesex),[copy_file/2,directory_file_path/3]).
:- autoload(library(gui_tracer),[gtrace/0]).
:- autoload(library(lists),
	    [ reverse/2,
	      member/2,
	      flatten/2,
	      append/3,
	      select/3,
	      nth1/3,
	      last/2
	    ]).
:- autoload(library(main),[main/0,argv_options/3]).
:- autoload(library(occurs),[sub_term/2]).
:- autoload(library(option),[option/2,select_option/3]).
:- autoload(library(readutil),[read_file_to_codes/3]).
:- autoload(library(statistics),[statistics/0]).

ltx2htm_version('0.98').                % for SWI-Prolog 5.6.18

page_header('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" \c
               "http://www.w3.org/TR/html4/strict.dtd">\n\n').
:- dynamic
    html_output_dir/1,              % output relative to this dir
    tex_file_base/1,                % Basename of the main file
    html_file_base/1,               % Basename of main output file
    html_split_level/1,             % Split upto this level
    bodycolor/1,                    % \bodycolor storage
    title/1,                        % \title{} storage
    author/1,                       % \auther{} command storage
    link_image/2,                   % Id, Image
    center_tables/0,
    quiet/0.

:- discontiguous
    cmd/2,
    cmd/3,
    cmd/4.


html_split_level(2).
html_file_base('Title').

:- multifile
    user:file_search_path/2.
:- dynamic
    user:file_search_path/2.

add_my_path :-
    user:file_search_path(latex2html, _),
    !.
add_my_path :-
    prolog_load_context(directory, Dir),
    asserta(user:file_search_path(latex2html, Dir)).

:- initialization(add_my_path, now).

user:file_search_path(psfig, tex(figs)).
user:file_search_path(includegraphics, tex(figs)).
user:file_search_path(includegraphics, tex(.)).
user:file_search_path(tex, '.').
user:file_search_path(img, '.').
user:file_search_path(img, icons).
user:file_search_path(img, latex2html(icons)).

:- use_foreign_library(foreign(tex)).

% support list_strings/0
:- multifile check:valid_string_goal/1.

check:valid_string_goal(tex:delete_all(_,S,_)) :- string(S).
check:valid_string_goal(tex:replace_all(_,S1,S2,_)) :- string(S1), string(S2).
check:valid_string_goal(tex:string_without(S,_,_,_)) :- string(S).


                 /*******************************
                 *         TEX INPUTS           *
                 *******************************/

read_tex_inputs :-
    getenv('TEXINPUTS', Val),
    !,
    read_tex_inputs(Val).
read_tex_inputs.

read_tex_inputs(Val) :-
    split(Val, 0':, PathElement),
    retractall(user:file_search_path(tex, _)),
    reverse(PathElement, RevPath),
    forall(member(E, RevPath), assert_tex_input(E)).

assert_tex_input('') :-
    !,
    retractall(user:file_search_path(tex, '.')),
    asserta(user:file_search_path(tex, '.')).
assert_tex_input(Dir) :-
    retractall(user:file_search_path(tex, Dir)),
    asserta(user:file_search_path(tex, Dir)).


                 /*******************************
                 *       EXTENSION MODULES      *
                 *******************************/

:- dynamic
    tex_extension_module/1.

tex_extension_module(tex).

:- module_transparent
    latex2html_module/0.

latex2html_module :-
    context_module(M),
    (   tex:tex_extension_module(M)
    ->  true
    ;   asserta(tex:tex_extension_module(M))
    ),
    M:dynamic((cmd/2, cmd/3, cmd/4, env/2, (#)/2)),
    M:discontiguous((cmd/2, cmd/3, cmd/4, env/2, (#)/2)),
    M:dynamic((list_command/4)).

%       Load a tex command file

tex_load_commands(File) :-
    (   package_alias(File, Alias),
        member(Term, [tex(Alias), latex2html(Alias)]),
        absolute_file_name(Term,
                           CmdFile,
                           [ extensions([cmd]),
                             access(read),
                             file_errors(fail)
                           ])
    ->  tex_read_commands(CmdFile),
        (   quiet
        ->  true
        ;   format(user_error, 'Loaded LaTeX commands from "~w"~n', [CmdFile])
        )
    ;   format(user_error,
               'Can not find command file "~w"~n', [File]),
        fail
    ).

tex_input_directory(D) :-
    asserta(user:file_search_path(tex, D)).

%       current_setting(?Term)
%
%       Report current settings to the extension module

current_setting(keep_figures) :-
    keep_figures(true).
current_setting(html_output_dir(Dir)) :-
    html_output_dir(Dir).


                 /*******************************
                 *            SETTINGS          *
                 *******************************/

:- dynamic
    keep_figures/1,
    onefile/1,
    makeindex/1,
    title/1,
    auther/1.

keep_figures(true).
onefile(false).
makeindex(false).
title('No title').
author('Anonymous').


                 /*******************************
                 *            TOPLEVEL          *
                 *******************************/


run_latex2html(TeXFile) :-
    reset_counters,
    reset_sections,
    reset_index,
    reset_labels,
    reset_cites,
    reset_output,
    tex_tokens(TeXFile, TeXTokens),
    (   member(env(document, _, _), TeXTokens)
    ->  translate(TeXTokens, preamble, HTML)
    ;   format(user_error,
               'No document environment; processing anyway~n', []),
        translate(TeXTokens, document, HTML)
    ),
    once(html_file_base(Base)),
    expand_macros([tell(Base), #header], Header),
    make_index(Index),
    flatten([ Header,
              HTML,
              Index
            ],
            HtmlDocument),
    collect_labels(HtmlDocument, Base),
    write_html(HtmlDocument),
    close_output.


latex2html(Spec) :-
    latex2html(Spec, []).

latex2html(Spec, Options) :-
    welcome(Options),
    tex_load_commands(latex),
    file_name_extension(Spec, tex,  TeXFile),
    file_name_extension(Base, tex,  TeXFile),
    absolute_file_name(tex(TeXFile),
                       [ access(read)
                       ],
                       TheTeXFile),
    asserta(html_output_dir(Base)),
    asserta(tex_file_base(Base)),
    run_latex2html(TheTeXFile),
    goodbye(Options),
    retract(tex_file_base(_)),
    retract(html_output_dir(_)).

%       make_output_directory
%
%       Create the output directory.

:- dynamic
    done_make_output_directory/0.

make_output_directory :-
    done_make_output_directory,
    !.
make_output_directory :-
    html_output_dir(Dir),
    !,
    (   exists_directory(Dir)
    ->  true
    ;   make_directory(Dir)
    ),
    assert(done_make_output_directory).

:- dynamic
    current_html_output_db/1,       % Base
    in_head/1.

reset_output :-
    retractall(current_html_output_db(_)),
    retractall(in_head(_)).

open_output(Base) :-
    make_output_directory,
    html_output_dir(Dir),
    !,
    atomic_list_concat([Dir, /, Base, '.html'], HtmlFile),
    tex_tell(HtmlFile),
    page_header(Header),
    put_html_token(html(Header)),
    put_html_token(html('<html>')),
    asserta(current_html_output_db(Base)).

close_output :-
    (   retract(current_html_output_db(_))
    ->  put_html_token(html('</body>')),
        put_html_token(html('</html>'))
    ;   true
    ),
    tex_told.

current_html_output(Raw) :-
    current_html_output_db(File),
    !,
    File = Raw.

welcome :-
    welcome([]).

welcome(Options) :-
    option(quiet(true), Options),
    !.
welcome(_Options) :-
    ltx2htm_version(Version),
    format(user_error, 'Welcome to LaTeX2HTML version ~w~n~n', [Version]).

goodbye(Options) :-
    option(quiet(true), Options),
    !.
goodbye(_Options) :-
    html_output_dir(Dir),
    html_file_base(Html),
    format(user_error, '~*t~72|~n', [0'*]),
    format(user_error,
           'Translation completed; output written to "~w/~w.html".~n',
           [Dir, Html]),
    format(user_error, 'Prolog statistics:~n~n', []),
    statistics.

%       translate(+TeX, +Mode, -HTML)

translate(TeX, Mode, HTML) :-
    translate(TeX, Mode, _, HTML).

translate([], Mode, Mode, []) :- !.
translate([\(Section, -, [{Title}])|T], Mode, Mode, HTML) :-
    trans_section(Section, Title, T, TitleHtml),
    !,
    append(TitleHtml, BodyHtml, HTML),
    translate(T, Mode, Mode, BodyHtml).
translate(['`','`'|T0], Mode0, Mode, [html(' &ldquo;')|T]) :-
    \+ in_macro(code(_)),
    !,
    translate(T0, Mode0, Mode, T).
translate(['\'','\''|T0], Mode0, Mode, [html('&rdquo; ')|T]) :-
    \+ in_macro(code(_)),
    !,
    translate(T0, Mode0, Mode, T).
translate([H0|T0], Mode0, Mode, [H|T]) :-
    !,
    translate_1(H0, Mode0, Mode1, H),
    translate(T0, Mode1, Mode, T).
translate(X, Mode, Mode, []) :-
    format(user_error, 'translate(~p) failed in mode "~w"~n', [X, Mode]).

translate_1(Term, Mode0, Mode, HTML) :-
    (   translate_2(Term, Mode0, Mode, HTML)
    ->  true
    ;   format(user_error, 'translate_2(~p, ~w, ...) failed~n',
               [Term, Mode0]),
        Mode = Mode0,
        HTML = []
    ).

translate_2(\(Cmd, Mod, Args), Mode0, Mode, HTML) :-    % \cmd*{Args}
    Term =.. [Cmd, Mod|Args],
    translate_command(Term, Mode0, Mode, HTML).
translate_2(\(Cmd, Args), Mode0, Mode, HTML) :-         % \cmd{Args}
    Term =.. [Cmd|Args],
    translate_command(Term, Mode0, Mode, HTML).
translate_2(\Cmd, Mode0, Mode, HTML) :-                 % \cmd
    translate_command(Cmd, Mode0, Mode, HTML).
translate_2(env(Env, Args, Bdy), Mode0, Mode, HTML) :-  % \begin{x} ... \end{x}
    Term =.. [Env, Args, Bdy],
    translate_environment(Term, Mode0, Mode, HTML).
translate_2($$(Expr), Mode, Mode, #quote(#var(HTML))) :-% $$...$$
    tex_atom_to_tokens(Expr, Tokens),
    translate(Tokens, math, _, HTML).
translate_2($(Expr), Mode, Mode, #var(HTML)) :-         % $...$
    tex_atom_to_tokens(Expr, Tokens),
    translate(Tokens, math, _, HTML).
translate_2(verbatim(_Cmd, Text), Mode, Mode,           % \begin{verbatim} ...
            #pre(code, pre(Text))).
translate_2(verb(_, Text), pcecode, pcecode, pre(Text)).
translate_2(Layout, pcecode, pcecode, []) :-
    atomic(Layout),
    !.
translate_2('\n', Mode, Mode, [html('<br>')]) :-
    Mode = group(Atts),
    memberchk(obeylines, Atts),
    !.
translate_2(verb(_, Text), Mode, Mode, #code(Text)).    % \verbX...X
translate_2([Atom], Mode, Mode, nospace(Atom)) :-       % {foo}
    atomic(Atom).
translate_2(Group, Mode, Mode, HTML) :-                 % {...}
    Group = [_|_],
    translate_group(Group, HTML).
translate_2(~, Mode, Mode, html('&nbsp;')).             % ~
translate_2('`', Mode, Mode, html('&lsquo;')).          % `
translate_2('\'', Mode, Mode, html('&rsquo;')).         % '
translate_2(~, Mode, Mode, html('&nbsp;')).             % ~
translate_2('\\[]', Mode, Mode, html('[]')).            % []
translate_2(Atom0, Mode, Mode, Atom) :-                 % Normal word
    atomic(Atom0),
    (   Mode = group(Atts),
        memberchk(font(sc), Atts)
    ->  upcase_atom(Atom0, Atom)
    ;   Atom = Atom0
    ).

translate_group(Group, [HTML, Close]) :-                % {...}
    translate(Group, group([]), MEnd, HTML),
    (   MEnd = group(Attributes),
        member(font(EndFont), Attributes),
        html_font(EndFont, _, Close)
    ->  true
    ;   Close = []
    ).

translate_command(Cmd, Mode0, Mode, HTML) :-
    translate_cmd(Cmd, Mode0, Mode, HTML0),
    expand_macros(HTML0, HTML).

translate_cmd(Cmd, Mode0, Mode, HTML) :-
    tex_extension_module(M),
    M:cmd(Cmd, Mode0, Mode, HTML),
    !.
translate_cmd(Cmd, Mode, Mode, HTML) :-
    tex_extension_module(M),
    M:cmd(Cmd, Mode, HTML),
    !.
translate_cmd(Cmd, Mode, Mode, HTML) :-
    tex_extension_module(M),
    M:cmd(Cmd, HTML),
    !.
translate_cmd(Cmd, Mode, Mode, []) :-
    functor(Cmd, Name, _),
    format(user_error,
           'Failed to translate \\~w in mode "~w"~n', [Name, Mode]),
    format(user_error, 'Term: "~p"~n', [Cmd]),
    true.

translate_environment(Env, Mode, Mode, HTML) :-
    translate_env(Env, Mode, Mode, HTML0),
    expand_macros(HTML0, HTML).

translate_env(Env, Mode, Mode, HTML) :-
    tex_extension_module(M),
    M:env(Env, HTML),
    !.
translate_env(Env, Mode, Mode, []) :-
    functor(Env, Name, _),
    format(user_error,
           'Failed to translate \\begin{~w} ... \\end{~w}~n',
           [Name, Name]),
%       format(user_error, 'Term: "~p"~n', [Env]),
    true.


                 /*******************************
                 *             LANGUAGE         *
                 *******************************/

language_map(figure,    'Figure').
language_map(table,     'Table').


                 /*******************************
                 *            # MACROS          *
                 *******************************/

:- dynamic
    in_anchor/0.                    % avoid nesting anchors

%       #(+Macro, -Expansion)
%       Do HTML macro expansion on the fly.

#(tell(_File),          []) :- onefile(true).
#(tell(File),           tell(File)).
#(head(Head),           [html('<head>'), HtmlHead, Style, html('</head>')]) :-
    expand_macros(Head, HtmlHead),
    expand_macros(#style, Style).
#(style,                [ html('<style type="text/css">'),
                          html(Style),
                          html('</style>')
                        ]) :-
    absolute_file_name(latex2html('latex2html.css'), StyleFile,
                       [ access(read) ]),
    read_file_to_codes(StyleFile, Codes, []),
    atom_codes(Style, Codes).
#(beginbody,            html(Body)) :-
    bodycolor(Colour),
    !,
    format(string(Body), '<body style="background:~w">', [Colour]).
#(beginbody,            html('<body>')).
#(endbody,              html('</body>')).
#(thetitle,             Title) :-
    title(Title).
#(theauthor,            Author) :-
    author(Author).
#(nameof(Type),         Name) :-
    language_map(Type, Name).
#(title(Text),          [html('<title>'),  Text, html('</title>')]).
#(var(Text),            [html('<var>'),    Text, html('</var>')]).
#(code(Text),           [html('<code>'),   Text, html('</code>')]).
#(pre(Text),            [html('<pre>'),    Text, html('</pre>')]).
#(pre(Class, Text),     [html(Begin),      Text, html('</pre>')]) :-
    format(atom(Begin), '<pre class="~w">', [Class]).
#(xmp(Text),            [html('<xmp>'),    Text, html('</xmp>')]).
#(strong(Text),         [html('<strong>'), Text, html('</strong>')]).
#(em(Text),             [html('<em>'),     Text, html('</em>')]).
#(b(Text),              [html('<b>'),      Text, html('</b>')]).
#(i(Text),              [html('<i>'),      Text, html('</i>')]).
#(tt(Text),             [html('<code>'),   Text, html('</code>')]).
#(sc(Text),             [html('<span style="font-variant:small-caps">'),
                                           Text, html('</span>')]).
#(div(Class, Text),     [html(Begin), Text, html('</div>')]) :-
    format(atom(Begin), '<div class="~w">', [Class]).
#(span(Class, Text),    [html(Begin), Text, html('</span>')]) :-
    format(atom(Begin), '<span class="~w">', [Class]).
#(center(Text),         [ html('<div style="text-align:center">'),
                          Text,
                          html('</div>')
                        ]).
#(navigate(Text),       [html('<div class="navigate">'), Text, html('</div>')]).
#(right(Text),          [html('<right>'),  Text, html('</right>')]).
#(quote(Text),          [html('<blockquote>'), Text, html('</blockquote>')]).
#(listing(Text),        [html('<p><table width="90%" align=center border=6 bgcolor="#e0e0e0"><tr><td nowrap>'), Text,
                         html('</table>')]).
#(abstract(Text),       [ html('<div class="abstract">'),
                          html('<div class="abstract-title">Abstract</div>'),
                          Text,
                          html('</div>')
                        ]).
#(footnote(Text),       #footnote(Mark, Text)) :-
    step_counter(footnote, Mark).
#(footnote(Mark, Text), [ html('<sup class="fn">'), Mark,
                          #span('fn-text', Text),
                          html('</sup>')
                        ]).
#(embrace(OC,Text),     [nospace(OA), Text, nospace(CA)]) :-
    string_codes(OC, [O,C]),
    char_code(OA, O),
    char_code(CA, C).
#(embrace(Text),        #embrace("()", Text)).
#(h(Level, Title),      [html(OpenH), Title, html(CloseH)]) :-
    h(Level, OpenH, CloseH).
#(h(Level, NumRef, Title), [h(Level, NumRef, Title), Title, html(CloseH)]) :-
    h(Level, _OpenH, CloseH).
#(predref(RN, Arity),   #lref(pred, Label, Text)) :-
    clean_tt(RN, Name),
    format(string(Text), '~w/~w', [Name, Arity]),
    latex2html4pl:predicate_refname(Name, Arity, Label).
#(dcgref(RN, Arity),   #lref(pred, Label, Text)) :-
    clean_tt(RN, Name),
    format(string(Text), '~w//~w', [Name, Arity]),
    latex2html4pl:dcg_refname(Name, Arity, Label).
#(funcref(RN, Arity),   #lref(function, Label, Text)) :-
    clean_tt(RN, Name),
    format(string(Text), '~w/~w', [Name, Arity]),
    latex2html4pl:function_refname(Name, Arity, Label).
#(row(Columns),         [html('<tr>'), HtmlCols, html('</tr>')]) :-
    add_td(Columns, HtmlCols).
#(label(Lbl, Text, Tag),label(ALabel, Expanded, Tag)) :-
    atom_string(ALabel, Lbl),
    expand_macros(Text, Expanded).
#(label(Lbl, Text),     label(ALabel, Expanded, -)) :-
    atom_string(ALabel, Lbl),
    expand_macros(Text, Expanded).
#(lref(Label, Text), Expanded) :-
    !,
    #(lref('', Label, Text), Expanded).
#(lref(_, _Label, Text),        Text) :-
    in_anchor,
    !.
#(lref(Class, Label, Text),     lref(Class, ALabel, Expanded)) :-
    canonicalise_label(Label, ALabel),
    asserta(in_anchor),
    expand_macros(Text, Expanded),
    retractall(in_anchor).
#(iflref(Label, Text),  iflref(ALabel, Expanded)) :-
    canonicalise_label(Label, ALabel),
    expand_macros(Text, Expanded).
#(url(_URL, Text),      Text) :-
    in_anchor,
    !.
#(url(URL, Text),       [html(Anchor), Expanded, html('</a>')]) :-
    format(string(Anchor), '<a class="url" href="~w">', URL),
    setup_call_cleanup(
        asserta(in_anchor, Ref),
        expand_macros(Text, Expanded),
        erase(Ref)).
#(cite(Key),            [ html('<cite>'),
                          Cites,
                          html('</cite>')
                        ]) :-
    cite_references(Key, cite, Cites).
#(opencite(Key),        [ html('<cite>'),
                          Cites,
                          html('</cite>')
                        ]) :-
    cite_references(Key, cite, Cites).
#(yearcite(Key),        [ html('<cite>'),
                          Cites,
                          html('</cite>')
                        ]) :-
    cite_references(Key, yearcite, Cites).
#(nocite(_), []).
#(header,               HTML) :-
    node_header(HTML).
#(header(Tag),          HTML) :-
    node_header(Tag, HTML).
#(footer(Tag),          HTML) :-
    node_footer(Tag, HTML).
#(next_and_prev_references,
        [ #iflref(prevfile,         '[Previous]'), ' ',
          #iflref(nextfile,         '[Next]')
        ]).
#(home_reference,
        [ #iflref(home,             '[Home]')
        ]).
#(Macro, []) :-
    format(user_error,
           'Post-processing macro #~p could not be expanded~n',
           [Macro]),
    gtrace, fail.

add_td([], []).
add_td([H|T0], [html('<td>'), H, html('</td>')|T]) :-
    add_td(T0, T).

cite_references(KeyIn, Functor, Refs) :-
    split(KeyIn, 0',, Keys),
    make_cite_references(Keys, Functor, Refs).

make_cite_references([], _, []).
make_cite_references([Key],    F, [#lref(cite, Key, Term)]) :-
    !,
    Term =.. [F, Key].
make_cite_references([Key|T0], F, [#lref(cite, Key, Term), nospace(','), ' '|T]) :-
    Term =.. [F, Key],
    make_cite_references(T0, F, T).

%       canonicalise_label(+Raw, -Canonical)
%
%       Ensures the label is either an atom, or fileof(Atom), so unification
%       will work properly.

canonicalise_label(Atom, Atom) :-
    atom(Atom),
    !.
canonicalise_label(Atomic, Atom) :-
    atomic(Atomic),
    !,
    atom_string(Atom, Atomic).
canonicalise_label(fileof(In), Out) :-
    onefile(true),
    !,
    canonicalise_label(In, Out).
canonicalise_label(fileof(In), fileof(Out)) :-
    canonicalise_label(In, Out).


%!      expand_macros(Raw, Expanded).
%!      macro_expand(Raw, Expanded).
%
%       Expand +Term, Mode+Term and #Macro commands into plain HTML

macro_expand(In, Out) :-
    expand_macros(In, Out).

expand_macros([], []) :- !.
expand_macros([[]|T0], [T]) :-
    !,
    expand_macros(T0, T).
expand_macros([H0|T0], [H|T]) :-
    !,
    expand_macro(H0, H),
    expand_macros(T0, T).
expand_macros(Macro, Expanded) :-
    expand_macro(Macro, Expanded).

expand_macro(+Term, HTML) :-
    !,
    translate(Term, normal, HTML0),
    expand_macros(HTML0, HTML).
expand_macro(Mode+Term, HTML) :-
    !,
    translate(Term, Mode, HTML0),
    expand_macros(HTML0, HTML).
expand_macro(#Macro, HTML) :-
    tex_extension_module(M),
    M:'#'(Macro, HTML0),
    !,
    push_macro(Macro),
    expand_macros(HTML0, HTML),
    pop_macro.
expand_macro(List, Expanded) :-
    List = [_|_],
    !,
    expand_macros(List, Expanded).
expand_macro(NoExpand, NoExpand).

push_macro(Macro) :-
    nb_current(macro_stack, Stack),
    !,
    b_setval(macro_stack, [Macro|Stack]).
push_macro(Macro) :-
    b_setval(macro_stack, [Macro]).

pop_macro :-
    nb_current(macro_stack, [_|Stack]),
    !,
    b_setval(macro_stack, Stack).

in_macro(Env) :-
    nb_current(macro_stack, Stack),
    \+ \+ memberchk(Env, Stack).


                 /*******************************
                 *             CITES            *
                 *******************************/

:- dynamic
    cite/2.                         % Key, Cite (HTML)

reset_cites :-
    retractall(cite(_, _)).

                 /*******************************
                 *             FLOAT            *
                 *******************************/

:- dynamic
    current_float/2.                % Type, Number

:- meta_predicate
    do_float(+, 0).

%       do_float(+Type(+Number), +Goal)
%
%       Run goal, registering the current float object processed.

do_float(Term, Goal) :-
    functor(Term, Type, 1),
    arg(1, Term, Number),
    asserta(tex:current_float(Type, Number), Ref),
    ignore(Goal),
    erase(Ref).


                 /*******************************
                 *            LABEL/REF         *
                 *******************************/

:- dynamic
    label/3,                        % Label, File, Index
    next_file/2,                    % File, NextFile
    section_label/2.                % Label, SecNr

reset_labels :-
    retractall(label(_, _, _)),
    retractall(next_file(_, _)),
    retractall(section_label(_, _)).

collect_labels([], _) :- !.
collect_labels([tell(File)|T], PreviousFile) :-
    !,
    (   PreviousFile \== File
    ->  assert(next_file(PreviousFile, File))
    ;   true
    ),
    collect_labels(T, File).
collect_labels([label(Label, _, Tag)|T], File) :-
    !,
    assert(label(Label, File, Tag)),
    collect_labels(T, File).
collect_labels([_|T], File) :-
    collect_labels(T, File).

label_tag(_Label, Tag) :-
    current_float(_, Tag),
    !.
label_tag(Label, Tag) :-
    section_tag(Tag),
    (   sub_atom(Label, 0, _, _, 'sec:')
    ->  atom_concat('sec:', Tag, SecTag),
        assert(section_label(Label, SecTag))
    ;   true
    ).


                 /*******************************
                 *          ENVIRONMENTS        *
                 *******************************/

%       env(+Env, -HTML)
%
%       Translate an environment.

env(pcecode(_, Tokens), #pre(code, HTML)) :-
    translate(Tokens, pcecode, HTML).
env(summarylist(_, Summary),
    [ html('<table>'),
      +Summary,
      html('</table>')
    ]).
env(parameters(_, Paramlist),           % Deprecated
    [ html('<table class="paramlist">'),
      Body,
      html('</table>')
    ]) :-
    table_body(Paramlist, [[], []], Body).
env(arguments(_, Arglist),
    [ html('<table class="arglist">'),
      Body,
      html('</table>')
    ]) :-
    table_body(Arglist, [[], []], Body).
env(comment(_, _), []).
env(htmlonly(_, Tokens), HTML) :-
    translate(Tokens, normal, HTML).
env(document(_, Contents), HTML) :-
    !,
    translate(Contents, document, HTML).
env(quote(_, Tokens), #quote(Quote)) :-
    translate_group(Tokens, Quote).
env(abstract(_, Tokens), #abstract(Quote)) :-
    translate(Tokens, normal, Quote).
env(center(_, Tokens), HTML) :-
    phrase(only_table(Table), Tokens),
    setup_call_cleanup(
        asserta(center_tables, Ref),
        translate([Table], normal, HTML),
        erase(Ref)).
env(center(_, Tokens), #center(Center)) :-
    translate(Tokens, normal, Center).
env(titlepage(_, _Page), []) :-
    (   quiet
    ->  true
    ;   format(user_error, 'Ignored the titlepage~n', [])
    ).
env(tabular([{Format}], Tokens), HTML) :-
    translate_table(Format, Tokens, HTML).
env(tabulary([_Width, {Format}], Tokens), HTML) :-
    translate_table(Format, Tokens, HTML).
env(array([{Format}], Tokens), HTML) :-
    translate_table(Format, Tokens, HTML).
env(table(_, Tokens), Table) :-
    step_counter(table, Tab),
    asserta(current_float(table, Tab), Ref),
    translate(Tokens, normal, Table),
    erase(Ref).
env(figure(_, Tokens), Figure) :-
    step_counter(figure, Fig),
    asserta(current_float(figure, Fig), Ref),
    translate(Tokens, normal, Figure),
    erase(Ref).
env(minipage(_, Tokens), Page) :-
    translate(Tokens, normal, Page).
env(thebibliography(Args, Tokens),
    [ SectionHeader,
      Open,
      HtmlItems,
      Close
    ]) :-
    memberchk(\newblock, Tokens),
%       writeln(Tokens),
    (   user_cmd(refname, document, _, Title)
    ->  true
    ;   Title = ['Bibliography']
    ),
    (   documentclass(article)
    ->  SecLevel = 2,
        Number = *
    ;   SecLevel = 1,
        Number = -
    ),
    translate_section(SecLevel, Number, Title, SectionHeader,
                      'Bibliography'),
    List = thebibliography,
    (   list_command(List, Args, Open, Close),
        items(Tokens, Items),
        translate_items(Items, List, HtmlItems)
    ->  true
    ;   format(user_error, 'Failed to translate "~w" list~n', [List])
    ).
env(thebibliography(_Args, _Tokens), []) :- !.
env(Env, [Open, HtmlItems, Close]) :-           % General lists
    functor(Env, List, _),
    tex_environment_function(List, list),
    !,
    arg(1, Env, Args),
    arg(2, Env, Tokens),
    (   tex_extension_module(M),
        M:list_command(List, Args, Open, Close),
        items(Tokens, Items),
        translate_items(Items, List, HtmlItems)
    ->  true
    ;   format(user_error, 'Failed to translate "~w" list~n', [List])
    ).

list_command(description,     _, html('<dl class="latex">'), html('</dl>')).
list_command(dlist,           _, html('<dl class="latex">'), html('</dl>')).
list_command(itemize,         _, html('<ul class="latex">'), html('</ul>')).
list_command(itemlist,        _, html('<ul class="latex">'), html('</ul>')).
list_command(shortlist,       _, html('<ul class="compact">'), html('</ul>')).
list_command(enumerate,       _, html('<ol class="latex">'), html('</ol>')).
list_command(thebibliography, _, html('<dl class="bib">'), html('</dl>')).

%       items(+Tokens, -Items)
%
%       Translate the item-list of a list-environment into a nested list,
%       where each first element is the item command, and the others are
%       the tokens of the item.

items([], []).
items([Cmd|More], [[Cmd|ItemTokens]|Items]) :-
    functor(Cmd, \, _),
    arg(1, Cmd, TexCmd),
    tex_command_function(TexCmd, item),
    !,
    item_commands(More, ItemTokens, Rest),
    items(Rest, Items).
items([Token|More], List) :-
    (   no_item_token(Token)
    ->  true
    ;   format(user_error, 'Skipped "~w"; no item~n', [Token])
    ),
    items(More, List).

%       no_item_token(+Token)
%
%       Succeeds if Tokens is a token that may appear before the first
%       item of a list, and should not be translated.

no_item_token(' ').
no_item_token('\n').
no_item_token(\par).
no_item_token(\(setlength, _)).
no_item_token(\(newcommand, _)).

item_commands(List, [], List) :-
    List = [Cmd|_],
    functor(Cmd, \, _),
    arg(1, Cmd, TexCmd),
    tex_command_function(TexCmd, item),
    !.
item_commands([H|T0], [H|T1], T2) :-
    item_commands(T0, T1, T2).
item_commands([], [], []).

%       translate_items(+ItemTokens, +List, -HTMLTokens)
%
%       Translate the TeX tokens for a list-item.  List is passed as context,
%       to ensure proper translation.

translate_items([], _, []).
translate_items([H0|T0], List, [H1|T1]) :-
    translate(H0, List, _, H1),
    translate_items(T0, List, T1).


                 /*******************************
                 *        ACTIVE COMMANDS       *
                 *******************************/

prolog_function(\(usepackage, [_,{File},_])) :-
    (   package_alias(File, Alias),
        (   atom_concat(sty_, Alias, Extension)
        ;   Extension = Alias
        ),
        member(Term, [tex(Extension), latex2html(Extension)]),
        absolute_file_name(Term, PlFile,
                           [ extensions([pl, qlf]),
                             access(read),
                             file_errors(fail)
                           ])
    ->  ensure_loaded(user:PlFile)
    ;   true
    ).
prolog_function(\(newcommand, [{Name}, [], {Expanded}])) :-
    declare_command(Name, 0, Expanded).
prolog_function(\(newcommand, [{Name}, [Args], {Expanded}])) :-
    declare_command(Name, Args, Expanded).
prolog_function(\(renewcommand, [{Name}, [], {Expanded}])) :-
    declare_command(Name, 0, Expanded).
prolog_function(\(renewcommand, [{Name}, [Args], {Expanded}])) :-
    declare_command(Name, Args, Expanded).

package_alias(pl, pldoc) :- !.
package_alias(Pkg, Pkg).


                 /*******************************
                 *            COMMANDS          *
                 *******************************/

:- dynamic
    documentclass/1.

%!  cmd(+Command, +Mode, -HTML)
%

cmd(onefile, preamble, []) :-
    retractall(onefile(_)),
    assert(onefile(true)).
cmd(htmlpackage({File}), preamble, []) :-
    (   absolute_file_name(tex(File), PlFile,
                           [ extensions([pl, qlf]),
                             access(read),
                             file_errors(fail)
                           ])
    ->  ensure_loaded(user:PlFile)
    ;   format(user_error, 'Cannot find Prolog extension "~w"~n', [File])
    ).
cmd(documentclass(_, {Class}), preamble, []) :-
    assert(documentclass(Class)).
cmd(usepackage(_, {_File}, _), preamble, []) :- !.
cmd(makeindex, preamble, []) :-
    retractall(makeindex(_)),
    asserta(makeindex(true)).
cmd(vskip(_), pcecode, verb('\n')).
cmd(lineno({_Line}), pcecode, verb('\n')).

%       cmd(+Command, -HTML)

cmd(newcommand({Name}, [], {Expanded}), []) :-
    declare_command(Name, 0, Expanded).
cmd(newcommand({Name}, [Args], {Expanded}), []) :-
    declare_command(Name, Args, Expanded).
cmd(renewcommand({Name}, [], {Expanded}), []) :-
    declare_command(Name, 0, Expanded).
cmd(renewcommand({Name}, [Args], {Expanded}), []) :-
    declare_command(Name, Args, Expanded).
cmd(def({'\\booktitle'}, {Title}), HTML) :-
    cmd(title({[Title]}), HTML).
cmd(def(_, _), []).
cmd(sloppy, []).
cmd(noindent, []).
cmd(clearpage, []).
cmd(cleardoublepage, []).
cmd(nopagebreak, []).
cmd(pagebreak, []).
cmd(linebreak, [html('<br>')]).
cmd(newpage, []).
cmd(hyphenpenalty(_), []).
cmd(newlength(_), []).
cmd(setlength(_,_), []).
cmd(settowidth(_, _), []).
cmd(setcounter({page}, _), []).
cmd(vskip(_), [html('<p>')]).
cmd(vspace(_), [html('<p>')]).
cmd(hspace(_), []).
cmd(headheight(_), []).
cmd(footheight(_), []).
cmd(vfill, [html('<p>')]).
cmd(vfil, [html('<p>')]).
cmd(hfill, []).
cmd(/, []).
cmd(and, [html('<br>')]).
cmd(leavevmode, []).
cmd(parskip(_), []).
cmd(parindent(_), []).
cmd(raggedright, []).                   % Always in HTML
cmd(tableofcontents,
    [ #tell('Contents'),
      #header(contents),
      #h(1, #label('document-contents', 'Table of Contents')),
      tableofcontents(document)
    ]).
cmd(printindex, []).

cmd(par, html('<p>')).                          % \par
cmd(\([]), html('<br>')).                       % \\
cmd(\([_Skip]), html('<p>')).                   % \\[skip]
cmd(newline, html('<br>')).                     % \newline

cmd(part({Title}), #h(1, +Title)).              % \part

cmd(chapter(M, {Title}), HTML) :-               % \chapter, \section, ...
    translate_section(1, M, Title, HTML).
cmd(section(M, {Title}), HTML) :-
    translate_section(2, M, Title, HTML).
cmd(subsection(M, {Title}), HTML) :-
    translate_section(3, M, Title, HTML).
cmd(subsubsection(M, {Title}), HTML) :-
    translate_section(4, M, Title, HTML).
cmd(paragraph({Title}), [#b(+Title), ' ']).
cmd(subparagraph({Title}), [#b(+Title), ' ']).

cmd(label({Label}), #label(Label, [], Tag)) :-  % \label and \xyzref
    label_tag(Label, Tag).
cmd(ref({RefName}), #lref(ref, RefName, ref(RefName))).
cmd(pageref({RefName}), #lref(ref, RefName, ref(RefName))).

cmd(secref({Label}), HTML) :-
    translate_reference(section, sec, Label, HTML).
cmd('Secref'({Label}), HTML) :-
    translate_reference('Section', sec, Label, HTML).
cmd(chapref({Label}), HTML) :-
    translate_reference(chapter, sec, Label, HTML).
cmd('Chapref'({Label}), HTML) :-
    translate_reference('Chapter', sec, Label, HTML).
cmd(figref({Label}), HTML) :-
    translate_reference(figure, fig, Label, HTML).
cmd('Figref'({Label}), HTML) :-
    translate_reference('Figure', fig, Label, HTML).
cmd(tabref({Label}), HTML) :-
    translate_reference(table, tab, Label, HTML).
cmd('Tabref'({Label}), HTML) :-
    translate_reference('Table', tab, Label, HTML).
cmd(appref({Label}), HTML) :-
    translate_reference(appendix, sec, Label, HTML).
cmd('Appref'({Label}), HTML) :-
    translate_reference('Appendix', sec, Label, HTML).
cmd(ifthenelse({Cond},{If},{Else}), [Result]) :-
    (   eval_if_then_else(Cond)
    ->  Result = If
    ;   Result = Else
    ).
cmd(title({Title}), []) :-                      % \title
    retractall(title(_)),
    translate(Title, title, HTML),
    assert(title(HTML)).
cmd(author({Author}), []) :-                    % \author
    retractall(author(_)),
    translate(Author, normal, HTML),
    assert(author(HTML)).
cmd(bodycolor({BG}), []) :-                     % \bodycolor
    retractall(bodycolor(_)),
    assert(bodycolor(BG)).
cmd(linkimage({Name}, {Icon}), []) :-           % \linkimage{Id, Path}
    make_output_directory,
    html_output_dir(Dir),
    absolute_file_name(img(Icon), Path),
    file_base_name(Path, Base),
    atomic_list_concat([Dir, Base], /, To),
    copy_file(Path, To),
    asserta(link_image(Name, Base)).
cmd(htmloutput({Dir}), []) :-
    (   done_make_output_directory
    ->  format(user_error,
               'Cannot change output directory after output has started~n',
               []),
        fail
    ;   clean_tt(Dir, Clean),
        retract(html_output_dir(_)),
        !,
        asserta(html_output_dir(Clean))
    ).
cmd(htmlmainfile({File}), []) :-
    retractall(html_file_base(_)),
    assert(html_file_base(File)).
cmd(htmlfiledepth({Depth}), []) :-
    atom_codes(Depth, Chars),
    number_codes(D, Chars),
    retractall(html_split_level(_)),
    assert(html_split_level(D)).

cmd(maketitle,                                  % \maketitle
    [ #div(title, #thetitle),
      #div(author, #theauthor)
    ]).

cmd(newblock, []).                              % BiBTeX \newblock
cmd(protect, []).                               % BiBTeX produced?
cmd(citename({TeX}), HTML) :-
    translate_group([\sc|TeX], HTML).
cmd(bibitem([TeXCite], {Key}),                  % \bibitem
    [ html('<dt class="bib">'), #label(Key, #strong(Cite)),
      html('<dd class="bib">') ]) :-
    translate(TeXCite, normal, Cite),
    assert(cite(Key, Cite)).
cmd(bibitem([], {Key}),
    [ html('<dt class="bib">'), #label(Key, #strong(Cite)),
      html('<dd class="bib">') ]) :-
    flag(cite, N, N+1),
    TeXCite is N + 1,
    expand_macros(#embrace("[]", TeXCite), Cite),
    assert(cite(Key, Cite)).
cmd(cite({Key}),        #cite(Key)).            % \cite
cmd(nocite({Key}),      #nocite(Key)).          % \nocite
cmd(yearcite({Key}),    #yearcite(Key)).        % \yearcite
cmd(opencite({Key}),    #opencite(Key)).        % \opencite
cmd(bibliography({_}), HTML) :-                 % \bibliography
    tex_file_base(File),
    (   absolute_file_name(tex(File), BiBFile,
                           [ extensions([bbl]),
                             access(read),
                             file_errors(fail)
                           ])
    ->  tex_tokens(BiBFile, TeXTokens),
        translate(TeXTokens, file, HTML)
    ;   format(user_error, 'No bibliography file~n', []),
        HTML = []
    ).
cmd(bibliographystyle(_), []).                  % \bibliographystyle
cmd(',', []).                                   % \,
cmd(-, []).                                     % \- (stop hyphenation)

cmd(emph({Tex}),   #em(+Tex)).                  % \emph{text}
cmd(texttt({Tex}), #tt(+Tex)).                  % \texttt{text}
cmd(textbf({Tex}), #b(+Tex)).                   % \textbf{text}
cmd(textit({Tex}), #i(+Tex)).                   % \textit{text}
cmd(mathit({Tex}), #i(+Tex)).                   % \textit{text}
cmd(textsf({Tex}), #b(+Tex)).                   % \textsf{text}
cmd(textsc({Tex}), #sc(+Tex)).                  % \textsc{text}

cmd(year,       Year) :-                        % \year
    get_time(Time),
    convert_time(Time, Year, _, _, _, _, _, _).
cmd('LaTeX',    'LaTeX').                       % \LaTeX
cmd('TeX',      'TeX').                         % \TeX

cmd(index({Term}), #label(RefName, [])) :-      % \index
    translate_index(Term, RefName).
cmd(idx({Term}), #label(RefName, Term)) :-      % \idx
    translate_index(Term, RefName).

cmd(footnote({TeX}), #(footnote(+TeX))).        % \footnote
cmd(footnotetext([Num], {Tokens}), [html('<p>'), #embrace(Num), Text]) :-
    translate(Tokens, normal, Text).

cmd(pagenumbering(_), []).                      % pagestyle stuff
cmd(pagestyle(_),     []).
cmd(thispagestyle(_), []).
cmd(fancyplain(_),    []).
cmd(lhead(_,_),       []).
cmd(chead(_,_),       []).
cmd(rhead(_,_),       []).
cmd(lfoot(_,_),       []).
cmd(cfoot(_,_),       []).
cmd(rfoot(_,_),       []).
cmd(rightmark,        []).
cmd(leftmark,         []).
cmd(footrulewidth(_), []).

cmd(centerline({Tex}), #center(+Tex)).
cmd(rightline({Tex}), #right(+Tex)).

cmd(email({Address}), #url(URL, Address)) :-
    format(string(URL), 'mailto:~w', [Address]).
cmd(url({Address}), #url(Address, Address)).
cmd(href({URL}, {Text}), #url(URL, +Text)).
cmd(file({File}), #tt(File)).
cmd(strong(             {A1}), #strong(+A1)).
cmd(tick({Tokens}),
    [ html('<li>'), html('<i>'), Tag, html('</i>'), html('<br>') ]) :-
    translate_group(Tokens, Tag).
cmd(item([]), html('<li>')).
cmd(item([Tag]),
    [ html('<li>'), html('<i>'), +Tag, html('</i>'), html('<br>') ]).
cmd(mbox({Boxed}), HTML) :-
    translate_group(Boxed, HTML).
cmd(makebox(_, _, {Boxed}), HTML) :-
    translate_group(Boxed, HTML).
cmd(raisebox(_, {Boxed}), HTML) :-
    translate_group(Boxed, HTML).
cmd(parbox(_, _, {Boxed}), HTML) :-
    translate_group(Boxed, HTML).
cmd(string({Text}), nospace(Text)).
cmd(ldots, '...').
cmd(cline(_), []).
cmd('%', nospace('%')).
cmd('#', nospace('#')).
cmd('$', nospace('$')).
cmd('&', nospace('&')).
cmd('{', nospace('{')).
cmd('}', nospace('}')).
cmd('[', nospace('[')).
cmd(']', nospace(']')).
cmd('"'({'\\i'}), html('&iuml;')).      % \"\i
cmd('"'({C}), html(Cmd)) :-             % \"[ouey...]
    atomic_list_concat([&, C, 'uml;'], Cmd).
cmd(''''({C}), html(Cmd)) :-            % \'[ouey...]
    atomic_list_concat([&, C, 'acute;'], Cmd).
cmd(' ', nospace(' ')).                 % :-)
cmd(copyright, html('&copy;')).         % \copyright
cmd(tm, html('&reg;')).                 % \tm
cmd(alpha, html('&alpha;')).
cmd(beta, html('&beta;')).
                                        % old stuff (< HTML 3)
cmd(alpha, #var(a)).                    % \alpha
cmd(beta, #var(b)).                     % \beta
cmd(tm, #embrace(tm)).                  % \tm
cmd(sum,  html('&Sigma;')).             % \sum

cmd(include({File}), HTML) :-
    absolute_file_name(tex(File), TeXFile,
                       [ extensions([tex]),
                         access(read),
                         file_errors(error)
                       ]),
    tex_tokens(TeXFile, TeXTokens),
    translate(TeXTokens, file, HTML).
cmd(input({File}), []) :-
    file_name_extension(_, sty, File),
    !.
cmd(input({File}), HTML) :-
    absolute_file_name(tex(File), TeXFile,
                       [ extensions([tex, '']),
                         access(read),
                         file_errors(error)
                       ]),
    tex_tokens(TeXFile, TeXTokens),
    translate(TeXTokens, file, HTML).
cmd('InputIfFileExists'({File},{True},{False}), HTML) :-
    (   absolute_file_name(tex(File), TeXFile,
                           [ extensions([tex, '']),
                             access(read),
                             file_errors(fail)
                           ])
    ->  HTML = [+True|FileHTML],
        tex_tokens(TeXFile, TeXTokens),
        translate(TeXTokens, file, FileHTML)
    ;   HTML = [+False]
    ).
cmd('IfFileExists'({File},{True},{False}), HTML) :-
    (   absolute_file_name(tex(File), _TeXFile,
                           [ extensions([tex, '']),
                             access(read),
                             file_errors(fail)
                           ])
    ->  HTML = [+True]
    ;   HTML = [+False]
    ).
cmd(appendix, []) :-
    appendix.
cmd(caption({Caption}),
    [ html('<div class="caption">'),
      #b([#nameof(Type), ' ', Number, ':', ' ']),
      +Caption,
      html('</div>')
    ]) :-
    current_float(Type, Number).

cmd(psdirectories({Spec}), []) :-
    split(Spec, 0',, Dirs),
    retractall(user:file_search_path(psfig, _)),
    forall(member(D, Dirs),
           assert(user:file_search_path(psfig, tex(D)))).
cmd(psfig({Spec}), html(Img)) :-
    psfig_options(Spec, Options),
    member(figure(File), Options),
    file_name_extension(Base, Ext, File),
    ps_extension(Ext),
    file_base_name(Base, GifBase),
    file_name_extension(GifBase, gif, GifFile),
    format(string(Img), '<img src="~w">', GifFile),
    make_output_directory,
    html_output_dir(Dir),
    atomic_list_concat([Dir, '/', GifFile], OutFile),
    (   keep_figures(true),
        exists_file(OutFile)
    ->  true
    ;   (   is_absolute_file_name(Base)
        ->  FileSpec = Base
        ;   FileSpec = tex(Base)
        ),
        ps2gif(FileSpec, OutFile)
    ).
cmd(includegraphics(_Options, {File}), html(Img)) :-
    findall(Ext, img_extension(Ext), Exts),
    (   is_absolute_file_name(File)
    ->  Spec = File
    ;   Spec = includegraphics(File)
    ),
    absolute_file_name(Spec, AbsImgFile,
                       [ extensions([''|Exts]),
                         access(read),
                         file_errors(fail)
                       ]),
    file_name_extension(_, Ext, AbsImgFile),
    img_extension(Ext),
    !,
    file_base_name(AbsImgFile, ImgFile),
    format(string(Img), '<img src="~w">', ImgFile),
    make_output_directory,
    html_output_dir(Dir),
    atomic_list_concat([Dir, '/', ImgFile], OutFile),
    (   keep_figures(true),
        exists_file(OutFile)
    ->  true
    ;   copy_file(AbsImgFile, OutFile)
    ).
cmd(includegraphics(_Options, {File}), html(Img)) :-
    ps_extension(Ext),
    absolute_file_name(includegraphics(File), PsFile,
                       [ extensions([Ext]),
                         access(read),
                         file_errors(fail)
                       ]),
    file_name_extension(Base, Ext, PsFile),
    file_base_name(Base, GifBase),
    file_name_extension(GifBase, gif, GifFile),
    format(string(Img), '<img src="~w">', GifFile),
    make_output_directory,
    html_output_dir(Dir),
    atomic_list_concat([Dir, '/', GifFile], OutFile),
    (   keep_figures(true),
        exists_file(OutFile)
    ->  true
    ;   ps2gif(PsFile, OutFile)
    ).
cmd(postscript({_Width}, {File}, Title),
    [ LabelHTML,
      ImgHTML,
      html('<p>'),
      Caption
      ]) :-
    file_name_extension(File, gif, GifFile),
    atom_concat('fig:', File, Label),
    step_counter(figure, Fig),
    do_float(figure(Fig),
             (   translate_command(caption(Title), float, _, Caption),
                 translate_command(label({Label}), float, _, LabelHTML)
             )),
    centered_img(GifFile, ImgHTML),
    make_output_directory,
    current_setting(html_output_dir(Dir)),
    atomic_list_concat([Dir, '/', GifFile], OutFile),
    (   current_setting(keep_figures),
        exists_file(OutFile)
    ->  true
    ;   ps2gif(psfig(File), OutFile, [margin(5)])
    ).
cmd(postscriptfig(_Options, {File}, Title),
    [ LabelHTML,
      ImgHTML,
      html('<p>'),
      Caption
    ]) :-
    (   absolute_file_name(psfig(File), ImgPath,
                           [ extensions([png,gif]),
                             file_errors(fail),
                             access(read)
                           ])
    ->  file_base_name(ImgPath, ImgFile)
    ;   file_name_extension(File, gif, ImgFile)
    ),
    atom_concat('fig:', File, Label),
    step_counter(figure, Fig),
    do_float(figure(Fig),
             (   translate_command(caption(Title), float, _, Caption),
                 translate_command(label({Label}), float, _, LabelHTML)
             )),
    centered_img(ImgFile, ImgHTML),
    make_output_directory,
    current_setting(html_output_dir(Dir)),
    directory_file_path(Dir, ImgFile, OutFile),
    (   current_setting(keep_figures),
        exists_file(OutFile)
    ->  true
    ;   nonvar(ImgPath)
    ->  copy_file(ImgPath, OutFile)
    ;   ps2gif(psfig(File), OutFile, [margin(5)])
    ).

centered_img(File, html(HTML)) :-
    format(string(HTML),
           '<div style="text-align:center"><img src="~w"></div>', File).

ps_extension(eps).
ps_extension(ps).

img_extension(gif).
img_extension(png).
img_extension(jpg).
img_extension(jpeg).

%
%       HTML documentation
%

cmd('HTML'({Tag}),      #code(Tag)).
cmd(latexcmd({Cmd}),    #code(BslCmd)) :-
    atom_concat(\, Cmd, BslCmd).
cmd(latexenv({Env}),    #code(Env)).
cmd(mode({Mode}),       #code(Mode)).


%
%       cmd(+Command, +Mode, -HTML
%

cmd(item([Tag]), description,                   % \item in description
    [ html('<dt>'), #b(+Tag), html('<dd>') ]).
cmd(item([Tokens]), itemlist,                   % \item in itemlist
    [ html('<li>'), html('<i>'), Tag, html('</i>'), html('<br>') ]) :-
    translate_group(Tokens, Tag).

cmd(times, math, [' ', html('&times;'), ' ']).
cmd(wedge, math, ['/\\']).
cmd(rightarrow, ['->']).
cmd('Rightarrow', ['=>']).
cmd('Leftrightarrow', ['<=>']).
cmd(pi, math, 'pi').
cmd(mu, math, html('&mu')).
%cmd(circ, math, html('&omicron;')).            % not in Netscape 4.51
cmd(circ, math, o).
cmd(rhd, math, html('&gt;')).
cmd(leq, math, '=<').
cmd(equiv, math, '==').
cmd(longrightarrow, math, '-->').
cmd(geq, math, '>=').
cmd(leq, math, '=<').
cmd(ge, math, '>=').
cmd(le, math, '<').
cmd(mid, math, '|').
cmd(pm, math, html('&#177;')).
cmd(langle, math, '<').
cmd(rangle, math, '>').
cmd(sin({Arg}), math, ['sin(', math+Arg, ')']).
cmd(cos({Arg}), math, ['cos(', math+Arg, ')']).
cmd(tan({Arg}), math, ['tan(', math+Arg, ')']).
cmd(sinh({Arg}), math, ['sinh(', math+Arg, ')']).
cmd(cosh({Arg}), math, ['cosh(', math+Arg, ')']).
cmd(tanh({Arg}), math, ['tanh(', math+Arg, ')']).
cmd(arcsin({Arg}), math, ['arcsin(', math+Arg, ')']).
cmd(arccos({Arg}), math, ['arccos(', math+Arg, ')']).
cmd(arctan({Arg}), math, ['arctan(', math+Arg, ')']).
cmd(ln({Arg}), math, ['ln(', math+Arg, ')']).
cmd(lg({Arg}), math, ['log10(', math+Arg, ')']).
cmd(log({Arg}), math, ['log(', math+Arg, ')']).
cmd(sqrt({Arg}), math, ['sqrt(', math+Arg, ')']).
cmd(not({=}), math, html('&ne')).
cmd(neq, math, html('&ne')).
cmd(exists, math, html('&exist')).
cmd(emptyset, math, html('&Oslash;')).
cmd(subset, math, 'subset').
cmd(frac({A1}, {A2}), math, [math+A1, '/', math+A2]).
cmd(mod({A1}, {A2}), math, [math+A1, ' ', 'mod', ' ', math+A2]).
cmd(rem({A1}, {A2}), math, [math+A1, ' ', 'rem', ' ', math+A2]).
cmd(div, math, div).
cmd(pow({A1}, {A2}), math, [math+A1, '**', math+A2]).
cmd(tt, math, []).                      % just ignore?

cmd(\([]), title, ' ').                 % ignore \\ in title

%
% cmd(+Command, +Mode0, -Mode, -HTML
%

cmd(Cmd, Mode0, Mode, HTML) :-
    user_cmd(Cmd, Mode0, Mode, HTML),
    !.
cmd(obeylines, group(Atts), group([obeylines|Atts]), []).
cmd(Font, group(Old), group([font(Font)|Old1]), HTML) :-
    html_font(Font, Open, _),
    !,
    (   select(font(OldFont), Old, Old1),
        html_font(OldFont, _, Close)
    ->  HTML = [Close,Open]
    ;   Old1 = Old,
        HTML = Open
    ).

html_font(em,           html('<em>'),           html('</em>')).
html_font(bf,           html('<b>'),            html('</b>')).
html_font(it,           html('<i>'),            html('</i>')).
html_font(mathit,       html('<i>'),            html('</i>')).
html_font(cal,          html('<i>'),            html('</i>')).
html_font(tt,           html('<code>'),         html('</code>')).
html_font(sf,           html('<b>'),            html('</b>')).
html_font(sc,           html('<span style="font-variant:small-caps">'),
                                                html('</span>')).
html_font(rm,           [],                     []).
html_font(sl,           html('<b>'),            html('</b>')).
html_font(scriptsize,   [],                     []).
html_font(footnotesize, html('<font size=-1>'), html('</font>')).
html_font(small,        html('<font size=-1>'), html('</font>')).
html_font(normalsize,   [],                     []).
html_font(large,        html('<font size=+1>'), html('</font>')).
html_font('Large',      html('<font size=+2>'), html('</font>')).
html_font('Huge',       html('<font size=+3>'), html('</font>')).


                 /*******************************
                 *          \REF, ETC.          *
                 *******************************/

%!  translate_reference(+Name, +Tag, +Label, -HTML)
%
%   Used for the translation of \secref, \figref, etc.

translate_reference(Name, Tag, Label,
          #lref(Tag, RefName, [Name, ' ', ref(RefName)])) :-
    format(atom(RefName), '~w:~w', [Tag, Label]).


%!  eval_if_then_else(+Cond) is semidet.
%
%   Currently only evaluates whether a command is empty, as in
%   \ifthenelse{\equal{}{\command}}{Then}{Else}

eval_if_then_else([\(equal,[{[]},{[\Command]}])]) :-
    clause(user_cmd(Command, Mode, Mode, HTML),
           expand_macro(macro_arg, '', Mode, Mode, HTML)),
    !.
eval_if_then_else(Cond) :-
    format('Cond = ~p~n', [Cond]).

                 /*******************************
                 *            TTY STUFF         *
                 *******************************/

%       clean_tt(+Raw, -Clean)
%
%       Cleans the TeX escapes to write down weird predicate names from
%       the input.

clean_tt([Atom], Atom) :-
    atomic(Atom),
    !.
clean_tt('\\Sdot', '.') :- !.
clean_tt(Raw, Clean) :-
    atom_codes(Raw, S0),
    (   append([0'{], S1, S0),
        append(S2, [0'}], S1)
    ->  true
    ;   S2 = S0
    ),
    (   append([0'\\, 0't, 0't], S3, S2)
    ->  true
    ;   S3 = S2
    ),
    clean_specials(S3, S4),
    replace_all(S4, "\\ldots", "...", S5),
    replace_all(S5, "\\bsl{}", "\\", S6),
    delete_all(S6, "\\string", S7),
    delete_all(S7, " ", S8),
    replace_all(S8, "~", " ", S9),
    atom_codes(Clean, S9).

clean_specials([], []).
clean_specials([0'\\, Special|T0], [Special|T]) :-
    string_code(_, "#$&%{}", Special),
    !,
    clean_specials(T0, T).
clean_specials([H|T0], [H|T]) :-
    clean_specials(T0, T).

delete_all(S0, DS, S) :-
    string_codes(DS, D),
    delete_all_codes(S0, D, S).

delete_all_codes(S0, D, S) :-
    (   append(D, Post, P2)
    ->  (   append(P1, P2, S0)
        ->  append(P1, Post, S1),
            delete_all_codes(S1, D, S)
        ;   S = S0
        )
    ).

replace_all(S0, FS, TS, S) :-
    string_codes(FS, F),
    string_codes(TS, T),
    replace_all_codes(S0, F, T, S).

replace_all_codes(S0, F, T, S) :-
    (   append(F, Post, P2)
    ->  (   append(P1, P2, S0)
        ->  append(P1, T, S1),
            append(S1, Post, S2),
            replace_all_codes(S2, F, T, S)
        ;   S = S0
        )
    ).


                 /*******************************
                 *           CASE (\SC)         *
                 *******************************/

upcase_html([], []).
upcase_html([H0|T0], [H|T]) :-
    atomic(H0),
    !,
    upcase_atom(H0, H),
    upcase_html(T0, T).
upcase_html([H|T0], [H|T]) :-
    upcase_html(T0, T).

capitalise_atom(In, Out) :-
    atom_codes(In, S0),
    capitalise(S0, S1, up),
    atom_codes(Out, S1).

capitalise([], [], _).
capitalise([H0|T0], [H|T], up) :-
    is_lower(H0),
    !,
    to_upper(H0, H),
    capitalise(T0, T, down).
capitalise([H|T0], [H|T], up) :-
    capitalise(T0, T, up).
capitalise([H|T0], [H|T], down) :-
    is_alpha(H),
    !,
    capitalise(T0, T, down).
capitalise([H|T0], [H|T], down) :-
    capitalise(T0, T, up).


                 /*******************************
                 *             SECTION          *
                 *******************************/

:- dynamic
    section_counter_array/1,        % counters(L0, L1, ...)
    section/3,                      % Level, Tag, Title
    appendix_section/1.             % First number of appendix

reset_sections :-
    retractall(section(_,_,_)),
    retractall(section_counter_array(_)),
    retractall(appendix_section(_)).

appendix :-
    section_counter_array(Counters),
    arg(1, Counters, Chapter),
    asserta(appendix_section(Chapter)).

%       trans_section(+SectionCmd, +TexTitle, +RestDocument, -HTML
%
%       Allow look-ahead for \label{}, so the label can be used to
%       generate the section filename.

trans_section(Section, Title, Rest, HTML) :-
    section_level(Section, Level),
    ignore(find_label(Rest, Label)),
    translate_section(Level, -, Title, HTML, Label).

section_level(chapter,       1).
section_level(section,       2).
section_level(subsection,    3).
section_level(subsubsection, 4).

find_label([' '|T], Label) :-
    !,
    find_label(T, Label).
find_label(['\n'|T], Label) :-
    !,
    find_label(T, Label).
find_label([\par|T], Label) :-
    !,
    find_label(T, Label).
find_label([\(label, [{RawLabel}])|_], Label) :-
    (   sub_atom(RawLabel, _, _, A, :)
    ->  sub_atom(RawLabel, _, A, 0, Label)
    ;   Label = RawLabel
    ).

%       translate_section(+Level, +Modify, +TitleTokens, -HTML[, +File])

translate_section(Level, Mod, TexTitle, HTML) :-
    translate_section(Level, Mod, TexTitle, HTML, _).

translate_section(Level, -, TeXTitle,
        [ Footer,
          Tell,
          Header,
          #h(Level, RefName,
             #label(RefName,
                    [ #span('sec-nr', Tag), ' ',
                      #span('sec-title', Title)
                    ]))
        ], NodeFile) :-
    !,
    translate(TeXTitle, normal, Title),
    section_tag(OldTag),
    increment_section_counter(Level, Tag, FirstSubSection),
    (   html_split_level(Split),
        Level =< Split
    ->  Tell = #tell(NodeFile),
        Header = #header(Tag),
        (   var(NodeFile)
        ->  section_file(Tag, Title, NodeFile)
        ;   true
        ),
        (   FirstSubSection
        ->  Footer = #footer(OldTag)
        ;   Footer = []
        )
    ;   Tell = [],
        Footer = [],
        Header = []
    ),
    format(string(RefName), 'sec:~w', [Tag]),
    assert(section(Level, Tag, Title)).
translate_section(Level, *, Title, #h(Level, +Title), _).

h(1, '<h1>', '</h1>').
h(2, '<h2>', '</h2>').
h(3, '<h3>', '</h3>').
h(4, '<h4>', '</h4>').
h(5, '<h5>', '</h5>').
h(6, '<h6>', '</h6>').

section_file(Tag, _, Node) :-
    atom_concat('sec-', Tag, Node).

increment_section_counter(Level, Tag, FirstSubSection) :-
    (   retract(section_counter_array(Old))
    ->  true
    ;   Old = counters(0,0,0,0,0,0)
    ),
    functor(New, counters, 6),
    update_section_counter(1, 6, Level, Old, New, FirstSubSection),
    asserta(section_counter_array(New)),
    section_tag(Tag).

update_section_counter(I, M, L, Old, New, FirstSubSection) :-
    (   I =< M
    ->  (   I == L
        ->  arg(I, Old, N),
            NN is N + 1,
            arg(I, New, NN),
            (   N == 0
            ->  FirstSubSection = true
            ;   FirstSubSection = fail
            )
        ;   I < L
        ->  arg(I, Old, N),
            arg(I, New, N)
        ;   arg(I, New, 0)
        ),
        NI is I + 1,
        update_section_counter(NI, M, L, Old, New, FirstSubSection)
    ;   true
    ).

section_tag(Tag) :-
    section_counter_array(Term),
    !,
    findall(A, (arg(_, Term, A), A > 0), L),
    (   appendix_section(AS),
        arg(1, Term, S),
        S > AS
    ->  App is (S - AS - 1) + 0'A,
        char_code(AN, App),
        L = [_|T],
        atomic_list_concat([AN|T], '.', Tag)
    ;   atomic_list_concat(L, '.', Tag)
    ).
section_tag('').

parent_tag(Section, Parent) :-
    atom_codes(Section, Chars),
    phrase(parent_section(Parent), Chars),
    !.

parent_section(Parent) -->
    string(ParentString),
    ".",
    integer(_),
    {atom_codes(Parent, ParentString)}.

:- dynamic
    section_level/1.

tableofcontents([]) :-
    \+ section(_,_,_),
    !.
tableofcontents(Sections) :-
    tableofcontents('', Sections).

tableofcontents(TagPrefix, #div(toc, Sections)) :-
    findall(S, section_html(TagPrefix, S), Sections).

section_html(TagPrefix,
             [ #div(Class, #lref(sec, Ref,
                                 [ #span('sec-nr', Tag), ' ',
                                   #span('sec-title', Title)
                                 ]))
             ]) :-
    section(Level, Tag, Title),
    html_split_level(Split),
    (   Level =< Split
    ->  Ref = fileof(RefName)
    ;   Ref = RefName
    ),
    atom_concat(TagPrefix, _, Tag),
    format(string(RefName), 'sec:~w', [Tag]),
    atom_concat('toc-h', Level, Class).


                 /*******************************
                 *             INDEX            *
                 *******************************/

:- dynamic
    index/3.                        % SortKey, Word, Tag

reset_index :-
    retractall(index(_,_, _)).

translate_index(Term, RefName) :-
    step_counter(index, N),
    clean_index(Term, Clean),
    format(string(RefName), 'idx:~w:~w', [Clean, N]),
    section_tag(Tag),
    add_to_index(Term, Tag:RefName).

clean_index(Raw, Cleaned) :-
    atom_codes(Raw, RawChars),
    clean_index_2(RawChars, Chars),
    atom_codes(Cleaned, Chars).

clean_index_2([], []).
clean_index_2([H|T0], [H|T]) :-
    valid_index_char(H),
    !,
    clean_index_2(T0, T).
clean_index_2([_|T0], T) :-
    clean_index_2(T0, T).

valid_index_char(C) :-
    between(0'a, 0'z, C).
valid_index_char(C) :-
    between(0'A, 0'Z, C).
valid_index_char(C) :-
    between(0'0, 0'9, C).

add_to_index(Term) :-
    section_tag(Tag),
    add_to_index(Term, Tag).

%!  add_to_index(+Term, +Tag)
%
%   Add Term to the index using the href Tag. If Tag is of the
%   format +Tag, it is the primary index for the term, normally
%   a pointer to the definition of Term.

add_to_index(Term, Tag) :-
    atom_codes(Term, Chars),
    atom_codes(Atom, Chars),        % So, sure we have an atom now
    sort_chars(Chars, Sort),
    atom_codes(SortKey, Sort),
    assert(index(SortKey, Atom, Tag)).

%sort_chars(Chars, Sort) :-
%       append("menu:", X, Chars), !,
%       sort_chars(X, Sort).
sort_chars(Chars, Sort) :-
    member(C, Chars),
    is_alpha(C),
    !,
    get_lower_letters(Chars, Sort).
sort_chars(Chars, Chars).

get_lower_letters([], []).
get_lower_letters([H0|T0], [H|T]) :-
    is_upper(H0),
    !,
    to_lower(H0, H),
    get_lower_letters(T0, T).
get_lower_letters([_|T0], T) :-
    get_lower_letters(T0, T).


make_index([]) :-
    makeindex(false),
    !.
make_index([]) :-
    \+ index(_, _, _),
    !.
make_index(HTML) :-
    HTML0 = [ #tell('DocIndex'),
              #header(index),
              #h(1, #label('document-index', 'Index')),
              html('<dl>'),
              Index,
              html('</dl>')
            ],
    setof(I, T^Tag^index(I, T, Tag), I0),
    index_html(I0, 0, Index),
    expand_macros(HTML0, HTML).

index_html([], _, []).
index_html([SortKey|T0], CL0, [Sep, TermHTML|TH]) :-
    add_separator(SortKey, CL0, CL, Sep),
    setof(Term, Tag^index(SortKey, Term, Tag), Terms),
    index_terms(Terms, TermHTML),
    index_html(T0, CL, TH).

index_terms([], []).
index_terms([Term0|T0], [ html('<dt>'),
                         HtmlTerm,
                         html('<dd>'),
                         Where
                       | TH
                       ]) :-
    findall(Tag, index(_, Term0, Tag), Tags),
    term_tokens(Term0, Tokens),
    translate(Tokens, index, _, Term),
    (   member(+PrimeTag, Tags)
    ->  HtmlTerm = #lref(idx, PrimeTag, Term)
    ;   HtmlTerm = Term
    ),
    maplist(index_href, Tags, Where),
    index_terms(T0, TH).

term_tokens('$/0', [verb(!,'$/0')]) :-
    !.
term_tokens('$/1', [verb(!,'$/1')]) :-
    !.
term_tokens(Term, Tokens) :-
    tex_atom_to_tokens(Term, Tokens).


index_href(+(_), []) :- !.
index_href(Tag:Label, [' ', #lref(idx, Label, Tag)]) :- !.
index_href(Tag, [' ', #lref(sec, RefName, Tag)]) :-
    format(string(RefName), 'sec:~w', Tag).

add_separator(Term, CL, CL, []) :-
    atom_chars(Term, [CL|_]),
    !.
add_separator(Term, _, CL, [ html('<dt class="index-sep">'),
                             Char,
                             html('<dd>')
                           ]) :-
    (   atom_chars(Term, [CL|_])
    ->  upcase_atom(CL, Char)
    ;   Char = '?'
    ).


                 /*******************************
                 *        STANDARD LINKS        *
                 *******************************/

node_header([#head(#title(#thetitle)),
             #beginbody
            ]) :-
    onefile(true),
    !.
node_header([#head([#title(#thetitle),
                    link(home),
                    link(contents),
                    link(index),
                    link(summary),
                    link(previous),
                    link(next)]),
             #beginbody,
             #navigate( [ body_link(home),
                          body_link(contents),
                          body_link(index),
                          body_link(summary),
                          body_link(previous),
                          body_link(next)
                      ])
            ]).

node_header(_, []) :-
    onefile(true),
    !.
node_header(SectionTag,
            [#head([#title([#thetitle, nospace(:), ' ',
                            'Section', SectionTag]),
                    link(home),
                    link(contents),
                    link(index),
                    link(summary),
                    link(up(UpRef)),
                    link(previous),
                    link(next)]),
             #beginbody,
             #navigate( [ body_link(home),
                          body_link(contents),
                          body_link(index),
                          body_link(summary),
                          body_link(up(UpRef)),
                          body_link(previous),
                          body_link(next)
                      ])
            ]) :-
    parent_tag(SectionTag, UpTag),
    !,
    format(string(UpRef), 'sec:~w', [UpTag]).
node_header(_, HTML) :-
    node_header(HTML).


node_footer(Tag, tableofcontents(section(Tag))).


subsection_index(Tag,
            [ html('<hr>'),
              #center([html('<h2>'), 'Section Index', html('</h2>')]),
              html('<hr>'),
              SubIndex
            ]) :-
    onefile(false),
    Tag \== '',
    atom_concat(Tag, '.', Filter),
    tableofcontents(Filter, SubIndex),
    sub_term(#lref(_,_,_), SubIndex),
    !. % it is not empty
subsection_index(_, []).


                 /*******************************
                 *           COUNTERS           *
                 *******************************/

:- dynamic
    counter/2.                              % Name, Value

reset_counters :-
    retractall(counter(_,_)).

set_counter(Name, Val) :-
    retractall(counter(Name, _)),
    asserta(counter(Name, Val)).


step_counter(Name, NewVal) :-
    (   retract(counter(Name, OldVal))
    ->  NewVal is OldVal + 1
    ;   NewVal is 1
    ),
    asserta(counter(Name, NewVal)).

                 /*******************************
                 *             RULES            *
                 *******************************/

cmd(rule({'\\linewidth'}, {_H}), [html('<hr>')]) :- !.
cmd(rule({W}, {H}), []) :-
    format('W = ~w, H = ~w~n', [W, H]).

                 /*******************************
                 *            TABLES            *
                 *******************************/

only_table(Table) -->
    white_spaces,
    [ Table ],
    { tabular_env(Table) },
    white_spaces.

white_spaces --> [' '],  !, white_spaces.
white_spaces --> ['\n'], !, white_spaces.
white_spaces --> [].



tabular_env(env(tabular,_,_)).
tabular_env(env(tabularlp,_,_)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Specifying the number of columns makes Netscape make the columns equally
width.  Thats not what we want.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

translate_table(Format, Body, HTML) :-
    atom_codes(Format, Fmt),
    table_frame(Fmt, Body, FrameAttributes, Fmt2, Body2),
    expand_table_commands(Body2, Body3),
    (   table_columns(Fmt2, _Ncols, ColAtts)
    ->  true
    ;   format(user_error, 'ERROR: Failed to parse tabular spec "~s"~n', [Fmt2]),
        ColAtts = []
    ),
    (   table_body(Body3, ColAtts, BodyHTML)
    ->  true
    ;   format(user_error, 'ERROR: Failed to translate table body~n', []),
        trace, fail
    ),
    HTML = [ html(Head),
%                ColHTML,
             BodyHTML,
             html('</table>')
           ],
    atom_concat('frame-', FrameAttributes, FrameClass),
    (   center_tables
    ->  Classes = [latex, FrameClass, center]
    ;   Classes = [latex, FrameClass]
    ),
    atomic_list_concat(Classes, ' ', ClassText),
    format(string(Head), '<table class="~w">', [ClassText]).

%       expand_table_commands(+BodyIn, -BodyOut)
%
%       This shouldn't be here, but it translates commands that expand
%       into the table-special commands & or \\.

expand_table_commands([], []).
expand_table_commands([\isa|T0], [&, '::=', &|T]) :-
    !,
    expand_table_commands(T0, T).
expand_table_commands([\ora|T0], [&,   '|', &|T]) :-
    !,
    expand_table_commands(T0, T).
expand_table_commands([H|T0], [H|T]) :-
    expand_table_commands(T0, T).

%!  table_frame(+Format, +Body, -FrameAttributes, -Format2, -Body2)
%
%   Extracts the frame attributes, controlling the border of the
%   table from the format and opening/closing \hline.
%
%   @arg FrameAttributes is one of =void=, =vsides=, =lhs=, =rhs=,
%   =hsides=, =above=, =below= or =box=

table_frame(Fmt, Body, TableAttributes, Fmt2, Body2) :-
    v_table_frame(Fmt, VFr, Fmt2),
    clean_body(Body, Body1),
    h_table_frame(Body1, HFr, Body2),
    table_frame(VFr, HFr, TableAttributes).

v_table_frame([0'||Fmt0], VFr, Fmt) :-
    !,
    (   append(Fmt, [0'|], Fmt0)
    ->  VFr = vsides                % |cols|
    ;   Fmt = Fmt0,                 % |cols
        VFr = lhs
    ).
v_table_frame(Fmt0, VFr, Fmt) :-
    (   append(Fmt, [0'|], Fmt0)
    ->  VFr = rhs                   % cols|
    ;   Fmt = Fmt0,                 % cols
        VFr = void
    ).

h_table_frame([\hline|Body0], VFr, Body) :-
    !,
    (   (   append(Body, [\hline], Body0)
        ;   append(Body, [\hline,\(\, _)], Body0)
        )
    ->  VFr  = hsides               % \hline body \hline [\\]
    ;   Body = Body0,               % \hline body
        VFr  = above
    ).
h_table_frame(Body0, HFr, Body) :-
    (   (   append(Body, [\hline], Body0)
        ;   append(Body, [\hline,\(\, _)], Body0)
        )
    ->  HFr  = below                % body \hline [\\]
    ;   Body = Body0,               % body
        HFr  = void
    ).

table_frame(X, void, X) :- !.
table_frame(void, X, X) :- !.
table_frame(vsides, hsides, box) :- !.
table_frame(X, Y, border) :-
    format(user_error,
           'Cannot combine ~w and ~w for table border~n', [X, Y]).


table_columns(Fmt, Ncols, Cols) :-
    table_columns(Fmt, 0, Ncols, Cols).

table_columns([],      Ncols, Ncols, []).
table_columns([0'l|T], NC0,   NC,    [[]|TH]) :-
    NC1 is NC0 + 1,
    table_columns(T, NC1, NC, TH).
table_columns([0'L|T], NC0,   NC,    [[]|TH]) :-
    NC1 is NC0 + 1,
    table_columns(T, NC1, NC, TH).
table_columns([0'c|T], NC0,   NC,    [[align=center]|TH]) :-
    NC1 is NC0 + 1,
    table_columns(T, NC1, NC, TH).
table_columns([0'C|T], NC0,   NC,    [[align=center]|TH]) :-
    NC1 is NC0 + 1,
    table_columns(T, NC1, NC, TH).
table_columns([0'r|T], NC0,   NC,    [[align=right]|TH]) :-
    NC1 is NC0 + 1,
    table_columns(T, NC1, NC, TH).
table_columns([0'R|T], NC0,   NC,    [[align=right]|TH]) :-
    NC1 is NC0 + 1,
    table_columns(T, NC1, NC, TH).
table_columns([0'p|T0], NC0,   NC,   [Col|TH]) :-
    phrase(parbox_width(_W), T0, T),
    Col = [],
    NC1 is NC0 + 1,
    table_columns(T, NC1, NC, TH).
% align=char is not supported.  There is no sensible way around
table_columns([0'D|T0], NC0,   NC,   [[/*align=char, char=Chr*/]|TH]) :-
    phrase(align_char(_Chr), T0, T),
    NC1 is NC0 + 1,
    table_columns(T, NC1, NC, TH).
table_columns([0'||T], NC0,  NC,     TH) :-
    table_columns(T, NC0, NC, TH).

parbox_width(W) -->
    "{",
    number(N),
    dimension_unit(U),
    "}",
    !,
    { relative_width(U, N, W) }.
parbox_width(-) -->
    "{",
    string_without("{}", _),
    "}",
    !.

align_char(Chr) -->             % D{inputsep}{outputsep}{decimal places}
    "{",
    string_without("{}", W),
    "}{",
    string_without("{}", _),
    "}{",
    number(_),
    "}",
    { atom_codes(Chr, W)
    }.

string_without(L, [C|T]) -->
    [C],
    {\+ string_code(_, L, C)},
    !,
    string_without(L, T).
string_without(_, []) --> [].

number(N) -->
    optional_sign(S),
    digit(C0),
    float_digits(C),
    { append(S, [C0|C], Chars),
      number_codes(N, Chars)
    }.

integer(N) -->
    optional_sign(S),
    digit(C0),
    digits(C),
    { append(S, [C0|C], Chars),
      number_codes(N, Chars)
    }.

optional_sign([0'-]) --> "-", !.
optional_sign([0'+]) --> "+", !.
optional_sign([])  --> "".

digit(C) -->
    [C],
    {between(0'0, 0'9, C)}.

digits([C0|C]) -->
    digit(C0),
    !,
    digits(C).
digits([]) --> [].

float_digits([C0|C]) -->
    digit(C0),
    !,
    float_digits(C).
float_digits([0'.|C]) -->
    ".",
    !,
    float_digits(C).
float_digits([]) --> [].


dimension_unit(in) --> "in".
dimension_unit(pt) --> "pt".
dimension_unit(cm) --> "cm".
dimension_unit(mm) --> "mm".

relative_width(in, N, W) :- W is integer(100 * N / 6).
relative_width(pt, N, W) :- W is integer(100 * (N/72) / 6).
relative_width(cm, N, W) :- W is integer(100 * (N/2.54) / 6).
relative_width(mm, N, W) :- W is integer(100 * (N/25.4) / 6).

%       The valign="top" should be at the table-level, but Netscape
%       doesn't appear to recognise this. It is just LaTeX's default,
%       while HTML's default is middle.


table_body([], _, []).
table_body([\(\, _)|T], _, []) :-
    all_white_space(T).
table_body([' '|T0], ColAtts, T) :-
    table_body(T0, ColAtts, T).
table_body(['\n'|T0], ColAtts, T) :-
    !,
    table_body(T0, ColAtts, T).
table_body([\hline|Body], ColAtts, [[ html('<tr class="hline">'),
                                      Row,
                                      html('</tr>')
                                    ]|Rest]) :-
    !,
    table_row(Body, 1, ColAtts, BodyRest, Row),
    table_body(BodyRest, ColAtts, Rest).
table_body(Body, ColAtts, [[ html('<tr>'),
                             Row,
                             html('</tr>')
                          ]|Rest]) :-
    table_row(Body, 1, ColAtts, BodyRest, Row),
    table_body(BodyRest, ColAtts, Rest).

table_row([], _, _, [], []) :- !.
table_row([' '|T0], C, ColAtts, T, TH) :-
    !,
    table_row(T0, C, ColAtts, T, TH).
table_row(['\n'|T0], C, ColAtts, T, TH) :-
    !,
    table_row(T0, C, ColAtts, T, TH).
table_row([\(multicolumn, [{N}, {A}, {Tokens}])|R0], C, ColAtts, R,
          [html(MC), Item|THtml]) :-
    column_alignment(A, Alignment),
    !,
    format(string(MC), '<td colspan=~w align=~w>', [N, Alignment]),
    translate_group(Tokens, Item),
    to_integer(N, N2),
    C2 is C + N2,
    table_cell(R0, R1, _, Last),          % discard tokens upto &
    (   Last == true
    ->  R = R1,
        THtml = []
    ;   table_row(R1, C2, ColAtts, R, THtml)
    ).
table_row(L, C, ColAtts, R,  [html(CellHeader), Chtml, html('</td>')|THtml]) :-
    cell_header(C, ColAtts, CellHeader),
    table_cell(L, T, Tokens, Last),
    translate_group(Tokens, Chtml),
    (   Last == true
    ->  R = T,
        THtml = []
    ;   C2 is C + 1,
        table_row(T, C2, ColAtts, R, THtml)
    ).

cell_header(C, ColAtts, Header) :-
    nth1(C, ColAtts, Spec),
    maplist(sgml_attribute, Spec, Attributes),
    atomic_list_concat(['<td'|Attributes], ' ', H0),
    atom_concat(H0, '>', Header).

sgml_attribute(Name=Value, Att) :-
    atomic_list_concat([Name, =, Value], Att).

to_integer(Atom, Integer) :-
    atom_codes(Atom, Chars),
    number_codes(Integer, Chars).

column_alignment(X, Alignment) :-
    atom_codes(X, Chars),
    phrase(column_alignment(Alignment), Chars).

column_alignment(A) -->
    vlines,
    calignment(A),
    !,
    vlines.

vlines -->
    "|",
    !,
    vlines.
vlines -->
    [].

calignment(left) --> "l".
calignment(center) --> "c".
calignment(right) --> "r".
calignment(left) --> [X],
    {format(user_error, 'Unknown multicolumn alignment: "~w"~n', [X])}.

table_cell([], [], [], true).
table_cell([&|L], L, [], false) :- !.
table_cell([\(\, _)|L], L, [], true) :- !.
table_cell([H|T0], R, [H|T], Last) :-
    table_cell(T0, R, T, Last).

all_white_space([]).
all_white_space([' '|T]) :-
    all_white_space(T).
all_white_space(['\n'|T]) :-
    all_white_space(T).

clean_body([], []).
clean_body([' '|T0], T) :-
    !,
    clean_body(T0, T).
clean_body(['\n'|T0], T) :-
    !,
    clean_body(T0, T).
clean_body(Body, Body1) :-
    (   append(Body1, T, Body),
        all_white_space(T)
    ->  true
    ).

                 /*******************************
                 *            FIGURES           *
                 *******************************/

%       psfig_options(+PsFigOptions, -OptionList)
%
%       Translate an option-list of psfig into a list of Name(Value)
%       terms for easy further processing.

psfig_options(Text, Options) :-
    atom_codes(Text, Chars),
    phrase(psfigoptions(Options), Chars).

psfigoptions([H|T]) -->
    psfigoption(H),
    psfigoptions(T).
psfigoptions([]) --> [].

psfigoption(Term) -->
    string(NS),
    "=",
    string_without(",", VS),
    (   ","
    ;   ""
    ),
    !,
    { atom_codes(Name, NS),
      atom_codes(Val, VS),
      Term =.. [Name, Val]
    }.


%!  fix_predicate_reference(+Ref0, -Ref)
%
%   Deal with references such as name/[1,2], send/2-12, etc and find
%   the first matching predicate from the referenced set.

fix_predicate_reference(Ref0, Ref) :-
    atom_codes(Ref0, Chars),
    phrase(predref(Name, Arities), Chars),
    member(Arity, Arities),
    format(atom(Ref), '~w/~w', [Name, Arity]),
    label(Ref, _, _),
    !.

predref(Name, Arities) -->
    string(Str0),
    "/[",
    arityspec(Arities),
    "]",
    !,
    {atom_codes(Name, Str0)}.

arityspec(As) -->
    integer(Low),
    "-",
    integer(High),
    {findall(A, between(Low, High, A), As)}.
arityspec(As) -->
    integer(Low),
    "..",
    {findall(A, between(Low, 10, A), As)}.
arityspec(A) -->
    enumerated_arities(A).

enumerated_arities([H|T]) -->
    integer(H),
    (   ","
    ->  enumerated_arities(T)
    ;   {T = []}
    ).

string([]) --> [].
string([H|T]) --> [H], string(T).

                 /*******************************
                 *           NEWCOMMAND         *
                 *******************************/

:- dynamic
    user_cmd/4.

declare_command(Name, ArgCAtom, Expanded) :-
    atom_concat(\, CmdName, Name),
    (   tex_command_property(CmdName, _, _) % test for existence
    ->  true
    ;   atom_codes(ArgCAtom, ArgCChars),
        number_codes(Args, ArgCChars),
        make_cmd_spec(Name, Args, CmdSpec), % \Name{+}...
        tex_declare(CmdSpec),
        functor(Head, CmdName, Args),
        cmd_parms(0, Args, Head, ParmList),
        Parms =.. [macro_arg|ParmList],
        assert((user_cmd(Head, Mode0, Mode, HTML) :-
                       expand_macro(Parms, Expanded, Mode0, Mode, HTML)))
    ).

make_cmd_spec(Name, ArgC, Spec) :-
    make_cmd_arg_spec(ArgC, ArgSpec),
    atomic_list_concat([Name|ArgSpec], Spec).

make_cmd_arg_spec(0, []).
make_cmd_arg_spec(N, ['{-}'|T]) :-
    NN is N - 1,
    make_cmd_arg_spec(NN, T).

cmd_parms(N, N, _, []) :- !.
cmd_parms(N, A, Head, [A0|AT]) :-
    I is N + 1,
    arg(I, Head, {A0}),
    cmd_parms(I, A, Head, AT).

expand_macro(Args, Macro, Mode0, Mode, HTML) :-
    atom_codes(Macro, Chars),
    replace_args(Chars, Args, Expanded),
    tex_atom_to_tokens(Expanded, Tokens),
    translate(Tokens, Mode0, Mode, HTML).

replace_args([], _, []).
replace_args([0'#,N|T], Args, Result) :-
    !,
    ArgN is N - 0'0,
    arg(ArgN, Args, Val),
    atom_codes(Val, Chars),
    append(Chars, RT, Result),
    replace_args(T, Args, RT).
replace_args([C|T0], Args, [C|T]) :-
    replace_args(T0, Args, T).


                 /*******************************
                 *            UTIL              *
                 *******************************/

%!  split(+Atom, +SepCode, -ListOfAtoms)

split(Atom, Sep, List) :-
    string_codes(SepS, [Sep]),
    split_string(Atom, SepS, "", Parts),
    maplist(atom_string, List, Parts).


                 /*******************************
                 *             PS2GIF           *
                 *******************************/

default_option(gs,      gs).
default_option(res,     72).
default_option(device,  ppmraw).
default_option(tmp,     Tmp) :-
    tmp_file(ps2gif, Tmp).

ps2gif(In, Out) :-
    ps2gif(In, Out, []).

ps2gif(In, Out, _Options) :-
    absolute_file_name(In, InFile,
                       [ access(read),
                         extensions([gif]),
                         file_errors(fail)
                       ]),
    !,
    atomic_list_concat(['cp ', InFile, ' ', Out], Cmd),
    shell(Cmd).
ps2gif(In, Out, Options) :-
    get_option(Options, tmp(Tmp)),
    get_option(Options, res(Res0)),
    (   absolute_file_name(In, InFile,
                           [ access(read),
                             extensions([ps, eps]),
                             file_errors(fail)
                           ])
    ->  true
    ;   format(user_error, 'Could not find figure "~w"~n', In),
        fail
    ),
    format(user_error, 'Converting ~w to .GIF~n', [InFile]),
    get_ps_parameters(InFile, EPS, bb(X1,Y1,X2,Y2)),
    (   get_option(Options, width(W))
    ->  ScaleX is W/((X2-X1)/72)
    ;   ScaleX is 1
    ),
    (   get_option(Options, height(H))
    ->  ScaleY is H/((Y2-Y1)/72)
    ;   ScaleY is 1
    ),
    ResX is Res0 * ScaleX,
    ResY is Res0 * ScaleY,
    (   ResX =:= ResY
    ->  Res = ResX
    ;   format(string(Res), '~wx~w', [ResX, ResY])
    ),
    BBX is -X1,
    BBY is -Y1,
    BBW0 = X2 - X1,
    BBH0 = Y2 - Y1,
    BBW is round(BBW0 * ResX / 72),
    BBH is round(BBH0 * ResY / 72),
    gs_command([size(BBW,BBH),tmp(Tmp),res(Res)|Options], Cmd),
    setup_call_cleanup(open(pipe(Cmd), write, Pipe),
                       (   format(Pipe, '~w ~w translate ', [BBX, BBY]),
                           format(Pipe, '(~w) run ', InFile),
                           (   EPS = eps
                           ->  format(Pipe, 'showpage ', [])
                           ;   true
                           ),
                           format(Pipe, 'quit~n', [])
                       ),
                       close(Pipe)),
    ppm2gif(Tmp, Out, Options),
    delete_file(Tmp).

ppm2gif(Tmp, Out, Options) :-
    (   get_option(Options, margin(B))
    ->  format(atom(Cmd),
               'pnmcrop < ~w | pnmmargin ~w | pnmmargin -black 1 | ppmquant 192 | ppmtogif > ~w',
                [Tmp, B, Out])
    ;   format(atom(Cmd), 'pnmcrop < ~w | ppmquant 192 | ppmtogif > ~w',
               [Tmp, Out])
    ),
    shell(Cmd).

gs_command(Options, Cmd) :-
    get_option(Options, gs(GS)),
    get_option(Options, res(Res)),
    get_option(Options, device(Dev)),
    get_option(Options, tmp(Tmp)),
    (   get_option(Options, size(W, H))
    ->  format(string(SCmd), '-g~wx~w', [W, H])
    ;   SCmd = ''
    ),
    format(atom(Cmd),
           '~w -q -dNOPAUSE -sDEVICE=~w ~w -r~w -sOutputFile=~w',
           [GS, Dev, SCmd, Res, Tmp]).


get_option(List, Term) :-
    memberchk(Term, List),
    !.
get_option(_, Term) :-
    functor(Term, Name, _),
    default_option(Name, Def),
    !,
    arg(1, Term, Def).


                 /*******************************
                 *            OUTPUT            *
                 *******************************/


:- dynamic
    pending_par/0.

%!  is_begin(-Tag, +HTML)
%
%   True when Tag is the (lowercase) open tag of HTML.

is_begin(Tag, HTML) :-
    atom(HTML),
    !,
    sub_atom(HTML, 0, _, _, <),
    (   sub_atom(HTML, 1, L, _, X),
        N is L+1,
        (   sub_atom(HTML, N, _, _, ' ')
        ;   sub_atom(HTML, N, _, _, '>')
        )
    ->  downcase_atom(X, Tag)
    ).
is_begin(X, HTML) :-
    var(HTML),
    format(atom(HTML), '<~w>', [X]).

is_end(Tag, HTML) :-
    atom(HTML),
    !,
    sub_atom(HTML, 0, _, _, '</'),
    (   sub_atom(HTML, 2, L, _, X),
        N is L+2,
        sub_atom(HTML, N, _, _, '>')
    ->  downcase_atom(X, Tag)
    ).
is_end(X, HTML) :-
    var(HTML),
    format(atom(HTML), '</~w>', [X]).



implicit_par(html(Begin)) :-
    implicit_par_tag(Tag),
    is_begin(Tag, Begin),
    !.

implicit_par_tag(h1).
implicit_par_tag(h2).
implicit_par_tag(h3).
implicit_par_tag(h4).
implicit_par_tag(pre).
implicit_par_tag(xmp).
implicit_par_tag(dl).
implicit_par_tag(dt).
implicit_par_tag(dd).
implicit_par_tag(table).
implicit_par_tag(blockquote).
implicit_par_tag(div).
%implicit_par_tag(center).

:- dynamic
    is_open/1.

%!  create_pending_open(+Token) is det.
%!  close_pending_open(+Token) is det.
%!  close_by(+Open, -CloseBy) is nondet.

create_pending_open(Cmd) :-
    (   close_by(Open, _)
    ;   create_env(Open)
    ),
    is_begin(Open, Cmd),
    !,
    debug(html, 'Push open of ~w', [Open]),
    asserta(is_open(Open)).
create_pending_open(_).

close_pending_open(Cmd) :-
    pop_implicit_close(Cmd),
    pop_close(Cmd).

pop_implicit_close(Cmd) :-
    (   once(is_open(Open)),
        close_by(Open, CloseOn),
        call(CloseOn, Cmd)
    ->  retract(is_open(Open)),
        debug(html, 'Pop open ~w on ~w', [Open, Cmd]),
        is_end(Open, Close),
        cmd_layout(Close, Pre, Post),
        !,
        put_html_token(html(Close, Pre, Post))
    ;   true
    ).

pop_close(Cmd) :-
    (   once(is_open(Open)),
        is_end(Open, Cmd)
    ->  retract(is_open(Open)),
        debug(html, 'Pop open ~w on ~w', [Open, Cmd])
    ;   true
    ).


create_env(dl).

close_by(dd, is_begin(dt)).
close_by(dd, is_end(dl)).
close_by(dt, is_begin(dd)).
close_by(dt, is_begin(dt)).
close_by(dt, is_end(dl)).

%!  write_html(+Tokens) is det.

write_html([]) :- !.                    % Unpack lists
write_html([html(DD)|T]) :-     % Delete empty DD followed by DT
    is_begin(dd, DD),
    empty_dd(T, Rest),
    !,
    write_html(Rest).
write_html([H|T]) :-
    !,
    write_html(H),
    write_html(T).
write_html('\n') :-
    pending_par,
    !.
write_html(' ') :-
    pending_par,
    !.
write_html(html('<p>')) :-
    !,
    (   once(is_open(Open)),
        Open = 'dl'
    ->  true
    ;   pending_par
    ->  true
    ;   assert(pending_par)
    ).
write_html(Token) :-
    retract(pending_par),
    !,
    (   implicit_par(Token)
    ->  true
    ;   cmd_layout(p, begin, Pre, Post)
    ->  put_html_token(html('<p>', Pre, Post))
    ),
    write_html(Token).
write_html(html(Cmd)) :-                        % HTML commands
    close_pending_open(Cmd),
    cmd_layout(Cmd, Pre, Post),
    !,
    put_html_token(html(Cmd, Pre, Post)),
    create_pending_open(Cmd).
write_html(ref(Label)) :-                      % References and labels
    !,
    (   label(Label, _, Ref)
    ->  write_html(Ref)
    ;   write_html('??'),
        format(user_error, 'No label for ref "~w"~n', [Label])
    ).
write_html(label(Label, Text, _)) :-
    !,
    (   in_anchor
    ->  write_html(Text)
    ;   format(string(Anchor), '<a id="~w">', [Label]),
        asserta(in_anchor),
        write_html([html(Anchor), Text, html('</a>')]),
        retractall(in_anchor)
    ).
write_html(h(Level, NumRefS, TitleTerm)) :-
    !,
    h(Level, OpenH0, _CloseH),
    (   atom_string(NumRef, NumRefS),
        section_label(Human, NumRef)
    ->  format(string(OpenH), '<h~d id="~w">', [Level, Human])
    ;   OpenH = OpenH0,
        (   sub_term(span('sec-title', Title), TitleTerm)
        ->  (   quiet,
                Title == ['Bibliography']
            ->  true
            ;   format(user_error,
                       'No label for section ~w ~w~n', [NumRefS, Title])
            )
        ;   format(user_error,
                   'No label for section ~w~n', [NumRefS])
        )
    ),
    write_html([html(OpenH)]).
write_html(body_link(Link)) :-
    !,
    (   translate_ref(Link, Ref, Type)
    ->  format(string(Anchor), '<a class="nav" href="~w">', [Ref]),
        capitalise_atom(Type, Text),
        (   link_image(Type, Image)
        ->  format(string(Img), '<img src="~w" alt="~w">', [Image, Text]),
            Label = html(Img)
        ;   Label = Text
        ),
        write_html([html(Anchor), Label, html('</a>')]),
        nl_html
    ;   true
    ).
write_html(link(Link)) :-
    !,
    (   translate_ref(Link, Ref, Type)
    ->  format(string(Html), '<link rel="~w" href="~w">', [Type, Ref]),
        write_html(html(Html)),
        nl_html
    ;   true
    ).
write_html(iflref(fileof(Label), Text)) :-
    !,
    (   label(Label, _, _)
    ->  write_html(lref(fileof(Label), Text))
    ;   true
    ).
write_html(iflref(Label, Text)) :-
    !,
    (   label(Label, _, _)
    ->  write_html(lref(Label, Text))
    ;   true
    ).
write_html(lref(Label, Text)) :-
    !,
    write_html(lref('', Label, Text)).
write_html(lref(sec, Label, Text)) :-
    label(Label, File, Nr),
    section(Level, _Title, Nr),
    html_split_level(Split),
    Level =< Split,
    !,
    format(string(Anchor),
           '<a class="~w" href="~w.html">', [sec, File]),
    write_html([html(Anchor), Text, html('</a>')]).
write_html(lref(Class, fileof(Label), Text)) :-
    !,
    (   label(Label, File, _)
    ->  format(string(Anchor),
               '<a class="~w" href="~w.html">', [Class, File]),
        write_html([html(Anchor), Text, html('</a>')])
    ;   write_html(lref(Label, Text))
    ).
write_html(lref(Class, Label, Text)) :-
    label(Label, File, _),
    !,
    (   section_label(Label, TheLabel)
    ->  true
    ;   TheLabel = Label
    ),
    (   in_anchor
    ->  macro_expand(Text, Expanded),
        write_html(Expanded)
    ;   asserta(in_anchor),
        (   onefile(false)
        ->  format(string(Anchor),
                   '<a class="~w" href="~w.html#~w">',
                   [Class, File, TheLabel])
        ;   format(string(Anchor),
                   '<a class="~w" href="#~w">', [Class, TheLabel])
        ),
        write_html([html(Anchor), Text, html('</a>')]),
        retractall(in_anchor)
    ).
write_html(lref(pred, Label, Text)) :-
    fix_predicate_reference(Label, FixedLabel),
    !,
    write_html(lref(pred, FixedLabel, Text)).
write_html(lref(pred, Label, Text)) :-
    !,
    (   quiet
    ->  true
    ;   format(user_error, 'No description for predicate "~w"~n', [Label])
    ),
    macro_expand(#span('pred-ext', Text), Expanded),
    write_html(Expanded).
write_html(lref(Class, Label, Text)) :-
    !,
    (   quiet
    ->  true
    ;   format(user_error, 'No label for ~w reference "~w"~n', [Class, Label])
    ),
    macro_expand(#b(Text), Expanded),
    write_html(Expanded).
write_html(cite(Key)) :-
    !,
    (   cite(Key, Cite)
    ->  write_html(Cite)
    ;   write_html([nospace('['), Key, nospace(']')]),
        format(user_error, 'No bibliography entry for "~w"~n', Key)
    ).
write_html(yearcite(Key)) :-
    !,
    (   cite(Key, CiteList)
    ->  (   last(CiteList, Year),
            name(Year, Chars),
            name(YearInt, Chars),
            integer(YearInt)
        ->  write_html(Year)
        ;   format(user_error,
                   'No year for bibliography entry "~w"~n', Key),
            write_html(cite(Key))
        )
    ;   write_html(cite(Key))
    ).
write_html(tableofcontents(document)) :-
    !,
    tableofcontents(Table),
    macro_expand(Table, HTML),
    write_html(HTML).
write_html(tableofcontents(section(Tag))) :-
    !,
    subsection_index(Tag, Table),
    macro_expand(Table, HTML),
    write_html(HTML).
write_html(tell(Base)) :-
    !,
    close_output,
    open_output(Base).
write_html(H) :-
    put_html_token(H),
    !.
write_html(_).

nl_html :-
    write_html(verb('\n')).

empty_dd([], []) :- !.
empty_dd(L, L) :-
    L = [html(DT)|_],
    is_begin(dt, DT),
    !.
empty_dd(['\n'|T0], T) :-
    empty_dd(T0, T).


%       translate_ref(+Label, -Anchor, -TextLabel)

translate_ref(next, Anchor, next) :-
    current_html_output(Current),
    next_file(Current, Next),
    atom_concat(Next, '.html', Anchor).
translate_ref(previous, Anchor, previous) :-
    current_html_output(Current),
    next_file(Prev, Current),
    atom_concat(Prev, '.html', Anchor).
translate_ref(up(Label), Anchor, up) :-
    label(Label, File, _),
    atom_concat(File, '.html', Anchor).
translate_ref(home, Anchor, home) :-
    html_file_base(Home),
    \+ current_html_output(Home),
    onefile(false),
    atom_concat(Home, '.html', Anchor).
translate_ref(contents, Anchor, contents) :-
    label('document-contents', File, _),
    atom_concat(File, '.html', Anchor).
translate_ref(index, Anchor, index) :-
    label('document-index', File, _),
    atom_concat(File, '.html', Anchor).
translate_ref(summary, Anchor, summary) :-
    label('sec:summary', File, _),
    atom_concat(File, '.html', Anchor).

cmd_layout(HTML, Pre, Post) :-
    is_end(Tag, HTML),
    !,
    cmd_layout(Tag, end, Pre, Post).
cmd_layout(HTML, Pre, Post) :-
    is_begin(Tag, HTML),
    !,
    cmd_layout(Tag, begin, Pre, Post).
cmd_layout(_, 0, 0).

cmd_layout(p,          begin, 2, 0).
cmd_layout(dl,         begin, 2, 1).
cmd_layout(dl,         end,   1, 2).
cmd_layout(dd,         begin, 0, 1).
cmd_layout(h1,         begin, 2, 0).
cmd_layout(h2,         begin, 2, 0).
cmd_layout(h3,         begin, 2, 0).
cmd_layout(h4,         begin, 2, 0).
cmd_layout(h5,         begin, 2, 0).
cmd_layout(h6,         begin, 2, 0).
cmd_layout(h1,         end,   0, 2).
cmd_layout(h2,         end,   0, 2).
cmd_layout(h3,         end,   0, 2).
cmd_layout(h4,         end,   0, 2).
cmd_layout(h5,         end,   0, 2).
cmd_layout(h6,         end,   0, 2).
cmd_layout(hr,         begin, 1, 1).
cmd_layout(br,         begin, 0, 1).
cmd_layout(div,        begin, 1, 0).
cmd_layout(div,        end,   0, 1).
cmd_layout(li,         begin, 1, 0).
cmd_layout(dt,         begin, 1, 0).
cmd_layout(dt,         end,   0, 1).
cmd_layout(dd,         begin, 1, 0).
cmd_layout(dd,         end,   0, 1).
cmd_layout(ul,         begin, 1, 1).
cmd_layout(ul,         end,   1, 1).
cmd_layout(ol,         begin, 1, 1).
cmd_layout(ol,         end,   1, 1).
cmd_layout(tr,         begin, 1, 0).
cmd_layout(tr,         end,   0, 1).
cmd_layout(tbody,      begin, 1, 1).
cmd_layout(thead,      begin, 1, 1).
cmd_layout(table,      begin, 1, 1).
cmd_layout(table,      end,   1, 1).
cmd_layout(listing,    begin, 2, 0).
cmd_layout(listing,    end,   0, 2).
cmd_layout(pre,        begin, 2, 0).
cmd_layout(pre,        end,   0, 2).
cmd_layout(xmp,        begin, 2, 0).
cmd_layout(xmp,        end,   0, 2).
cmd_layout(head,       begin, 1, 1).
cmd_layout(head,       end,   1, 1).
cmd_layout(center,     begin, 1, 1).
cmd_layout(center,     end,   1, 1).
cmd_layout(body,       begin, 2, 1).
cmd_layout(body,       end,   1, 1).
cmd_layout(html,       begin, 0, 1).
cmd_layout(html,       end,   1, 1).
cmd_layout(blockquote, begin, 1, 0).
cmd_layout(blockquote, end,   0, 1).
cmd_layout(style,      begin, 1, 1).
cmd_layout(style,      end, 1, 1).
cmd_layout(sloppy,     end,   1, 1).

:- initialization
   read_tex_inputs.


		 /*******************************
		 *            MAIN		*
		 *******************************/

:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, Files, Options),
    set_debugging(Options),
    set_quiet(Options),
    set_text_inputs(Options),
    (   select_option(pl(true), Options, ROptions)
    ->  set_prolog_flag(toplevel_goal, prolog),
        (   input_file(Files, File)
        ->  format('Run using ?- ~q.~n', [latex2html(File, ROptions)])
        ;   true
        )
    ;   input_file(Files, File)
    ->  latex2html(File, Options)
    ;   usage,
        halt(1)
    ).

input_file([File], Base) :-
    file_name_extension(Base, tex, File),
    !.
input_file([File], File).

set_quiet(Options) :-
    option(quiet(true), Options),
    !,
    assert(quiet).
set_quiet(_).

set_text_inputs(Options) :-
    option(texinputs(Val), Options),
    !,
    read_tex_inputs(Val).
set_text_inputs(_).

set_debugging(Options) :-
    (   option(gtrace(true), Options)
    ->  gtrace
    ;   option(trace(true), Options)
    ->  trace
    ;   true
    ).


usage :-
    format('Usage: latex2html [--pl] [--quiet] [--tex_inputs=...] file~n').
