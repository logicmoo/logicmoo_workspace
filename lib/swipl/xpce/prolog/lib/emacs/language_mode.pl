/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  1985-2018, University of Amsterdam
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

:- module(emacs_language_mode, []).
:- use_module(library(pce)).
:- use_module(library(hyper)).
:- use_module(library(socket), [gethostname/1]).
:- use_module(library(debug)).
:- use_module(library(atom)).
:- require([ auto_call/1,
             chain_list/2,
             default/3,
             ignore/1,
             member/2,
             pce_define_type/2
           ]).

:- emacs_begin_mode(language, fundamental,
                    "Edit (programming) languages",
        [ indent_line                   = key('TAB'),
          backward_delete_char_untabify = key(backspace),
          align_close_bracket           = key(']') + key('}') + key(')'),
          insert_file_header            = key('\\C-c\\C-f'),
          insert_section_header         = key('\\eh'),
          insert_comment_block          = key('\\C-c\\C-q'),
          insert_line_comment           = key('\\e;'),
          close_block_comment           = key('/'),
          find_tag                      = key('\\e.') + button(browse),
          camelcase_word                = key('\\C-c\\C-c'),
          underscores_word              = key('\\C-c\\C--')
        ],
        [ '"'  = string_quote('"'),
          '''' = string_quote(''''),
          paragraph_end(regex('\\s*\n|/\\* - - -|- - -.*\\*/\n'))
        ]).


variable(bracket_gen,           int*,  get,  "Generation we checked brackets").
variable(bracket_caret,         int*,  get,  "Location we checked brackets").
variable(comment_column,        int,   both, "Column for line comment").
variable(parameter_indentation, int,   both, "Indentation for parameters").
variable(show_line_numbers,     'int|bool', get,  "Show line numbers?").

class_variable(comment_column,        int,        48).
class_variable(show_line_numbers,     'int|bool', 250000).
class_variable(parameter_indentation, int,        4).

setup_mode(E) :->
    "Switch editor into fill-mode"::
    send_super(E, setup_mode),
    send(E, fill_mode, @on),
    send(E, style, matching_bracket, style(background := skyblue)).


                 /*******************************
                 *      COMMENT; HEADERS        *
                 *******************************/

:- pce_group(comment).

line_comment(E, CS:name) :<-
    "Fetch the line-comment start sequence"::
    get(E, syntax, Syntax),
    member(CSlen, [1, 2]),
    get(Syntax, comment_start, CSlen, CS),
    get(Syntax, comment_end, CSlen, CE),
    send(CE, equal, '\n').

line_comment_column_in_context(E) :->
    get(E, line_comment_column_in_context, Col),
    send(E, report, inform, 'Column %d', Col).

line_comment_start(TB, Re, Caret, To, Start) :-
    send(Re, search, TB, Caret, To),
    get(Re, register_start, 1, Start).

line_comment_column_in_context(E, Col:int) :<-
    "Find the column for line comment nearby"::
    member(CSlen, [1, 2]),
    get(E?syntax, comment_start, CSlen, CS),
    !,
    get(E, caret, Caret),
    get(E, text_buffer, TB),
    get(TB, size, TBLen),
    get(regex(''), quote, CS, string(QCS)),
    format(string(ReS), '^\\s*\\S[^\n]*(~w)', [QCS]),
    new(Re, regex(ReS)),
    Start is max(Caret-10000, 0),
    End is min(Caret+10000, TBLen),
    ignore(line_comment_start(TB, Re, Caret, Start, M1)),
    ignore(line_comment_start(TB, Re, Caret, End,   M2)),
    (   nonvar(M1), nonvar(M2)
    ->  (   abs(M1-Caret) < abs(M2-Caret)
        ->  SCM = M1
        ;   SCM = M2
        )
    ;   nonvar(M1)
    ->  SCM = M1
    ;   nonvar(M2)
    ->  SCM = M2
    ),
    get(E, column, SCM, Col).

insert_line_comment(E, Arg:[int]) :->
    "Insert (line) comment"::
    member(CSlen, [1, 2]),
    get(E?syntax, comment_start, CSlen, CS),
    !,
    get(E, caret, Caret),
    get(E, text_buffer, TB),
    get(TB, scan, Caret, line, 0, start, SOL),
    get(TB, scan, Caret, line, 0, end,   EOL),
    (   integer(Arg), Arg > 4
    ->  CC = Arg
    ;   Arg == @default,
        get(E, line_comment_column_in_context, CC),
        CC > 20, CC < 100
    ->  true
    ;   get(E, comment_column, CC)
    ),
    (   get(regex(?(regex(''), quote, CS)), search, TB, SOL, EOL, Start)
    ->  send(E, caret, Start),
        send(E, align, CC),
        send(E, forward_char, CSlen + 1)
    ;   send(E, end_of_line),
        send(E, just_one_space),
        send(E, align, CC),
        get(E?syntax, comment_end, CSlen, CE),
        (   send(CE, equal, '\n')
        ->  send(E, format, '%s ', CS)
        ;   send(E, format, '%s  %s', CS, CE),
            send(E, backward_char, CSlen + 1)
        )
    ).


comment_region(E) :->
    "Toggle-Comment the region using line-comments"::
    get(E, line_comment, Comment),
    get(E, region, tuple(Start, End)),
    get(E, text_buffer, TB),
    get(TB, scan, Start, line, 0, start, S0),
    comment_lines(TB, S0, End, Comment).

comment_lines(_TB, S0, End, _Comment) :-
    S0 >= End,
    !.
comment_lines(TB, S0, End, Comment) :-
    atom_length(Comment, L),
    (   get(regex(''), quote, Comment, RE),
        send(regex(RE), match, TB, S0)
    ->  send(TB, delete, S0, L),
        S1 = S0,
        End1 is End - L
    ;   send(TB, insert, S0, Comment),
        S1 is S0 + L,
        End1 is End + L
    ),
    (   get(TB, scan, S1, line, 1, start, S2)
    ->  comment_lines(TB, S2, End1, Comment)
    ;   true
    ).


fill_comment_paragraph(M, Justify:justify=[bool|int], From:from=[int],
                       Lead0:lead=[char_array]) :->
    "Fill paragraph in (line) comment"::
    (   Lead0 == @default
    ->  (   get(M?syntax, comment_start, 1, CS)
        ->  Lead = CS
        ;   send(M, report, warning, 'No line-comment character defined'),
            fail
        )
    ;   Lead = Lead0
    ),
    new(Re, regex(string('^((%s)?[ \t]*$|[^%s])', Lead, Lead))),
    get(M, caret, Caret),
    get(M, text_buffer, TB),
    (   From \== @default
    ->  Start = From
    ;   get(Re, search, TB, Caret, 0, StartPar),
        get(TB, scan, StartPar, line, 1, start, Start)
    ->  true
    ;   Start = 0
    ),
    (   get(Re, search, TB, Start, End)
    ->  true
    ;   get(TB, size, End)
    ),
    free(Re),
    debug(fill(comment), '~p: filling ~d..~d', [M, Start, End]),
    (   new(LeadRe, regex(string('%s([^\n\t]*)\t[\t]*', Lead))),
        send(LeadRe, match, TB, Start),
        get(LeadRe, register_size, 1, Size),
        Size > 0
    ->  LeadCont = Lead
    ;   nonvar(CS)
    ->  new(LeadRe, regex(string('%s%s*[ \t]*', CS, CS))),
        LeadCont = @default
    ;   new(LeadRe, regex(string('%s[ \t]*', Lead))),
        LeadCont = @default
    ),
    send(M, fill_comment, Start, End, LeadRe, Justify, LeadCont),
    free(LeadRe).

%       ->fill_comment
%
%       Fill a region using a regex that defines leading comment

fill_comment(M,
             Start:from=int, End:to=int,
             Re:leading=regex, Justify:justify=[bool|int],
             LeadCont:lead_continuation=[char_array]) :->
    "Fill paragraph given `leading' regex"::
    (   (Justify == @default ; Justify == @off ; Justify == 0)
    ->  TheJustify = @off
    ;   TheJustify = @on
    ),
    get(M, text_buffer, TB),
    get(M, caret, Caret),
    new(CaretF, fragment(TB, Caret, 0)),
    new(EndF, fragment(TB, End, 0)),
    get(Re, match, TB, Start, LeadChars),
    get(TB, contents, Start, LeadChars, Lead),
    LeadEnd is Start + LeadChars,
    get(M, column, LeadEnd, LeadCol),
    uncomment(M, Re, LeadCol, Start, EndF),
    get(M, right_margin, RM0),
    RM is RM0 - LeadCol,
    get(EndF, start, NewEnd),
    debug(fill(comment), '->fill: ~d ~d', [Start, NewEnd]),
    send(M, fill, Start, NewEnd, 0, RM, TheJustify),
    (   LeadCont == @default
    ->  TheLeadCont = Lead
    ;   TheLeadCont = LeadCont
    ),
    comment(M, Start, EndF, Lead, TheLeadCont, LeadCol),
    send(M, caret, CaretF?start),
    free(EndF),
    free(CaretF).

uncomment(_M, _Re, _LeadCol, Here, EndF) :-
    get(EndF, start, End),
    Here >= End,
    !.
uncomment(M, Re, LeadCol, Here, EndF) :-
    get(M, text_buffer, TB),
    get(Re, match, TB, Here, Len),
    send(M, caret, Here),
    send(M, column, LeadCol),
    get(M, caret, AtLeadCol),
    DelLen is min(Len, AtLeadCol-Here),
    send(TB, delete, Here, DelLen),
    get(TB, scan, Here, line, 1, start, NextHere),
    uncomment(M, Re, LeadCol, NextHere, EndF).

comment(_, Here, EndF, _, _, _Col) :-
    get(EndF, start, End),
    Here >= End,
    !.
comment(M, Here, EndF, Lead, LeadCont, Col) :-
    get(M, text_buffer, TB),
    send(TB, insert, Here, Lead),
    get(Lead, size, Size),
    send(M, align, Col, Here+Size),
    get(TB, scan, Here, line, 1, start, NextHere),
    comment(M, NextHere, EndF, LeadCont, LeadCont, Col).


insert_comment_block(E) :->
    "Insert header/footer for long comment"::
    send(E, insert,
'/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
'),
    send(E, previous_line, 2).


insert_section_header(E) :->
    "Insert Prolog/C section header"::
    send(E, insert,
'\t\t /*******************************
\t\t *               C\t\t*
\t\t *******************************/
').

                 /*******************************
                 *          FILE HEADER         *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->insert_file_header
        Inserts a .fileheader it finds in the current directory or one of
        its parent directories.  If no file it found it uses a default.

        Next it makes the substitutions from file_header_parameter/3 and
        finally, if the header contains %. it removes this and sets the
        caret at this position.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

insert_file_header(M) :->
    "Insert .fileheader or default file-header"::
    get(M, directory, Dir),
    find_file_header(Dir, Header),
    (   file_header_parameter(Escape, M, Value),
        substitute(Header, Escape, Value),
        fail
    ;   true
    ),
    (   new(Here, regex('%\\.')),
        send(Here, search, Header)
    ->  send(Here, register_value, Header, ''),
        get(Here, register_start, Offset),
        get(M, caret, Caret),
        send(M, insert, Header),
        send(M, caret, Caret+Offset)
    ;   send(M, insert, Header)
    ).


find_file_header(Dir, Header) :-
    get(Dir, file, '.fileheader', File),
    send(File, access, read),
    !,
    send(File, open, read),
    get(File, read, Header),
    send(File, close).
find_file_header(Dir, Header) :-
    get(Dir, parent, Parent),
    find_file_header(Parent, Header).
find_file_header(_, Header) :-
    new(Header, string),
    send_list(Header, append,
              [ '/*  File:    %F\n',
                '    Author:  %U\n',
                '    Created: %D\n',
                '    Purpose: %.\n',
                '*/\n\n'
              ]).

file_header_parameter('%Y', _,  Year) :-
    get(new(date), year, Year).
file_header_parameter('%F', M, FileName) :-
    get(M?file, base_name, FileName).
file_header_parameter('%U', _, UserName) :-
    get(@pce, user, User),
    (   get(@pce, user_info, gecos, User, UserName)
    ->  true
    ;   UserName = User
    ).
file_header_parameter('%E', _, Email) :-
    (   getenv('EMAIL', Email)
    ->  true
    ;   get(@pce, user, User),
        gethostname(Host),
        atomic_list_concat([User, Host], @, Email)
    ).
file_header_parameter('%D', _, Date) :-
    new(D, date),
    get(D, year, Year),
    get(D, month_name, @on, Month),
    get(D, day, Day),
    new(Date, string('%s %2d %d', Month, Day, Year)).

substitute(String, From, To) :-
    send(regex(From), for_all, String,
         message(@arg1, replace, @arg2, To)).


                 /*******************************
                 *             FILLING          *
                 *******************************/

:- pce_group(fill).


fill_paragraph(M, Justify:[int]) :->
    "Fill comment paragraph"::
    get(M, caret, Caret),
    (   in_fillable_comment(M, Caret, Lead)
    ->  (   get(M?syntax, comment_start, 1, Lead)
        ->  send(M, fill_comment_paragraph, Justify)
        ;   send(M, fill_comment_paragraph, Justify, lead := Lead)
        )
    ;   send_super(M, fill_paragraph, Justify)
    ).

%!  in_fillable_comment(+Mode, +Caret, -Lead) is semidet.
%
%   True when Caret is inside a comment starting at the beginning of a
%   line.

in_fillable_comment(M, Caret, Lead) :-
    get(M, scan_syntax, 0, Caret, tuple(comment, Start)),
    (   get(M?syntax, comment_start, 1, CS),
        get(M, column, Start, 0),
        send(M, looking_at, CS, Start)
    ->  Lead = CS
    ;   send(M, looking_at, '/\\*', Start),
        get(M, scan, Caret, line, 0, start, SOL),
        send(M, looking_at, '[ \t]*\\*[ \t]', SOL)
    ->  Lead = '[ \t]*\\*'
    ),
    !.
in_fillable_comment(M, Caret, CS) :-
    get(M, column, Caret, 0),
    get(M?syntax, comment_start, 1, CS),
    send(M, looking_at, CS, Caret).


justify_paragraph(M) :->
    "->fill_paragraph using right-margin"::
    send(M, fill_paragraph, 1).


auto_fill(M, From:[int], Regex:[regex]) :->
    "Auto fill in comment mode"::
    (   From == @default
    ->  get(M, caret, Caret)
    ;   Caret = From
    ),
    (   get(M, scan_syntax, 0, Caret, tuple(comment, Start)),
        get(M, scan, Caret, line, 0, start, SOL)
    ->  (   in_fillable_comment(M, Caret, Lead)
        ->  (   get(M?syntax, comment_start, 1, Lead)
            ->  send(M, fill_comment_paragraph, @off, SOL)
            ;   send(M, fill_comment_paragraph, @off, SOL, Lead)
            )
        ;   get(M, column, Start, 0),
            get(M, editor, Editor),
            send_class(Editor, editor, auto_fill(Caret, Regex))
        )
    ).


                 /*******************************
                 *          INDENTATION         *
                 *******************************/

:- pce_group(indent).

newline_and_indent(E, Arg:[int]) :->
    "Insert newline and indent as TAB"::
    send(E, newline, Arg),
    send(E, indent_line).


indent_line(E) :->
    "Indent current line"::
    send(E, beginning_of_text_on_line),
    (   send(E, indent_close_bracket_line)
    ;   send(E, indent_expression_line)
    ;   send(E, indent_comment_line)
    ;   send(E, align_with_previous_line)
    ).


indent_close_bracket_line(E, Brackets:[name], Base:[int]) :->
    "Indent a line holding a bracket"::
    default(Brackets, ')}]', B1),
    get(E, text_buffer, TB),
    get(E, caret, Caret),
    get(TB, character, Caret, Char),
    get(B1, index, Char, _),
    get(TB, matching_bracket, Caret, OpenPos),
    (   Base \== @default
    ->  OpenPos >= Base
    ;   true
    ),
    (   get(E, argument_indent, OpenPos, Col)
    ->  true
    ;   get(E, column, OpenPos, Col)
    ),
    send(E, align_line, Col).


%       ->indent_expression_line
%
%       This deal with the following layouts:
%
%         ==
%               [ aap,
%                 noot
%               | mies
%               ]
%
%         functor(arg1,
%                 arg2)
%
%         functor(
%            arg1,
%            arg2)
%         ==

indent_expression_line(E, Brackets:[name], Base:[int]) :->
    "Indent current line according to expression"::
    default(Brackets, ')}]', B1),
    atom_codes(B1, B2),
    get(E, text_buffer, TB),
    member(Bracket, B2),
        pce_catch_error(mismatched_bracket,
                        get(TB, matching_bracket, E?caret,
                            Bracket, OpenPos)),
        !,
        (   Base \== @default
        ->  OpenPos >= Base
        ;   true
        ),
        (   send(E, looking_at, '[,|]')     % line starts with , or |
        ->  get(E, column, OpenPos, Col)
        ;   get(E, argument_indent, OpenPos, StartCol)
        ->  get(E, parameter_indentation, PI),
            Col is StartCol+PI
        ;   get(TB, scan, OpenPos, line, 0, end, EOL),
            get(TB, skip_comment, OpenPos+1, EOL, P1),
            get(E, column, P1, Col)
        ),
        send(E, align_line, Col),
    !.


argument_indent(E, OpenPos:int, StartCol:int) :<-
    "Get column for indented arguments from OpenPos"::
    get(E, text_buffer, TB),
    get(TB, scan, OpenPos, line, 0, end, EOL),
    get(TB, skip_comment, OpenPos+1, EOL, P1),
    P1 == EOL,
    get(E, scan, OpenPos, term, -1, start, SOT),
    get(E, column, SOT, StartCol).


%       ->indent_comment_line
%

indent_comment_line(M) :->
    "Copy leading comment of previous line"::
    get(M, text_buffer, TB),
    get(M, caret, Caret),
    get(M, scan, Caret, line, -1, start, SOPL),
    debug(indent(comment), 'Prev line at ~d', [SOPL]),
    (   get(TB?syntax, comment_start, 1, CS),
        new(LeadRe, regex(string('%s[ \t]*', CS))),
        get(LeadRe, match, TB, SOPL, Len),  % Previous holds comment
        CLine = string('%s?[ \t]*$', CS)
    ->  true
    ;   get(TB?syntax, comment_start, 2, '/*'),
        new(LeadRe, regex(string('[ \t]*(/\\*|\\*)[ \t]*'))),
        get(LeadRe, match, TB, SOPL, Len),
        debug(indent(comment), ' *-match', []),
        get(M, scan_syntax, 0, Caret, tuple(comment, StartComment)),
        send(M, looking_at, '/\\*', StartComment),
        CLine = ' *\\*?[ \t]*$',
        BlockComment = true
    ),
    get(M, scan, Caret, line, 0, start, SOL),
    send(M, looking_at, CLine, SOL),
    get(TB, contents, SOPL, Len, Lead),
    (   BlockComment == true
    ->  (   send(Lead, sub, /)
        ->  send(Lead, translate, /, ' '),
            send(Lead, append, ' ')
        ;   true
        )
    ;   true
    ),
    get(M, scan, SOL, line, 0, end, EOL),
    send(TB, delete, SOL, EOL-SOL),
    send(M, insert, Lead).


close_block_comment(M, Times:[int], Id:[event_id]) :->
    "Close a /** ... */ multiline comment"::
    (   Times == @default,
        get(M?syntax, comment_start, 2, '/*'),
        get(M, caret, Caret),
        get(M, scan, Caret, line, 0, start, SOL),
        send(M, looking_at, '[ \t]*\\*[ \t]*$', SOL)
    ->  send(M, just_one_space),
        send(M, backward_delete_char),
        send(M, insert, /)
    ;   send(M, insert_self, Times, Id)
    ).


                /********************************
                *           ALIGNMENT           *
                ********************************/

alignment_of_previous_line(E, Leading:[regex], Indent:int) :<-
    "Find the indentation of the previous line"::
    get(E, caret, Caret),
    get(E, scan, Caret, line, -1, start, LineStart),
    get(E, scan, Caret, term, -1, start, TermStart),
    (   TermStart < LineStart
    ->  get(E, indentation, TermStart, Leading, Indent)
    ;   get(E, indentation, LineStart, Leading, Indent)
    ).


align_with_previous_line(E, Leading:[regex]) :->
    "Align current_line with the one above"::
    get(E, alignment_of_previous_line, Leading, Indent),
    send(E, align_line, Indent).


align_close_bracket(E, Times:[int], Id:[event_id]) :->
    "Insert and align with matching open bracket"::
    send(E, insert_self, Times, Id),
    get(E, caret, Caret),
    get(E, scan, Caret, line, 0, start, SOL),
    get(E, scan, Caret, line, 0, end,   EOL),
    (   get(E, skip_comment, SOL, EOL, P0),
        Caret =:= P0+1
    ->  send(E, indent_line),
        send(E, forward_char)
    ;   true
    ).


                 /*******************************
                 *           UNTABIFY           *
                 *******************************/

backward_delete_char_untabify(M, Times:[int]) :->
    "Delete characters backward"::
    get(M, caret, Caret),
    get(M, character, Caret-1, Char),
    (   send(M?syntax, has_syntax, Char, white_space)
    ->  get(M, column, Col),
        default(Times, 1, Tms),
        send(M, align, Col-Tms),
        (   get(M, column, Col)
        ->  send(M, backward_delete_char, Times)
        ;   true
        )
    ;   send(M, backward_delete_char, Times)
    ).


                 /*******************************
                 *            TAGS              *
                 *******************************/

:- initialization
   pce_define_type(emacs_tag, name).

visit_tag_table(M, Table:tag_file='file|directory') :->
    "Load specified GNU-Emacs (etags) tag-table"::
    (   send(Table, instance_of, directory)
    ->  find_tag_from_dir(Table, TagFile)
    ;   TagFile = Table
    ),
    get(TagFile, absolute_path, TagFileName),
    (   send(TagFile, access, read)
    ->  auto_call(emacs_init_tags(TagFileName)),
        send(M, report, status, 'Loaded TAG table %s', TagFileName)
    ;   send(M, report, warning, '%s: not accessible', TagFileName),
        fail
    ).

find_tag_from_dir(Dir, File) :-
    get(Dir, file, 'TAGS', File),
    send(File, exists),
    !.
find_tag_from_dir(Dir, File) :-
    get(Dir, parent, Parent),
    find_tag_from_dir(Parent, File).

ensure_loaded_tags(M) :->
    "Make sure we have a tag-table loaded"::
    (   auto_call(emacs_tag_file(_))
    ->  emacs_update_tags
    ;   get(M, directory, Dir),
        (   send(?(Dir, file, 'TAGS'), exists)
        ->  get(Dir, path, Path),
            auto_call(emacs_init_tags(Path))
        ;   send(M, noarg_call, visit_tag_table)
        )
    ).

default_tag(M, DefTag:name) :<-
    "Return default tag from current word"::
    (   get(M, word, Word),
        send(regex('[a-zA-Z0-9_]*$'), match, Word)
    ->  DefTag = Word
    ;   DefTag = ''
    ).


expand_tag(M, Tag:[name], TheTag:name) :<-
    "Expand tag using tag-table"::
    send(M, ensure_loaded_tags),
    (   Tag == @default
    ->  get(M, word, DefTag),
        new(I, emacs_tag_item('Find tag', DefTag)),
        get(M, prompt_using, I, TagString),
        get(TagString, value, TheTag)
    ;   TheTag = Tag
    ).


find_tag(M, Tag:emacs_tag, Where:[{here,tab,window}], Editor:editor) :<-
    "Jump to indicated tag entry"::
    (   get(M, directory, Dir),
        find_tag_from_dir(Dir, TagFile),
        get(TagFile, directory_name, TagDirName),
        (   SearchDir = TagDirName
        ;   true
        ),
        debug(emacs(tag), 'Search ~q from ~q', [Tag, SearchDir]),
        emacs_update_tags,
        auto_call(emacs_tag(Tag, SearchDir, File, Line))
    ->  new(B, emacs_buffer(File)),
        get(B, open, Where, Frame),
        get(Frame, editor, Editor),
        send(M, location_history),
        send(Editor, line_number, Line),
        adjust_tag(Editor, Tag),
        send(M, location_history, title := Tag)
    ;   send(M, report, warning, 'Cannot find tag %s', Tag),
        fail
    ).


find_tag(M, Tag:emacs_tag) :->
    "Jump to entry from TAG table"::
    ignore(get(M, find_tag, Tag, tab, _)). % avoid delegation to menu-bar


adjust_tag(E, Tag) :-
    get(E, text_buffer, TB),
    get(E, caret, Here),
    new(Re, regex('')),
    get(Re, quote, Tag, QTag),
    (   send(Re, pattern, string('\\\\y%s\\\\y', QTag))
    ;   send(Re, pattern, string('\\\\y%s', QTag))
    ),
    closest(Re, TB, Here, Pos),
    !,
    send(E, caret, Pos).


closest(Re, TB, Here, Pos) :-
    get(Re, search, TB, Here, P1),
    !,
    (   get(Re, search, TB, Here, 0, P2)
    ->  closest_element(P1, P2, Here, Pos)
    ;   Pos = P1
    ).
closest(Re, TB, Here, Pos) :-
    get(Re, search, TB, Here, 0, Pos).

closest_element(A, B, Here, A) :-
    abs(A-Here) =< abs(B-Here),
    !.
closest_element(A, B, Here, B) :-
    abs(B-Here) < abs(A-Here).


                 /*******************************
                 *         MISCELLANEOUS        *
                 *******************************/

beginning_of_text_on_line(E) :->
    "Position caret at first non-white on line"::
    get(E, caret, Caret),
    get(E, scan, Caret, line, 0, start, SOL),
    (   get(E?syntax, comment_start, 1, CS),
        new(LeadRe, regex(string('%s[ \t]*', CS))),
        get(E, text_buffer, TB),
        get(LeadRe, match, TB, SOL, Len)
    ->  AtText is SOL+Len,
        send(E, caret, AtText)
    ;   get(E, scan, Caret, line, 0, end,   EOL),
        get(E, skip_comment, SOL, EOL, P0),
        send(E, caret, P0)
    ).


new_caret_position(M, Caret:int) :->
    "Update line number"::
    send_super(M, new_caret_position, Caret),
    (   get(M, frame, Frame),
        send(Frame, has_send_method, show_line_number)
    ->  get(M, show_line_numbers, How),
        (   How == @off
        ->  send(Frame, show_line_number, @nil)
        ;   (   (   integer(How)
                ->  Caret =< How
                ;   get(M, show_line_numbers, @on)
                )
            ->  get(M, line_number, Line),
                send(Frame, show_line_number, Line)
            ;   send(Frame, show_line_number, too_expensive)
            )
        ;   true
        )
    ;   true
    ),
    send(M, highlight_matching_bracket, Caret).


show_line_numbers(M, Show:bool) :->
    "Show/do not show line numbers"::
    send(M, slot, show_line_numbers, Show),
    send(M, new_caret_position, M?caret).


what_syntax(E) :->
    "Find syntax at caret"::
    get(E, caret, C),
    get(E, scan_syntax, 0, C, tuple(Syntax, Start)),
    send(E, report, inform,
         'Syntax: "%s"; started at %d', Syntax, Start).


                 /*******************************
                 *       MATCHING BRACKETS      *
                 *******************************/

highlight_matching_bracket(M, CaretSpec:[int]) :->
    (   CaretSpec == @default
    ->  get(M, caret, Caret)
    ;   Caret = CaretSpec
    ),
    (   send(M, highlight_up_to_date, Caret)
    ->  true
    ;   get(M, text_buffer, TB),
        get(TB, syntax, SyntaxTable),
        (   (   Here = Caret,
                get(TB, character, Here, Char),
                send(SyntaxTable, has_syntax, Char, open_bracket)
            ;   Here is Caret - 1,
                get(TB, character, Here, Char),
                send(SyntaxTable, has_syntax, Char, close_bracket)
            )
        ->  get(TB, matching_bracket, Here, Match),
            send(M, show_matching_bracket_fragment, Match)
        ;   send(M, unshow_matching_bracket_fragment)
        )
    ;   true
    ).

highlight_up_to_date(M, Caret:int) :->
    "True if highlight indication is up-to-date"::
    get(M, text_buffer, TB),
    get(TB, generation, Gen),
    (   get(M, bracket_caret, Caret),
        get(M, bracket_gen, Gen)
    ->  true
    ;   send(M, slot, bracket_caret, Caret),
        send(M, slot, bracket_gen, Gen),
        fail
    ).

show_matching_bracket_fragment(M, At:int) :->
    "Show matching bracket at position"::
    get(M, text_buffer, TB),
    (   get(TB, hypered, matching_bracket_fragment, F)
    ->  send(F, start, At),
        send(F, length, 1)
    ;   new(_, partof_hyper(TB,
                            fragment(TB, At, 1, matching_bracket),
                            matching_bracket_fragment,
                            text_buffer))
    ).

unshow_matching_bracket_fragment(M) :->
    "Delete the matching bracket fragment"::
    get(M, text_buffer, TB),
    (   get(TB, hypered, matching_bracket_fragment, F)
    ->  free(F)
    ;   true
    ).


                 /*******************************
                 *      RESTYLE IDENTIFIERS     *
                 *******************************/

camelcase_word(M, Arg:[int]) :->
    "Change word to CamelCase"::
    send(M, restyle_word, 'OneTwo', Arg).

underscores_word(M, Arg:[int]) :->
    "Change word to undercore_mode"::
    send(M, restyle_word, one_two, Arg).

restyle_word(M, Style:{'OneTwo',oneTwo,one_two,'One_Two'}, Arg:[int]) :->
    "Restyle the current identifier word"::
    default(Arg, 1, Times),
    forall(between(1, Times, _),
           ( (   send(M, looking_at, '\\w')
             ->  get(M, caret, Here)
             ;   get(M, caret, Caret),
                 get(M, scan, Caret, word, 1, start, Here)
             ),
             get(M, scan, Here, word, 0, end, End),
             get(M, contents, Here, End-Here, string(Word)),
             restyle_identifier(Style, Word, NewWord),
             send(M, delete, Here, End),
             send(M, caret, Here),
             send(M, insert, NewWord))).


                 /*******************************
                 *            HELP              *
                 *******************************/

prolog_manual(_, On:[name]) :->
    "Open Prolog manual"::
    (   On == @default
    ->  help
    ;   help(On)
    ).

:- emacs_end_mode.

