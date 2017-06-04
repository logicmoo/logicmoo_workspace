/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(ws_cover, [cache_file_lines/0]).

:- reexport(library(ws_browser)).
:- use_module(library(ntabling)).
:- use_module(library(apply)).
:- use_module(library(gcover)).
:- use_module(library(http/html_write)).
:- use_module(library(module_files)).
:- use_module(library(pldoc/doc_htmlsrc)).

ws_browser:provides_method(gcover).

:- table
       source_file/1,
       source_file_line/4.

ws_browser:fetch_files_properties_hook(gcover, [ccov, clss, lcov, lits], FileMG) :-
    findall(File-[CCov, Clss, LCov, Lits],
            ( source_file(File),
              cover_info(File, CCov, Clss, LCov, Lits)
            ), FileMU),
    sort(FileMU, FileML),
    group_pairs_by_key(FileML, FileMG).

source_file(File) :-
    distinct(File, covered_db(File, _, _, _, _, _)).

cache_file_lines :-
    findall(File, source_file(File), FileL),
    length(FileL, N),
    forall(nth1(I, FileL, File),
           ( format(user_error, "Caching ~w of ~w files\r", [I, N]),
             ignore(source_file_line(File, _, _, _))
           )),
    nl(user_error).

cover_info(File, CCov, Clss, LCov, Lits) :-
    CountC = count(0, 0),
    CountL = count(0, 0),
    ( source_file_line(File, L1, L2, Scope),
      ( Scope = cl(_)
      ->Count = CountC
      ; Count = CountL
      ),
      Count = count(C1, N1),
      succ(N1, N),
      nb_setarg(2, Count, N),
      ( covered_db(File, L1, L2, _, _, _)
      ->succ(C1, C),
        nb_setarg(1, Count, C)
      ; true
      ),
      fail
    ; true
    ),
    CountC = count(CCov, Clss),
    CountL = count(LCov, Lits).

%! ports_color(List:list(pairs), Color:atm)
%
%  Convention: the color that affects the clause should be
%  darker than those that affects only literals.
%
%  Keep the order since it is the priority.

ports_color([(success)-_, failure-_, multi-_], lightpink).
ports_color([(success)-_, multi-_],            yellowgreen).
ports_color([(success)-_, failure-_],          orange).
ports_color([uncovered-[cl(_)-_]],             bisque).
ports_color([(exit)-_,    fail-_],             yellow).
ports_color([(exit)-_,    call-_],             lime).
ports_color([Port-_], Color) :- port_color(Port, Color).

port_color(exception,    red).
port_color(exception(_), red).
port_color(failure,      orangered).
port_color(success,      greenyellow).
port_color(multi,        green).
port_color(fail,         fuchsia).
port_color(redo,         lightblue).
port_color(redoi,        cyan).
port_color(exit,         greenyellow).
port_color(call,         darkgreen).
% Note that exitcl and unify are converted to failure and success:
port_color(exitcl,       orchid).
port_color(unify,        orange).
port_color(uncovered,    white).

ws_browser:show_source_hook(gcover, File) :-
    format('Content-type: text/html~n~n', []),
    source_to_html(File, stream(current_output),
                   [format_comments(true), skin(coverage_js(File))]).

source_file_line(File, L1, L2, Scope) :-
    file_clause(File, Ref),
    source_clause_line(File, Ref, L1, L2, Scope).

clause_id(Ref, File, CI) :-
    nth_clause(M:H, I, Ref),
    functor(H, F, A),
    ( module_file(M, File)
    ->CI = F/A-I
    ; CI = M:F/A-I
    ).

source_clause_line(File, Ref, L1, L2, cl(CI)) :-
    clause_id(Ref, File, CI),
    clause_property(Ref, line_count(L1)),
    loc_file_line(clause(Ref), File, L1, L2).
source_clause_line(File, Ref, L1, L2, lt(TInstr)) :-
    '$break_pc'(Ref, PC1, _NextPC1),
    '$fetch_vm'(Ref, PC1, PC, TInstr),
    \+ skip_instr(TInstr),
    loc_file_line(clause_pc(Ref, PC), File, L1, L2).

skip_instr(i_cut).
skip_instr(i_enter).
skip_instr(i_exit).

file_clause(File, Ref) :-
    current_predicate(M:F/A),
    functor(H, F, A),
    \+ predicate_property(M:H, imported_from(_)),
    \+ predicate_property(M:H, dynamic),
    nth_clause(M:H, _, Ref),
    clause_property(Ref, file(File)).

%!  covered(+File, -L1, -L2, -Port, -Tag, -Count)
%
%   Get on  backtracking coverage information per  each line, and the  Port that
%   has been tried in the program point  specified by File, L1 and L2, including
%   `uncovered` which is used to detect if such code has been covered or not, in
%   such case the  Tag can be clause  or literal, depending if is  the clause or
%   the  literal that  has not  been  covered. Note  that could  happend that  a
%   covered line  does not have  an 'uncovered' entry,  for instance if  at some
%   late point the system was unable to get the program point.

covered(File, L1, L2, Port, Tag, Count) :-
    covered_db(File, L1, L2, Port, Tag, Count).
covered(File, L1, L2, uncovered, Scope, 0) :-
    source_file_line(File, L1, L2, Scope).

property_lines(File, List, Tail) :-
    findall((L1-L2)-(Port-(Tag-Count)),
            covered(File, L1, L2, Port, Tag, Count),
            Pairs),
    sort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    foldl(property_lines_each, Grouped, List, Tail).

porttags_color(Pairs, Color) :-
    ports_color(Ports, Color),
    subset(Ports, Pairs).

property_lines_each((L1-L2)-PortTagCL) -->
    { group_pairs_by_key(PortTagCL, PortTagCGU),
      ( subtract(PortTagCGU, [uncovered-_], PortTagCG),
        PortTagCG \= []
      ->true
      ; PortTagCG = PortTagCGU
      ),
      once(porttags_color(PortTagCG, Color)),
      findall(L, between(L1, L2, L), LineL)
    },
    foldl(line_color(Color), LineL),
    ['  tT["', L1, '"]="'],
    foldl(port_tags_text, PortTagCG),
    ['";\n'].

line_color(Color, Line) --> ['  lC["', Line, '"]="', Color, '";\n'].

port_tags_text(Port-TagCL) -->
    { group_pairs_by_key(TagCL, TagCG),
      maplist(tag_count, TagCG, TagC)
    },
    [Port, ":", TagC,"\\n"].

tag_count(Tag-L, Tag:S) :-
    sum_list(L, S).

:- public coverage_js/3.

coverage_js(File, header, Out) :-
    phrase(html([script([type('text/javascript')
                        ],
                        ['function updateColorLine(){\n',
                         '  var lC={};\n',
                         '  var tT={};\n',
                         \property_lines(File),
                         '  elements=document.getElementsByClassName("line-no");\n',
                         '  for (var i=0; i < elements.length; i++) {\n',
                         '    var key=elements[i].innerText.trim();\n',
                         '    if (typeof lC[key] !== \'undefined\') {\n',
                         '      elements[i].style.backgroundColor=lC[key];\n',
                         '    };\n',
                         '    if (typeof tT[key] !== \'undefined\') {\n',
                         '      elements[i].style.textDecoration="underline";\n',
                         '      elements[i].classList.add("tooltip");\n',
                         '      var t=document.createElement("span");\n',
                         '      t.classList.add("tooltiptext");\n',
                         '      t.classList.add("tooltiptext::after");\n',
                         '      t.classList.add("tooltip-right");\n',
                         '      var content=document.createTextNode(tT[key]);\n',
                         '      t.appendChild(content);\n',
                         '      elements[i].appendChild(t);\n',
                         '    }\n',
                         '  }\n',
                         '}\n'
                        ]),
                 style([],
                       [
"
span.directive {
    display: inline;
}

.tooltip {
    position: relative;
    display: inline-block;
    border-bottom: 1px dotted #ccc;
    color: #006080;
}

.tooltip .tooltiptext {
    visibility: hidden;
    position: absolute;
    //width: 120px;
    background-color: dimgray;
    color: white;
    text-align: center;
    padding: 5px 0;
    border-radius: 6px;
    z-index: 1;
    opacity: 0;
    transition: opacity 1s;
}

.tooltip:hover .tooltiptext {
    visibility: visible;
    opacity: 1;
}

.tooltip-right {
  top: -5px;
  left: 125%;  
}

.tooltip-right::after {
    content: "";
    position: absolute;
    top: 50%;
    right: 100%;
    margin-top: -5px;
    border-width: 5px;
    border-style: solid;
    border-color: transparent #555 transparent transparent;
}

.tooltip-bottom {
  top: 135%;
  left: 50%;  
  margin-left: -60px;
}

.tooltip-bottom::after {
    content: "";
    position: absolute;
    bottom: 100%;
    left: 50%;
    margin-left: -5px;
    border-width: 5px;
    border-style: solid;
    border-color: transparent transparent #555 transparent;
}

.tooltip-top {
  bottom: 125%;
  left: 50%;  
  margin-left: -60px;
}

.tooltip-top::after {
    content: "";
    position: absolute;
    top: 100%;
    left: 50%;
    margin-left: -5px;
    border-width: 5px;
    border-style: solid;
    border-color: #555 transparent transparent transparent;
}

.tooltip-left {
  top: -5px;
  bottom:auto;
  right: 128%;  
}
.tooltip-left::after {
    content: "";
    position: absolute;
    top: 50%;
    left: 100%;
    margin-top: -5px;
    border-width: 5px;
    border-style: solid;
    border-color: transparent transparent transparent #555;
}
"
                       ])
                ]), Tokens),
    print_html(Out, Tokens).
coverage_js(_, footer, Out) :-
    phrase(html(script([type('text/javascript')
                       ],
                       ['updateColorLine();'])
               ), Tokens),
    print_html(Out, Tokens).
