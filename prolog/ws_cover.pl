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

:- module(ws_cover, []).

:- reexport(library(ws_browser)).
:- use_module(library(apply)).
:- use_module(library(gcover)).
:- use_module(library(http/html_write)).
:- use_module(library(module_files)).
:- use_module(library(tabling)).
:- use_module(library(pldoc/doc_htmlsrc)).

ws_browser:provides_method(gcover).

ws_browser:fetch_module_files_hook(gcover, ModuleFiles) :-
    findall(M-File,
            ( covered_db(File, _, _, _, _),
              module_file(M, File)
            ), MFileU),
    sort(MFileU, MFileS),
    group_pairs_by_key(MFileS, ModuleFiles).

% This is in order of priority
port_color(exception,    red).
port_color(exception(_), red).
port_color(fail,         magenta).
port_color(redo(_),      blue).
% port_color(redo(X),      white) :- nonvar(X), X \= 0.
port_color(redo,         blue).
port_color(exit,         lime).
port_color(unify,        orange).
port_color(call,         yellow).

ws_browser:show_source_hook(gcover, Module, File) :-
    format('Content-type: text/html~n~n', []),
    source_to_html(File, stream(current_output),
                   [format_comments(true), skin(coverage_js(Module, File))]).

:- table
   file_line_end/4.

file_line_end(Module, File, L1, L2) :-
    catch(open(File, read, In), _, fail),
    set_stream(In, newline(detect)),
    call_cleanup(
        ( read_source_term_at_location(
              In, _,
              [ line(L1),
                module(Module)
              ]),
          stream_property(In, position(Pos)),
          stream_position_data(line_count, Pos, L2)
        ),
        close(In)).

property_lines(Module, File) -->
    { findall(Line-(Port-Tag), covered_db(File, Line, Port, Tag, _), Pairs),
      sort(Pairs, Sorted),
      group_pairs_by_key(Sorted, Grouped)
    },
    foldl(property_lines_each(Module, File), Grouped).

ports_color(Pairs, Color) :-
    port_color(Port, Color),
    memberchk(Port-_, Pairs).

property_lines_each(Module, File, Line-PortTagL) -->
    { group_pairs_by_key(PortTagL, PortTagG),
      once(ports_color(PortTagG, Color)),
      file_line_end(Module, File, Line, LEnd),
      findall(L, between(Line, LEnd, L), LL)
    },
    foldl(line_color(Color), LL),
    ['  tT["', Line, '"]="'],
    foldl(port_tags_text, PortTagG),
    ['";\n'].

line_color(Color, Line) --> ['  lC["', Line, '"]="', Color, '";\n'].

port_tags_text(Port-TagL) --> [Port, ":", TagL,"\\n"].

:- public coverage_js/4.

coverage_js(Module, File, header, Out) :-
    phrase(html([script([type('text/javascript')
                        ],
                        ['function updateColorLine(){\n',
                         '  var lC={};\n',
                         '  var tT={};\n',
                         \property_lines(Module, File),
                         '  elements=document.getElementsByClassName("line-no");\n',
                         '  for (var i=0; i < elements.length; i++) {\n',
                         '    var key=elements[i].innerText.trim();\n',
                         '    if (typeof lC[key] !== \'undefined\') {\n',
                         '      elements[i].style.backgroundColor=lC[key];\n',
                         '    };\n',
                         '    if (typeof tT[key] !== \'undefined\') {\n',
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
coverage_js(_, _, footer, Out) :-
    phrase(html(script([type('text/javascript')
                       ],
                       ['updateColorLine();'])
               ), Tokens),
    print_html(Out, Tokens).
