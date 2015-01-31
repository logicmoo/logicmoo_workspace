:- module(ws_cover, []).

:- reexport(library(ws_browser)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(location_utils)).
:- use_module(library(maplist_dcg)).
:- use_module(library(group_pairs_or_sort)).
:- use_module(library(gcover)).
:- use_module(library(module_files)).

:- http_handler(root(colors_code), colors_code, []). % /module?file=<file>

colors_code(_) :-
    reply_html_page([title('Colors Code')
		    ],
		    [table([border(1)],
			   [\header,
			    \enum_colors
			   ])
		    ]).

code_webcolor(Color, C) :-
    format(atom(C), '#~|~`0t~16R~6+', [Color]).

enum_colors -->
    { findall(Port, port_color(Port, _), PortL),
      oset_power(PortL, PortLL)
    },
    maplist_dcg(enum_colors_row(PortL), PortLL).

enum_colors_row(PortL, SubPortL) -->
    { maplist(enum_colors_cell(SubPortL), PortL, TDL),
      ports_color(SubPortL, Color),
      code_webcolor(Color, AC),
      append(TDL, [td([bgcolor=AC], '')], TD)
    },
    html(tr(TD)).

enum_colors_cell(SubPortL, Port, td(T)) :-
    ( memberchk(Port, SubPortL)
    ->T='X'
    ; T='-'
    ).

header -->
    {findall(td([bgcolor=AC], b(Text)),
	     ( port_color(Port, Color),
	       code_webcolor(Color, AC),
	       term_to_atom(Port, Text)
	     ), HCols, [td(b(color))])},
    html(tr(HCols)).

:- multifile
    ws_browser:provides_method/1,
    ws_browser:fetch_module_files_hook/2,
    ws_browser:show_source_hook/2.

ws_browser:provides_method(gcover).

ws_browser:fetch_module_files_hook(gcover, ModuleFiles) :-
    findall(M-File,
	    ( covered_db(_, _, File, _, _, _),
	      module_file(M, File)
	    ), MFileU),
    sort(MFileU, MFileS),
    group_pairs_by_key(MFileS, ModuleFiles).

port_color(call,         rgb(0xFF,0xFF,0x00)). % yellow
% port_color(call,        0x000000). % black
port_color(exception,    rgb(0xFF,0x00,0x00)). % red.
port_color(exception(_), rgb(0xFF,0x00,0x00)). % red.
port_color(exit,         rgb(0x00,0xFF,0x00)). % green
port_color(fail,         rgb(0xFF,0x00,0xFF)). % fuchsia
port_color(redo(0),      rgb(0x00,0x00,0xFF)) :- !. % blue.
port_color(redo(_),      rgb(0xFF,0xFF,0xFF)). % ignore, white, but show in hint
port_color(redo,         rgb(0x00,0x00,0xFF)). % blue.
port_color(unify,        rgb(0xA9,0xA9,0xA9)). % gray.

% port_text(call, call).
% port_text(exception, exception).
% port_text(exit, exit).
% port_text(fail, fail).
% port_text(redo(_), redo).
% port_text(unify, unify).

add_port_colors(Port, rgb(R0,G0,B0), rgb(R,G,B)) :-
    port_color(Port, rgb(R1,G1,B1)),
    R is R0+R1,
    G is G0+G1,
    B is B0+B1.

rgb_color(rgb(R, G, B), Color) :-
    Color is R * 0x10000 + G * 0x100 + B.

ports_color(PortL, Color) :-
    memberchk(fail, PortL),
    \+ memberchk(exit, PortL), !,
    port_color(fail, RGB),
    rgb_color(RGB, Color).
ports_color(PortL, Color) :-
    memberchk(exit, PortL),
    \+ memberchk(fail, PortL), !,
    port_color(exit, RGB),
    rgb_color(RGB, Color).
ports_color(PortL, Color) :-
    memberchk(fail, PortL),
    memberchk(exit, PortL), !,
    rgb_color(rgb(0xFF,0xA5,0x00), Color).
ports_color(PortL, Color) :-
    member(Port, [fail,exit]),
    memberchk(Port, PortL), !,
    port_color(Port, RGB),
    rgb_color(RGB, Color).
ports_color(PortL, Color) :-
    maplist_dcg(add_port_colors, PortL, rgb(0,0,0), ColorT),
    length(PortL, N),
    ( N \= 0
    ->ColorT=rgb(RT,GT,BT),
      R is RT//N, G is GT//N, B is BT//N,
      rgb_color(rgb(R,G,B),Color)
    ; Color = 0xFFFFFF
    ).

ctx(PortL, Text, HTML) :-
    sort(PortL, PortS),
    maplist(term_to_atom, PortS, PortT),
    ports_color(PortL, Color),
    code_webcolor(Color, C),
    format(atom(AC), 'background:~a', [C]),
    format(atom(PortA), 'ports=~w',[PortT]),
    HTML=span([style=AC, title=PortA], Text).

context_port(fr-PortTagL, Port0, Port) :-
    pairs_keys(PortTagL, PortL),
    append(Port0, PortL, Port).

context_port(to-PortTagL, Port0, Port) :-
    pairs_keys(PortTagL, PortL),
    subtract(Port0, PortL, Port).

gcover_format(Pos-CovL,
	      gf(Pos0, CtxC, Raw0, [HTML|Tail]),
	      gf(Pos,  CtxL, Raw1, Tail)) :-
    Length is Pos-Pos0,
    sub_string(Raw0, 0, Length, After, Text),
    enum_lines(Text, HTML0 ),
    sub_string(Raw0, _, After,  0,     Raw1),
    ( CtxC = []
    ->HTML=span([], HTML0 )
    ; ctx(CtxC, HTML0, HTML)
    ),
    maplist_dcg(context_port, CovL, CtxC, CtxL).

enum_lines(Text, HTML) :-
    ( sub_string(Text, Before, L, After, '\n')
    ->B is Before+L,
      sub_string(Text, 0, B, _, Line),
      sub_string(Text, _, After, 0, Text2),
      HTML=[Line, i([],'')|HTML2],
      enum_lines(Text2, HTML2)
    ; HTML=[Text]
    ).

ws_browser:show_source_hook(gcover, File) :-
    directory_file_path(_, Name, File),
    read_file_to_string(File, Raw, []),
    string_length(Raw, Length),
    findall(Pos,
	    ( covered_db(Fr, To, File, Port, Tag, _Count),
	      ( Pos=Fr-(fr-(Port-Tag))
	      ; Pos=To-(to-(Port-Tag))
	      )
	    ), CovU),
    sort(CovU, CovL),
    group_pairs_or_sort(CovL, CovG0),
    append(CovG0, [Length-[]], CovG),
    maplist_dcg(gcover_format, CovG, gf(0, [], Raw, Text), gf(Length, [], "", "")),
    reply_html_page([title(Name),
		     style('pre.code { counter-reset: listing; }\n\c
			  .code i:before { counter-increment: listing; content: counter(listing) ". "; color: gray;}\n\c
			  .code i { float: left; clear: both; min-width: 3.5em; }\n\c
			  .code:before { counter-increment: listing; content: counter(listing) ". "; color: gray; display: inline-block; min-width: 3.5em; }\n\c
			  ')
		    ],
		    [pre([class="code"], Text)]).
