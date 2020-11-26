/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2016, VU University Amsterdam
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

:- module(swish_render_tiles,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(http/html_write)).
:- use_module('../render').

:- register_renderer(tiles, "Render chess board representations").

/** <module> SWISH chessboard renderer

Render chessboards. Currently only deals with the N-queens problem. This
file is nevertheless called =chess.pl= because   it should be trivial to
extend this to more general chess positions.

The   styling   is    a    small     modification    of    [CSS3   Chess
Board](http://designindevelopment.com/css/css3-chess-board/)
*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Render an N-queens  problem.  This   renderer  assumes  that the
%	solution is represented by a permutation   of a list of integers
%	1..N, where the I-th integer describes   the column of the queen
%	at row I.

term_rendering(Term, _Vars, _Options) -->
	{ is_map(Term,Tiles),
    Term=..[_,_|Rows],
	  length(Rows, N),
	  LineHeight0 is ceiling(500/N),
    (LineHeight0>32->
      LineHeight=LineHeight0
    ;
      LineHeight=32
    )
	},
	html(div([ style('display:inline-block;'
			),
		   'data-render'('Tile map')
		 ],
		 [table(\tiles(Rows,Tiles,LineHeight)),
		   \tile_map_style
		 ])).

is_map(Term,Tiles) :-
  Term=..[map,Tiles,R1|Rows],
  is_list(Tiles),
  R1=..[_|Row1],
  length(Row1,W),
  maplist(row_length(W),Rows).

row_length(W,Row):-
  Row=..[row|R],
  length(R,W).

tiles([],_Tiles,_Width) --> [].
tiles([H|T],Tiles,Width) -->
  {H=..[_|Row]},
	html(tr(\nrow(Row,Tiles,Width))),
	tiles(T,Tiles,Width).

nrow([],_Tiles,_Width) --> !.
nrow([Tile|TTiles],Tiles,Width) -->
	{ member(tile(Tile,URL),Tiles) },
	  html(td(img([src(URL),alt(Tile),width(Width)],[]))),
	nrow(TTiles,Tiles,Width).


tile_map_style -->
	html({|html||
<style>
table { border-collapse: collapse; padding: 0px; }
table tr td { padding: 0px; border-collapse: collapse;}
</style>
	     |}).
