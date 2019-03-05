:- module(lps_2d,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).
:- use_module('../../swish/lib/render').

:- register_renderer(lps_2d, "2d world display of a LPS execution").

term_rendering(lps_visualization(_T,TwoD), _Vars, _Options) --> 
	{TwoD \= []},
	% TODO: add export
	html(
		div([ 'data-render'('As LPS 2D world')],[
		canvas([id(lps_2dworld)], []),
		% Depends on /lps/2dWorld.js
		\js_script({|javascript(TwoD)||
			twoDworld(TwoD);
		|})
	])).

