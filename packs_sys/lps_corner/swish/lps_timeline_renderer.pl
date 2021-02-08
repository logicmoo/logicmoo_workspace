:- module(lps_timeline,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).
:- use_module(swish(lib/render)).

:- multifile user:extra_swish_resource/1. 
user:extra_swish_resource(script([src('/lps/timeline.js')],[])).

:- register_renderer(lps_timeline, "Timeline of a LPS execution").

% adapted from lib/render/c3.pl and com/declarativa/fiji/reporting/timelineTemplate.html
term_rendering(lps_visualization(T,_TwoD), _Vars, _Options) --> 
	{T=_}, % validate T?
	% to add export...:
	html(
		%TODO: add resizing! timeline widget dependent!!!!
		div([ /*class([ 'render-C3','export-dom', 'reactive-size']),*/ 'data-render'('As LPS timeline')],[
		div(id(lps_timeline), []),
		% And now our script:
		% Preloaded from /lps/timeline.js: 
		\js_script({|javascript(T)||
			drawTimeline(T);
		|})
	])).

