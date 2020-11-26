/* <module> swish_render_html
% Provides HTML Rendering in SWISH
%
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- if(exists_source(swish(lib/render))).

:- module(swish_render_html,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(http/html_write)).
:- use_module(swish(lib/render)).

:- register_renderer(html, "Render html representations").

/* <module> SWISH html renderer

*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Render an N-queens  problem.  This   renderer  assumes  that the
%	solution is represented by a permutation   of a list of integers
%	1..N, where the I-th integer describes   the column of the queen
%	at row I.



term_rendering(Term, _Vars, _Options) --> { compound(Term),Term=html(_) % ,  pengines:not_sandboxed(_User, swish) 
  }, html(Term).

:- endif.

end_of_file.

 X = cp_menu:menu([places-[item(100,home,'Home'),item(200,list_graphs,'Graphs'),item(200,list_prefixes,'Prefixes')],
                   admin-[item(100,list_users,'Users'),item(200,settings,'Settings'),item(300,statistics,'Statistics')],
                   repository-[item(100,load_file_form,'Load local file'),item(200,load_url_form,'Load from HTTP'),item(300,load_library_rdf_form,'Load from library'),item(400,remove_statements_form,'Remove triples'),item(500,clear_repository_form,'Clear repository')],
                   query-[item(100,yasgui_editor,'YASGUI SPARQL Editor'),item(200,query_form,'Simple Form'),item(300,swish,'SWISH Prolog shell')],
                   swish-[item(90,swish,'Swish Home')],
                   (swish/swish)+ class(login)-[item(901,(swish/swish)+class(login),'Example KB')],
                   help-[item(100,wiki_help,'Documentation'),item(150,tutorial,'Tutorial'),item(200,cp_help,'Roadmap'),item(300,http_help,'HTTP Services')],
                   user-[item(100,login_form,'Login')]]),
   (term_rendering(html([\X]),_,[],Out,Out2)).
