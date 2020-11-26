:- module(mud_specific, [
	  style/1]).
/** <module> Code specific to this particular MOO's web presence

*/

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).


%%	style_name(-Style:ftTerm) is nondet
%
%       binds Style to a styling option.
%	usually called with a partially bound argument whose unbound
%	parts are bound by style/1.
%
%	Style may be
%
%	name(X) - X is the name of the style used by user:head and
%		  user:body
%
%       map  - if present, display a gridded map area
%
%       map_grid_size(X,Y)  - map "squares" are this size in pixels
%
%	map_display(+Semantics, -TermerizedHTML)
%	                    - turn semantics into html. If this fails
%	                    will use blank to fill in cell
%	blank(-TermerizedHTML) contents of the td field if there is no
%			       semantic content or map_display fails
%
%
%
style(name(startrek)).
style(map).
style(map_grid_size(32, 32)).
style(map_display(wall, 'W' )).
style(map_display(floor, '.' )).

:- multifile user:head/4, user:body/4.

%%	user:head(+Style:atom, +Head:html, ?A:List, ?B:List) is det
%
%	override head generation for this moo
%
%	this predicate is REQUIRED if style is not logicmoo
%
user:head(startrek, Head) -->
	html(head([
		 title('Star Trek'),
		 Head
	     ])).

%%	user:body(+Style:atom, +Head:html, ?A:List, ?B:List) is det
%
%	override body generation for this moo
%
%	this predicate is REQUIRED if style is not logicmoo
%
user:body(startrek, Body) -->
	html(body([
		 h1('Star Trek'),
		 div(ftID(content), Body)
	     ])).
