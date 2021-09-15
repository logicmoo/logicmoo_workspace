/*
* Copyright (C) 2002, 2007, 2012, 2016 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

/*
Changelog

Fri Nov 16 18:12:03 2012:
Added encoding option to allow proper writing of UTF-8 files
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% XML Writer
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(xml_writer, [write_structure/2]).


:- use_module(err).
:- use_module(library(sgml)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% write_structure(+Items, +Options)
%%%% 
%%%% Write XML to the standard output.
%%%%
%%%% Items is a list of element/3 terms as returned by the library(sgml)
%%%% xml-parser. Also e/3 instead of element/3 is accepted.
%%%%
%%%% See make_options/2 for valid elements of Options.
%%%%
%%%% Option namespaces(Ns), where Ns is a list of Abbreviation-Namespace
%%%% pairs.
%%%%
write_structure(Items, Options) :-
	make_options(Options, Opts),
	( memberchk( namespaces(Ns), Options ) ->
	  true
	; Ns = []
	),
	write_structure(Items, 0, Ns, Opts).

make_options(Options, Opts) :-
	( memberchk(indent(Indent), Options) ->
	  true
	; Indent=2
	),
	( memberchk(space(Space), Options) ->
	  true
	; Space=ignore
	),
	( memberchk(style(Style), Options) ->
	  true
	; Style=xml
	),
	%% Effects quoting, does not set the stream property:
	( memberchk(encoding(Encoding), Options) ->
	  true
	; Encoding=ascii
	),

	make_opts(Indent, Space, Encoding, Style, Opts).

make_opts(I, S, E, Y, opts(I, S, E, Y)).

get_opt_indent(opts(I, _, _, _), I).
get_opt_space(opts(_, S, _, _), S).
% get_opt_convert_to_utf_8(opts(_, _, E, _), E). % obsolete
get_opt_encoding(opts(_, _, E, _), E).
get_opt_style(opts(_, _, _, Y), Y).

write_structure([Item|Items], Depth, Ns, Opts) :-
	!,
	write_item(Item, Depth, Ns, Opts),
	write_structure(Items, Depth, Ns, Opts).
write_structure([], _, _, _) :-
	!.
write_structure(X, _, _, _) :-
	err('Malformed content list: ~q.', [X]).

write_item(X, _, _, _) :-
	var(X),
	!,
	err('Variable content item.').
write_item(element(T, A, C), D, Ns, Opts) :-
	!,
	add_namespaces(A, Ns, Ns1),
	convert_name(T, Ns1, T1),
	may_indent(Opts, D),
	write('<'),
	write(T1),
	write_attributes(A, D, Ns, Opts),
	( C = [] ->
	  ( get_opt_style(Opts, html) ->
	    write(' >')
          ; get_opt_style(Opts, html4) ->
	    ( html4_noendtag(T1) ->
              write('>')
	    ; write('></'),
	      write(T1),
	      write('>')
	    )
	  ; write(' />')
	  ),
	  may_nl(Opts)
	; write('>'),
	  D1 is D + 1,
	  subelement_opts(T, C, Opts, Opts1),
	  may_nl(Opts1),
	  write_structure(C, D1, Ns1, Opts1),
	  may_indent(Opts1, D),
	  write('</'),
	  write(T1),
	  write('>'),
	  may_nl(Opts)
	).
write_item(e(T, A, C), D, Ns, Opts) :-
	!,
	write_item(element(T, A, C), D, Ns, Opts).
write_item(xml(X), _, _, _) :-
	atomic(X),
	!,
	write(X).
write_item(X, _, _, Opts) :-
	atomic(X),
	!,
	write_converted_text(X, Opts).
write_item(entity(No), _, _, _) :-
	!,
	format('&#~d;', [No]).
write_item(Item, _, _, _) :-
	err('Malformed or unsupported content item: ~q.', [Item]).

subelement_opts(_, _, Opts, Opts) :-
	get_opt_space(Opts, preserve),
	!.
subelement_opts(T, _, Opts, Opts1) :-
	get_opt_space(Opts, html),
	html_pre_tag(T),
	!,
	make_space_preserve_opts(Opts, Opts1).
subelement_opts(_, Cs, Opts, Opts1) :-
	member(C, Cs),
	\+ C = element(_, _, _),
	\+ C = e(_, _, _),	
	!,
	make_space_preserve_opts(Opts, Opts1).
subelement_opts(_, Cs, Opts, Opts1) :-
	get_opt_space(Opts, html),
	member(C, Cs),
	once((C = element(T, _, _) ; C = e(T, _, _))),
	\+ html_indent_tag(T),
	!,
	make_space_preserve_opts(Opts, Opts1).
subelement_opts(_, _, Opts, Opts).

make_space_preserve_opts(Opts, Opts1):-
	get_opt_indent(Opts, I),
	get_opt_encoding(Opts, E),
	get_opt_style(Opts, Y),
	make_opts(I, preserve, E, Y, Opts1).

can_indent(Opts) :-
	\+ get_opt_space(Opts, preserve).

html_pre_tag(pre).
html_pre_tag('PRE').

%
% This should mean, that "around these tags whitespace can be inserted".
% Not sure if all this is correct.
%
html_indent_tag(html).
html_indent_tag(head).
html_indent_tag(title).
html_indent_tag(body).
html_indent_tag(table).
html_indent_tag(tbody).
html_indent_tag(tr).
html_indent_tag(td).
html_indent_tag(hr).
html_indent_tag(p).
html_indent_tag(ul).
html_indent_tag(ol).
html_indent_tag(dl).
html_indent_tag(li).
html_indent_tag(dt).
html_indent_tag(dd).
html_indent_tag(meta).
html_indent_tag(form).
html_indent_tag(h1).
html_indent_tag(h2).
html_indent_tag(h3).
html_indent_tag(h4).
html_indent_tag(option).
html_indent_tag(select).

may_indent(Opts, D) :-
	can_indent(Opts),
	!,
	get_opt_indent(Opts, I),
	J is I * D,
	indent(J).
may_indent(_, _).

may_nl(Opts) :-
	can_indent(Opts),
	!,
	nl.
may_nl(_).


%%
%% Probable SWI-Bug:
%% line_position seems not to work for printing on text buffers.
%% 

write_attributes(AVs, D, Ns, Opts) :-
	write_attributes_1(AVs, 0, D, Ns, Opts).

write_attributes_1([A=V|AVs], Nth, D, Ns, Opts) :-
	!,
	convert_name(A, Ns, A1),
	get_opt_indent(Opts, I),
	( Nth > 0, I > 0 ->
	  J is I * D + 4,
	  indent(J)
        ; true
	),
	write(' '),
	write(A1),
	write('="'),
	write_converted_text(V, Opts),
	write('"'),
	( AVs = [] ->
	  true
	; nl,
	  Nth1 is Nth + 1,
	  write_attributes_1(AVs, Nth1, D, Ns, Opts)
	).
write_attributes_1([], _, _, _, _) :-
	!.
write_attributes_1(X, _, _, _, _) :-
	err('Malformed attribute list: ~q.', [X]).

add_namespaces([xmlns:N=Prefix|As], Ns, [N-Prefix|Ns1]) :-
	!,
	add_namespaces(As, Ns, Ns1).
add_namespaces([xmlns=Prefix|As], Ns, [''-Prefix|Ns1]) :-
	!,
	add_namespaces(As, Ns, Ns1).
add_namespaces([_|As], Ns, Ns1) :-
	add_namespaces(As, Ns, Ns1).
add_namespaces([], Ns, Ns).

builtin_namespace(xml).
builtin_namespace(xmlns).
builtin_namespace(rdf).

convert_name(N:X, Ns, X1) :-
	!,
	( builtin_namespace(N) ->
	  atomic_list_concat([N, ':', X], X1)
        ; memberchk(Abbrev-N, Ns) ->
	  (Abbrev = '' ->
	    X1 = X  
	  ; atomic_list_concat([Abbrev, ':', X], X1)
	  )
	; atomic_list_concat([N, ':', X], X1)
	).
convert_name(X, _, X).

% write_converted_text(X, Opts) :-
% 	convert_text(X, Opts, Y),
% 	write(Y).
%
% convert_text(X, Opts, Y) :-
% 	atom(X),
% 	!,
% 	xml_quote(X, Opts, Y).
% convert_text(X, Opts, Y) :-
% 	term_to_atom(X, X1),
% 	convert_text(X1, Opts, Y).


write_converted_text(X, Opts) :-
	atom(X),
	!,
	write_xml_quote(X, Opts).
write_converted_text(X, Opts) :-
	term_to_atom(X, X1),
	write_converted_text(X1, Opts).

indent(N) :-
	tab(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Quoting
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_xml_quote(In, Opts) :-
	get_opt_encoding(Opts, Enc),
	xml_quote_cdata(In, Out, Enc),
	write(Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

html4_noendtag(area).
html4_noendtag(base).
html4_noendtag(br).
html4_noendtag(col).
html4_noendtag(command).
html4_noendtag(embed).
html4_noendtag(hr).
html4_noendtag(img).
html4_noendtag(input).
html4_noendtag(keygen).
html4_noendtag(link).
html4_noendtag(meta).
html4_noendtag(param).
html4_noendtag(source).
html4_noendtag(track).
html4_noendtag(wbr).

