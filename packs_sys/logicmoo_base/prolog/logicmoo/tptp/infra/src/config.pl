/*
* Copyright (C) 2002, 2007 Christoph Wernhard
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

:- module(config, [init_config/0,
		   config/2,
		   print_default_config/0,
		   print_config_doc/1,
		   document_file/2,
		   template_file/2]).

:- use_module(rdf_read).
:- use_module(rdf_writer).
:- use_module(xml_writer).
:- use_module('swilib/err').
:- use_module(textutil).
:- use_module(knowledgebase).
:- use_module(rdf_convert).

:- dynamic(user_config/3).
:- dynamic(infradir/1).

config(P, V) :-
	user_config(P, _, V),
	!.
config(P, V) :-
	default_config(P, _, V),
	!.
config(P, _) :-
	err('Missing configuration item: ~q.', [P]).

read_config_file(File) :-
	read_rdf(File, Triples),
	canonicalize_triples(Triples, Triples1),
	retractall(user_config(_, _, _)),
	( member(rdf(_, P, literal(O)), Triples1),
 	  sub_atom(P, 0, B, _, 'http://www.infraengine.com/config/'),
 	  sub_atom(P, B, _, 0, P1),
	  once(default_config(P1, Group, _)),
	  msg('User configuration: ~q=~q', [P1, O]),
	  assert(user_config(P1, Group, O)),
	  fail
	; true
	).

default_config(tmp_dir, general, '$HOME/.infra/tmp').

default_config(webserver_password_file, webserver, '$HOME/.infra/passwd').
default_config(webget_wget_parameters, webget, '--user-agent=Seahorse/1.0').
default_config(webget_client, webget, wget).

default_config(pages_brief_comment_size, pages, '50').
default_config(pages_dotgraph_fontname, pages, 'Arial').
% default_config(pages_dotgraph_fontsize, pages, '12').
default_config(pages_dotgraph_fontsize, pages, '11').
% default_config(planner_dotgraph_fontname, planner, 'Andale_Mono').
% default_config(planner_dotgraph_fontname, planner, 'lcdxmr').
% default_config(planner_dotgraph_fontname, planner, 'Courier_New_Bold').
% default_config(planner_dotgraph_fontname, planner, 'courbd.ttf').
default_config(planner_dotgraph_fontname, planner, 'Courier-Bold').
default_config(planner_dotgraph_fontsize, planner, '11').

% DejaVu-Sans DejaVuSans
% DejaVu-Sans-Mono
% gsfonts
% ttf-dejavu

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Print Config
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Write a RDF config file with the default settings to standard output.
%%%% 
print_default_config :-
	NAs = ['http://www.infraengine.com/config/'-''],
	setof(G-PVs, setof(P-V, default_config(P, G, V), PVs), GPVs),
	findall(rdf(blank(GI), P1, V1),
		( member(G1-PVs1, GPVs),
		  config_group_class(G1, Class),
		  gensym(group_instance, GI),
		  ( P1 = rdf_type, V1 = Class
		  ; member(P2-V2, PVs1),
		    atom_concat('http://www.infraengine.com/config/', P2, P1),
		    V1 = literal(V2)
		  )
		),
		Triples),
	triples_to_xml(Triples, NAs, XML),
	write_structure(XML, [encoding('UTF-8'), indent(4)]).

%%%% 
%%%% Print configuration documentation to standard output.
%%%% Requires a knowledgebase, which has config_schema.rdf loaded.
%%%% Output is XML quoted (encoding UTF-8).
%%%% 
print_config_doc(KB) :-
	NAs = ['http://www.infraengine.com/config/'-''],
	( fact(KB, Group, rdfs_subClassOf,
	       'http://www.infraengine.com/config/Config'),
	  ( Item = Group,
	    fact(KB, Item, rdfs_comment, literal(Comment)),
	    Type = 'Class'
	  ; setof(Prop, fact(KB, Prop, rdfs_domain, Group), Props),
	    member(Item, Props),
	    fact(KB, Item, rdfs_comment, literal(Comment)),
	    Type = 'Property'	    
	  ),    
	  print_comment(Comment, Item, NAs, Type),
	  fail  
        ; true
	).

print_comment(Comment, Item, NAs, Type) :-
	( atom(Comment) ->
	  Comment1 = Comment
	; term_to_atom(Comment, Comment1) % *** or extract atoms...
	),
	xml_quote(Comment1, [encoding('UTF-8')], XComment),
	xml_name(Item, NAs, Item1),
	( Type = 'Class' ->
	  Indent1 = 0, Indent2 = 4
	; Indent1 = 4, Indent2 = 8
	),
	write_lines(Item1, Indent1, 78), nl,
	write_lines(XComment, Indent2, 78), nl.


xml_name(I, Ns, I1) :-
	public_item(I, I2),
	member(N-A, Ns),
	sub_atom(I2, 0, B, R, N),
	R > 0,
	!,
	sub_atom(I2, B, _, 0, L),
	( A = '' -> I3 = L
	; concat_atom([A, ':', L], I3)
	),
	xml_quote(I3, [], I1).
xml_name(I, Ns, _) :-
	err('Failed to print xml name ~q ~q.', [I, Ns]).

config_group_class(general, 'http://www.infraengine.com/config/GeneralConfig').
config_group_class(webget, 'http://www.infraengine.com/config/WebgetConfig').
config_group_class(webserver, 'http://www.infraengine.com/config/WebserverConfig').
config_group_class(pages, 'http://www.infraengine.com/config/PagesConfig').
config_group_class(planner, 'http://www.infraengine.com/config/PlannerConfig').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic document_prefix/1.
:- dynamic template_prefix/1.

%%%% 
%%%% document_file(+Basename, -Filename)
%%%% 
%%%% Basename is the postfix of the name of file to be delivered as
%%%% Web document. Filename is absolute filename of the corresponding
%%%% file. Since basename might be supplied by the user,  document_file
%%%% fails if the prefix of the absolute filename differs from the value
%%%% of document_prefix/1, to protect from retrieving files in other
%%%% directories by ".." or $FOO in Basename.
%%%%
%%%% Note: absolute_file_name does sometimes expand symbolic links
%%%% and sometimes not (bug in SWI?), so symbolic links below
%%%% the document root possibly might not work.
%%%%
document_file(Basename, Filename) :-
	atom(Basename),
	var(Filename),
	document_prefix(DocumentPrefix),
	concat_atom([DocumentPrefix, Basename], File1),
	absolute_file_name(File1, Filename, [file_errors(fail)]),
	atom_prefix(Filename, DocumentPrefix). %% to prevent .. expansion

template_file(Basename, Filename) :-
	template_prefix(TemplatePrefix),
	concat_atom([TemplatePrefix, Basename], File1),
	absolute_file_name(File1, Filename, [file_errors(fail)]).

init_document_dir :-	
	( getenv('INFRADIR', INFRADIR),
	  INFRADIR \= '' ->
	  true
	; expand_file_name('$HOME/infra', [INFRADIR|_])
	),
	concat_atom([INFRADIR, '/lib/documents/'], DocumentPrefix),
	concat_atom([INFRADIR, '/lib/templates/'], TemplatePrefix),
	retractall(document_prefix(_)),
	retractall(template_prefix(_)),	
	asserta(document_prefix(DocumentPrefix)),
	asserta(template_prefix(TemplatePrefix)).	

init_infradir :-
	( getenv('INFRADIR', INFRADIR),
	  INFRADIR \= '' ->
	    true
	; expand_file_name('$HOME/infra', [INFRADIR|_])
	),
	retractall(infradir(_)),
	assert(infradir(INFRADIR)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Init Config
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_config :-
	expand_file_name('$HOME/.infra/config.rdf', [ConfigFile]),
	msg('Loading configuration file: ~w.', [ConfigFile]),
	read_config_file(ConfigFile),
	init_infradir,
	init_document_dir.


