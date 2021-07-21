/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2018, VU University Amsterdam
			      CWI, Amsterdam
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

    Changes by:    Riccardo Zese
    E-mail:        riccardo.zese@unife.it
*/

:- module(swish_examples, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_path)).
:- use_module(library(filesex)).
:- use_module(library(apply)).
:- swish_examples:use_module(library(option)).
:- use_module(library(lists)).
:- if(exists_source(library(atom))).
:- use_module(library(atom)).
:- endif.

:- use_module(storage).
:- use_module(md_eval).


/** <module> Serve example files

Locate and serve files for  the  _Examples_   menu  as  well as examples
included from overview notebooks. The examples come from two sources:

  - Prolog files in the file search path `examples`.  Such files are
    distributed with SWISH.
  - Gitty files marked as `example`.  Such files can be created by the
    users.

This  module  also  makes   the    known   examples   available  through
swish_provides/1  for  supporting  conditional   statements  on  example
overview notebooks.
*/

:- multifile
	user:file_search_path/2,
	swish_config:config/2,
	swish_config:source_alias/2.

% make example(File) find the example data
ufsp((examples)).
ufsp((examples/inference)).
ufsp((examples/learning)).
ufsp((examples/lemur)).
ufsp((examples/phil)).
ufsp((examples/aleph)).

ufsp((examples/trill)).

ufsp((examples/lps_corner)).
ufsp((examples/lps_corner/'CLOUT_workshop')).
ufsp((examples/lps_corner/'forTesting')).
ufsp((examples/lps_corner/'survival_game')).
ufsp((examples/logicmoo)).
ufsp((examples/logtalk)).
ufsp((examples/pfc)).
ufsp((examples/prologmud)).

user:file_search_path(e, swish(UFSP)):- ufsp(UFSP).
user:file_search_path(example, swish(UFSP)):- ufsp(UFSP).



:- http_handler(swish(list_extended_examples/Example),
		list_extended_example(Method, Example),
                [ method(Method),
                  methods([get,post,put]),
                  id(swish_group_extended_examples)]).

list_extended_example(_GetPostPut, Example, _Request) :-
    http_absolute_location(swish(e/Example), HREF, []),
	atom_concat('/opt/logicmoo_workspace/packs_web/swish/examples/',Example,Dir),
        dir_to_pattern(Dir,Pattern),
	expand_file_name(Pattern, Files),
  % wdmsg(Example=Files),
	convlist(ex_file_json(HREF), Files, Menu),
	reply_json(Menu).

dir_to_pattern(Dir,Pattern):- sub_string(Dir, _, _, _, "*"),!,Dir=Pattern.
dir_to_pattern(Dir,Pattern):- string_concat(Dir, "/*", Pattern).
dir_to_pattern(Dir,Pattern):- string_concat(Dir, "/*.{pl,swinb,*}", Pattern).
	
:- http_handler(swish(list_extended_examples),
		list_extended_examples, [id(swish_extended_examples)]).
% :- http_handler(swish(list_extended_examples), list_extended_examples, [id(swish_extended_examples_from_root)]).

list_extended_examples(_Request) :-
     examples(AllExamples, [community(true)]),

     example_menu(AllExamples, Menu),
     reply_json(Menu).


is_edit_any:- swish_config:config(edit_any,   true),!.
is_edit_any.

% make SWISH serve /example/File as example(File).
%swish_config:source_alias(example, [access(read), search('*.{pl,swinb}')]).
%swish_config:source_alias(e, [access(read), search('*.{pl,swinb}')]).
swish_config:source_alias(e, [access(both),search('*')]).
swish_config:source_alias(example, [access(both),search('*')]).


swish_config:source_alias(example, [access(read), search(What)]):-
  (is_edit_any) 
    ->   What = '*.*' ;   What = '*.{pl,swinb}'.

swish_config:source_alias(example, [access(read), search('*/*.*')]):- is_edit_any.
swish_config:source_alias(example, [access(read), search('*/*/*.*')]):- is_edit_any.
swish_config:source_alias(example, [access(read), search('*/*/*/*.*')]):- is_edit_any.

:- http_handler(swish(list_examples),
		list_examples, [id(swish_examples)]).
:- http_handler(root(list_examples),
		list_examples, [id(swish_examples_from_root)]).


%%	list_examples(+Request)
%
%	Get a list of registered example code. Examples are described in
%	a file swish_examples('index.json').

list_examples(_Request) :-
    directory_index_json(swish(examples), JSON0),
	http_absolute_location(swish(example), HREF, []),
	add_examples_href(HREF, JSON0, JSON),
	Menu = JSON.get(files),
	reply_json(Menu).

list_examples(_Request) :-
    examples(AllExamples, [community(false),extra_files(false)]),
	example_menu(AllExamples, Menu),
	reply_json(Menu).

example_menu(AllExamples, Menu) :-
	convlist(pos_ranked, AllExamples, ForMenu),
	insert_group_dividers(ForMenu, Menu).

pos_ranked(Ex) :-
	Rank = Ex.get(grank),
	Rank > 0.

insert_group_dividers([], []).
insert_group_dividers([H1,H2|T], List) :-
	!,
	(   H1.grank // 10000 =\= H2.grank // 10000
	->  List = [H1, json{type:divider}|Rest]
	;   List = [H1|Rest]
	),
	insert_group_dividers([H2|T], Rest).
insert_group_dividers([H], [H]).


%%	examples(JSON:list, +Options) is det.
%
%	JSON is a list of JSON dicts containing the keys below. The list
%	is composed from all *.pl files in the search path `example`.
%
%	  - file:File
%	  - href:URL
%	  - title:String
%	  - requires:Term
%	  - group:String

examples(AllExamples, Options) :-
	swish_examples(SWISHExamples, Options),
	(   option(community(true), Options)
	->  community_examples(CommunityEx)
	;   no_examples(CommunityEx)
	),
	join_examples([CommunityEx|SWISHExamples], AllExamples).

:- dynamic
	swish_example_cache/2.

no_examples(json{}).

swish_examples(SWISHExamples, Options) :-
        option(extra_files(false), Options),!, no_examples(SWISHExamples).
swish_examples(SWISHExamples,_Options) :-
	swish_example_cache(SWISHExamples, Time),
	get_time(Now),
	Now - Time < 60,
	!.
swish_examples(SWISHExamples, Options) :-
	swish_examples_no_cache(SWISHExamples, Options),
	get_time(Now),
	retractall(swish_example_cache(_,_)),
	assertz(swish_example_cache(SWISHExamples, Now)).

swish_examples_no_cache(SWISHExamples, Options) :-
        option(extra_files(false), Options),!, no_examples(SWISHExamples).
swish_examples_no_cache(SWISHExamples,_Options) :-
	http_absolute_location(swish(e), HREF, []),
	findall(Index,
		absolute_file_name(example(.), Index,
				   [ access(read),
				     file_type(directory),
				     file_errors(fail),
				     solutions(all)
				   ]),
		ExDirs),
	convlist(index_json(HREF), ExDirs, SWISHExamples).


join_examples(PerDirV, Files) :-
        flatten(PerDirV,PerDir),
	menu_groups(PerDir, Groups),
	convlist(get_or(files, []), PerDir, FilesPerDir),
	append(FilesPerDir, Files0),
	convlist(add_grank(Groups), Files0, Files1),
	sort(grank, =<, Files1, Files).

add_grank(Groups, File0, File) :-
	get_or(rank,  500,  File0, FRank),
	GroupName = File0.get(group),
	member(Group, Groups),
	Group.get(group) == GroupName,
	GRank is FRank + Group.get(rank), !,
	File = File0.put(grank, GRank).
add_grank(_, File0, File) :-
	File = File0.put(grank, -1).

menu_groups(PerDir, Groups) :-
	convlist(get_or(menu, []), PerDir, GroupsPerDir),
	append(GroupsPerDir, Groups0),
	sort(group, @>, Groups0, Groups1),
	sort(rank,  =<, Groups1, Groups).

get_or(Key, Default, Dict, Value) :-
	(   is_dict(Dict),
	    Value = Dict.get(Key)
	->  true
	;   Value = Default
	).

%!	index_json(+BaseHREF, +Directory, -JSON)
%
%	Produce a JSON description for  the   examples  in the directory
%	Dir. This deals with two scenarios:   if  a file `index.json` is
%	provided, use this file  and  add   the  not-described  files as
%	examples that are not included in   the menu. If no `index.json`
%	is present, all files are added as example files.

directory_index_json(Dir, JSON) :- 
    absolute_file_name(Dir, Path),!,
	directory_file_path(Path, 'index.json', File),
	access_file(File, read), !,
	read_file_to_json(File, JSON).

index_json(HREF, Dir, JSON) :-
	directory_index_json(Dir, JSON0),
	add_examples_href(HREF, JSON0, JSON1),
	add_other_files(HREF, Dir, JSON1, JSON).

index_json(HREF, Dir, json{menu:[json{group:Group, rank:10000}],
			   files:Files}) :- 
	example_files(HREF, Dir, Files0),
    file_base_name(Dir, Base),
    format(atom(Group),'~w',[Base]),
	convlist(add_group(Group), Files0, Files).
	
	
index_json(HREF, Dir, json{menu:[json{group:examples, rank:10000}],
			   files:Files}) :-
	example_files(HREF, Dir, Files0),
	convlist(add_group(examples), Files0, Files).

example_files(HREF, LDir, JSON) :-
  absolute_file_name(LDir, Dir, [access(read), file_errors(fail), file_type(directory)]),
	string_concat(Dir, "/*.{pl,swinb}", Pattern),
	expand_file_name(Pattern, Files),
	convlist(ex_file_json(HREF), Files, JSON).

read_file_to_json(File, JSON) :-
	setup_call_cleanup(
	    open(File, read, In, [encoding(utf8)]),
	    json_read_dict(In, JSON, [default_tag(json)]),
	    close(In)).

%!	add_examples_href(+HREF, +JSON0, -JSON) is det.
%
%	Add a `href` key pointing at the example. Also removes all items
%	that are not dicts or have no `file` key.

add_examples_href(HREF, JSON0, JSON) :-
    is_dict(JSON0),
	Files0 = JSON0.get(files), !,
	add_examples_href(HREF, Files0, Files),
	JSON = JSON0.put(files, Files).

add_examples_href(HREF, Files0, Files):-
    convlist(add_href(HREF), Files0, Files).

add_href(HREF0, Dict, Dict2) :-
	is_dict(Dict),
	( \+ (_ = Dict.get(href))),	
	directory_file_path(HREF0, Dict.get(file), HREF),
	Dict2 = Dict.put(href, HREF).
add_href(_, Dict, Dict).

add_group(Group, Dict0, Dict) :-
	is_dict(Dict0), !,
	Dict = Dict0.put(group, Group).
add_group(_, Dict, Dict).

% add_other_files(_HREF, _Dir, JSON, JSON):-!.
add_other_files(HREF, Dir, JSON0, JSON) :-
	example_files(HREF, Dir, Files),
	get_or(files, [], JSON0, Files0),
	exclude(in_ex_list(Files0), Files, New),
	append(Files0, New, AllFiles),
	JSON = JSON0.put(files, AllFiles).

in_ex_list(Examples, Ex) :-
	File = Ex.file,
	member(Ex2, Examples),
	is_dict(Ex2),
	File = Ex2.get(file),
	!.

%%	ex_file_json(+ExampleBase, +Path, -JSON) is det.
%
%	@tbd	Beautify title from file-name (_ --> space, start
%		with capital, etc).
%
%	Create a JSON representation for the given example file.

ex_file_json(HREF0, Path, json{type:"unknown", file:File, href:HREF, title:Title, group:Group}) :-
  exists_file(Path),
	file_base_name(Path, File),
  File \== is_dir, File \== 'index.json',
	file_name_extension(Base, _, File),
	file_name_to_title(Base, Title),
	file_group(File,OneDir), format(atom(Group),'~w',[OneDir]),
	directory_file_path(HREF0, File, HREF).

ex_file_json(HREF0, Path, json{type:"submenu", items:Files, href:HREF, title:Title, group:Group}) :-
  fail, exists_directory(Path),
	file_base_name(Path, File),
  atomic_list_concat([Path,'/*'],Matches),
  expand_file_name(Matches,Files),
	file_name_extension(Base, _, File),
	file_name_to_title(Base, Title),
	file_group(File,OneDir), format(atom(Group),'~w',[OneDir]),
	directory_file_path(HREF0, File, HREF).

file_group(File,Group):-
  absolute_file_name(example(File), AFN, 
				   [ access(read),
				     file_errors(fail)
				   ]),
   file_directory_name(AFN,FullDir),
   file_base_name(FullDir,Group), !.				     
file_group(File,OneDir):- 
  file_directory_name(File,FullDir),file_base_name(FullDir,OneDir), !.
file_group(_,extras).


file_name_to_title(Path, Title):- atom(Path), sub_string(Path, _, _, _, '/'),
	file_base_name(Path, File), !,
        file_name_to_title(File, Title).
file_name_to_title(Path, Title):- atom(Path), sub_string(Path, _, _, _, '.'),
	file_name_extension(File, _, Path), !,
        file_name_to_title(File, Title).

:- if(current_predicate(restyle_identifier/3)).
file_name_to_title(Base, Title) :-
	restyle_identifier(style(true,false,' '), Base, Title).
:- else.
file_name_to_title(Base, Base).
:- endif.


%!	md_eval:provides(?Term) is nondet.
%
%	Make examples available through swish_provides/1. Can be used in
%	dynamic cells as, e.g.,:
%
%	  ```
%	  :- if(swish_provides(example('chat80.pl',_,_))).
%	  ...
%	  :- endif.
%	  ```

:- multifile
	md_eval:provides/1.

md_eval:provides(example(Name, Group, Example)) :-
	examples(Examples, []),
	(   var(Name)
	->  member(Example0, Examples),
	    atom_string(Name, Example0.get(file))
	;   member(Example0, Examples),
	    atom_string(Name, Example0.get(file))
	->  true
	),
	atom_string(Group,  Example0.get(group)),
	active_example(Example0, Example).

active_example(Example0, Example) :-
	term_string(Cond, Example0.get(requires)),
	\+ swish_provides(Cond),
	(   cond_reason(Cond, Fmt, Args)
	->  format(string(Reason), Fmt, Args)
	;   format(string(Reason), 'missing requirement: ~q', [Cond])
	),
	Example = Example0.put(blocked, Reason).
active_example(Example, Example).

cond_reason(plugin(Name), 'missing plugin: ~w', [Name]).


:- dynamic(example_menu_item/2).

:- asserta(cliopatria:menu_item(Loc,Title):- example_menu_item(Loc,Title)).

% example_menu_item(901=swish/swish+class(login), 'Example KB').

:- use_module(filesystems).

		 /*******************************
		 *	      STORAGE		*
		 *******************************/

%%	community_examples(-Dict) is det.
%
%	Extract examples from the gitty store.

community_examples(json{menu:[json{group:community, rank:50000}],
			files:Files}) :-
	swish_config:config(community_examples, true),
	!,
	findall(Ex, community_example(Ex), Files).
community_examples(json{}).

community_example(json{title:Title, file:File, group:community, type:store}) :-
	storage_file(File),
	storage_meta_data(File, Meta),
	Meta.get(example) == true,
	(   Title = Meta.get(title), Title \== ""
	->  true
	;   file_name_extension(Base, _, File),
	    file_name_to_title(Base, Title)
	).
