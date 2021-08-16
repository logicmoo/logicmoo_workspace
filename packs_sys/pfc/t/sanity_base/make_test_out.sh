#!/usr/bin/env swipl
%
%  Generates README.md
%
% Dec 13, 2035
% Douglas Miles

:- module(make_readme, [fs_write_html/1,list_filesystems/1,
  get_fs_title/2,file_shorter_name/2,fs_write_file/4,list_pack_filesystem/1]).

:- use_module(library(logicmoo_util_common)).
:- use_module(library(rtrace)).

:- multifile(pldoc_register:process_stored_comments/0).
/*
:- dynamic(pldoc_register:process_stored_comments/0).
:- asserta(pldoc_register:process_stored_comments).

:- multifile(pldoc:pldoc_loading/0).
:- dynamic(pldoc:pldoc_loading/0).
:- assert(pldoc:pldoc_loading).

:- multifile(loop_check:lco_goal_expansion/2).
:- dynamic(loop_check:lco_goal_expansion/2).
*/

pldoc_register:process_stored_comments.

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_path)).
:- use_module(library(filesex)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(settings)).

% :- use_module(storage).

:- use_module(pack(plweb/pack_info),[pack_file_hierarchy//1]).


:- use_module(library(debug)).
:- use_module(library(persistency)).
:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/html_head)).
:- user:use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pldoc/doc_search)).

:- use_module(library(http/mimetype)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_html),
	      [ doc_for_file/2			% other imports conflict
	      ]).				% with doc_wiki
:- use_module(library(pldoc/doc_htmlsrc)).
:- use_module(library(prolog_xref)).

:- use_module(pack(plweb/pack_info)).


 :- meta_predicate smtp:error_cleanup(*,0).
 :- meta_predicate smtp:do_send_mail_cont(*,*,*,1,*,*).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (0.080 sec)
 :- meta_predicate smtp:do_send_mail(*,+,*,1,*).


/** <module> Serve filesystem files

Locate and serve files for the _Filesystems_   menu. The filesystems come from
two sources:

  - Prolog files in the file search path `filesystems`
  - Gitty files marked as `filesystem`.
*/

:- multifile
	user:file_search_path/2,
	swish_config:config/2,
	swish_config:source_alias/2.

% make filesystem(File) find the filesystem data
user:file_search_path(project_files, CWD) :- working_directory(CWD,CWD).
% user:file_search_path(project_files, pack(pfc/t/sanity_base)).
%user:file_search_path(project_files, pack(pfc/prolog)).
%user:file_search_path(project_files, swish(examples)).
%user:file_search_path(project_files, swish(examples/inference)).
%user:file_search_path(project_files, swish(examples/learning)).
%user:file_search_path(project_files, swish(examples/lemur)).

%user:file_search_path('swish/filesystem', '/').
user:file_search_path(filesystem, '/').
user:file_search_path(filesystem_files, '/').


:- multifile
    http:status_page/3,             % +Status, +Context, -HTML
    http:post_data_hook/3.          % +Data, +Out, +HdrExtra

http:status_page(Term, Context, HTML):- wdmsg(http:status_page(Term, Context, HTML)),fail.

% http:status_page(not_found(URL), [], RET):- '/example/inference/inference_examples_R.swinb'
   
% handler(swish(help), swish_help:serve_files_in_directory(swish_help), true, [id(help)]).

:- asserta((http_dispatch:handler(root(example), http_server_files:serve_files_in_directory(example), true, [id(example_serv)]))).


		 /*******************************
		 *	    SWISH CONFIG	*
		 *******************************/

%%	swish_config:config(-Name, -Profiles) is det.
%
%	Provides the object `config.swish.filesystem_files`, a  JSON object that
%	provides the available filesystem.


swish_config:config(N,V):-filesystem_files_conf(N,V).

% filesystem_files_conf(filesystem_files, _{html:"", json:[]}) :- !.

filesystem_files_conf(filesystem_files, _{html:X, json:FileFilesystems}) :-
        reset_fs_names,
	with_output_to(string(X),filesystem_files(FileFilesystems)),!,
        reset_fs_names.


% make SWISH serve /filesystem/File as filesystem(File).
% swish_config:source_alias(filesystem, [access(read), search('*.{pl,swinb}')]).
%swish_config:source_alias(filesystem, [access(read), search('*.{pl,swinb,}')]).
swish_config:source_alias(filesystem, [access(both)]).
swish_config:source_alias('swish/filesystem', [access(both)]).


:- http_handler(swish(list_filesystems),
		list_filesystems, [id(make_readme)]).

:- module_transparent(fs_write_html/1).
fs_write_html(HTML):- phrase(html(HTML), Tokens), current_output(Out), html_write:print_html(Out, Tokens).

list_pack_filesystem(Pack):-
 Pack = pfc,
 pack_info: (mirror_pack(Pack),
   %mirror_pack(Pack),
   pack_archive(Pack, _Hash, Archive),
   ensure_xref_pack(Archive),
   findall(File, pack_file(Pack, File, _Size, _XrefID), Files),
   files_to_tree(Files, Trees),
   fs_write_html( [
              div(class('pack-files'),
		 ul(class(tree),
		    \dir_nodes(Pack, Trees)))])),!.

:- thread_local(file_had_title/2).

reset_fs_names:- retractall(file_had_title(_,_)).

%%	list_filesystems(+Request)
%
%	Get a list of registered filesystem code. Filesystems are described in
%	a file make_readme('index.json').

% list_filesystems(_Request) :- list_pack_filesystem(pfc),!.

list_filesystems(_Request) :- 
   swish_config:config(filesystem_files, JSON),
        reply_json(JSON).


:- system:import(list_filesystems/1).

list_filesystems2(_Request) :-
        storage_filesystems(StorageFilesystems),
	reply_json(StorageFilesystems).

%%	filesystem_files(JSON:list) is det.
%
%	JSON is a list of JSON dicts containing the keys below. The list
%	is composed from all *.pl files in the search path `filesystem`.
%
%	  - file:File
%	  - href:URL
%	  - title:String

filesystem_files(AllFilesystems) :-
	% http_absolute_location(swish(filesystem), HREF, []),
            HREF = '' ,
	findall(Index,
		absolute_file_name(project_files(.), Index,
				   [ access(read),
				     file_type(directory),
				     file_errors(fail),
				     solutions(all)
				   ]),
		ExDirs),
	maplist(fs_inx_json(HREF), ExDirs, JSON),
	append(JSON, AllFilesystems).

fs_inx_json(HREF, Dir, JSON) :-  fail,
	directory_file_path(Dir, 'index.json', File),
	fail, 
        access_file(File, read), !,
	fs_read_file_to_json(File, JSON0),
	maplist(fs_add_href(HREF), JSON0, JSON).
fs_inx_json(HREF, Dir, JSON) :-
	string_concat(Dir, "/*/*.*", Pattern0),
        expand_file_name(Pattern0, Files0),
        string_concat(Dir, "/*.*", Pattern1),
        expand_file_name(Pattern1, Files1),
        append(Files0, Files1,Files3),
        sort(Files3,Files),
	maplist(fsex_file_json(HREF), Files, JSON).

fs_read_file_to_json(File, JSON) :-
	setup_call_cleanup(
	    open(File, read, In, [encoding(utf8)]),
	    json_read_dict(In, JSON),
	    close(In)).

fs_add_href(HREF0, Dict, Dict2) :-
	is_dict(Dict),
	directory_file_path(HREF0, Dict.get(file), HREF), !,
	Dict2 = Dict.put(href, HREF).
fs_add_href(_, Dict, Dict).

%%	fsex_file_json(+FilesystemBase, +Path, -JSON) is det.
%
%	@tbd	Beautify title from file-name (_ --> space, start
%		with capital, etc).

fsex_file_json(HREF, Path, json{file:Path, href:HREF, title:UsedTitle}) :-
  \+ exists_file(Path),UsedTitle=Path,!.

fsex_file_json(HREF0, Path, json{file:Path, href:HREF, title:UsedTitle}) :-
      must_det_l((  
      % File = Path,
      % file_base_name(Path, File),      
      % file_name_extension(Base, _, File),
      % Title = Path,      
      get_fs_title(Path,Title),
      % directory_file_path(HREF0, Path, HREF),
      atom_concat(HREF0, Path, HREF),
      fs_write_file(Path, HREF, Title, UsedTitle))).

fs_write_file(_File, HREF, _Title, UsedTitle):- file_had_title(HREF,UsedTitle),!.
fs_write_file(File, _HREF, _Title, UsedTitle):- file_had_title(File,UsedTitle),!.
fs_write_file(File, HREF, Title, UsedTitle):- 
	file_had_title(_, Title), 
        file_shorter_name(File,Base),
	atomic_list_concat([Title,' - ',Base],NewTitle),
	fs_write_file(File, HREF, NewTitle, UsedTitle).
%fs_write_file(File, HREF, Title,UsedTitle):- fs_names(_, Title),!,fs_write_file(File, HREF, File,UsedTitle).
fs_write_file(File, HREF, Title, Title):- asserta(file_had_title(HREF,Title)),
      fs_write_file_in_list(File, HREF, Title).

:- thread_local(file_dir_written/1).

fs_write_file_in_list(File, HREF, Title):- file_dir_written(Was),atom_concat(Was,Shorter,File),!,
   fs_write_html([a(href(HREF), [Shorter]),&(nbsp),Title,br([],[])]),!.

fs_write_file_in_list(File, HREF, Title):- retractall(file_dir_written(_)),
      file_shorter_name2(File, Shorter),  
      atom_concat(Was,Shorter,File),
      asserta(file_dir_written(Was)),!,
      fs_write_html([Was,br([],[])]),
      fs_write_html([a(href(HREF), [Shorter]),&(nbsp),Title,br([],[])]),!.


useful_title(Title):- \+ un_helpfull_name(Title), \+ file_had_title(_FilePath, Title),!.

un_helpfull_name('').
un_helpfull_name(prolog).
un_helpfull_name(pack).
un_helpfull_name(t).

file_shorter_name(Path, Shorter):-
     atomic_list_concat(O,'/',Path),
     exclude(un_helpfull_name,O,PathL),O \= PathL, 
     atomic_list_concat(PathL,'/',O2),
     file_shorter_name(O2, Shorter),!.
file_shorter_name(Path, Shorter):- 
     atom_concat(Next,'.pl',Path),!,
     file_shorter_name(Next, Shorter).
file_shorter_name(Path, Shorter):- file_shorter_name2(Path, Shorter).
file_shorter_name2(Path, Shorter):-
     directory_file_path(Dir, File, Path),
     file_base_name(Dir, Base),
     atomic_list_concat([Base,/,File],Shorter),!.
file_shorter_name2(File,File).



% a(href=HREF,Title)),pack_file_link(File


		 /*******************************
		 *	      STORAGE		*
		 *******************************/

old_title(Title) --> "%", whites, !, dcg_title(upper,Title).

get_fs_title(FilePath, Title) :- file_had_title(FilePath, Title),!.

get_fs_title(FilePath, Title) :-
        setup_call_cleanup(
           open(FilePath, read, In),
           (repeat,
             (at_end_of_stream(In) ;
              (read_line_to_codes(In, Line)->
                phrase(new_dcg_title(Title), Line)))),
           close(In))-> nonvar(Title),!.
/*
get_fs_title(FilePath, Title) :-
        setup_call_cleanup(
           open(FilePath, read, In),
           (repeat,
             (at_end_of_stream(In) ;
              (read_line_to_codes(In, Line)->
                phrase(dcg_title(upper,Title), Line)))),
           close(In))-> useful_title(Title),!.

get_fs_title(FilePath, Title) :-
        setup_call_cleanup(
           open(FilePath, read, In),
           (repeat,
             (at_end_of_stream(In) ;
              (read_line_to_codes(In, Line)->
                phrase(dcg_title(alpha,Title), Line)))),
           close(In))-> useful_title(Title),!.

get_fs_title(FilePath, Title) :-
	first_line_fst(FilePath, FirstLine),
	(   FirstLine == end_of_file
	->  fail
        	;   phrase(old_title(Title), FirstLine)
	)
            ->useful_title(Title),!.
*/

get_fs_title(FilePath, Title) :- file_shorter_name(FilePath, Title),!.


get_fs_title(FilePath, Title) :-
	first_line_fst(FilePath, FirstLine),
	(   FirstLine == end_of_file
	->  (Title = "Empty",fail)
	;   phrase(dcg_title(graph,Title), FirstLine)
	)->useful_title(Title),!.

first_line_fst(File, Line) :-
	setup_call_cleanup(
	    open(File, read, In),
	    read_line_to_codes(In, Line),
	    close(In)).

new_dcg_title(Title) --> `Tests`,!, gfst_rest(Codes),
     { string_codes(Title, Codes) },!.
new_dcg_title(Title) --> [_],new_dcg_title(Title).


dcg_title(Type,Title) -->
	non_letters(Type), one_upper(Type,C),!, gfst_rest(Codes),
	{ string_codes(Title, [C|Codes]) }.


% dcg_title(_,"No title") --> gfst_rest(_).

one_upper(Type,C) --> [C],{ char_type(C,Type) }.

non_letters(_Type) --> `:-`,!,{fail}.
non_letters(_Type) --> `(`,!,{fail}.
non_letters( Type) --> [C],{ \+ char_type(C,Type) },!,non_letters(Type).
non_letters(_Type) --> [].

gfst_rest(List, List, []).


% :- pack_info:update_pack_metadata.

% mirror_packs.

/*
:- multifile(cp_menu:menu_items/3).
:- dynamic(cp_menu:menu_items/3).
:- multifile(cp_menu:menu_item/3).
:- dynamic(cp_menu:menu_item/3).
*/
:- export(menu_item/3).
:- export(menu_items/3).

menu_items([]) --> [].
menu_items([H|T]) --> menu_item(H), menu_items(T).

menu_item(item(_Rank, Spec, Label)) -->
	{ atom(Spec) },
	{ (   \+ sub_atom(Spec, 0, _, _, 'http://'),
	      catch(http_location_by_id_local(Spec, Location), E,
		    (   print_message(informational, E),
			fail))
	  ->  true
	  ;   Location = Spec
	  )
	},!,
	html(li(a([href(Location)], Label))).

menu_item(item(_Rank, _Spec, _Label)) -->[].


http_location_by_id_local(Location, Location).

test12345:- fail.
test12345:- fs_write_html( \menu_items([ % item(100, yasgui_editor, 'YASGUI SPARQL Editor'), item(200, query_form, 'Simple Form'), item(300, swish, 'SWISH Prolog shell'), 
                                             item(302, "/tutorial/tutorial.swinb", 'Tutorial Tutorials'), 
                                             item(302, '/swish/example/Rdataframe.swinb', 'Examples/Rdataframe.swinb'), 
                                             item(302, '/swish/example/Rdownload.swinb', 'Examples/Rdownload.swinb'),
                                             item(302, '/swish/example/Rserve.swinb', 'Examples/Rserve.swinb'), item(302, '/swish/example/aleph/abduce.pl', 'Aleph/abduce'), item(302, '/swish/example/aleph/aleph_examples.swinb', 'Aleph learning example programs'), item(302, '/swish/example/aleph/aleph_examples.swinb', 'Aleph/aleph examples.swinb'), item(302, '/swish/example/aleph/animals.pl', 'Simple illustration of interactive construction of tree-based models'), item(302, '/swish/example/aleph/constraints.pl', 'Aleph/constraints'), item(302, '/swish/example/aleph/features.pl', 'Aleph/features'), item(302, '/swish/example/aleph/gcws.pl', 'Simple illustration of the technique of generalised'), item(302, '/swish/example/aleph/good.pl', 'Simple illustration of the use of recording good clauses found'), item(302, '/swish/example/aleph/modes.pl', 'Simple illustration of the automatic extraction of modes'), item(302, '/swish/example/aleph/posonly.pl', 'Simple illustration of positive-only learning within Aleph'), item(302, '/swish/example/aleph/recursion.pl', 'Simple illustration of the learning of recursive predicates'), item(302, '/swish/example/aleph/refine.pl', 'Simple illustration of the use of user-defined refinement operators'), item(302, '/swish/example/aleph/train.pl', 'Simple illustration of the use of Aleph on'), item(302, '/swish/example/aleph/weather.pl', 'Aleph/weather'), item(302, '/swish/example/aleph/wedge.pl', 'Simple illustration of constructing tree-based models within Aleph'), item(302, '/swish/example/animals.pl', 'Simple illustration of interactive construction of tree-based models'), item(302, '/swish/example/basic_graph_examples.swinb', 'Examples/basic graph examples.swinb'), item(302, '/swish/example/c3_examples.swinb', 'Examples/c3 examples.swinb'), item(302, '/swish/example/clpfd_queens.pl', 'Examples/clpfd queens'), item(302, '/swish/example/clpfd_sudoku.pl', 'Examples/clpfd sudoku'), item(302, '/swish/example/constraints.pfc', 'Examples/constraints.pfc'), item(302, '/swish/example/database.pl', 'Doing database manipulation'), item(302, '/swish/example/dict.swinb', 'Dict tutorial'), item(302, '/swish/example/dict.swinb', 'Examples/dict.swinb'), item(302, '/swish/example/eliza.pl', 'Examples/eliza'), item(302, '/swish/example/enumerating_examples.swinb', 'Examples/enumerating examples.swinb'), item(302, '/swish/example/examples.swinb', 'Examples/examples.swinb'), item(302, '/swish/example/examples.swinb', 'Prolog Example programs'), item(302, '/swish/example/examples_swish.swinb', 'Examples/examples swish.swinb'), item(302, '/swish/example/expert_system.pl', 'A meta-interpreter implementing'), item(302, '/swish/example/grammar.pl', 'Render parse trees using a tree, but ignore lists Relies on native SVG'), item(302, '/swish/example/houses_puzzle.pl', 'Examples/houses puzzle'), item(302, '/swish/example/htmlcell.swinb', 'Examples/htmlcell.swinb'), item(302, '/swish/example/inference/admission.pl', 'Inference/admission'), item(302, '/swish/example/inference/alarm.pl', 'Inference/alarm'), item(302, '/swish/example/inference/alarm_R.pl', 'Inference/alarm R'), item(302, '/swish/example/inference/arithm.pl', 'Inference/arithm'), item(302, '/swish/example/inference/arithm_R.pl', 'Inference/arithm R'), item(302, '/swish/example/inference/bloodtype.pl', 'Inference/bloodtype'), item(302, '/swish/example/inference/bloodtype_R.pl', 'Inference/bloodtype R'), item(302, '/swish/example/inference/coin.pl', 'Inference/coin'), item(302, '/swish/example/inference/coin.swinb', 'Inference/coin.swinb'), item(302, '/swish/example/inference/coin2.pl', 'Inference/coin2'), item(302, '/swish/example/inference/coin2_R.pl', 'Inference/coin2 R'), item(302, '/swish/example/inference/coin_R.pl', 'Inference/coin R'), item(302, '/swish/example/inference/coinmc.pl', 'Inference/coinmc'), item(302, '/swish/example/inference/coinmc_R.pl', 'Inference/coinmc R'), item(302, '/swish/example/inference/coinmsw.pl', 'Inference/coinmsw'), item(302, '/swish/example/inference/coinmsw_R.pl', 'Inference/coinmsw R'), item(302, '/swish/example/inference/cont.swinb', 'Inference/cont.swinb')])
     ).

:- export(test12345/0).

:- fixup_exports.
:- debug.


/*
:- if(exists_source(components(menu))).
:- system:use_module(components(menu)).	% ClioPatria Menu
:- cp_menu:export(cp_menu:menu_items/3).
:- cp_menu:export(cp_menu:menu_item/2).
:- import(cp_menu:menu_items/3).
:- import(cp_menu:menu_item/2).
:- endif.
*/
:- list_filesystems(_Request).

% :-  filesystem_files(_18464).
