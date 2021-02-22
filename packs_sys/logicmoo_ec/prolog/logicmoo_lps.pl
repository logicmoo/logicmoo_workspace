:- module(logicmoo_lps,[
  run_lps/1,
  test_logicmoo_lps/1,
  test_logicmoo_lps/0, 
  test_logicmoo_lps_sanity/0,
  test_logicmoo_lps_full/0]).

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_common)).

:- reexport(logicmoo_planner).

:- if(\+ exists_source(swish(lib/render))).
   :- if(exists_directory('../logicmoo_webui/swish')).
      :- add_absolute_search_folder(swish,'../logicmoo_webui/swish').
   :- else. :- if(exists_directory('../../logicmoo_webui/swish')).
      :- add_absolute_search_folder(swish,'../../logicmoo_webui/swish').
   :- endif. :- endif.
:- endif.



:- if( \+ current_predicate(swish:is_really_module/0)).
swish:is_really_module.
:- endif.


:- user:use_module(library(listing)).
:- prolog_listing:use_module(library(lists)).

run_lps(File):- with_abs_paths(run_lps0,File).

run_lps0(File):- wdmsg(run_lps(File)),fail.
run_lps0(File):- absolute_file_name(File,DB), exists_file(DB), !,
  update_changed_files, 
  atom_concat("mod_",DB,Mod),run_lps_db(Mod,DB).

run_lps_db(DB,File):-
   DB:unload_file(File),
   abolish_lps_module(DB),
   setup_call_cleanup(true,
     run_lps_db_now(DB,File),
     ((DB:unload_file(File),
       abolish_lps_module(DB)))).

run_lps_db_now(DB,File):-
   DB:use_module(library(lps_corner)), 
   %listing(db:actions/1),
   %listing(interpreter:actions/1),
   interpreter:check_lps_program_module(DB),
   interpreter:must_lps_program_module(DB),
   DB:consult(File),
      interpreter:must_lps_program_module(DB),
   write('% '), writeq(:-listing(DB:_)),writeln('.\n'),
   elsewhere:listing(DB:_),!,
   prolog_statistics:time(DB:golps(X)),
   %listing(interpreter:lps_program_module/1),
   wdmsg(dB(DB,X)).

abolish_lps_module(DB):- 
 forall((current_predicate(DB:F/A),functor(P,F,A),\+ predicate_property(DB:P,imported_from(_))),abolish(DB:F/A)), !.
abolish_lps_module(M):-
 notrace(forall(
   (current_predicate(M:F/A), functor(P,F,A), \+ predicate_property(M:P, imported_from(_))),
    (predicate_property(M:P, static) -> abolish(M:F/A) ; retractall(M:P)))),!,
   (exists_file(M) -> unload_file(M) ; true).


load_lps_corner:-!.
load_lps_corner:- lps_corner:use_module(library(lps_corner)).

test_logicmoo_lps(Files):- run_lps(Files).

test_logicmoo_lps_full:- 
  debug(lps(term_expand)),
  run_lps(library('../examples/binaryChop2.pl')),
  %test_logicmoo_lps('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/lps_user_examples/*.lps'),
  test_logicmoo_lps(library(('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/lps_user_examples/{s,S}*.pl'))),
  test_logicmoo_lps(library(('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/lps_user_examples/*.pl'))),
  nodebug(lps(term_expand)),!,
  test_logicmoo_lps_sanity.
  

test_logicmoo_lps:- test_logicmoo_lps_sanity.

test_logicmoo_lps_sanity:- 
  debug(lps(term_expand)),
  test_logicmoo_lps('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/lps_user_examples/*cooking*.pl'),!,
  test_logicmoo_lps('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/lps_user_examples/*goat*.pl'),!,
  nodebug(lps(term_expand)),!.

:- dynamic user:prolog_file_type/2.
:- multifile user:prolog_file_type/2.

user:prolog_file_type(lps, prolog).


     

