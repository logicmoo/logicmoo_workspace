
:- module(logicmoo_webui,[
   load_web_package_dirs/0,
   webui_load_swish_and_clio/0,
   webui_start_swish_and_clio/0]).

:- use_module(library(prolog_pack)).

:- if( \+ current_prolog_flag(windows,true)).
 :- if( \+ exists_source(library(phil))).
  attach_linuxOnly_packs_web :- (working_directory(Dir,Dir);prolog_load_context(directory,Dir)),
          ('../packs_lib'=Rel;'../../packs_lib'=Rel;'../../../packs_lib'=Rel;
           '../../../../packs_lib'=Rel;'../../../../../../packs_lib'=Rel),
        absolute_file_name(Rel,LinuxOnlyPacks,[relative_to(Dir),file_type(directory),file_errors(fail)]),
        attach_packs(LinuxOnlyPacks),!,
        pack_list_installed.

 :- endif.
:- endif.

:- dynamic(lmconfig:logicmoo_webui_dir/1).

:- lmconfig:logicmoo_webui_dir(_) -> true;
  prolog_load_context(directory,Dir),asserta(lmconfig:logicmoo_webui_dir(Dir)).

%:- listing(lmconfig:logicmoo_webui_dir/1).

attach_packs_relative_web_dir(Rel):-
   once(((
    lmconfig:logicmoo_webui_dir(Dir),
    (absolute_file_name(Rel,PackDir,[relative_to(Dir),file_type(directory),file_errors(fail)]);
      absolute_file_name(Rel,PackDir,[file_type(directory),file_errors(fail)])),
    writeln(attach_packs(PackDir)),attach_packs(PackDir)));writeln(failed(attach_packs_relative_web_dir(Rel)))).



load_web_package_dirs:- 

  findall(PackDir,'$pack':pack(Pack, PackDir),Before),  

   ignore(catch(make_directory('/tmp/tempDir/pack'),_,true)),
   (user:file_search_path(pack,'/tmp/tempDir/pack') -> true ; asserta(user:file_search_path(pack,'/tmp/tempDir/pack'))),
   attach_packs('/tmp/tempDir/pack'),
   % pack_install(trill,[upgrade(true),interactive(false)]),    
   % pack_install(cplint_r,[upgrade(true),interactive(false)]),
   % pack_install(rocksdb,[upgrade(true),interactive(false)]),
   % pack_install(bddem,[upgrade(true),interactive(false)]),    
   % pack_install(sldnfdraw,[upgrade(true),interactive(false)]),
   % pack_install(phil,[upgrade(true),interactive(false)]),
   !,

  ignore(( \+ exists_source(library(logicmoo_common)), attach_packs_relative_web_dir('../../logicmoo_utils/../'))),
  ignore(( \+ exists_source(library(sldnfdraw)), attach_packs_relative_web_dir('../../packs_lib/'))),
  ignore(( \+ exists_source(library(lps_corner)), attach_packs_relative_web_dir('../..'))),
  ignore(( \+ exists_source(library(rserve_client)), attach_packs_relative_web_dir('../packs_web/swish/pack/'))),
  % ignore(( \+ exists_source(library(rserve_client)), attach_packs_relative_web_dir('../swish/pack/'))),
  % ignore(( \+ exists_source(pack(plweb/pack_info)), attach_packs('/opt/logicmoo_workspace/packs_web'))),
  findall(PackDir,'$pack':pack(Pack, PackDir),After),
  (Before\==After -> (writeln(load_package_dirs(After)),pack_list_installed) ; true),
  !.


  
:- initialization(load_web_package_dirs, now).
:- initialization(load_web_package_dirs, restore_state).

:- multifile(sandbox:safe_primitive/1).
:- dynamic(sandbox:safe_primitive/1).
:- multifile(sandbox:safe_meta_predicate/1).
:- dynamic(sandbox:safe_meta_predicate/1).

:- dynamic(http_unix_daemon:http_daemon/0).

:- use_module(library(logicmoo_common)).
:- if(\+ prolog_load_context(reloading,true)).
:- use_module(library(sandbox)).
:- use_module(library(pengines_sandbox)).
% :- rtrace.
:- system:use_module(library(console_input)).
:- system:use_module(library(date)).
:- system:use_module(library(make)).
:- system:use_module(library(qsave)).
:- system:use_module(library(prolog_autoload)).
:- system:use_module(library(lists)).
:- system:use_module(library(backcomp)).
:- system:use_module(library(edit)).
:- system:use_module(library(prolog_trace)).
:- system:use_module(library(threadutil)).
:- system:use_module(library(yall)).
:- system:use_module(library(time)).
:- abolish(system:time/1).
:- system:use_module(library(statistics)).


:- system:use_module(library(apply)).
:- system:use_module(library(assoc)).
:- system:use_module(library(base64)).
:- system:use_module(library(charsio)).
:- system:use_module(library(codesio)).
:- system:use_module(library(crypt)).
:- system:use_module(library(ctypes)).
:- system:use_module(library(dialect)).
:- system:use_module(library(doc_files)).
:- system:use_module(library(doc_http)).
:- system:use_module(library(edinburgh)).
:- system:use_module(library(error)).
:- system:use_module(library(filesex)).
:- system:use_module(library(gensym)).
:- system:use_module(library(git)).
:- system:use_module(library(http/html_head)).
:- system:use_module(library(http/http_dispatch)).
:- system:use_module(library(http/http_path)).
:- system:use_module(library(http/mimetype)).
:- system:use_module(library(lazy_lists)).
:- system:use_module(library(listing)).
:- system:use_module(library(lists)).
:- system:use_module(library(memfile)).
:- system:use_module(library(modules)).
:- system:use_module(library(nb_rbtrees)).
:- system:use_module(library(occurs)).
:- system:use_module(library(operators)).
:- system:use_module(library(option)).
:- system:use_module(library(ordsets)).
:- system:use_module(library(pairs)).
:- system:use_module(library(pldoc)).
:- system:use_module(library(pldoc/doc_html)).
:- system:use_module(library(pldoc/doc_process)).
:- system:use_module(library(pldoc/doc_search)).
:- system:use_module(library(pldoc/doc_util)).
:- system:use_module(library(pldoc/man_index)).
:- system:use_module(library(porter_stem)).
%:- system:use_module(library(pprint)).
:- system:use_module(library(predicate_options)).
:- system:use_module(library(process)).
:- system:use_module(library(prolog_clause)).
:- system:use_module(library(prolog_code)).
:- system:use_module(library(prolog_codewalk)).
:- system:use_module(library(prolog_config)).
:- system:use_module(library(prolog_source)).
:- system:use_module(library(prolog_stack)).
:- system:use_module(library(prolog_xref)).
:- system:use_module(library(pure_input)).
:- system:use_module(library(quintus)).
:- system:use_module(library(readutil)).
:- system:use_module(library(sgml)).
:- system:use_module(library(sgml_write)).
:- system:use_module(library(sha)).
:- system:use_module(library(shell)).
:- system:use_module(library(shlib)).
:- system:use_module(library(socket)).
:- system:use_module(library(solution_sequences)).
:- system:use_module(library(sort)).
:- system:use_module(library(ssl)).
:- system:use_module(library(system)).
:- system:use_module(library(thread_pool)).
:- system:use_module(library(uri)).
:- system:use_module(library(url)).
:- system:use_module(library(uuid)).
:- system:use_module(library(varnumbers)).
:- system:use_module(library(when)).
:- system:use_module(library(writef)).
:- system:use_module(library(zlib)).

%:- system:use_module(library(jpl)).
%:- use_module(library(wfs)).
:- system:use_module(library(wfs),[call_residual_program/2,call_delays/2,delays_residual_program/2,answer_residual/2]).
%:- system:use_module(library(gui_tracer)). % autoloading swi_ide:guitracer/0 from /usr/lib/swipl/xpce/prolog/lib/gui_tracer
%:- system:use_module(library(swi_compatibility)). %% autoloading swi_ide:auto_call/1 from /usr/lib/swipl/xpce/prolog/lib/swi_compatibility
:- endif.

sandbox:safe_primitive(dumpst:dumpST).
sandbox:safe_meta_predicate(system:notrace/1).

:- if(\+ prolog_load_context(reloading,true)).
:- use_module(library(sandbox)).
:- use_module(library(pengines_sandbox)).
:- endif.

:- use_module(library(logicmoo_web_long_message)).

inoxf(Goal):- ignore(notrace(on_x_fail(Goal))).

webui_load_swish_and_clio:-
   ignore(notrace(on_x_fail(webui_load_swish_and_clio_now))).

webui_load_swish_and_clio_now:-
   maplist(inoxf,[
   lmconfig:logicmoo_webui_dir(Dir),
   % trace,
   absolute_file_name('../../swish/run_swish_and_clio',Run,[relative_to(Dir),file_type(prolog),file_errors(fail)]),
   user:ensure_loaded(Run),
   swish_app:load_config('./config-enabled-swish'),
   listing(swish_config:login_item/2)]),!.


webui_start_swish_and_clio:- 
   maplist(inoxf,[
   webui_load_swish_and_clio,
   broadcast:broadcast(http(pre_server_start)),
   cp_server:cp_server([]),
   set_long_message_server('https://logicmoo.org'),
   broadcast:broadcast(http(post_server_start)),
   swish:start_swish_stat_collector]),!.

:- initialization(webui_start_swish_and_clio,restore).
:- initialization(webui_start_swish_and_clio,program).

