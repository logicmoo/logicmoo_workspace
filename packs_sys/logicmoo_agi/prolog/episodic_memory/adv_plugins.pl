/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Feb 20, 2020 - Andrew Dougherty
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
%
*/
:- '$set_source_module'(mu).


:- multifile(adv_global:nomic_config_dir/1).
:- dynamic(adv_global:nomic_config_dir/1).


local_resolve_dir(Target, Res):- \+ atom(Target), !, expand_file_search_path(Target, Mid), local_resolve_dir(Mid, Res), !.
local_resolve_dir(Target, Res):- prolog_load_context(directory, Dir), absolute_file_name(Target, Res, [relative_to(Dir), file_type(directory), access(read), file_errors(fail)]), !.
local_resolve_dir(Target, Res):- absolute_file_name(Target, Res, [file_type(directory), access(read), file_errors(fail)]), !.
local_resolve_dir(Target, Res):- absolute_file_name(Target, Res, [file_type(directory), access(none)]), !.

add_config_dir(Target):- is_list(Target), !, must_maplist(add_config_dir, Target).
add_config_dir(Target):-
  catch(local_resolve_dir(Target, Where), error(existence_error(source_sink, Where), _), true),
  (\+ exists_directory(Where) -> catch(make_directory_path(Where), _, true); true),
  (\+ exists_directory(Where) -> dbug1(no_dir(Where)); assert_if_new(adv_global:nomic_config_dir(Where))), !.


%add_config_dir(Target):- expand_file_search_path(Target, Result)*->add_config_dir(Result);add_config_dir_0(Target ).


:- add_config_dir('../config_nomicmu/').
:- add_config_dir(app_preferences('.config/nomicmu/')).

scan_and_load_plugins:-
  forall(adv_global:nomic_config_dir(Dir),
    ignore(scan_and_load_plugins(Dir))).

scan_and_load_plugins(Dir):-
   directory_file_path(Dir, 'plugins/*.pl', Expand),
   expand_file_name(Expand, Res),
   forall(member(File, Res), load_plugin_file(File)).

load_plugin_file(File):-
  dmsg(load_plugin_file(File)), ensure_loaded(File).

per_plugin(Call, Mem0, Mem9):-
  strip_module(Call, M, P),
  P=..PList,
  get_plugin_prefixes(PrefixList),
  SE = did(false),
  per_plugin(SE, PrefixList, M, PList, Mem0, Mem9), !,
  SE \== did(false).

per_plugin(SE, [Plug|List], M, Call, Mem0, Mem9):- !,
  per_plugin(SE, Plug, M, Call, Mem0, Mem1)
   -> per_plugin(SE, List, M, Call, Mem1, Mem9)
   ;  per_plugin(SE, List, M, Call, Mem0, Mem9).
per_plugin(_SE, [], _M, _Call, Mem0, Mem0):- !.
per_plugin(SE, Prefix, M, [P|ARGS], Mem0, Mem9):-
   %get_plug_prefix(Plug, Prefix),
   atom_concat(Prefix, P, F),
   append(ARGS, [Mem0, Mem9], AARGS),
   CALL=..[F|AARGS],
   (current_predicate(_, M:CALL) -> call_plugin(M, CALL) -> nb_setarg(1, SE, true); Mem0=Mem9 ; Mem0=Mem9).

call_plugin(M, CALL):- catch(M:call(CALL), _, fail).

get_plugin_prefixes(PrefixList):- get_plugins(PlugList), must_maplist(get_plug_prefix, PlugList, PrefixList).

get_plug_prefix(Plug, Prefix):- get_advstate(S0), getprop(Plug, prefix=Prefix, S0), !.

get_plugins(PlugList):- get_advstate(S0),
   get_objects(inherited(nomicmu_plugin);inherit(nomicmu_plugin), PlugList, S0), !.

:- during_boot(scan_and_load_plugins).
