/* * module * 
% Very simple... but kept separate to maintain modularity
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
%
% :- module(modScansrc, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

%:- export(found_undef/3).
%found_undef(_,_,_).
%:- dynamic undef/2.

/*
% when we import new and awefull code base (the previous )this can be helpfull
% we redfine list_undefined/1 .. this is the old version
:- export(scansrc_list_undefined/1).
scansrc_list_undefined(_):-!.
scansrc_list_undefined(A):- real_list_undefined(A).

list_undefined:-real_list_undefined([]).

:- export(real_list_undefined/1).
real_list_undefined(A):-
 merge_options(A, [module_class([user])], B),
        prolog_walk_code([undefined(trace), on_trace(found_undef)|B]),
        findall(C-D, retract(undef(C, D)), E),
        (   E==[]
        ->  true
        ;   print_message(warning, check(undefined_predicates)),
            keysort(E, F),
            group_pairs_by_key(F, G),
            maplist(check:report_undefined, G)
        ).


:- export(remove_undef_search/0).
remove_undef_search:- ((
 '@'(use_module(library(check)),'user'),
 redefine_system_predicate(check:list_undefined(_)),
 abolish(check:list_undefined/1),
 assert((check:list_undefined(A):- not(thread_self_main),!, ignore(A=[]))),
 assert((check:list_undefined(A):- reload_library_index,  update_changed_files,call(thread_self_main),!, ignore(A=[]))),
 assert((check:list_undefined(A):- ignore(A=[]),scansrc_list_undefined(A))))).
*/

baseKB:action_info(actScansrc,"Scan for sourcecode modifed on filesystem and logicmoo. NOTE: only new files with this mask (src_incoming/*/?*.pl) are picked up on").
baseKB:agent_call_command(Agent,actScansrc):-  once('@'(agent_call_safely(Agent,actScansrc),'user')).

:-export(actScansrc/0).
actScansrc :- 
 ensure_loaded(library(make)),
 on_x_debug((
  reload_library_index,
  %remove_undef_search,
  update_changed_files,
  include_moo_files_not_included('../src_mud/*/?*.pl'),
  include_moo_files_not_included('../src_game/*/?*.pl'),   
   % autoload,
   % include_moo_files_not_included('../src_incoming/*/*/?*.pl'),
   % make,
   % include_moo_files_not_included('../src_incoming/*/?*.pfc.pl'),
   rescandb,
   !)). 

include_moo_files_not_included(Mask):- 
   expand_file_name(Mask,X),
     forall(member(E,X),include_moo_file_ni(E)).

include_moo_file_ni(M):-absolute_file_name(M,EX,[expand(true),access(read),file_type(prolog)]),include_moo_file_ni_1(EX).


/*

:-export(mmake/0).
mmake:- update_changed_files.
:-export(update_changed_files/0).

update_changed_files:-thread_signal(main,update_changed_files0).
update_changed_files0 :-
  locally(set_prolog_flag(dialect_pfc,default),
       (( set_prolog_flag(verbose_load,true),
        ensure_loaded(library(make)),
	findall(File, make:modified_file(File), Reload0),
	list_to_set(Reload0, Reload),
	(   prolog:make_hook(before, Reload)
	->  true
	;   true
	),
	print_message(silent, make(reload(Reload))),
	maplist(make:reload_file, Reload),
	print_message(silent, make(done_mud(Reload))),
	(   prolog:make_hook(after, Reload)
	->  true
	;   
           true %list_undefined,list_void_declarations
	)))).
*/

include_moo_file_ni_1(M):- atomic_list_concat([_,_|_],'_i_',M),!.
include_moo_file_ni_1(M):- atomic_list_concat([_,_|_],'_c_',M),!.
include_moo_file_ni_1(M):- source_file_property(M,_),!.
include_moo_file_ni_1(M):- source_file_property(_,includes(M)),!.

include_moo_file_ni_1(M):- baseKB:ensure_loaded(M).


:- include(prologmud(mud_footer)).
