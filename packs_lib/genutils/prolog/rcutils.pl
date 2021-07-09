/* 
	Copyright 2015 Samer Abdallah (UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(rcutils, 
      [  persistent_history/2
      ,  confirm_on_halt/0
      ]).

/** <module> Utilities for your .swiplrc 

   This module provides confirm_on_halt/0, to make it harder to exit Prolog
   unintentionally due to over-enthusiastic Ctrl-D pressing,
   persistent_history/2, to keep and periodically save the current command
   line history to an arbitrary file, and defines the file_search_path/2
   location 'home', which maps to the expand_file_name/2 expansion of '~'.

   You might find it useful to put this in your .plrc/.swiplrc, eg
   ===
   :- if(exists_source(library(rcutils)))
   :- use_module(library(rcutils)).
   :- persistent_history('.swipl.history',[interval(300)]).
   :- confirm_on_halt.
   :- endif.
   ===

*/

user:file_search_path(home,Home) :- expand_file_name('~',[Home]).

:- use_module(library(hostname)).

%% confirm_on_halt is det.
%  Installs confirm_halt/0 as a hook to be called before exitting Prolog. 
confirm_on_halt :- at_halt(confirm_halt).

%% confirm_halt is det.
%
%  Asks the user to confirm that they want to exit Prolog. If they do
%  not, then cancel_halt/1 is called.
confirm_halt :-
	write('Are you sure you want to exit? [y/n] '), flush_output,
	repeat,
	get_single_char(C),
	(	C=0'y -> writeln(y)
	;	C=0'n -> cancel_halt('Exit cancelled')
	;	fail
	).

:- dynamic persistent_history_stream_file/2.

%% persistent_history(+File:text, +Opts:options) is det.
%
%  This disables SWIs built-in persistent command line history mechanism if it is
%  enabled and replaces it with one that saves the history in an arbitrary file. This can be useful if the
%  history file is in a directory that is kept synchronised among many computers, stored,
%  or backed up in some other way. This history includes comment lines that give show
%  the command line arguments given to swipl each time it is started. 
%
%  If called during SWI Prolog initialisation (ie in the ~/.swiplrc file, in a command
%  line -g goal, or in a program loaded with -s on the command line) then the line editor
%  and history mechanisms will not have been initialised yet, so you must load whichever
%  line editor library you like (readline or editline) first.
%
%  Valid options are:
%     *  interval(+Interval:number)
%        If supplied, the history is saved every Interval seconds. Otherwise, the history
%        is only saved when Prolog exits (using at_halt/1).
% 
% For example, the author finds it useful to add the following to his ~/.swiplrc:
% ==
% :- use_module(library(hostname)).
% persistent_history :-
%    hostname(Hostname),
%    atom_concat('.swipl_history.',Hostname,HistFile),
%    load_files(library(readline), [if(not_loaded)]),
%    set_prolog_flag(readline, readline), % work around bug in readline foreign library
%    persistent_history(HistFile,[interval(60)]).
% ==
% This means that any program can call persistent_history/0 to get a host specific
% history file, which is useful when a directory is shared or synchronised across
% several machines.

persistent_history(H,Opts) :- 
   current_input(S),
	(	persistent_history_stream_file(S,H) -> true
	;	persistent_history_stream_file(S,H1) -> throw(persistent_history_mismatch(H1,H))
	;	print_message(information, rcutils:history_using_file(H)),
		(  \+current_predicate(prolog_history:prolog_history/1)
      -> create_prolog_flag(save_history, false, [type(boolean)])
      ;  current_prolog_flag(save_history,true) ,
         prolog_history:history_loaded(PHFile)
      -> prolog_history:write_history(PHFile),
         prolog_history(disable),
         print_message(information, rcutils:closed_prolog_history(PHFile))
      ;  set_prolog_flag(save_history,false)
      ),
		(exists_file(H) -> histop(S, load(H)); true),
		assert(persistent_history_stream_file(S,H)),
		current_prolog_flag(os_argv,ARGV),
		atomics_to_string(ARGV," ",Command),
		history_event('Start: ~s',[Command]),
		at_halt(history_event('Halt',[])),
      (  option(interval(Interval),Opts)
      -> print_message(information, rcutils:history_save_interval(Interval)), 
         periodic_save_history(Interval)
      ;  histop(S, save(H))
      )
	).

history_event(Msg,Args) :-
	persistent_history_stream_file(S,H),
	get_time(Now),
	format_time(string(Time),'%+',Now),
	format(string(Info),Msg,Args),
	format(atom(Line),'% ~w | ~s',[Time,Info]),
	debug(history,'History event: ~s',[Line]),
   histop(S, add(Line)),
	histop(S, save(H)).


periodic_save_history(Interval) :-
	persistent_history_stream_file(S,H),
	debug(history,'Saving history to "~s"...',[H]),
   histop(S, save(H)),
   alarm(Interval,periodic_save_history(Interval),_,[remove(true)]).

histop(S,Op) :- once(prolog:history(S,Op)).

prolog:message(rcutils:history_using_file(H))    --> ['Using persistent history file: "~s"'-[H]].
prolog:message(rcutils:history_save_interval(I)) --> ['Will save history every ~w seconds.'-[I]].
prolog:message(rcutils:closed_prolog_history(F)) --> ['Saved final prolog_history snapshot to ~w.'-[F]].
