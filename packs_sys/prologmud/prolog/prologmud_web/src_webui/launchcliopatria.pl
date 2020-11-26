#!/usr/bin/env swipl 

% :- throw('dont call lauchclio!').

:- initialization cp_server.

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file provides a skeleton startup file.  It can be localized by running

    % ./configure			(Unix)
    % Double-clicking win-config.exe	(Windows)

After  that,  the  system  may  be  customized  by  copying  or  linking
customization  files  from  config-available    to  config-enabled.  See
config-enabled/README.txt for details.

To run the system, do one of the following:

    * Running for development
      Run ./run.pl (Unix) or open run.pl by double clicking it (Windows)

    * Running as Unix daemon (service)
      See daemon.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% Setup search path for cliopatria. We add  both a relative and absolute
% path. The absolute path allow us to  start in any directory, while the
% relative one ensures that the system remains working when installed on
% a device that may be mounted on a different location.

add_relative_search_path(Alias, Abs) :-
	is_absolute_file_name(Abs), !,
	prolog_load_context(file, Here),
	relative_file_name(Abs, Here, Rel),
	assertz(user:file_search_path(Alias, Rel)).
add_relative_search_path(Alias, Rel) :-
	assertz(user:file_search_path(Alias, Rel)).

% :- use_module(cliopatria_binding).
user:file_search_path(cliopatria, '../../ClioPatria').
:- add_relative_search_path(cliopatria, '../../ClioPatria').

% Make loading files silent. Comment if you want verbose loading.

:- current_prolog_flag(verbose, Verbose),
   asserta(saved_verbose(Verbose)),
   set_prolog_flag(verbose, silent).


		 /*******************************
		 *	      LOAD CODE		*
		 *******************************/

% Use the ClioPatria help system.  May   be  commented to disable online
% help on the source-code.

:- use_module(cliopatria('applications/help/load')).

% Load ClioPatria itself.  Better keep this line.

:- use_module(cliopatria(cliopatria)).

% Load package manager

:- use_module(library(cpack/cpack)).

% Load the remainder of the  configuration. The directory config-enabled
% can also be used to  load   additional  (plugin)  functionality.

:- use_module(library(conf_d)).

:- load_conf_d([ 'config-enabled' ], []).

% :- ensure_loaded(logicmoo(dbase/dbase_rdf_entailment)).


% Get back normal verbosity of the toplevel.

:- (   retract(saved_verbose(Verbose))
   ->  set_prolog_flag(verbose, Verbose)
   ;   true
   ).


