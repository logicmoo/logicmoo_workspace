%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: lib/systemvar.pl
%
%    WRITTEN BY: Sebastian Sardina (ssardina@cs.toronto.edu)
%    Time-stamp: <2008-11-29 09:11:03 ssardina>
%    TESTED    : ECLiPSe 5.4 on RedHat Linux 6.2-7.2
%    TYPE CODE : system independent predicates
%
% DESCRIPTION: wide-system variables and constants
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             July 9, 2002
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
%
%                          All Rights Reserved
%
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The following definition of constants are provided:
%
% -- main_dir(Dir) : main directory of the whole code
% -- type_prolog(T) : current prolog engine is T (swi/ecl/sics/van)
% -- executable_path(A, P) : P is the executable path for software A
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- ensure_loaded('logicmoo_workarounds').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% AUTOMATIC LOAD OF REQUIRED LIBRARIES %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This subsection does the following:
%  (a) provides main_dir(Path)
%  (b) loads the neccessary compatibility library: compat_ecl or compat_swi
%  (c) sets ` to be the string construct (using set_backquoted_string)


%:- ignore((type_prolog(swi),use_module(tools_swi))).

% Path is the root path of the IndiGolog system
% In SWI Pwd will be a string already
main_dir(Path):- getenv('PATH_INDIGOLOG',Pwd),
                 (string(Pwd) -> atom_string(APwd, Pwd) ; APwd=Pwd),
                 concat_atom([APwd, '/'], Path).


% REALIZE WHICH PROLOG WE ARE RUNNING
% [Code suggested by Viktor Engelmann <Viktor.Engelmann@rwth-aachen.de>]

:- dynamic type_prolog/1.

% guess_prolog:-!.

guess_prolog :- iso:current_prolog_flag(eclipse_info_suffix,_), !,
		assert(type_prolog(ecl)).
guess_prolog :- iso:current_prolog_flag(executable,_), !,
		assert(type_prolog(swi)).
guess_prolog :- iso:current_prolog_flag(language,sicstus), !,
		assert(type_prolog(sic)).
guess_prolog :- assert(type_prolog(vanilla)).
:- type_prolog(_) -> true ; guess_prolog.


:- main_dir(_)->true;throw(no_main_dir).

% This is the initialization needed for each type of Prolog used
:- type_prolog(ecl) ->        % INITIALIZATION FOR ECLIPSE PROLOG
	use_module(library(tools_ecl)),
	set_flag(debug_compile, off),
	set_flag(variable_names, off),
	set_backquoted_string
	;
   type_prolog(swi) ->        % INITIALIZATION FOR SWI-PROLOG
	main_dir(Dir),
	concat_atom([Dir,'lib'], LibDir),
	assert(library_directory(LibDir)),
	use_module(library(eclipse_swi)), init_eclipse_lib, % ECLIPSE Compat lib
	use_module(library(tools_swi)),
	use_module(library(time)),	% for call_with_time_limit/2
	style_check(-discontiguous),
	set_prolog_flag(optimise, true),
	set_backquoted_string
	;
	true.



% Defines the path of executables used to define device managers
executable_path(swi, '/usr/bin/swipl').
executable_path(eclipse, '/opt/bin/eclipse-pl').  % if available
executable_path(tcltk, '/usr/bin/wish').
executable_path(xterm, '/usr/bin/xterm').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: lib/systemvar.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




