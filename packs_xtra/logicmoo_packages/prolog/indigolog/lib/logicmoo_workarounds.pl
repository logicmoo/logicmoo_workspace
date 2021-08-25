%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: lib/er1-actions.pl
%
%    WRITTEN BY: Sebastian Sardina (ssardina@cs.toronto.edu)
%    Time-stamp: <03/05/04 12:47:48 ssardina>
%    TESTED    : ECLiPSe 5.4 on RedHat Linux 6.2-7.2
%    TYPE CODE : system independent predicates 
%
% DESCRIPTION: mapping between ER1 low-level actions and ER1 IndiGolog actions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The following definition of constants are provided:
%
% -- actionNum(ER1-HighLevelAction, ER1-LowLevelAction)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type_prolog(swi).
% set_debug_level(_).
% report_message(A,B):-writeq(report_message(A,B)),nl.
%:- ensure_loaded('lib/common').

% load_device(Env, Command, Address)
:-multifile(load_device/3).
:-multifile(device_manager/4).

:-use_module(library(process)).

:- current_prolog_flag(windows,true)->
   setenv('PATH_INDIGOLOG','t:/devel/logicmoo/src_modules/indigolog');
   setenv('PATH_INDIGOLOG','/devel/logicmoo/src_modules/indigolog').

user:file_search_path(library,ATLIB):-getenv('PATH_INDIGOLOG',AT),atom_concat(AT,'/lib',ATLIB).
user:file_search_path(indigolog,AT):-getenv('PATH_INDIGOLOG',AT).

win_fork(G,SERVIO,PID):-atom_concat('swipl-win.exe ',G,AC),writeq(win_fork(AC,SERVIO)),nl,
      win_exec(AC,showdefault),PID = 0.

% win_fork(" -t  start -f t:/devel/logicmoo/src_incoming/indigolog/Env/env_wumpus.pl 
%  host=localhost port=47853  debug=1 ipwumpus=127.0.0.1 portwumpus=9002 ppits=10 nogolds=1 size=8 idrun='indigolog(default)' idscenario='random' ; exit").
%


:- current_prolog_flag(windows,true)->
   asserta((fork(G):-win_fork(G)));
   use_module(library(unix)).

:- use_module(eclipse_swi).
%:- ensure_loaded('common').
:- ensure_loaded(tools_swi).

% fork(G):-writeq(fork(G)),nl.

% type_prolog(swi).
 %set_debug_level(_).
 %report_message(A,B):-writeq(report_message(A,B)),nl.

%Load the TCP Library
/*
:- use_module(library(socket)).
%:- use_module(library(unix)).
:-use_module(library(system)).
:- use_module(library(readutil)).
:-use_module(library(listing)).
:- use_module(library(shell)).
:- use_module(library(shlib)).
:- use_module(library(url)).
:- use_module(library(quintus)).
:- use_module(library(qsave)).
:- use_module(library(sgml)).
:- use_module(library(occurs)).
%:-use_module(library(rdf)).
*/
