% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(run_testcases, [
		run_testcases/0
	]).

:- use_module(run_acerules).

/** <module> Run testcases

Runs all the testcases that can be found.

@author Tobias Kuhn
@version 2007-02-06
*/


%% run_testcases
%
% Runs all the testcases that are found in the directory "testcases/" and stores the
% results in the same directory.

run_testcases :- 
    user:file_search_path(ace_rules, Dir),!,
    setup_call_cleanup(
      working_directory(Was, Dir),    
      run_testcases_here,
      working_directory(_, Was)).


run_testcases_here :-
    expand_file_name('testcases/court/input/*', CourtFiles),
    run_files_court(CourtFiles),
    expand_file_name('testcases/stable/input/*', StableFiles),
    run_files_stable(StableFiles),
    expand_file_name('testcases/stable_strong/input/*', StableStrongFiles),
    run_files_stable_strong(StableStrongFiles).


%% run_files_court
%
% Runs the testcases for the courteous mode.

run_files_court([]).

run_files_court([InputFile|Rest]) :-
    atom_concat('testcases/court/input/', FileName, InputFile),
    atom_concat('testcases/court/output/', FileName, OutputFile),
    run(InputFile, OutputFile, court),
    run_files_court(Rest).


%% run_files_stable
%
% Runs the testcases for the stable mode.

run_files_stable([]).

run_files_stable([InputFile|Rest]) :-
    atom_concat('testcases/stable/input/', FileName, InputFile),
    atom_concat('testcases/stable/output/', FileName, OutputFile),
    run(InputFile, OutputFile, stable),
    run_files_stable(Rest).


%% run_files_stable_strong
%
% Runs the testcases for the stable with strong negation mode.

run_files_stable_strong([]).

run_files_stable_strong([InputFile|Rest]) :-
    atom_concat('testcases/stable_strong/input/', FileName, InputFile),
    atom_concat('testcases/stable_strong/output/', FileName, OutputFile),
    run(InputFile, OutputFile, stable_strong),
    run_files_stable_strong(Rest).
