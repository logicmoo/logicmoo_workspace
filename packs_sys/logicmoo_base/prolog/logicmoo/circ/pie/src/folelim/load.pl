%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Set PIE environment variable to the PIE source directory of your
%%%% installation
%%%%

:- 
   ( ( getenv('PIE', PIE) ;
       prolog_load_context(directory, Dir),
       concat_atom([PIE, '/folelim'], Dir) ) ->
     PROVERS=PIE,

     format(user_error, 'Loading from source directory ~q~n', [PROVERS]),
     concat_atom([PROVERS, '/swilib'], Swilib),
     concat_atom([PROVERS, '/nf'], Nf),
     concat_atom([PROVERS, '/pplatex'], PPLatex),
     concat_atom([PROVERS, '/toytools'], Toytools),
     concat_atom([PROVERS, '/folelim'], Folelim),
     concat_atom([PROVERS, '/cmprover'], CMProver),
     concat_atom([PROVERS, '/lp'], Lp),
     concat_atom([PROVERS, '/kbset'], KBSet),
     asserta(user:file_search_path(swilib, Swilib)),
     asserta(user:file_search_path(nf, Nf)),
     asserta(user:file_search_path(pplatex, PPLatex)),		 
     asserta(user:file_search_path(toytools, Toytools)),
     asserta(user:file_search_path(folelim, Folelim)),
     asserta(user:file_search_path(cmprover, CMProver)),
     asserta(user:file_search_path(lp, Lp)),
     asserta(user:file_search_path(kbset, KBSet))
   ; format(user_error, 'Source directory not found - exiting~n', []),
     halt
   ).

:- set_prolog_flag(toplevel_print_anon, false).
:- set_prolog_flag(verbose, true).
:- set_prolog_flag(verbose_load, true).

:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).
:- set_stream(user_input, encoding(utf8)).
:- set_stream(user_output, encoding(utf8)).
:- set_stream(user_error, encoding(utf8)).

:- use_module(nf(nf)).
:- use_module(nf(nfutils)).
:- use_module(swilib(dotgraph)).
:- use_module(swilib(pretty)).
:- use_module(swilib(info)).

:- use_module(folelim(prooftask_cm)).
:- use_module(folelim(craigtask_cm)).
:- use_module(folelim(support_scratch)).

:- use_module(folelim(solution)).
:- use_module(folelim(pointwise)).

%% Incomplete stuff, needs work:
%%
:- use_module(folelim(craigtask_hyper)).
:- use_module(folelim(hyper_proof_conversion)).
%%
:- use_module(folelim(forminspect_fol)).

:- use_module(toytools(config)).
:- use_module(toytools(toyproject)).
:- use_module(toytools(grounder)).
:- use_module(toytools(auxiliary)).
:- use_module(toytools(dimacsio)).
:- use_module(toytools(external)).
:- use_module(toytools(toyutils)).
% :- use_module(lp(lputils)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% This Section mainly concerns ToyElim and SAT-Solver setup
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Edit this to configure ToyElim to work with external solvers
%%%% 
:- set_toyelim_configuration(
	[ %%%%
	  %%%% Set verbosity higher to get notice when external solvers
	  %%%% are called:
	  %%%%
	  verbosity = 4,
	  %%%%
	  %%%% Temporary files for use with external solvers are written
	  %%%% into this directory:
	  %%%%
	  tmpdir = '/tmp',
	  %%%%
	  %%%% Specify which solvers should be used SAT, QBF and Variable
	  %%%% Elimination problems. Use 'none' to specify that no external
	  %%%% solver should be used for the respective type of problem. The
	  %%%% solvers specified here must match an entry in the list
	  %%%% external_solvers, which is specified next.
	  %%%%
	  default_external_sat_solver = minisat,
	  default_external_qbf_solver = none,
	  default_external_elim_solver = none,
	  %% default_external_qbf_solver = qbf(1),
	  % default_external_elim_solver = riss_elim(0),
	  % debug_external_elim_solver = true,
	  %%%%
	  %%%% Specify calls to external solvers. At least the solvers
	  %%%% declared as default_external_... should be specified here.
	  %%%% List entries are pairs SolverId-CommandLinePrefix.
	  %%%%
	  %%%% The SolverId is used to choose the input format (see
	  %%%% external.pl).
	  %%%%
	  external_solvers =
	[      minisat - 'minisat -verb=1',
	       minisat2(0) - 'minisat -verb=1',
	       minisat2(1) - 'minisat -verb=2',
	       minisat2(2) - 'minisat-core -verb=2',
	       riss(0) - '/Users/ch/get/bin/riss',
	       riss_elim(0) - '/Users/ch/get/bin/coprocessor',
	       dnf_compiler(0)-'/Users/ch/get/bin/dnf_compiler_0.sh',
	       dnf_compiler(1)-'/Users/ch/get/bin/dnf_compiler_1.sh',
	       dnf_compiler(2)-'/Users/ch/get/bin/dnf_compiler_2.sh',
	       qbf(0) - '/Users/ch/get/quantor-3.0/quantor',
	       qbf(1) - '/Users/ch/get/depqbf-0.1/depqbf']
	]).


/*
%%%% 
%%%% Uncomment this if you do not want to use external solvers
%%%% 
:- set_toyelim_configuration(
     [ default_external_sat_solver = none,
       default_external_qbf_solver = none ]).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


