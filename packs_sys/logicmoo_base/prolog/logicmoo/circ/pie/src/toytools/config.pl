%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2010, 2015 Christoph Wernhard
%%%%
%%%% This program is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% This program is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(config, [get_conf/2,
		   set_toyelim_configuration/1,
		   get_toyelim_configuration/1,
		   print_toyelim_configuration_doc/0]).


:- use_module(swilib(pretty)).
:- use_module(swilib(err)).

:- dynamic get_conf_1/2.

get_conf(Key, Value) :-
	( get_conf_1(Key, Value1) ->
	  Value = Value1
	; err('Bad configuration key: ~q', [Key])
	).     

set_toyelim_configuration(KVs) :-
	( member(K=V, KVs),
	  retractall(get_conf_1(K, _)),
	  assert(get_conf_1(K, V)),
	  fail
	; true
	).

get_toyelim_configuration(KVs) :-
	findall(K=V, (conf(K, _, _), get_conf(K,V)), KVs).	

init_configuration :-
	( get_conf_1(_, _) ->
	  true
	; findall(K=V, (conf(K, V, _)), KVs),
	  set_toyelim_configuration(KVs)
	).

conf(verbosity, 0,
     ['Verbosity, a number >= 0. Messages are printed if their associated',
      'threshold is below the specified verbosity. The policy there is',
      'roughly as follows:',
      '    0 : No messages',
      '    1 : Important information',
      '   10 : Information about global progress',
      '   20 : Special information about local methods',
      '   30 : Calls to external solvers',
      '   40 : Success of a scheduled method among alternatives',
      '   50 : Details of calls to external solvers',
      '   60 : Progress of local methods',
      '   70 : Information useful for debugging']).
conf(tmpdir, '/tmp',
     ['Directory for temporary files. An atom.']).
conf(keep_tmpfiles, false,
     ['If true, temporary files are given names suffixed with',
      'successive numbers. Values: true or false']).
conf(time_limit(external_solvers), 30,
     ['Default time limit in seconds for external solvers.']).
conf(external_solvers,
     [minisat2(0)-'/Users/ch/get1/bin/minisat -verb=1',
      minisat2(1)-'/Users/ch/get1/bin/minisat -verb=2',
      minisat2(2)-'/Users/ch/get1/bin/minisat-core -verb=2',
      riss(0)-'/Users/ch/get/bin/riss',
      riss_elim(0)-'/Users/ch/get/bin/riss',
      dnf_compiler(0)-'/Users/ch/get/bin/dnf_compiler_0.sh',
      qbf(0)-'/Users/ch/get/quantor-3.0/quantor',
      qbf(1)-'/Users/ch/get/depqbf-0.1/depqbf'],
     ['External solvers. List of SolverId-CommandLinePrefix pairs. SolverId',
      'is a one-argument term where the functor determines the interface',
      '(in- and output formats, interpretation of result status) of the',
      'solver. The argument term just serves as identifier.',
      'CommandLinePrefix specifies the prefix of the corresponding shell',
      'call.']).
conf(default_external_sat_solver, minisat2(0),
     ['The solver (specified with external_solvers) that is used by default',
      'for SAT problems. The atom \'none\' if no external SAT solver should',
      'be used.']).
conf(default_external_qbf_solver, qbf(0),
     ['The solver (specified with external_solvers) that is used by default',
      'for QBF problems, The atom \'none\' if no external QBF solver should',
      'be used.']).
conf(default_external_elim_solver, riss_elim(0),
     ['The solver (specified with external_solvers) that is used by default',
      'for variable elimination problems. The atom \'none\' if no external',
      'variable elimination solver should be used.']).
conf(version, 0,
     ['A specific implementation version that should be used. Sometimes',
      'older versions are better debugged or work better with certain',
      'applications.',
      '   0 : use the most recent version',
      '   1 : use the old (before March 2011) implementation of elim/2']).
conf(record_mode, false,
     ['In record mode, information about projection computation tasks',
      'encountered is stored and can be retrieved afterwards with rec/1.',
      'Values: true or false']).
conf(time_limit(cnf_nondef), 1.0,
     ['Time limit in seconds for computing non-definitional CNFs.']).
conf(elim_method, default,
     ['Specificates setup of elimination methods to be used']).
conf(decomp_method, 2,
     ['[experimental] Method for distributing projection over conjunction',
      'used in certain simplifications. Possible values:',
      '  0: no distribution',
      '  1: distribution over binary conjunctions',
      '  2: distribution of single literals to forget over n-ary',
      '     conjunctions',
      '  3: distribution over n-ary conjunctions']).
conf(simp_main_method, 1,
     ['[experimental] Method used in certain simplifications.',
      'Possible values:',
      '  1: method 1',
      '  2: method 2']).
conf(simp_aux_method, 1,
     ['[experimental] Method used in certain simplifications.',
      'Possible values:',
      '  1: method 1',
      '  2: method 2 (suited for external elimination solvers)']).
conf(time_limit(simp), 5,
     ['[deprecated] Time limit in seconds for some rewriting simplifications']).
conf(simplifications, default,
     ['[deprecated] Determines how some simplifications are performed. Possible',
      'values are:',
      '  default : the default method.',
      '  rough   : a fast method, but with fewer simplifications']).     
conf(debug_external_elim_solver, false,
     ['Useful to debug an external solver for variable elimination',
      'If true, calls to the default_external_elim_solver are',
      'also evaluated by the fallback solver and their results',
      'are compared.',
      'Values: true or false']).
conf(fol_equality_mode, standard,
     ['FOL: Determines how equality is handled. Values are: ',
      '  standard : different terms may denote the same object',
      '  herbrand : different terms denote different objects']).
% conf(prover9_tmp_input,
%      '/tmp/te_prover9.in',
%      ['FOL: Temporary input file for Prover 9']).
% conf(prover9_tmp_output,
%      '/tmp/te_prover9.out',
%      ['FOL: Temporary output file for Prover 9']).
% conf(prover9_tmp_error,
%      '/tmp/te_prover9.err',
%      ['FOL: Temporary error output file for Prover 9']).
conf(prover9_cmd,
     'prover9',
     ['FOL: Prover9 executable']).
conf(mace4_cmd,
     'mace4',
     ['FOL: Mace4 executable']).
conf(mace4_timeout,
     5,
     ['FOL: Mace4 timeout in seconds, a number']).
conf(prover9_timeout,
     10,
     ['FOL: Prover9 timeout in seconds, a number']).
conf(hyper_tmp_input,
     '/tmp/te_hyper.in',
     ['FOL: Temporary input file for Hyper']).
conf(hyper_tmp_output,
     '/tmp/te_hyper.out',
     ['FOL: Temporary output file for Hyper']).
conf(hyper_tmp_error,
     '/tmp/te_hyper.err',
     ['FOL: Temporary error output file for Hyper']).
conf(hyper_version,
     'new_hyper',
     ['FOL: Version of Hyper (input and proof formats differ). Possible values: ',
      '   new_hyper - for Hyper as of 2014',
      '   old_krh   - for KRHyper as of 2005']).
conf(hyper_cmd,
     'hyper',
     ['FOL: Hyper executable']).
     

print_toyelim_configuration_doc :-
	write('===================='),
	writeln('=========================================================='),
	writeln('Configuration Options'),
	write('===================='),
	writeln('=========================================================='),
	( conf(Opt, Default, DocLines),
	  format('~q, default: ~q~n', [Opt, Default]),
	  ( member(Line, DocLines),
	    format('  ~w~n', [Line]),
	    fail
	  ; true
	  ),
	  write('--------------------'),
	  writeln('----------------------------------------------------------'),
	  fail
        ; true
        ).

:- init_configuration.