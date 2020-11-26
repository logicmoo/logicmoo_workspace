
?- use_module(library(random)).
?- use_module(library(system)).
?- use_module(library(lists)).

?- compile(dynamics).
?- consult(dctg).
?- consult(parameters_P).
?- compile(operators).
?- consult(dctg_pp).
?- compile(utils).

?- dctg_file_P(FileDCTG),grammar(FileDCTG), make_grammar_table.
?- tell('compile_file.pl'),     % fast: new
	write('
?- use_module(library(lists)).
        '),
        nl,
	listing,
	told.

?- compile('ccs_utils').
?- compile('dctg_gen').
?- compile('dctg_reprod').
?- compile('dctg_utils').
?- compile('generate').
?- compile('gp_engine').
?- compile('lamarckian').
?- compile('evaluation').
?- compile('file_stats').

% following must follow 'parameters_P' above.

?- fitness_func_P(File), compile(File). 

?- fast:compile('compile_file.pl').
?- clean_up.

