
% ?- load_files(library(random)).
% ?- load_files(library(date)).
% ?- load_files(library(strings)).

?- use_module(library(random)).
?- use_module(library(system)).
?- use_module(library(lists)).

?- consult(library(sre_dna/dynamics)).
?- consult(library(sre_dna/dctg)).
?- consult(library(sre_dna/parameters_P)).
?- consult(library(sre_dna/operators)).
?- consult(library(sre_dna/dctg_pp)).
?- consult(library(sre_dna/utils)).

?- dctg_file_P(FileDCTG),grammar(FileDCTG), make_grammar_table.
regen:- tell( 'compile_file.pl' ),     % fast: new
	write( '?- use_module(library(lists)).
        '),
        nl,
	listing,
	told.
:- regen. 



?- consult(library(sre_dna/ccs_utils)).
?- consult(library(sre_dna/dctg_gen)).
?- consult(library(sre_dna/dctg_reprod)).
?- consult(library(sre_dna/dctg_utils)).
?- consult(library(sre_dna/generate)).
?- consult(library(sre_dna/gp_engine)).
?- consult(library(sre_dna/lamarckian)).
?- consult(library(sre_dna/evaluation)).
?- consult(library(sre_dna/file_stats)).

% following must follow 'parameters_P' above.
% Convenient to put here, as interactive debugging of DCTG-GP is easier.

/*
?- consult(library(sre_dna/sre_mutation3)).
?- consult(library(sre_dna/sre_crossover3a)).
?- consult(library(sre_dna/dna_proc)).
?- consult(library(sre_dna/mask_optimizer)).

?- dna_file_P(DNA_file), consult(DNA_file).
*/

?- fitness_func_P(File), consult(File). 

?- fast:consult(compile_file).
?- clean_up.

