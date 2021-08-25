%% File: leancop_main.pl  -  Version: 1.0  -  Date: 3 July 2009
%%
%% Purpose: Call the leanCoP core prover for a given formula
%%
%% Authors: Jens Otten
%% Web:     www.leancop.de
%%
%% Usage:   leancop_main(X,S,R). % proves formula in file X with
%%                               %  settings S and returns result R
%%
%% Copyright: (c) 2009 by Jens Otten
%% License:   GNU General Public License


:- assert(prolog(swi)).  % Prolog dialect: eclipse, sicstus, swi
:- dynamic(axiom_path/1).


% execute predicates specific to Prolog dialect

:- \+ prolog(eclipse) -> true ;
   nodbgcomp,        % switch off debugging mode
   set_flag(print_depth,10000),  % set print depth
   set_flag(variable_names,off),
   ( getenv('TPTP',Path) -> Path1=Path ; Path1='' ),
   retract_all(axiom_path(_)), assert(axiom_path(Path1)),
   [leancop21].      % load leanCoP core prover

:- \+ prolog(sicstus) -> true ;
   use_module(library(system)),  % load system module
   ( environ('TPTP',Path) -> Path1=Path ; Path1='' ),
   retractall(axiom_path(_)), assert(axiom_path(Path1)),
   [leancop21_sic].  % load leanCoP core prover

:- prolog(Dialect), Dialect\=swi -> true ;
   set_prolog_flag(optimise,true),  % optimise compilation
   ( getenv('TPTP',Path) -> Path1=Path ; Path1='' ),
   retractall(axiom_path(_)), assert(axiom_path(Path1)),
   [leancop21_swi].  % load leanCoP core prover


% load additional leanCoP components

:- [leancop_proof].  % load program for proof presentation
:- [leancop_tptp2].  % load program for TPTP input syntax


%%% call leanCoP core prover

leancop_main(File,Settings,Result) :-
    axiom_path(AxPath), ( AxPath='' -> AxDir='' ;
    name(AxPath,AxL), append(AxL,[47],DirL), name(AxDir,DirL) ),
    ( leancop_tptp2(File,AxDir,[_],F,Conj) ->
      Problem=F ; [File], f(Problem), Conj=non_empty ),
    ( Conj\=[] -> Problem1=Problem ; Problem1=(~Problem) ),
    leancop_equal(Problem1,Problem2),
    make_matrix(Problem2,Matrix,Settings),
    ( prove2(Matrix,Settings,Proof) ->
      ( Conj\=[] -> Result='Theorem' ; Result='Unsatisfiable' ) ;
      ( Conj\=[] -> Result='Non-Theorem' ; Result='Satisfiable' )
    ),
    output_result(File,Matrix,Proof,Result,Conj).

% print status and connection proof

output_result(File,Matrix,Proof,Result,Conj) :-
    ( Conj\=[] -> Art=' is a ' ; Art=' is ' ), nl,
    print(File), print(Art), print(Result), nl,
    Proof=[] -> true ; ( Result='Theorem' -> Out=' for ' ;
      Result='Unsatisfiable' -> Out=' for negated ' ; true ),
    ( var(Out) -> true ; print('Start of proof'), print(Out),
      print(File), nl, leancop_proof(Matrix,Proof),
      print('End of proof'), print(Out), print(File), nl ).
