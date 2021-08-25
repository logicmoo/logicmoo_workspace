/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                            UPV-Curry Interpreter

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

:-module(curry,[process_curry/1]).


% =========================================================================
%  Add this directory and the pack files (also Logicmoo Library Utils)
% =========================================================================
:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir), DirFor = upv_curry,
   absolute_file_name('../../..',Y,[relative_to(Dir),file_type(directory)]),
   (user:file_search_path(DirFor,Dir);asserta(user:file_search_path(DirFor,Dir))) ->
   (user:file_search_path(pack,Y);asserta(user:file_search_path(pack,Y))) -> attach_packs.
:- initialization(attach_packs).
:- user:ensure_loaded(library(logicmoo_utils)).
% =========================================================================
:- expects_dialect(sicstus).
:- thread_local(current_prolog_flag_double_quotes/1).
:- current_prolog_flag(double_quotes, ResetTo),asserta(current_prolog_flag_double_quotes(ResetTo)).
:- set_prolog_flag(double_quotes, codes).

is_cdl(A,A):-var(A),!.
is_cdl(A,B):- sicstus_atom_chars(A,B),!. 
is_cdl(A,B):-string(A),!,show_call(string_codes(A,B)).
is_cdl(A,A):-must(is_list(A)),!.

sappend(A0,B0,C0):- must(is_cdl(A0,A)),must(is_cdl(B0,B)),must(is_cdl(C0,C)),append(A,B,C).
sicstus_atom_chars(N,S):-name(N,S).


% ---------------------------------------------------------------------------
% Global Control -----------------------------------------------------
% ---------------------------------------------------------------------------

:- use_module(library(system)), use_module(library(lists)).

sicstus_compile:- user:compile(['common.pl','parser.pl','typecheck.pl','gendtree.pl',
            'write2.pl','eval.pl','menu.pl']).


:-include('common.pl').
:-include('parser.pl').
:-include('typecheck.pl').
:-include('gendtree.pl').
:-include('gendtree.pl').
:-include('write2.pl').
:-include('eval.pl').
:-include('menu.pl').

:- dynamic 
     prelude/3. %Parser,TypeChecker,DefTree

%---Save all information about loaded program in the functor "prelude"
save_state :-
     % Info from Parser
        findall(type(X,Y),type(X,Y),               TypesParser),
        findall(function(X,Y),function(X,Y),       FuncsParser),
        findall(constructor(X,Y),constructor(X,Y), ConsParser),
        findall(infix(X,Y,Z),infix(X,Y,Z),         InfixParser),
        findall(builtin(X),builtin(X),             NoRedefParser),

     % Info from Type Checker
        findall(annotation(X,Y),annotation(X,Y),       AnnCheckType),
        findall(function(X,Y,Z),function(X,Y,Z),       FuncsCheckType),
        findall(constructor(X,Y,Z),constructor(X,Y,Z), ConsCheckType),

     % Info from Def Trees
        findall(dt(X,Y,Z),dt(X,Y,Z),                 DTDefTree),

      retractall(prelude(_,_,_)),
      assert(
       prelude(
        [TypesParser,FuncsParser,ConsParser,InfixParser,NoRedefParser],
        [AnnCheckType,FuncsCheckType,ConsCheckType],
        [DTDefTree]
      )).

%---Retrieve information about prelude for parser 
% (called in "program")
prelude_parser :-
      prelude(InfoParser,_,_),
      assertAll(InfoParser).

% Retrieve information about prelude for type checker 
% (called in "checkType")
prelude_typechecker :-
      prelude(_,InfoTypeChecker,_),
      assertAll(InfoTypeChecker).

% Retrieve information about prelude for deftree generation 
% (called in "deftrees")
prelude_deftree :-
      prelude(_,_,InfoDefTree),
      assertAll(InfoDefTree).

assertAll([]).
assertAll([X|Xs]) :- assertAll1(X),assertAll(Xs).

assertAll1([]).
assertAll1([X|Xs]) :- assert(X),assertAll1(Xs).

%---Load Prelude File
load_prelude :-
   write('Loading prelude...'),nl,
   name(':load "prelude"',Command), %--needed to allow "
   on_exception(_,
     ( must(command_read(Command)),! ;halt ),
     (write('Internal error processing prelude file'),nl,halt(-1))),
   nl.

:- use_module(library(check)).
% :- check:list_strings.

curry_toplevel:- 
  must_det_l((
   nl,nl,
   write('***********UPV-Curry interpreter (Version 14 Apr 2000)*******************'),
   nl,nl,
   % already done the curry_init,
   % Help
   command_help(":help "),   
   main)).

curry_init:-
  must_det_l((
   % Save state with basic info
   save_state,
   % Load Prelude
   load_prelude,
   % Save state with prelude info
   save_state)).

:- curry_init.

:- if_startup_script(curry_toplevel).

process_curry(Input):-
  must_det_l((
    any_to_string(Input,InputS),
    string_codes(InputS,InputS1))),
    process_then(InputS1,true),!.

:-must_det_l((retract(current_prolog_flag_double_quotes(ResetTo)),
  set_prolog_flag(double_quotes, ResetTo))).
