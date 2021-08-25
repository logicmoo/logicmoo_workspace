% ===================================================================
% PURPOSE
% This File is the bootstrap for the Sigma Infence engine For SWI-Prolog
% So first is loads the proper files and then starts up the system
% ===================================================================

:-use_module(library(system)).
:-use_module(library(shlib)).
:-use_module(library(listing)).
:-use_module(library(sgml)).
:-use_module(library(rdf)).
:- use_module(library(socket)).  
 %Load the TCP Library
%:- use_module(library(unix)).      
:- use_module(library(shell)).      
:- use_module(library(shlib)).        
:- use_module(library(url)).        
:- use_module(library(quintus)).        
:- use_module(library(qsave)).        
    

%:- use_module(library(backcomp)).        


%:-make.
	
:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

:-include('sigma_header.pl').

recompile_all :- !.



multi_module(X):-
	dynamic(X),
	multifile(X),
	module_transparent(X).

register_sigma_file_type(Filename,Type) :-
	(sigma_file_type(Filename,Type) ->
		true;
		assertz(sigma_file_type(Filename,Type))),!.

writeModeSet(Mode):-
	(unsetSigmaOption(client=_)),
	(setSigmaOption(client=Mode)),!.

:-ensure_loaded('sigma_utility.pl').

:-absolute_file_name(.,Local),assert('LOGIC_ENGINE_RT'(Local)),assert(library_directory(Local)).

:-absolute_file_name('../..',X),assert('ROOT_RT'(X)).

% numbervars/1 (just simpler numbervars.. will use a rand9ome start point so if a partially numbered getPrologVars wont get dup getPrologVars

numbervars(X):-get_time(T),convert_time(T,A,B,C,D,E,F,G),!,numbervars(X,'$VAR',G,_).

once(X,Y):-once(X),once(Y).

:- dynamic sigmaCache/1.
:- dynamic sigmaCache/6.
:- dynamic sigmaCache/5.
:- dynamic sigmaCache/9.
	


moduleEnsureLoaded(X):-
	catch(ensure_loaded(X),_,(catch((atom_concat('mod/',X,Y),
	ensure_loaded(Y)),_,format(user_error,';; file find error ~q ~q',[X,E])))).
	


:- set_prolog_flag(optimise,false).
:- set_prolog_flag(file_name_variables,false).
:- set_prolog_flag(agc_margin,0).
:- set_prolog_flag(trace_gc,false).
:-set_prolog_flag(character_escapes,true).
%:-set_prolog_flag(double_quotes,string).
:-set_prolog_flag(double_quotes,codes).
:-set_prolog_flag(report_error,true).
:-set_prolog_flag(verbose,normal).
:-set_prolog_flag(unknown,error).



callIfPlatformWin32(G):-prolog_flag(windows,true),!,ignore(G).
callIfPlatformWin32(G):-!.

callIfPlatformUnix(G):- \+ prolog_flag(windows,true),!,ignore(G).
callIfPlatformUnix(G):-!.

/*
:- callIfPlatformWin32(set_prolog_flag(debug_on_error,true)).
:- callIfPlatformUnix(set_prolog_flag(debug_on_error,false)).
:- callIfPlatformUnix(set_prolog_flag(debug_on_error,true)).
*/

% Using getCputime/1 (in Sigma code) since Eclipse prolog (another port for Sigma)  chokes on getCputime/1
getCputime(Start):-statistics(cputime,Start).

isCharCodelist([]).  isCharCodelist([A|T]):-integer(A),A>9,A<128,isCharCodelist(T).

% Using retractAllProlog/1 (in Sigma code) since Eclipse prolog (another port for Sigma)  chokes on retractall/1
retractAllProlog(X):-retractall(X).

:- current_prolog_flag(arch,'i386-win32') -> 
      ensure_loaded('sigma_win32.pl') ; ensure_loaded('sigma_unix.pl').

% Each prolog has a specific way it could unnumber the result of a numbervars
% TODO find optimal solution

unnumbervars(X,Y):-term_to_atom(X,A),atom_to_term(A,Y,_).


% Prolog specific code choices
if_prolog(swi,G):-call(G).  % Run B-Prolog Specifics
if_prolog(_,_):-!.  % Dont run SWI Specificd or others

% used like if_prolog(bp,do_bp_stuff),if_prolog(swi,do_swi_stuff) inline in Sigma code

% ========================================================================================
% Some prologs have a printf() type predicate.. so I made up fmtString/writeFmt in the Sigma code that calls the per-prolog mechaism
% in SWI it's formzat/N and sformat/N  
% ========================================================================================

writeFmtFlushed(X,Y,Z):-catch((format(X,Y,Z),flush_output(X)),_,true).
writeFmtFlushed(X,Y):-catch((format(X,Y),flush_output),_,true).

writeFmt(X,Y,Z):-catch(format(X,Y,Z),_,true),catch((flush_output(X)),_,true).
writeFmt(X,Y):-format(X,Y),catch(flush_output,_,true).
writeFmt(X):-format(X,[]),catch(flush_output,_,true).
fmtString(X,Y,Z):-sformat(X,Y,Z).
fmtString(Y,Z):-sformat(Y,Z).

% =============================================
% Sets Sigma Defaults (After intialization)
% =============================================

:-assert(show_entails).
:-assert(show_all_proofs).
:-assert(debug_tests).

:-ensure_loaded('sigma_bootstrap.pl').
%:-ensure_loaded('sigma_response.pl').
%:-ensure_loaded('sigma_useragent_query.pl').

% =============================================
% Load the Sigma header and Engine 
% =============================================

prologAtInitalization(V):-at_initialization(V),!,debugOnFailure(V).

:-  setSigmaOptionDefaults.


:-callIfPlatformUnix(prologAtInitalization(cs)).	 	






