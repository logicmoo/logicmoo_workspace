/*************************************************************************

         name: godis-vcr-nuance
      version: 
  description: application specification file
       author: Staffan Larsson
 
*************************************************************************


/*========================================================================
   Select datatypes

   Speficies a list of datatypes to be loaded. Each item DataType in the
   list corresponds to a file DataType.pl in the search path.
========================================================================*/

selected_datatypes([string, move, atom, integer, real, bool, record,
set, stack, stackset, queue, oqueue, pair, godis_datatypes]).


/*========================================================================
   Select modules

   Each module spec has the form Predicate:FileName, where Predicate is the
   unary predicate used to call the module, and FileName.pl is the a file
   in the search path containing the module specification
========================================================================*/

selected_modules([ input : input_nuance_score_oaa,
		   interpret: interpret_simple,
		   update : update,
		   select : select,
		   generate : generate_simple,
		   output : output_nuance_basic_oaa
		   ]).

 % dme_module/1 - spefifies which modules have unlimited access to TIS

dme_modules([ update, select ]).

/*========================================================================
   Select resources

   Speficies a list of resources to be loaded. Each item
   ResourceFile in the list corresponds to a  a file ResourceFile.pl
   in the search path. The file defines a resource object with the same
   name as the file.
========================================================================*/

selected_resources( [asrg_vcr_english,
		     device_vcr,
		     lexicon_vcr_english,
		     lexicon_vcr_svenska,
		     domain_vcr,
		     oaag
		    ] ).

selected_macro_file( godis_macros ).

/*========================================================================
   operations to execute on TIS reset
========================================================================*/

reset_operations( [ set( program_state, run),
		    set( language, Lang ),
		    set( asr_grammar,$$dash2underscore(asrg-Domain-Lang) ),
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( devices,
			 record([vcr=device_vcr]) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
		    push(/private/agenda,do(top)),
		    push( /shared/actions, top ) ]):-
	flag( language, Lang ),
	flag( domain, Domain ).

/*========================================================================
   Set flags
========================================================================*/

:- setflag(show_rules,yes).
:- setflag(show_state,all).

/*========================================================================
   Run
========================================================================*/

quiet:-
	setflag(show_rules,no),
	setflag(show_state,no).
verb:-
	setflag(show_rules,yes),
	setflag(show_state,all).


run :- run(vcr-english),halt.

% reload update rules
rur :-
	update:ensure_loaded(library(update_rules)).

rsr :-
	select:ensure_loaded(library(selection_rules)).

:- assert(hide_path('UNLIKELYPAHNAME')).


/*

Below are some attempts enable starting all processes from prolog. I donät think it works. SL 0402.

% start OAA
start_command(oaa,'C:/Program/oaa2.2.1/runtime/prolog/fac.exe').

% start Nuance License manager
start_command(nlm,'C:/Program/Nuance/v8.0.0/run-nlm.bat').

% run Nuance
start_command(nuance,'C:/CVS/godis/godis-apps/domain-vcr/setup-nuance.bat').

start_command(recserver, 'start recserver -package C:\CVS\godis\godis-apps\domain-vcr\Resources\asrg_vcr_english lm.Addresses=localhost').

start_command(vocalizer, 'start vocalizer -language USEnglish').

start_command(nuance_asr, 'java -cp C:\CVS\trindikit\dist\classes;C:\Program\Nuance\v8.0.0\java\nsc.jar;C:\Program\Nuance\v8.0.0\java\jsc-source.jar;C:\Program\Nuance\v8.0.0\java\vcomsc.jar;C:\CVS\trindikit\lib\jars\oaa2.jar se.gu.ling.trindikit.oaa.nuance.nsc.OAABasicNuanceSpeechChannel -package C:\CVS\godis\godis-apps\domain-vcr\Resources\asrg_vcr_english lm.Addresses=localhost client.TTSAddresses=localhost:32323').




setup(P):-
	start_command(P,OAA),
	exec(OAA,[std,std,std],PID),
	write('Succeeded setting up '),
	write(P), write(' with PID '), write(PID), nl,
	assert(pid(P,PID)).

setup:-
	setup(oaa),
	setup(nlm).
%	setup(nuance).
%	setup(recserver),
%	read(_),
%	setup(vocalizer),
%	read(_),
%	setup(nuance_asr).
	
kill:-
	kill(oaa),
	kill(nlm).
%	kill(nuance).

kill(P):-
	pid(P,PID),
	on_exception(_,
		     (kill(PID,9), write('killed '), write(P), nl),
		     (write(P),write(' is already dead.'),nl) ),
	retract(pid(P,PID)).
*/
% GODIS spec filename here





