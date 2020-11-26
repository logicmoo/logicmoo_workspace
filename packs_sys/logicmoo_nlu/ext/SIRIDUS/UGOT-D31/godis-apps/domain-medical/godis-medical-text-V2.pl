/*************************************************************************

         name: godis-medical-text V2
      version: 
  description: Specification file for GoDiS-AOD medical application
       author: Staffan Larsson
 
*************************************************************************/

% Make sure you have set the GODIS environment variable!
:-ensure_loaded('$GODIS/prolog/godis/general/search_paths').
% Load starter routines; don't change
:- use_module( library(starter) ).


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

selected_modules([ input : input_textscore,
		   %input:input_simpletext,
		   interpret: interpret_simple,
		   %interpret: interpret_empty,
		   update : update,
		   select : select,
		   generate : generate_simple,
		   %generate : generate_empty,
		   output:output_simpletext
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

selected_resources( [
		     device_medical,
		     lexicon_medical_english_V2,
		     domain_medical_V2
		    ] ).

selected_macro_file( godis_macros ).

batch_files([]).

/*========================================================================
   operations to execute on TIS reset
========================================================================*/

reset_operations( [ set( program_state, run),
		    set( language, Lang ),
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( devices,
			 record([medical=device_medical]) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
		    score := 1.0 % default
		  ]):-
	flag( language, Lang ),
	flag( domain, Domain ).

/*========================================================================
   Run
========================================================================*/

quiet:-
	setflag(show_rules,no),
	setflag(show_state,no).
verb:-
	setflag(show_rules,yes),
	setflag(show_state,all).

run( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	start_trindikit.


run :- run(medical-english).

% reload update rules
rur :-
	update:ensure_loaded(library(update_rules)).

rsr :-
	select:ensure_loaded(library(selection_rules)).

:- assert(hide_path('UNLIKELYPAHNAME')).


/*========================================================================
   Search paths
========================================================================*/



% Select appropriate godis library/libraries
:- assertz(user:library_directory(godis('godis-aod'))).

% Set the application home path
:- assertz(user:file_search_path(home,'$GODIS/../godis-apps/domain-medical')).
:- assertz(user:library_directory(home(''))).
% Add any subdirectories to the application home directory below
:- assertz(user:library_directory(home('Resources'))).
%:- assertz(user:library_directory(home(asrg_vcr_english))).


/*========================================================================
   Initialization
========================================================================*/

:- use_module(library(system)).


% GODIS spec filename here
:- init( 'godis-medical-text-V2' ).

% BEFORE RUNNING, START THE JAVA MEDICAL APPLICATION
% cd C:\CVS\godis\godis-apps\domain-medical\Database
% java OAAWorkshopDatabase diseasedb.ser