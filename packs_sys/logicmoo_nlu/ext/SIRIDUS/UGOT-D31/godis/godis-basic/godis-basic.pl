
/*************************************************************************

         name: godis-basic
  description: godis-basic specification file
 
*************************************************************************/

:- ensure_loaded( search_paths ).

/*========================================================================
   Select datatypes

   Speficies a list of datatypes to be loaded. Each item DataType in the
   list corresponds to a file DataType.pl in the search path.
========================================================================*/

selected_datatypes([string, move, atom, integer, bool, record, set, stack,
stackset, assocset, godis_datatypes]).


/*========================================================================
   Select modules

   Each module spec has the form Predicate:FileName, where Predicate is the
   unary predicate used to call the module, and FileName.pl is the a file
   in the search path containing the module specification
========================================================================*/

selected_modules([ input : input_simpletext,
		   interpret : interpret_simple,
		   update : update,
		   select : select,
		   %generate : generate_empty,
		   generate : generate_simple,
		   output : output_simpletext
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
		     database_travel,
		     lexicon_travel_english,
		     domain_travel
		    ] ).

/*========================================================================
   operations to execute on TIS reset
========================================================================*/

reset_operations( [ set( program_state, run),
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( database, $$dash2underscore(database-Domain) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
%		    ! ($domain :: plan( Q, Plan )), % single issue
%		    /private/plan := Plan,
%		    push(/shared/qud, Q), 
		    push(/private/agenda,greet)]):-
	flag( language, Lang ),
	flag( domain, Domain ).

batch_files( [ '~sl/GoDiS/Batch/testdialog3.txt']).