
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- compile('$REGULUS/PrologLib/utilities').

regulus_saved_state_file(AbsSavedStateFile) :-
	SavedStateFile = '$REGULUS/tmp/regulus_saved_state.sav',
	safe_absolute_file_name(SavedStateFile, AbsSavedStateFile).

restore_regulus_saved_state_file :-
	regulus_saved_state_file(AbsSavedStateFile),
	restore(AbsSavedStateFile).

regulus_saved_state_exists_and_is_up_to_date :-
	regulus_saved_state_file(SavedStateFile),
	safe_file_exists(SavedStateFile),
	
	datime_for_file_list([SavedStateFile], latest, DateOfSavedState),
	
	datime_for_directory('$REGULUS/Prolog', latest, pl, LastTimeRegulusPrologFilesTouched),
	datime_for_directory('$REGULUS/PrologLib', latest, pl, LastTimeRegulusPrologLibFilesTouched),

	earlier_datime(LastTimeRegulusPrologFilesTouched, DateOfSavedState),
	earlier_datime(LastTimeRegulusPrologLibFilesTouched, DateOfSavedState).

load_regulus_creating_saved_state :-	
	load_regulus,
	
	regulus_saved_state_file(SavedStateFile),
	create_directory_for_file_if_necessary(SavedStateFile),
	save_program(SavedStateFile).

load_regulus :-
	% Compile utilities
	compile('$REGULUS/PrologLib/utilities'),
	compile('$REGULUS/Prolog/regulus_utilities'),
	% This seems to be necessary in SP 4.2.1 for reasons I don't understand
	[library(codesio)],

	% Compile the main Regulus code
	compile('$REGULUS/Prolog/regulus2nuance'),
	compile('$REGULUS/Prolog/regulus_top'),

	% Compile EBL code
	compile('$REGULUS/Prolog/ebl_make_training_data'),
	compile('$REGULUS/Prolog/ebl_train'),
	compile('$REGULUS/Prolog/ebl_postprocess').	

load_or_restore_regulus_creating_saved_state_if_necessary :-
	(   regulus_saved_state_exists_and_is_up_to_date ->
	    format('~N~n--- REGULUS SAVED STATE APPEARS TO BE UP TO DATE. RESTORING.~n~n', []),
	    restore_regulus_saved_state_file
	;
	    otherwise ->
	    format('~N~n--- CAN\'T FIND UP TO DATE REGULUS SAVED STATE. LOADING FILES AND CREATING IT.~n~n', []),
	    load_regulus_creating_saved_state
	).
