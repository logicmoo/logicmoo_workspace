
/*

Toy example illustrating dynamic lexicon capabilities in Regulus.

Top-level predicate:

toy1_app(Port, RecognitionPackage, GrammarFile, DynamicLexAssociationsFile)

- Port is the port the app uses to talk to the Regserver

- RecognitionPackage is the recognition package to use when starting the Regserver.
  This should be a dummy JIT-enabled top-level package.

- GrammarFile is the dynamic GSL file created by Regulus. It must be in the sister directory Nuance.

- DynamicLexAssociationsFile is the dynamic lexicon associations file created
  by Regulus when compiling a dynamic Regulus grammar.

Sample call:

toy1_app(1975,
	 '$REGULUS/Examples/Toy1SpecialisedDynamic/Generated/placeholder_jit_recogniser',
	 'recogniser.grammar',
	 '$REGULUS/Examples/Toy1SpecialisedDynamic/Generated/toy1_dynamic_lex_associations.pl'). 

The app starts up and goes into a loop. At each iteration, the user can type one
of the following:

- [Return] Perform recognition and print result.

- NEW NAME <Name>  e.g.
  NEW NAME beverley
  NEW NAME howard the duck
  Add the new name to the vocabulary, using the lexicon macro system_name_phrase.
  The effect should be as if the lexicon entry

  @person_name(Name)

  had been included in the original grammar.

- EXIT Exit loop and quit.

*/

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%======================================================================

:- use_module('$REGULUS/Prolog/dynamic_lexicon_runtime').

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').

%======================================================================

toy1_app :-
	toy1_app(1975,
		 '$REGULUS/Examples/Toy1SpecialisedDynamic/Generated/placeholder_jit_recogniser',
		 'recogniser.grammar',
		 '$REGULUS/Examples/Toy1SpecialisedDynamic/Generated/toy1_dynamic_lex_associations.pl'
		 ).

toy1_app(Port, RecognitionPackage, GrammarFile, DynamicLexAssociationsFile) :-
	% Create the file reference to use for the recognition call
	format_to_atom('<file:../Generated/~w#MAIN>', [GrammarFile], FileReference),
	initialise(Port,
		   RecognitionPackage,
		   DynamicLexAssociationsFile,
		   'audio.OutputVolume=200'),
	loop(FileReference).
		       
%======================================================================

initialise(Port, Package, DynamicLexAssociationsFile, NuanceParams) :-
	% Initialise the dynamic lexicon database, using DynamicLexAssociationsFile
	init_dynamic_lexicon_runtime(DynamicLexAssociationsFile),

	% Start Regserver using the dummy top-level package
	absolute_file_name(Package, AbsPackage),
	regulus_sockettalk_debug,
	regulus_sockettalk_init(Port, AbsPackage, NuanceParams),
	!.

%======================================================================

loop(FileReference) :-
	format('~N[RETURN] TO RECOGNISE, "NEW NAME <name>" TO ADD NAME, "EXIT" TO END >> ', []),
	read_line(Line),
	split_string_into_words(Line, Words),
	(   Words = ['EXIT'] ->
	    regulus_sockettalk_exit_server
	;
	    Words = ['NEW', 'NAME' | Name] ->
	    add_name(Name),
	    !,
	    loop(FileReference)
	;	    
	    otherwise ->
	    % Call recognition using the file reference.
	    regulus_sockettalk_recognise(FileReference, Recognised),
	    format('~N~nRecognised: ~w~n~n', [Recognised]),
	    !,
	    loop(FileReference)
	).

%======================================================================

add_name(NameList) :-
	% Check that NameList is a list 
	(   is_list(NameList) ->
	    true
	;
	    format('~N*** Error: arg to add_name must be a list~n', []),
	    fail
	),
	% Turn it into a comma-list. E.g. change [howard, the, duck] into (howard, the, duck)
	list_to_comma_list(NameList, Name),
	% Turn it into a semantic constant. E.g. change [howard, the, duck] into howard_the_duck
	join_with_underscore(NameList, NameConst),
	% Make the macro-call that will be the virtual lexicon entry, e.g.
	%
	%   @person_name((howard, the, duck), howard_the_duck)
	%
	% We use person_name for concreteness, but any other macro defined
	% in the DynamicLexAssociationsFile would work.
	MacroCall = @person_name(Name, NameConst),

	% Call assert_dynamic_lex_entry to add the entry.
	format('~N~n--- Adding name entry for: ~w ', [Name]),
	(   assert_dynamic_lex_entry(MacroCall) ->
	    format('~w~n', ['--- OK'])
	;
	    otherwise ->
	    format('~w~n', ['--- Error'])
	),
	!.
add_name(NameList) :-
	format('~N*** Error: bad call: ~w~n', [add_name(NameList)]),
	fail.






	