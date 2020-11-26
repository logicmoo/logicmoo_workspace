
:- ['$REGULUS/Examples/Toy1SpecialisedDynamic/Prolog/toy1_app.pl'].

:- toy1_app(1975,
	    '$REGULUS/Examples/Toy1SpecialisedDynamic/Generated/placeholder_jit_recogniser',
	    'recogniser.grammar',
	    '$REGULUS/Examples/Toy1SpecialisedDynamic/Generated/toy1_dynamic_lex_associations.pl'
	    ).

:- halt.	

