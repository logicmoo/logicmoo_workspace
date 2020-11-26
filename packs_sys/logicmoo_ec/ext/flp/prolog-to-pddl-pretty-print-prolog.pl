:- ensure_loaded('util').
:- consult('/home/andrewdo/lib/swipl/pack/wam_common_lisp/prolog/wam_cl/printers').

prolog_to_verb(Prolog,Verb) :-
	convert_prolog_to_f_print_input(Prolog,FPrintInput),
	view([fPrintInput,FPrintInput]),
	with_output_to(atom(TmpVerb),f_print(FPrintInput,_)),
	downcase_atom(TmpVerb,Verb).

convert_prolog_to_f_print_input(Prolog,FPrintInput) :-
	(   is_list(Prolog) ->
	    (	Prolog = [P|R],
		findall(O,(member(I,R),convert_prolog_to_f_print_input(I,O)),Os),
		FPrintInput = [P|Os]) ;
	    (	atomic(Prolog) ->
		FPrintInput = Prolog ;
		(   var(Prolog) ->
		    FPrintInput = Prolog ;
		    (	compound_name_arguments(Prolog,P,R),
			findall(O,(member(I,R),convert_prolog_to_f_print_input(I,O)),Os),
			FPrintInput = [P|Os])))).



test_prolog_to_verb :-
	prolog_to_verb(define(problem(flp1),':domain'(flp),[':objects',driving,walking,-,modeOfTransportation,bluetoothKeyboard,tissues,-,object,andrewDougherty,meredithMcGhan,-,person,auroraIllinois,flintMichigan,-,physicalLocation,townhomeOfEleanorAndAndrewAndMeredith,-,residence,bluetoothKeyboard,-,tool,andrewDoughertysHypotheticalCar,meredithMcGhansCar,-,vehicle],[':init',autonomous(andrewDougherty),autonomous(meredithMcGhan),location(andrewDougherty,townhomeOfEleanorAndAndrewAndMeredith),location(andrewDoughertysHypotheticalCar,auroraIllinois),location(bluetoothKeyboard,townhomeOfEleanorAndAndrewAndMeredith),location(meredithMcGhan,flintMichigan),location(meredithMcGhansCar,flintMichigan),mobile(andrewDoughertysHypotheticalCar),mobile(bluetoothKeyboard),mobile(meredithMcGhansCar),'travel-path'(driving,auroraIllinois,townhomeOfEleanorAndAndrewAndMeredith),'travel-path'(driving,flintMichigan,auroraIllinois),'travel-distance'(driving,auroraIllinois,townhomeOfEleanorAndAndrewAndMeredith)='5','travel-distance'(driving,flintMichigan,auroraIllinois)='500','travel-duration'(driving,auroraIllinois,townhomeOfEleanorAndAndrewAndMeredith)='0.15','travel-duration'(driving,flintMichigan,auroraIllinois)='7'],[':goal',[and,'directly-holding'(andrewDougherty,bluetoothKeyboard)]],':metric'(minimize,'total-time'())),Verb),
	view([output,Verb]).
