:-module(output,[init/0,output/0,quit/0,shutup/0]).
:-use_module(trindikit(tkit_oaa)).
:-use_module(trindikit(tkit_tis_access)).
:-use_module(library(system)).

init.
quit.

	

output:-
	write('output called\n'),
	check_condition(X = $output),
	write(X),
	name(A,X),
	check_condition(Y = $output_gui),
	name(B,Y),
	solve(outputGuiTxt(A),[reply(none)]),
	solve(outputGui(B),[reply(none)]),
	%atom_chars(Text,X),
	
%	atom_concat('tts-it -text \"',Text,Halv),
%	atom_concat(Halv,'\"',Hel),
	
	%atom_concat('./TTS-It.exe -text \"',Text,Halv),
	%atom_concat(Halv,'\"',Hel),
	
	%system:exec(Hel,[std,std,std],Pid),
	%solve(setTTSPid(Pid)),
	%system:wait(Pid,_),

	
	%no barge in
%	system:system(Hel),

	%external agent
	solve(tts_output(A)),
	
	apply_update( set( latest_speaker, sys ) ),
	% next_moves moved to latest_moves
	
	check_condition( $next_moves = set(MoveList) or
		         $next_moves = queue(MoveList) or
		         $next_moves = oqueue(MoveList) ),
	
	apply_update( clear( next_moves ) ),
	
	%type( latest_moves, LMType ),
	
	%MoveStruct =.. [LMType,MoveList],
	%apply_update( set( latest_moves, MoveStruct ) ),
	apply_update( set(latest_moves,oqueue(MoveList))),
	write('output succeeded\n'),!.

output:- write('output failed\n').
	

prepare_for_stupid_dos_encoding("","").

prepare_for_stupid_dos_encoding([229|Sicstus],[213|Dos]):-!,
	prepare_for_stupid_dos_encoding(Sicstus,Dos).
prepare_for_stupid_dos_encoding([228|Sicstus],[245|Dos]):-!,
	prepare_for_stupid_dos_encoding(Sicstus,Dos).
prepare_for_stupid_dos_encoding([246|Sicstus],[247|Dos]):-!,
	prepare_for_stupid_dos_encoding(Sicstus,Dos).

prepare_for_stupid_dos_encoding([197|Sicstus],[213|Dos]):-!,
	prepare_for_stupid_dos_encoding(Sicstus,Dos).
prepare_for_stupid_dos_encoding([196|Sicstus],[241|Dos]):-!,
	prepare_for_stupid_dos_encoding(Sicstus,Dos).
prepare_for_stupid_dos_encoding([214|Sicstus],[247|Dos]):-!,
	prepare_for_stupid_dos_encoding(Sicstus,Dos).

prepare_for_stupid_dos_encoding([C|Sicstus],[C|Dos]):-
	prepare_for_stupid_dos_encoding(Sicstus,Dos).