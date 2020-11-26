
:- use_module('$REGULUS/PrologLib/batchrec_tools').

% do_batchrec(+Wavfiles, +Transcriptions, +RecParams, +BatchrecTraceFile, +PrologFile, +TmpDirectory)

run_batchrec :-
	do_batchrec('$REGULUS/Examples/Toy1/corpora/wavfiles.txt',
		    '$REGULUS/Examples/Toy1/corpora/transcriptions.txt',
		    [package='$REGULUS/Examples/Toy1/Generated/recogniser',
		     grammar='.MAIN'],
		    '$REGULUS/Examples/Toy1/Generated/trace.txt',
		    '$REGULUS/Examples/Toy1/Generated/trace.pl',
		    '$REGULUS/Examples/Toy1/Generated').


	    