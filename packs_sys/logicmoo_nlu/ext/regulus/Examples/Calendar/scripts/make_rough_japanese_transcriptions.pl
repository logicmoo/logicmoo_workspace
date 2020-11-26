
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- use_module('$REGULUS/PrologLib/batchrec_tools').

make_mayuko :-
	format('~N--- Make rough transcriptions file for Mayuko data~n', []),
	directory_to_rough_transcriptions_file('$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/2008-09-23_Mayuko',
					       '$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/mayuko.txt',
					       [package='$REGULUS/Examples/Calendar/Generated/japanese_recogniser',
						grammar='.MAIN',
						'rec.Pruning=1200'],
					       'C:/tmp').

make_ozawa :-
	format('~N--- Make rough transcriptions file for Ozawa data~n', []),
	directory_to_rough_transcriptions_file('$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/2008-09-23_Ozawa',
					       '$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/ozawa.txt',
					       [package='$REGULUS/Examples/Calendar/Generated/japanese_recogniser',
						grammar='.MAIN',
						'rec.Pruning=1200'],
					       'C:/tmp').

make_yuriko :-
	format('~N--- Make rough transcriptions file for Yuriko data~n', []),
        directory_to_rough_transcriptions_file('$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/2008-09-28_Yuriko',
					       '$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/yuriko.txt',
					       [package='$REGULUS/Examples/Calendar/Generated/japanese_recogniser',
						grammar='.MAIN',
						'rec.Pruning=1200'],
					       'C:/tmp').

make_kikuko :-
	format('~N--- Make rough transcriptions file for Kikuko data~n', []),
        directory_to_rough_transcriptions_file('$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/2008-09-29_Kikuko',
					       '$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/kikuko.txt',
					       [package='$REGULUS/Examples/Calendar/Generated/japanese_recogniser',
						grammar='.MAIN',
						'rec.Pruning=1200'],
					       'C:/tmp').

make_miki :-
	format('~N--- Make rough transcriptions file for Miki data~n', []),
        directory_to_rough_transcriptions_file('$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/2008-09-30_Miki',
					       '$REGULUS/Examples/Calendar/corpora/JapDataCollSep2008/miki.txt',
					       [package='$REGULUS/Examples/Calendar/Generated/japanese_recogniser',
						grammar='.MAIN',
						'rec.Pruning=1200'],
					       'C:/tmp').
						
go :-
	make_mayuko,
	make_ozawa,
	make_yuriko,
	make_kikuko,
	make_miki.

:- go.

:- halt.

					