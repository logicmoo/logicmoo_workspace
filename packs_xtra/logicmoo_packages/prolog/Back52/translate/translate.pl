%% (c) 1992, 1993 by KIT-BACK, TU Berlin. All rights reserved.

:- ['~kit/back/back5/translate/b5tf'].


v42v5(SourceFileName,TargetFileName) :-
	translate_file(SourceFileName,TargetFileName,
                            '~kit/back/back5/translate/v42v5').

mb2v5(SourceFileName,TargetFileName) :-
	translate_file(SourceFileName,FileName,
                            '~kit/back/back5/translate/mb2v5_1'),
	translate_file(FileName,TargetFileName,
                            '~kit/back/back5/translate/mb2v5_2').


:- nl,
   write(' *** You can translate a file from BackV4 syntax into BackV5 syntax with:'),
   nl,
   write(' *** v42v5( V4-filename , V5(new)-filename ).'),
   nl,nl.