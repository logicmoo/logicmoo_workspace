:- module(simplify_dialogue_move,
	[simplify_dialogue_move/2]
    ).

%======================================================================

:- use_module('$REGULUS/Examples/Calendar/Prolog/dialogue_utils').

:- use_module(library(lists)).
:- use_module('$REGULUS/PrologLib/utilities').

%======================================================================

simplify_dialogue_move(MoveIn, MoveOut) :-
	make_dialogue_move_canonical(MoveIn, MoveNext1),
	simplify_dialogue_move1(MoveNext1, MoveNext2),
	make_dialogue_move_canonical(MoveNext2, MoveOut),
	!.
simplify_dialogue_move(MoveIn, MoveIn).

%======================================================================

simplify_dialogue_move1([F | R], Out) :-
	merge_element_in_list(R, F, Merged, Rest),
	!,
	simplify_dialogue_move1([Merged | Rest], Out).
simplify_dialogue_move1([F | R], Out) :-
	simplify_dialogue_move1(R, R1),
	!,
	(   merge_element_in_list(R1, F, Merged, Rest) ->
	    simplify_dialogue_move1([Merged | Rest], Out)
	;
	    Out = [F | R1]
	).	    
	
simplify_dialogue_move1(Other, Other).

merge_element_in_list([F | R], EltToMerge, Merged, R) :-
	merge_element(EltToMerge, F, Merged),
	!.
merge_element_in_list([F | R], EltToMerge, Merged, [F | R1]) :-
	merge_element_in_list(R, EltToMerge, Merged, R1),
	!.

%======================================================================

% merge_element(Elt1, Elt2, Merged),

merge_element(in_interval=interval(datime(generic, generic, generic, H1, M1, S1),
				   datime(generic, generic, generic, H2, M2, S2)),
	      on_date=datime(Y, Mo, D, 0, 0, 0),
	      in_interval=interval(datime(Y, Mo, D, H1, M1, S1),
				   datime(Y, Mo, D, H2, M2, S2))
	     ).
merge_element(in_interval=interval(datime(generic, generic, generic, H11, M11, S11),
				   datime(generic, generic, generic, H12, M12, S12)),
	      in_interval=interval(datime(Y, Mo, D, H21, M21, S21),
				   datime(Y, Mo, D, H22, M22, S22)),
	      in_interval=interval(Start, End)) :-
	number(Y), number(Mo), number(D),
	merge_element(in_interval=interval(datime(Y, Mo, D, H11, M11, S11),
					   datime(Y, Mo, D, H12, M12, S12)),
	      in_interval=interval(datime(Y, Mo, D, H21, M21, S21),
				   datime(Y, Mo, D, H22, M22, S22)),
	      in_interval=interval(Start, End)).
merge_element(in_interval=interval(datime(Y, Mo, D, H21, M21, S21),
				   datime(Y, Mo, D, H22, M22, S22)),
	      in_interval=interval(datime(generic, generic, generic, H11, M11, S11),
				   datime(generic, generic, generic, H12, M12, S12)),
	      in_interval=interval(Start, End)) :-
	number(Y), number(Mo), number(D),
	merge_element(in_interval=interval(datime(Y, Mo, D, H11, M11, S11),
					   datime(Y, Mo, D, H12, M12, S12)),
		      in_interval=interval(datime(Y, Mo, D, H21, M21, S21),
					   datime(Y, Mo, D, H22, M22, S22)),
		      in_interval=interval(Start, End)).
merge_element(in_interval=interval(Start1, End1),
	      in_interval=interval(Start2, End2),
	      in_interval=interval(Start, End)) :-
	real_datime(Start1),
	real_datime(End1),
	real_datime(Start2),
	real_datime(End2),
	
	\+ earlier_datime(End1, Start2),
	\+ earlier_datime(End2, Start1),
	
	(   earlier_datime(Start1, Start2) ->
	    Start = Start2
	;
	    Start = Start1
	),
	(   earlier_datime(End1, End2) ->
	    End = End1
	;
	    End = End2
	).
merge_element(in_interval=interval(Start1, End1),
	      on_date=datime(Y, M, D, 0, 0, 0),
	      Result) :-
	merge_element(in_interval=interval(Start1, End1),
		      in_interval=interval(datime(Y, M, D, 0, 0, 0),
					   datime(Y, M, D, 23, 59, 59)),
		      Result).

		     

