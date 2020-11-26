
:- use_module(library(assoc3)).
:- use_module(library(avl)).

%----------------------------------------------------------

assoc_test :-
	prolog_flag(version, VersionAtom),
	format('~N~w~n~n', [VersionAtom]),
	make_lots_of_big_assocs_timed(20, 500),
	make_lots_of_big_assocs_timed(20, 1000),
	make_lots_of_big_assocs_timed(20, 2000),
	make_lots_of_big_assocs_timed(20, 4000),
	make_lots_of_big_assocs_timed(20, 8000),
	make_lots_of_big_assocs_timed(20, 16000).

make_lots_of_big_assocs_timed(NAssocs, AssocSize) :-
	format('~N~n~d assocs, ~d elements each~n', [NAssocs, AssocSize]),
	statistics(runtime, [StartTime, _]),
	make_lots_of_big_assocs(NAssocs, AssocSize),
	statistics(runtime, [EndTime, _]),
	% Time returned is in milliseconds.
	TimeTaken is ( float(EndTime) - float(StartTime) ) / 1000.0,
	format('~NTime taken: ~1f seconds~n', [TimeTaken]).	

make_lots_of_big_assocs(NAssocs, _AssocSize) :-
	NAssocs < 0,
	!.
make_lots_of_big_assocs(NAssocs, AssocSize) :-
	NAssocs >= 0,
	make_big_assoc(AssocSize, _Assoc),
	(   0 is NAssocs mod 2 ->
	    format('~d ', [NAssocs]),
	    flush_output(user)
	;
	    true
	),
	NAssocs1 is NAssocs - 1,
	!,
	make_lots_of_big_assocs(NAssocs1, AssocSize).

make_big_assoc(AssocSize, Assoc) :-
	empty_assoc(StartAssoc),
	fill_big_assoc(AssocSize, StartAssoc-Assoc),
	access_all_big_assoc(AssocSize, Assoc),
	assoc_to_list(Assoc, _List).

fill_big_assoc(I, AssocIn-AssocIn) :-
	I < 0,
	!.
fill_big_assoc(I, AssocIn-AssocOut) :-
	I >= 0,
	put_assoc(I, AssocIn, I, AssocNext),
	I1 is I - 1,
	!,
	fill_big_assoc(I1, AssocNext-AssocOut).

access_all_big_assoc(I, _Assoc) :-
	I < 0,
	!.
access_all_big_assoc(I, Assoc) :-
	I >= 0,
	( get_assoc(I, Assoc, _Val) ; true ),
	I1 is I - 1,
	!,
	access_all_big_assoc(I1, Assoc).

%----------------------------------------------------------

avl_test :-
	prolog_flag(version, VersionAtom),
	format('~N~w~n~n', [VersionAtom]),
	make_lots_of_big_avls_timed(20, 500),
	make_lots_of_big_avls_timed(20, 1000),
	make_lots_of_big_avls_timed(20, 2000),
	make_lots_of_big_avls_timed(20, 4000),
	make_lots_of_big_avls_timed(20, 8000),
	make_lots_of_big_avls_timed(20, 16000).

make_lots_of_big_avls_timed(NAvls, AvlSize) :-
	format('~N~n~d AVLs, ~d elements each~n', [NAvls, AvlSize]),
	statistics(runtime, [StartTime, _]),
	make_lots_of_big_avls(NAvls, AvlSize),
	statistics(runtime, [EndTime, _]),
	% Time returned is in milliseconds.
	TimeTaken is ( float(EndTime) - float(StartTime) ) / 1000.0,
	format('~NTime taken: ~1f seconds~n', [TimeTaken]).	

make_lots_of_big_avls(NAvls, _AvlSize) :-
	NAvls =< 0,
	!.
make_lots_of_big_avls(NAvls, AvlSize) :-
	NAvls > 0,
	make_big_avl(AvlSize, _Avl),
	(   0 is NAvls mod 2 ->
	    format('~d ', [NAvls]),
	    flush_output(user)
	;
	    true
	),
	NAvls1 is NAvls - 1,
	!,
	make_lots_of_big_avls(NAvls1, AvlSize).

make_big_avl(AvlSize, Avl) :-
	empty_avl(StartAvl),
	fill_big_avl(AvlSize, StartAvl-Avl),
	access_all_big_avl(AvlSize, Avl),
	avl_to_list(Avl, _List).

fill_big_avl(I, AvlIn-AvlIn) :-
	I =< 0,
	!.
fill_big_avl(I, AvlIn-AvlOut) :-
	I > 0,
	avl_store(I, AvlIn, I, AvlNext),
	I1 is I - 1,
	!,
	fill_big_avl(I1, AvlNext-AvlOut).

access_all_big_avl(I, _Avl) :-
	I < 0,
	!.
access_all_big_avl(I, Avl) :-
	I >= 0,
	( avl_member(I, Avl, _Val) ; true ),
	I1 is I - 1,
	!,
	access_all_big_avl(I1, Avl).

	
