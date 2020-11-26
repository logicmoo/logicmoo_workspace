/*************************************************************************

         name: semsort_player.pl 
	 date: 2004-10-25
       author: Andreas Wallentin
 
*************************************************************************/
:-multifile sem_sort/2.

:- ensure_loaded( stations_player ).
:- use_module( dbase, [song_list/1,
		       group_in_db/1,
		       album_in_db/1,
		       station_in_db/1] ).

:- ensure_loaded( [albums,songs,groups,digits_svenska_player,digits_english_player] ).
:- use_module(library(lists),[append/3,
			      member/2,
			      is_list/1]).

/*----------------------------------------------------------------------
    sort_restr( +Prop )

    Prop fulfils the sortal restrictions on propositions
----------------------------------------------------------------------*/

sem_sort( english, language ).
sem_sort( svenska, language ).

sem_sort( player, domain ).

% sem_sort(Name,name):-
% 	name(Name).

%%%   ACTIONS
sem_sort( restart,           action ).
sem_sort( handle_player,     action ).
sem_sort( handle_playlist,   action ).
sem_sort( handle_stations,   action ).

sem_sort( start,             action ).
sem_sort( start_specific,    action ).
sem_sort( stop,              action ).
sem_sort( pause,             action ).
sem_sort( resume,            action ).
%sem_sort( fast_rewind,       action ).
%sem_sort( play_playlist,     action ).
sem_sort( fast_forward,      action ).
sem_sort( rewind,            action ).

sem_sort( playlist_add,      action ).
sem_sort( playlist_del,      action ).
%sem_sort( playlist_del_specific,  action ).
%sem_sort( playlist_shuffle,  action ).
sem_sort( remove,            action ).
sem_sort( listen_to,         action ).

sem_sort( vol_up,            action ).
sem_sort( vol_down,          action ).
sem_sort( next_song,         action ).
sem_sort( previous_song,     action ).

sem_sort( show_list,         action ).
%
%%%
%%%%% använd dbase för att kolla album, grupper osv...
%%%
%

% sem_sort( radio, jpp ).
% sem_sort( music, jpp ).

%sem_sort( ToPlay, toPlay ):-
%	approved_item(ToPlay).

sem_sort( ToPlay, index ):-
	(
	  approved_item(ToPlay)
	;
	  fix_item(ToPlay,_)
	)
	;
	(
	  approved_item_eng(ToPlay)
	;
	  fix_item_eng(ToPlay,_)
	).


sem_sort( Station, station ):-
	validIP(Station),
	station_in_db(Station).
sem_sort( Station, station ):-
	is_list(Station),
	station_in_db(Station).

sem_sort( ListName, playlist ):-
	correct_playlist_end(ListName).

% [stalker]
sem_sort( X, item ):-
	song2(X).

%sem_sort( X, itemRem ):-
%	integer(X).

sem_sort( X, year ):-
	X = tre
	; X = fyra
	; X = fem
	; X = not_known.

sem_sort( Group, group ):-
	(
	  group(Group)
	;
	  group_atom(Group)
	).
%	( group_in_db(Group)
%	; group_atom(Group)
%	).

sem_sort( Album, album ):-
	( album_in_db(Album)
	; album_atom(Album)
	).

sem_sort( X, song ):-
	( X = [X1,Y],
	    atomic(X1),
	    atomic(Y)
	;
	    song_atom(X)
	).

%sem_sort( X, song ):-
%	song_atom(X).


%%% bara en liten koll mot alla låtar i DB
%%% skall egentligen vara en "db" med alla
%%% möjliga låtar som finns
song2( X ):-
	song_list(SongList),
	song_list2(SL2),
	(
	  member(X,SongList)
	;
	  member(X-_Art,SL2)
	).


/*--------------------
conceptual hierarichy
--------------------*/

%isa(station,item).
isa(itemAdd,item).
%isa(itemRem,item).
isa(song,item).

isa(itemRem,index).
isa(what_to_play,index).

%isa(solo,artist).
isa(artist,group).
isa(song_artist,group).

%isa(artist,artist).
isa(groupToAdd,group).
isa(composer,group).

isa(genreOfArtist, genre).
isa(genreOfMusic, genre).


isa( T0, T2 ):-
	T0 \= T2,
	isa( T0, T1 ),
	isa( T1, T2 ).


%validIP('192.168.0.171').
%   or
%validIP('127.16.60.17:1234').
%number_chars(Number,NumberCharList)
validIP(IPAtom):-
	atomic(IPAtom),
	name(IPAtom,ListOfNumCodes),
	splitList(ListOfNumCodes,[N1,N2,N3,N4]),
	integer(N1),
	integer(N2),
	integer(N3),
	integer(N4).

validIP(IPAtom):-
	atomic(IPAtom),
	name(IPAtom,ListOfNumCodes),
	splitList(ListOfNumCodes,[N1,N2,N3,N4,Port]),
	integer(N1),
	integer(N2),
	integer(N3),
	integer(N4),
	integer(Port).

splitList(CodeList,[N1,N2,N3,N4]):-
	append(First,[46|Rest],CodeList),
	append(Second,[46|Rest2],Rest),
	append(Third,[46|Fourth],Rest2),
	number_chars(N1,First),
	number_chars(N2,Second),
	number_chars(N3,Third),
	number_chars(N4,Fourth).

splitList(CodeList,[N1,N2,N3,N4,Port]):-
	append(First,[46|Rest],CodeList),
	append(Second,[46|Rest2],Rest),
	append(Third,[46|Rest3],Rest2),
	append(Fourth,[58|P_no], Rest3),
	number_chars(N1,First),
	number_chars(N2,Second),
	number_chars(N3,Third),
	number_chars(N4,Fourth),
	number_chars(Port,P_no).


% correct_playlist_end( ListName ):-
% 	atomic(ListName),
% 	name(ListName,ListNameChars),
% 	name('.m3u',M3UChars),
% 	(
% 	  ends_with(ListNameChars,M3UChars)
% 	->
% 	  true
% 	;
% 	  false
% 	).

% correct_item_end( ItemName ):-
% 	atomic(ItemName),
% 	format("itemname ~w~n",[ItemName]),
% 	name(ItemName,ItemNameChars),
% 	ends(AltList),
% 	member(End,AltList),
% 	name(End,Mp3Chars),
% 	(
% 	  ends_with(ItemNameChars,Mp3Chars)
% 	->
% 	  true
% 	;
% 	  false
% 	).

correct_playlist_end([_,punkt,mtreu]).
correct_item_list([_,punkt,mptre]).
correct_item_list([_,punkt,au]).
%%% approved music file extensions
ends(['.mp3','.wav','.au']).

ends_with( ListNameChars, EndChars ):-
	append(_,Ends,ListNameChars),
	EndChars = Ends.

%%% approved_item(+WhatToPlay)
approved_item(ListToPlay):-
%	format("vad kommer?:: ~w\n",[ListToPlay]),
	(
	  ListToPlay = [nästa]
	;
	  ListToPlay = [föregående]
	;
	  ListToPlay = [nummer,XX],
	  number_phrase([XX],X),
%	  format("spela nummer:: ~w\n",[X]),
	  integer(X)
	).

%%% fix_item(+RemItemList, -Index)
fix_item(List,Index):-
	(
	  List = [nummer,X],
	  number_phrase([X],Index)
	;
	  List = [den,XOrd],
	  (
	    XOrd = första,
	    Index = 1
	  ;
	    XOrd = andra,
	    Index = 2
	  ;
	    XOrd = tredje,
	    Index = 3
	  ;
	    XOrd = fjärde,
	    Index = 4
	  ;
	    XOrd = femte,
	    Index = 5
	  ;
	    XOrd = sjätte,
	    Index = 6
	  ;
	    XOrd = sjunde,
	    Index = 7
	  ;
	    XOrd = åttonde,
	    Index = 8
	  ;
	    XOrd = nionde,
	    Index = 9
	  ;
	    XOrd = tionde,
	    index = 10
	  )
	).

%%% approved_item(+WhatToPlay)
approved_item_eng(ListToPlay):-
%	format("vad kommer?:: ~w\n",[ListToPlay]),
	(
	  ListToPlay = [next]
	;
	  ListToPlay = [previous]
	;
	  ListToPlay = [number,XX],
	  number_phrase_eng([XX],X),
%	  format("spela nummer:: ~w\n",[X]),
	  integer(X)
	).

%%% fix_item(+RemItemList, -Index)
fix_item_eng(List,Index):-
	(
	  List = [number,X],
	  number_phrase_eng([X],Index)
	;
	  List = [the,XOrd],
	  (
	    XOrd = first,
	    Index = 1
	  ;
	    XOrd = second,
	    Index = 2
	  ;
	    XOrd = third,
	    Index = 3
	  ;
	    XOrd = fourth,
	    Index = 4
	  ;
	    XOrd = fifth,
	    Index = 5
	  ;
	    XOrd = sixth,
	    Index = 6
	  ;
	    XOrd = seventh,
	    Index = 7
	  ;
	    XOrd = eighth,
	    Index = 8
	  ;
	    XOrd = ninth,
	    Index = 9
	  ;
	    XOrd = tenth,
	    index = 10
	  )
	).



