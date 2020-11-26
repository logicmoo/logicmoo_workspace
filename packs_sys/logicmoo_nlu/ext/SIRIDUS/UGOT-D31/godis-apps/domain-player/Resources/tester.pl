:- use_module([library(lists),library(charsio)]).
:- discontiguous playing/1.

get_name:-
	showList(List),
	fix_list(List,[],X),
	print_ans(X).

fix_list([],L,L).
fix_list([First|Xs],Tmp,Ans):-
	arg(4,First,Info),
	length(Info,Length),
	( Length > 6
	-> Info = [title(Tit),artist(Grp)|_],
	    append(Tmp,[Grp-Tit],T)
	; Info = [title(Chan)|_],
	    append(Tmp,[Chan],T)
	),
	fix_list(Xs,T,Ans).

%get_info([],
print_ans([]).
print_ans([X|Xs]):-
	format("~w\n",[X]),
	print_ans(Xs).

	
%%% 	name(X,LL),
%%% 	append(Title,[32,45,32|Group],LL),
%%% 	name(TitleN,Title),
%%% 	name(GroupN,Group),
%%% 	rem_last_whs(TitleN,Tit),
%%% 	rem_first_whs(GroupN,Grp).


showList([item('(05:27) stalker - Covenant',
	       327,
	       '/home/andreas/music/covenant/stalker.mp3',
	       [title(stalker),
		artist('Covenant'),
		album(''),
		track(32),
		playTime(327),
		samplingRate(44100),
		bitRate(160000)]),
	  item('(05:19) Leviathan - Covenant' ,
	       319,
	       '/home/andreas/music/covenant/leviathan.mp3',
	       [title('Leviathan' ),
		artist('Covenant' ),
		album('Europa' ),
		genre('Electronic'),
		track(32),
		playTime(319),
		samplingRate(44100),
		bitRate(128000)])
	 ]).
showList([item('(03:25) Trust U',
 	       205,
 	       '/home/andreas/music/mesh/trust_you.mp3',
 	       [title('Trust U'),
 		artist('Mesh'),
 		album(1),
 		genre('(None)'),
 		track(1),
 		playTime(205),
 		samplingRate(44100),
 		bitRate(320000)]),
 	  item('(04:54) Valentina - Komputer',
 	       294,
 	       '/home/andreas/music/komputer/valentina.mp3',
 	       [title('Valentina'),
 		artist('Komputer'),
 		album('The World of Tomorrow'),
 		genre(misc),
 		track(4),
 		playTime(294),
 		samplingRate(44100),
 		bitRate(128000)]),
 	  item('DigitalGunfire.com - [Industrial:EBM:Futurepop] Long Range, Hard Hitting!',
 	       -1,
 	       'http://129.16.159.166:8000',
 	       [title('DigitalGunfire.com - [Industrial:EBM:Futurepop] Long Range, Hard Hitting!'),
 		genre('industrial ebm electronic'),
 		track(-1),
 		playTime(0),
 		samplingRate(44100),
 		bitRate(128000)])
 	 ]).

























findAlbums( Artist, Albums ):-
	(
	  setof(Album, NotUsed^Not2^post(Artist,Album,NotUsed,Not2), AlbumList )%% albums == list of lists
	  ->
	  nice_atom(AlbumList,Albums)
	;
	  Albums = ''
	).

findArtistsSong( Song, Artists ):-
	(
	  findall(Artist, (post(Artist,_,SongList,_),member(Song,SongList)), ArtistList )
	  ->
	  nice_atom(ArtistList,Artists)
	;
	  Artists = ''
	).

findArtistsAlbum( Album, Artists ):-
	(
	  setof(Artist, N^NN^post(Artist,Album,N,NN), ArtistList )
	  ->
	  nice_atom(ArtistList,Artists)
	;
	  Artists = ''
	).

findSongsArtist( Artist, Songs ):-
	(
	  setof(Song, Not^N2^post(Artist,Not,Song,N2), SongList),
	  findall(Song2, (post(best_of,Not,List),member(Song2-Artist,List) ), SongList2)
	->
	  nice_song_atom(SongList,Songs1),
	  nice_atom(SongList2,Songs2),
	  atom_concat(Songs1,',',TmpSongs),
	  atom_concat(TmpSongs,Songs2,Songs),
	  !
	;
	  findall(Song2, (post(best_of,Not,List),member(Song2-Artist,List) ), SongList2),
	  nice_atom(SongList2,Songs),
	  !
	;
	  Songs = ''
	).


post( [spock],            [single],                    [ [romulan,ale] ],                                    _genre ).

%%% songs that will be added to local disc soon...
post( [dia,psalma],       [gryningstid],               [ [alla,älskar,dig],[hon,får],[tro,rätt,tro,fel],[den,som,spar],
							 [emelie],[kalla,sinnen],[grytfot],[gryningsvisa,i,d,moll],
							 [sol,över,oss] ],  _genre ).

post( [ebba,grön],        ['1978','1982'],             [ [profit],[ung,och,sänkt],[tyst,för,fan],[mona,tumbas,slim,club],
							 [vad,skall,du,bli],[häng,gud],[totalvägra],[beväpna,er],
							 [det,måste,vara,radion],[pervers,politiker],[staten,och,kapitalet],
							 [ung,och,kåt],['800',grader],[mamma,pappa,barn],[mental,istid],
							 [flyktsoda],[uppgång,och,fall],[tittar,på,tv],
							 [nu,släcks,tusen,människoliv] ],  _genre ).

%%% specialpost för samlingsalbum...
%%% post( ?X, ?Title, ?ListWithSong-Group)
post( best_of, [topp,hits], [ [se,på,mig]-[jan,johansen],
			      [det,vackraste]-[cecilia,vennersten],
			      [dina,färger,var,blå]-[tommy,nilsson],
			      [tess]-[peter,lemarc],
			      [vad,du,ser,är,vad,du,får]-[lisa,nilsson],
			      [emelie]-[spock],
			      [går,ut,med,mig,själv]-[mauro,scocco],
			      [när,vägarna,lockar]-[malin,julia] ] ).

nice_song_atom( [NiceAtom], SongsAtom ):- 
	nice_atom(NiceAtom,SongsAtom).
%%% om lista med listor av listor
%%% artist har gjort flera album
nice_song_atom( [SongList|Rest], SongsAtom ):-
	nice_song_atom([SongList],Tmp),
	nice_song_atom(Rest,Tmp2),
	atom_concat(Tmp,',',T),
	atom_concat(T,Tmp2,SongsAtom).


make_name(List,Atom):-
	make_name(List,[],Atom),
	!.
make_name([],_,'').
make_name([Last],_,Atom):-
	atom_concat('',Last,Atom).
make_name([First|Rest],Temp,Atom):-
	make_name(Rest,Temp,A2),
	atom_concat(First,'_',Atom2),
	atom_concat(Atom2,A2,Atom).

nice_atom( [], '' ).
nice_atom( [AlbumNameList|Xs], AlbumsAtom ):-
	make_name(AlbumNameList,TempAtom),
	nice_atom(Xs,Temp2Atom),
	(
	  Temp2Atom = ''
	->
	  AlbumsAtom = TempAtom
	;
	  atom_concat(TempAtom,',',T2),
	  atom_concat(T2,Temp2Atom,AlbumsAtom)
	).






































get_name2(X):-
	playing(List),
	fix_list2(List,X).

fix_list2([whatIsPlaying(L)],Tit-Grp):-
	arg(1,L,X),%% == formatted name
	name(X,LL),
	append(Title,[32,45,32|Group],LL),
	name(TitleN,Title),
	name(GroupN,Group),
	rem_last_whs(TitleN,Tit),
	rem_first_whs(GroupN,Grp).


playing([whatIsPlaying(item('(07:08) Warsaw Ghetto              -                  Nitzer Ebb',
			    428,
			    '/home/andreas/music/nitzer_ebb/warsaw_ghetto.mp3',
			    [title('Warsaw Ghetto'),
			     artist('Nitzer Ebb'),
			     album(_),
			     track(32),
			     playTime(428),
			     samplingRate(44100),
			     bitRate(112000)
			    ]
			   )
		      )
	]).

playing( [whatIsPlaying(item('DigitalGunfire.com - [Industrial:EBM:Futurepop] Long Range, Hard Hitting!',
			     -1,
			     'http://129.16.159.166:8000',
			     [title('DigitalGunfire.com - [Industrial:EBM:Futurepop] Long Range, Hard Hitting!'),
			      genre('industrial ebm electronic'),
			      track(-1),
			      playTime(0),
			      samplingRate(44100),
			      bitRate(128000)])) ]).



rem_first_whs(Name,Res):-
	name(Name,Codes),
	r1(Codes,R1),
	name(Res,R1).

rem_last_whs(Name,Res):-
	name(Name,Codes),
%%%	reverse(R2,R1),
%%%	r1(R1,Res1),
%%%	reverse(Res1,Res2),
	r2(Codes,R2),
	name(Res,R2).


r1([X|Xs],A):-
	X \= 32,
	A = [X|Xs].

r1([X|Xs],Res):-
	 X = 32,
	 r1(Xs,Res).

r2(List,A):-
	append(_,[X],List),
	X \= 32,
	A = List.

r2(List,Res):-
	append(Ls,[X],List),
	X = 32,
	r2(Ls,Res).
