
/*************************************************************************

         name: songs.pl 
	 date: 2004-11-18
       author: Andreas Wallentin

       Supposed to contain "all possible" songs from an external DB
       
*************************************************************************/
:- use_module( library(lists), [member/2] ).
:- use_module( dbase,          [break_list/2,
				song_list/1,
				song_list2/1] ).

%%% Checks if the atom/song name exists
%%% The atom is broken down to list of lists
%%% in order to work statisfyingly...so far
%%%
%%% Atom == 'people are strange'
%%%song_atom( +Song )
song_atom( Atom ):-
%	format("inne i albuemdfn~n",[]),
	atomic(Atom),
	name(Atom,CharList),
	break_list(CharList,SongList),
	chk_songs(SongList).

chk_songs([Song|Rest]):-
	song_list(SL),
	song_list2(SL2),
	(
	  member(Song,SL)
	;
	  member(Song-_Art,SL2)
	),
	chk_songs(Rest).
chk_songs([]).

%post( best_of, [svenska,klassiker], [ [flickorna,på,tv,två]-[gyllene,tider]

%%% po( best_of, [svenska,klassiker], [ [flickorna,på,tv,två]-[gyllene,tider],
%%% 				      [solglasögon]-[docent,död],
%%% 				      [åttahundra,grader]-[ebba,grön],
%%% 				      [flickan,och,kråkan]-[mikael,wiehe],
%%% 				      [diamanter]-[lustans,lakejer],
%%% 				      [blinkar,blå]-[adolphson,och,falk],
%%% 				      [sommaren,är,kort]-[tomas,ledin],
%%% 				      [öppna,landskap]-[ulf,lundell],
%%% 				      [segla,på,ett,moln]-[annelie,ryde],
%%% 				      [vintersaga]-[monica,törnell],
%%% 				      [du,ska,va,president]-[imperiet],
%%% 				      [vindarna]-[freda],
%%% 				      [jag,blir,hellre,jagad,av,vargar]-[orup],
%%% 				      [efter,stormen]-[marie,fredrisson],
%%% 				      [håll,om,mig]-[peter,lemarc],
%%% 				      [vem,ska,jag,tro,på]-[di,leva],
%%% 				      [kärlekens,tunga]-[eldkvarn],
%%% 				      [vingar]-[mikael,rickfors],
%%% 				      [vara,vänner]-[jakob,hellman],
%%% 				      [ängeln,i,rummet]-[eva,dahlgren],
%%% 				      [det,hon,vill,ha]-[christer,sandelin] ] ).
