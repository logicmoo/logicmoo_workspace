
/*************************************************************************

         name: albums.pl 
	 date: 2004-11-03
       author: Andreas Wallentin
 
*************************************************************************/
:-multifile synset/2,sem_sort/2.
:-discontiguous synset/2,sem_sort/2.

:- use_module( dbase,          [break_list/2] ).

%%% Atom == sequencer, seq2
album_atom( Atom ):-
	atomic(Atom),
	name(Atom,CharList),
	break_list(CharList,AlbumList),
	chk_albums(AlbumList).


album([sequencer]).
album([viral,extinction]).
album([valentia]).
album([in,this,place,forever]).
album([bona,drag]).
album([that,total,age]).
album([please]).
%% spocks album...


%%% all "possible" albums
album([1978,1982]).
album([alla,talar,svenska]).
album([black,celebration]).
album([construction,time,again]).
album([curse]).
album([dynamite]).
album([europa]).
album([for,your,pleasure]).
album([gryningstid]).
album([här,och,nu]).
album([ingenmansland]).
album([i,vargens,spår]).%%finns låt med samma namn
album([julens,sånger]).
album([northen,light]).
album([people,are,strange]).
album([peps,bitar]).
album([sehnsucht]). 
album([some,great,reward]).
album([the,immaculate,collection]).
album([the,white,room]).
album([violator]).  
album([wish]).

%% album med samma namn som grupp
album([jumper]).
album([nordman]).

%% samlingsalbum - hur representera i dbase?
album([topp,hits]).
album([svenska,klassiker]).

%%% default - if unknown
album([not,known]).


chk_albums( [] ).
chk_albums( [Album|Xs] ):-
	album(Album),
	chk_albums(Xs).
