 
/*************************************************************************

         name: domain_player.pl
	 date: 2004-10-25
       author: Andreas Wallentin 
 
DH: 
- commented out start_playlist
- renamed fast_rewind to rewind
- commented out playlist_del_specific and playlist_shuffle since they are not covered by GF grammar
*************************************************************************/

:- module( domain_player, [ plan/2,
			    issue/2,
			    sort_restr/1,
			    isa/2,
			    postcond/2,
			    depends/2,
			    incompatible/2
			  ] ).

resource_of_type(domain).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( dbase ).

:- ensure_loaded( library( semsort_player ) ).
:- ensure_loaded( groups ).
:- ensure_loaded( albums ).
:- ensure_loaded( stations_player ).

%%:- ensure_loaded( songs ).

/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?ActionOrIssue, ?Plan )
----------------------------------------------------------------------*/
incompatible(_,_):-fail.
default_question(dummy).

/*
  Vad man ska kunna göra med spelaren:
  
  - start,stop,paus,resume                          ok
  - spela viss spellista                            ok
  - lägga till och ta bort från spellista           ok
  - lyssna på (web)radiostationer                   ok
  - spola fram å tilbaks                            ok
  - hantera volymen                                 ok
  - hantera genre                                   ~
  - kolla vilka låtar/album som finns               ok


  findout -  måste få reda på allt
  raise   -  kör igenom men frågar om inte svar
  bind    -  anonymt, om passerad är det kört, måste inte sättas
	       
*/

% menus
plan( up, [] ).

plan( top,
      [ forget_all,
	%dev_do(player,'VisTop'),
	raise(X^action(X)),
	findout( set([action(handle_player),
		      action(handle_playlist),
		      action(handle_stations)
		     ])
	       )
      ]).
postcond( top, none ).

%%% just handling basic player interaction
plan( handle_player,
      [%%% öppna meny is GUI
       %%dev_do(player,'OpenPlayerMenu'),
       findout( set([action(start),
		     action(stop),
		     action(pause),
		     action(resume),
		     action(rewind),
%		     action(start_playlist),
		     action(start_specific)
		     ])
	       )%,
   %%    forget_all
      ] ).
postcond( handle_player, done(start) ).
postcond( handle_player, done(start_specific) ).
postcond( handle_player, done(stop) ).
postcond( handle_player, done(pause) ).
postcond( handle_player, done(resume) ).
postcond( handle_player, done(rewind) ).
%postcond( handle_player, done(start_playlist) ).


%%% handling playlist management
plan( handle_playlist,
      [%%% öppna menu
       %%dev_do(player,'OpenPlaylistMenu'),
       findout( set([action(playlist_add),
		      action(playlist_del),  %% delete playlist
%		      action(playlist_del_specific),  %% remove X from playlist
%		      action(playlist_shuffle)
		     ])
	       )
      %% forget_all
      ]).
postcond( handle_playlist, done(playlist_add) ).
postcond( handle_playlist, done(playlist_del) ).
%postcond( handle_playlist, done(playlist_del_specific) ).
%postcond( handle_playlist, done(playlist_shuffle) ).





%%%%%%%%%%%%%%%%%%  PLAYER INTERACTION  %%%%%%%%%%%%%%%%%%

plan( restart,
      [ forget_all ] ).
postcond( restart, none ).

%%% plan( start,
%%%       [ dev_query(player,player_status(_S)),
%%% 	if_then_else( player_status(pause),
%%% 		      [dev_do(player,'Resume')],%report('Play',failed(S))],
%%% 		      [dev_do(player,'Start') ] )
%%%       ]).
plan( start,
      [ %%dev_do(player,'VisStart'),
	dev_query(player,S^player_status(S)),
	if_then_else( player_status(pause),
		      [dev_do(player,'Resume')], %report('Play',failed(S))],
		      [dev_do(player,'Start') ] )
      ]).
postcond( start, done('Start') ).
postcond( start, done('Resume') ).
postcond( start, status('Start', failed(_)) ).

%%% för att spela en speciell låt:
%%% nästa, nummer tre osv...
plan( start_specific,
      [ %%dev_do(player,'VisStartSpecific'),
	findout(X^what_to_play(X)),
	dev_do(player,'StartSpecific')] ).
postcond( start_specific, done('StartSpecific') ).

plan( stop,
      [ %%dev_do(player,'VisStop'),
	dev_do(player,'Stop') ]).
postcond( stop, done('Stop') ).

plan( pause,
      [ %%dev_do(player,'VisPause'),
	dev_do(player,'Pause') ]).
postcond( pause, done('Pause') ).

plan( resume,
      [ %%dev_do(player,'VisResume'),
	dev_query(player,S^player_status(S)),
	if_then_else( player_status(pause),
		      [dev_do(player,'Resume')],
		      [dev_do(player,'Resume')] ) %%report('Resume',failed)] )
      ]).
postcond( resume, done('Resume') ).
postcond( resume, status('Resume', failed) ).


%plan( start_playlist,
%      [ %%dev_do(player,'VisStartPlaylist'),
%	findout(X^playlist(X)),
%	dev_do(player,'StartPlaylist')
%      ]).
%postcond( start_playlist, done('StartPlaylist') ).

plan( vol_up,
      [ dev_do(player,'IncreaseVol') ]).
postcond( vol_up, done('IncreaseVol') ).

plan( vol_down,
      [ dev_do(player,'DecreaseVol') ]).
postcond( vol_down, done('DecreaseVol') ).



plan( rewind,
      [ %%dev_do(player,'VisFastRewind'),
%%% öppna menu i GUI
	%%dev_do(player,'OpenSubstartMenu'),
	findout( set([action(fast_forward),
		      action(rewind),
		      action(next_song),
		      action(previous_song)
		     ])
	       )%,
       %% hur komma tillbaka till top när den är klar??
       %% om man kommer via annan plan först
       %forget_all  %% in order to handle 'no' as answer
      ]).
postcond( rewind, done(fast_forward) ).
postcond( rewind, done(rewind) ).
postcond( rewind, done(next_song) ).
postcond( rewind, done(previous_song) ).

plan( fast_forward,
      [ dev_do(player,'FF') ]).
postcond( fast_forward, done('FF') ).

plan( rewind,
      [ dev_do(player,'Rew') ]).
postcond( rewind, done('Rew') ).

plan( next_song,
      [ dev_do(player, 'Next' ) ]).
postcond( next_song, done('Next') ).

plan( previous_song,
      [ dev_do(player, 'Previous' ) ]).
postcond( previous_song, done('Previous') ).


%%%%%%%%%%%%%%%%%%  PLAYLIST INTERACTION  %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%  RADIO STATION INTERACTION  %%%%%%%%%%%%%%%%%%

%%% gammal, provar ny för att fixa svaret "en låt"
plan( listen_to,
      [ findout( set([action(handle_stations),
		      action(playlist_add)
		     ])
	       )
      ]).
postcond( listen_to, done(handle_stations) ).
postcond( listen_to, done(playlist_add) ).


plan( handle_stations,
      [	findout(X^station(X)),
	dev_do(player,'SetStation')
      ]).
postcond( handle_stations, done('SetStation') ).


%%% notera ändringsförslag  i update_rules och selection_rules
plan(playlist_add,
     [ %subgoal(X^path(X)),
	%findout(X^path(X)), %%findout(X^path(X))
       %%dev_do(player,'VisPlaylistAdd'),
       findout(Y^group(Y)),
       findout(Z^item(Z)),
       dev_query(dbase,X^path(X)),
       if_then_else(not(path('')),
		    [dev_set(player,path,X),
		     dev_do(player,'PlaylistAdd') ],
 		    [ if_then(group(G),
			      if_then(item(S),
				      report('PlaylistAdd',failed(G,S))
				     ))]
  		   )
     ]).

postcond( playlist_add, done('PlaylistAdd') ).
postcond( playlist_add, status('PlaylistAdd', failed(_,_) ) ).


plan( playlist_del,
      [ dev_do(player,'PlaylistDel')  %% deletes current playlist
       ]).
postcond( playlist_del, done('PlaylistDel') ).

%plan( playlist_del_specific,
%      [ %%dev_do(player,'VisPlaylistDelSpecific'),
%	findout(X^itemRem(X)),
%	dev_do(player,'PlaylistDelSpecific')%% deletes items from current playlist
%       ]).
%postcond( playlist_del_specific, done('PlaylistDelSpecific') ).

%plan( playlist_shuffle,
%      [ dev_do(player,'PlaylistShuffle')  %% shuffles current playlist
%       ]).
%postcond( playlist_shuffle, done('PlaylistShuffle') ).


plan( show_list,
      [ %%dev_do(player,'VisShowList'),
	dev_do(player,'ShowList') ] ).
postcond( show_list, done('ShowList') ).


%%%%%%%%%%%%%%%%%%  REMOVAL INTERACTION  %%%%%%%%%%%%%%%%%%

plan( remove,
      [ findout( set([action(playlist_del),
		      action(playlist_del_specific)
		     ])
	       )
      ]).
postcond( remove, done(playlist_del) ).
postcond( remove, done(playlist_del_specific) ).


%%%%%%%%%%%%%%%%%%  QUESTIONS INTERACTION  %%%%%%%%%%%%%%%%%%

%% what album by Artist
plan( X^albums_by_artist(X), %% bara sort_restr??
      [ %%dev_do(player,'VisAlbumsByArtist'),
	findout(Art^artist(Art)), %% valid_param, bara??
	dev_query(dbase, X^albums_by_artist(X) )
      ]).

%% what artist made Song
plan( Group^artists_song(Group),
      [ %%dev_do(player,'VisArtistsSong'),
	findout(Song^song(Song)),
	dev_query(dbase, Group^artists_song(Group))
      ]).

%% what artist made Album
plan( Art^artists_album(Art),
      [ %%dev_do(player,'VisArtistsAlbum'),
	findout(Alb^album(Alb)),
	dev_query(dbase, Art^artists_album(Art) ),
	dev_set(player,art,Art)
      ]).

%% what songs made by artist
plan( Songs^songs_by_artist(Songs),
      [ %%dev_do(player,'VisSongsByArtist'),
	findout(Art^song_artist(Art)),
	dev_query(dbase,Songs^songs_by_artist(Songs))
      ]).


%%% the song currently being handled
plan( X^current_song(X),
      [ dev_query(player,X^current_song(X)) ]).


plan(X^path(X),
     [ %%dev_do(player,'VisPath'),
       findout(Y^group(Y)),
       findout(Z^item(Z)),
       dev_query(dbase,X^path(X))
     ]).

/*
*/

/*
Kommande planer??
%%% fråga: vilka låtar har X gjort, X == han,hon,de,dem,dom
plan(Songs^songs_by_current_artist(Songs),
     [ %%findout(Cur^current_artist(Cur)),
       if_then_else( group(Group),
		     [ dev_query(dbase,songs_by_current_artist(Songs)) ],
		     [] )
     ] ).


%%% finns låt/path is spellista??
plan( Path^in_playlist(Path),
      [dev_query(player,in_playlist(Path))] ).

%%% vilka grupper finns det i databasen?
plan( Groups^all_groups(Groups),
      [dev_query(dbase,all_groups(Groups))] ).

*/


depends(dummy,dummy).
/*--------------------------------------------------------------
     Conceptual knowledge
----------------------------------------------------------------------*/

% sortal restrictions; determine relevance and resolvement
% all POSSIBLE propositions, see valid_parameter/1

sort_restr( domain( X ) ) :- sem_sort( X, domain ).% SL021125
sort_restr( language( X ) ) :- sem_sort( X, language ).
sort_restr( not P ) :- sort_restr( P ).

sort_restr( action( X ) ) :- sem_sort( X, action ).
sort_restr( action( respond(Q) ) ) :- sort_restr( issue(Q) ).

%% the first checks are made in semsort_player, since there isn't
%% any module/file representing "all possible choices"
sort_restr( playlist(X) ):-      sem_sort( X, playlist ).
%sort_restr( itemAdd(X) ):-       sem_sort(X,item).

sort_restr( song(X) ):-          sem_sort(X,item).
sort_restr( item(X) ):-          sem_sort(X,item).
sort_restr( current_song(X) ):-  sem_sort(X,song).
sort_restr( what_to_play(X) ):-  sem_sort(X,index).
sort_restr( itemRem(X) ):-       sem_sort(X,index).
sort_restr( itemRem(X) ):-       sem_sort(X,index).
sort_restr( groupToAdd(X) ):-    group( X ).
sort_restr( artist(X) ):-        group( X ).
sort_restr( group(X) ):-         group( X ).
sort_restr( song_artist(X) ):-   group( X ).
sort_restr( album(X) ):-         album( X ).
sort_restr( artists_song(X) ):-  group_atom( X ).
sort_restr( artists_album(X) ):- group_atom( X ).
sort_restr( albums_by_artist(X) ):- album_atom( X ).
sort_restr( songs_by_artist(X) ):-  song_atom( X ).
sort_restr( station(X) ):-	 radio_station( X ).
sort_restr( year(X) ):-          sem_sort( X, year ).
sort_restr( path(X) ):-          atomic( X ).%,format("hallå: ~w\n",[X]).
%sort_restr( X^path(X) ):-          atomic( X ),format("hallå: ~w\n",[X]).
sort_restr( not path(X) ):-     format("hallå: ~w\n",[X]), atomic( X ).
sort_restr( fail(Path^path(Path),no_matches) ).
%% "ren" databassökning
% issues
sort_restr( issue(Q) ):-
	plan( Q, _ ),
	\+ sort_restr( action( Q ) ).

sort_restr( issue(Q) ):-
	plan( _, Plan ),
	member( findout(Q), Plan ),
	\+ sort_restr( action( Q ) ).

% metaissue

sort_restr( und(_DP*P) ):- sort_restr(P).

% sloppy, but allows "no" as answer to clarification alt-q
% could be replaced by general rule saying that not(set([p1, ..., ])) is sortally correct,
% menaing the same as (not P1 and not p2 and...)

sort_restr( not und(_DP*set(_))).

%%% parameter validity; determines acceptance
%%% checks all ACTUAL props in DB

%%% kolla mot FAKTISK "databas"

%valid_parameter( X ):- sort_restr( X ).

valid_parameter( path(X) ):-          atomic( X ).

valid_parameter( domain( X ) ) :-    sem_sort( X, domain ).
valid_parameter( language( X ) ) :-  sem_sort( X, language ).
valid_parameter( action(X) ) :-      sem_sort( X, action ).
valid_parameter( not P ) :-          valid_parameter( P ).
valid_parameter( action( respond(Q) ) ) :- valid_parameter( issue(Q) ).

valid_parameter( playlist(X) ):-      sem_sort( X, playlist ).
%valid_parameter( itemAdd(X) ):-       sem_sort( X, item ).
valid_parameter( item(X) ):-          sem_sort( X, item ).
valid_parameter( song(X) ):-          sem_sort( X, item ).
valid_parameter( station(X) ):-       sem_sort( X, station ).
valid_parameter( groupToAdd(X) ):-    sem_sort( X, group ).
valid_parameter( group(X) ):-         sem_sort( X, group ).
valid_parameter( artist(X) ):-        sem_sort( X, group ).
valid_parameter( song_artist(X) ):-   sem_sort( X, group ).
valid_parameter( album(X) ):-         sem_sort( X, album ).
valid_parameter( path(Path) ):-       atomic(Path).
valid_parameter( what_to_play(X) ):-  sem_sort( X, index).
valid_parameter( itemRem(X) ):-       sem_sort( X, index ).
valid_parameter( index(X) ):-         sem_sort( X, index ).

%%% frågorna behövs inte?  varför?
%valid_parameter( albums_by_artist(X) ):- sem_sort( X, album ).
%valid_parameter( artists_song(X) ):-  sem_sort( X, group ).
%valid_parameter( artists_album(X) ):- sem_sort( X, group ).
%valid_parameter( current_song(X) ):-  sem_sort( X, song ).
