
/*********************************************************
       name: device_player.pl
     player: jlGui - audio player (forthcoming)
       date: 2004-10-25
     author: Andreas Wallentin

**********************************************************/
 
:- module( device_dbase, [ dev_set/2,
			   dev_get/2,
			   dev_do/2,
			   %dev_query/2, 
			   dev_query/3, %DH 2003-03-21
			   valid_parameter/1
			 ] ).

:- use_module( library(lists), [ member/2, select/3, is_list/1 ] ).
:- use_module( library(system), [ datime/1 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:- use_module( dbase ).

:- ensure_loaded( library(oaag) ).

:- dynamic variable_value/2.
%% also see default_value/2

environment_mode(simulation).
%environment_mode(none).


init_dbase:-
	environment_mode(simulation),
	user:flag( visualize_devices, yes ),
	!,%trace,
	ensure_loaded(library(visualization)),
	gfx_set_initial_values(device_dbase,dbase).

init_dbase. %see end of file


%%% query(+Query, +Parameters)
query( A^albums_by_artist(A),      [artist] ).
query( A^artists_song(A),          [song] ).
query( A^artists_album(A),         [album] ).
query( A^songs_by_artist(A),       [song_artist] ).
query( Path^path(Path),            [group,item] ).


%%% dev_query changed to allow for optional parameters,
%%% perform_query does the trick instead... DH - 21/3-03  

dev_query( Query, Commitments, Answer):-
	query(Query,Vars),
	set_command_variables(Vars,Commitments,_Values),
	perform_query(Query,Answer).

%%% %% Answers == list of answers
%%% dev_query2( Query, Commitments, Answers):-
%%% 	query(Query,Vars),
%%% 	set_command_variables(Vars,Commitments,_Values),
%%% 	perform_query(Query,Answers).

perform_query( Albums^albums_by_artist(Albums), albums_by_artist(Albums) ):-
	dev_get(artist, Artist ),
	findAlbums(Artist,Albums).

perform_query( Artists^artists_song(Artists), artists_song(Artists) ):-
	dev_get(song,Song),
	findArtistsSong(Song,Artists).

perform_query( Artists^artists_album(Artists), artists_album(Artists) ):-
	dev_get(album,Album),
	findArtistsAlbum(Album,Artists).

perform_query( Songs^songs_by_artist(Songs), songs_by_artist(Songs) ):-
	dev_get(song_artist,Artist),
	findSongsArtist(Artist,Songs).


%%% Path == 'path/to/song_as_atom'
%%% assume file extension *.mp3
perform_query( Path^path(Path), Answer ):-
	(
	  dev_get(group,Group),
	  dev_get(item,Song),
	  song_by_artist(Song,Group,DefPath),
	  make_name(Group,G_name),
	  make_name(Song,S_name),
	  full_path(DefPath,G_name,S_name,Path),
	  format("path: ~w\n",[Path]),
	  Answer = path(Path)
	;
	  Answer = path('')
	  %Answer = not(path(Path))
	  %Answer = fail(Path^path(Path),no_matches)
	).


dev_set(ID,Value1) :-
	environment_mode(simulation),
	interpret_pragmatically(ID,Value1,Value),
%% för att rensa gammalt krafs
	try(retract(variable_value(ID,_))),
	assert(variable_value(ID,Value)),
	( user:flag(visualize_devices,yes) ->
	    gfx_set_node_value(dbase,ID,Value)
	;
	    true
	).


dev_get(ID,Value) :-
	environment_mode(simulation),
	(
	  variable_value(ID,CurrentValue)
	->
%	  format("device - ~w - ~w~n",[ID,CurrentValue]),
	  Value = CurrentValue
	;
%	  format('fel saker ~a -> ~w\n',[ID,Value]),
	  default_value(ID,Value)
	  
	).
%format(' *** ~a -> ~w\n',[ID,Value]).


dev_do(Command,Commitments) :-
	%error:report(['COMMAND=',Command]),
	%format("~nCOMMITS = ~w~n",[Commitments]),
	%format("~nCOMMAND = ~w~n",[Command]),
	action(Command,Vars),           %% Command = AddContact, Vars = [contactName,number]
	%format("~nLIST = ~w~n",[Vars]), 
	set_command_variables(Vars,Commitments,Values),
	( environment_mode(simulation) ->
	    output_upnp(Command,Values) ;
	    true ),
	perform_command(Command).


set_command_variables([],_,[]).
set_command_variables([Var|Vars],Commitments,[Val|Vals]) :-  %% [Var/contactName, Vars/number]
	Com =.. [ Var, Val ],
%%%   	error:report(['Com=',Com]),
%%%   	error:report(['Var=',Var]),
%%%   	error:report(['Val=',Val]),
%%%  	error:report(['Commits=',Commitments]),
	(
	  member(Com,Commitments)
	;
	  Val = not_known
	),
	dev_set(Var,Val),
	set_command_variables(Vars,Commitments,Vals).

% perform_command( 'SetArtist' ):-
% 	dev_get(dbaseArtist,Name),
% 	format("Du letar efter: ~w",[Name]),
% 	findAlbum(Name,Album),
% 	dev_set(album,Album).


perform_command( _ ) :- true.


interpret_pragmatically(_,V,V).
valid_parameter(_).


output_upnp(Command,Parameters) :-
 	Term =.. [ Command | Parameters ],
 	format('\n[UPnP] ~w\n\n',[Term]).

try(G) :-
	( G -> true ; true ).

%%% path name made for Unix/Linux
%%% assume *.mp3 as file extension
full_path('',_,_,'').
full_path(DefaultPath,G_name,S_name,Path):-
	atom_concat(DefaultPath,G_name,Semi2Path),
	atom_concat(Semi2Path,'/',Semi3Path),
	atom_concat(Semi3Path,S_name,AlmostPath),
	atom_concat(AlmostPath,'.mp3',Path).

:- init_dbase.
