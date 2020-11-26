/*********************************************************
       name: device_player.pl
     player: jlGui - audio player (forthcoming)
       date: 2004-10-25
     author: Andreas Wallentin

**********************************************************/
 
:- module( device_player, [ dev_set/2,
			    dev_get/2,
			    dev_do/2,
				%dev_query/2, 
			    dev_query/3, %DH 2003-03-21
			    valid_parameter/1
			  ] ).
resource_of_type(upnp_dev).

:- use_module( library(lists),    [ member/2, select/3, is_list/1, append/3 ] ).
:- use_module( library(system),   [ datime/1 ] ).
:- use_module( dbase,[make_name/2] ).

:- ensure_loaded( [semsort_player, stations_player, digits_svenska_player] ).
:- use_module( trindikit(tkit_oaa),[solve/1] ).

:- dynamic variable_value/2.
%% also see default_value/2

environment_mode(simulation).
%environment_mode(none).

/*
  punkter på vad som ska göras

  * spela
  * ladda spellista
  * hoppa framåt/bakåt

  se jlgui_commands.txt för alternativ

*/

%%% kan man ha samma var.namn??
%%% nej

%%% Actions (action(+Name,+Parameters))
                                                %%% Ready for OAA
action( 'Start',           [] ).                      %%% oaa
action( 'Stop',            [] ).                      %%% oaa
action( 'Pause',           [] ).                      %%% oaa
action( 'Resume',          [] ).                      %%% oaa
action( 'FF',              [] ).
action( 'Rew',             [] ).
action( 'StartPlaylist',   [playlist] ).              %%% oaa

action( 'SetStation',      [station] ).               %%% oaa
action( 'PlaylistAdd',     [] ).                      %%% oaa
action( 'PlaylistDel',     [] ).                      %%% oaa
action( 'PlaylistDelSpecific',     [itemRem] ).
action( 'PlaylistShuffle', [] ).                      %%% oaa

action( 'IncreaseVol',     [] ).                      %%% oaa
action( 'DecreaseVol',     [] ).                      %%% oaa
action( 'Next',            [] ).                      %%% oaa
action( 'Previous',        [] ).                      %%% oaa

action( 'StartSpecific',   [what_to_play] ).
action( 'ShowList',        [] ).

%%% Actions for opening player menues
action( 'OpenPlayerMenu',  [] ).
action( 'OpenSubstartMenu',  [] ).
action( 'OpenPlaylistMenu',  [] ).

%%% Actions for visualise IS
action( X, [] ):-
	atom_concat('Vis', _Anything, X).

%%% "deletePlan(PlanName)";
%%% "visualiseIS(PlanName)";


%%% Default values for variables 
default_value( player_status,    stop ).
default_value( playlist_name,    '/home/andreas/music/default.m3u' ). 
default_value( current_song,     [] ).
%default_value( current_group,    [] ).
%default_value( listen_to_radio,  yes ).

query( S^current_song(S),     [] ).
%query( G^current_group(G),    [] ).
query( S^player_status(S),    [] ).
%query( listen_to_radio(_R),  [] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%init_player:-
	%environment_mode(simulation),
	%user:flag( visualize_devices, yes ),
	%!,%trace,
	%ensure_loaded(library(visualization)),
	%gfx_set_initial_values(device_player,stop).

init_player. %see end of file


%%% dev_query changed to allow for optional parameters,
%%% perform_query does the trick instead... DH - 21/3-03  

dev_query( Query, Commitments, Answer):-
	query(Query,Vars),
	set_command_variables(Vars,Commitments,_Values),
	perform_query(Query,Answer).

%%% perform_query( current_song([Tit,Grp]), current_song([Tit,Grp]) ):-
%%% 	tkit_oaa:solve(whatIsPlaying(_S),[],S), %%whatIsPlaying/1
%%% 	( S = [] -> Tit = '', Grp = '' ;
%%% 	    fix_list(S,Tit-Grp)
%%% 	).

perform_query( X^current_song(X), current_song([Tit,Grp]) ):-
	tkit_oaa:solve(whatIsPlaying(S)), %%whatIsPlaying/1
	( S = [] -> Tit = '', Grp = '' ;
	    fix_list(S,Tit-Grp)
	).

%%% perform_query( Group^current_group(Group), current_group(GroupName) ):-
%%% 	dev_get(current_group, Group),
%%% 	make_name(Group,GroupName).

perform_query( Status^player_status(Status), player_status(Status) ):-
	dev_get(player_status,Status).
%%% 	(
%%% 	  Status = pause,
%%% 	  Answer = player_status(Status)
%%% 	;
%%% 	  Answer = not(player_status(Status))
%%% 	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% perform_query( listen_to_radio(R), Answer ):-
%%% 	format("letar efter radio~n",[]),
%%% 	dev_get(listen_to_radio,R),
%%% 	(R = yes,Answer = listen_to_radio(R)
%%% 	; Answer = not(listen_to_radio(R)) ),
%%% 	format("radio == ~w~n",[R]).
	    
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dev_set(ID,Value1) :-
	environment_mode(simulation),
	interpret_pragmatically(ID,Value1,Value),
%% för att rensa gammalt krafs
	try(retract(variable_value(ID,_))),
	assert(variable_value(ID,Value)),
	format(' *** ~a <- ~w\n',[ID,Value]).

%	( user:flag(visualize_devices,yes) ->
%	    gfx_set_node_value(player,ID,Value)
%	;
%	    true
%%	).
	

dev_get(ID,Value) :-
	environment_mode(simulation),
	( variable_value(ID,CurrentValue) ->
	    %format("device - ~w - ~w~n",[ID,CurrentValue]),
	    Value = CurrentValue
	;
	    default_value(ID,Value)
	).
%%	format(' *** ~a -> ~w\n',[ID,Value]).

dev_do(Command,Commitments) :-
%%% 	error:report(['COMMAND=',Command]),
%%% 	format("~nCOMMITS = ~w~n",[Commitments]),
%%% 	format("~nCOMMAND = ~w~n",[Command]),
	action(Command,Vars),           %% Command = AddContact, Vars = [contactName,number]
	%format("~nLIST = ~w~n",[Vars]), 
	set_command_variables(Vars,Commitments,Values),
	( environment_mode(simulation) ->
	    output_upnp(Command,Values) ;
	    true ),
	
	perform_command(Command).


set_command_variables([],_,[]).
%%                [contactName,number]      
set_command_variables([Var|Vars],Commitments,[Val|Vals]) :-  %% [Var/contactName, Vars/number]
	Com =.. [ Var, Val ],
%%%   	error:report(['Com=',Com]),
%%%   	error:report(['Var=',Var]),
%%%   	error:report(['Val=',Val]),
%%%  	error:report(['Coms=',Commitements]),
	member(Com,Commitments),
	dev_set(Var,Val),
	set_command_variables(Vars,Commitments,Vals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% %       Player menu interaction
%%% perform_command( 'OpenPlayerMenu' ):-
%%% 	tkit_oaa:solve(openPlayerMenu).

%%% perform_command( 'OpenSubstartMenu' ):-
%%% 	tkit_oaa:solve(openSubstartMenu).

%%% perform_command( 'OpenPlaylistMenu' ):-
%%% 	tkit_oaa:solve(openPlaylistMenu).


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% playPlayer(X) för nr X i spelllistan
perform_command( 'Start' ):-
	dev_get( player_status, Value ),
	try(retract( variable_value(player_status,Value) ) ),
	dev_set( player_status, playing ),
 	tkit_oaa:solve(playPlayer).
%%% %%	format("tar bort start igen...~w\n",[A]),
%%% 	tkit_oaa:solve(deletePlan('Start')).

perform_command( 'StartSpecific' ):-
	dev_get( what_to_play, ToPlay ),
	format("att spelas: ~w\n",[ToPlay]),
	(
	  ToPlay = [nästa],
	  tkit_oaa:solve(playNext)
	;
	  ToPlay = [föregående],
	  tkit_oaa:solve(playPrevious)
	;
	  fix_item(ToPlay,Ind),
	  Index is Ind - 1,
	  tkit_oaa:solve(playPlayer(Index))
	)
	;
	dev_get( what_to_play, ToPlay ),
	(
	  ToPlay = [next],
	  tkit_oaa:solve(playNext)
	;
	  ToPlay = [previous],
	  tkit_oaa:solve(playPrevious)
	;
	  fix_item_eng(ToPlay,Ind),
	  Index is Ind - 1,
	  tkit_oaa:solve(playPlayer(Index))
	).

perform_command( 'Stop' ):-
	dev_get( player_status, Value ),
	try(retract( variable_value(player_status,Value) )),
	dev_set( player_status, stop ),
	tkit_oaa:solve(stopPlayer). %% stopPlayer/0

perform_command( 'Pause' ):-
	dev_get( player_status, Value ),
	try(retract( variable_value(player_status,Value) )),
	dev_set( player_status, pause ),
	tkit_oaa:solve(pausePlayer). %%pausePlayer/0

%%% bara om den står i pausläge
perform_command( 'Resume' ):-
	try(retract( variable_value(player_status,_) )),
	dev_set( player_status, playing ),
	tkit_oaa:solve(resumePlayer). %%resumePlayer/0

perform_command( 'StartPlaylist' ):- 
	dev_get( playlist, Name ),
	try(retract( variable_value(playlist_name,_) )),
	dev_set( playlist_name, Name),
	make_playlist(Name,List),
	tkit_oaa:solve(openPlaylist(List)).

perform_command( 'SetStation' ):-
	dev_get( station, Station ),
	try(retract( variable_value(station,_) )),
	get_station(Station,IP_atom),
	tkit_oaa:solve(addURL(IP_atom,_X)).

perform_command( 'PlaylistAdd' ):-
	dev_get( path, Path ),
	tkit_oaa:solve(addFile(Path,_F)).

perform_command( 'PlaylistDel' ):-
	dev_get( playlist_name, Name ),
	try(retractall( variable_value(playlist_name,Name))),
	tkit_oaa:solve(clearPlaylist).

perform_command( 'PlaylistDelSpecific' ):- %% remItems
	(
	  dev_get( itemRem, Ind ),
	  fix_item(Ind,Ind2)
	;
	  dev_get( itemRem, Ind ),
	  fix_item_eng(Ind,Ind2)
	),
	Index is Ind2 - 1,
	tkit_oaa:solve(removeIndex(Index)).
	
perform_command( 'PlaylistShuffle' ):-
	tkit_oaa:solve(shuffleList(_A)). 

perform_command( 'Next' ):-
	format("Next\n",[]),
	tkit_oaa:solve(playNext).

perform_command( 'Previous' ):-
	format("Previous\n",[]),
	tkit_oaa:solve(playPrevious).

perform_command( 'FF' ).
perform_command( 'Rew' ).

%%% fix getGain/1 first...
perform_command( 'IncreaseVol' ):-
	tkit_oaa:solve(setGain(0.9)).
perform_command( 'DecreaseVol' ):-
	tkit_oaa:solve(setGain(0.3)).

perform_command( 'ShowList' ):-
	tkit_oaa:solve(showList(List)),
	fix_list(List,[],Ans),
	print_ans(1,Ans),
	format("\n",[]).

%%% visualise the IS
%perform_command( VisPlan ):-
%	atom_concat('Vis', Plan, VisPlan),
%	tkit_oaa:solve(visualiseIS(Plan)).


perform_command( _ ) :- true.



interpret_pragmatically(_,V,V).
valid_parameter(_).


output_upnp(Command,Parameters) :-
 	Term =.. [ Command | Parameters ],
 	format('\n[UPnP] ~w\n\n',[Term]).

try(G) :-
	( G -> true ; true ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                 Help predicates                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%  making "playlist name"  %%%%%%%%%

make_playlist([],_default).
make_playlist(NameList,PlayList):-
	m2(NameList,List),
	%%% default library for music/playlists
	atom_concat('/home/andreas/music/',List,PlayList),
	!.

m2(NList,PList):-
	clean_name(NList,List),
	make_name2(List,[],PList).

make_name2([],_,'').
make_name2([Last],_,Atom):-
	atom_concat('',Last,Atom2),
	atom_concat(Atom2,'.m3u',Atom).
make_name2([First|Rest],Temp,Atom):-
	make_name2(Rest,Temp,A2),
	atom_concat(First,' ',Atom2),
	atom_concat(Atom2,A2,Atom).

clean_name(List,CL):-
	append(CL,[punkt,mtreu],List).


%%%%%%%%%%%  fixing list, what is playing  %%%%%%%%%
fix_list([whatIsPlaying(L)],Tit-Grp):-
	arg(1,L,X),%% == formatted name
	name(X,LL),
	append(Title,[32,45,32|Group],LL),
	name(TitleN,Title),
	name(GroupN,Group),
	rem_last_whs(TitleN,Tit),
	rem_first_whs(GroupN,Grp).

%%%%%%%%%%%%%%%  for showing playlist   %%%%%%%%%%%%%%%%
fix_list([],L,L).
fix_list([First|Xs],Tmp,Ans):-
	arg(4,First,Info),
	length(Info,Length),
	( Length > 6
	-> Info = [title(TitX),artist(GrpX)|_],
	    rem_last_whs(TitX,TitY),
	    rem_last_whs(GrpX,GrpY),
	    atom_concat(GrpY,' ',Grp),
	    atom_concat(' ',TitY,Tit),
	    append(Tmp,[Grp-Tit],T)
	; Info = [title(Chan)|_],
	    append(Tmp,[Chan],T)
	),
	fix_list(Xs,T,Ans).

print_ans(_,[]).
print_ans(N,[X|Xs]):-
	format("~w. ~w\n",[N,X]),
	N2 is N + 1,
	print_ans(N2,Xs).

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


:- init_player.
