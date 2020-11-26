--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Media/English/:../../../Resource/Media/Swedish:../../../Resource/Numbers/:../../../Core:../../../Core/Shared/:../../../Core/System

concrete sys_domain_player_sem of sys_domain_player = sharedDomainPro, systemCorePro  ** {

flags conversion=finite;


lin
-- PROPOSITIONS

	songProp song = { s = "song" ++ "(" ++ song.s ++ ")" };
	itemProp song =  { s = "item" ++ "(" ++ song.s ++ ")" };
	currentSongProp song = { s = "current_song" ++ "(" ++ song.s ++ ")" };

	whatToPlayPropNum number = { s = "what_to_play" ++ "(" ++ number.s ++ ")" };
	whatToPlayPropOrd order = { s = "what_to_play" ++ "(" ++ order.s ++ ")" };

	itemRemPropNum number = { s = "itemRem" ++ "(" ++ number.s ++ ")" };
	itemRemPropOrd order = { s = "itemRem" ++ "(" ++ order.s ++ ")" };
	
	groupToAddProp artist = { s = "groupToAdd" ++ "(" ++ artist.s ++ ")" };
	artistProp artist = { s = "artist" ++ "(" ++ artist.s ++ ")"};
	groupProp artist = { s = "group" ++ "(" ++ artist.s ++ ")" };
	songArtistProp artist = { s = "song_artist" ++ "(" ++ artist.s ++ ")" };
	
	albumProp album = { s = "album" ++ "(" ++ album.s ++ ")" };

	artistsSongProp artist = { s = "artist_song" ++ "(" ++ artist.s ++ ")" };
	artistsAlbumProp artist = { s = "artists_album" ++ "(" ++ artist.s ++ ")" };

	albumArtistProp album = { s = "albums_by_artist" ++ "(" ++ album.s ++ ")" };

	songsArtistProp song = { s = "songs_by_artist" ++ "(" ++ song.s ++ ")" };

	stationProp station = { s = "station" ++ "(" ++ station.s ++ ")" };

	actionProp _ action = {s = "action" ++ "(" ++ action.s ++ ")"};

-- sort_restr( song(X) ):-          sem_sort(X,item).
-- sort_restr( item(X) ):-          sem_sort(X,item).
-- sort_restr( current_song(X) ):-  sem_sort(X,song).
-- sort_restr( what_to_play(X) ):-  sem_sort(X,index).
-- sort_restr( itemRem(X) ):-       sem_sort(X,index).
-- sort_restr( itemRem(X) ):-       sem_sort(X,index).
-- sort_restr( groupToAdd(X) ):-    group( X ).
-- sort_restr( artist(X) ):-        group( X ).
-- sort_restr( group(X) ):-         group( X ).
-- sort_restr( song_artist(X) ):-   group( X ).
-- sort_restr( album(X) ):-         album( X ).
-- sort_restr( artists_song(X) ):-  group_atom( X ).
-- sort_restr( artists_album(X) ):- group_atom( X ).
-- sort_restr( albums_by_artist(X) ):- album_atom( X ).
-- sort_restr( songs_by_artist(X) ):-  song_atom( X ).
-- sort_restr( station(X) ):-	 radio_station( X ).
-- sort_restr( year(X) ):-          sem_sort( X, year ).
-- sort_restr( path(X) ):-          atomic( X ).%,format("hallå: ~w\n",[X]).
-- %sort_restr( X^path(X) ):-          atomic( X ),format("hallå: ~w\n",[X]).
-- sort_restr( not path(X) ):-     format("hallå: ~w\n",[X]), atomic( X ).
-- sort_restr( fail(Path^path(Path),no_matches) ).

pattern
-- Asks
	whatSongQuestion = ["X ^ item ( X )"];
	whatArtistQuestion = ["X ^ group ( X )"];
	whatIndexQuestion = ["X ^ index ( X )"];
	whatToRemoveQuestion = ["X ^ song_to_remove ( X )"];
	whatStationQuestion = ["X ^ station ( X )"];
	whatAlbumQuestion = ["X ^ album ( X )"];
	whatToPlayQuestion = ["X ^ what_to_play ( X )"];
	whatToRemove = ["X ^ itemRem ( X )"];

	


-- Confirms

	addedToPlaylist = "playlist_add";    -- "The playlist is increased"
	removedFromPLaylist = "playlist_del"; -- "The playlist is reduced"
	clearedPlaylist = "playlist_clear";  -- "The playlist is cleared"
	turnedUpVolume = "vol_up";  -- "Turning up the volume"
	loweredVolume = "vol_down";  -- "Lowering the volume"
	startingThePlayer = variants {"start" ; "start_specific"}; 
	stoppingThePlayer = "stop"; -- "Stopping the music"
	pausingThePlayer = "pause"; -- "Pausing the music"
	resumingThePlayer = "resume"; -- "Resuming the music"
	shuffleTheList = "shuffle"; -- "The list has been shuffled"
	ffing = "fast_fowrward";
	rewinding = "rewind";
	handlingstations = "handle_stations";
	handlingplayer = "handle_player";
	handlingplaylist = "handle_playlist";
	showedList = "show_list";


}