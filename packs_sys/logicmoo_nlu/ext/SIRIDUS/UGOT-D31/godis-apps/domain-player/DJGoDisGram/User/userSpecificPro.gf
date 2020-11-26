-- SWEDISH VERSION, UNCOMMENT AS NEEDED
-- --# -path=.:../:../DBase/Swedish:../DBase:../Numbers:../Shared

-- ENGLISH VERSION, UNCOMMENT AS NEEDED
--# -path=.:../:../DBase/English/:../DBase/:../Numbers:../Shared


concrete userSpecificPro of userSpecific = userGeneralPro, sharedSpecificPro ** {

flags conversion=finite;


lin
	answerSongArtistPlay song artist = { s = ["answer ( item ("] ++ song.s ++ [" ) ) ,"] ++ 
						["answer ( group ("] ++ artist.s ++ [") )"]};
	answerSongArtistAdd song artist = { s = ["answer ( item ("] ++ song.s ++ [" ) ) ,"] ++ 
						["answer ( group ("] ++ artist.s ++ [") )"]};
}