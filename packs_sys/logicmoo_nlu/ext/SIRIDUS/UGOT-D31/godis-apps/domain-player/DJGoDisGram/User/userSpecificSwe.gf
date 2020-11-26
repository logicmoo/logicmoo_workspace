--# -path=.:../:../DBase/Swedish:../DBase:../Numbers:../Shared


concrete userSpecificSwe of userSpecific = userGeneralSwe, sharedSpecificSwe ** { 

flags conversion=finite;


lin

	-- CompoundedAnswers
	answerSongArtistPlay song artist = {s = variants {(song.s ++ "med" ++ artist.s)
					; (artist.s ++ "med" ++ song.s)} };
		
	answerSongArtistAdd song artist = {s = variants {(song.s ++ "med" ++ artist.s)
					; (artist.s ++ "med" ++ song.s)} };
		

pattern
	askArtist = variants { ["vad har jag"] ; ["vilka låtar har jag"] ; ["har jag någonting"]}
					++ variants {"med" ; "av"};


	askSong = ["vem har"] ++ variants {"skrivit"; "gjort"};

	askCurrent = ["vad heter"] ++ variants {["den här"] ; ["låten som spelas nu"]};


}