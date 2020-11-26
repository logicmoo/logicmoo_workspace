-- SWEDISH VERSION, UNCOMMENT WHEN NEEDED
-- --# -path=.:../:../DBase/Swedish:../DBase:../Numbers:../Shared:../User

-- ENGLISH VERSION, UNCOMMENT WHEN NEEDED
--# -path=.:../:../DBase/English:../DBase:../Numbers:../Shared:../User

abstract userSpecific = userGeneral, sharedSpecific ** {


fun
	-- CompoundedAnswers

	answerSongArtistPlay : Song -> Artist -> AnswerList playTask;
	answerSongArtistAdd : Song -> Artist -> AnswerList addTask;


}