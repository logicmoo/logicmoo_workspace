--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Time:../../../Core:../../../Core/Shared/:../../../Core/System:../../../Core/User

abstract sys_domain_agenda = sharedDomain, systemCore ** {


fun

-- Special Proposition

	recordingDay : TVStation -> Day -> Time -> Time -> Proposition addTask;
	recordingWeekday : TVStation -> Weekday -> Time -> Time -> Proposition addTask;
	
-- CONFIRMS!
	addedRecording : Confirm;
	removedAll : Confirm;



-- SYSTEMASK?

	whatDay : SingleAsk;
	whatStartTime : SingleAsk;
	whatEndTime : SingleAsk;
	whatChannel : SingleAsk;
}







