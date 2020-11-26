--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Time:../../../Core:../../../Core/Shared/:../../../Core/System:../../../Core/User

concrete sys_domain_agenda_english of sys_domain_agenda = sharedDomainEng, systemCoreEng ** {

lin

-- Special Proposition

	recordingDay channel day start_time end_time = {s = day.s ++ 
					"on" ++ channel.s ++ "from" ++
					start_time.s ++ "to" ++ 
					end_time.s}; 

	recordingWeekday channel weekday start_time end_time = {s = "on" ++ weekday.s ++ 
					"on" ++ channel.s ++ "from" ++
					start_time.s ++ "to" ++ 
					end_time.s}; 
	
pattern
-- CONFIRMS!
	addedRecording = ["added a recording"];
	removedAll = ["removed all planned recordings"];

-- SYSTEMASK?

	whatDay = ["what day do you want to record on"];
	whatStartTime = ["at what time should the recording start"];
	whatEndTime = ["when do you want the recording to end"];
	whatChannel = ["what channel do you want to record"];

}


