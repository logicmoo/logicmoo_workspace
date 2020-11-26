--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Time:../../../Core:../../../Core/Shared/:../../../Core/System:../../../Core/User

concrete sys_domain_agenda_svenska of  sys_domain_agenda = sharedDomainSwe, systemCoreSwe ** {

lin 

-- Special Proposition

	recordingDay channel day start_time end_time = {s = day.s ++ 
					"på" ++ channel.s ++ "från" ++
					start_time.s ++ "till" ++ 
					end_time.s}; 
	recordingWeekday channel weekday start_time end_time = {s = "på" ++ weekday.s ++ 
					"på" ++ channel.s ++ "från" ++
					start_time.s ++ "till" ++ 
					end_time.s}; 
	

pattern
-- CONFIRMS!
	addedRecording = ["lade till en inspelning"];
	removedAll = ["raderade alla plannerade inspelningar"];

-- SYSTEMASK?

	whatDay = ["vilken dag vill du spela in på"];
	whatStartTime = ["vilken tid ska inspeningen starta"];
	whatEndTime = ["när ska inspelningen sluta"];
	whatChannel = ["vilken kanal vill du spela in"];
}


