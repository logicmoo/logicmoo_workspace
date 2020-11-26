--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Time:../../../Core:../../../Core/Shared/:../../../Core/System:../../../Core/User

concrete sys_domain_agenda_sem of  = sys_domain_agenda sharedDomainPro, systemCorePro  ** {

lin

-- Special Proposition

	recordingDay channel day start_time end_time = {s = "recording" ++ "(" ++
					"day" ++ "(" day.s ++ ")" ++ "," ++  
					"channel" ++ "(" ++ channel.s ++ ")" ++ "," ++ 
					"start_time" ++ "(" ++ start_time.s ++ ")" ++ "," ++ 
					"end_time" ++ "(" ++ end_time.s ++ ")" ++ ")"};

	recordingWeekday channel weekday start_time end_time = {s = "recording" ++ "(" ++
					"day" ++ "(" weekday.s ++ ")" ++ "," ++  
					"channel" ++ "(" ++ channel.s ++ ")" ++ "," ++ 
					"start_time" ++ "(" ++ start_time.s ++ ")" ++ "," ++ 
					"end_time" ++ "(" ++ end_time.s ++ ")" ++ ")"};
	
-- CONFIRMS!
	addedRecording = variants{["addRecording"] ; "addRecording_alone"};
	removedAll = ["removeAll"];

-- SYSTEMASK?

	whatDay = ["X ^ day ( X )"];
	whatStartTime = ["X ^ start_time ( X )"];
	whatEndTime = ["X ^ end_time ( X )"];
	whatChannel = ["X ^ channel ( X )"];
}