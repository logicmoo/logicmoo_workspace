--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Time:../../../Core:../../../Core/Shared/:../../../Core/System:../../../Core/User

concrete sharedDomainEng of sharedDomain = sharedCoreEng, DBEng ** 
		open SpecResEng in {
lin
	-- ANSWERS


	makeStartTimeAnswer time = {s = "from" ++ time.s};
	makeEndTimeAnswer time = {s = "to" ++ time.s};
	makeTVStationAnswer station = {s = "on" ++ station.s};
	makeDayAnswer day = {s = day.s};
	makeWeekdayAnswer weekday = {s = "on" ++ weekday.s};

-- LEXICON

pattern
	addRecording = variants {["add a recording"]};
	addRecording_alone = variants {["add a recording"]};

	removeAll = ["remove my recordings"];

	checkup = ["what recordings do i have"];	
}








