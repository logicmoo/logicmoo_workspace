--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Time:../../../Core:../../../Core/Shared/:../../../Core/System:../../../Core/User

concrete sharedDomainSwe of sharedDomain = sharedCoreSwe, DBSwe ** open SpecResSwe in{

lin
	-- ANSWERS


	makeStartTimeAnswer time = {s = "från" ++ time.s};
	makeEndTimeAnswer time = {s = "till" ++ time.s};
	makeTVStationAnswer station = {s = "på" ++ station.s};
	makeDayAnswer day = {s = day.s};
	makeWeekdayAnswer weekday = {s = "på" ++ weekday.s};

-- LEXICON

pattern
	addRecording = variants {["lägga till en inspelning"]};
	addRecording_alone = variants {["lägga till en inspelning"]};

	removeAll = ["ta bort mina inspelningar"];

	checkup = ["vilka inspelningar har jag"];	

}