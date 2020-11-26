--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Time:../../../Core:../../../Core/Shared/:../../../Core/System:../../../Core/User

concrete sharedDomainPro of sharedDomain = sharedCorePro, DBPro ** {

flags lexer=code ; unlexer=code ;

lin

	-- ANSWERS
	makeStartTimeAnswer time = {s = "answer" ++ "(" ++ "start_time" ++ "(" ++ time.s ++ ")" ++ ")"};
	makeEndTimeAnswer time = {s = "answer" ++ "(" ++ "stop_time" ++ "(" ++ time.s ++ ")" ++ ")"};
	makeChannelAnswer channel = {s = "answer" ++ "(" ++ "channel" ++ "(" ++ channel.s ++ ")" ++ ")"};
	makeDayAnswer day = {s = "answer" ++ "(" ++ "day" ++ "(" ++ day.s ++ ")" ++ ")"};
	makeWeekdayAnswer weekday = {s = "answer" ++ "(" ++ "day" ++ "(" ++ weekday.s ++ ")" ++ ")"};
-- LEXICON

pattern
	addEntry = variants {["add_entry"]};
	addEntry_alone = "add_entry";

	removeAll = "delete";

	checkup = ["checkup"];	

}