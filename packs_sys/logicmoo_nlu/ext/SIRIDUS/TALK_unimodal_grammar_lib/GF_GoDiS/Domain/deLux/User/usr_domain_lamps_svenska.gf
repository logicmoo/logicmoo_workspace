--# -path=.:../:../../:../Shared/:../../../Resource/Home/:../../../Core:../../../Core/Shared/:../../../Core/User

concrete usr_domain_lamps_svenska of usr_domain_lamps = userCoreSwe, sharedDomainSwe ** { 

lin

	-- CompoundedAnswers
	answerLampLocTurnOn lamp loc = {s = lamp.s ++ "i" ++ loc.s};
		
	answerLampLocTurnOff lamp loc = {s = lamp.s ++ "i" ++ loc.s};
}