--# -path=.:../:../../:../Shared/:../../../Resource/Home/:../../../Core:../../../Core/Shared/:../../../Core/User

concrete usr_domain_lamps_english of usr_domain_lamps = userCoreEng, sharedDomainEng ** { 

lin

	-- CompoundedAnswers
	-- CompoundedAnswers
	answerLampLocTurnOn lamp loc = {s = lamp.s ++ "in" ++ loc.s};
		
	answerLampLocTurnOff lamp loc = {s = lamp.s ++ "in" ++ loc.s};
}