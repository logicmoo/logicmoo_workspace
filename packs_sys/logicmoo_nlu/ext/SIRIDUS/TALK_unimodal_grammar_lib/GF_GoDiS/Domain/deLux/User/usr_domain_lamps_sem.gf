--# -path=.:../:../../:../Shared/:../../../Resource/Home/:../../../Core:../../../Core/Shared/:../../../Core/User

concrete usr_domain_lamps_sem of usr_domain_lamps = userCorePro, sharedDomainPro ** {

lin

	-- CompoundedAnswers
	answerLampLocTurnOn lamp loc = {s = "answer(lamp(" ++ lamp.s ++ ")," ++ 
					"answer(loc(" ++ loc.s ++ ")"};
		
	answerLampLocTurnOff lamp loc = {s = "answer(lamp(" ++ lamp.s ++ ")," ++ 
					"answer(loc(" ++ loc.s ++ ")"};

}