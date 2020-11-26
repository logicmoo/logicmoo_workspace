--# -path=.:../:../../:../Shared/:../../../Resource/Home/:../../../Core:../../../Core/Shared/:../../../Core/System

--# -path=.:../:../DBase/:../Shared


concrete sys_domain_lamps_english of sys_domain_lamps = sharedDomainEng, systemCoreEng ** {


lin

-- PROPOSITIONS

	lampProp lamp = { s = lamp.s };
	locProp loc =  { s = loc.s };

	whatToTurnOffProp lamp = { s = lamp.s };
	whatToTurnOnProp lamp = { s = lamp.s };

pattern

-- Asks

	whatLampQuestion = ["what lamp do you mean"];
	whatLocQuestion = ["what room do you mean"];

-- Confirms

	turnedOnLamp = ["the light is on"];
	turnedOffLamp = ["the light is turned off"];
}


