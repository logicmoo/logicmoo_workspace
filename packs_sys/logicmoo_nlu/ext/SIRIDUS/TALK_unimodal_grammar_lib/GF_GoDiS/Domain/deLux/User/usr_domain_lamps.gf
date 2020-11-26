--# -path=.:../:../../:../Shared/:../../../Resource/Home/:../../../Core:../../../Core/Shared/:../../../Core/User

abstract usr_domain_lamps = userCore, sharedDomain ** {


fun
	-- CompoundedAnswers

	answerLampLocTurnOn : Lamp -> Room -> AnswerList onTask;
	answerLampLocTurnOff : Lamp -> Room -> AnswerList offTask;


}