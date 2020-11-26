possibleResponse(whatIf(not(receive(andrewDougherty,foodStamps))),getJobImmediately).

planForContingency(powerOutageTo1125KnocksOutPowerToFLPSite).

planForContingency(whatIf(not(receive(andrewDougherty,foodStamps)))).
%% Find out about foodstamps by the end of this month.  do I find out
%% by a call or by a letter.  Either phone, snail mail or email.

planForContingency(whatHappensIfIDontGetDisability).
%% howLongIsFamilyWillingToHelpMeWhileIAmWaitingToFindOutAboutDisability.

possibleResponse(whatIf(not(receive(andrewDougherty,disability))),getJobImmediately).

suggests(meredithMcGhan,possibleResponse(whatHappensIfIDontGetDisability,moveToMichigan)).

%% hasConsequence(getJobImmediately,'makes it harder to get disability').

planForContingency(whatIf(evaLosesHerJob)).
planForContingency(whatIf(evaMovesBackInWithUs)).

%% look into using Cyc #$Situation s to describe these things

%% ("This #$Microtheory contains rules about relations #$mtPrecedes and \n#$mtAlternatives between instances of \n#$ReasoningWithMultiFutureMicrotheory, and rules concerning truth \nconditions of formulas in those instances.")


planForContingency(whatIf(has(Person,bloodSugarAttack))) :-
	isa(Person,person).

planForContingency(whatIf(has(Person,hospitalization))) :-
	isa(Person,person).

planForContingency(whatIf(has(Person,physicalFall))) :-
	isa(Person,person).

%% clean up after biological

planForContingency(whatIf((hasWithDrawal(Withdrawal,BankAccount),unexpected(Withdrawal)))) :-
	isa(BankAccount,backAccount).

planForContingency(whatIf(intervenes(Situation,God))) :-
	isa(God,god),
	hasSituation(Situation).

planForContingency(whatIf('(come up with plan for if no one to help Andy, Andy sick, Andy can not take care of Mom, Mom can not take care of Andy)')).
