:- expects_dialect(lps).

:- dynamic  located(Party, thirdCountry), 
    decisionPursuantToArticle_45_3(Event),  legallyBindingEnforceableInstrument(Process),
	bindingCorporateRules(Process),  standardDataProtectionClauses(Process),
	approvedCodeOfConduct(Process), approvedCertificateMechanism(Process).

permitted(Event) :- isa(Event, transfer(Enterprise, Data, Party) ),
    not decisionPursuantToArticle_45_3(Event),
	(   isa(Enterprise, controller); isa(Enterprise, processor)),
	isa(Data, personalData),
	(   located(Party, thirdCountry); isa(Party, internationalOrganisation)),
	partOf(Event, Process),
	appropriateSafeguards(Process), 
	enforceableDataSubjectRights(Process),
	legalRemediesForDataSubjects(Process).
 
appropriateSafeguards(Process) :- legallyBindingEnforceableInstrument(Process);
	bindingCorporateRules(Process);
	standardDataProtectionClauses(Process);
	approvedCodeOfConduct(Process);
	approvedCertificateMechanism(Process).  

isa(event001, transfer(companyA_Ireland, data, companyA_US) ).
isa(companyA_Ireland, controller).
isa(data, personalData).
located(companyA_US, thirdCountry).

partOf(event001, process001).
enforceableDataSubjectRights(process001).
legalRemediesForDataSubjects(process001).
% bindingCorporateRules(process001).


/** <examples>
?- permitted(event001).
?- 	appropriateSafeguards(Process).
*/
