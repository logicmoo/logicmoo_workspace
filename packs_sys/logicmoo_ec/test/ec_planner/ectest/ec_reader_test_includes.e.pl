:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_foundations.e',262).
:- call_pel_directive(translate(unskipped,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(ecalc).
:- call_pel_directive(translate(begining,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e.pl')).
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: includes/SaaSPatterns/sorts.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',7).
% sort rule,subject,object,action,ruleeffect,policy,policyset
sort(rule).
sort(subject).
sort(object).
sort(action).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',7).
sort(ruleeffect).
sort(policy).
sort(policyset).
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: includes/SaaSPatterns/RulesPatterns/ruleOutput.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',25).
% fluent F_RuleDenied(rule)
fluent(f_ruleDenied(rule)).

% fluent F_RulePermitted(rule)
fluent(f_rulePermitted(rule)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',28).
% event Epermit(rule)
event(epermit(rule)).

% event EDeny(rule)
event(eDeny(rule)).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',31).
% [rule,time]
 % Initiates(EDeny(rule),F_RuleDenied(rule),time).
initiates_at(eDeny(Rule),f_ruleDenied(Rule),Time).


% [rule,time]
 % Initiates(Epermit(rule),F_RulePermitted(rule),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',31).
initiates_at(epermit(Rule),
	     f_rulePermitted(Rule),
	     Time).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',35).
% [rule]
 % !HoldsAt(F_RulePermitted(rule),0).
holds_at(not(f_rulePermitted(Rule)),0).


% [rule]
 % !HoldsAt(F_RuleDenied(rule),0).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',35).
holds_at(not(f_ruleDenied(Rule)),0).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',49).
%;[rule] HoldsAt(F_RulePermitted(rule),3) | HoldsAt(F_RuleDenied(rule),3).
%;[rule,time] Happens(RuleDeny(rule), time) |  Happens(RulePermit(rule), time) -> time=2.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: includes/SaaSPatterns/RulesPatterns/targetHolds.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',57).
% fluent F_TargetHolds(rule)
fluent(f_targetHolds(rule)).

% fluent F_TargetDoesntHolds(rule)
fluent(f_targetDoesntHolds(rule)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',60).
% event E_MatchRuleParametters(rule)
event(e_matchRuleParametters(rule)).

% event E_DontMatchRuleParametters(rule)
event(e_dontMatchRuleParametters(rule)).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',63).
% [rule,time]
 % Initiates(E_MatchRuleParametters(rule),F_TargetHolds(rule),time).
initiates_at(e_matchRuleParametters(Rule),
	     f_targetHolds(Rule),
	     Time).


% [rule,time]
 % Initiates(E_DontMatchRuleParametters(rule),F_TargetDoesntHolds(rule),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',63).
initiates_at(e_dontMatchRuleParametters(Rule),
	     f_targetDoesntHolds(Rule),
	     Time).


% [rule,time]
 % Happens(E_MatchRuleParametters(rule), time) -> !HoldsAt(F_TargetHolds(rule),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',65).
happens_at(e_matchRuleParametters(Rule), Time) ->
    holds_at(not(f_targetHolds(Rule)), Time).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',67).
% [rule,time]
 % Happens(E_DontMatchRuleParametters(rule), time) -> !HoldsAt(F_TargetDoesntHolds(rule),time).
happens_at(e_dontMatchRuleParametters(Rule), Time) ->
    holds_at(not(f_targetDoesntHolds(Rule)), Time).


% [rule]
 % !HoldsAt(F_TargetHolds(rule),0).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',69).
holds_at(not(f_targetHolds(Rule)),0).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',71).
% [rule]
 % !HoldsAt(F_TargetDoesntHolds(rule),0).
holds_at(not(f_targetDoesntHolds(Rule)),0).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',75).
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: includes/SaaSPatterns/RulesPatterns/ConditionsVerification.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',78).
% fluent F_ConditionSatisfied(rule)
fluent(f_conditionSatisfied(rule)).


%;event E_ConditionSatisfied(rule)
%;[rule,time] Initiates(E_ConditionSatisfied(rule),F_ConditionSatisfied(rule),time).
%;[rule,time] Happens(E_ConditionSatisfied(rule),time) -> HoldsAt(F_TargetHolds(rule),time).
%;[rule,time] Happens(E_ConditionSatisfied(rule), time) -> !HoldsAt(F_ConditionSatisfied(rule),time).
% [rule]
 % HoldsAt(F_ConditionSatisfied(rule),0).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',88).
holds_at(f_conditionSatisfied(Rule),0).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: includes/SaaSPatterns/RulesPatterns/ruleModel.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',96).
% fluent F_RuleEffectPermitted(rule)
fluent(f_ruleEffectPermitted(rule)).


%; prédéfinies

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',98).
% fluent F_RuleEffectNOTpermitted(rule) 
fluent(f_ruleEffectNOTpermitted(rule)).


%;prédéfinies

% fluent F_RuleDenied(rule)
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',101).
fluent(f_ruleDenied(rule)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',103).
% fluent F_RulePermitted(rule)
fluent(f_rulePermitted(rule)).

% fluent F_RuleNotApplicable(rule)
fluent(f_ruleNotApplicable(rule)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',108).
% event Epermit(rule)
event(epermit(rule)).

% event EDeny(rule)
event(eDeny(rule)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',110).
% event ERuleDoesNotApply(rule)
event(eRuleDoesNotApply(rule)).


% [rule,time]
 % Initiates(EDeny(rule),F_RuleDenied(rule),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',112).
initiates_at(eDeny(Rule),f_ruleDenied(Rule),Time).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',114).
% [rule,time]
 % Initiates(Epermit(rule),F_RulePermitted(rule),time).
initiates_at(epermit(Rule),
	     f_rulePermitted(Rule),
	     Time).


% [rule,time]
 % Initiates(ERuleDoesNotApply(rule),F_RuleNotApplicable(rule),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',114).
initiates_at(eRuleDoesNotApply(Rule),
	     f_ruleNotApplicable(Rule),
	     Time).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',118).
% [rule,time]
 % Happens(EDeny(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
%                                             & HoldsAt(F_ConditionSatisfied(rule),time)
%                                             & HoldsAt(F_RuleEffectNOTpermitted(rule),time).
happens_at(eDeny(Rule), Time) ->
    holds_at(f_targetHolds(Rule), Time),
    holds_at(f_conditionSatisfied(Rule), Time),
    holds_at(f_ruleEffectNOTpermitted(Rule), Time).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',123).
% [rule,time]
 % Happens(Epermit(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
%                                             & HoldsAt(F_ConditionSatisfied(rule),time)
%                                             & HoldsAt(F_RuleEffectPermitted(rule),time).
happens_at(epermit(Rule), Time) ->
    holds_at(f_targetHolds(Rule), Time),
    holds_at(f_conditionSatisfied(Rule), Time),
    holds_at(f_ruleEffectPermitted(Rule), Time).


% [rule,time]
 % Happens(ERuleDoesNotApply(rule),time) -> HoldsAt(F_TargetDoesntHolds(rule),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',126).
happens_at(eRuleDoesNotApply(Rule), Time) ->
    holds_at(f_targetDoesntHolds(Rule), Time).


% [rule]
 % !HoldsAt(F_RulePermitted(rule),0).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',130).
holds_at(not(f_rulePermitted(Rule)),0).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',132).
% [rule]
 % !HoldsAt(F_RuleDenied(rule),0).
holds_at(not(f_ruleDenied(Rule)),0).


% [rule]
 % !HoldsAt(F_RuleNotApplicable(rule),0).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',132).
holds_at(not(f_ruleNotApplicable(Rule)),0).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',138).
%;********************************************************************************************************************
%;--------------------------------------------------------------------------------------------------------------------
%;********************************************************************************************************************
%;[rule] HoldsAt(F_RulePermitted(rule),3) | HoldsAt(F_RuleDenied(rule),3).
%;[rule,time] Happens(RuleDeny(rule), time) |  Happens(RulePermit(rule), time) -> time=2.
%;[rule,time,ruleeffect] Happens(EDeny(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                                %;    & HoldsAt(F_ConditionSatisfied(rule),time)
                                                %;    & ruleeffect=Deny.
%;[rule,time,ruleeffect] Happens(Epermit(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                               %;    & HoldsAt(F_ConditionSatisfied(rule),time)
                                               %;    & ruleeffect=Permit.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: includes/SaaSPatterns/ordering.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% [rule,time]
 
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',163).
% Happens(E_MatchRuleParametters(rule), time) | Happens(E_DontMatchRuleParametters(rule), time) -> time = 0.
happens_at(e_matchRuleParametters(Rule), Time);happens_at(e_dontMatchRuleParametters(Rule), Time) ->
    Time=0.


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',165).
% [rule,time]
 % Happens(EDeny(rule), time) | Happens(Epermit(rule), time) | Happens(ERuleDoesNotApply(rule), time) -> time = 1.
happens_at(eDeny(Rule), Time);happens_at(epermit(Rule), Time);happens_at(eRuleDoesNotApply(Rule), Time) ->
    Time=1.


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',167).
%;[policy,time] Happens(E_policyPermit(policy), time) | Happens(E_policyDeny(policy), time) | Happens(E_PolicyDoesNotApply(policy),time) -> time = 2.
%;[policyset,time] Happens(E_policysetPermit(policyset), time) | Happens(E_policysetDeny(policyset), time) | Happens(E_policysetDontApply(policyset),time) -> time = 3.
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: includes/SaaSPatterns/PolicySetPatterns/policySetModel.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',177).
% event E_policysetPermit(policyset)
event(e_policysetPermit(policyset)).

% event E_policysetDeny(policyset)
event(e_policysetDeny(policyset)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',179).
% event E_policysetDontApply(policyset)
event(e_policysetDontApply(policyset)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',181).
% fluent F_policysetPermitted(policyset)
fluent(f_policysetPermitted(policyset)).

% fluent F_policysetDenied(policyset)
fluent(f_policysetDenied(policyset)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',183).
% fluent F_policySetNotApplicable(policyset)
fluent(f_policySetNotApplicable(policyset)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',185).
% predicate PolicysetHaspolicies(policyset,policy)
predicate(policysetHaspolicies(policyset,policy)).


% [policyset,time]
 % Initiates(E_policysetPermit(policyset),F_policysetPermitted(policyset),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',187).
initiates_at(e_policysetPermit(Policyset),
	     f_policysetPermitted(Policyset),
	     Time).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',189).
% [policyset,time]
 % Initiates(E_policysetDeny(policyset),F_policysetDenied(policyset),time).
initiates_at(e_policysetDeny(Policyset),
	     f_policysetDenied(Policyset),
	     Time).


% [policyset,time]
 % Initiates(E_policysetDontApply(policyset),F_policySetNotApplicable(policyset),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',189).
initiates_at(e_policysetDontApply(Policyset),
	     f_policySetNotApplicable(Policyset),
	     Time).


%; 'policies combaning algorithm (stategy) : All Permit'
% [policyset,policy,time]
 % Happens(E_policysetPermit(policyset),time) & PolicysetHaspolicies(policyset,policy) -> HoldsAt(F_policyPermitted(policy),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',195).
happens_at(e_policysetPermit(Policyset), Time), policysetHaspolicies(Policyset, Policy) ->
    holds_at(f_policyPermitted(Policy), Time).


%; 'policies combaning algorithm (stategy) : Deny override'
% [policyset,time]
 % Happens(E_policysetDeny(policyset),time) -> {policy}  PolicysetHaspolicies(policyset,policy) & HoldsAt(F_policyDenied(policy),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',199).
happens_at(e_policysetDeny(Policyset), Time) ->
    exists([Policy],
            (policysetHaspolicies(Policyset, Policy), holds_at(f_policyDenied(Policy), Time))).


%; 'policies combaning algorithm (stategy) : All Permit'
% [policyset,policy,time]
 % Happens(E_policysetDontApply(policyset),time) & PolicysetHaspolicies(policyset,policy) -> HoldsAt(F_policyNotApplicable(policy),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',203).
happens_at(e_policysetDontApply(Policyset), Time), policysetHaspolicies(Policyset, Policy) ->
    holds_at(f_policyNotApplicable(Policy), Time).


% [policyset]
% !HoldsAt(F_policysetPermitted(policyset),0).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',205).
holds_at(not(f_policysetPermitted(Policyset)),0).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',207).
% [policyset]
% !HoldsAt(F_policysetDenied(policyset),0).
holds_at(not(f_policysetDenied(Policyset)),0).


% [policyset]
% !HoldsAt(F_policySetNotApplicable(policyset),0).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',207).
holds_at(not(f_policySetNotApplicable(Policyset)),0).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: includes/SaaSPatterns/PolicyPatterns/policyModel.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',215).
% event E_policyPermit(policy)
event(e_policyPermit(policy)).

% event E_policyDeny(policy)
event(e_policyDeny(policy)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',217).
% event E_PolicyDoesNotApply(policy)
event(e_policyDoesNotApply(policy)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',219).
% fluent F_policyPermitted(policy)
fluent(f_policyPermitted(policy)).

% fluent F_policyDenied(policy)
fluent(f_policyDenied(policy)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',221).
% fluent F_policyNotApplicable(policy)
fluent(f_policyNotApplicable(policy)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',223).
% predicate PolicyHasRules(policy,rule)
predicate(policyHasRules(policy,rule)).


% [policy,time]
 % Initiates(E_policyPermit(policy),F_policyPermitted(policy),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',225).
initiates_at(e_policyPermit(Policy),
	     f_policyPermitted(Policy),
	     Time).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',227).
% [policy,time]
 % Initiates(E_policyDeny(policy),F_policyDenied(policy),time).
initiates_at(e_policyDeny(Policy),
	     f_policyDenied(Policy),
	     Time).


% [policy,time]
 % Initiates(E_PolicyDoesNotApply(policy),F_policyNotApplicable(policy),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',227).
initiates_at(e_policyDoesNotApply(Policy),
	     f_policyNotApplicable(Policy),
	     Time).


%; 'Rule combaning algorithm (stategy) : All Permit'
% [policy,rule,time]
 % Happens(E_policyPermit(policy),time) & PolicyHasRules(policy,rule) -> HoldsAt(F_RulePermitted(rule),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',233).
happens_at(e_policyPermit(Policy), Time), policyHasRules(Policy, Rule) ->
    holds_at(f_rulePermitted(Rule), Time).


%; 'Rule combaning algorithm (stategy) : Deny override (s il existe au moin une règle satisfaite)'
% [policy,time]
 % Happens(E_policyDeny(policy),time) -> {rule}  PolicyHasRules(policy,rule) & HoldsAt(F_RuleDenied(rule),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',237).
happens_at(e_policyDeny(Policy), Time) ->
    exists([Rule],
            (policyHasRules(Policy, Rule), holds_at(f_ruleDenied(Rule), Time))).


%; 'Rule combaning algorithm (stategy) : All not Applicable'
% [policy,time,rule]
 % Happens(E_PolicyDoesNotApply(policy),time) & PolicyHasRules(policy,rule) -> HoldsAt(F_RuleNotApplicable(rule),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',241).
happens_at(e_policyDoesNotApply(Policy), Time), policyHasRules(Policy, Rule) ->
    holds_at(f_ruleNotApplicable(Rule), Time).


% [policy]
% !HoldsAt(F_policyPermitted(policy),0).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',243).
holds_at(not(f_policyPermitted(Policy)),0).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',245).
% [policy]
% !HoldsAt(F_policyDenied(policy),0).
holds_at(not(f_policyDenied(Policy)),0).


% [policy]
% !HoldsAt(F_policyNotApplicable(policy),0).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',245).
holds_at(not(f_policyNotApplicable(Policy)),0).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: includes/SaaSPatterns/input.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',253).
% subject Navas
t(subject,navas).

% object Gloves
t(object,gloves).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',255).
% action Get
t(action,get).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e',257).
:- call_pel_directive(translate(ending,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/ec_reader_test_includes.e.pl')).
