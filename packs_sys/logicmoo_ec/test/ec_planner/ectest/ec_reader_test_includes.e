


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/sorts.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sort rule,subject,object,action,ruleeffect,policy,policyset














; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/RulesPatterns/ruleOutput.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fluent F_RuleDenied(rule)
fluent F_RulePermitted(rule)

event Epermit(rule)
event EDeny(rule)

[rule,time] Initiates(EDeny(rule),F_RuleDenied(rule),time).
[rule,time] Initiates(Epermit(rule),F_RulePermitted(rule),time).


[rule] !HoldsAt(F_RulePermitted(rule),0).
[rule] !HoldsAt(F_RuleDenied(rule),0).












;[rule] HoldsAt(F_RulePermitted(rule),3) | HoldsAt(F_RuleDenied(rule),3).
;[rule,time] Happens(RuleDeny(rule), time) |  Happens(RulePermit(rule), time) -> time=2.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/RulesPatterns/targetHolds.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fluent F_TargetHolds(rule)
fluent F_TargetDoesntHolds(rule)

event E_MatchRuleParametters(rule)
event E_DontMatchRuleParametters(rule)

[rule,time] Initiates(E_MatchRuleParametters(rule),F_TargetHolds(rule),time).
[rule,time] Initiates(E_DontMatchRuleParametters(rule),F_TargetDoesntHolds(rule),time).

[rule,time] Happens(E_MatchRuleParametters(rule), time) -> !HoldsAt(F_TargetHolds(rule),time).
[rule,time] Happens(E_DontMatchRuleParametters(rule), time) -> !HoldsAt(F_TargetDoesntHolds(rule),time).


[rule] !HoldsAt(F_TargetHolds(rule),0).
[rule] !HoldsAt(F_TargetDoesntHolds(rule),0).



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/RulesPatterns/ConditionsVerification.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fluent F_ConditionSatisfied(rule)
;event E_ConditionSatisfied(rule)


;[rule,time] Initiates(E_ConditionSatisfied(rule),F_ConditionSatisfied(rule),time).

;[rule,time] Happens(E_ConditionSatisfied(rule),time) -> HoldsAt(F_TargetHolds(rule),time).

;[rule,time] Happens(E_ConditionSatisfied(rule), time) -> !HoldsAt(F_ConditionSatisfied(rule),time).


[rule] HoldsAt(F_ConditionSatisfied(rule),0).



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/RulesPatterns/ruleModel.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fluent F_RuleEffectPermitted(rule)
; prédéfinies
fluent F_RuleEffectNOTpermitted(rule) 
;prédéfinies


fluent F_RuleDenied(rule)
fluent F_RulePermitted(rule)
fluent F_RuleNotApplicable(rule)



event Epermit(rule)
event EDeny(rule)
event ERuleDoesNotApply(rule)


[rule,time] Initiates(EDeny(rule),F_RuleDenied(rule),time).
[rule,time] Initiates(Epermit(rule),F_RulePermitted(rule),time).
[rule,time] Initiates(ERuleDoesNotApply(rule),F_RuleNotApplicable(rule),time).


[rule,time] Happens(EDeny(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                            & HoldsAt(F_ConditionSatisfied(rule),time)
                                            & HoldsAt(F_RuleEffectNOTpermitted(rule),time).


[rule,time] Happens(Epermit(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                            & HoldsAt(F_ConditionSatisfied(rule),time)
                                            & HoldsAt(F_RuleEffectPermitted(rule),time).

[rule,time] Happens(ERuleDoesNotApply(rule),time) -> HoldsAt(F_TargetDoesntHolds(rule),time).



[rule] !HoldsAt(F_RulePermitted(rule),0).
[rule] !HoldsAt(F_RuleDenied(rule),0).
[rule] !HoldsAt(F_RuleNotApplicable(rule),0).




;********************************************************************************************************************
;--------------------------------------------------------------------------------------------------------------------
;********************************************************************************************************************

;[rule] HoldsAt(F_RulePermitted(rule),3) | HoldsAt(F_RuleDenied(rule),3).




;[rule,time] Happens(RuleDeny(rule), time) |  Happens(RulePermit(rule), time) -> time=2.

;[rule,time,ruleeffect] Happens(EDeny(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                                ;    & HoldsAt(F_ConditionSatisfied(rule),time)
                                                ;    & ruleeffect=Deny.


;[rule,time,ruleeffect] Happens(Epermit(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                               ;    & HoldsAt(F_ConditionSatisfied(rule),time)
                                               ;    & ruleeffect=Permit.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/ordering.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[rule,time] Happens(E_MatchRuleParametters(rule), time) | Happens(E_DontMatchRuleParametters(rule), time) -> time = 0.

[rule,time] Happens(EDeny(rule), time) | Happens(Epermit(rule), time) | Happens(ERuleDoesNotApply(rule), time) -> time = 1.

;[policy,time] Happens(E_policyPermit(policy), time) | Happens(E_policyDeny(policy), time) | Happens(E_PolicyDoesNotApply(policy),time) -> time = 2.


;[policyset,time] Happens(E_policysetPermit(policyset), time) | Happens(E_policysetDeny(policyset), time) | Happens(E_policysetDontApply(policyset),time) -> time = 3.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/PolicySetPatterns/policySetModel.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
event E_policysetPermit(policyset)
event E_policysetDeny(policyset)
event E_policysetDontApply(policyset)

fluent F_policysetPermitted(policyset)
fluent F_policysetDenied(policyset)
fluent F_policySetNotApplicable(policyset)

predicate PolicysetHaspolicies(policyset,policy)


[policyset,time] Initiates(E_policysetPermit(policyset),F_policysetPermitted(policyset),time).
[policyset,time] Initiates(E_policysetDeny(policyset),F_policysetDenied(policyset),time).
[policyset,time] Initiates(E_policysetDontApply(policyset),F_policySetNotApplicable(policyset),time).



; 'policies combaning algorithm (stategy) : All Permit'
[policyset,policy,time] Happens(E_policysetPermit(policyset),time) & PolicysetHaspolicies(policyset,policy) -> HoldsAt(F_policyPermitted(policy),time).


; 'policies combaning algorithm (stategy) : Deny override'
[policyset,time] Happens(E_policysetDeny(policyset),time) -> {policy}  PolicysetHaspolicies(policyset,policy) & HoldsAt(F_policyDenied(policy),time).


; 'policies combaning algorithm (stategy) : All Permit'
[policyset,policy,time] Happens(E_policysetDontApply(policyset),time) & PolicysetHaspolicies(policyset,policy) -> HoldsAt(F_policyNotApplicable(policy),time).


[policyset]!HoldsAt(F_policysetPermitted(policyset),0).
[policyset]!HoldsAt(F_policysetDenied(policyset),0).
[policyset]!HoldsAt(F_policySetNotApplicable(policyset),0).



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/PolicyPatterns/policyModel.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
event E_policyPermit(policy)
event E_policyDeny(policy)
event E_PolicyDoesNotApply(policy)

fluent F_policyPermitted(policy)
fluent F_policyDenied(policy)
fluent F_policyNotApplicable(policy)

predicate PolicyHasRules(policy,rule)


[policy,time] Initiates(E_policyPermit(policy),F_policyPermitted(policy),time).
[policy,time] Initiates(E_policyDeny(policy),F_policyDenied(policy),time).
[policy,time] Initiates(E_PolicyDoesNotApply(policy),F_policyNotApplicable(policy),time).



; 'Rule combaning algorithm (stategy) : All Permit'
[policy,rule,time] Happens(E_policyPermit(policy),time) & PolicyHasRules(policy,rule) -> HoldsAt(F_RulePermitted(rule),time).


; 'Rule combaning algorithm (stategy) : Deny override (s il existe au moin une règle satisfaite)'
[policy,time] Happens(E_policyDeny(policy),time) -> {rule}  PolicyHasRules(policy,rule) & HoldsAt(F_RuleDenied(rule),time).


; 'Rule combaning algorithm (stategy) : All not Applicable'
[policy,time,rule] Happens(E_PolicyDoesNotApply(policy),time) & PolicyHasRules(policy,rule) -> HoldsAt(F_RuleNotApplicable(rule),time).


[policy]!HoldsAt(F_policyPermitted(policy),0).
[policy]!HoldsAt(F_policyDenied(policy),0).
[policy]!HoldsAt(F_policyNotApplicable(policy),0).



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/input.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
subject Navas
object Gloves
action Get

