fluent F_RuleEffectPermitted(rule); prédéfinies
fluent F_RuleEffectNOTpermitted(rule) ;prédéfinies


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
