fluent F_ConditionSatisfied(rule)
;event E_ConditionSatisfied(rule)


;[rule,time] Initiates(E_ConditionSatisfied(rule),F_ConditionSatisfied(rule),time).

;[rule,time] Happens(E_ConditionSatisfied(rule),time) -> HoldsAt(F_TargetHolds(rule),time).

;[rule,time] Happens(E_ConditionSatisfied(rule), time) -> !HoldsAt(F_ConditionSatisfied(rule),time).


[rule] HoldsAt(F_ConditionSatisfied(rule),0).
