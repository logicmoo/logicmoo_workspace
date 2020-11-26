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
