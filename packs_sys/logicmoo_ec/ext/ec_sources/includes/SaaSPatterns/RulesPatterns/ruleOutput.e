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
