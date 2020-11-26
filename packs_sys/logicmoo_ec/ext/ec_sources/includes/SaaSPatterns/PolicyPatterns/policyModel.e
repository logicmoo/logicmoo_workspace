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
