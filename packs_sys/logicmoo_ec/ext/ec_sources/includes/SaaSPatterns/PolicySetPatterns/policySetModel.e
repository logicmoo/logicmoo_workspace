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
