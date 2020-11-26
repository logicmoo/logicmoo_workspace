[rule,time] Happens(E_MatchRuleParametters(rule), time) | Happens(E_DontMatchRuleParametters(rule), time) -> time = 0.

[rule,time] Happens(EDeny(rule), time) | Happens(Epermit(rule), time) | Happens(ERuleDoesNotApply(rule), time) -> time = 1.

;[policy,time] Happens(E_policyPermit(policy), time) | Happens(E_policyDeny(policy), time) | Happens(E_PolicyDoesNotApply(policy),time) -> time = 2.


;[policyset,time] Happens(E_policysetPermit(policyset), time) | Happens(E_policysetDeny(policyset), time) | Happens(E_policysetDontApply(policyset),time) -> time = 3.
