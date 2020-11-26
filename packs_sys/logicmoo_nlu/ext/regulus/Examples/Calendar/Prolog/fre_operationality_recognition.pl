
% Operationality for French/Catalan recognition
%
% (Simple NP == NP which does not dominate PP)

/*

There is a tricky point here concerning the extent to which we generalise
PP structure:

a) If we choose to make VP and PP operational under PP/OPTIONAL_PP, then
the specialised grammar generalises well, but we can get attachment ambiguities.

b) If we don't allow these constituents to be operational, then we need a lot
more training data to capture the range of combinations of subordinate clause
and PP.

The example that first revealed this problem:

    avez vous de la fièvre quand vous avez mal à la tête

where the issue is whether "à la tête" attaches to "avoir mal" or "avoir de la fièvre".

*/
                       
change_rule_and_context(_Context, med_utterance) :-
	cat(med_utterance),
	\+ gap.

change_rule_and_context(med_utterance, vp) :-
	cat(vp),
	\+ gap.
change_rule_and_context(med_utterance, np) :-
	cat(np),
	\+ gap,
	( dominates(pp) ; dominates(de_pp) ).
change_rule_and_context(med_utterance, simple_np) :-
	cat(np),
	\+ gap,
	\+ dominates(pp), \+ dominates(de_pp).
change_rule_and_context(med_utterance, pp) :-
	cat(pp),
	\+ gap.
change_rule_and_context(med_utterance, optional_pp) :-
	cat(optional_pp).
change_rule_and_context(med_utterance, vbar) :-
	cat(vbar),
	\+ gap.

change_rule_and_context(vp, np) :-
	cat(np),
	\+ gap,
	( dominates(pp) ; dominates(de_pp) ).
change_rule_and_context(vp, simple_np) :-
	cat(np),
	\+ gap,
	\+ dominates(pp), \+ dominates(de_pp).
change_rule_and_context(vp, pp) :-
	cat(pp),
	\+ gap.
change_rule_and_context(vp, optional_pp) :-
	cat(optional_pp).
change_rule_and_context(vp, vbar) :-
	cat(vbar),
	\+ gap.

change_rule_and_context(np, simple_np) :-
	cat(np),
	\+ gap,
	( dominates(pp) ; dominates(de_pp) ).
change_rule_and_context(np, pp) :-
	cat(pp),
	\+ gap.
change_rule_and_context(np, de_pp) :-
	cat(de_pp).

change_rule_and_context(pp, vp) :-
	cat(vp),
	\+ gap.
change_rule_and_context(pp, pp) :-
	cat(pp),
	\+ gap.
change_rule_and_context(pp, np) :-
	cat(np),
	\+ gap,
	( dominates(pp) ; dominates(de_pp) ).
change_rule_and_context(pp, simple_np) :-
	cat(np),
	\+ gap,
	\+ dominates(pp), \+ dominates(de_pp).

change_rule_and_context(de_pp, simple_np) :-
	cat(np),
	\+ dominates(pp), \+ dominates(de_pp).

change_rule_and_context(optional_pp, pp) :-
	cat(pp),
	\+ gap.

%change_rule_and_context(vbar, vbar) :-
%	cat(vbar),
%	\+ gap.
change_rule_and_context(vbar, np) :-
	cat(np),
	\+ gap,
	( dominates(pp) ; dominates(de_pp) ).
change_rule_and_context(vbar, simple_np) :-
	cat(np),
	\+ gap,
	\+ dominates(pp), \+ dominates(de_pp).
change_rule_and_context(vbar, optional_adverb) :-
	cat(optional_adverb).

change_rule_and_context(_Context, lexical) :-
	lexical.

