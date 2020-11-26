
lf_rewrite([null,B], true).
lf_rewrite([passive_agent,A], true).

lf_rewrite([tense,A,[present,continuous]], [tense,A,present]).
lf_rewrite([tense,A,[present,perfect]], [tense,A,past]).

lf_rewrite([you,X], [you,X]).
lf_rewrite([one,C], [one,C]).
lf_rewrite([it,C], [thing,C]).
lf_rewrite([physical_object,C], [thing,C]).

lf_rewrite([stack_up,C,A,B,up], [stack_up,C,A,B]).

macro(be_or_sit_loc(LocPrepSense, LocPred),
      lf_rewrite([be, C, A, [LocPrepSense, B]],
		 [LocPred, C, A, B])).
macro(be_or_sit_loc(LocPrepSense, LocPred),
      lf_rewrite(([there_is,C,A], [LocPrepSense,C,B]),
		 [LocPred, C, A, B])).

@be_or_sit_loc(in_loc, be_in_loc).

lf_rewrite(([Card, C], [of, C, Suit], [club, Suit]),
	   [specific_card, C, [club, Card]]).
lf_rewrite(([Card, C], [of, C, Suit], [diamond, Suit]),
	   [specific_card, C, [diamond, Card]]).
lf_rewrite(([Card, C], [of, C, Suit], [heart, Suit]),
	   [specific_card, C, [heart, Card]]).
lf_rewrite(([Card, C], [of, C, Suit], [spade, Suit]),
	   [specific_card, C, [spade, Card]]).


