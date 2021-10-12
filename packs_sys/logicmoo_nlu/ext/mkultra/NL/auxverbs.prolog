%%
%% Auxilliary verbs
%%

%% aux(+Gap, ?Polarity, ?Agreement, ?Tense, ?Aspect, ?Form, ?ModalLF)
%  An optional string of auxilliaries and/or negative particle.

:- randomizable aux/9, aux_without_do_support/9.
aux(nogap, affirmative, _Agreement, present, simple, simple, X^X) --> [ ].
aux(np(NP, Case, Agreement), affirmative, Agreement, Tense, simple, base, X^X) -->
   aux_do(Tense, Agreement),
   np(NP, Case, Agreement, nogap, nogap).
aux(Gap, negative, Agreement, Tense, simple, base, X^X) -->
   aux_do(Tense, Agreement),
   [ not ],
   aux_gap(Gap).
aux(Gap, Polarity, Agreement, Tense, Aspect, Form, ModalLF) -->
   aux_without_do_support(Gap, Polarity, Agreement, Tense, Aspect, Form, ModalLF).

:- register_lexical_item(not).

aux_without_do_support(Gap, Polarity, Agreement, future, Aspect, Form, X^X) -->
   [ will ],
   opt_not(Polarity),
   aux_gap(Gap),
   aux_aspect_future(Aspect, Agreement, Form).
aux_without_do_support(Gap, Polarity, _Agreement, present, simple, base, P^LF) -->
   [ ModalAux ],
   { modal_aux(ModalAux), LF =.. [ModalAux, P] },
   opt_not(Polarity),
   aux_gap(Gap).
aux_without_do_support(Gap, Polarity, Agreement, Tense, Aspect, Form, X^X) -->
   aux_aspect(Gap, Polarity, Tense, Aspect, Agreement, Form).

:- register_lexical_item(will).


%% aux_aspect_future(?Aspect, ?Agreement, ?Form)
%  An optional string of auxilliaries devoted to aspect (be, have), and/or negative particle.
%  Special case for after the auxilliary will.  Don't have to check for not particle.

:- randomizable aux_aspect_future//3.
aux_aspect_future(simple, _, simple) --> [ ].
aux_aspect_future(progressive, Agreement, present_participle) -->
   aux_be(future, Agreement).
aux_aspect_future(Aspect, Agreement, Form) -->
   aux_have(future, Agreement),
   aux_perfect(Aspect, Agreement, Form).

%% aux_aspect(+Gap, ?Polarity, ?Tense, ?Aspect, ?Agreement, ?Form)
%  An optional string of auxilliaries devoted to aspect (be, have), and/or negative particle.
%  Case where there has been no prior will auxilliary.

:- randomizable aux_aspect//5.
aux_aspect(nogap, affirmative, _, simple, _, simple) --> [ ].
aux_aspect(Gap, Polarity, Tense, progressive, Agreement, present_participle) -->
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   aux_gap(Gap).
aux_aspect(Gap, Polarity, Tense, Aspect, Agreement, Form) -->
   aux_have(Tense, Agreement),
   opt_not(Polarity),
   aux_gap(Gap),
   aux_perfect(Aspect, Agreement, Form).

%% aux_gap(+Gap)
%  Inserts NP for Gap here if need be
aux_gap(nogap) --> [ ].
aux_gap(np(LF, Case, Agreement)) -->
   np(LF, Case, Agreement, nogap, nogap).

:- randomizable aux_perfect//3.
aux_perfect(perfect, _Agreement, past_participle) -->
   [ ].
aux_perfect(perfect_progressive, Agreement, present_participle) -->
   aux_be(past, Agreement).

:- randomizable opt_not//1.
opt_not(affirmative) --> [ ].
opt_not(negative) --> [not].

%%%
%%% Lexical entries
%%%

:- randomizable(modal_aux/1).
modal_aux(can).
modal_aux(may).
modal_aux(should).
modal_aux(must).

:- register_all_lexical_items([A], modal_aux(A)).

:- randomizable aux_do//2.
aux_do(present, Agreement) -->
	[ do ],
	{ Agreement \== third:singular }.
aux_do(present, third:singular) -->
	[ does ].
aux_do(past, _Agreement) --> [did].

:- register_lexical_items([do, does, did, doing]).

:- randomizable aux_have//2.
aux_have(present, Agreement) -->
	[ have ],
	{ Agreement \== third:singular }.
aux_have(present, third:singular) -->
	[ has ].
aux_have(past, _Agreement) --> [had].
aux_have(future, _Agreement) --> [have].

:- register_lexical_items([have, has, had, having]).

:- randomizable aux_be//2.
aux_be(present, first:singular) -->
	[ am ].
aux_be(present, second:singular) -->
	[ are ].
aux_be(present, third:singular) -->
	[ is ].
aux_be(present, _:plural) -->
	[ are ].
aux_be(past, first:singular) -->
	[ was ].
aux_be(past, second:singular) -->
	[ were ].
aux_be(past, third:singular) -->
	[ was ].
aux_be(past, _:plural) -->
	[ were ].
aux_be(Tense, _Agreement) -->
   % Only applies if we already know this is future tense
   { Tense == future },
   [be].

:- register_lexical_items([am, are, is, was, were, be, being]).
