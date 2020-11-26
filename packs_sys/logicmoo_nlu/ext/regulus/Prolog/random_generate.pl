
% random_generate.pl

%---------------------------------------------------------------

:- module(random_generate,
	  [random_generate_words/2,
	   random_generate_atom_list/3,
	   random_generate_and_print_atom_list/3]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(timeout)).

%----------------------------------------------------------------------

% Max amount of time to spend trying to generate a sentence, in seconds.
generation_time_limit(1).

%----------------------------------------------------------------------

random_generate_words(Words, MaxDepth) :-
	generation_time_limit(TimeLimit),
	TimeLimitInMilliseconds is integer( 1000 * TimeLimit ),
	
	time_out(random_generate_words1(Words, MaxDepth),
		 TimeLimitInMilliseconds,
		 Result),
	
	(   Result = time_out ->
	    
	    fail ;

	    true
	).	

random_generate_words1(Words, MaxDepth) :-
	random_pick_top_category(Cat),
	arg(6, Cat, []),
	random_generate_from_category(Cat, MaxDepth),
	arg(5, Cat, Words),
	!.

random_generate_atom_list(N, List, _MaxDepth) :-
	N < 1,
	List = [],
	!.
random_generate_atom_list(N, List, MaxDepth) :-
	N >= 1,
	(   random_generate_words(Words, MaxDepth) ->
	    
	    join_with_spaces(Words, F),
	    format('.', []),
	    N1 is N - 1,
	    List = [F | R] ;

	    format('-', []),
	    N1 is N,
	    List = R
	),
	flush_output(user),
	!,
	random_generate_atom_list(N1, R, MaxDepth).

random_generate_and_print_atom_list(N, Stream, MaxDepth) :-
	random_generate_atom_list(N, List, MaxDepth),
	print_generated_sentences_to_stream(List, Stream).

%----------------------------------------------------------------------

random_pick_top_category(RandomCat) :-
	findall(Cat, top_level_category(Cat), Cats),
	random_element_of_list(RandomCat, Cats).

top_level_category(Cat) :-
	regulus_preds:top_level_category(TopLevelGrammar),
	functor(Cat, TopLevelGrammar, 6).

%----------------------------------------------------------------------

random_generate_from_category(Cat, MaxDepth) :-
	MaxDepth > 0,
	random_expand_cat(Cat, Body),
	MaxDepth1 is MaxDepth - 1,
	random_generate_from_body(Body, MaxDepth1).

%----------------------------------------------------------------------

random_expand_cat(Cat, RandomBody) :-
	findall([Cat, Body], expand_head(Cat, Body), Pairs),
	random_element_of_list([RandomHead, RandomBody], Pairs),
	Cat = RandomHead.

expand_head(Head, Body) :-
	current_predicate(dcg_clause, user:dcg_clause(_, _)),
	user:dcg_clause(Head, Body).

%----------------------------------------------------------------------

random_generate_from_body((F, R), MaxDepth) :-
	!,
	random_generate_from_body(F, MaxDepth),
	random_generate_from_body(R, MaxDepth).

random_generate_from_body(Disjunction, MaxDepth) :-
	is_disjunction(Disjunction),
	!,
	disjunction_to_list(Disjunction, List),
	random_element_of_list(Element, List),
	random_generate_from_body(Element, MaxDepth).

random_generate_from_body(Cat, MaxDepth) :-
	functor(Cat, _CatSymbol, 6),
	!,
	random_generate_from_category(Cat, MaxDepth).

random_generate_from_body('C'(F, Word, R), _MaxDepth) :-
	!,
	F = [Word | R].

random_generate_from_body(true, _MaxDepth) :-
	!.

random_generate_from_body((X = Y), _MaxDepth) :-
	!,
	X = Y.

random_generate_from_body(merge_globals(_, _), _MaxDepth) :-
	!.

%----------------------------------------------------------------------

print_generated_sentences_to_stream([], _Stream).
print_generated_sentences_to_stream([F | R], S) :-
	format(S, '~N~w~n', [F]),
	!,
	print_generated_sentences_to_stream(R, S).

%----------------------------------------------------------------------

random_element_of_list(X, List) :-
	length(List, N),
	N > 0,
	N1 is N + 1,
	random(1, N1, Index),
	safe_nth(Index, List, RandomElt),
	delete(List, RandomElt, RList),
	!,
	(   X = RandomElt ;
	    random_element_of_list(X, RList)
	).

%----------------------------------------------------------------------

is_disjunction(Disjunction) :-
	compound(Disjunction),
	functor(Disjunction, ';', 2).

disjunction_to_list(Disjunction, List) :-
	is_disjunction(Disjunction),
	Disjunction = (F ; R),
	List = [F | R1],
	!,
	disjunction_to_list(R, R1).
disjunction_to_list(Disjunction, List) :-
	List = [Disjunction].
