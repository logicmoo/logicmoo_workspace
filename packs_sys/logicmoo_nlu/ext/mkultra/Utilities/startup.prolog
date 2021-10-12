%%%
%%% Utilities involved in loading the KB and lexicon
%%%

:- public load_csv_row/2.
:- public begin_csv_loading/1, end_csv_loading/1.
:- external begin_csv_loading/1, end_csv_loading/1.

%% load_csv_row(+RowNumber, +RowData)
%  IMPERATIVE
%  Called for each row of a CSV file when it's loaded.  RowData
%  is a structure whose functor is the name of the file (w/o
%  extension and whose arguments are the data from the row's
%  columns.
%
%  If there's a load_special_csv_row rule that matches the RowData,
%  it's allowed to process the row.  Otherwise the RowData is
%  asserted into the KB as a fact.
load_csv_row(Row, Assertion) :-
   load_special_csv_row(Row, Assertion).
load_csv_row(_, Assertion) :-
   assertz(Assertion).

%% register_all_lexical_items(?ListTemplate, :Generator)
%  Registers all the lexical items from ListTemplate for each solution of Generator.
:- public register_all_lexical_items/2.
register_all_lexical_items(ListTemplate, Generator) :-
   forall(Generator,
	  register_lexical_items(ListTemplate)).

%% register_lexical_items(+List)
%  Register each of a series of lexical items.
register_lexical_items(List) :-
   forall(member(Item, List),
	  register_lexical_item(Item)).

%% assert_phrase_rule(Phrase, Words) is det
%  Asserts that Phrase can be matched by Words (a list of symbols).
%  Asserts the DCG rule: Phrase --> Words.
assert_phrase_rule(_, []) :- !.
assert_phrase_rule(Phrase, Words) :-
   assertion(\+ (member(X, Words), \+ atomic(X)),
	     Words:"Phrase must be a list of symbols"),
   forall(member(Word, Words),
	  register_lexical_item(Word)),
   append(Words, Tail, WordsWithTail),
   term_append(Phrase, [WordsWithTail, Tail], DCGRule),
   assertz($global::DCGRule).

%% assert_phrase_rule(Phrase, Words, Guard) is det
%  Asserts that Phrase can be matched by Words (a list of symbols) if
%  Guard is true.
%  Asserts the DCG rule: Phrase --> Words, {Guard}.
assert_phrase_rule(Phrase, Words, Guard) :-
   assertion(\+ (member(X, Words), \+ atomic(X)),
	     Words:"Phrase must be a list of symbols"),
   append(Words, Tail, WordsWithTail),
   term_append(Phrase, [WordsWithTail, Tail], DCGRule),
   assertz((DCGRule :- Guard)).

%% assert_proper_name(+Object, +Name, +Number) is det
%  Asserts that Object has proper name Name (a list of words) with
%  gramatical number Number (singular or plural).
%  Functionally, this means it adds the grammar rule:
%    proper_name(Object, Number) --> Name.
assert_proper_name(Object, [ ], _) :-
   % Don't default a name for GameObjects
   is_class(Object, $'GameObject'),
   !.
assert_proper_name(Object, [ ], NumberSpec) :-
   !,
   assert_proper_name(Object, [Object], NumberSpec).
assert_proper_name(Object, Name, NumberSpec) :-
   number_spec_number(NumberSpec, Number) -> 
      assert_phrase_rule(proper_name(Object, Number), Name)
      ;
      log(bad_grammatical_number(NumberSpec:Name)).

% This is just to handle defaulting of number to singular, and to
% catch mistyped number.
number_spec_number([ ], singular).
number_spec_number(singular, singular).
number_spec_number(plural, plural).
