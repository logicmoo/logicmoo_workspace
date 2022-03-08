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

load_csv_row(Row, Assertion):- unnumbervars(Assertion,AssertionU),!,load_csv_row0(Row, AssertionU),!.
load_csv_row0(Row, Assertion) :-
   load_special_csv_row(Row, Assertion), assertz_if_new(Assertion),!.
load_csv_row0(_, Assertion) :-
   assertz_if_new(Assertion).

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


repaired_words([],[]):-!.
repaired_words(Words,RWords):- atom(Words),atom_contains(Words,'_'),atomic_list_concat(RWords,'_',Words).
repaired_words(Words,RWords):- atom(Words),atom_contains(Words,' '),atomic_list_concat(RWords,' ',Words).
repaired_words(Words,RWords):- \+ is_list(Words),!,RWords=[Words].
repaired_words([W|Words],RWords):- repaired_words(W,RW),repaired_words(Words,RWordL),
  append(RW,RWordL,RWords).

%% assert_phrase_rule(Phrase, Words) is det
%  Asserts that Phrase can be matched by Words (a list of symbols).
%  Asserts the DCG rule: Phrase --> Words.
assert_phrase_rule(Phrase, Words) :- assert_phrase_rule(Phrase, Words, true),!.
assert_phrase_rule(Phrase, Words):- throw(assert_phrase_rule(Phrase, Words)).

%% assert_phrase_rule(Phrase, Words, Guard) is det
%  Asserts that Phrase can be matched by Words (a list of symbols) if
%  Guard is true.
%  Asserts the DCG rule: Phrase --> Words, {Guard}.

assert_phrase_rule(_Phrase, [], _Goal) :- !.
%assert_phrase_rule(Phrase, [Art|S], Goal) :- art_the(Art),!,assert_phrase_rule(Phrase,S, Goal).
assert_phrase_rule(Phrase, Words, Guard):- repaired_words(Words,RWords),Words\==RWords,!,
  assert_phrase_rule(Phrase, RWords, Guard).
assert_phrase_rule(Phrase, Words, Guard):- 
   assertion(\+ (member(X, Words), \+ atomic(X)),
	     Words:"Phrase must be a list of symbols"),
   assert_phrase_subrules(Phrase, Words, Guard).

assert_phrase_subrules(Phrase, [W|Words], Guard):-
  assert_phrase_1subrule(Phrase, [W|Words], Guard),
  nop(assert_phrase_subrules(Phrase, Words, Guard)).
assert_phrase_subrules(_Phrase, [], _Guard).

assert_phrase_1subrule(Phrase, Words, Guard):- 
  Rule = phrase_rule(Phrase, Words, Guard),
   assertz_if_new(/*$global::*/Rule),
   log(apr(Rule)),!.

assert_phrase_1subrule(Phrase, Words, Guard) :- 
   (Guard==true -> DCG = (Phrase --> (Words)); DCG = (Phrase --> (Words, {Guard}))),
   expand_uterm(DCG,DCGRule),
  assertz_if_new(/*$global::*/DCGRule),
  log(ap(DCGRule)),!.
assert_phrase_1subrule(Phrase, Words, Guard):- throw(assert_phrase_rule(Phrase, Words,Guard)).

%% assert_proper_name(+Object, +Name, +Number) is det
%  Asserts that Object has proper name Name (a list of words) with
%  gramatical number Number (singular or plural).
%  Functionally, this means it adds the grammar rule:
%    proper_name(Object, Number) --> Name.

:- assume_dyn_fail(proper_name_without_the//2).

%proper_name_without_the

assert_proper_name(Object, [ ], _) :-
   % Don't default a name for GameObjects
   is_class(Object, $'GameObject'),
   !.
assert_proper_name(Object, [ ], NumberSpec) :-
   !,
   assert_proper_name(Object, [Object], NumberSpec).
assert_proper_name(Object, [the | Name], NumberSpec) :-
   !,
   (number_spec_number(NumberSpec, Number) -> 
      assert_phrase_rule(proper_name_without_the(Object, Number), Name)
      ;
      log(error(bad_grammatical_number(NumberSpec:Name)))).
assert_proper_name(Object, Name, NumberSpec) :-
   number_spec_number(NumberSpec, Number) -> 
      assert_phrase_rule(proper_name(Object, Number), Name)
      ;
      log(error(bad_grammatical_number(NumberSpec:Name))).

% This is just to handle defaulting of number to singular, and to
% catch mistyped number.
number_spec_number(Plural, plural):- plural == Plural.
number_spec_number([X], N):- nonvar(X), number_spec_number(X, N).
number_spec_number(_, singular).

