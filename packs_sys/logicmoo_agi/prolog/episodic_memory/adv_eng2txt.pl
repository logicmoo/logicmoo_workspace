

:- '$set_source_module'(mu).

% Inform notation
% 'c'  character)
% "string" string
% "~"  quotation mark
% "^"  newline
% @   accent composition, variables 00 thru 31
% \   line continuation
% Engish messages need to be printable from various perspectives:
% person (1st/2nd/3rd), tense(past/present)
% "You go south." / "Floyd wanders south."
%  {'$agent $go $1', ExitName }
%  { person(Agent), tense(go, Time), ExitName, period }
%  {'$p $t $w', Agent, go, ExitName}
% "You take the lamp." / "Floyd greedily grabs the lamp."
%  Agent=floyd, {'%p quickly grab/T %n', Agent, grab, Thing }
%    else {'%p take/T %n', Agent, take, Thing }
% %p Substitute parameter as 1st/2nd/3rd person ("I"/"you"/"Floyd").
%   Implicit in who is viewing the message.
%   Pronouns: gender, reflexive, relative, nominative, demonstratve...?
% %n Substitute name/description of parameter ("the brass lamp").
% /T Modify previous word according to tense ("take"/"took").
%   Implicit in who is viewing the message? Context when printed?
% /N Modify previous word according to number ("coin"/"coins").
%   What number?
% %a Article - A or An (indefinite) or The (definite) ?
%
% I go/grab/eat/take
% you go/grab/eat/take
% she goes/grabs/eats/takes
% floyd goes/grabs/eats/takes
%
% eng(subject(Agent), 'quickly', verb(grab, grabs), the(Thing))
% [s(Agent), 'quickly', v(grab, grabs), the(Thing)]

english_directve(quoted(_)).
english_directve(cap(_)).
english_directve(subj(_)).
english_directve(person(_, _)).
english_directve(tense(_, _)).
english_directve(a(_)).
english_directve(the(_)).
english_directve(num(_, _, _)).
english_directve(aux(_)).
english_directve(silent(_)).
english_directve(P):- english_suffix(S), safe_functor(P, S, 1).

english_suffix(s).
english_suffix(es).
english_suffix(er).
english_suffix(ed).
english_suffix(ly).
english_suffix(ing).


capitalize([First|Rest], [Capped|Rest]) :- !,
 capitalize(First, Capped).
capitalize(Atom, Capitalized) :-
 atom(Atom), % [] is an atom
 downcase_atom(Atom, Lower),
 atom_chars(Lower, [First|Rest]),
 upcase_atom(First, Upper),
 atom_chars(Capitalized, [Upper|Rest]).
capitalize(Atom, Capitalized) :-
 inst_of(Atom, Type, _), !, capitalize(Type, Capitalized).

context_agent(Agent, Context):- atom(Context), !, Context=Agent.
context_agent(Agent, Context):-
 declared(agent(Agent), Context), !.
context_agent(Agent, Context):-
 declared(propOf(_, Agent), Context), !.
context_agent(Agent, Context):-
 declared(inst(Agent), Context), !.
context_agent(Agent, Context):- \+ is_list(Context),
 action_doer(Context, Likely), Agent=Likely.


% compile_eng(Context, Atom/Term/List, TextAtom).
% Compile Eng terms to ensure subject/verb agreement:
% If subject is agent, convert to 2nd person, else use 3rd person.
% Context specifies agent, and (if found) subject of sentence.

compile_eng(_Context, _Done, Text) :- assertion(var(Text)), fail.
compile_eng(_Context, Done, '') :- Done == [], !.

compile_eng(Context, [cap(subj(Agent)), aux(be)|More], Person) :- !,
 compile_eng(Context, [cap(subj(Agent)), person(are, is)|More], Person) .

compile_eng(Context, [AN, Apple|More], Text) :-
 (AN==a;AN==an), !,
 compile_eng_txt(Context, [Apple|More], TxtApple),
 name(TxtApple, [A|_]),
 char_type(A, to_lower(Vowel)),
 (adv_vowel(Vowel) -> atom_concat('an ', TxtApple, Text);atom_concat('a ', TxtApple, Text)).
% mu:compile_eng([agent(_Player_1), person(_Player_1)], a(floyd), _64404)
compile_eng(Context, [First|Rest], [First2|Rest2]) :-
 compile_eng(Context, First, First2),
 compile_eng(Context, Rest, Rest2), !.

compile_eng(_Context, Object, Text) :-
 is_x_instance(Object), inst_of(Object, Text, _).


compile_eng(_Context, aux(be), 'is') :- !.
compile_eng(Context, aux(Can), Text) :- !, compile_eng_txt(Context, Can, Text).
compile_eng(Context, quoted(Can), Out) :- !, compile_eng_txt(Context, Can, Text), atomic_list_concat([' "', Text, '" '], Out).

compile_eng(Context, subj(Agent), Person) :-
 context_agent(Agent, Context),
 declared(person(Person), Context), !.
compile_eng(Context, subj(Other), Compiled) :- !,
 compile_eng(Context, Other, Compiled).
compile_eng(Context, Agent, Person) :-
 context_agent(Agent, Context),
 declared(person(Person), Context).
compile_eng(Context, person(Second, _Third), Compiled) :-
 declared(subj(Agent), Context),
 context_agent(Agent, Context),
 compile_eng(Context, Second, Compiled).
compile_eng(Context, person(_Second, Third), Compiled) :-
 compile_eng(Context, Third, Compiled).
compile_eng(Context, tense(Verb, Tense), Compiled) :-
 verb_tensed(Context, Verb, Tense, Compiled).
compile_eng(Context, cap(Eng), Compiled) :-
 must_mw((compile_eng(Context, Eng, Lowercase),
 capitalize(Lowercase, Compiled))).
compile_eng(_Context, silent(_Eng), '').

compile_eng(_Context, extra_verbose_eng(_Eng), '...' ):- debugging(noverbose, true), !.
compile_eng(Context, extra_verbose_eng(Eng), O ):-
 compile_eng_txt(Context, Eng, Compiled),
 format(atom(O), "\u001b[31m~w\u001b[0m", [Compiled]), !.
%compile_eng(Context, extra_verbose_eng(Eng), '...verbose...'(Compiled) ):-
% compile_eng_txt(Context, Eng, Compiled).

compile_eng(Context, Inst, TheThing):- atom(Inst), !, must_mw1(compile_eng_atom(Context, Inst, TheThing)).

/*compile_eng(Context, String, Text):- string(String),
 name(Atom, String), compile_eng(Context, Atom, Text).
*/
compile_eng(_Context, Inst, Inst):- string(Inst), !.
compile_eng(_Context, Inst, Text):- string(Inst), !, format(atom(Text), '~q', [Inst]).
compile_eng(_Context, Inst, Text):- \+ compound(Inst), !, format(atom(Text), '~w', [Inst]).

compile_eng(Context, s(Word), Textually) :- % TODO make actually plural
 compile_eng_txt(Context, Word, Textual),
 atom(Textual),
 atom_concat(Textual, "s", Textually).

compile_eng(Context, Wordly, Textually) :- safe_functor(Wordly, S, 1), english_suffix(S),
 Wordly =..[S, Word],
 compile_eng_txt(Context, Word, Textual),
 atom(Textual), add_suffix(Textual, S, Textually), !.

compile_eng(Context, DetWord, AThing) :-
 compound(DetWord), DetWord=..[Det, Word],
 member(Det, [the, some, a, an, '']),
 compile_eng(Context, [Det, Word], AThing), !.

/*compile_eng(Context, Prop, Text):- \+ atomic(Prop),
 logic2english(you, Prop, TextMid), !,
 compile_eng(Context, ['\n'|TextMid], Text), !.
*/
compile_eng(_Context, Prop, Prop).




compile_eng_atom(Context, Inst, TheThing):-
  inst_of(Inst, Type, N), N\==0, !,
 (nth0(N, [(unknown), '', the, thee, old, some, a], Det) -> true; atom_concat('#', N, Det)),
 compile_eng(Context, [Det, Type], TheThing).

compile_eng_atom(Context, Atom, Text):- fail, atom(Atom), must_mw1(atomic_list_concat(ABC, ' ', Atom)),
 ABC=[A, B|C], !, compile_eng_txt(Context, [A, B|C], Text).
%compile_eng_atom(Context, Word, Textually) :- atom(Word), atom_length(Word, L), L>3, atom_contains(Word, ' '), into_text80(Word, Words), !, compile_eng_txt(Context, Words, Textually).
compile_eng_atom(Context, Word, Textually) :- atom(Word), atom_length(Word, L), L>3,
  atom_contains(Word, '_'), \+ atom_contains(Word, '('), atomic_list_concat(Words, '_', Word), !, compile_eng_txt(Context, Words, Textually).
compile_eng_atom(_Context, Inst, Text):- \+ compound(Inst), !, format(atom(Text), '~w', [Inst]).


adv_vowel(a). adv_vowel(e). adv_vowel(i). adv_vowel(o). adv_vowel(u).

verb_tensed(Context, Verb, past, Compiled):-
 compile_eng_txt(Context, Verb, Word),
 pasitfy_word(Word, Compiled).
verb_tensed(Context, Verb, _Tense, Compiled):-
 compile_eng_txt(Context, Verb, Compiled).

add_suffix(Textual, es, Textually):- atom_concat(Textual, s, Textually).
add_suffix(Textual, S, Textually):- atom_concat(Textual, S, Textually).

pasitfy_word(take, took).
pasitfy_word(make, made).
pasitfy_word(move, moved).
pasitfy_word(eat, ate).
pasitfy_word(Verb, Compiled):- \+ atom(Verb), !, Compiled=Verb.
pasitfy_word(Verb, Compiled):- atomic_concat(Verb, 'ed', Compiled).


nospace(_, ', ').
nospace(_, ';').
nospace(_, ':').
nospace(_, '.').
nospace(_, '?').
nospace(_, '!').
nospace(_, '\'').
nospace('\'', _).
nospace(_, '"').
nospace('"', _).
nospace(_, Letter) :- char_type(Letter, space).
nospace(Letter, _) :- char_type(Letter, space).

no_space_words('', _).
no_space_words(_, '').
no_space_words(W1, W2) :-
 atomic(W1),
 atomic(W2),
 atom_chars(W1, List),
 last(List, C1),
 atom_chars(W2, [C2|_]),
 nospace(C1, C2).

insert_spaces([W], [W]).
insert_spaces([W1, W2|Tail1], [W1, W2|Tail2]) :-
 xnotrace(no_space_words(W1, W2)),
 !,
 insert_spaces([W2|Tail1], [W2|Tail2]).
insert_spaces([W1, W2|Tail1], [W1, ' ', W3|Tail2]) :-
 insert_spaces([W2|Tail1], [W3|Tail2]).
insert_spaces([], []).



make_atomic(_, Atom, Atom) :- atomic(Atom), !.
make_atomic(Context, Some, Text):- is_list(Some),
  must_maplist(make_atomic(Context), Some, Stuff), atomic_list_concat(Stuff, ' ', Text), !.

make_atomic(_Context, anglify(NewCtx, Some), Text):- !,
  logic2english(NewCtx, Some, Term),
  term_to_atom(Term, Text), !.

make_atomic(Context, cap(subj(Some)), Text):- !,
  compile_eng(Context, cap(Some), Term),
  term_to_atom(Term, Text), !.

make_atomic(Context, cap(Some), Text):- !,
  compile_eng(Context, cap(Some), Term),
  term_to_atom(Term, Text), !.


make_atomic(Context, Logic, Text):- fail, Logic =.. [F|Some],
  must_maplist(logic2english(Context), Some, Stuff),
  Term =.. [F|Stuff],
  term_to_atom(Term, Text), !.
make_atomic(_, Term, Atom) :-  format(atom(Atom), '~p', [Term]), !.
make_atomic(_, Term, Atom) :-  term_to_atom(Term, Atom), !.

eng2txt(Agent, _Person, LogicalEnglish, Text) :- compound(LogicalEnglish), \+ is_list(LogicalEnglish), !,
  logic2english(Agent, LogicalEnglish, Text), !.
eng2txt(Agent, Person, LogicalEnglish, Text) :- \+ is_list(LogicalEnglish), !,
  eng2txt(Agent, Person, [LogicalEnglish], Text), !.
eng2txt(Agent, Person, Eng, Text) :- assertion(nonvar(Eng)),
 % Find subject, if any.
 quietly((
  findall(subj(Subject), findterm(subj(Subject), Eng), Context),
  list_to_set([agent(Agent), person(Person)| Context], NewContext),
  compile_eng_txt(NewContext, Eng, Text))).
eng2txt(_Agent, _Person, Text, Text).

compile_eng_txt(_Context, Done, '') :- Done == [], !.
compile_eng_txt(Context, [First], Text):- compile_eng_txt(Context, First, Text), !.
compile_eng_txt(Context, Eng, Text):-
 flatten([Eng], FEng),
 compile_eng_txt_pt2(Context, FEng, FText),
 format(atom(Text), '~w', FText), !.

% Compile recognized structures.
compile_eng_txt_pt2(Context, EngIn, Text) :-
 assertion(nonvar(EngIn)),
 flatten([EngIn], Eng),
 must_maplist(compile_eng(Context), Eng, Compiled),
 % Flatten any sub-lists.
 flatten([Compiled], FlatList),
 % Convert terms to atom-strings.
 findall(Atom, (member(Term, FlatList), make_atomic(Context, Term, Atom)), AtomList),
 findall(Atom2, (member(Atom2, AtomList), Atom2\=''), AtomList1),
 grammar_check(Context, AtomList1, AtomList2),
 % Add spaces.
 dbug(printer, 'insert_spaces(~w)~n', [AtomList2]),
 insert_spaces(AtomList2, SpacedList),
 % Return concatenated atoms.
 concat_atom(SpacedList, Text), !.


grammar_check(_Context, [], []).
grammar_check(Context, [AN, Apple|More], Text) :- fail,
 (AN==a;AN==an), !,
 compile_eng_txt(Context, [Apple|More], TxtApple),
 name(TxtApple, [A|_]),
 char_type(A, to_lower(Vowel)),
 (adv_vowel(Vowel) -> atom_concat('an ', TxtApple, Text);atom_concat('a ', TxtApple, Text)).

grammar_check(Context, [Word|More], [Word|MoreN]) :-
 grammar_check(Context, More, MoreN).

grammar_check(_Context, A, A).




the(State, Object, Text) :-
 getprop(Object, name(D), State),
 % name(Object, D),
 % props(Object, List), member(name(D), List).
 compile_eng_txt(State, D, AD),
 atom_concat('the ', AD, Text).

an(State, Object, Text) :-
 getprop(Object, name(D), State),
 compile_eng_txt(State, D, AD),
 atom_concat('a ', AD, Text).

num(_Singular, Plural, [], Plural).
num(Singular, _Plural, [_One], Singular).
num(_Singular, Plural, [_One, _Two|_Or_More], Plural).

expand_english(State, the(Object), Text) :-
 the(State, Object, Text).
expand_english(State, an(Object), Text) :-
 an(State, Object, Text).
expand_english(_State, num(Sing, Plur, List), Text) :-
 num(Sing, Plur, List, Text).
expand_english(_State, [], '').
expand_english(State, [Term|Tail], [NewTerm|NewTail]) :-
 expand_english(State, Term, NewTerm),
 expand_english(State, Tail, NewTail).
expand_english(_State, Term, Term).


