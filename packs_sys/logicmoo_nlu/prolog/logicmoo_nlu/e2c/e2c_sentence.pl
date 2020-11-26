% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% Grammar %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================

 char_type_sentence(?, ask).
 char_type_sentence((.), tell).
 char_type_sentence((.), act).
 char_type_sentence((!), act).
 char_type_sentence((!), tell).

utterance(Type, LF, S, E):- var(Type), is_list(S), append(First, [ Char], S),
  \+ \+ char_type_sentence(Char, _), !,
  char_type_sentence(Char, Type), utterance(Type, LF, First, E).

utterance(act, LF) -->   quietly(imperative(LF)).
utterance(ask, LF) -->   quietly(question(LF)).
utterance(tell, LF) -->  declarative(LF).

parse_for('evtState', Evt, LF, LFOut) --> theText1(to), verb_phrase(Evt, _, VerbLF), conjoin_lf(LF, VerbLF, LFOut).
parse_for('evtState', Evt, LF, LFOut) --> frame_sentence(Evt, VerbLF), conjoin_lf(LF, VerbLF, LFOut).

system:sentence_breaker(A) :-
    parser_e2c:
    (   make,
        to_wordlist_atoms(A, C),
        call_print_reply(list(List)=B,
                         parser_e2c:sentence_breaker(true, List, B, C, D)),
        !,
        D=[]
    ).

sentence_breaker(LFIn, [H|List], LFOut) -->  dcg_when([_|_], sentence_part(LFIn, H, LFMid)), sentence_breaker(LFMid, List, LFOut).
sentence_breaker(LFMid, [], LFMid) --> !.

sentence_part(LF, var(X), LFOut) --> named_var_match(contains(''), X, LF, LFOut), !.
sentence_part(LF, prep(Prep), LF) --> theText1(Prep), {prep_dict(Prep), ok_prep(Prep)}.
sentence_part(LF, verb_tensed(Verb, Formed), LF) --> theText1(Formed), {clex_verb(Formed, Verb, _Tv, _Type)}.
sentence_part(LF, np(X, LFOut), LF) --> noun_phrase0(_SO, X, true, LFOut), !.
sentence_part(LF, verb(Word), LF) --> [w(Word, open)], !.
sentence_part(LF, Other, LF) --> [Other].

% =================================================================
% Imperative sentences
% =================================================================
imperative(do(X, LFOut)) --> verb_phrase(_NewFrame, X, LFOut), optionalText1('!'), optionalText1('.'), nvd('Speaker', X).

% =================================================================
% Declarative sentences
% =================================================================

% [if|when|whenever] Cond then Result
declarative(LFOut) --> (theText1(if);theBaseForm(when)), frame_sentence(_Frame1, LHS), {add_quant(all, LHS)},
     optionalText1(', '), theText1(then), frame_sentence(_Frame2, RHS), {add_quant(exists, RHS), expand_quants(LHS=>RHS, LFOut)}, !.
% declarative(LFOut) --> frame_sentence0(_Frame, LFOut).
declarative(LFOut) --> frame_sentence(LFOut), optionalText1('.').

:-add_e2c("The man who smokes Blends has a neighbor who drinks water.", sanity).

% frame_sentence(Frame, LF) --> tag(Frame, frame_sentence, LF), !.
/* when using frame_sentence we need to pass 4 arguments,

   the first will be the Frame varialbe of the first main verb
   the second will match LFOut in the head of the DGC clause
   the third is the list containing the words in the frame_sentence
   the Fourth is the empty list.
   Example:
     frame_sentence(Frame, Meaning, [every, man, that, paints, likes, monet], []) */

% :- style_check(-discontiguous).

:-add_e2c("no two owners eat pizza", sanity).
frame_sentence0(Frame, LFOut) --> theText1(no), number_of(2), !,
  noun_phrase(_SO, X, true, XCond),
  verb_phrase(Frame, X, XLF),
  { term_variables(XLF, Vars), exclude(==(X), Vars, RestOfVars),

    copy_term((RestOfVarsX, X, XCond, XLF),
    (RestOfVarsY, Y, YCond, YLF, RestOfVars)),
    RestOfVarsY=RestOfVarsX},
  % LFOut = (((XCond & YCond & XLF & YLF) => ~(X=Y))).
  {(LFOut = (((XCond & different(X, Y) & YCond & XLF)) => ~YLF))}.
  % LFOut = (((XCond & XLF & different(X, Y) & YLF) => ~YCond)).


:-add_e2c("no three owners eat pizza", sanity).   % alt: less than three owners eat pizza
:-add_e2c("no three owners eat the same pizza", sanity).  % alt: less than three owners eat off the same instance
:-add_e2c("no three owners eat the same kind of pizza", sanity).  % alt: less than three owners eat an instance of the same classes
frame_sentence0(Frame, LFOut) --> theText1(no), number_of(3),
  noun_phrase(_SO, X, true, XCond),
  verb_phrase(Frame, X, XLF),
  {copy_term((X, XCond, XLF), (Y, YCond, YLF)),
   copy_term((X, XCond, XLF), (Z, ZCond, ZLF))},
  {(LFOut = ((((forKxyz(X, Y, Z) &XCond & different(X, Y) & different(X, Z) & different(Z, Y) & YCond & ZCond) => (( XLF & ZLF) => ~YLF)))))}.


:-add_e2c("no owners eat the same pizza", sanity).
:-add_e2c("no owners eat the same kind of pizza", sanity).
frame_sentence0(Frame, LFOut) --> theText1(no),
  noun_phrase2(_SO, X, true, XCond),
  verb_phrase(Frame, X, XLF),
  {copy_term((X, XCond, XLF), (Y, YCond, YLF))},
  {(LFOut = (((XCond & different(X, Y) & YCond & XLF)) => ~YLF))}.


frame_sentence(Frame, LFOut)-->frame_sentence0(Frame, LFOut).
:-add_e2c("there are 5 houses", sanity).
:-add_e2c("there are not 5 houses", sanity).
:-add_e2c("there are not zero houses", sanity).
frame_sentence(Frame, LFOut) --> optionalText1('there'), is_be(Frame, _X, Assn, LFOut), !, frame_sentence(Frame, Assn).
% Regular NP+VP
frame_sentence(Frame, LFOut) --> noun_phrase(subj, X, true, Precond), verb_phrase(Frame, X, Postcond),
  conjoin_lf(Postcond, Precond, LFOut).
frame_sentence(LFOut) --> frame_sentence(Frame, LFOut), {add_quant(exists, LFOut), add_quant(exists, Frame)}.

add_quant(Type, X):- var(X) -> (get_attr(X, '$quant_needed', _)->true;put_attr(X, '$quant_needed', Type));
   (term_variables(X, Plain), must_maplist(add_quant(Type), Plain)).

term_plain_variables(LF, Plain):- term_variables(LF, Vars), exclude(attvar, Vars, Plain).
% =================================================================
% Interogative sentences @TODO
% =================================================================

question(Q                     ) --> tag(_NewFrame, question, Q), !.
question(Q                     ) --> interogative(Q), optionalText1('?').
% Questions
question(LFOut => answer(LFOut)) --> frame_sentence(_NewFrame, LFOut), [?].


% "is joe walking" (Inverted sentences)
sentence_inv(X, LFOut) -->  aux(_Tense+fin/_Form, S0, LFOut), noun_phrase(subj, X, LF, S0), verb_phrase(_NewFrame, X, LF).

% "What have you?"
% "who eats?"
% "What do you have?"
% "What do you think?"
% "How are you?"
% "How will you?"
interogative(LFOut => answer(X))   -->  whpron(X, LF, LFOut), verb_phrase(_NewFrame, X, LF).  % verb Form be _Tense+fin
% "where is joe walking?"
interogative(LFOut => answer(X))   -->  whpron(X, LF, LFOut), sentence_inv(X, LF).   % was gap(noun_phrase, X)
% "could he think of it?"
interogative(LFOut => answer(yes)) -->  sentence_inv(_X, LFOut).   % was nogap

% @TODO
% "is joe a person?"
% "are you happy?"
% "Could the dog"
% interogative(LFOut => answer(yes)) -->  copula_is_does, noun_phrase(subj, (X^SO)^LFOut, nogap), noun_phrase(subj, (X^true)^q(exists, X, SO & true), nogap).



