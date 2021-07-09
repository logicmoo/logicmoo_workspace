% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% Grammar %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================

 char_type_sentence(w(W,_),Type):- !, char_type_sentence(W,Type).
 char_type_sentence(?, ask).
 char_type_sentence((.), tell).
 char_type_sentence((.), act).
 char_type_sentence((!), act).
 char_type_sentence((!), tell).

partition_segs(Segs,W2s,Spans):- partition(\=(span(_)),Segs,W2s,Spans),!.

write_simple_seg(w(W,_)):-!,write_cyan(W).
write_simple_seg(span(W)):-is_list(W),member(S,W),ignore(member('#'(T),W)),compound(S),S=seg(B,E),!,write_cyan(span(B,T,E)).
write_simple_seg(span([W1,W2|_])):-!,write_cyan(span_(W1,W2)).
write_cyan(P):- color_format(hfg(cyan),'<~w>',[P]).

user:portray(X):- notrace((tracing,compound(X),write_simple_seg(X))),!.


utterance(Type, LF, Sentence, E):- (into_lexical_segs(Sentence,Segs)->Sentence\==Segs),!,utterance(Type, LF, Segs, E).
utterance(Type, LF, Segs, E):-  var(Type), is_list(S), partition_segs(Segs,W2s,_Spans), last(W2s,Char),
  char_type_sentence(Char, Type),
  select(Char,S,_First),!,
  utterance(Type, LF, Segs, UE),
  phrase(UE,optional(Char),E).
  

  %append([Char],ES,E).

utterance(Type, LFOut, Sentence, E):- 
 (grab_primary_segs(0,Sentence,Segs,true,Primary)->Sentence\==Segs),!,
   conjoin_lf(Primary,LF,LFOut),
   utterance(Type, LF, Segs, E).


utterance(Type, LFOut)-->
 {b_setval('$prev_w2s',[]),b_setval('$prev_spans',[])},
  utterance2(Type, LF),
  {b_getval('$prev_spans',Spans),
  list_to_conjuncts('&',Spans,SpansLF),
  conjoin_lf(SpansLF,LF,LFOut)}.


utterance2(act, LF) -->   quietly(imperative(LF)).
utterance2(ask, LF) -->   quietly(question(LF)).
utterance2(tell, LF) -->  declarative(LF).
utterance2(Type, warn(unparsed(Type,Sentence)), Sentence, []).

phrase_unit('VP',X,_,X).
phrase_unit('PP',X,_,X).
phrase_unit('NP',_,X,X).
%phrase_unit('S1',_,X,X).
%phrase_unit('S',_,X,X).
%phrase_unit(_,_,X,X).

is_word_between(B,E,W2):- W2=w(_,L),
  member(loc(N),L),between(B,E,N).

loc_of(W2,N):- W2=w(_,L), member(loc(N),L).

insert_just_before0(_,Item,[],[Item]):- !.
insert_just_before0(E,Item,[W2|SegsM2],[Item,W2|SegsM2]):- loc_of(W2,N),N>E,!.
insert_just_before0(E,Item,[W2|SegsM2],[W2|SegsM3]):- insert_just_before(E,Item,SegsM2,SegsM3).

insert_just_before(E,Item,In,Out):- 
  sort_words(In,Mid),
  insert_just_before0(E,Item,Mid,MidM),
  sort_words(MidM,Out).
 

get_word_range(B,E,SegsM,Range,SegsM2):- partition(is_word_between(B,E),SegsM,Range,SegsM2).

grab_primary_segs(Sz,SegsI,SegsO,LFI,LFOut):- Sz==0, select(span(L),SegsI,SegsM), 
  member(alt(span),L), \+ member(seg(X,X),L),!,
  wdmsg(dropping_alt(L)),
  grab_primary_segs(Sz,SegsM,SegsO,LFI,LFOut).

grab_primary_segs(Sz,SegsI,SegsO,LFI,LFOut):- % Sz==0,
  select(span(L),SegsI,SegsM),
  nop(singleton(Range)),
  member(phrase(NP),L), member(NP,['NP','WHNP']),
 once((
  member('#'(Ref),L),
  member(childs(0),L),
  member(seg(B,E),L),
  get_word_range(B,E,SegsM,Range,SegsM2),
  member(txt(Words),L), p_n_atom(Words,VarNameU),
  p_n_atom(Ref,RefU),
  atomic_list_concat(['XVAR',RefU,VarNameU,B,E],'_',VarName),
  FullInfo = [loc(B),pos(NP),equals(VarName)
  % ,words(Range)
  |L],
  insert_just_before(E,w(VarName,FullInfo),SegsM2,SegsM3),
  LFMid1 = info(VarName,FullInfo),
  !)),
  member(txt(Words),L), 
  %phrase(noun_phrase(subj,X,true,LFNP),Range,[]), conjoin_lf(LFNP, LFMid1, LFMid2),!,
  conjoin_lf(LFI, LFMid1, LFMid3),!,
  grab_primary_segs(Sz,SegsM3,SegsO,LFMid3,LFOut).

  
grab_primary_segs(Sz,SegsI,SegsO,LFI,LFOut):-  fail,
 Sz==0,
 select(span(L),SegsI,SegsM), 
 once((member(seg(X,XX),L),XX is X+ Sz,
  (Sz == 0 -> LX=X ; (member(phrase(NVP),L), phrase_unit(NVP,X,XX,LX))),
  W2 = w(W,L2), member(W2,SegsI),member(loc(LX),L2),nb_set_add(W2,L),
  %member(txt(Words),L),
  p_n_atom(W,VarNameU),
  atom_concat(VarNameU,LX,VarName),
  %debug_var(VarName,Var), % = '$VAR'(VarName),
  LFMid1 = info(VarName,W2))),
  conjoin_lf(LFI, LFMid1, LFMid2),!,
  grab_primary_segs(Sz,SegsM,SegsO,LFMid2, LFOut).

grab_primary_segs(Sz,SegsI,SegsO,LFI,LFOut):- Sz<4,
  Sz2 is Sz+ 1, !, grab_primary_segs(Sz2,SegsI,SegsO,LFI,LFOut).
grab_primary_segs(_Sz,SegsI,SegsI,LFIOut,LFIOut).   

parse_for('evtState', Evt, LF, LFOut) --> theText1(to), verb_phrase(Evt, _, VerbLF), conjoin_lf(LF, VerbLF, LFOut).
parse_for('evtState', Evt, LF, LFOut) --> frame_sentence(Evt, VerbLF), conjoin_lf(LF, VerbLF, LFOut).

system:sentence_breaker(A) :-
    parser_e2c:
    (   make,
        into_lexical_segs(A, C),
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
imperative(IC, A, B) :- notrace(as_w2_segs(A, C)),!,imperative(IC, C, B).
imperative(do(X, LFOut)) --> verb_phrase(_NewFrame, X, LFOut), optionalText1('!'), optionalText1('.'), nvd('Speaker', X).

% =================================================================
% Declarative sentences
% =================================================================

% [if|when|whenever] Cond then Result
declarative(IC, A, B) :- notrace(as_w2_segs(A, C)),!,declarative(IC, C, B).
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
interogative(IC, A, B) :- notrace(as_w2_segs(A, C)),!,interogative(IC, C, B).
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



