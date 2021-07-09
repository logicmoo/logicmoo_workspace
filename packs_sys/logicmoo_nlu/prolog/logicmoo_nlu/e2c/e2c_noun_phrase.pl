% =================================================================
% Noun Phrase
% =================================================================

:- ensure_loaded(e2c_quantifiers).
:- ensure_loaded(library(body_reordering/logicmoo_util_body_reorder)).
% what the product is
noun_phrase9(SO, X, LF, Out) --> theText1(what), noun_phrase(SO, Y, LF, LF0),
  theText1(is), conjoin_lf(LF0 , what_is(Y, X), Out).
% a certain type
noun_phrase9(_SO, X, LF, exist(X, LF & iza(X, 'tType'))) --> theText1([a, certain, type]).

:- decl_is_dcg(noun_phrase(_SO, _X, _LF, _LFOut)).

noun_phrase(_SO, X, LF, LFOut) --> theTextW2(_Text,L),{member(phrase('NP'),L),member(equals(Root),L),!,conjoin_lf(LF , equalsVar(X,Root), LFOut)}.
noun_phrase(_SO, X, LF, LFOut) --> {sub_compound(iza(V, What), LF), V==X, nonvar(What)}, parse_for(What, X, LF, LFOut).

noun_phrase(SO, X, LF, LFOut) -->
   ignore_quant(LF),
   noun_phrase0(SO, X, LF, LFOut),
   ignore_quant(LFOut).

noun_phrase0(SO, X, LF, LFOut) --> noun_phrase9(SO, X, LF, LFOut).

% some/all
noun_phrase0(SO, X, LF, LFOut) -->
    determiner(X, DetProps), !,
    add_traits(X, DetProps, LF, PreProps),
    noun_phrase1(SO, X, PreProps, FLMid),
    dcg_thru_2args(noun_post_mod(SO, X), FLMid, LFOut).

% "her friend" liked "his friend"
noun_phrase1(SO, X, LF0, LFOut) -->
  theText1(Her), {nl_call(poss_pron_lex, Her, Fem, Pers, SgOrpl)},
  dcg_when(theText1(_), noun_phrase1(obj(_K), X, LF0, LF1)),
  add_traits(Y, [owner(X, Y), gender(Fem), person(Pers), v_arg(SO), SgOrpl], LF1, LFOut).

% happy friends
% evil bush
noun_phrase1(SO, X, LF, LFOut) -->
    dcg_when((theText1(_),theText1(_)),
      dcg_and(dcg_thru_2args(noun_pre_mod(SO, X), LF, PreProps), [_|_])),
    % {PreProps \== LF}, !,
    noun_phrase1(SO, X, PreProps, LFOut).

noun_phrase1(SO, X, LF, LFOut) -->
   noun_phrase2(SO, X, LF, LFOut).

noun_phrase2(SO, X, LF, LFOut) -->
    noun(SO, X, NounProps),
    add_traits(X, NounProps, LF, LFOut).

% "hers" liked "hers"
noun_phrase2(SO, X, LF0, LFOut) -->
  theText1(Hers), {atom_concat(Her, 's', Hers), nl_call(poss_pron_lex, Her, Fem, Pers, SgOrpl)},
  add_traits(Y, [owner(X, Y), gender(Fem), person(Pers), v_arg(SO), SgOrpl], LF0, LFOut), nvd(Hers, X).

% "his" liked "his"
noun_phrase2(SO, X, LF0, LFOut) -->
  theText1(His), {nl_call(poss_pron_lex, His, Gender, Pers, SgOrpl)},
  add_traits(Y, [owner(X, Y), gender(Gender), person(Pers), v_arg(SO), SgOrpl], LF0, LFOut), nvd(His, X).

% Fred
noun_phrase2(_SO, X, LF, LF) --> proper_noun(X).

noun_phrase2(_SO, X, LF, ~exist(X, LF)) --> theText1(nothing).
noun_phrase2(_SO, X, LF, exist(X, LF)) --> theText1(something).

% it, she, we, them, everyone
noun_phrase2(SO, X, LF, Out) -->
  pronoun(SO, X, LF, LF2),
  dcg_thru_2args(noun_post_mod(SO, X), LF2, Out).


% =================================================================
% Relational Clauses / Noun Satellites
% =================================================================

rel_clause(_SO, X, LF, Out) --> theText1(Who), {relpron_dict(Who)}, !, verb_phrase(_NewFrame, X, LF0), conjoin_lf(LF , LF0, Out).

  relpron_dict(that).  relpron_dict(who).  relpron_dict(whom).

adjective(X, MProps) --> quietly(maybe_negated_dcg(adjective1(X), MProps)).
adjective1(X, MProps) --> named_var_match(contains('ADJ'), Var, iza(X, Var), MProps).
adjective1(X, MProps)  -->  {nop( \+ dcg_peek(verb1(_V)))}, theText1(Adj), {nop(Adj \== liked), adj_lf(X, Adj, MProps)}.

 adj_lf(X, Adj, ISA) :-
   ((   (if_defined(parser_chat80:adj_lex( Adj, _)), RAdj=Adj);
         clex_iface:clex_adj(Adj, RAdj, _);
         talkdb:talk_db(_, RAdj, Adj);
        (talkdb:talk_db(adj, Adj), RAdj=Adj))),
    \+ prep_dict(Adj),
    into_isa3(X, tColFn(RAdj), ISA).

noun_pre_mod(_SO, _X, _LF, _Out) --> pronoun(_SO1, _X1, _LF1, _Out1), !, {fail}.
noun_pre_mod(_SO, X, LF, Out) --> adjective(X, MProps), conjoin_lf(LF, MProps, Out).
noun_pre_mod(SO, X, LF, Out) --> near_noun_mod(SO, X, LF, Out).

near_noun_mod(_SO, X, LF, Out) --> theText1([hapilly, maried]), conjoin_lf(LF , hapilly_maried(X), Out).

% "of London"
noun_post_mod(SO, X, LF, Out) --> theText1(of), noun_phrase(SO, Y, LF, LF0), conjoin_lf(LF0, of(X, Y), Out).
% "owned by me"
noun_post_mod(SO, X, LF, Out) --> theText1([owned, by]), noun_phrase(SO, Y, LF & owner(X, Y), Out).
noun_post_mod(SO, X, LF, Out)  --> prepositional_phrase(SO, X, _Frame, LF, Out).
noun_post_mod(SO, X, LF, Out)  --> rel_clause(SO, X, LF, Out).
noun_post_mod(_SO, _X, LF, LF)  --> theText1(each);theText1(all).

noun_post_mod(SO, X, LF, Out) --> near_noun_mod(SO, X, LF, Out).

% =================================================================
%  Word-level Negation
% =================================================================
:-add_e2c("terry writes a non-program.", tell).
:-add_e2c("every nonhuman programmer writes a program.", tell).
:-add_e2c("every human programmer writes a not a program.", tell).

:-add_e2c("every human programmer happily writes a not a program.", tell).

pos_or_neg(-) --> theText1(not), % plt, !,
                                 optionalText1('a'), optionalText1('an').
pos_or_neg(-) --> theText1(not).
pos_or_neg(-) --> theText1(non), optionalText1('-').
pos_or_neg(-) --> theText1(Neg), {atomic(Neg), neg_to_pos(Neg, Pos)}, !, dcg_push_w2(Pos).
pos_or_neg(+) --> [].

neg_to_pos(UnHappy, Happy) :- atom_concat('un', Happy, UnHappy).
neg_to_pos(InHumane, Humane):- atom_concat('in', Humane, InHumane).
neg_to_pos(NonHuman, Human) :- atom_concat('non-', Human, NonHuman).
neg_to_pos(NonHuman, Human) :- atom_concat('non', Human, NonHuman).
neg_to_pos(DisEnchanted, Enchanted) :- atom_concat('dis', Enchanted, DisEnchanted).

maybe_did_hack(PosLF, skipped, PosLF).
maybe_did_hack(PosLF, (-) , ~PosLF):-!.
maybe_did_hack(PosLF, (ly) , ly(PosLF)):-!.
maybe_did_hack(PosLF, _, PosLF).

maybe_negated_dcg(DCGGoal1, LF) -->
 {quietly(append_term(DCGGoal1, PosLF, DCGGoal0))},
  pos_or_neg(Did), !, DCGGoal0,
 {maybe_did_hack(PosLF, Did, LF)}.

has_suffix(Ly, Ly) --> theText1(DasterdLy), {atom_concat(Dasterd, Ly, DasterdLy)}, dcg_push_w2(Dasterd).
has_suffix('ly', 'ly') --> theText1(Happily), {atom_concat(Happ, 'ily', Happily), atom_concat(Happ, 'y', Happy)}, dcg_push_w2(Happy).
has_suffix(_Ly, skipped) --> [].

dcg_maybe_suffixed(DCGGoal1, Suffix, LF) -->
  {append_term(DCGGoal1, PosLF, DCGGoal0)},
   has_suffix(Suffix, Did), !, DCGGoal0,
  {maybe_did_hack(PosLF, Did, LF)}.

% =================================================================
%  Noun Units
% =================================================================
whpron(X, LF, Out) --> theText1(WH), {whpron_dict(WH, Type)}, !, add_traits(X, [pronounWHFn(WH), iza(Type)], LF, Out).

  whpron_dict(whom, tAgent).
  whpron_dict(who, tAgent).
  whpron_dict(when, actDoing).
  whpron_dict(what, tSomethingExisting).
  whpron_dict(why, tSituation).
  whpron_dict(how, actDoing).
  whpron_dict(Which, tThing):- talkdb:talk_db(pronoun, Which), atom_concat('w', _, Which).


% pers_pron_lex
%pronoun(_SO, X, _LF, _Out) --> determiner(X, _), !, {fail}.

pronoun(SO, X, LF, Out) --> theText1(noone), dcg_push_w2(nobody), !, pronoun(SO, X, LF, Out).

pronoun(_SO, X, LF, Out) --> theText1(none), add_traits(X, [pronounQFn("none"), quant(no)], LF, Out), !.
pronoun(_SO, X, LF, Out) --> theText1(one), add_traits(X, [pronounQFn("one"), numberOf(1), quant(exists)], LF, Out), !.
pronoun(_SO, X, LF, Out) --> theText1(some), add_traits(X, [pronounQFn("some"), atLeast(1), quant(exists)], LF, Out), !.

pronoun(SO, X, LF, Out) --> theText1(WH), {WH=him;WH=he}, !, add_traits(X, [pronounSubjFn(WH), varnamedFn('He'), v_arg(SO), gender(masc)], LF, Out).

%pronoun(SO, X, LF, Out) --> theText1(one), dcg_push_w2(someone), !, pronoun(SO, X, LF, Out).

pronoun(SO, X, LF, Out) --> theText1(Nobody), {nl_call(quantifier_pron_lex, Nobody, No, Body), pronoun_ok(_Subj, SO)},
 {pronoun_var(Body, VarFn, X)},
  add_traits(X, [pronounQuantFn(Nobody), quant(No), varnamedFn(VarFn), Body], LF, Out), !.

pronoun(SO, X, LF, Out) --> theText1(She), {nl_call(pers_pron_lex, She, Fem, Third, Sing, Subj), nop(pronoun_ok(Subj, SO))},
 {pronoun_var(She, VarFn, X)},
  add_traits(X, [pronounPersonalFn(She), gender(Fem), person(Third), varnamedFn(VarFn), Sing, v_arg(Subj)], LF, Out), !.

pronoun(SO, X, LF, Out) --> theText1(Herself), {nl_call(reflexive_pronoun, VarName, Herself, Traits),
  (member(v_arg(O), Traits)-> pronoun_ok(O, SO); true), pronoun_var(VarName, VarFn, X)},
  add_traits(X, [pronounReflxFn(Herself), varnamedFn(VarFn)|Traits], LF, Out), !.

pronoun(SO, X, LF, Out) --> {fail}, theText1(Hers),
 {atom_string(Hers, StrHers), nl_call(acnl, 'pronounStrings', CycWordHers, StrHers, _),
  findall_set1(pos(Prop), nl_call(acnl, 'partOfSpeech', CycWordHers, Prop, StrHers, _), Props)},
  add_traits(X, [pronounCycFn(Hers), v_arg(SO)|Props], LF, Out).

pronoun(SO, X, LF, Out) --> theText1(WH), {talkdb:talk_db(pl_pronoun, WH)}, !, add_traits(X, [pronounPluralFn(WH), v_arg(SO)], LF, Out).

pronoun(SO, X, LF, Out) --> theText1(WH), {talkdb:talk_db(fem, WH)}, !, add_traits(X, [pronounFemFn(WH), v_arg(SO), gender(fem)], LF, Out).
pronoun(SO, X, LF, Out) --> theText1(WH), {whpron_dict(WH, Type)}, !, add_traits(X, [pronounSubjWHFn(WH), v_arg(SO), iza(Type)], LF, Out).

pronoun(SO, X, LF, Out) --> theText1(WH), {talkdb:talk_db(pronoun, WH)}, !, add_traits(X, [pronounUnkFn(WH), v_arg(SO)], LF, Out).


findall_set1(T, G, S):- findall(T, G, L), list_to_set(L, S).

pronoun_ok(Obj, Subject):- Obj == obj(_K), Subject == subj, !, fail.
pronoun_ok(_80, _SO).


reflexive_pronoun(she, herself, [v_arg(obj(_K)), person(3), sg, gender(fem)]).
reflexive_pronoun(he, himself, [v_arg(obj(_K)), person(3), sg, gender(masc)]).
reflexive_pronoun(it, itself, [v_arg(obj(_K)), person(3), sg, gender(neut)]).
reflexive_pronoun(them, themself, [v_arg(obj(_K)), person(3), pl, iza(tAgent)]).
reflexive_pronoun(you, yourself, [v_arg(obj(_K)), person(2), pl, iza(tAgent)]).
reflexive_pronoun(me, myself, [v_arg(obj(_K)), person(1), sg, iza(tAgent)]).
reflexive_pronoun(you, yourself, [person(2), sg, iza(tAgent)]).
reflexive_pronoun(us, ourselves, [person(1), pl, iza(tAgent)]).
reflexive_pronoun(us, ourself, [v_arg(obj(_K)), person(1), pl, iza(tAgent)]).
reflexive_pronoun(them, themselves, [person(3), pl, iza(tAgent)]).
reflexive_pronoun(you, yourselves, [person(2), pl, iza(tAgent)]).


value_obj(_SO, Entity, LF, LF) --> proper_noun(Entity).
value_obj(_SO, X, LF, LF) --> numberic_value(X).
value_obj(_SO, X, LF, Out) --> named_var_match(len(1), X, LF, Out).
value_obj(_SO, X, LF, MProps & LF) --> adjective1(X, MProps).

numberic_value(N) --> w2txt(nb(N)), !, {nonvar(N)}.
numberic_value(N) --> w2txt(W), {atom_number(W, N)}.
numberic_value(N) --> [A], {atom(A), atom_number(A, N)}, !.
numberic_value(N) --> [N], {number(N)}, !.

proper_noun(Entity) --> quietly((w2txt(PN1), w2txt(PN2), {was_propercase(PN1), was_propercase(PN2),
                                             downcase_atom(PN1, DN1), downcase_atom(PN2, DN2),
                                             atomic_list_concat([DN1, DN2], '_', Entity)})).
proper_noun(Entity) --> quietly((w2txt(PN), {pn_lf(PN, Entity)})).


   pn_lf(nb(N), N):-!.
   pn_lf(Name, Name):- atom(Name), atom_contains(Name, '~').
   pn_lf(Name, Value) :- atom(Name), pn_dict(Name), i_name(i, Name, Value).

   was_propercase(Name):- atomic(Name), atom_length(Name, L), L>1, toPropercase(Name, PC), !, PC==Name.

    pn_dict(Name):- was_propercase(Name), !.
    pn_dict(Name):- parser_chat80:name_template_LF(Name, _).


    %pn_dict(Name):- atom(Name), downcase_atom(Name, Down),
    pn_dict_tiny(begriffsschrift, place).  pn_dict_tiny(gottlob, city).

    pn_dict_tiny(principia, book).
    pn_dict_tiny(thingy, thing).
    pn_dict_tiny(lunar, robot).
    pn_dict_tiny(shrdlu, robot).
    pn_dict_tiny(monet, person).
    pn_dict_tiny(alfred, person).
    pn_dict_tiny(terry, person).
    pn_dict_tiny(john, person).
    pn_dict_tiny(dmiles, person).
    pn_dict_tiny(annie, person).
    pn_dict_tiny(bertrand, person).
    pn_dict_tiny(ohad, person).
    pn_dict_tiny(bill, person).


%noun(_SO, X, ~something(X)) --> theText1(nothing), nvd(nothing, X).
%noun(_SO, X, something(X)) --> theText1(something), nvd(something, X).

% person/unperson
noun(SO, X, LF) --> maybe_negated_dcg(noun1(SO, X), LF).
noun(SO, X, LF) --> pronoun(SO, X, true, LF).
ohnun(_SO, X, LF) --> named_var_match(len(1), X, true, LF).

noun1(SO, X, LF) --> theText1(N), {N\==are, noun_lf(SO, X, N, LF)}, nvd(N, X).
% noun1(SO, X, LF) --> named_var_match(Sub, SO, X, true, LF).

 noun_lf(_SO, X, Word, Out) :-
   (Word=Sing;Word=Plur), talkdb:talk_db(noun1, Sing, Plur), !,
    ISA = Sing,
    (Sing==Plur -> add_traits(X, [ISA, denote(Word)], true, Out) ;
    (Word==Plur ->  add_traits(X, [ISA, pl], true, Out) ;
     add_traits(X, [ISA, sg], true, Out))).

 noun_lf(_SO, X, Mass, Out) :- talkdb:talk_db(noun2, Mass), add_traits(X, [Mass, denote(Mass)], true, Out).
 noun_lf(_SO, X, Sing, Out) :- noun_dict(Sing), add_traits(X, [ Sing], true, Out).

  noun_dict(author).  noun_dict(human).  noun_dict(book).  noun_dict(professor).  noun_dict(program).
  noun_dict(programmer).  noun_dict(student).  noun_dict(man).  noun_dict(woman).


% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% INLINED VARS %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================
named_var_match(Sub, X, LF, Out) --> w2txt(?), w2txt(VarName), {sub_var_match(Sub, VarName, _VarFn, X, LF, Out)}, !.
named_var_match(Sub, X, LF, Out) --> w2txt(QVAR), {atom_concat('?', VarName, QVAR), sub_var_match(Sub, VarName, _VarFn, X, LF, Out)}, !.
named_var_match(Sub, X, LF, Out) --> w2txt(QVAR), {atom_concat('varref', VarName, QVAR), sub_var_match(Sub, VarName, _VarFn, X, LF, Out)}, !.

%var_match(pronoun(_X), VarName):- !, pronoun_to_var(VarName, _).
var_match(len(X), VarName):- !, atom_length(VarName, Len), Len==X.
% OR
var_match(A+B, VarName):- !, var_match(A, VarName)  ;  var_match(B, VarName).
var_match([A|B], VarName):- !, var_match(A, VarName)  ;  var_match(B, VarName).
% NOT
var_match(-B, VarName):- !, \+ var_match(B, VarName).
var_match(A-B, VarName):- !, var_match(A, VarName), \+ var_match(B, VarName).
% AND
var_match(A*B, VarName):- !, var_match(A, VarName)  , var_match(B, VarName).
var_match(startsWith(Sub), VarName):- downcase_atom(Sub, SubDC), downcase_atom(VarName, VarDown), atom_concat(SubDC, _, VarDown).
var_match(endsWith(Sub), VarName):- downcase_atom(Sub, SubDC), downcase_atom(VarName, VarDown), atom_concat(_, SubDC, VarDown).
var_match(contains(Sub), VarName):- downcase_atom(Sub, SubDC), downcase_atom(VarName, VarDown), atom_contains(VarDown, SubDC).

sub_var_match(Sub, VarName, VarFn, X, LF, Out):- VarName\=='',
   var_match(Sub, VarName),
   my_framevar(VarName, VarFn, X),
   add_traits(X, [varnamedFn(VarFn)], LF, Out), !.

my_framevar(VarName, VarFn, X):- downcase_atom(VarName, DC), pronoun_to_var(DC, Var), !, foc_framevar(Var, VarFn, X).
my_framevar(Var, VarFn, X):- foc_framevar(Var, VarFn, X), !.

pronoun_var(PN, VarFn, X):- pronoun_to_var(PN, VarFn), !, foc_framevar(VarFn, _, X).

pronoun_to_var(PN, _):- \+ atomic(PN), !, fail.
pronoun_to_var(PN, VarFn):- atomic(PN), downcase_atom(PN, DC), DC\==PN, !, pronoun_to_var(DC, VarFn).
pronoun_to_var(PN, VarFn):- reflexive_pronoun(Var, PN, _), toPropercase(Var, VarFn).
pronoun_to_var(i, 'Me').
pronoun_to_var(her, 'She').
pronoun_to_var(him, 'He').
pronoun_to_var(we, 'Us').
pronoun_to_var(they, 'Them').
pronoun_to_var(PN, VarFn):- talkdb:talk_db(personal, PN), toPropercase(PN, VarFn).
pronoun_to_var(PN, VarFn):- reflexive_pronoun(PN, _, _), toPropercase(PN, VarFn).
pronoun_to_var(PN, VarFn):- reorder_if_var(PN, atom_concat(PN, 'self', Self), reflexive_pronoun(Use, Self, _)), toPropercase(Use, VarFn).
pronoun_to_var(PN, VarFn):- nl_call(quantifier_pron_lex, PN, No, Body), atom_concat(No, Body, VarName), toPropercase(VarName, VarFn).

