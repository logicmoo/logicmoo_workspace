% ===================================================================
% File 'parser_lexical_gen.pl'
% Purpose: English to KIF conversions from SWI-Prolog
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_lexical_gen.pl' 1.0.0
% Revision:  $Revision: 1.666 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:- module(parser_lexical_gen, [skip_lex_arg_type/2, guess_arg_type/2]).



generate_mfa(M, F, A):-
  (integer(A)-> true ; between(2, 13, A)),
  (var(F)-> (gen_preds_m_p(M, F, A), MU=M) ; true),
   (once((functor(C, F, A),
         current_predicate(MU:F/A),
         once((predicate_property(MU:C, imported_from(M))
           -> true
           ; ( ( \+ predicate_property(MU:C, imported_from(_))), MU=M)))))).

make_first_arg_caller(Arg, M, F, A):-
  (\+ atom(M) ; \+ atom(F); \+ integer(A)) , !,
  forall(generate_mfa(M, F, A),
    ( % wdmsg(make_first_arg_caller(Arg, M, F, A)),
     make_first_arg_caller(Arg, M, F, A))).

make_first_arg_caller(Arg, M, F, A):-
  make_first_arg_caller(Arg, M, F, A, _, _).

make_first_arg_caller(Arg, M, F, A, P, C):-
  format('~N% -- ~q. -- %~n', [maybe_skip_search(M, F, A)]),
  functor(C, F, A), functor(P, F, A),
  ignore(arg(Arg, C, X)), ignore(arg(Arg, P, +(X))),
  matcher_to_data_args(var, data, P),
  findall(P, M:C, L), sort(L, S), % pprint(S),
  print_first_arg_caller(M, F, A, P, C, S).
print_first_arg_caller(M, _, _, P, _, S):-
  length(S, L), L<50, !,
  maplist({M}/[E]>>
    (P=E, % matcher_to_data_args(==(data), _, P),
     % matcher_to_data_args(get_vv(X), X, P),
      ignore(print_some_clauses(M, P))), S), !.
print_first_arg_caller(M, F, A, _, _, _):-
  functor(PC, F, A),
  print_some_clauses(M, PC).

nb_setarg_if_better(N, P, _):- arg(N, P, W), W== data, !.
nb_setarg_if_better(N, P, A):- nb_setarg(N, P, A).

guess_args_of(P, Guess):-
 ignore((arg(_, P, G), ground(G), matcher_to_data_args(nb_setarg_if_better, guess_arg_type(X), X, 1, P, Guess))),
 nop((writeq(guess_args_of(P, Guess)), nl)),
 ignore((
   arg(N1, Guess, text(a)),
   arg(N2, Guess, text(a)),
   N1\==N2,
   arg(N1, P, A1), arg(N2, P, A2),
   A1\==A2,
   is_atom_word(A1), is_atom_word(A2),
   atom_concat(A1, _, A2),
   atom_length(A1, L), L > 3,
   nb_setarg(N1, Guess, text(base)),
   nb_setarg(N2, Guess, text(a)))),
 duplicate_term(Guess, GuessD),
 duplicate_term(Guess, Guess),
 nb_setval('$guess', GuessD).


guess_arg_type(X, S):- string(S), X= text(str).
guess_arg_type(X, S):- is_synset_id(S), X = id(wn).
guess_arg_type(X, S):- number(S), X = data.
guess_arg_type(X, S):- var(S), X = data.
guess_arg_type(X, S):- S==[], X = unk.
guess_arg_type(listof(X), S):- S=[E|_], nonvar(E), guess_arg_type(X, E), X \== unk.
guess_arg_type(X, S):- \+ atom(S), !, X = unk.

guess_arg_type(X, S):- S=='', X = data.
guess_arg_type(pos, neutr).
guess_arg_type(X, S):- atom_concat(_, '_tt', S), X = concept(tt).
guess_arg_type(X, S):- atom_concat(_, 'TheWord', S), X = id(cyc).
guess_arg_type(X, S):- atom_concat('TTWord', _, S), X = id(tt).
guess_arg_type(T, S):- atom_length(S, L), L>2,
  atom_chars(S, C),
  exclude([X]>>(char_type(X, lower), char_type(X, alpha)), C, Cs),
  left_over_word_type(Cs, T).
guess_arg_type(_, unk).

% (Cs==[] -> T = text(a) ; Cs = ['-'] -> T = mword ; Cs=['_'] -> T = concpt() ).
% left_over_word_type(Cs, T).
left_over_word_type([], text(a)).
left_over_word_type(['_'], text(a)).
left_over_word_type(['-'], wordh).
left_over_word_type([_], unk).


rule_clause_porp_ok(RCl, RCl):- !, fail.
rule_clause_porp_ok(0, _):- !.
rule_clause_porp_ok(1, 2):- !, fail.
rule_clause_porp_ok(Rl, _):- Rl > 2, !, fail.
rule_clause_porp_ok(Rl, Cl):- Rl + 3 < Cl.

print_some_clauses(M, P):-
  duplicate_term(P, Call),
  duplicate_term(P, Guess),
  format('~n  % ----------~n'),
  format('~N  %  ?- ~q. ~n', [print_some_clauses(M, P)]),
  matcher_to_data_args(==(data), _, Call),
  matcher_to_data_args(get_vv(X), X, Call),
  flag('$sample', _, 0),
  print_some_clauses_g(Guess, M, Call), !,
  format('~N lex_arg_type( sem, 0, ~p, ~p).~n', [M, Guess]).

 print_some_clauses_g(Guess, M, P):-  arg(_, P, V), nonvar(V), !,
  MP=M:P, predicate_property(MP, number_of_clauses(_Cl)),
   repeat,
   call(M:P),
   flag('$sample', X, X+1),
   format('~N  %          lex_arg_type( sem, 0, ~p, ~p).~n', [M, P]),
   guess_args_of(P, Guess),
   X>5, !.

print_some_clauses_g(Guess, M, P):-
  MP=M:P, predicate_property(MP, number_of_clauses(Cl)),
   repeat,
   Nth is random(Cl)+1, nth_clause(MP, Nth, Ref), clause(P, G, Ref),
   ignore(G),
   flag('$sample', X, X+1),
   format('~N  %          lex_arg_type( sem, 0, ~p, ~p).~n', [M, P]),
   guess_args_of(P, Guess),
   X>5, !.





print_some_clauses_g(Guess, M, P):-
 MP=M:P,
 predicate_property(MP, number_of_clauses(Cl)),
 ((Cl>7;true) -> findnsols(5, _, ((
   repeat, Nth is random(Cl)+1, nth_clause(MP, Nth, Ref), clause(P, _, Ref),
   guess_args_of(P, Guess),
   format('~N  %          lex_arg_type( sem, 0, ~p, ~p).~n', [M, P+Guess]))), _);
 findnsols(5, _, ((clause(P, true, _Ref),
   format('~N ex_lex_arg_type2( _, ~p, ~p).~n', [M, P]), guess_args_of(P, Guess) )), _)), !.

gen_preds_m_p(M, F, A):- (atom(M)->true;member(M, [
 nldata_freq_pdat, nldata_BRN_WSJ_LEXICON, nldata_dictionary_some01, nldata_colloc_pdat,
 parser_chat80,
 parser_e2c, tt0,
 wnframes, vndata, framenet, clex, talk_db])),
 current_predicate(M:F/A), A>1, functor(P, F, A),
 MP=M:P,
 F\==verbnet_frame,
 \+ predicate_property(MP, imported_from(_)),
 predicate_property(MP, number_of_rules(Rl)),
 predicate_property(MP, number_of_clauses(Cl)),
 rule_clause_porp_ok(Rl, Cl).

gen_preds_m_p(M, F, A):-
 M:F/A = MFA,
 member(MFA,
 [% ace_niceace:atom_capitalize/2,
 vndata:verbnet_frame/7,
 %parser_e2c:type_wrd_sem/3,
 %talk_db:talk_db/2, talk_db:talk_db/3, talk_db:talk_db/4, talk_db:talk_db/6,

 mu:thetaRole/9
 % wnframes:opposite/4,
 ]),
 current_predicate(MFA), functor(P, F, A),
 must(\+ predicate_property(M:P, imported_from(_))).




gen_preds_m_p :- forall(gen_preds_m_p(M, F, A), (functor(P, F, A), print_some_clauses(M, P))).



:- dynamic(maybe_skip_search/2).

maybe_skip_search(parser_chat80, active_passive_subjcase, 2). %%
maybe_skip_search(parser_chat80, anot_xmask, 3). %%
maybe_skip_search(parser_chat80, apply_set, 5). %%
maybe_skip_search(parser_chat80, compatible_pos_db, 2). %%
maybe_skip_search(parser_chat80, conj_apply, 4). %%
maybe_skip_search(parser_chat80, deepen_case, 2). %%
maybe_skip_search(parser_chat80, extract_var, 3). %%
maybe_skip_search(parser_chat80, freevars, 2). %%
maybe_skip_search(parser_chat80, i_neg, 2). %%
maybe_skip_search(parser_chat80, i_sup_op, 2). %%
maybe_skip_search(parser_chat80, index_det, 2). %%
maybe_skip_search(parser_chat80, index_slot, 3). %%
maybe_skip_search(parser_chat80, is_to_role_case, 3). %%
maybe_skip_search(parser_chat80, is_trace82, 2). %%
maybe_skip_search(parser_chat80, lists_first, 2). %%
maybe_skip_search(parser_chat80, marked, 4). %%
maybe_skip_search(parser_chat80, must_test_80, 3). %%
maybe_skip_search(parser_chat80, must_test_802, 3). %%
maybe_skip_search(parser_chat80, must_test_803, 3). %%
maybe_skip_search(parser_chat80, must_test_804, 3). %%
maybe_skip_search(parser_chat80, must_test_80_sanity, 3). %%
maybe_skip_search(parser_chat80, nd, 3). %%
maybe_skip_search(parser_chat80, nd, 4). %%
maybe_skip_search(parser_chat80, nd, 5). %%
maybe_skip_search(parser_chat80, noText, 4). %%
maybe_skip_search(parser_chat80, op_apply, 3). %%
maybe_skip_search(parser_chat80, open_quant, 5). %%
maybe_skip_search(parser_chat80, or_xmask, 3). %%
maybe_skip_search(parser_chat80, participle_vt, 3). %%
maybe_skip_search(parser_chat80, portray_bit, 4). %%
maybe_skip_search(parser_chat80, pos_conversion_db, 5). %%
maybe_skip_search(parser_chat80, quant_op, 4). %%
maybe_skip_search(parser_chat80, slot_match, 5). %%
maybe_skip_search(parser_chat80, slot_tag, 3). %%
maybe_skip_search(parser_chat80, strip_key, 2). %%
maybe_skip_search(parser_chat80, subquery, 6). %%
maybe_skip_search(parser_chat80, test_quiet, 4). %%
maybe_skip_search(parser_chat80, txt_no_db, 2). %%
maybe_skip_search(parser_chat80, txt_not_db, 2). %%
maybe_skip_search(parser_chat80, txt_there_db, 2). %%
maybe_skip_search(parser_chat80, value80, 3). %%
maybe_skip_search(parser_chat80, virtual, 3). %%
maybe_skip_search(parser_e2c, make_object, 4). %%
maybe_skip_search(parser_e2c, test_e2c, 2). %%
maybe_skip_search(parser_e2c, to_person, 2). %%
maybe_skip_search(talk_db, clause_always, 2). %%
maybe_skip_search(talk_db, talk_db_argsIsa, 3). %%
maybe_skip_search(talk_db, talk_db_pos_trans, 2). %%
maybe_skip_search(vndata, verbnet_external_id, 4). %%
maybe_skip_search(vndata, verbnet_pred, 2). %%
maybe_skip_search(vndata, verbnet_type, 2). %%
maybe_skip_search(wnframes, ss_type, 2). %%
maybe_skip_search(wnframes, wn_cat, 2). %%
maybe_skip_search(parser_chat80, punct_to_sent_type, 3).
maybe_skip_search(parser_chat80, chat80, 2).

:- dynamic(skip_lex_arg_type/2).

skip_lex_arg_type(parser_chat80, inverse_db(more, -, less)).
skip_lex_arg_type(parser_e2c, det_quantify(text(a), data, data, data)).
skip_lex_arg_type(parser_e2c, into_split(pos, data)).
skip_lex_arg_type(M, P):- compound(P), functor(P, F, A), maybe_skip_search(M, F, A), !.


/*
lex_arg_type( syn, 0, clex, clex_adj(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, adj_itr(text(a), text(base))).
lex_arg_type( syn, 0, clex, adj_itr_comp(text(a), text(base))).
lex_arg_type( syn, 0, clex, adj_itr_sup(text(a), text(base))).

lex_arg_type( syn, 0, clex, adj_tr(text(a), text(base), prep)).
lex_arg_type( syn, 0, clex, adj_tr_comp(text(a), text(base), prep)).
lex_arg_type( syn, 0, clex, adj_tr_sup(text(a), text(base), prep)).

lex_arg_type( syn, 0, clex, adv(text(a), text(base))).
lex_arg_type( syn, 0, clex, adv_comp(text(a), text(base))).
lex_arg_type( syn, 0, clex, adv_sup(text(a), text(base))).
lex_arg_type( syn, 0, clex, pndef_sg(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, pndef_pl(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, dv_finsg(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, dv_infpl(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, dv_pp(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, iv_finsg(text(a), text(base))).
lex_arg_type( syn, 0, clex, iv_infpl(text(a), text(base))).
lex_arg_type( syn, 0, clex, mn_pl(text(a), text(base))).
lex_arg_type( syn, 0, clex, mn_sg(text(a), text(base))).
lex_arg_type( syn, 0, clex, noun_mass(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, noun_sg(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, pn_pl(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, pn_sg(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, noun_pl(text(a), text(base), data)).
lex_arg_type( syn, 0, clex, prep(text(a), data)).
lex_arg_type( syn, 0, clex, tv_finsg(text(a), text(base))).
lex_arg_type( syn, 0, clex, tv_infpl(text(a), text(base))).
lex_arg_type( syn, 0, clex, tv_pp(text(a), text(base))).
*/


%:- gen_preds_m_p.


/*

gen_preds_atomic(C, P):- !, fail,
 gen_preds_m_p(C, M, P, GoalPre, GoalPost),
 GoalPre,
 do_gen_preds_atomic(C, M, P),
 GoalPost.

qcallc(MP):- quietly((catch(call(MP), _, fail))).

do_gen_preds_atomic(C, M, P):- !, arg(_, P, C), call(M:P).
do_gen_preds_atomic(C, M, P):- fail,
 binds_with(C, W), arg(_N, P, W),
 qcallc(M:P), nonvar(W).
do_gen_preds_atomic(C, M, P):- fail,
 qcallc(M:P), notrace(sub_var(C, P)).
do_gen_preds_atomic(C, M, P):-
 qcallc(M:P), notrace(arg(_, P, C)).

binds_with(C, W):- put_attr(W, binds_atomic, C).

binds_atomic:attr_unify_hook(C, Val):- C==Val.
binds_atomic:attr_unify_hook(C, Val):- compound(Val), !, arg(_, Val, V), V==C.
%binds_atomic:attr_unify_hook(C, Val):- compound(Val), notrace(sub_var(C, Val)).

% term_to_info(C, P):- between(2, 12, A), functor(P, cyc_t, A), arg(N, P, C), N>1, call(P).
*/




end_of_file.







specialWrite(List):- specialWrite(' ', ' ', List).
specialWrite_commas(List):- specialWrite(' ', ', ', List).
specialWrite_and(List):- specialWrite(', ', ', and ', List).

from_isEach(isEach(SubList), SubList).
from_isEach(WASEACH, SubList):- WASEACH =.. [isEach|SubList].

% specialWrite(+Before, +After, +Conj, +List).
specialWrite(_, _, _, []).
% isEach
specialWrite(Before, After, Conj, [H|T]):-
   from_isEach(H, SubList),
   write(Before),
   specialWrite_and(SubList),
   write(After),
   specialWrite(Before, After, Conj, T).
% last Item
specialWrite(Before, After, Conj, [H, T]):-
   write(Before),
   write(H),
   write(Conj),
   specialWrite(Before, After, Conj, T).
% Rest Of Items
specialWrite(Conj, [H|T]):-
   write(Before),
   write(H),
   write(After),
   specialWrite(Before, After, Conj, T).




