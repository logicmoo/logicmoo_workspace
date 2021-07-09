% ===================================================================
% File 'parser_lexical.pl'
% Purpose: English to KIF conversions from SWI-Prolog
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_ProNTo.pl' 1.0.0
% Revision:  $Revision: 1.666 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:-module(parser_lexical, [ ]).


:- set_module(class(library)).
:- set_module(base(system)).

% ?- use_module(library(logicmoo_nlu/parser_lexical)).

:- multifile(check:list_undefined/1).
:- dynamic(check:list_undefined/1).
:- use_module(library(make)), use_module(library(check)), redefine_system_predicate(check:list_undefined/1).
%:- abolish(check:list_undefined/1).
:- asserta((check:list_undefined(Stuff):- Stuff==[], wdmsg(list_undefined(Stuff)),!)).
:- listing(check:list_undefined).
% :- break.


:- use_module(library(pfc_lib)).
:- use_module(nl_pipeline).


:- '$set_source_module'(baseKB).
:- module(baseKB).
:- use_module(library(pfc)).

:- share_mp(common_logic_kb_hooks:cyckb_t/1).
:- share_mp(common_logic_kb_hooks:cyckb_t/2).
:- share_mp(common_logic_kb_hooks:cyckb_t/3).
:- share_mp(common_logic_kb_hooks:cyckb_t/4).
:- share_mp(common_logic_kb_hooks:cyckb_t/5).
:- share_mp(common_logic_kb_hooks:cyckb_t/6).
:- share_mp(common_logic_kb_hooks:cyckb_t/7).
:- forall(between(1, 8, N), share_mp(common_logic_kb_hooks:cyckb_t/N)).

:- system:use_module(parser_stanford).

:- kb_global(baseKB:nlfw/4).
%:- share_mp(nlf:f/4).

:- use_module(parser_lexical_gen). 

guess_strip_module(M:F,M,F):- !.
guess_strip_module(MF,M,MF):- atom(MF), functor(P,MF,2),!,guess_strip_module(P,M,_).
guess_strip_module(MF,M,MF):- predicate_module(MF,M).
guess_strip_module(MF,M,F):- strip_module(MF,M,F).

connect_preds(HMF, BMF):- 
 guess_strip_module(HMF,HM,HF),
 guess_strip_module(BMF,BM,BF),
 forall(between(1, 13, N),
 ( length(ARGS, N),
   share_mp(BM:BF/N),multifile(HM:HF/N),dynamic(HM:HF/N),
   H=..[HF|ARGS],
   B=..[BF|ARGS],
   multifile(BM:BF/N),dynamic(BM:BF/N),   
   asserta_if_new(HM:(H:- BM:B)))).


common_logic_kb_hooks:cyckb_t(A, B, C):- cyckb_p2(A, [B, C]).
common_logic_kb_hooks:cyckb_t(A, B, C, D):- cyckb_p2(A, [B, C, D]).
common_logic_kb_hooks:cyckb_t(A, B, C, D, E):- cyckb_p2(A, [B, C, D, E]).
common_logic_kb_hooks:cyckb_t(A, B, C, D, E, F):- cyckb_p2(A, [B, C, D, E, F]).

:- connect_preds(common_logic_kb_hooks:cyckb_t, cyckb_h).
:- connect_preds(cyckb_h, kb0988:ac).
%:- connect_preds(ac, t).

cyckb_p2(A, BC):- \+ is_list(BC), !, between(2, 10, N), length(BC, N), cyckb_p2(A, BC).
cyckb_p2(A, [B, C|D]):- atom(C), downcase_atom(C, C), cvt_to_real_string(C, S), cyckb_p3(A, B, [S|D]).
cyckb_p2(A, [B, B1, C|D]):- atom(C), downcase_atom(C, C), cvt_to_real_string(C, S), cyckb_p3(A, B, [B1, S|D]).
cyckb_p2(A, [B, C|D]):- string(C), into_text100_atoms(C, O), O\=[_], maplist(cvt_to_real_string, O, S), ST=..[s|S], cyckb_p3(A, B, [ST|D]).
cyckb_p2(A, [B, B1, C|D]):- string(C), into_text100_atoms(C, O), O\=[_], maplist(cvt_to_real_string, O, S), ST=..[s|S], cyckb_p3(A, B, [B1, ST|D]).
%cyckb_p2(A, [B, C|D]):- \+ ((arg(_, v(B, C), X), compound(X))), between(2, 10, N), functor(S, s, N), arg(_, cyckb_h(B, C), S), cyckb_p3(A, B, [C|D]).
%cyckb_p2(A, [B, C|D]):- cyckb_p3(A, B, [C|D]).

cyckb_p3(A, B, [H|T]):- apply(cyckb_h(A, B), [H|T]).

is_synset_id(X):- integer(X), X > 100001739.

synset_to_w(X, W1, SK):- wnframes:s(X, W1, SK, _, _, _)*->true;wnframes:sk(X, W1, SK).
synset_to_w(X, W1, SK):- wnframes:sk(X, W1, SK).

synset_to_words(X, W1, Name):-
   findall(SK, wnframes:s(X, W1, SK, _, _, _), List),
   must_or_rtrace(predsort(longer_names, List, _Set)),
   must(synset_to_w(X, W1, SK)),
   must(wnframes:g(X, G)),
   concat_missing(SK, [X, G|List], Name), !.

longer_names(R, C2, C1):- atom_length(C1, L1), atom_length(C2, L2), compare(R, L1, L2), R \== (=), !.
longer_names(R, C2, C1):- compare(R, C1, C2).

concat_missing(Named, [Name|More], Out):- atom_contains(Named, Name)
 -> concat_missing(Named, More, Out)
 ;(atomic_list_concat([Named, '-', Name], M), concat_missing(M, More, Out)).
concat_missing(Named, [], Out):- Named=Out.

%synset_to_words(X, S, Set):- findall(SK, ((nonvar(S)->true;member(S, [5, 4, 3, 2, 1])), wnframes:sk(X, S, SK)), List), list_to_set(List, Set), Set\==[], !.

english_some(X, Words):- is_synset_id(X), synset_to_words(X, _, Words), !.
english_some(X, Y):- \+ compound(X), !, Y=X.
english_some([fr, X1, M, X2|More], Y):- is_synset_id(X1), synset_to_words(X1, X2, SK), !, english_some([vnframe, M, SK|More], Y). 
english_some([X1, X2|More], Y):- integer(X2), is_synset_id(X1), synset_to_words(X1, X2, SK), !, english_some([SK|More], Y).
english_some(H-T, HH-TT):- !, english_some(H, HH), =(T, TT).
english_some([H|T], [HH|TT]):- !, english_some(H, HH), english_some(T, TT).
english_some(X, Y):-
  compound_name_arguments(X, F, Args), F \== sk,
  english_some([F|Args], [_|ArgsO]), !,
  compound_name_arguments(Y, F, ArgsO).
english_some(X, X):- !.

lex_frivilous(senseExamples).
lex_frivilous(senseComments).
lex_frivilous(senseDefinition).
lex_frivilous(X):- lex_frivilous_maybe(X).


y_skip(Info, Why):- compound(Info),!,sub_term(Sub,Info),atom(Sub),y_skip(Sub,Why),dmsg(y_skip(Info,Why)).
y_skip(Info, mws):- lex_mws(Info).
y_skip(Info, friv):- lex_frivilous(Info).
y_skip(isa, friv).

lex_frivilous_maybe(posForms).
lex_frivilous_maybe(posBaseForms).
lex_frivilous_maybe(subcatFrame).
lex_frivilous_maybe(s).
lex_frivilous_maybe(g).

lex_frivilous_col(xtLexicalWord).
lex_frivilous_col(xtEnglishWord).
lex_frivilous_col(tIndividual).

:- add_e2c("a red cat fastly jumped onto the table which is in the kitchen of the house").
:- add_e2c("After Slitscan, Laney heard about another job from Rydell, the night security man at the Chateau.").
:- add_e2c("Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.").
:- add_e2c("Concrete beams overhead had been hand-painted to vaguely resemble blond oak.").
:- add_e2c("The chairs, like the rest of the furniture in the Chateau s lobby, were oversized to the extent that whoever sat in them seemed built to a smaller scale.").
:- add_e2c("Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").
:- add_e2c("A little tribute to Gibson", noun_phrase).
:- add_e2c('"You look like the cat that swallowed the canary, " he said, giving her a puzzled look.', [quotes]).
:- add_e2c("The monkey heard about the very next ship which is yellow and green.").

%lex_mws(genTemplateConstrained).
%lex_mws(genTemplate).
lex_mws(headMedialString).
lex_mws(compoundString).

lex_mws(prepCollocation).
lex_mws(abbreviationForMultiWordString).
lex_mws(multiWordStringDenotesArgInReln).
lex_mws(compoundSemTrans).
lex_mws(multiWordSemTrans).
lex_mws(multiWordString).
lex_mws(mws).
lex_mws(xPPCompFrameFn).
lex_mws(hyphenString).


%is_word(W):- atom(W), guess_arg_type(X, W), !, X==text(a).
%is_atom_word(W):- atom(W), guess_arg_type(X, W), !, X==text(a).

not_contains_overlap(A, W):- \+ contains_overlap(A, W).

contains_overlap(_Atoms, Was):- \+ (select(txt(_O1), Was, Was1), member(txt(_O2), Was1)), !.

contains_overlap(A, Was):- atomic(A), !, member(txt(A), Was), !.
contains_overlap(A;B, Was):- !, contains_overlap(A, Was);contains_overlap(B, Was).
contains_overlap(E, Was):- is_list(E), !, contains_overlap_list(E, Was), !.

contains_overlap(X, Was):- compound(X), arg(_, X, E), is_list(E), !, contains_overlap_list(E, Was), !.
contains_overlap(X, Was):- compound(X), atoms_of(X, E), contains_overlap(E, Was), !.

contains_overlap_list([], _).
contains_overlap_list([A|B], Was):- !, append(_Left, [txt(A)|Right], Was), contains_overlap_list(B, Right), !.

contains_overlap_list(Atoms, Was):- is_list(Atoms), !,
  list_to_set(Atoms, Atoms1),
  select(A1, Atoms1, Atoms2),
  member(A2, Atoms2),
  binds_with(A1, O1),
  binds_with(A2, O2),
  member(Find1, [txt(O1)]), member(Find2, [txt(O2)]),
  select(Find1, Was, Was1), member(Find2, Was1),
  nop(wdmsg(contains_overlap( A1+O1, A2+O2))), !.

to_atom_or_string(A, W):-  nonvar(W), !, to_atom_word(A, O), !, O==W.

to_atom_word(A, W):- nonvar(W), !, to_atom_word(A, O), !, O==W.
to_atom_word(A, W):- to_case_break_atoms(A, O), !, (O=[W]->true;(O=['"', W, '"']->true;O=[x, DC, 'The', 'Word'], downcase_atom(DC, W))).

filter_mmw(X, O):- A=val([]), filter_mmw(A, X, X), !, arg(1, A, O).

filter_mmw(_, _Was, X):- X == [], !.
filter_mmw(O, _Was, X):- var(X), !, append_o(var(X), O).
filter_mmw(O, Was, [H|T]):-!, filter_mmw(O, Was, H), filter_mmw(O, Was, T).
%filter_mmw(_, _Was, level(Kind, 1, _, _, _)):- !.
filter_mmw(O, Was, X):- select(X, Was, WasNt), !, filter_mmw(O, WasNt, X).
filter_mmw(O, Was, level(_, 0, _, X, _)):- !, filter_mmw(O, Was, X).
filter_mmw(_, Was, X):- compound(X), functor(X, flexicon, A), arg(A, X, E), not_contains_overlap(E, Was), !.
filter_mmw(_, Was, X):- compound(X), functor(X, MW, _), lex_mws(MW), not_contains_overlap(X, Was), !.
filter_mmw(_, _Was, X):- compound(X), functor(X, MW, _), lex_frivilous(MW), !.
filter_mmw(_, _Was, isa(_, MW)):- lex_frivilous_col(MW), !.
filter_mmw(O, _Was, X):- append_o(X, O).

append_o(X, O):- O=val([]), !, nb_setarg(1, O, [X]).
append_o(X, val(List)):- o_put(X, List).
o_put(F, List):- memberchk(F, List), !.
o_put(F, List):- List=[_|T], (T==[] -> nb_setarg(2, List, [F]) ; o_put(F, T)).

lex_print(X):- X == [], !, wdmsg(X), !.
lex_print(X):- is_list(X), !, maplist(lex_print0, X).
lex_print(X):- lex_print0(X), !.
lex_print0(level(_, 0, _, X, _)):- !, lex_print0(  X).
%lex_print0(level(_, 1, _, _, _)):- !.
lex_print0(isa(_, MW)):- lex_frivilous_col(MW), !.
%lex_print0(X):- english_some(X, Y), wdmsg(Y), !.
lex_print0(X):- english_some(X, Y), print_reply_colored(Y).

%cvt_to_qa_string(A, M):- atomic_list_concat(['"', A, '"'], M).
cvt_to_qa_string(A, M):- cvt_to_real_string(A, M).
cvt_to_atom(A, M):- atomic_list_concat([A], M).

cvt_to_real_string(NBA, M):- compound(NBA), NBA = nb(A), assertion(number(A)), !, cvt_to_real_string(A, M).
cvt_to_real_string(A, M):- atom_string(A, M).

% correct_dos(Todo, TodoS):- flatten([Todo], TodoF), (Todo\==TodoF -> my_l2s(TodoF, TodoS); TodoS=Todo), !.
correct_dos(Todo, TodoS):- flatten([Todo], TodoF), my_l2s(TodoF, TodoS), !.

add_do_more(More, Todo, NewDone, NewTodo):-
 flatten([More], MoreS),
 add_todo_list(MoreS, Todo, NewDone, NewTodo), !.

add_todo_list([], Todo, _Done, Todo):-!.
add_todo_list([M|MoreS], Todo, Done, NewTodo):- member_eq0(M, Done), !, add_todo_list(MoreS, Todo, Done, NewTodo).
add_todo_list([M|MoreS], Todo, Done, NewTodo):- add_if_new(Todo, [M], TodoM), !,
 add_todo_list(MoreS, TodoM, Done, NewTodo).

first_clause_only(Head):- Found=fnd(0), nth_clause(Head, Nth, Cl), Found==fnd(0),
   Nth\==1, clause(Head, Body, Cl), call(Body), nb_setarg(1, Found, Nth).


text_to_cycword(String, P, C, How):- !, first_clause_only(text_to_cycword(String, P, C, How)).
text_to_cycword(String, P, C, How):- \+ string(String), cvt_to_real_string(String, RealString), !, text_to_cycword(RealString, P, C, How).
text_to_cycword(String, P, C, cyckb_h(P, C, String)):- base_to_cycword(String, P, C).
text_to_cycword(String, P, C, How):- string_lower(String, DCString), DCString\==String, !, text_to_cycword(DCString, P, C, How).
text_to_cycword(String, Pos, C, (to_base_form(String, Pos, BaseWord), cyckb_h(Pred, C, BaseWord))):- fail,
  to_base_form(String, Pos, BaseWord), BaseWord\==String,
  base_to_cycword(BaseWord, Pred, C).

text_to_cycword(String, Pos, C, cyckb_h(Pred, C, BaseWord)):-
  to_base_form(String, Pos, BaseWord), BaseWord\==String,
  base_to_cycword(BaseWord, Pred, C).

to_base_form(String, Used, BaseWord):- \+ atom(String), string_to_atom(String, Atom), !, to_base_form(Atom, Used, BaseWord).
to_base_form(String, Used, BaseWord):- call_lex_arg_type(text(a), text(base), String, BaseWord, Used).
to_base_form(String, 'xtAgentitiveNoun', BaseWord):- morph_stem(String, BaseWord, 'er').
to_base_form(String, 'xtAdverb', BaseWord):- morph_stem(String, BaseWord, 'ly').
to_base_form(String, 'xtUn', BaseWord):- morph_stem(String, 'un', BaseWord).

morph_stem(String, Base, Suffix):- atom_concat(Base, Suffix, String).
morph_stem(String, Base, Suffix):- morph_atoms(String, [[Base, -Suffix]]).

base_to_cycword(String, Pos, C):- ac(partOfSpeech, C, Pos, String).
base_to_cycword(String, P, C):-
  nonvar(String), cvt_to_real_string(String, QAString), cyckb_h(P, C, QAString),
  ok_speech_part_pred(P).

%morph_atoms(causer, [[W, -er]]). W = cause

string_to_info(String, P):- fail,
 catch(downcase_atom(String, Atom), _, fail),
 atom_length(Atom, Len), Len > 1,
 term_to_info(Atom, P). % , functor(P, F, _), guess_pred_pos(P, String, Pos).

% string_to_pos(String, Pos):- atom_ string(Atom, String), term_to_info(Atom, P), guess_pred_pos(P, String, Pos).

guess_pred_pos(P, _String, Pos):- arg(_, P, Pos), nonvar(Pos), member(Pos, [n, a, s, v, a, j, r, jj, adv, adj, nn, pp, prep]), !.
guess_pred_pos(P, String, Pos):- arg(_, P, Pos), nonvar(Pos), Pos \== String, !.
%guess_pred_pos(P, _, Pos):- functor(P, Pos, _).

ok_speech_part_pred(P):-
 P\==firstNameInitial, P\==middleNameInitial,
 (
 cyckb_h(isa, P, rtSpeechPartPredicate); \+ cyckb_h(isa, P, _)), !.

subtype_index(_, +(_), _, _Value, _CArg, _PreCall, _PostCall):- !, fail.
subtype_index(_, W, W, Value, CArg, PreCall, PostCall):- PreCall = (CArg = Value), PostCall = true.
subtype_index(_, W, W- _Pos, Value, CArg, PreCall, PostCall):- PreCall = (CArg = (Value-_)), PostCall = (true;true).
subtype_index(_, text(a), text(str), Value, CArg, PreCall, PostCall):-  PreCall = cvt_to_real_string(Value, CArg), PostCall = true.
%subtype_index(_, text(a), text(base), Value, CArg, PreCall, PostCall):- PreCall = (CArg = Value), PostCall = true.
                                               
%subtype_index(_, W, any(W), Value, CArg, PreCall, PostCall):- !, PreCall = freeze(CArg, sub_var(Value, CArg)), PostCall = true.
subtype_index(_, W, any(W), Value, CArg, PreCall, PostCall):-  PreCall = true, PostCall = sub_value_word(Value, CArg).
%subtype_index(_, W, seq(W), Value, CArg, PreCall, PostCall):- /*atom(W), */ PreCall = (CArg = [_|_]), PostCall = member(Value, CArg).
subtype_index(_, W, seq(W), Value, CArg, PreCall, PostCall):- /*atom(W), */ PreCall = (CArg = [_|_]), PostCall = sub_value_word(Value, CArg).
subtype_index(_, W, listof(W), Value, CArg, PreCall, PostCall):- PreCall = (CArg = [_|_]), PostCall = member(Value, CArg).

sub_value_word(_Valu, CArg):- var(CArg),!,fail.
sub_value_word(Value, CArg):- is_list(CArg),!,CArg=[CArgF],sub_value_word(Value, CArgF).
sub_value_word(Value, CArg):- \+ compound(CArg),!,Value==CArg.
sub_value_word(Value, CArg):- arg(_,CArg,CArgF),sub_value_word(Value, CArgF).

doable_type(_, DoType, Type):- nonvar(DoType),
  % DoType\==text(str),
  DoType\==data, DoType=Type, !.

matcher_to_data_args( Matcher1, Data, P):-
  matcher_to_data_args(setarg, Matcher1, Data, 1, P, P), !.

matcher_to_data_args(SetArg, Matcher1, Data, N, C, P):-
  % functor(C, F, A), functor(P, F, A),
  ignore((arg(N, C, Match),
  copy_term(Matcher1+Data+Match, Matcher1C+DataC+MatchC),
  ignore((once(call(Matcher1C, MatchC)), (DataC==unk -> true ; call(SetArg, N, P, DataC)))),
  N2 is N+1,
  matcher_to_data_args(SetArg, Matcher1, Data, N2, C, P))).

% matcher_to_data_args(_Matcher1, _Data, _N, _C, _P):-!.

copy_value_args(P, C):- functor(P, F, A), functor(C, F, A), P=..[_|PList], C=..[_|CList], maplist(copy_value_arg, PList, CList).
copy_value_arg(P, C):- ignore((compound(P), P = +(C))).


is_atom_word(W):- atom(W), guess_arg_type(X, W), !, X==text(a).
get_vv(X, Arg):- compound(Arg), Arg = +(X).


get_test_verbs(V):- wnframes:s(_, _, V, v, _, _).
baseKB:sanity_test:- forall(get_test_verbs(V), lex_info(V)).



:- export(lex_winfo/1).
lex_winfo(Value):- 
  lex_winfo(Value,R),
  maplist(wdmsg, R).

merge_lists(L,R):- (L==[] ; R ==[]),!.
merge_lists(L,R):- nb_set_add(L,R),nb_set_add(R,L).

:- export(lex_winfo/2).
lex_winfo(W2,R):- is_list(W2),!, maplist(lex_winfo,W2,R),!.
lex_winfo(W2,R):- (var(W2);W2=span(_)),!,R=W2.
%lex_winfo(W2,W2):-!.
lex_winfo(W2,W2O):- W2=W2O, W2 = w(Word, Had),!, 
  nonvar(Word), !,  is_list(Had), 
  (member(lex_winfo,Had) -> true; 
     (lex_winfo_r(Word,R),unlevelize(R,R2),nb_set_add(W2,[lex_winfo|R2]))).
lex_winfo(Word,W2):- lex_winfo_r(Word,Had),  W2 = w(Word, [lex_winfo|Had]),!.
lex_winfo(W,W):-!.


lex_winfo_r(Word,R):- lex_tinfo(text(a), Word, R).


:- export(lex_tinfo/3).
lex_tinfo(Type, Value,DatumF):-
 findall(Datum, get_info_about_type(_All, 0, Type, Value, Datum), DatumL),
   correct_dos(DatumL, DatumF),
   nop(maplist(wdmsg, DatumF)), !.



unlevelize(R1,R2):- is_list(R1),!,maplist(unlevelize,R1,R2).
unlevelize(X,Y):- unlevelize0(X,M),!,unlevelize(M,Y).
unlevelize(X,X).

unlevelize0(level(_, 0, _, X, _),X):- !.
unlevelize0(level(_, _, _, X, _),X):- !.
unlevelize0(text_to_cycword(_, _,_,X),X):-!.
unlevelize0(todo(_, cycpred,X), cycpred):-  atom(X),!.
unlevelize0(todo(_, X,Y),Z):- atom(X),append_term(X,Y,Z).
unlevelize0(todo(_, X,Y),eq(X,Y)):-!.
%unlevelize0(todo(_, X,Y),Z):- append_term(X,Y,Z).


call_lex_arg_type(TypeIn, TypeOut, Value, Result, C):-
  find_lex_arg_type( _, _, M, P),
  arg(I, P, TypeIn), arg(O, P, TypeOut),
  I \== O, copy_value_args(P, C),
  arg(I, C, Value), arg(O, C, Result),
  call(M:C).

%converts_arg_type(system, =(X, X)).
converts_arg_type(system, atom_string(text(a), text(s))).
converts_arg_type(system, atom_string(text(base), text(s))).
converts_arg_type(system, atom_string(text(base), text(a))).

call_converter(TypeIn, TypeOut, Value, Result):-
  converts_arg_type(M, P),
  arg(I, P, TypeIn), arg(O, P, TypeOut),
  I \== O, copy_value_args(P, C),
  arg(I, C, Value), arg(O, C, Result),
  call(M:C).

find_lex_arg_type(Kind, Level, M, P):- lex_arg_type(Kind, Level, M, P), current_predicate(_, M:P).

get_info_about_type(Kind, Level, Type, Value, MoreF):-
  get_info_about_type0(Kind, Level, Type, Value, MoreF)
   *-> true
   ; get_info_about_type0(Kind, f(Level), Type, Value, MoreF).

get_info_about_type0(Kind, Level, Type, Value, MoreF):-
  (number(Level)-> Level2 is Level+1; Level2 = 1),
  find_lex_arg_type(Kind, Level, M, P),
  arg(N, P, Matcher),
  nonvar(Matcher),
  subtype_index(Level, Type, Matcher, Value, CArg, PreCall, PostCall),
  functor(P, F, A),
  functor(C, F, A),
  % \+ ((arg(N, P, PArg), PArg=text(base), arg(_, P, text(a)), \+ arg(_, P, pos))),
  copy_value_args(P, C),
  arg(N, C, CArg),
  call(PreCall),
   ignore(( (Level\==0, Level\==1 % ; PArg==text(base)
   ) , wdmsg(( P:Level -> (C, PostCall))))),
   matcher_to_data_args(=(Matcher), data, P),
  % wdmsg(M:get_info_about_type(Kind, Level, Type, Value, P->C, PostCall)),
  once((findall([level(Kind, Level, Type, C, Value)|Extra], ((call(M:C)), call(PostCall),
                         P=..[_|PRest], C=..[_|CRest],
                         make_new_todos(Kind, Type, Level2, CRest, PRest, [], Extra)), More1),
  flatten(More1, MoreF))).


make_new_todos(_Kind, _Was, _Level, [], [], InOut, InOut):- !.
make_new_todos(Kind, Was, Level, [C|CRest], [P|PRest], In, Out):-
 (var(C);var(P);
  C==[];
  (P = +(_));
  P==data;
  (P==text(base), Kind==syn);
  P == pos;
  (Was==P);
  (Was\==text(a), P==text(a));
  (P==text(str));
  (number(Level), Level>2);
  (Was==text(base), P==text(base))), !,
 make_new_todos(Kind, Was, Level, CRest, PRest, In, Out).


:- forall((clause(ac(_, xBadTheWord, _, _, TakingABath), true, R);clause(ac(_, xBadTheWord, _, _, _, TakingABath), true, R)),
  ignore((member(X, [actTakingABath, tGroupedSpa, tObjectHotTub]), sub_var(X, TakingABath),
   erase(R)))).


make_new_todos(Kind, Was, Level, [C|CRest], [P|PRest], In, Out):-
  (Was==text(a), P==text(base)),
  %Level0 is Level -1, !,
  make_new_todos(Kind, accept, Level, [C|CRest], [P|PRest], In, Out).

make_new_todos(Kind, Was, Level, [C|CRest], [P|PRest], In, Out):-
  fail,
  (Was==text(a), P==text(a)),
  Level0 is Level -1, !,
  make_new_todos(Kind, accept, Level0, [C|CRest], [P|PRest], In, Out).

make_new_todos(Kind, Was, Level, [C|CRest], [P|PRest], In, Out):-
 (P == pos),
 make_new_todos(Kind, Was, Level, CRest, PRest, In, Mid),
% wdmsg(data(Level, P, C)),
 add_if_new(Mid, data(Level, P, C), Out), !.

make_new_todos(Kind, Was, Level, [C|CRest], [P|PRest], In, Out):-
 make_new_todos(Kind, Was, Level, CRest, PRest, In, Mid),
 % wdmsg(adding_todo(Level, P, C)),
 add_if_new(Mid, todo(Level, P, C), Out), !.


add_if_new(Done, Doing, NewDone):-
  member_eq0(Doing, Done)
   -> NewDone = Done
   ; append(Done, [Doing], NewDone).

:- export(lexfw_info/1).
lexfw_info(String):-
 lexfw_info(_AllKind, String, Datum),
 lex_print(Datum).

:- kb_global(do_e2c_fwd/2).
:- kb_global(nl_pass1/1).
:- export(lexfw_info/3).
lexfw_info(_Kind, String, Out):-
 %mpred_retract(nl_pass1),
 forall(do_e2c_fwd(Was, _), mpred_retract(do_e2c_fwd(Was, _))),
 % forall(nlfw(M, N, Data, Ace), mpred_remove(nlfw(M, N, Data, Ace))),
 gensym(lexfw_info_, ID),
 into_text100_atoms(String, Words), % maplist(into_dm, Words, Todo),
 into_acetext(Words, Ace), cvt_to_real_string(Ace, SAce),
 ain(do_e2c_fwd(SAce, ID)), !,
 ain(nl_pass1),
 show_ace_id(ID),
 Out = [], !.

show_ace_id(ID):- forall((nlfw(N, M, Gaf, ID), \+ functor(Gaf, xclude, _)), wdmsg(nlfw(N, M, Gaf, ID))).

text_into_wall(String, ID, Ace, WalledWords, 0, N):-
 into_text100_atoms(String, Words),
 ignore(gensym(ace_, ID)),
 into_acetext(Words, AceA), cvt_to_real_string(AceA, Ace),
 maplist(cvt_to_real_string, Words, SWords),
 Left = ['L-WALL'|SWords],
 append(Left, ['R-WALL'], WalledWords), !,
 length(Left, N).

% :- prolog_load_context(source, File), format("~N~n?- ~q.~n", [(mpred_trace_exec, rtrace(mpred_remove_file_support(File)))]).
% :- break.


maybe_text(W, W):- atom_contains(W, '-WALL'), !.
maybe_text(W, txt(S)):- cvt_to_real_string(W, S).

% ((nl_pass1, nlfw(M, N, text80(WalledWords), Ace)/buffer_words(M, Ace, )


append_segment(N, M, Len, StringW, Ace):-
 segment(N, M, Len, StringW, Ace).


segment(N, M, Len, StringW, Ace):-
  nlfw(SN, SM, text80(Words), Ace),
  between(0, SN, N),
  between(N, SM, M),
  between(1, SM, Len),
  Len is M-N+1,
  length(Left, N),
  length(StringW, Len),
  append(Left, StringW, Words).


% ((nlfw(N, N, cycpos(xtWHDeterminer, _, _), Ace)/M is N+1) ==>nlfw(M, M, xclude(xtWHDeterminer,
/*
:- (prolog_load_context(reloading, true)
      -> (prolog_load_context(source, File), mpred_remove_file_support(File))
     ; true).


((nlfw(_N, _M, text80(Words), Ace)/nth0(NM, Words, W), maybe_text(W, WW))==> nlfw(NM, NM, WW, Ace)).
((do_e2c_fwd(String, ID)/text_into_wall(String, ID, Ace, WalledWords, M, N)) ==> (nlfw(M, N, ace_text(Ace), ID), nlfw(M, N, text80(WalledWords), ID))).
:- ain((nlfw(M, N, cycpos(xtPreposition, C, W), Ace)==> nlfw(M, N, xclude(xtAdverb, C, W), Ace))).
:- ain((nlfw(M, N, cycpos(xtAdjectiveGradable, C, W), Ace)==> nlfw(M, N, xclude(xtClosedClassWord, C, W), Ace))).
:- ain((nlfw(M, N, cycpos(xtAdjectiveGradable, C, W), Ace)==> nlfw(M, N, xclude(xtNoun, C, W), Ace))).
:- ain((nlfw(M, N, cycpos(xtPronoun, C, W), Ace)==> nlfw(M, N, xclude(xtDeterminer, C, W), Ace))).
:- ain((nlfw(M, N, cycpos(xtDeterminer, C, W), Ace)==> nlfw(M, N, xclude(xtAdverb, C, W), Ace))).
:- ain((nlfw(M, N, cycpos(xtDeterminer, C, W), Ace)==> nlfw(M, N, xclude(xtAdjective, C, W), Ace))).


((later_on, nlfw(M, N, txt(W), Ace)/member(Pos, [xtNoun, xtVerb, xtAdjective, xtAdverb, xtPronoun, xtPreposition, xtDeterminer]))
  ==> nlfw(M, N, maybe_pos(Pos, W), Ace)).
((nlfw(M, N, txt(W), Ace)/text_to_cycword(W, P, C, _Why)) ==> nlfw(M, N, cycword(P, C, W), Ace)).
((nlfw(M, N, txt(W), Ace)/clex_word(P, W, C, T)) ==> nlfw(M, N, clex_word(P, clexFn(C), T, W), Ace)).
%((nlfw(M, N, cycword(P, C, W), Ace)/cycpred_to_cycpos(P, Pos) ==> nlfw(M, N, cycpos(Pos, C, W), Ace))).
((
   nlfw(M, N, cycpos(Pos, C, W), Ace),
   \+ nlfw(M, N, xclude(Pos, C, W), Ace),
 %  nlfw(M, N, cycword(_, C, W), Ace),
  {ac(denotation, C, Pos, _, Subj)})==> nlfw(M, N, value(Subj, C, W), Ace)).

:- ain((nlfw(M, N, cycword(PosL, C, W), Ace)/(pos_inherit(PosL, PosH), \+ notInheritPos(PosH)))==>nlfw(M, N, cycpos(PosH, C, W), Ace)).
:- ain((nlfw(M, N, cycpos(PosL, C, W), Ace)/(pos_inherit(PosL, PosH), \+ notInheritPos(PosH)))==>nlfw(M, N, cycpos(PosH, C, W), Ace)).
:- ain((nlfw(M, N, xclude(PosL, C, W), Ace)/(pos_inherit(PosH, PosL)))==>nlfw(M, N, xclude(PosH, C, W), Ace)).
:- ain((nlfw(M, N, xclude(Pos, C, W), Ace))==> \+ nlfw(M, N, cycpos(Pos, C, W), Ace)).
%:- ain((nlfw(M, N, xclude(Pos, C, W), Ace))==> \+ nlfw(M, N, cycword(Pos, C, W), Ace)).
%:- ain((nlfw(M, N, cycpos(Pos, C, W), Ace))==> \+ nlfw(M, N, xclude(Pos, C, W), Ace)).
*/



:- export(lex_info/3).
lex_info(Kind, String, Out):-
 must_det_l((
 into_text100_atoms(String, Words), % maplist(into_dm, Words, Todo),
 into_acetext(Words, Ace), cvt_to_real_string(Ace, SAce),
 text_to_corenlp(SAce,[],Todo),
 maplist(print_reply_colored,Todo), print_reply_colored("==============================================================="),
 Level = 0,
 ignore((Todo=[])),
 findall(sentence(N,WS,Info),member(sentence(N,WS,Info),Todo),Sents),
 maplist(remove_broken_corefs(Sents),Todo,NewTodo),
 lex_info(Kind, Level, NewTodo, [text80(Words)], Datum),
 % maplist(lex_winfo(Kind, Level, Words), Words, Datums),append(Datums, Datum),
 filter_mmw(Datum, Out))), !.

lex_winfo(Kind, Level, Words, String, Datum):-
 cvt_to_atom(String, AString),
 lex_info(Kind, Level, txt(AString), [text80(Words)], Datum).

%into_dm(String, txt(AString)):- cvt_to_atom(String, AString).

didnt_do(Todo, skipped(Todo)).

remove_broken_corefs(Sents,coref(Sent,_, _, _,_,_,_,_,_,_,_),[]):- \+ member(sentence(Sent,_,_),Sents),!.
remove_broken_corefs(_Sents,sentence(N,Words,Info),sent(N,Words,Info)).
remove_broken_corefs(_Sents,A,A).

lex_info(_Kind, _Level, [], Done, Out):- !, Done = Out.
lex_info(Kind, Level, Todo, Done, Out):- correct_dos(Todo, TodoS), TodoS\==Todo, !, lex_info(Kind, Level, TodoS, Done, Out).
lex_info(Kind, Level, Todo, Done, Out):- correct_dos(Done, DoneS), DoneS\==Done, !, lex_info(Kind, Level, Todo, DoneS, Out).
lex_info(_Kind, Level, Todo, Done, Out) :- Level > 3, !, maplist(didnt_do, Todo, NotTodo), append(Done, NotTodo, Out).
lex_info(Kind, Level, Todo, Done, Out):- must(lex_info_loop(Kind, Level, Todo, Done, Out)).

lex_info_loop(Kind, Level, [M:Did|Todo], Done, Out):- atom(M), !, lex_info(Kind, Level, [Did|Todo], Done, Out).
lex_info_loop(Kind, Level, Todo, Done, Out):- lex_info_impl(Kind, Level, Todo, Done, Out), !.
lex_info_loop(Kind, Level, [Did|Todo], Done, Out):-
 Did =..[T, F|RestP],
 member(T, [acnl, cyckb_h, t, talk_db]),
 (T == acnl -> append(Rest, [_Ref], RestP) ; RestP= Rest),
% (T == t -> TAdd = Do ; TAdd = T),
 atom(F),
 Do =..[F|Rest],
 lex_info(Kind, Level, [Do|Todo], Done, Out).
lex_info_loop(Kind, Level, [Did|Todo], Done, Out):-
 compound(Did), functor(Did, F, A),
 findall(Arg, (arg(_, Did, Arg), nonvar(Arg), searches_arg(F, A, Arg)), List),
 List\==[],
 maplist(add_search_arg, List, DoNow),
 add_do_more(DoNow, Todo, Done, NewTodo),
 lex_info(Kind, Level, NewTodo, Done, Out).
lex_info_loop(Kind, Level, [Did|Todo], Done, Out):-
 add_if_new(Done, Did, NewDone),
 lex_info(Kind, Level, Todo, NewDone, Out).




lex_info_impl(Kind, Level, Todo, Done, Out):- 
   findall(sentence(N,Words,Info),member(sentence(N,Words,Info),Todo),Sents), Sents\==[],
   maplist(remove_broken_corefs(Sents),Todo,NewTodo), 
   NewTodo\==Todo, !, 
   lex_info(Kind, Level, NewTodo, Done, Out).


lex_info_impl(Kind, Level, [txt(String)|Todo], Done, Out):-
 cvt_to_atom(String, Atom),
 get_lex_info(Kind, text(a), Atom, Result),
 append(Done, Result, DoneResult), my_l2s(DoneResult, NewDone),
 lex_info(Kind, Level, Todo, NewDone, Out).

lex_info_impl(Kind, Level, [todo(Type, Value)| Todo], Done, Out):- !,
  lex_info(Kind, Level, [todo(Level, Type, Value)| Todo], Done, Out).

lex_info_impl(Kind, _Lev__, [todo(Level, DoType, Value)| Todo], Done, Out):-
 doable_type(Level, DoType, Type),
 Doing = todo( /*_Agent,*/ Level, Type, Value),
 add_if_new(Done, Doing, NewDone),
 findall(Info, get_info_about_type(Kind, Level, Type, Value, Info), More),
 add_do_more(More, Todo, NewDone, NewTodo),
 lex_info(Kind, Level, NewTodo, NewDone, Out), !.

lex_info_impl(Kind, Level, [cycWord(P, CycWord)|Todo], Done, Out):-
 findall(concept(Subj), cycword_to_cycconcept(P, CycWord, Subj), More1),
 findall(Info, term_to_infolist(CycWord, Info), More2),
 add_if_new(Done, cycWord(P, CycWord), NewDone),
 add_do_more([More1, More2], Todo, NewDone, NewTodo),
 lex_info(Kind, Level, NewTodo, NewDone, Out).

lex_info_impl(Kind, Level, [concept(C)|Todo], Done, Out):- fail,
 findall(Info, term_to_infolist(C, Info), More2),
 add_if_new(Done, concept(C), NewDone),
   add_do_more(More2, Todo, NewDone, NewTodo),
   lex_info(Kind, Level, NewTodo, NewDone, Out).


lex_info_impl(Kind, Level, [Sent|Todo], Done, Out):-
 Sent = sent(N,Words,_Info),
   % append(Done,[Sent],NewDone),     
   add_do_more([Words, do_mws(N,Words)], Todo, Done, NewTodo),
   lex_info(Kind, Level, NewTodo, Done, Out).

lex_info_impl(Kind, Level, [TOK|Todo], Done, Out):- 
 TOK = tok(Index,PennPos,_Base,String, Info), !,
 functor(TOK,_,A),
  must_or_rtrace((   
   findall_set(CPos,extend_brillPos(PennPos,CPos),CPosSet),
   POSINFO = [PennPos|CPosSet],
   append(Info,POSINFO,PropsTOK),
   nb_setarg(A,TOK,PropsTOK),
   findall_set(How, (text_pos_cycword(String, POSINFO, How)), CycWordInfo),
   nb_set_add(PropsTOK,CycWordInfo),   
   append(Done,Todo,AllInfo),
   findall_set(How, (find_coref(Index, AllInfo, How)), CorefInfo),
   nb_set_add(PropsTOK,CorefInfo),
   forall(member(cycWord(CycWord),PropsTOK),
   (findall_set(How, (cycword_sem(CycWord, PropsTOK, How)), CycSem),
    nb_set_add(PropsTOK,CycSem))),
   sort(PropsTOK,PropsTOKS),
   nb_setarg(A,TOK,PropsTOKS),     
   lex_info(Kind, Level, Todo, Done, Out))).

find_coref(Index, AllInfo, [coREF(CRN,RefNum),A,B,C,D]):- 
  member(coref( _, seg(Index-_), RefNum, Words,
   A,B,C,D,CRN,_TrackBack, _Attribs),AllInfo), 
   arg(1,RefNum,Num),
   (number(Num) -> 
     (atomic_list_concat(Words,'_',NameI),
      atomic_list_concat([NameI,Num],'__',NameD),
      upcase_atom(NameD,Name),
      nb_setarg(1,RefNum,'$VAR'(Name))) ; true).
find_coref(Index, AllInfo, [coREF(CRN,RefNum)]):- 
  member(coref( _, seg(S-E), RefNum, _Words,
   _A,_B,_C,_D,CRN,_TrackBack, _Attribs),AllInfo),
   Index>S,Index=<E.


findall_set(Temp,Goal,Set):-   
   findall(Temp,Goal,List),flatten(List,Flat),list_to_set(Flat,Set),!.

text_pos_cycword(String, MorePos, [cycWord(C)|Out]):- 
  cvt_to_atom(String,AString),text_to_cycword(AString, P, C, How), 
  (not_violate_pos(MorePos,[P,How])
    ->Out=[P]
     ;(Out=[y_violate(How)])). 

cycword_sem(CycWord, MorePos, Out):- 
  term_to_info(CycWord, Info),
  (y_skip(Info,Why) -> Out=y_skip(Why);
   (not_violate_pos(MorePos,Info)
    ->Out=Info
     ;(Out=y_skip(violate),dmsg(y_violate(Info))))).

% :- forall(ac(mostSpeechPartPreds, B, C), retract(ac(speechPartPreds, B, C))).
cycpred_to_cycpos(Pred, Pos):- atom(Pred), pos_inherit(Pred, M),atom(M),atom_concat(xt,_,M),M\==xtNLWordForm,
  Pred\==M,(M=Pos;cycpred_to_cycpos(M, Pos)),atom(Pos).

cycpred_to_cycpos_1(Pred, Pos):- nonvar(Pred),
 ac(speechPartPreds, Pos, Pred), \+ ac(mostSpeechPartPreds, Pos, _), \+ ac(mostSpeechPartPreds, _, Pred).




filter_lex(OutS,[PennPos|MorePos],OutF):-
 include(not_violate_pos([PennPos|MorePos]),OutS,OutF).

%not_violate_pos(_,_):-!.
not_violate_pos(_MorePos,Var):- var(Var),!.
not_violate_pos(_MorePos,[]):-!.
not_violate_pos(MorePos,[H|T]):- !, not_violate_pos(MorePos,H),not_violate_pos(MorePos,T).
not_violate_pos(MorePos,OutS):- violate_pos(MorePos,OutS),!,fail.
not_violate_pos(_MorePos,_OutS).

violate_pos(MorePos,OutS):- \+ compound(OutS), !, violate_pos1(MorePos,OutS).
violate_pos(MorePos,Did):- Did =..[T, F|Rest], member(T, [acnl, cyckb_h, t, talk_db]),
 atom(F), Do =..[F|Rest], !,                                     
 violate_pos(MorePos,Do).
% violate_pos(MorePos,OutS):- functor(OutS,F,_),violate_pos1(MorePos,F),!.
violate_pos(MorePos,OutS):- functor(OutS,F,_),pos_inherit(F, Pos),violate_pos1(MorePos,Pos).
violate_pos(MorePos,OutS):- arg(_,OutS,E), atom(E), violate_pos1(MorePos,E),!.
violate_pos(MorePos,OutS):- violate_pos1(MorePos,OutS).

pos_list([xtCoordinatingConjunction,xtVerb,xtAdjective,xtAdverb,xtPreposition,xtPunctuationSP]).
         

incompatible_pos(Pos1,Pos2):- pos_list(PosList),member(Pos2,PosList),member(Pos1,PosList),Pos1\==Pos2.
incompatible_pos(Pos1,Pos2):- pos_list(PosList),member(PosA,PosList),member(PosB,[xtDeterminer,xtNoun,xtPronoun]),
  ((Pos1=PosA,Pos2=PosB);(Pos1=PosB,Pos2=PosA)).

% cyckb_h(denotation,xUseTheWord,xtMassNoun,0,actUsingAnObject)

violate_pos1(MorePos,OutS):- incompatible_pos(XtNoun,XtVerb),member(XtNoun,MorePos),pos_inherit(OutS,XtVerb),
  dmsg(incompatible_pos(XtVerb,XtNoun,MorePos)).
violate_pos1(_,todo).
violate_pos1(_,txt).
violate_pos1(_,comment).
violate_pos1(_,flexicon).
violate_pos1(_,M):- atom(M),member(M,[mws,flexicon,fsr]).
%violate_pos(MorePos,OutS,OutF).


notInheritPos(xtSententialConstituent).
notInheritPos(xtWHAdverb).
notInheritPos(xtWHWord).
notInheritPos(tIndividual).
notInheritPos(tThing).
notInheritPos(xtNLWordForm).

pos_upwards(N,xtAdjective):- member(N,[adjSemTrans,xRegularAdjFrame,xtAdjectiveGradable]).
pos_upwards(N,xtVerb):- 
 member(N,[verbSemTrans,
  verbSemTransCanonical,
  templateExpressionForVerbWRTClassAndFrame,
  sententialPhraseForVerbWithFrameGeneric,
  verbSenseGuessedFromVerbClass]).
pos_upwards(N,xtNoun):- member(N,[nounSemTrans,agentiveNounSemTrans]).
pos_upwards(PosL, PosH):- ac(genls, PosL, PosH).



pos_inherit_u(PosL, PosH):- pennSyntacticTags(PosL, PosH).
pos_inherit_u(Pred,  Pos):- ac(speechPartPreds, Pos, Pred).
pos_inherit_u(PosL, PosH):- pos_upwards(PosL, PosH).
pos_inherit_u(PosL, PosH):- ac(syntacticCategoryTags,PosH,PosL).


pos_inherit_d(PosL, PosH):- ac(syntacticCategoryTags,PosH,PosL).
pos_inherit_d(PosL, PosH):- pos_upwards(PosL, PosH).
pos_inherit_d(Pred,  Pos):- ac(speechPartPreds, Pos, Pred).
pos_inherit_d(PosL, PosH):- pennSyntacticTags(PosL, PosH).

pos_inherit_all(Pos, Pos).
pos_inherit_all(Pred, PosO):- nonvar(PosO),!, pos_inherit_d(Mid, PosO), pos_inherit_all(Pred, Mid).
pos_inherit_all(Pred, PosO):- var(Pred),pos_list(List),!,member(PosH,[xtDeterminer,xtNoun,xtPronoun|List]), pos_inherit_all(PosO, PosH), pos_inherit_all(Pred, PosO).
pos_inherit_all(Pred, PosO):- pos_inherit_u(Pred, Pos), \+ notInheritPos(Pos), pos_inherit_all(Pos, PosO).

pos_inherit(Pred, PosO):- no_repeats(pos_inherit_all(Pred, PosO)).



extend_brillPos(In,[Out]):- freeze(cvt_to_real_string(In,Str)),ac(pennTagString,Out,Str).
extend_brillPos('PRP$',['Possessive','xtNoun']):- !.
extend_brillPos('PRP$',['Possessive'|Rest]):- extend_brillPos('PRP',Rest).
%extend_brillPos('PRP',['SpecialDeterminer','xtDeterminer','second']).
extend_brillPos(In,Out):- bposToCPos(In,Out).
extend_brillPos(In,form(Out)):- bposToCPosForm(In,Out).
extend_brillPos(In,Out):- brillPos([In|Out]) *-> true 
 ; (freeze(In,downcase_atom(In,DC)),freeze(DC,upcase_atom(DC,In)),In\==DC,brillPos([DC|Out])).


add_search_arg(Arg, concept(Arg)).

  % 202488488
searches_arg(_F, _A, _Arg):- !, fail.
searches_arg(_F, _A, Arg):- is_synset_id(Arg), !.
searches_arg(_F, _A, Arg):- \+ atom(Arg), !, fail.
searches_arg(_F, _A, Arg):- atom_length(Arg, Len), Len<4, !, fail.
% searches_arg(_F, _A, Arg):- atom_contains(Arg, '.'), !.
%searches_arg(_F, _A, Arg):- (atom_contains(Arg, '.');atom_contains(Arg, '-');atom_contains(Arg, '%')), !.

:- ensure_loaded(library('../ext/ProNTo/Schlachter/pronto_morph_engine.pl')).
%  morph_atoms(causer, [[W, -er]]).


:- abolish(tmp:saved_denote_lex/3).
:- dynamic(tmp:saved_denote_lex/3).
%get_lex_info(Kind, text(a), String, Out):- catch(downcase_atom(String, DCAtom), _, fail), DCAtom\==String, !, get_lex_info(Kind, text(a), DCAtom, Out).
get_lex_info(_Kind, Type, DCAtom, Out):- tmp:saved_denote_lex(Type, DCAtom, Out), !.
get_lex_info(Kind, Type, DCAtom, Out):- do_lex_info(Kind, Type, DCAtom, Out), asserta(tmp:saved_denote_lex(Type, DCAtom, Out)), !.


do_lex_info(Kind, text(Type), AString, OutS):-
 findall([cycWord(P, C), Kind], text_to_cycword(AString, P, C, Kind), More1),
 NewDone = [txt(AString)],
 cvt_to_atom(AString, Atom),
 Level = 0,
 add_do_more([todo( Level, text(Type), Atom), More1], [], NewDone, NewTodo),
 lex_info(Kind, Level, NewTodo, NewDone, Out), !,
% =(Out, OutS).
 predsort(ignore_level, Out, OutS).

% my_l2s(List, Set) :- !, List=Set.
my_l2s(List, Set) :-
    must_be(list, List),
    lists:number_list(List, 1, Numbered),
    sort(1, @=<, Numbered, ONum),
    lists:remove_dup_keys(ONum, NumSet),
    sort(2, @=<, NumSet, ONumSet),
    pairs_keys(ONumSet, Set), !.

ignore_level( ( = ), level(Kind, _, _, C1, _), level(Kind, _, _, C2, _)):- compare(( = ), C1, C2), !.
ignore_level(R, C1, C2):- compare(R, C1, C2), !.



%term_to_info(C, P):- gen_preds_atomic(C, P).
%term_to_info(C, P):- between(2, 12, A), functor(P, cyckb_h, A), call(P), sub_term(X, P), X==C.
term_to_infolist(C, _Info):- number(C), \+ (C > 100001739 ; C < - 100000), !, fail.
term_to_infolist(C, Info):-
 findall(P, term_to_info(C, P), L),
 correct_dos(L, Info).

%term_to_info(Term, Info):- in_call(Term, Info, Template, cyckb_h('genTemplate', _, Template)).
%term_to_info(Term, Info):- in_call(Term, Info, Template, cyckb_h('genTemplateConstrained', _, _, Template)).
term_to_info(Term, Info):- Info=cyckb_h(_Pred, Cont), call(Info), sub_var(Term, Cont).
term_to_info(Term, Info):- Info=cyckb_h(_Pred, Term, S), call(Info), \+ string(S).
term_to_info(Term, Info):- Info=cyckb_h(_Pred, Term, _, S), call(Info), \+ string(S).
term_to_info(Term, Info):- between(5, 12, A), functor(Info, cyckb_h, A), arg(N, Info, Term), N>1, call(Info).

%term_to_info(C, P):- ac_nl_info_1(C, Results), member(P, Results).
%term_to_info(C, P):- between(3, 12, A), functor(P, acnl, A), arg(N, P, C), N<A, N>1, call(P).

in_call(C, P, Template, Call):- P=Call, call(P), once(sub_var(C, Template)).


cycword_to_cycconcept(Pred, C, Subj):- ac(speechPartPreds, Pos, Pred), ac(denotation, C, Pos, _, Subj).
% cycword_to_cycconcept(_P, C, Subj):- acnl(denotation, C, _, _, Subj, _).


:- dynamic(lex_arg_type/4).

lex_arg_type( _, _, M, P):- nonvar(P), skip_lex_arg_type(M, P), !.



lex_arg_type( syn, 0, parser_lexical, text_to_cycword(text(a), cycpred, cycword, data)).
%lex_arg_type( syn, _, parser_lexical, cycpred_to_cycpos(cycpred, cycpos)).
lex_arg_type( syn, _, parser_lexical, cycword_to_cycconcept(-cycpred, -cycword, value)).


lex_arg_type( syn, 0, clex, clex_word(pos, text(a), text(base), data)).

lex_arg_type( syn, 0, verbnet, verbnet(text(b), data, data, data)).
lex_arg_type( sem, 0, verbnet, verbnet(text(b), data, data, data)).

lex_arg_type( syn, 0, framenet, fnpattern(text(a), id(fn), concept(fn), data)).

lex_arg_type( sem, 0, framenet, frel(+(causative_of), concept(fn2), concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(coreset), concept(fn2), concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(excludes), concept(fn2), concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(inchoative_of), concept(fn2), concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(inheritance), data, concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(perspective_on), concept(fn2), concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(precedes), concept(fn2), concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(reframing_mapping), concept(fn2), concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(requires), concept(fn2), concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(see_also), concept(fn2), concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(subframe), concept(fn2), concept(fn2))).
lex_arg_type( sem, 0, framenet, frel(+(using), concept(fn2), /*concept(fn2)*/ data )).
lex_arg_type( sem, 0, framenet, frels(+(causative_of), concept(fn2), concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(coreset), concept(fn2), concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(excludes), concept(fn2), concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(inchoative_of), concept(fn2), concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(inheritance), data, concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(perspective_on), concept(fn2), concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(precedes), concept(fn2), concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(reframing_mapping), concept(fn2), concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(requires), concept(fn2), concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(see_also), concept(fn2), concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(subframe), concept(fn2), concept(fn2), data, data)).
lex_arg_type( sem, 0, framenet, frels(+(using), concept(fn2), data, /* concept(fn2), */ data, data)).

%lex_arg_type( syn, 0, framenet, fsr(text(a)-pos, concept(fn), data)).
lex_arg_type( sem, 0, framenet, semtype(concept(fn), data, data)).
lex_arg_type( syn, 0, mu, thetaRole(text(a), data, concept(tt2), data, data, concept(tt2), text(str), text(str), data)).

lex_arg_type( sem, 0, tt0, ttholds(concept(tt), concept(tt))).
lex_arg_type( sem, 0, tt0, ttholds(data, concept(tt))).
lex_arg_type( sem, 0, tt0, ttholds(data, id(tt), pos)).
lex_arg_type( sem, 0, tt0, ttholds(data, id(tt), pos, data)).
lex_arg_type( sem, 0, tt0, ttholds(data, id(tt), pos, data, concept(tt))).
lex_arg_type( sem, 0, tt0, ttholds(data, concept(tt), data)).
lex_arg_type( sem, 0, tt0, ttholds(pos, id(tt), text(str))).

lex_arg_type( syn, f(0), nldata_BRN_WSJ_LEXICON, text_bpos(text(a), pos)).
lex_arg_type( syn, 0, nldata_dictionary_some01, explitVocab(text(a), pos)).
lex_arg_type( syn, f(0), nldata_freq_pdat, text_bpos(data, text(a), pos)).
lex_arg_type( sem, 0, nldata_colloc_pdat, mws(seq(text(a)), pos)).
lex_arg_type( sem, 0, nldata_dictionary_some01, dictionary(pos, seq(text(a)), seq(text(a)))).

lex_arg_type( sem, 0, parser_chat80, adj_sign_lex(text(base), data)).
% lex_arg_type( sem, 0, parser_chat80, adjunction_lf(text(base), data, data)).
lex_arg_type( syn, 0, parser_chat80, adjunction_lf(any(text(a)), data, data, data)).
lex_arg_type( syn, 0, parser_chat80, aggr_adj_lex(text(a), data, data, text(base))).
lex_arg_type( syn, 0, parser_chat80, aggr_noun_lex(text(a), data, data, text(base))).
lex_arg_type( sem, 0, parser_chat80, borders(text(base), text(base))).
lex_arg_type( sem, 0, parser_chat80, city(text(base), text(base), data)).
lex_arg_type( sem, 0, parser_chat80, comparator_lex(text(base), data, data, data, data)).
lex_arg_type( sem, 0, parser_chat80, contains(text(base), text(base))).
lex_arg_type( sem, 0, parser_chat80, contains0(text(base), text(base))).
lex_arg_type( sem, 0, parser_chat80, context_pron_lex(text(base), data, data)).
% lex_arg_type( sem, 0, parser_chat80, adj_lex(text(a), data)).
lex_arg_type( sem, 0, parser_chat80, country(text(base), text(base), data, data, data, data, text(base), text(base))).
lex_arg_type( sem, 0, parser_chat80, det_lex(text(base), data, text(base), data)).
lex_arg_type( sem, 0, parser_chat80, in_continent(text(base), text(base))).
lex_arg_type( sem, 0, parser_chat80, int_art_lex(text(a), data, data, data)).
lex_arg_type( sem, 0, parser_chat80, int_pron_lex(text(a), data)).
lex_arg_type( sem, 0, parser_chat80, intrans_LF(text(base), data, data, data, data, data)).
%lex_arg_type( sem, 0, parser_chat80, inverse_lex(text(a), data, text(a))).
lex_arg_type( sem, 0, parser_chat80, latitude80(text(base), data)).
lex_arg_type( sem, 0, parser_chat80, loc_pred_prep_lex(text(a), data, data)).
lex_arg_type( sem, 0, parser_chat80, measure_op_lex(text(a), data, data, data)).
lex_arg_type( sem, 0, parser_chat80, measure_unit_type_lex(text(a), data, data, data)).
lex_arg_type( syn, 0, parser_chat80, meta_noun_lex(text(a), data, data, data, data, data, data)).
%lex_arg_type( sem, 0, parser_chat80, name_db(seq(text(a)), text(base))).
lex_arg_type( syn, 0, parser_chat80, pers_pron_lex(text(a), pos, data, pos, pos)).
lex_arg_type( syn, 0, parser_chat80, poss_pron_lex(text(a), pos, data, pos)).
lex_arg_type( syn, 0, parser_chat80, pronoun_to_var(text(a), upcase(text(a)))).
%lex_arg_type( sem, 0, parser_chat80, punct_to_sent_type(text(base), data, pos)).
lex_arg_type( sem, 0, parser_chat80, quantifier_pron_lex(text(a), text(base), data)).
lex_arg_type( sem, 0, parser_chat80, ratio_lex(text(base), text(base), data, data)).
lex_arg_type( sem, 0, parser_chat80, rel_adj_lex(text(a), text(base))).
lex_arg_type( sem, 0, parser_chat80, rel_pron_lex(text(a), pos)).
lex_arg_type( sem, 0, parser_chat80, river_pathlist(text(base), any(text(base)))).
lex_arg_type( sem, 0, parser_chat80, sup_adj_lex(text(a), text(base))).
lex_arg_type( sem, 0, parser_chat80, sup_op(text(a), data)).
lex_arg_type( syn, 0, parser_chat80, terminator_lex(text(a), data)).
lex_arg_type( sem, 0, parser_chat80, tr_number(text(a), data)).
lex_arg_type( sem, 0, parser_chat80, trans_LF(text(a), data, data, data, data, data, data, data, data)).
lex_arg_type( sem, 0, parser_chat80, type_measured_by_pred_db(data, data, text(a))).
lex_arg_type( sem, 0, parser_chat80, units_lex(text(a), data)).
lex_arg_type( sem, 0, parser_chat80, regular_past_lex(text(a), text(base))).
lex_arg_type( sem, 0, parser_chat80, subj_obj_LF(data, text(a), data, data, data, data, data)).

lex_arg_type( sem, 0, parser_e2c, aux_lf(text(a), data, data, data)).
lex_arg_type( syn, 0, parser_e2c, char_type_sentence(text(a), pos)).
lex_arg_type( syn, 0, parser_e2c, comparative_number(seq(text(a)), data)).
lex_arg_type( syn, 0, parser_e2c, flexicon(pos, data, any(text(a)))).
lex_arg_type( syn, 0, parser_e2c, idiomatic_replace(seq(text(a)), seq(text(a)))).

lex_arg_type( syn, 0, parser_e2c, is_junct(text(a), data)).
lex_arg_type( syn, 0, parser_e2c, pn_dict_tiny(text(a), data)).
lex_arg_type( syn, 0, parser_e2c, reflexive_pronoun(text(base), text(a), data)).
lex_arg_type( syn, 0, parser_e2c, type_wrd_frm5(pos, text(a), data, data, data)).
lex_arg_type( syn, 0, parser_e2c, type_wrd_sem(pos, any(text(a)), data)).
lex_arg_type( syn, 0, parser_e2c, type_wrd_sem5(pos, text(a), data, data, data)).
lex_arg_type( syn, 0, parser_e2c, type_wrd_wrd_sem6(pos, text(a), text(base), data, data, data)).
lex_arg_type( syn, 0, parser_e2c, whpron_dict(text(a), data)).

lex_arg_type( syn, 0, talk_db, talk_db(pos, text(a))).
lex_arg_type( syn, 0, talk_db, talk_db(+(domain), text(base), data)).
lex_arg_type( syn, 0, talk_db, talk_db(+(noun1), text(base), text(a))).
lex_arg_type( syn, 0, talk_db, talk_db(+(superl), text(base), text(a))).
lex_arg_type( syn, 0, talk_db, talk_db(+(comp), text(base), text(a))).
lex_arg_type( syn, 0, talk_db, talk_db(pos, text(a), text(a), text(base))).
lex_arg_type( syn, 0, talk_db, talk_db(pos, text(base), text(a), text(a), text(a), text(a))).

lex_arg_type( sem, 0, vndata, verbnet_class(concept(vn), data, concept(vn), listof(concept(vn)))).
lex_arg_type( sem, 0, vndata, verbnet_example(concept(vn), data)).
lex_arg_type( sem, 0, vndata, verbnet_frame(data, verb(vn(concept(vn))), data, data, data, data, concept(vn))).
lex_arg_type( sem, 0, vndata, verbnet_frame_prop(concept(vn), data, data)).
lex_arg_type( sem, 0, vndata, verbnet_frame_vars(concept(vn), data, data)).
lex_arg_type( sem, 0, vndata, verbnet_initial_vars(concept(vn), data, data)).
lex_arg_type( sem, 0, vndata, verbnet_map_wn(text(a), listof(concept(wn)), concept(vn))).
lex_arg_type( sem, 0, vndata, verbnet_semantics(concept(vn), data)).
lex_arg_type( sem, 0, vndata, verbnet_syntax(concept(vn), data)).
lex_arg_type( sem, 0, vndata, verbnet_to_framenet(concept(vn), text(a), concept(fn))).
lex_arg_type( syn, 0, vndata, verbnet_word(text(a), concept(vn), data)).

lex_arg_type( syn, 0, wnframes, sk(id(wn), data, data)).
lex_arg_type( syn, 0, wnframes, s(id(wn), data, text(a), pos, data, data)).
lex_arg_type( sem, 0, wnframes, syntax(id(wn), data, pos)).
lex_arg_type( sem, 0, wnframes, ant(id(wn), data, id(wn), data)).
lex_arg_type( sem, 0, wnframes, at(id(wn), id(wn))).
lex_arg_type( sem, 0, wnframes, cls(id(wn), data, id(wn), data, t)).
lex_arg_type( sem, 0, wnframes, cs(id(wn), id(wn))).
lex_arg_type( sem, 0, wnframes, der(id(wn), data, id(wn), data)).
lex_arg_type( sem, 0, wnframes, ent(id(wn), id(wn))).
lex_arg_type( sem, 0, wnframes, fr(id(wn), data, data)).
% lex_arg_type( sem, 0, wnframes, g(id(wn), data)).
lex_arg_type( sem, 0, wnframes, hyp(id(wn), data)).
lex_arg_type( sem, 0, wnframes, ins(id(wn), id(wn))).
lex_arg_type( sem, 0, wnframes, mm(id(wn), id(wn))).
lex_arg_type( sem, 0, wnframes, mp(id(wn), id(wn))).
lex_arg_type( sem, 0, wnframes, ms(id(wn), id(wn))).
lex_arg_type( sem, 0, wnframes, opposite(pos, text(base), text(base), data)).
lex_arg_type( sem, 0, wnframes, per(id(wn), data, id(wn), data)).
lex_arg_type( sem, 0, wnframes, ppl(id(wn), data, id(wn), data)).
lex_arg_type( sem, 0, wnframes, sa(id(wn), data, id(wn), data)).
lex_arg_type( sem, 0, wnframes, sim(id(wn), id(wn))).
lex_arg_type( sem, 0, wnframes, vgp(id(wn), data, id(wn), data)).

binds_with(C, _Val):- \+ atomic(C), !, fail.
binds_with(C, Val):- var(Val), !, put_attr(Val, binds_atomic, C).
binds_with(C, Val):- compound(Val), !, sub_term(V, Val), atomic(V), same_atoms(C, V), !.
binds_with(C, Val):- same_atoms(C, Val).

same_atoms(A1, A2):- A1==A2->true;(A2\==[], A1\==[], downcase_atom(A1, V1), downcase_atom(A2, V2), !, V1==V2).


% binds_with(C, Val):- compound(Val), !, arg(_, Val, V), V==C.
% binds_with(C, Val):- var(C), !, freeze(C, binds_with(C, Val)).

binds_atomic:attr_unify_hook(C, Val):- binds_with(C, Val).


%:- set_prolog_flag(debugger_write_options, [quoted(true), portray(true), max_depth(20), attributes(dots)]).
:- fixup_exports.
/* first_clause_only tests  */

fco_test(A):- !, first_clause_only(fco_test(A)).
fco_test(A):- member(A, [1, 2]).
fco_test(A):- member(A, [3, 4]).

/*
:- begin_tests(first_clause_only).

test(first_clause_only, all(X == [1, 2])) :-
        ( fco_test(X) ).

:- end_tests(first_clause_only).
*/

test_lex_info:- forall(lex_info(X),lex_info(X)).

baseKB:feature_test:-test_lex_info.

:- export(lex_info/1).
lex_info(String):- nonvar(String),!,
 lex_info(_AllKinds, String, Datum),
 lex_print(Datum).

lex_info('There are 5 houses with five different owners.').

lex_info(".\nThe Norwegian lives in the first house.\n.").
        
lex_info("Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").

lex_info('
 These five owners drink a certain type of beverage, smoke a certain brand of cigar and keep a certain pet.
 No owners have the same pet, smoke the same brand of cigar or drink the same beverage.
 The man who smokes Blends has a neighbor who drinks water.
 A red cat fastly jumped onto the table which is in the kitchen of the house.
 After Slitscan, Laney heard about another job from Rydell, the night security man at the Chateau.
 Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.
 Concrete beams overhead had been hand-painted to vaguely resemble blond oak.
 The chairs, like the rest of the furniture in the Chateau\'s lobby, were oversized to the extent that whoever sat in them seemed built to a smaller scale.
 Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.
 A book called, "A little tribute to Gibson".
 "You look like the cat that swallowed the canary, " he said, giving her a puzzled look.').



