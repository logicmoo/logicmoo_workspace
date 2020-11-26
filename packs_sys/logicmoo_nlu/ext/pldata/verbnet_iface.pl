% ===================================================================
% File 'logicmoo_module_aiml_loader.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

:- module(verbnet_iface, []).

:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo/xml_reader)).


%:- use_module(library(nomic_mu)).

syntaxify_slots([], SyntaxV, SyntaxV).
syntaxify_slots(SlotsV, SyntaxV, NewSyntaxV):- is_list(SyntaxV),
  !, must_maplist(syntaxify_slots(SlotsV), SyntaxV, NewSyntaxV).
syntaxify_slots([V|SlotsV], SyntaxV, NewSyntaxV):- !,
  syntaxify_slots(V, SyntaxV, M),
  syntaxify_slots(SlotsV, M, NewSyntaxV).
syntaxify_slots(isa(X, Y), np(X), np(X, [+Y])).
syntaxify_slots(~(isa(X, Y)), np(X), np(X, [-Y])).
syntaxify_slots(isa(X, Y), np(X, L), np(X, [+Y|L])).
syntaxify_slots(~(isa(X, Y)), np(X, L), np(X, [-Y|L])).
syntaxify_slots(or(isa(X, Y), Z), np(X), np(X, [or(isa(X, Y), Z)])).
syntaxify_slots(or(isa(X, Y), Z), np(X, L), np(X, [or(isa(X, Y), Z)|L])).
syntaxify_slots(_, X, X).

into_lf(X, X).

varify_frames:-
 ((Cl =
   (verbnet_frame(N1, N2, L, A1, A2, B, Frame):-
    nop(english(Example)),
    %continue_parsing(A , NewSyntaxV),
    into_lf(LFV, B)))),

  forall(call(vndata:verbnet_frame_vars(Frame, Vars, Slots)),
  must_det_l((
   vndata:verbnet_example(Frame, Example),
   vndata:verbnet_syntax(Frame, Syntax),
   vndata:verbnet_semantics(Frame, Logic),
   subst_vars(Vars, Slots, SlotsV),
   subst_vars(Vars, Syntax, SyntaxV),
   subst_vars(Vars, Logic, LogicV),
   syntaxify_slots(SlotsV, SyntaxV, OUT),
   [N1, N2|Rest]=OUT,
   length(Rest, L),
   (L = 0 -> (A1 = [], A2= []) ;
    L = 1 -> ([A1] = Rest , A2= []) ;
    L = 2 -> ([A1, A2] = Rest) ;
   (((append(A1, [PP|More], Rest), \+ functor(PP, np, _)) -> A2 = [PP|More] ;
   [A1|A2] = Rest))),
   listify(LogicV, LogicVL),
   append(LogicVL, SlotsV, LF),
   once(list_to_conjuncts(', ', LF, LFV)),
   assert_verbnet(Cl)))),
  nop(dmsg(call(listing(vndata:verbnet_frame)))).



trim_varname(V, VV):- atom_concat('?', M, V), !, trim_varname(M, VV).
trim_varname(V, VV):- first_char_to_upper(V, V1), atom_subst(V1, '-', '_', VV).

subst_vars([V|Vars], B, A):- trim_varname(V, VV), !,
     atom_concat('?', VV, VN1),
  atom_subst(VV, "_", "-", VN2),
 atom_subst(VN1, "_", "-", VN3),
       downcase_atom(VV, VN4),
  var_subst4(  V, (VV), B, M0),
  var_subst4(VN1, (VV), M0, M1),
  var_subst4(VN2, (VV), M1, M2),
  var_subst4(VN3, (VV), M2, M3),
  var_subst4(VN4, (VV), M3, M4),
  var_subst4(VV, '$VAR'(VV), M4, M),
  subst_vars(Vars, M, A).
subst_vars(_, B, A):- var_subst4('VerbSpecific', '$VAR'('Pred'), B, A).


assert_verbnet(O):- sub_term(S, O), is_list(S), member(C, S), compound(C), !, vndata_assert(O), is_reloading((nl, portray_clause(:-O), !, nl)).
assert_verbnet(O):- O = (_:-_), !, vndata_assert(O), is_reloading((nl, portray_clause(O), !, nl)).
assert_verbnet(O):- assert_verbnet0(O).
assert_verbnet0(O):- vndata_assert(O), is_reloading(format('~p.~n', [O])).
assert_verbnet1(O):- vndata_assert(O), is_reloading(format('~p.~n', [O])).

vndata_assert(O):- expand_to_hb(O, H, B), vndata_assert(H, B).
vndata_assert(H, B):- vndata:clause(H, B, Ref), vndata:clause(HH, BB, Ref), ((H+B)=@=(HH+BB)), !.
vndata_assert(H, B):- vndata:assert((H:-B)).

var_subst4(Find, Replace, X, Res) :- X == Find, !, Res = Replace.
var_subst4(_, _, X, Res) :- \+ compound(X), X = Res.
var_subst4(_, _, '$VAR'(X), '$VAR'(X)) :-!.
var_subst4(Find, Replace, X, Res) :-
       compound_name_arguments(X, F, L),
       must_maplist(var_subst4(Find, Replace), L, RL),
       compound_name_arguments(Res, F, RL).

is_reloading(G):- nb_current(verbnet_iface_loaded, true)->G;true.

kill_old_preds:-
 forall(
  member(F/A, [
   verbnet_frame_prop/3,
   verbnet_initial_vars/3,
   verbnet_example/2,
   verbnet_word/3,
  % verbnet_map_wn/4,
   verbnet_class/4,
   verbnet_external_id/4,
   verbnet_frame_vars/3,
   verbnet_to_framenet/3,
   verbnet_semantics/2,
   verbnet_syntax/2,
   verbnet_frame/7,
   verbnet_pred/2, verbnet_type/1, verbnet_type/2]),
   (must(abolish(vndata:F, A)), must(vndata:dynamic(F/A)), functor(P, F, A), must(retractall(vndata:P)), dmsg(call(must(listing(vndata:F)))))).

:- is_reloading(kill_old_preds).

ppfs:- cls, make, kill_old_preds, ppfs0, !.
ppfs0 :- ppfs(pldata('verbnet-3.2/*.xml')),
  %listing(vndata:verbnet_type), nb_setval(verbnet_iface_loaded, true),
  varify_frames,
  nop(dmsg(call(listing(vndata:verbnet_pred)))).

ppfs1 :- cls, make, kill_old_preds,
          ppfs('verbnet-3.2/weather-57.xml'),
          ppfs('verbnet-3.2/drive*.xml'),
          ppfs('verbnet-3.2/cooking*.xml'),
          ppfs('verbnet-3.2/fulfilling*.xml'),
          ppfs('verbnet-3.2/accept-*.xml'),

         % listing(verbnet_pred), listing(verbnet_type),
          varify_frames.


% ppfs('../aiml/chomskyAIML/*.aiml').
% ppfs:-ppfs('../aiml/chomskyAIML/chomsky001.aiml').
ppfs(FN):-
   absolute_file_name(FN, FN0),
   expand_file_name(FN0, Exp), member(F, Exp), ppfs2(F), fail.
ppfs(_):-!.

:- nb_setval(themerole_vars, []), nb_setval(themerole_props, []).

set_themerole_vars(V, Props):- nb_setval(themerole_vars, []), nb_setval(themerole_props, Props), extend_themerole_vars(V).
get_themerole_vars(V, Props):- nb_current(themerole_vars, V), nb_getval(themerole_props, Props).

extend_themerole_vars(X):- must_maplist(into_var, X, XX), must_maplist(add_var_vn, XX).

print_themeroles(ID, P):-
   must(get_themerole_vars(X, Y)),
   O=..[P, ID, X, Y],
   assert_verbnet(O), !.

rewrite_sem(STUFF, STUFF):- var(STUFF), !.
rewrite_sem(X, V):- into_var(X, V), !.
rewrite_sem(STUFF, STUFFY):- is_list(STUFF), !, must_maplist(rewrite_sem, STUFF, STUFFY).
rewrite_sem(STUFF, STUFF):- \+ compound(STUFF), !.
rewrite_sem(STUFF, STUFFY):- STUFF=..List, must_maplist(rewrite_sem, List, ListO), STUFFY=..ListO.

into_var('ThemRole':V, V):-atom(V), !, add_var_vn(V).
into_var(verb(X, Y), verb(X, Y)):-!.
into_var('VerbSpecific', verb(ID, 'VerbSpecific')):- nb_current(verbnet_id, ID), nb_current('$frame_VERB', true).
%into_var('VerbSpecific', verb(ID, 'VerbSpecific')):- nb_current(verbnet_id, ID), nb_current('$frame_VERB', true).
% into_var('VerbSpecific', 'VerbSpecific'):-
into_var(V:List, V):- is_list(List), !, must_maplist(add_var_prop(V), List).
into_var(V:or(List), V):- is_list(List), !, add_var_prop(V, or(List)).
%into_var([[+Spatial]], prep(Spatial, 'Prep')):- add_var_prop('Prep', Spatial).
%into_var([+Spatial], prep(Spatial, 'Prep')):- add_var_prop('Prep', Spatial).
into_var('Event':DE, E):- de_to_d_e(DE, D, P, E), !, add_var_prop(D, +'actEvent'), add_var_prop(E, +'actEvent'), add_var_prop(D, P), add_var_prop(E, P).
into_var(T:V-P, V):-!, add_var_prop(V, P), add_var_prop(V, +T).
into_var(T:V, V):-!, add_var_prop(V, +T).
into_var(V-P, V):-!, add_var_prop(V, P).
into_var(X, X):-atom(X).

de_to_d_e(DE, D, P, E):- atom_to_term(DE, T, Vars), compound(T), must(de_to_d_e(DE, T, Vars, D, E, P)).
de_to_d_e(_DE, FA, [EVar=A], EVar, EVar, P):-FA=..[during, A], !, P=true.
de_to_d_e(_DE, FA, [EVar=A], EVar, E, P):-FA=..[F, A], !,
  toPropercase(F, FP),
  atom_concat(FP, EVar, E), add_var_vn(EVar), add_var_vn(E), P=..[F, EVar, E].

add_var_vn('$FVAR'(X)):- !, add_var_vn(X).
add_var_vn('$VAR'(X)):- !, add_var_vn(X).
add_var_vn(X):- trim_varname(X, XX), extend_val(themerole_vars, XX).


make_conjs(X, XX, OrProps):- is_list(XX), !,
 must_maplist(make_conjs(X), XX, Props), list_to_conjuncts(', ', Props, OrProps), !.
make_conjs(X, or(XX), OrProps):- is_list(XX), !,
 must_maplist(make_conjs(X), XX, Props), list_to_conjuncts(or, Props, OrProps), !.
make_conjs(X, XX, isa(X, XX)):- var(XX), !.
make_conjs(X, +XX, isa(X, XXX)):- correct_type(XX, XXX).
make_conjs(X, -XX, ~(isa(X, XXX))):- correct_type(XX, XXX).
make_conjs(X, XX, O):- XX=='VerbSpecific', !, make_conjs(X, '$VERB', O).
make_conjs(X, XX, isa(X, XXX)):- atom(XX), atom_contains(XX, "?"), trim_varname(XX, XXX).
make_conjs(X, XX, isa(X, XXX)):- atom(XX), correct_type(XX, XXX), !.
make_conjs(_, XXX, XXX).

correct_type(XX, XX):- \+ atom(XX), !.
correct_type('Event', 'actEvent').
correct_type(body_part, 'tBodyPart').
correct_type(XX, XXX):- atom_contains(XX, "$"), !, XXX=XX.
correct_type(XX, XXX):- upcase_atom(XX, U), XX==U, toPropercase(XX, Y), atom_concat('t', Y, XXX), !.
correct_type(XX, XXX):- downcase_atom(XX, DC), XX\==DC, !, XXX=XX.
correct_type(XX, XXX):- atom_contains(XX, "_"), !, XXX=XX.
correct_type(XX, XXX):- first_char_to_upper(XX, XX0), atom_concat('t', XX0, XXX), !.
correct_type(XX, XX):-!.



add_var_prop(X, XX):- trim_varname(X, V), add_var_vn(V), add_var_prop_0(V, XX).
add_var_prop_0(X, XX):- is_list(XX), !, must_maplist(add_var_prop_0(X), XX).
add_var_prop_0(_, XX):- XX==true, !.
add_var_prop_0(X, XX):- make_conjs(X, XX, OrProps), extend_val(themerole_props, OrProps).



extend_val(V, XX):- is_list(XX), !, maplist(extend_val(V), XX).
extend_val(V, XX):- nb_getval(V, Props), conjoin_props(Props, XX, NewProps), nb_setval(V, NewProps).
conjoin_props(X, Y, XY):- member(Y, X) -> XY=X ; append(X, [Y], XY).

ppfs2(File):-
    fileToLineInfoElements(_Ctx, File, Z),
    set_themerole_vars([], []),
    mustToEach(no_parent, [], Z).


split_atom(A, B, C, E):- split_string(A, B, C, E0), must_maplist(any_to_atom, E0, E), !.

split_g(Atom, G):- splt_g0(Atom, GG), exclude(=(''), GG, G), !.

string_trim(V, S):- must((split_string(V, "", " \n\t\r", VV), exclude(=(''), VV, G), !, G=[S])).

splt_g0(Atom, G):- atom_contains(Atom, ', '), split_atom(Atom, ", ", " ", G).
splt_g0(Atom, G):- split_atom(Atom, " ", " ", G).
splt_g0('', []).

split_g_or_same(Sep, Atom, G):- atom_contains(Atom, Sep), split_atom(Atom, Sep, "", GG), maybe_unlist(GG, G), !.
split_g_or_same(_, A, A).

maybe_unlist([GG], G):- !, maybe_unlist(GG, G).
maybe_unlist(G, G).

pairs_to_values(SO, Vs):- must_maplist(arg2of, SO, Vs).
arg2of(_=V, V).
arg2of(V, V).

crrect_value_arg(descriptionNumber, V, V):-!.
crrect_value_arg(example, V, VV):- !, atom_string(V, VV).
crrect_value_arg(E, [V], VV):-!, crrect_value_arg(E, V, VV).
crrect_value_arg(_, V, VV):- compound(V), !, VV=V.
% crrect_value_arg(_, V, VV):- \+ atom(V), !, VV=V.
crrect_value_arg(_, V, V):- atom_number(V, _), !.
crrect_value_arg(_, V, VV):- split_g(V, VV).



%get_ext_index([ExtID0|ExtIDMore], Index):-split_string(ExtID, ".", ".", ExtIDDots)





append_terms([H|List], O):- append_terms(H, List, O).

append_terms(P, L, O):- L==[], P = O, !.
append_terms(P, L, O):- P==[], L = O, !.
append_terms([F|P], L, O):- atom(F), is_list(P), !, PP=..[F|P], append_terms(PP, L, O), !.
append_terms(P, L, O):- is_list(P), PP=..[lst|P], append_terms(PP, L, O), !.
append_terms(P, L, O):- is_list(L)->append_termlist(P, L, O);append_term(P, L, O), !.

mustToEach(ID, Ctx, Z):- must(writeToEach(ID, Ctx, Z)), !.

writeToEach(_ID, _Ctx, []):-!.
writeToEach(_ID, _Ctx, ''):-!.

writeToEach(_ID, t(Before, After), X):-!, append_terms(Before, X, M), append_terms(M, After, O), !, assert_verbnet0(O).
writeToEach(ID, Ctx, List):- is_list(List), !, must_maplist(mustToEach(ID, Ctx), List), !.
writeToEach(ID, _Ctx, N==V):- crrect_value_arg(N, V, VV), !, atom_concat('verbnet_', N, Pred), P=..[Pred, ID, VV], !, assert_verbnet(P).
writeToEach(ID, _Ctx, N=V):- crrect_value_arg(N, V, VV), !, P=..[verbnet_frame_prop, ID, N, VV], !, assert_verbnet(P).

%mustToEach(_ID, _Ctx, element('MEMBER', _S, [])):-!.
writeToEach( ID, _Ctx, element('MEMBER', S, [])):-
  select(wn=WN, S, _), split_g(WN, WnL), !,
  select(name=Name, S, _), atom_string(Name, WStr), assert_verbnet0(verbnet_word(Name, ID, WStr)),
   ignore((\+ \+ ((select(grouping=Gs, S, _), split_g(Gs, GsL), must_maplist(mustToEach(ID, t(verbnet_to_framenet(ID, Name), [])), GsL))))),
   assert_verbnet0(verbnet_map_wn(Name, WnL, ID)),
   !.


writeToEach(ParentID, Ctx, element(CLASS, S, STUFF)):- % member(CLASS, ['VNCLASS', 'VNSUBCLASS']), !,
   member('ID'=ID, S), !,
   nb_setval(verbnet_id, ID),
   flag(frame_num, _, 0),
   must(get_themerole_vars(Was, WasP)),
   is_reloading(format('~N~n~n% =========================================================~n', [])),
   assert_verbnet0(verbnet_class(ID, CLASS, ParentID, Ctx)), !,
   get_external_id(ID, ExtID, Index),
   assert_verbnet1(verbnet_external_id(ID, CLASS, ExtID, Index)), !,
   is_reloading(format('~N% =========================================================~n', [])),
   must((select(element('MEMBERS', [], WORDS), STUFF, STUFF1), mustToEach(ID, [ID|Ctx], WORDS),
   is_reloading(nl),
   must((select(element('THEMROLES', [], SS), STUFF1, STUFF2),
                       simplify_seme('THEMROLES', SS, SSS),
                       must(extend_themerole_vars(SSS)),
                       is_reloading(format('~N% =========================================================~n', [])),
                       print_themeroles(ID, verbnet_initial_vars),
                       nop(mustToEach(ID, [themrole, ID|Ctx], SSS)))),
   is_reloading(nl),
   select(element('FRAMES', [], FRAMES), STUFF2, STUFF3),
      mustToEach(ID, [ID|Ctx], element('FRAMES', [], FRAMES)),
   select(element('SUBCLASSES', [], SUBCLASSES), STUFF3, STUFF4), mustToEach(ID, [ID|Ctx], SUBCLASSES),
   STUFF4==[])), !,
   must(set_themerole_vars(Was, WasP)).


writeToEach(ID, Ctx, element('FRAMES', [], LIST)):- flag(frame_num, _, 0), !, mustToEach(ID, Ctx, LIST).

writeToEach(ID, Ctx, element('FRAME', Info, STUFF)):- !,
  flag(frame_num, N, N+1),
  must(get_themerole_vars(Was, WasP)),
  (N==0->atomic_list_concat([ID, '_f', N], NewID);atomic_list_concat([ID, '_f', N], NewID)),
  must_maplist(mustToEach(NewID, Ctx), Info),
  % goveler
  ignore((member(element('SEMANTICS', [], SS), STUFF), simplify_seme('SEMANTICS', SS, SSS), rewrite_sem(SSS, _Sem))),
  mustToEach(NewID, Ctx, STUFF), %e(STUFF)
  print_themeroles(NewID, verbnet_frame_vars),
  must(set_themerole_vars(Was, WasP)).


writeToEach(ID, Ctx, element('DESCRIPTION', S, [])):- !, must_maplist(mustToEach(ID, Ctx), S).
writeToEach(ID, Ctx, element('EXAMPLE', [], [S])):-!, mustToEach(ID, Ctx, example==S), !.
writeToEach(ID, Ctx, element('SYNTAX', [], SS)):- simplify_syns('SYNTAX', SS, SSS),
   mustToEach(ID, Ctx, pre_syntax=SSS),
   rewrite_sem(SSS, SYNTAX),
   mustToEach(ID, Ctx, syntax==SYNTAX).
writeToEach(ID, Ctx, element('SEMANTICS', [], SS)):- simplify_seme('SEMANTICS', SS, SSS),
   %mustToEach(ID, Ctx, pre_semantics=SSS),
   rewrite_sem(SSS, Sem),
   mustToEach(ID, Ctx, semantics==Sem), !.

%mustToEach(ID, Ctx, element(Tag, [], STUFF)):- member(Tag, ['FRAMES', 'MEMBERS', 'EXAMPLES']), !, mustToEach(ID, Ctx, STUFF).
writeToEach(ID, Ctx, element(Tag, [], STUFF)):- !, mustToEach(ID, [Tag|Ctx], STUFF).

writeToEach(_, Ctx, Atom):-append_terms(Ctx, Atom, O), !, assert_verbnet(O).
writeToEach(_, Ctx, S):-length(Ctx, L), tab(L), append_terms(S, Ctx, O), assert_verbnet(O), !.


% :- ppfs.
% sub_compound(S, C):- sub_term(E, C), nonvar(S), S=E.


simplify_seme(Kind, S, SSS):- simplify_sem(S, SS), (S==SS -> SSS=SS ; simplify_seme(Kind, SS, SSS)), !.


simplify_sem(STUFF, STUFF):- assertion(nonvar(STUFF)), \+ compound(STUFF), !.

simplify_sem(STUFF, STUFFY):- is_list(STUFF), !, must_maplist(simplify_sem, STUFF, STUFFY).
%simplify_sem([[]], []).
simplify_sem(element('ARGS', [], LIST), args(LISTO)):- must_maplist(simplify_sem, LIST, LISTO).

simplify_sem(element('ARG', [type='VerbSpecific', value=V], []), 'VerbSpecific':V):- nb_setval('$frame_VERB', true).
simplify_sem(element('ARG', [type=T, value=V], []), T:V):-!.
simplify_sem(element('ARG', [type=T, value=V], MORE), '$$$$$$$$$$$$$$  XXXXXXXXXXXXXXXX $$$$$$$$$$$$$'(T:V, MORE)).
%simplify_sem(element('PRED', [value=V], [['Event':'during(E)', 'ThemRole':'Agent', 'ThemRole':'Theme']]


simplify_sem(element('PRED', [bool=(!)|More], ARGS), ~(P)):- !, simplify_sem(element('PRED', More, ARGS), P).

simplify_sem(element('PRED', [value='Pred'], [args(ARGS)]), P):-
  Cause = 'Pred', add_var_vn(Cause), add_var_prop(Cause, '$VERB'),
  nb_setval('$frame_Pred', Cause),
  P=..[holds, Cause|ARGS], !,
  length(ARGS, A), assert_if_new(vndata:verbnet_pred(Cause, A)).


simplify_sem(element('PRED', [value='Adv'], [args(ARGS)]), P):-
  Cause = 'Adv', add_var_vn(Cause), add_var_prop(Cause, '$ADV'),
  nb_setval('$frame_Pred', Cause),
  P=..[holds, Cause|ARGS], !,
  length(ARGS, A), assert_if_new(vndata:verbnet_pred(Cause, A)).

simplify_sem(element('PRED', [value='Prep'], [args(ARGS)]), P):-
  Cause = 'Prep', add_var_vn(Cause), add_var_prop(Cause, '$PREP'),
  nb_setval('$frame_Pred', Cause),
  P=..[holds, Cause|ARGS], !,
  length(ARGS, A), assert_if_new(vndata:verbnet_pred(Cause, A)).


simplify_sem(element('PRED', [value='Adj'], [args(ARGS)]), P):-
  Cause = 'Adj', add_var_vn(Cause), add_var_prop(Cause, '$ADJ'),
  nb_setval('$frame_Pred', Cause),
  P=..[holds, Cause|ARGS], !,
  length(ARGS, A), assert_if_new(vndata:verbnet_pred(Cause, A)).


simplify_sem(element('PRED', [value=Cause], [args(ARGS)]), P):-
  P=..[Cause|ARGS], !,
  length(ARGS, A), assert_if_new(vndata:verbnet_pred(Cause, A)).


%simplify_sem(element('PRED', [value=V], [['Event':'during(E)', 'ThemRole':'Agent', 'ThemRole':'Theme']]
simplify_sem(element('THEMROLE', [type=Result], []), (Result:[])):-!.
simplify_sem(element('THEMROLE', [type=Result], [OR]), (Result:OR)).
simplify_sem(element('THEMROLE', [type=Result], OR), (Result:OR)).
simplify_sem(element(_, [], LIST), LISTO):- must_maplist(simplify_sem, LIST, LISTO).
simplify_sem(element(_, [logic=or], LIST), or(LISTO)):- must_maplist(simplify_sem, LIST, LISTO).
simplify_sem(element(RESTR, ['Value'=(+), type=V], []), +V):- maybe_type(V, RESTR).
simplify_sem(element(RESTR, ['Value'=(-), type=V], []), -V):- maybe_type(V, RESTR).
simplify_sem(element(RESTR, [type=V], []), V):- maybe_type(V, RESTR).
simplify_sem(STUFF, STUFFY):- STUFF=..List, must_maplist(simplify_sem, List, ListO), STUFFY=..ListO.


maybe_type(V, _):- atom_contains(V, ?), !.
maybe_type(V, _):- atom_contains(V, "("), !.
maybe_type(V, T):- T\=='SYNRESTR', correct_type(V, VV), assert_if_new(vndata:verbnet_type(VV, T)).
maybe_type(V, T):- assert_if_new(vndata:verbnet_type(V, T)).

simplify_syns(Kind, S, SSS):- simplify_syn(S, SS), (S==SS -> SSS=SS ; simplify_syns(Kind, SS, SSS)), !.

% simplify_syn(O, O):-!.

% simplify_syn(STUFF, STUFF):- assertion(nonvar(STUFF)), fail.
simplify_syn(STUFF, STUFF):- (var(STUFF);STUFF==[]), !.

% simplify_syn([np(Theme, [+to_be])], [txt(to), txt(be), np(Theme)]):-!.
% simplify_syn([np(Theme-[+what_inf])|More], [prep(what), np(Theme-[+inf])|MoreO]):- simplify_syn(More, MoreO).
simplify_syn(np(Theme-[+Type]), np(Type, Theme)):-!.

simplify_syn([np(Theme-[])|More], [np(Theme)|MoreO]):- simplify_syn(More, MoreO).
simplify_syn([np(Theme-Types)|More], [np(Theme, Types)|MoreO]):- simplify_syn(More, MoreO).

% simplify_syn(STUFF, STUFFY):- is_list(STUFF), !, must_maplist(simplify_syn, STUFF, STUFFY).
simplify_syn([[LOC]|T], TT):- is_list(LOC), !, simplify_syn([prep('Prep', LOC)|T], TT), add_var_vn('Prep').
simplify_syn([H|T], [HH|TT]):- !, simplify_syn(H, HH), simplify_syn(T, TT).



simplify_syn(VERB, verb(vn(ID))):- VERB == 'VERB', nb_current(verbnet_id, ID).
% simplify_syn(VERB, verb( 'VerbSpecific')):- VERB == 'VERB', nb_current(verbnet_id, ID).
simplify_syn(restr(synrestrs, and, [restr(synrestr, V)]), restr(V)).
simplify_syn(element(NP, S, [element(_SELRESTRS, [], [])]), O):- !, simplify_syn(element(NP, S, []), O).
simplify_syn(element(NP, [value=Asset], [T]), NP:Asset-T).
simplify_syn(element(NP, [], [[T]]), NP:T).
simplify_syn(element(NP, [value=Asset], [[]]), NP:Asset).
simplify_syn(element(LEX, [value=OF], []), LEX:OF).
simplify_syn(element(V, [], []), V).
simplify_syn('NP':V, np(V)).
simplify_syn('PREP':V, prep(OR)):- to_word_or(V, OR), !.
simplify_syn('ADV':V, adv(OR)):- to_word_or(V, OR), !.
simplify_syn('ADJ':V, adj(OR)):- to_word_or(V, OR), !.
simplify_syn('LEX':V, lex(OR)):- to_word_or(V, OR), !.
simplify_syn(V, txt(OR)):- atomic(V), atom_contains(V, ' '), to_word_or(V, OR), !.

simplify_syn(STUFF, STUFF):- \+ compound(STUFF), !.
simplify_syn(element(A, B, C), Next):-!, simplify_sem(element(A, B, C), Next).
% simplify_syn(STUFF, STUFFY):-simplify_sem(STUFF, STUFFY), !.
simplify_syn(STUFF, STUFFY):- STUFF=..List, must_maplist(simplify_syn, List, ListO), STUFFY=..ListO.

to_word_or(V, OR):- compound(V), !, OR=V.
to_word_or(V, OR):- must((split_g(V, VV), must_maplist(split_g_or_same('_'), VV, VVV), list_to_conjuncts((;), VVV, OR))).




/* -----------------------------------------------------------
   verbnet_role(?Role, ?IsaRole, ?Comment)

   Based on VerbNet and LIRICS.
   See: Bonial et al. 2011, Sowa 2000.
   Purpose, Reason
----------------------------------------------------------- */


verbnet_role('Actor', 'Participant', 'participant that is an instigator').
verbnet_role('Undergoer', 'Participant', 'participant that is not an instigator').
verbnet_role('Time', 'Participant', 'participant that indicates an instance or interval of time').
verbnet_role('Place', 'Participant', 'participant that represents the place in which an entity exists'). % state->place
verbnet_role('Abstract', 'Participant', 'participant that represents a goal, property or manner').          % added

verbnet_role('Agent', 'Actor', 'intentional actor').
verbnet_role('Cause', 'Actor', 'unintentional actor (animate or inanimate)').

verbnet_role('Patient', 'Undergoer', 'undergoer that experiences a change of state, location or condition; exists independently of the event').
verbnet_role('Instrument', 'Undergoer', 'undergoer that is manipulated by an agent; exists independently of the event').
verbnet_role('Beneficiary', 'Undergoer', 'undergoer that is potentially advantaged or disadvantaged by the event').
verbnet_role('Theme', 'Undergoer', 'undergoer that is central to an event; not structurally changed by the event').

verbnet_role('Topic', 'Theme', 'theme with propositional information content'). % definition changed from LIRICS
verbnet_role('Pivot', 'Theme', 'theme that is more central than another theme in an event').

verbnet_role('Start', 'Time', 'time that indicates when an events begins or a state becomes true').
verbnet_role('Finish', 'Time', 'time that indicates when an events ends or a state becomes false').
verbnet_role('Duration', 'Time', 'length or extend of time').
verbnet_role('Frequency', 'Time', 'number of occurences of an event within a given time span').

verbnet_role('Location', 'Place', 'concrete place').
verbnet_role('Source', 'Place', 'concrete or abstract place that is starting point of action').
verbnet_role('Destination', 'Place', 'place that is an end point of an action').
verbnet_role('Path', 'Place', '').
verbnet_role('Value', 'Place', 'place along a formal scale').

verbnet_role('Goal', 'Abstract', 'purpose of an (intentional) action').
verbnet_role('Manner', 'Abstract', 'the particular way an event unfolds').
verbnet_role('Attribute', 'Abstract', 'property of an entity').

verbnet_role('Extent', 'Value', 'value indicating the amount of measurable change to a participant').
verbnet_role('Asset', 'Value', 'value that is a concrete object').

verbnet_role('Recipient', 'Destination', 'animate destination').
verbnet_role('Experiencer', 'Patient', 'patient that is aware in perception events').
verbnet_role('Result', 'Goal', 'goal that comes into existence through the event').
verbnet_role('Stimulus', 'Cause', 'cause in perception event that elicits emotional or psychological response').


/* -----------------------------------------------------------
   Conversion from old (VerbNet) to new names
----------------------------------------------------------- */

verbnet_old2new('Co-Agent', 'Agent'):- !.
verbnet_old2new('Co-Theme', 'Theme'):- !.
verbnet_old2new('Co-Patient', 'Patient'):- !.
verbnet_old2new('Initial_Time', 'Start'):- !.
verbnet_old2new('Final_Time', 'Finish'):- !.
verbnet_old2new('Initial_Location', 'Source'):- !.
verbnet_old2new('Final_Location', 'Destination'):- !.
verbnet_old2new('Result', 'Goal'):- !.
verbnet_old2new('Proposition', 'Topic'):- !.
verbnet_old2new('Trajectory', 'Path'):- !.
verbnet_old2new('Product', 'Result'):- !.
verbnet_old2new('Material', 'Source'):- !.
verbnet_old2new('Predicate', 'Attribute'):- !.
verbnet_old2new('Reflexive', 'Theme'):- !.
verbnet_old2new(R, R).

/* ----------------------------------------------------------------------
   Syntactic Restrictions
---------------------------------------------------------------------- */

restr(Restr, Type):-
  Restr = [element('SYNRESTRS', [], L)],
  member(element('SYNRESTR', ['Value'='+', type=Type], []), L), !.

ing(acc_ing).
ing(oc_ing).
ing(ac_ing).
ing(be_sc_ing).
ing(np_omit_ing).

inf(oc_to_inf).
inf(ac_to_inf).
inf(sc_to_inf).
inf(vc_to_inf).
inf(rs_to_inf).
inf(to_inf_rs).

bare(oc_bare_inf).

s_restr(np_to_inf).
s_restr(that_comp).
s_restr(for_comp).
s_restr(wh_comp).
s_restr(quotation).

s_restr(np_ppart).     % ???
s_restr(np_p_ing).     % ???
s_restr(np_ing).       % ???

s_restr(how_extract).
s_restr(what_extract).

s_restr(wh_inf).
s_restr(what_inf).
s_restr(wheth_inf).



/* ----------------------------------------------------------------------
   Format VerbNet ID
---------------------------------------------------------------------- */

get_external_id(ID, ExtID, Index):-
   atomic_list_concat([_, ExtID0|ExtIDMore], "-", ID),
   atomic_list_concat([ExtID0|ExtIDMore], "-", ExtID),
   get_ext_index(ExtID, Index).

get_ext_index(ExtID, Index):- atom_chars(ExtID, Chars) , formatID(Chars, Index).

formatID(Chars, [Pre, Sep1|L]):-
   Seps = ['-', '.'], member(Sep1, Seps),
   append(PreChars, [Sep1|RestChars], Chars),
   \+ ( member(Sep2, Seps), member(Sep2, PreChars) ), !,
   formatNumber(PreChars, Pre),
   formatID(RestChars, L).

formatID(Chars, [ID]):-
   formatNumber(Chars, ID).

formatNumber(Chars, Num):-
   Chars = [First|_],
   member(First, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']), !,
   number_chars(Num, Chars).

formatNumber(Chars, Atom):-
   atom_chars(Atom, Chars).


:- fixup_exports.

:- if( \+ (prolog_load_context(reloading, true))).
:- ppfs0.
:- endif.

end_of_file.



true.

mu:  ?- listing(verbnet_frame).
:- dynamic vndata:verbnet_frame/7.

vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('accept-77')),
                     1,
                     np(Theme, [+how_extract]),
                     [],
                     A,
                     'accept-77_f0') :-
    nop(english("I accept how you do it.")),
    into_lf((approve(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('accept-77')),
                     1,
                     np(Theme, [-sentential]),
                     [],
                     A,
                     'accept-77_f1') :-
    nop(english("I accepted it.")),
    into_lf((approve(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('accept-77')),
                     1,
                     np(Theme, [+poss_ing]),
                     [],
                     A,
                     'accept-77_f2') :-
    nop(english("I accepted their writing novels.")),
    into_lf((approve(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('accept-77')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'accept-77_f3') :-
    nop(english("I accepted that they wrote novels.")),
    into_lf((approve(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('accept-77')),
                     1,
                     np(Theme, [+what_extract]),
                     [],
                     A,
                     'accept-77_f4') :-
    nop(english("I accepted what they were doing.")),
    into_lf((approve(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('accompany-51.7')),
                     1,
                     np(Theme, [+tAnimate]),
                     [],
                     A,
                     'accompany-51.7_f0') :-
    nop(english("Jackie accompanied Rose.")),
    into_lf((motion(E, Agent), motion(E, Theme), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('accompany-51.7')),
                     3,
                     [np(Theme, [+tAnimate])],
                     [prep(Prep, [+spatial]), np(Destination)],
                     A,
                     'accompany-51.7_f1') :-
    nop(english("Jackie accompanied Rose to the store.")),
    into_lf((motion(E, Agent), motion(E, Theme), holds(Prep, E, Theme, Destination), holds(Prep, E, Agent, Destination), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('acquiesce-95')),
                     2,
                     prep(to),
                     np(Co_Agent,
                        [ or(isa(Co_Agent, tAnimate),
                             isa(Co_Agent, tOrganization))
                        ]),
                     A,
                     'acquiesce-95_f0') :-

    nop(english("The enemy soldiers submitted to us.")),
    into_lf((yield(E, Agent, Co_Agent), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)), A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('acquiesce-95')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'acquiesce-95_f1') :-
    nop(english("The king acquiesced to be in the same room with the paupers")),
    into_lf((yield(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('acquiesce-95')),
                     2,
                     prep(to),
                     np(Theme),
                     A,
                     'acquiesce-95_f2') :-
    nop(english("The enemy soldiers submitted to demands.")),
    into_lf((yield(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('addict-96')),
                     3,
                     [ np(Patient,
                          [ or(isa(Patient, tAnimate),
                               isa(Patient, tOrganization))
                          ])
                     ],
                     [prep(to), np(Stimulus)],
                     A,
                     'addict-96_f0') :-
    nop(english("I addicted him to Douglas Adams.")),
    into_lf((cause(Agent, E), desire(ResultE, Patient, Stimulus), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('addict-96')),
                     3,
                     [ np(Patient,
                          [ or(isa(Patient, tAnimate),
                               isa(Patient, tOrganization))
                          ])
                     ],
                     [prep(to), np(Stimulus, [+sc_ing])],
                     A,
                     'addict-96_f1') :-
    nop(english("Mary addicted him to going on long journeys.")),
    into_lf((cause(Agent, E), desire(ResultE, Patient, Stimulus), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('adjust-26.9')),
                     1,
                     np(Patient),
                     [],
                     A,
                     'adjust-26.9_f0') :-
    nop(english("He adapted himself.")),
    into_lf((cause(Agent, E), adjust(E, Patient, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('adjust-26.9')),
                     3,
                     [np(Patient)],
                     [prep(to), np(Goal, [-sentential])],
                     A,
                     'adjust-26.9_f1') :-
    nop(english("He adapted himself to the situation.")),
    into_lf((adjust(E, Patient, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('adjust-26.9')),
                     3,
                     [np(Patient)],
                     [prep(to), np(Goal, [+oc_ing])],
                     A,
                     'adjust-26.9_f2') :-
    nop(english("He adapted himself to waking up early.")),
    into_lf((adjust(E, Patient, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('adjust-26.9')),
                     2,
                     prep(to),
                     np(Goal, [+sc_ing]),
                     A,
                     'adjust-26.9_f3') :-
    nop(english("He adapted to waking up early.")),
    into_lf((adjust(E, Patient, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('adjust-26.9')),
                     2,
                     prep(to),
                     np(Goal),
                     A,
                     'adjust-26.9_f4') :-
    nop(english("He adapted to the situation.")),
    into_lf((adjust(E, Patient, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('admire-31.2')),
                     1,
                     np(Stimulus),
                     [],
                     A,
                     'admire-31.2_f0') :-
    nop(english("The tourists admired the paintings.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('admire-31.2')),
                     3,
                     [np(Stimulus)],
                     [prep(for), np(Attribute)],
                     A,
                     'admire-31.2_f1') :-
    nop(english("I admired him for his honesty.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, '$VAR'('Stimulus, Attribute')), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('admire-31.2')),
                     1,
                     np(Stimulus),
                     [],
                     A,
                     'admire-31.2_f2') :-
    nop(english("I admired the honesty in him.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('admire-31.2')),
                     1,
                     np(Stimulus, [+that_comp]),
                     [],
                     A,
                     'admire-31.2_f3') :-
    nop(english("The children liked that the clown had a red nose.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('admire-31.2')),
                     1,
                     np(Stimulus, [+be_sc_ing]),
                     [],
                     A,
                     'admire-31.2_f4') :-
    nop(english("I loved writing.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('admire-31.2')),
                     1,
                     np(Stimulus, [+poss_ing]),
                     [],
                     A,
                     'admire-31.2_f5') :-
    nop(english("I loved him writing novels.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('admire-31.2-1')),
                     1,
                     np(Stimulus, [+sc_to_inf]),
                     [],
                     A,
                     'admire-31.2-1_f0') :-
    nop(english("I loved to write.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('admit-65')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tOrganization))
                        ]),
                     [],
                     A,
                     'admit-65_f0') :-
    nop(english("She admitted us.")),
    into_lf((admit(E, Agent, Theme, Location), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('admit-65')),
                     2,
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tOrganization))
                        ]),
                     np(Location, [+adv_loc]),
                     A,
                     'admit-65_f1') :-
    nop(english("She admitted us here.")),
    into_lf((admit(E, Agent, Theme, Location), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('admit-65')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tOrganization))
                          ])
                     ],
                     [ prep(Prep, [+loc]),
                       np(Location, or([+Location, -region]))
                     ],
                     A,
                     'admit-65_f2') :-
    nop(english("She allowed us near the house.")),
    into_lf((admit(E, Agent, Theme, Location), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('adopt-93')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'adopt-93_f0') :-
    nop(english("Soon, the new President will assume office.")),
    into_lf((adopt(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('advise-37.9')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'advise-37.9_f0') :-
    nop(english("Ellen alerted Helen.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('advise-37.9')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [ prep((against;about;concerning;on;regarding;respecting)),
                       np(Topic, [+tCommunication])
                     ],
                     A,
                     'advise-37.9_f1') :-
    nop(english("Ellen warned Helen against skating on thin ice.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('advise-37.9')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication, +that_comp]),
                     A,
                     'advise-37.9_f2') :-
    nop(english("Ellen warned Helen that the party would be tonight.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('advise-37.9')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication, +wh_inf]),
                     A,
                     'advise-37.9_f3') :-
    nop(english("Ellen warned Helen how to avoid the crowd.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('advise-37.9')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication, +quotation]),
                     A,
                     'advise-37.9_f4') :-
    nop(english("Ellen warned Helen, 'Avoid that hole in the sidewalk.'")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('advise-37.9')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(of), np(Topic, [+tCommunication])],
                     A,
                     'advise-37.9_f5') :-
    nop(english("My accountant warned me of the new loopholes in the tax code.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('advise-37.9-1')),
                     2,
                     prep((against;about;concerning;on;regarding;respecting)),
                     np(Topic, [+tCommunication]),
                     A,
                     'advise-37.9-1_f0') :-
    nop(english("Ellen warned against skating on thin ice.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('advise-37.9-1')),
                     1,
                     np(Topic, [+tCommunication, +that_comp]),
                     [],
                     A,
                     'advise-37.9-1_f1') :-
    nop(english("Ellen warned that the party would be tonight.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('advise-37.9-1')),
                     1,
                     np(Topic, [+tCommunication, +wh_inf]),
                     [],
                     A,
                     'advise-37.9-1_f2') :-
    nop(english("Ellen warned how to avoid the crowd.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('advise-37.9-1')),
                     1,
                     np(Topic, [+tCommunication, +quotation]),
                     [],
                     A,
                     'advise-37.9-1_f3') :-
    nop(english("Ellen warned, 'Avoid that hole in the sidewalk.'")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('allow-64')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'allow-64_f0') :-
    nop(english("They allow birds.")),
    into_lf((allow(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('allow-64')),
                     1,
                     np(Theme, [+ac_ing]),
                     [],
                     A,
                     'allow-64_f1') :-
    nop(english("They allow smoking.")),
    into_lf((allow(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('allow-64')),
                     1,
                     np(Theme, [+np_tobe]),
                     [],
                     A,
                     'allow-64_f2') :-
    nop(english("They allow us to be smokers.")),
    into_lf((allow(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('allow-64')),
                     1,
                     np(Theme, [+poss_ing]),
                     [],
                     A,
                     'allow-64_f3') :-
    nop(english("They allow our smoking.")),
    into_lf((allow(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('allow-64')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'allow-64_f4') :-
    nop(english("They allow that we smoke.")),
    into_lf((allow(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('amalgamate-22.2')),
                     3,
                     [],
                     ['ADV', prep(with), np(Co_Patient)],
                     A,
                     'amalgamate-22.2_f0') :-
    nop(english("Folk songs alternate well with pop songs.")),
    into_lf((property('$VAR'('Patient+Co_Patient'), Prop), holds(Adv, Prop), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+plural]),
                     verb(vn('amalgamate-22.2')),
                     1,
                     'ADV',
                     [],
                     A,
                     'amalgamate-22.2_f1') :-
    nop(english("The pieces interconnect easily.")),
    into_lf((property('$VAR'('Patient_i+Patient_j'), Prop), holds(Adv, Prop), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('amalgamate-22.2-1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((into;to;with)), np(Co_Patient, [+tConcrete])],
                     A,
                     'amalgamate-22.2-1_f0') :-
    nop(english("Diabetics can now incorporate sugar into their desserts.")),
    into_lf((cause(Agent, E), degradation_material_integrity(ResultE, Patient), degradation_material_integrity(ResultE, Co_Patient), mingled(ResultE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('amalgamate-22.2-1')),
                     1,
                     np(Patient, [+tConcrete, +plural]),
                     [],
                     A,
                     'amalgamate-22.2-1_f1') :-
    nop(english("A new firm will integrate their ice cream production lines.")),
    into_lf((cause(Agent, E), degradation_material_integrity(ResultE, Patient_i), degradation_material_integrity(ResultE, Patient_j), mingled(ResultE, Physical, Patient_i, Patient_j), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('amalgamate-22.2-1')),
                     2,
                     prep(with),
                     np(Co_Patient, [+tConcrete]),
                     A,
                     'amalgamate-22.2-1_f2') :-
    nop(english("Ice cream integrates with desserts.")),
    into_lf((degradation_material_integrity(ResultE, Patient), degradation_material_integrity(ResultE, Co_Patient), mingled(ResultE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('amalgamate-22.2-1-1')),
                     0,
                     [],
                     [],
                     A,
                     'amalgamate-22.2-1-1_f0') :-
    nop(english("The yolks and the whites intermingled.")),
    into_lf((degradation_material_integrity(ResultE, Patient_i), degradation_material_integrity(ResultE, Patient_j), mingled(ResultE, Physical, Patient_i, Patient_j), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tAbstract)),
                          or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('amalgamate-22.2-2')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [ prep(with),
                       np(Co_Patient,
                          [ or(isa(Co_Patient, tAnimate),
                               isa(Co_Patient, tAbstract))
                          ])
                     ],
                     A,
                     'amalgamate-22.2-2_f0') :-
    nop(english("The merger associated company A with company B.")),
    into_lf((cause(Agent, E), together(EndE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), or(isa(Co_Patient, tAnimate), isa(Co_Patient, tAbstract)), or(isa(Agent, tAnimate), isa(Agent, tAbstract)), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tAbstract)),
                          or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('amalgamate-22.2-2')),
                     1,
                     np(Patient, [+tConcrete, +plural]),
                     [],
                     A,
                     'amalgamate-22.2-2_f1') :-
    nop(english("The merger associated the two companies.")),
    into_lf((cause(Agent, E), together(EndE, Physical, Patient_i, Patient_j), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), or(isa(Co_Patient, tAnimate), isa(Co_Patient, tAbstract)), or(isa(Agent, tAnimate), isa(Agent, tAbstract)), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('amalgamate-22.2-2')),
                     2,
                     prep(with),
                     np(Co_Patient,
                        [ or(isa(Co_Patient, tAnimate),
                             isa(Co_Patient, tAbstract))
                        ]),
                     A,
                     'amalgamate-22.2-2_f2') :-
    nop(english("Company A associated with Company B.")),
    into_lf((together(EndE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), or(isa(Co_Patient, tAnimate), isa(Co_Patient, tAbstract)), or(isa(Agent, tAnimate), isa(Agent, tAbstract)), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('amalgamate-22.2-2-1')),
                     0,
                     [],
                     [],
                     A,
                     'amalgamate-22.2-2-1_f0') :-
    nop(english("Plays and ballets alternate.")),
    into_lf((together(EndE, Physical, Patient_i, Patient_j), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), or(isa(Co_Patient, tAnimate), isa(Co_Patient, tAbstract)), or(isa(Agent, tAnimate), isa(Agent, tAbstract)), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('amalgamate-22.2-3')),
                     1,
                     np(Patient, [+plural]),
                     [],
                     A,
                     'amalgamate-22.2-3_f0') :-
    nop(english("John opposed the two ideas.")),
    into_lf((cause(Agent, E), together(EndE, '$VAR'('Abstract/physical'), Patient_i, Patient_j), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa('$VAR'('Abstract/physical'), 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('amalgamate-22.2-3-1')),
                     3,
                     [np(Patient)],
                     [prep(to), np(Co_Patient)],
                     A,
                     'amalgamate-22.2-3-1_f0') :-
    nop(english("John introduced Mary to me.")),
    into_lf((cause(Agent, E), together(EndE, '$VAR'('Abstract/physical'), Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa('$VAR'('Abstract/physical'), 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tAnimate, +plural]),
                     verb(vn('amalgamate-22.2-3-1-1')),
                     0,
                     [],
                     [],
                     A,
                     'amalgamate-22.2-3-1-1_f0') :-
    nop(english("John and Mary married.")),
    into_lf((together(EndE, '$VAR'('Abstract/physical'), Patient_i, Patient_j), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(Co_Patient, tAnimate), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa('$VAR'('Abstract/physical'), 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('amalgamate-22.2-3-2')),
                     0,
                     [],
                     [],
                     A,
                     'amalgamate-22.2-3-2_f0') :-
    nop(english("John and Mary are engaged.")),
    into_lf((together(EndE, Physical, Patient_i, Patient_j), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('amuse-31.1')),
                     1,
                     np(Experiencer, [+tAnimate]),
                     [],
                     A,
                     'amuse-31.1_f0') :-
    nop(english("The clown amused the children.")),
    into_lf((cause(Stimulus, E), emotional_state(ResultE, Emotion, Experiencer), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('amuse-31.1')),
                     1,
                     'ADV',
                     [],
                     A,
                     'amuse-31.1_f1') :-
    nop(english("Little children amuse easily.")),
    into_lf((property(Experiencer, Prop), holds(Adv, Prop), isa(Experiencer, tAnimate), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('amuse-31.1')),
                     0,
                     [],
                     [],
                     A,
                     'amuse-31.1_f2') :-
    nop(english("The clown amused.")),
    into_lf((cause(Stimulus, E), emotional_state(ResultE, Emotion, Experiencer), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Stimulus, [+genitive]),
                     lex('\'s'),
                     2,
                     verb(vn('amuse-31.1')),
                     np(Experiencer, [+tAnimate]),
                     A,
                     'amuse-31.1_f3') :-
    nop(english("The clown's antics amused the children.")),
    into_lf((cause(Stimulus, E), emotional_state(E, Emotion, Experiencer), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('amuse-31.1')),
                     2,
                     np(Experiencer, [+tAnimate]),
                     np(Result),
                     A,
                     'amuse-31.1_f4') :-
    nop(english("That movie bored me silly.")),
    into_lf((cause(Stimulus, E), emotional_state(ResultE, Emotion, Experiencer), holds(Pred, ResultE, Experiencer), isa(Experiencer, tAnimate), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimal]),
                     verb(vn('animal_sounds-38')),
                     0,
                     [],
                     [],
                     A,
                     'animal_sounds-38_f0') :-
    nop(english("The dog barked.")),
    into_lf((emit(E, Agent, Sound), isa(Agent, tAnimal), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimal]),
                     verb(vn('animal_sounds-38')),
                     2,
                     prep(Prep, [+dest_dir]),
                     np(Recipient),
                     A,
                     'animal_sounds-38_f1') :-
    nop(english("The dog barked at the cat.")),
    into_lf((emit(E, Agent, Sound), holds(Prep, E, Sound, Recipient), isa(Agent, tAnimal), isa(Prep, '$PREP'), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimal]),
                     verb(vn('animal_sounds-38')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'animal_sounds-38_f2') :-
    nop(english("The dog barked a warning.")),
    into_lf((emit(E, Agent, Theme), isa(Agent, tAnimal), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimal]),
                     verb(vn('animal_sounds-38')),
                     1,
                     np(Location),
                     [],
                     A,
                     'animal_sounds-38_f3') :-
    nop(english("Birds sang in the trees.")),
    into_lf((emit(E, Agent, Sound), location(E, Agent, Location), isa(Agent, tAnimal), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(np(Location),
                     verb(vn('animal_sounds-38')),
                     2,
                     prep(with),
                     np(Agent, [+tAnimal]),
                     A,
                     'animal_sounds-38_f4') :-
    nop(english("The tree sang with birds.")),
    into_lf((emit(E, Agent, Sound), location(E, Sound, Location), isa(Agent, tAnimal), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('appeal-31.4')),
                     2,
                     prep(to),
                     np(Experiencer, [+tAnimate]),
                     A,
                     'appeal-31.4_f0') :-
    nop(english("This painting appeals to Malinda.")),
    into_lf((emotional_state(E, Emotion, Experiencer), cause(Stimulus, E), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('appeal-31.4-1')),
                     2,
                     prep(at),
                     np(Experiencer, [+tAnimate]),
                     A,
                     'appeal-31.4-1_f0') :-
    nop(english("The irritation niggled at Melinda.")),
    into_lf((emotional_state(E, Emotion, Experiencer), cause(Stimulus, E), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('appeal-31.4-2')),
                     2,
                     prep(on),
                     np(Experiencer, [+tAnimate]),
                     A,
                     'appeal-31.4-2_f0') :-
    nop(english("The music grates on me.")),
    into_lf((emotional_state(E, Emotion, Experiencer), cause(Stimulus, E), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('appeal-31.4-3')),
                     0,
                     [],
                     [],
                     A,
                     'appeal-31.4-3_f0') :-
    nop(english("Stuff matters.")),
    into_lf((emotional_state(E, Emotion, Experiencer), cause(Stimulus, E), isa(Experiencer, tAnimate), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('appear-48.1.1')),
                     0,
                     [],
                     [],
                     A,
                     'appear-48.1.1_f0') :-
    nop(english("A ship appeared.")),
    into_lf((appear(E, Theme), isa(E, actEvent)), A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('appear-48.1.1')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location),
                     A,
                     'appear-48.1.1_f1') :-
    nop(english("A ship appeared on the horizon.")),
    into_lf((appear(E, Theme), ~holds(Prep, StartE, Theme, Location), holds(Prep, EndE, Theme, Location), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('appear-48.1.1')),
                     3,
                     [np(Theme)],
                     [prep(Prep, [+loc]), np(Location)],
                     A,
                     'appear-48.1.1_f2') :-
    nop(english("There appeared a ship on the horizon.")),
    into_lf((appear(E, Theme), ~holds(Prep, StartE, Theme, Location), holds(Prep, EndE, Theme, Location), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(prep(Prep, [+loc]),
                     np(Location),
                     2,
                     verb(vn('appear-48.1.1')),
                     np(Theme),
                     A,
                     'appear-48.1.1_f3') :-
    nop(english("On the horizon appeared a large ship.")),
    into_lf((appear(E, Theme), ~holds(Prep, StartE, Theme, Location), holds(Prep, EndE, Theme, Location), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('appear-48.1.1')),
                     1,
                     np(Location, [+adv_loc]),
                     [],
                     A,
                     'appear-48.1.1_f4') :-
    nop(english("It appeared there.")),
    into_lf((appear(E, Theme), ~location(StartE, Theme, Location), location(EndE, Theme, Location), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('appoint-29.1')),
                     2,
                     np(Theme),
                     np(Result),
                     A,
                     'appoint-29.1_f0') :-
    nop(english("We elected him governor.")),
    into_lf((designated(ResultE, Theme, Result), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('appoint-29.1')),
                     3,
                     [np(Theme)],
                     [lex(as), np(Result, [-sentential])],
                     A,
                     'appoint-29.1_f1') :-
    nop(english("We elected him as governor.")),
    into_lf((designated(ResultE, Theme, Result), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('appoint-29.1')),
                     4,
                     [np(Theme)],
                     [lex(to), lex(be), np(Result, [-sentential])],
                     A,
                     'appoint-29.1_f2') :-
    nop(english("We elected Alan to be our new governor.")),
    into_lf((designated(ResultE, Theme, Result), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('appoint-29.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'appoint-29.1_f3') :-
    nop(english("They elected John.")),
    into_lf((designated(ResultE, Theme, Result), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('assessment-34.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'assessment-34.1_f0') :-
    nop(english("The inspector analyzed the building.")),
    into_lf((assess(E, Agent, Theme, Attribute), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('assessment-34.1')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Attribute)],
                     A,
                     'assessment-34.1_f1') :-
    nop(english("The inspector analyzed the building for its soundness.")),
    into_lf((assess(E, Agent, Theme, Attribute), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('assessment-34.1')),
                     3,
                     [np(Theme, [+genitive])],
                     [lex('\'s'), np(Attribute)],
                     A,
                     'assessment-34.1_f2') :-
    nop(english("The inspector analyzed the building's soundness.")),
    into_lf((assess(E, Agent, Theme, Attribute), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('assuming_position-50')),
                     0,
                     [],
                     [],
                     A,
                     'assuming_position-50_f0') :-
    nop(english("The dog flopped.")),
    into_lf((~position(StartE, Agent, Pos), motion(E, Agent), position(EndE, Agent, Pos), isa(Agent, tAnimate), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Pos, Pred), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('assuming_position-50')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     A,
                     'assuming_position-50_f1') :-
    nop(english("The dog flopped in the corner.")),
    into_lf((~position(StartE, Agent, Pos), motion(E, Agent), position(EndE, Agent, Pos), holds(Prep, E, Agent, Location), isa(Agent, tAnimate), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Pos, Pred), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('avoid-52')),
                     1,
                     np(Location),
                     [],
                     A,
                     'avoid-52_f0') :-
    nop(english("We avoided the area.")),
    into_lf((avoid(E, Agent, Location), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('avoid-52')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'avoid-52_f1') :-
    nop(english("We avoided the ball.")),
    into_lf((avoid(E, Agent, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('avoid-52')),
                     1,
                     np(Theme, [+be_sc_ing]),
                     [],
                     A,
                     'avoid-52_f2') :-
    nop(english("He avoided going to the area.")),
    into_lf((avoid(E, Agent, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('avoid-52')),
                     1,
                     np(Theme, [+poss_ing]),
                     [],
                     A,
                     'avoid-52_f3') :-
    nop(english("He avoided his nagging.")),
    into_lf((avoid(E, Agent, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('banish-10.2')),
                     1,
                     np(Theme, [+tAnimate]),
                     [],
                     A,
                     'banish-10.2_f0') :-
    nop(english("The king banished the general.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), location(EndE, Theme, Destination), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tAnimate), isa(Source, tLocation), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('banish-10.2')),
                     3,
                     [np(Theme, [+tAnimate])],
                     [prep(Prep, [+src]), np(Source, [+tLocation])],
                     A,
                     'banish-10.2_f1') :-
    nop(english("The king banished the general from the army.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tAnimate), isa(Source, tLocation), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('banish-10.2')),
                     3,
                     [np(Theme, [+tAnimate])],
                     [prep(to), np(Destination, [-tRegion, +tLocation])],
                     A,
                     'banish-10.2_f2') :-
    nop(english("The king deported the general to the isle.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), location(EndE, Theme, Destination), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tAnimate), isa(Source, tLocation), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('base-97.1')),
                     3,
                     [np(Theme)],
                     [prep(on), np(Source)],
                     A,
                     'base-97.1_f0') :-
    nop(english("We based our plans on his information.")),
    into_lf((cause(E, Agent), base(E, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('base-97.1')),
                     3,
                     [np(Theme)],
                     [prep(on), np(Source, [+wh_comp])],
                     A,
                     'base-97.1_f1') :-
    nop(english("They based their decision on whether he happened to come or not.")),
    into_lf((cause(E, Agent), base(E, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('base-97.1')),
                     3,
                     [np(Theme)],
                     [prep(on), np(Source, [+poss_ing])],
                     A,
                     'base-97.1_f2') :-
    nop(english("They based their plan on his seizing the base.")),
    into_lf((cause(E, Agent), base(E, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('base-97.1')),
                     3,
                     [np(Theme)],
                     [prep(on), np(Source, [+acc_ing])],
                     A,
                     'base-97.1_f3') :-
    nop(english("They based their plans on him getting in on time.")),
    into_lf((cause(E, Agent), base(E, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('battle-36.4')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+wh_comp])
                     ],
                     A,
                     'battle-36.4_f0') :-
    nop(english("I battled with him about whether he should kill the peasants.")),
    into_lf((social_interaction(E, Agent, Co_Agent), conflict(E, Agent, Co_Agent), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('battle-36.4')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+what_extract])
                     ],
                     A,
                     'battle-36.4_f1') :-
    nop(english("I battled with him about whether he should go.")),
    into_lf((social_interaction(E, Agent, Co_Agent), conflict(E, Agent, Co_Agent), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('battle-36.4')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+what_inf])
                     ],
                     A,
                     'battle-36.4_f2') :-
    nop(english("I battled with him about what to do.")),
    into_lf((social_interaction(E, Agent, Co_Agent), conflict(E, Agent, Co_Agent), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('battle-36.4')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+wheth_inf])
                     ],
                     A,
                     'battle-36.4_f3') :-
    nop(english("I battled with him about whether to go.")),
    into_lf((social_interaction(E, Agent, Co_Agent), conflict(E, Agent, Co_Agent), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('battle-36.4')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [-sentential])
                     ],
                     A,
                     'battle-36.4_f4') :-
    nop(english("I battled with him about it.")),
    into_lf((social_interaction(E, Agent, Co_Agent), conflict(E, Agent, Co_Agent), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('battle-36.4-1')),
                     0,
                     [],
                     [],
                     A,
                     'battle-36.4-1_f0') :-
    nop(english("They battled.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), conflict(E, Agent_i, Agent_j), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('battle-36.4-1')),
                     2,
                     prep(about),
                     np(Topic, [+poss_ing]),
                     A,
                     'battle-36.4-1_f1') :-
    nop(english("They battled about his coming.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), conflict(E, Agent_i, Agent_j), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('battle-36.4-1')),
                     2,
                     prep(about),
                     np(Topic, [+wh_comp]),
                     A,
                     'battle-36.4-1_f2') :-
    nop(english("They battled about whether he should go.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), conflict(E, Agent_i, Agent_j), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('battle-36.4-1')),
                     2,
                     prep(about),
                     np(Topic, [+what_extract]),
                     A,
                     'battle-36.4-1_f3') :-
    nop(english("They battled about what was the right thing.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), conflict(E, Agent_i, Agent_j), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('battle-36.4-1')),
                     2,
                     prep(about),
                     np(Topic, [+wheth_inf]),
                     A,
                     'battle-36.4-1_f4') :-
    nop(english("They battled about whether to go.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), conflict(E, Agent_i, Agent_j), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('battle-36.4-1')),
                     2,
                     prep(about),
                     np(Topic, [+what_inf]),
                     A,
                     'battle-36.4-1_f5') :-
    nop(english("They battled about what to do.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), conflict(E, Agent_i, Agent_j), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('battle-36.4-1')),
                     2,
                     prep(about),
                     np(Topic, [-sentential]),
                     A,
                     'battle-36.4-1_f6') :-
    nop(english("They battled about it.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), conflict(E, Agent_i, Agent_j), about(E, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('become-109.1')),
                     1,
                     np(Result),
                     [],
                     A,
                     'become-109.1_f0') :-
    nop(english("The belt came undone")),
    into_lf((state(ResultE, Result, Patient), holds(Pred, ResultE, Patient), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('become-109.1-1')),
                     1,
                     np(Result, [-sentential]),
                     [],
                     A,
                     'become-109.1-1_f0') :-
    nop(english("He became a banker.")),
    into_lf((state(ResultE, Result, Patient), holds(Pred, ResultE, Patient), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('become-109.1-1-1')),
                     2,
                     prep(in),
                     np(Result, [-sentential]),
                     A,
                     'become-109.1-1-1_f0') :-
    nop(english("The matter seems in dispute.")),
    into_lf((state(ResultE, Result, Patient), holds(Pred, ResultE, Patient), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('beg-58.2')),
                     1,
                     np(Topic, [-for_comp]),
                     [],
                     A,
                     'beg-58.2_f0') :-
    nop(english("I begged for her to do it.")),
    into_lf((supplicate(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('beg-58.2')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(for), np(Topic, [-sentential])],
                     A,
                     'beg-58.2_f1') :-
    nop(english("I begged her for release.")),
    into_lf((supplicate(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('beg-58.2')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+oc_to_inf]),
                     A,
                     'beg-58.2_f2') :-
    nop(english("I begged her to do it.")),
    into_lf((supplicate(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('beg-58.2')),
                     2,
                     prep(for),
                     np(Topic, [-sentential]),
                     A,
                     'beg-58.2_f3') :-
    nop(english("I begged for release.")),
    into_lf((supplicate(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('beg-58.2')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+that_comp]),
                     A,
                     'beg-58.2_f4') :-
    nop(english("I begged him that he come.")),
    into_lf((supplicate(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('beg-58.2')),
                     1,
                     np(Topic, [+sc_to_inf]),
                     [],
                     A,
                     'beg-58.2_f5') :-
    nop(english("I begged to come.")),
    into_lf((supplicate(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('begin-55.1')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'begin-55.1_f0') :-
    nop(english("He began to pack.")),
    into_lf((begin(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('begin-55.1-1')),
                     1,
                     np(Theme, [+be_sc_ing]),
                     [],
                     A,
                     'begin-55.1-1_f0') :-
    nop(english("John began going to the area.")),
    into_lf((begin(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('begin-55.1-1')),
                     0,
                     [],
                     [],
                     A,
                     'begin-55.1-1_f1') :-
    nop(english("The storm began.")),
    into_lf((begin(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('begin-55.1-1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'begin-55.1-1_f2') :-
    nop(english("John started the party.")),
    into_lf((begin(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('begin-55.1-1')),
                     2,
                     prep(with),
                     np(Instrument),
                     A,
                     'begin-55.1-1_f3') :-
    nop(english("The party began with a bang.")),
    into_lf((begin(E, Theme), use(E, Agent, Instrument), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('begin-55.1-1')),
                     3,
                     [np(Theme)],
                     [prep(with), np(Instrument)],
                     A,
                     'begin-55.1-1_f4') :-
    nop(english("I began the party with a speech.")),
    into_lf((begin(E, Theme), use(E, Agent, Instrument), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Instrument),
                     verb(vn('begin-55.1-1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'begin-55.1-1_f5') :-
    nop(english("A murder began the book.")),
    into_lf((begin(E, Theme), use(E, Agent, Instrument), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('being_dressed-41.3.3')),
                     2,
                     prep(in),
                     np(Theme),
                     A,
                     'being_dressed-41.3.3_f0') :-
    nop(english("She was always clad in black.")),
    into_lf((wear(E, Agent, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bend-45.2')),
                     1,
                     np(Patient, [+tSolid]),
                     [],
                     A,
                     'bend-45.2_f0') :-
    nop(english("Tony bent the rod.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), physical_form(ResultE, Form, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bend-45.2')),
                     3,
                     [np(Patient, [+tSolid])],
                     [prep(with), np(Instrument, [+tSolid])],
                     A,
                     'bend-45.2_f1') :-
    nop(english("Tony bent the rod with pliers.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), physical_form(ResultE, Form, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid]),
                     verb(vn('bend-45.2')),
                     0,
                     [],
                     [],
                     A,
                     'bend-45.2_f2') :-
    nop(english("The rod bent.")),
    into_lf((physical_form(ResultE, Form, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid]),
                     verb(vn('bend-45.2')),
                     1,
                     'ADV',
                     [],
                     A,
                     'bend-45.2_f3') :-
    nop(english("Copper rods bend easily.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Instrument, [+tSolid]),
                     verb(vn('bend-45.2')),
                     1,
                     np(Patient, [+tSolid]),
                     [],
                     A,
                     'bend-45.2_f4') :-
    nop(english("The pliers bent the rod.")),
    into_lf((contact(E, Instrument, Patient), physical_form(ResultE, Form, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bend-45.2')),
                     2,
                     np(Patient, [+tSolid]),
                     np(Result),
                     A,
                     'bend-45.2_f5') :-
    nop(english("Tony folded the flaps open.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), physical_form(ResultE, Form, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bend-45.2')),
                     3,
                     [np(Patient, [+tSolid])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'bend-45.2_f6') :-
    nop(english("Tony bent the rod into a U.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), physical_form(ResultE, Form, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bend-45.2')),
                     5,
                     [np(Patient, [+tSolid])],
                     [ prep((to;into)),
                       np(Result, [+state]),
                       prep(with),
                       np(Instrument, [+tSolid])
                     ],
                     A,
                     'bend-45.2_f7') :-
    nop(english("Tony bent the rod into a U with pliers.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), physical_form(ResultE, Form, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('berry-13.7')),
                     0,
                     [],
                     [],
                     A,
                     'berry-13.7_f0') :-
    nop(english("The children berry in the summer.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('bill-54.5')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'bill-54.5_f0') :-
    nop(english("The phone company billed me.")),
    into_lf((financial_relationship(E, Agent, Recipient, Asset), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Asset, tCurrency), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('bill-54.5')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Asset, [+tCurrency]),
                     A,
                     'bill-54.5_f1') :-
    nop(english("The phone company billed me $10.")),
    into_lf((financial_relationship(E, Agent, Recipient, Asset), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Asset, tCurrency), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('body_internal_motion-49')),
                     0,
                     [],
                     [],
                     A,
                     'body_internal_motion-49_f0') :-
    nop(english("Sylvia fidgeted.")),
    into_lf((body_motion(E, Agent), isa(Agent, tAnimate), isa(Patient, tRefl), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('body_internal_states-40.6')),
                     0,
                     [],
                     [],
                     A,
                     'body_internal_states-40.6_f0') :-
    nop(english("Sharon shivered.")),
    into_lf((body_reflex(E, Experiencer), involuntary(E, Experiencer), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('body_internal_states-40.6')),
                     2,
                     prep((from;at)),
                     np(Stimulus),
                     A,
                     'body_internal_states-40.6_f1') :-
    nop(english("Sharon shivered from fear.")),
    into_lf((body_reflex(E, Experiencer), involuntary(E, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('braid-41.2.2')),
                     1,
                     np(Patient, [+tBodyPart]),
                     [],
                     A,
                     'braid-41.2.2_f0') :-
    nop(english("Celia brushed the baby's hair.")),
    into_lf((take_care_of(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('break-45.1')),
                     1,
                     np(Patient, [+tSolid]),
                     [],
                     A,
                     'break-45.1_f0') :-
    nop(english("Tony broke the window.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('break-45.1')),
                     3,
                     [np(Patient, [+tSolid])],
                     [prep(with), np(Instrument, [+tSolid])],
                     A,
                     'break-45.1_f1') :-
    nop(english("Tony broke the window with a hammer.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid]),
                     verb(vn('break-45.1')),
                     0,
                     [],
                     [],
                     A,
                     'break-45.1_f2') :-
    nop(english("The window broke.")),
    into_lf((degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid]),
                     verb(vn('break-45.1')),
                     1,
                     'ADV',
                     [],
                     A,
                     'break-45.1_f3') :-
    nop(english("Crystal vases break easily.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Instrument, [+tSolid]),
                     verb(vn('break-45.1')),
                     1,
                     np(Patient, [+tSolid]),
                     [],
                     A,
                     'break-45.1_f4') :-
    nop(english("The hammer broke the window.")),
    into_lf((contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('break-45.1')),
                     2,
                     np(Patient, [+tSolid]),
                     np(Result),
                     A,
                     'break-45.1_f5') :-
    nop(english("Tony broke the piggy bank open.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('break-45.1')),
                     4,
                     [np(Patient, [+tSolid]), np(Result)],
                     [prep(with), np(Instrument, [+tSolid])],
                     A,
                     'break-45.1_f6') :-
    nop(english("Tony broke the piggy bank open with a hammer.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('break-45.1')),
                     3,
                     [np(Patient, [+tSolid])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'break-45.1_f7') :-
    nop(english("Tony broke the glass to pieces.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('break-45.1')),
                     5,
                     [np(Patient, [+tSolid])],
                     [ prep((to;into)),
                       np(Result, [+state]),
                       prep(with),
                       np(Instrument, [+tSolid])
                     ],
                     A,
                     'break-45.1_f8') :-
    nop(english("Tony broke the glass to pieces with a hammer.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid]),
                     verb(vn('break-45.1')),
                     2,
                     prep((to;into)),
                     np(Result, [+state]),
                     A,
                     'break-45.1_f9') :-
    nop(english("The glass broke into a thousand pieces.")),
    into_lf((degradation_material_integrity(ResultE, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('breathe-40.1.2')),
                     0,
                     [],
                     [],
                     A,
                     'breathe-40.1.2_f0') :-
    nop(english("Paul breathed.")),
    into_lf((body_process(E, Agent), emit(E, Agent, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('breathe-40.1.2')),
                     2,
                     prep((on;onto)),
                     np(Destination),
                     A,
                     'breathe-40.1.2_f1') :-
    nop(english("Paul breathed on Mary.")),
    into_lf((body_process(E, Agent), emit(E, Agent, Theme), holds(Prep, E, Theme, Destination), isa(Agent, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('breathe-40.1.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'breathe-40.1.2_f2') :-
    nop(english("The dragon breathed fire.")),
    into_lf((body_process(E, Agent), emit(E, Agent, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('breathe-40.1.2')),
                     3,
                     [np(Theme)],
                     [prep((on;onto)), np(Destination)],
                     A,
                     'breathe-40.1.2_f3') :-
    nop(english("The dragon breathed fire on Mary.")),
    into_lf((body_process(E, Agent), emit(E, Agent, Theme), holds(Prep, E, Theme, Destination), isa(Agent, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('breathe-40.1.2-1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'breathe-40.1.2-1_f0') :-
    nop(english("Paul breathed a deep breath.")),
    into_lf((body_process(E, Agent), emit(E, Agent, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bring-11.3')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'bring-11.3_f0') :-
    nop(english("Nora brought the book.")),
    into_lf((motion(E0, Theme), equals(E0, E1), motion(E1, Agent), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(E1, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bring-11.3')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep((against;before;into;on;to;onto)),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'bring-11.3_f1') :-
    nop(english("Nora brought the book to the meeting.")),
    into_lf((motion(E0, Theme), location(EndE0, Theme, Destination), equals(E0, E1), motion(E1, Agent), location(EndE1, Agent, Destination), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bring-11.3')),
                     3,
                     [],
                     [ prep((against;before;into;on;to;onto)),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ]),
                       np(Theme, [+tConcrete])
                     ],
                     A,
                     'bring-11.3_f2') :-
    nop(english("Nora brought to lunch the book.")),
    into_lf((motion(E0, Theme), location(EndE0, Theme, Destination), equals(E0, E1), motion(E1, Agent), location(EndE1, Agent, Destination), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bring-11.3')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'bring-11.3_f3') :-
    nop(english("Nora brought the book from home.")),
    into_lf((motion(E0, Theme), location(StartE0, Theme, Initial_Location), equals(E0, E1), motion(E1, Agent), location(StartE1, Agent, Initial_Location), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(StartE0, actEvent), start(E0, StartE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bring-11.3')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation]),
                       prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'bring-11.3_f4') :-
    nop(english("Nora brought the book from home to the meeting.")),
    into_lf((motion(E0, Theme), location(StartE0, Theme, Initial_Location), location(EndE0, Theme, Destination), equals(E0, E1), motion(E1, Agent), location(StartE1, Agent, Initial_Location), location(EndE1, Agent, Destination), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(StartE0, actEvent), start(E0, StartE0), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Instrument),
                     verb(vn('bring-11.3')),
                     2,
                     np(Theme, [+tConcrete]),
                     np(Destination,
                        [ or(isa(Destination, tAnimate),
                             (isa(Destination, tLocation), ~isa(Destination, tRegion))),
                          +adv_loc
                        ]),
                     A,
                     'bring-11.3_f5') :-
    nop(english("The train brought us here.")),
    into_lf((motion(E0, Theme), motion(E1, Instrument), location(EndE0, Theme, Destination), location(EndE1, Instrument, Destination), equals(E0, E1), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(E1, actEvent), isa(EndE0, actEvent), end(E0, EndE0), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bring-11.3-1')),
                     2,
                     np(Destination,
                        [ +tAnimate,
                          or(isa(Destination, tAnimate),
                             (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                        ]),
                     np(Theme, [+tConcrete]),
                     A,
                     'bring-11.3-1_f0') :-
    nop(english("Nora brought Pamela the book.")),
    into_lf((motion(E, Theme), direction(E, Toward, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(Destination, tAnimate), isa(E, actEvent), isa(Toward, 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('bring-11.3-1')),
                     2,
                     np(Theme, [+tConcrete, +plural]),
                     lex(together),
                     A,
                     'bring-11.3-1_f1') :-
    nop(english("Nora brought us together.")),
    into_lf((motion(E, Theme), cause(Agent, E), ~together(StartE, '$VAR'('Abstract/physical'), Theme_i, Theme_j), together(EndE, '$VAR'('Abstract/physical'), Theme_i, Theme_j), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(Destination, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa('$VAR'('Abstract/physical'), 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     1,
                     np(Product),
                     [],
                     A,
                     'build-26.1_f0') :-
    nop(english("Martha carves toys.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     0,
                     [],
                     [],
                     A,
                     'build-26.1_f1') :-
    nop(english("Martha carves.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     3,
                     [np(Product)],
                     [prep((from;out;of)), np(Material, [+tConcrete])],
                     A,
                     'build-26.1_f2') :-
    nop(english("Martha carved a toy out of a piece of wood.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     3,
                     [np(Material, [+tConcrete])],
                     [prep(into), np(Product)],
                     A,
                     'build-26.1_f3') :-
    nop(english("Martha carved the piece of wood into a toy.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     3,
                     [np(Product)],
                     [ prep(for),
                       np(Beneficiary,
                          [ or(isa(Beneficiary, tAnimate),
                               isa(Beneficiary, tOrganization))
                          ])
                     ],
                     A,
                     'build-26.1_f4') :-
    nop(english("Martha carved a toy for the baby.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     4,
                     [ np(Beneficiary,
                          [ or(isa(Beneficiary, tAnimate),
                               isa(Beneficiary, tOrganization))
                          ]),
                       np(Product)
                     ],
                     [prep((from;out;of)), np(Material, [+tConcrete])],
                     A,
                     'build-26.1_f5') :-
    nop(english("Martha carved the baby a toy out of a piece of wood.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     5,
                     [np(Product)],
                     [ prep((from;out;of)),
                       np(Material, [+tConcrete]),
                       prep(for),
                       np(Beneficiary,
                          [ or(isa(Beneficiary, tAnimate),
                               isa(Beneficiary, tOrganization))
                          ])
                     ],
                     A,
                     'build-26.1_f6') :-
    nop(english("Martha carved a toy out of a piece of wood for the baby.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     3,
                     [np(Material, [+tConcrete])],
                     [ prep(for),
                       np(Beneficiary,
                          [ or(isa(Beneficiary, tAnimate),
                               isa(Beneficiary, tOrganization))
                          ])
                     ],
                     A,
                     'build-26.1_f7') :-
    nop(english("Martha carved a piece of wood for the baby.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     5,
                     [np(Material, [+tConcrete])],
                     [ prep(into),
                       np(Product),
                       prep(for),
                       np(Beneficiary,
                          [ or(isa(Beneficiary, tAnimate),
                               isa(Beneficiary, tOrganization))
                          ])
                     ],
                     A,
                     'build-26.1_f8') :-
    nop(english("Martha carved a piece of wood into a toy for the baby.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Material, [+tConcrete]),
                     verb(vn('build-26.1')),
                     1,
                     np(Product),
                     [],
                     A,
                     'build-26.1_f9') :-
    nop(english("This wood carved beautiful toys.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     4,
                     [ np(Beneficiary,
                          [ or(isa(Beneficiary, tAnimate),
                               isa(Beneficiary, tOrganization))
                          ]),
                       np(Product)
                     ],
                     [prep(for), np(Asset, [+tCurrency])],
                     A,
                     'build-26.1_f10') :-
    nop(english("The contractor will build you a house for $100, 000.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), benefit(E, Beneficiary), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1')),
                     5,
                     [np(Product)],
                     [ prep((from;out;of)),
                       np(Material, [+tConcrete]),
                       prep(for),
                       np(Asset, [+tCurrency])
                     ],
                     A,
                     'build-26.1_f11') :-
    nop(english("Martha will carve a toy out of a piece of wood for $50.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('build-26.1-1')),
                     3,
                     [np(Product)],
                     [prep(for), np(Asset, [+tCurrency])],
                     A,
                     'build-26.1-1_f0') :-
    nop(english("The contractor builds houses for $100, 000.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Asset, [+tCurrency]),
                     verb(vn('build-26.1-1')),
                     1,
                     np(Product),
                     [],
                     A,
                     'build-26.1-1_f1') :-
    nop(english("$100, 000 builds a house.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Asset, [+tCurrency]),
                     verb(vn('build-26.1-1')),
                     2,
                     np(Beneficiary,
                        [ or(isa(Beneficiary, tAnimate),
                             isa(Beneficiary, tOrganization))
                        ]),
                     np(Product),
                     A,
                     'build-26.1-1_f2') :-
    nop(english("$100, 000 will build you a house.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), benefit(E, Beneficiary), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Asset, [+tCurrency]),
                     verb(vn('build-26.1-1')),
                     3,
                     [np(Product)],
                     [prep((from;out;of)), np(Material, [+tConcrete])],
                     A,
                     'build-26.1-1_f3') :-
    nop(english("$100, 000 builds a house out of sticks.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Asset, [+tCurrency]),
                     verb(vn('build-26.1-1')),
                     4,
                     [ np(Beneficiary,
                          [ or(isa(Beneficiary, tAnimate),
                               isa(Beneficiary, tOrganization))
                          ]),
                       np(Product)
                     ],
                     [prep((from;out;of)), np(Material, [+tConcrete])],
                     A,
                     'build-26.1-1_f4') :-
    nop(english("$100, 000 builds you a house out of sticks.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), benefit(E, Beneficiary), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Material, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Location, [+tConcrete]),
                     verb(vn('bulge-47.5.3')),
                     2,
                     prep(with),
                     np(Theme),
                     A,
                     'bulge-47.5.3_f0') :-
    nop(english("The bag is bulging with groceries.")),
    into_lf((filled_with(E, Location, Theme), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Location, [+tConcrete]),
                     verb(vn('bulge-47.5.3')),
                     0,
                     [],
                     [],
                     A,
                     'bulge-47.5.3_f1') :-
    nop(english("The bag is bulging.")),
    into_lf((filled_with(E, Location, Theme), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('bump-18.4')),
                     2,
                     prep((against;into;onto)),
                     np(Location, [+tConcrete]),
                     A,
                     'bump-18.4_f0') :-
    nop(english("The grocery cart hit against the wall.")),
    into_lf((manner(E, Directedmotion, Theme), ~contact(E, Theme, Location), manner(EndE, Forceful, Theme), contact(EndE, Theme, Location), isa(Location, tConcrete), isa(Theme, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete, +plural]),
                     verb(vn('bump-18.4-1')),
                     1,
                     lex(together),
                     [],
                     A,
                     'bump-18.4-1_f0') :-
    nop(english("The grocery carts thudded together.")),
    into_lf((manner(E, Directedmotion, Theme_i), manner(E, Directedmotion, Theme_j), manner(EndE, Forceful, Theme_i), manner(EndE, Forceful, Theme_j), contact(EndE, Theme_i, Theme_j), isa(Location, tConcrete), isa(Theme, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('butter-9.9')),
                     1,
                     np(Destination, [-tRegion, +tLocation]),
                     [],
                     A,
                     'butter-9.9_f0') :-
    nop(english("Lora buttered the toast.")),
    into_lf((motion(E, Theme), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('butter-9.9')),
                     3,
                     [np(Destination, [-tRegion, +tLocation])],
                     [prep(with), np(Theme, [+tConcrete])],
                     A,
                     'butter-9.9_f1') :-
    nop(english("Lora buttered the toast with unsalted butter.")),
    into_lf((motion(E, Theme), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Attribute, [+tScalar]),
                     lex(of),
                     2,
                     np(Patient),
                     verb(vn('calibratable_cos-45.6')),
                     A,
                     'calibratable_cos-45.6_f0') :-
    nop(english("The price of oil soared.")),
    into_lf((change_value(E, Direction, Attribute, Patient), isa(Attribute, tScalar), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('calibratable_cos-45.6')),
                     2,
                     prep(in),
                     np(Attribute, [+tScalar]),
                     A,
                     'calibratable_cos-45.6_f1') :-
    nop(english("Oil soared in price.")),
    into_lf((change_value(E, Direction, Attribute, Patient), isa(Attribute, tScalar), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Attribute, [+tScalar]),
                     lex(of),
                     3,
                     [np(Patient)],
                     [verb(vn('calibratable_cos-45.6')), np(Extent)],
                     A,
                     'calibratable_cos-45.6_f2') :-
    nop(english("The price of oil increased ten percent.")),
    into_lf((change_value(E, Direction, Attribute, Patient), amount_changed(E, Attribute, Patient, Extent), isa(Attribute, tScalar), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Attribute, [+tScalar]),
                     lex(of),
                     4,
                     [np(Patient)],
                     [ verb(vn('calibratable_cos-45.6')),
                       prep(by),
                       np(Extent)
                     ],
                     A,
                     'calibratable_cos-45.6_f3') :-
    nop(english("The price of oil increased by ten percent.")),
    into_lf((change_value(E, Direction, Attribute, Patient), amount_changed(E, Attribute, Patient, Extent), isa(Attribute, tScalar), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     lex('\'s'),
                     3,
                     [np(Attribute, [+tScalar])],
                     [verb(vn('calibratable_cos-45.6')), np(Extent)],
                     A,
                     'calibratable_cos-45.6_f4') :-
    nop(english("Oils's price increased ten percent.")),
    into_lf((change_value(E, Direction, Attribute, Patient), amount_changed(E, Attribute, Patient, Extent), isa(Attribute, tScalar), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     lex('\'s'),
                     4,
                     [np(Attribute, [+tScalar])],
                     [ verb(vn('calibratable_cos-45.6')),
                       prep(by),
                       np(Extent)
                     ],
                     A,
                     'calibratable_cos-45.6_f5') :-
    nop(english("Oils's price increased by ten percent.")),
    into_lf((change_value(E, Direction, Attribute, Patient), amount_changed(E, Attribute, Patient, Extent), isa(Attribute, tScalar), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('calibratable_cos-45.6')),
                     0,
                     [],
                     [],
                     A,
                     'calibratable_cos-45.6_f6') :-
    nop(english("Imports have declined.")),
    into_lf((change_value(E, Direction, Attribute, Patient), isa(Attribute, tScalar), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('calibratable_cos-45.6')),
                     2,
                     prep(by),
                     np(Extent),
                     A,
                     'calibratable_cos-45.6_f7') :-
    nop(english("Nonperforming assets at these banks declined by %15.")),
    into_lf((change_value(E, Direction, Attribute, Patient), amount_changed(E, Attribute, Patient, Extent), isa(Attribute, tScalar), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     lex('\'s'),
                     2,
                     np(Attribute, [+tScalar]),
                     verb(vn('calibratable_cos-45.6-1')),
                     A,
                     'calibratable_cos-45.6-1_f0') :-
    nop(english("Oil's price soared.")),
    into_lf((change_value(E, Direction, Attribute, Patient), isa(Attribute, tScalar), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('calve-28')),
                     0,
                     [],
                     [],
                     A,
                     'calve-28_f0') :-
    nop(english("The cat kittened.")),
    into_lf((give_birth(EndE, Agent, Patient), cause(Agent, E), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('captain-29.8')),
                     2,
                     prep(for),
                     np(Beneficiary, [+tConcrete]),
                     A,
                     'captain-29.8_f0') :-
    nop(english("Her cousin clerked for Judge Davis.")),
    into_lf((masquerade(E, Agent, Role), benefit(E, Beneficiary), isa(Agent, tAnimate), isa(Beneficiary, tConcrete), isa(E, actEvent), isa(Role, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('captain-29.8-1')),
                     1,
                     np(Beneficiary, [+tConcrete]),
                     [],
                     A,
                     'captain-29.8-1_f0') :-
    nop(english("Miriam tutored her brother.")),
    into_lf((masquerade(E, Agent, Role), benefit(E, Beneficiary), isa(Agent, tAnimate), isa(Beneficiary, tConcrete), isa(E, actEvent), isa(Role, Pred)),
            A).
vndata:verbnet_frame(np(Beneficiary, [+tConcrete]),
                     verb(vn('captain-29.8-1-1')),
                     1,
                     np(Agent, [+tAnimate]),
                     [],
                     A,
                     'captain-29.8-1-1_f0') :-
    nop(english("The show starred Miriam.")),
    into_lf((masquerade(E, Agent, Role), benefit(E, Beneficiary), isa(Agent, tAnimate), isa(Beneficiary, tConcrete), isa(E, actEvent), isa(Role, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('care-88.1')),
                     0,
                     [],
                     [],
                     A,
                     'care-88.1_f0') :-
    nop(english("He doesn't care.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('care-88.1')),
                     1,
                     np(Stimulus, [+that_comp]),
                     [],
                     A,
                     'care-88.1_f1') :-
    nop(english("He doesn't care that she comes.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('care-88.1')),
                     1,
                     np(Stimulus, [+wh_comp]),
                     [],
                     A,
                     'care-88.1_f2') :-
    nop(english("He doesn't care whether she comes.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('care-88.1-1')),
                     2,
                     prep(about),
                     np(Stimulus, [+what_extract]),
                     A,
                     'care-88.1-1_f0') :-
    nop(english("He doesn't care about what he should do.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('care-88.1-1')),
                     1,
                     np(Stimulus, [+what_extract]),
                     [],
                     A,
                     'care-88.1-1_f1') :-
    nop(english("I don't care what he does.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('care-88.1-1')),
                     2,
                     prep(about),
                     np(Stimulus, [+wh_comp]),
                     A,
                     'care-88.1-1_f2') :-
    nop(english("He doesn't care about how/whether she comes.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'carry-11.4_f0') :-
    nop(english("Amanda carried the package.")),
    into_lf((motion(E0, Theme), equals(E0, E1), motion(E1, Agent), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E0, actEvent), isa(E1, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [prep((to;towards)), np(Destination)],
                     A,
                     'carry-11.4_f1') :-
    nop(english("Amanda carried the package to New York.")),
    into_lf((motion(E0, Theme), location(EndE0, Theme, Destination), equals(E0, E1), motion(E1, Agent), location(EndE1, Agent, Destination), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E0, actEvent), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'carry-11.4_f2') :-
    nop(english("Amanda carried the package from home.")),
    into_lf((motion(E0, Theme), location(StartE0, Theme, Initial_Location), equals(E0, E1), motion(E1, Agent), location(StartE1, Agent, Initial_Location), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E0, actEvent), isa(StartE0, actEvent), start(E0, StartE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation]),
                       prep((to;towards)),
                       np(Destination)
                     ],
                     A,
                     'carry-11.4_f3') :-
    nop(english("Amanda carried the package from home to New York.")),
    into_lf((motion(E0, Theme), location(StartE0, Theme, Initial_Location), location(EndE0, Theme, Destination), equals(E0, E1), motion(E1, Agent), location(StartE1, Agent, Initial_Location), location(EndE1, Agent, Destination), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E0, actEvent), isa(StartE0, actEvent), start(E0, StartE0), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep((to;towards)),
                       np(Destination),
                       prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'carry-11.4_f4') :-
    nop(english("Amanda carried the package to New York from home.")),
    into_lf((motion(E0, Theme), location(StartE0, Theme, Initial_Location), location(EndE0, Theme, Destination), equals(E0, E1), motion(E1, Agent), location(StartE1, Agent, Initial_Location), location(EndE1, Agent, Destination), cause(Agent, E0), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E0, actEvent), isa(StartE0, actEvent), start(E0, StartE0), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4-1')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'carry-11.4-1_f0') :-
    nop(english("Amanda shoved the box.")),
    into_lf((motion(E, Theme), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4-1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'carry-11.4-1_f1') :-
    nop(english("Amanda shoved the box from the corner.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4-1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [prep((to;towards)), np(Destination)],
                     A,
                     'carry-11.4-1_f2') :-
    nop(english("Amanda shoved the box to John.")),
    into_lf((motion(E, Theme), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4-1')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation]),
                       prep((to;towards)),
                       np(Destination)
                     ],
                     A,
                     'carry-11.4-1_f3') :-
    nop(english("Amanda shoved the box from the corner to John.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4-1')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep((to;towards)),
                       np(Destination),
                       prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'carry-11.4-1_f4') :-
    nop(english("Amanda shoved the box to John from the corner.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carry-11.4-1-1')),
                     2,
                     np(Destination, [+tAnimate]),
                     np(Theme, [+tConcrete]),
                     A,
                     'carry-11.4-1-1_f0') :-
    nop(english("Amanda shoved John the box.")),
    into_lf((motion(E, Theme), direction(E, Toward, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(Destination, tAnimate), isa(E, actEvent), isa(Toward, 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carve-21.2-1')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'carve-21.2-1_f0') :-
    nop(english("Carol crushed the ice.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carve-21.2-1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(with), np(Instrument, [+tConcrete])],
                     A,
                     'carve-21.2-1_f1') :-
    nop(english("Carol crushed the ice with a hammer.")),
    into_lf((cause(Agent, E), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('carve-21.2-1')),
                     1,
                     'ADV',
                     [],
                     A,
                     'carve-21.2-1_f2') :-
    nop(english("The ice crushes easily.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('carve-21.2-1')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'carve-21.2-1_f3') :-
    nop(english("The hammer crushed the marble.")),
    into_lf((contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), physical_form(ResultE, Form, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('carve-21.2-1')),
                     1,
                     'ADV',
                     [],
                     A,
                     'carve-21.2-1_f4') :-
    nop(english("That hammer crushes well.")),
    into_lf((property(Instrument, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carve-21.2-2')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'carve-21.2-2_f0') :-
    nop(english("Carol carved the stone.")),
    into_lf((cause(Agent, E), manner(E, Motion, Agent), contact(E, Agent, Patient), degradation_material_integrity(ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('carve-21.2-2')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(with), np(Instrument, [+tConcrete])],
                     A,
                     'carve-21.2-2_f1') :-
    nop(english("Carol carved the stone with a chisel.")),
    into_lf((cause(Agent, E), manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('carve-21.2-2')),
                     1,
                     'ADV',
                     [],
                     A,
                     'carve-21.2-2_f2') :-
    nop(english("Marble carves easily.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('carve-21.2-2')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'carve-21.2-2_f3') :-
    nop(english("That chisel carved the statue.")),
    into_lf((contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('carve-21.2-2')),
                     1,
                     'ADV',
                     [],
                     A,
                     'carve-21.2-2_f4') :-
    nop(english("That chisel carves well.")),
    into_lf((property(Instrument, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('change_bodily_state-40.8.4')),
                     0,
                     [],
                     [],
                     A,
                     'change_bodily_state-40.8.4_f0') :-
    nop(english("Sharon fainted.")),
    into_lf((body_reflex(E, Experiencer), involuntary(E, Experiencer), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('change_bodily_state-40.8.4')),
                     2,
                     prep((from;at)),
                     np(Stimulus),
                     A,
                     'change_bodily_state-40.8.4_f1') :-
    nop(english("Sharon fainted from hunger.")),
    into_lf((body_reflex(E, Experiencer), involuntary(E, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tConcrete),
                               isa(Theme, tOrganization))
                          ])
                     ],
                     [lex(as), np(Attribute, [+oc_ing])],
                     A,
                     'characterize-29.2_f0') :-
    nop(english("He characterized him as being smart.")),
    into_lf((characterize(E, Theme, Attribute), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tConcrete),
                               isa(Theme, tOrganization))
                          ])
                     ],
                     [lex(as), np(Attribute, [+small_clause])],
                     A,
                     'characterize-29.2_f1') :-
    nop(english("He characterized him to be smart.")),
    into_lf((characterize(E, Theme, Attribute), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tConcrete),
                               isa(Theme, tOrganization))
                          ])
                     ],
                     [prep(as), np(Attribute, [-sentential])],
                     A,
                     'characterize-29.2_f2') :-
    nop(english("He characterized him as smart.")),
    into_lf((characterize(E, Theme, Attribute), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2-1')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization)),
                          +how_extract
                        ]),
                     [],
                     A,
                     'characterize-29.2-1_f0') :-
    nop(english("He described how he could rise to high office.")),
    into_lf((describe(E, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2-1')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization)),
                          +wh_inf
                        ]),
                     [],
                     A,
                     'characterize-29.2-1_f1') :-
    nop(english("He described how to do it.")),
    into_lf((describe(E, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2-1-1')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization)),
                          +ac_ing
                        ]),
                     [],
                     A,
                     'characterize-29.2-1-1_f0') :-
    nop(english("He described going to work.")),
    into_lf((describe(E, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2-1-1')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization)),
                          +what_extract
                        ]),
                     [],
                     A,
                     'characterize-29.2-1-1_f1') :-
    nop(english("He described what we should do.")),
    into_lf((describe(E, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2-1-1')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization)),
                          +what_inf
                        ]),
                     [],
                     A,
                     'characterize-29.2-1-1_f2') :-
    nop(english("He described what to do.")),
    into_lf((describe(E, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2-1-2')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization)),
                          +np_ppart
                        ]),
                     [],
                     A,
                     'characterize-29.2-1-2_f0') :-
    nop(english("He revealed the children found.")),
    into_lf((describe(E, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2-1-2')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization)),
                          +wh_comp
                        ]),
                     [],
                     A,
                     'characterize-29.2-1-2_f1') :-
    nop(english("He revealed if we should come.")),
    into_lf((describe(E, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2-1-2')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization)),
                          +what_extract
                        ]),
                     [],
                     A,
                     'characterize-29.2-1-2_f2') :-
    nop(english("He revealed what he should do.")),
    into_lf((describe(E, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('characterize-29.2-1-2')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization)),
                          +what_inf
                        ]),
                     [],
                     A,
                     'characterize-29.2-1-2_f3') :-
    nop(english("He revealed what to do.")),
    into_lf((describe(E, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('chase-51.6')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'chase-51.6_f0') :-
    nop(english("Jackie chased the thief.")),
    into_lf((motion(E, Agent), motion(E, Theme), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('chase-51.6')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [prep(Prep, [+spatial]), np(Location)],
                     A,
                     'chase-51.6_f1') :-
    nop(english("Jackie chased the thief down the street.")),
    into_lf((motion(E, Agent), motion(E, Theme), holds(Prep, E, Theme, Location), holds(Prep, E, Agent, Location), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('chase-51.6')),
                     2,
                     prep(after),
                     np(Theme, [+tConcrete]),
                     A,
                     'chase-51.6_f2') :-
    nop(english("Jackie chased after the thief.")),
    into_lf((motion(E, Agent), motion(E, Theme), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('cheat-10.6')),
                     3,
                     [ np(Source,
                          [ or(isa(Source, tAnimate),
                               (isa(Source, tLocation), ~isa(Source, tRegion)))
                          ])
                     ],
                     [prep(of), np(Theme)],
                     A,
                     'cheat-10.6_f0') :-
    nop(english("The doctor cured Pat of pneumonia.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Source, tAnimate), (isa(Source, tLocation), ~isa(Source, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('cheat-10.6')),
                     3,
                     [ np(Source,
                          [ or(isa(Source, tAnimate),
                               (isa(Source, tLocation), ~isa(Source, tRegion)))
                          ])
                     ],
                     [prep((out;of)), np(Theme)],
                     A,
                     'cheat-10.6_f1') :-
    nop(english("The swindler cheated Pat out of her fortune.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Source, tAnimate), (isa(Source, tLocation), ~isa(Source, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('cheat-10.6')),
                     1,
                     np(Source,
                        [ or(isa(Source, tAnimate),
                             (isa(Source, tLocation), ~isa(Source, tRegion)))
                        ]),
                     [],
                     A,
                     'cheat-10.6_f2') :-
    nop(english("The doctor cured Pat.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Source, tAnimate), (isa(Source, tLocation), ~isa(Source, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('cheat-10.6-1')),
                     3,
                     [np(Theme)],
                     [ prep(from),
                       np(Source,
                          [ or(isa(Source, tAnimate),
                               (isa(Source, tLocation), ~isa(Source, tRegion)))
                          ])
                     ],
                     A,
                     'cheat-10.6-1_f0') :-
    nop(english("The swindler bilked 20 dollars from his boss.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Source, tAnimate), (isa(Source, tLocation), ~isa(Source, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('chew-39.2-1')),
                     1,
                     np(Patient, [+tSolid, +tComestible]),
                     [],
                     A,
                     'chew-39.2-1_f0') :-
    nop(english("Cynthia nibbled the carrot.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('chew-39.2-1')),
                     0,
                     [],
                     [],
                     A,
                     'chew-39.2-1_f1') :-
    nop(english("Cynthia nibbled.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('chew-39.2-1')),
                     2,
                     prep(at),
                     np(Patient, [+tSolid, +tComestible]),
                     A,
                     'chew-39.2-1_f2') :-
    nop(english("Cynthia nibbled at the carrot.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('chew-39.2-1')),
                     2,
                     prep(on),
                     np(Patient, [+tSolid, +tComestible]),
                     A,
                     'chew-39.2-1_f3') :-
    nop(english("Cynthia nibbled on the carrot.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('chew-39.2-2')),
                     1,
                     np(Patient, [-tSolid, +tComestible]),
                     [],
                     A,
                     'chew-39.2-2_f0') :-
    nop(english("Cynthia sipped the drink.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), ~isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('chew-39.2-2')),
                     0,
                     [],
                     [],
                     A,
                     'chew-39.2-2_f1') :-
    nop(english("Cynthia sipped.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), ~isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('chew-39.2-2')),
                     2,
                     prep(from),
                     np(Patient, [-tSolid, +tComestible]),
                     A,
                     'chew-39.2-2_f2') :-
    nop(english("Cynthia sipped from the drink.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), ~isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('chit_chat-37.6')),
                     0,
                     [],
                     [],
                     A,
                     'chit_chat-37.6_f0') :-
    nop(english("Susan was chitchatting.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('chit_chat-37.6')),
                     2,
                     prep(with),
                     np(Co_Agent,
                        [ or(isa(Co_Agent, tAnimate),
                             isa(Co_Agent, tOrganization))
                        ]),
                     A,
                     'chit_chat-37.6_f1') :-
    nop(english("Susan chitchatted with Rachel.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), transfer_info(E, Co_Agent, Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('chit_chat-37.6')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+tCommunication])
                     ],
                     A,
                     'chit_chat-37.6_f2') :-
    nop(english("Susan chitchatted with Rachel about the problem.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), transfer_info(E, Co_Agent, Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('chit_chat-37.6')),
                     0,
                     [],
                     [],
                     A,
                     'chit_chat-37.6_f3') :-
    nop(english("Susan and Rachel chitchatted.")),
    into_lf((transfer_info(E, Agent_i, Agent_j, Topic), transfer_info(E, Agent_j, Agent_i, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('chit_chat-37.6')),
                     4,
                     [],
                     [ prep(about),
                       np(Topic, [+tCommunication]),
                       prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ])
                     ],
                     A,
                     'chit_chat-37.6_f4') :-
    nop(english("Susan chitchatted about the problem with Rachel.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), transfer_info(E, Co_Agent, Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('chit_chat-37.6')),
                     2,
                     prep(about),
                     np(Topic, [+tCommunication]),
                     A,
                     'chit_chat-37.6_f5') :-
    nop(english("Susan and Rachel chitchatted about matters of great import.")),
    into_lf((transfer_info(E, Agent_i, Agent_j, Topic), transfer_info(E, Agent_j, Agent_i, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('chit_chat-37.6')),
                     2,
                     prep(about),
                     np(Topic, [+tCommunication]),
                     A,
                     'chit_chat-37.6_f6') :-
    nop(english("Susan chitchatted about matters of great import.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('classify-29.10')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'classify-29.10_f0') :-
    nop(english("The curator classified the artifacts.")),
    into_lf((group(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('classify-29.10')),
                     3,
                     [np(Theme)],
                     [prep(as), np(Goal, [-sentential])],
                     A,
                     'classify-29.10_f1') :-
    nop(english("She classified the works as 'dangerous'.")),
    into_lf((group(E, Theme, Goal), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('classify-29.10')),
                     3,
                     [np(Theme)],
                     [prep(in), np(Goal)],
                     A,
                     'classify-29.10_f2') :-
    nop(english("She classified the articles into different genres.")),
    into_lf((group(E, Theme, Goal), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('clear-10.3')),
                     1,
                     np(Location, [+tLocation]),
                     [],
                     A,
                     'clear-10.3_f0') :-
    nop(english("The strong winds cleared the sky.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Location), ~location(EndE, Theme, Location), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('clear-10.3')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [prep(Prep, [+src]), np(Location, [+tLocation])],
                     A,
                     'clear-10.3_f1') :-
    nop(english("Doug cleaned the dishes from the table.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Location), ~location(EndE, Theme, Location), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('clear-10.3')),
                     3,
                     [np(Location, [+tLocation])],
                     [prep(of), np(Theme, [+tConcrete])],
                     A,
                     'clear-10.3_f2') :-
    nop(english("Doug cleaned the table of dishes.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Location), ~location(EndE, Theme, Location), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Location, [+tLocation]),
                     verb(vn('clear-10.3-1')),
                     0,
                     [],
                     [],
                     A,
                     'clear-10.3-1_f0') :-
    nop(english("The sky cleared.")),
    into_lf((location(StartE, Theme, Location), ~location(EndE, Theme, Location), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('clear-10.3-1')),
                     2,
                     prep(Prep, [+src]),
                     np(Location, [+tLocation]),
                     A,
                     'clear-10.3-1_f1') :-
    nop(english("Clouds cleared from the sky.")),
    into_lf((location(StartE, Theme, Location), ~location(EndE, Theme, Location), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('cling-22.5')),
                     2,
                     prep((to;with)),
                     np(Co_Theme, [+tConcrete]),
                     A,
                     'cling-22.5_f0') :-
    nop(english("The child clung to her mother.")),
    into_lf((together(EndE, Physical, Theme, Co_Theme), isa(Theme, tConcrete), isa(Co_Theme, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete, +plural]),
                     verb(vn('cling-22.5')),
                     1,
                     lex(together),
                     [],
                     A,
                     'cling-22.5_f1') :-
    nop(english("The child and her mother clung together.")),
    into_lf((together(EndE, Physical, Theme_i, Theme_j), isa(Theme, tConcrete), isa(Co_Theme, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('coil-9.6')),
                     3,
                     [np(Theme, [+tElongated, +tNonrigid])],
                     [ prep(Prep, [+path]),
                       np(Location, [+tConcrete])
                     ],
                     A,
                     'coil-9.6_f0') :-
    nop(english("Cora coiled the rope around the post.")),
    into_lf((rotational_motion(E, Theme), ~holds(Prep, StartE, Theme, Location), holds(Prep, E, Theme, Location), cause(Agent, E), isa(Agent, int_control), isa(Theme, tNonrigid), isa(Theme, tElongated), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Theme, [+tElongated, +tNonrigid]),
                     verb(vn('coil-9.6')),
                     2,
                     prep(Prep, [+path]),
                     np(Location, [+tConcrete]),
                     A,
                     'coil-9.6_f1') :-
    nop(english("The rope coiled around the post.")),
    into_lf((rotational_motion(E, Theme), ~holds(Prep, StartE, Theme, Location), holds(Prep, E, Theme, Location), isa(Agent, int_control), isa(Theme, tNonrigid), isa(Theme, tElongated), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Theme, [+tElongated, +tNonrigid]),
                     verb(vn('coil-9.6')),
                     3,
                     [],
                     [ 'ADV',
                       prep(Prep, [+path]),
                       np(Location, [+tConcrete])
                     ],
                     A,
                     'coil-9.6_f2') :-
    nop(english("That type of rope coils easily around the post.")),
    into_lf((property(Theme, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Theme, tNonrigid), isa(Theme, tElongated), isa(Location, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Theme, [+tElongated, +tNonrigid]),
                     verb(vn('coil-9.6-1')),
                     1,
                     'ADV',
                     [],
                     A,
                     'coil-9.6-1_f0') :-
    nop(english("The rope curled upward.")),
    into_lf((rotational_motion(E, Theme), holds(Adv, Prop), property(Theme, Prop), isa(Agent, int_control), isa(Theme, tNonrigid), isa(Theme, tElongated), isa(Location, tConcrete), isa(Adv, '$ADV'), isa(E, actEvent), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('coloring-24')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'coloring-24_f0') :-
    nop(english("Claire colors the picture.")),
    into_lf((cause(Agent, E), apply_material(E, Material, Patient), covered(ResultE, Material, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(E, actEvent), isa(Material, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('coloring-24')),
                     2,
                     np(Patient, [+tConcrete]),
                     np(Result),
                     A,
                     'coloring-24_f1') :-
    nop(english("Claire colors the picture red.")),
    into_lf((cause(Agent, E), apply_material(E, Material, Patient), covered(ResultE, Material, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Material, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('coloring-24')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'coloring-24_f2') :-
    nop(english("Claire painted the wall into a splotchy mess.")),
    into_lf((cause(Agent, E), apply_material(E, Material, Patient), covered(ResultE, Material, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Material, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complain-37.8')),
                     0,
                     [],
                     [],
                     A,
                     'complain-37.8_f0') :-
    nop(english("Susan complained.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complain-37.8')),
                     2,
                     prep(to),
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     A,
                     'complain-37.8_f1') :-
    nop(english("Susan complained to Rachel.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complain-37.8')),
                     1,
                     np(Topic, [+tCommunication, +that_comp]),
                     [],
                     A,
                     'complain-37.8_f2') :-
    nop(english("Susan complained that the party would be tonight.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complain-37.8')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +that_comp])
                     ],
                     A,
                     'complain-37.8_f3') :-
    nop(english("Susan complained to Rachel that the party would be tonight.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complain-37.8')),
                     2,
                     prep(about),
                     np(Topic, [+tCommunication]),
                     A,
                     'complain-37.8_f4') :-
    nop(english("Ellen complained about the situation.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complain-37.8')),
                     4,
                     [],
                     [ prep(about),
                       np(Topic, [+tCommunication]),
                       prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'complain-37.8_f5') :-
    nop(english("Ellen complained about the situation to Helen.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complain-37.8')),
                     1,
                     np(Topic, [+tCommunication, +quotation]),
                     [],
                     A,
                     'complain-37.8_f6') :-
    nop(english("Ellen complained, 'The mail didn't come today.'")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complain-37.8')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +quotation])
                     ],
                     A,
                     'complain-37.8_f7') :-
    nop(english("Ellen complained to Helen, 'The mail didn't come today.'")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complain-37.8')),
                     4,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+tCommunication])
                     ],
                     A,
                     'complain-37.8_f8') :-
    nop(english("The prime minister complained to the former president about U.S. interference in his country's affairs.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complete-55.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'complete-55.2_f0') :-
    nop(english("Wilma completed the assignment.")),
    into_lf((end(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('complete-55.2')),
                     1,
                     np(Theme, [+be_sc_ing]),
                     [],
                     A,
                     'complete-55.2_f1') :-
    nop(english("She quit smoking.")),
    into_lf((end(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('comprehend-87.2')),
                     1,
                     np(Stimulus, [+how_extract]),
                     [],
                     A,
                     'comprehend-87.2_f0') :-
    nop(english("I understand how she did it.")),
    into_lf((understand(E, Experiencer, Stimulus, Attribute), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('comprehend-87.2')),
                     1,
                     np(Stimulus),
                     [],
                     A,
                     'comprehend-87.2_f1') :-
    nop(english("I understand the problem.")),
    into_lf((understand(E, Experiencer, Stimulus, Attribute), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('comprehend-87.2')),
                     3,
                     [np(Stimulus)],
                     [prep(as), np(Attribute)],
                     A,
                     'comprehend-87.2_f2') :-
    nop(english("I understood it as a joke.")),
    into_lf((understand(E, Experiencer, Stimulus, Attribute), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('comprehend-87.2')),
                     1,
                     np(Stimulus, [+poss_ing]),
                     [],
                     A,
                     'comprehend-87.2_f3') :-
    nop(english("I understand their wanting more minutes.")),
    into_lf((understand(E, Experiencer, Stimulus, Attribute), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('comprehend-87.2')),
                     1,
                     np(Stimulus, [+wh_comp]),
                     [],
                     A,
                     'comprehend-87.2_f4') :-
    nop(english("I understand why we should help them.")),
    into_lf((understand(E, Experiencer, Stimulus, Attribute), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('comprehend-87.2')),
                     1,
                     np(Stimulus, [+what_extract]),
                     [],
                     A,
                     'comprehend-87.2_f5') :-
    nop(english("I understand what we should do.")),
    into_lf((understand(E, Experiencer, Stimulus, Attribute), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('comprehend-87.2')),
                     1,
                     np(Stimulus, [+that_comp]),
                     [],
                     A,
                     'comprehend-87.2_f6') :-
    nop(english("I understand that he is a good doctor.")),
    into_lf((understand(E, Experiencer, Stimulus, Attribute), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('comprehend-87.2-1')),
                     1,
                     np(Stimulus, [+ac_ing]),
                     [],
                     A,
                     'comprehend-87.2-1_f0') :-
    nop(english("I understand wanting more.")),
    into_lf((understand(E, Experiencer, Stimulus, Attribute), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('comprehend-87.2-1')),
                     1,
                     np(Stimulus, [+wh_inf]),
                     [],
                     A,
                     'comprehend-87.2-1_f1') :-
    nop(english("I understand how to do it.")),
    into_lf((understand(E, Experiencer, Stimulus, Attribute), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('comprehend-87.2-1')),
                     1,
                     np(Stimulus, [+to_be]),
                     [],
                     A,
                     'comprehend-87.2-1_f2') :-
    nop(english("I understand him to be a good doctor.")),
    into_lf((understand(E, Experiencer, Stimulus, Attribute), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('concealment-16')),
                     3,
                     [np(Patient)],
                     [prep(from), np(Beneficiary, [+tAnimate])],
                     A,
                     'concealment-16_f0') :-
    nop(english("Frances hid the presents from Sally.")),
    into_lf((cause(Agent, E), visible(StartE, Patient), ~visible(EndE, Patient), holds(Prep, E, Patient, Beneficiary), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('concealment-16')),
                     3,
                     [np(Patient)],
                     [prep(Prep, [+loc]), np(Location)],
                     A,
                     'concealment-16_f1') :-
    nop(english("Frances hid the presents behind the books in the drawer.")),
    into_lf((cause(Agent, E), visible(StartE, Patient), ~visible(EndE, Patient), location(ResultE, Patient, Location), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('concealment-16')),
                     1,
                     np(Patient),
                     [],
                     A,
                     'concealment-16_f2') :-
    nop(english("Frances hid the presents.")),
    into_lf((visible(StartE, Patient), ~visible(EndE, Patient), cause(Agent, E), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('concealment-16-1')),
                     0,
                     [],
                     [],
                     A,
                     'concealment-16-1_f0') :-
    nop(english("The children hid.")),
    into_lf((visible(StartE, Agent), ~visible(EndE, Agent), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('concealment-16-1')),
                     2,
                     prep(from),
                     np(Beneficiary, [+tAnimate]),
                     A,
                     'concealment-16-1_f1') :-
    nop(english("The children hid from Sally.")),
    into_lf((visible(StartE, Agent), ~visible(EndE, Agent), holds(Prep, E, Agent, Beneficiary), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('concealment-16-1')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location),
                     A,
                     'concealment-16-1_f2') :-
    nop(english("The children hid in the chimney.")),
    into_lf((visible(StartE, Agent), ~visible(EndE, Agent), location(E, Agent, Location), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confess-37.10')),
                     1,
                     np(Topic, [+how_extract]),
                     [],
                     A,
                     'confess-37.10_f0') :-
    nop(english("He confessed how she did it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confess-37.10')),
                     1,
                     np(Topic, [+to_be]),
                     [],
                     A,
                     'confess-37.10_f1') :-
    nop(english("I admitted him to be a great smoker.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confess-37.10')),
                     1,
                     np(Topic, [+np_ppart]),
                     [],
                     A,
                     'confess-37.10_f2') :-
    nop(english("He confessed himself flummoxed.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confess-37.10')),
                     1,
                     np(Topic, [+be_sc_ing]),
                     [],
                     A,
                     'confess-37.10_f3') :-
    nop(english("They confessed stealing the money.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confess-37.10')),
                     1,
                     np(Topic, [-sentential]),
                     [],
                     A,
                     'confess-37.10_f4') :-
    nop(english("They confessed it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confess-37.10')),
                     3,
                     [np(Topic, [-sentential])],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'confess-37.10_f5') :-
    nop(english("They confessed it to us.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confess-37.10')),
                     1,
                     np(Topic, [+poss_ing]),
                     [],
                     A,
                     'confess-37.10_f6') :-
    nop(english("They confessed their stealing.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confess-37.10')),
                     2,
                     prep(to),
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     A,
                     'confess-37.10_f7') :-
    nop(english("They confessed to us.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confess-37.10')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+how_extract])
                     ],
                     A,
                     'confess-37.10_f8') :-
    nop(english("They confessed to us how it happened.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confess-37.10')),
                     1,
                     np(Topic, [+that_comp]),
                     [],
                     A,
                     'confess-37.10_f9') :-
    nop(english("They confessed that they had stolen.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('confine-92')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'confine-92_f0') :-
    nop(english("We committed John.")),
    into_lf((~location(StartE, Theme, Destination), location(EndE, Theme, Destination), confined(ResultE, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(Destination, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('confine-92-1')),
                     3,
                     [np(Theme)],
                     [prep(to), np(Destination, [+tLocation])],
                     A,
                     'confine-92-1_f0') :-
    nop(english("We committed John to prison.")),
    into_lf((~location(StartE, Theme, Destination), location(EndE, Theme, Destination), confined(ResultE, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(Destination, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confront-98')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'confront-98_f0') :-
    nop(english("John confronted the disaster.")),
    into_lf((confront(E, Agent, Theme, Instrument), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('confront-98')),
                     3,
                     [np(Theme)],
                     [prep(with), np(Instrument, [-sentential])],
                     A,
                     'confront-98_f1') :-
    nop(english("John confronted it with emergency measures.")),
    into_lf((confront(E, Agent, Theme, Instrument), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('conjecture-29.5-1')),
                     4,
                     [np(Theme)],
                     [lex(to), lex(be), np(Predicate)],
                     A,
                     'conjecture-29.5-1_f0') :-
    nop(english("He felt the choice to be a bad one.")),
    into_lf((believe(E, Agent, Theme, Predicate), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('conjecture-29.5-1')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'conjecture-29.5-1_f1') :-
    nop(english("The populace feel that the RIAA has too much power.")),
    into_lf((believe(E, Agent, Theme, Predicate), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('conjecture-29.5-1')),
                     1,
                     np(Theme, [-sentential]),
                     [],
                     A,
                     'conjecture-29.5-1_f2') :-
    nop(english("He denied the horrid woman's statement.")),
    into_lf((believe(E, Agent, Theme, Predicate), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('conjecture-29.5-2')),
                     2,
                     np(Theme),
                     np(Predicate),
                     A,
                     'conjecture-29.5-2_f0') :-
    nop(english("He asserts himself to be the world's foremost authority.")),
    into_lf((declare(E, Agent, Theme, Predicate), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('conjecture-29.5-2')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'conjecture-29.5-2_f1') :-
    nop(english("The prisoner admitted that he may have been in the area.")),
    into_lf((declare(E, Agent, Theme, Predicate), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('conjecture-29.5-2')),
                     1,
                     np(Theme, [-sentential]),
                     [],
                     A,
                     'conjecture-29.5-2_f2') :-
    nop(english("The prisoner admitted his guilt.")),
    into_lf((declare(E, Agent, Theme, Predicate), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9')),
                     1,
                     np(Theme, [+how_extract]),
                     [],
                     A,
                     'consider-29.9_f0') :-
    nop(english("I considered how he could become professor.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9')),
                     1,
                     np(Theme, [+wh_inf]),
                     [],
                     A,
                     'consider-29.9_f1') :-
    nop(english("I considered how to be a professor.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1')),
                     1,
                     np(Theme, [+np_p_ing]),
                     [],
                     A,
                     'consider-29.9-1_f0') :-
    nop(english("They considered him as being stupid.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1')),
                     1,
                     np(Theme, [+to_be]),
                     [],
                     A,
                     'consider-29.9-1_f1') :-
    nop(english("They considered him to be the professor.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'consider-29.9-1_f2') :-
    nop(english("They considered that he was the professor.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1-1')),
                     2,
                     np(Theme, [+np_ppart]),
                     np(Attribute, [-sentential]),
                     A,
                     'consider-29.9-1-1_f0') :-
    nop(english("I considered the matter closed.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1-1')),
                     1,
                     np(Theme, [+wh_comp]),
                     [],
                     A,
                     'consider-29.9-1-1_f1') :-
    nop(english("He considered whether he should come.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1-1')),
                     1,
                     np(Theme, [+what_extract]),
                     [],
                     A,
                     'consider-29.9-1-1_f2') :-
    nop(english("He considered what he should do.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1-1')),
                     1,
                     np(Theme, [+what_inf]),
                     [],
                     A,
                     'consider-29.9-1-1_f3') :-
    nop(english("He considered what to do.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1-1-1')),
                     1,
                     np(Theme, [+be_sc_ing]),
                     [],
                     A,
                     'consider-29.9-1-1-1_f0') :-
    nop(english("He considered smoking.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1-1-1')),
                     2,
                     np(Theme),
                     np(Attribute, [-sentential]),
                     A,
                     'consider-29.9-1-1-1_f1') :-
    nop(english("They considered him stupid.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1-1-1')),
                     2,
                     np(Theme),
                     np(Attribute, [-sentential]),
                     A,
                     'consider-29.9-1-1-1_f2') :-
    nop(english("They considered him professor.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1-1-1')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Attribute, [-sentential])],
                     A,
                     'consider-29.9-1-1-1_f3') :-
    nop(english("They considered him for professor.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-1-1-1')),
                     1,
                     np(Theme, [+wheth_inf]),
                     [],
                     A,
                     'consider-29.9-1-1-1_f4') :-
    nop(english("He considered whether to clean the house.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-2')),
                     2,
                     np(Theme),
                     np(Attribute, [-sentential]),
                     A,
                     'consider-29.9-2_f0') :-
    nop(english("They considered him stupid.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-2')),
                     2,
                     np(Theme),
                     np(Attribute, [-sentential]),
                     A,
                     'consider-29.9-2_f1') :-
    nop(english("They considered him professor.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-2')),
                     1,
                     np(Theme, [+to_be]),
                     [],
                     A,
                     'consider-29.9-2_f2') :-
    nop(english("They considered him to be the professor.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-2')),
                     2,
                     np(Theme, [+np_ppart]),
                     np(Attribute, [-sentential]),
                     A,
                     'consider-29.9-2_f3') :-
    nop(english("They considered the children found.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consider-29.9-2')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'consider-29.9-2_f4') :-
    nop(english("They considered that he was the professor.")),
    into_lf((consider(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('conspire-71')),
                     0,
                     [],
                     [],
                     A,
                     'conspire-71_f0') :-
    nop(english("John and Sarah conspired.")),
    into_lf((conspire(E, Agent_i, Agent_j, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('conspire-71')),
                     2,
                     prep(with),
                     np(Co_Agent,
                        [ or(isa(Co_Agent, tAnimate),
                             isa(Co_Agent, tOrganization))
                        ]),
                     A,
                     'conspire-71_f1') :-
    nop(english("John conspired with Sarah.")),
    into_lf((conspire(E, Agent, Co_Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('conspire-71')),
                     2,
                     prep(against),
                     np(Beneficiary),
                     A,
                     'conspire-71_f2') :-
    nop(english("John conspired against her.")),
    into_lf((conspire(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('conspire-71')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'conspire-71_f3') :-
    nop(english("The US conspired to overthrow the UN.")),
    into_lf((conspire(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consume-66')),
                     1,
                     np(Asset),
                     [],
                     A,
                     'consume-66_f0') :-
    nop(english("I spent all that time.")),
    into_lf((exist(StartE, Asset), ~exist(EndE, Asset), spend(E, Agent, Asset, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consume-66')),
                     2,
                     np(Asset),
                     np(Goal, [+sc_ing]),
                     A,
                     'consume-66_f1') :-
    nop(english("I spent the time worrying about the future.")),
    into_lf((exist(StartE, Asset), ~exist(EndE, Asset), spend(E, Agent, Asset, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consume-66-1')),
                     3,
                     [np(Asset)],
                     [prep(on), np(Goal, [+sc_ing])],
                     A,
                     'consume-66-1_f0') :-
    nop(english("I spent the time on worrying about the future.")),
    into_lf((exist(StartE, Asset), ~exist(EndE, Asset), spend(E, Agent, Asset, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('consume-66-1')),
                     3,
                     [np(Asset)],
                     [prep(on), np(Goal, [-sentential])],
                     A,
                     'consume-66-1_f1') :-
    nop(english("I spent the time on worries.")),
    into_lf((exist(StartE, Asset), ~exist(EndE, Asset), spend(E, Agent, Asset, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('contiguous_location-47.8')),
                     1,
                     np(Co_Theme, [+tConcrete]),
                     [],
                     A,
                     'contiguous_location-47.8_f0') :-
    nop(english("Italy borders France.")),
    into_lf((contact(E, Theme, Co_Theme), exist(E, Theme), exist(E, Co_Theme), isa(Theme, tConcrete), isa(Co_Theme, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     lex(and),
                     2,
                     np(Co_Theme, [+tConcrete]),
                     verb(vn('contiguous_location-47.8-1')),
                     A,
                     'contiguous_location-47.8-1_f0') :-
    nop(english("Italy and France touch.")),
    into_lf((contact(E, Theme, Co_Theme), exist(E, Theme), exist(E, Co_Theme), isa(Theme, tConcrete), isa(Co_Theme, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('contiguous_location-47.8-2')),
                     0,
                     [],
                     [],
                     A,
                     'contiguous_location-47.8-2_f0') :-
    nop(english("DEC has often dominated.")),
    into_lf((contact(E, Theme, Co_Theme), exist(E, Theme), exist(E, Co_Theme), isa(Theme, tConcrete), isa(Co_Theme, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('continue-55.3')),
                     1,
                     np(Theme, [+to_inf_rs]),
                     [],
                     A,
                     'continue-55.3_f0') :-
    nop(english("He continued to pack.")),
    into_lf((continue(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Time, tTime), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('continue-55.3')),
                     0,
                     [],
                     [],
                     A,
                     'continue-55.3_f1') :-
    nop(english("The storm continued.")),
    into_lf((continue(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Time, tTime), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('continue-55.3')),
                     0,
                     [],
                     [],
                     A,
                     'continue-55.3_f2') :-
    nop(english("He continued.")),
    into_lf((continue(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Time, tTime), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('continue-55.3')),
                     2,
                     prep(until),
                     np(Time, [+tTime]),
                     A,
                     'continue-55.3_f3') :-
    nop(english("The party continued until 8 PM.")),
    into_lf((continue(E, Theme), holds(Prep, E, Theme, Time), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Time, tTime), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('continue-55.3')),
                     2,
                     prep(until),
                     np(Time, [+tTime]),
                     A,
                     'continue-55.3_f4') :-
    nop(english("We continued until 8 PM.")),
    into_lf((continue(E, Theme), cause(Agent, E), holds(Prep, E, Theme, Time), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Time, tTime), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('contribute-13.2')),
                     3,
                     [np(Theme)],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'contribute-13.2_f0') :-
    nop(english("We contributed our paycheck to her.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('contribute-13.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'contribute-13.2_f1') :-
    nop(english("I donated my house (to somebody).")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('contribute-13.2')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Theme)
                     ],
                     A,
                     'contribute-13.2_f2') :-
    nop(english("He is willing to sacrifice to the arbitrage trader some small profit.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('contribute-13.2-1')),
                     2,
                     prep(to),
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     A,
                     'contribute-13.2-1_f0') :-
    nop(english("He donated directly to JSP members.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('contribute-13.2-1')),
                     3,
                     [np(Theme)],
                     [ prep(among),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization)),
                            +plural
                          ])
                     ],
                     A,
                     'contribute-13.2-1_f1') :-
    nop(english("She distributed the money among oversea units.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('contribute-13.2-1-1')),
                     0,
                     [],
                     [],
                     A,
                     'contribute-13.2-1-1_f0') :-
    nop(english("Some of the members may donate privately.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('contribute-13.2-2')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Theme),
                     A,
                     'contribute-13.2-2_f0') :-
    nop(english("They will reimburse them up to $500.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('contribute-13.2-2-1')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'contribute-13.2-2-1_f0') :-
    nop(english("He reimbursed members.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Patient, [+tAnimate]),
                     verb(vn('convert-26.6.2')),
                     2,
                     prep(to),
                     np(Goal, [+sc_ing]),
                     A,
                     'convert-26.6.2_f0') :-
    nop(english("He converted to believing in Buddha.")),
    into_lf((convert(E, Patient, Source, Goal), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient, [+tAnimate]),
                     verb(vn('convert-26.6.2')),
                     2,
                     prep(to),
                     np(Goal),
                     A,
                     'convert-26.6.2_f1') :-
    nop(english("He converted to Buddhism.")),
    into_lf((convert(E, Patient, Source, Goal), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient, [+tAnimate]),
                     verb(vn('convert-26.6.2-1')),
                     0,
                     [],
                     [],
                     A,
                     'convert-26.6.2-1_f0') :-
    nop(english("He converted.")),
    into_lf((convert(E, Patient, Source, Goal), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('convert-26.6.2-1')),
                     1,
                     np(Patient, [+tAnimate]),
                     [],
                     A,
                     'convert-26.6.2-1_f1') :-
    nop(english("I converted him.")),
    into_lf((convert(E, Patient, Source, Goal), cause(E, Agent), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('convert-26.6.2-1')),
                     3,
                     [np(Patient, [+tAnimate])],
                     [prep(to), np(Goal, [+oc_ing])],
                     A,
                     'convert-26.6.2-1_f2') :-
    nop(english("I converted him to believing in Buddha.")),
    into_lf((convert(E, Patient, Source, Goal), cause(E, Agent), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('convert-26.6.2-1')),
                     3,
                     [np(Patient, [+tAnimate])],
                     [prep(to), np(Goal)],
                     A,
                     'convert-26.6.2-1_f3') :-
    nop(english("I converted him to Buddhism.")),
    into_lf((convert(E, Patient, Source, Goal), cause(E, Agent), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient, [+tAnimate]),
                     verb(vn('convert-26.6.2-1')),
                     4,
                     [],
                     [prep(from), np(Source), prep(to), np(Goal)],
                     A,
                     'convert-26.6.2-1_f4') :-
    nop(english("He converted from Christianity to Buddhism.")),
    into_lf((convert(E, Patient, Source, Goal), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('cooking-45.3')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'cooking-45.3_f0') :-
    nop(english("Jennifer baked the potatoes.")),
    into_lf((cause(Agent, E), apply_heat(E, Instrument, Patient), cooked(ResultE, Form, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('cooking-45.3')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((in;on;with)), np(Instrument, [+tSolid])],
                     A,
                     'cooking-45.3_f1') :-
    nop(english("Jennifer baked the potatoes in the oven.")),
    into_lf((cause(Agent, E), apply_heat(E, Instrument, Patient), cooked(ResultE, Form, Patient), use(E, Agent, Instrument), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('cooking-45.3')),
                     0,
                     [],
                     [],
                     A,
                     'cooking-45.3_f2') :-
    nop(english("The potatoes baked.")),
    into_lf((cooked(ResultE, Form, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('cooking-45.3')),
                     1,
                     'ADV',
                     [],
                     A,
                     'cooking-45.3_f3') :-
    nop(english("Idaho potatoes bake beautifully.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Instrument, [+tSolid]),
                     verb(vn('cooking-45.3')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'cooking-45.3_f4') :-
    nop(english("This oven bakes potatoes.")),
    into_lf((apply_heat(E, Instrument, Patient), cooked(ResultE, Form, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('cooking-45.3')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'cooking-45.3_f5') :-
    nop(english("Jennifer baked the potatoes to a crisp.")),
    into_lf((cause(Agent, E), apply_heat(E, Instrument, Patient), cooked(ResultE, Form, Patient), holds(Pred, ResultE, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('cooking-45.3')),
                     5,
                     [np(Patient, [+tConcrete])],
                     [ prep((to;into)),
                       np(Result, [+state]),
                       prep((in;on;with)),
                       np(Instrument, [+tSolid])
                     ],
                     A,
                     'cooking-45.3_f6') :-
    nop(english("Jennifer baked the potatoes to a crisp in the oven.")),
    into_lf((cause(Agent, E), apply_heat(E, Instrument, Patient), cooked(ResultE, Form, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('cooking-45.3')),
                     2,
                     prep((to;into)),
                     np(Result, [+state]),
                     A,
                     'cooking-45.3_f7') :-
    nop(english("Potatoes bake to a crisp.")),
    into_lf((apply_heat(E, Instrument, Patient), cooked(ResultE, Form, Patient), holds(Pred, ResultE, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('cooking-45.3')),
                     4,
                     [],
                     [ prep((to;into)),
                       np(Result, [+state]),
                       prep((in;on;with)),
                       np(Instrument, [+tSolid])
                     ],
                     A,
                     'cooking-45.3_f8') :-
    nop(english("Potatoes bake to a crisp in the oven.")),
    into_lf((apply_heat(E, Instrument, Patient), cooked(ResultE, Form, Patient), holds(Pred, ResultE, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Form, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('cooperate-73-1')),
                     2,
                     prep(on),
                     np(Theme, [-sentential]),
                     A,
                     'cooperate-73-1_f0') :-
    nop(english("They collaborated on the task.")),
    into_lf((cooperate(E, Agent_i, Agent_j, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('cooperate-73-1')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(on),
                       np(Theme, [-sentential])
                     ],
                     A,
                     'cooperate-73-1_f1') :-
    nop(english("John collaborated with Paul on the task.")),
    into_lf((cooperate(E, Agent, Co_Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('cooperate-73-1')),
                     2,
                     prep(in),
                     np(Theme, [-sentential]),
                     A,
                     'cooperate-73-1_f2') :-
    nop(english("They collaborated in the task.")),
    into_lf((cooperate(E, Agent_i, Agent_j, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('cooperate-73-1')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(in),
                       np(Theme, [-sentential])
                     ],
                     A,
                     'cooperate-73-1_f3') :-
    nop(english("John collaborated with Paul in the task.")),
    into_lf((cooperate(E, Agent, Co_Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('cooperate-73-1')),
                     2,
                     prep(in),
                     np(Theme, [+sc_ing]),
                     A,
                     'cooperate-73-1_f4') :-
    nop(english("They collaborated in finishing the task.")),
    into_lf((cooperate(E, Agent_i, Agent_j, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('cooperate-73-2')),
                     2,
                     prep(in),
                     np(Theme, [-sentential]),
                     A,
                     'cooperate-73-2_f0') :-
    nop(english("They participated in the task.")),
    into_lf((cooperate(E, Agent_i, Agent_j, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('cooperate-73-2')),
                     2,
                     prep(in),
                     np(Theme, [+sc_ing]),
                     A,
                     'cooperate-73-2_f1') :-
    nop(english("They participated in finishing the task.")),
    into_lf((cooperate(E, Agent_i, Agent_j, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('cooperate-73-3')),
                     2,
                     prep(on),
                     np(Theme, [-sentential]),
                     A,
                     'cooperate-73-3_f0') :-
    nop(english("They worked on the task.")),
    into_lf((cooperate(E, Agent_i, Agent_j, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('cooperate-73-3')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(on),
                       np(Theme, [-sentential])
                     ],
                     A,
                     'cooperate-73-3_f1') :-
    nop(english("John worked with Paul on the task.")),
    into_lf((cooperate(E, Agent, Co_Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('cooperate-73-3')),
                     2,
                     prep(at),
                     np(Theme, [+sc_ing]),
                     A,
                     'cooperate-73-3_f2') :-
    nop(english("They worked at finishing the task.")),
    into_lf((cooperate(E, Agent_i, Agent_j, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('cooperate-73-3')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(at),
                       np(Theme, [+sc_ing])
                     ],
                     A,
                     'cooperate-73-3_f3') :-
    nop(english("John worked with Paul at finishing the task.")),
    into_lf((cooperate(E, Agent, Co_Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('cope-83')),
                     2,
                     prep(with),
                     np(Theme, [+sc_ing]),
                     A,
                     'cope-83_f0') :-
    nop(english("He managed with dealing the cards.")),
    into_lf((cope(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('cope-83-1')),
                     0,
                     [],
                     [],
                     A,
                     'cope-83-1_f0') :-
    nop(english("He managed.")),
    into_lf((cope(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('cope-83-1-1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'cope-83-1-1_f0') :-
    nop(english("He managed the climb.")),
    into_lf((cope(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('correlate-86.1')),
                     2,
                     prep(with),
                     np(Co_Theme, [+ac_ing]),
                     A,
                     'correlate-86.1_f0') :-
    nop(english("He alternates with keeping them.")),
    into_lf((correlate(E, Theme, Co_Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('correlate-86.1')),
                     3,
                     [np(Theme)],
                     [prep(with), np(Co_Theme, [+ac_ing])],
                     A,
                     'correlate-86.1_f1') :-
    nop(english("We alternated it with keeping them locked up.")),
    into_lf((cause(E, Agent), correlate(E, Theme, Co_Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1')),
                     0,
                     [],
                     [],
                     A,
                     'correspond-36.1_f0') :-
    nop(english("They agreed.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1')),
                     2,
                     prep(about),
                     np(Theme, [+poss_ing]),
                     A,
                     'correspond-36.1_f1') :-
    nop(english("They agreed about his coming.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1')),
                     2,
                     prep(about),
                     np(Theme, [+wh_comp]),
                     A,
                     'correspond-36.1_f2') :-
    nop(english("They agreed about whether he should go.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1')),
                     2,
                     prep(about),
                     np(Theme, [+what_extract]),
                     A,
                     'correspond-36.1_f3') :-
    nop(english("They agreed about what should be done.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1')),
                     2,
                     prep(about),
                     np(Theme, [+wheth_inf]),
                     A,
                     'correspond-36.1_f4') :-
    nop(english("They agreed about whether to go.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1')),
                     2,
                     prep(about),
                     np(Theme, [+what_inf]),
                     A,
                     'correspond-36.1_f5') :-
    nop(english("They agreed about what to do.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1')),
                     2,
                     prep(about),
                     np(Theme, [-sentential]),
                     A,
                     'correspond-36.1_f6') :-
    nop(english("They agreed about it.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('correspond-36.1')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Theme, [+wh_comp])
                     ],
                     A,
                     'correspond-36.1_f7') :-
    nop(english("They agreed with him about whether he should kill the peasants.")),
    into_lf((social_interaction(E, Agent, Co_Agent), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('correspond-36.1')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Theme, [+what_extract])
                     ],
                     A,
                     'correspond-36.1_f8') :-
    nop(english("They agreed with him about what he should do.")),
    into_lf((social_interaction(E, Agent, Co_Agent), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('correspond-36.1')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Theme, [+what_inf])
                     ],
                     A,
                     'correspond-36.1_f9') :-
    nop(english("They agreed with him about what to do.")),
    into_lf((social_interaction(E, Agent, Co_Agent), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('correspond-36.1')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Theme, [+wheth_inf])
                     ],
                     A,
                     'correspond-36.1_f10') :-
    nop(english("They agreed with him about whether to go.")),
    into_lf((social_interaction(E, Agent, Co_Agent), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('correspond-36.1')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Theme, [-sentential])
                     ],
                     A,
                     'correspond-36.1_f11') :-
    nop(english("They agreed with him about it.")),
    into_lf((social_interaction(E, Agent, Co_Agent), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1')),
                     1,
                     np(Theme, [+what_extract]),
                     [],
                     A,
                     'correspond-36.1_f12') :-
    nop(english("We agreed what he should do.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('correspond-36.1-1')),
                     3,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       np(Theme, [+how_extract])
                     ],
                     A,
                     'correspond-36.1-1_f0') :-
    nop(english("They agreed with her how it should be done.")),
    into_lf((social_interaction(E, Agent, Co_Agent), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('correspond-36.1-1')),
                     3,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       np(Theme, [+wh_inf])
                     ],
                     A,
                     'correspond-36.1-1_f1') :-
    nop(english("They agreed with her how to do it.")),
    into_lf((social_interaction(E, Agent, Co_Agent), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1-1')),
                     1,
                     np(Theme, [+what_inf]),
                     [],
                     A,
                     'correspond-36.1-1_f2') :-
    nop(english("We agreed what to do.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1-1-1')),
                     1,
                     np(Theme, [+wh_comp]),
                     [],
                     A,
                     'correspond-36.1-1-1_f0') :-
    nop(english("We agreed whether he should come.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('correspond-36.1-1-1')),
                     1,
                     np(Theme, [+wheth_inf]),
                     [],
                     A,
                     'correspond-36.1-1-1_f1') :-
    nop(english("We debated whether to clean the house.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), about(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('cost-54.2')),
                     1,
                     np(Value),
                     [],
                     A,
                     'cost-54.2_f0') :-
    nop(english("The book costs $10.")),
    into_lf((value(E, Theme, Value), isa(Theme, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('cost-54.2')),
                     2,
                     np(Beneficiary,
                        [ or(isa(Beneficiary, tAnimate),
                             isa(Beneficiary, tOrganization))
                        ]),
                     np(Value),
                     A,
                     'cost-54.2_f1') :-
    nop(english("The bill will cost them 500 million dollars.")),
    into_lf((value(E, Theme, Value), benefit(E, Beneficiary), isa(Theme, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('crane-40.3.2')),
                     1,
                     np(Patient, [+tBodyPart]),
                     [],
                     A,
                     'crane-40.3.2_f0') :-
    nop(english("Jennifer craned her neck.")),
    into_lf((cause(Agent, E), transfer_info(E, Agent, Recipient, Theme), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(Theme, tCommunication), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('crane-40.3.2')),
                     3,
                     [np(Patient, [+tBodyPart])],
                     [ prep(Prep, [+dest_dir]),
                       np(Recipient, [+tAnimate])
                     ],
                     A,
                     'crane-40.3.2_f1') :-
    nop(english("Jennifer wagged her finger at the naughty child.")),
    into_lf((cause(Agent, E), transfer_info(E, Agent, Recipient, Theme), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(Theme, tCommunication), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('crane-40.3.2')),
                     5,
                     [np(Patient, [+tBodyPart])],
                     [ prep(Prep, [+dest_dir]),
                       np(Recipient, [+tAnimate]),
                       prep(in),
                       np(Theme, [+tCommunication])
                     ],
                     A,
                     'crane-40.3.2_f2') :-
    nop(english("Jennifer wagged her finger at the naughty child in disapproval.")),
    into_lf((cause(Agent, E), transfer_info(E, Agent, Recipient, Theme), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(Theme, tCommunication), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('create-26.4')),
                     1,
                     np(Result),
                     [],
                     A,
                     'create-26.4_f0') :-
    nop(english("David constructed a house.")),
    into_lf((~exist(StartE, Result), exist(ResultE, Result), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('create-26.4')),
                     3,
                     [np(Result)],
                     [prep((from;out;of)), np(Material)],
                     A,
                     'create-26.4_f1') :-
    nop(english("David constructed a house out of sticks.")),
    into_lf((~exist(StartE, Result), exist(ResultE, Result), made_of(ResultE, Result, Material), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('create-26.4')),
                     3,
                     [np(Result)],
                     [prep(for), np(Beneficiary, [+tAnimate])],
                     A,
                     'create-26.4_f2') :-
    nop(english("David dug a hole for me.")),
    into_lf((~exist(StartE, Result), exist(ResultE, Result), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('create-26.4')),
                     3,
                     [np(Result)],
                     [lex(as), np(Attribute)],
                     A,
                     'create-26.4_f3') :-
    nop(english("They designed the Westinghouse-Mitsubishi venture as a non-equity transaction.")),
    into_lf((~exist(StartE, Result), exist(ResultE, Result), cause(E, Agent), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('create-26.4-1')),
                     2,
                     np(Beneficiary, [+tAnimate]),
                     np(Result),
                     A,
                     'create-26.4-1_f0') :-
    nop(english("David dug me a hole.")),
    into_lf((~exist(StartE, Result), exist(ResultE, Result), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('curtsey-40.3.3')),
                     0,
                     [],
                     [],
                     A,
                     'curtsey-40.3.3_f0') :-
    nop(english("The princess curtseyed.")),
    into_lf((cause(Agent, E), transfer_info(E, Agent, Recipient, Topic), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('curtsey-40.3.3')),
                     2,
                     prep(Prep, [+dest_dir]),
                     np(Recipient, [+tAnimate]),
                     A,
                     'curtsey-40.3.3_f1') :-
    nop(english("The princess curtseyed to the queen.")),
    into_lf((cause(Agent, E), transfer_info(E, Agent, Recipient, Topic), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('curtsey-40.3.3')),
                     1,
                     np(Topic, [+tCommunication]),
                     [],
                     A,
                     'curtsey-40.3.3_f2') :-
    nop(english("The princess curtseyed her assent.")),
    into_lf((cause(Agent, E), transfer_info(E, Agent, Recipient, Topic), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('curtsey-40.3.3')),
                     3,
                     [np(Topic, [+tCommunication])],
                     [prep(to), np(Recipient, [+tAnimate])],
                     A,
                     'curtsey-40.3.3_f3') :-
    nop(english("The princess curtsied her assent to the queen.")),
    into_lf((cause(Agent, E), transfer_info(E, Agent, Recipient, Topic), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'cut-21.1_f0') :-
    nop(english("Carol cut the bread.")),
    into_lf((cause(Agent, E), manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(with), np(Instrument, [+tConcrete])],
                     A,
                     'cut-21.1_f1') :-
    nop(english("Carol cut the bread with a knife.")),
    into_lf((cause(Agent, E), manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1')),
                     2,
                     lex(at),
                     np(Patient, [+tConcrete]),
                     A,
                     'cut-21.1_f2') :-
    nop(english("Carol cut at the bread.")),
    into_lf((cause(Agent, E), manner(E, Motion, Agent), contact(E, Instrument, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Motion, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1')),
                     4,
                     [],
                     [ lex(at),
                       np(Patient, [+tConcrete]),
                       prep(with),
                       np(Instrument, [+tConcrete])
                     ],
                     A,
                     'cut-21.1_f3') :-
    nop(english("Carol cut at the bread with a knife.")),
    into_lf((cause(Agent, E), manner(E, Motion, Agent), contact(E, Instrument, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Motion, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('cut-21.1')),
                     1,
                     'ADV',
                     [],
                     A,
                     'cut-21.1_f4') :-
    nop(english("The bread cuts easily.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('cut-21.1')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'cut-21.1_f5') :-
    nop(english("The knife cut the bread.")),
    into_lf((contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('cut-21.1')),
                     1,
                     'ADV',
                     [],
                     A,
                     'cut-21.1_f6') :-
    nop(english("This knife cuts well.")),
    into_lf((property(Instrument, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1')),
                     2,
                     np(Patient, [+tConcrete]),
                     np(Result),
                     A,
                     'cut-21.1_f7') :-
    nop(english("Carol cut the envelop open.")),
    into_lf((cause(Agent, E), manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1')),
                     4,
                     [np(Patient, [+tConcrete]), np(Result)],
                     [prep(with), np(Instrument, [+tConcrete])],
                     A,
                     'cut-21.1_f8') :-
    nop(english("Carol cut the envelope open with the knife.")),
    into_lf((cause(Agent, E), manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'cut-21.1_f9') :-
    nop(english("Carol cut the envelope into pieces.")),
    into_lf((cause(Agent, E), manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1')),
                     5,
                     [np(Patient, [+tConcrete])],
                     [ prep((to;into)),
                       np(Result, [+state]),
                       prep(with),
                       np(Instrument, [+tConcrete])
                     ],
                     A,
                     'cut-21.1_f10') :-
    nop(english("Carol cut the envelope into pieces with a knife.")),
    into_lf((cause(Agent, E), manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('cut-21.1')),
                     2,
                     np(Patient, [+tConcrete]),
                     np(Result),
                     A,
                     'cut-21.1_f11') :-
    nop(english("The knife cut the envelope open.")),
    into_lf((manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('cut-21.1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'cut-21.1_f12') :-
    nop(english("The knife cut the envelope into pieces.")),
    into_lf((manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(from), np(Source)],
                     A,
                     'cut-21.1_f13') :-
    nop(english("Sympathetic fans clipped copies of Ms. Shere's recipes from magazines.")),
    into_lf((manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), holds(Pred, ResultE, Patient), cause(E, Agent), holds(Prep, E, Patient, Source), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(Prep, '$PREP'), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1')),
                     5,
                     [np(Patient, [+tConcrete])],
                     [ prep(from),
                       np(Source),
                       prep(with),
                       np(Instrument, [+tConcrete])
                     ],
                     A,
                     'cut-21.1_f14') :-
    nop(english("Susan cut the recipes from the magazine with a sharp knife.")),
    into_lf((manner(E, Motion, Agent), contact(E, Instrument, Patient), use(E, Agent, Instrument), degradation_material_integrity(ResultE, Patient), cause(E, Agent), holds(Prep, E, Patient, Source), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('cut-21.1-1')),
                     1,
                     np(Patient,
                        [ or(isa(Patient, tBodyPart),
                             isa(Patient, tRefl)),
                          +tConcrete
                        ]),
                     [],
                     A,
                     'cut-21.1-1_f0') :-
    nop(english("Carol cut her finger.")),
    into_lf((manner(E, Motion, Agent), contact(E, Instrument, Patient), degradation_material_integrity(ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), or(isa(Patient, tBodyPart), isa(Patient, tRefl)), isa(E, actEvent), isa(Motion, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('debone-10.8')),
                     1,
                     np(Source, [+tConcrete]),
                     [],
                     A,
                     'debone-10.8_f0') :-
    nop(english("The cook deboned the fish.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Theme, Pred), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4')),
                     2,
                     np(Theme),
                     np(Result, [-sentential]),
                     A,
                     'declare-29.4_f0') :-
    nop(english("The president declared Smith foolish.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4')),
                     2,
                     np(Theme),
                     np(Result, [-sentential]),
                     A,
                     'declare-29.4_f1') :-
    nop(english("The president declared Smith professor.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4')),
                     1,
                     np(Theme, [+to_be]),
                     [],
                     A,
                     'declare-29.4_f2') :-
    nop(english("The president maintained Smith to be a good man.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4')),
                     2,
                     np(Theme, [+np_ppart]),
                     np(Result, [-sentential]),
                     A,
                     'declare-29.4_f3') :-
    nop(english("The president judged the matter closed.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'declare-29.4_f4') :-
    nop(english("The president maintained that the matter was closed.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4-1')),
                     1,
                     np(Theme, [+what_extract]),
                     [],
                     A,
                     'declare-29.4-1_f0') :-
    nop(english("The president declared what we should do.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4-1')),
                     1,
                     np(Theme, [+what_inf]),
                     [],
                     A,
                     'declare-29.4-1_f1') :-
    nop(english("The president declared what to do.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4-1')),
                     3,
                     [np(Theme)],
                     [prep(as), np(Result, [-sentential])],
                     A,
                     'declare-29.4-1_f2') :-
    nop(english("The president declared John as stupid.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4-1-1')),
                     1,
                     np(Theme, [+how_extract]),
                     [],
                     A,
                     'declare-29.4-1-1_f0') :-
    nop(english("John declared how he did it.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4-1-1-1')),
                     1,
                     np(Theme, [+wh_inf]),
                     [],
                     A,
                     'declare-29.4-1-1-1_f0') :-
    nop(english("John declared how to do it.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4-1-1-2')),
                     1,
                     np(Theme, [+wh_comp]),
                     [],
                     A,
                     'declare-29.4-1-1-2_f0') :-
    nop(english("John judged whether he should come.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4-1-1-2')),
                     1,
                     np(Theme, [+wheth_inf]),
                     [],
                     A,
                     'declare-29.4-1-1-2_f1') :-
    nop(english("John judged whether to clean the house.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4-1-1-3')),
                     1,
                     np(Theme, [+be_sc_ing]),
                     [],
                     A,
                     'declare-29.4-1-1-3_f0') :-
    nop(english("John professed loving the miscreants.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4-2')),
                     1,
                     np(Theme, [+how_extract]),
                     [],
                     A,
                     'declare-29.4-2_f0') :-
    nop(english("The president proclaimed how we should work.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('declare-29.4-2')),
                     1,
                     np(Theme, [+wh_inf]),
                     [],
                     A,
                     'declare-29.4-2_f1') :-
    nop(english("The president proclaimed how to do it.")),
    into_lf((declare(E, Agent, Theme, Result), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('dedicate-79')),
                     3,
                     [np(Theme)],
                     [prep(to), np(Goal, [+sc_ing])],
                     A,
                     'dedicate-79_f0') :-
    nop(english("I dedicated myself to helping us.")),
    into_lf((dedicate(E, Agent, Theme, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('dedicate-79')),
                     3,
                     [np(Theme)],
                     [prep(to), np(Goal, [-sentential])],
                     A,
                     'dedicate-79_f1') :-
    nop(english("I dedicated myself to the cause.")),
    into_lf((dedicate(E, Agent, Theme, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('deduce-97.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'deduce-97.2_f0') :-
    nop(english("He deduced the truth about the story.")),
    into_lf((conclude(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('deduce-97.2')),
                     3,
                     [np(Theme)],
                     [prep(from), np(Source)],
                     A,
                     'deduce-97.2_f1') :-
    nop(english("He deduced the truth from the facts.")),
    into_lf((conclude(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('deduce-97.2')),
                     3,
                     [],
                     [ prep(from),
                       np(Theme, [+that_comp]),
                       np(Source)
                     ],
                     A,
                     'deduce-97.2_f2') :-
    nop(english("He deduced that she was a spy from miniature camera she had in her pen.")),
    into_lf((conclude(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('deduce-97.2')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'deduce-97.2_f3') :-
    nop(english("He deduced that this factor must have influenced the system.")),
    into_lf((conclude(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('defend-85')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tOrganization))
                        ]),
                     [],
                     A,
                     'defend-85_f0') :-
    nop(english("I defended them.")),
    into_lf((defend(E, Agent, Theme, Co_Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('defend-85')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tOrganization))
                          ])
                     ],
                     [prep(against), np(Co_Theme, [-sentential])],
                     A,
                     'defend-85_f1') :-
    nop(english("I defended them against the warriors.")),
    into_lf((defend(E, Agent, Theme, Co_Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('defend-85')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tOrganization)),
                          +ac_ing
                        ]),
                     [],
                     A,
                     'defend-85_f2') :-
    nop(english("I defended building the house.")),
    into_lf((defend(E, Agent, Theme, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('defend-85')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tOrganization)),
                          +poss_ing
                        ]),
                     [],
                     A,
                     'defend-85_f3') :-
    nop(english("I defended their building the house.")),
    into_lf((defend(E, Agent, Theme, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('destroy-44')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'destroy-44_f0') :-
    nop(english("The Romans destroyed the city.")),
    into_lf((cause(Agent, E), destroyed(ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('destroy-44')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(with), np(Instrument, [+tConcrete])],
                     A,
                     'destroy-44_f1') :-
    nop(english("The builders destroyed the warehouse with explosives.")),
    into_lf((cause(Agent, E), use(E, Agent, Instrument), destroyed(ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('destroy-44')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'destroy-44_f2') :-
    nop(english("The explosives destroyed the warehouse.")),
    into_lf((cause(Agent, E), use(E, Agent, Instrument), destroyed(ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('devour-39.4-1')),
                     1,
                     np(Patient, [+tSolid, +tComestible]),
                     [],
                     A,
                     'devour-39.4-1_f0') :-
    nop(english("Cynthia devoured the pizza.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('devour-39.4-2')),
                     1,
                     np(Patient, [-tSolid, +tComestible]),
                     [],
                     A,
                     'devour-39.4-2_f0') :-
    nop(english("Cynthia swilled the beverage.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), ~isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('differ-23.4')),
                     2,
                     prep((from;with)),
                     np(Co_Theme),
                     A,
                     'differ-23.4_f0') :-
    nop(english("The winter schedule differed from the spring schedule.")),
    into_lf(different(Theme, Co_Theme), A).
vndata:verbnet_frame(np(Theme, [+plural]),
                     verb(vn('differ-23.4')),
                     0,
                     [],
                     [],
                     A,
                     'differ-23.4_f1') :-
    nop(english("This flyer and that flyer differ.")),
    into_lf(different(Theme, Co_Theme), A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('dine-39.5')),
                     0,
                     [],
                     [],
                     A,
                     'dine-39.5_f0') :-
    nop(english("Cynthia breakfasted.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('dine-39.5')),
                     2,
                     prep(on),
                     np(Patient, [+tComestible]),
                     A,
                     'dine-39.5_f1') :-
    nop(english("Cynthia breakfasted on peaches.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('disappearance-48.2')),
                     0,
                     [],
                     [],
                     A,
                     'disappearance-48.2_f0') :-
    nop(english("The crowd vanished.")),
    into_lf((disappear(E, Patient), isa(E, actEvent)), A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('disappearance-48.2')),
                     2,
                     prep(from),
                     np(Initial_Location),
                     A,
                     'disappearance-48.2_f1') :-
    nop(english("A valuable manuscript vanished from the library.")),
    into_lf((disappear(E, Patient), holds(Prep, EndE, Patient, Initial_Location), isa(Prep, '$PREP'), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('disassemble-23.3')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'disassemble-23.3_f0') :-
    nop(english("I unscrewed the handle.")),
    into_lf((cause(Agent, E), together(StartE, Physical, Patient, Co_Patient), apart(ResultE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Physical, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('disassemble-23.3')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(from), np(Co_Patient, [+tConcrete])],
                     A,
                     'disassemble-23.3_f1') :-
    nop(english("I unscrewed the handle from the box.")),
    into_lf((cause(Agent, E), together(StartE, Physical, Patient, Co_Patient), apart(ResultE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Physical, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('disassemble-23.3')),
                     1,
                     'ADV',
                     [],
                     A,
                     'disassemble-23.3_f2') :-
    nop(english("That new handle unscrews easily.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84')),
                     1,
                     np(Theme, [+how_extract]),
                     [],
                     A,
                     'discover-84_f0') :-
    nop(english("I discovered how she did it.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84')),
                     1,
                     np(Theme, [+wh_inf]),
                     [],
                     A,
                     'discover-84_f1') :-
    nop(english("I discovered how to do it.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84')),
                     1,
                     np(Theme, [-sentential]),
                     [],
                     A,
                     'discover-84_f2') :-
    nop(english("I discovered the fleece.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'discover-84_f3') :-
    nop(english("I discovered that it didn't make sense.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84')),
                     1,
                     np(Theme, [+what_extract]),
                     [],
                     A,
                     'discover-84_f4') :-
    nop(english("I discovered what he should do.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84')),
                     1,
                     np(Theme, [+what_inf]),
                     [],
                     A,
                     'discover-84_f5') :-
    nop(english("I discovered what to do.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1')),
                     2,
                     prep(about),
                     np(Theme, [+np_ing]),
                     A,
                     'discover-84-1_f0') :-
    nop(english("I discovered about him drinking.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1')),
                     3,
                     [],
                     [ prep(from),
                       np(Source),
                       np(Theme, [+how_extract])
                     ],
                     A,
                     'discover-84-1_f1') :-
    nop(english("I discovered from them how she did it.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1')),
                     3,
                     [],
                     [prep(from), np(Source), np(Theme, [+wh_inf])],
                     A,
                     'discover-84-1_f2') :-
    nop(english("I discovered from them how to do it.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1')),
                     4,
                     [],
                     [ prep(from),
                       np(Source),
                       prep(about),
                       np(Theme, [+wh_comp])
                     ],
                     A,
                     'discover-84-1_f3') :-
    nop(english("I discovered from him about whether he should kill the peasants.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1')),
                     4,
                     [],
                     [ prep(from),
                       np(Source),
                       prep(about),
                       np(Theme, [+what_extract])
                     ],
                     A,
                     'discover-84-1_f4') :-
    nop(english("I discovered from him about what he should do.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1')),
                     4,
                     [],
                     [ prep(from),
                       np(Source),
                       prep(about),
                       np(Theme, [+wh_inf])
                     ],
                     A,
                     'discover-84-1_f5') :-
    nop(english("I learned from him about what to do.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1')),
                     4,
                     [],
                     [ prep(from),
                       np(Source),
                       prep(about),
                       np(Theme, [+wheth_inf])
                     ],
                     A,
                     'discover-84-1_f6') :-
    nop(english("I discovered from him about whether to go.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'discover-84-1_f7') :-
    nop(english("I discovered that it didn't make sense.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1')),
                     1,
                     np(Theme, [+what_inf]),
                     [],
                     A,
                     'discover-84-1_f8') :-
    nop(english("I discovered what to do.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1-1')),
                     0,
                     [],
                     [],
                     A,
                     'discover-84-1-1_f0') :-
    nop(english("I learned.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1-1')),
                     2,
                     prep(about),
                     np(Theme, [+poss_ing]),
                     A,
                     'discover-84-1-1_f1') :-
    nop(english("I learned about his drinking.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1-1')),
                     2,
                     prep(about),
                     np(Theme, [+what_extract]),
                     A,
                     'discover-84-1-1_f2') :-
    nop(english("I learned about what he drank.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1-1')),
                     2,
                     prep(about),
                     np(Theme, [-sentential]),
                     A,
                     'discover-84-1-1_f3') :-
    nop(english("I learned about the drinking.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('discover-84-1-1')),
                     4,
                     [],
                     [ prep(about),
                       np(Theme, [-sentential]),
                       prep(from),
                       np(Source)
                     ],
                     A,
                     'discover-84-1-1_f4') :-
    nop(english("I learned about it from a book.")),
    into_lf((discover(E, Agent, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('dress-41.1.1')),
                     0,
                     [],
                     [],
                     A,
                     'dress-41.1.1_f0') :-
    nop(english("Marlene dressed.")),
    into_lf((take_care_of(E, Agent, Patient), isa(Agent, tAnimate), or(isa(Patient, tAnimate), isa(Patient, tRefl)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('dress-41.1.1')),
                     1,
                     np(Patient,
                        [ or(isa(Patient, tAnimate),
                             isa(Patient, tRefl))
                        ]),
                     [],
                     A,
                     'dress-41.1.1_f1') :-
    nop(english("Marlene dressed the baby.")),
    into_lf((take_care_of(E, Agent, Patient), isa(Agent, tAnimate), or(isa(Patient, tAnimate), isa(Patient, tRefl)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('dress-41.1.1')),
                     1,
                     np(Patient,
                        [ or(isa(Patient, tAnimate),
                             isa(Patient, tRefl)),
                          +refl
                        ]),
                     [],
                     A,
                     'dress-41.1.1_f2') :-
    nop(english("Marlene dressed herself.")),
    into_lf((take_care_of(E, Agent, Patient), isa(Agent, tAnimate), or(isa(Patient, tAnimate), isa(Patient, tRefl)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent),
                     verb(vn('dressing_well-41.3.2')),
                     2,
                     np(Patient, [+tRefl]),
                     lex(up),
                     A,
                     'dressing_well-41.3.2_f0') :-
    nop(english("She spruced herself up before the job interview.")),
    into_lf((take_care_of(E, Agent, Patient), isa(Patient, tRefl), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent),
                     verb(vn('dressing_well-41.3.2')),
                     2,
                     lex(up),
                     np(Patient, [+tRefl]),
                     A,
                     'dressing_well-41.3.2_f1') :-
    nop(english("She spruced up herself before the job interview.")),
    into_lf((take_care_of(E, Agent, Patient), isa(Patient, tRefl), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'drive-11.5_f0') :-
    nop(english("Amanda drove the package.")),
    into_lf((motion(E0, Theme), equals(E0, E1), motion(E1, Agent), cause(Agent, E0), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(E1, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep((to;towards)),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'drive-11.5_f1') :-
    nop(english("Amanda drove the package to New York.")),
    into_lf((motion(E0, Theme), location(EndE0, Theme, Destination), equals(E0, E1), motion(E1, Agent), location(EndE1, Agent, Destination), cause(Agent, E0), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'drive-11.5_f2') :-
    nop(english("Amanda drove the package from home.")),
    into_lf((motion(E0, Theme), location(StartE0, Theme, Initial_Location), equals(E0, E1), motion(E1, Agent), location(StartE1, Agent, Initial_Location), cause(Agent, E0), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(StartE0, actEvent), start(E0, StartE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation]),
                       prep((to;towards)),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'drive-11.5_f3') :-
    nop(english("Amanda drove the package from home to New York.")),
    into_lf((motion(E0, Theme), location(StartE0, Theme, Initial_Location), location(EndE0, Theme, Destination), equals(E0, E1), motion(E1, Agent), location(StartE1, Agent, Initial_Location), location(EndE1, Agent, Destination), cause(Agent, E0), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(StartE0, actEvent), start(E0, StartE0), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ]),
                       prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'drive-11.5_f4') :-
    nop(english("Amanda drove the package to New York from home.")),
    into_lf((motion(E0, Theme), location(StartE0, Theme, Initial_Location), location(EndE0, Theme, Destination), equals(E0, E1), motion(E1, Agent), location(StartE1, Agent, Initial_Location), location(EndE1, Agent, Destination), cause(Agent, E0), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(StartE0, actEvent), start(E0, StartE0), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5-1')),
                     2,
                     np(Destination,
                        [ or(isa(Destination, tAnimate),
                             (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                        ]),
                     np(Theme, [+tConcrete]),
                     A,
                     'drive-11.5-1_f0') :-
    nop(english("Amanda flew me the package.")),
    into_lf((motion(E, Theme), direction(E, Toward, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(Toward, 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5-1')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'drive-11.5-1_f1') :-
    nop(english("Amanda shuttled her children.")),
    into_lf((motion(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5-1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'drive-11.5-1_f2') :-
    nop(english("Amanda shuttled her children from Philadelphia.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5-1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'drive-11.5-1_f3') :-
    nop(english("Amanda shuttled the children to school.")),
    into_lf((motion(E, Theme), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5-1')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation]),
                       prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'drive-11.5-1_f4') :-
    nop(english("Amanda trucked the package from Philadelphia to her mother's house.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tVehicle))
                        ]),
                     verb(vn('drive-11.5-1')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ]),
                       prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'drive-11.5-1_f5') :-
    nop(english("Amanda trucked the package to her mother's house from Philadelphia.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tVehicle)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('dub-29.3')),
                     2,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization))
                        ]),
                     np(Result),
                     A,
                     'dub-29.3_f0') :-
    nop(english("The captain named the ship Seafarer.")),
    into_lf((designated(ResultE, Theme, Result), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('dub-29.3')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tOrganization))
                        ]),
                     [],
                     A,
                     'dub-29.3_f1') :-
    nop(english("The captain baptized the ship.")),
    into_lf((cause(Agent, E), designated(ResultE, Theme, Result), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tConcrete), isa(Theme, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('eat-39.1-1')),
                     1,
                     np(Patient, [+tSolid, +tComestible]),
                     [],
                     A,
                     'eat-39.1-1_f0') :-
    nop(english("Cynthia ate the peach.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('eat-39.1-1')),
                     0,
                     [],
                     [],
                     A,
                     'eat-39.1-1_f1') :-
    nop(english("Cynthia ate.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('eat-39.1-1')),
                     2,
                     prep(at),
                     np(Patient, [+tSolid, +tComestible]),
                     A,
                     'eat-39.1-1_f2') :-
    nop(english("Cynthia ate at the peach.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('eat-39.1-1')),
                     2,
                     prep(Prep, [+src]),
                     np(Source),
                     A,
                     'eat-39.1-1_f3') :-
    nop(english("He ate off of the table.")),
    into_lf((take_in(E, Agent, Patient), holds(Prep, E, Patient, Source), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('eat-39.1-2')),
                     1,
                     np(Patient, [-tSolid, +tComestible]),
                     [],
                     A,
                     'eat-39.1-2_f0') :-
    nop(english("Cynthia drank the wine.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), ~isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('eat-39.1-2')),
                     0,
                     [],
                     [],
                     A,
                     'eat-39.1-2_f1') :-
    nop(english("Cynthia drank.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), ~isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('eat-39.1-2')),
                     2,
                     prep(Prep, [+src]),
                     np(Source),
                     A,
                     'eat-39.1-2_f2') :-
    nop(english("He drank out of the goblet.")),
    into_lf((take_in(E, Agent, Patient), holds(Prep, E, Patient, Source), isa(Agent, tAnimate), isa(Patient, tComestible), ~isa(Patient, tSolid), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('empathize-88.2')),
                     0,
                     [],
                     [],
                     A,
                     'empathize-88.2_f0') :-
    nop(english("You really have to empathize.")),
    into_lf((emotional_state(E, Experiencer), in_reaction_to(E, Stimulus), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('empathize-88.2')),
                     2,
                     prep(with),
                     np(Stimulus, [-sentential]),
                     A,
                     'empathize-88.2_f1') :-
    nop(english("You really have to empathize with them.")),
    into_lf((emotional_state(E, Experiencer), in_reaction_to(E, Stimulus), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('empathize-88.2')),
                     2,
                     prep(with),
                     np(Stimulus, [+poss_ing]),
                     A,
                     'empathize-88.2_f2') :-
    nop(english("You really have to empathize with their doing it.")),
    into_lf((emotional_state(E, Experiencer), in_reaction_to(E, Stimulus), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('empathize-88.2')),
                     2,
                     prep(with),
                     np(Stimulus, [+what_inf]),
                     A,
                     'empathize-88.2_f3') :-
    nop(english("You've really got to empathize with what they want.")),
    into_lf((emotional_state(E, Experiencer), in_reaction_to(E, Stimulus), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('enforce-63')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'enforce-63_f0') :-
    nop(english("I cannot impose my opinion.")),
    into_lf((enforce(E, Agent, Theme), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('enforce-63')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'enforce-63_f1') :-
    nop(english("I cannot impose that I get what I want.")),
    into_lf((enforce(E, Agent, Theme), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause, [+tAbstract]),
                     verb(vn('engender-27')),
                     1,
                     np(Theme, [+tAbstract]),
                     [],
                     A,
                     'engender-27_f0') :-
    nop(english("Racial inequality engenders conflict.")),
    into_lf((~exist(StartE, Theme), exist(ResultE, Theme), nonagentive_cause(Cause, E), isa(Cause, tAbstract), isa(Theme, tAbstract), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Cause, [+tAbstract]),
                     verb(vn('engender-27')),
                     2,
                     np(Theme, [+tAbstract]),
                     np(Predicate, [+that_comp]),
                     A,
                     'engender-27_f1') :-
    nop(english("The bug causes the chip to give wrong answers for some mathematical calculations.")),
    into_lf((~exist(StartE, Theme), exist(ResultE, Theme), nonagentive_cause(Cause, E), isa(Cause, tAbstract), isa(Theme, tAbstract), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('ensure-99')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'ensure-99_f0') :-
    nop(english("Our exertions ensured a good outcome.")),
    into_lf((cause(Cause, E), ensure(Theme, Beneficiary), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('ensure-99')),
                     2,
                     np(Beneficiary),
                     np(Theme, [-sentential]),
                     A,
                     'ensure-99_f1') :-
    nop(english("Our exertions ensured us a good outcome.")),
    into_lf((cause(Cause, E), ensure(Theme, Beneficiary), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('ensure-99')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Beneficiary)],
                     A,
                     'ensure-99_f2') :-
    nop(english("The circumstances ensured a good outcome for him.")),
    into_lf((cause(Cause, E), ensure(Theme, Beneficiary), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('ensure-99')),
                     2,
                     np(Beneficiary),
                     np(Theme, [+oc_to_inf]),
                     A,
                     'ensure-99_f3') :-
    nop(english("The circumstances ensured that he would arrive in time.")),
    into_lf((cause(Cause, E), ensure(Theme, Beneficiary), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('entity_specific_cos-45.5')),
                     0,
                     [],
                     [],
                     A,
                     'entity_specific_cos-45.5_f0') :-
    nop(english("The roses bloomed.")),
    into_lf((state(ResultE, Endstate, Patient), isa(Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Endstate, Pred)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('entity_specific_modes_being-47.2')),
                     0,
                     [],
                     [],
                     A,
                     'entity_specific_modes_being-47.2_f0') :-
    nop(english("The beer foamed.")),
    into_lf((exist(E, Theme), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('entity_specific_modes_being-47.2')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     A,
                     'entity_specific_modes_being-47.2_f1') :-
    nop(english("A fire raged in the mountains.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('entity_specific_modes_being-47.2')),
                     1,
                     np(Theme, [-definite]),
                     [],
                     A,
                     'entity_specific_modes_being-47.2_f2') :-
    nop(english("There raged a fire.")),
    into_lf((exist(E, Theme), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     2,
                     verb(vn('entity_specific_modes_being-47.2')),
                     np(Theme),
                     A,
                     'entity_specific_modes_being-47.2_f3') :-
    nop(english("All through the mountains raged a fire.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Location, [-tRegion, +tLocation]),
                     verb(vn('entity_specific_modes_being-47.2')),
                     2,
                     prep(with),
                     np(Theme),
                     A,
                     'entity_specific_modes_being-47.2_f4') :-
    nop(english("The garden flowered with roses.")),
    into_lf((exist(E, Theme), location(E, Theme, Location), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('equip-13.4.2')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(with), np(Theme)],
                     A,
                     'equip-13.4.2_f0') :-
    nop(english("Brown equipped Jones with a camera.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('equip-13.4.2-1')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'equip-13.4.2-1_f0') :-
    nop(english("Brown equipped his soldiers.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('equip-13.4.2-1-1')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'equip-13.4.2-1-1_f0') :-
    nop(english("The huge load burdened the mule.")),
    into_lf((has_possession(E, Recipient, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('escape-51.1')),
                     0,
                     [],
                     [],
                     A,
                     'escape-51.1_f0') :-
    nop(english("The prisoners advanced.")),
    into_lf((motion(E, Theme), path(E, Theme, Initial_Location, Trajectory, Destination), isa(Theme, tConcrete), isa(Initial_Location, tConcrete), isa(Destination, tConcrete), isa(Trajectory, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('escape-51.1')),
                     2,
                     prep(Prep, [+path]),
                     np(Initial_Location, [+tConcrete]),
                     A,
                     'escape-51.1_f1') :-
    nop(english("He came from France.")),
    into_lf((motion(E, Theme), path(E, Theme, Initial_Location, Trajectory, Destination), isa(Theme, tConcrete), isa(Initial_Location, tConcrete), isa(Destination, tConcrete), isa(Trajectory, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('escape-51.1')),
                     2,
                     prep(Prep, [+path]),
                     np(Destination, [+tConcrete]),
                     A,
                     'escape-51.1_f2') :-
    nop(english("He came to Colorado.")),
    into_lf((motion(E, Theme), path(E, Theme, Initial_Location, Trajectory, Destination), isa(Theme, tConcrete), isa(Initial_Location, tConcrete), isa(Destination, tConcrete), isa(Trajectory, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('escape-51.1')),
                     2,
                     prep(Prep, [+path]),
                     np(Trajectory, [+tConcrete]),
                     A,
                     'escape-51.1_f3') :-
    nop(english("He came through the door.")),
    into_lf((motion(E, Theme), path(E, Theme, Initial_Location, Trajectory, Destination), isa(Theme, tConcrete), isa(Initial_Location, tConcrete), isa(Destination, tConcrete), isa(Trajectory, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('escape-51.1')),
                     4,
                     [],
                     [ prep(Prep, [+path]),
                       np(Initial_Location, [+tConcrete]),
                       prep(Prep, [+path]),
                       np(Destination, [+tConcrete])
                     ],
                     A,
                     'escape-51.1_f4') :-
    nop(english("He came from France to Colorado.")),
    into_lf((motion(E, Theme), path(E, Theme, Initial_Location, Trajectory, Destination), isa(Theme, tConcrete), isa(Initial_Location, tConcrete), isa(Destination, tConcrete), isa(Trajectory, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('escape-51.1-1')),
                     1,
                     np(Initial_Location, [+tConcrete]),
                     [],
                     A,
                     'escape-51.1-1_f0') :-
    nop(english("The convict escaped the prison.")),
    into_lf((motion(E, Theme), path(E, Theme, Initial_Location, Trajectory, Destination), isa(Theme, tConcrete), isa(Initial_Location, tConcrete), isa(Destination, tConcrete), isa(Trajectory, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('escape-51.1-2')),
                     1,
                     np(Destination, [+tConcrete]),
                     [],
                     A,
                     'escape-51.1-2_f0') :-
    nop(english("He entered the room.")),
    into_lf((motion(E, Theme), path(E, Theme, Initial_Location, Trajectory, Destination), isa(Theme, tConcrete), isa(Initial_Location, tConcrete), isa(Destination, tConcrete), isa(Trajectory, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('escape-51.1-3')),
                     1,
                     np(Trajectory, [+tConcrete]),
                     [],
                     A,
                     'escape-51.1-3_f0') :-
    nop(english("He climbed the mountain.")),
    into_lf((motion(E, Theme), path(E, Theme, Initial_Location, Trajectory, Destination), isa(Theme, tConcrete), isa(Initial_Location, tConcrete), isa(Destination, tConcrete), isa(Trajectory, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('establish-55.5')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'establish-55.5_f0') :-
    nop(english("Zellig Harris established Penn's linguistics department.")),
    into_lf((begin(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('establish-55.5-1')),
                     3,
                     [np(Theme)],
                     [prep(with), np(Instrument)],
                     A,
                     'establish-55.5-1_f0') :-
    nop(english("I opened the event with a speech.")),
    into_lf((begin(E, Theme), use(E, Agent, Instrument), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('estimate-34.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'estimate-34.2_f0') :-
    nop(english("He estimated the probability of Don Quixote spontaneously combusting upon contact with the curry poweder.")),
    into_lf((assess(E, Agent, Theme, Value), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('estimate-34.2')),
                     3,
                     [np(Theme)],
                     [prep(at), np(Value)],
                     A,
                     'estimate-34.2_f1') :-
    nop(english("He estimated the probability at 0.9.")),
    into_lf((assess(E, Agent, Theme, Value), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('estimate-34.2')),
                     1,
                     np(Value, [+how_extract]),
                     [],
                     A,
                     'estimate-34.2_f2') :-
    nop(english("He estimated how likely it would be.")),
    into_lf((assess(E, Agent, Theme, Value), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('estimate-34.2')),
                     1,
                     np(Theme, [+to_be]),
                     [],
                     A,
                     'estimate-34.2_f3') :-
    nop(english("He estimated this outcome to be likely.")),
    into_lf((assess(E, Agent, Theme, Value), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('estimate-34.2')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'estimate-34.2_f4') :-
    nop(english("He estimated that it would be likely.")),
    into_lf((assess(E, Agent, Theme, Value), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('estimate-34.2')),
                     1,
                     np(Theme, [+what_extract]),
                     [],
                     A,
                     'estimate-34.2_f5') :-
    nop(english("He estimated what would be likely.")),
    into_lf((assess(E, Agent, Theme, Value), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('exceed-90')),
                     1,
                     np(Co_Theme),
                     [],
                     A,
                     'exceed-90_f0') :-
    nop(english("Her performance exceeds our expectations.")),
    into_lf((exceed(E, Theme, Co_Theme, Attribute), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('exceed-90')),
                     3,
                     [np(Co_Theme)],
                     [prep(in), np(Attribute)],
                     A,
                     'exceed-90_f1') :-
    nop(english("Her performance exceeds ours in brilliance.")),
    into_lf((exceed(E, Theme, Co_Theme, Attribute), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('exceed-90-1')),
                     5,
                     [np(Co_Theme)],
                     [prep(in), np(Attribute), prep(by), np(Extent)],
                     A,
                     'exceed-90-1_f0') :-
    nop(english("She exceeded him in speed by two miles per hour.")),
    into_lf((exceed(E, Theme, Co_Theme, Attribute, Extent), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('exchange-13.6')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Co_Theme)],
                     A,
                     'exchange-13.6_f0') :-
    nop(english("Gwen exchanged the dress for a shirt.")),
    into_lf((has_possession(StartE, Agent, Theme), ~has_possession(StartE, Agent, Co_Theme), has_possession(EndE, Agent, Co_Theme), ~has_possession(EndE, Agent, Theme), transfer(E, Theme), transfer(E, Co_Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('exchange-13.6')),
                     1,
                     np(Theme, [+plural]),
                     [],
                     A,
                     'exchange-13.6_f1') :-
    nop(english("Twenty couples exchanged rings.")),
    into_lf((has_possession(StartE, Agent_i, Theme_i), has_possession(EndE, Agent_j, Theme_i), has_possession(StartE, Agent_j, Theme_j), has_possession(EndE, Agent_i, Theme_j), transfer(E, Theme_i), transfer(E, Theme_j), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('exchange-13.6')),
                     2,
                     prep(Prep, [+loc]),
                     np(Theme, [+plural]),
                     A,
                     'exchange-13.6_f2') :-
    nop(english("He swapped between the two.")),
    into_lf((has_possession(StartE, Agent, Theme_i), has_possession(EndE, Agent, Theme_j), transfer(E, Theme_i), transfer(E, Theme_j), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('exchange-13.6')),
                     3,
                     [np(Theme, [+plural])],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ])
                     ],
                     A,
                     'exchange-13.6_f3') :-
    nop(english("Cathay is swapping equity stakes with the U.S. carrier.")),
    into_lf((has_possession(StartE, Agent, Theme_i), has_possession(EndE, Co_Agent, Theme_i), has_possession(StartE, Co_Agent, Theme_j), has_possession(EndE, Agent, Theme_j), transfer(E, Theme_i), transfer(E, Theme_j), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('exchange-13.6')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'exchange-13.6_f4') :-
    nop(english("She exchanged her purchase.")),
    into_lf((transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('exchange-13.6-1')),
                     3,
                     [np(Location, [-tRegion, +tLocation])],
                     [prep(with), np(Co_Theme)],
                     A,
                     'exchange-13.6-1_f0') :-
    nop(english("One bell can swap places with another.")),
    into_lf((location(StartE, Theme, Location_i), location(EndE, Theme, Location_j), location(StartE, Co_Theme, Location_j), location(EndE, Co_Theme, Location_i), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+plural]),
                     verb(vn('exchange-13.6-1')),
                     1,
                     np(Location, [-tRegion, +tLocation]),
                     [],
                     A,
                     'exchange-13.6-1_f1') :-
    nop(english("The bells traded places.")),
    into_lf((location(StartE, Theme_i, Location_i), location(EndE, Theme_i, Location_j), location(StartE, Theme_j, Location_j), location(EndE, Theme_j, Location_i), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('exchange-13.6-1-1')),
                     2,
                     prep((for;into)),
                     np(Co_Theme),
                     A,
                     'exchange-13.6-1-1_f0') :-
    nop(english("They tend to substitute for more mundane things.")),
    into_lf((transfer(E, Theme), transfer(E, Co_Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('exhale-40.1.3-1')),
                     0,
                     [],
                     [],
                     A,
                     'exhale-40.1.3-1_f0') :-
    nop(english("Paul exhaled.")),
    into_lf((body_process(E, Pivot), emit(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('exhale-40.1.3-1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'exhale-40.1.3-1_f1') :-
    nop(english("Paul exhaled a breath.")),
    into_lf((body_process(E, Pivot), emit(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('exhale-40.1.3-2')),
                     0,
                     [],
                     [],
                     A,
                     'exhale-40.1.3-2_f0') :-
    nop(english("Paul inhaled.")),
    into_lf((body_process(E, Pivot), take_in(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('exhale-40.1.3-2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'exhale-40.1.3-2_f1') :-
    nop(english("Paul inhaled water.")),
    into_lf((body_process(E, Pivot), take_in(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('exist-47.1')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     A,
                     'exist-47.1_f0') :-
    nop(english("Unicorns don't exist on Earth.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('exist-47.1-1')),
                     0,
                     [],
                     [],
                     A,
                     'exist-47.1-1_f0') :-
    nop(english("Unicorns don't exist.")),
    into_lf((exist(E, Theme), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('exist-47.1-1-1')),
                     1,
                     np(Pivot),
                     [],
                     A,
                     'exist-47.1-1-1_f0') :-
    nop(english("I endured the routine heavy traffic.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Pivot), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('feeding-39.7')),
                     1,
                     np(Recipient, [+tAnimate]),
                     [],
                     A,
                     'feeding-39.7_f0') :-
    nop(english("Teresa bottlefed the baby.")),
    into_lf((take_in(E, Recipient, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tComestible), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('feeding-39.7')),
                     3,
                     [np(Theme, [+tComestible])],
                     [prep(to), np(Recipient, [+tAnimate])],
                     A,
                     'feeding-39.7_f1') :-
    nop(english("Teresa bottlefed soy milk to the baby.")),
    into_lf((take_in(E, Recipient, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tComestible), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('feeding-39.7')),
                     2,
                     np(Recipient, [+tAnimate]),
                     np(Theme, [+tComestible]),
                     A,
                     'feeding-39.7_f2') :-
    nop(english("Teresa bottlefed the baby soy milk.")),
    into_lf((take_in(E, Recipient, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tComestible), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('ferret-35.6')),
                     3,
                     [np(Theme)],
                     [prep(Prep, [+dir]), np(Source)],
                     A,
                     'ferret-35.6_f0') :-
    nop(english("I ferreted the secret out of him.")),
    into_lf((search(E, Agent, Source, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('fill-9.8')),
                     3,
                     [np(Destination, [-tRegion, +tLocation])],
                     [prep(with), np(Theme, [+tConcrete])],
                     A,
                     'fill-9.8_f0') :-
    nop(english("Leslie staffed the store with employees.")),
    into_lf((motion(E, Theme), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('fill-9.8')),
                     3,
                     [np(Destination, [-tRegion, +tLocation])],
                     [prep(in), np(Theme, [+tConcrete])],
                     A,
                     'fill-9.8_f1') :-
    nop(english("Leigh swaddled the baby in blankets.")),
    into_lf((motion(E, Theme), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('fill-9.8')),
                     1,
                     np(Destination, [-tRegion, +tLocation]),
                     [],
                     A,
                     'fill-9.8_f2') :-
    nop(english("The employees staffed the store.")),
    into_lf((location(E, Theme, Destination), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('fill-9.8')),
                     1,
                     np(Destination, [-tRegion, +tLocation]),
                     [],
                     A,
                     'fill-9.8_f3') :-
    nop(english("Leslie staffed the store.")),
    into_lf((motion(E, Theme), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('fill-9.8-1')),
                     0,
                     [],
                     [],
                     A,
                     'fill-9.8-1_f0') :-
    nop(english("CFC's pollute.")),
    into_lf((location(E, Theme, Destination), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('fire-10.10')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tOrganization))
                        ]),
                     [],
                     A,
                     'fire-10.10_f0') :-
    nop(english("I fired two secretaries.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(Source, tOrganization), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('fire-10.10')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tOrganization))
                          ])
                     ],
                     [prep(from), np(Source, [+tOrganization])],
                     A,
                     'fire-10.10_f1') :-
    nop(english("I fired two secretaries from the company.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(Source, tOrganization), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('fire-10.10')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tOrganization))
                          ])
                     ],
                     [prep(as), np(Attribute, [-sentential])],
                     A,
                     'fire-10.10_f2') :-
    nop(english("I fired him as my chief of staff.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source, Attribute), ~location(EndE, Theme, Source, Attribute), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(Source, tOrganization), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Location),
                     verb(vn('fit-54.3')),
                     1,
                     np(Value),
                     [],
                     A,
                     'fit-54.3_f0') :-
    nop(english("Each room sleeps five people.")),
    into_lf((capacity(E, Location, Value), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('fit-54.3')),
                     3,
                     [np(Value)],
                     [prep(in), np(Location)],
                     A,
                     'fit-54.3_f1') :-
    nop(english("We sleep five people in each room.")),
    into_lf((capacity(E, Location, Value), cause(Agent, E), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('flinch-40.5')),
                     0,
                     [],
                     [],
                     A,
                     'flinch-40.5_f0') :-
    nop(english("Sharon flinched.")),
    into_lf((flinch(E, Experiencer), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('flinch-40.5')),
                     2,
                     prep((at;from)),
                     np(Stimulus),
                     A,
                     'flinch-40.5_f1') :-
    nop(english("Sharon flinched at the sight of the accident.")),
    into_lf((flinch(E, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('floss-41.2.1')),
                     1,
                     np(Patient, [+tBodyPart]),
                     [],
                     A,
                     'floss-41.2.1_f0') :-
    nop(english("The hygienist flossed my teeth.")),
    into_lf((take_care_of(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('floss-41.2.1')),
                     0,
                     [],
                     [],
                     A,
                     'floss-41.2.1_f1') :-
    nop(english("I flossed.")),
    into_lf((take_care_of(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('floss-41.2.1')),
                     3,
                     [np(Patient, [+tBodyPart])],
                     [prep(with), np(Instrument)],
                     A,
                     'floss-41.2.1_f2') :-
    nop(english("She flossed her teeth with floss.")),
    into_lf((take_care_of(E, Agent, Patient), use(E, Agent, Instrument), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('floss-41.2.1')),
                     2,
                     prep(with),
                     np(Instrument),
                     A,
                     'floss-41.2.1_f3') :-
    nop(english("She brushed with a toothbrush.")),
    into_lf((take_care_of(E, Agent, Patient), use(E, Agent, Instrument), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('focus-87.1')),
                     2,
                     prep(on),
                     np(Theme, [-sentential]),
                     A,
                     'focus-87.1_f0') :-
    nop(english("We focused on it.")),
    into_lf((concentrate(E, Experiencer, Theme), cause(E, Theme), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('focus-87.1')),
                     2,
                     prep(on),
                     np(Theme, [+sc_ing]),
                     A,
                     'focus-87.1_f1') :-
    nop(english("We focused on reading the book.")),
    into_lf((concentrate(E, Experiencer, Theme), cause(E, Theme), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('focus-87.1')),
                     2,
                     prep(on),
                     np(Theme, [+what_extract]),
                     A,
                     'focus-87.1_f2') :-
    nop(english("We focused on what he wanted.")),
    into_lf((concentrate(E, Experiencer, Theme), cause(E, Theme), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('focus-87.1')),
                     2,
                     prep(on),
                     np(Theme, [+what_inf]),
                     A,
                     'focus-87.1_f3') :-
    nop(english("We focused on what to do.")),
    into_lf((concentrate(E, Experiencer, Theme), cause(E, Theme), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('forbid-67')),
                     1,
                     np(Theme, [+np_p_ing]),
                     [],
                     A,
                     'forbid-67_f0') :-
    nop(english("The rules forbid us from smoking.")),
    into_lf((forbid(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('forbid-67')),
                     1,
                     np(Theme, [+poss_ing]),
                     [],
                     A,
                     'forbid-67_f1') :-
    nop(english("The rules forbid our smoking.")),
    into_lf((forbid(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('forbid-67')),
                     1,
                     np(Theme, [+ac_ing]),
                     [],
                     A,
                     'forbid-67_f2') :-
    nop(english("The rules forbid smoking.")),
    into_lf((forbid(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('forbid-67')),
                     1,
                     np(Theme, [-sentential]),
                     [],
                     A,
                     'forbid-67_f3') :-
    nop(english("The rules forbid it.")),
    into_lf((forbid(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('force-59')),
                     1,
                     np(Patient,
                        [ or(isa(Patient, tAnimate),
                             isa(Patient, tOrganization))
                        ]),
                     [],
                     A,
                     'force-59_f0') :-
    nop(english("I forced him.")),
    into_lf((force(E, Agent, Patient, Result), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('force-59')),
                     3,
                     [ np(Patient,
                          [ or(isa(Patient, tAnimate),
                               isa(Patient, tOrganization))
                          ])
                     ],
                     [prep(into), np(Result, [+oc_ing])],
                     A,
                     'force-59_f1') :-
    nop(english("I forced him into coming.")),
    into_lf((force(E, Agent, Patient, Result), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('force-59')),
                     3,
                     [ np(Patient,
                          [ or(isa(Patient, tAnimate),
                               isa(Patient, tOrganization))
                          ])
                     ],
                     [prep(into), np(Result, [-sentential])],
                     A,
                     'force-59_f2') :-
    nop(english("I forced John into the chairmanship.")),
    into_lf((force(E, Agent, Patient, Result), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('force-59-1')),
                     2,
                     np(Patient,
                        [ or(isa(Patient, tAnimate),
                             isa(Patient, tOrganization))
                        ]),
                     np(Result, [+oc_to_inf]),
                     A,
                     'force-59-1_f0') :-
    nop(english("I forced him to come.")),
    into_lf((force(E, Agent, Patient, Result), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('free-80')),
                     3,
                     [ np(Source,
                          [ or(isa(Source, tAnimate),
                               isa(Source, tOrganization))
                          ])
                     ],
                     [prep(of), np(Theme, [-sentential])],
                     A,
                     'free-80_f0') :-
    nop(english("It freed him of guilt.")),
    into_lf((cause(Cause, E), ~free(StartE, Theme, Source), free(EndE, Theme, Source), or(isa(Source, tAnimate), isa(Source, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('free-80-1')),
                     3,
                     [ np(Source,
                          [ or(isa(Source, tAnimate),
                               isa(Source, tOrganization))
                          ])
                     ],
                     [prep(of), np(Theme, [+oc_ing])],
                     A,
                     'free-80-1_f0') :-
    nop(english("It freed him of feeling guilt.")),
    into_lf((cause(Cause, E), ~free(StartE, Theme, Source), free(EndE, Theme, Source), or(isa(Source, tAnimate), isa(Source, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('fulfilling-13.4.1')),
                     3,
                     [np(Theme)],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'fulfilling-13.4.1_f0') :-
    nop(english("Brown presented a plaque to Jones.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('fulfilling-13.4.1')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(with), np(Theme)],
                     A,
                     'fulfilling-13.4.1_f1') :-
    nop(english("Brown presented Jones with a plaque.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('fulfilling-13.4.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'fulfilling-13.4.1_f2') :-
    nop(english("Brown presented the plaque.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('fulfilling-13.4.1-1')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'fulfilling-13.4.1-1_f0') :-
    nop(english("Hess supplied its customers.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAbstract),
                             isa(Agent, tAnimate)),
                          or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('fulfilling-13.4.1-2')),
                     2,
                     prep(for),
                     np(Recipient,
                        [ or(isa(Recipient, tAbstract),
                             isa(Recipient, tAnimate)),
                          or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     A,
                     'fulfilling-13.4.1-2_f0') :-
    nop(english("Hess provides for its customers.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), or(isa(Recipient, tAbstract), isa(Recipient, tAnimate)), or(isa(Agent, tAbstract), isa(Agent, tAnimate)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('funnel-9.3')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+dest_conf]),
                       np(Destination, [-tRegion, +tLocation])
                     ],
                     A,
                     'funnel-9.3_f0') :-
    nop(english("I funneled the mixture into the bottle.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('funnel-9.3')),
                     2,
                     np(Theme, [+tConcrete]),
                     np(Destination, [-tRegion, +tLocation, +adv_loc]),
                     A,
                     'funnel-9.3_f1') :-
    nop(english("I spooned the sauce there.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('funnel-9.3-1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep((between;in;between)),
                       np(Destination, [-tRegion, +tLocation, +plural])
                     ],
                     A,
                     'funnel-9.3-1_f0') :-
    nop(english("He wedged the diamond between shifting dunes.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('funnel-9.3-1-1')),
                     2,
                     prep(into),
                     np(Destination, [-tRegion, +tLocation]),
                     A,
                     'funnel-9.3-1-1_f0') :-
    nop(english("The company is wedging into new markets.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('future_having-13.3')),
                     3,
                     [np(Theme)],
                     [prep(to), np(Goal)],
                     A,
                     'future_having-13.3_f0') :-
    nop(english("We offered our paycheck to her.")),
    into_lf((has_possession(StartE, Agent, Theme), future_possession(EndE, Goal, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('future_having-13.3')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'future_having-13.3_f1') :-
    nop(english("I promised my house (to somebody).")),
    into_lf((has_possession(StartE, Agent, Theme), future_possession(EndE, Goal, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('future_having-13.3')),
                     2,
                     np(Goal),
                     np(Theme),
                     A,
                     'future_having-13.3_f2') :-
    nop(english("I promised somebody my time.")),
    into_lf((has_possession(StartE, Agent, Theme), future_possession(EndE, Goal, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('future_having-13.3')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Goal)],
                     A,
                     'future_having-13.3_f3') :-
    nop(english("The House allocated more than $1.8 million for the pensions and expenses of former presidents.")),
    into_lf((has_possession(StartE, Agent, Theme), future_possession(EndE, Goal, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('get-13.5.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'get-13.5.1_f0') :-
    nop(english("Carmen bought a dress.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), ~isa(Asset, tLocation), ~isa(Asset, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('get-13.5.1')),
                     3,
                     [np(Theme)],
                     [prep(from), np(Source, [+tConcrete])],
                     A,
                     'get-13.5.1_f1') :-
    nop(english("Carmen bought a dress from Diana.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), ~isa(Asset, tLocation), ~isa(Asset, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('get-13.5.1')),
                     3,
                     [np(Theme)],
                     [ prep(for),
                       np(Beneficiary,
                          [ or(isa(Beneficiary, tAnimate),
                               isa(Beneficiary, tOrganization))
                          ])
                     ],
                     A,
                     'get-13.5.1_f2') :-
    nop(english("Carmen bought a dress for Mary.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), ~isa(Asset, tLocation), ~isa(Asset, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('get-13.5.1')),
                     2,
                     np(Beneficiary,
                        [ or(isa(Beneficiary, tAnimate),
                             isa(Beneficiary, tOrganization))
                        ]),
                     np(Theme),
                     A,
                     'get-13.5.1_f3') :-
    nop(english("Carmen bought Mary a dress.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), ~isa(Asset, tLocation), ~isa(Asset, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('get-13.5.1')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Asset, [-tRegion, -tLocation])],
                     A,
                     'get-13.5.1_f4') :-
    nop(english("Carmen bought a dress for $50.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), ~isa(Asset, tLocation), ~isa(Asset, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Asset, [-tRegion, -tLocation]),
                     verb(vn('get-13.5.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'get-13.5.1_f5') :-
    nop(english("$50 won't even buy a dress.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), ~isa(Asset, tLocation), ~isa(Asset, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('get-13.5.1')),
                     5,
                     [np(Theme)],
                     [ prep(from),
                       np(Source, [+tConcrete]),
                       prep((at;for)),
                       np(Asset, [-tRegion, -tLocation])
                     ],
                     A,
                     'get-13.5.1_f6') :-
    nop(english("FMC has bought 565, 000 shares from Nortek Inc. at $23.50 a share.")),
    into_lf((has_possession(StartE, Source, Theme), has_possession(EndE, Agent, Theme), transfer(E, Theme), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), ~isa(Asset, tLocation), ~isa(Asset, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('get-13.5.1-1')),
                     3,
                     [np(Theme)],
                     [ prep((from;for;on)),
                       np(Source, [+tAbstract, +tConcrete])
                     ],
                     A,
                     'get-13.5.1-1_f0') :-
    nop(english("John earned $10 million on proceeds from the sale.")),
    into_lf((has_possession(StartE, Source, Theme), has_possession(EndE, Agent, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), ~isa(Asset, tLocation), ~isa(Asset, tRegion), isa(Source, tAbstract), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('give-13.1')),
                     3,
                     [np(Theme)],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'give-13.1_f0') :-
    nop(english("They lent a bicycle to me.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('give-13.1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Theme),
                     A,
                     'give-13.1_f1') :-
    nop(english("They lent me a bicycle.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('give-13.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'give-13.1_f2') :-
    nop(english("I leased my house (to somebody).")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('give-13.1')),
                     2,
                     prep(to),
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     A,
                     'give-13.1_f3') :-
    nop(english("The bank lent to fewer customers.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), transfer(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('give-13.1-1')),
                     3,
                     [np(Theme)],
                     [prep((for;at)), np(Asset)],
                     A,
                     'give-13.1-1_f0') :-
    nop(english("He leased the car for $200 a week.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), has_possession(StartE, Recipient, Asset), has_possession(EndE, Agent, Asset), transfer(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('give-13.1-1')),
                     5,
                     [np(Theme)],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       prep((at;for;on)),
                       np(Asset)
                     ],
                     A,
                     'give-13.1-1_f1') :-
    nop(english("I leased the car to my friend for $5 a month.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), has_possession(StartE, Recipient, Asset), has_possession(EndE, Agent, Asset), transfer(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('give-13.1-1')),
                     4,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Theme)
                     ],
                     [prep((at;for;on)), np(Asset)],
                     A,
                     'give-13.1-1_f2') :-
    nop(english("I leased him the car for $250 a month.")),
    into_lf((has_possession(StartE, Agent, Theme), has_possession(EndE, Recipient, Theme), has_possession(StartE, Recipient, Asset), has_possession(EndE, Agent, Asset), transfer(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('gobble-39.3-1')),
                     1,
                     np(Patient, [+tSolid, +tComestible]),
                     [],
                     A,
                     'gobble-39.3-1_f0') :-
    nop(english("Cynthia gobbled the pizza.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('gobble-39.3-1')),
                     2,
                     np(Patient, [+tSolid, +tComestible]),
                     lex(up),
                     A,
                     'gobble-39.3-1_f1') :-
    nop(english("Cynthia gobbled the pizza up.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('gobble-39.3-1')),
                     2,
                     np(Patient, [+tSolid, +tComestible]),
                     lex(down),
                     A,
                     'gobble-39.3-1_f2') :-
    nop(english("Cynthia gobbled the pizza down.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('gobble-39.3-2')),
                     1,
                     np(Patient, [-tSolid, +tComestible]),
                     [],
                     A,
                     'gobble-39.3-2_f0') :-
    nop(english("Cynthia quaffed her mead.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), ~isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('gobble-39.3-2')),
                     2,
                     np(Patient, [-tSolid, +tComestible]),
                     lex(down),
                     A,
                     'gobble-39.3-2_f1') :-
    nop(english("Cynthia quaffed down the mixture.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), ~isa(Patient, tSolid), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('gorge-39.6')),
                     2,
                     prep(on),
                     np(Patient, [+tComestible]),
                     A,
                     'gorge-39.6_f0') :-
    nop(english("Cynthia gorged on peaches.")),
    into_lf((take_in(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tComestible), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('groom-41.1.2')),
                     1,
                     np(Patient, [+tAnimate]),
                     [],
                     A,
                     'groom-41.1.2_f0') :-
    nop(english("Sheila groomed the horse.")),
    into_lf((take_care_of(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Material, [+tConcrete]),
                     verb(vn('grow-26.2')),
                     2,
                     prep(into),
                     np(Product, [+tConcrete]),
                     A,
                     'grow-26.2_f0') :-
    nop(english("That acorn will grow into an oak tree.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), isa(Agent, tAnimate), isa(Material, tConcrete), isa(Product, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Product, [+tConcrete]),
                     verb(vn('grow-26.2')),
                     2,
                     prep((from;out;of)),
                     np(Material, [+tConcrete]),
                     A,
                     'grow-26.2_f1') :-
    nop(english("An oak tree will grow from that acorn.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), isa(Agent, tAnimate), isa(Material, tConcrete), isa(Product, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('grow-26.2')),
                     3,
                     [np(Product, [+tConcrete])],
                     [prep((from;out;of)), np(Material, [+tConcrete])],
                     A,
                     'grow-26.2_f2') :-
    nop(english("The gardener grew an oak tree from that acorn.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), isa(Agent, tAnimate), isa(Material, tConcrete), isa(Product, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('grow-26.2')),
                     3,
                     [np(Material, [+tConcrete])],
                     [prep(into), np(Product, [+tConcrete])],
                     A,
                     'grow-26.2_f3') :-
    nop(english("The gardener grew that acorn into an oak tree.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), cause(Agent, E), isa(Agent, tAnimate), isa(Material, tConcrete), isa(Product, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('help-72')),
                     1,
                     np(Beneficiary,
                        [ or(isa(Beneficiary, tAnimate),
                             isa(Beneficiary, tOrganization))
                        ]),
                     [],
                     A,
                     'help-72_f0') :-
    nop(english("I helped him.")),
    into_lf((help(E, Agent, Beneficiary, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('help-72')),
                     3,
                     [ np(Beneficiary,
                          [ or(isa(Beneficiary, tAnimate),
                               isa(Beneficiary, tOrganization))
                          ])
                     ],
                     [prep(with), np(Theme, [-sentential])],
                     A,
                     'help-72_f1') :-
    nop(english("I helped him with homework.")),
    into_lf((help(E, Agent, Beneficiary, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('help-72')),
                     3,
                     [ np(Beneficiary,
                          [ or(isa(Beneficiary, tAnimate),
                               isa(Beneficiary, tOrganization))
                          ])
                     ],
                     [prep(with), np(Theme, [+oc_ing])],
                     A,
                     'help-72_f2') :-
    nop(english("I helped him with finishing the homework.")),
    into_lf((help(E, Agent, Beneficiary, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('help-72')),
                     2,
                     np(Beneficiary,
                        [ or(isa(Beneficiary, tAnimate),
                             isa(Beneficiary, tOrganization))
                        ]),
                     np(Theme, [+oc_to_inf]),
                     A,
                     'help-72_f3') :-
    nop(english("I helped him to finish the homework.")),
    into_lf((help(E, Agent, Beneficiary, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('help-72')),
                     2,
                     prep(with),
                     np(Theme, [+ac_ing]),
                     A,
                     'help-72_f4') :-
    nop(english("I helped with finishing the homework.")),
    into_lf((help(E, Agent, Beneficiary, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('help-72-1')),
                     2,
                     prep(with),
                     np(Theme, [-sentential]),
                     A,
                     'help-72-1_f0') :-
    nop(english("I helped with the homework.")),
    into_lf((help(E, Agent, Beneficiary, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Beneficiary, tAnimate), isa(Beneficiary, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tPlural, +tConcrete]),
                     verb(vn('herd-47.5.2')),
                     0,
                     [],
                     [],
                     A,
                     'herd-47.5.2_f0') :-
    nop(english("The kids are assembling.")),
    into_lf((~together(StartE, Physical, Theme_i, Theme_j), together(EndE, Physical, Theme_i, Theme_j), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Theme, tPlural), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Physical, 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('herd-47.5.2')),
                     1,
                     np(Theme, [+tPlural, +tConcrete]),
                     [],
                     A,
                     'herd-47.5.2_f1') :-
    nop(english("The teacher gathered the kids.")),
    into_lf((cause(Agent, E), ~together(StartE, Physical, Theme_i, Theme_j), together(EndE, Physical, Theme_i, Theme_j), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Theme, tPlural), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Physical, 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('herd-47.5.2')),
                     2,
                     np(Theme, [+tPlural, +tConcrete]),
                     lex(together),
                     A,
                     'herd-47.5.2_f2') :-
    nop(english("The teacher gathered the kids together.")),
    into_lf((cause(Agent, E), ~together(StartE, Physical, Theme_i, Theme_j), together(EndE, Physical, Theme_i, Theme_j), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Theme, tPlural), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Physical, 'Constant'), isa(Theme_i, Pred), isa(Theme_j, Pred), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('hiccup-40.1.1')),
                     0,
                     [],
                     [],
                     A,
                     'hiccup-40.1.1_f0') :-
    nop(english("Paul hiccuped.")),
    into_lf((body_process(E, Agent), involuntary(E, Agent), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('hire-13.5.3')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tOrganization))
                        ]),
                     [],
                     A,
                     'hire-13.5.3_f0') :-
    nop(english("I hired two secretaries.")),
    into_lf((cause(Agent, E), ~location(StartE, Theme), location(EndE, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('hire-13.5.3')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tOrganization))
                          ])
                     ],
                     [prep(as), np(Attribute, [-sentential])],
                     A,
                     'hire-13.5.3_f1') :-
    nop(english("I hired two secretaries as helpers.")),
    into_lf((cause(Agent, E), ~location(StartE, Theme, Attribute), location(EndE, Theme, Attribute), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'hit-18.1_f0') :-
    nop(english("Paula hit the ball.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(with), np(Instrument, [+tConcrete])],
                     A,
                     'hit-18.1_f1') :-
    nop(english("Paula hit the ball with a stick.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1')),
                     2,
                     np(Patient, [+tConcrete, +plural]),
                     lex(together),
                     A,
                     'hit-18.1_f2') :-
    nop(english("Paula hit the sticks together.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Patient_i), manner(E, Directedmotion, Patient_j), ~contact(E, Patient_i, Patient_j), manner(EndE, Forceful, Patient_i), manner(EndE, Forceful, Patient_j), contact(EndE, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1')),
                     2,
                     np(Patient, [+tConcrete]),
                     np(Result),
                     A,
                     'hit-18.1_f3') :-
    nop(english("Paul kicked the door open.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1')),
                     4,
                     [np(Patient, [+tConcrete]), np(Result)],
                     [prep(with), np(Instrument, [+tConcrete])],
                     A,
                     'hit-18.1_f4') :-
    nop(english("Paul hit the door open with his foot.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'hit-18.1_f5') :-
    nop(english("Paul hit the window to pieces.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1')),
                     5,
                     [np(Patient, [+tConcrete])],
                     [ prep((to;into)),
                       np(Result, [+state]),
                       prep(with),
                       np(Instrument, [+tConcrete])
                     ],
                     A,
                     'hit-18.1_f6') :-
    nop(english("Paul hit the window to pieces with a hammer.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1')),
                     3,
                     [np(Instrument, [+tConcrete])],
                     [prep((against;on)), np(Patient, [+tConcrete])],
                     A,
                     'hit-18.1_f7') :-
    nop(english("Paula hit the stick against/on the fence.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('hit-18.1')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'hit-18.1_f8') :-
    nop(english("The stick hit the fence.")),
    into_lf((manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('hit-18.1')),
                     2,
                     np(Patient, [+tConcrete]),
                     np(Result),
                     A,
                     'hit-18.1_f9') :-
    nop(english("The stick hit the door open.")),
    into_lf((manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('hit-18.1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'hit-18.1_f10') :-
    nop(english("The hammer hit the window to pieces.")),
    into_lf((manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1-1')),
                     2,
                     lex(at),
                     np(Patient, [+tConcrete]),
                     A,
                     'hit-18.1-1_f0') :-
    nop(english("Paul hit at the window.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), or(isa(Instrument, tBodyPart), isa(Instrument, tRefl)), isa(E, actEvent), isa(Directedmotion, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1-1')),
                     4,
                     [],
                     [ lex(at),
                       np(Patient, [+tConcrete]),
                       prep(with),
                       np(Instrument,
                          [ or(isa(Instrument, tBodyPart),
                               isa(Instrument, tRefl)),
                            +tConcrete
                          ])
                     ],
                     A,
                     'hit-18.1-1_f1') :-
    nop(english("Paul hit at the window with an open hand.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), or(isa(Instrument, tBodyPart), isa(Instrument, tRefl)), isa(E, actEvent), isa(Directedmotion, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1-1')),
                     1,
                     np(Instrument,
                        [ or(isa(Instrument, tBodyPart),
                             isa(Instrument, tRefl)),
                          +tConcrete
                        ]),
                     [],
                     A,
                     'hit-18.1-1_f2') :-
    nop(english("Paul hit his elbow.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), or(isa(Instrument, tBodyPart), isa(Instrument, tRefl)), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('hit-18.1-1')),
                     3,
                     [ np(Instrument,
                          [ or(isa(Instrument, tBodyPart),
                               isa(Instrument, tRefl)),
                            +tConcrete
                          ])
                     ],
                     [prep((against;on)), np(Patient, [+tConcrete])],
                     A,
                     'hit-18.1-1_f3') :-
    nop(english("Paul hit his elbow on the doorknob.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), or(isa(Instrument, tBodyPart), isa(Instrument, tRefl)), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('hold-15.1')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'hold-15.1_f0') :-
    nop(english("She held the rail.")),
    into_lf((hold(E, Agent, Theme), contact(E, Agent, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('hold-15.1-1')),
                     1,
                     np(Theme, [+tBodyPart, +tConcrete]),
                     [],
                     A,
                     'hold-15.1-1_f0') :-
    nop(english("She held his arm.")),
    into_lf((hold(E, Agent, Theme), contact(E, Agent, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Theme, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('hunt-35.1')),
                     3,
                     [np(Location)],
                     [prep(for), np(Theme)],
                     A,
                     'hunt-35.1_f0') :-
    nop(english("I hunted the woods for game.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('hunt-35.1')),
                     4,
                     [],
                     [ prep(for),
                       np(Theme),
                       prep(Prep, [+loc]),
                       np(Location)
                     ],
                     A,
                     'hunt-35.1_f1') :-
    nop(english("I hunted for game in the woods.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('hunt-35.1')),
                     4,
                     [],
                     [ prep(Prep, [+loc]),
                       np(Location),
                       prep(for),
                       np(Theme)
                     ],
                     A,
                     'hunt-35.1_f2') :-
    nop(english("I hunted in the woods for game.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('hunt-35.1')),
                     3,
                     [np(Theme)],
                     [prep(Prep, [+loc]), np(Location)],
                     A,
                     'hunt-35.1_f3') :-
    nop(english("I hunted game in the woods.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('hunt-35.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'hunt-35.1_f4') :-
    nop(english("I was hunting game.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('hunt-35.1')),
                     0,
                     [],
                     [],
                     A,
                     'hunt-35.1_f5') :-
    nop(english("I was hunting.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('hurt-40.8.3-1')),
                     1,
                     np(Patient,
                        [ +tBodyPart,
                          or(isa(Patient, tBodyPart),
                             isa(Patient, tRefl))
                        ]),
                     [],
                     A,
                     'hurt-40.8.3-1_f0') :-
    nop(english("Tessa hurt/sprained her ankle.")),
    into_lf((harmed(E, Patient), experience(E, Experiencer), isa(Experiencer, tAnimate), or(isa(Patient, tBodyPart), isa(Patient, tRefl)), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient,
                        [ +tBodyPart,
                          or(isa(Patient, tBodyPart),
                             isa(Patient, tRefl))
                        ]),
                     verb(vn('hurt-40.8.3-1-1')),
                     0,
                     [],
                     [],
                     A,
                     'hurt-40.8.3-1-1_f0') :-
    nop(english("My ankle twisted.")),
    into_lf((harmed(E, Patient), experience(E, Experiencer), isa(Experiencer, tAnimate), or(isa(Patient, tBodyPart), isa(Patient, tRefl)), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('hurt-40.8.3-2')),
                     1,
                     np(Patient,
                        [ or(isa(Patient, tBodyPart),
                             isa(Patient, tRefl))
                        ]),
                     [],
                     A,
                     'hurt-40.8.3-2_f0') :-
    nop(english("Tessa hurt/sprained her ankle.")),
    into_lf((harmed(E, Patient), experience(E, Experiencer), isa(Experiencer, tAnimate), or(isa(Patient, tBodyPart), isa(Patient, tRefl)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('hurt-40.8.3-2')),
                     1,
                     np(Patient,
                        [ or(isa(Patient, tBodyPart),
                             isa(Patient, tRefl))
                        ]),
                     [],
                     A,
                     'hurt-40.8.3-2_f1') :-
    nop(english("Tessa hurt herself.")),
    into_lf((harmed(E, Patient), experience(E, Experiencer), isa(Experiencer, tAnimate), or(isa(Patient, tBodyPart), isa(Patient, tRefl)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('illustrate-25.3')),
                     1,
                     np(Destination, [+tConcrete]),
                     [],
                     A,
                     'illustrate-25.3_f0') :-
    nop(english("The jeweler decorated the ring.")),
    into_lf((created_image(ResultE, Theme), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Destination, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('illustrate-25.3')),
                     3,
                     [np(Destination, [+tConcrete])],
                     [prep(with), np(Theme)],
                     A,
                     'illustrate-25.3_f1') :-
    nop(english("The jeweler decorated the ring with the name.")),
    into_lf((created_image(ResultE, Theme), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Destination, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('image_impression-25.1')),
                     3,
                     [np(Theme)],
                     [ [or([+loc, +dest_conf])],
                       np(Destination, [+tConcrete])
                     ],
                     A,
                     'image_impression-25.1_f0') :-
    nop(english("Smith inscribed his name on the ring.")),
    into_lf((created_image(ResultE, Theme), holds(Prep, EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Destination, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('image_impression-25.1')),
                     3,
                     [np(Destination, [+tConcrete])],
                     [prep(with), np(Theme)],
                     A,
                     'image_impression-25.1_f1') :-
    nop(english("Smith inscribed the ring with his name.")),
    into_lf((created_image(ResultE, Theme), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Destination, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('image_impression-25.1')),
                     1,
                     np(Destination, [+tConcrete]),
                     [],
                     A,
                     'image_impression-25.1_f2') :-
    nop(english("Smith was inscribing the rings.")),
    into_lf((created_image(ResultE, Theme), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Destination, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('image_impression-25.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'image_impression-25.1_f3') :-
    nop(english("Smith inscribed his name.")),
    into_lf((created_image(ResultE, Theme), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Destination, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('image_impression-25.1')),
                     0,
                     [],
                     [],
                     A,
                     'image_impression-25.1_f4') :-
    nop(english("Smith was inscribing.")),
    into_lf((created_image(ResultE, Theme), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Destination, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('indicate-78')),
                     1,
                     np(Topic, [+how_extract]),
                     [],
                     A,
                     'indicate-78_f0') :-
    nop(english("This indicates how she did it.")),
    into_lf((indicate(E, Cause, Recipient, Topic), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('indicate-78')),
                     1,
                     np(Topic, [+wh_inf]),
                     [],
                     A,
                     'indicate-78_f1') :-
    nop(english("This indicates how to do it.")),
    into_lf((indicate(E, Cause, Recipient, Topic), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('indicate-78')),
                     1,
                     np(Topic, [-sentential]),
                     [],
                     A,
                     'indicate-78_f2') :-
    nop(english("This indicates it.")),
    into_lf((indicate(E, Cause, Recipient, Topic), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('indicate-78')),
                     1,
                     np(Topic, [+wh_comp]),
                     [],
                     A,
                     'indicate-78_f3') :-
    nop(english("This indicates whether he is the thief.")),
    into_lf((indicate(E, Cause, Recipient, Topic), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('indicate-78')),
                     1,
                     np(Topic, [+what_extract]),
                     [],
                     A,
                     'indicate-78_f4') :-
    nop(english("This indicates what he should do.")),
    into_lf((indicate(E, Cause, Recipient, Topic), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('indicate-78-1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+that_comp, -tensed_that])
                     ],
                     A,
                     'indicate-78-1_f0') :-
    nop(english("This indicates to us that they had stolen money.")),
    into_lf((indicate(E, Cause, Recipient, Topic), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('indicate-78-1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+wh_comp])
                     ],
                     A,
                     'indicate-78-1_f1') :-
    nop(english("This indicates to me whether they did it.")),
    into_lf((indicate(E, Cause, Recipient, Topic), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('indicate-78-1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+what_extract])
                     ],
                     A,
                     'indicate-78-1_f2') :-
    nop(english("This indicates to me what he is.")),
    into_lf((indicate(E, Cause, Recipient, Topic), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('indicate-78-1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+how_extract])
                     ],
                     A,
                     'indicate-78-1_f3') :-
    nop(english("This indicates to us how it happended.")),
    into_lf((indicate(E, Agent, Recipient, Topic), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('indicate-78-1-1')),
                     1,
                     np(Topic, [+to_be]),
                     [],
                     A,
                     'indicate-78-1-1_f0') :-
    nop(english("This indicates him to be the thief.")),
    into_lf((indicate(E, Cause, Recipient, Topic), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     1,
                     np(Topic, [+how_extract]),
                     [],
                     A,
                     'inquire-37.1.2_f0') :-
    nop(english("I asked how she did it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     1,
                     np(Topic, [+wh_inf]),
                     [],
                     A,
                     'inquire-37.1.2_f1') :-
    nop(english("I asked how to do it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     0,
                     [],
                     [],
                     A,
                     'inquire-37.1.2_f2') :-
    nop(english("I asked.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'inquire-37.1.2_f3') :-
    nop(english("I asked him.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     1,
                     np(Topic, [-sentential]),
                     [],
                     A,
                     'inquire-37.1.2_f4') :-
    nop(english("I asked a question.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(about), np(Topic, [-sentential])],
                     A,
                     'inquire-37.1.2_f5') :-
    nop(english("I asked him about it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+wh_comp]),
                     A,
                     'inquire-37.1.2_f6') :-
    nop(english("I asked him whether he was going.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+what_extract]),
                     A,
                     'inquire-37.1.2_f7') :-
    nop(english("I asked him what he was doing.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+wheth_inf]),
                     A,
                     'inquire-37.1.2_f8') :-
    nop(english("I asked him whether to clean the house.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+what_inf]),
                     A,
                     'inquire-37.1.2_f9') :-
    nop(english("He asked him what to do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     2,
                     prep(about),
                     np(Topic, [+wh_comp]),
                     A,
                     'inquire-37.1.2_f10') :-
    nop(english("He asked about whether he wanted to go.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(about), np(Topic, [+what_extract])],
                     A,
                     'inquire-37.1.2_f11') :-
    nop(english("He asked him about what he wanted.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     2,
                     prep(about),
                     np(Topic, [+wheth_inf]),
                     A,
                     'inquire-37.1.2_f12') :-
    nop(english("He asked about whether to go.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     2,
                     prep(about),
                     np(Topic, [+what_inf]),
                     A,
                     'inquire-37.1.2_f13') :-
    nop(english("He asked about what to do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     2,
                     prep(about),
                     np(Topic, [-sentential]),
                     A,
                     'inquire-37.1.2_f14') :-
    nop(english("I asked about it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     3,
                     [],
                     [ prep(of),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+how_extract])
                     ],
                     A,
                     'inquire-37.1.2_f15') :-
    nop(english("He asked of her how it works.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     3,
                     [],
                     [ prep(of),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+wh_inf])
                     ],
                     A,
                     'inquire-37.1.2_f16') :-
    nop(english("He asked of her how to do it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     4,
                     [],
                     [ prep(of),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+wh_comp])
                     ],
                     A,
                     'inquire-37.1.2_f17') :-
    nop(english("He asked of her about whether he should kill the peasants.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     4,
                     [],
                     [ prep(of),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+what_extract])
                     ],
                     A,
                     'inquire-37.1.2_f18') :-
    nop(english("He asked of her about what he should do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     4,
                     [],
                     [ prep(of),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+what_inf])
                     ],
                     A,
                     'inquire-37.1.2_f19') :-
    nop(english("He asked of her about what to do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     4,
                     [],
                     [ prep(of),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+wheth_inf])
                     ],
                     A,
                     'inquire-37.1.2_f20') :-
    nop(english("I asked of him about whether to go.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     1,
                     np(Topic, [+wh_comp]),
                     [],
                     A,
                     'inquire-37.1.2_f21') :-
    nop(english("He asked whether he should come.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     1,
                     np(Topic, [+what_extract]),
                     [],
                     A,
                     'inquire-37.1.2_f22') :-
    nop(english("He asked what he should do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     1,
                     np(Topic, [+wheth_inf]),
                     [],
                     A,
                     'inquire-37.1.2_f23') :-
    nop(english("He asked whether to clean the house.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     1,
                     np(Topic, [+what_inf]),
                     [],
                     A,
                     'inquire-37.1.2_f24') :-
    nop(english("He asked what to do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('inquire-37.1.2')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+how_extract]),
                     A,
                     'inquire-37.1.2_f25') :-
    nop(english("He asked him how he came.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     1,
                     np(Topic, [+tCommunication]),
                     [],
                     A,
                     'instr_communication-37.4_f0') :-
    nop(english("Heather cabled the news.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'instr_communication-37.4_f1') :-
    nop(english("Heather cabled Sara.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     3,
                     [np(Topic, [+tCommunication])],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'instr_communication-37.4_f2') :-
    nop(english("Heather cabled the news to Sara.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication]),
                     A,
                     'instr_communication-37.4_f3') :-
    nop(english("Heather cabled Sara the news.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication]),
                     A,
                     'instr_communication-37.4_f4') :-
    nop(english("Heather cabled Sara, 'Come immediately.'")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication])
                     ],
                     A,
                     'instr_communication-37.4_f5') :-
    nop(english("Heather cabled to Sara, 'Come immediately.'")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(about), np(Topic, [+tCommunication])],
                     A,
                     'instr_communication-37.4_f6') :-
    nop(english("Heather cabled Sara about the situation.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     1,
                     np(Topic, [+tCommunication, +that_comp]),
                     [],
                     A,
                     'instr_communication-37.4_f7') :-
    nop(english("Heather cabled that the party would be tonight.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     1,
                     np(Topic, [+tCommunication, +wh_inf]),
                     [],
                     A,
                     'instr_communication-37.4_f8') :-
    nop(english("Heather cabled when to send the package.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     1,
                     np(Topic, [+tCommunication, +ac_to_inf]),
                     [],
                     A,
                     'instr_communication-37.4_f9') :-
    nop(english("Heather cabled when to send the package.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication, +that_comp]),
                     A,
                     'instr_communication-37.4_f10') :-
    nop(english("Heather cabled Sara that the party would be tonight.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication, +wh_inf]),
                     A,
                     'instr_communication-37.4_f11') :-
    nop(english("Heather cabled Sara when to send the package.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication, +ac_to_inf]),
                     A,
                     'instr_communication-37.4_f12') :-
    nop(english("Heather cabled Sara to come.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +that_comp])
                     ],
                     A,
                     'instr_communication-37.4_f13') :-
    nop(english("Heather cabled to Sara that the party would be tonight.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +wh_inf])
                     ],
                     A,
                     'instr_communication-37.4_f14') :-
    nop(english("Heather cabled to Sara when to send the package.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +ac_to_inf])
                     ],
                     A,
                     'instr_communication-37.4_f15') :-
    nop(english("Heather cabled to Sara to come.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('instr_communication-37.4')),
                     3,
                     [],
                     [ prep(for),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +for_comp])
                     ],
                     A,
                     'instr_communication-37.4_f16') :-
    nop(english("Heather cabled for Sara to come.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('interrogate-37.1.3')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'interrogate-37.1.3_f0') :-
    nop(english("I interrogated him.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('interrogate-37.1.3')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(as), np(Attribute, [-sentential])],
                     A,
                     'interrogate-37.1.3_f1') :-
    nop(english("I interrogated him as a suspect.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic, Attribute), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('interrogate-37.1.3')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(about), np(Topic, [-sentential])],
                     A,
                     'interrogate-37.1.3_f2') :-
    nop(english("I interrogated him about the incident.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('investigate-35.4')),
                     3,
                     [np(Location)],
                     [prep(for), np(Theme)],
                     A,
                     'investigate-35.4_f0') :-
    nop(english("We investigated the area for bombs.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('investigate-35.4')),
                     1,
                     np(Location),
                     [],
                     A,
                     'investigate-35.4_f1') :-
    nop(english("We inspected the area.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('involve-107')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tOrganization))
                        ]),
                     [],
                     A,
                     'involve-107_f0') :-
    nop(english("They thoughtlessly involved me.")),
    into_lf((involve(E, Agent, Theme, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('involve-107')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tOrganization))
                          ])
                     ],
                     [prep(in), np(Goal)],
                     A,
                     'involve-107_f1') :-
    nop(english("They thoughtlessly involved me in their affairs.")),
    into_lf((involve(E, Agent, Theme, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Theme, tAnimate), isa(Theme, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tHuman]),
                     verb(vn('judgment-33')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'judgment-33_f0') :-
    nop(english("They praised the volunteers.")),
    into_lf((declare(E, Agent, Theme, Attribute), isa(Agent, tHuman), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tHuman]),
                     verb(vn('judgment-33')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Attribute)],
                     A,
                     'judgment-33_f1') :-
    nop(english("They praised the volunteers for their dedication.")),
    into_lf((declare(E, Agent, Theme, Attribute), isa(Agent, tHuman), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tHuman]),
                     verb(vn('judgment-33')),
                     1,
                     np(Theme, [+genitive]),
                     [],
                     A,
                     'judgment-33_f2') :-
    nop(english("They praised the volunteer's dedication.")),
    into_lf((declare(E, Agent, Theme, Attribute), isa(Agent, tHuman), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tHuman]),
                     verb(vn('judgment-33')),
                     3,
                     [np(Theme)],
                     [prep(as), np(Attribute)],
                     A,
                     'judgment-33_f3') :-
    nop(english("I judged him as a good man.")),
    into_lf((declare(E, Agent, Theme, Attribute), isa(Agent, tHuman), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tHuman]),
                     verb(vn('judgment-33')),
                     2,
                     np(Theme),
                     np(Attribute, [+small_clause]),
                     A,
                     'judgment-33_f4') :-
    nop(english("I judged him to be a good man.")),
    into_lf((declare(E, Agent, Theme, Attribute), isa(Agent, tHuman), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('keep-15.2')),
                     3,
                     [np(Theme)],
                     [ prep(Prep, [+loc]),
                       np(Location, [-tRegion, +tLocation])
                     ],
                     A,
                     'keep-15.2_f0') :-
    nop(english("Michelle kept the papers in the desk.")),
    into_lf((holds(Prep, E, Theme, Location), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('keep-15.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'keep-15.2_f1') :-
    nop(english("Many small investors in Belgium store securities.")),
    into_lf((has_possession(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Material),
                     verb(vn('knead-26.5')),
                     2,
                     prep(into),
                     np(Product),
                     A,
                     'knead-26.5_f0') :-
    nop(english("The dough twirled into a pretzel.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), ~state(StartE, Endstate, Material), state(ResultE, Endstate, Material), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE), isa(Endstate, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('knead-26.5')),
                     3,
                     [np(Material)],
                     [prep(into), np(Product)],
                     A,
                     'knead-26.5_f1') :-
    nop(english("I kneaded the dough into a loaf.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), made_of(ResultE, Product, Material), ~state(StartE, Endstate, Material), state(ResultE, Endstate, Material), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE), isa(Endstate, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('knead-26.5')),
                     1,
                     np(Material),
                     [],
                     A,
                     'knead-26.5_f2') :-
    nop(english("I kneaded the dough.")),
    into_lf((~state(StartE, Endstate, Material), state(ResultE, Endstate, Material), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Endstate, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('learn-14')),
                     3,
                     [np(Topic)],
                     [prep(from), np(Source)],
                     A,
                     'learn-14_f0') :-
    nop(english("Rhoda learned French from an old book.")),
    into_lf((transfer_info(E, Source, Agent, Topic), cause(Agent, E), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('learn-14')),
                     2,
                     prep(from),
                     np(Source),
                     A,
                     'learn-14_f1') :-
    nop(english("Rhoda learned from an old book.")),
    into_lf((transfer_info(E, Source, Agent, Topic), cause(Agent, E), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('learn-14')),
                     1,
                     np(Topic),
                     [],
                     A,
                     'learn-14_f2') :-
    nop(english("Kissinger learned his lesson.")),
    into_lf((transfer_info(E, Source, Agent, Topic), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('learn-14-1')),
                     0,
                     [],
                     [],
                     A,
                     'learn-14-1_f0') :-
    nop(english("She studied.")),
    into_lf((transfer_info(E, Source, Agent, Topic), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('learn-14-2')),
                     1,
                     np(Topic, [+that_comp]),
                     [],
                     A,
                     'learn-14-2_f0') :-
    nop(english("The defense also learned that college-educated people were uncharacteristically conservative.")),
    into_lf((transfer_info(E, Source, Agent, Topic), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('learn-14-2-1')),
                     2,
                     prep((of;about)),
                     np(Topic),
                     A,
                     'learn-14-2-1_f0') :-
    nop(english("The president learned of a coup plot that might endanger his life.")),
    into_lf((transfer_info(E, Source, Agent, Topic), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('leave-51.2')),
                     1,
                     np(Initial_Location, [-tRegion, +tLocation]),
                     [],
                     A,
                     'leave-51.2_f0') :-
    nop(english("We abandoned the area.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), ~location(EndE, Theme, Initial_Location), direction(E, From, Theme, Initial_Location), isa(Theme, tAnimate), isa(Initial_Location, tLocation), ~isa(Initial_Location, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE), isa(From, 'Constant')),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('leave-51.2-1')),
                     0,
                     [],
                     [],
                     A,
                     'leave-51.2-1_f0') :-
    nop(english("The crowd left.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), ~location(EndE, Theme, Initial_Location), direction(E, From, Theme, Initial_Location), isa(Theme, tAnimate), isa(Initial_Location, tLocation), ~isa(Initial_Location, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE), isa(From, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('lecture-37.11')),
                     2,
                     prep(about),
                     np(Topic, [+ac_ing]),
                     A,
                     'lecture-37.11_f0') :-
    nop(english("She lectured about traveling.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('lecture-37.11-1')),
                     0,
                     [],
                     [],
                     A,
                     'lecture-37.11-1_f0') :-
    nop(english("She lectured.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('lecture-37.11-1')),
                     2,
                     prep(about),
                     np(Topic, [-sentential]),
                     A,
                     'lecture-37.11-1_f1') :-
    nop(english("She lectured about her travels.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('lecture-37.11-1')),
                     4,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [-sentential])
                     ],
                     A,
                     'lecture-37.11-1_f2') :-
    nop(english("She lectured to the class about her travels.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('lecture-37.11-1-1')),
                     2,
                     prep(about),
                     np(Topic, [+poss_ing]),
                     A,
                     'lecture-37.11-1-1_f0') :-
    nop(english("She lectured about her traveling.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('lecture-37.11-2')),
                     2,
                     prep(about),
                     np(Topic, [-sentential]),
                     A,
                     'lecture-37.11-2_f0') :-
    nop(english("She dwelled on her travels.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('lecture-37.11-2')),
                     2,
                     prep(on),
                     np(Topic, [+poss_ing]),
                     A,
                     'lecture-37.11-2_f1') :-
    nop(english("She dwelled on her traveling.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [-tAnimate]),
                     verb(vn('light_emission-43.1')),
                     0,
                     [],
                     [],
                     A,
                     'light_emission-43.1_f0') :-
    nop(english("The jewel sparkled.")),
    into_lf((emit(E, Theme, Prop), ~isa(Theme, tAnimate), isa(E, actEvent), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Theme, [-tAnimate]),
                     verb(vn('light_emission-43.1')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location),
                     A,
                     'light_emission-43.1_f1') :-
    nop(english("Jewels sparkled on the crown.")),
    into_lf((emit(E, Theme, Prop), holds(Prep, E, Prop, Location), ~isa(Theme, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Location),
                     verb(vn('light_emission-43.1')),
                     2,
                     prep(with),
                     np(Theme, [-tAnimate]),
                     A,
                     'light_emission-43.1_f2') :-
    nop(english("The crown sparkled with jewels.")),
    into_lf((emit(E, Theme, Prop), location(E, Prop, Location), ~isa(Theme, tAnimate), isa(E, actEvent), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('light_emission-43.1')),
                     3,
                     [np(Theme, [-tAnimate])],
                     [prep(Prep, [+loc]), np(Location)],
                     A,
                     'light_emission-43.1_f3') :-
    nop(english("There sparkled a magnificent diamond on his finger.")),
    into_lf((emit(E, Theme, Prop), holds(Prep, E, Prop, Location), ~isa(Theme, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(prep(Prep, [+loc]),
                     np(Location),
                     3,
                     [],
                     [ lex(there),
                       verb(vn('light_emission-43.1')),
                       np(Theme, [-tAnimate])
                     ],
                     A,
                     'light_emission-43.1_f4') :-
    nop(english("On his finger there sparkled a magnificent diamond.")),
    into_lf((emit(E, Theme, Prop), holds(Prep, E, Prop, Location), ~isa(Theme, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent),
                     verb(vn('light_emission-43.1')),
                     1,
                     np(Theme, [-tAnimate]),
                     [],
                     A,
                     'light_emission-43.1_f5') :-
    nop(english("The stagehand flashed the lights.")),
    into_lf((emit(E, Theme, Prop), cause(Agent, E), ~isa(Theme, tAnimate), isa(E, actEvent), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('limit-76')),
                     1,
                     np(Patient,
                        [ or(isa(Patient, tAnimate),
                             isa(Patient, tOrganization))
                        ]),
                     [],
                     A,
                     'limit-76_f0') :-
    nop(english("The lack of money limited the help.")),
    into_lf((limit(E, Cause, Patient, Goal), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('limit-76')),
                     3,
                     [ np(Patient,
                          [ or(isa(Patient, tAnimate),
                               isa(Patient, tOrganization))
                          ])
                     ],
                     [prep(to), np(Goal, [+oc_ing])],
                     A,
                     'limit-76_f1') :-
    nop(english("The lack of money limited them to helping to our peers.")),
    into_lf((limit(E, Cause, Patient, Goal), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('limit-76')),
                     3,
                     [ np(Patient,
                          [ or(isa(Patient, tAnimate),
                               isa(Patient, tOrganization))
                          ])
                     ],
                     [prep(to), np(Goal)],
                     A,
                     'limit-76_f2') :-
    nop(english("The lack of money limited the help to our peers.")),
    into_lf((limit(E, Cause, Patient, Goal), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('limit-76')),
                     2,
                     np(Patient,
                        [ or(isa(Patient, tAnimate),
                             isa(Patient, tOrganization))
                        ]),
                     np(Goal, [+oc_to_inf]),
                     A,
                     'limit-76_f3') :-
    nop(english("The lack of money limited them to help their peers only.")),
    into_lf((limit(E, Cause, Patient, Goal), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('linger-53.1')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location),
                     A,
                     'linger-53.1_f0') :-
    nop(english("Sasha dawdled in the museum.")),
    into_lf((linger(E, Agent, Theme), holds(Prep, E, Agent, Location), isa(Agent, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('linger-53.1')),
                     2,
                     prep(over),
                     np(Theme),
                     A,
                     'linger-53.1_f1') :-
    nop(english("Sasha dawdled over lunch.")),
    into_lf((linger(E, Agent, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('linger-53.1-1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'linger-53.1-1_f0') :-
    nop(english("He delayed the watering.")),
    into_lf((delay(E, Agent, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('lodge-46')),
                     2,
                     prep((at;in;with)),
                     np(Location),
                     A,
                     'lodge-46_f0') :-
    nop(english("Cornelia lodged with the Stevensons.")),
    into_lf((~location(StartE, Theme, Location), location(E, Theme, Location), isa(Theme, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('lodge-46')),
                     1,
                     np(Location, [+adv_loc]),
                     [],
                     A,
                     'lodge-46_f1') :-
    nop(english("We camped there.")),
    into_lf((~location(StartE, Theme, Location), location(E, Theme, Location), isa(Theme, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Pivot),
                     verb(vn('long-32.2')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'long-32.2_f0') :-
    nop(english("John hankered to move up the corporate ladder.")),
    into_lf((desire(E, Pivot, Theme), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot),
                     verb(vn('long-32.2-1')),
                     2,
                     prep(for),
                     np(Theme),
                     A,
                     'long-32.2-1_f0') :-
    nop(english("Danny longs for a sunny day.")),
    into_lf((desire(E, Pivot, Theme), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot),
                     verb(vn('long-32.2-2')),
                     2,
                     prep(after),
                     np(Theme),
                     A,
                     'long-32.2-2_f0') :-
    nop(english("Danny yearns after sunny days.")),
    into_lf((desire(E, Pivot, Theme), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     0,
                     [],
                     [],
                     A,
                     'manner_speaking-37.3_f0') :-
    nop(english("Susan whispered.")),
    into_lf((cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     2,
                     prep(Prep, [+dest_dir]),
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     A,
                     'manner_speaking-37.3_f1') :-
    nop(english("Susan whispered to/at Rachel.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     2,
                     prep(about),
                     np(Topic, [+tCommunication]),
                     A,
                     'manner_speaking-37.3_f2') :-
    nop(english("Susan whispered about the party.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     1,
                     np(Topic, [+tCommunication]),
                     [],
                     A,
                     'manner_speaking-37.3_f3') :-
    nop(english("Susan whispered the news/a few words.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     1,
                     np(Topic, [+tCommunication]),
                     [],
                     A,
                     'manner_speaking-37.3_f4') :-
    nop(english("Susan whispered, 'Leave the room.'")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     3,
                     [np(Topic, [+tCommunication])],
                     [ prep(Prep, [+dest_dir]),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'manner_speaking-37.3_f5') :-
    nop(english("Susan whispered the news/a few words to/at Rachel.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     1,
                     np(Topic, [+tCommunication, +that_comp]),
                     [],
                     A,
                     'manner_speaking-37.3_f6') :-
    nop(english("Susan whispered that the party would be tonight.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     1,
                     np(Topic, [+tCommunication, +wh_inf]),
                     [],
                     A,
                     'manner_speaking-37.3_f7') :-
    nop(english("Susan whispered how to avoid the crowd.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     1,
                     np(Topic, [+tCommunication, +ac_to_inf]),
                     [],
                     A,
                     'manner_speaking-37.3_f8') :-
    nop(english("Susan whispered to come.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     3,
                     [],
                     [ prep(Prep, [+dest_dir]),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +that_comp])
                     ],
                     A,
                     'manner_speaking-37.3_f9') :-
    nop(english("Susan whispered to Rachel that the party would be tonight.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     3,
                     [],
                     [ prep(Prep, [+dest_dir]),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +wh_inf])
                     ],
                     A,
                     'manner_speaking-37.3_f10') :-
    nop(english("Susan whispered to Rachel how to avoid the crowd.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     3,
                     [],
                     [ prep(Prep, [+dest_dir]),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +oc_to_inf])
                     ],
                     A,
                     'manner_speaking-37.3_f11') :-
    nop(english("Susan whispered to Rachel to come.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     3,
                     [],
                     [ prep(for),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +oc_to_inf])
                     ],
                     A,
                     'manner_speaking-37.3_f12') :-
    nop(english("Susan whispered for me to come.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('manner_speaking-37.3')),
                     3,
                     [],
                     [ prep(Prep, [+dest_dir]),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication])
                     ],
                     A,
                     'manner_speaking-37.3_f13') :-
    nop(english("Susan whispered to Rachel, 'Leave the room.'")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate, +plural]),
                     verb(vn('marry-36.2')),
                     0,
                     [],
                     [],
                     A,
                     'marry-36.2_f0') :-
    nop(english("Bill and Kathy married.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), isa(Agent, tAnimate), isa(Co_Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('marry-36.2')),
                     1,
                     np(Co_Agent, [+tAnimate]),
                     [],
                     A,
                     'marry-36.2_f1') :-
    nop(english("Bill married Kathy.")),
    into_lf((social_interaction(E, Agent, Co_Agent), isa(Agent, tAnimate), isa(Co_Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('marvel-31.3')),
                     2,
                     prep(by),
                     np(Experiencer, [+tAnimate]),
                     A,
                     'marvel-31.3_f0') :-
    nop(english("The beauty of the Grand Canyon has been marveled over by countless tourists.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('marvel-31.3')),
                     0,
                     [],
                     [],
                     A,
                     'marvel-31.3_f1') :-
    nop(english("I will suffer.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('marvel-31.3-1')),
                     2,
                     prep(at),
                     np(Stimulus),
                     A,
                     'marvel-31.3-1_f0') :-
    nop(english("Megan marveled at the Grand Canyon.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('marvel-31.3-2')),
                     2,
                     prep(about),
                     np(Stimulus),
                     A,
                     'marvel-31.3-2_f0') :-
    nop(english("We cared about the opera.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('marvel-31.3-3')),
                     2,
                     prep(for),
                     np(Stimulus),
                     A,
                     'marvel-31.3-3_f0') :-
    nop(english("I grieve for my departed Juliet.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('marvel-31.3-4')),
                     2,
                     prep(from),
                     np(Stimulus),
                     A,
                     'marvel-31.3-4_f0') :-
    nop(english("I suffered from occasional blackouts.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('marvel-31.3-5')),
                     2,
                     prep(in),
                     np(Stimulus),
                     A,
                     'marvel-31.3-5_f0') :-
    nop(english("I delighted in such pleasant pursuits.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('marvel-31.3-6')),
                     2,
                     prep(of),
                     np(Stimulus),
                     A,
                     'marvel-31.3-6_f0') :-
    nop(english("I certainly approved of Mary's acquaitance.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('marvel-31.3-7')),
                     2,
                     prep(on),
                     np(Stimulus),
                     A,
                     'marvel-31.3-7_f0') :-
    nop(english("I grooved on David Lynch films.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('marvel-31.3-8')),
                     2,
                     prep(over),
                     np(Stimulus),
                     A,
                     'marvel-31.3-8_f0') :-
    nop(english("I anguished over Aslan's pain.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('marvel-31.3-9')),
                     2,
                     prep(to),
                     np(Stimulus),
                     A,
                     'marvel-31.3-9_f0') :-
    nop(english("I reacted to the experience violently.")),
    into_lf((emotional_state(ResultE, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('masquerade-29.6')),
                     2,
                     lex(as),
                     np(Attribute),
                     A,
                     'masquerade-29.6_f0') :-
    nop(english("Dina masqueraded as a lawyer.")),
    into_lf((masquerade(E, Agent, Attribute), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('masquerade-29.6-1')),
                     2,
                     lex(like),
                     np(Attribute),
                     A,
                     'masquerade-29.6-1_f0') :-
    nop(english("Dina acted like a cretin.")),
    into_lf((masquerade(E, Agent, Attribute), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('masquerade-29.6-2')),
                     1,
                     np(Attribute),
                     [],
                     A,
                     'masquerade-29.6-2_f0') :-
    nop(english("They serve a restricted role on the bench.")),
    into_lf((masquerade(E, Agent, Attribute), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('masquerade-29.6-2')),
                     2,
                     prep(in),
                     np(Attribute),
                     A,
                     'masquerade-29.6-2_f1') :-
    nop(english("He had served in financial planning positions.")),
    into_lf((masquerade(E, Agent, Attribute), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('matter-91')),
                     0,
                     [],
                     [],
                     A,
                     'matter-91_f0') :-
    nop(english("It matters.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(Experiencer, tOrganization), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('matter-91')),
                     2,
                     prep(to),
                     np(Experiencer, [+tOrganization, +tAnimate]),
                     A,
                     'matter-91_f1') :-
    nop(english("It matters to me.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(Experiencer, tOrganization), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(lex(it),
                     verb(vn('matter-91')),
                     3,
                     [],
                     [ prep(to),
                       np(Experiencer, [+tOrganization, +tAnimate]),
                       np(Stimulus, [+that_comp])
                     ],
                     A,
                     'matter-91_f2') :-
    nop(english("It matters to me that they were wrong.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(Experiencer, tOrganization), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(lex(it),
                     verb(vn('matter-91')),
                     1,
                     np(Stimulus, [+that_comp]),
                     [],
                     A,
                     'matter-91_f3') :-
    nop(english("It matters that they left.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(Experiencer, tOrganization), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Stimulus, [+that_comp]),
                     verb(vn('matter-91')),
                     0,
                     [],
                     [],
                     A,
                     'matter-91_f4') :-
    nop(english("That he came matters.")),
    into_lf((emotional_state(E, Emotion, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(Experiencer, tOrganization), isa(E, actEvent), isa(Emotion, Pred)),
            A).
vndata:verbnet_frame(np(Theme, [+tElongated]),
                     verb(vn('meander-47.7')),
                     2,
                     prep(Prep, [+path]),
                     np(Location, [+tConcrete]),
                     A,
                     'meander-47.7_f0') :-
    nop(english("The river runs through the valley.")),
    into_lf((holds(Prep, E, Theme, Location), exist(E, Theme), isa(Location, tConcrete), isa(Theme, tElongated), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(prep(Prep, [+path]),
                     np(Location, [+tConcrete]),
                     2,
                     verb(vn('meander-47.7')),
                     np(Theme, [+tElongated]),
                     A,
                     'meander-47.7_f1') :-
    nop(english("Through the valley meanders the river.")),
    into_lf((holds(Prep, E, Theme, Location), exist(E, Theme), isa(Location, tConcrete), isa(Theme, tElongated), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('meander-47.7')),
                     3,
                     [],
                     [ prep(Prep, [+path]),
                       np(Location, [+tConcrete]),
                       np(Theme, [+tElongated])
                     ],
                     A,
                     'meander-47.7_f2') :-
    nop(english("There meanders through the valley a river.")),
    into_lf((holds(Prep, E, Theme, Location), exist(E, Theme), isa(Location, tConcrete), isa(Theme, tElongated), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('meander-47.7')),
                     3,
                     [np(Theme, [+tElongated])],
                     [ prep(Prep, [+path]),
                       np(Location, [+tConcrete])
                     ],
                     A,
                     'meander-47.7_f3') :-
    nop(english("There meanders a river through the valley.")),
    into_lf((holds(Prep, E, Theme, Location), exist(E, Theme), isa(Location, tConcrete), isa(Theme, tElongated), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('meet-36.3-1')),
                     2,
                     prep(with),
                     np(Co_Agent, [+tAnimate]),
                     A,
                     'meet-36.3-1_f0') :-
    nop(english("Brenda met with Molly.")),
    into_lf((social_interaction(E, Agent, Co_Agent), isa(Agent, tAnimate), isa(Co_Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate, +plural]),
                     verb(vn('meet-36.3-1')),
                     0,
                     [],
                     [],
                     A,
                     'meet-36.3-1_f1') :-
    nop(english("Brenda and Molly met.")),
    into_lf((social_interaction(E, Agent, Co_Agent), isa(Agent, tAnimate), isa(Co_Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('meet-36.3-1')),
                     1,
                     np(Co_Agent, [+tAnimate]),
                     [],
                     A,
                     'meet-36.3-1_f2') :-
    nop(english("Anne met Cathy.")),
    into_lf((social_interaction(E, Agent, Co_Agent), isa(Agent, tAnimate), isa(Co_Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate, +plural]),
                     verb(vn('meet-36.3-1')),
                     0,
                     [],
                     [],
                     A,
                     'meet-36.3-1_f3') :-
    nop(english("The committee met.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), isa(Agent, tAnimate), isa(Co_Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('meet-36.3-2')),
                     2,
                     prep(with),
                     np(Co_Agent, [+tAnimate]),
                     A,
                     'meet-36.3-2_f0') :-
    nop(english("Brenda fought with Molly.")),
    into_lf((social_interaction(E, Agent, Co_Agent), conflict(E, Agent, Co_Agent), isa(Agent, tAnimate), isa(Co_Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate, +plural]),
                     verb(vn('meet-36.3-2')),
                     0,
                     [],
                     [],
                     A,
                     'meet-36.3-2_f1') :-
    nop(english("Brenda and Molly fought.")),
    into_lf((social_interaction(E, Agent, Co_Agent), conflict(E, Agent, Co_Agent), isa(Agent, tAnimate), isa(Co_Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('meet-36.3-2')),
                     1,
                     np(Co_Agent, [+tAnimate]),
                     [],
                     A,
                     'meet-36.3-2_f2') :-
    nop(english("Anne fought Cathy.")),
    into_lf((social_interaction(E, Agent, Co_Agent), conflict(E, Agent, Co_Agent), isa(Agent, tAnimate), isa(Co_Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate, +plural]),
                     verb(vn('meet-36.3-2')),
                     0,
                     [],
                     [],
                     A,
                     'meet-36.3-2_f3') :-
    nop(english("The populace fought.")),
    into_lf((social_interaction(E, Agent_i, Agent_j), conflict(E, Agent_i, Agent_j), isa(Agent, tAnimate), isa(Co_Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('mine-10.9')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'mine-10.9_f0') :-
    nop(english("The men mined the gold.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Initial_Location), ~location(EndE, Theme, Initial_Location), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('mine-10.9')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'mine-10.9_f1') :-
    nop(english("The men mined the gold from the abandoned mine.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Initial_Location), ~location(EndE, Theme, Initial_Location), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('mix-22.1-1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((with;into;to)), np(Co_Patient, [+tConcrete])],
                     A,
                     'mix-22.1-1_f0') :-
    nop(english("Herman mixed the eggs with the cream.")),
    into_lf((cause(Agent, E), degradation_material_integrity(ResultE, Patient), degradation_material_integrity(ResultE, Co_Patient), mingled(ResultE, Physical, Patient, Co_Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('mix-22.1-1')),
                     1,
                     np(Patient, [+tConcrete, +plural]),
                     [],
                     A,
                     'mix-22.1-1_f1') :-
    nop(english("Herman mixed the eggs.")),
    into_lf((cause(Agent, E), degradation_material_integrity(ResultE, Patient_i), degradation_material_integrity(ResultE, Patient_j), mingled(ResultE, Physical, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('mix-22.1-1')),
                     2,
                     np(Patient, [+tConcrete, +plural]),
                     lex(together),
                     A,
                     'mix-22.1-1_f2') :-
    nop(english("Herman mixed the eggs and the cream together.")),
    into_lf((cause(Agent, E), degradation_material_integrity(ResultE, Patient_i), degradation_material_integrity(ResultE, Patient_j), mingled(ResultE, Physical, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('mix-22.1-1')),
                     2,
                     prep((with;into;to)),
                     np(Co_Patient, [+tConcrete]),
                     A,
                     'mix-22.1-1_f3') :-
    nop(english("The eggs mixed with the cream.")),
    into_lf((degradation_material_integrity(ResultE, Patient), degradation_material_integrity(ResultE, Co_Patient), mingled(ResultE, Physical, Patient, Co_Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('mix-22.1-1')),
                     3,
                     [],
                     [ 'ADV',
                       prep((with;into;to)),
                       np(Co_Patient, [+tConcrete])
                     ],
                     A,
                     'mix-22.1-1_f4') :-
    nop(english("Eggs mix well with cream.")),
    into_lf((property('$VAR'('Patient+Co_Patient'), Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('mix-22.1-1')),
                     1,
                     'ADV',
                     [],
                     A,
                     'mix-22.1-1_f5') :-
    nop(english("Eggs mix well.")),
    into_lf((property('$VAR'('Patient_i+Patient_j'), Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('mix-22.1-1')),
                     2,
                     'ADV',
                     lex(together),
                     A,
                     'mix-22.1-1_f6') :-
    nop(english("Eggs and cream mix well together.")),
    into_lf((property('$VAR'('Patient_i+Patient_j'), Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('mix-22.1-1-1')),
                     0,
                     [],
                     [],
                     A,
                     'mix-22.1-1-1_f0') :-
    nop(english("The eggs and the cream mixed.")),
    into_lf((degradation_material_integrity(ResultE, Patient_i), degradation_material_integrity(ResultE, Patient_j), mingled(ResultE, Physical, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('mix-22.1-1-1')),
                     1,
                     lex(together),
                     [],
                     A,
                     'mix-22.1-1-1_f1') :-
    nop(english("The eggs and the cream mixed together.")),
    into_lf((degradation_material_integrity(ResultE, Patient_i), degradation_material_integrity(ResultE, Patient_j), mingled(ResultE, Physical, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('mix-22.1-2')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((with;into;to)), np(Co_Patient, [+tConcrete])],
                     A,
                     'mix-22.1-2_f0') :-
    nop(english("Herman added a computer to the network.")),
    into_lf((cause(Agent, E), together(EndE, Physical, Patient, Co_Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('mix-22.1-2')),
                     1,
                     np(Patient, [+tConcrete, +plural]),
                     [],
                     A,
                     'mix-22.1-2_f1') :-
    nop(english("Herman connected the computers.")),
    into_lf((cause(Agent, E), together(EndE, Physical, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('mix-22.1-2')),
                     2,
                     np(Patient, [+tConcrete, +plural]),
                     lex(together),
                     A,
                     'mix-22.1-2_f2') :-
    nop(english("Herman connected the computers together.")),
    into_lf((cause(Agent, E), together(EndE, Physical, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('mix-22.1-2')),
                     3,
                     [],
                     [ 'ADV',
                       prep((with;into;to)),
                       np(Co_Patient, [+tConcrete])
                     ],
                     A,
                     'mix-22.1-2_f3') :-
    nop(english("This computer will connect well to the network.")),
    into_lf((property('$VAR'('Patient+Co_Patient'), Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('mix-22.1-2')),
                     1,
                     'ADV',
                     [],
                     A,
                     'mix-22.1-2_f4') :-
    nop(english("These computers connected well.")),
    into_lf((property('$VAR'('Patient_i+Patient_j'), Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('mix-22.1-2')),
                     2,
                     'ADV',
                     lex(together),
                     A,
                     'mix-22.1-2_f5') :-
    nop(english("These computers connected well together.")),
    into_lf((property('$VAR'('Patient_i+Patient_j'), Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('mix-22.1-2-1')),
                     2,
                     prep((with;into;to)),
                     np(Co_Patient, [+tConcrete]),
                     A,
                     'mix-22.1-2-1_f0') :-
    nop(english("My computer connected to his computer.")),
    into_lf((together(EndE, Physical, Patient, Co_Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('mix-22.1-2-1')),
                     0,
                     [],
                     [],
                     A,
                     'mix-22.1-2-1_f1') :-
    nop(english("Our computers connected.")),
    into_lf((together(EndE, Physical, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('mix-22.1-2-1')),
                     1,
                     lex(together),
                     [],
                     A,
                     'mix-22.1-2-1_f2') :-
    nop(english("Our computers connected together.")),
    into_lf((together(EndE, Physical, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('modes_of_being_with_motion-47.3')),
                     0,
                     [],
                     [],
                     A,
                     'modes_of_being_with_motion-47.3_f0') :-
    nop(english("A flag fluttered.")),
    into_lf((exist(E, Theme), motion(E, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('modes_of_being_with_motion-47.3')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     A,
                     'modes_of_being_with_motion-47.3_f1') :-
    nop(english("A flag fluttered over the fort.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), motion(E, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('modes_of_being_with_motion-47.3')),
                     1,
                     np(Theme, [+tConcrete, -definite]),
                     [],
                     A,
                     'modes_of_being_with_motion-47.3_f2') :-
    nop(english("There fluttered a flag (over the fort).")),
    into_lf((exist(E, Theme), motion(E, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     2,
                     verb(vn('modes_of_being_with_motion-47.3')),
                     np(Theme, [+tConcrete]),
                     A,
                     'modes_of_being_with_motion-47.3_f3') :-
    nop(english("Over the fort fluttered a flag.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), motion(E, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('modes_of_being_with_motion-47.3')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'modes_of_being_with_motion-47.3_f4') :-
    nop(english("The patriots waved the flag.")),
    into_lf((cause(Agent, E), motion(E, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('multiply-108')),
                     1,
                     np(Theme, [+plural]),
                     [],
                     A,
                     'multiply-108_f0') :-
    nop(english("I summed the numbers.")),
    into_lf((calculate(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('multiply-108-1')),
                     3,
                     [np(Theme)],
                     [prep(by), np(Co_Theme)],
                     A,
                     'multiply-108-1_f0') :-
    nop(english("I multiplied x by y.")),
    into_lf((calculate(E, Agent, Theme, Co_Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('multiply-108-2')),
                     3,
                     [np(Theme)],
                     [prep(from), np(Co_Theme)],
                     A,
                     'multiply-108-2_f0') :-
    nop(english("I subtracted x from y.")),
    into_lf((calculate(E, Agent, Theme, Co_Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('multiply-108-3')),
                     3,
                     [np(Theme)],
                     [prep(over), np(Co_Theme)],
                     A,
                     'multiply-108-3_f0') :-
    nop(english("I averaged x over n.")),
    into_lf((calculate(E, Agent, Theme, Co_Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('murder-42.1')),
                     1,
                     np(Patient, [+tAnimate]),
                     [],
                     A,
                     'murder-42.1_f0') :-
    nop(english("Brutus murdered Julius Cesar.")),
    into_lf((cause(Agent, E), alive(StartE, Patient), ~alive(ResultE, Patient), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('murder-42.1')),
                     3,
                     [np(Patient, [+tAnimate])],
                     [prep(with), np(Instrument)],
                     A,
                     'murder-42.1_f1') :-
    nop(english("Caesar killed Brutus with a knife.")),
    into_lf((cause(Agent, E), alive(StartE, Patient), ~alive(ResultE, Patient), use(E, Agent, Instrument), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Instrument, [+tConcrete]),
                     verb(vn('murder-42.1-1')),
                     1,
                     np(Patient, [+tAnimate]),
                     [],
                     A,
                     'murder-42.1-1_f0') :-
    nop(english("The DDT killed the insects.")),
    into_lf((alive(StartE, Patient), ~alive(ResultE, Patient), cause(Agent, E), use(E, Agent, Instrument), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(Instrument, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('neglect-75-1')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'neglect-75-1_f0') :-
    nop(english("I neglected to do the job.")),
    into_lf((neglect(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('neglect-75-1-1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'neglect-75-1-1_f0') :-
    nop(english("I neglected the job.")),
    into_lf((neglect(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('neglect-75-2')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'neglect-75-2_f0') :-
    nop(english("I managed to do the job.")),
    into_lf((~neglect(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('neglect-75-2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'neglect-75-2_f1') :-
    nop(english("I managed the job.")),
    into_lf((~neglect(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('nonvehicle-51.4.2')),
                     0,
                     [],
                     [],
                     A,
                     'nonvehicle-51.4.2_f0') :-
    nop(english("They rowed.")),
    into_lf((motion(E, Agent), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('nonvehicle-51.4.2')),
                     2,
                     prep(Prep, [+path]),
                     np(Location, [+tConcrete]),
                     A,
                     'nonvehicle-51.4.2_f1') :-
    nop(english("They rowed along the canal.")),
    into_lf((motion(E, Agent), holds(Prep, E, Agent, Location), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('nonvehicle-51.4.2')),
                     3,
                     [np(Theme, [+tAnimate])],
                     [ prep(Prep, [+path]),
                       np(Location, [+tConcrete])
                     ],
                     A,
                     'nonvehicle-51.4.2_f2') :-
    nop(english("He rowed Penny across the lake.")),
    into_lf((motion(E0, Theme), holds(Prep, E0, Theme, Location), cause(Agent, E0), equals(E0, E1), motion(E1, Agent), holds(Prep, E1, Agent, Location), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E0, actEvent), isa(E1, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('nonvehicle-51.4.2')),
                     1,
                     np(Theme, [+tAnimate]),
                     [],
                     A,
                     'nonvehicle-51.4.2_f3') :-
    nop(english("He drove Penny.")),
    into_lf((motion(E0, Theme), cause(Agent, E0), equals(E0, E1), motion(E1, Agent), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(E0, actEvent), isa(E1, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('nonvehicle-51.4.2')),
                     1,
                     np(Location, [+tConcrete]),
                     [],
                     A,
                     'nonvehicle-51.4.2_f4') :-
    nop(english("They rowed the canals of Venice.")),
    into_lf((motion(E, Agent), via(E, Agent, Location), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('nonverbal_expression-40.2')),
                     0,
                     [],
                     [],
                     A,
                     'nonverbal_expression-40.2_f0') :-
    nop(english("Paul laughed.")),
    into_lf((cause(Agent, E), express(E, Agent, Theme), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('nonverbal_expression-40.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'nonverbal_expression-40.2_f1') :-
    nop(english("Paul laughed a cheerful laugh.")),
    into_lf((cause(Agent, E), express(E, Agent, Theme), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('nonverbal_expression-40.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'nonverbal_expression-40.2_f2') :-
    nop(english("She laughed her excitement.")),
    into_lf((cause(Agent, E), express(E, Agent, Theme), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('nonverbal_expression-40.2')),
                     2,
                     prep(in),
                     np(Theme),
                     A,
                     'nonverbal_expression-40.2_f3') :-
    nop(english("She laughed in embarrassment.")),
    into_lf((cause(Agent, E), express(E, Agent, Theme), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('nonverbal_expression-40.2')),
                     2,
                     prep(Prep, [+dest_dir]),
                     np(Recipient, [+tAnimate]),
                     A,
                     'nonverbal_expression-40.2_f4') :-
    nop(english("Paul laughed at Mary.")),
    into_lf((cause(Agent, E), transfer_info(E, Agent, Recipient, Theme), express(E, Agent, Theme), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('obtain-13.5.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'obtain-13.5.2_f0') :-
    nop(english("Carmen obtained the spare part.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('obtain-13.5.2')),
                     3,
                     [np(Theme)],
                     [prep(from), np(Source, [+tConcrete])],
                     A,
                     'obtain-13.5.2_f1') :-
    nop(english("Carmen obtained the spare part from Diana.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('obtain-13.5.2-1')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Asset, [+tCurrency])],
                     A,
                     'obtain-13.5.2-1_f0') :-
    nop(english("Carmen purchased a dress for $50.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Asset, [+tCurrency]),
                     verb(vn('obtain-13.5.2-1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'obtain-13.5.2-1_f1') :-
    nop(english("$50 won't even purchase a dress.")),
    into_lf((has_possession(StartE, Source, Theme), transfer(E, Theme), has_possession(EndE, Agent, Theme), cause(Agent, E), cost(E, Asset), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Source, tConcrete), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('occurrence-48.3')),
                     0,
                     [],
                     [],
                     A,
                     'occurrence-48.3_f0') :-
    nop(english("A serious accident happened.")),
    into_lf((occur(E, Theme), isa(E, actEvent)), A).
vndata:verbnet_frame(lex(there),
                     verb(vn('occurrence-48.3')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'occurrence-48.3_f1') :-
    nop(english("There happened a serious accident.")),
    into_lf((occur(E, Theme), isa(E, actEvent)), A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('occurrence-48.3')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location),
                     A,
                     'occurrence-48.3_f2') :-
    nop(english("A serious accident happened in front of them.")),
    into_lf((occur(E, Theme), holds(Prep, E, Theme, Location), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(prep(Prep, [+loc]),
                     np(Location),
                     2,
                     verb(vn('occurrence-48.3')),
                     np(Theme),
                     A,
                     'occurrence-48.3_f3') :-
    nop(english("In front of them happened a serious accident.")),
    into_lf((occur(E, Theme), holds(Prep, E, Theme, Location), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(it),
                     verb(vn('occurrence-48.3')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'occurrence-48.3_f4') :-
    nop(english("It occurred that they left.")),
    into_lf((occur(E, Theme), isa(E, actEvent)), A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('order-60')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'order-60_f0') :-
    nop(english("He ordered John.")),
    into_lf((command(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('order-60')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+oc_to_inf]),
                     A,
                     'order-60_f1') :-
    nop(english("He ordered John to eat his sloth.")),
    into_lf((command(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('order-60-1')),
                     1,
                     np(Topic, [+that_comp, -tensed_that]),
                     [],
                     A,
                     'order-60-1_f0') :-
    nop(english("John ordered that she come immediately.")),
    into_lf((command(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('order-60-1')),
                     1,
                     np(Topic, [+that_comp]),
                     [],
                     A,
                     'order-60-1_f1') :-
    nop(english("He ordered that she should come immediately.")),
    into_lf((command(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent),
                     verb(vn('orphan-29.7')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'orphan-29.7_f0') :-
    nop(english("The king knighted the brave soldier.")),
    into_lf((designated(ResultE, Theme, Role), cause(Agent, E), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Role, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('other_cos-45.4')),
                     1,
                     np(Patient),
                     [],
                     A,
                     'other_cos-45.4_f0') :-
    nop(english("Bill dried the clothes.")),
    into_lf((cause(Agent, E), state(ResultE, Endstate, Patient), isa(Agent, int_control), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Endstate, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('other_cos-45.4')),
                     3,
                     [np(Patient)],
                     [prep(with), np(Instrument)],
                     A,
                     'other_cos-45.4_f1') :-
    nop(english("Bill dried the clothes with a hairdryer.")),
    into_lf((cause(Agent, E), state(ResultE, Endstate, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Endstate, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('other_cos-45.4')),
                     0,
                     [],
                     [],
                     A,
                     'other_cos-45.4_f2') :-
    nop(english("The clothes dried.")),
    into_lf((state(ResultE, Endstate, Patient), isa(Agent, int_control), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Endstate, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('other_cos-45.4')),
                     1,
                     'ADV',
                     [],
                     A,
                     'other_cos-45.4_f3') :-
    nop(english("Cotton clothes dry easily.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Instrument),
                     verb(vn('other_cos-45.4')),
                     1,
                     np(Patient),
                     [],
                     A,
                     'other_cos-45.4_f4') :-
    nop(english("The hairdryer dried the clothes.")),
    into_lf((use(E, Agent, Instrument), state(ResultE, Endstate, Patient), isa(Agent, int_control), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Endstate, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('other_cos-45.4-1')),
                     1,
                     np(Result),
                     [],
                     A,
                     'other_cos-45.4-1_f0') :-
    nop(english("The clothes dried wrinkled.")),
    into_lf((state(ResultE, Endstate, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Endstate, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('overstate-37.12')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'overstate-37.12_f0') :-
    nop(english("He overstated the position.")),
    into_lf((declare(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot,
                        [ or(isa(Pivot, tAnimate),
                             isa(Pivot, tOrganization))
                        ]),
                     verb(vn('own-100')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'own-100_f0') :-
    nop(english("I own twelve oxen.")),
    into_lf((has_possession(E, Pivot, Theme), or(isa(Pivot, tAnimate), isa(Pivot, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient, [+tBodyPart]),
                     verb(vn('pain-40.8.1')),
                     0,
                     [],
                     [],
                     A,
                     'pain-40.8.1_f0') :-
    nop(english("My eyes are itching.")),
    into_lf((discomfort(E, Patient), experience(E, Experiencer), isa(Experiencer, tAnimate), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient, [+tBodyPart]),
                     verb(vn('pain-40.8.1')),
                     1,
                     np(Experiencer, [+tAnimate]),
                     [],
                     A,
                     'pain-40.8.1_f1') :-
    nop(english("My eyes are itching me.")),
    into_lf((discomfort(E, Patient), experience(E, Experiencer), isa(Experiencer, tAnimate), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient, [+tBodyPart]),
                     verb(vn('pain-40.8.1')),
                     2,
                     prep(from),
                     np(Stimulus),
                     A,
                     'pain-40.8.1_f2') :-
    nop(english("My eyes are itching from the smoke.")),
    into_lf((discomfort(E, Patient), experience(E, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('patent-101')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'patent-101_f0') :-
    nop(english("I patented my idea.")),
    into_lf((license(ResultE, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('pay-68')),
                     3,
                     [np(Asset, [+tCurrency])],
                     [prep(for), np(Theme)],
                     A,
                     'pay-68_f0') :-
    nop(english("He paid 5000 pounds for the car.")),
    into_lf((transfer(E, Theme), has_possession(StartE, Agent, Asset), has_possession(EndE, Agent, Theme), value(E, Theme, Asset), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('pay-68')),
                     1,
                     np(Asset, [+tCurrency]),
                     [],
                     A,
                     'pay-68_f1') :-
    nop(english("He paid 5000 pounds.")),
    into_lf((transfer(E, Theme), has_possession(StartE, Agent, Asset), has_possession(EndE, Agent, Theme), value(E, Theme, Asset), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('pay-68-1')),
                     4,
                     [np(Recipient), np(Asset, [+tCurrency])],
                     [prep(for), np(Theme)],
                     A,
                     'pay-68-1_f0') :-
    nop(english("He paid me 5000 pounds for the car.")),
    into_lf((transfer(E, Theme), transfer(E, Asset), has_possession(StartE, Agent, Asset), ~has_possession(StartE, Agent, Theme), has_possession(EndE, Agent, Theme), ~has_possession(EndE, Agent, Asset), value(E, Theme, Asset), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('pay-68-1')),
                     2,
                     np(Recipient),
                     np(Asset, [+tCurrency]),
                     A,
                     'pay-68-1_f1') :-
    nop(english("He paid me 5000 pounds.")),
    into_lf((transfer(E, Theme), transfer(E, Asset), has_possession(StartE, Agent, Asset), ~has_possession(StartE, Agent, Theme), has_possession(EndE, Agent, Theme), ~has_possession(EndE, Agent, Asset), value(E, Theme, Asset), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(Asset, tCurrency), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('peer-30.3')),
                     2,
                     prep(Prep, [+spatial]),
                     np(Stimulus),
                     A,
                     'peer-30.3_f0') :-
    nop(english("We peered at the baby.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('pelt-17.2')),
                     3,
                     [np(Destination, [+tConcrete])],
                     [prep(with), np(Theme, [+tConcrete])],
                     A,
                     'pelt-17.2_f0') :-
    nop(english("Steve pelted Anna with acorns.")),
    into_lf((motion(E, Theme), ~contact(E, Agent, Theme), cause(Agent, E), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), isa(Agent, int_control), isa(Theme, tConcrete), isa(Destination, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('pelt-17.2')),
                     1,
                     np(Destination, [+tConcrete]),
                     [],
                     A,
                     'pelt-17.2_f1') :-
    nop(english("Steve pelted Anna.")),
    into_lf((motion(E, Theme), ~contact(E, Agent, Theme), cause(Agent, E), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), isa(Agent, int_control), isa(Theme, tConcrete), isa(Destination, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('performance-26.7-1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'performance-26.7-1_f0') :-
    nop(english("Sandy sang a song.")),
    into_lf((perform(E, Agent, Theme), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('performance-26.7-1')),
                     0,
                     [],
                     [],
                     A,
                     'performance-26.7-1_f1') :-
    nop(english("Sandy sang.")),
    into_lf((perform(E, Agent, Theme), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('performance-26.7-1')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Beneficiary, [+tAnimate])],
                     A,
                     'performance-26.7-1_f2') :-
    nop(english("Sandy sang a song for me.")),
    into_lf((perform(E, Agent, Theme), benefit(E, Beneficiary), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('performance-26.7-1-1')),
                     2,
                     np(Beneficiary, [+tAnimate]),
                     np(Theme),
                     A,
                     'performance-26.7-1-1_f0') :-
    nop(english("Sandy sang me a song.")),
    into_lf((perform(E, Agent, Theme), benefit(E, Beneficiary), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('performance-26.7-2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'performance-26.7-2_f0') :-
    nop(english("Claire drew a picture.")),
    into_lf((~exist(StartE, Theme), exist(ResultE, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('performance-26.7-2')),
                     0,
                     [],
                     [],
                     A,
                     'performance-26.7-2_f1') :-
    nop(english("Claire draws.")),
    into_lf((~exist(StartE, Theme), exist(ResultE, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('performance-26.7-2')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Beneficiary, [+tAnimate])],
                     A,
                     'performance-26.7-2_f2') :-
    nop(english("Claire drew a picture for me.")),
    into_lf((~exist(StartE, Theme), exist(ResultE, Theme), cause(Agent, E), benefit(E, Beneficiary), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('performance-26.7-2-1')),
                     2,
                     np(Beneficiary, [+tAnimate]),
                     np(Theme),
                     A,
                     'performance-26.7-2-1_f0') :-
    nop(english("Claire drew me a picture.")),
    into_lf((~exist(StartE, Theme), exist(ResultE, Theme), cause(Agent, E), benefit(E, Beneficiary), isa(Agent, tAnimate), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('pit-10.7')),
                     1,
                     np(Source, [+tConcrete]),
                     [],
                     A,
                     'pit-10.7_f0') :-
    nop(english("The cook boned the fish.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), isa(Agent, tAnimate), isa(Source, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tHuman),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('pocket-9.10')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'pocket-9.10_f0') :-
    nop(english("Lydia pocketed the change.")),
    into_lf((motion(E, Theme), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tHuman), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Destination, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tHuman),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('pocket-9.10')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [prep((in;on;under)), np(Destination, [+tConcrete])],
                     A,
                     'pocket-9.10_f1') :-
    nop(english("Lydia pocketed the change in her left pocket.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tHuman), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Destination, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('pocket-9.10-1')),
                     2,
                     prep((in;on;under)),
                     np(Destination, [+tConcrete]),
                     A,
                     'pocket-9.10-1_f0') :-
    nop(english("I landed in Russia.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), or(isa(Agent, tHuman), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Destination, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('pocket-9.10-1')),
                     0,
                     [],
                     [],
                     A,
                     'pocket-9.10-1_f1') :-
    nop(english("The plane landed.")),
    into_lf((motion(E, Theme), or(isa(Agent, tHuman), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Destination, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('pocket-9.10-1')),
                     1,
                     np(Destination, [+tConcrete]),
                     [],
                     A,
                     'pocket-9.10-1_f2') :-
    nop(english("I landed there.")),
    into_lf((motion(E, Theme), location(EndE, Theme, Destination), or(isa(Agent, tHuman), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Destination, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('poison-42.2')),
                     1,
                     np(Patient, [+tAnimate]),
                     [],
                     A,
                     'poison-42.2_f0') :-
    nop(english("The witch poisoned Snow White.")),
    into_lf((cause(Agent, E), harmed(E, Patient), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('poison-42.2')),
                     2,
                     np(Patient, [+tAnimate]),
                     np(Result),
                     A,
                     'poison-42.2_f1') :-
    nop(english("The Boston Strangler strangled his victims dead.")),
    into_lf((cause(Agent, E), harmed(E, Patient), holds(Pred, ResultE, Patient), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('poison-42.2')),
                     3,
                     [np(Patient, [+tAnimate])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'poison-42.2_f2') :-
    nop(english("The Boston Strangler strangled his victims to death.")),
    into_lf((cause(Agent, E), harmed(E, Patient), holds(Pred, ResultE, Patient), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('poison-42.2')),
                     3,
                     [np(Patient, [+tAnimate])],
                     [prep(with), np(Instrument)],
                     A,
                     'poison-42.2_f3') :-
    nop(english("The queen poisoned Snow White with an apple.")),
    into_lf((cause(Agent, E), harmed(E, Patient), isa(Agent, tAnimate), isa(Patient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('poke-19')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'poke-19_f0') :-
    nop(english("Allison poked the cloth.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Instrument, tPointy), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('poke-19')),
                     3,
                     [np(Instrument, [+tPointy])],
                     [prep((through;into)), np(Patient, [+tConcrete])],
                     A,
                     'poke-19_f1') :-
    nop(english("Allison poked the needle through the cloth.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), in(EndE, Instrument, Patient), use(E, Agent, Instrument), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Instrument, tPointy), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('poke-19')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(with), np(Instrument, [+tPointy])],
                     A,
                     'poke-19_f2') :-
    nop(english("Allison poked the cloth with the needle.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), use(E, Agent, Instrument), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Instrument, tPointy), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('poke-19')),
                     2,
                     lex(at),
                     np(Patient, [+tConcrete]),
                     A,
                     'poke-19_f3') :-
    nop(english("Allison poked at the cloth.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Instrument, tPointy), isa(E, actEvent), isa(Directedmotion, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('poke-19')),
                     4,
                     [],
                     [ lex(at),
                       np(Patient, [+tConcrete]),
                       prep(with),
                       np(Instrument, [+tPointy])
                     ],
                     A,
                     'poke-19_f4') :-
    nop(english("Allison poked at the cloth with the needle.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), use(E, Agent, Instrument), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Instrument, tPointy), isa(E, actEvent), isa(Directedmotion, 'Constant')),
            A).
vndata:verbnet_frame(np(Instrument, [+tPointy]),
                     verb(vn('poke-19')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'poke-19_f5') :-
    nop(english("The needle poked the cloth.")),
    into_lf((manner(E, Directedmotion, Instrument), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Instrument, tPointy), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('pour-9.5')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tSubstance),
                               (isa(Theme, tConcrete), isa(Theme, tPlural)))
                          ])
                     ],
                     [ prep(Prep, [+path, -dest_dir]),
                       np(Destination, [-tRegion, +tLocation])
                     ],
                     A,
                     'pour-9.5_f0') :-
    nop(english("Tamara poured water into the bowl.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, E, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), or(isa(Theme, tSubstance), (isa(Theme, tConcrete), isa(Theme, tPlural))), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Initial_Location, tLocation), ~isa(Initial_Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('pour-9.5')),
                     2,
                     np(Theme,
                        [ or(isa(Theme, tSubstance),
                             (isa(Theme, tConcrete), isa(Theme, tPlural)))
                        ]),
                     np(Destination, [-tRegion, +tLocation, +adv_loc]),
                     A,
                     'pour-9.5_f1') :-
    nop(english("Tamara poured water here.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, E, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), or(isa(Theme, tSubstance), (isa(Theme, tConcrete), isa(Theme, tPlural))), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Initial_Location, tLocation), ~isa(Initial_Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Theme,
                        [ or(isa(Theme, tSubstance),
                             (isa(Theme, tConcrete), isa(Theme, tPlural)))
                        ]),
                     verb(vn('pour-9.5')),
                     2,
                     prep(Prep, [+path, -dest_dir]),
                     np(Destination, [-tRegion, +tLocation]),
                     A,
                     'pour-9.5_f2') :-
    nop(english("Water poured onto the plants.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, E, Theme, Destination), isa(Agent, tAnimate), or(isa(Theme, tSubstance), (isa(Theme, tConcrete), isa(Theme, tPlural))), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Initial_Location, tLocation), ~isa(Initial_Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('pour-9.5')),
                     5,
                     [ np(Theme,
                          [ or(isa(Theme, tSubstance),
                               (isa(Theme, tConcrete), isa(Theme, tPlural)))
                          ])
                     ],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [-tRegion, +tLocation]),
                       prep(Prep, [+dest_conf]),
                       np(Destination, [-tRegion, +tLocation])
                     ],
                     A,
                     'pour-9.5_f3') :-
    nop(english("Maria poured water from the bowl into the cup.")),
    into_lf((~holds(Prep, StartE, Theme, Destination), holds(Prep, E, Theme, Initial_Location), holds(Prep, E, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), or(isa(Theme, tSubstance), (isa(Theme, tConcrete), isa(Theme, tPlural))), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Initial_Location, tLocation), ~isa(Initial_Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Theme,
                        [ or(isa(Theme, tSubstance),
                             (isa(Theme, tConcrete), isa(Theme, tPlural)))
                        ]),
                     verb(vn('pour-9.5')),
                     4,
                     [],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [-tRegion, +tLocation]),
                       prep(Prep, [+dest_conf]),
                       np(Destination, [-tRegion, +tLocation])
                     ],
                     A,
                     'pour-9.5_f4') :-
    nop(english("Water poured from the bowl into the cup.")),
    into_lf((~holds(Prep, StartE, Theme, Destination), holds(Prep, E, Theme, Initial_Location), holds(Prep, E, Theme, Destination), isa(Agent, tAnimate), or(isa(Theme, tSubstance), (isa(Theme, tConcrete), isa(Theme, tPlural))), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Initial_Location, tLocation), ~isa(Initial_Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('preparing-26.3-1')),
                     1,
                     np(Product, [+tConcrete]),
                     [],
                     A,
                     'preparing-26.3-1_f0') :-
    nop(english("Donna fixed a sandwich.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(Product, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('preparing-26.3-1')),
                     2,
                     np(Beneficiary, [+tAnimate]),
                     np(Product, [+tConcrete]),
                     A,
                     'preparing-26.3-1_f1') :-
    nop(english("Donna fixed me a sandwich.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(Product, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('preparing-26.3-1')),
                     3,
                     [np(Product, [+tConcrete])],
                     [prep(for), np(Beneficiary, [+tAnimate])],
                     A,
                     'preparing-26.3-1_f2') :-
    nop(english("Donna fixed a sandwich for me.")),
    into_lf((~exist(StartE, Product), exist(ResultE, Product), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(Product, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('preparing-26.3-2')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'preparing-26.3-2_f0') :-
    nop(english("Donna grilled steaks.")),
    into_lf((~state(StartE, Endstate, Patient), state(ResultE, Endstate, Patient), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(Patient, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Endstate, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('preparing-26.3-2')),
                     2,
                     np(Beneficiary, [+tAnimate]),
                     np(Patient, [+tConcrete]),
                     A,
                     'preparing-26.3-2_f1') :-
    nop(english("Donna grilled me steaks.")),
    into_lf((~state(StartE, Endstate, Patient), state(ResultE, Endstate, Patient), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(Patient, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Endstate, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('preparing-26.3-2')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(for), np(Beneficiary, [+tAnimate])],
                     A,
                     'preparing-26.3-2_f2') :-
    nop(english("Donna grilled steaks for me.")),
    into_lf((~state(StartE, Endstate, Patient), state(ResultE, Endstate, Patient), cause(Agent, E), benefit(E, Beneficiary), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Beneficiary, tAnimate), isa(Patient, tConcrete), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Endstate, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('price-54.4')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'price-54.4_f0') :-
    nop(english("The dealer valued the book.")),
    into_lf((value(ResultE, Theme, Value), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Value, tCurrency), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('price-54.4')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [prep(at), np(Value, [+tCurrency])],
                     A,
                     'price-54.4_f1') :-
    nop(english("The dealer valued the book at $200.")),
    into_lf((value(ResultE, Theme, Value), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Value, tCurrency), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('promise-37.13')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic),
                     A,
                     'promise-37.13_f0') :-
    nop(english("I promised him the house.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('promise-37.13')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+that_comp]),
                     A,
                     'promise-37.13_f1') :-
    nop(english("I promised him that he would arrive in time.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('promise-37.13')),
                     1,
                     np(Topic, [+possing]),
                     [],
                     A,
                     'promise-37.13_f2') :-
    nop(english("I promised his getting the position.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('promise-37.13')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+that_comp])
                     ],
                     A,
                     'promise-37.13_f3') :-
    nop(english("I promised to him that he would arrive in time.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('promise-37.13')),
                     1,
                     np(Topic, [+that_comp]),
                     [],
                     A,
                     'promise-37.13_f4') :-
    nop(english("I promised that I would come.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('promote-102')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'promote-102_f0') :-
    nop(english("We promoted the cause.")),
    into_lf((promote(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('promote-102')),
                     1,
                     np(Theme, [+ac_ing]),
                     [],
                     A,
                     'promote-102_f1') :-
    nop(english("We promoted writing novels.")),
    into_lf((promote(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('promote-102')),
                     1,
                     np(Theme, [+poss_ing]),
                     [],
                     A,
                     'promote-102_f2') :-
    nop(english("We promoted their writing novels.")),
    into_lf((promote(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('push-12')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'push-12_f0') :-
    nop(english("Nora heaved the chair.")),
    into_lf((cause(Agent, E), contact(E, Agent, Theme), exert_force(E, Agent, Theme), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('push-12-1')),
                     2,
                     np(Theme, [+tConcrete]),
                     np(Result),
                     A,
                     'push-12-1_f0') :-
    nop(english("Nora yanked the button loose.")),
    into_lf((cause(Agent, E), contact(E, Agent, Theme), exert_force(E, Agent, Theme), holds(Pred, ResultE, Theme), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('push-12-1')),
                     2,
                     prep((at;against;on)),
                     np(Theme, [+tConcrete]),
                     A,
                     'push-12-1_f1') :-
    nop(english("Nora jerked at the wall.")),
    into_lf((cause(Agent, E), contact(E, Agent, Theme), exert_force(E, Agent, Theme), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('push-12-1-1')),
                     2,
                     prep(Prep, [+dir]),
                     np(Trajectory),
                     A,
                     'push-12-1-1_f0') :-
    nop(english("Nora pushed through the crowd.")),
    into_lf((cause(Agent, E), contact(E, Agent, Theme), exert_force(E, Agent, Theme), motion(E, Agent), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('put-9.1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+loc]),
                       np(Destination, [-tRegion, +tLocation])
                     ],
                     A,
                     'put-9.1_f0') :-
    nop(english("I put the book on/under/near the table.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('put-9.1')),
                     2,
                     np(Theme, [+tConcrete]),
                     np(Destination, [-tRegion, +tLocation, +adv_loc]),
                     A,
                     'put-9.1_f1') :-
    nop(english("I put the book here/there.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('put-9.1-1')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'put-9.1-1_f0') :-
    nop(english("I stashed the book.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('put-9.1-2')),
                     3,
                     [],
                     [ prep((on;upon)),
                       np(Destination, [-tRegion, +tLocation]),
                       np(Theme, [+tAbstract, +tConcrete])
                     ],
                     A,
                     'put-9.1-2_f0') :-
    nop(english("They put upon me a brilliant, red helm.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Theme, tAbstract), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('put_direction-9.4')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'put_direction-9.4_f0') :-
    nop(english("I lifted the books.")),
    into_lf((motion(E, Theme), exert_force(E, Agent, Theme, Direction), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('put_direction-9.4')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+spatial]),
                       np(Destination, [-tRegion, +tLocation])
                     ],
                     A,
                     'put_direction-9.4_f1') :-
    nop(english("I lifted the books onto the table.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Destination), exert_force(E, Agent, Theme, Direction), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('put_direction-9.4')),
                     2,
                     np(Theme, [+tConcrete]),
                     np(Destination, [-tRegion, +tLocation, +adv_loc]),
                     A,
                     'put_direction-9.4_f2') :-
    nop(english("I dropped the books here.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Destination), exert_force(E, Agent, Theme, Direction), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(Direction, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('put_spatial-9.2')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+loc]),
                       np(Destination, [-tRegion, +tLocation])
                     ],
                     A,
                     'put_spatial-9.2_f0') :-
    nop(english("Cheryl stood the books on the shelf.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), position(EndE, Theme, Pos), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE), isa(Pos, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('put_spatial-9.2')),
                     2,
                     np(Theme, [+tConcrete]),
                     np(Destination, [-tRegion, +tLocation, +adv_loc]),
                     A,
                     'put_spatial-9.2_f1') :-
    nop(english("Cheryl stood the books there.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), position(EndE, Theme, Pos), cause(Agent, E), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE), isa(Pos, Pred)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('put_spatial-9.2-1')),
                     2,
                     prep(Prep, [+loc]),
                     np(Destination, [-tRegion, +tLocation]),
                     A,
                     'put_spatial-9.2-1_f0') :-
    nop(english("The books lean against the shelf.")),
    into_lf((holds(Prep, E, Theme, Destination), position(E, Theme, Pos), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(Pos, Pred)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('put_spatial-9.2-1')),
                     1,
                     np(Destination, [-tRegion, +tLocation, +adv_loc]),
                     [],
                     A,
                     'put_spatial-9.2-1_f1') :-
    nop(english("The books lean there.")),
    into_lf((holds(Prep, E, Theme, Destination), position(E, Theme, Pos), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(Pos, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tConcrete]),
                     verb(vn('reach-51.8')),
                     1,
                     np(Destination, [+tLocation]),
                     [],
                     A,
                     'reach-51.8_f0') :-
    nop(english("They reached the hill.")),
    into_lf((motion(E, Agent), ~location(StartE, Agent, Destination), location(EndE, Agent, Destination), isa(Agent, tConcrete), isa(Agent, tOrganization), isa(Destination, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('reflexive_appearance-48.1.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'reflexive_appearance-48.1.2_f0') :-
    nop(english("I presented a solution.")),
    into_lf((appear(E, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('reflexive_appearance-48.1.2')),
                     3,
                     [np(Theme)],
                     [prep(to), np(Recipient, [+tAnimate])],
                     A,
                     'reflexive_appearance-48.1.2_f1') :-
    nop(english("I presented a solution to him.")),
    into_lf((appear(E, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('reflexive_appearance-48.1.2')),
                     1,
                     np(Reflexive, [+refl]),
                     [],
                     A,
                     'reflexive_appearance-48.1.2_f2') :-
    nop(english("A solution presented itself yesterday.")),
    into_lf((appear(E, Theme), isa(Agent, tAnimate), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('refrain-69')),
                     0,
                     [],
                     [],
                     A,
                     'refrain-69_f0') :-
    nop(english("He refrained.")),
    into_lf((abstain(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('refrain-69')),
                     2,
                     prep(from),
                     np(Theme, [+sc_ing]),
                     A,
                     'refrain-69_f1') :-
    nop(english("He refrained from eating.")),
    into_lf((abstain(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('refrain-69')),
                     2,
                     prep(from),
                     np(Theme, [-sentential]),
                     A,
                     'refrain-69_f2') :-
    nop(english("He refrained from the lurid activity while in the lady's company.")),
    into_lf((abstain(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('register-54.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'register-54.1_f0') :-
    nop(english("He clocked the runners.")),
    into_lf((value(E, Agent, Theme, Value), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('register-54.1')),
                     3,
                     [np(Theme)],
                     [prep(at), np(Value, [-sentential])],
                     A,
                     'register-54.1_f1') :-
    nop(english("He clocked the runners at different speeds.")),
    into_lf((value(E, Agent, Theme, Value), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('register-54.1')),
                     2,
                     prep(at),
                     np(Value, [-sentential]),
                     A,
                     'register-54.1_f2') :-
    nop(english("He clocked at 85km/hr.")),
    into_lf((value(E, Agent, Theme, Value), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('register-54.1-1')),
                     1,
                     np(Value),
                     [],
                     A,
                     'register-54.1-1_f0') :-
    nop(english("The package weighed ten pounds.")),
    into_lf((value(E, Agent, Theme, Value), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('rehearse-26.8')),
                     0,
                     [],
                     [],
                     A,
                     'rehearse-26.8_f0') :-
    nop(english("He rehearsed.")),
    into_lf((perform(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('rehearse-26.8')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'rehearse-26.8_f1') :-
    nop(english("He rehearsed the song.")),
    into_lf((perform(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('rehearse-26.8')),
                     1,
                     np(Theme, [+sc_ing]),
                     [],
                     A,
                     'rehearse-26.8_f2') :-
    nop(english("He rehearsed singing the song.")),
    into_lf((perform(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('relate-86.2-1')),
                     1,
                     np(Co_Theme),
                     [],
                     A,
                     'relate-86.2-1_f0') :-
    nop(english("This concerns both of us.")),
    into_lf((relate(E, Theme, Co_Theme), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('relate-86.2-2')),
                     2,
                     prep(to),
                     np(Co_Theme),
                     A,
                     'relate-86.2-2_f0') :-
    nop(english("This pertains to both of us.")),
    into_lf((relate(E, Theme, Co_Theme), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('rely-70')),
                     2,
                     prep(on),
                     np(Theme, [+sc_ing]),
                     A,
                     'rely-70_f0') :-
    nop(english("She relies on coming back in time.")),
    into_lf((depend(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('rely-70')),
                     2,
                     prep(on),
                     np(Theme, [+np_ing]),
                     A,
                     'rely-70_f1') :-
    nop(english("She relies on him coming back in time.")),
    into_lf((depend(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('rely-70')),
                     2,
                     prep(on),
                     np(Theme, [+np_to_inf]),
                     A,
                     'rely-70_f2') :-
    nop(english("She relies on him to help.")),
    into_lf((depend(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('rely-70')),
                     2,
                     prep(on),
                     np(Theme, [+poss_ing]),
                     A,
                     'rely-70_f3') :-
    nop(english("She relies on his helping.")),
    into_lf((depend(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('rely-70')),
                     2,
                     prep(on),
                     np(Theme, [-sentential]),
                     A,
                     'rely-70_f4') :-
    nop(english("She relies on it.")),
    into_lf((depend(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('remedy-45.7')),
                     1,
                     np(Patient),
                     [],
                     A,
                     'remedy-45.7_f0') :-
    nop(english("Bill dried the clothes.")),
    into_lf((cause(Agent, E), state(ResultE, Endstate, Patient), isa(Agent, int_control), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Endstate, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('remedy-45.7')),
                     3,
                     [np(Patient)],
                     [prep(with), np(Instrument)],
                     A,
                     'remedy-45.7_f1') :-
    nop(english("Bill dried the clothes with a hairdryer.")),
    into_lf((cause(Agent, E), state(ResultE, Endstate, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Endstate, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('remedy-45.7')),
                     1,
                     'ADV',
                     [],
                     A,
                     'remedy-45.7_f2') :-
    nop(english("Cotton clothes dry easily.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, int_control),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('remove-10.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'remove-10.1_f0') :-
    nop(english("Doug removed the smudges.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), or(isa(Agent, int_control), isa(Agent, tOrganization)), isa(Source, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, int_control),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('remove-10.1')),
                     3,
                     [np(Theme)],
                     [prep(Prep, [+src]), np(Source, [+tLocation])],
                     A,
                     'remove-10.1_f1') :-
    nop(english("Doug removed the smudges from the tabletop.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), or(isa(Agent, int_control), isa(Agent, tOrganization)), isa(Source, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Pivot),
                     verb(vn('require-103')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'require-103_f0') :-
    nop(english("Success requires hard work.")),
    into_lf((necessitate(E, Pivot, Theme, Source), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot),
                     verb(vn('require-103')),
                     3,
                     [np(Theme)],
                     [prep(from), np(Source)],
                     A,
                     'require-103_f1') :-
    nop(english("Success requires hard work from us.")),
    into_lf((necessitate(E, Pivot, Theme, Source), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot),
                     verb(vn('require-103')),
                     1,
                     np(Theme, [+for_comp]),
                     [],
                     A,
                     'require-103_f2') :-
    nop(english("Success requires for us to work hard.")),
    into_lf((necessitate(E, Pivot, Theme, Source), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot),
                     verb(vn('require-103')),
                     1,
                     np(Theme, [+ac_ing]),
                     [],
                     A,
                     'require-103_f3') :-
    nop(english("Success requires working long hours.")),
    into_lf((necessitate(E, Pivot, Theme, Source), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot),
                     verb(vn('require-103')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'require-103_f4') :-
    nop(english("Success requires that we work hard.")),
    into_lf((necessitate(E, Pivot, Theme, Source), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('resign-10.11')),
                     0,
                     [],
                     [],
                     A,
                     'resign-10.11_f0') :-
    nop(english("He resigned.")),
    into_lf((cause(Agent, E), location(StartE, Source), ~location(EndE, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('resign-10.11')),
                     1,
                     np(Source),
                     [],
                     A,
                     'resign-10.11_f1') :-
    nop(english("I resigned my position.")),
    into_lf((cause(Agent, E), location(StartE, Source), ~location(EndE, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('resign-10.11')),
                     2,
                     prep(from),
                     np(Source, [-sentential]),
                     A,
                     'resign-10.11_f2') :-
    nop(english("I resigned from the military.")),
    into_lf((cause(Agent, E), location(StartE, Source), ~location(EndE, Source), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('risk-94')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'risk-94_f0') :-
    nop(english("I cannot risk my job.")),
    into_lf((risk(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('risk-94')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'risk-94_f1') :-
    nop(english("I cannot risk that I get into trouble.")),
    into_lf((risk(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('risk-94-1')),
                     1,
                     np(Theme, [+be_sc_ing]),
                     [],
                     A,
                     'risk-94-1_f0') :-
    nop(english("I cannot risk smoking.")),
    into_lf((risk(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('roll-51.3.1')),
                     0,
                     [],
                     [],
                     A,
                     'roll-51.3.1_f0') :-
    nop(english("The ball rolled.")),
    into_lf((motion(E, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('roll-51.3.1')),
                     2,
                     prep(Prep, [+spatial]),
                     np(Location, [+tConcrete]),
                     A,
                     'roll-51.3.1_f1') :-
    nop(english("The ball rolled down the hill.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Location), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('roll-51.3.1')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'roll-51.3.1_f2') :-
    nop(english("Bill rolled the ball.")),
    into_lf((motion(E, Theme), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('roll-51.3.1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+path]),
                       np(Location, [+tConcrete])
                     ],
                     A,
                     'roll-51.3.1_f3') :-
    nop(english("Bill rolled the ball down the hill.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Location), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('roll-51.3.1')),
                     1,
                     np(Result),
                     [],
                     A,
                     'roll-51.3.1_f4') :-
    nop(english("The drawer rolled open.")),
    into_lf((motion(E, Theme), holds(Pred, ResultE, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('roll-51.3.1')),
                     2,
                     prep((to;into)),
                     np(Result, [+state]),
                     A,
                     'roll-51.3.1_f5') :-
    nop(english("The drawer rolled to an open position.")),
    into_lf((motion(E, Theme), holds(Pred, ResultE, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('roll-51.3.1')),
                     2,
                     np(Theme, [+tConcrete]),
                     np(Result),
                     A,
                     'roll-51.3.1_f6') :-
    nop(english("Bill rolled the drawer open.")),
    into_lf((motion(E, Theme), cause(Agent, E), holds(Pred, ResultE, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('roll-51.3.1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'roll-51.3.1_f7') :-
    nop(english("Bill rolled the drawer to an open position.")),
    into_lf((motion(E, Theme), cause(Agent, E), holds(Pred, ResultE, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('rummage-35.5')),
                     2,
                     prep((over;through;into)),
                     np(Location),
                     A,
                     'rummage-35.5_f0') :-
    nop(english("He pawed over the documents.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('rummage-35.5')),
                     4,
                     [],
                     [ prep(Prep, [+loc]),
                       np(Location),
                       prep(for),
                       np(Theme)
                     ],
                     A,
                     'rummage-35.5_f1') :-
    nop(english("He rummaged through the door for the ball.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('rummage-35.5-1')),
                     3,
                     [np(Location)],
                     [prep(for), np(Theme)],
                     A,
                     'rummage-35.5-1_f0') :-
    nop(english("We rummaged the drawer for important documents.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tMachine))
                        ]),
                     verb(vn('run-51.3.2')),
                     0,
                     [],
                     [],
                     A,
                     'run-51.3.2_f0') :-
    nop(english("The horse jumped.")),
    into_lf((motion(E, Theme), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tMachine))
                        ]),
                     verb(vn('run-51.3.2')),
                     2,
                     prep(Prep, [+spatial]),
                     np(Location, [+tConcrete]),
                     A,
                     'run-51.3.2_f1') :-
    nop(english("The horse jumped over the fence.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Location), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('run-51.3.2')),
                     3,
                     [],
                     [ prep(Prep, [+path]),
                       np(Location, [+tConcrete, +concrete]),
                       np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tMachine)),
                            -definite
                          ])
                     ],
                     A,
                     'run-51.3.2_f2') :-
    nop(english("There jumped out of the box a little white rabbit.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Location), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('run-51.3.2')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tMachine)),
                            -definite
                          ])
                     ],
                     [ prep(Prep, [+path]),
                       np(Location, [+tConcrete])
                     ],
                     A,
                     'run-51.3.2_f3') :-
    nop(english("There jumped a little white rabbit out of the box.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Location), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(prep(Prep, [+path]),
                     np(Location, [+tConcrete]),
                     2,
                     verb(vn('run-51.3.2')),
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tMachine))
                        ]),
                     A,
                     'run-51.3.2_f4') :-
    nop(english("Out of the box jumped a little white rabbit.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Location), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tMachine))
                        ]),
                     verb(vn('run-51.3.2-1')),
                     1,
                     np(Location, [+tConcrete, +concrete]),
                     [],
                     A,
                     'run-51.3.2-1_f0') :-
    nop(english("The horse jumped the stream.")),
    into_lf((motion(E, Theme), via(E, Theme, Location), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('run-51.3.2-2')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tMachine))
                          ])
                     ],
                     [ prep(Prep, [+spatial]),
                       np(Location, [+tConcrete])
                     ],
                     A,
                     'run-51.3.2-2_f0') :-
    nop(english("Tom jumped the horse over the fence.")),
    into_lf((motion(E0, Theme), holds(Prep, E0, Theme, Location), cause(Agent, E0), equals(E0, E1), motion(E1, Agent), holds(Prep, E1, Agent, Location), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E0, actEvent), isa(E1, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('run-51.3.2-2')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tMachine))
                          ])
                     ],
                     [ prep(Prep, [+spatial]),
                       np(Location, [+tConcrete])
                     ],
                     A,
                     'run-51.3.2-2_f1') :-
    nop(english("The lion tamer jumped the lions through the loop.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Location), cause(Agent, E), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('run-51.3.2-2')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tMachine))
                        ]),
                     [],
                     A,
                     'run-51.3.2-2_f2') :-
    nop(english("Tom jumped the horse.")),
    into_lf((motion(E0, Theme), cause(Agent, E0), equals(E0, E1), motion(E1, Agent), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(E0, actEvent), isa(E1, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('run-51.3.2-2')),
                     1,
                     np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tMachine))
                        ]),
                     [],
                     A,
                     'run-51.3.2-2_f3') :-
    nop(english("The lion tamer jumped the lions.")),
    into_lf((motion(E, Theme), cause(Agent, E), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('run-51.3.2-2')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tAnimate),
                               isa(Theme, tMachine))
                          ])
                     ],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'run-51.3.2-2_f4') :-
    nop(english("Tom walked the dog to exhaustion.")),
    into_lf((motion(E, Theme), cause(Agent, E), holds(Pred, ResultE, Theme), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Theme,
                        [ or(isa(Theme, tAnimate),
                             isa(Theme, tMachine))
                        ]),
                     verb(vn('run-51.3.2-2-1')),
                     1,
                     np(Location, [+tConcrete, +concrete]),
                     [],
                     A,
                     'run-51.3.2-2-1_f0') :-
    nop(english("The horse jumped the stream.")),
    into_lf((motion(E, Theme), via(E, Theme, Location), isa(Agent, tAnimate), or(isa(Theme, tAnimate), isa(Theme, tMachine)), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('rush-53.2')),
                     2,
                     prep(with),
                     np(Theme),
                     A,
                     'rush-53.2_f0') :-
    nop(english("Maggie hurried with her lunch.")),
    into_lf((rush(E, Agent, Theme), end(EndE, Theme), isa(Agent, tAnimate), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('rush-53.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'rush-53.2_f1') :-
    nop(english("Maggie hurried her sister.")),
    into_lf((rush(E, Agent, Theme), cause(Agent, E), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('rush-53.2')),
                     2,
                     prep(through),
                     np(Theme),
                     A,
                     'rush-53.2_f2') :-
    nop(english("Mary hurried through the museum.")),
    into_lf((rush(E, Agent, Theme), end(EndE, Theme), isa(Agent, tAnimate), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7')),
                     1,
                     np(Topic, [+tCommunication]),
                     [],
                     A,
                     'say-37.7_f0') :-
    nop(english("Ellen said a few words.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7')),
                     3,
                     [np(Topic, [+tCommunication])],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'say-37.7_f1') :-
    nop(english("Ellen said a few words to Helen.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +quotation])
                     ],
                     A,
                     'say-37.7_f2') :-
    nop(english("Mr. Jacobson announced to the trading mob: 'We're going to trade on the bell.'")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7')),
                     1,
                     np(Topic, [+tCommunication, +quotation]),
                     [],
                     A,
                     'say-37.7_f3') :-
    nop(english("He says, 'It has to go.'")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7')),
                     1,
                     np(Topic, [+tCommunication, +how_extract]),
                     [],
                     A,
                     'say-37.7_f4') :-
    nop(english("John suggested how she could do it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7')),
                     1,
                     np(Topic, [+tCommunication, +wh_inf]),
                     [],
                     A,
                     'say-37.7_f5') :-
    nop(english("John suggested how to do it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7')),
                     1,
                     np(Topic, [+tCommunication, +what_inf]),
                     [],
                     A,
                     'say-37.7_f6') :-
    nop(english("John suggested what to do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7')),
                     1,
                     np(Topic, [+tCommunication, +what_extract]),
                     [],
                     A,
                     'say-37.7_f7') :-
    nop(english("John suggested what we should eat.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-1')),
                     1,
                     np(Topic, [+tCommunication, +that_comp]),
                     [],
                     A,
                     'say-37.7-1_f0') :-
    nop(english("He ordered that he go.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-1')),
                     1,
                     np(Topic, [+tCommunication, +that_comp, -tensed_that]),
                     [],
                     A,
                     'say-37.7-1_f1') :-
    nop(english("John suggested that he should go.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-1-1')),
                     3,
                     [np(Topic, [+tCommunication, -sentential])],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'say-37.7-1-1_f0') :-
    nop(english("John suggested eye glasses to her.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-1-1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +how_extract])
                     ],
                     A,
                     'say-37.7-1-1_f1') :-
    nop(english("John suggested to her how she could do it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-1-1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +wh_inf])
                     ],
                     A,
                     'say-37.7-1-1_f2') :-
    nop(english("John suggested to her how to do it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-1-1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +that_comp])
                     ],
                     A,
                     'say-37.7-1-1_f3') :-
    nop(english("John suggested to him that he goes.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-1-1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic,
                          [+tCommunication, +that_comp, -tensed_that])
                     ],
                     A,
                     'say-37.7-1-1_f4') :-
    nop(english("John suggested to him that he go.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-1-1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +what_extract])
                     ],
                     A,
                     'say-37.7-1-1_f5') :-
    nop(english("John suggested to her what she could do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-1-1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +what_inf])
                     ],
                     A,
                     'say-37.7-1-1_f6') :-
    nop(english("John suggested to her what to do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-2')),
                     3,
                     [np(Topic, [+tCommunication, -sentential])],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'say-37.7-2_f0') :-
    nop(english("He suggested it to her.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-2')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +how_extract])
                     ],
                     A,
                     'say-37.7-2_f1') :-
    nop(english("John suggested to her how she could do it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-2')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +wh_inf])
                     ],
                     A,
                     'say-37.7-2_f2') :-
    nop(english("John suggested to her how to do it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-2')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +that_comp])
                     ],
                     A,
                     'say-37.7-2_f3') :-
    nop(english("John suggested to him that he goes.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-2')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic,
                          [+tCommunication, +that_comp, -tensed_that])
                     ],
                     A,
                     'say-37.7-2_f4') :-
    nop(english("John suggested to him that he go.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-2')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +what_extract])
                     ],
                     A,
                     'say-37.7-2_f5') :-
    nop(english("John suggested to her what she could do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('say-37.7-2')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+tCommunication, +what_inf])
                     ],
                     A,
                     'say-37.7-2_f6') :-
    nop(english("John suggested to her what to do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Topic, tCommunication), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('scribble-25.2')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'scribble-25.2_f0') :-
    nop(english("The jeweler printed the name.")),
    into_lf((created_image(ResultE, Theme), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Destination, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('scribble-25.2')),
                     3,
                     [np(Theme)],
                     [ [or([+loc, +dest_conf])],
                       np(Destination, [+tConcrete])
                     ],
                     A,
                     'scribble-25.2_f1') :-
    nop(english("The jeweler printed the name on the ring.")),
    into_lf((created_image(ResultE, Theme), holds(Prep, EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Destination, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('scribble-25.2')),
                     0,
                     [],
                     [],
                     A,
                     'scribble-25.2_f2') :-
    nop(english("Smith was scribbling.")),
    into_lf((created_image(ResultE, Theme), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Destination, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('search-35.2')),
                     3,
                     [np(Location)],
                     [prep(for), np(Theme)],
                     A,
                     'search-35.2_f0') :-
    nop(english("I searched the cave for treasure.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('search-35.2')),
                     4,
                     [],
                     [ prep(for),
                       np(Theme),
                       prep(Prep, [+loc]),
                       np(Location)
                     ],
                     A,
                     'search-35.2_f1') :-
    nop(english("I searched for treasure in the cave.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('search-35.2')),
                     4,
                     [],
                     [ prep(Prep, [+loc]),
                       np(Location),
                       prep(for),
                       np(Theme)
                     ],
                     A,
                     'search-35.2_f2') :-
    nop(english("I searched in the cave for treasure.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('search-35.2')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location),
                     A,
                     'search-35.2_f3') :-
    nop(english("I searched through America.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('see-30.1')),
                     1,
                     np(Stimulus),
                     [],
                     A,
                     'see-30.1_f0') :-
    nop(english("I saw the play.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('see-30.1')),
                     1,
                     np(Stimulus, [+that_comp]),
                     [],
                     A,
                     'see-30.1_f1') :-
    nop(english("I can see that you are feeling great.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('see-30.1')),
                     1,
                     np(Stimulus),
                     [],
                     A,
                     'see-30.1_f2') :-
    nop(english("I sensed the eagerness in him.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('see-30.1')),
                     1,
                     np(Stimulus, [+how_extract]),
                     [],
                     A,
                     'see-30.1_f3') :-
    nop(english("I saw how he arrived.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('see-30.1')),
                     1,
                     np(Stimulus, [+what_extract]),
                     [],
                     A,
                     'see-30.1_f4') :-
    nop(english("I saw what they did.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('see-30.1-1')),
                     1,
                     np(Stimulus, [+oc_bare_inf]),
                     [],
                     A,
                     'see-30.1-1_f0') :-
    nop(english("I saw her bake the cake.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('see-30.1-1')),
                     1,
                     np(Stimulus, [+oc_ing]),
                     [],
                     A,
                     'see-30.1-1_f1') :-
    nop(english("I saw him laughing.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('see-30.1-1')),
                     1,
                     np(Stimulus, [+pos_ing]),
                     [],
                     A,
                     'see-30.1-1_f2') :-
    nop(english("I saw their laughing and joking.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('see-30.1-1-1')),
                     2,
                     prep((about;of)),
                     np(Stimulus),
                     A,
                     'see-30.1-1-1_f0') :-
    nop(english("The entire class heard about/of the sermon.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('seem-109')),
                     1,
                     np(Attribute, [-sentential]),
                     [],
                     A,
                     'seem-109_f0') :-
    nop(english("He appeared crazy.")),
    into_lf((seem(E, Theme, Attribute), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('seem-109-1')),
                     1,
                     np(Attribute, [-sentential]),
                     [],
                     A,
                     'seem-109-1_f0') :-
    nop(english("He seemed a fool.")),
    into_lf((seem(E, Theme, Attribute), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('seem-109-1-1')),
                     2,
                     prep(in),
                     np(Attribute, [-sentential]),
                     A,
                     'seem-109-1-1_f0') :-
    nop(english("The matter seems in dispute.")),
    into_lf((seem(E, Theme, Attribute), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('seem-109-1-1-1')),
                     1,
                     np(Attribute, [+rs_to_inf]),
                     [],
                     A,
                     'seem-109-1-1-1_f0') :-
    nop(english("He seemed to come.")),
    into_lf((seem(E, Theme, Attribute), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('send-11.1')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'send-11.1_f0') :-
    nop(english("Nora sent the book.")),
    into_lf((motion(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('send-11.1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'send-11.1_f1') :-
    nop(english("Nora sent the book from Paris.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('send-11.1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'send-11.1_f2') :-
    nop(english("Nora sent the book to London.")),
    into_lf((motion(E, Theme), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('send-11.1')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation]),
                       prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'send-11.1_f3') :-
    nop(english("Nora sent the book from Paris to London.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('send-11.1')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ]),
                       prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'send-11.1_f4') :-
    nop(english("TransCanada is shifting its HQ to Calgary from Toronto.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), location(EndE, Theme, Destination), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('send-11.1-1')),
                     2,
                     np(Destination,
                        [ +tAnimate,
                          or(isa(Destination, tAnimate),
                             (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                        ]),
                     np(Theme, [+tConcrete]),
                     A,
                     'send-11.1-1_f0') :-
    nop(english("Nora sent me the book.")),
    into_lf((motion(E, Theme), direction(E, Toward, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(Destination, tAnimate), isa(E, actEvent), isa(Toward, 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('separate-23.1')),
                     3,
                     [np(Patient)],
                     [prep(from), np(Co_Patient)],
                     A,
                     'separate-23.1_f0') :-
    nop(english("I separated the yolk from the white.")),
    into_lf((cause(Agent, E), together(StartE, '$VAR'('Physical/abstract'), Patient, Co_Patient), apart(EndE, '$VAR'('Physical/abstract'), Patient, Co_Patient), isa(Agent, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa('$VAR'('Physical/abstract'), 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('separate-23.1')),
                     1,
                     np(Patient, [+plural]),
                     [],
                     A,
                     'separate-23.1_f1') :-
    nop(english("I separated the yolk and the white.")),
    into_lf((cause(Agent, E), together(StartE, '$VAR'('Physical/abstract'), Patient_i, Patient_j), apart(EndE, '$VAR'('Physical/abstract'), Patient_i, Patient_j), isa(Agent, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa('$VAR'('Physical/abstract'), 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Patient, [+plural]),
                     verb(vn('separate-23.1')),
                     0,
                     [],
                     [],
                     A,
                     'separate-23.1_f2') :-
    nop(english("The yolk and the white separated.")),
    into_lf((together(StartE, '$VAR'('Physical/abstract'), Patient_i, Patient_j), apart(EndE, '$VAR'('Physical/abstract'), Patient_i, Patient_j), isa(Agent, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa('$VAR'('Physical/abstract'), 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('separate-23.1')),
                     3,
                     [],
                     ['ADV', prep(from), np(Co_Patient)],
                     A,
                     'separate-23.1_f3') :-
    nop(english("Cream separates easily from milk.")),
    into_lf((property('$VAR'('Patient+Co_Patient'), Prop), holds(Adv, Prop), isa(Agent, tAnimate), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+plural]),
                     verb(vn('separate-23.1')),
                     1,
                     'ADV',
                     [],
                     A,
                     'separate-23.1_f4') :-
    nop(english("Egg yolks and egg whites separate easily.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), isa(Agent, tAnimate), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('separate-23.1-1')),
                     2,
                     prep(from),
                     np(Co_Patient),
                     A,
                     'separate-23.1-1_f0') :-
    nop(english("The yolk separated from the white.")),
    into_lf((together(StartE, '$VAR'('Physical/abstract'), Patient, Co_Patient), apart(EndE, '$VAR'('Physical/abstract'), Patient, Co_Patient), isa(Agent, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa('$VAR'('Physical/abstract'), 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('separate-23.1-2')),
                     2,
                     prep(with),
                     np(Co_Patient),
                     A,
                     'separate-23.1-2_f0') :-
    nop(english("The yoke parted with the white.")),
    into_lf((together(StartE, '$VAR'('Physical/abstract'), Patient, Co_Patient), apart(EndE, '$VAR'('Physical/abstract'), Patient, Co_Patient), isa(Agent, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa('$VAR'('Physical/abstract'), 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('settle-89')),
                     0,
                     [],
                     [],
                     A,
                     'settle-89_f0') :-
    nop(english("Eventually, they settled.")),
    into_lf((agree(ResultE, Agent, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('settle-89')),
                     2,
                     prep(on),
                     np(Goal, [+what_inf]),
                     A,
                     'settle-89_f1') :-
    nop(english("They settled on what to do.")),
    into_lf((agree(ResultE, Agent, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('settle-89')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(on),
                       np(Goal, [-sentential])
                     ],
                     A,
                     'settle-89_f2') :-
    nop(english("I settled with them on a novel proposal.")),
    into_lf((agree(ResultE, Agent, Co_Agent, Goal), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('shake-22.3')),
                     1,
                     'ADV',
                     [],
                     A,
                     'shake-22.3_f0') :-
    nop(english("Eggs whisk easily.")),
    into_lf((property('$VAR'('Patient+Co_Patient'), Prop), holds(Adv, Prop), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete, +plural]),
                     verb(vn('shake-22.3')),
                     2,
                     'ADV',
                     lex(together),
                     A,
                     'shake-22.3_f1') :-
    nop(english("Sugar and cream whip together easily.")),
    into_lf((property('$VAR'('Patient+Co_Patient'), Prop), holds(Adv, Prop), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('shake-22.3-1')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((with;into;to)), np(Co_Patient, [+tConcrete])],
                     A,
                     'shake-22.3-1_f0') :-
    nop(english("Herman whipped sugar with the cream.")),
    into_lf((cause(Agent, E), degradation_material_integrity(ResultE, Patient), degradation_material_integrity(ResultE, Co_Patient), mingled(ResultE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('shake-22.3-1')),
                     2,
                     np(Patient, [+tConcrete, +plural]),
                     lex(together),
                     A,
                     'shake-22.3-1_f1') :-
    nop(english("Herman whipped sugar and the cream together.")),
    into_lf((cause(Agent, E), degradation_material_integrity(ResultE, Patient), degradation_material_integrity(ResultE, Co_Patient), mingled(ResultE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Patient, [+tConcrete]),
                     verb(vn('shake-22.3-1-1')),
                     3,
                     [],
                     [ prep((with;into;to)),
                       np(Co_Patient, [+tConcrete]),
                       'ADV'
                     ],
                     A,
                     'shake-22.3-1-1_f0') :-
    nop(english("Sugar whips into cream easily.")),
    into_lf((property('$VAR'('Patient+Co_Patient'), Prop), holds(Adv, Prop), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('shake-22.3-1-1')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'shake-22.3-1-1_f1') :-
    nop(english("Herman whipped cream.")),
    into_lf((cause(Agent, E), degradation_material_integrity(ResultE, Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('shake-22.3-2')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((with;into;to)), np(Co_Patient, [+tConcrete])],
                     A,
                     'shake-22.3-2_f0') :-
    nop(english("Herman gathered the students into a group.")),
    into_lf((cause(Agent, E), together(EndE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('shake-22.3-2')),
                     2,
                     np(Patient, [+tConcrete, +plural]),
                     lex(together),
                     A,
                     'shake-22.3-2_f1') :-
    nop(english("Herman gathered the students together.")),
    into_lf((cause(Agent, E), together(EndE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('shake-22.3-2-1')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'shake-22.3-2-1_f0') :-
    nop(english("Herman spliced ropes.")),
    into_lf((cause(Agent, E), together(EndE, Physical, Patient, Co_Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tConcrete), isa(Co_Patient, tConcrete), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant')),
            A).
vndata:verbnet_frame(np(Experiencer, [+tAnimate]),
                     verb(vn('sight-30.2')),
                     1,
                     np(Stimulus),
                     [],
                     A,
                     'sight-30.2_f0') :-
    nop(english("The crew spotted the island.")),
    into_lf((perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('simple_dressing-41.3.1')),
                     1,
                     np(Theme, [+tGarment]),
                     [],
                     A,
                     'simple_dressing-41.3.1_f0') :-
    nop(english("She always wears purple dresses.")),
    into_lf((wear(E, Agent, Theme), isa(Agent, tAnimate), isa(Theme, tGarment), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('slide-11.2')),
                     0,
                     [],
                     [],
                     A,
                     'slide-11.2_f0') :-
    nop(english("The books slid.")),
    into_lf((motion(E, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('slide-11.2')),
                     2,
                     prep(Prep, [+path, -dest_dir]),
                     np(Initial_Location, [+tLocation]),
                     A,
                     'slide-11.2_f1') :-
    nop(english("The books slid from the table.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('slide-11.2')),
                     2,
                     prep(to),
                     np(Destination,
                        [ or(isa(Destination, tAnimate),
                             (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                        ]),
                     A,
                     'slide-11.2_f2') :-
    nop(english("The books slid to the floor.")),
    into_lf((motion(E, Theme), location(EndE, Theme, Destination), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('slide-11.2')),
                     4,
                     [],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation]),
                       prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'slide-11.2_f3') :-
    nop(english("The books slid from the table to the floor.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), location(EndE, Theme, Destination), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('slide-11.2')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'slide-11.2_f4') :-
    nop(english("Carla slid the books.")),
    into_lf((motion(E, Theme), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('slide-11.2')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [prep(Prep, [+path, -dest_dir]), np(Trajectory)],
                     A,
                     'slide-11.2_f5') :-
    nop(english("Carla slid the books across the table.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('slide-11.2')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'slide-11.2_f6') :-
    nop(english("Carla slid the books to the floor.")),
    into_lf((motion(E, Theme), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('slide-11.2')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation]),
                       prep(to),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'slide-11.2_f7') :-
    nop(english("Carla slid the books from one end of the table to the other.")),
    into_lf((motion(E, Theme), location(StartE, Theme, Initial_Location), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('slide-11.2-1')),
                     2,
                     np(Destination,
                        [ +tAnimate,
                          or(isa(Destination, tAnimate),
                             (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                        ]),
                     np(Theme, [+tConcrete]),
                     A,
                     'slide-11.2-1_f0') :-
    nop(english("Carla slid John the books.")),
    into_lf((motion(E, Theme), direction(E, Toward, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(Destination, tAnimate), isa(E, actEvent), isa(Toward, 'Constant'), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [-tAnimate]),
                     verb(vn('smell_emission-43.3')),
                     0,
                     [],
                     [],
                     A,
                     'smell_emission-43.3_f0') :-
    nop(english("The onions reeked.")),
    into_lf((emit(E, Theme, Odor), ~isa(Theme, tAnimate), isa(E, actEvent), isa(Odor, Pred)),
            A).
vndata:verbnet_frame(np(Location),
                     verb(vn('smell_emission-43.3')),
                     0,
                     [],
                     [],
                     A,
                     'smell_emission-43.3_f1') :-
    nop(english("The room reeked.")),
    into_lf((emit(E, Theme, Odor), location(E, Odor, Location), ~isa(Theme, tAnimate), isa(E, actEvent), isa(Odor, Pred)),
            A).
vndata:verbnet_frame(np(Location),
                     verb(vn('smell_emission-43.3')),
                     2,
                     prep(of),
                     np(Theme, [-tAnimate]),
                     A,
                     'smell_emission-43.3_f2') :-
    nop(english("The room reeked of onions.")),
    into_lf((emit(E, Theme, Odor), location(E, Odor, Location), ~isa(Theme, tAnimate), isa(E, actEvent), isa(Odor, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('snooze-40.4')),
                     0,
                     [],
                     [],
                     A,
                     'snooze-40.4_f0') :-
    nop(english("Gloria snoozed.")),
    into_lf((sleep(E, Agent), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('snooze-40.4')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'snooze-40.4_f1') :-
    nop(english("Gloria slept the sleep of the dead.")),
    into_lf((sleep(E, Agent), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [-tAnimate]),
                     verb(vn('sound_emission-43.2')),
                     0,
                     [],
                     [],
                     A,
                     'sound_emission-43.2_f0') :-
    nop(english("The door hinges squeaked.")),
    into_lf((emit(E, Theme, Sound), ~isa(Theme, tAnimate), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(np(Theme, [-tAnimate]),
                     verb(vn('sound_emission-43.2')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location),
                     A,
                     'sound_emission-43.2_f1') :-
    nop(english("Horns beeped in the street.")),
    into_lf((emit(E, Theme, Sound), holds(Prep, E, Sound, Location), ~isa(Theme, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(np(Location),
                     verb(vn('sound_emission-43.2')),
                     2,
                     prep(with),
                     np(Theme, [-tAnimate]),
                     A,
                     'sound_emission-43.2_f2') :-
    nop(english("The street sang with horns.")),
    into_lf((emit(E, Theme, Sound), location(E, Sound, Location), ~isa(Theme, tAnimate), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(prep(Prep, [+loc]),
                     np(Location),
                     2,
                     verb(vn('sound_emission-43.2')),
                     np(Theme, [-tAnimate]),
                     A,
                     'sound_emission-43.2_f3') :-
    nop(english("In the hallway ticked a gradfather clock.")),
    into_lf((emit(E, Theme, Sound), holds(Prep, E, Sound, Location), ~isa(Theme, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('sound_emission-43.2')),
                     3,
                     [np(Theme, [-tAnimate])],
                     [prep(Prep, [+loc]), np(Location)],
                     A,
                     'sound_emission-43.2_f4') :-
    nop(english("There ticked a grandather clock in the hallway.")),
    into_lf((emit(E, Theme, Sound), holds(Prep, E, Sound, Location), ~isa(Theme, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(np(Agent),
                     verb(vn('sound_emission-43.2')),
                     1,
                     np(Theme, [-tAnimate]),
                     [],
                     A,
                     'sound_emission-43.2_f5') :-
    nop(english("I buzzed the bell.")),
    into_lf((emit(E, Theme, Sound), cause(Agent, E), ~isa(Theme, tAnimate), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(np(Theme, [-tAnimate]),
                     verb(vn('sound_emission-43.2-1')),
                     1,
                     np(Time),
                     [],
                     A,
                     'sound_emission-43.2-1_f0') :-
    nop(english("The bell chimed the hour.")),
    into_lf((emit(E, Theme, Sound), ~isa(Theme, tAnimate), isa(E, actEvent), isa(Sound, Pred)),
            A).
vndata:verbnet_frame(np(Theme, [+tSound]),
                     verb(vn('sound_existence-47.4')),
                     0,
                     [],
                     [],
                     A,
                     'sound_existence-47.4_f0') :-
    nop(english("The voices echoed.")),
    into_lf((exist(E, Theme), isa(Theme, tSound), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tSound]),
                     verb(vn('sound_existence-47.4')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     A,
                     'sound_existence-47.4_f1') :-
    nop(english("The voices echoed through the hall.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), isa(Theme, tSound), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('sound_existence-47.4')),
                     1,
                     np(Theme, [+tSound, -definite]),
                     [],
                     A,
                     'sound_existence-47.4_f2') :-
    nop(english("There echoed voices through the hall.")),
    into_lf((exist(E, Theme), isa(Theme, tSound), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     2,
                     verb(vn('sound_existence-47.4')),
                     np(Theme, [+tSound]),
                     A,
                     'sound_existence-47.4_f3') :-
    nop(english("Through the hall echoed a loud cry.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), isa(Theme, tSound), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Location, [-tRegion, +tLocation]),
                     verb(vn('sound_existence-47.4')),
                     2,
                     prep(with),
                     np(Theme, [+tSound]),
                     A,
                     'sound_existence-47.4_f4') :-
    nop(english("The hall echoed with voices.")),
    into_lf((exist(E, Theme), location(E, Theme, Location), isa(Theme, tSound), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('spank-18.3')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'spank-18.3_f0') :-
    nop(english("Paula spanked the child.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Location, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('spank-18.3')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(with), np(Instrument, [+tConcrete])],
                     A,
                     'spank-18.3_f1') :-
    nop(english("Paula spanked the child with her right hand.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Location, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('spank-18.3')),
                     2,
                     np(Patient, [+tConcrete]),
                     np(Result),
                     A,
                     'spank-18.3_f2') :-
    nop(english("They spanked him dead.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('spank-18.3')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'spank-18.3_f3') :-
    nop(english("They spanked him to death.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), holds(Pred, ResultE, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('spank-18.3')),
                     5,
                     [np(Patient, [+tConcrete])],
                     [ prep((to;into)),
                       np(Result, [+state]),
                       prep(with),
                       np(Instrument, [+tConcrete])
                     ],
                     A,
                     'spank-18.3_f4') :-
    nop(english("They spanked him to death with a bat.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('spank-18.3')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [ prep(Prep, [+loc]),
                       np(Location, [+tConcrete, +body_part])
                     ],
                     A,
                     'spank-18.3_f5') :-
    nop(english("Paula spanked the naughty child on the back.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Location, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('spank-18.3')),
                     5,
                     [np(Patient, [+tConcrete])],
                     [ prep(Prep, [+loc]),
                       np(Location, [+tConcrete, +body_part]),
                       prep(with),
                       np(Instrument, [+tConcrete])
                     ],
                     A,
                     'spank-18.3_f6') :-
    nop(english("Paula spanked the naughty child on the back with a paddle.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Location, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('spank-18.3')),
                     1,
                     np(Patient, [+tConcrete, +genitive, +body_part]),
                     [],
                     A,
                     'spank-18.3_f7') :-
    nop(english("Paula spanked the naughty child's back.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), isa(Agent, int_control), isa(Patient, tConcrete), isa(Instrument, tConcrete), isa(Location, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('spatial_configuration-47.6')),
                     0,
                     [],
                     [],
                     A,
                     'spatial_configuration-47.6_f0') :-
    nop(english("John slouched.")),
    into_lf((position(E, Theme, Pos), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent), isa(Pos, Pred)),
            A).
vndata:verbnet_frame(np(Theme, [+tConcrete]),
                     verb(vn('spatial_configuration-47.6')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     A,
                     'spatial_configuration-47.6_f1') :-
    nop(english("The statue stood on the corner.")),
    into_lf((position(E, Theme, Pos), holds(Prep, E, Theme, Location), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(Pos, Pred)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('spatial_configuration-47.6')),
                     3,
                     [],
                     [ prep(Prep, [+loc]),
                       np(Location, [-tRegion, +tLocation]),
                       np(Theme, [+tConcrete, -definite])
                     ],
                     A,
                     'spatial_configuration-47.6_f2') :-
    nop(english("There stood on the corner a statue.")),
    into_lf((position(E, Theme, Pos), holds(Prep, E, Theme, Location), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(Pos, Pred)),
            A).
vndata:verbnet_frame(prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     2,
                     verb(vn('spatial_configuration-47.6')),
                     np(Theme, [+tConcrete]),
                     A,
                     'spatial_configuration-47.6_f3') :-
    nop(english("On the pedestal stood a statue.")),
    into_lf((position(E, Theme, Pos), holds(Prep, E, Theme, Location), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(Pos, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('spatial_configuration-47.6')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+loc]),
                       np(Location, [-tRegion, +tLocation])
                     ],
                     A,
                     'spatial_configuration-47.6_f4') :-
    nop(english("They stood the statue on the pedestal.")),
    into_lf((cause(Agent, E), ~position(StartE, Theme, Pos), position(EndE, Theme, Pos), holds(Prep, E, Theme, Location), isa(Agent, tAnimate), isa(Theme, tConcrete), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Pos, Pred), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('spend_time-104')),
                     1,
                     np(Theme, [+tTime]),
                     [],
                     A,
                     'spend_time-104_f0') :-
    nop(english("I misspent 5 hours.")),
    into_lf((exist(StartE, Theme), ~exist(EndE, Theme), spend(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(Theme, tTime), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('split-23.2')),
                     3,
                     [np(Patient, [+tSolid])],
                     [prep((off;off;of;from)), np(Co_Patient, [+tSolid])],
                     A,
                     'split-23.2_f0') :-
    nop(english("I broke the twig off the branch.")),
    into_lf((cause(Agent, E), together(StartE, Physical, Patient, Co_Patient), apart(ResultE, Physical, Patient, Co_Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Physical, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('split-23.2')),
                     2,
                     np(Patient, [+tSolid, +plural]),
                     lex(apart),
                     A,
                     'split-23.2_f1') :-
    nop(english("I broke the twig and the branch apart.")),
    into_lf((cause(Agent, E), together(StartE, Physical, Patient_i, Patient_j), apart(ResultE, Physical, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Physical, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid]),
                     verb(vn('split-23.2')),
                     2,
                     prep((off;off;of;from)),
                     np(Co_Patient, [+tSolid]),
                     A,
                     'split-23.2_f2') :-
    nop(english("The twig broke off the branch.")),
    into_lf((together(StartE, Physical, Patient, Co_Patient), apart(ResultE, Physical, Patient, Co_Patient), isa(Agent, int_control), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Physical, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid, +plural]),
                     verb(vn('split-23.2')),
                     1,
                     lex(apart),
                     [],
                     A,
                     'split-23.2_f3') :-
    nop(english("The twig and the branch broke apart.")),
    into_lf((together(StartE, Physical, Patient_i, Patient_j), apart(ResultE, Physical, Patient_i, Patient_j), isa(Agent, int_control), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Physical, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid]),
                     verb(vn('split-23.2')),
                     3,
                     [],
                     [ prep((off;off;of;from)),
                       np(Co_Patient, [+tSolid]),
                       'ADV'
                     ],
                     A,
                     'split-23.2_f4') :-
    nop(english("Twigs break off of those branches easily.")),
    into_lf((property('$VAR'('Patient+Co_Patient'), Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid, +plural]),
                     verb(vn('split-23.2')),
                     2,
                     lex(apart),
                     'ADV',
                     A,
                     'split-23.2_f5') :-
    nop(english("Those twigs and branches break apart easily.")),
    into_lf((property(Patient, Prop), holds(Adv, Prop), isa(Agent, int_control), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('spray-9.7')),
                     3,
                     [np(Theme)],
                     [ [or([+loc, +dest_conf])],
                       np(Destination, [-tRegion, +tLocation])
                     ],
                     A,
                     'spray-9.7_f0') :-
    nop(english("Jessica loaded boxes into the wagon.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('spray-9.7')),
                     3,
                     [np(Destination, [-tRegion, +tLocation])],
                     [prep(with), np(Theme)],
                     A,
                     'spray-9.7_f1') :-
    nop(english("Jessica loaded the wagon with boxes.")),
    into_lf((motion(E, Theme), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('spray-9.7')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'spray-9.7_f2') :-
    nop(english("Jessica squirted water.")),
    into_lf((motion(E, Theme), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('spray-9.7')),
                     1,
                     np(Destination, [-tRegion, +tLocation]),
                     [],
                     A,
                     'spray-9.7_f3') :-
    nop(english("Jessica sprayed the wall.")),
    into_lf((motion(E, Theme), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Destination, tLocation), ~isa(Destination, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme,
                        [ or(isa(Theme, tSubstance),
                             (isa(Theme, tConcrete), isa(Theme, tPlural)))
                        ]),
                     verb(vn('spray-9.7-1')),
                     2,
                     [or([+loc, +dir, +dest_conf])],
                     np(Destination, [-tRegion, +tLocation]),
                     A,
                     'spray-9.7-1_f0') :-
    nop(english("Paint sprayed onto the wall.")),
    into_lf((motion(E, Theme), ~holds(Prep, StartE, Theme, Destination), holds(Prep, EndE, Theme, Destination), isa(Agent, tAnimate), isa(Destination, tLocation), ~isa(Destination, tRegion), or(isa(Theme, tSubstance), (isa(Theme, tConcrete), isa(Theme, tPlural))), isa(Prep, '$PREP'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('spray-9.7-1')),
                     3,
                     [ np(Theme,
                          [ or(isa(Theme, tSubstance),
                               (isa(Theme, tConcrete), isa(Theme, tPlural)))
                          ])
                     ],
                     [lex(at), np(Destination, [-tRegion, +tLocation])],
                     A,
                     'spray-9.7-1_f1') :-
    nop(english("Jessica squirted water at me.")),
    into_lf((motion(E, Theme), ~location(StartE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Destination, tLocation), ~isa(Destination, tRegion), or(isa(Theme, tSubstance), (isa(Theme, tConcrete), isa(Theme, tPlural))), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE)),
            A).
vndata:verbnet_frame(np(Theme,
                        [ or(isa(Theme, tSubstance),
                             (isa(Theme, tConcrete), isa(Theme, tPlural)))
                        ]),
                     verb(vn('spray-9.7-1-1')),
                     1,
                     np(Destination, [-tRegion, +tLocation]),
                     [],
                     A,
                     'spray-9.7-1-1_f0') :-
    nop(english("Crowds packed the stands.")),
    into_lf((location(E, Theme, Destination), isa(Agent, tAnimate), isa(Destination, tLocation), ~isa(Destination, tRegion), or(isa(Theme, tSubstance), (isa(Theme, tConcrete), isa(Theme, tPlural))), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('stalk-35.3')),
                     3,
                     [np(Location)],
                     [prep(for), np(Theme)],
                     A,
                     'stalk-35.3_f0') :-
    nop(english("I stalked the woods for game.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('stalk-35.3')),
                     3,
                     [np(Theme)],
                     [prep(Prep, [+loc]), np(Location)],
                     A,
                     'stalk-35.3_f1') :-
    nop(english("I stalked game in the woods.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('stalk-35.3')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'stalk-35.3_f2') :-
    nop(english("I tracked prints.")),
    into_lf((search(E, Agent, Location, Theme), isa(Agent, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('steal-10.5')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'steal-10.5_f0') :-
    nop(english("The thief stole the paint.")),
    into_lf((manner(E, Illegal, Agent), has_possession(StartE, Source, Theme), has_possession(EndE, Agent, Theme), ~has_possession(EndE, Source, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Source, tAnimate), (isa(Source, tLocation), ~isa(Source, tRegion))), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(Illegal, 'Constant'), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('steal-10.5')),
                     3,
                     [np(Theme)],
                     [ prep(Prep, [+src]),
                       np(Source,
                          [ or(isa(Source, tAnimate),
                               (isa(Source, tLocation), ~isa(Source, tRegion)))
                          ])
                     ],
                     A,
                     'steal-10.5_f1') :-
    nop(english("The thief stole the paint from the museum.")),
    into_lf((manner(E, Illegal, Agent), has_possession(StartE, Source, Theme), has_possession(EndE, Agent, Theme), ~has_possession(EndE, Source, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Source, tAnimate), (isa(Source, tLocation), ~isa(Source, tRegion))), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(Illegal, 'Constant'), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('steal-10.5')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Beneficiary, [+tAnimate])],
                     A,
                     'steal-10.5_f2') :-
    nop(english("The thief stole the paint for Mary.")),
    into_lf((manner(E, Illegal, Agent), has_possession(StartE, Source, Theme), has_possession(EndE, Beneficiary, Theme), ~has_possession(EndE, Source, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Source, tAnimate), (isa(Source, tLocation), ~isa(Source, tRegion))), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(Illegal, 'Constant'), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('steal-10.5')),
                     5,
                     [np(Theme)],
                     [ prep(Prep, [+src]),
                       np(Source,
                          [ or(isa(Source, tAnimate),
                               (isa(Source, tLocation), ~isa(Source, tRegion)))
                          ]),
                       prep(for),
                       np(Beneficiary, [+tAnimate])
                     ],
                     A,
                     'steal-10.5_f3') :-
    nop(english("The thief stole the paint from John for Mary.")),
    into_lf((manner(E, Illegal, Agent), has_possession(StartE, Source, Theme), has_possession(EndE, Beneficiary, Theme), ~has_possession(EndE, Source, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Source, tAnimate), (isa(Source, tLocation), ~isa(Source, tRegion))), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(Illegal, 'Constant'), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('steal-10.5-1')),
                     2,
                     np(Beneficiary, [+tAnimate]),
                     np(Theme),
                     A,
                     'steal-10.5-1_f0') :-
    nop(english("The thief stole Mary some paint.")),
    into_lf((manner(E, Illegal, Agent), has_possession(StartE, Source, Theme), has_possession(EndE, Beneficiary, Theme), ~has_possession(EndE, Source, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Source, tAnimate), (isa(Source, tLocation), ~isa(Source, tRegion))), isa(Beneficiary, tAnimate), isa(E, actEvent), isa(Illegal, 'Constant'), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('stimulus_subject-30.4')),
                     1,
                     'ADJ',
                     [],
                     A,
                     'stimulus_subject-30.4_f0') :-
    nop(english("That pea soup tasted delicious.")),
    into_lf((holds(Pred, E, Stimulus), perceive(E, Experiencer, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(Pred, '$VERB'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Stimulus),
                     verb(vn('stimulus_subject-30.4')),
                     3,
                     [],
                     ['ADJ', prep(to), np(Experiencer, [+tAnimate])],
                     A,
                     'stimulus_subject-30.4_f1') :-
    nop(english("That pea soup tasted delicious to me.")),
    into_lf((perceive(E, Experiencer, Stimulus), holds(Pred, E, Stimulus), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(Pred, '$VERB'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('stop-55.4')),
                     1,
                     np(Theme, [+be_sc_ing]),
                     [],
                     A,
                     'stop-55.4_f0') :-
    nop(english("He stopped going to the area.")),
    into_lf((end(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('stop-55.4')),
                     0,
                     [],
                     [],
                     A,
                     'stop-55.4_f1') :-
    nop(english("The storm ended.")),
    into_lf((end(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('stop-55.4')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'stop-55.4_f2') :-
    nop(english("John ended the party.")),
    into_lf((end(E, Theme), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('stop-55.4')),
                     2,
                     prep(with),
                     np(Instrument),
                     A,
                     'stop-55.4_f3') :-
    nop(english("The party ended with a bang.")),
    into_lf((end(E, Theme), use(E, Agent, Instrument), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('stop-55.4')),
                     3,
                     [np(Theme)],
                     [prep(with), np(Instrument)],
                     A,
                     'stop-55.4_f4') :-
    nop(english("I ended the party with a speech.")),
    into_lf((end(E, Theme), use(E, Agent, Instrument), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme),
                     verb(vn('stop-55.4-1')),
                     1,
                     'ADV',
                     [],
                     A,
                     'stop-55.4-1_f0') :-
    nop(english("The party stopped easily.")),
    into_lf((end(E, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('subjugate-42.3')),
                     1,
                     np(Patient,
                        [ or(isa(Patient, tAnimate),
                             isa(Patient, tOrganization))
                        ]),
                     [],
                     A,
                     'subjugate-42.3_f0') :-
    nop(english("Winnie the Pooh subjugated the unfortunate Pixies.")),
    into_lf((cause(Agent, E), subjugated(E, Patient), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('subjugate-42.3')),
                     3,
                     [ np(Patient,
                          [ or(isa(Patient, tAnimate),
                               isa(Patient, tOrganization))
                          ])
                     ],
                     [prep(with), np(Instrument)],
                     A,
                     'subjugate-42.3_f1') :-
    nop(english("Russia subjugated Mongolia with overwhelming force.")),
    into_lf((cause(Agent, E), subjugated(E, Patient), use(E, Agent, Instrument), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Patient, tAnimate), isa(Patient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Source),
                     verb(vn('substance_emission-43.4')),
                     0,
                     [],
                     [],
                     A,
                     'substance_emission-43.4_f0') :-
    nop(english("The fountain gushed.")),
    into_lf((emit(E, Source, Theme), ~isa(Theme, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Source),
                     verb(vn('substance_emission-43.4')),
                     1,
                     np(Theme, [-tAnimate]),
                     [],
                     A,
                     'substance_emission-43.4_f1') :-
    nop(english("The well gushed oil.")),
    into_lf((emit(E, Source, Theme), ~isa(Theme, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [-tAnimate]),
                     verb(vn('substance_emission-43.4')),
                     2,
                     prep((from;out;of)),
                     np(Source),
                     A,
                     'substance_emission-43.4_f2') :-
    nop(english("Oil gushed from the well.")),
    into_lf((emit(E, Source, Theme), ~isa(Theme, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [-tAnimate]),
                     verb(vn('substance_emission-43.4')),
                     2,
                     prep(Prep, [+path]),
                     np(Location),
                     A,
                     'substance_emission-43.4_f3') :-
    nop(english("Water gushed through the streets.")),
    into_lf((emit(E, Source, Theme), holds(Prep, E, Theme, Location), ~isa(Theme, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Location),
                     verb(vn('substance_emission-43.4')),
                     2,
                     prep(with),
                     np(Theme, [-tAnimate]),
                     A,
                     'substance_emission-43.4_f4') :-
    nop(english("The streets gushed with water.")),
    into_lf((emit(E, Source, Theme), location(E, Theme, Location), ~isa(Theme, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(prep(Prep, [+loc]),
                     np(Location),
                     2,
                     verb(vn('substance_emission-43.4')),
                     np(Theme, [-tAnimate]),
                     A,
                     'substance_emission-43.4_f5') :-
    nop(english("Over the fire bubbled a fragant stew.")),
    into_lf((emit(E, Source, Theme), holds(Prep, E, Theme, Location), ~isa(Theme, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('substance_emission-43.4')),
                     3,
                     [np(Theme, [-tAnimate])],
                     [prep(Prep, [+loc]), np(Location)],
                     A,
                     'substance_emission-43.4_f6') :-
    nop(english("There bubbled a fragant stew over the fire.")),
    into_lf((emit(E, Source, Theme), holds(Prep, E, Theme, Location), ~isa(Theme, tAnimate), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [-tAnimate]),
                     verb(vn('substance_emission-43.4')),
                     1,
                     lex(out),
                     [],
                     A,
                     'substance_emission-43.4_f7') :-
    nop(english("The water seeped out.")),
    into_lf((emit(E, Source, Theme), ~isa(Theme, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Source),
                     verb(vn('substance_emission-43.4')),
                     1,
                     np(Theme, [-tAnimate]),
                     [],
                     A,
                     'substance_emission-43.4_f8') :-
    nop(english("The bill shed 50 pages of pork-barrel addons.")),
    into_lf((emit(E, Source, Theme), ~isa(Theme, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent),
                     verb(vn('substance_emission-43.4-1')),
                     1,
                     np(Source),
                     [],
                     A,
                     'substance_emission-43.4-1_f0') :-
    nop(english("I bled him.")),
    into_lf((emit(E, Source, Theme), cause(Agent, E), ~isa(Theme, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('succeed-74-1')),
                     0,
                     [],
                     [],
                     A,
                     'succeed-74-1_f0') :-
    nop(english("I succeeded.")),
    into_lf((successful_in(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('succeed-74-1')),
                     2,
                     prep(in),
                     np(Theme, [+sc_ing]),
                     A,
                     'succeed-74-1_f1') :-
    nop(english("I succeeded in climbing the mountain.")),
    into_lf((successful_in(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('succeed-74-1')),
                     2,
                     prep(in),
                     np(Theme, [-sentential]),
                     A,
                     'succeed-74-1_f2') :-
    nop(english("I succeeded in the endeavour.")),
    into_lf((successful_in(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('succeed-74-1-1')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'succeed-74-1-1_f0') :-
    nop(english("I managed to pass the test.")),
    into_lf((successful_in(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('succeed-74-2')),
                     0,
                     [],
                     [],
                     A,
                     'succeed-74-2_f0') :-
    nop(english("I failed.")),
    into_lf((~successful_in(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('succeed-74-2')),
                     1,
                     np(Theme, [-sentential]),
                     [],
                     A,
                     'succeed-74-2_f1') :-
    nop(english("I failed the test.")),
    into_lf((~successful_in(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('succeed-74-2')),
                     2,
                     prep(in),
                     np(Theme, [+sc_ing]),
                     A,
                     'succeed-74-2_f2') :-
    nop(english("I failed in attempting the test.")),
    into_lf((~successful_in(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('succeed-74-2')),
                     2,
                     prep(in),
                     np(Theme, [-sentential]),
                     A,
                     'succeed-74-2_f3') :-
    nop(english("I failed in maths.")),
    into_lf((~successful_in(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('succeed-74-2')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'succeed-74-2_f4') :-
    nop(english("I failed to pass the test.")),
    into_lf((~successful_in(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('suffocate-40.7')),
                     1,
                     np(Theme, [+tAnimate]),
                     [],
                     A,
                     'suffocate-40.7_f0') :-
    nop(english("The pirates drowned the sailor.")),
    into_lf((cause(Agent, E), suffocate(E, Theme), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('suffocate-40.7')),
                     0,
                     [],
                     [],
                     A,
                     'suffocate-40.7_f1') :-
    nop(english("The sailor drowned.")),
    into_lf((suffocate(E, Theme), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('suffocate-40.7')),
                     2,
                     prep(to),
                     np(Result, [+state]),
                     A,
                     'suffocate-40.7_f2') :-
    nop(english("He choked/suffocated to death.")),
    into_lf((suffocate(E, Theme), holds(Pred, ResultE, Theme), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('suffocate-40.7')),
                     3,
                     [np(Theme, [+tAnimate])],
                     [prep(to), np(Result, [+state])],
                     A,
                     'suffocate-40.7_f3') :-
    nop(english("The pirate choked the sailor to death.")),
    into_lf((cause(Agent, E), suffocate(E, Theme), holds(Pred, ResultE, Theme), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('suspect-81')),
                     3,
                     [np(Theme)],
                     [prep(of), np(Attribute, [-sentential])],
                     A,
                     'suspect-81_f0') :-
    nop(english("I suspected him of infidelity.")),
    into_lf((suspect(E, Agent, Theme, Attribute), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('suspect-81')),
                     3,
                     [np(Theme)],
                     [prep(of), np(Attribute, [+oc_ing])],
                     A,
                     'suspect-81_f1') :-
    nop(english("I suspected him of lying.")),
    into_lf((suspect(E, Agent, Theme, Attribute), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('suspect-81')),
                     1,
                     np(Theme, [+poss_ing]),
                     [],
                     A,
                     'suspect-81_f2') :-
    nop(english("I suspected his philandering.")),
    into_lf((suspect(E, Agent, Theme, Attribute), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('sustain-55.6')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'sustain-55.6_f0') :-
    nop(english("They maintained a strong government.")),
    into_lf((continue(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('sustain-55.6')),
                     3,
                     [np(Theme)],
                     [prep(through), np(Instrument, [-sentential])],
                     A,
                     'sustain-55.6_f1') :-
    nop(english("I maintained a strong government through ruthless domination.")),
    into_lf((continue(E, Agent, Theme), use(E, Agent, Instrument), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('sustain-55.6')),
                     3,
                     [np(Theme)],
                     [prep(through), np(Instrument, [+ac_ing])],
                     A,
                     'sustain-55.6_f2') :-
    nop(english("They maintained a strong government through ruthlessly crushing opponents.")),
    into_lf((continue(E, Agent, Theme), use(E, Agent, Instrument), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tPlural, +tConcrete]),
                     verb(vn('swarm-47.5.1-1')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     A,
                     'swarm-47.5.1-1_f0') :-
    nop(english("Bees are swarming in the garden.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), motion(E, Theme), isa(Theme, tConcrete), isa(Theme, tPlural), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('swarm-47.5.1-1')),
                     3,
                     [np(Theme, [+tPlural, +tConcrete, -definite])],
                     [ prep(Prep, [+loc]),
                       np(Location, [-tRegion, +tLocation])
                     ],
                     A,
                     'swarm-47.5.1-1_f1') :-
    nop(english("There swarm bees in the garden.")),
    into_lf((exist(E, Theme), motion(E, Theme), holds(Prep, E, Theme, Location), isa(Theme, tConcrete), isa(Theme, tPlural), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     2,
                     verb(vn('swarm-47.5.1-1')),
                     np(Theme, [+tPlural, +tConcrete]),
                     A,
                     'swarm-47.5.1-1_f2') :-
    nop(english("In the aquarium swam a striped fish.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), motion(E, Theme), isa(Theme, tConcrete), isa(Theme, tPlural), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Location, [-tRegion, +tLocation]),
                     verb(vn('swarm-47.5.1-1')),
                     2,
                     prep(with),
                     np(Theme, [+tPlural, +tConcrete]),
                     A,
                     'swarm-47.5.1-1_f3') :-
    nop(english("The garden is swarming with bees.")),
    into_lf((exist(E, Theme), location(E, Theme, Location), motion(E, Theme), isa(Theme, tConcrete), isa(Theme, tPlural), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tPlural, +tConcrete]),
                     verb(vn('swarm-47.5.1-2')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location, [-tRegion, +tLocation]),
                     A,
                     'swarm-47.5.1-2_f0') :-
    nop(english("Flowers abound in the garden.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), isa(Theme, tConcrete), isa(Theme, tPlural), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(there),
                     verb(vn('swarm-47.5.1-2')),
                     3,
                     [np(Theme, [+tPlural, +tConcrete, -definite])],
                     [ prep(Prep, [+loc]),
                       np(Location, [-tRegion, +tLocation])
                     ],
                     A,
                     'swarm-47.5.1-2_f1') :-
    nop(english("There abound flowers in the garden.")),
    into_lf((exist(E, Theme), holds(Prep, E, Theme, Location), isa(Theme, tConcrete), isa(Theme, tPlural), isa(Location, tLocation), ~isa(Location, tRegion), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Location, [-tRegion, +tLocation]),
                     verb(vn('swarm-47.5.1-2-1')),
                     2,
                     prep(with),
                     np(Theme, [+tPlural, +tConcrete]),
                     A,
                     'swarm-47.5.1-2-1_f0') :-
    nop(english("The garden abounds with flowers.")),
    into_lf((exist(E, Theme), location(E, Theme, Location), isa(Theme, tConcrete), isa(Theme, tPlural), isa(Location, tLocation), ~isa(Location, tRegion), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('swat-18.2')),
                     1,
                     np(Patient, [+tConcrete]),
                     [],
                     A,
                     'swat-18.2_f0') :-
    nop(english("Paula swatted the fly.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Location, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('swat-18.2')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep(with), np(Instrument, [+tSolid])],
                     A,
                     'swat-18.2_f1') :-
    nop(english("Paula swatted the fly with a dishcloth.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), use(E, Agent, Instrument), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Location, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('swat-18.2')),
                     2,
                     lex(at),
                     np(Patient, [+tConcrete]),
                     A,
                     'swat-18.2_f2') :-
    nop(english("Paula swatted at the fly.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Location, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('swat-18.2')),
                     4,
                     [],
                     [ lex(at),
                       np(Patient, [+tConcrete]),
                       prep(with),
                       np(Instrument, [+tSolid])
                     ],
                     A,
                     'swat-18.2_f3') :-
    nop(english("Paula swatted at the fly with a dishcloth.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), use(E, Agent, Instrument), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Location, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('swat-18.2')),
                     2,
                     np(Patient, [+tConcrete]),
                     np(Result),
                     A,
                     'swat-18.2_f4') :-
    nop(english("Paula sliced the bag open.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), holds(Pred, ResultE, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('swat-18.2')),
                     4,
                     [np(Patient, [+tConcrete]), np(Result)],
                     [prep(with), np(Instrument, [+tSolid])],
                     A,
                     'swat-18.2_f5') :-
    nop(english("Paula swatted the fly dead with a dishcloth.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('swat-18.2')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'swat-18.2_f6') :-
    nop(english("The cat clawed the couch to pieces.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), holds(Pred, ResultE, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('swat-18.2')),
                     5,
                     [np(Patient, [+tConcrete])],
                     [ prep((to;into)),
                       np(Result, [+state]),
                       prep(with),
                       np(Instrument, [+tSolid])
                     ],
                     A,
                     'swat-18.2_f7') :-
    nop(english("The cat clawed the counch to pieces with her sharp nails.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Instrument), ~contact(E, Instrument, Patient), manner(EndE, Forceful, Instrument), contact(EndE, Instrument, Patient), holds(Pred, ResultE, Patient), use(E, Agent, Instrument), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant'), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('swat-18.2')),
                     3,
                     [np(Patient, [+tConcrete])],
                     [ prep(Prep, [+loc]),
                       np(Location, [+tConcrete, +body_part])
                     ],
                     A,
                     'swat-18.2_f8') :-
    nop(english("Paula swatted Deirdre on the back.")),
    into_lf((cause(Agent, E), manner(E, Directedmotion, Agent), ~contact(E, Agent, Patient), manner(EndE, Forceful, Agent), contact(EndE, Agent, Patient), isa(Agent, tAnimate), isa(Patient, tConcrete), isa(Instrument, tSolid), isa(Location, tConcrete), isa(E, actEvent), isa(Directedmotion, 'Constant'), isa(EndE, actEvent), end(E, EndE), isa(Forceful, 'Constant')),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('talk-37.5')),
                     0,
                     [],
                     [],
                     A,
                     'talk-37.5_f0') :-
    nop(english("Susan talked.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('talk-37.5')),
                     2,
                     prep(to),
                     np(Co_Agent,
                        [ or(isa(Co_Agent, tAnimate),
                             isa(Co_Agent, tOrganization))
                        ]),
                     A,
                     'talk-37.5_f1') :-
    nop(english("Susan talked to Rachel.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('talk-37.5')),
                     2,
                     prep(with),
                     np(Co_Agent,
                        [ or(isa(Co_Agent, tAnimate),
                             isa(Co_Agent, tOrganization))
                        ]),
                     A,
                     'talk-37.5_f2') :-
    nop(english("Susan talked with Rachel.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), transfer_info(E, Co_Agent, Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('talk-37.5')),
                     4,
                     [],
                     [ prep(to),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+tCommunication])
                     ],
                     A,
                     'talk-37.5_f3') :-
    nop(english("Susan talked to Rachel about the problem.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('talk-37.5')),
                     4,
                     [],
                     [ prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ]),
                       prep(about),
                       np(Topic, [+tCommunication])
                     ],
                     A,
                     'talk-37.5_f4') :-
    nop(english("Susan talked with Rachel about the problem.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), transfer_info(E, Co_Agent, Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('talk-37.5')),
                     0,
                     [],
                     [],
                     A,
                     'talk-37.5_f5') :-
    nop(english("Susan and Rachel talked.")),
    into_lf((transfer_info(E, Agent_i, Agent_j, Topic), transfer_info(E, Agent_j, Agent_i, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization)),
                          +plural
                        ]),
                     verb(vn('talk-37.5')),
                     1,
                     lex(together),
                     [],
                     A,
                     'talk-37.5_f6') :-
    nop(english("Susan and Rachel talked together.")),
    into_lf((transfer_info(E, Agent_i, Agent_j, Topic), transfer_info(E, Agent_j, Agent_i, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('talk-37.5')),
                     4,
                     [],
                     [ prep(about),
                       np(Topic, [+tCommunication]),
                       prep(to),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ])
                     ],
                     A,
                     'talk-37.5_f7') :-
    nop(english("Susan talked about the problem to Rachel.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('talk-37.5')),
                     4,
                     [],
                     [ prep(about),
                       np(Topic, [+tCommunication]),
                       prep(with),
                       np(Co_Agent,
                          [ or(isa(Co_Agent, tAnimate),
                               isa(Co_Agent, tOrganization))
                          ])
                     ],
                     A,
                     'talk-37.5_f8') :-
    nop(english("Susan talked about the problem with Rachel.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), transfer_info(E, Co_Agent, Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('talk-37.5')),
                     2,
                     prep(about),
                     np(Topic, [+tCommunication]),
                     A,
                     'talk-37.5_f9') :-
    nop(english("Susan talked about the problems of modern America.")),
    into_lf((transfer_info(E, Agent, Co_Agent, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Co_Agent, tAnimate), isa(Co_Agent, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('tape-22.4')),
                     1,
                     np(Patient, [+tSolid]),
                     [],
                     A,
                     'tape-22.4_f0') :-
    nop(english("Linda taped the picture.")),
    into_lf((cause(Agent, E), attached(EndE, Patient, Instrument), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Instrument, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('tape-22.4')),
                     3,
                     [np(Patient, [+tSolid])],
                     [prep((to;on;onto)), np(Co_Patient, [+tSolid])],
                     A,
                     'tape-22.4_f1') :-
    nop(english("Linda taped the picture to the wall.")),
    into_lf((cause(Agent, E), together(EndE, Physical, Patient, Co_Patient), attached(EndE, Patient, Instrument), attached(EndE, Co_Patient, Instrument), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant'), isa(Instrument, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('tape-22.4')),
                     2,
                     np(Patient, [+tSolid, +plural]),
                     lex(together),
                     A,
                     'tape-22.4_f2') :-
    nop(english("Linda taped the label and the cover together.")),
    into_lf((cause(Agent, E), together(EndE, Physical, Patient_i, Patient_j), attached(EndE, Patient_i, Instrument), attached(EndE, Patient_j, Instrument), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant'), isa(Instrument, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid]),
                     verb(vn('tape-22.4')),
                     3,
                     [],
                     ['ADV', prep((to;on;onto)), np(Co_Patient, [+tSolid])],
                     A,
                     'tape-22.4_f3') :-
    nop(english("Labels tape easily to that kind of cover.")),
    into_lf((property('$VAR'('Patient+Co_Patient'), Prop), holds(Adv, Prop), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid, +plural]),
                     verb(vn('tape-22.4')),
                     1,
                     'ADV',
                     [],
                     A,
                     'tape-22.4_f4') :-
    nop(english("Most labels tape easily.")),
    into_lf((property('$VAR'('Patient_i+Patient_j'), Prop), holds(Adv, Prop), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid, +plural]),
                     verb(vn('tape-22.4')),
                     2,
                     'ADV',
                     lex(together),
                     A,
                     'tape-22.4_f5') :-
    nop(english("Labels and covers tape easily together.")),
    into_lf((property('$VAR'('Patient_i+Patient_j'), Prop), holds(Adv, Prop), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid, +plural]),
                     verb(vn('tape-22.4')),
                     2,
                     lex(together),
                     'ADV',
                     A,
                     'tape-22.4_f6') :-
    nop(english("Labels and covers tape together easily.")),
    into_lf((property('$VAR'('Patient_i+Patient_j'), Prop), holds(Adv, Prop), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(Adv, '$ADV'), isa(Prop, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('tape-22.4')),
                     2,
                     np(Patient, [+tSolid]),
                     np(Result),
                     A,
                     'tape-22.4_f7') :-
    nop(english("Linda taped the box shut.")),
    into_lf((cause(Agent, E), attached(EndE, Patient, Instrument), holds(Pred, ResultE, Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(Pred, '$VERB'), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Instrument, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient, [+tSolid]),
                     verb(vn('tape-22.4-1')),
                     2,
                     prep((to;on;onto)),
                     np(Co_Patient, [+tSolid]),
                     A,
                     'tape-22.4-1_f0') :-
    nop(english("It clamped on his ankle.")),
    into_lf((together(EndE, Physical, Patient, Co_Patient), attached(EndE, Patient, Instrument), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Patient, tSolid), isa(Co_Patient, tSolid), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE), isa(Physical, 'Constant'), isa(Instrument, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'tell-37.2_f0') :-
    nop(english("John informed me.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(of), np(Topic, [+tCommunication, -sentential])],
                     A,
                     'tell-37.2_f1') :-
    nop(english("John informed me of the situation.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication, +that_comp]),
                     A,
                     'tell-37.2_f2') :-
    nop(english("John informed me that his situation had changed.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2-1')),
                     1,
                     np(Topic, [+tCommunication]),
                     [],
                     A,
                     'tell-37.2-1_f0') :-
    nop(english("Ellen told a story.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2-1')),
                     3,
                     [np(Topic, [+tCommunication])],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'tell-37.2-1_f1') :-
    nop(english("Ellen told a story to Helen.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2-1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication]),
                     A,
                     'tell-37.2-1_f2') :-
    nop(english("Ellen told Helen a story.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2-1')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'tell-37.2-1_f3') :-
    nop(english("Ellen told Helen.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2-1')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(about), np(Topic, [+tCommunication])],
                     A,
                     'tell-37.2-1_f4') :-
    nop(english("Ellen told Helen about the situation.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2-1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication, +that_comp]),
                     A,
                     'tell-37.2-1_f5') :-
    nop(english("Ellen told Helen that the party would be tonight.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2-1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication, +wh_inf]),
                     A,
                     'tell-37.2-1_f6') :-
    nop(english("Ellen told Helen how to avoid the crowd.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('tell-37.2-1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+tCommunication, +oc_to_inf]),
                     A,
                     'tell-37.2-1_f7') :-
    nop(english("Ellen told Helen to come.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(Topic, tCommunication), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('throw-17.1')),
                     1,
                     np(Theme, [+tConcrete]),
                     [],
                     A,
                     'throw-17.1_f0') :-
    nop(english("Steve tossed the ball.")),
    into_lf((motion(E1, Theme), exert_force(E0, Agent, Theme), contact(EndE0, Agent, Theme), ~contact(E1, Agent, Theme), cause(Agent, E1), meets(E0, E1), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E1, actEvent), isa(E0, actEvent), isa(EndE0, actEvent), end(E0, EndE0)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('throw-17.1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ [or([+dest, +loc])],
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'throw-17.1_f1') :-
    nop(english("Steve tossed the ball to the garden.")),
    into_lf((exert_force(E0, Agent, Theme), contact(EndE0, Agent, Theme), motion(E1, Theme), ~contact(E1, Agent, Theme), ~location(StartE1, Theme, Destination), location(EndE1, Theme, Destination), cause(Agent, E1), meets(E0, E1), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('throw-17.1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'throw-17.1_f2') :-
    nop(english("Steve tossed the ball from the corner.")),
    into_lf((exert_force(E0, Agent, Theme), contact(EndE0, Agent, Theme), motion(E1, Theme), ~contact(E1, Agent, Theme), location(StartE1, Theme, Initial_Location), cause(Agent, E1), meets(E0, E1), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('throw-17.1')),
                     5,
                     [np(Theme, [+tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation]),
                       prep(Prep, [+dest_dir]),
                       np(Destination,
                          [ or(isa(Destination, tAnimate),
                               (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                          ])
                     ],
                     A,
                     'throw-17.1_f3') :-
    nop(english("Steve tossed the ball from the corner to the garden.")),
    into_lf((exert_force(E0, Agent, Theme), contact(EndE0, Agent, Theme), motion(E1, Theme), ~contact(E1, Agent, Theme), location(StartE1, Theme, Initial_Location), location(EndE1, Theme, Destination), cause(Agent, E1), meets(E0, E1), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E0, actEvent), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('throw-17.1')),
                     2,
                     np(Theme, [+tConcrete]),
                     np(Destination,
                        [ or(isa(Destination, tAnimate),
                             (isa(Destination, tLocation), ~isa(Destination, tRegion))),
                          +adv_loc
                        ]),
                     A,
                     'throw-17.1_f4') :-
    nop(english("I threw the package away.")),
    into_lf((motion(E1, Theme), exert_force(E0, Agent, Theme), contact(EndE0, Agent, Theme), ~contact(E1, Agent, Theme), cause(Agent, E), meets(E0, E1), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(E1, actEvent), isa(E0, actEvent), isa(EndE0, actEvent), end(E0, EndE0), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('throw-17.1-1')),
                     2,
                     np(Destination,
                        [ +tAnimate,
                          or(isa(Destination, tAnimate),
                             (isa(Destination, tLocation), ~isa(Destination, tRegion)))
                        ]),
                     np(Theme, [+tConcrete]),
                     A,
                     'throw-17.1-1_f0') :-
    nop(english("Steve tossed John the ball.")),
    into_lf((exert_force(E0, Agent, Theme), contact(EndE0, Agent, Theme), motion(E1, Theme), ~contact(E1, Agent, Theme), ~location(StartE1, Theme, Destination), location(EndE1, Theme, Destination), cause(Agent, E1), meets(E0, E1), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(Destination, tAnimate), isa(E0, actEvent), isa(EndE0, actEvent), end(E0, EndE0), isa(E1, actEvent), isa(StartE1, actEvent), start(E1, StartE1), isa(EndE1, actEvent), end(E1, EndE1)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('throw-17.1-1-1')),
                     3,
                     [np(Theme, [+tConcrete])],
                     [prep(into), np(Result)],
                     A,
                     'throw-17.1-1-1_f0') :-
    nop(english("The proposal throws the House's work into chaos.")),
    into_lf((exert_force(E, Cause, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(Destination, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Cause),
                     verb(vn('throw-17.1-1-1')),
                     3,
                     [],
                     [ prep(into),
                       np(Result),
                       np(Theme, [+tConcrete])
                     ],
                     A,
                     'throw-17.1-1-1_f1') :-
    nop(english("The proposal throws into chaos the House's proposal.")),
    into_lf((exert_force(E, Cause, Theme), isa(Agent, int_control), isa(Theme, tConcrete), isa(Initial_Location, tLocation), or(isa(Destination, tAnimate), (isa(Destination, tLocation), ~isa(Destination, tRegion))), isa(Destination, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient, [+tBodyPart]),
                     verb(vn('tingle-40.8.2')),
                     0,
                     [],
                     [],
                     A,
                     'tingle-40.8.2_f0') :-
    nop(english("My heart is pounding.")),
    into_lf((discomfort(E, Patient), experience(E, Experiencer), isa(Experiencer, tAnimate), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Patient, [+tBodyPart]),
                     verb(vn('tingle-40.8.2')),
                     2,
                     prep((from;at)),
                     np(Stimulus),
                     A,
                     'tingle-40.8.2_f1') :-
    nop(english("My heart is pounding from fear.")),
    into_lf((discomfort(E, Patient), experience(E, Experiencer), in_reaction_to(E, Stimulus), isa(Experiencer, tAnimate), isa(Patient, tBodyPart), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('touch-20')),
                     1,
                     np(Experiencer, [+tConcrete]),
                     [],
                     A,
                     'touch-20_f0') :-
    nop(english("Carrie touched the cat.")),
    into_lf((cause(Agent, E), contact(EndE, Agent, Experiencer), isa(Agent, int_control), isa(Experiencer, tConcrete), isa(Instrument, tSolid), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('touch-20')),
                     3,
                     [np(Experiencer, [+tConcrete])],
                     [prep(with), np(Instrument, [+tSolid])],
                     A,
                     'touch-20_f1') :-
    nop(english("Carrie touched the cat with the stick.")),
    into_lf((cause(Agent, E), contact(EndE, Instrument, Experiencer), use(E, Agent, Instrument), isa(Agent, int_control), isa(Experiencer, tConcrete), isa(Instrument, tSolid), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('touch-20-1')),
                     1,
                     np(Experiencer,
                        [ or(isa(Experiencer, tBodyPart),
                             isa(Experiencer, tRefl)),
                          +tConcrete
                        ]),
                     [],
                     A,
                     'touch-20-1_f0') :-
    nop(english("Carrie touched his shoulder.")),
    into_lf((contact(EndE, Agent, Experiencer), cause(Agent, E), isa(Agent, int_control), isa(Experiencer, tConcrete), isa(Instrument, tSolid), or(isa(Experiencer, tBodyPart), isa(Experiencer, tRefl)), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('touch-20-1')),
                     3,
                     [ np(Experiencer,
                          [ or(isa(Experiencer, tBodyPart),
                               isa(Experiencer, tRefl)),
                            +tConcrete
                          ])
                     ],
                     [prep(with), np(Instrument, [+tSolid])],
                     A,
                     'touch-20-1_f1') :-
    nop(english("Carrie touched his shoulder with the stick.")),
    into_lf((cause(Agent, E), contact(EndE, Instrument, Experiencer), use(E, Agent, Instrument), isa(Agent, int_control), isa(Experiencer, tConcrete), isa(Instrument, tSolid), or(isa(Experiencer, tBodyPart), isa(Experiencer, tRefl)), isa(E, actEvent), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('transcribe-25.4')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'transcribe-25.4_f0') :-
    nop(english("The secretary transcribed the speech.")),
    into_lf((created_image(ResultE, Theme), location(EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Destination, tConcrete), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('transcribe-25.4')),
                     3,
                     [np(Theme)],
                     [ [or([+loc, +dest_conf])],
                       np(Destination, [+tConcrete])
                     ],
                     A,
                     'transcribe-25.4_f1') :-
    nop(english("The secretary transcribed the speech into the record.")),
    into_lf((created_image(ResultE, Theme), holds(Prep, EndE, Theme, Destination), cause(Agent, E), isa(Agent, tAnimate), isa(Destination, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1')),
                     1,
                     np(Topic, [+how_extract]),
                     [],
                     A,
                     'transfer_mesg-37.1.1_f0') :-
    nop(english("I explained how it can be done.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1')),
                     1,
                     np(Topic, [+wh_inf]),
                     [],
                     A,
                     'transfer_mesg-37.1.1_f1') :-
    nop(english("I explained how to do it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+how_extract])
                     ],
                     A,
                     'transfer_mesg-37.1.1_f2') :-
    nop(english("I explained to her how it can be done.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1')),
                     3,
                     [],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ]),
                       np(Topic, [+wh_inf])
                     ],
                     A,
                     'transfer_mesg-37.1.1_f3') :-
    nop(english("I explained to her how to do it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1')),
                     1,
                     np(Topic, [+what_extract]),
                     [],
                     A,
                     'transfer_mesg-37.1.1_f4') :-
    nop(english("I explained what he should do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1')),
                     1,
                     np(Topic, [+what_inf]),
                     [],
                     A,
                     'transfer_mesg-37.1.1_f5') :-
    nop(english("I explained what to do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1')),
                     1,
                     np(Topic, [-sentential]),
                     [],
                     A,
                     'transfer_mesg-37.1.1_f6') :-
    nop(english("I explained the matter.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1')),
                     3,
                     [np(Topic, [-sentential])],
                     [ prep(to),
                       np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     A,
                     'transfer_mesg-37.1.1_f7') :-
    nop(english("I explained the matter to them.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1')),
                     2,
                     prep(from),
                     np(Source),
                     A,
                     'transfer_mesg-37.1.1_f8') :-
    nop(english("I explained from Simone de Beauvoir's essay on the Marquis de Sade.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1')),
                     0,
                     [],
                     [],
                     A,
                     'transfer_mesg-37.1.1_f9') :-
    nop(english("I explained.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1-1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [-sentential]),
                     A,
                     'transfer_mesg-37.1.1-1_f0') :-
    nop(english("Wanda taught the students French.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1-1-1')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'transfer_mesg-37.1.1-1-1_f0') :-
    nop(english("Wanda taught the students.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1-1-1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+oc_to_inf]),
                     A,
                     'transfer_mesg-37.1.1-1-1_f1') :-
    nop(english("Wanda asked me to present a new proposal.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1-1-1')),
                     3,
                     [ np(Recipient,
                          [ or(isa(Recipient, tAnimate),
                               isa(Recipient, tOrganization))
                          ])
                     ],
                     [prep(about), np(Topic, [+ac_ing])],
                     A,
                     'transfer_mesg-37.1.1-1-1_f2') :-
    nop(english("Wanda told me about climbing the mountain.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1-1-1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+that_comp]),
                     A,
                     'transfer_mesg-37.1.1-1-1_f3') :-
    nop(english("She told him that he should not go.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1-1-1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+what_extract]),
                     A,
                     'transfer_mesg-37.1.1-1-1_f4') :-
    nop(english("She told him what she was doing.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1-1-1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+what_inf]),
                     A,
                     'transfer_mesg-37.1.1-1-1_f5') :-
    nop(english("She told him what to do.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('transfer_mesg-37.1.1-1-1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+how_extract]),
                     A,
                     'transfer_mesg-37.1.1-1-1_f6') :-
    nop(english("She told him how he did it.")),
    into_lf((transfer_info(E, Agent, Recipient, Topic), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('try-61')),
                     1,
                     np(Theme, [+np_omit_ing]),
                     [],
                     A,
                     'try-61_f0') :-
    nop(english("I tried exercising.")),
    into_lf((attempt(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('try-61')),
                     1,
                     np(Theme, [-sentential]),
                     [],
                     A,
                     'try-61_f1') :-
    nop(english("I tried the exercise routine.")),
    into_lf((attempt(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('try-61')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'try-61_f2') :-
    nop(english("I tried to exercise.")),
    into_lf((attempt(E, Agent, Theme), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('turn-26.6.1')),
                     5,
                     [np(Patient)],
                     [ prep(from),
                       np(Material),
                       prep(into),
                       np(Result)
                     ],
                     A,
                     'turn-26.6.1_f0') :-
    nop(english("The witch turned him from a prince into a frog.")),
    into_lf((state(StartE, Material, Patient), ~state(StartE, Result, Patient), state(ResultE, Result, Patient), ~state(ResultE, Material, Patient), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('turn-26.6.1')),
                     4,
                     [],
                     [ prep(from),
                       np(Material),
                       prep(into),
                       np(Result)
                     ],
                     A,
                     'turn-26.6.1_f1') :-
    nop(english("He turned from a prince into a frog.")),
    into_lf((state(StartE, Material, Patient), ~state(StartE, Result, Patient), state(ResultE, Result, Patient), ~state(ResultE, Material, Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('turn-26.6.1')),
                     3,
                     [np(Patient)],
                     [prep(into), np(Result)],
                     A,
                     'turn-26.6.1_f2') :-
    nop(english("The witch turned him into a frog.")),
    into_lf((~state(StartE, Result, Patient), state(ResultE, Result, Patient), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('turn-26.6.1')),
                     2,
                     prep(into),
                     np(Result),
                     A,
                     'turn-26.6.1_f3') :-
    nop(english("He turned into a frog.")),
    into_lf((~state(StartE, Result, Patient), state(ResultE, Result, Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Patient),
                     verb(vn('turn-26.6.1')),
                     0,
                     [],
                     [],
                     A,
                     'turn-26.6.1_f4') :-
    nop(english("The man was converting again.")),
    into_lf((state(StartE, Material, Patient), ~state(ResultE, Material, Patient), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Material, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('turn-26.6.1')),
                     1,
                     np(Patient),
                     [],
                     A,
                     'turn-26.6.1_f5') :-
    nop(english("The 1980s bull market transformed the U.S. securities business.")),
    into_lf((state(StartE, Material, Patient), ~state(StartE, Result, Patient), state(ResultE, Result, Patient), ~state(ResultE, Material, Patient), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('turn-26.6.1-1')),
                     2,
                     prep(to),
                     np(Result),
                     A,
                     'turn-26.6.1-1_f0') :-
    nop(english("He converted to a world without CFCs.")),
    into_lf((state(StartE, Material, Agent), state(ResultE, Result, Agent), ~state(StartE, Result, Agent), ~state(ResultE, Material, Agent), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Material, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('turn-26.6.1-1')),
                     4,
                     [],
                     [ prep(from),
                       np(Material, [+Pred]),
                       prep(to),
                       np(Result)
                     ],
                     A,
                     'turn-26.6.1-1_f1') :-
    nop(english("The thrift converted from a mutual form in April 1986 to a stock form of ownership.")),
    into_lf((state(StartE, Material, Patient), ~state(StartE, Result, Patient), state(ResultE, Result, Patient), ~state(ResultE, Material, Patient), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(Material, Pred), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('turn-26.6.1-1')),
                     4,
                     [],
                     [prep(to), np(Result), prep(from), np(Material)],
                     A,
                     'turn-26.6.1-1_f2') :-
    nop(english("The thrift converted to a stock form of ownership from a mutual form in April 1986.")),
    into_lf((state(StartE, Material, Patient), ~state(StartE, Result, Patient), state(ResultE, Result, Patient), ~state(ResultE, Material, Patient), cause(Agent, E), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('urge-58.1')),
                     1,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     [],
                     A,
                     'urge-58.1_f0') :-
    nop(english("I advised him.")),
    into_lf((urge(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('urge-58.1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+oc_to_inf]),
                     A,
                     'urge-58.1_f1') :-
    nop(english("I advised him to come.")),
    into_lf((urge(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('urge-58.1')),
                     2,
                     np(Recipient,
                        [ or(isa(Recipient, tAnimate),
                             isa(Recipient, tOrganization))
                        ]),
                     np(Topic, [+that_comp]),
                     A,
                     'urge-58.1_f2') :-
    nop(english("I advised him that he should come.")),
    into_lf((urge(E, Agent, Recipient, Topic), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), or(isa(Recipient, tAnimate), isa(Recipient, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('use-105')),
                     2,
                     np(Theme),
                     'ADV',
                     A,
                     'use-105_f0') :-
    nop(english("I used the money well.")),
    into_lf((use(E, Agent, Theme, Predicate), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('use-105')),
                     3,
                     [np(Theme)],
                     [prep(for), np(Predicate)],
                     A,
                     'use-105_f1') :-
    nop(english("I spent the money for my training.")),
    into_lf((use(E, Agent, Theme, Predicate), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('use-105')),
                     2,
                     np(Theme),
                     np(Predicate, [+vc_to_inf]),
                     A,
                     'use-105_f2') :-
    nop(english("I used the cupboard to store food.")),
    into_lf((use(E, Agent, Theme, Predicate), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('vehicle-51.4.1')),
                     0,
                     [],
                     [],
                     A,
                     'vehicle-51.4.1_f0') :-
    nop(english("Claire skated.")),
    into_lf((motion(E, Theme), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('vehicle-51.4.1')),
                     2,
                     prep(Prep, [+path]),
                     np(Location, [+tConcrete]),
                     A,
                     'vehicle-51.4.1_f1') :-
    nop(english("Claire skated along the canal.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Location), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('vehicle-51.4.1')),
                     3,
                     [np(Theme, [+tAnimate])],
                     [ prep(Prep, [+path]),
                       np(Location, [+tConcrete])
                     ],
                     A,
                     'vehicle-51.4.1_f2') :-
    nop(english("He skated Penny around the rink.")),
    into_lf((motion(E0, Theme), holds(Prep, E0, Theme, Location), cause(Agent, E0), equals(E0, E1), motion(E1, Agent), holds(Prep, E1, Agent, Location), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E0, actEvent), isa(E1, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('vehicle-51.4.1')),
                     1,
                     np(Theme, [+tAnimate]),
                     [],
                     A,
                     'vehicle-51.4.1_f3') :-
    nop(english("He skated Penny.")),
    into_lf((motion(E0, Theme), cause(Agent, E0), equals(E0, E1), motion(E1, Agent), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(E0, actEvent), isa(E1, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('vehicle-51.4.1')),
                     2,
                     np(Theme, [+tAnimate]),
                     np(Result),
                     A,
                     'vehicle-51.4.1_f4') :-
    nop(english("He skated Penny exhausted.")),
    into_lf((motion(E, Theme), cause(Agent, E), holds(Pred, ResultE, Theme), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('vehicle-51.4.1')),
                     3,
                     [np(Theme, [+tAnimate])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'vehicle-51.4.1_f5') :-
    nop(english("He skated Penny to exhaustion.")),
    into_lf((motion(E, Theme), cause(Agent, E), holds(Pred, ResultE, Theme), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('vehicle-51.4.1-1')),
                     1,
                     np(Location, [+tConcrete]),
                     [],
                     A,
                     'vehicle-51.4.1-1_f0') :-
    nop(english("Claire skated the canals.")),
    into_lf((motion(E, Theme), via(E, Theme, Location), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tOrganization))
                        ]),
                     verb(vn('void-106')),
                     1,
                     np(Patient),
                     [],
                     A,
                     'void-106_f0') :-
    nop(english("I nullified their advantage.")),
    into_lf((cause(Agent, E), designated(StartE, Patient), ~designated(EndE, Patient), or(isa(Agent, tAnimate), isa(Agent, tOrganization)), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('waltz-51.5')),
                     0,
                     [],
                     [],
                     A,
                     'waltz-51.5_f0') :-
    nop(english("They waltzed.")),
    into_lf((motion(E, Theme), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('waltz-51.5')),
                     2,
                     prep(Prep, [+path]),
                     np(Location, [+tConcrete]),
                     A,
                     'waltz-51.5_f1') :-
    nop(english("They waltzed across the room.")),
    into_lf((motion(E, Theme), holds(Prep, E, Theme, Location), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('waltz-51.5')),
                     3,
                     [np(Theme, [+tAnimate])],
                     [ prep(Prep, [+path]),
                       np(Location, [+tConcrete])
                     ],
                     A,
                     'waltz-51.5_f2') :-
    nop(english("He waltzed her across the floor.")),
    into_lf((motion(E0, Theme), holds(Prep, E0, Theme, Location), cause(Agent, E0), equals(E0, E1), motion(E1, Agent), holds(Prep, E1, Agent, Location), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(Prep, '$PREP'), isa(E0, actEvent), isa(E1, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('waltz-51.5')),
                     2,
                     np(Theme, [+tAnimate]),
                     np(Result),
                     A,
                     'waltz-51.5_f3') :-
    nop(english("He waltzed her dizzy.")),
    into_lf((motion(E, Theme), cause(Agent, E), holds(Pred, ResultE, Result), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('waltz-51.5')),
                     3,
                     [np(Theme, [+tAnimate])],
                     [prep((to;into)), np(Result, [+state])],
                     A,
                     'waltz-51.5_f4') :-
    nop(english("He waltzed her to exhaustion.")),
    into_lf((motion(E, Theme), cause(Agent, E), holds(Pred, ResultE, Result), isa(Agent, tAnimate), isa(Theme, tAnimate), isa(Location, tConcrete), isa(Pred, '$VERB'), isa(E, actEvent), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'want-32.1_f0') :-
    nop(english("Dorothy needs new shoes.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1-1')),
                     1,
                     np(Theme, [+for_comp]),
                     [],
                     A,
                     'want-32.1-1_f0') :-
    nop(english("I need for her to be happy.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1-1')),
                     1,
                     np(Theme, [+np_omit_ing]),
                     [],
                     A,
                     'want-32.1-1_f1') :-
    nop(english("I need exercising.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1-1')),
                     2,
                     np(Theme),
                     'ADJ',
                     A,
                     'want-32.1-1_f2') :-
    nop(english("She wanted the meat red.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1-1')),
                     1,
                     np(Theme),
                     [],
                     A,
                     'want-32.1-1_f3') :-
    nop(english("I needed him here.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1-1')),
                     1,
                     np(Theme, [+acc_ing]),
                     [],
                     A,
                     'want-32.1-1_f4') :-
    nop(english("I need him cooking.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1-1')),
                     1,
                     np(Theme, [+np_ppart]),
                     [],
                     A,
                     'want-32.1-1_f5') :-
    nop(english("I need the children found.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1-1')),
                     1,
                     np(Theme, [+poss_ing]),
                     [],
                     A,
                     'want-32.1-1_f6') :-
    nop(english("I needed his cooking.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1-1')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'want-32.1-1_f7') :-
    nop(english("I needed to come.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1-1-1')),
                     1,
                     np(Theme, [+np_to_inf]),
                     [],
                     A,
                     'want-32.1-1-1_f0') :-
    nop(english("I needed him to go.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Pivot, [+tAnimate]),
                     verb(vn('want-32.1-1-1')),
                     1,
                     np(Theme, [+to_be]),
                     [],
                     A,
                     'want-32.1-1-1_f1') :-
    nop(english("I need him to be nice.")),
    into_lf((desire(E, Pivot, Theme), isa(Pivot, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(lex(it),
                     lex('[+be]'),
                     1,
                     verb(vn('weather-57')),
                     [],
                     A,
                     'weather-57_f0') :-
    nop(english("It's raining.")),
    into_lf((weather(E, Weather_type, Theme), or(isa(Theme, tConcrete), isa(Theme, tForce)), isa(E, actEvent), isa(Weather_type, Pred)),
            A).
vndata:verbnet_frame(lex(it),
                     lex('[+be]'),
                     2,
                     verb(vn('weather-57')),
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tForce))
                        ]),
                     A,
                     'weather-57_f1') :-
    nop(english("It's raining cats and dogs.")),
    into_lf((weather(E, Weather_type, Theme), or(isa(Theme, tConcrete), isa(Theme, tForce)), isa(E, actEvent), isa(Weather_type, Pred)),
            A).
vndata:verbnet_frame(lex('it[+be]'),
                     verb(vn('weather-57')),
                     2,
                     prep(with),
                     np(Theme,
                        [ or(isa(Theme, tConcrete),
                             isa(Theme, tForce))
                        ]),
                     A,
                     'weather-57_f2') :-
    nop(english("It was pelting with rain.")),
    into_lf((weather(E, Weather_type, Theme), or(isa(Theme, tConcrete), isa(Theme, tForce)), isa(E, actEvent), isa(Weather_type, Pred)),
            A).
vndata:verbnet_frame(np(Theme, [+tAnimate]),
                     verb(vn('weekend-56')),
                     2,
                     prep(Prep, [+loc]),
                     np(Location, [+tLocation]),
                     A,
                     'weekend-56_f0') :-
    nop(english("My family always summered at the seashore.")),
    into_lf((location(E, Theme, Location), isa(Theme, tAnimate), isa(Location, tLocation), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('wink-40.3.1')),
                     1,
                     np(Patient, [+tBodyPart]),
                     [],
                     A,
                     'wink-40.3.1_f0') :-
    nop(english("Linda winked her eye.")),
    into_lf((cause(Agent, E), motion(E, Patient), transfer_info(E, Agent, Recipient, Theme), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(Theme, tCommunication), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('wink-40.3.1')),
                     3,
                     [np(Patient, [+tBodyPart])],
                     [prep(in), np(Theme, [+tCommunication])],
                     A,
                     'wink-40.3.1_f1') :-
    nop(english("Linda winked her eye in agreement.")),
    into_lf((cause(Agent, E), motion(E, Patient), transfer_info(E, Agent, Recipient, Theme), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(Theme, tCommunication), isa(Recipient, tAnimate), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('wink-40.3.1-1')),
                     0,
                     [],
                     [],
                     A,
                     'wink-40.3.1-1_f0') :-
    nop(english("Linda winked.")),
    into_lf((cause(Agent, E), motion(E, Patient), transfer_info(E, Agent, Recipient, Theme), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(Theme, tCommunication), isa(Recipient, tAnimate), isa(E, actEvent), isa(Patient, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('wink-40.3.1-1')),
                     2,
                     prep(Prep, [+dest_dir]),
                     np(Recipient, [+tAnimate]),
                     A,
                     'wink-40.3.1-1_f1') :-
    nop(english("Linda winked at the audience.")),
    into_lf((cause(Agent, E), motion(E, Patient), transfer_info(E, Agent, Recipient, Theme), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(Theme, tCommunication), isa(Recipient, tAnimate), isa(E, actEvent), isa(Patient, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('wink-40.3.1-1')),
                     1,
                     np(Theme, [+tCommunication]),
                     [],
                     A,
                     'wink-40.3.1-1_f2') :-
    nop(english("Linda winked her agreement.")),
    into_lf((cause(Agent, E), motion(E, Patient), transfer_info(E, Agent, Recipient, Theme), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(Theme, tCommunication), isa(Recipient, tAnimate), isa(E, actEvent), isa(Patient, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('wink-40.3.1-1')),
                     2,
                     prep(in),
                     np(Theme, [+tCommunication]),
                     A,
                     'wink-40.3.1-1_f3') :-
    nop(english("Linda winked in agreement.")),
    into_lf((cause(Agent, E), motion(E, Patient), transfer_info(E, Agent, Recipient, Theme), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(Theme, tCommunication), isa(Recipient, tAnimate), isa(E, actEvent), isa(Patient, Pred)),
            A).
vndata:verbnet_frame(np(Agent, [+tAnimate]),
                     verb(vn('wink-40.3.1-1')),
                     1,
                     np(Theme, [+tCommunication]),
                     [],
                     A,
                     'wink-40.3.1-1_f4') :-
    nop(english("His brother nodded vigorous assent.")),
    into_lf((cause(Agent, E), motion(E, Patient), transfer_info(E, Agent, Recipient, Theme), isa(Agent, tAnimate), isa(Patient, tBodyPart), isa(Theme, tCommunication), isa(Recipient, tAnimate), isa(E, actEvent), isa(Patient, Pred)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('wipe_instr-10.4.2')),
                     3,
                     [np(Theme, [-tAnimate, +tConcrete])],
                     [ prep(Prep, [+src]),
                       np(Initial_Location, [+tLocation])
                     ],
                     A,
                     'wipe_instr-10.4.2_f0') :-
    nop(english("Carla shoveled the snow from the walk.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Initial_Location), ~location(EndE, Theme, Initial_Location), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Theme, tConcrete), ~isa(Theme, tAnimate), isa(Initial_Location, tLocation), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('wipe_instr-10.4.2')),
                     1,
                     np(Initial_Location, [+tLocation]),
                     [],
                     A,
                     'wipe_instr-10.4.2_f1') :-
    nop(english("Carla shoveled the walk.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Initial_Location), ~location(EndE, Theme, Initial_Location), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Theme, tConcrete), ~isa(Theme, tAnimate), isa(Initial_Location, tLocation), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('wipe_instr-10.4.2')),
                     0,
                     [],
                     [],
                     A,
                     'wipe_instr-10.4.2_f2') :-
    nop(english("Carla was vacuuming.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Initial_Location), ~location(EndE, Theme, Initial_Location), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Theme, tConcrete), ~isa(Theme, tAnimate), isa(Initial_Location, tLocation), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('wipe_instr-10.4.2')),
                     2,
                     np(Initial_Location, [+tLocation]),
                     np(Result),
                     A,
                     'wipe_instr-10.4.2_f3') :-
    nop(english("Carla shoveled the walk clean.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Initial_Location), ~location(EndE, Theme, Initial_Location), holds(Pred, ResultE, Initial_Location), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Theme, tConcrete), ~isa(Theme, tAnimate), isa(Initial_Location, tLocation), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(Pred, '$VERB'), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE), isa(ResultE, actEvent), result(E, ResultE)),
            A).
vndata:verbnet_frame(np(Agent,
                        [ or(isa(Agent, tAnimate),
                             isa(Agent, tMachine))
                        ]),
                     verb(vn('wipe_instr-10.4.2-1')),
                     3,
                     [np(Theme, [-tAnimate, +tConcrete])],
                     [prep(Prep, [+dest_conf]), np(Destination)],
                     A,
                     'wipe_instr-10.4.2-1_f0') :-
    nop(english("He plowed the snow back into the ditch.")),
    into_lf((cause(Agent, E), ~location(StartE, Theme, Destination), location(EndE, Theme, Destination), or(isa(Agent, tAnimate), isa(Agent, tMachine)), isa(Theme, tConcrete), ~isa(Theme, tAnimate), isa(Initial_Location, tLocation), isa(Instrument, tConcrete), ~isa(Instrument, tAnimate), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('wipe_manner-10.4.1')),
                     3,
                     [np(Theme, [-tAnimate, +tConcrete])],
                     [prep(Prep, [+src]), np(Source, [+tLocation])],
                     A,
                     'wipe_manner-10.4.1_f0') :-
    nop(english("Brian wiped the fingerprints from the counter.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), isa(Agent, int_control), isa(Theme, tConcrete), ~isa(Theme, tAnimate), isa(Source, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('wipe_manner-10.4.1')),
                     1,
                     np(Source, [+tLocation]),
                     [],
                     A,
                     'wipe_manner-10.4.1_f1') :-
    nop(english("Brian wiped the counter.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), isa(Agent, int_control), isa(Theme, tConcrete), ~isa(Theme, tAnimate), isa(Source, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('wipe_manner-10.4.1')),
                     1,
                     np(Theme, [-tAnimate, +tConcrete]),
                     [],
                     A,
                     'wipe_manner-10.4.1_f2') :-
    nop(english("Barry Cryer erased the writing.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), ~location(EndE, Theme, Source), isa(Agent, int_control), isa(Theme, tConcrete), ~isa(Theme, tAnimate), isa(Source, tLocation), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Agent, [+int_control]),
                     verb(vn('wipe_manner-10.4.1-1')),
                     2,
                     lex(at),
                     np(Source, [-tRegion, +tLocation]),
                     A,
                     'wipe_manner-10.4.1-1_f0') :-
    nop(english("Brian wiped at the counter.")),
    into_lf((cause(Agent, E), location(StartE, Theme, Source), location(EndE, Theme, Source), isa(Agent, int_control), isa(Theme, tConcrete), ~isa(Theme, tAnimate), isa(Source, tLocation), ~isa(Source, tRegion), isa(E, actEvent), isa(StartE, actEvent), start(E, StartE), isa(EndE, actEvent), end(E, EndE)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('wish-62')),
                     1,
                     np(Theme, [+for_comp]),
                     [],
                     A,
                     'wish-62_f0') :-
    nop(english("I wished for her to do it.")),
    into_lf((desire(E, Experiencer, Theme), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('wish-62')),
                     1,
                     np(Theme, [-sentential]),
                     [],
                     A,
                     'wish-62_f1') :-
    nop(english("I wished it.")),
    into_lf((desire(E, Experiencer, Theme), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('wish-62')),
                     1,
                     np(Theme, [+to_be]),
                     [],
                     A,
                     'wish-62_f2') :-
    nop(english("I wished him to be nice.")),
    into_lf((desire(E, Experiencer, Theme), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('wish-62')),
                     1,
                     np(Theme, [+np_ppart]),
                     [],
                     A,
                     'wish-62_f3') :-
    nop(english("I wished the children found.")),
    into_lf((desire(E, Experiencer, Theme), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('wish-62')),
                     1,
                     np(Theme, [+that_comp]),
                     [],
                     A,
                     'wish-62_f4') :-
    nop(english("He wished that she would come immediately.")),
    into_lf((desire(E, Experiencer, Theme), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Experiencer,
                        [ or(isa(Experiencer, tAnimate),
                             isa(Experiencer, tOrganization))
                        ]),
                     verb(vn('wish-62')),
                     1,
                     np(Theme, [+sc_to_inf]),
                     [],
                     A,
                     'wish-62_f5') :-
    nop(english("He wished to come.")),
    into_lf((desire(E, Experiencer, Theme), or(isa(Experiencer, tAnimate), isa(Experiencer, tOrganization)), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('withdraw-82-1')),
                     0,
                     [],
                     [],
                     A,
                     'withdraw-82-1_f0') :-
    nop(english("He backed out.")),
    into_lf((renege(E, Agent, Source), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('withdraw-82-1')),
                     2,
                     prep(of),
                     np(Source),
                     A,
                     'withdraw-82-1_f1') :-
    nop(english("He backed out of the trip.")),
    into_lf((renege(E, Agent, Source), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('withdraw-82-1')),
                     2,
                     prep(of),
                     np(Source, [+sc_ing]),
                     A,
                     'withdraw-82-1_f2') :-
    nop(english("He backed out of going on the trip.")),
    into_lf((renege(E, Agent, Source), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('withdraw-82-2')),
                     1,
                     lex(away),
                     [],
                     A,
                     'withdraw-82-2_f0') :-
    nop(english("He got away.")),
    into_lf((renege(E, Agent, Source), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('withdraw-82-2')),
                     2,
                     prep(from),
                     np(Source),
                     A,
                     'withdraw-82-2_f1') :-
    nop(english("He got away from the area.")),
    into_lf((renege(E, Agent, Source), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('withdraw-82-2')),
                     2,
                     prep(from),
                     np(Source, [+sc_ing]),
                     A,
                     'withdraw-82-2_f2') :-
    nop(english("He backed away from going on the trip.")),
    into_lf((renege(E, Agent, Source), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('withdraw-82-3')),
                     0,
                     [],
                     [],
                     A,
                     'withdraw-82-3_f0') :-
    nop(english("He withdrew.")),
    into_lf((renege(E, Agent, Source), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(E, actEvent)),
            A).
vndata:verbnet_frame(np(Agent, [+tOrganization, +tAnimate]),
                     verb(vn('withdraw-82-3')),
                     2,
                     prep(from),
                     np(Source),
                     A,
                     'withdraw-82-3_f1') :-
    nop(english("He withdrew from the trip.")),
    into_lf((renege(E, Agent, Source), isa(Agent, tAnimate), isa(Agent, tOrganization), isa(E, actEvent)),
            A).

true.

mu:  ?-

