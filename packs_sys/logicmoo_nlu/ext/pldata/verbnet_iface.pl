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

