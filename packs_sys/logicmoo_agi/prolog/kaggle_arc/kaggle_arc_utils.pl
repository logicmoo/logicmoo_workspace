/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

/*
my_len(X,Y):- var(X),!,length(X,Y).
my_len(X,Y):- is_list(X),!,length(X,Y).
my_len(X,Y):- functor([_|_],F,A),functor(X,F,A),!,length(X,Y).
my_len(X,Y):- dumpST,!,break.
*/

set_nth1(1,[_|Row],E,[E|Row]):-!.
set_nth1(N,[W|Row],E,[W|RowMod]):- Nm1 is N-1, set_nth1(Nm1,Row,E,RowMod).

make_list_inited(0,_,[]):-!.
make_list_inited(1,E,[E]):-!.
make_list_inited(N,E,[E|List]):- Nm1 is N -1,make_list_inited(Nm1,E,List).

nth_fact(P,I):- clause(P,true,Ref),nth_clause(P,I,Ref).

nonvar_or_ci(C):- (nonvar(C);attvar(C)),!.

add_i(Info):- 
 quietly((tersify(Info,InfoT),
 nb_current(test_rules,TRules),
 nb_current(pair_rules,PRules),
  nb_set_add(TRules,InfoT),
  nb_set_add(PRules,InfoT),
 nop(pt(cyan,+InfoT)))).

add_i(F,Info):- 
 append_term(i(F),Info,FInfo),
 add_i(FInfo).

add_rule(Info):- add_i(rule,Info).
add_cond(Info):- add_i(cond,Info).
%do_action(Info):- guess_pretty(Info),add_i(action,Info),call(Info).
do_action(Call):- !, copy_term(Call,Info),call(Call),add_i(action,Info).
add_action(Info):- add_i(action,Info).
add_note(Info):- add_i(note,Info).
add_indiv(W,Info):- add_i(indiv(W),Info).
add_comparitor(Info):- add_i(comparitor,Info).
show_rules:- 
 nb_current(pair_rules,PRules), maplist(pt(cyan),PRules),
 nb_current(test_rules,TRules), maplist(pt(blue),TRules),
 !.
  


contains_nonvar(N,Info):- sub_term(E,Info),nonvar_or_ci(E),E=N,!.

max_min(A,B,C,D):- must_be_free(C),must_be_free(D),notrace(max_min0(A,B,C,D)).
max_min0(A,B,B,B):- plain_var(A),!.
max_min0(A,B,A,A):- plain_var(B),!.
max_min0(A,B,A,B):- A>B,!.
max_min0(A,B,B,A).

as_debug(9,_):- !.
as_debug(_,G):- wots(S,G),format('~NDEBUG: ~w~N',[S]).

count_each([],_,[]).
count_each([C|L],GC,[Len-C|LL]):- include(==(C),GC,Lst),length(Lst,Len),count_each(L,GC,LL).

/*
print_points_grid(Points):- 
 points_range(Points, LoH, LoV, HiH, HiV, H, V), writeqln(size_range(LoH, LoV, HiH, HiV, H, V)), points_to_grid(Points, Grid), print_grid(Grid).

print_points_grid(Grid):- 
 points_range(Grid, LoH, LoV, HiH, HiV, _H, _V), print_grid(Grid, LoH, LoV, HiH, HiV, Grid).
*/


%print_trainer:- kaggle_arc_train(Name, Stuff), atom_json_term(Stuff, JSON, []), print_arc(Name, JSON).
%print_evaler:- kaggle_arc_eval(Name, Stuff), atom_json_term(Stuff, JSON, []), print_arc(Name, JSON).

 /*
% data looks like

kaggle_arc_train('007bbfb7', trn, [[0, 7, 7], [7, 7, 7], [0, 7, 7]], [[0,0,0,0, 7, 7,0, 7, 7], [0,0,0, 7, 7, 7, 7, 7, 7], [0,0,0,0, 7, 7,0, 7, 7], [0, 7, 7,0, 7, 7,0, 7, 7], [7, 7, 7, 7, 7, 7, 7, 7, 7], [0, 7, 7,0, 7, 7,0, 7, 7], [0,0,0,0, 7, 7,0, 7, 7], [0,0,0, 7, 7, 7, 7, 7, 7], [0,0,0,0, 7, 7,0, 7, 7]]).
kaggle_arc_train('007bbfb7', trn, [[4,0, 4], [0,0,0], [0, 4,0]], [[4,0, 4,0,0,0, 4,0, 4], [0,0,0,0,0,0,0,0,0], [0, 4,0,0,0,0,0, 4,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0, 4,0, 4,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0, 4,0,0,0,0]]).
kaggle_arc_train('007bbfb7', trn, [[0,0,0], [0,0, 2], [2,0, 2]], [[0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 2], [0,0,0,0,0,0, 2,0, 2], [0,0,0,0,0,0,0,0,0], [0,0, 2,0,0,0,0,0, 2], [2,0, 2,0,0,0, 2,0, 2]]).
kaggle_arc_train('007bbfb7', trn, [[6, 6,0], [6,0,0], [0, 6, 6]], [[6, 6,0, 6, 6,0,0,0,0], [6,0,0, 6,0,0,0,0,0], [0, 6, 6,0, 6, 6,0,0,0], [6, 6,0,0,0,0,0,0,0], [6,0,0,0,0,0,0,0,0], [0, 6, 6,0,0,0,0,0,0], [0,0,0, 6, 6,0, 6, 6,0], [0,0,0, 6,0,0, 6,0,0], [0,0,0,0, 6, 6,0, 6, 6]]).
kaggle_arc_train('007bbfb7', trn, [[2, 2, 2], [0,0,0], [0, 2, 2]], [[2, 2, 2, 2, 2, 2, 2, 2, 2], [0,0,0,0,0,0,0,0,0], [0, 2, 2,0, 2, 2,0, 2, 2], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0, 2, 2, 2, 2, 2, 2], [0,0,0,0,0,0,0,0,0], [0,0,0,0, 2, 2,0, 2, 2]]).
kaggle_arc_train('007bbfb7', tst, [[7,0, 7], [7,0, 7], [7, 7,0]], [[7,0, 7,0,0,0, 7,0, 7], [7,0, 7,0,0,0, 7,0, 7], [7, 7,0,0,0,0, 7, 7,0], [7,0, 7,0,0,0, 7,0, 7], [7,0, 7,0,0,0, 7,0, 7], [7, 7,0,0,0,0, 7, 7,0], [7,0, 7, 7,0, 7,0,0,0], [7,0, 7, 7,0, 7,0,0,0], [7, 7,0, 7, 7,0,0,0,0]]).

kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0], [0,0, 3,0,0,0], [0, 3,0, 3,0,0], [0,0, 3,0, 3,0], [0,0,0, 3,0,0], [0,0,0,0,0,0]], [[0,0,0,0,0,0], [0,0, 3,0,0,0], [0, 3, 4, 3,0,0], [0,0, 3, 4, 3,0], [0,0,0, 3,0,0], [0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0,0,0,0,0], [0,0,0, 3,0, 3,0,0,0,0], [0,0, 3,0,0,0, 3,0,0,0], [0,0,0,0,0, 3,0, 3,0,0], [0,0,0, 3,0, 3, 3,0,0,0], [0,0, 3, 3, 3,0,0,0,0,0], [0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0,0,0,0,0], [0,0,0, 3,0, 3,0,0,0,0], [0,0, 3,0,0,0, 3,0,0,0], [0,0,0,0,0, 3, 4, 3,0,0], [0,0,0, 3,0, 3, 3,0,0,0], [0,0, 3, 3, 3,0,0,0,0,0], [0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0, 3,0,0,0,0], [0,0,0,0, 3,0,0,0,0,0], [0, 3, 3,0, 3, 3,0, 3,0,0], [3,0,0, 3,0,0, 3,0, 3,0], [0,0,0, 3,0,0, 3, 3,0,0], [0,0,0, 3,0,0, 3,0,0,0], [0,0,0, 3,0,0, 3,0,0,0], [0,0,0,0, 3, 3,0, 3,0,0], [0,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0, 3,0,0,0,0], [0,0,0,0, 3,0,0,0,0,0], [0, 3, 3,0, 3, 3,0, 3,0,0], [3,0,0, 3, 4, 4, 3, 4, 3,0], [0,0,0, 3, 4, 4, 3, 3,0,0], [0,0,0, 3, 4, 4, 3,0,0,0], [0,0,0, 3, 4, 4, 3,0,0,0], [0,0,0,0, 3, 3,0, 3,0,0], [0,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0,0,0,0,0], [0,0, 3, 3, 3, 3,0,0,0,0], [0,0, 3,0,0, 3,0,0,0,0], [0,0, 3,0,0, 3,0, 3,0,0], [0,0, 3, 3, 3, 3, 3, 3, 3,0], [0,0,0, 3,0,0,0,0, 3,0], [0,0,0, 3,0,0,0, 3, 3,0], [0,0,0, 3, 3,0,0, 3,0, 3], [0,0,0, 3,0, 3,0,0, 3,0], [0,0,0,0, 3,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0], [0,0, 3, 3, 3, 3,0,0,0,0], [0,0, 3, 4, 4, 3,0,0,0,0], [0,0, 3, 4, 4, 3,0, 3,0,0], [0,0, 3, 3, 3, 3, 3, 3, 3,0], [0,0,0, 3,0,0,0,0, 3,0], [0,0,0, 3,0,0,0, 3, 3,0], [0,0,0, 3, 3,0,0, 3, 4, 3], [0,0,0, 3, 4, 3,0,0, 3,0], [0,0,0,0, 3,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3,0, 3, 3,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0, 3,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0,0,0, 3,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0, 3,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3,0,0,0,0, 3,0, 3,0,0], [0,0,0,0,0,0, 3, 3,0,0, 3,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3,0,0, 3, 3,0,0, 3,0,0, 3,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0, 3,0, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3, 4, 3, 3,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 3,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0,0,0, 3,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0, 3,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3,0,0,0,0, 3,0, 3,0,0], [0,0,0,0,0,0, 3, 3, 4, 4, 3,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 4, 4, 3, 3,0,0, 3,0,0, 3,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0, 3, 4, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', tst, [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0, 3,0, 3, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3, 3, 3, 3, 3,0, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0, 3,0,0,0,0, 3,0,0, 3,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3, 3,0, 3, 3, 3,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0,0, 3,0,0,0, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3,0,0,0, 3,0,0], [0,0,0,0,0,0, 3, 3,0, 3,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0, 3,0,0,0,0,0, 3, 3,0,0,0,0,0,0,0,0,0,0], [0, 3,0, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0, 3, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3,0,0,0, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3,0,0,0, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0, 3, 4, 3, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3, 3, 3, 3, 3,0, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0, 3, 4, 4, 4, 4, 3, 4, 4, 3,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3, 3,0, 3, 3, 3,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0,0, 3, 4, 4, 4, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0, 3, 3, 4, 3,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0, 3,0,0,0,0,0, 3, 3,0,0,0,0,0,0,0,0,0,0], [0, 3, 4, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0, 3, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
*/
%tell(s), ignore((nl, nl, test_pairs(Name, ExampleNum, In, Out), format('~N~q.~n', [test_pairs_cache(Name, ExampleNum, In, Out)]), fail)), told.
map_pred(Pred, P, X) :- call(Pred, P, X)*->true;map_pred0([],Pred, P, X).
map_pred(NoCycles,_Pred, P, X) :- member(E,NoCycles), E==P,!, X = P.
map_pred(NoCycles,Pred, P, X) :- call(Pred, P, X)*->true;map_pred0(NoCycles,Pred, P, X).

map_pred0(_NoCycles,_Pred, Args, ArgSO) :- Args==[],!, ArgSO=[].
map_pred0(NoCycles,Pred, P, X) :- fail,  attvar(P), !, %duplicate_term(P,X),P=X,
  get_attrs(P,VS), 
  map_pred([P|NoCycles],Pred, VS, VSX),
  P=X, 
  put_attrs(X,VSX),!.
map_pred0(_NoCycles,_Pred, P, P1) :- (is_ftVar(P); \+ compound(P)), !, must(P1=P), !.
% map_pred0(NoCycles,Pred, Args, ArgSO) :- is_list(Args), !,  maplist(map_pred([Args|NoCycles],Pred), Args, ArgS), ArgS=ArgSO.
map_pred0(NoCycles,Pred, IO, [O|ArgS]) :- 
  IO= [I|Args], !, 
  map_pred([IO,ArgS|NoCycles],Pred, I, O), 
  map_pred0([IO,I|NoCycles],Pred, Args, ArgS).
map_pred0(NoCycles,Pred, P, P1) :-  
  compound_name_arguments(P, F, Args),  
  maplist(map_pred([P|NoCycles],Pred),Args,ArgS), 
  compound_name_arguments(P1, F, ArgS).
%map_pred(_Pred, P, P).


:- meta_predicate map_pred(2, ?, ?, ?, ?).
map_pred(Pred, P, X, Sk, P1) :- must_be_free(X), call(Pred, P, X), !, must(Sk=P1), !.
map_pred(_Pred, P, _, _, P1) :- is_ftVar(P), !, must(P1=P), !.
map_pred(Pred, [P|Args], X, Sk, [P1|ArgS]) :- !, map_pred(Pred, P, X, Sk, P1), !, must(map_pred(Pred, Args, X, Sk, ArgS)), !.
map_pred(Pred, P, X, Sk, P1) :- compound(P), !, compound_name_arguments(P, F, Args), map_pred(Pred, [F|Args], X, Sk, [Fs|ArgS]), !, compound_name_arguments(P1, Fs, ArgS), !.
map_pred(_Pred, P, _, _, P).


plain_var(V):- var(V), \+ get_attr(V,ci,_).

must_be_free(AllNew):- var(AllNew),!.
must_be_free(AllNew):- dumpST,wdmsg(must_be_free(AllNew)),break,fail.

intersection([],LeftOverB,[],[],LeftOverB):-!.
intersection(LeftOverA,[],[],LeftOverA,[]):-!.
intersection([A|APoints],BPoints,[A|Intersected],LeftOverA,LeftOverB):-
  select(A,BPoints,BPointsMinusA),!,
  intersection(APoints,BPointsMinusA,Intersected,LeftOverA,LeftOverB).
intersection([A|APoints],BPoints,Intersected,[A|LeftOverA],LeftOverB):-
  intersection(APoints,BPoints,Intersected,LeftOverA,LeftOverB).


pred_intersection(_P2,[],LeftOverB,  [],[], [],LeftOverB):-!.
pred_intersection(_P2,LeftOverA,[],  [],[], LeftOverA,[]):-!.
pred_intersection(P2,[A|APoints],BPoints,[A|IntersectedA],[B|IntersectedB],LeftOverA,LeftOverB):-
  select(B,BPoints,BPointsMinusA),
  \+ \+ call(P2,A,B),!,
  pred_intersection(P2,APoints,BPointsMinusA,IntersectedA,IntersectedB,LeftOverA,LeftOverB).
pred_intersection(P2,[A|APoints],BPoints,IntersectedA,IntersectedB,[A|LeftOverA],LeftOverB):-
  pred_intersection(P2,APoints,BPoints,IntersectedA,IntersectedB,LeftOverA,LeftOverB).

:- fixup_exports.


end_of_file.

?- make_grid(3,3,G),
   hv_value(G,V,2,2),dif(V,Z),hv_value(G,Z,3,3),Z=blue,
   print_grid(G),
   all_rotations(G,R).
