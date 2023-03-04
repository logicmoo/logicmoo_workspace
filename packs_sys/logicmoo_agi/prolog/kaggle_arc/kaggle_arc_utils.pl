/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

/*
my_len(X,Y):- var(X),!,length(X,Y).
my_len(X,Y):- is_list(X),!,length(X,Y).
my_len(X,Y):- functor([_|_],F,A),functor(X,F,A),!,length(X,Y).
my_len(X,Y):- arcST,!,ibreak.
*/
sort_safe(I,O):- catch(sort(I,O),_,I=O).

with_tty_false(Goal):- with_set_stream(current_output,tty(false),Goal).
with_tty_true(Goal):- with_set_stream(current_output,tty(true),Goal).


nb_subst(Obj,New,Old):-
  get_setarg_p1(nb_setarg,Found,Obj,P1),Found=@=Old,!,
  p2_call(P1,New),!,nb_subst(Obj,New,Old).
nb_subst(_Obj,_New,_Old).

system:any_arc_files(Some):- is_list(Some),!, Some\==[],fmy_maplist(any_arc_files,Some).
system:any_arc_files(Some):- atom_contains(Some,'arc').

:- thread_local(in_memo_cached/5).
:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).
prolog:make_hook(before, Some):- any_arc_files(Some), forall(muarc:clear_all_caches,true).

:- multifile(muarc:clear_all_caches/0).
:- dynamic(muarc:clear_all_caches/0).
muarc:clear_all_caches:-  \+ luser_getval(extreme_caching,true), retractall(in_memo_cached(_,_,_,_,_)), fail.
%arc_memoized(G):- !, call(G).

arc_memoized(G):- compound(G),ground(G),functor(G,F,1),functor(C,F,1),!,arc_memoized(C),G=C,!.
arc_memoized(G):-
  copy_term(G,C,GT),
  (Key = (C+GT)),
  (in_memo_cached(Key,C,track,started,Info)->throw(already_memoizing(in_memo_cached(Key,C,track,started,Info))) ; true),
  numbervars(Key,0,_,[attvar(bind),singletons(true)]),!,
  setup_call_cleanup((asserta(in_memo_cached(Key,C,track,started,_),Started)),
  catch(
  (in_memo_cached(Key,C,GT,Found,AttGoals)*->(G=Found,my_maplist(call,AttGoals))
    ; ((call(G),copy_term(G,CG,GG)) *->asserta(in_memo_cached(Key,C,GT,CG,GG))
                  ;asserta(in_memo_cached(Key,C,GT,failed,_)))),
  E, (retractall(in_memo_cached(Key,C,GT,_,_)),throw(E))),erase(Started)).

set_nth1(1,[_|Row],E,[E|Row]):-!.
set_nth1(N,[W|Row],E,[W|RowMod]):- Nm1 is N-1, set_nth1(Nm1,Row,E,RowMod).

findall_count(T,G,N):- findall_set(T,G,S),length(S,N).

findall_set(T,G,S):- findall(T,G,L),list_to_set(L,S).

make_list_inited(0,_,[]):-!.
make_list_inited(1,E,[E]):-!.
make_list_inited(N,E,[E|List]):- Nm1 is N -1,make_list_inited(Nm1,E,List).

nth_fact(P,I):- clause(P,true,Ref),nth_clause(P,I,Ref).

nonvar_or_ci(C):- (nonvar(C);attvar(C)),!.

add_i(Info):- 
 quietly((tersify(Info,InfoT),
 luser_getval(test_rules,TRules),
 luser_getval(pair_rules,PRules),
  nb_set_add(TRules,InfoT),
  nb_set_add(PRules,InfoT),
 nop(pp(cyan,+InfoT)))).

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
 luser_getval(pair_rules,PRules), my_maplist(pp(cyan),PRules),
 luser_getval(test_rules,TRules), my_maplist(pp(blue),TRules),
 !.
  
sub_atom_value(TestID,A):- sub_term(A,TestID),(atom(A);string(A)).

my_list_to_set(List, Set):- my_list_to_set(List, (=) ,Set).
my_list_to_set_variant(List, Set):- my_list_to_set(List, (=@=) ,Set).
my_list_to_set_cmp(List, Set):- my_list_to_set(List, (=@=) ,Set).

my_list_to_set([E|List],P2, Set):- select(C,List,Rest), p2_call(P2, E,C), !, my_list_to_set([E|Rest],P2, Set).
my_list_to_set([E|List],P2, [E|Set]):-!, my_list_to_set(List,P2, Set).
my_list_to_set([],_,[]).

my_list_to_set_cmp([E|List],C3, Set):- select(C,List,Rest), p2_call(C3,R,E,C), 
   R== (=), my_list_to_set_cmp([C|Rest],C3, Set),!.
  my_list_to_set_cmp([E|List],C3, [E|Set]):-!, my_list_to_set_cmp(List,C3, Set).
my_list_to_set_cmp([],_,[]).


contains_nonvar(N,Info):- sub_term(E,Info),nonvar_or_ci(E),E=N,!.

max_min(A,B,C,D):- must_be_free(C),must_be_free(D),max_min0(A,B,C,D).
max_min0(A,B,B,B):- plain_var(A).
max_min0(A,B,A,A):- plain_var(B),!.
max_min0(A,B,C,D):- number(A),number(B), !, ((A > B) -> (C=A, D=B) ; (C=B, D=A)).
max_min0(_,A,A,A):- number(A),!.
max_min0(A,_,A,A):- number(A),!.
max_min0(_,_,_,_).

as_debug(L,G):- as_debug(L,true,G).
as_debug(9,_,_):- !.
as_debug(_,C,G):- ignore(catch((call(C)->wots(S,G),format('~NDEBUG: ~w~N',[S]);true),_,true)).

shall_count_as_same(A,B):- same_term(A,B),!. % unify ok_ok cmatch
shall_count_as_same(A,B):- plain_var(A),!,A==B.
shall_count_as_same(A,B):- atomic(A),!, A=@=B.
shall_count_as_same(A,B):- var(B),!,A=@=B.
shall_count_as_same(A,B):- A=@=B,!.
shall_count_as_same(A,B):- copy_term(B,C), A=B, B=@=C,!.
shall_count_as_same(A,B):- \+ A \= B, !.

count_each([C|L],GC,[Len-C|LL]):- include(shall_count_as_same(C),GC,Lst),length(Lst,Len),!,count_each(L,GC,LL).
count_each([],_,[]).

count_each_inv([C|L],GC,[C-Len|LL]):- include(shall_count_as_same(C),GC,Lst),length(Lst,Len),count_each_inv(L,GC,LL).
count_each_inv([],_,[]).

maplist_n(N,P,[H1|T1]):-
  p2_call(P,N,H1), N1 is N+1,
  maplist_n(N1,P,T1).
maplist_n(_N,_P,[]).

maplist_n(N,P,[H1|T1],[H2|T2]):-
  call(P,N,H1,H2), N1 is N+1,
  maplist_n(N1,P,T1,T2).
maplist_n(_N,_P,[],[]).

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
map_pred(Pred, P, X) :- map_pred([],Pred, P, X).
%map_pred(NoCycles,_Pred, P, X) :- member(E,NoCycles), E==P,!, X = P.
map_pred(NoCycles,Pred, P, X) :- p2_call(Pred, P, X)*->true;map_pred0(NoCycles,Pred, P, X).

map_pred1(Pred, P, P1) :- map_pred1(P, Pred, P, P1).

map_pred0(_NoCycles,_Pred, Args, ArgSO) :- must_be_free(ArgSO), Args==[],!, ArgSO=[].
map_pred0(_NoCycles, Pred, P, P1) :-  p2_call(Pred, P, P1),!. % *->true;fail.
map_pred0(NoCycles,Pred, P, X) :- fail, attvar(P), !, %duplicate_term(P,X),P=X, 
  get_attrs(P,VS), map_pred([P|NoCycles],Pred, VS, VSX), P=X, put_attrs(X,VSX),!.
map_pred0(NoCycles,Pred, P, X):- map_pred1(NoCycles,Pred, P, X).

map_pred1(_NoCycles,_Pred, P, P1) :- ( \+ compound(P) ; is_ftVar(P)), !, must_det_ll(P1=P), !.
% map_pred0(NoCycles,Pred, Args, ArgSO) :- is_list(Args), !, my_maplist(map_pred([Args|NoCycles],Pred), Args, ArgS), ArgS=ArgSO.
map_pred1(NoCycles,Pred, IO, OO) :- is_list(IO),!, my_maplist(map_pred(NoCycles,Pred), IO, OO).
map_pred1(NoCycles,Pred, IO, [O|ArgS]) :-  IO= [I|Args], !, 
  map_pred([IO,ArgS|NoCycles],Pred, I, O), map_pred0([IO,I|NoCycles],Pred, Args, ArgS).
map_pred1(NoCycles,Pred, P, P1) :-  
  compound_name_arguments(P, F, Args), my_maplist(map_pred([P|NoCycles],Pred),Args,ArgS), compound_name_arguments(P1, F, ArgS).
%map_pred(_Pred, P, P).
/*
:- meta_predicate map_pred(2, ?, ?, ?, ?).
map_pred(Pred, P, X, Sk, P1) :- must_be_free(X), p2_call(Pred, P, X), !, must(Sk=P1), !.
map_pred(_Pred, P, _, _, P1) :- is_ftVar(P), !, must(P1=P), !.
map_pred(Pred, [P|Args], X, Sk, [P1|ArgS]) :- !, map_pred(Pred, P, X, Sk, P1), !, must(map_pred(Pred, Args, X, Sk, ArgS)), !.
map_pred(Pred, P, X, Sk, P1) :- compound(P), !, compound_name_arguments(P, F, Args), map_pred(Pred, [F|Args], X, Sk, [Fs|ArgS]), !, compound_name_arguments(P1, Fs, ArgS), !.
map_pred(_Pred, P, _, _, P).
*/

into_grid_or_var(G,G):- is_cons(G),!.
into_grid_or_var(G,G):- var(G),!.
into_grid_or_var(O,G):- cast_to_grid(O,G,_Uncast),!.

maybe_mapgrid(P2,I,O):- is_grid(I),!,mapgrid(P2,I,O).
maybe_mapgrid(P3,I,O,M):- is_grid(I),!,mapgrid(P3,I,O,M).
maybe_mapgrid(P4,I,O,M,N):- is_grid(I),!,mapgrid(P4,I,O,M,N).

mapgrid(P4,Grid,GridM,GridN,GridO):- into_grid_or_var(Grid,G1),into_grid_or_var(GridM,G2),into_grid_or_var(GridN,G3),into_grid_or_var(GridO,G4),mapg_list(P4,G1,G2,G3,G4).
mapg_list(P4,Grid,GridM,GridN,GridO):- is_list(Grid),!,my_maplist(mapg_list(P4),Grid,GridM,GridN,GridO).
mapg_list(P4,Grid,GridM,GridN,GridO):- call(P4,Grid,GridM,GridN,GridO),!.

mapgrid(P3,Grid,GridN,GridO):- into_grid_or_var(Grid,G1),into_grid_or_var(GridN,G2),into_grid_or_var(GridO,G3),mapg_list(P3,G1,G2,G3).
mapg_list(P3,Grid,GridN,GridO):- is_list(Grid),!,my_maplist(mapg_list(P3),Grid,GridN,GridO).
mapg_list(P3,Grid,GridN,GridO):- call(P3,Grid,GridN,GridO),!.

mapgrid(P2, Grid,GridN):- into_grid_or_var(Grid,G1),into_grid_or_var(GridN,G2),mapg_list(P2, G1,G2).
mapg_list(P2, Grid,GridN):- is_list(Grid),!,my_maplist(mapg_list(P2),Grid,GridN).
mapg_list(P2, Grid,GridN):- p2_call(P2, Grid,GridN),!.

mapgrid(P1,Grid):- into_grid_or_var(Grid,G1),mapg_list(P1,G1).
mapg_list(P1,Grid):- is_list(Grid),!,my_maplist(mapg_list(P1),Grid).
mapg_list(P1,Grid):- p1_call(P1,Grid),!.


maplist_ignore(_3,H,I,J):- (H==[];I==[],J==[]),!,(ignore(H=[]),ignore(I=[]),ignore(J=[])).
maplist_ignore(P3,H,I,J):- \+ is_list(H),!, ignore(p2_call(P3,H,I,J)).
maplist_ignore(P3,[H|Grid],[I|GridN],[J|GridO]):- maplist_ignore(P3,H,I,J), !,maplist_ignore(P3,Grid,GridN,GridO).

maplist_ignore(_2,H,I):- (H==[];I==[]),!,(ignore(H=[]),ignore(I=[])).
maplist_ignore(P2, H,I):- \+ is_list(H),!, ignore(p2_call(P2, H,I)).
maplist_ignore(P2, [H|Grid],[I|GridN]):- maplist_ignore(P2, H,I), !,maplist_ignore(P2, Grid,GridN).

%p1_or(P1,Q1,E):- must_be(callable,P1),!, (p1_call(P1,E);p1_call(Q1,E)).

p1_call((P1;Q1),E):- must_be(callable,P1),!, (p1_call(P1,E);p1_call(Q1,E)).
p1_call((P1,Q1),E):- must_be(callable,P1),!, (p1_call(P1,E),p1_call(Q1,E)).
p1_call(or(P1,Q1),E):- must_be(callable,P1),!, (p1_call(P1,E);p1_call(Q1,E)).
p1_call(and(P1,Q1),E):- must_be(callable,P1),!, (p1_call(P1,E),p1_call(Q1,E)).
p1_call(not(not(P1)),E):- !, p1_call(P1,E).
p1_call(not(P1),E):- !, not(p1_call(P1,E)).
p1_call(once(P1),E):- !, once(p1_call(P1,E)).
p1_call(chk(P1),E):- !, \+ \+ (p1_call(P1,E)).
p1_call( \+ (P1),E):- !, \+ p1_call(P1,E).
p1_call(P1,E):- !, call(P1,E).

p2_call_p2(P2a,P2b,A,B):- p2_call(P2a,A,M),p2_call(P2b,M,B).

p2_call(p1_call(P1),E,O):- !, p1_call(P1,E), E=O.
p2_call([P2],Grid,GridN):- !, p2_call(P2, Grid,GridN).
p2_call([P2|P2L],Grid,GridN):- !, p2_call(P2, Grid,GridM),p2_call(P2L,GridM,GridN).
p2_call(ignore(P2),A,B):- p2_call(P2,A,B)*->true;A=B.
p2_call(type(Type,P2),A,B):- coerce(Type,A,AA),p2_call(P2,AA,B).
p2_call(or(P2,Q2),A,B):- must_be(callable,P2),!, (p2_call(P2,A,B);p2_call(Q2,A,B)).
p2_call(and(P2,Q2),A,B):- must_be(callable,P2),!, (p2_call(P2,A,AB),p2_call(Q2,AB,B)).
p2_call(P2,A,B):- call(P2,A,B).


p1_or(P1A,P1B,X):- p1_call(P1A,X)->true;p1_call(P1B,X).
p1_and(P1A,P1B,X):- p1_call(P1A,X),p1_call(P1B,X).
p1_not(P1,E):- \+ p1_call(P1,E).
p1_arg(N,P1,E):- arg(N,E,Arg),p1_call(P1,Arg).
p1_subterm(P1,E):- sub_term(Arg,E),p1_call(P1,Arg).
my_partition(_,[],[],[]):-!.
my_partition(P1,[H|L],[H|I],E):- \+ \+ p1_call(P1,H),!,
  my_partition(P1,L,I,E).
my_partition(P1,[H|L],I,[H|E]):- 
   my_partition(P1,L,I,E),!.
my_partition(P1,H,I,HE):- arcST,ibreak,
  my_partition(P1,[H],I,HE).


mapgroup(P2,G1,L2):- into_list(G1,L1),!, with_group(L1,my_maplist(P2,L1,L2)).
mapgroup(P1,G1):- into_list(G1,L1), !, with_group(L1,my_maplist(P1,L1)).

selected_group(Grp):- nb_current('$outer_group',Grp),!.
selected_group([]).

with_group([O|Grp],Goal):- compound(O),O=obj(_),!, locally(nb_setval('$outer_group',[O|Grp]),Goal).
with_group(_,Goal):- call(Goal).

into_mlist(L,L).
my_maplist(P4,G1,L2,L3,L4):- into_mlist(G1,L1),!, with_group(L1,maplist(P4,L1,L2,L3,L4)).
my_maplist(P3,G1,L2,L3):- into_mlist(G1,L1),!, with_group(L1,maplist(P3,L1,L2,L3)).
my_maplist(P2,G1,L2):- into_mlist(G1,L1),!, with_group(L1,maplist(P2,L1,L2)).
my_maplist(P1,G1):- into_mlist(G1,L1), !, with_group(L1,maplist(P1,L1)).


my_include(P1,[H|L],O):- (p2_call(p1_call(P1),H,HH)*->(my_include(P1,L,I),O=[HH|I]);my_include(P1,L,O)).
my_include(_,_,[]).

%my_exclude(P1,I,O):- my_include(not(P1),I,O).
my_exclude(P1,I,O):- my_partition(P1,I,_,O).


subst_1L([],Term,Term):-!.
subst_1L([X-Y|List], Term, NewTerm ) :-
  subst0011(X, Y, Term, MTerm ),
  subst_1L(List, MTerm, NewTerm ).

subst_2L([],_,I,I).
subst_2L([F|FF],[R|RR],I,O):- subst0011(F,R,I,M),subst_2L(FF,RR,M,O).


subst001(I,F,R,O):- subst0011(F,R,I,O),!.


subst0011(X, Y, Term, NewTerm ) :-
  copy_term((X,Y,Term),(CX,CY,Copy),Goals), 
  (Goals==[]
   ->subst0011a( X, Y, Term, NewTerm )
   ;(subst0011a(CX, CY, Goals, NewGoals),
     (NewGoals==Goals -> 
       subst0011a( X, Y, Term, NewTerm )
       ; (subst0011a(CX, CY, Copy, NewCopy),
          NewTerm = NewCopy, my_maplist(call,NewGoals))))).
         
    

subst0011a(X, Y, Term, NewTerm ) :-
 ((X==Term)-> Y=NewTerm ;
  (is_list(Term)-> my_maplist(subst0011a(X, Y), Term, NewTerm );
   (( \+ compound(Term); Term='$VAR'(_))->Term=NewTerm;
     ((compound_name_arguments(Term, F, Args),
       my_maplist(subst0011a(X, Y), Args, ArgsNew),
        compound_name_arguments( NewTerm, F, ArgsNew )))))),!.

subst001C(I,F,R,O):- subst001_p2(same_term,I,F,R,O),!.
subst0011C(F,R,I,O):- subst0011_p2(same_term,F,R,I,O),!.
subst_2LC(F,R,I,O):- subst_2L_p2(same_term,F,R,I,O).

subst_2L_p2(_P2, [],_,I,I):-!.
subst_2L_p2(_P2, _,[],I,I):-!.
subst_2L_p2(P2, [F|FF],[R|RR],I,O):- subst0011_p2(P2, F,R,I,M),subst_2L_p2(P2, FF,RR,M,O).

subst001_p2(P2, I,F,R,O):- subst0011_p2(P2, F,R,I,O),!.

subst_1L_p2(_,  [],Term,Term):-!.
subst_1L_p2(P2, [X-Y|List], Term, NewTerm ) :-
  subst0011_p2(P2, X, Y, Term, MTerm ),
  subst_1L_p2(P2, List, MTerm, NewTerm ).

subst0011_p2(P2, X, Y, Term, NewTerm ) :-
  copy_term((X,Y,Term),(CX,CY,Copy),Goals), 
  (Goals==[]
  ->subst0011a_p2(P2, X, Y, Term, NewTerm )
  ;(subst0011a_p2(P2, CX, CY, Goals, NewGoals),
     (NewGoals==Goals -> 
       subst0011a_p2(P2, X, Y, Term, NewTerm )
       ; (subst0011a_p2(P2, CX, CY, Copy, NewCopy),
          NewTerm = NewCopy, my_maplist(call,NewGoals))))).

subst0011a_p2(P2, X, Y, Term, NewTerm ) :-
 (p2_call(P2,X,Term)-> Y=NewTerm ;
  (is_list(Term)-> my_maplist(subst0011a_p2(P2, X, Y), Term, NewTerm );
   (( \+ compound(Term); Term='$VAR'(_))->Term=NewTerm;
     ((compound_name_arguments(Term, F, Args),
       my_maplist(subst0011a_p2(P2, X, Y), Args, ArgsNew),
        compound_name_arguments( NewTerm, F, ArgsNew )))))),!.



ppa(FF):-
  copy_term(FF,FA,GF), 
  numbervars(FA+GF,0,_,[attvar(bind),singletons(true)]),
  sort_safe(GF,GS),write(' '),
  locally(b_setval(arc_can_portray,nil),
      ppawt(FA)),format('~N'),
  ignore((GS\==[], format('\t'),ppawt(attvars=GS),nl)),nl,!.

ppawt(FA):-
  write_term(FA,[numbervars(false), quoted(true), 
   character_escapes(true),cycles(true),dotlists(false),no_lists(false),
    blobs(portray),attributes(dots), 
    portray(true), partial(false), fullstop(true),
    %portray(false), partial(true), fullstop(true),
   ignore_ops(false), quoted(true), quote_non_ascii(true), brace_terms(false)]).


intersection([],LeftOverB,[],[],LeftOverB):-!.
intersection(LeftOverA,[],[],LeftOverA,[]):-!.
intersection([A|APoints],BPoints,[A|Intersected],LeftOverA,LeftOverB):-
  select(A,BPoints,BPointsMinusA),!,
  intersection(APoints,BPointsMinusA,Intersected,LeftOverA,LeftOverB).
intersection([A|APoints],BPoints,Intersected,[A|LeftOverA],LeftOverB):-
  intersection(APoints,BPoints,Intersected,LeftOverA,LeftOverB).

:- meta_predicate(each_obj(?,?,0)).
each_obj([],_,_):-!.
each_obj([Obj|List],Obj,Goal):- ignore(Goal), each_obj(List,Obj,Goal).

pred_intersection(_P2, [],LeftOverB, [],[], [],LeftOverB):-!.
pred_intersection(_P2, LeftOverA,[], [],[], LeftOverA,[]):-!.
pred_intersection(P2, [A|APoints],BPoints,[A|IntersectedA],[B|IntersectedB],LeftOverA,LeftOverB):-
  select(B,BPoints,BPointsMinusA),
  \+ \+ p2_call(P2, A,B),!,
  pred_intersection(P2, APoints,BPointsMinusA,IntersectedA,IntersectedB,LeftOverA,LeftOverB).
pred_intersection(P2, [A|APoints],BPoints,IntersectedA,IntersectedB,[A|LeftOverA],LeftOverB):-
  pred_intersection(P2, APoints,BPoints,IntersectedA,IntersectedB,LeftOverA,LeftOverB).




run_source_code(ShareVars, SourceCode, Vs, QQ):- 
  QQ = source_buffer(SourceCode,Vs),!, 
  %print(term=Sourcecode -> vs=Vs), 
  my_maplist(share_vars(Vs),ShareVars),
  (\+ is_list(SourceCode)
    -> mort(SourceCode)
    ; my_maplist(mort,SourceCode)).

run_source_code(ShareVars, Vs, QQ):- 
  QQ = source_buffer(SourceCode,Vs),!, 
  %print(term=Sourcecode -> vs=Vs), 
  my_maplist(share_vars(Vs),ShareVars),
  (\+ is_list(SourceCode)
    -> mort(SourceCode)
    ; my_maplist(mort,SourceCode)).


%vars_to_dictation([_=Value|Gotten],TIn,TOut):- is_vm_map(Value),!, vars_to_dictation(Gotten,TIn,TOut).

vars_to_dictation([Name=Value|Gotten],TIn,TOut):- !,
  my_assertion(atom(Name)),
  vars_to_dictation(Gotten,TIn,TMid), 
  to_prop_name(Name,UName),
  tio_tersify(Value,ValueT),!,
  put_dict(UName,TMid,ValueT,TOut).

vars_to_dictation([NameValue|Gotten],TIn,TOut):- !,
  vars_to_dictation(Gotten,TIn,TMid), 
  to_prop_name(NameValue,UName),
  tio_tersify(NameValue,ValueT),!,
  put_dict(UName,TMid,ValueT,TOut).

vars_to_dictation([NameValue|Gotten],TIn,TOut):- compound(NameValue),compound_name_arguments(NameValue,Name,Value),!, 
  vars_to_dictation([Name=Value|Gotten],TIn,TOut).
  
vars_to_dictation([],T,T).

tio_tersify(Value,ValueT):- is_grid(Value),!,ValueT=_.
tio_tersify(Value,Value).
:- export(copy_qq_//1).

copy_qq_([]) --> [].
copy_qq_([C|Cs]) --> [C], copy_qq_(Cs).

:- export(copy_qq//1).
muarc:copy_qq(A) --> copy_qq_(Cs), {atom_codes(A, Cs)}.

to_prop_name(Name=_,UName):- nonvar(Name),!,to_prop_name(Name,UName).
to_prop_name(Name,UName):- compound(Name),compound_name_arity(Name,F,_),!,to_prop_name(F,UName).
to_prop_name(Name,UName):- to_case_breaks(Name,Breaks),xtis_to_atomic(Breaks,UName).

xtis_to_atomic([xti(Str,upper),xti(StrL,lower)|Breaks],StrO):- string_upper(Str,Str),
   atom_chars(Str,CharsList),append(Left,[U],CharsList),
   name(S1,Left),atomic_list_concat([S1,'_',U,StrL],'',StrUL),!,
   xtis_to_atomic([xti(StrUL,lower)|Breaks],StrO).
xtis_to_atomic([],'').
xtis_to_atomic([xti(Str,_)],Lower):- downcase_atom(Str,Lower).
xtis_to_atomic([XTI|Breaks],Atomic):- 
  xtis_to_atomic([XTI],S1),xtis_to_atomic(Breaks,S2),!,atomic_list_concat([S1,S2],'_',Atomic).

share_vars(Vs,Name=Value):- member(VName=VValue,Vs),VName==Name,!,(Value=VValue->true;trace_or_throw(cant(share_vars(Vs,Name=Value)))).
share_vars(_,Name=_):- string_concat('_',_,Name),!. % Hide some vars
share_vars(V,Name=Value):- dmsg(missing(share_vars(V,Name=Value))),!.



parse_expansions(_,Vs,Vs,Src,Src):- \+ compound(Src),!.
parse_expansions(_,Vs0,Vs,dont_include(Var),nop(dont_include(Var))):- 
  dont_include_var(Vs0,Vs,Var),!.
parse_expansions(F, Vs0,Vs,[Src0|Sourcecode0],[Src|Sourcecode]):- !,
  parse_expansions(F, Vs0, Vs1, Src0, Src),
  parse_expansions(F, Vs1, Vs, Sourcecode0, Sourcecode).
parse_expansions(FF, Vs0, Vs, Cmpd0, Cmpd):- 
  compound_name_arguments(Cmpd0,F,Args0), 
  parse_expansions([F|FF], Vs0, Vs, Args0,Args),
  compound_name_arguments(Cmpd,F,Args).

dont_include_var(Vs0,Vs,Var):- select(_=VV,Vs0,Vs),VV==Var,!.
dont_include_var(Vs,Vs,_).
  
append_sets(Sets,Set):- append(Sets,List),list_to_set(List,Set).
append_sets(Set1,Set2,Set):- append(Set1,Set2,List),list_to_set(List,Set).
flatten_sets(Sets,Set):- flatten(Sets,List),list_to_set(List,Set).

print_prop_val(N=V):- to_prop_name(N,P),format('~N\t\t'),print(P=V),nl.


ignore_numvars(Name='$VAR'(Name)).

:- include(kaggle_arc_footer).


end_of_file.

?- make_grid(3,3,G),
   hv_c_value(G,V,2,2),dif(V,Z),hv_c_value(G,Z,3,3),Z=blue,
   print_grid(G),
   all_rotations(G,R).
