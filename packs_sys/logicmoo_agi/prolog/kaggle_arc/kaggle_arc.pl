
:- dynamic('$exported_op'/3).
:- multifile('$exported_op'/3).
:- system:ensure_loaded(library(logicmoo_common)).
/*
JSON Conversion
:- use_module(library(http/json_convert)).

test_pairs(Name,Type,In,Out):- 
  kaggle_arc_eval(Name,Stuff), once(atom_json_term(Stuff,json(L),[])),
  json_pairs(L,Type,In,Out).

json_pairs([],_,_,_):- !, fail.
json_pairs(json(T),Type,In,Out):-!,json_pairs(T,Type,In,Out).
json_pairs([input=In,output=Out],_Type,In,Out):-!.
json_pairs(Type=List,Type,In,Out):-!,member(L,List),
   json_pairs(L,Type,In,Out).
json_pairs([H|T],Type,In,Out):-!, 
  (json_pairs(H,Type,In,Out);json_pairs(T,Type,In,Out)).

%print_trainer:- kaggle_arc_train(Name,Stuff), atom_json_term(Stuff,JSON,[]),print_arc(Name,JSON).
%print_evaler:- kaggle_arc_eval(Name,Stuff), atom_json_term(Stuff,JSON,[]),print_arc(Name,JSON).

*/
:- dynamic(cmem/3).
:- ensure_loaded(kaggle_arc_train).
:- ensure_loaded(kaggle_arc_eval).


print_trainer(Name):- forall(kaggle_arc(Name,Type,In,Out),print_arc_in_out(Name,Type,In,Out)).
print_trainer:- cls,mmake,forall(test_names_by_hard(Name),print_trainer(Name)).

print_trainer0:- print_trainer(t('25d487eb')).
print_eval0:- print_trainer(v('009d5c81')).

nth_fact(P,I):- clause(P,true,Ref),nth_clause(P,I,Ref).

% Type is tst or trn
kaggle_arc(t(Name),TypeI,In,Out):-   
  nth_fact(kaggle_arc_train(Name,Type,In,Out),This),
  once(nth_fact(kaggle_arc_train(Name,Type,_,_),Start)), I is This - Start,TypeI=Type+I.
kaggle_arc(v(Name),TypeI,In,Out):- 
  nth_fact(kaggle_arc_eval(Name,Type,In,Out),This),
  member(Type,[trn,tst]),once(nth_fact(kaggle_arc_eval(Name,Type,_,_),Start)), 
  I is This - Start+1,TypeI=Type+I.

test_name(Name):- 
  findall(Name,kaggle_arc(Name,_,_,_),All),
  sort(All,AllS),member(Name,AllS).

fav(X,[]):- clause(fav(X),true).
%fav(v('e9bb6954'),['box of nine draw outward, if you hit a drawn line blacken it']).

fav(t('25d487eb')).
fav(v('762cd429')).
fav(X):- clause(show_indv(X),true).
fav(X):- clause(fav(X,_),true).

show_indv(v('20818e16')).
show_indv(X):- fail, fav(X).

test_info(X,Info):- fav(X,Info)->true;Info=[].
fix_test_name(X,X):- kaggle_arc(X,_,_,_).
fix_test_name(X,X):- compound(X).
fix_test_name(X,t(X)):- kaggle_arc(t(X),_,_,_).
fix_test_name(X,v(X)):- kaggle_arc(v(X),_,_,_).

test_names_by_hard(Name):- fav(Name).
test_names_by_hard(Name):-   
  findall(Hard-Name,(test_name(Name),hardness_of_name(Name,Hard)),All),
  keysort(All,AllK),
  member(_-Name,AllK),\+ fav(Name).

ascending_hard:-
  tell('arc_ascending.pl'),
  forall(test_names_by_hard(Name),
    forall(kaggle_arc(Name,Type,In,Out),format('~q.~n',[kaggle_arc_ord(Name,Type,In,Out)]))),
  told,
  reconsult(arc_ascending).

max_min(A,B,A,B):- A>B,!.
max_min(A,B,B,A).

hardness_of_name(Name,Hard):-
 %Type=tst+_,
 Type=_,
 findall(Hard,
 (kaggle_arc(Name,Type,In,Out),
  grid_size(In,size(H0,V0)),
  grid_size(Out,size(H,V)),
  max_min(H,V,C,_),
  max_min(H0,V0,C0,_),
  HV is C*C, HV0 is C0*C0,
  max_min(HV,HV0,Max,Min),
  D is Max-Min,
  Hard is Max*10000+D),All),
  sort(All,AllK),last(AllK,Hard).

  /*
% data looks like

kaggle_arc_train('007bbfb7',trn,[[0,7,7],[7,7,7],[0,7,7]],[[0,0,0,0,7,7,0,7,7],[0,0,0,7,7,7,7,7,7],[0,0,0,0,7,7,0,7,7],[0,7,7,0,7,7,0,7,7],[7,7,7,7,7,7,7,7,7],[0,7,7,0,7,7,0,7,7],[0,0,0,0,7,7,0,7,7],[0,0,0,7,7,7,7,7,7],[0,0,0,0,7,7,0,7,7]]).
kaggle_arc_train('007bbfb7',trn,[[4,0,4],[0,0,0],[0,4,0]],[[4,0,4,0,0,0,4,0,4],[0,0,0,0,0,0,0,0,0],[0,4,0,0,0,0,0,4,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,4,0,4,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,4,0,0,0,0]]).
kaggle_arc_train('007bbfb7',trn,[[0,0,0],[0,0,2],[2,0,2]],[[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,2],[0,0,0,0,0,0,2,0,2],[0,0,0,0,0,0,0,0,0],[0,0,2,0,0,0,0,0,2],[2,0,2,0,0,0,2,0,2]]).
kaggle_arc_train('007bbfb7',trn,[[6,6,0],[6,0,0],[0,6,6]],[[6,6,0,6,6,0,0,0,0],[6,0,0,6,0,0,0,0,0],[0,6,6,0,6,6,0,0,0],[6,6,0,0,0,0,0,0,0],[6,0,0,0,0,0,0,0,0],[0,6,6,0,0,0,0,0,0],[0,0,0,6,6,0,6,6,0],[0,0,0,6,0,0,6,0,0],[0,0,0,0,6,6,0,6,6]]).
kaggle_arc_train('007bbfb7',trn,[[2,2,2],[0,0,0],[0,2,2]],[[2,2,2,2,2,2,2,2,2],[0,0,0,0,0,0,0,0,0],[0,2,2,0,2,2,0,2,2],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,2,2,2,2,2,2],[0,0,0,0,0,0,0,0,0],[0,0,0,0,2,2,0,2,2]]).
kaggle_arc_train('007bbfb7',tst,[[7,0,7],[7,0,7],[7,7,0]],[[7,0,7,0,0,0,7,0,7],[7,0,7,0,0,0,7,0,7],[7,7,0,0,0,0,7,7,0],[7,0,7,0,0,0,7,0,7],[7,0,7,0,0,0,7,0,7],[7,7,0,0,0,0,7,7,0],[7,0,7,7,0,7,0,0,0],[7,0,7,7,0,7,0,0,0],[7,7,0,7,7,0,0,0,0]]).

kaggle_arc_train('00d62c1b',trn,[[0,0,0,0,0,0],[0,0,3,0,0,0],[0,3,0,3,0,0],[0,0,3,0,3,0],[0,0,0,3,0,0],[0,0,0,0,0,0]],[[0,0,0,0,0,0],[0,0,3,0,0,0],[0,3,4,3,0,0],[0,0,3,4,3,0],[0,0,0,3,0,0],[0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b',trn,[[0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,0,0,0,0,0],[0,0,0,3,0,3,0,0,0,0],[0,0,3,0,0,0,3,0,0,0],[0,0,0,0,0,3,0,3,0,0],[0,0,0,3,0,3,3,0,0,0],[0,0,3,3,3,0,0,0,0,0],[0,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0]],[[0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,0,0,0,0,0],[0,0,0,3,0,3,0,0,0,0],[0,0,3,0,0,0,3,0,0,0],[0,0,0,0,0,3,4,3,0,0],[0,0,0,3,0,3,3,0,0,0],[0,0,3,3,3,0,0,0,0,0],[0,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b',trn,[[0,0,0,0,0,3,0,0,0,0],[0,0,0,0,3,0,0,0,0,0],[0,3,3,0,3,3,0,3,0,0],[3,0,0,3,0,0,3,0,3,0],[0,0,0,3,0,0,3,3,0,0],[0,0,0,3,0,0,3,0,0,0],[0,0,0,3,0,0,3,0,0,0],[0,0,0,0,3,3,0,3,0,0],[0,0,0,0,0,0,0,0,3,0],[0,0,0,0,0,0,0,0,0,0]],[[0,0,0,0,0,3,0,0,0,0],[0,0,0,0,3,0,0,0,0,0],[0,3,3,0,3,3,0,3,0,0],[3,0,0,3,4,4,3,4,3,0],[0,0,0,3,4,4,3,3,0,0],[0,0,0,3,4,4,3,0,0,0],[0,0,0,3,4,4,3,0,0,0],[0,0,0,0,3,3,0,3,0,0],[0,0,0,0,0,0,0,0,3,0],[0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b',trn,[[0,0,0,0,0,0,0,0,0,0],[0,0,3,3,3,3,0,0,0,0],[0,0,3,0,0,3,0,0,0,0],[0,0,3,0,0,3,0,3,0,0],[0,0,3,3,3,3,3,3,3,0],[0,0,0,3,0,0,0,0,3,0],[0,0,0,3,0,0,0,3,3,0],[0,0,0,3,3,0,0,3,0,3],[0,0,0,3,0,3,0,0,3,0],[0,0,0,0,3,0,0,0,0,0]],[[0,0,0,0,0,0,0,0,0,0],[0,0,3,3,3,3,0,0,0,0],[0,0,3,4,4,3,0,0,0,0],[0,0,3,4,4,3,0,3,0,0],[0,0,3,3,3,3,3,3,3,0],[0,0,0,3,0,0,0,0,3,0],[0,0,0,3,0,0,0,3,3,0],[0,0,0,3,3,0,0,3,4,3],[0,0,0,3,4,3,0,0,3,0],[0,0,0,0,3,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b',trn,[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,3,3,3,3,0,3,3,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,3,0,0,0,0,0,0,0,3,0],[0,0,0,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0],[0,0,0,0,3,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0],[0,0,3,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,3,3,0,0,0,0,3,0,3,0,0],[0,0,0,0,0,0,3,3,0,0,3,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,0,0,3,3,0,0,3,0,0,3,0,0],[0,0,0,0,0,0,0,3,3,3,3,0,3,0,0,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,3,0,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,3,3,3,3,4,3,3,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,4,3,0,0,0,0,0,0,0,3,0],[0,0,0,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,4,4,4,4,4,4,3,0,0,0,0],[0,0,0,0,3,0,0,0,3,4,4,4,4,4,4,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,4,4,4,4,4,4,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,4,4,4,4,4,4,3,0,0,0,0],[0,0,3,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,3,3,0,0,0,0,3,0,3,0,0],[0,0,0,0,0,0,3,3,4,4,3,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,4,4,3,3,0,0,3,0,0,3,0,0],[0,0,0,0,0,0,0,3,3,3,3,0,3,0,0,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,3,4,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b',tst,[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,3,0,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,3,3,3,3,0,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,3,0,0,0,0,3,0,0,3,0,0,0,0,0,0,0],[0,0,0,0,3,3,3,3,3,0,3,3,3,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,3,0,0],[0,0,0,0,0,0,0,0,0,3,3,3,3,3,0,0,0,3,0,0],[0,0,0,0,0,0,0,0,0,3,0,0,0,3,0,0,0,3,0,0],[0,0,0,0,0,0,0,0,3,3,3,3,3,3,0,0,0,3,0,0],[0,0,0,0,0,0,3,3,0,3,0,0,0,3,3,3,3,3,0,0],[0,0,3,0,0,0,0,0,3,3,0,0,0,0,0,0,0,0,0,0],[0,3,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,0,3,3,3,3,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,0,0,0,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,0,0,0,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,3,3,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,3,4,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,3,3,3,3,0,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,3,4,4,4,4,3,4,4,3,0,0,0,0,0,0,0],[0,0,0,0,3,3,3,3,3,0,3,3,3,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,4,4,4,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,4,4,4,3,0,0],[0,0,0,0,0,0,0,0,0,3,3,3,3,3,4,4,4,3,0,0],[0,0,0,0,0,0,0,0,0,3,4,4,4,3,4,4,4,3,0,0],[0,0,0,0,0,0,0,0,3,3,3,3,3,3,4,4,4,3,0,0],[0,0,0,0,0,0,3,3,4,3,0,0,0,3,3,3,3,3,0,0],[0,0,3,0,0,0,0,0,3,3,0,0,0,0,0,0,0,0,0,0],[0,3,4,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,0,3,3,3,3,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,4,4,4,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,4,4,4,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,3,3,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
*/
%tell(s),ignore((nl,nl,test_pairs(Name,Type,In,Out),format('~N~q.~n',[test_pairs_cache(Name,Type,In,Out)]),fail)),told.

is_grid([[C|H]|R]):- is_list(H),is_list(R),(integer(C);var(C)),
  length([C|H],L),is_row_len(L,[C|H]),maplist(is_row_len(L),R).

is_row_len(N,L):- length(L,N).

current_test_name(Name):- nb_current(test_name,Name),!.
current_test_name([]).

print_arc_in_out(Name,Type,In,Out):-
 setup_call_cleanup((
  current_test_name(CName),
  ignore((CName\==Name,dash_char(60,"A"),dash_char(6,"\n"),nl)),
  dash_char(60,"V"),nl,
  nb_setval(test_name,Name),
  print_grid(Name=in(Type),In),
  print_grid(Name=out(Type),Out),nl),true,notrace).

print_arc(Name,json(JSON)):-!, print_arc(Name,JSON).
print_arc(Name,trn=Y):- !, print_arc(Name,Y).
print_arc(Name,X->Y):- !, print_arc(in(Name),X), print_arc(out(Name),Y).
print_arc(Name,X=Y):- !, print_arc(Name=X,Y).
print_arc(Name,[H|L]):- is_grid([H|L]),!,print_grid(Name,[H|L]).
print_arc(Name,[H|L]):- !, maplist(print_arc(Name),[H|L]).
print_arc(Name,Val):- print_tree_nl(Name=Val).

% Grid pretty printing
print_grid(Name,Grid):- 
  % dash_char(60,"="),
  wdmsg(Name),  
  % assert_id_grid_cells(Name,Grid),!,
  describe_features(Grid),
  nop(print_grid(Grid)).

describe_features(Grid):-
  describe_feature(Grid,[grid_size,unique_colors,colors_count,=,individuals]).
describe_feature(Grid,List):- is_list(List),!,maplist(describe_feature(Grid),List).
describe_feature(Grid,Pred):- call(Pred,Grid,Res)->print_equals(Grid,Pred,Res);print_equals(Pred,f),!.
  
print_equals(Grid,individuals,Res):- grid_size(Grid,size(H,V)),  
   maplist(embue_points(size(H,V)),Res,ResO),
   print_equals(individuals,ResO).
print_equals(_Grid,Pred,Res):- print_equals(Pred,Res).

writeqln(X):- format('~q',[X]),format('~N').


dash_char(H,C):-forall(between(0,H,_),write(C)).
print_equals(N,V):- \+ compound(V),writeqln(N=V).
print_equals(=,V):- is_grid(V),grid_size(V,size(H,_)),!,dash_char(H,"="),print_grid(V).
print_equals(N,V):- is_grid(V),!,writeqln(N),print_grid(V).
print_equals(N,[G|V]):- is_list_of_points([G|V]),
  length([G|V],Len),
  grid_size(G,size(H,_V)),
  writeqln(N=len(Len)),  !,
 ignore((nb_current(test_name,Name),show_indv(Name),
  dash_char(H,"-"),nl,
  forall(member(E,[G|V]),(print_points_grid(E),dash_char(H,"-"),nl)))).

print_equals(N,[G|V]):- 
  is_grid(G),is_list(V),maplist(is_grid,V),!,
  length([G|V],Len),
  grid_size(G,size(H,_V)),
  writeqln(N=len(Len)),  
  dash_char(H,"-"),
  forall(member(E,[G|V]),(print_grid(E),dash_char(H,"-"),nl)).
print_equals(N,V):- better_value(V,BV)-> BV\=@=V, !,print_equals(N,BV).
print_equals(N,[S|L]):- string(S),writeq(N),write('= '),write(S),maplist(commawrite,L),nl.
print_equals(N,V):- print_tree(N=V).

commawrite(S):- write(','),write(S).

is_list_of_points([G|V]):- is_points(G),is_list(V),maplist(is_points,V).

print_points_grid(Points):- points_range(Points,LoH,LoV,HiH,HiV),
  writeqln(size_range(LoH,LoV,HiH,HiV)),points_to_grid(Points,Grid),
  print_grid(Grid).

as_color(Count-Num,List):- color_name(Num,Name),wots(List,color_print(Num,Name=Count)).
better_value(V,List):- is_list(V), maplist(as_color,V,List).
better_value([G|V],List):- 
  is_list_of_points([G|V]),
  maplist(points_to_grid,[G|V],List),
  [G|V] \=@= List.

black_first(SK,[N-0|BF]):-select(N-0,SK,BF),!.
black_first(BF,[0-0|BF]).

pixels(G,GL):- flatten(G,GF),include(integer,GF,GL).
unique_colors(G,UC):- pixels(G,GF),sort(GF,GS),length(GS,UC).
colors_count(G,BF):- pixels(G,GF),sort(GF,GS),count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),black_first(SK,BF).
num_objects(G,NO):- individuals(G,GS),length(GS,NO).

count_each([],_,[]).
count_each([C|L],GC,[Len-C|LL]):- include(==(C),GC,Lst),length(Lst,Len),count_each(L,GC,LL).

%print_grid(Grid):- is_grid(Grid),!, maplist(print_rows,Grid),nl.
print_grid(Grid):- grid_size(Grid,size(HH,VV)),
  forall(between(1,VV,V),
   ((format('~N'),forall(between(1,HH,H), 
     (hv_value_or(Grid,C,H,V,0),
       print_g(C)))))),format('~N').
print_rows(List):- maplist(print_g,List),nl.
%block_colors([(black),(blue),(red),(green),(yellow),'#c0c0c0',(magenta),'#ff8c00',(cyan),'#8b4513']).
block_colors([(black),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
named_colors([(black),(blue),(red),(green),(yellow),silver,(purple),(orange),(cyan),(brown)]).

silver('#7b7b7b').
silver('#c0c0c0').
silver('#9a9a9a').


color_print(C,W):- integer(C),C\==0,block_colors(L),nth0(C,L,Color),ansi_format(fg(Color),'~w',[W]),!.
color_print(C,W):- (var(C);C=0),!,write(W).
color_name(C,W):-  named_colors(L),nth0(C,L,W).


print_g(V):- var(V),!,write(' ?').
print_g(0):- !,write(' .').
print_g(N):- write(' '),print_g1(N).
print_g1(C):- color_print(C,'o').
print_g1(C):- write(' '),write(C).

arc_test_1(Name):- doall(print_trainer(Name)),nop((individuals(Name=out(tst),O),print_equals(parint_grid,O))).
arc_test_1:- arc_test_1(v('009d5c81')).
arc_test_2:- arc_test_1(t('25d487eb')).
arc_test:- arc_test_2.
arc_test:- arc_test_1.
erase_grid(ID):- retractall(cmem(ID,_HV,_C)).

grid_cells(ID,Cells):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Cells).
grid_cells(Grid,Cells):- grid_to_id(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Cells).

assert_id_cells(ID,Cells):- maplist(assert_id_cell(ID),Cells).
assert_id_cell(ID,-(C,HV)):- assert(cmem(ID,HV,C)).
assert_hvc_cell(ID,H,V,C):- hv_point(H,V,HV),assert(cmem(ID,HV,C)).

:- dynamic(is_grid_id/2).
grid_to_id(Grid,ID):- is_grid_id(Grid,ID),!.
grid_to_id(Grid,ID):- gensym('grid_',ID),assert_id_grid_cells(ID,Grid),assert(is_grid_id(Grid,ID)),!.

:- dynamic(grid_sz/3).
% Grid to_fast_workspace
assert_id_grid_cells(ID,Grid):-
   grid_size(Grid,size(SH,SV)),
   retractall(grid_sz(ID,_,_)),
   assert(grid_sz(ID,SH,SV)),
   erase_grid(ID),   
   forall(between(1,SH,H),
    forall(between(1,SV,V),
     ignore((hv_value(Grid,C,H,V),C\==0,assert_hvc_cell(ID,H,V,C))))).


% Random Non Blk Eles
first_color(Grid1,C1):- sub_term(C1,Grid1),C1 \= 0,integer(C1).

% Grid size/resize
make_lengths(N,L):- length(L,N).


grid_size(ID,size(H,V)):- grid_sz(ID,H,V),!.
grid_size(Grid,Size):-  is_points(Grid), !, points_size(Grid,Size).
grid_size(Grid,Size):- grid_size_nd(Grid,Size),!.
grid_size(_,size(30,30)).

as_hv_point(H,V,C,C-Point):- hv_point(H,V,Point),!.
as_hv_point(H,V,_,Point):- hv_point(H,V,Point),!.
as_hv_point(H,V,_,size(H,V)).
%as_hv_point(inf,inf,size_range(_,_,_,_)).


points_size(Grid,Size):- 
   Size = size(_,_), member(Size,Grid).
points_size(Grid,Size):- 
   size_range(inf,inf,0,0,Grid,_,_,H,V),!,
   Size = size(H,V).


points_range(Points,LoH,LoV,HiH,HiV):-
   size_range(inf,inf,0,0,Points,LoH,LoV,HiH,HiV).

size_range(WLoH,WLoV,WHiH,WHiV,[],WLoH,WLoV,WHiH,WHiV).
size_range(WLoH,WLoV,WHiH,WHiV,[E|L],LoH,LoV,HiH,HiV):- !,
  size_range(WLoH,WLoV,WHiH,WHiV,E,MLoH,MLoV,MHiH,MHiV),
  size_range(MLoH,MLoV,MHiH,MHiV,L,LoH,LoV,HiH,HiV).
size_range(_,_,_,_,size_range(LoH,LoV,HiH,HiV),LoH,LoV,HiH,HiV):-!.
size_range(WLoH,WLoV,WHiH,WHiV,size_range(ILoH,ILoV,IHiH,IHiV),LoH,LoV,HiH,HiV):- 
  max_min(WLoV,ILoV,_,LoV),max_min(WHiV,IHiV,HiV,_),
  max_min(WLoH,ILoH,_,LoH),max_min(WHiH,IHiH,HiH,_).

size_range(WLoH,WLoV,WHiH,WHiV,size(_,_),WLoH,WLoV,WHiH,WHiV).

size_range(WLoH,WLoV,WHiH,WHiV,fsize(IHiH,IHiV),WLoH,WLoV,HiH,HiV):- 
  max_min(WHiV,IHiV,HiV,_), max_min(WHiH,IHiH,HiH,_).


size_range(WLoH,WLoV,WHiH,WHiV,Point,LoH,LoV,HiH,HiV):- as_hv_point(H,V,_,Point),!,
  max_min(WLoV,V,_,LoV),max_min(WHiV,V,HiV,_),
  max_min(WLoH,H,_,LoH),max_min(WHiH,H,HiH,_).


grid_size_nd([C,R|Rows],size(H,V)):- 
   (var(Rows)->between(2,30,V);!), 
   length([C,R|Rows],V),
   (var(R)->between(1,30,H);true), 
   length(R,H),
   (is_list(C)->true;(length(C,H),maplist(make_lengths(H),Rows))).
grid_size_nd([L],size(H,1)):- (var(L)->between(1,30,H);true), length(L,H).

% make or do plan
do_change(Change,Grid1,Grid2):- \+ is_list(Change),!,one_change(Change,Grid1,Grid2).
do_change(Change,Grid1,Grid2):- do_change_nd(Change,Grid1,Grid2).

do_change_nd([],Grid1,Grid1).
do_change_nd([H|T],Grid1,Grid2):- one_change(H,Grid1,GridM),do_change_nd(T,GridM,Grid2).

one_change(same,Grid1,Grid2):- is_grid(Grid2),Grid1=Grid2,!.
one_change(colorChange(C1,C2),Grid1,Grid2):- 
  first_color(Grid1,C1),ignore((is_grid(Grid2),first_color(Grid2,C2))),
  subst(Grid1,C1,C2,Grid2).
one_change(blank1Color(C1),Grid1,Grid2):- 
  first_color(Grid1,C1),copy_cells(==(C1),free_cell,Grid1,Grid2).
one_change(same_size,Grid1,Grid2):- var(Grid2),grid_size(Grid1,C1),grid_size(Grid2,C1),!.
one_change(resize(C1,C2),Grid1,Grid2):- var(Grid2),grid_size(Grid1,C1),grid_size(Grid2,C2).

adjacent_point(HV,HV2):- adjacent_point(HV,_Dir,HV2).

individuals(ID,IndvO):- 
  individuals_raw(ID,Indv),
  unraw_inds(Indv,IndvO).

unraw_inds(IndvS,IndvO):-
  unraw_inds1(IndvS,Indv),
  unraw_inds2(Indv,IndvO),!.

unraw_inds1(IndvS,IndvO):- 
  findall(Hard-Indv,(member(Indv,IndvS),length(Indv,Hard)),All),
  keysort(All,AllK),
  reverse(AllK,AllR),
  findall(Indv,member(_-Indv,AllR),IndvO).

unraw_inds2(IndvS,IndvO):-   fail,
  select([C-Point1],IndvS,Rest1),
  select([C-Point2],Rest1,Rest),
  findall(C-Point,member([C-Point],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(IndvM,IndvMO),
  Grp=[C-Point1,C-Point2|CRest],
  IndvO=[Grp|IndvMO].
unraw_inds2(IndvS,IndvO):-   
  select([C-Point1],IndvS,Rest1),
  select([C2-Point2],Rest1,Rest),
  findall(C3-Point3,member([C3-Point3],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(IndvM,IndvMO),
  Grp=[C-Point1,C2-Point2|CRest],
  IndvO=[Grp|IndvMO].
unraw_inds2(IndvS,IndvO):-   
  select([C-Point1],IndvS,Rest1),
  select(Grp,Rest1,Rest),
  adjacent_groups([C-Point1],Grp),
  unraw_inds2([[C-Point1|Grp]|Rest],IndvO).
/*
unraw_inds2(IndvS,IndvO):-   
  select([C1-Point1],IndvS,Rest),
  nearby_one(C,C1-Point1,Rest),
  select([C-Point2],Rest1,Rest),
  findall(C-Point,member([C-Point],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(IndvM,IndvMO),
  Grp=[C-Point1,C-Point2|C-Rest],
  IndvO=[Grp|IndvMO].
*/
unraw_inds2(IndvS,IndvS).

adjacent_groups(Grp1,Grp2):- member(_-P1,Grp1),member(_-P2,Grp2),adjacent_point(P1,P2).


individuals_raw(ID,Indv):-
  grid_cells(ID,Cells),
  individuals_list(Cells,Indv).

individuals_list([],[]):-!.
individuals_list(Cells,[Indv|IndvList]):- 
    select(C-HV,Cells,Rest), adjacent_point(HV,HV2),select(C-HV2,Rest,ScanPoints),!,  
    all_individuals_near(C,[C-HV,C-HV2],ScanPoints,NextScanPoints,Indv),!,
    individuals_list(NextScanPoints,IndvList). 
individuals_list(Cells,IndvList):- maplist(obj1,Cells,IndvList).
obj1(X,[X]).

all_individuals_near(_C,NewSet,[],[],[],NewSet):-!.
all_individuals_near(C,Indv,ScanPoints,NewScanPoints,NewSet):-
   individuals_near(C,Indv,ScanPoints,New,NextScanPoints),
   (New == [] -> (NewSet = Indv, NewScanPoints = NextScanPoints)
    ; (append(Indv,New,IndvNew),
        all_individuals_near(C,IndvNew,NextScanPoints,NewScanPoints,NewSet))).

individuals_near(_C,_From,[],[],[]):-!.

individuals_near(C,From,[E|ScanPoints],[E|Nears],NextScanPoints):- nearby_one(C,E,From),!,
  individuals_near(C,[E|From],ScanPoints,Nears,NextScanPoints).

individuals_near(C,From,[E|ScanPoints],Nears,[E|NextScanPoints]):- individuals_near(C,From,ScanPoints,Nears,NextScanPoints).

nearby_one(C,C-E,List):- adjacent_point(E2,E), member(C-E2,List).
/*
nearby_one(C,C-E,List):- adjacent_point(E2,E), member(OC-E2,List),nothing_near(C,E,OC,E2,List).

nothing_near(OrigC,OrigPoint,OC,E,List):- OC\==OrigC,
          adjacent_point(E2,E), member(C2-E2,List),
          E2\==OrigPoint, C2\==OrigC, C2\==OC.
*/

individuals_list([],[],[]).
individuals_list([C-HV|T],[C-HV,How|Adjs],Rest):-
  adjacent_point(HV,How,HV2),
  select(C-HV2,T,RT),RT\==T,!,
  individuals_list([C-HV2|RT],Adjs,Rest).
individuals_list([C-HV|T],[C-HV],T). 
% eventually use 2nd arg as a hueristic

%g(V,H,obj(OV,OH,C,[CellList])).

%individuals_from(Cells,C,HV,[How|Indv]):- adjacent_point(HV,How,HV2),individual_from(ID,C,H2,V2,Indv).
%individuals_from(Cells,_C,_HV,[]):- !.

embue_points(size(H,V),Res,[size(H,V)|Res]). 


points_to_grid(Points,Grid):- 
  must_det_l((
  grid_size(Points,size(H,V)),
  grid_size_nd(Grid,size(H,V)),
  maplist(add_point(Grid),Points))).




insert_row(N,Row,Grid,NewGrid):- grid_size(Grid,size(H,V)), insert_row(N,Row,Grid,size(H,V),NewGrid).
insert_row(N,Row,Grid,size(H,V),NewGrid):- N<0, NewN is V + N+1,!,insert_row(NewN,Row,Grid,size(H,V),NewGrid).
insert_row(N,Row,Grid,size(H,_),NewGrid):- length(Row,H),length(Left,N),append(Left,Right,Grid),append(Left,[Row|Right],NewGrid).

insert_col(N,Col,Grid,NewGrid):- grid_size(Grid,size(H,V)), insert_col(N,Col,Grid,size(H,V),NewGrid).
insert_col(N,Col,Grid,size(H,V),NewGrid):- N<0, NewN is H + N+1,!,insert_col(NewN,Col,Grid,size(H,V),NewGrid).
insert_col(N,Col,Grid,size(_,V),NewGrid):- length(Col,V),maplist(insert_col_at(N),Col,Grid,NewGrid).

insert_col_at(N,Col,Row,NewRow):- length(Left,N),append(Left,Right,Row),append(Left,[Col|Right],NewRow).

insert_ele(N,V,L,NL):- length(Left,N),append(Left,Right,L),append(Left,[V|Right],NL).

delete_row(N,Grid,NewGrid):- N < 0, length(Grid,L), DR is L+N+1,delete_row(DR,Grid,NewGrid).
delete_row(N,Grid,NewGrid):- length(Left,N),append(Left,[_|Right],Grid),append(Left,Right,NewGrid).

delete_col(N,Grid,NewGrid):- maplist(delete_row(N),Grid,NewGrid).

map_nth(P,N,Grid):- nth1(N,Grid,E),call(P,E).
map_row(P,N,Grid):- map_nth(maplist(P),N,Grid).
map_col(P,N,Grid):- maplist(map_nth(P,N),Grid).

add_point(_,size(_,_)).
add_point(Grid,Point):- as_hv_point(H,V,C,Point),add_h_v_c(Grid,H,V,C).
%add_h_v_c(Grid,H,V,C):- var(C),!,nop(add_h_v_c(Grid,H,V,C)).
add_h_v_c(Grid,H,V,C):- hv_value(Grid,Was,H,V),ignore(Was=C).



hv_value_or(Grid,C,H,V,Else):- hv_value(Grid,C,H,V)*->true;C=Else.
hv_value(ID,C,H,V):- cmem(ID,HV,C),hv_point(H,V,HV).
hv_value(Points,C,H,V):- is_list(Points), is_points(Points), !,member(C-HV,Points),hv_point(H,V,HV).
hv_value(Grid,C,H,V):- is_grid(Grid),!,nth1(V,Grid,Row),nth1(H,Row,C).
/*b_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),set_nth1(H,Row,C).
nb_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),nb_set_nth1(H,Row,C).
b_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),rplc_nth1(H,Row,OldC,NewC).
nb_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),nb_rplc_nth1(H,Row,OldC,NewC).

*/
% turtle(H,V,Dir,N,H2,V2):- 
prim_ops([
call_object_grid_size(obj),
trim_grid_to_size(point,size),
fill_from_point(point,color),
create_a_ray(point,dir,len),
object_as_own_grid(obj,gridOps),
copy_one_object(obj,point),
rotate_one_object(obj,nsew),
flatten_one_object(obj),
sort_by_gravity(nsew),
flip_grid(hOrv),
rotate_grid(nsew)]).

is_points(H):- is_list(H),maplist(is_cpoint,H).
is_cpoint(C):- \+ compound(C),!,fail.
is_cpoint(size_range(C,_,_,_)):- !, nonvar(C).
is_cpoint(prop(_)).
is_cpoint(prop(_,_)).
is_cpoint(size(_,_)).
is_cpoint(C-P):- integer(C),!,atom(P).
is_cpoint(C-P):- var(C),!,atom(P).


create_movements:- 
 forall( between(1,30,H),
  forall(between(1,30,V),
  calc_movement(H,V))).

:- initialization(create_movements).

calc_movement(H,V):- forall(nav(Dir,HO,VO), save_calc_movement(H,V,Dir,HO,VO)).

save_calc_movement(H,V,Dir,HO,VO):- H2 is HO+H, V2 is VO+V,
  ignore((between(1,30,H2), between(1,30,V2), 
     format(atom(HV),'point_~|~`0t~d~2+_~|~`0t~d~2+',  [H,V]),
    format(atom(HV2),'point_~|~`0t~d~2+_~|~`0t~d~2+', [H2,V2]),
    assert_if_new(adjacent_point(HV,Dir,HV2)),
    assert_if_new(hv_point(H,V,HV)),
    assert_if_new(adjacent_point(H,V,Dir,H2,V2)))).
  
  



nav(s,0,1). nav(e, 1,0). nav(w,-1,0). nav(n,0,-1).
nav(se, 1,1). nav(sw,-1,1). nav(nw,-1,-1). nav(ne, 1,-1).

rot45(s,sw). rot45(sw,w). rot45(w,nw). rot45(nw,n). rot45(n,ne). rot45(ne,e). rot45(e,se). rot45(se,s).


free_cell(Var):- var(Var),!.
free_cell(0).

copy_cells(B,A,H,HH):- call(B,H),!,call(A,HH).
copy_cells(_,_,H,H):- \+ is_list(H),!.
copy_cells(_,_,[],[]):-!. 
copy_cells(B,A,[H|T],[HH|TT]):-!, copy_cells(B,A,H,HH), copy_cells(B,A,T,TT).
  

same_grid(Grid1,Grid1).

  


