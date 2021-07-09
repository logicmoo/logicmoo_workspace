
:- style_check(-discontiguous).
% Ran with (base) root@mail:/opt/logicmoo_workspace/packs_sys/logicmoo_ec/prolog/ec_planner# swipl -f /opt/logicmoo_workspace/.swiplrc -l ec_config.pl

expression(SVO):-  expression_l(ExpandLists),print_el(nl),expand_lists1(ExpandLists,SVO).
expression([S,V,O]):- fail,  verb(X,V),subj(X,S),nl, obj(X,O), (S=(exemplar) -> O==S ; true),
   V\==O,
   O \== (intent),
   true.

type_list(X,Y):-expression_l([X,contains,Y]),!.

narrative_onto :- fail.

%type_list(changeOf,[add,del,'Change']).
%type_list(happeningOf,['start/Cont/Stop']).
/*
expression_l([
  beliefState,
  contains,
  [intent,worldState]]).

expression_l([
  beliefs,
  contains,
  [holds(beliefState)]]).

expression_l([
  precept,
  contains,
  [change(worldState)]]).

expression_l([
  worldState,
  contains,
  [acting,exemplar,hasProps]]).

expression_l([
  [(hasProps)],
   ['enables/Disables'],
  [(worldState)]]).
expression_l([
  [acting],
   ['resultsIn/Terminates'],
  [change(worldState)]]).

expression_l([
  [change(acting)],
   causes,
  [worldState]]).


expression_l([
   narrativePLL,
  contains,
  ['NarrativePLL',bagOf(beliefState),bagOf(precept)]]).

expression_l([
  narrativePLL,
  +,'precept', (=>),
  [beliefs]]).
*/

%expression_l(4, [[a,b,c],[p1,p2],[x,y,z]]).
expression_l([
  [narrative],
  [contains],
  [narrative,state,event]]) :- narrative_onto.
expression_l([
  [event],
  [contains],
  [(precept),(action),change(state)]]) :- narrative_onto.
expression_l([
  [some(state)],
  [contains],
  [(goal),(hasProp),(exemplar)]]) :- narrative_onto.

%type_list(event,[(precept),(action),change(state)]).
%type_list(state,[(goal),(hasProp),(exemplar)]).

expression_l([
  [state],
  +,precept, (=>),
  [event,state]]) :- narrative_onto.

expression_l([
  [event,state],
   ['enables/Disables',resultsIn,terminates],
  [state,event]]) :- narrative_onto.

expression_l([
  [action],
   causes,
  [goal,action,hasProp,precept,exemplar,narrative]]) :- narrative_onto.

expression_l([
  [goal,action,hasProp,precept],
  [contains],
  [goal,action,hasProp]]) :- narrative_onto.


expression_l([belief(state),'enables/Disables',action]) :- narrative_onto.
expression_l([doing(action),'Causes/Terminate',state]) :- narrative_onto.
expression_l([state,contains,[true(prop),false(prop)]]) :- narrative_onto.
expression_l([action,contains,[start(act),cont(act),stop(act)]]) :- narrative_onto.
expression_l([plan,contains,listOf(action)]) :- narrative_onto.

expression_l([some(happening),each([is,will,can]),optional(not),be,each([possible,impossible,happening]),each([before,after,when,during,exclusive-to]),
   someone,each([has,will,could]),optional(not),each([start,stop,continue]),to,optional(not),each([want,notice,remember,forget,cause,prevent]),
  some(happening),each([does,will,could]),optional(not),optional(start,stop,continue),to,optional(not),be,optional(not),each([possible,impossible,happening])]):-
  fail.

expression_l([some(happening),each([is,will,can]),optional(not),be,each([possible,impossible,happening]),each([before,after,when,during,exclusive-to]),
  physics,each([has,will,could]),optional(not),each([start,stop,continue]),to,optional(not),each([cause,prevent]),
  some(happening),each([does,will,could]),optional(not),optional(start,stop,continue),to,optional(not),be,optional(not),each([possible,impossible,happening])]):-
 fail.
expression_l([some(happening),each([is,will,can]),optional(not),be,each([possible,impossible,happening]),
  each([before,after,during,exclusive-to]),
  when,physics,each([has,will,could]),optional(not),each([cause,prevent]),
  some(happening),'to/from',optional(not),each([start,stop,continue])]):-
 fail.

expression_l([some(happening),each([is,will,can]),optional(not),be,each([possible,impossible,occuring]),
  each([before,after,during,exclusive-to]),some(happening)]).

/*
expression_l([some(stag),'is/isnt',instance,of,type]).
expression_l([some(stag),'is/isnt',subpart,of,some(stag)]).
expression_l([some(stag),'is/isnt',slot-role,of,some(stag)]).
expression_l([some(stag),and,some(stag),'has/will/could',optional(not),combined,to,make,some(stag)]).
*/
expand_lists1([A|Lists],[A|VO]):- atom(A),!, expand_lists(Lists,VO).
expand_lists1(Lists,VO):- expand_lists(Lists,VO).

expand_lists([Expand|Lists],[S|VO]):- expand_ele(Expand,S), expand_lists(Lists,VO).
expand_lists([],[]).

expand_ele(Atom,S):- var(Atom),!,S=Atom.
expand_ele(Expand,S):- type_list(Expand,List),!,expand_ele(List,S).
expand_ele(Atom,S):- \+ compound(Atom),!,S=Atom.
expand_ele(Expand,S):- is_list(Expand),!,member(List,Expand),expand_ele(List,S).
expand_ele(Expand,S):- compound_name_arguments(Expand,'optional',List),!,expand_ele([''|List],S).
expand_ele(some(X),S):- !, expand_ele(X,N),gensym(N,S).
expand_ele(each(X),S):- !, expand_ele(X,S).
expand_ele(Expand,S):- compound_name_arguments(Expand,F,[X]),
  expand_ele(F,FS),compound_name_arguments(S,FS,[Y]),expand_ele(X,Y).
expand_ele(S,S).

subj(1,(narrativePLL)).
subj(1,(hasProp)).
subj(1,(intent)).
subj(1,(acting)).
verb(1,plus(precept)).
verb(1,contains).
verb(1,enables).
verb(1,disables).
verb(1,sequencesTo).
obj(1,O):-subj(1,O).


verb(2,triggers).
subj(2,(narrativePLL)).
subj(2,(precept)).
subj(2,(hasProp)).
%subj(2,(intent)).
subj(2,(acting)).

%verb(3,contains).
%verb(3,enables).
%verb(3,disables).
%verb(3,sequencesTo).
verb(3,plus(precept)).
verb(3,plus(acting)).
subj(3,(narrativePLL)).
subj(3,(hasProp)).
subj(3,(intent)).
subj(3,(acting)).

%verb(_,sequencesTo(byChoice)).
%verb(_,'sequencesTo(Automatically)').
%verb(_,sequenceTo('auto/Choice')).
%verb(X,implies).

%subj(1,(phrase)).
%subj(1,(percept)).

%subj(1,(exemplar)).
%subj(X,class).

toPC(t(R,C),M):- toPC(C,PC),M=..[R,PC],!.
toPC(S,M):- is_list(S),!,must_maplist(toPC,S,M).
toPC(S,M):- compound(S),!,
   compound_name_arguments(S,F,Args),
   toPC([F|Args],[U|ArgsO]),
   compound_name_arguments(M,U,ArgsO),!.
toPC(X,Y):- toUpperCamelcase(X,Y).

print_el(nl):- format('~N~n',[]),!.
print_el(S):- toPC(S,O), format('~w ',[O]),!.


printall:-   
  expression(SVO),  
  \+ [exemplar|_]=SVO,
  format('~N',[]),
  maplist(print_el,SVO),
  format('~N',[]),
  fail.
printall.

nm:- mmake,
 %cls, 
 printall.
%:- nm.

end_of_file.


