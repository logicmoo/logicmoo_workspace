/** <examples>
?- aleph_sample:induce_tree(Program).
*/
:- module(aleph_sample,[]).

:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- use_module(library(logicmoo_utils)).

:- use_module(h_muarc_alephlib).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(prolog).
:- endif.

:- aleph.
% Simple illustration of positive-only learning within Aleph
% To run do the following:
%       a. Load Aleph
%       b. read_all(animals).
%       c. sat(1).
%       d. reduce.
%	or
%       a. Load Aleph
%       b. read_all(animals).
%       c. induce.

/** <examples>
?- induce(Program).
*/


:- aleph.

:- aleph_set(evalfn,posonly).
:- aleph_set(clauselength,2).
:- aleph_set(gsamplesize,20).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% class/2 learns the class (mammal/fish/reptile/bird) of various animals.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode declarations

:- modeh(1,class(+animal,#class)).
:- modeb(1,has_gills(+animal)).
:- modeb(1,has_covering(+animal,#covering)).
:- modeb(1,has_legs(+animal,#nat)).
:- modeb(1,homeothermic(+animal)).
:- modeb(1,has_eggs(+animal)).
:- modeb(1,not(has_gills(+animal))).
:- modeb(1,nhas_gills(+animal)).
:- modeb(*,habitat(+animal,#habitat)).
:- modeb(1,has_milk(+animal)).

:-determination(class/2,has_gills/1).
:-determination(class/2,has_covering/2).
:-determination(class/2,has_legs/2).
:-determination(class/2,momeotermic/1).
:-determination(class/2,has_egss/1).
:-determination(class/2,nhas_gills/1).
:-determination(class/2,habitat/2).
:-determination(class/2,has_milk/1).

:-begin_bg.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Types

animal(dog).  animal(dolphin).  animal(platypus).  animal(bat).
animal(trout).  animal(herring).  animal(shark). animal(eel).
animal(lizard).  animal(crocodile).  animal(t_rex).  animal(turtle).
animal(snake).  animal(eagle).  animal(ostrich).  animal(penguin).
animal(cat). animal(dragon).  animal(girl).  animal(boy).


class(mammal).  class(fish).  class(reptile).  class(bird).

covering(hair).  covering(none).  covering(scales).  covering(feathers).

habitat(land).  habitat(water).  habitat(air).  habitat(caves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Background knowledge

has_covering(dog,hair).
has_covering(dolphin,none).
has_covering(platypus,hair).
has_covering(bat,hair).
has_covering(trout,scales).
has_covering(herring,scales).
has_covering(shark,none).
has_covering(eel,none).
has_covering(lizard,scales).
has_covering(crocodile,scales).
has_covering(t_rex,scales).
has_covering(snake,scales).
has_covering(turtle,scales).
has_covering(eagle,feathers).
has_covering(ostrich,feathers).
has_covering(penguin,feathers).

has_legs(dog,4).
has_legs(dolphin,0).
has_legs(platypus,2).
has_legs(bat,2).
has_legs(trout,0).
has_legs(herring,0).
has_legs(shark,0).
has_legs(eel,0).
has_legs(lizard,4).
has_legs(crocodile,4).
has_legs(t_rex,4).
has_legs(snake,0).
has_legs(turtle,4).
has_legs(eagle,2).
has_legs(ostrich,2).
has_legs(penguin,2).

has_milk(dog).
has_milk(cat).
has_milk(dolphin).
has_milk(bat).
has_milk(platypus).


homeothermic(dog).
homeothermic(cat).
homeothermic(dolphin).
homeothermic(platypus).
homeothermic(bat).
homeothermic(eagle).
homeothermic(ostrich).
homeothermic(penguin).


habitat(dog,land).
habitat(dolphin,water).
habitat(platypus,water).
habitat(bat,air).
habitat(bat,caves).
habitat(trout,water).
habitat(herring,water).
habitat(shark,water).
habitat(eel,water).
habitat(lizard,land).
habitat(crocodile,water).
habitat(crocodile,land).
habitat(t_rex,land).
habitat(snake,land).
habitat(turtle,water).
habitat(eagle,air).
habitat(eagle,land).
habitat(ostrich,land).
habitat(penguin,water).

has_eggs(platypus).
has_eggs(trout).
has_eggs(herring).
has_eggs(shark).
has_eggs(eel).
has_eggs(lizard).
has_eggs(crocodile).
has_eggs(t_rex).
has_eggs(snake).
has_eggs(turtle).
has_eggs(eagle).
has_eggs(ostrich).
has_eggs(penguin).

has_gills(trout).
has_gills(herring).
has_gills(shark).
has_gills(eel).

nhas_gills(X) :- animal(X), not(has_gills(X)).
aleph_false:-class(X,Y),class(X,Z),Y\=Z.

:-end_bg.
:-begin_in_pos.
class(eagle,bird).
class(bat,mammal).
class(dog,mammal).
class(bat,mammal).
class(eagle,bird).
class(ostrich,bird).
class(shark,fish).
class(crocodile,reptile).
class(bat,mammal).
class(shark,fish).
class(penguin,bird).
class(shark,fish).
class(crocodile,reptile).
class(crocodile,reptile).
class(shark,fish).
class(dog,mammal).
class(snake,reptile).
class(platypus,mammal).
class(t_rex,reptile).
class(crocodile,reptile).
:-end_in_pos.

:-begin_in_neg.

:-end_in_neg.


%:- fixup_exports.

%:- module(user).
%:- user:ensure_loaded('../kaggle_arc').
%:- module(aleph_sample).
:- wdmsg("% try:  ?- aleph_sample:induce_tree(Program),!,pp(Program). ").

aa:- nodebug,notrace,cls,(aleph_sample:induce_tree(Program)),notrace,!,pp(Program).

end_of_file.


% Simple illustration of constructing tree-based models within Aleph
% To run do the following:
%       a. Load Aleph
%       b. read_all(wedge).
%       c. induce_tree.

% Model trees are constructed by specifying a predicate that
% will be used for model construction for examples in a leaf.
% The user has to provide a definition for this predicate that
% is able to: (a) construct the model; and (b) predict using
% the model constructed. The trick used is the equal as that
% for lazy evaluation.

% Learning a model tree
% The function to be learnt is:
%             y = f(x) =  x + 1 (x =< 0)
%                      = -x + 1 ( x > 0)
% That is:
%
%                 |
%                /|\
%               / | \
%              /  |  \
%      --------------------------
%                 0

% what Aleph actually learns with the data given is:
%             y = f(x) =  x + 1 (x =< -0.5)
%                      = -x + 1 ( x > -0.5)
% adding more examples rectifies this: see wedge.f


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% specify tree type

:- aleph_set(tree_type,model).
:- aleph_set(evalfn,mse).
:- aleph_set(minpos,2).       % minimum examples in leaf for splitting
:- aleph_set(mingain,0.01).	% toy example needs this to be low
:- aleph_set(dependent,2).	% second argument of f/2 is to predicted
:- aleph_set(verbosity,10).
%:- aleph_set(mingain,-1e10).

% specify predicate definition to use for model construction
:- model(predict/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode declarations

:- modeh(1,f(+x,-y)).
:- modeb(1,lteq(+x,#threshold)).
:- modeb(1,predict(+x,-y,#params)).

:- determination(f/2,lteq/2).
:- determination(f/2,predict/3).

:-begin_bg.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type definitions

threshold(-0.5).
threshold(0.0).
threshold(0.5).

params([_Slope,_Constant,_Sd]).

list([_|_]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Background

lteq(X,Y):-
        var(Y), !,
        X = Y.
lteq(X,Y):-
        number(X), number(Y),
        X =< Y.

% definition for model construction (parameter estimation)
predict(X,Y,[M,C,Sd]):-
	list(X),
	list(Y), !,
	l_regress1(Y,X,M,C,Sd).
% definition for prediction
predict(X,Y,[M,C,_]):-
	number(X), var(Y), !,
	Y is M*X + C.
% definition for model checking
predict(X,Y,[M,C,Sd]):-
	number(Y), number(X), !,
	Y1 is M*X + C,
	Diff is Y - Y1,
	abs_val(Diff,ADiff),
	ADiff =< 3*Sd.

% very simple univariate linear regression
l_regress1([YVals|_],[XVals|_],M,C,0.0):-
        YVals = [Y1,Y2|_],
        XVals = [X1,X2|_],
        M is (Y2-Y1)/(X2-X1),
        C is Y1 - M*X1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraints

% remove redundant checks for =<
% prune((_:-Body)):-
% 	has_literal(lteq(X,Y),Body,Left),
% 	has_literal(lteq(X1,Y1),Left,_),
% 	X == X1,
% 	Y1 =< Y.
% 
% has_literal(L,(L,L1),L1).
% has_literal(L,(_,L1),Left):-
% 	!,
% 	has_literal(L,L1,Left).
% has_literal(L,L,true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities

abs_val(X,Y):- X < 0, !, Y is -X.
abs_val(X,X):- X >= 0.

:-end_bg.
:-begin_in_pos.

f(-1.0,0.0).
f(-0.5,0.5).
% f(-0.25,0.75).	% adding this results in the correct theory
f(0.0,1.0).
f(0.5,0.5).
f(1.0,0.0).

:-end_in_pos.
:-begin_in_neg.
:-end_in_neg.




