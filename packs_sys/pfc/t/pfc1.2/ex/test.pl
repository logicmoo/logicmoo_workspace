%% some simple tests to see if Pfc is working properly


time(Call,Time) :-
  statistics(runtime,_),
  call(Call),
  statistics(runtime,[_,Time]).


test0 :- 
  add([(p(X) => q),
       p(1),
       (p(X), ~r(X) => s(X)),
       (t(X), {X>0} => r(X)),
       (t(X), {X<0} => minusr(X)),
       t(-2),
       t(1)]).

test1 :-
  consult('~finin/pfc/kinship.pfc'),
  consult('~finin/pfc/finin.pfc').

