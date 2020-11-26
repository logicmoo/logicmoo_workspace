%% some simple tests to see if Pfc is working properly

:- mpred_trace.

time(Call,Time) :-
  statistics(runtime,_),
  call(Call),
  statistics(runtime,[_,Time]).


test0 :- 
  fcAdd([(p(X) ==> q),
       p(1),
       (p(X), ~r(X) ==> s(X)),
       (t(X), {X>0} ==> r(X)),
       (t(X), {X<0} ==> minusr(X)),
       t(-2),
       t(1)]).

test1 :-
  consult('~finin/pfc/examples/kinship.pfc'),
  consult('~finin/pfc/examples/finin.pfc').

test2 :-
  fcAdd([(a(X),~b(Y)/(Y>X) ==> biggest(a)),
       (b(X),~a(Y)/(Y>X) ==> biggest(b)),
        a(5)]).


%test3 :-
%  fcAdd([(a(X),\+(b(Y))/(Y>X) ==> biggest(a)),
%       (b(X),\+a((Y))/(Y>X) ==> biggest(b)),
%        a(5)]).


test4 :-
    fcAdd([(foo(X), bar(Y)/{X=:=Y} ==> foobar(X)),
         (foobar(X), go ==> found(X)),
	 (found(X), {X>=100} ==> big(X)),
	 (found(X), {X>=10,X<100} ==> medium(X)),
	 (found(X), {X<10} ==> little(X)),
	 foo(1),
	 bar(2),
	 bar(1),
	 foo(100),
	 goAhead,
	 bar(100)
	]).


test5 :-
    fcAdd([(faz(X), ~baz(Y)/{X=:=Y} ==> fazbaz(X)),
         (fazbaz(X), go ==> found(X)),
	 (found(X), {X>=100} ==> big(X)),
	 (found(X), {X>=10,X<100} ==> medium(X)),
	 (found(X), {X<10} ==> little(X)),
	 faz(1),
	 goAhead,
	 baz(2),
	 baz(1)
	]).


test6 :-
    fcAdd([(d(X), ~f(Y)/{X=:=Y} ==> justD(X)),
         (justD(X), go ==> dGo(X)),
	 d(1),
	 go,
	 f(1)
	]).


test7 :-
    fcAdd([(g(X), h(Y)/{X=:=Y} ==> justG(X)),
         (justG(X), go ==> gGo(X)),
	 g(1),
	 go,
	 h(1)
	]).


test8 :-
    fcAdd([(j(X), k(Y) ==> bothJK(X,Y)),
         (bothJK(X,Y), go ==> jkGo(X,Y)),
	 j(1),
	 go,
	 k(2)
	]).


test9 :-
    fcAdd([(j(X), k(Y) ==> bothJK(X,Y)),
         (bothJK(X,Y) ==> jkGo(X,Y)),
	 j(1),
	 k(2)
	]).

test10 :-
  fcAdd([
	(j(X), k(Y) ==> bothJK(X,Y)),
	(bothJK(X,Y), go ==> jkGo(X,Y)),
	j(1),
	go,
	k(2)
       ]).
