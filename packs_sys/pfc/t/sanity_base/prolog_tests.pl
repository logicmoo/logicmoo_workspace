%% some simple tests to see if Pfc is working properly

:- include(test_header).

:- mpred_trace.

time(Call,Time) :-
  statistics(runtime,_),
  call(Call),
  statistics(runtime,[_,Time]).


test0 :- 
  ain([(p(X) ==> q),
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
  ain([(a(X),~b(Y)/(Y>X) ==> biggest(a)),
       (b(X),~a(Y)/(Y>X) ==> biggest(b)),
        a(5)]).


%test3 :-
%  ain([(a(X),\+(b(Y))/(Y>X) ==> biggest(a)),
%       (b(X),\+a((Y))/(Y>X) ==> biggest(b)),
%        a(5)]).


test4 :-
    ain([(foo(X), bar(Y)/{X=:=Y} ==> foobar(X)),
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
    ain([(faz(X), ~baz(Y)/{X=:=Y} ==> fazbaz(X)),
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
    ain([(d(X), ~f(Y)/{X=:=Y} ==> justD(X)),
         (justD(X), go ==> dGo(X)),
	 d(1),
	 go,
	 f(1)
	]).


test7 :-
    ain([(g(X), h(Y)/{X=:=Y} ==> justG(X)),
         (justG(X), go ==> gGo(X)),
	 g(1),
	 go,
	 h(1)
	]).


test8 :-
    ain([(j(X), k(Y) ==> bothJK(X,Y)),
         (bothJK(X,Y), go ==> jkGo(X,Y)),
	 j(1),
	 go,
	 k(2)
	]).


test9 :-
    ain([(j(X), k(Y) ==> bothJK(X,Y)),
         (bothJK(X,Y) ==> jkGo(X,Y)),
	 j(1),
	 k(2)
	]).

test10 :-
  ain([
	(j(X), k(Y) ==> bothJK(X,Y)),
	(bothJK(X,Y), go ==> jkGo(X,Y)),
	j(1),
	go,
	k(2)
       ]).


:- dynamic(full_name/2).
:- dynamic(host_name/2).
:- dynamic(user_login/1).

:- include('attvar_01.pl').
:- include('fc_01.pfc').
:- include('bc_01.pfc').
:- include('birds_01a.pfc').
:- include('birds_01b.pfc').
:- include('birds_01c.pfc').
:- include('conan_01.pfc.pl').
:- include('df_01.pfc').
:- include('dl_01.pfc').
:- include('dmost_01.pfc').
:- include('file_01.pfc').
:- include('if_missing_01.pfc').
:- include('if_missing_01a.pfc').
:- include('load_time_01.pfc').
:- include('mpred_pfc_test_01.pl').
:- include('nd_01.pl').
:- include('neg_01.pfc').
:- include('pl_01.pfc').
:- include('shift_01.pl').
:- include('sv_default_01.pfc').
:- include('sv_in_arg_01.pfc').
:- include('sv_in_arg_01d.pfc').
:- include('undo_01.pfc').
:- include('undo_01a.pfc').
:- include('update_list_01.pfc').
:- include('update_number_01.pfc').
:- include('mt_01.pl').
:- include('mt_01a.pl').
:- include('mt_01b.pl').
:- include('mt_01c.pl').
:- include('mt_01d.pl').
:- include('mt_01e.pl').

