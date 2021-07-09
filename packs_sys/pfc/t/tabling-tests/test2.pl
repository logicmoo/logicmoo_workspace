
:- '$set_source_module'(_,user).
:- 'module'(user).

:- if((fail,exists_source(library(drac)))).
	:- use_module(library(drac)).
:- else.
	:- use_module(library(dra)).
:- endif.

/*
% Simpler example than example12.pl, but the number of predicates involved in mutual recursion will also increase at runtime.

expected_variants([p(3,_),p(2,_),q(2,_),q(3,_),p(_,_)]).
% Note: p(3,_) and q(3,_) are empty tables, but they are there.
expected_answers_for_variant(p(_,_),[p(1,2),p(2,3),p(1,3)]).
expected_answers_for_variant(p(3,_),[]).
expected_answers_for_variant(p(2,_),[p(2,3)]).
expected_answers_for_variant(q(2,_),[q(2,3)]).
expected_answers_for_variant(q(3,_),[]).
*/

:-table((p/2, q/2)).
:-export((p/2, q/2)).


p(X,Y) :-p(X,Z), q(Z,Y).
p(X,Y) :-e(X,Y).
q(X,Y) :-p(X,Y).

e(1,2).
e(2,3).

:- listing(p/2).

:- once(\+ tnot(p(_X,_Y))).

?- print_tables.

:- module_property('$dra',file(X)),file_directory_name(X,D),asserta(user:library_directory(D)),
			make_library_index(D, [ 'dr*.pl' ]).

?- listing((coinductive1)/1).
?- listing((coinductive0)/1).

lm:- use_module(library(logicmoo_utils)).
:-user:lm.

:- doc_server(6568,[allow(ip(_,_,_,_))]).

abolish_all(F/A):- functor(P,F,A),forall(current_predicate(_,M:P),(predicate_property(M:P,_),abolish(M:F/A))).

:- meta_predicate(call_all(0)).
call_defined(G):- current_predicate(_,G),predicate_property(G,_),!,run_test_goal(G).

load(F):- filematch(F,X), exists_file(X),!,load_files([X]),!.
load(X):- set_prolog_flag(verbose_file_search,true),prolog_load_context(directory,D),absolute_file_name(X,R,[relative_to(D)]),load_files([X]),!.
load(X):-  prolog_load_context(directory,D),trace,writeq(load(F->X)),[X].

abolish_top:- abolish_all(top/0),abolish_all(go/0),abolish_all(main/0),abolish_all(demo/0),retractall(is_topl(_)),!.

call_top:- member(G,[go,top,main,demo]),current_predicate(_,M:G),!,run_test_goal(M:G).
call_top:- forall(retract(is_topl(G)),run_test_goal(G)).

run_test_goal(G):- once(on_f_debug(G)).

:- dynamic(user:at_end_of_file/1).
user:term_expansion(end_of_file,end_of_file):- retract(user:at_end_of_file(G)),once(run_test_goal(G)),fail.

pf(X):- exists_source(X),!,abolish_top,!,load_files([X]),call_top.
pf(X):- expand_file_name(X,L),[X]\==L,!,maplist(pf,L).
pf(X):- dmsg(not_found(X)).

pfg(X):- exists_source(X),!,abolish_top,asserta(at_end_of_file(user:call_top)),load_files([X]).

:-source_location(S,_),prolog_load_context(module,FM),
 forall(source_file(M:H,S),
  ignore((functor(H,F,A),
   \+ atom_concat('$',_,F),
      M:export(M:F/A),
   \+ predicate_property(M:H,transparent),
%    dra_w(M:H),
   \+ atom_concat('__aux',_,F), FM:module_transparent(M:F/A)))).


:-pf(('examples/XSB/fib.tlp') ).

% :-pf(('examples/co_t.tlp') ).


:-pf(('examples/coind2.tlp') ).
% :-pf(('examples/LTL/v.pl') ).
%:-pf(('examples/mini_graph.tlp') ).
%:-pf(('examples/mini_language.tlp') ).
:-pf(('examples/paper_example.tlp') ).


:-['Bench/tabling/run'].

:-break.

:-pf(('Bench/prolog/run')).
:-pf(('Bench/clpfd/run')).
:-pf(('Bench/aspclp/run')).

:-break.

t0:-time([('examples/XSB/farmer.tlp')]).
tn:-time([('examples/tnot1.tlp')]).
t1:-time(process_file(('examples/XSB/farmer.tlp') )),!.
t2:-time([('examples/XSB/ham.tlp')]).
t2a:-time([('examples/XSB/ham_auto.tlp')]).

t2b:-time(pf(('examples/XSB/ham.tlp') )).
t3:-[(('examples/graph.tlp') )].
t4:-pf(('examples/module.tlp') ).
t4:-[(('examples/paper_example.tlp') )].
t4:-pf(('examples/conditional.clp') ).
t4:-pf(('examples/simple1.tlp') ).
t4:-pf(('examples/simple1_old_first.tlp') ).
t4:-pf(('examples/conditional.clp') ).
t4:-pf(('examples/small_comment_example.tlp') ).
t4:-pf(('examples/coind_new.tlp') ).
t5:-consult('Bench/tabling/tcl.pl').





		 /*******************************
		 *	      SCRIPTS		*
		 *******************************/


:- dynamic
	script_dir/1.

set_script_dir :-
	script_dir(_), !.
set_script_dir :-
	find_script_dir(Dir),
	assert(script_dir(Dir)).

find_script_dir(Dir) :-
	prolog_load_context(file, File),
	follow_links(File, RealFile),
	file_directory_name(RealFile, Dir).

follow_links(File, RealFile) :-
	read_link(File, _, RealFile), !.
follow_links(File, File).


:- set_script_dir.

run_test_script(Script) :-
	file_base_name(Script, Base),
	file_name_extension(Pred, _, Base),
	load_files(Script, [silent(true), if(changed)]),
	(   current_prolog_flag(verbose, normal)
	->  format(user_error, '(~w)', [Base]), flush_output
	;   true
	),
	call_test(Pred, script).

run_test_scripts(Directory) :-
	(   script_dir(ScriptDir),
	    atomic_list_concat([ScriptDir, /, Directory], Dir),
	    exists_directory(Dir)
	->  true
	;   Dir = Directory
	),
	atom_concat(Dir, '/*.pl', Pattern),
	expand_file_name(Pattern, Files),
	file_base_name(Dir, BaseDir),
	format(user_error, '~NRunning scripts from ~w ', [BaseDir]),
	run_scripts(Files),
	format(user_error, ' done~n', []).

run_scripts([]).
run_scripts([H|T]) :-
	(   catch(run_test_script(H), Except, true)
	->  (   var(Except)
	    ->  put_ok
	    ;   Except = blocked(Reason)
	    ->  assert(blocked(H, Reason)),
		put_blocked
	    ;   script_failed(H, Except)
	    )
	;   script_failed(H, fail)
	),
	run_scripts(T).

script_failed(File, fail) :-
	format(user_error, '~NScript ~w failed~n', [File]),
	assert(failed(script(File))).
script_failed(File, Except) :-
	message_to_string(Except, Error),
	format(user_error, '~NScript ~w failed: ~w~n', [File, Error]),
	assert(failed(script(File))).





%%	testdir(Dir)
%
%	Enumerate directories holding tests.

testdir('Tests/core').
testdir('Tests/attvar').
testdir('Tests/library').
testdir('Tests/charset').
testdir('Tests/eclipse').
testdir('Tests/clp').
testdir('Tests/GC').
testdir('Tests/thread') :-
	current_prolog_flag(threads, true).
testdir('Tests/save').

:- dynamic
	failed/1,
	blocked/2.

test :-
	retractall(failed(_)),
	retractall(blocked(_,_)),
	forall(testset(Set), runtest(Set)),
	scripts,
	garbage_collect,
	garbage_collect_atoms,
	trim_stacks,
	statistics,
	report_blocked,
	report_failed.

scripts :-
	forall(testdir(Dir), run_test_scripts(Dir)).


report_blocked :-
	findall(Head-Reason, blocked(Head, Reason), L),
	(   L \== []
        ->  format(user_error, '~NThe following tests are blocked:~n', []),
	    (	member(Head-Reason, L),
		format(user_error, '    ~p~t~40|~w~n', [Head, Reason]),
		fail
	    ;	true
	    )
        ;   true
	).
report_failed :-
	findall(X, failed(X), L),
	length(L, Len),
	(   Len > 0
        ->  format(user_error, '~N*** ~w tests failed ***~n', [Len]),
	    fail
        ;   format(user_error, '~NAll tests passed~n', [])
	).

%%	call_test(:Goal, +Line)
%
%	Call the actual test. If dmalloc/3 is provided, call through
%	this leak-detection hook.
%
%	@see test/dmalloc.pl

:- meta_predicate
	call_test(0, +).

:- if(current_predicate(dmalloc/3)).
call_test(Goal, script) :- !,
	dmalloc((Goal->true), '*** Script ~w ***', [Goal]).
call_test(Goal, Line) :-
	dmalloc((Goal->true), '*** Line ~d: ~p ***', [Line, Goal]).
:- elif(current_prolog_flag(test_concurrent, true)).
call_test(Goal, _Line) :-
	test_name(Goal, Name),
	with_mutex(Name, Goal).

test_name(M:G, Name) :- !,
	functor(G, Pred, Arity),
	format(atom(Name), 'test ~w:~w/~d', [M,Pred,Arity]).
test_name(G, Name) :- !,
	functor(G, Pred, Arity),
	format(atom(Name), 'test ~w/~d', [Pred,Arity]).
:- else.
call_test(Goal, _Line) :-
	Goal, !.
:- endif.

runtest(Name) :-
	format(user_error, '~NRunning test set "~w" ', [Name]),
	functor(Head, Name, 1),
	findall(Head-R, nth_clause_head(Head, R), Heads),
	unique_heads(Heads),
	member(Head-R, Heads),
	clause_property(R, line_count(Line)),
	(   current_prolog_flag(verbose, normal)
	->  format(user_error, '(~w)', [Line])
	;   true
	),
	(   catch(call_test(Head, Line), Except, true)
	->  (   var(Except)
	    ->  put_ok
	    ;   Except = blocked(Reason)
	    ->  assert(blocked(Head, Reason)),
		put_blocked
	    ;   test_failed(R, Except)
	    )
	;   test_failed(R, fail)
	),
	fail.
runtest(_) :-
	format(user_error, ' done.~n', []).

nth_clause_head(Head, R) :-
	nth_clause(Head, _N, R),
	clause(Head, _, R).

unique_heads(Heads) :-
	keysort(Heads, Sorted),
	check_uniqye(Sorted).

check_uniqye([]).
check_uniqye([Head-R1,Head-R2|T]) :- !,
	clause_property(R1, line_count(Line1)),
	clause_property(R1, file(File1)),
	clause_property(R2, line_count(Line2)),
	clause_property(R2, file(File2)),
	format('~N~w:~d: test ~w duplicated at ~w:~d~n',
	       [File2, Line2, Head, File1, Line1]),
	check_uniqye([Head-R1|T]).
check_uniqye([_|T]) :-
	check_uniqye(T).


:- if(current_prolog_flag(test_concurrent, true)).
put_ok :-
	thread_self(Me),
	atom_concat(tester_, Id, Me), !,
	write(user_error, Id).
:- endif.
put_ok :-
	write(user_error, .).

put_blocked :-
	write(user_error, !).

test_failed(R, Except) :-
	clause(Head, _, R),
	functor(Head, Name, 1),
	arg(1, Head, TestName),
	clause_property(R, line_count(Line)),
	clause_property(R, file(File)),
	(   Except == fail
	->  format(user_error, '~N~w:~d: Test ~w(~w) failed~n',
		   [File, Line, Name, TestName])
	;   message_to_string(Except, Error),
	    format(user_error, '~N~w:~d: Test ~w(~w):~n~t~8|ERROR: ~w~n',
		   [File, Line, Name, TestName, Error])
	),
	assert(failed(Head)).

blocked(Reason) :-
	throw(blocked(Reason)).


%%	error(+Exception, +Expected)
%
%	Check whether the correct exception  is thrown, disregarding the
%	2nd context argument.

error(error(Ex, _Ctx), Expected) :-
	subsumes_term(Expected, Ex), !.
error(error(Ex, _Ctx), Expected) :-
	format(user_error,
	       '~NWrong exception: ~p (expected ~p)~n', [Ex, Expected]),
	fail.

error_context(error(_, context(Pred, _)), Pred) :- !.
error_context(error(_, context(Module:Pred, _)), Pred) :-
	hidden_module(Module), !.
error_context(Error, _Pred) :-
	format(user_error, 'Wrong error context: ~q~n', [Error]),
	fail.

hidden_module(user) :- !.
hidden_module(system) :- !.
hidden_module(M) :-
	sub_atom(M, 0, _, _, $).
