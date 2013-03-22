:- module(assrt_example, [test1/0, test2/0]).

% :- expects_dialect(ciao).

:- use_module(tools(assertions)).
:- use_module(tools(rtchecks)).
:- use_module(engine(basic_props)).

term_expansion(A, B) :-
    ciao:ciao_trans('rtchecks$ciao':sentence_trans, call_sentence_expansion, A, B).

goal_expansion(A, B) :-
    ciao:ciao_trans('rtchecks$ciao':goal_trans, call_goal_expansion, A, B).

:- pred maplist(A, B) : (callable(A), list(B)).

% maplist1(A, B) :-
%     display(maplist1(A,B)),
%     nl.

test1 :-
    B = [1,2,3|4],
    maplist(display,B).

test2 :-
    m1:expects_dialect(ciao),
    m1:use_module(rtchecks(rtchecks_tr)),
    m1:valid_commands(A),
    write(A),nl,
    absolute_file_name(rtchecks(rtchecks_tr), F),
    m2:expects_dialect(ciao),
    m2:use_module(rtchecks(rtchecks_tr)),
    m1:unload_file(F),
    m2:valid_commands(B),
    write(B),nl.
