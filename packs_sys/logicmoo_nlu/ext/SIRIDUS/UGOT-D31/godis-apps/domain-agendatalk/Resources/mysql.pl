/*=====================================================
Written by Gianluca Caruso, University of Pisa.
Modified by Maria Håkansson and Peo Grimheden and Rebecca Jonson
====================================================*/

:-module(mysql,[mysql/3]).

:- use_module(library(system)).
:- use_module(library(charsio)).
:- use_module(library(lists)).


parse_result(Stream,[]) :-
        at_end_of_stream(Stream),
        !.
parse_result(Stream,Tuples) :-
        tuples(Stream,Tuples).

tuples(Stream,[]) :-
        at_end_of_stream(Stream),
        !.
tuples(Stream,[Tuple|Tuples]) :-
        tuple(Stream,Tuple),
        tuples(Stream,Tuples).

tuple(Stream,[]) :-
        at_end_of_stream(Stream),
        !.
tuple(Stream,[]) :-
        at_end_of_line(Stream),
        skip_line(Stream),
        !.
tuple(Stream,[Field|Tuple]) :-
        field(Stream,Field),
        tuple(Stream,Tuple).

field(Stream,[]) :-
        at_end_of_line(Stream),
        !.
field(Stream,Chars) :-
        get0(Stream,X),
        % add chars to current field until a tab is reached
        ( X == 0'\t -> Chars = [];      
            ( field(Stream,Xs), Chars = [X|Xs] ) ).

/*==================================================
    mysql(+Expr, -Result, -ErrorResult)
    -- Takes a SQL-query returns the answer or []
    -- Returns error message if it occurs or []
===================================================*/
        
mysql(Expr,Result,ErrRes) :-
        mysql_com(Com),
        exec(Com,[pipe(In),pipe(Out),pipe(Err)],_),
        write(In,Expr),
        close(In),
        parse_result(Out,Result),
	parse_result(Err,ErrRes),
        close(Out).

/*========================================================
    mysql_com(Arg)
    -- -s = silent mood, -w = retry if connection is down,
    -u = user, password, name of database
==========================================================*/

mysql_com('mysql -s -w agendatalk').



















