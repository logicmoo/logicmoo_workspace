 
/*  DCTG.PL  */

/*  Shelved on 21st August 1990.  */


/*
This file defines a DCTG translator as described in ...

It makes a few changes ... operator prec ... from

It also defines the predicate dctg_reconsult, for loading
grammars from file. The specification follows. */


/*  dctg_reconsult( File+ ):                     
        File: a filename (atom).
    Loads the DCTG grammar in File, converting it to internal
    form useable by the parser.

    dctg_reconsult expects the following terms in its file:
        ?- (X)                  Call X, and go on to the next term.
        :- (X)                  Call X, and go on to the next term.
        X::=Y                   Translate as a DCTG clause, and assert
                                the corresponding Prolog.
        X:-Y                    Assert.
        Any other term X.       Assert.

    dctg_reconsult acts like ordinary reconsult in its dealings with
    repeated clauses. If it meets a clause C with functor and arity
    F,A, then it will always assert the clause. It will delete
    all other clauses for the same functor and arity, provided that
    they did not immediately precede the current one C.

    This applies both to ordinary Prolog clauses, and to DCTG clauses.
*/
:- op( 100, yfx, ^^ ).
:- op( 254, xfx, ::= ).
:- op( 255, xfx, <:> ).
:- op( 255, xfx, && ).
:- op( 254, xfx, ::- ).
:- op( 253, xfx, from ).


translate_rule( (LP::=[]<:>Sem), H ) :-
    !,
    t_lp( LP, [], S, S, Sem, H ).

translate_rule( (LP::=[]), H ) :-
    !,
    t_lp( LP, [], S, S, [], H ).

translate_rule( (LP::=RP<:>Sem), (H:-B) ) :-
    !,
    t_rp( RP, [], StL, S, SR, B1 ),
    reverse( StL, RStL ),
    t_lp( LP, RStL, S, SR, Sem, H ),
    tidy( B1, B ).

translate_rule( (LP::=RP), (H:-B) ) :-
    translate_rule( (LP::=RP<:>[]), (H:-B) ).


t_lp( (LP,List), StL, S, SR, Sem, H ) :-
    append( List, SR, List2 ),
    makelist( Sem, Semantics ),
    add_extra_args( [node(LP,StL,Semantics),S,List2], LP, H ).

t_lp( LP, StL, S, SR, Sem, H ) :-
    makelist( Sem, Semantics ),
    add_extra_args( [node(LP,StL,Semantics),S,SR], LP, H ).


t_rp( !, St, St, S, S, ! ) :- !.

t_rp( [], St, [ [] | St ], S, S1, S=S1 ) :- !.

t_rp( [X], St, [ [X] | St ], S, SR, c(S,X,SR) ) :- !.

t_rp( [X|R], St, [ [X|R] | St ], S, SR, (c(S,X,SR1),RB) ) :-
    !,
    t_rp( R, St, [ R | St ], SR1, SR, RB ).

t_rp( {T}, St, St, S, S, T ) :- !.

t_rp( (T,R), St, StR, S, SR, (Tt,Rt) ) :-
    !,
    t_rp( T, St, St1, S, SR1, Tt ),
    t_rp( R, St1, StR, SR1, SR, Rt ).

t_rp( T^^N, St, [N|St], S, SR, Tt ) :-
    add_extra_args( [N,S,SR], T, Tt ).

t_rp( T, St, [St1|St], S, SR, Tt ) :-
    add_extra_args( [St1,S,SR], T, Tt ).


add_extra_args( L, T, T1 ) :-
    T =.. Tl,
    append( Tl, L, Tl1 ),
    T1 =.. Tl1.


tidy( ((P1,P2),P3), Q ) :-
    tidy( (P1,(P2,P3)), Q ).

tidy( (P1,P2), (Q1,Q2) ) :-
    !,
    tidy( P1, Q1 ),
    tidy( P2, Q2 ).

tidy( A, A ).


c( [X|S], X, S ).


makelist( Sem, [Sem] ) :-
    var( Sem ),
    !.

makelist( (Sem1&&Sem2), [Sem1_|List] ) :-
    !,
    makelist_1( Sem1, Sem1_ ),
    makelist_1( Sem2, Sem2_ ),
    makelist( Sem2_, List ).

makelist( [], [] ) :- !.

makelist( Sem, [Sem] ).


makelist_1( (Attr from Var), Sem ) :-
    !,
    Attr_V =.. [ Attr, V ],
    Sem = (Attr_V ::- Var^^Attr_V).

makelist_1( Sem, Sem ).


node( _, _, Sem ) ^^ Args :-
    Sem ^^ Args.

[ (Args::-Traverse) | Rules ] ^^ Args :-
    Traverse.

[ Args | Rules ] ^^ Args.

[ _ | Rules ] ^^ Args :-
    Rules ^^ Args.


append( [], L, L ) :- !.

append( [X|R], L, [X|R1] ) :-
    append( R, L, R1 ).


reverse( X, RX ) :-
    rev1( X, [], RX ).


rev1( [], R, R ) :- !.

rev1( [X|Y], Z, R ) :-
    rev1( Y, [X|Z], R ).


dctg_reconsult( File ) :-
    seeing( CIS ),
    see( File ), seen,
    see( File ),
    dctg_reconsult_1( '$none' ),
    seen,
    see( CIS ).


dctg_reconsult_1( Previous ) :-
    read( Term ),
    (
        Term = end_of_file
    ->
        true
    ;
        process_term( Term, Previous, Next ),
        dctg_reconsult_1( Next )
    ).


process_term( ?-(Term), _, '$none' ) :-
    call( Term ) -> true ; true.

process_term( :-(Term), _, '$none' ) :-
    call( Term ) -> true ; true.

process_term( DCTG, Previous, Next  ) :-
    ( DCTG = (_ ::= _) ; DCTG = (_ <:> _) ),
    !,
    translate_rule( DCTG, Prolog ),
    process_term( Prolog, Previous, Next ).

process_term( :-(Head,Tail), Previous, clause(Functor,Arity) ) :-
    !,
    functor( Head, Functor, Arity ),
    (
        Previous \= clause(Functor,Arity)
    ->
        abolish1( Functor, Arity )
    ;
        true
    ),
    assert1( (Head:-Tail) ).

process_term( Head, Previous, Next ) :-
    process_term( (Head:-true), Previous, Next ).



abolish1( F,A) :-
    write(abolishing(F,A)), nl,
    abolish(F,A).

assert1(A) :-
    write(asserting(A)), nl,
    assert(A).
