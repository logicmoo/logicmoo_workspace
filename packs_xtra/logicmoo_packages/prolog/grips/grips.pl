/*  GRIPS.PL  */


/*
    This and its associated files was written by Jocelyn Paine, Department
    of Experimental Psychology, Oxford University in 1987, and modified
    in 1991.              
    Use them as you wish, provided that you retain this acknowledgement.
*/


/*
Introduction - exported predicates.
-----------------------------------

For an introduction to GRIPS, see the documentation. This file exports
the following predicates to GRIPSTOP.PL:
1. trans_guard      - translate a condition.
2. trans_expr       - translate an expression.
3. trans_directive  - translate a directive.
4. trans_def        - translate a definition.

These are used by the top-level interpreter and by the consult/reconsult
predicates defined in GRIPSTOP.PL.          
*/


/*
Utilities.
----------

This section defines several utility predicates:

1.  append and member, with the usual definitions.

2.  real(N) and numeric(N).
    real succeeeds iff its argument is a real (I have assumed Poplog
    here - this is NOT PORTABLE).
    numeric suceeds iff its argument is a number.
    Some systems may already supply these.

3.  fpbagof( Template, Goal, List ).
    Like the standard definition of bagof, but returns the empty
    list instead of failing when there are no solutions. The way I use
    fpbagof does not require the answer to be sorted.
    My definition assumes Poplog and is NOT PORTABLE.

    fpbagof is called by translated 'all X where G' expressions.

4.  foreach( Cond, Act ).
    Calls Act for each resatisfaction of Cond.

    foreach is called by translated 'foreach' commands.

5.  conjoin( G1, G2, G1G2 ).
    conjoin( G1, G2, G3, G1G2G3 ).
    conjoin( G1, G2, G3, G4, G1G2G3G4 ).
    These take two (or three...) Prolog goals and conjoin them. If one of
    the goals is 'true' or 'fail', the conjunction is optimised accordingly.
*/


append( [X|L1], L2, [X|L3] ) :-
    append( L1, L2, L3 ).
append( [], L, L ).


member( X, [X|_] ).
member( X, [_|T] ) :-
    member( X, T ).


real( X ) :-
    prolog_eval( dataword(quote(X)), D ),
    ( D = ddecimal ; D = decimal ), !.


numeric( X ) :-
    integer( X ).
numeric( X ) :-
    real( X ).


fpbagof( A, B, C ) :-
    fast_bagof( A, B, C ), !.
fpbagof( A, B, [] ) :- !.


foreach( Cond, Act ) :-
    call( Cond ), not( (Act;true) ).
foreach( _, _ ).


conjoin( true, G, G ) :- !.
conjoin( G, true, G ) :- !.
conjoin( fail, _, fail ) :- !.
conjoin( G1, G2, (G1,G2) ) :- !.


conjoin( G1, G2, G3, G1G2G3 ) :-
    conjoin( G2, G3, G2G3 ),
    conjoin( G1, G2G3, G1G2G3 ).


conjoin( G1, G2, G3, G4, G1G2G3G4 ) :-
    conjoin( G2, G3, G4, G2G3G4 ),
    conjoin( G1, G2G3G4, G1G2G3G4 ).


/*
Operator definitions.
---------------------

By choosing suitable precedence and associativities, we can find
operators to give us all the syntactic constructs we want.
The operator definitions are those for PDP-11 Prolog; YOU WILL HAVE TO
CHANGE THEM IF YOU WANT DEC-10 OPERATORS OR IF YOU WANT DIFFERENT
ARITHMETIC OPERATORS (** was added because Poplog Prolog lacks an
exponentiation operator: it's a functor known to 'is' but not an operator).

The predicate 'check_syntax' is called when translating terms, to ensure
that operators are combined correctly. It is NOT PORTABLE: it assumes
the existence of a system predicate 'prolog_error' which reports
user-defined errors as though they were system-detected errors. In
prolog_error(M,C), M is the error message (an atom), and C is a list of
"culprits": the particular things which caused the error. 'prolog_error'
writes the string M followed by 'culprits were' and C, and then aborts to
the top-level.        
*/


:- op( 256, fx, [ pr, test, do ] ).
:- op( 100, fx, q ).
:- op( 256, xfx, <- ).
:- op( 254, xfx, [ if, foreach, => ] ).
:- op( 254, yfx, else ).
:- op( 256, xfx, [ does ] ).
:- op( 252, xfy, and ).
:- op( 254, xfy, or ).
:- op( 5, xfy, where ).
:- op( 5, fx, all ).
:- op( 255, fx, grips ).

:- op( 31, yfx ,++ ).
:- op( 11, yfx, ** ).
:- op( 21, yfx, rem ).


check_syntax( Goal, String, Culprits ) :-
    call( Goal ), !.
check_syntax( Goal, String, Culprits ) :-
    prolog_error( String, Culprits ).


/*
Translating definitions.
------------------------

Like Prolog, GRIPS definitions come as Prolog terms, usually read from a
file in standard Prolog syntax (augmented by the operator definitions
above) and terminated by a dot. Unlike Prolog, there may be several
different kinds of term: function definitions, predicate definitions,
command definitions. These are described in the GRIPS documentation.

GRIPS translates each definition into a Prolog clause and asserts that
clause. To do so, it calls one main predicate, trans_def( GD+, Clause- ).

Here, GD is a GRIPS definition. 'trans_def' translates it into a Prolog
clause in Clause, which can then be asserted. 'trans_def' fails if its
first argument is not a GRIPS definition (note that this means it FAILS
if given H:-T as a first argument).

The idea behind translating functions is given in the documentation.
Briefly, a definition of the form
    f(A,B) <- g(C) if D.
is translated into the clause
    f(A,B,V) :- D', !, g(C',V).
where V is a new variable introduced to carry the result. Guards such
as D are translated (into D') to unwind function evaluations in the
arguments to the predicates they call. Function arguments such as C are
also unwound. This may involve introducing auxiliary goals to evaluate
functions called inside these arguments.


Implementation.
---------------

'trans_def' calls trans_guard or trans_expr to translate the body. To
translate the head of function definitions, it calls
    trans_head( Head_G, V, Head ).

The first argument is a head, either an atom or a term of the form
    H(A1,...An).
The third argument will become a term with one more argument than Head_G,
where the final argument is V:
    H(A1,...An,V)

If the first argument is an atom H, then the third will become
    H(V).
*/


trans_def( (Head_G <- Var_G), Head_P ) :-
    var( Var_G ),
    !,
    trans_head( Head_G, Var_G, Head_P ).

trans_def( (Head_G <- Expr_G) , (Head_P:-Tail_P) ) :-
    !,
    trans_expr( Expr_G, ResultVar, Tail_P ),
    trans_head( Head_G, ResultVar, Head_P ).

trans_def( (Head_G does Var_G), (Head_G :- Var_G) ) :-
    var( Var_G ),
    !.

trans_def( (Head_G does Command_G) , (Head_G:-Tail_P) ) :-
    !,
    trans_guard( Command_G, Tail_P ).

trans_def( (Head_G if Var_G), (Head_G :- Var_G) ) :-
    var( Var_G ),
    !.

trans_def( (Head_G if Guard_G), (Head_G:-Tail_P) ) :-
    !,
    trans_guard( Guard_G, Tail_P ).


trans_head( Head_G, ResultVar, Head_P ) :-
    Head_G =.. [ F | Args_G ],
    append( Args_G, [ResultVar], Args_P ),
    Head_P =.. [ F | Args_P ].


/*
Translating directives.
-----------------------

A GRIPS source file may contain several kinds of directive. These are
described in the documentation section on "Immediate mode evaluation".
They are translated by the main predicate 'trans_directive'.

The first argument of trans_directive( D, G ) is the directive. The
second argument will be bound to the corresponding Prolog goal.
'trans_directive' will fail if D is not a Prolog or GRIPS directive.
*/


trans_directive( (?-Goal_P), Goal_P ) :- !.

trans_directive( (:-Goal_P), Goal_P ) :- !.

trans_directive( grips(Expr_G), Goal_P ) :-
    !,
    trans_expr( Expr_G, _, Goal_P ).

trans_directive( do(Guard_G), Goal_P ) :-
    !,
    trans_guard( Guard_G, Goal_P ).

trans_directive( test(Guard_G), Goal_P ) :-
    !,
    trans_guard( Guard_G, Goal_P ).


/*
Translating guards.
-------------------

The main predicate for this is 'trans_guard( Gd, G )'. Here, Gd is a
GRIPS guard and G is its translation as a Prolog goal. The difference
between guards and goals is that arguments in guards may contain
function calls. 'trans_guard' translates these into auxiliary goals and
conjoins these goals with those in the original guard.


Implementation.
---------------

There are two tricky points. The first is the treatment of
    V = Expr
If V is unbound at translation-time, we assume that it is to be assigned
the result of Expr. This being so, we generate code that makes Var share
directly with the variable holding the result of Expr. This is an example
of what I call 'variable deletion' (see under trans_expr).      

The other is the translation of system predicates. Consider the guards
    assert( t(u) )
    not( found(F) )
    phrase( sentence, L )
    1 + 2 > A

We know that
    (a) The user probably doesn't want t(u) treated as an evaluable
        expression: it's a term to be asserted as it stands.
    (b) The same is true of the first argument of 'phrase'. However, the
        second is presumably a list expression which is to be
        evaluated.
    (c) The argument to 'not' is to be treated as a goal.
    (d) Prolog will evaluate the arguments to >, as long as they're
        the same kind of term as that which 'is' can evaluate.
To deal with these special cases, we have a table of special predicates
and their argument classes. If trans_guard detects such a predicate
(by 'is_special_guard') then it calls 'trans_special_guard' to translate
the whole guard. 'trans_special_guard' runs over each argument, looking
up its class and translating it accordingly.
*/


trans_guard( Var, Var ) :-
    var( Var ), !.

trans_guard( nothing, true ) :- !.

trans_guard( pr(Goal_P), Goal_P ) :- !.

trans_guard( test(Guard_G), Goal_P ) :-
    !,
    trans_guard( Guard_G, Goal_P ).

trans_guard( do(Guard_G), Goal_P ) :-
    !,
    trans_guard( Guard_G, Goal_P ).

trans_guard( (Guard_G=>Acts_G), Goal_P ) :-
    !,
    trans_condguard( (Guard_G=>Acts_G), Goal_P ).

trans_guard( (Acts_G if Guard_G), Goal_P ) :-
    !,
    trans_condguard( (Acts_G if Guard_G), Goal_P ).

trans_guard( else(If_G,Out_G), Goal_P ) :-
    !,
    trans_condguard( else(If_G,Out_G), Goal_P ).

trans_guard( (Act_G foreach Guard_G), foreach(Guard_P,Act_P) ) :-
    !,
    trans_guard( Guard_G, Guard_P ),
    trans_guard( Act_G, Act_P ).

trans_guard( (Guard1_G or Guard2_G), (Guard1_P;Guard2_P) ) :-
    !,
    trans_guard( Guard1_G, Guard1_P ),
    trans_guard( Guard2_G, Guard2_P ).

trans_guard( (Guard1_G and Guard2_G), Goal_P ) :-
    !,
    trans_guard( Guard1_G, Guard1_P ),
    trans_guard( Guard2_G, Guard2_P ),
    conjoin( Guard1_P, Guard2_P, Goal_P ).

trans_guard( Var=Expr_G, ExprGoal_P ) :-
    var( Var ),
    nonvar( Expr_G ),
    !,
    trans_expr( Expr_G, Var, ExprGoal_P ).

trans_guard( Expr_G=Var, ExprGoal_P ) :-
    var( Var ),
    nonvar( Expr_G ),
    !,
    trans_expr( Expr_G, Var, ExprGoal_P ).

trans_guard( Guard_G, Goal_P ) :-
    is_special_guard( Guard_G ),
    !,
    trans_special_guard( Guard_G, Goal_P ).

trans_guard( Guard_G, Goal_P ) :-
    /*  Default case - translate all the arguments, treating them
        as expressions. PreGoals_P will hold any goals which are
        necessary to evaluate these arguments. Conjoin these goals
        with MainGoal_P, which is the goal corresponding to
        Guard_G itself, and return that as the result.
    */                    
    Guard_G =.. [ F | GuardsArgs_G ],
    trans_arglist( GuardsArgs_G, GuardsArgs_P, PreGoals_P ),
    MainGoal_P =.. [ F | GuardsArgs_P ],
    conjoin( PreGoals_P, MainGoal_P, Goal_P ).


trans_special_guard( Guard_G, Goal_P ) :-
    functor( Guard_G, F, Arity ),
    functor( Template, F, Arity ),
    is_special_predicate( Template ),
    /*  Template now contains the argument-class template for the predicate
        that is Guard_G's principal functor.
    */

    functor( MainGoal_P, F, Arity ),
    /*  MainGoal_P is now a structure with the same functor and arity as
        Guard_G, but with all the arguments uninstantiated.
        'trans_call_by_template' will instantiate them.
    */
    trans_call_by_template( Template, Arity, 1, Guard_G, PreGoals_P, MainGoal_P ),

    /*  PreGoals_P is now a (possibly empty) sequence of goals that evaluates
        the arguments in MainGoal_P. Conjoining it to MainGoal_P gives us
        our result.
    */
    conjoin( PreGoals_P, MainGoal_P, Goal_P ).


trans_call_by_template( Template, Arity, ArgNo, Guard_G, PreGoal_P, MainGoal_P ) :-
    ArgNo =< Arity,
    !,

    arg( ArgNo, Template, Form ),
    /*  Form is the argument class for argument ArgNo.  */

    arg( ArgNo, Guard_G, Arg_G ),
    /*  Arg_G is the corresponding argument itself.  */

    trans_arg( Form, Arg_G, ArgsGoal_P, Arg_P ),
    /*  Now ArgsGoal_P is a (possibly empty) goal to evaluate variable(s)
        occuring in Arg_P.
    */

    arg( ArgNo, MainGoal_P, Arg_P ),
    /*  Make Arg_P the corresponding argument of the final goal.  */

    NextArgNo is ArgNo + 1,
    trans_call_by_template( Template, Arity, NextArgNo, Guard_G, RestArgGoals_P, MainGoal_P ),
    conjoin( ArgsGoal_P, RestArgGoals_P, PreGoal_P ).

trans_call_by_template( Template, Arity, ArgNo, Guard_G, true, MainGoal_P ).
    /*  If we've finished, just return a dummy pre-goal and leave the main
        goal alone.
    */


trans_arg( i, Arg_G, true, Arg_G ) :- !.

trans_arg( e, Arg_G, ArgGoal_P, Arg_P ) :-
    !,
    trans_expr( Arg_G, Arg_P, ArgGoal_P ).

trans_arg( g, Arg_G, true, Arg_P ) :-
    !,
    trans_guard( Arg_G, Arg_P ).

trans_arg( a, Arg_G, ArgGoal_P, Arg_P ) :-
    !,
    trans_arith( Arg_G, Arg_P, ArgGoal_P ).


/*
Translating expressions.
------------------------

The main predicate for this is 'trans_expr( Expr, V, Goal )'. Here, Expr
is the GRIPS expression. V will become a term which is the result of
evaluating Expr, once Goal has been called. Thus, once you've done
    call( Goal )
then V contains the result of evaluating Expr.


Implementation.
---------------

'trans_expr' may call 'trans_guard'. To translate lists, it calls
'trans_list'; to translate arithmetic expressions that can be evaluated
by "is" (such as 2+4), it calls 'trans_arith'; and to translate the
arguments of a general expression, it calls 'trans_arglist', which
recursively calls 'trans_expr'. To translate conditional expressions,
it calls 'trans_condexpr'.                 

The idea behind GRIPS is that each GRIPS expression is translated into
a goal/term pair <G;T>. The term T is the expression itself. If this
expression is not ground, then the goal G binds some or all of the variables
in T.

For example:
                Expression          Goal G          Term T
                ----------          ------          ------
                a                   null            a
                1                   null            1
                eval(a)             a(V)            V
                f(x)                f(x,V)          V
                1+2                 V is 1+2        V
                L1++L2              append(L1,L2,V) V
                q(f(x))             null            f(x)
In general, if T is fully ground, G should be null. If G is not null, then
T contains at least one variable (in this version of GRIPS, it will always
_be_ a variable).

'trans_expr(E,T,G)' performs this translation. G will become the goal. If
null, it's represented by 'true'. If non-null, it may be a conjunction
(or alternation) of sub-goals. T becomes the term.

Given this, the translation of function heads is easy. To translate
    f <- rhs
we translate rhs, giving a goal G and a term T. We then insert T as the
final argument of f, and give it the tail G:
    f(T) :- G
This inserts results directly into f's argument, so that for example
    f(N) <- 2
becomes
    f(N,2).

A point that's important in translating conditional expressions is that it's
possible to do what I call 'variable introduction'. The pair
    <G;T>
is equivalent to
    < (G,V=T) ; V >
where V is a new variable that does not occur in G or T. Usually, we don't
want unnecessary equalses. However, in translating branches of an if..else,
we have to make sure that all the result expressions are variables. This
is 'trans_nonfree_expr' does. You can see why if you think about translating
    f(N) <- 1 if p(X) else 2
You can't insert the 1 or 2 directly into f's head; instead, you must
introduce a common variable which can be set by either branch:
    f(N,V) :- ( p(X), !, V=1 ; V=2 ).

The converse of variable introduction is variable deletion, where we can
optimise out a subgoal of the form
    V=W
and replace following uses of V by W. This is done by trans_guard.
*/


trans_expr( Expr_G, Expr_G, true ) :-
    ( atomic( Expr_G ) ; var( Expr_G ) ),
    !.

trans_expr( q(Expr_G), Expr_G, true ) :- !.

trans_expr( eval(Atom_G), V, Goal_P ) :-
    ( atom( Atom_G ) ),
    !,
    Goal_P =.. [ Atom_G, V ].

trans_expr( eval(Expr_G), Expr_P, ExprGoal_P ) :-
    !,
    trans_expr( Expr_G, Expr_P, ExprGoal_P ).

trans_expr( test(Expr_G), true, ExprGoal_P ) :-
    !,
    trans_guard( Expr_G, ExprGoal_P ).

trans_expr( do(Expr_G), true, ExprGoal_P ) :-
    !,
    trans_guard( Expr_G, ExprGoal_P ).

trans_expr( pr(Expr_G), true, Expr_G ) :-
    !.

trans_expr( (Guard_G=>Acts_G), Expr_P, Goal_P ) :-
    !,
    trans_condexpr( (Guard_G=>Acts_G), Expr_P, Goal_P ).

trans_expr( (Acts_G if Guard_G), Expr_P, Goal_P ) :-
    !,
    trans_condexpr( (Acts_G if Guard_G), Expr_P, Goal_P ).

trans_expr( else(If_G,Out_G), Expr_P, Goal_P ) :-
    !,
    trans_condexpr( else(If_G,Out_G), Expr_P, Goal_P ).

trans_expr( all Form where Guard_G, V, fpbagof(Form,Guard_P,V) ) :-
    !,
    trans_guard( Guard_G, Guard_P ).

trans_expr( [H|T], Expr_P, ExprGoal_P ) :-
    trans_list( [H|T], Expr_P, ExprGoal_P ), !.

trans_expr( Expr_G, V, ExprGoal_P ) :-
    is_arithmetic_call( Expr_G ),
    !,
    trans_arith( Expr_G, ExprsIsPart_P, ExprPreGoal_P ),
    conjoin( ExprPreGoal_P, V is ExprsIsPart_P, ExprGoal_P ).

trans_expr( Expr_G where Guard_G, Expr_P, ExprGoal_P ) :-
    !,
    trans_guard( Guard_G, Guard_P ),
    trans_expr( Expr_G, Expr_P, ExprMainGoal_P ),
    conjoin( Guard_P, ExprMainGoal_P, ExprGoal_P ).

trans_expr( L1_G++L2_G, L12_P, L12Goal_P ) :-
    !,
    trans_expr( L1_G, L1_P, L1Goal_P ),
    trans_expr( L2_G, L2_P, L2Goal_P ),
    conjoin( L1Goal_P, L2Goal_P, append(L1_P,L2_P,L12_P), L12Goal_P ).

trans_expr( Expr_G, Expr_P, ExprGoal_P ) :-
    /*  This is the default case. We assume that Expr_G is to be taken
        as a function call. To turn this call into a Prolog goal, we
        do two things. (1) translate the arguments, generating code
        for pre-evaluating them if necessary. (2) Append a variable to
        the final argument list: that will hold the result of the
        call.
    */
    Expr_G =.. [ F | Args_G ],
    trans_arglist( Args_G, Args_P, ArgsGoal_P ),
    append( Args_P, [Expr_P], ArgsAndResult_P ),
    ExprMainGoal_P =.. [ F | ArgsAndResult_P ],
    conjoin( ArgsGoal_P, ExprMainGoal_P, ExprGoal_P ).


trans_list( Var, Var, true ) :-
    var( Var ),
    !.

trans_list( [], [], true ) :- !.

trans_list( [H_G|T_G], [H_P|T_P], Goal_P ) :-
    !,
    trans_expr( H_G, H_P, HGoal_P ),
    (
        trans_list( T_G, T_P, TGoal_P )
    ->
        true
    ;
        trans_expr( T_G, T_P, TGoal_P )
    ),
    conjoin( HGoal_P, TGoal_P, Goal_P ).
    /*  This conditional takes care of lists such as
            [ H | f(T) ]
        where the tail is not explicitly a list, and hence
        must be handled by trans_expr.
    */


trans_arglist( [A1_G | An_G ], [ A1_P | An_P ], Goal_P ) :-
    trans_arglist( An_G, An_P, AnGoal_P ),
    trans_expr( A1_G, A1_P, A1Goal_P ),
    conjoin( A1Goal_P, AnGoal_P, Goal_P ).

trans_arglist( [], [], true ).


/*
Expressions in 'is'.
--------------------

If an expression is something like X+Y or sin(2*pi), i.e. something that
has the correct arity and functor to be evaluated by 'is', then it's
translated by 'trans_arith( Expr, IsPart, Goal )'. Here, Expr is the
arithmetic expression. IsPart becomes a term which can be evaluated
by doing
    V is IsPart
and Goal becomes a goal which pre-evaluates any variables in IsPart.
So to evaluate Expr, you do
    call( Goal ), V is IsPart
which puts the result into V.

Essentially, given an arithmetic expression, 'trans_arith' does NOT
unwind its arguments if they themselves are arithmetic expressions. In
this, it differs from 'trans_expr'. Given
    2 + 3*5
'trans_expr' would (if it didn't call 'trans_arith' to handle such
cases) generate calls of the form
    V is 3*5, V' is 2+V
    result variable = V'

However, 'trans_arith' would optimise.
*/


trans_arith( Expr_G, Expr_G, true ) :-
    ( numeric(Expr_G) ; var(Expr_G) ), !.

trans_arith( Expr_G, Expr_P, ArgsGoal_P ) :-
    is_arithmetic_call( Expr_G ),
    Expr_G =.. [ F | Args_G ],
    trans_arith_args( Args_G, Args_P, ArgsGoal_P ),
    Expr_P =.. [ F | Args_P ], !.

trans_arith( Expr_G, Expr_P, ExprGoal_P ) :-
    trans_expr( Expr_G, Expr_P, ExprGoal_P ).


trans_arith_args( [], [], true ) :- !.

trans_arith_args( [A1_G|An_G], [A1_P|An_P], ArgsGoal_P ) :-
    trans_arith( A1_G, A1_P, A1Goal_P ),
    trans_arith_args( An_G, An_P, AnGoal_P ),
    conjoin( A1Goal_P, AnGoal_P, ArgsGoal_P ).


/*
Translating conditional expressions.
------------------------------------

These expressions are translated in two stages. The first removes the
operators defining the surface syntax and builds a list of the form
    [ Cond1, Expr1, Cond2, Expr2 ... ]
where the final condition may be 'true' if the expression finishes with
a default result. This is done by ifelses_to_list.

The second stage translates this list to a goal/term pair. As discussed
under "Translating expressions", if the list has more than one alternative,
we need to make all the alternative results assigned to a common variable.
This is done by 'trans_condexprlist_1'.

There are analogues for translating conditional guards.
*/


ifelses_to_list( else(E1,E2), In, Out ) :-
    !,
    check_syntax( (E1=if(_,_);E1=(_=>_);E1=else(_,_)), 'No IF before ELSE', [] ),
    ifelses_to_list( E2, In, In1 ),
    ifelses_to_list( E1, In1, Out ).

ifelses_to_list( if(Then,Cond), In, [ Cond, Then | In ] ) :-
    !.

ifelses_to_list( =>(Cond,Then), In, [ Cond, Then | In ] ) :-
    !.

ifelses_to_list( Default, In, [ true, Default | In ] ) :-
    !.


trans_condexpr( Expr_G, Expr_P, ExprGoal_P ) :-
    ifelses_to_list( Expr_G, [], List ),
    trans_condexprlist( List, Expr_P, ExprGoal_P ).


trans_condexprlist( [true,Expr_G|_], Expr_P, ExprGoal_P ) :-
    !,
    trans_expr( Expr_G, Expr_P, ExprGoal_P ).

trans_condexprlist( [Guard_G,Expr_G], Expr_P, ExprGoal_P ) :-
    !,
    trans_guard( Guard_G, Guard_P ),
    trans_expr( Expr_G, Expr_P, ExprMainGoal_P ),
    conjoin( Guard_P, !, ExprMainGoal_P, ExprGoal_P ).

trans_condexprlist( L, V, Goal ) :-
    !,
    trans_condexprlist_1( L, V, Goal ).


trans_condexprlist_1( [true,Expr_G|_], V, ExprGoal_P ) :-
    !,
    trans_nonfree_expr( Expr_G, V, ExprGoal_P ).

trans_condexprlist_1( [Guard_G,Expr_G], V, ExprGoal_P ) :-
    !,
    trans_guard( Guard_G, Guard_P ),
    trans_nonfree_expr( Expr_G, V, ExprMainGoal_P ),
    conjoin( Guard_P, !, ExprMainGoal_P, ExprGoal_P ).

trans_condexprlist_1( [Guard_G,Expr_G|Rest], V, Goal_P ) :-
    !,
    trans_guard( Guard_G, Guard_P ),
    trans_nonfree_expr( Expr_G, V, ExprMainGoal_P ),
    conjoin( Guard_P, !, ExprMainGoal_P, ExprGoal_P ),
    trans_condexprlist_1( Rest, V, RestGoal_P ),
    Goal_P = ( ExprGoal_P ; RestGoal_P ).


trans_nonfree_expr( Expr_G, ResultVar, ExprGoal_P ) :-
    trans_expr( Expr_G, Expr_P, ExprMainGoal_P ),
    (
        var(Expr_P)
    ->
        ResultVar = Expr_P,
        ExprGoal_P = ExprMainGoal_P
    ;
        conjoin( ExprMainGoal_P, ResultVar=Expr_P, ExprGoal_P )
    ).


trans_condguard( Guard_G, GuardGoal_P ) :-
    ifelses_to_list( Guard_G, [], List ),
    trans_condguardlist( List, GuardGoal_P ).


trans_condguardlist( [true,Act_G|_], ActGoal_P ) :-
    !,
    trans_guard( Act_G, ActGoal_P ).

trans_condguardlist( [Guard_G,Act_G], ActGoal_P ) :-
    !,
    trans_guard( Guard_G, Guard_P ),
    trans_guard( Act_G, ActMainGoal_P ),
    conjoin( Guard_P, !, ActMainGoal_P, ActGoal_P ).

trans_condguardlist( L, Goal ) :-
    !,
    trans_condguardlist_1( L, Goal ).


trans_condguardlist_1( [true,Act_G|_], ActGoal_P ) :-
    !,
    trans_guard( Act_G, ActGoal_P ).

trans_condguardlist_1( [Guard_G,Act_G], ActGoal_P ) :-
    !,
    trans_guard( Guard_G, GuardGoal_P ),
    trans_guard( Act_G, ActMainGoal_P ),
    conjoin( Guard_P, !, ActMainGoal_P, ActGoal_P ).

trans_condguardlist_1( [Guard_G,Act_G|Rest], Goal_P ) :-
    !,
    trans_guard( Guard_G, Guard_P ),
    trans_guard( Act_G, ActMainGoal_P ),
    conjoin( Guard_P, !, ActMainGoal_P, ActGoal_P ),
    trans_condguardlist_1( Rest, RestGoal_P ),
    Goal_P = ( ActGoal_P ; RestGoal_P ).                   


/*
Classifying predicates and functors.
------------------------------------

There are two classifications which GRIPS needs:

1. Predicates and their arguments.

    It needs to know, for built-in predicates, whether the arguments are
    to be left alone (as for assert), treated as goals (as for not),
    treated as expressions (as for the 2nd argument of phrase), or
    treated as arithmetic expressions (as for <). 'is_special_predicate'
    classifies accordingly. The meaning of the argument letters is:
        i = leave alone (Identity).
        g = goal.
        e = expression.
        a = arithmetic expression.
    The predicates listed are those we use in Poplog: YOU MAY NEED TO
    CHANGE THEM FOR YOUR SYSTEM.

2. Functors used by "is".

    When translating arithmetic expressions, GRIPS needs to know which
    functor/arity combinations are recognised by "is". This information
    is given by 'is_arithmetic_functor'. The functors listed are those
    recognised by Poplog: YOU WILL NEED TO CHANGE THEM FOR YOUR SYSTEM.

There are two other predicates built on these, to recognise terms
containing the special predicates or functors. 'is_special_guard'
succeeds if the argument is a term whose principal functor satisfies
'is_special_predicate'; 'is_arithmetic_call' succeeds if its argument is
a term whose principal functor satisfies 'is_arithmetic_functor'.
*/


is_special_guard( G ) :-
    functor( G, F, A ),
    functor( Copy, F, A ),
    is_special_predicate( Copy ).


is_special_predicate( assert(i) ).
is_special_predicate( asserta(i) ).
is_special_predicate( assertz(i) ).
is_special_predicate( retract(i) ).
is_special_predicate( retractall(i) ).

is_special_predicate( phrase(i,e) ).

is_special_predicate( call(g) ).
is_special_predicate( not(g) ).
is_special_predicate( ','(g,g) ).
is_special_predicate( ;(g,g) ).

is_special_predicate( a>a ).
is_special_predicate( a<a ).
is_special_predicate( a>=a ).
is_special_predicate( a=<a ).
is_special_predicate( a=:=a ).
is_special_predicate( a=\=a ).


is_arithmetic_call( E ) :-
    functor( E, F, A ),
    is_arithmetic_functor( F, A ).


is_arithmetic_functor( +, 2 ).
is_arithmetic_functor( -, 2 ).
is_arithmetic_functor( -, 1 ).
is_arithmetic_functor( *, 2 ).
is_arithmetic_functor( /, 2 ).
is_arithmetic_functor( **, 2 ).
is_arithmetic_functor( div, 2 ).
is_arithmetic_functor( mod, 2 ).
is_arithmetic_functor( rem, 2 ).
is_arithmetic_functor( abs, 1 ).
is_arithmetic_functor( arccos, 1 ).
is_arithmetic_functor( arctan, 1 ).
is_arithmetic_functor( arctan2, 2 ).
is_arithmetic_functor( cos, 1 ).
is_arithmetic_functor( exp, 1 ).
is_arithmetic_functor( fracof, 1 ).
is_arithmetic_functor( intof, 1 ).
is_arithmetic_functor( log, 1 ).
is_arithmetic_functor( log10, 1 ).
is_arithmetic_functor( max, 2 ).
is_arithmetic_functor( min, 2 ).
is_arithmetic_functor( random, 1 ).
is_arithmetic_functor( round, 1 ).
is_arithmetic_functor( sin, 1 ).
is_arithmetic_functor( sqrt, 1 ).
is_arithmetic_functor( tan, 1 ).
