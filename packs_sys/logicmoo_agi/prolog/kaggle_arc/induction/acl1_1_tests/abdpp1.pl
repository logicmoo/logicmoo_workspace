/* abdpp.pl, Abductive proof procedure, version for the learning system ACL1 */

verapp('1').

/* Characteristics:

        Depth bound on derivation: a limit is placed on the number of abductive and
                consistency derivations

        Assumption on the body of rules: the rules and integrity constraints contain first
                non abducibles and then abducibles in their bodies.
                In this way non abducibles are resolved before abucibles in the abductive
                derivation, so that they may provide all possible instantiations for
                non ground abducibles.

        Negation: negation for abducible and non abducible predicates is automatically
                treated by means of abduction.
                The negation of abducible and non abducible predicates is considered
                as abducible and a constraint <-a(X),not(a(X)) is imposed.
                This is done implicitely, so the negation of predicates does not have
                to be included in the list of abducible predicates
                nor the integrity constraints <-a(X),not(a(X)) have to be inlcuded
                in the program. However, if a negative literal for a non abducible
                predicate succeeds, it is abduced, i.e. it is not included in the delta set.

        Extensional derivation: the procedure performs an hybrid extensional/intensional
                derivation. The current goal is first checked against the current training
                set that is stored by means of the two facts
                current_eplus(Ep) and current_eminus(Em).
                In the abductive derication, if a positive literal is among the current set
                of positive examples, it is reduced, if it is among the current set of negative
                examples, the abductive derivation fails. Viceversa if the literal is negative.
                In the consistency derivation if a positive literal is among the current set
                of positive examples, it is reduced, if it is among the current set of negative
                examples, the constraint is discarded. Viceversa if the literal is negative.

        Abducibles may have a partial definition in the program:
                in the abductive derivation, before trying to abduce the literal,
                it is checked whether it can be derived by means of one of the clauses
                for it.
                In the consistency derivation, when an abducible has a partial definition,
                the set of resolvents of the abducible with the clauses of the program is
                generated and added to the set of constraints. If all the resolvents fail,
                then the negation of all the instantiations of the literal in each resolvent
                are added to the Delta set.

        Literals abduced for previous examples:
                the literals abduced for other examples are stored with the predicate
                already_abduced(AlreadyAbduced).
                where AlreadyAbduced has the form
                [[ex1,abd1,..,abdn],[ex2,...]..]
                The procedure checks if an abducible has already been abduced for
                another example, if so it reduces the goal and adds it to the Delta set
                for the current example.

        Non ground abduction:
                if a non ground abducible is encountered in the abductive phase it
                is first checked if it can be grounded by unifying it with a clause in the
                background, with an example or with a previously abduced literal.
                If this is not possible, then the abductive derivation fails, no non ground
                abduction is performed in the abductive derivation.

                In the consistency phase the abduction of non ground abducibles is allowed
                in a restricted case by grounding the abducible with a skolem constant
                For example: consider the clause
                father(X,Y):-parent(X,Z).
                and the goal: not(father(jeroen,andre))
                Suppose there is no parent(jeroen,cost) in the background, then
                we abduce not(parent(jeroen,skolem))


        Inconsistency checking:
                literals that must be true according to integrity constraints are
                not abduced:
                for example, if we have the abducible goal male(a)
                and we have in the background
                        female(a)
                        <-male(X),female(X)
                then male(a) is surely true according to the background and the constraints.
                Therefore it is not considered an assumption and added to the delta set.
                The inconsistency predicate checks whether a literal is inconsistent in the
                above sense by checking that there is at least one constraint in which the
                literal appears that fails. No new assumption can be created by the inconsistency
                checking.


        Selection rule in integrity constraints:
                non abducibles are reduced first, without choice point
                then abducibles are reduced, keeping a choice point,
                so that all possible orders of reducing the abducibles are tried.
                In this way all the minimal models are found.
*/

:-dynamic not/1.

:-use_module(library(lists)).

abducible(A):-
        abducibles(AbdSet),
        memberchk(A,AbdSet),!.


/* Abductive derivation:
solve_abd(Goal,AbdIn,AbdOut,Depth) */

/* normal resolution steps*/
solve_abd([],Abd,Abd,_Depth):-!.

solve_abd([true|R],AbdIn,AbdOut,Depth):-!,
        solve_abd(R,AbdIn,AbdOut,Depth).

/* depth limit reached */
solve_abd(Goal,_,_,0):-!,
        format("Depth limit reached, goal ~p~N",[Goal]),
        fail.

/* Extensional derivation steps: */
/* fact in the training set */
solve_abd([not(A)|B],AbdIn,AbdOut,Depth):-
        current_eminus(Em),
        member(A,Em),
        solve_abd(B,AbdIn,AbdOut,Depth).

solve_abd([A|B],AbdIn,AbdOut,Depth):-
        current_eplus(Ep),
        member(A,Ep),
        solve_abd(B,AbdIn,AbdOut,Depth).

/* opposite of the fact in the training set: failure*/
solve_abd([not(A)|_B],_AbdIn,_AbdOut,_Depth):-
        current_eplus(Ep),
        member(A,Ep),!,
        fail.

solve_abd([A|_B],_AbdIn,_AbdOut,_Depth):-
        current_eminus(Em),
        member(A,Em),!,
        fail.
/* End of extensional derivation steps */

/* resolution step */
solve_abd([A|B],AbdIn,AbdOut,Depth):-
        resolution_step([A|B],AbdIn,AbdOut,Depth).

resolution_step([A|B],AbdIn,AbdOut,Depth):-
        clause(A,BodyAnd),
        and2list(BodyAnd,Body),
        Depth1 is Depth - 1,
        insert_nabd_1st(Body,B,B1),
        solve_abd(B1,AbdIn,AbdOut,Depth1).

/* NAF goal: not(A) where A is not abd */
solve_abd([not(A)|B],AbdIn,AbdOut,Depth):-
        \+(abducible(A)),!,
        solve_con2([[A]],[not(A)|AbdIn],AbdOut1,Depth),
        delete(AbdOut1,not(A),AbdOut2),
        solve_abd(B,AbdOut2,AbdOut,Depth).

/* end of normal resolution steps */

/* (1) A  has already been abduced */
solve_abd([A|B],AbdIn,AbdOut,Depth):-
        memberchk(A,AbdIn),!,
        solve_abd(B,AbdIn,AbdOut,Depth).

/* A has been abduced for other examples */
solve_abd([A|B],AbdIn,AbdOut,Depth):-
        already_abduced(AA),
        ext_memberchk(A,AA),!,
        solve_abd(B,[A|AbdIn],AbdOut,Depth).

ext_memberchk(A,[[_Ex|AbdList]|_RestEx]):-
        memberchk(A,AbdList),!.

ext_memberchk(A,[[_Ex|_AbdList]|RestEx]):-
        ext_memberchk(A,RestEx).

/* (2) A is abducible but neither A nor not(A) have already been abduced */
solve_abd([not(A)|B],AbdIn,AbdOut,Depth):-
        abducible(A),!,
        ground(A),
        \+(memberchk(A,AbdIn)),
        \+(resolution_step([not(A)|B],AbdIn,AbdOut,Depth)),
        % no resolution step is possible for not(A), flounders on AbdOut
        % but in this case is what we want
        solve_con(not(A),[not(A)|AbdIn],AbdOut1,Depth),
        solve_abd(B,AbdOut1,AbdOut,Depth).


solve_abd([A|B],AbdIn,AbdOut,Depth):-
        abducible(A),!,
        ground(A),
        \+(memberchk(not(A),AbdIn)),
        \+(resolution_step([A|B],AbdIn,AbdOut,Depth)),
        % no resolution step is possible for A
        solve_con(A,[A|AbdIn],AbdOut1,Depth),
        solve_abd(B,AbdOut1,AbdOut,Depth).

/* A not a list */
solve_abd(A,AbdIn,AbdOut,Depth):-
        \+(is_list(A)),!,
        solve_abd([A],AbdIn,AbdOut,Depth).


/* Consistency derivation:
solve_con(Goal,AbdIn,AbdOut,Depth) */

solve_con(Goal,AbdIn,AbdOut,Depth):-
        get_all_ic(Goal,[First_ic | Rest_ics]),!,
                                /* V b: modified */
        select_literal(First_ic,First_ic1),
                                /* V b: added */
        solve_con2([First_ic1 | Rest_ics],AbdIn,AbdOut,Depth).
                                /* V b: modified */

/* if there are no ics, then Goal can be abducted */
solve_con(_Goal,Abd,Abd,_Depth):-!.



/* predicate that selects one literal from the ic and puts it in the head of ic
the selection is done only if the ic contains only abducibles */

select_literal([],[]):-!.

select_literal([Lit|RestIcIn],[L|IcOut]):-
        abducible(Lit),!,
        select(L,[Lit|RestIcIn],IcOut).

select_literal(Ic,Ic).

select_literal_in_1st_ic([],[]):-!.

select_literal_in_1st_ic([FirstIc|RestIc],[NewFirstIc|RestIc]):-
        select_literal(FirstIc,NewFirstIc).

get_all_ic(Goal,[[NegGoal] | List_of_ic]):-
        neg(Goal,NegGoal), % the ic <-a,not(a) is implicit and is added
                           % singularly to List_of_ic
        findall(Ic,get_ic(Goal,Ic),List_of_ic).

get_ic(Goal,Ic):-
        ic(X),memberchk(Goal,X),delete(X,Goal,Ic).

/* all the ic are satisfied */
solve_con2([],Abd,Abd,_):-!.

/* one of the Ic has been violated */
solve_con2([ [] |_],_,_,_):-!,fail.

/* depth limit reached */
solve_con2(Ics,_,_,0):-!,
        format("Depth limit reached, ics ~p~N",[Ics]),
        fail.

solve_con2([ [true|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-!,
        solve_con2([Rest_l|Rest_ic],AbdIn,AbdOut,Depth).

/* Extensional derivation steps: */
/* if L1 is in the training set, delete it */
solve_con2([ [not(L1)|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        current_eminus(Em),
        member(L1,Em),!,% in reality this prevents from finding other solutions
        select_literal(Rest_l,Rest_l1),
        solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth).

solve_con2([ [L1|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        current_eplus(Ep),
        member(L1,Ep),!,
        select_literal(Rest_l,Rest_l1),
        solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth).

/* if not(L1) is in the training set, then remove the ic */
solve_con2([ [L1|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        current_eminus(Em),
        member(L1,Em),!,
        select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,AbdIn,AbdOut,Depth).

solve_con2([ [not(L1)|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        current_eplus(Ep),
        member(L1,Ep),!,
        select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,AbdIn,AbdOut,Depth).
/* End of extensional derivation steps */

/* (3) first try to use resolution */
solve_con2([ [true|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-!,
        select_literal(Rest_l,Rest_l1),
        solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth).


solve_con2([ [L1|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        % some clauses are available for L1
        findall(NewIc,(
                clause(L1,BodyAnd),
                and2list(BodyAnd,Body),
                insert_nabd_1st(Body,Rest_l,NewIc)
                ),
                NewListIc),
        (NewListIc=[]->
                fail
        ;
                !
        ),
        appendc(NewListIc,Rest_ic,[ Ic | Rest_ic1]),
        select_literal(Ic,Ic1),
        Depth1 is Depth - 1,
        (abducible(L1)->
        % if L1 was an abducible with a partial definition:
        % add to the set of abducible the negation of all the literals
        % obtained resolving L1 with the partial definition because all resolvents
        % must fail
                findall(L1,(
                        clause(L1,_BodyAnd)
                        ),
                        ListL1),
                complement_list(ListL1,ListnotL1),
                append(ListnotL1,AbdIn,Abd1)
        ;
                Abd1=AbdIn
        ),
        solve_con2([Ic1 | Rest_ic1],Abd1,AbdOut,Depth1).

/* (4) L1 has already been abduced */
solve_con2([ [L1|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        memberchk(L1,AbdIn),!,
        select_literal(Rest_l,Rest_l1),
        solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth).

/* Check if it has been abduced for other examples */
solve_con2([ [L1|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        already_abduced(AA),
        ext_memberchk(L1,AA),!,
        select_literal(Rest_l,Rest_l1),
        solve_con2([Rest_l1|Rest_ic],[L1|AbdIn],AbdOut,Depth).

/* (5) the opposite of L1 has been abduced */
solve_con2([ [not(L1)| _Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        memberchk(L1,AbdIn),!,
        select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,AbdIn,AbdOut,Depth).

solve_con2([ [L1|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        memberchk(not(L1),AbdIn),!,
        select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,AbdIn,AbdOut,Depth).

/*  check if the opposite of L1 has been abduced for other examples */
solve_con2([ [not(L1)| _Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        already_abduced(AA),
        ext_memberchk(L1,AA),!,
        select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,[L1|AbdIn],AbdOut,Depth).

solve_con2([ [L1|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        already_abduced(AA),
        ext_memberchk(not(L1),AA),!,
        select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,[not(L1)|AbdIn],AbdOut,Depth).

/* (6) L1 is abducible but neither L1 nor not(L1) have already been abduced */

/* (6a) see if not(L1) is implied by the constraints, i.e. L1 violates at least
        one constraint: proof of inconsistency for not(L1)  */
solve_con2([ [L1|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        pred_of_lit(L1,P),
        abducible(P),
        inconsistency(L1,AbdIn,Depth),!,
        select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,AbdIn,AbdOut,Depth).

/* (6b) start an abductive derivation for not(L1) */
/* Negation as Failure literal for abducibles and non-abducibles
NAF of abd is treated in the same way as for non-abd.
Two valued interpretation of negation: if the opposite literal can not be
assumed, we consider the literal true, even if it can be unknown (if it
can not be abductively derived). It is a semplification.
 */
solve_con2([ [not(L1)|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-!,
        ground(L1),
        % if L1 is non ground, the literal can not be reduced
        % and we have to backtrack on literal selection. Otherwise, L1
        % may succeed with an instantiation even if the constraint is
        % violated.
        (solve_abd(L1,AbdIn,AbdOut1,Depth)->
        % if the derivation succeed, then drop the branch
                select_literal_in_1st_ic(Rest_ic,Rest_ic1),
                solve_con2(Rest_ic1,AbdOut1,AbdOut,Depth)
        ;
        % if the derivation fails, then drop the literal
                select_literal(Rest_l,Rest_l1),
                solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth)
        ).


/* L1 is abducible and ground, we try to abduce the negation, if it is possible
then we discard the constraint, otherwise we discard the literal */
solve_con2([ [L1|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        abducible(L1),ground(L1),!,
        (solve_abd(not(L1),AbdIn,AbdOut1,Depth)->
                select_literal_in_1st_ic(Rest_ic,Rest_ic1),
                solve_con2(Rest_ic1,AbdOut1,AbdOut,Depth)
        ;
                select_literal(Rest_l,Rest_l1),
                solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth)
        ).

/* L1 is abducible and not ground, we first skolemize it and then try to abduce the negation,
if it is possible then we discard the constraint, otherwise we discard the literal */
solve_con2([ [L1|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        abducible(L1),!,skolem(L1),
        (solve_abd(not(L1),AbdIn,AbdOut1,Depth)->
                select_literal_in_1st_ic(Rest_ic,Rest_ic1),
                solve_con2(Rest_ic1,AbdOut1,AbdOut,Depth)
        ;
                select_literal(Rest_l,Rest_l1),
                solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth)
        ).


/* (7) L1 is not abducible and has no successful rule: the constraint is discarded */
solve_con2([ [_L1|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,AbdIn,AbdOut,Depth).

inconsistency(Goal,Abd,Depth):-
        get_all_ic(Goal,ICs),!,
        at_least_one_violated(ICs,Abd,Depth).

at_least_one_violated([],_Abd,_Depth):-!,fail.

at_least_one_violated([Ic|_RestIc],Abd,Depth):-
        solve_nc(Ic,Abd,Depth),!.

at_least_one_violated([_Ic|RestIc],Abd,Depth):-
        at_least_one_violated(RestIc,Abd,Depth).

/* Abductive derivation without consistency
solve_nc(Goal,Abd,Depth) */

/* normal resolution steps */
solve_nc([],_Abd,_Depth):-!.

solve_nc([true|B],Abd,Depth):-!,
        solve_nc(B,Abd,Depth).

/* depth limit reached */
solve_nc(Goal,_,0):-!,
        format("Depth limit reached in inconsistency, goal ~p~N",[Goal]),
        fail.

solve_nc([A|B],Abd,Depth):-!,
        solve_nc(A,Abd,Depth),
        solve_nc(B,Abd,Depth).

solve_nc(A,Abd,Depth):-
        clause(A,BodyAnd),
        and2list(BodyAnd,Body),
        Depth1 is Depth - 1,
        solve_nc(Body,Abd,Depth1).
/* end of normal resolution steps */

/* (1) A has already been abduced */
solve_nc(A,Abd,_Depth):-
        abducible(A),
        memberchk(A,Abd),!.

/* insert_nabd_1st: insert non abducibles in the bocy first.
Assumption: the non abd already in the body are before the abd */
insert_nabd_1st([],Cl,Cl):-!.

insert_nabd_1st([Nonabd|RestBody],RestIc,[Nonabd|NewRestIc]):-
        \+(abducible(Nonabd)),!,
        insert_nabd_1st(RestBody,RestIc,NewRestIc).

/* when only abducible are left, they are added to the bottom of the ic */
insert_nabd_1st(RestBodyOfAbd,RestIc,NewIc):-
        appendc(RestIc,RestBodyOfAbd,NewIc).

appendc([],L,L):-!. % append redefined for optimization: ! added

appendc([H|T],L,[H|T1]):-
        appendc(T,L,T1).

skolem(not(Goal)):-!,
        Goal=..[_|Args],
        skolem1(Args).

skolem(Goal):-
        Goal=..[_|Args],
        skolem1(Args).

skolem1([]):-!.

skolem1([H|T]):-
        (var(H)->
                new_const(H)
        ;
                true
        ),
        skolem1(T).

new_const(H):-
        retract(new_const_number(N)),
        N1 is N + 1,
        assert(new_const_number(N1)),
        number_chars(N,Nstr),
        append("skolem",Nstr,Nconst),
        name(H,Nconst).

:-dynamic new_const_number/1.

new_const_number(0).


and2list(A,[A]):-
        \+(A=(_B,_C)).


and2list((A,R),[A|T]):-
        and2list(R,T).

neg(not(Goal),Goal):-!.

neg(Goal,not(Goal)).

pred_of_lit(not(P),P):-!.

pred_of_lit(P,P).

complement_list([],[]).

complement_list([L1|ListL1],[not(L1)|ListnotL1]):-
        complement_list(ListL1,ListnotL1).
