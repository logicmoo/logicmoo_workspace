/* ACL1 algorithm for single predicate learning
                Version 1                        */
ver('1').

/* Version 1
Caracteristics:

        Top-down albgorithm with beam search

        Abductive proof procedure substitutes the prolog proof procedure

        Stopping criterion for the specialization loop: it stops when
                1) the first rule of the beam does not cover any negative ex
                2) a max number of specializing steps has been done

        No backtracking on clause addition

        optimization  of clause testing: the first resolution step
                is performed using only the current clause

        Hybrid extensional/intensional derivation: positive and negative examples
                are considered as facts that can be used in the derivation of examples
                for other predicates. All examples apart from the one we are
                        deriving are considered as facts in the derivation of the current
                example.

        Target predicates can be abducibles: assumptions about them are moved to the
                training set and become new training examples
*/

/*
Parameters:
        a number of parameters is defined by the facts at the beginning of the file.
        Set their value by changing the file and reloading it or by retract and asserts.

        verbosity(V): specifies the verbosity level.  Set by default to 3
        0: nothing is printed apart from the output rules
        1: every rule that is added to the theory
        2: every rule added to the agenda
        3: the agenda at each specializing step

        beamsize(Beamsize): specifies the size of the beam. Set by default to 5
        
        nmax(Nmax): maximum number of specializayion.  Set by default to 10

        der_depth(Depth): depth of derivations.  Set by default to 20

        kt(K):  threshold on K+ and K-.  Set by default to 0.1

	min_cov(MC): minimum number of examples that each clause must cover. 
		Set by default to 1

*/
:- dynamic max_spec_steps/1,der_depth/1,beamsize/1,verbosity/1,kt/1,min_cov/1.

max_spec_steps(10).
der_depth(20).
beamsize(5).
verbosity(3).
kt(0.1). 
min_cov(1).


/* Algorithm: (in parenthesis the name of the predicates)

induce: (i)
Theory:=0
Delta:=0
while E+!=0 do (covering_loop)
        initialize the agenda (initialize_agenda)
        generate one clause Cl (specialize)
        evaluate Cl in oder to find the examples E+(Cl) covered by Cl and the
                relative set of assumptions Delta(Cl)
        E+=E+ - E+(Cl)                                          \
        Add to E+ the pos lit of target pred in Delta(Cl)       |-(update)
        Add to E- the neg lit of target pred in Delta(Cl)       /
        Add Cl to the Theory
        Add Delta(Cl) to Delta
endwhile


specialize:
        pick the first clause in the agenda
                for each refinement (add_ref_to_agenda)
                        evaluate the refinement (evaluate)
                        if the refinement covers at least one pos ex
                        with or without abd
                                        orderly insert [Rule,Value] into Agenda
                                without exceeding Beamsize
                                (insert_in_order)
                endfor
        until the best rule in the agenda is consistent or the maximum number of
                specialization steps has been reached
        pick the first clause Cl in the agenda

Insertion of rules in the agenda:
        1) refinements with the same value of a rule already
                present are inserted after
        2) when 2 clauses have the same value for the heuristic, 
                the clause with the higher number of pos ex covered without abd
                is put first


*/

:-consult(abdpp1).
:-consult(icl).
:-consult(output).


i(File):-
        name(File,FileString),
        append(FileString,".bg",FileStringBg),
        name(FileBg,FileStringBg),
        [FileBg],
        title(File,user_output),
        retract_old_pred,
        statistics(runtime,[_,_]),
        i(Rules,Delta),
        statistics(runtime,[_,T]),
        append(FileString,".rules",FileOutString),
        name(FileOut,FileOutString),
        open(FileOut,write,Stream),
        title(File,Stream),
        title(File,user_output),
        T1 is T /1000,
        format(Stream,"Execution time ~f seconds. Generated rules~N",[T1]),
        format("Execution time ~f seconds. Generated rules~N",[T1]),
        print_list(Rules,Stream),
        print_list(Rules,user_output),
        close(Stream),
        call_ICL(FileString,Delta).


i(Rules,Delta):-
        eplus(Eplus),
        eminus(Eminus),
        print_number_neg(Eminus),
        covering_loop(Eplus,Eminus,[],Rules,[],Delta).

initialize_agenda(Agenda):-
        findall([H,V],bias(rule(H,_B),V,_Bias),L),
        init_ag(L,[],Agenda).

init_ag([],Agenda,Agenda):-!.

init_ag([[P,Var]|Rp],Agendain,[ [rule(P,[]),_V,_C,_Np,_Npa,_Nm,_Nma,1,1,Var]
                | Agendaout]):-!,
        init_ag(Rp,Agendain,Agendaout).


/* covering_loop(Eplus,Eminus,Rulesin,Rulesout,Deltain,Deltaout) */
/* no more positive examples to cover */
covering_loop([],_Eminus,Rules,Rules,Delta,Delta):-!.

/* some eplus still to cover: generate a new clause */
covering_loop(Eplus,Eminus,Rulesin,Rulesout,Deltain,Deltaout):-
        print_ex_rem(Eplus,Eminus),
        initialize_agenda(Agenda1),
        evaluate_new_agenda(Agenda1,[],Agenda,Eplus,Eminus,Deltain),
        specialize(Agenda,Agenda2,Eplus,
                Eminus,Deltain,0),
        Agenda2=[ [Cl,_V,C|_]|_Agenda3],
        evaluate(_Value,Cl,Eplus,Epluscovered,Eminus,Eminuscovered,Deltain,
                Delta1,0,0,Np,Nm,Npa,Nma,1,1,_Kp,_Km),
        deleteall(Delta1,Deltain,NewDelta),
        print_new_clause(Cl,C,Np,Npa,Nm,Nma,
                Epluscovered,Eminuscovered,NewDelta),
        asserta_rule(Cl),
        update(Eplus,Epluscovered,Eplus1,Eminus,Eminus1,Delta1),
        % the delta is not modified: assumption are ADDED to the tr. set
        % not moved
        !,
        covering_loop(Eplus1,Eminus1,
                [[Cl,Np,Npa,Nm,Nma,Epluscovered,Eminuscovered,NewDelta]
                |Rulesin],Rulesout,Delta1,
                        Deltaout).



/* specialize(Agenda,Clout,AllowedLiterals,Eplus,Eminus,
        Deltain) */

/* stopping criterion (1): when the 1st rule does not cover any negative
        example */
specialize([ [BestRule,Value,yes|RestRule] |RestAgenda],
        [ [BestRule,Value,yes|RestRule] |RestAgenda],
        _Eplus,_Eminus,_Delta,_N):-!,
        print_current_agenda([ [BestRule,Value,yes|RestRule] |RestAgenda]).

/* stopping criterion (2): reached the limit on the number of specializing
        steps */
specialize(Agenda,Agenda,
        _Eplus,_Eminus,_Delta,N):-
        max_spec_steps(Nmax),
        N=Nmax,
        print_max_spec_steps(Nmax),!.

specialize([ [BestRule,_Value,_End,Np,Npa,Nm,Nma,Kp,Km,Var] |Rest],Agendaout,
        Eplus,
        Eminus,Delta,N):-
        print_spec_step(N),
        print_current_agenda([ [BestRule,_Value,_End,Np,Npa,Nm,Nma,Kp,Km,Var]
                |Rest]),
        print_refinements,
        bias(BestRule,Var,AllowedLiterals),
        add_ref_to_agenda(BestRule,Np,Nm,Kp,Km,Var,AllowedLiterals,
                Eplus,Eminus,
                Delta,
                Rest,
                Agenda),
        N1 is N + 1,
        !,
        specialize(Agenda,Agendaout,Eplus,Eminus,Delta,N1).

add_ref_to_agenda(_Rule,_Np,_Nm,_Kp,_Km,_Var,[],_Eplus,_Eminus,
        _Delta,Agenda,
        Agenda):-!.

/* case in which Lit and the current rule have no variable in common */
add_ref_to_agenda(rule(H,B),Np,Nm,Kp,Km,Var,[Lit|RestLit],Eplus,Eminus,Delta,
	Agendain,
	Agendaout):-
	no_common_var(Lit,[H|B]),!,
	add_ref_to_agenda(rule(H,B),Np,Nm,Kp,Km,Var,RestLit,Eplus,Eminus,Delta,
	Agendain,
	Agendaout).

add_ref_to_agenda(rule(H,B),Np,Nm,Kp,Km,Var,[Lit|RestLit],Eplus,Eminus,Delta,
        Agendain,
        Agendaout):-
        \+(memberchk_eq(Lit,B)),!,
        appendc(B,[Lit],B1),
        evaluate(Value,rule(H,B1),Eplus,_Epluscovered,Eminus,
                _Eminuscovered,Delta,
                _Deltaout,Np,Nm,Np1,Nm1,Npa,Nma,Kp,Km,Kp1,Km1),
        Npt is Np1 + Npa,
	min_cov(MC),
        (Npt>=MC->
                (Nm1=0->
                        C=yes
                ;
                        C=no
                ),
                print_refinement([rule(H,B1),Value,C,Np1,Npa,Nm1,Nma,Kp1,Km1]),
                beamsize(Beamsize),
                insert_in_order([rule(H,B1),Value,C,Np1,Npa,Nm1,Nma,Kp1,Km1,
                        Var],
                        Beamsize,Agendain,Agenda1)
        ;
                Agenda1=Agendain
        ),
        !, % tail rec. optim.
        add_ref_to_agenda(rule(H,B),Np,Nm,Kp,Km,Var,RestLit,Eplus,Eminus,
        Delta,
        Agenda1,Agendaout).

/* case in which Lit is already in the body */
add_ref_to_agenda(rule(H,B),Np,Nm,Kp,Km,Var,[_Lit|RestLit],Eplus,Eminus,
        Delta,Agendain,
        Agendaout):-!,
        add_ref_to_agenda(rule(H,B),Np,Nm,Kp,Km,Var,RestLit,Eplus,Eminus,
                Delta,Agendain,
                Agendaout).


/* ************** Predicates for clause evaluation ********* */
evaluate_new_agenda([],Agenda,Agenda,_,_,_):-!.

evaluate_new_agenda([[Rule,_V,_C,_Np,_Npa,_Nm,_Nma,Kp,Km,Var]|Agenda],
                AgendaNewin,
                AgendaNewout,
                Eplus,Eminus,Delta):-
        evaluate1(Value,Rule,Eplus,_Epc,Eminus,_Emc,Delta,_Dout,Np,Nm,
                Npa,Nma,Kp,Km,C),
        beamsize(Beamsize),
        (Np>0->
                insert_in_order([Rule,Value,C,Np,Npa,Nm,Nma,Kp,Km,Var],
                                Beamsize,AgendaNewin,AgendaNew1)
        ;
                AgendaNew1=AgendaNewin
        ),
        evaluate_new_agenda(Agenda,AgendaNew1,AgendaNewout,Eplus,Eminus,Delta).

/* evaluate1: used to re-evaluate clauses in the agenda, it does not calculate a
new k but uses the one evaluated before, since it does not know the previous
number of covered examples*/
evaluate1(Value,Cl,Eplus,Epluscovered,Eminus,Eminuscovered,
                Deltain,Deltaout,Nplus,Nminus,Nplusa,Nminusa,
                Ktrueplus,Ktrueminus,Correct):-
        asserta_rule(Cl),
        assert(Cl), % used for optimization in epluscovered and eminuscovered
        Cl=rule(H,_B),
        functor(H,Hpred,_),
        asserta(current_eplus_tot(Eplus)),
        asserta(current_eminus_tot(Eminus)),
        epluscovered(Eplus,[],Epluscovered,Deltain,Delta1,0,Nplus,0,Nplusa,
                Hpred),
        eminuscovered(Eminus,[],Eminuscovered,Delta1,Deltaout,0,Nminus,
                0,Nminusa,Hpred),
        retract_rule(Cl),
        retract(Cl),
        retract(current_eplus_tot(Eplus)),
        retract(current_eminus_tot(Eminus)),
        (Nminus=0->
                Correct=yes
        ;
                Correct=no
        ),
        Value is (Nplus + Ktrueplus * Nplusa ) /
        (Nplus + Nminus + Ktrueplus *Nplusa + Ktrueminus * Nminusa ).

/* evaluate: used to evaluate clauses in the agenda */
evaluate(Value,Cl,Eplus,Epluscovered,Eminus,Eminuscovered,
                Deltain,Deltaout,Npprec,Nmprec,Nplus,Nminus,
                Nplusa,Nminusa,
                Kp,Km,Ktrueplus,Ktrueminus):-
        asserta_rule(Cl),
        assert(Cl),     % used for optimization in epluscovered and eminuscov.
        Cl=rule(H,B),
        functor(H,Hpred,_),
        asserta(current_eplus_tot(Eplus)),
        asserta(current_eminus_tot(Eminus)),
        epluscovered(Eplus,[],Epluscovered,Deltain,Delta1,0,Nplus,
                0,Nplusa,Hpred),
        eminuscovered(Eminus,[],Eminuscovered,Delta1,Deltaout,0,Nminus,
                0,Nminusa,Hpred),
        retract_rule(Cl),
        retract(Cl),
        retract(current_eplus_tot(Eplus)),
        retract(current_eminus_tot(Eminus)),
        last(B,L),
        (abducible(L)->
                kt(KT), % k threshold
                Ktp is Kp * Nplus / Npprec,
                        %Nplus / (Npprec - Nplusa),
                (Ktp > KT ->
                        Ktrueplus=Ktp
                ;
                        Ktrueplus=KT
                ),
                Ktm is Km * Nminus / Nmprec,
                        %(Nminus)/(Nmprec - Nminusa),
                (Ktm > KT ->
                        Ktrueminus=Ktm
                ;
                        Ktrueminus=KT
                )
        ;
                Ktrueplus=Kp, Ktrueminus=Km
        ),
        Value is (Nplus + Ktrueplus * Nplusa) /
                         (Nplus + Nminus + Ktrueplus *Nplusa + Ktrueminus * Nminusa).

epluscovered([],Epc,Epc,Delta,Delta,N,N,Na,Na,_Hpred):-!.
        % end of recursion: no more pos ex to test

epluscovered([H|T],Epcin,Epcout,Deltain,Deltaout,Npin,Npout,Npain,Npaout,Hp):-
        \+(functor(H,Hp,_)),!,
% example not belonging to the current target pred
        epluscovered(T,Epcin,Epcout,Deltain,Deltaout,
                Npin,Npout,Npain,Npaout,Hp).

epluscovered([H|T],Epcin,Epcout,Deltain,Deltaout,Npin,Npout,Npain,Npaout,Hp):-
        assert_current_tr_set(H),
        asserta(already_abduced(Deltain)),
        der_depth(Depth),
        rule(H,Body),   % optimization: the first resolution step is done
                        % with the rule
        (solve_abd(Body,[],Delta,Depth) ->
                % covered example
                ([]=Delta->
                        Np1 is Npin +1,
                        Npa1=Npain,
                        Deltain1=Deltain
                ;
                        Deltain1=[[H|Delta]|Deltain],
                        Npa1 is Npain +1,
                        Np1=Npin
                ),
                retract(already_abduced(_)),
                retract_current_tr_set,!,
                epluscovered(T,[H|Epcin],Epcout,Deltain1,Deltaout,Np1,Npout,
                        Npa1,Npaout,Hp)
        ;
                % uncovered example
                retract(already_abduced(_)),
                retract_current_tr_set,!,
                epluscovered(T,Epcin,Epcout,Deltain,Deltaout,Npin,Npout,
                        Npain,Npaout,Hp)
        ).

eminuscovered([],Emc,Emc,Delta,Delta,N,N,Na,Na,_Hp):-!.
        % end of recursion: no more neg ex to test

eminuscovered([H|T],Emcin,Emcout,Deltain,Deltaout,Nmin,Nmout,Nmain,Nmaout,Hp):-
        \+(functor(H,Hp,_)),!, % example not belonging to the current target pred
        eminuscovered(T,Emcin,Emcout,Deltain,Deltaout,Nmin,Nmout,Nmain,
                Nmaout,Hp).

eminuscovered([H|T],Emcin,Emcout,Deltain,Deltaout,Nmin,Nmout,Nmain,Nmaout,Hp):-
        assert_current_tr_set(H),
        asserta(already_abduced(Deltain)),
        der_depth(Depth),
        rule(H,Body),
        (solve_con2([Body],[not(H)],Delta1,Depth)->
        % optmization: the rule is used immediately in the consistency phase
        % in the hypothesis  the on not(H) there is only the negation ic
                % example not covered (not(H) succeeds)
                delete(Delta1,not(H),Delta2),
                % not(H) is removed from abduced literals
                ([]\==Delta2->
                        Nma1 is Nmain +1,
                        Deltain1=[[not(H)|Delta2]|Deltain]
                ;
                        Nma1 is Nmain,
                        Deltain1=Deltain
                ),
                retract(already_abduced(_)),
                retract_current_tr_set,!,
                eminuscovered(T,Emcin,Emcout,Deltain1,Deltaout,Nmin,Nmout,
                        Nma1,Nmaout,Hp)
        ;
                Nm1 is Nmin +1,
                retract_current_tr_set,
                retract(already_abduced(_)),!,
                eminuscovered(T,[H|Emcin],Emcout,Deltain,Deltaout,Nm1,Nmout,
                        Nmain,Nmaout,Hp)
        ).


/* ************* Auxiliary Predicates ***************** */


update(Eplusin,Epluscov,Eplusout,Eminusin,Eminusout,Delta):-
        deleteall(Eplusin,Epluscov,Eplus1),
        add_deltas(Delta,Eplus1,Eplusout,Eminusin,Eminusout).

add_deltas([],Eplus,Eplus,Eminus,Eminus).

add_deltas([[_Example|Delta]|RestDeltas],Eplusin,Eplusout,Eminusin,Eminusout):-
        add_abd(Delta,Eplusin,Eplus1,Eminusin,Eminus1),
        add_deltas(RestDeltas,Eplus1,Eplusout,Eminus1,Eminusout).

add_abd([],Eplus,Eplus,Eminus,Eminus):-!.

add_abd([not(A)|DeltaRest],Eplusin,Eplusout,Eminusin,[A|Eminusout]):-
        bias(rule(A,_),_,_),!, % A is a target predicate
        add_abd(DeltaRest,Eplusin,Eplusout,Eminusin,Eminusout).

add_abd([A|DeltaRest],Eplusin,[A|Eplusout],Eminusin,Eminusout):-
        bias(rule(A,_),_,_),!,
        add_abd(DeltaRest,Eplusin,Eplusout,Eminusin,Eminusout).

add_abd([_|DeltaRest],Eplusin,Eplusout,Eminusin,Eminusout):-
        add_abd(DeltaRest,Eplusin,Eplusout,Eminusin,Eminusout).

insert_in_order(NewRuleItem,BeamSize,[],
        [NewRuleItem]):-
        BeamSize>0,!.

insert_in_order(_NewRuleItem,0,Agenda,Agenda):-!.

insert_in_order([NewRule,NewValue|NewRest],BeamSize,
        [ [Rule,Value|Rest] |RestAgendain],
        [ [NewRule,NewValue|NewRest],[Rule,Value|Rest] |RestAgendaout]):-
        (NewValue>Value % bigger heuristic
        ;
        NewValue=Value,
        NewRest=[_NC,NNp|_NR],
        Rest=[_C,Np|_R],
        NNp>Np),!,  % equal heuristic, more pos ex covered
        length(RestAgendain,L),
        (L>BeamSize->
                nth(BeamSize,RestAgendain,_Last,RestAgendaout)
        ;
                RestAgendaout=RestAgendain
        ).

insert_in_order(NewRuleItem,BeamSize,
        [ RuleItem |RestAgenda],
        [ RuleItem |RestAgendaout]):-
        BeamSize1 is BeamSize -1,
        insert_in_order(NewRuleItem,BeamSize1,RestAgenda,
                RestAgendaout).

deleteallocc(H,Ep,Ep):-
        \+(member(H,Ep)),!.

deleteallocc(H,Epi,Epo):-
        functor(H,Hp,Ar),
        functor(H1,Hp,Ar),
        select(H1,Epi,Ep1),
        deleteallocc(H,Ep1,Epo).

% select a literal from the bias
select_literal_to_add(L,AllLitin,AllLitout):-
                        member(L,AllLitin),
                        delete(AllLitin,L,AllLitout).

no_common_var(Lit,L):-
	get_var_term(Lit,Var),
	get_var_list(L,[],Var1),
	no_common_var_lists(Var,Var1).

get_var_term(Var,[Var]):-
	var(Var),!.
	
get_var_term(Term,Var):-
	Term=..[_|LArg],
	get_var_list(LArg,[],Var).

get_var_list([],Var,Var):-!.

get_var_list([H|Tail],VarIn,VarOut):-
	get_var_term(H,VarH),
	append(VarIn,VarH,VarTemp),
	get_var_list(Tail,VarTemp,VarOut).

no_common_var_lists([],_):-!.

no_common_var_lists([H|Tail],Var):-
	\+ memberchk_eq(H, Var),
	no_common_var_lists(Tail,Var).
		


/* ********* Predicates for modifying the database ********* */

retract_old_pred:-
        retractall(current_eplus(_)),
        retractall(current_eplus_tot(_)),
        retractall(current_eminus_tot(_)),
        retractall(current_eminus(_)),
        retractall(already_abduced(_)),
        retractall(rule(_,_)),
        findall(X,(bias(rule(X,_),_,_),retractall(X)),_L).

assert_current_tr_set(Ex):-
        current_eplus_tot(Eplus),
        delete(Eplus,Ex,Epr),
        asserta(current_eplus(Epr)),
        current_eminus_tot(Eminus),
        delete(Eminus,Ex,Emr),
        asserta(current_eminus(Emr)).

retract_current_tr_set:-
        retract(current_eplus(_Epr)),
        retract(current_eminus(_Emr)).

asserta_rule(rule(H,[])):-
        asserta(H).

asserta_rule(rule(H,B)):-
        list2and(B,Ba),
        asserta((H:-Ba)).

retract_rule(rule(H,[])):-
        retract(H).

retract_rule(rule(H,B)):-
        list2and(B,Ba),
        retract((H:-Ba)).

/* ********** Predicates for list handling *********** */

list2and([],true):-!.

list2and([X],X):-
        \+(is_list(X)),!.

list2and([H|T],(H,Ta)):-!,
        list2and(T,Ta).

memberchk_eq(A, [A1|_]) :-A==A1, !.
memberchk_eq(A, [_|B]) :-
        memberchk_eq(A, B).

deleteall(Eplus,[],Eplus):-!.

deleteall(Eplusin,[H|T],Eplusout):-
        delete(Eplusin,H,Eplusout1),
        deleteall(Eplusout1,T,Eplusout).
