/* M-ACL algorithm for multiple predicate learning */
ver('1').

:- multifile(abducibles/1).
:- dynamic(abducibles/1).
:- set_prolog_flag(double_quotes,codes).

/*
Characteristics:

        Backtracking on clause addition

        Extensional coverage of example, using the original training set,
                not the only that is obtained by abductively extending it

        Test of the positive examples: first an (extensional only) prolog
        derivation for them is tried.
        If it fails, then the abductive derivation is tried.
        The extensional prolog derivation is implemented by means of the abd pp
        without abducibles.

        Non abducible are always inserted before literals in the body of rules, so
                that abducible always appear at the end

        Target clauses have the form
                clause((Head:-Body),Name,EplusCovered).

        The delta set has the form
                [[abd,list_of_clauses]]
        for each abducible, it is stored as well the list of clauses that
                generated it

M_ACL is based on the ACL1 algorithm for single predicate learning

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


kt(0.1). % k threshold

max_spec_steps(10).
der_depth(50).
beamsize(10).
verbosity(3).
%m(2).
min_cov(1).
stopping_acc(0.8).


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

%:-consult(abdpp1).
%:-consult(abdpp_mple).
%:-consult(output).


i(File):-
        atom_concat(File,'.bg',FileBg),
        [FileBg],
        atom_concat(File,'.rules',FileOut),
        title(FileBg,FileOut,user_output),
        retract_old_pred,
        statistics(runtime,[_,_]),
        induce(Rules,_Delta),
        statistics(runtime,[_,T]),
        T1 is T /1000,
        open(FileOut,write,Stream),
        title(FileBg,FileOut,Stream),
        title(FileBg,FileOut,user_output),
        format(Stream,"/* Execution time ~f seconds. Generated rules */~N",[T1]),
        format("/* Execution time ~f seconds. Generated rules */~N",[T1]),
        print_list(Rules,Stream),
        print_list(Rules,user_output),
        print_backtracked_clauses(user_output),
        print_backtracked_clauses(Stream),
        close(Stream).


induce(Rules,Delta):-
        eplus(Eplus),
        covering_loop(Eplus,[],[],[],Rules,[],Delta).

initialize_agenda(Agenda):-
        findall([H,V],bias(rule(H,_B),V,_Bias),L),
        init_ag(L,[],Agenda).

init_ag([],Agenda,Agenda):-!.

init_ag([[P,Var]|Rp],Agendain,[ [rule(P,[],Name),_V,_GC,_LC,_Np,_Npa,_Nm,_Nma,Var]| Agendaout]):-!,
        new_clause_name(Name),
        init_ag(Rp,Agendain,Agendaout).

/* covering_loop(Eplus,Eminus,Rulesin,Rulesout,Deltain,Deltaout) */
/* no more positive examples to cover */
covering_loop([],[],_EminusA,Rules,Rules,Delta,Delta):-!.

/* some eplus still to cover: generate a new clause */
covering_loop(Eplus,EplusA,EminusA,RulesIn,RulesOut,DeltaIn,DeltaOut):-
        print_ex_rem(Eplus,EplusA,EminusA),
        specialization_loop(Eplus,EplusA,EminusA,DeltaIn,
        Clause,Epc,Epac,Emac,Np,Npa,Nm,Nma,Delta1,GC,LC),
        deleteall(Delta1,DeltaIn,NewDelta),
        print_new_clause(Clause,GC,LC,Np,Npa,Nm,Nma,
                Epc,Epac,Emac,NewDelta),
        asserta_rule(Clause,Epc,Epac),
        update(Eplus,Epc,NEp1,EplusA,Epac,NEpa1,EminusA,NEma1,NewDelta),
        (GC=no->
                % backtracking on previous clauses
                backtracking(Emac,Delta1,Delta2,NEp1,NEp,NEpa1,NEpa,NEma1,NEma,RulesIn,Rules1)
        ;
                NEp=NEp1,
                NEpa=NEpa1,
                NEma=NEma1,

                Delta2=Delta1,
                Rules1=RulesIn
        ),!,
        covering_loop(NEp,NEpa,NEma,
                [[Clause,Np,Npa,Nm,Nma,Epc,Epac,Emac,NewDelta]
                |Rules1],RulesOut,Delta2,DeltaOut).

specialization_loop(Eplus,EplusA,EminusA,Deltain,
        Clause,Epc,Epac,Emac,Np,Npa,Nm,Nma,Delta1,GC,LC):-
        initialize_agenda(Agenda1),
        evaluate_new_agenda(Agenda1,[],Agenda,Eplus,EplusA,
                EminusA,Deltain),
        assert(loc_cons(no_loc_cons,0)),
        specialize(Agenda,Agenda2,Eplus,EplusA,EminusA,Deltain,0),
        (Agenda2\==[]->
        % globally consistent clause found
                Agenda2=[ [Clause,_V|_]|_Agenda3],
                retract(loc_cons(_Clause,_))
        ;
        % locally consisten clause found
                retract(loc_cons(Clause,_))
        ),
        evaluate(_Value,Clause,Eplus,Epc,EplusA,Epac,_Eminuscovered,
                EminusA,Emac,Deltain,Delta1,Np,Npa,Nm,Nma,GC,LC).


/* specialize(Agenda,Clout,AllowedLiterals,Eplus,Eminus,
        Deltain) */

/* stopping criterion (0): no globally consistent clause found, only locally
        consistent */
specialize([],[],
        _Eplus,_Epa,_EminusA,_Delta,_N):-!,
        loc_cons(Clause,_),
        Clause\==no_loc_con.

/* stopping criterion (1): when the 1st rule does not cover any negative
        example */
specialize([ [BestRule,Value,yes|RestRule] |RestAgenda],
        [ [BestRule,Value,yes|RestRule] |RestAgenda],
        _Eplus,_Epa,_EminusA,_Delta,_N):-!,
        print_current_agenda([ [BestRule,Value,yes|RestRule] |RestAgenda]).

/* stopping criterion (2): heuristic function > Threshold */
/* spcialize([ [BestRule,Value,_End] |_Rest],BestRule,_AllowedLiterals,_Eplus,
        _Eminus,_Delta,_N):-
        Value>0.95,!. */

/* stopping criterion (3): reached the limit on the number of specializing

        steps */

specialize(Agenda,Agenda,
        _Eplus,_Epa,_EminusA,_Delta,N):-
        max_spec_steps(Nmax),
        N=Nmax,
        print_max_spec_steps(Nmax),!.

specialize([ [BestRule,_Value,_GC,_LC,Np,Npa,Nm,Nma,Var] |Rest],Agendaout,
        Eplus,Epa,EminusA,Delta,N):-
        print_spec_step(N),
        print_current_agenda([ [BestRule,_Value,_GC,_LC,Np,Npa,Nm,Nma,Var]
                |Rest]),
        print_refinements,
        BestRule=rule(H,B,_Name),
        bias(rule(H,B),Var,AllowedLiterals),
        add_ref_to_agenda(BestRule,Var,AllowedLiterals,
                Eplus,Epa,EminusA,
                Delta, Rest, Agenda),
        N1 is N + 1,
        !,
        specialize(Agenda,Agendaout,Eplus,Epa,EminusA,Delta,N1).


add_ref_to_agenda(_Rule,_Var,[],_Eplus,_Epa,_EminusA,
        _Delta,Agenda,
        Agenda):-!.

add_ref_to_agenda(rule(H,B,Name),Var,[Lit|RestLit],Eplus,Epa,EminusA,Delta,
        Agendain,
        Agendaout):-
        \+(memberchk_eq(Lit,B)),!,
        (abducible(Lit)->
                append(B,[Lit],B1)
        ;
                separate_abd_nabd(B,[],Nabd,Abd),
                append(Nabd,[Lit],Nabd1),
                append(Nabd1,Abd,B1)
        ),
        new_clause_name(NewName),
        evaluate(Value,rule(H,B1,NewName),Eplus,_Epluscovered,Epa,_Epac,_Emc,EminusA,
                _EminuscoveredA,Delta,
                _Deltaout,Np,Npa,Nm,Nma,GC,LC),
        Npt is Np + Npa,
	min_cov(MC),
        (Npt>=MC->
                print_refinement([rule(H,B1,NewName),Value,GC,LC,Np,Npa,Nm,Nma]),
                beamsize(Beamsize),
                insert_in_order([rule(H,B1,NewName),Value,GC,LC,Np,Npa,Nm,Nma,Var],
                        Beamsize,Agendain,Agenda1),
	        (LC=yes->
        	        loc_cons(Clause,V),
                	(Value>V->
                        	retract(loc_cons(Clause,V)),
	
        	                assert(loc_cons(rule(H,B1,NewName),Value)),
                	        print_loc_cons(rule(H,B1,NewName),Value)
                	;
                        	true
                	)
        	;
                	true
        	)
        ;
                Agenda1=Agendain
        ),
        !, % tail rec. optim.
        add_ref_to_agenda(rule(H,B,Name),Var,RestLit,Eplus,Epa,EminusA,
        Delta,Agenda1,Agendaout).

/* case in which Lit is already in the body */
add_ref_to_agenda(rule(H,B,Name),Var,[_Lit|RestLit],Eplus,Epa,EminusA,
        Delta,Agendain,
        Agendaout):-!,
        add_ref_to_agenda(rule(H,B,Name),Var,RestLit,Eplus,Epa,EminusA,
                Delta,Agendain,
                Agendaout).


backtracking(Emac,DeltaIn,DeltaOut,EpIn,EpOut,EpaIn,EpaOut,EmaIn,EmaOut,
        RulesIn,RulesOut):-
        pick_clauses(Emac,DeltaIn,[],ClauseList),
        print_backtracking(ClauseList),
        remove_clause(ClauseList,DeltaIn,DeltaOut,EpIn,EpOut,EpaIn,EpaOut,EmaIn,EmaOut,RulesIn,RulesOut).



pick_clauses([],_D,Cl,Cl):-!.

pick_clauses([Ex|Rest],Delta,ClIn,ClOut):-
        findall(Clause, member([not(Ex),Clause],Delta),List),
        append(List,ClIn,Cl1),
        pick_clauses(Rest,Delta,Cl1,ClOut).


remove_clause([],Delta,Delta,Ep,Ep,Epa,Epa,Ema,Ema,Rules,Rules):-!.


remove_clause([Name|Rest],DeltaIn,DeltaOut,EpIn,EpOut,EpaIn,EpaOut,EmaIn,EmaOut,RulesIn,RulesOut):-
        retract(rule(H,B,Name,Epc,Epac)),
        assert(backtracked(rule(H,B,Name))),
        append(Epc,EpIn,Ep1),
        append(Epac,EpaIn,Epa1),
        remove_abduced(Name,DeltaIn,Delta1,Removed),
        remove_examples(Removed,Epa1,Epa2,EmaIn,Ema1),
        delete1(RulesIn,[rule(_,_,Name)|_],Rules1),
        remove_clause(Rest,Delta1,DeltaOut,Ep1,EpOut,Epa2,EpaOut,Ema1,EmaOut,
                Rules1,RulesOut).


remove_abduced(Name,DeltaIn,DeltaOut,RemovedEx):-
        findall(Ex,member([Ex,Name],DeltaIn),RemovedEx),
        findall([Ex,Name],member([Ex,Name],DeltaIn),RemovedDelta),
        deleteall(DeltaIn,RemovedDelta,DeltaOut).

remove_examples([],Epa,Epa,Ema,Ema):-!.

remove_examples([not(Ex)|Rest],EpaIn,EpaOut,EmaIn,EmaOut):-
        delete(EmaIn,Ex,Ema1),
        remove_examples(Rest,EpaIn,EpaOut,Ema1,EmaOut).

remove_examples([Ex|Rest],EpaIn,EpaOut,EmaIn,EmaOut):-
        % if the example was already covered, do not do anything
        (delete(EpaIn,Ex,Epa1);Epa1=EpaIn),
        remove_examples(Rest,Epa1,EpaOut,EmaIn,EmaOut).


/* ************** Predicates for clause evaluation ********* */
evaluate_new_agenda([],Agenda,Agenda,_,_,_,_):-!.

evaluate_new_agenda([[Rule,_V,GC,LC,Np,Npa,Nm,Nma,Var]|Agenda],
                AgendaNewin,
                AgendaNewout,
                Ep,Epa,Ema,Delta):-
        evaluate(Value,Rule,Ep,_Epc,Epa,_Epac,_Emc,Ema,_Emac,Delta,_Dout,Np,Npa,Nm,Nma,GC,LC),
        beamsize(Beamsize),
        Npt is Np+Npa,
        (Npt>0->
                insert_in_order([Rule,Value,GC,LC,Np,Npa,Nm,Nma,Var],
                                Beamsize,AgendaNewin,AgendaNew1)
        ;
                AgendaNew1=AgendaNewin
        ),
        evaluate_new_agenda(Agenda,AgendaNew1,AgendaNewout,Ep,Epa,Ema,Delta).




/*  evaluate(Value,rule(H,[]),Eplus,_Epluscovered,Eminus,_Eminuscovered,
                Delta,_Deltaout),
*/

evaluate(Value,Cl,Ep,Epc,Epa,Epac,Emc,
                Ema,Emac,Deltain,Deltaout,Np,Npa,Nm,Nma,GC,LC):-
        asserta_rule(Cl,[],[]),
        assert(Cl), %for optimization in the evaluation proc
        Cl=rule(H,_B,_N),
        functor(H,Hpred,_),
        eminus(Em),
        eplus(Eplus),
        append(Eplus,Epa,Ept),
        append(Em,Ema,Emt),
        asserta(current_eplus_tot(Ept)),
        asserta(current_eminus_tot(Emt)),
        epluscovered(Ep,[],Epc,Deltain,Delta1,0,Np,
                Hpred),
        eminuscovered(Em,[],Emc,Delta1,Delta2,0,Nm,
                Hpred),
        epluscovered(Epa,[],Epac,Delta2,Delta3,
                0,Npa,Hpred),
        eminuscovered(Ema,[],Emac,Delta3,Deltaout,
                0,Nma,Hpred),
        retract(Cl),
        retract_rule(Cl),
        retract(current_eplus_tot(Ept)),
        retract(current_eminus_tot(Emt)),
        (Nm=0->
                LC=yes,
                (Nma=0->
                        GC=yes
                ;
                        GC=no
                )
        ;
                LC=no,
                GC=no
        ),
        heuristic_function(Value,Np,Npa,Nm,Nma,Hpred).

heuristic_function(0,0,0,_Nminus,_Nminusa,_Hpred):-!.

heuristic_function(Value,Nplus,Nplusa,Nminus,Nminusa,_Hpred):-
        Value is (Nplus + Nplusa ) /
        (Nplus + Nminus + Nplusa + Nminusa ).

/* epluscovered(Eplus,Epluscoveredin,Epluscoveredout,Deltain,Deltaout,
                0,Nplus,0,Nplusa) */

epluscovered([],Epc,Epc,Delta,Delta,N,N,_Hpred):-!.
        % end of recursion: no more pos example to try

epluscovered([H|T],Epcin,Epcout,Deltain,Deltaout,Npin,Npout,Hp):-
        \+(functor(H,Hp,_)), % example not belonging to the current target pred
        epluscovered(T,Epcin,Epcout,Deltain,Deltaout,
                Npin,Npout,Hp).

epluscovered([H|T],Epcin,Epcout,Deltain,Deltaout,Npin,Npout,Hp):-
        assert_current_tr_set(H),
        der_depth(Depth),
        rule(H,Body,Name),        %optimization: the first resolution step is done

                        % with the rule
        add_cl_name(Body,Body1,Name),
        (coverage_test(H,Body1,Deltain,Depth,Delta1,yes)->
                % H is not covered abducing it
                Np1 is Npin +1,
                append(Delta1,Deltain,Delta2),
                remove_duplicates(Delta2,Delta3),
                retract_current_tr_set,
                epluscovered(T,[H|Epcin],Epcout,Delta3,Deltaout,Np1,Npout,
                        Hp)
        ;
                retract_current_tr_set,
                epluscovered(T,Epcin,Epcout,Deltain,Deltaout,Npin,Npout,
                        Hp)
        ).

coverage_test(_H,Body1,_Deltain,Depth,[],Ans):-
        retract(abducibles(LA)),
        asserta(abducibles([])),
        (solve_abd(Body1,[],[],Depth)->
                Ans1=yes
        ;
                Ans1=no
        ),
        retract(abducibles([])),
        asserta(abducibles(LA)),
        Ans1=Ans.
% in this way it fails only after having reasserted the abducibles set

coverage_test(H,Body1,Deltain,Depth,Delta1,yes):-
        solve_abd(Body1,[],Delta1,Depth),\+(member(H,Delta1)),
                \+ inconsistent(Delta1,Deltain).



inconsistent([[D,_]|_Rest],Deltain):-
        neg(D,NotD),
        member([NotD,_],Deltain),!.

inconsistent([_|Rest],Deltain):-
        inconsistent(Rest,Deltain).


eminuscovered([],Emc,Emc,Delta,Delta,N,N,_Hp):-!.
        % fine della ricorsione: non ci sono piu es neg da tentare

eminuscovered([H|T],Emcin,Emcout,Deltain,Deltaout,Nmin,Nmout,Hp):-
        \+(functor(H,Hp,_)), % example not belonging to the current target pred
        eminuscovered(T,Emcin,Emcout,Deltain,Deltaout,Nmin,Nmout,Hp).

eminuscovered([H|T],Emcin,Emcout,Deltain,Deltaout,Nmin,Nmout,Hp):-
        assert_current_tr_set(H),
        rule(H,Body,Name),
        add_cl_name(Body,Body1,Name),
        der_depth(Depth),
        ((solve_con2([Body1],[[not(H),top]],Delta1,Depth),
        \+ inconsistent(Delta1,Deltain))->
        % optimization: the rule is used immediately in the consistency phase.
        % hp: on not(H) there only the naf ic
                % ex not covered (not(H) succeeds)
                delete(Delta1,[not(H),top],Delta2),
                % not(H) removed from Delta
                append(Delta2,Deltain,Delta3),
                remove_duplicates(Delta3,Delta4),
                retract_current_tr_set,
                eminuscovered(T,Emcin,Emcout,Delta4,Deltaout,Nmin,Nmout,Hp)

        ;
                Nm1 is Nmin +1,
                retract_current_tr_set,
                eminuscovered(T,[H|Emcin],Emcout,Deltain,Deltaout,Nm1,Nmout,Hp)
        ).



remove_duplicates(A,B):- list_to_set(A,B).

/* ************* Auxiliary Predicates ***************** */

separate_abd_nabd([],Nabd,Nabd,[]):-!.

separate_abd_nabd([H|T],Nabd,Nabd,[H|T]):-
        abducible(H),!.

separate_abd_nabd([H|T],Nabdin,Nabdout,Abd):-
        append(Nabdin,[H],Nabd1),
        separate_abd_nabd(T,Nabd1,Nabdout,Abd).


update(Ep,Epc,NEp,Epa,Epac,NEpa,Ema,NEma,Delta1):-
        deleteall(Ep,Epc,NEp),
        deleteall(Epa,Epac,NEpa1),
        add_ex(Delta1,NEpa1,NEpa,Ema,NEma).

add_ex([],Eplus,Eplus,Eminus,Eminus):-!.

add_ex([[not(A),_]|DeltaRest],Eplusin,Eplusout,Eminusin,[A|Eminusout]):-
        !,
        add_ex(DeltaRest,Eplusin,Eplusout,Eminusin,Eminusout).

add_ex([[A,_]|DeltaRest],Eplusin,[A|Eplusout],Eminusin,Eminusout):-
        add_ex(DeltaRest,Eplusin,Eplusout,Eminusin,Eminusout).


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
        NewRest=[_NGC,_NLC,NNp|_NR],
        Rest=[_GC,_LC,Np|_R],
        NNp>Np),!,  % equal heuristic, more pos ex covered
        length(RestAgendain,L),
        (L>BeamSize->
                nth0(BeamSize,RestAgendain,_Last,RestAgendaout)
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


new_clause_name(H):- gensym('cl_',H).
/*
        retract(new_const_number_clauses(N)),
        N1 is N + 1,
        assert(new_const_number_clauses(N1)),
        number_chars(N,Nstr),
        append(`c`,Nstr,Nconst),
        name(H,Nconst).

:-dynamic new_const_number_clauses/1.

new_const_number_clauses(0).
*/

/* ********* Predicates for modifying the database ********* */

retract_old_pred:-
        retractall(current_eplus(_)),
        retractall(current_eplus_tot(_)),
        retractall(current_eminus_tot(_)),
        retractall(current_eminus(_)),
        retractall(already_abduced(_)),
        retractall(rule(_,_,_)),
        retractall(rule(_,_,_,_,_)),
        retractall(loc_cons(_,_)),
        retractall(backtracked(_)).


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

asserta_rule(rule(H,B,Name),Epc,Epac):-
        assertz(rule(H,B,Name,Epc,Epac)).

retract_rule(rule(_H,_B,Name)):-
        retract(rule(_,_,Name,_,_)).

/* ********** Predicates for list handling *********** */

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

delete1([],_El,[]):-!.

delete1([El|Tin],El,Tout):-!,
        delete1(Tin,El,Tout).

delete1([H|Tin],El,[H|Tout]):-!,
        delete1(Tin,El,Tout).



/* abdpp.pl, Abductive proof procedure */
verapp(mple).

/* version mple
	extensional derivation added
	no abduction of non ground abducibles
*/

/* version mpl for multiple predicate learning
	delta set of the form
	[[abd,cl],...]
	learned clauses of the form
		rule(Head,Body,Name,Epluscovered,EplusAbducedCovered).
	background clauses as before
	abduction for bk clauses:
	[abd,'bk']
*/

/* version n2
	no more extensional derivation
*/

/* version n1
	corrected a bug in the consistency procedure:
		when a literal not(a) is encountered
		if the abd pp for a suceeds, then we discard the branch
		if it fails, then we must discrd the literal.
*/

/* version n
	the negation of abducible predicates does not have to be included
	in the set of abducible predicates nor the ic <-a(X),not(a(X))
*/

/* version m
	background rules specified as normal prolog rules, without
	the predicate rule(..)
*/

/* version l
	procedure for abducible target: also the examples are used in the
	derivation 
*/

/* version k
	corrected a bug of version i:
		the literal to be adbuced is skolemized only if it is positive
		skolem((not(Goal),..) did not work
*/

/* version j
	corrected a bug of the inconsistency procedure:
		even if no cosntraint was violated, the inc suceedeed
		becuase at_least_one_violated with [] succeeded
*/

/* version i
        treatment of non ground abducibles: every free variable is skolemized
		when the consistency is entered
		restriction: only constraint of the type
			<-a(X),... (type2)
		and not of the type 
			<-not(a(X)).... (type1)
		type1 ic could provide instantiations for X, so they should
		be tried first
		
*/

/* version h:
	ensured that non abducibles are resolved before abucibles in the 
		abductive derivation
*/

/* version g
        negation as failure literals for non abducible predicates are
        treated differently: they are not included in the set of abducibles
        the negation of non abd pred has not to be specified among abd nor
        the ic a,not(a) have to be specified for non abd

        but the negation of abd and the relative constraint have to be spec

	added the message when the depth limit is reached in the consistency
*/

/* version f
	inconsistency checking using only rules, not 
		abduced literals
	corrected the loop problem of inconsistency checking
	select_literal_in_1st_ic added in order to simplify the code
*/

/* version e
	literals that must be true according to ics are not abduced:
		inconsistency checking added
	from setof to findall for abducibles non ground
	selection rule in ics: 
		reduce first non abducibles, without choice point
		then abducibles, keeping a choice point, when the ic contains
			only non abd
		assumption: the rules and ic contain first non abd and then
			abd in their bodies
*/

/* Version d: limit on the depth of derivations added */

/* Version c: merge of version b and a

	version for Sicstus #2.1

	1) uniform treatment of user defined abducibles and default abducibles:
	besides the user defined abd and their constraints, the user must  
	include in the abducibles all the default literals not(a) plus all the
	constraints
	<-a,not(a). (version c) Example of input:

	rule(p,[q]).

	abducibles([not(p),q,not(q)]).

	ic([p,not(p)]).
	ic([q,not(q)]).
	
	2) the algorithm is able to find all the alternative explanations
	(version b) */

/* Version b: differences with version a1:
	the selection of the literal to be proved false in the 
	consistency phase is now done by a select predicate which leaves
	other choices open for backtracking
	In this way this algorithm finds all the minimal models !*/

/* Version a1: differences with version a
	no 3): negation of abducibles has not to appear in the list of
	abd.*/
	

/* Version a: differences with version 0
	1) treatment of negation as abduction for predicates
	not abducibile: not(p) and <-p,not(p) have to be included
	2) abducible can appear in the head of clauses
	3) negation of abducibles have to appear in the list of
	abducibles

*/

/* Version 0:
(negation of abducible is dealt implicitly by the
algorithm, therefore the ics expressing <-abd,not(abd) dont have
to be expressed)
falso, devono essere espressi altrimenti quando si fa la query
	not(abd)
viene subito abdotto not(abd) senza passare alla consistency per
abd*/
:-dynamic not/1.

:-use_module(library(lists)).

abducible(A):-
	abducibles(AbdSet),
	memberchk(A,AbdSet),!.


/* Abductive derivation:
solve_abd(Goal,AbdIn,AbdOut,Depth) */

/* normal resolution steps */
solve_abd([],Abd,Abd,_Depth):-!.

solve_abd([[true,_]|R],AbdIn,AbdOut,Depth):-!,
	solve_abd(R,AbdIn,AbdOut,Depth).  

/* depth limit reached */
solve_abd(Goal,_,_,0):-!,
	format("Depth limit reached, goal ~p~N",[Goal]),
        fail.

/* fact in the training set, ver l */
solve_abd([[not(A),_]|B],AbdIn,AbdOut,Depth):-
        current_eminus(Em),
        member(A,Em),
        solve_abd(B,AbdIn,AbdOut,Depth).

solve_abd([[A,_]|B],AbdIn,AbdOut,Depth):-
        current_eplus(Ep),
        member(A,Ep),
        solve_abd(B,AbdIn,AbdOut,Depth).

/* opposite of the fact in the training set: failure*/
solve_abd([[not(A),_]|_B],_AbdIn,_AbdOut,_Depth):-
        current_eplus(Ep),
        member(A,Ep),!,
        fail.

solve_abd([[A,_]|_B],_AbdIn,_AbdOut,_Depth):-
        current_eminus(Em),
        member(A,Em),!,
        fail.

/* resolution step */

/* background clause */
solve_abd([[A,_]|B],AbdIn,AbdOut,Depth):-
	clause(A,BodyAnd),
	and2list(BodyAnd,Body1),
	add_cl_name(Body1,Body,bk),
	Depth1 is Depth - 1,
	insert_nabd_1st(Body,B,B1),
	solve_abd(B1,AbdIn,AbdOut,Depth1).

/* target clause */
solve_abd([[A,_Name]|B],AbdIn,AbdOut,Depth):-
        rule(A,Body1,Name,_Epc,_Epac),
        add_cl_name(Body1,Body,Name),
        Depth1 is Depth - 1,
        insert_nabd_1st(Body,B,B1),
	solve_abd(B1,AbdIn,AbdOut,Depth1).


/* NAF goal: not(A) where A is not abd */
solve_abd([[not(A),Name]|B],AbdIn,AbdOut,Depth):-
        \+(abducible(A)),!,
        solve_con2([[A,Name]],[[not(A),Name]|AbdIn],AbdOut1,Depth),
        delete(AbdOut1,[not(A),Name],AbdOut2),
	solve_abd(B,AbdOut2,AbdOut,Depth).

/* end of normal resolution steps */

/* (1) A  has already been abduced */
solve_abd([[A,Name]|B],AbdIn,AbdOut,Depth):-
	memberchk([A,_],AbdIn),!,
	(memberchk([A,Name],AbdIn)-> 
        % abducible already present for the same clause, don't add it
		solve_abd(B,AbdIn,AbdOut,Depth)
        ;
	% abducible present for a different clause: add it
		solve_abd(B,[[A,Name]|AbdIn],AbdOut,Depth)
        ).


/* (2) A is abducible but neither A nor not(A) have already been abduced */
solve_abd([[not(A),Name]|B],AbdIn,AbdOut,Depth):-
	ground(A),
	abducible(A),!,
	\+(memberchk([A,_],AbdIn)),
	solve_con([not(A),Name],[[not(A),Name]|AbdIn],AbdOut1,Depth),
	solve_abd(B,AbdOut1,AbdOut,Depth).


solve_abd([[A,Name]|B],AbdIn,AbdOut,Depth):-
	ground(A),
	abducible(A),!,
	\+(memberchk([not(A),_],AbdIn)),
% only negation constraints are considered, therefore no consistency has to
% be tried
	solve_abd(B,[[A,Name]|AbdIn],AbdOut,Depth).

/* A not a list */
solve_abd(A,AbdIn,AbdOut,Depth):-
        \+(is_list(A)),!,
        solve_abd([[A,top]],AbdIn,AbdOut,Depth).


/* Consistency derivation:
solve_con([Goal,Name],AbdIn,AbdOut,Depth) */

solve_con([Goal,Name],AbdIn,AbdOut,Depth):-
% only negation constraints are considered
	skolem(Goal),
	neg(Goal,NegGoal),
	solve_con2([[[NegGoal,Name]]],AbdIn,AbdOut,Depth).
				/* V b: modified */


/* v b: predicate that selects one literal from the ic
 	and puts it in the head of ic
the selection is done only if the ic contains only abducibles */

select_literal_in_1st_ic([],[]):-!.

select_literal_in_1st_ic([FirstIc|RestIc],[NewFirstIc|RestIc]):-
	select_literal(FirstIc,NewFirstIc).

select_literal([],[]):-!.

select_literal([[Lit,Name]|RestIcIn],[L|IcOut]):-
	abducible(Lit),!,
	select(L,[[Lit,Name]|RestIcIn],IcOut).

select_literal(Ic,Ic).

/* all the ic are satisfied */
solve_con2([],Abd,Abd,_):-!. 

/* one of the Ic has been violated */
solve_con2([ [] |_],_,_,_):-!,fail.

/* depth limit reached */
solve_con2(Ics,_,_,0):-!,
	format("Depth limit reached, ics ~p~N",[Ics]),
        fail.

/* fact in the training set, ver l   */
/* if L1 is in the training set, delete it */
solve_con2([ [[not(L1),_]|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        current_eminus(Em),
        member(L1,Em),!,% in reality this prevents from finding other solutions
        select_literal(Rest_l,Rest_l1),
        solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth).

solve_con2([ [[L1,_]|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        current_eplus(Ep),
        member(L1,Ep),!,
        select_literal(Rest_l,Rest_l1),
        solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth).

/* if not(L1) is in the training set, then remove the ic */

solve_con2([ [[L1,_]|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        current_eminus(Em),
        member(L1,Em),!,
        select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,AbdIn,AbdOut,Depth).

solve_con2([ [[not(L1),_]|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        current_eplus(Ep),
        member(L1,Ep),!,
        select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,AbdIn,AbdOut,Depth).



/* (3) first try to use resolution */
solve_con2([ [[true,_]|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-!,
        select_literal(Rest_l,Rest_l1),
        solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth).


% background predicate
solve_con2([ [[L1,_]|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
	findall(NewIc,(
	        clause(L1,BodyAnd),
        	and2list(BodyAnd,Body1),
		add_cl_name(Body1,Body,bk),
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
	solve_con2([Ic1 | Rest_ic1],AbdIn,AbdOut,Depth1).   	

% target predicate
solve_con2([ [[L1,NameL1]|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
        findall(NewIc,(
                rule(L1,Body1,Name,_Ep,_Epac),
                add_cl_name(Body1,Body,Name),
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
	((ground(L1),abducible(L1))-> % L1 was an abducible with a partial definition:
        % add to the set of abducible the negation of all the literals
        % obtained resolving L1 with the partial definition
                findall([L1,NameL1],(
                        rule(L1,_Body,_,_,_)
                        ),
                        ListL1),
                complement_list(ListL1,ListnotL1),
                append(ListnotL1,AbdIn,Abd0),
		remove_duplicates(Abd0,Abd1)
        ;
                Abd1=AbdIn
        ),
        solve_con2([Ic1 | Rest_ic1],Abd1,AbdOut,Depth1).

/* (4) L1 has already been abduced */
solve_con2([ [[L1,_Name]|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
	memberchk([L1,_],AbdIn),!,
	select_literal(Rest_l,Rest_l1),
% do not add the abducible
	solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth).


/* (5) not(L1) has been abduced */
solve_con2([ [[not(L1),Name]| _Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
	memberchk([L1,_],AbdIn),!,
	select_literal_in_1st_ic(Rest_ic,Rest_ic1),
	(memberchk([L1,Name],AbdIn)-> 
        % abducible already present for the same clause, don't add it
                solve_con2(Rest_ic1,AbdIn,AbdOut,Depth)
        ;
                solve_con2(Rest_ic1,[[L1,Name]|AbdIn],AbdOut,Depth)
        ).

solve_con2([ [[L1,Name]|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
	memberchk([not(L1),_],AbdIn),!,
	select_literal_in_1st_ic(Rest_ic,Rest_ic1),
	(memberchk([not(L1),Name],AbdIn)-> 
	% abducible already present for the same clause, don't add it
		solve_con2(Rest_ic1,AbdIn,AbdOut,Depth)
	;
        	solve_con2(Rest_ic1,[[not(L1),Name]|AbdIn],AbdOut,Depth)
	).

/* (6) L1 is abducible but neither L1 nor not(L1) have already been abduced */

/* (6b) start an abductive derivation for not(L1) */

/* ver n1: Negation as Failure literal for abducibles and non-abducibles
NAF of abd is treated in the same way as for non-abd
 */
solve_con2([ [[not(L1),Name]|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
	ground(L1),% if L1 is non ground, the literal can not be reduced
	% and we have to backtrack on literal selection. Otherwise, L1
	% may succeed with an instantiation even if the constraint is 
	% violated (see example voted)
	solve_abd([[L1,Name]],AbdIn,AbdOut1,Depth),
	% if the derivation succeed, then drop the branch
	% and abduce L1
	select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,AbdOut1,AbdOut,Depth).

solve_con2([ [[not(_L1),_Name]|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-!,
        % if the derivation fails, then drop the literal
 	select_literal(Rest_l,Rest_l1),
	solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth).


/* ver n1:  L1 is abducible, we try to abduce the negation, if it is possible
then we discard the constraint, otherwise we discard the literal */
solve_con2([ [[L1,Name]|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
	ground(L1),
	abducible(L1),
	solve_abd([[not(L1),Name]],AbdIn,AbdOut1,Depth),
	select_literal_in_1st_ic(Rest_ic,Rest_ic1),
       	solve_con2(Rest_ic1,AbdOut1,AbdOut,Depth).

solve_con2([ [[L1,_Name]|Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
% discard the literal
        abducible(L1),!,
	select_literal(Rest_l,Rest_l1),
	solve_con2([Rest_l1|Rest_ic],AbdIn,AbdOut,Depth).


/* (7) L1 is not abducible and has no successful rule, then drop the branch */
solve_con2([ [[_L1,_]|_Rest_l] |Rest_ic],AbdIn,AbdOut,Depth):-
	select_literal_in_1st_ic(Rest_ic,Rest_ic1),
        solve_con2(Rest_ic1,AbdIn,AbdOut,Depth).


/* insert_nabd_1st: assumption: in Body the non abd are before the abd */

insert_nabd_1st([],Cl,Cl):-!.

insert_nabd_1st([[Nonabd,Name]|RestBody],RestIc,[[Nonabd,Name]|NewRestIc]):-
	\+(abducible(Nonabd)),!,
	insert_nabd_1st(RestBody,RestIc,NewRestIc).

insert_nabd_1st(RestBodyOfAbd,RestIc,NewIc):-
	appendc(RestIc,RestBodyOfAbd,NewIc).
/* when only abducible are lerft, they are added to the bottom of the ic */

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


new_const(H):- gensym('skolem_',H).
/*
	retract(new_const_number(N)),
	N1 is N + 1,
	assert(new_const_number(N1)),
	number_chars(N,Nstr),
	append("skolem",Nstr,Nconst),
	name(H,Nconst).

:-dynamic new_const_number/1.
	
new_const_number(0).
*/

and2list(A,[A]):-
	\+(A=(_B,_C)).


and2list((A,R),[A|T]):-
	and2list(R,T).
	
neg(not(Goal),Goal):-!.

neg(Goal,not(Goal)).

pred_of_lit(not(P),P):-!.

pred_of_lit(P,P).

add_cl_name([],[],_Name):-!.

add_cl_name([H|Body1],[[H,Name]|Body],Name):-
	add_cl_name(Body1,Body,Name).

complement_list([],[]).

complement_list([[L1,Name]|ListL1],[[not(L1),Name]|ListnotL1]):-
        complement_list(ListL1,ListnotL1).

/* output commands for acl1_1.pl
version 1
*/
title(File,FileOut,Stream):-
        max_spec_steps(Spec),
        der_depth(Der),
        beamsize(B),
        ver(V),
        verbosity(Ver),
        verapp(Vapp),
        min_cov(MC),
        stopping_acc(A),
        format(Stream,"~N/*~NM-ACL ver ~a AbdProofProc. ver ~a~N\c
Input file: ~a, Output file: ~a~N",
                [V,Vapp,File,FileOut]),
        format(Stream,"Max spec steps=~w, Beamsize=~w, Derivation depth=~w,\c
Verbosity=~w, ~NMin cov=~w, Accuracy stopping threshold=~3f~N",
                        [Spec,B,Der,Ver,MC,A]),
        eplus(Eplus),
        length(Eplus,Np),
        eminus(Eminus),
        length(Eminus,Nm),
        format(Stream,"~w positive examples, ~w negative examples~N*/~N",
                [Np,Nm]).


print_number_neg(Eminus):-
        length(Eminus,L),
        verbosity(V),
        V>0,
        format("Negative examples: ~w~N",[L]).

print_list([],_Stream):-!.

print_list([[Cl,Np,Npa,Nm,Nma,Epc,Epac,Emac,NewDelta]|T],Stream):-
        print_list(T,Stream),
        format(Stream," ~N",[]),
        print_clause(Stream,Cl,yes,yes,Np,Npa,Nm,Nma,Epc,Epac,Emac,NewDelta).

print_new_clause(Cl,GC,LC,Np,Npa,Nm,Nma,
                Epc,Epac,Emac,NewDelta):-
        verbosity(V),
        V>0,
        format(" ~N ~NGenerated Clause:~N",[]),
        print_clause(user_output,Cl,GC,LC,Np,Npa,Nm,Nma,Epc,Epac,Emac,NewDelta),
        (V>3->
                get0(_)
        ;
                true
        ).



print_clause(Stream,rule(H,B,Name),GC,LC,_Np,_Npa,_Nm,_Nma,
                Epc,Epac,Emac,NewDelta):-
        print_single_clause(Stream,H,B),
        format(Stream,"/* Name: ~a GC: ~a, LC: ~a \c
~NCovered positive examples: ~p~N\c
Covered positive abduced examples: ~p~N\c
Covered negative abduced examples: ~p~N\c
Abduced literals: ~p  */~N ~N",
                [Name,GC,LC,
                Epc,Epac,Emac,NewDelta]).

print_single_clause(Stream,H,B):-
        list2and(B,Ba),
        writevars(Stream,(H:-Ba)),write(Stream,'.'),nl(Stream).


print_backtracked_clauses(Stream):-
        findall(Clause,backtracked(Clause),L),
        format(Stream,"/* Backtracked clauses ~N",[]),
        \+ \+ ( numbervars(L,0,_,[singletons(true)]),print_list_backtracked(L,Stream)),
        format(Stream," ~N*/~N",[]).

print_list_backtracked([],_Stream):-!.

print_list_backtracked([rule(H,B,Name)|T],Stream):-
        print_list(T,Stream),
        format(Stream," ~N",[]),
        format(Stream,"~a: ",[Name]),
        print_single_clause(Stream,H,B).


print_ex_rem(Eplus,EplusA,Eminus):-
        verbosity(V),
        V>0,
        format("Current training set~N",[]),
        length(Eplus,Lp),
        format("Positive examples:         N+ =~w~NE+ =~p~N~N",[Lp,Eplus]),
        length(EplusA,LpA),
        format("Abduced positive examples: Na+=~w~NEa+=~p~N~N",[LpA,EplusA]),
        length(Eminus,Lm),
        format("Abduced negative examples: Na-=~w~NEa-=~p~N~N",[Lm,Eminus]).


print_agenda([]):-!,
        verbosity(V),
        (V>3->
                get0(_)
        ;
                true
        ).


print_agenda([H|T]):-
   \+ \+ ( numbervars(H,0,_,[singletons(true)]),
        format("[~p,~3f,~a,~a,~w,~w,~w,~w,~i]~N",H)),
        print_agenda(T).

print_max_spec_steps(Nmax):-
        verbosity(V),
        V>1,
        format("Reached the max number ~w of specializing steps~N",[Nmax]).

print_refinements:-
        verbosity(V),
        V>1,
        format(" ~NRefinements added to agenda: ~N",[]).

print_spec_step(N):-
        verbosity(V),
        V>1,
        format(" ~NSpecializing step n.~w.",[N]).

print_current_agenda(Agenda):-
        verbosity(V),
        V>2,
        format(" ~NCurrent Agenda:~N",[]),
        print_agenda(Agenda).

print_refinement([rule(H,B1,Name),Value,GC,LC,Np,Npa,Nm,Nma]):-
        verbosity(V),
        V>1,
        \+ \+ ( numbervars([rule(H,B1,Name),Value,GC,LC,Np,Npa,Nm,Nma],0,_,[singletons(true)]),
        format("[~p,~3f,~a,~a,~w,~w,~w,~w]~N",
                        [rule(H,B1,Name),Value,GC,LC,Np,Npa,Nm,Nma])).

print_loc_cons(Cl,Value):-
        verbosity(V),
        V>1,
        format("New locally consistent clause found:~N\c
        ~p Value=~w",[Cl,Value]).

print_backtracking(ClauseList):-
        verbosity(V),
        V>0,
        format("Backtracking: retracting clauses~N",[]),
        print_bkt_cl(ClauseList).

print_bkt_cl([]):-
        format(" ~N",[]).

print_bkt_cl([N|T]):-
        rule(H,B,N,Epa,Emc),
        \+ \+ ( numbervars(rule(H,B,N,Epa,Emc),0,_,[singletons(true)]),
        format("~p~N",[rule(H,B,N,Epa,Emc)])),
        print_bkt_cl(T).

writevars(Stream,Y):- portray_clause(Stream,Y),!.
/*writevars(Stream,X) :-
  freshcopy(X,Y),
  numbervars(Y,0,_L),
  write(Stream,Y).
% freshcopy(+X,-Y)
% get a copy of term X with fresh variables
freshcopy(X,Y) :-
  assert(newcopy(X)),
  retract(newcopy(Y)),
  !.
*/

