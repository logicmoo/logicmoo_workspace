:- set_prolog_flag(print_depth,1000).
:- set_prolog_flag(variable_names,off).

:- dynamic(count_inferences_pred/1).
:- dynamic(trace_search_progress_pred/1).
:- dynamic(compile_proof_printing/0).
:- dynamic(ncalls/1).

%%% Sound unification.
%%%
%%% `add_sound_unification' transforms a clause so that its
%%% head has no repeated variables.  Unifying a goal with
%%% the clause head can then be done soundly without the occurs
%%% check.  The rest of the unification can then be done in
%%% the body of the transformed clause, using the sound `unify'
%%% predicate.
%%%
%%% For example,
%%%    p(X,Y,f(X,Y)) :- true.
%%% is transformed into
%%%    p(X,Y,f(X1,Y1)) :- unify(X,X1), unify(Y,Y1).

add_sound_unification((Head :- Body),(Head1 :- Body1)) :-
        linearize(Head,Head1,[],_,true,Matches),
        conjoin(Matches,Body,Body1).

linearize(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut) :-
        nonvar(TermIn) ->
                functor(TermIn,F,N),
                myfunctor(TermOut,F,N),
                linearize_args(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut,1,N);
        identical_member(TermIn,VarsIn) ->
                VarsOut = VarsIn,
                conjoin(MatchesIn,unify(TermIn,TermOut),MatchesOut);
        %true ->
                TermOut = TermIn,
                VarsOut = [TermIn|VarsIn],
                MatchesOut = MatchesIn.

linearize_args(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut,I,N) :-
        I > N ->
                VarsOut = VarsIn,
                MatchesOut = MatchesIn;
        %true ->
                arg(I,TermIn,ArgI),
                linearize(ArgI,NewArgI,VarsIn,Vars1,MatchesIn,Matches1),
                arg(I,TermOut,NewArgI),
                I1 is I + 1,
                linearize_args(TermIn,TermOut,Vars1,VarsOut,Matches1,MatchesOut,I1,N).

%%% Sound unification algorithm with occurs check that is called
%%% by code resulting from the `add_sound_unification' transformation.
%%% This should be coded in a lower-level language for efficiency.

unify(X,Y) :-
        var(X) ->
                (var(Y) ->
                        X = Y;
                %true ->
                        functor(Y,_,N),
                        (N = 0 ->
                                true;
                        N = 1 ->
                                arg(1,Y,Y1), not_occurs_in(X,Y1);
                        %true ->
                                not_occurs_in_args(X,Y,N)),
                        X = Y);
        var(Y) ->
                functor(X,_,N),
                (N = 0 ->
                        true;
                N = 1 ->
                        arg(1,X,X1), not_occurs_in(Y,X1);
                %true ->
                        not_occurs_in_args(Y,X,N)),
                X = Y;
        %true ->
                functor(X,F,N),
                functor(Y,F,N),
                (N = 0 ->
                        true;
                N = 1 ->
                        arg(1,X,X1), arg(1,Y,Y1), unify(X1,Y1);
                %true ->
                        unify_args(X,Y,N)).

unify_args(X,Y,N) :-
        N = 2 ->
                arg(2,X,X2), arg(2,Y,Y2), unify(X2,Y2),
                arg(1,X,X1), arg(1,Y,Y1), unify(X1,Y1);
        %true ->
                arg(N,X,Xn), arg(N,Y,Yn), unify(Xn,Yn),
                N1 is N - 1, unify_args(X,Y,N1).

not_occurs_in(Var,Term) :-
        Var == Term ->
                fail;
        var(Term) ->
                true;
        %true ->
                functor(Term,_,N),
                (N = 0 ->
                        true;
                N = 1 ->
                        arg(1,Term,Arg1), not_occurs_in(Var,Arg1);
                %true ->
                        not_occurs_in_args(Var,Term,N)).

not_occurs_in_args(Var,Term,N) :-
        N = 2 ->
                arg(2,Term,Arg2), not_occurs_in(Var,Arg2),
                arg(1,Term,Arg1), not_occurs_in(Var,Arg1);
        %true ->
                arg(N,Term,ArgN), not_occurs_in(Var,ArgN),
                N1 is N - 1, not_occurs_in_args(Var,Term,N1).

%%% Depth-first iterative-deepening search.
%%%
%%% `add_complete_search' adds arguments DepthIn and DepthOut
%%% to each PTTP literal to control bounded depth-first
%%% search.  When a literal is called,
%%% DepthIn is the current depth bound.  When
%%% the literal exits, DepthOut is the new number
%%% of levels remaining after the solution of
%%% the literal (DepthIn - DepthOut is the number
%%% of levels used in the solution of the goal.)
%%%
%%% For clauses with empty bodies or bodies
%%% composed only of builtin functions,
%%% DepthIn = DepthOut.
%%%
%%% For other clauses, the depth bound is
%%% compared to the cost of the body.  If the
%%% depth bound is exceeded, the clause fails.
%%% Otherwise the depth bound is reduced by
%%% the cost of the body.
%%%
%%% p :- q , r.
%%% is transformed into
%%% p(DepthIn,DepthOut) :-
%%%     DepthIn >= 2, Depth1 is DepthIn - 2,
%%%     q(Depth1,Depth2),
%%%     r(Depth2,DepthOut).
%%%
%%% p :- q ; r.
%%% is transformed into
%%% p(DepthIn,DepthOut) :-
%%%     DepthIn >= 1, Depth1 is DepthIn - 1,
%%%     (q(Depth1,DepthOut) ; r(Depth1,DepthOut)).

add_complete_search((Head :- Body),(Head1 :- Body1)) :-
        Head =.. L,
        append(L,[DepthIn,DepthOut],L1),
        Head1 =.. L1,
        (functor(Head,query,_) ->
                add_complete_search_args(Body,DepthIn,DepthOut,Body1);
        nonzero_search_cost(Body,Cost) ->
                add_complete_search_args(Body,Depth1,DepthOut,Body2),
                conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),Body2,Body1);
        %true ->
                add_complete_search_args(Body,DepthIn,DepthOut,Body1)).

add_complete_search_args(Body,DepthIn,DepthOut,Body1) :-
        Body = (A , B) ->
                add_complete_search_args(A,DepthIn,Depth1,A1),
                add_complete_search_args(B,Depth1,DepthOut,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                search_cost(A,CostA),
                search_cost(B,CostB),
                (CostA < CostB ->
                        add_complete_search_args(A,DepthIn,DepthOut,A1),
                        add_complete_search_args(B,Depth1,DepthOut,B2),
                        Cost is CostB - CostA,
                        conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),B2,B1);
                CostA > CostB ->
                        add_complete_search_args(A,Depth1,DepthOut,A2),
                        add_complete_search_args(B,DepthIn,DepthOut,B1),
                        Cost is CostA - CostB,
                        conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),A2,A1);
                %true ->
                        add_complete_search_args(A,DepthIn,DepthOut,A1),
                        add_complete_search_args(B,DepthIn,DepthOut,B1)),
                disjoin(A1,B1,Body1);
        Body = search(Goal,Max,Min,Inc) ->
                PrevInc is Min + 1,
                add_complete_search_args(Goal,DepthIn1,DepthOut1,Goal1),
                DepthIn = DepthOut,
                Body1 = search(Goal1,Max,Min,Inc,PrevInc,DepthIn1,DepthOut1);
        Body = search(Goal,Max,Min) ->
                add_complete_search_args(search(Goal,Max,Min,1),DepthIn,DepthOut,Body1);
        Body = search(Goal,Max) ->
                add_complete_search_args(search(Goal,Max,0),DepthIn,DepthOut,Body1);
        Body = search(Goal) ->
                add_complete_search_args(search(Goal,1000000),DepthIn,DepthOut,Body1);
        functor(Body,search_cost,_) ->
                DepthIn = DepthOut,
                Body1 = true;
        builtin(Body) ->
                DepthIn = DepthOut,
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,[DepthIn,DepthOut],L1),
                Body1 =.. L1.

nonzero_search_cost(Body,Cost) :-
        search_cost(Body,Cost),
        Cost > 0.

%%% Search cost is computed by counting literals in the body.
%%% It can be given explicitly instead by including a number, as in
%%%   p :- search_cost(3).     (ordinarily, cost would be 0)
%%%   p :- search_cost(1),q,r. (ordinarily, cost would be 2)
%%%   p :- search_cost(0),s.   (ordinarily, cost would be 1)
%%%
%%% Propositional goals are not counted into the search cost so
%%% that fully propositional problems can be solved without
%%% deepening when iterative-deepening search is used.

search_cost(Body,N) :-
        Body = search_cost(M) ->
                N = M;
        Body = (A , B) ->
                (A = search_cost(M) ->  % if first conjunct is search_cost(M),
                        N = M;          % search cost of the entire conjunction is M
                %true ->
                        search_cost(A,N1),
                        search_cost(B,N2),
                        N is N1 + N2);
        Body = (A ; B) ->
                search_cost(A,N1),
                search_cost(B,N2),
                min(N1,N2,N);
        builtin(Body) ->
                N = 0;
        functor(Body,_,2) ->  % zero-cost 2-ary (0-ary plus ancestor lists) predicates
                N = 0;        % heuristic for propositional problems
        %true ->
                N = 1.

%%% Depth-first iterative-deepening search can be
%%% specified for a goal by wrapping it in a call
%%% on the search predicate:
%%%    search(Goal,Max,Min,Inc)
%%% Max is the maximum depth to search (defaults to a big number),
%%% Min is the minimum depth to search (defaults to 0),
%%% Inc is the amount to increment the bound each time (defaults to 1).
%%%
%%% Depth-first iterative deepening search can be
%%% specified inside the PTTP formula by compiling
%%%   query :- search(p(b,a,c),Max,Min,Inc)
%%% and executing
%%%   query.
%%% or directly by the user by compiling
%%%   query :- p(b,a,c))
%%% and executing
%%%   search(query,Max,Min,Inc).
%%%
%%% The search(Goal,Max,Min,Inc) predicate adds
%%% DepthIn and DepthOut arguments to its goal argument.

search(Goal,Max,Min,Inc) :-
        PrevInc is Min + 1,
        add_complete_search_args(Goal,DepthIn,DepthOut,Goal1),
        (compile_proof_printing ->
                add_proof_recording_args(Goal1,_Proof,_ProofEnd,Goal2);
        %true ->
                Goal2 = Goal1),
        !,
        search(Goal2,Max,Min,Inc,PrevInc,DepthIn,DepthOut).

search(Goal,Max,Min) :-
        search(Goal,Max,Min,1).

search(Goal,Max) :-
        search(Goal,Max,0).

search(Goal) :-
        search(Goal,1000000).

%%% Actual search driver predicate.
%%% Note that depth-bounded execution of Goal is enabled by
%%% the fact that the DepthIn and DepthOut arguments of
%%% search are also the DepthIn and DepthOut arguments of Goal.

search(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn,_DepthOut) :-
        Min > Max,
        !,
        fail.
search(Goal,_Max,Min,_Inc,PrevInc,DepthIn,DepthOut) :-
        trace_search_progress_pred(P1),
        L1 =.. [P1,Min],
        call(L1),
        DepthIn = Min,
        call(Goal),
        DepthOut < PrevInc.   % fail if this solution was found in previous search
search(Goal,Max,Min,Inc,_PrevInc,DepthIn,DepthOut) :-
        Min1 is Min + Inc,
        search(Goal,Max,Min1,Inc,Inc,DepthIn,DepthOut).

%%% Complete inference.
%%%
%%% Model elimination reduction operation and
%%% identical ancestor goal pruning.
%%%
%%% Two arguments are added to each literal, one
%%% for all the positive ancestors, one for all
%%% the negative ancestors.
%%%
%%% Unifiable membership is checked in the list 
%%% of opposite polarity to the goal
%%% for performing the reduction operation.
%%%
%%% Identity membership is checked in the list
%%% of same polarity as the goal
%%% for performing the ancestor goal pruning operation.
%%% This is not necessary for soundness or completeness,
%%% but is often effective at substantially reducing the
%%% number of inferences.
%%%
%%% The current head goal is added to the front
%%% of the appropriate ancestor list during the
%%% call on subgoals in bodies of nonunit clauses.

add_ancestor((Head :- Body),(Head1 :- Body1)) :-
        functor(Head,query,_) ->
                Head1 = Head,
                add_ancestor_args(Body,[[],[]],Body1);
        %true ->
                Head =.. L,
                append(L,[PosAncestors,NegAncestors],L1),
                Head1 =.. L1,
                add_ancestor_args(Body,[NewPosAncestors,NewNegAncestors],Body2),
                (Body == Body2 ->
                        Body1 = Body2;
                negative_literal(Head) ->
                        NewPosAncestors = PosAncestors,
                        conjoin((NewNegAncestors = [Head|NegAncestors]),Body2,Body1);
                %true ->
                        NewNegAncestors = NegAncestors,
                        conjoin((NewPosAncestors = [Head|PosAncestors]),Body2,Body1)).

add_ancestor_args(Body,AncestorLists,Body1) :-
        Body = (A , B) ->
                add_ancestor_args(A,AncestorLists,A1),
                add_ancestor_args(B,AncestorLists,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                add_ancestor_args(A,AncestorLists,A1),
                add_ancestor_args(B,AncestorLists,B1),
                disjoin(A1,B1,Body1);
        Body =.. [search,Goal|L] ->
                add_ancestor_args(Goal,AncestorLists,Goal1),
                Body1 =.. [search,Goal1|L];
        builtin(Body) ->
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,AncestorLists,L1),
                Body1 =.. L1.

ancestor_tests(P,N,Result) :-
        P == query ->
                Result = true;
        %true ->
                negated_functor(P,NotP),
                N2 is N - 2,            % N - 2 due to two ancestor-list arguments
                functor(Head1,P,N2),
                Head1 =.. [P|Args1],
                Head2 =.. [NotP|Args1],
                append(Args1,[PosAncestors,NegAncestors],Args),
                Head =.. [P|Args],
                (negative_functor(P) ->
                        C1Ancestors = NegAncestors, C2Ancestors = PosAncestors;
                %true ->
                        C1Ancestors = PosAncestors, C2Ancestors = NegAncestors),
                C1 = (Head :- identical_member(Head1,C1Ancestors), !, fail),
                count_inferences_pred(IncNcalls),
                (N2 = 0 ->              % special case for propositional calculus
                        conjoin((identical_member(Head2,C2Ancestors) , !),IncNcalls,V);
                %true ->
                        conjoin(unifiable_member(Head2,C2Ancestors),IncNcalls,V)),
                (compile_proof_printing ->
                        conjoin(V,infer_by(red),V1);
                %true ->
                        V1 = V),
                C2 = (Head :- V1),
                conjoin(C1,C2,Result).

procedures_with_ancestor_tests([[P,N]|Preds],Clauses,Procs) :-
        procedure(P,N,Clauses,Proc1),
        ancestor_tests(P,N,Tests),
        conjoin(Tests,Proc1,Proc),
        procedures_with_ancestor_tests(Preds,Clauses,Procs2),
        conjoin(Proc,Procs2,Procs).
procedures_with_ancestor_tests([],_Clauses,true).

identical_member(X,[Y|_])  :-           % run-time predicate for
        X == Y,                         % finding identical ancestor
        !.
identical_member(X,[_|L]) :-
        identical_member(X,L).

unifiable_member(X,[Y|_]) :-            % run-time predicate for
        unify(X,Y).                     % finding complementary ancestor
unifiable_member(X,[_|L]) :-
        unifiable_member(X,L).

%%% Proof Printing.
%%%
%%% Add extra arguments to each goal so that information
%%% on what inferences were made in the proof can be printed
%%% at the end.

add_proof_recording((Head :- Body),(Head1 :- Body1)) :-
        Head =.. L,
        append(L,[Proof,ProofEnd],L1),
        Head1 =.. L1,
        add_proof_recording_args(Body,Proof,ProofEnd,Body2),
        (functor(Head,query,_) ->
                conjoin(Body2,write_proved(Proof,ProofEnd),Body1);
        %true ->
                Body1 = Body2).

add_proof_recording_args(Body,Proof,ProofEnd,Body1) :-
        Body = (A , B) ->
                add_proof_recording_args(A,Proof,Proof1,A1),
                add_proof_recording_args(B,Proof1,ProofEnd,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                add_proof_recording_args(A,Proof,ProofEnd,A1),
                add_proof_recording_args(B,Proof,ProofEnd,B1),
                disjoin(A1,B1,Body1);
        Body =.. [search,Goal|L] ->
                add_proof_recording_args(Goal,Proof,ProofEnd,Goal1),
                Body1 =.. [search,Goal1|L];
        Body = infer_by(X) ->
                Body1 = (Proof = [X|ProofEnd]);
        Body = fail ->
                Body1 = Body;
        builtin(Body) ->
                Proof = ProofEnd,
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,[Proof,ProofEnd],L1),
                Body1 =.. L1.

write_proved(Proof,ProofEnd) :-
        write('proved by'),
        write_proof(Proof,ProofEnd).

write_proof(Proof,ProofEnd) :-
        Proof == ProofEnd,
        !.
write_proof([X|Y],ProofEnd) :-
        write(' '),
        write(X),
        write_proof(Y,ProofEnd).

%%% Negation normal form to Prolog clause translation.
%%% Include a literal in the body of each clause to
%%% indicate the number of the formula the clause came from.

clauses((A , B),L,WffNum) :-
        !,
        clauses(A,L1,WffNum),
        WffNum2 is WffNum + 1,
        clauses(B,L2,WffNum2),
        conjoin(L1,L2,L).
clauses(A,L,WffNum) :-
        head_literals(A,Lits),
        clauses(A,Lits,L,WffNum).

clauses(A,[Lit|Lits],L,WffNum) :-
        body_for_head_literal(Lit,A,Body1),
        (compile_proof_printing ->
                conjoin(infer_by(WffNum),Body1,Body);
        %true ->
                Body = Body1),
        clauses(A,Lits,L1,WffNum),
        conjoin((Lit :- Body),L1,L).
clauses(_,[],true,_).

head_literals(Wff,L) :-
        Wff = (A :- B) ->               % contrapositives are not formed for A :- ... inputs
                head_literals(A,L);
        Wff = (A , B) ->
                head_literals(A,L1),
                head_literals(B,L2),
                union(L1,L2,L);
        Wff = (A ; B) ->
                head_literals(A,L1),
                head_literals(B,L2),
                union(L1,L2,L);
        %true ->
                L = [Wff].

body_for_head_literal(Head,Wff,Body) :-
        Wff = (A :- B) ->
                body_for_head_literal(Head,A,A1),
                conjoin(A1,B,Body);
        Wff = (A , B) ->
                body_for_head_literal(Head,A,A1),
                body_for_head_literal(Head,B,B1),
                disjoin(A1,B1,Body);
        Wff = (A ; B) ->
                body_for_head_literal(Head,A,A1),
                body_for_head_literal(Head,B,B1),
                conjoin(A1,B1,Body);
        Wff == Head ->
                Body = true;
        negated_literal(Wff,Head) ->
                Body = false;
        %true ->
                negated_literal(Wff,Body).

%%% predicates returns a list of the predicates appearing in a formula.

predicates(Wff,L) :-
        Wff = (A :- B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        Wff = (A , B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        Wff = (A ; B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        functor(Wff,search,_) ->        % list predicates in first argument of search
                arg(1,Wff,X),
                predicates(X,L);
        builtin(Wff) ->
                L = [];
        %true ->
                functor(Wff,F,N),
                L = [[F,N]].

%%% procedure returns a conjunction of the clauses
%%% with head predicate P/N.

procedure(P,N,Clauses,Proc) :-
        Clauses = (A , B) ->
                procedure(P,N,A,ProcA),
                procedure(P,N,B,ProcB),
                conjoin(ProcA,ProcB,Proc);
        (Clauses = (A :- B) , functor(A,P,N)) ->
                Proc = Clauses;
        %true ->
                Proc = true.

%%% pttp is the PTTP compiler top-level predicate.

pttp(X) :-
        time(pttp1(X),'Compilation').

pttp1(X) :-
        nl,
        write('PTTP input formulas:'),
        apply_to_conjuncts(X,write_clause,_),
        nl,
        clauses(X,X0,1),
        apply_to_conjuncts(X0,add_count_inferences,X1),
        apply_to_conjuncts(X1,add_ancestor,X2),
        predicates(X2,Preds0),
        reverse(Preds0,Preds),
        procedures_with_ancestor_tests(Preds,X2,X3),
        apply_to_conjuncts(X3,add_sound_unification,X4),
        apply_to_conjuncts(X4,add_complete_search,X5),
        (compile_proof_printing ->
                apply_to_conjuncts(X5,add_proof_recording,Y);
        %true ->
                Y = X5),
        nl,
        write('PTTP output formulas:'),
        apply_to_conjuncts(Y,write_clause,_),
        nl,
        nl,

        File = 'temp.prolog',                     % Quintus Prolog on Sun
%       File = 'darwin:>stickel>pttp>temp.prolog',% change file name for other systems

        open(File,write,OutFile),
        write_clause_to_file(Y,OutFile),
        close(OutFile),
        compile(File),
        nl,
        !.

write_clause_to_file((A,B),OutFile) :-
	write_clause_to_file(A,OutFile),
	write_clause_to_file(B,OutFile),
        !.
write_clause_to_file(A,OutFile) :-
	nl(OutFile),
	write(OutFile,A),
	write(OutFile,.),
	!.


query(M) :-                             % call query with depth bound M
        compile_proof_printing -> 
                query(M,_N,_Proof,_ProofEnd);
        %true ->
                query(M,_N).

query :-                                % unbounded search of query
        query(1000000).

%%% Utility functions.

%%% Sometimes the `functor' predicate doesn't work as expected and
%%% a more comprehensive predicate is needed.  The `myfunctor'
%%% predicate overcomes the problem of functor(X,13,0) causing
%%% an error in Symbolics Prolog.  You may need to use it if
%%% `functor' in your Prolog system fails to construct or decompose
%%% terms that are numbers or constants.

myfunctor(Term,F,N) :-
        nonvar(F),
        atomic(F),
        N == 0,
        !,
        Term = F.
myfunctor(Term,F,N) :-
        nonvar(Term),
        atomic(Term),
        !,
        F = Term,
        N = 0.
myfunctor(Term,F,N) :-
        functor(Term,F,N).

nop(_).

append([],L,L).
append([X|L1],L2,[X|L3]) :-
        append(L1,L2,L3).

reverse([X|L0],L) :-
        reverse(L0,L1),
        append(L1,[X],L).
reverse([],[]).

union([X|L1],L2,L3) :-
        identical_member(X,L2),
        !,
        union(L1,L2,L3).
union([X|L1],L2,[X|L3]) :-
        union(L1,L2,L3).
union([],L,L).

intersection([X|L1],L2,[X|L3]) :-
        identical_member(X,L2),
        !,
        intersection(L1,L2,L3).
intersection([_X|L1],L2,L3) :-
        intersection(L1,L2,L3).
intersection([],_L,[]).

%%% min(X,Y,Min) :-
%%%        X =< Y ->
%%%                Min = X;
%%%        %true ->
%%%                Min = Y.

conjoin(A,B,C) :-
        A == true ->
                C = B;
        B == true ->
                C = A;
        A == false ->
                C = false;
        B == false ->
                C = false;
        %true ->
                C = (A , B).

disjoin(A,B,C) :-
        A == true ->
                C = true;
        B == true ->
                C = true;
        A == false ->
                C = B;
        B == false ->
                C = A;
        %true ->
                C = (A ; B).

negated_functor(F,NotF) :-
        name(F,L),
        name(not_,L1),
        (append(L1,L2,L) ->
                true;
        %true ->
                append(L1,L,L2)),
        name(NotF,L2).

negated_literal(Lit,NotLit) :-
        Lit =.. [F1|L1],
        negated_functor(F1,F2),
        (var(NotLit) ->
                NotLit =.. [F2|L1];
        %true ->
                NotLit =.. [F2|L2],
                L1 == L2).

negative_functor(F) :-
        name(F,L),
        name(not_,L1),
        append(L1,_,L).

negative_literal(Lit) :-
        functor(Lit,F,_),
        negative_functor(F).

apply_to_conjuncts(Wff,P,Wff1) :-
        Wff = (A , B) ->
                apply_to_conjuncts(A,P,A1),
                apply_to_conjuncts(B,P,B1),
                conjoin(A1,B1,Wff1);
        %true ->
                T1 =.. [P,Wff,Wff1],
                call(T1).

write_clause(A) :-
        nl,
        write(A),
        write(.).

write_clause(A,_) :-                    % 2-ary predicate can be used as
        write_clause(A).                % argument of apply_to_conjuncts

%%% Inference counting is turned on by count_inferences,
%%% off by dont_count_inferences.
%%%
%%% Inferences are counted by retracting the current count
%%% and asserting the incremented count, so inference counting
%%% is very slow.

count_inferences :-                     % enables compilation of inference counting
        retract(count_inferences_pred(_)),% this slows down the code substantially
        fail.
count_inferences :-
        assert(count_inferences_pred(inc_ncalls)).

dont_count_inferences :-                % disables compilation of inference counting
        retract(count_inferences_pred(_)),% this is the default for acceptable performance
        fail.
dont_count_inferences :-
        assert(count_inferences_pred(true)).

:- dont_count_inferences.               % default is to not count inferences

%%% Transformation to add inference counting to a clause.

add_count_inferences((Head :- Body),(Head :- Body1)) :-
        functor(Head,query,_) ->
                Body1 = Body;
        %true ->
                count_inferences_pred(P),
                conjoin(P,Body,Body1).

clear_ncalls :-
        retract(ncalls(_)),
        fail.
clear_ncalls :-
        assert(ncalls(0)).

inc_ncalls :-
        retract(ncalls(N)),
        N1 is N + 1,
        assert(ncalls(N1)),
        !.

%%% Search tracing is turned on by trace_search,
%%% off by dont_trace_search.

trace_search :-                         % enables search progress reports
        retract(trace_search_progress_pred(_)),
        fail.
trace_search :-
        assert(trace_search_progress_pred(write_search_progress)).

dont_trace_search :-                    % disables search progress reports
        retract(trace_search_progress_pred(_)),
        fail.
dont_trace_search :-
        assert(trace_search_progress_pred(nop)).

:- trace_search.                        % default is to trace searching

write_search_progress(Level) :-
        ncalls(N),
        (N > 0 -> write(N) , write(' inferences so far.') ; true),
        nl,
        write('Begin cost '),
        write(Level),
        write(' search...  ').

%%% Proof printing is turned on by print_proof,
%%% off by dont_print_proof.
%%%
%%% print_proof or dont_print_proof should be
%%% executed before the problem is compiled.

print_proof :-                          % enable proof printing
        retract(compile_proof_printing),
        fail.
print_proof :-
        assert(compile_proof_printing).

dont_print_proof :-                     % disable proof printing
        retract(compile_proof_printing),
        fail.
dont_print_proof.

:- print_proof.                         % default is to print proof

%%% A query can be timed by time(query).

time(X) :-
        time(X,'Execution').

time(X,Type) :-
        clear_ncalls,

        statistics(runtime,[T1,_]),     % Quintus Prolog on Sun
%        T1 is get-internal-run-time,  % Common Lisp time function

        call(X),

        statistics(runtime,[T2,_]),     % Quintus Prolog on Sun
        Secs is (T2 - T1) / 1000.0,     % Quintus measures runtime in milliseconds
%        T2 is get-internal-run-time,  % Common Lisp time function
%        Secs is (T2 - T1) / 977.0,      % internal-time-units-per-second on Darwin

        nl,
        write(Type),
        write(' time: '),
        ncalls(N),
        (N > 0 -> write(N) , write(' inferences in ') ; true),
        write(Secs),
        write(' seconds, including printing'),
        nl.

%%% List of builtin predicates that can appear in clause bodies.
%%% No extra arguments are added for ancestor goals or depth-first
%%% iterative-deepening search.  Also, if a clause body is
%%% composed entirely of builtin goals, the head is not saved
%%% as an ancestor for use in reduction or pruning.
%%% This list can be added to as required.

builtin(T) :-
        functor(T,F,N),
        builtin(F,N).

builtin(!,0).
builtin(true,0).
builtin(fail,0).
builtin(succeed,0).
builtin(trace,0).
builtin(atom,1).
builtin(integer,1).
builtin(number,1).
builtin(atomic,1).
builtin(constant,1).
builtin(functor,3).
builtin(arg,3).
builtin(var,1).
builtin(nonvar,1).
builtin(call,1).
builtin(=,2).
builtin(\=,2).
builtin(==,2).
builtin(\==,2).
builtin(>,2).
builtin(<,2).
builtin(>=,2).
builtin(=<,2).
builtin(is,2).
builtin(display,1).
builtin(write,1).
builtin(nl,0).
builtin(infer_by,_).
builtin(write_proved,_).
builtin(search,_).
builtin(search_cost,_).
builtin(unify,_).
builtin(identical_member,_).
builtin(unifiable_member,_).
builtin(inc_ncalls,0).

%%% Theorem proving examples from
%%%   Chang, C.L. and R.C.T. Lee.
%%%   Symbolic Logic and Mechanical Theorem Proving.
%%%   Academic Press, New York, 1973, pp. 298-305.

%%% Note that the search driver predicate
%%% can be invoked by search(query) as in
%%% chang_lee_example8 or can be explicitly
%%% included in the query as in chang_lee_example2.

chang_lee_example2 :-
        nl,
        write(chang_lee_example2),
        pttp((
                p(e,X,X),
                p(X,e,X),
                p(X,X,e),
                p(a,b,c),
                (p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
                (p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
                (query :- search(p(b,a,c)))
        )),
        time(query).

chang_lee_example8 :-
        nl,
        write(chang_lee_example8),
        pttp((
                l(1,a),
                d(X,X),
                (p(X) ; d(g(X),X)),
                (p(X) ; l(1,g(X))),
                (p(X) ; l(g(X),X)),
                (not_p(X) ; not_d(X,a)),
                (not_d(X,Y) ; not_d(Y,Z) ; d(X,Z)),
                (not_l(1,X) ; not_l(X,a) ; p(f(X))),
                (not_l(1,X) ; not_l(X,a) ; d(f(X),X)),
                (query :- (p(X) , d(X,a)))
        )),
        time(search(query)).








