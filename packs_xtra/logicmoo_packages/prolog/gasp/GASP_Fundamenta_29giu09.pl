%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GASP 03/07/2009 - for Fundamenta
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WRT LaSh08 version
%%% 1. Added rule indexing for speeding up fixpoints
%%%    (roughly 35% faster)
%%% 2. Added constraint with negated rule for avoiding the permutation problem
%%%    (inspired from asperix)
%%% 3. Added CSP based propagation of false head to positive body, both for constraints and/or rules with false head in the model
%%% 4. Constraints handled separately (they are not seen as applicable rules anymore) and 
%%%    processed only by propagators and tp/wf at each iteration of search
%%% 5. Added CSP based propagation of unsupported heads (from undefined to false) if every rule with a specific ground head
%%%    cannot produce the atom. We assume that undefined atoms in body+ are true and undefined atoms in body- are false, to preserve correctness
%%% 6. At each application of a rule, added a fixpoint between the two propagation steps and the tp operator.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Libraries, operators, and general parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(file_systems)).
:- op(200,fx,not).
:- op(150,fx,'1').
:- op(100,xf,'1').
:- op(300,xfy,neq).
:- op(300,xfy,eq).

%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% OPTIONAL: setting the working directory 
 my_cd('C:/agostino/lavori/incorso/gasp').
 :-my_cd(WD),file_systems:current_directory(_,WD).

:-prolog_flag(unknown,_,fail).  


%%% NOTE: maxint=268435455 for clpfd.
%%% ARITY 4 bigM(127).     
%%% ARITY 3 

bigM(645).  

%%% ARITY 2 bigM(16383).


%%% BUILT-INS

builtin('<').  
builtin('>').  
builtin('=').
builtin('==').
builtin('eq').
builtin('neq').

%%% go/1 and /2 compute all stable models, go/3 one per time 
%%% They take the Program as input and computes stable model(s)
%%% Launch as :- go('filename.lp').  [default: all solutions and printed]
%%% or :- go('filename.lp',write).   [The same]
%%% or :- go('filename.lp',nowrite). [solutions are hidden]
%%% or :- go('filename.lp',Model, write/nowrite) [One solution per time]
%%% or :- go('filename.lp',Model, write/nowrite),gasp_statistics. [for statistics]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go(Program) :-
     go(Program,write).

go(Program,WriteFlag) :-
     go(Program,_,WriteFlag),
     fail.
go(_,_) :-
     gasp_statistics.
     
go(Program,Model,WriteFlag) :-
     reset,
     read_program(Program,ProgList),
     empty_domains(ProgList,IniDoms),
     split_facts(ProgList,Facts,Rules,Flag), %%% separates facts and rules and 
     dep_graph(IniDoms,Rules), %%% generate the predicate dependency graph 
     writetime('### Loading time: '),        %%% tests if it is definite (Flag)   
     facts(Facts,IniDoms,FactDoms),          %%% FactDoms: facts are true
     (  Flag=0, % definite
        tp(Rules,FactDoms,Model),
         writeln('### P is a definite program. Its minimum model is:'),
         update(model)
         ,write_model(WriteFlag,Model)
       ;
       (Flag=1;Flag=2), %%% =1 without constraints =2 with constraints                   
         wf(Rules, FactDoms, WFModel), 
         %%write('DEBUG: wellfounded model'),write(Flag),
         %%write_model(write,WFModel),
         ( wf_stable(WFModel,Flag),!,     %
           writeln('### There is a wellfounded model'),
           update(model), 
           write_model(WriteFlag,WFModel),!,
           Model=WFModel           
           ;
           writeln('### There are no wellfounded models.       ###'),
           dom_update([false],WFModel,WFModel1),  %% for 
           fixpoint(Rules,WFModel1,Model),  
           update(model), %%% it updates sol(N)
           writetime('### Next stable model computed in time: '),%%% 
           write_model(WriteFlag,Model)
         )  
         ;   
         Flag=3, %%% =3 with function(s): particular cases                   
         wf(Rules, FactDoms, WFModel), 
         fun_fixpoint(Rules,WFModel,Model,Program),
         update(model),
         writetime('### Next Stable model computed in: ')%%% 
         ,write_model(WriteFlag,Model)
     ).
           
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MODELS:
%%% Each predicate p of arity N is assigned to a term
%%% - atom(p,N,POSDOM,NEGDOM)
%%% where POSDOM,NEGDOM are fdsets.
%%% - POSDOM is the set of true values for p
%%% - NEGDOM is the set of false values for p
%%% If the program is definite, then NEGDOM=[]
%%% otherwise NEGDOM contains elements to satisfy negative 
%%% literals when looking for stable models
%%% A model is a list of those atoms (actually, the list of POSDOM)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% For arity 0 predicates, DOM =[] means false, DOM = [0] true
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% empty_domains sets to [] all domains for non-builtin predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_domains([],[]).
empty_domains([rule(A,B,C)|R], DOMS) :-
    append([A,B,C], ATOMS),
    atom_dom(ATOMS, DOMS1),
    empty_domains(R, DOMS2),
    disj_union(DOMS1,DOMS2,DOMS).

atom_dom([],[atom(false,0,[],[])]). %%%
atom_dom([A|R],S) :-
    A =.. [P|_], builtin(P),!,
    atom_dom(R,S).
atom_dom([A|R],[atom(P,N,[],[])|S]) :-
    A =.. [P|Args], length(Args,N),
    atom_dom(R,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Standard T_P of logic programming (see below notes for negative
%%% literals)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tp(ProgList,I,M) :-
     tp_step(ProgList,I,I1,[],F),!, %% tp_step is deterministic.
     (F=[],!,  M = I1; %% we added a ! to avoid to enter
      F=[0],! , fail;   %% there is no rule 0: fails    
      %tp(Prog,I1,M)).        %%% Naive version       
      choose_rules(F,RULES,1), tp(RULES,I1,M)). %% FASTER, with indexing  

tp_step([],I,I,F,F).
tp_step(_,_I,_I1,[0],[0]):-!. %% as soon as get an inconsistency -> break
tp_step([Rule|ProgList],I,I1,F0,F2) :-
     apply_def_rule(Rule,I,Inew,F0,F1),
     tp_step(ProgList,Inew,I1,F1,F2).
     
choose_rules(F,RULES_real,Flag) :- %%% Flag=2 means that:
      sort(F,Fsort),               %%% only the positive part of the
      ( Flag=1, findall(RULES,     %%% program is used 
              (member(A,Fsort),dependency(A,_,RULES)), RULES_F);
        Flag=2, findall(RULES,  
              (member(A,Fsort),posdependency(A,_,RULES)), RULES_F)),
      append(RULES_F,RULES_Flat),  %%% append unario. Lento?            
      sort(RULES_Flat,RULES_Flat2),
      map_rules(RULES_Flat2,RULES_real).
      
map_rules([],[]).
map_rules([Num|NR],[Rule|RR]) :-  
      rule_no(Num,Rule),
      map_rules(NR,RR).
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% update_dom computes the effect of T_P on a single rule
%%%%% T_P considers also negation for its use in S.M. search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_def_rule(rule([H],PBody,NBody),I,[atom(F,ARITY,NEWPDOM,NEG)|PARTI],F0,F1) :-
      copy_term([H,PBody,NBody],[H1,PBody1,NBody1]),  %%% variable renaming
      term_variables([H1,PBody1],VARS),       %%% collect fresh vars
      bigM(M),M1 is M-1,domain(VARS,0,M1),    %%% fix (wide) domains for the fresh vars
      H1 =..[F|ARGS],
      tuple_num(ARGS,VAR,ARITY),
      select(atom(F,ARITY,OLD,NEG),I,PARTI),
      %%%
      (
      build_constraint(PBody1,I,1,pos),      %%% C3 = 1 iff the positive body is satisfied by I
      build_constraint(NBody1,I,1,negknown), %%% C4 = 1 iff the negative body is satisfied by I
      nin_set(ARITY,VAR,OLD),      %%% do not apply if head is already true 
%      nin_set(ARITY,VAR,NEG),      %%% Rimosso consistency check, devo applicare e poi fallire, non skip dell'applicazione della regola!
      findall(X,(X #= VAR,labeling([ff],VARS)),[L|IST]),      
      ( check_consistent([L|IST],ARITY,NEG),
        list_to_fdset([L|IST],SET),
        fdset_union(OLD,SET,NEWPDOM),
        %%% UPDATE
        F1=[F|F0],!
        ;
        %%% check_consistent fails on the domain updating: 
        F1=[0]), !
        ;
        %%% Nothing to update
        NEWPDOM=OLD, F1 = F0
      ).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Computing Standard (definite) T_P
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% build_constraint returns a constraint that must hold for
%%%% the rule is applied
%%%% Negative literals treatment: if Sign=negknown,
%%%% it is implemented as:
%%%%% -   A in T_P(I,J) iff (A :- B, not C) in P and
%%%%%     B in I and C in J
%%%%%  (I true facts, J false facts)
%%%% Otherwise it is left as a constraint (neg) as called by
%%%% the wellfounded part.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  build_constraint is used for computing wellfouned sets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
build_constraint([R|Rs],I,C,Sign):-  %%% Built-in ATOMS          
      R =.. [F|ARGS],
      builtin(F),!, 
      expression(F,ARGS,C2),
      C  #<=>  C2 #/\ C1,
      build_constraint(Rs,I,C1,Sign).
build_constraint([R|Rs],I,C,Sign):-  %%% Other ATOMS           
      R =.. [F|ARGS],!,
      tuple_num(ARGS,VAR,ARITY),
      member(atom(F,ARITY,DOMF,NDOMF),I),
      (
       Sign=notneg,!,     %%% Negative unknown 
       nin_set(ARITY,VAR,NDOMF,C2);
       Sign=neg,!,     %%% Negative unknown 
       nin_set(ARITY,VAR,DOMF,C2);
       Sign=pos,!,     %%% Positive
       C2  #<=> VAR in_set DOMF;
       Sign=negknown,  %%% Negative known 
       C2 #<=> VAR in_set NDOMF;
       Sign=nowhere,!,  %%% not in positive and not in negative
       nin_set(ARITY,VAR,DOMF,C3),
       nin_set(ARITY,VAR,NDOMF,C4),
       C2 #<=> C3 #/\ C4
       ),  
       build_constraint(Rs,I,C1,Sign),
       C #<=>  C1 #/\ C2.
build_constraint([],_,1,_).
      
      
%%% special constraint: computes the number of atoms in the list R|Rs are in I+
%%%   and set it to HowMany (exclude the expressions, for which only constraints are set). 
%%% Moreover every el in list R|Rs is not in I-
%%% Pos is unified with the position in the List of the atom that is not in I+
%%% TotalRules is a ground value that account for the number of non expression atoms (useful to know how many non-expression atoms are satisfiable at most)

build_constraint_one_nonground([R|Rs],I,HowMany, TotalRules, Pos, Ct):-
      R =.. [F|ARGS],
      builtin(F),!, 
      expression(F,ARGS,1),
      Pos #\= Ct,
      Ct1 is Ct+1,
      build_constraint_one_nonground(Rs,I,HowMany,TotalRules, Pos, Ct1).
  
build_constraint_one_nonground([R|Rs],I,HowMany, TotalRules1, Pos, Ct):-
      R =.. [F|ARGS],
      tuple_num(ARGS,VAR,ARITY),
      member(atom(F,ARITY,DOMF,NDOMF),I),
      nin_set(ARITY,VAR,NDOMF,1),      %% never negative
%      in_set(ARITY,VAR,DOMF,C3),      
      C3 #<=> VAR in_set DOMF,
      HowMany #= C3 + HowMany1,      
      C3 #= 0 #<=> Pos #=Ct,
      Ct1 is Ct+1,
      TotalRules1 #= TotalRules+1,
      build_constraint_one_nonground(Rs,I,HowMany1,TotalRules, Pos, Ct1).

build_constraint_one_nonground([],_I,0, 0, _Pos, _Ct).

      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% facts adds the domain values imposed by (ground) facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

facts([],DOMS,DOMS).
facts([rule([A],[],[])|R],OLDDOMS,NEWDOMS) :-
       A =..[P|ARGS],
       tuple_num(ARGS,D,ARITY),
       select(atom(P,ARITY,PDOM,NEG),OLDDOMS,PARTDOMS),
       fdset_add_element(PDOM,D,NEWPDOM),
       facts(R,[atom(P,ARITY,NEWPDOM,NEG)|PARTDOMS],NEWDOMS).      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% well founded models computed as described in:
%%% Improving the Alternating Fixpoint: The Transformation Approach
%%% by Zukowski, Brass, and Freitag
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wf(ProgList,Empty, M) :-
      def_rules(ProgList,DefProg),
      alternating(2,DefProg, Empty,Empty,K0), %% K0 = lfp(T_P)
      alternating(1,ProgList, K0,  Empty,U0), %% U0 = lpf(T_{P,K0})
      wf(ProgList,Empty, K0, U0, M).

wf(ProgList,Empty,Ki,Ui,M) :-
     alternating(1,ProgList, Ui, Empty,Ki1), % Ki1 = lfp(T_{P,U_{i}})
     ( Ki1==Ki, !, 
       wellfoundedmodel(Ki,Ui,M);
       alternating(1,ProgList, Ki1,Empty,Ui1), % Ui1 = lfp(T_{P,K_i1})
       wf(ProgList,Empty,Ki1,Ui1,M)
     ).
            
%%% W*_P = K union {not(A) : A in U}   
      
wellfoundedmodel([],_,[]).
wellfoundedmodel([atom(P,AR,PDOM,_)|K],U,[atom(P,AR,PDOM,NDOM)|M]) :-
      select(atom(P,AR,UDOM,_),U,RU),
      complement_set(AR,UDOM,NDOM),
      wellfoundedmodel(K,RU,M). 

%%% The first call is with the definite program (Flag=2):
%%% particular case. The other with Flag=1
  
alternating(Flag,ProgList,J,I,M) :-
      wf_step(ProgList,J,I,I1,[],F),!,
      (F = [], !, M = I;
       %%%% Naive VERSION alternating(Flag, ProgList,J,I1,M)). %  
        choose_rules(F,RULES,Flag),  %%% Faster, with indexing 
        alternating(Flag,RULES,J,I1,M)).
        
wf_step([],_J,I,I,F0,F0).
wf_step([Rule|ProgList],J,I,I1,F0,F2) :-
      alternating_apply_rule(Rule,J,I,Inew,F0,F1),
      wf_step(ProgList,J,Inew,I1,F1,F2).
      
%%% T_{P,J}(I) = { A : (A :- PBody, neg NBody) in P ,
%%%                     PBody \subseteq I and
%%%                     NBody \cap J = \emptyset }

alternating_apply_rule(rule([H],PBody,NBody),J,I,[atom(F,ARITY,NEWPDOM,NEG)|PARTI],F0,F1) :-
      copy_term([H,PBody,NBody],[H1,PBody1,NBody1]),  %%% variable renaming
      term_variables([H1,PBody1],VARS),    %%% collect fresh vars
      bigM(M),M1 is M-1,domain(VARS,0,M1), %%% fix (wide) domains for the fresh vars
      H1 =..[F|ARGS],
      tuple_num(ARGS,VAR,ARITY),
      select(atom(F,ARITY,OLD,NEG),I,PARTI),
      ( build_constraint(PBody1,I,1,pos),   %%% C = 1 iff the body is satisfied by I
        build_constraint(NBody1,J,1,neg), 
        nin_set(ARITY,VAR,OLD,1),      %%% do not apply if head is already true 
      %%% nin_set(ARITY,VAR,NEG,C4),  %%% consistency check
        findall(X,(X #= VAR,labeling([ff],VARS)),[L|IST]),
        list_to_fdset([L|IST],SET),
        fdset_union(OLD,SET,NEWPDOM),
        F1=[F|F0], !;
        NEWPDOM=OLD, F1 = F0
      ).
                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Non determinsitic fixpoint procedure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fixpoint(Rules,I1,I2) :-
%       propagate(Rules,I1,I11,_),
       fixpoint_tp_propagate(Rules,I1,I11),
%write_fdmodel_complete(I11),
       lev_fixpoint(Rules,I11,I2,[],0).

lev_fixpoint(Rules,I1,I2, PrevRules,Level) :-
       statistics(runtime,_), %%% TIMINGS
       collect_applicable_rules(Rules,I1,ApplRules), %%% slow point       
       
       update(time_applicable), %%% TIMINGS
       (ApplRules=[],  %%% writeln('No appl rules'),
         !, I2=I1; %%%IT1=I2; %% no rules ->  stable model        
        
        %%% remove the rules that would be failing (head is false in I1). if the remainder is empty this is not a stable model!        
        remove_head_conflict_rules(ApplRules,ApplRulesOK),
        %%% remove from applicable rules the ones that are already applied (they would be failing here)
        %%% compute the sublist (left) to be appened as next PrevRules (collect the left part for each parent)
        diff_list(ApplRulesOK,PrevRules,ActualApplRules),      %%% diff_list -> n log n 
%write_fdmodel_complete(I1),
%writeln([applrules,lev,Level]),        
%write_applrules(ApplRulesOK),
        sublist(ActualApplRules,[GRULE],LeftRulesIdx,1,_),
        prefix_length(ActualApplRules,LeftRules,LeftRulesIdx),  %% get the left set of rules        
        append(PrevRules,LeftRules,NewPrevRules),
        get_bodyMinus(LeftRules,NewLeftProgram),
        append(Rules,NewLeftProgram,NewRules),

        apply_rule_1(GRULE, I1, IN, _),
        update(nodes), %%% TIMINGS

%writeln([Level,GRULE]),        
        %% collect list of already used rules
        statistics(runtime,_), %%% TIMINGS

%write_fdmodel_complete(IN),
        fixpoint_tp_propagate(NewRules,IN,IN3),

%writeln([ok]),

        update(time_prop), %%% TIMINGS
        Level1 is Level+1,
        lev_fixpoint(NewRules,IN3,I2,NewPrevRules,Level1)
      ).

remove_head_conflict_rules([[_,0]|R],R1):-
  remove_head_conflict_rules(R,R1).
remove_head_conflict_rules([[Rule,1]|R],[Rule|R1]):-
  remove_head_conflict_rules(R,R1).
remove_head_conflict_rules([],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% "MODEL" Checking procedures %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% wf_stable/2
%%% Tests if a well-founded model is complete, hence stable

wf_stable([],_).
wf_stable([atom(false,_,_,_)|M],1) :- %% particular case
      !, wf_stable(M,1). %% for constraints-free programs
wf_stable([atom(_,AR,PDOM,NDOM)|M],Flag) :-
      complement_set(AR,PDOM,NDOM),
      wf_stable(M,Flag). 

%%%%%%%% check_consistent/3      
%%% fails if one element in the list is in the negative domain     

check_consistent([],_ARITY,_NEG).
check_consistent([L|Rest],ARITY,NEG):-
     nin_set(ARITY,L,NEG),
     check_consistent(Rest,ARITY,NEG).
      
%%%%%%%%% constraints_check/1
%%% Tests if the constraints are verified.
%%% Observe that for each constraint :- Goal
%%% we add a rule
%%% false :- Goal.
%%% When false is present in a model ->
%%% a constraint has been falsified (Goal is true in the Model)
%%% Using apply_rule1 should be useless

constraints_check(Model) :-
    member(atom(false,0,[],_),Model).     

%%%% get_bodyMinus
% retieves the negative part of the bodies
      
get_bodyMinus([],[]).
get_bodyMinus([rule(_H,_BP,[false|BM])|LeftRules],[rule([false],[],[false|BM])|NewLeftConstraints]):-!,
  get_bodyMinus(LeftRules,NewLeftConstraints).
get_bodyMinus([rule(_H,_BP,BM)|LeftRules],[rule([false],[],[false|BM])|NewLeftConstraints]):-
  get_bodyMinus(LeftRules,NewLeftConstraints).
      
%%%%% collect_applicable_rules/3
%% collects all the applicable rules given the interpretation I.
%% Notice the (non standard) the use of findall/4 to avoid an append
%% HeadNoConflict states whether the application of the rule produces a head that is already in the model as positive/undefined or in the negative form

collect_applicable_rules([Rule|Rules],I,List):-       
       collect_applicable_rules(Rules,I,B),
       findall(Output,(apply_rule(Rule,I, _M, GroundRule,HeadNoConflict),
               Output=[GroundRule,HeadNoConflict]
%      ,constraints_check(M)
              ),List,B).
collect_applicable_rules([],_I,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% non deterministic rule application: apply_rule (_1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% used for building the set of applicable rules (constraints included)
%% cannot include apply_rule_1, otherwise if a constraint cannot be satisfied,
%% it results that the rule is not applicable

apply_rule(rule([H],Bpos,Bneg),I1,I2,rule([H1],Bpos1,Bneg1),HeadNoConflict) :-

      statistics(runtime,_), %%% TIMINGS

      Bneg \= [], %%% Must have negative literals
      copy_term([H,Bpos,Bneg],[H1,Bpos1,Bneg1]), 
      H1 =.. [F|ARGS],  
      term_variables([H1,Bpos1],VARS), %%% All vars are in the positive part    
      bigM(M), M1 is M-1, domain(VARS,0,M1),               
      build_constraint(Bpos1,I1,1,pos), %%% 1 means positive part is satisfied
      build_constraint(Bneg1,I1,1,neg), 
      tuple_num(ARGS,Arg,ARITY),
      member(atom(F,ARITY,PDOM,NDOM),I1),
      nin_set(ARITY,Arg,PDOM),
      nin_set(ARITY,Arg,NDOM,HeadNoConflict),  %% flag to export that states wheter the application of the rule contradicts the model (helps in deciding when the stable model / failure is reached)
      
              
      update(time_csp), %%% TIMINGS
      statistics(runtime,_), %%% TIMINGS
      labeling([ff],VARS), %%% Positive part satisfied
      update(time_labeling), %%% TIMINGS
      dom_update(Bneg1,I1,I2). %%% Update negative part            

%%% apply_rule_1 unfolds apply_rule optimizing the constraint case
  
apply_rule_1(rule([false],Bpos,Bneg),I1,I2,rule([false],Bpos1,Bneg1)) :- !,
      Bneg \= [], %%% Must have negative literals
      copy_term([Bpos,Bneg],[Bpos1,Bneg1]), 
      sub_list(Bneg1, BnegNOC, BnegC),
      BnegC=[_|_], %% at least one element in BnegC   
      term_variables(Bpos1,VARS), %%% All vars are in the positive part    
      bigM(M), M1 is M-1, domain(VARS,0,M1),               
      build_constraint(Bpos1,I1,1,pos), %%% 1 means positive part is satisfied
      build_constraint(BnegNOC,I1,1,neg), %%% a part of the negative part is satisfied
      build_constraint(BnegC,I1,1,pos), %%% the rest is falsified   
      labeling([ff],VARS), %%% Positive part satisfied
      dom_update(Bneg1,I1,I2). %%% Update negative part            

apply_rule_1(rule([H],Bpos,Bneg),I1,I2,rule([H1],Bpos1,Bneg1)) :-
      H\=false,!,
      Bneg \= [], %%% Must have negative literals
      copy_term([H,Bpos,Bneg],[H1,Bpos1,Bneg1]), 
      H1 =.. [F|ARGS],  
      term_variables([H1,Bpos1],VARS), %%% All vars are in the positive part    
      bigM(M), M1 is M-1, domain(VARS,0,M1),               
      build_constraint(Bpos1,I1,1,pos), %%% 1 means positive part is satisfied
      build_constraint(Bneg1,I1,1,neg), %%% 1 means negative part is satisfied
      tuple_num(ARGS,Arg,ARITY),
      member(atom(F,ARITY,PDOM,_),I1),
      nin_set(ARITY,Arg,PDOM), %%% This avoids to use the same rule twice
      labeling([ff],VARS), %%% Positive part satisfied
      dom_update(Bneg1,I1,I2). %%% Update negative part            

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%           PROPAGATION            %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fixpoint_tp_propagate(NewRules,I,IOut):-
        tp(NewRules,I,IN2),
%writeln(tpOk),
        constraints_check(IN2),
%writeln(checkOk),
        update(time_tp), %%% TIMINGS                            
        statistics(runtime,_), %%% TIMINGS (inutile?)
        %%% uncomment one of the two following lines (activate propagator)
        propagate(NewRules,IN2,IN3,Changed),
        (Changed=0,!,
         IOut=IN3;
         fixpoint_tp_propagate(NewRules,IN3,IOut)        
        ).

%%% for each rule, build a CSP that produces the ground literals that are set to false
%%% if there is a body+ completely ground, but one literal
%%% AND the head is false
%%% Changed >0 if a changed happened
propagate(Rules,I1,I4,Changed):-
      propagate1(Rules,I1,I2,Changed1),  %%% head false -> try to imply a false predicate in the body
      propagate2(Rules,I2,I2,I3,Changed2),  %%% undef head (ground) -> try to set it = false (no support)
      (Changed1+Changed2#>0,!,propagate(Rules,I3,I4,Changed3),Changed#=Changed1+Changed2+Changed3;
       I3=I4,Changed#=Changed1+Changed2).


propagate2(_,[],M,M,0).
propagate2(Rules,[Atom|R],In,Out,Changed):-
  actual_propagation2(Atom,Rules,In,In1,Changed1), 
  Changed#=Changed1+Changed2,
  propagate2(Rules,R,In1,Out,Changed2).
 

%%% for each undef domain value for the predicate P
%%% check if this is supported by at least one rule
%%% assuming that undef val are set to true in positive body and to false in negative body
%%% if no support for any rule -> the atom is set to false

actual_propagation2(atom(P,Arity,Dom,NDom),Rules,In,In1,Changed):-
%writeln(atom(P,Arity,Dom,NDom)),      
   %% slow point
   get_rules_with_head(Rules,P,Arity,List),
%writeln([rules,List]),
   compute_support(P,Arity,In,List,ListPotentiallySupported),
%writeln([supported,ListPotentiallySupported]),
   complement_set(Arity,Dom,CDom),
   fdset_subtract(CDom,NDom,UDom), %indefinito         
%writeln([undef,UDom]),

   (ListPotentiallySupported=[],!,NewNegatives=UDom;   
   %% Udom - ListPotentiallySupported => become negative -> dom_update In-->In1
   list_to_fdset(ListPotentiallySupported,SET),
   fdset_subtract(UDom,SET,NewNegatives)
   ),
   
%writeln([becomeNeg,NewNegatives]),      
    (NewNegatives=[],!,In1=In,Changed=0;
     select(atom(P,Arity,PDOM,NEGDOM),In,Irest),
     fdset_union(NEGDOM,NewNegatives,NEWDOM),          %% if D is already in NEGDOM
     In1=[atom(P,Arity,PDOM,NEWDOM)|Irest],
     Changed=1
    )
   
   .

%%% given a rule and model I1 compute the tuples supported by the rule
compute_support(_P,_Arity,_I1,[],[]).
compute_support(P,Arity,I1,[rule([H],Bpos,Bneg)|R],R2):-
      copy_term([H,Bpos,Bneg],[H1,Bpos1,Bneg1]), 
      H1 =.. [F|ARGS],  
      term_variables(Bpos1,VARS), %%% All vars are in the positive part    
      bigM(M), M1 is M-1, domain(VARS,0,M1),               
      build_constraint(Bpos1,I1,1,notneg), %%% positive body can be true/undefined but no in negative (otherwise no applicability)
      build_constraint(Bneg1,I1,1,neg),    %%% negative body can be false/undefined but no in positive (otherwise no applicability)
      
      %% the head is currently unknown
      tuple_num(ARGS,Arg,ARITY),
      member(atom(F,ARITY,PDOM,NDOM),I1),
      nin_set(ARITY,Arg,PDOM),
      nin_set(ARITY,Arg,NDOM),              
      findall(Head,(labeling([ff],VARS),Head=Arg),Supported),   
      append(Supported,R1,R2),
      compute_support(P,Arity,I1,R,R1).
compute_support(P,Arity,I1,[_|R],R1):-!,
   compute_support(P,Arity,I1,R,R1).


%%% get_rules_with_head/3:
%%% given a predicate and arity, collects all the rules with matching head 

get_rules_with_head([],_P,_Arity,[]).
get_rules_with_head([rule([H],PBody,NBody)|Rules],P,Arity,[rule([H],PBody,NBody)|R1]):- 
   H =.. [F|ARGS],
   tuple_num(ARGS,_,ARITY),
   F=P,
   ARITY=Arity,!,
   get_rules_with_head(Rules,P,Arity,R1).
get_rules_with_head([_|Rules],P,Arity,R1):-    
   get_rules_with_head(Rules,P,Arity,R1).


propagate1([],I,I,0).
propagate1([R|Rules],I,I1,Changed1):-!,
      actual_propagation(R,I,I2,Changed),
      propagate1(Rules,I2,I1,Changed2),
      Changed1#=Changed+Changed2.
    
%%% propagation rule: CSP with 
%%% if Bp and Bn are in the model I, then enforce Pp and/or Pn in the new model

actual_propagation(rule([false],Bpos,Bneg),I1,I2,Changed) :-
%writeln([prop,rule([false],Bpos,Bneg)]),
      copy_term([Bpos,Bneg],[Bpos1,Bneg1]), 
%      delete(Bneg1,false,Bneg2),
      term_variables([Bpos1],VARS), %%% All vars are in the positive part    
      bigM(M), M1 is M-1, domain(VARS,0,M1),               
      length(Bpos1,Len),
      domain([Pos],1,Len),
      build_constraint_one_nonground(Bpos1,I1,NTrue,MaxTrue,Pos,1), %%% count how many are in I+, and we set it to Len-1. all of them are NOT in I-. Pos is the position of the only undefined literal
      NTrue #= MaxTrue -1,
      build_constraint(Bneg1,I1,1,neg), %%% 1 means negative part is satisfied
      
      findall(Ground,(labeling([ff],[Pos|VARS]),nth1(Pos,Bpos1,Ground)
%,writeln([Ground,':-',Bpos1,Bneg1])
               ),[L|IST]),
      dom_update([L|IST],I1,I2), %%% Update negative part
      Changed=1,
      !.
      
actual_propagation(rule([H],Bpos,Bneg),I1,I2,Changed) :-
      H\=false,
%writeln([prop,rule([H],Bpos,Bneg)]),
%trace,
      copy_term([H,Bpos,Bneg],[H1,Bpos1,Bneg1]), 
%      delete(Bneg1,false,Bneg2),
      H1 =.. [F|ARGS],  
      term_variables(Bpos1,VARS), %%% All vars are in the positive part    
      tuple_num(ARGS,Arg,ARITY),
      member(atom(F,ARITY,_PDOM,NDOM),I1),
      Arg in_set NDOM,
      length(Bpos1,Len),
      domain([Pos],1,Len),
      build_constraint_one_nonground(Bpos1,I1,NTrue,MaxTrue,Pos,1), %%% count how many are in I+, and we set it to Len-1. all of them are NOT in I-. Pos is the position of the only undefined literal
      NTrue #= MaxTrue -1,
      build_constraint(Bneg1,I1,1,neg), %%% 1 means negative part is satisfied      
      findall(Ground,(labeling([ff],[Pos|VARS]),nth1(Pos,Bpos1,Ground)
%,writeln([Ground,[H1],':-',Bpos1,Bneg1])
                     ),[L|IST]),
      dom_update([L|IST],I1,I2) %%% Update negative part
      ,Changed=1
      ,!
      .

actual_propagation(_P,I,I,0). %% if fails -> no propagation for rule


%%%actual_propagation([Bp,Bn,Pp,Pn],I,I2):-
%%%      copy_term([Bp,Bn,Pp,Pn],[Bp1,Bn1,Pp1,Pn1]), 
%%%      term_variables([Bp1,Pp1],VARS), %%% 
%%%      bigM(M), M1 is M-1, domain(VARS,0,M1),               
%%%      build_constraint(Bp1,I,1,pos), %%% 1 means positive part is satisfied
%%%      build_constraint(Bn1,I,1,negknown), %%% 1 means negative part is satisfied
%%%       (Pn1==[],!, %% Pp1 not in pos AND not in neg
%%%        build_constraint(Pp1,I,1,nowhere) %%% nowhere -> the atom is not yet decided
%%%        ;
%%%        Pp1==[],
%%%        build_constraint(Pn1,I,1,nowhere) %%% nowhere
%%%       ),     %
%%%%
%%%
%%%       (Pn1==[],!, %% Pp1 not in pos AND not in neg
%%%      findall(X,(labeling([ff],VARS),[X]=Pp1),[L|IST]),      
%%%        dom_pos_update([L|IST],I,I2)
%%%        ;
%%%        Pp1==[],
%%%      findall(X,(labeling([ff],VARS),[X]=Pn1),[L|IST]),      
%%%writeln([prop ,[L|IST]]),
%%%        dom_update([L|IST],I,I2)
%%%       ),!.
%%%
%%%%%% if no propagation is possible (fail while building constraints) -> no model change
%actual_propagation(_P,I,I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% domain updating
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dom_update([],I,I).
dom_update([A|R],I1,I2) :-      
      A =.. [F|ARGS],
      tuple_num(ARGS,D,ARITY),
      select(atom(F,ARITY,PDOM,NEGDOM),I1,Irest),
      fdset_add_element(NEGDOM,D,NEWDOM),          %% if D is already in NEGDOM
      dom_update(R,[atom(F,ARITY,PDOM,NEWDOM)|Irest],I2).%% then NEWDOM=NEGDOM
           
dom_pos_update([],I,I).
dom_pos_update([A|R],I1,I2) :-      
      A =.. [F|ARGS],
      tuple_num(ARGS,D,ARITY),
      select(atom(F,ARITY,PDOM,NEGDOM),I1,Irest),
      fdset_add_element(PDOM,D,NEWPDOM),          
      dom_neg_update(R,[atom(F,ARITY,NEWPDOM,NEGDOM)|Irest],I2).%% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% expression parses algebraic expressions into FD expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            
expression(F,[TX,TY],C2) :-
       term_conv(TX,TERMX),
       term_conv(TY,TERMY),  
       (F = '=', !,   C2 #<=>  TERMX #= TERMY;       
        F = 'eq', !,  C2 #<=>  TERMX #= TERMY;       
        F = '==',!,   C2 #<=>  TERMX #= TERMY;       
        F = '<', !,   C2 #<=>  TERMX #< TERMY;
        F = '>', !,   C2 #<=>  TERMX #> TERMY;
        F = 'neq', !, C2 #<=>  TERMX #\= TERMY).       

term_conv(V,V) :- var(V),!.
term_conv(N,N) :- number(N),!.
%% binary functions
term_conv(TI,TO) :- 
      TI =.. [OP,A,B],!,
      term_conv(A,A1), term_conv(B,B1),
      (OP = '+',  !,  TO #= A1 + B1;
       OP = '-',  !,  TO #= A1 - B1;
       OP = '/',  !,  TO #= A1 / B1;
       OP = 'mod',!,  TO #= A1 mod B1;
       OP = '*',      TO #= A1 * B1).
%% unary functions       
term_conv(TI,TO) :- 
      TI =.. [OP,A],
      term_conv(A,A1),
      (OP = 'abs',  !,  TO #= abs(A1);
       OP = '-', !, TO #= -(A1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%%%%%%%%    PARSING PHASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%(ok)%%%%%%%%%%%%% read_program/2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Given a prolog file name ProgFile
%%%%% output a list of clauses in the form
%%%%% rule([H],Body+,Body-)
%%%%% where H is rule head atom
%%%%% Body+ is the list of positive atoms in body
%%%%% Body- is the list of atoms occuring negated in body
%%%%% example ProgList=[rule([p],[q,r(X)],[s(X,a)]),   %%% p :- q,r(X),not s(X,a).
%%%%%                   rule([p],[],[q]),              %%% p :- not q.
%%%%%                   rule([],[a],[b]), ]            %%% :- a, not b.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_program(ProgFile,ProgList) :-
    see(ProgFile),
    read_all(ProgList),
    writeln('### Program read without errors            ###'),
    writeln(ProgFile),
    seen.

read_all([Rule|ProgList]) :-
    read(Clause),
    Clause \== end_of_file, !,
    (  Clause = ('1'({ A : B })'1' :- C),!, %%% Aggregate clause
       split_pos_neg(C,P,N),                %%% restricted syntax
       Rule = rule([A],[B|P],N)             %%% Use with parsimony
    ;
       Clause = (A :- B),!, %% Rules with Body
       split_pos_neg(B,P,N),
       Rule = rule([A],P,N)
     ;
       Clause = (:- B),!,    %%% Constraints 
       split_pos_neg(B,P,N), %%% Introduced faked rules
       Rule = rule([false],P,[false|N])
     ;
       Rule = rule([Clause],[],[])), %% Facts
    read_all(ProgList).
read_all([]).

%(ok)%%%%%%% input rules split_pos_neg/3 and split_facts/3

split_pos_neg((not C1,D),R1,[C1|R2]):- !,
   split_pos_neg(D,R1,R2).
split_pos_neg((C,D),[C|R1],R2):- !,
   split_pos_neg(D,R1,R2).
split_pos_neg(not A,[],[A]):- !.
split_pos_neg(P,[P],[]).

%%%% The 4th parameter of split_facts is 
%%%% 0 for definite program, 
%%%% 1 for general programs without constraints and
%%%% 2 for programs with constraints but without functions
%%%% 3 for programs with functions 

split_facts([],[],[],0).
split_facts([rule([INTERVAL],[],[])|ProgList],   %%% This is the case of "domain" 
            [rule([H1],[],[])|Facts],            %%% predicates, such as p(1..3). 
            [rule([H2],[H3, X = Y + 1, X < B + 1],[])|Rules],ND):- 
       INTERVAL =.. [P,A..B], !,    
       H1 =..[P,A], H2 =..[P,X], H3 =..[P,Y],     
       split_facts(ProgList,Facts,Rules,ND).
split_facts([rule([FUNCTION],_,_)|ProgList], Facts,Rules,3):- %%% case of functions 
       FUNCTION =.. [assignment|_],!,
       split_facts(ProgList,Facts,Rules,_).
split_facts([rule(A,[],[])|ProgList],[rule(A,[],[])|Facts],Rules,ND):- !,
       split_facts(ProgList,Facts,Rules,ND).
split_facts([rule(A,B,[])|ProgList], Facts,[rule(A,B,[])|Rules],ND):- !,
       split_facts(ProgList,Facts,Rules,ND).
split_facts([rule([false],B,C)|ProgList],Facts,[rule([false],B,C)|Rules],Flag):- !,
       split_facts(ProgList,Facts,Rules,ND),
       Flag is max(ND,2).
split_facts([NRULE|ProgList], Facts,[NRULE|Rules],Flag):-
       split_facts(ProgList,Facts,Rules,ND),
       Flag is max(ND,1).
       
def_rules([],[]).
def_rules([rule(_,_,[_|_])|ProgList], Defrules) :-
      !, def_rules(ProgList,Defrules).
def_rules([RULE|ProgList],[RULE|Defrules]) :-
      def_rules(ProgList,Defrules).

%%%% dep_graph/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% It generates a dependency graph.
%%%% Each rule is assigned to an integer
%%%% and asserted as rule_no(NUMBER,RULE)
%%%% Each predicate p of arity AR is assigned 
%%%% to the list of rules where it appears "positively"
%%%% in the body. Facts of the form
%%%% dependency(p,AR,[(rule)N1,...,(rule)Np).
%%%% Moreover,  a fact of the form
%%%% posdependency(p,AR,[(rule)N1,...,(rule)Np).
%%%% is also asserted to link the definite clauses
%%%% where p occur in the body.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dep_graph(IniDoms,Rules):-
    retractall(dependency(_,_,_)),
    retractall(posdependency(_,_,_)),    
    retractall(rule_no(_,_)),
    list_rules(Rules,1),
    dep_graph_aux(IniDoms,Rules).

list_rules([],_).
%%% Currently, constraints are used as
%%% other rules    
%%% list_rules([rule([false],_,_)|R],N) :- !,  list_rules(R,N).
list_rules([RULE|R],N) :-
    assert(rule_no(N,RULE)),
    M is N + 1,
    list_rules(R,M).
    
dep_graph_aux([],_).
dep_graph_aux([atom(false,_,_,_)|R],ProgList) :- !,
   dep_graph_aux(R,ProgList). 
dep_graph_aux([atom(P,N,_,_)|R],ProgList) :-
    functor(Atom,P,N),
    findall(K, (  member(rule(A,B,C),ProgList),                  
                  memberchk(Atom,B),
                  %%%append(B,C,D),memberchk(Atom,D),
                  rule_no(K,RULE1),
                  copy_term(rule(A,B,C),RULE1) ), 
            RULES), 
   assert(dependency(P,N,RULES)), %% avoid multiple occs
   findall(H, (  member(rule(A1,B1,[]),ProgList),                  
                  memberchk(Atom,B1),
                  rule_no(H,RULE2),
                  copy_term(rule(A1,B1,[]),RULE2) ), 
            POSRULES), 
   assert(posdependency(P,N,POSRULES)), %% avoid multiple occs
   %%% write(dependency(P,N,RULES)),nl,                                  
   dep_graph_aux(R,ProgList).
     

%remove_false([],[]).
%remove_false([false|R],T):-!,
%     remove_false(R,T).
%remove_false([A|R],[A|T]):-
%     %%%A\=false,!,
%     remove_false(R,T).

%% for each element, produce a constraint
%% if a predicate is the only one not in the model -> force the false version of it
loop_for_each_el(_,[],_,_,L,L).
loop_for_each_el(Ra,[R|Rs],[],BN,L,L1):-
     R =.. [F|_ARGS], %% builtin -> no propagation
     builtin(F),!,
     loop_for_each_el([R|Ra],Rs,[],BN,L,L1).
loop_for_each_el(Ra,[R|Rs],[],BN,L,L1):-
     append(Ra,Rs,RR),
     loop_for_each_el([R|Ra],Rs,[],BN,[[RR,BN,[],[R]]|L],L1).
       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%      AUXILIARY PREDICATES    %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% tuple_num returns the code VAL associated to a tuple
%%%% and the arity AR of a tuple
%%%% it works for N <= 3. It is easy to be extended
%%%% It is called by most of the predicates

tuple_num(ARGS,VAL,ARITY) :-
       (ARGS = [],!, VAL=0, ARITY=0;    
        ARGS = [VAL],!,  ARITY=1;
        ARGS = [Arg1,Arg2],!, bigM(M), ARITY=2, VAL #= M*Arg1+Arg2;
        ARGS = [Arg1,Arg2,Arg3], bigM(M), ARITY=3, VAL #= M*M*Arg1+M*Arg2+Arg3).

%%% num_tuple it the reverse function (from VAL to ARGS)

num_tuple(ARGS,VAL,ARITY) :-
       (ARITY=0,!, ARGS = [], VAL=0 ;    
        ARITY=1,!, ARGS = [VAL] ;
        ARITY=2,!, bigM(M), Arg1 is VAL//M, Arg2 is VAL mod M, ARGS = [Arg1,Arg2];
        ARITY=3,!, bigM(M), 
                   Arg1 is  VAL//(M*M), 
                   Arg2 is (VAL-Arg1*M*M)//M,
                   Arg3 is  VAL mod M, 
                   ARGS = [Arg1,Arg2,Arg3]).
                   
%%% pair_proj is bi-directional and works for pairs

pair_proj(PAIR,X,Y) :-
        bigM(M),
        PAIR #= M*X + Y,
        X #= PAIR/M,
        Y #= PAIR mod M.
        
triple_proj(TRIPLE,X,Y,Z) :-
        bigM(M),
        TRIPLE #= M*M*X + M*Y + Z,
        X #= TRIPLE/M/M,
        Y #= (TRIPLE/M) mod M,
        Z #= TRIPLE mod M.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% nin_set/4 sets the constraint C to 1 iff
%% X is not in SET.
%% nin_set/3 adds the constraint X is not in SET.

nin_set(ARITY,X,SET,C) :-
      complement_set(ARITY,SET,COMPL),    
      C #<=> X in_set COMPL.

nin_set(ARITY,X,SET) :-
      complement_set(ARITY,SET,COMPL),    
      X in_set COMPL.

%% complement set computes the complement wrt  a universe set
%% of the fd_set UDOM. The universe set changes according to the 
%% arity

complement_set(AR,UDOM,NDOM) :-
      bigM(M),
      (AR=0,!,VAL=0;
       AR=1,!,VAL is M -1;
       AR=2,!,VAL is M*M -1;
       AR=3,VAL is M*M*M -1),
       fdset_interval(Universe,0,VAL), 
       %write(acca(Universe,0,VAL,UDOM)),
       fdset_subtract(Universe,UDOM,NDOM).

%%% diff_list(X,Y,Z) returns in Z the list of elements in X and not in Y
%%% diff_list(X,Y,Z,K) returns in Z the list of elements in X and not in Y, K is sort(X),
  
%%% nlogn version (if sort is n log n)
diff_list(ApplRules,PrevRules,NewRules) :-
        sort(PrevRules,SortedPrevRules), %% SORT is not important here. 
        sort(ApplRules,SortedApplRules), %% Only to fast the rest
        extract_new_rules(SortedPrevRules,SortedApplRules,NewRules).
diff_list(ApplRules,PrevRules,NewRules,SortedApplRules) :-
        sort(PrevRules,SortedPrevRules), %% SORT is not important here. 
        sort(ApplRules,SortedApplRules), %% Only to fast the rest
        extract_new_rules(SortedPrevRules,SortedApplRules,NewRules).
extract_new_rules([A|R1],[A|R2],R3):- !,
        extract_new_rules(R1,R2,R3).
extract_new_rules([A|R1],[B|R2],R3):-
        compare(<,A,B),!,
        extract_new_rules(R1,[B|R2],R3).
extract_new_rules([A|R1],[B|R2],[B|R3]):-
      % compare(>,A,B),!,
        extract_new_rules([A|R1],R2,R3).
extract_new_rules([],A,A):-!.
extract_new_rules(_,[],[]).

%%% disj_union appends two lists and sort them without repetitions

disj_union(A,B,C) :-
       append(A,B,D),
       sort(D,C). %%% remove_dups(D,C).  

%%% computes non-deterministically P(List) i.e. lists with elements from input list
%%% third arg -> complement    

sub_list([],[],[]).
sub_list([A|R],[A|R1],R2):-
       sub_list(R,R1,R2).
sub_list([A|R],R1,[A|R2]):-
       sub_list(R,R1,R2).

%%% 

increasing(List) :-
    lex_chain([List],[increasing]),
    labeling([],List).

%%% Reset of global parameters (timings, number of solutions)

reset :- 
   statistics(runtime,[It,_]),
   %% RETRACT ALL OLD VALUES
   retractall(sol(_)),
   retractall(it(_)),
   retractall(time_applicable(_)),
   retractall(time_implicit(_)),
   retractall(time_tp(_)),
   retractall(time_csp(_)),
   retractall(time_prop(_)),
   retractall(time_labeling(_)),
   retractall(nodes(_)),
   retractall(last_time(_)),
   %% ASSERT INITIAL VALUES
   assert(sol(0)),
   assert(it(It)),
   assert(time_applicable(0)),
   assert(time_implicit(0)),
   assert(time_tp(0)),
   assert(time_csp(0)),
   assert(time_prop(0)),
   assert(time_labeling(0)),
   assert(nodes(0)),
   assert(last_time(It)).

%%% To skip timings, uncomment the following line:
%%% update(_) :-!.
              
update(model) :- !,
      retract(sol(N)), N1 is N+1,
      assert(sol(N1)).
update(nodes) :- !,
      retract(nodes(N)), N1 is N+1,
      assert(nodes(N1)).
update(F) :- %%% for various time_...
      statistics(runtime,[_,A]),            
      Tin =.. [F,B],
      retract(Tin),
      C is A+B,
      Tout =.. [F,C],
      assert(Tout).
       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%    PRINTING PREDICATES   %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeln(A) :- write(A),nl.

writetime(String) :-
     statistics(runtime,[T,_]),
     last_time(T1),
     T2 is T-T1,
     retract(last_time(T1)),
     assert(last_time(T)),
     write(String),write(T2),writeln(' ms ###').

%%%% write_model/2: if the flag is "write" prints a stable model 

write_model(write,MOD) :-
   !,
   writeln('###############################################'),
   writeln('############# Computed model ##################'),
   writeln('###############################################'),
   write_fdmodel(MOD),
   writeln('###############################################').
write_model(_,_) :-
   writeln('### Computed model not printed, as requested ###').
   
write_fdmodel([]).
%write_fdmodel([atom(false,0,_,_)|R]):- !,
%   write_fdmodel(R).
write_fdmodel([atom(P,0,Dom,_)|R]):-
   format("### ~a/0: ",[P]),
   (Dom=[],!, writeln(false); writeln(true)),
    write_fdmodel(R).
write_fdmodel([atom(P,1,Dom,_)|R]):-
   format("### ~a/1: ",[P]),
   fdset_to_list(Dom,LDom), writelist(LDom),nl,
   write_fdmodel(R).
write_fdmodel([atom(P,2,Dom,_)|R]):-
   format("### ~a/2: ",[P]),
   fdset_to_list(Dom,LDom),write_proj(2,LDom),nl,
   write_fdmodel(R).
write_fdmodel([atom(P,3,Dom,_)|R]):-
   format("### ~a/3: ",[P]),
   fdset_to_list(Dom,LDom), write_proj(3,LDom),nl,
   write_fdmodel(R).

write_fdmodel_complete([]).
%write_fdmodel_complete([atom(false,0,_,_)|R]):- !,
%   write_fdmodel_complete(R).
write_fdmodel_complete([atom(P,0,Dom,NDom)|R]):-
   format("### ~a/0: ",[P]),
   (Dom=[],!, write('?'); write(true)),
   (NDom=[],!, write('?'); write(false)),
   nl,
    write_fdmodel_complete(R).
write_fdmodel_complete([atom(P,1,Dom,NDom)|R]):-
   format("### ~a/1: ",[P]),
   fdset_to_list(Dom,LDom), 
   complement_set(1,Dom,CDom),
%   write(cacca(Dom,CDom)),nl,
   fdset_subtract(CDom,NDom,UDom), %indefinito         
   fdset_to_list(UDom,LUDom), 
   writelist(LDom),
   write(' undef'),
   writelist(LUDom),
   nl,
   write_fdmodel_complete(R).
write_fdmodel_complete([atom(P,2,Dom,NDom)|R]):-
   format("### ~a/2: ",[P]),
   fdset_to_list(Dom,LDom),
   complement_set(2,Dom,CDom),
%   write(cacca(Dom,CDom)),nl,
   fdset_subtract(CDom,NDom,UDom), %indefinito         
   fdset_to_list(UDom,LUDom),
   write_proj(2,LDom),
   write(' undef'),
   write_proj(2,LUDom),   
   nl,
   write_fdmodel_complete(R).
write_fdmodel_complete([atom(P,3,Dom,NDom)|R]):-
   format("### ~a/3: ",[P]),
   fdset_to_list(Dom,LDom), write_proj(3,LDom),
   complement_set(3,Dom,CDom),
%   write(cacca(Dom,CDom)),nl,
   fdset_subtract(CDom,NDom,UDom), %indefinito         
   write(' undef'),
   fdset_to_list(UDom,LUDom), write_proj(3,LUDom),
   nl,
   write_fdmodel_complete(R).


write_proj(_,[]):-!.
write_proj(2,[D|R]) :-!,
   bigM(M), X is D//M, Y is D mod M,
   format("(~d,~d) ",[X,Y]),
   write_proj(2,R).
write_proj(3,[D|R]) :-!,
   bigM(M), X is D//(M*M), Y is (D-X*M*M)//M, Z is D mod M,
   format("(~d,~d,~d) ",[X,Y,Z]),   
   write_proj(3,R).
writelist([]).
writelist([D|R]) :-
   write(D),write(' '), 
   writelist(R).   

gasp_statistics :-
     sol(N), statistics(runtime,[T,_]),
     it(IT), TotT is (T - IT)/1000,
     time_applicable(TimeA),TotTimeA is TimeA/1000,
     time_implicit(TimeI),TotTimeI is TimeI/1000,
     time_tp(TimeT),TotTimeT is TimeT/1000,
     time_csp(TimeC),TotTimeC is TimeC/1000,
     time_prop(TimeP),TotTimeP is TimeP/1000,
     time_labeling(TimeL),TotTimeL is TimeL/1000,
     nodes(Nodes),
     format("### Computed  ~d  stable model(s) in  ~2F s ###\n",[N,TotT]),
     format("### Time for implicit rule ~2F s            ###\n",[TotTimeI]),
     format("### Time for applicable rule ~2F s          ###\n",[TotTimeA]),
     format("### Time for csp ~2F s                      ###\n",[TotTimeC]),
     format("### Time for labeling ~2F s                 ###\n",[TotTimeL]),
     format("### Time for propagation ~2F s              ###\n",[TotTimeP]),
     format("### Time for tp + check ~2F s               ###\n",[TotTimeT]),
     format("### Nodes ~d                                ###\n",[Nodes]),
     write('############################################ END').  

     
write_applrules([]).
write_applrules([R|Ules]):-
  writeln(R),
  write_applrules(Ules).
     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
%%% EXTRA CODE 1. SIMPLE ASP COMPUTATION (without any ordering control)   
%%% It can replace fixpoint predicate 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slow_fixpoint(Rules,I0,M) :-
      (nd_choice(Rules,I0,I1),
       tp(Rules,I1,I2),  
       constraints_check(I2),
       slow_fixpoint(Rules,I2,M);
       stable(Rules,I0),!, M=I0).

nd_choice([Rule|_], I1,I2 ) :- 
      apply_rule(Rule, I1, I2,_,_).
nd_choice([_|Rules],  I1, I2 ) :- 
      nd_choice(Rules,  I1, I2).

stable(Rules,I0) :-
      constraints_check(I0),
      \+ nd_choice(Rules,I0,_).
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% BETA PART: Functions-driven fixpoint, 

fun_fixpoint(Rules,I1,I2,Program) :-
    writeln('### Started function part '),
    functions(I1,Inew,Program), %%% Constraint-based function   
    tp(Rules,Inew,Ineww),
    constraints_check(Ineww),
    fixpoint(Rules,Ineww,I2).

functions(WFModel, [atom(assignment,2,ASS,NDOM)|RModel],Program) :-
           member(atom(domain,1,NUM,_),WFModel),  %%% general: function domain 
           member(atom(range,1,RAN,_),WFModel),   %%% general: function range  
           select(atom(assignment,2,_,NDOM),WFModel,RModel), %%% general: function
           fdset_size(NUM,L), 
           length(Xs,L), length(Ys,L),
           funbuild(Xs,Ys,NUM,RAN,FUN), %%% general: map of (X,Y)
           increasing(Xs), %% general: removes symmetries and grounds the domain
           %% all_different(Ys), %%% use for one-to-one functions
           constraint_adhoc(Program,FUN,RModel),
           labeling([],FUN),
           list_to_fdset(FUN,ASS).
functions(WFModel, [atom(assignment,3,ASS,NDOM)|RModel],Program) :-
           member(atom(domain,1,NUM,_),WFModel),  %%% general: function domain 
           member(atom(range1,1,RAN1,_),WFModel),   %%% general: function range  
           member(atom(range2,1,RAN2,_),WFModel),   %%% general: function range  
           select(atom(assignment,3,_,NDOM),WFModel,RModel), %%% general: function
           fdset_size(NUM,L), 
           length(Xs,L), length(Ys,L), length(Zs,L),
           funbuild(Xs,Ys,Zs,NUM,RAN1,RAN2,FUN), %%% general: map of (X,Y,Z)
           increasing(Xs), %% general: removes symmetries and grounds the domain
           %% all_different(Ys), %%% use for one-to-one functions
           constraint_adhoc(Program,FUN,RModel),
           labeling([],FUN),
writeln(FUN),		   
           list_to_fdset(FUN,ASS).
funbuild([],[],_,_,[]).   
funbuild([X|Xs],[Y|Ys],DOM,RAN,[D|Fun]) :-
           X in_set DOM, Y in_set RAN,
           tuple_num([X,Y],D,2),
           funbuild(Xs,Ys,DOM,RAN,Fun).
funbuild([],[],[],_,_,_,[]).   
funbuild([X|Xs],[Y|Ys],[Z|Zs],DOM,RAN1,RAN2,[D|Fun]) :-
           X in_set DOM, Y in_set RAN1,Z in_set RAN2,
           tuple_num([X,Y,Z],D,3),
           funbuild(Xs,Ys,Zs,DOM,RAN1,RAN2,Fun).
           
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AD-HOC PART
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constraint_adhoc(Program,FUN,Model) :-
          ( 
          Program='test5.lp' -> write(constraintSchur), constraint_schur(FUN),!;
%          Program='test3.lp' -> constraint_marriage(Model,FUN),!;
          Program='test6.lp' -> constraint_marriage(Model,FUN),!;
          Program='test9.lp' -> constraint_square(Model,FUN),!;
          Program='test4.lp' -> constraint_queens(FUN),!;
          true
          ).
           
%%% :- domain(X),range(Y), assignment(X,Y), X < Y.
%%%%% Do not consider "domain" and "range" 
%%%%% One occurrence of "assignment" => one level of recursion 
%%%%% only one built-in atom "X<Y"

constraint_test3([]).
constraint_test3([PAIR|FUN]) :-
    pair_proj(PAIR,X,Y),
    X #>= Y, %%% The complement of the atom
    constraint_test3(FUN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
%%% From "queens"
%%% TWO ASP CONSTRAINTS

constraint_queens(FUN) :-
    constraint_queens_horizontal(FUN),
    constraint_queens_diagonal(FUN).

%%% Horizontal
%%% :- assignment(X1,Y1), assignment(X2,Y2), 
%%%    Y1==Y2,neq(X1,X2).
%%% Two "assignment": double recursion
%%% two built-in atoms  

constraint_queens_horizontal([]).
constraint_queens_horizontal([PAIR|FUN]) :-
    constraint_queens_horizontal_2(PAIR,FUN),
    constraint_queens_horizontal(FUN).
constraint_queens_horizontal_2(_,[]).
constraint_queens_horizontal_2(PAIR1,[PAIR2|FUN]) :-
    pair_proj(PAIR1,X1,Y1),
    pair_proj(PAIR2,X2,Y2),
    (X1 \= X2,!,Y1 #\= Y2;
    true),      
    %%% Actually, since we know that X1 < X2
    %%% thus one could write simply Y1 #\= Y2
    constraint_queens_horizontal_2(PAIR1,FUN).
        
%%% Diagonal
%%%:- assignment(X1,Y1), assignment(X2,Y2),
%%%   neq(X1,X2), abs(X1-X2) == abs(Y1-Y2).
%%% Two "assignments": double recursion
%%% Two built-in atoms   

constraint_queens_diagonal([]).
constraint_queens_diagonal([PAIR|FUN]) :-
    constraint_queens_diagonal_2(PAIR,FUN),
    constraint_queens_diagonal(FUN).
constraint_queens_diagonal_2(_,[]).
constraint_queens_diagonal_2(PAIR1,[PAIR2|FUN]) :-
    pair_proj(PAIR1,X1,Y1),
    pair_proj(PAIR2,X2,Y2),   %%% 
    (X1 \= X2,!, 
    abs(X1-X2) #\= abs(Y1-Y2);
    true), %%% The complement of the atom
    %%% Actually, we know that X1 < X2
    %%% thus one could write simply abs(X1-X2) #\= abs(Y1-Y2)
    constraint_queens_diagonal_2(PAIR1,FUN).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% From Schur
%%% :- assignment(X1,Y), assignment(X2,Y), assignment(X3,Y),
%%%    X3 = X1+X2.
%%% Three assignments: triple recursion.
%%% TAKE CARE THAT HERE RECURSION USES <= !!!

constraint_schur([]).
constraint_schur([PAIR|FUN]) :-
    constraint_schur2(PAIR,[PAIR|FUN]),
    constraint_schur(FUN).

constraint_schur2(_,[]).
constraint_schur2(PAIR1,[PAIR2|FUN]) :-
    constraint_schur3(PAIR1,PAIR2,[PAIR2|FUN]),
    constraint_schur2(PAIR1,FUN).

constraint_schur3(_,_,[]).
constraint_schur3(PAIR1,PAIR2,[PAIR3|FUN]) :-
    pair_proj(PAIR1,X1,Y1),
    pair_proj(PAIR2,X2,Y2),
    pair_proj(PAIR3,X3,Y3),
    %%% we know that Xi's are ground
    %%%(Y1 #= Y2 #/\ Y1 #= Y3) #=> (X3 #\= X1 + X2),
    (X3 =:= X1+X2,!,(Y1 #= Y2 #=> Y1 #\= Y3);
     true),
    constraint_schur3(PAIR1,PAIR2,FUN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% :-  hate(X,Y),assignment(X,Y).
%%% 1 occ of assignment: one recursion
%%% predicate non built.in hate: the model is needed
%%% as parameter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constraint_marriage(Model,FUN) :-
    member(atom(hate,2,EDGES,_),Model),
    constraint_marriage_rec(FUN,EDGES).
constraint_marriage_rec([],_).
constraint_marriage_rec([PAIR|FUN],EDGES) :-
    nin_set(2,PAIR,EDGES),
    constraint_marriage_rec(FUN,EDGES).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% square: for each pair of squares, no overlap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constraint_square(Model,FUN) :-
  constraint_square(Model,0,FUN),  %%% no overlap
  constraint_square1(Model,0,FUN).  %% inside master square

constraint_square1(_,_,[]).
constraint_square1(Model,N,[TRIPLE|FUN]) :-
    triple_proj(TRIPLE,N,X1,Y1),
	%%% retrieve master size
    member(atom(master_size,1,MSIZE,_),Model),
	member(atom(size,2,SIZE,_),Model),
	%% retrieve size of current square
	pair_proj(PAIR,N,Size),
	PAIR in_set SIZE,	
	MS in_set MSIZE,
	X1+Size#=<MS,
	Y1+Size#=<MS,
	
    N1 is N+1,
    constraint_square1(Model,N1,FUN).

  

constraint_square(_,_,[]).
constraint_square(Model,N,[TRIPLE|FUN]) :-
	N1 is N+1,
    constraint_square_2(Model,N,TRIPLE,N1,FUN),
    constraint_square(Model,N1,FUN).
constraint_square_2(_,_,_,_,[]).
constraint_square_2(Model,S1,TRIPLE1,S2,[TRIPLE2|FUN]) :-

    triple_proj(TRIPLE1,S1,X1,Y1),
    triple_proj(TRIPLE2,S2,X2,Y2),   %%% 
	member(atom(size,2,SIZE,_),Model),
	%% retrieve size
	pair_proj(PAIR1,S1,Size1),
	pair_proj(PAIR2,S2,Size2),
	PAIR1 in_set SIZE,
	PAIR2 in_set SIZE,
		
%writeln([S1,S2,Size1,Size2]),

	X2+Size2#=<X1 #\/ X2#>=X1+Size1 #\/ Y2+Size2#=<Y1 #\/ Y2#>=Y1+Size1,
	
	S21 is S2+1,
    constraint_square_2(Model,S1,TRIPLE1,S21,FUN).

%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
