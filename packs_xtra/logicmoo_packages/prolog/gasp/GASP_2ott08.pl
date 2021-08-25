%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GASP 29/09/2008
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SICSTUS 4 clpfd code for computing answer sets
%%% Copia Destra @ by DE GRAPPA research group
%%% Alessandro Dal Palu' Agostino Dovier
%%% Enrico Pontelli Gianfranco Rossi
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Current limits: admitted predicates of arity 0, 1, 2, or 3 only.
%%% All constants must be numbers between 0 and  bigM-1 
%%% Predicate arguments must be expressions with expected value in 0..bigM-1 
%%% (e.g., q(a), p(f(0,1)) are not admitted)  
%%% It accepts "interval facts" (e.g. p(1..3))
%%% Note: To be soon extended with cardinality constraints

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% OPTIONAL: setting the working directory 
 my_cd('C:/agostino/lavori/incorso/gasp').
 :-my_cd(WD),file_systems:current_directory(_,WD).

:-prolog_flag(unknown,_,fail).  

bigM(645).    %%% NOTE: 645^3 < maxint for clpfd
builtin('<'). %%%       For arity 4 use 127
builtin('=').
builtin('==').
builtin('neq').

%%% go/1 and /2 compute all stable models, go/3 one per time 
%%% They take the Program as input and computes stable model(s)
%%% Launch as :- go('filename.lp').  [default: all solutions and printed]
%%% or :- go('filename.lp',write).   [The same]
%%% or :- go('filename.lp',nowrite). [solutions are hidden]
%%% or :- go('filename.lp',Model, write/nowrite) [One solution per time]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go(Program) :-
   go(Program,write).

go(Program,WriteFlag) :-
     go(Program,_,WriteFlag),
     fail.
go(_,_) :-
     sol(N), statistics(runtime,[T,_]),
     it(IT), TotT is (T - IT)/1000,
     format("### Computed  ~d  stable model(s) in  ~2F s ###\n",[N,TotT]),
     write('############################################ END').  
go(Program,Model,WriteFlag) :-
     reset,
     read_program(Program,ProgList),
     empty_domains(ProgList,IniDoms),
     split_facts(ProgList,Facts,Rules,Flag), %%% separates facts and rules and 
     writetime('### Loading time: '),        %%% tests if it is definite (Flag)   
     facts(Facts,IniDoms,FactDoms),          %%% FactDoms: facts are true
     (  Flag=0, % definite
        tp(Rules,FactDoms,Model),
         writeln('### P is a definite program. Its minimum model is:'),
         write_model(WriteFlag,Model)
       ;
       (Flag=1;Flag=2), %%% =1 without constraints =2 with constraints                   
         wf(Rules, FactDoms, WFModel), 
         ( wf_stable(WFModel,Flag),!,     %
           writeln('### There is a wellfounded model'),
           write_model(WriteFlag,WFModel),!,
           Model=WFModel           
           ;
           writeln('### There are no wellfounded models.       ###'),
           fixpoint(Rules,WFModel,Model),  
           writetime('### Next stable model computed in time: '),%%% 
           write_model(WriteFlag,Model)
         )  
        ;   
         Flag=3, %%% =3 with function(s)                   
         wf(Rules, FactDoms, WFModel), 
         fun_fixpoint(Rules,WFModel,Model,Program),
         writetime('### Next Stable model computed in: '),%%% 
         write_model(WriteFlag,Model)
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
     tp_step(ProgList,I,I1,0,F),!, %% tp_step is deterministic.
     (F=0,!, M = I1;       %% we added a ! to avoid to enter
      tp(ProgList,I1,M)).      %% failing occurrences of tp_step 

tp_step([],I,I,F,F).
tp_step([Rule|ProgList],I,I1,F0,F2) :-
     apply_def_rule(Rule,I,Inew,F0,F1),
     tp_step(ProgList,Inew,I1,F1,F2).

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
      nin_set(ARITY,VAR,NEG),      %%% consistency check
      findall(X,(X #= VAR,labeling([ff],VARS)),[L|IST]),
      list_to_fdset([L|IST],SET),
      fdset_union(OLD,SET,NEWPDOM),
      F1=1,
      !;
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
       Sign=neg,!,     %%% Negative unknown 
       nin_set(ARITY,VAR,DOMF,C2);
       Sign=pos,!,     %%% Positive
       C2  #<=> VAR in_set DOMF;
       Sign=negknown,  %%% Negative known 
       C2 #<=> VAR in_set NDOMF
       ),  
       build_constraint(Rs,I,C1,Sign),
       C #<=>  C1 #/\ C2.
build_constraint([],_,1,_).
      
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
      alternating(DefProg, Empty,Empty,K0), %% K0 = lfp(T_P)
      alternating(ProgList, K0,  Empty,U0), %% U0 = lpf(T_{P,K0})
      wf(ProgList,Empty, K0, U0, M).

wf(ProgList,Empty,Ki,Ui,M) :-
     alternating(ProgList, Ui, Empty,Ki1), % Ki1 = lfp(T_{P,U_{i}})
     alternating(ProgList, Ki1,Empty,Ui1), % Ui1 = lfp(T_{P,K_i1})
     (Ki1==Ki, Ui1==Ui, !, 
      wellfoundedmodel(Ki,Ui,M);
      wf(ProgList,Empty,Ki1,Ui1,M)).
            
%%% W*_P = K union {not(A) : A in U}   
      
wellfoundedmodel([],_,[]).
wellfoundedmodel([atom(P,AR,PDOM,_)|K],U,[atom(P,AR,PDOM,NDOM)|M]) :-
      select(atom(P,AR,UDOM,_),U,RU),
      complement_set(AR,UDOM,NDOM),
      wellfoundedmodel(K,RU,M). 
 
alternating(ProgList,J,I,M) :-
      wf_step(ProgList,J,I,I1),!,
      (I == I1, !, M = I;
      alternating(ProgList,J,I1,M)).      
      
wf_step([],_J,I,I).
wf_step([Rule|ProgList],J,I,I1) :-
      alternating_apply_rule(Rule,J,I,Inew),
      wf_step(ProgList,J,Inew,I1).

%%% T_{P,J}(I) = { A : (A :- PBody, neg NBody) in P ,
%%%                     PBody \subseteq I and
%%%                     NBody \cap J = \emptyset }

alternating_apply_rule(rule([H],PBody,NBody),J,I,[atom(F,ARITY,NEWPDOM,NEG)|PARTI]) :-
      copy_term([H,PBody,NBody],[H1,PBody1,NBody1]),  %%% variable renaming
      term_variables([H1,PBody1],VARS),    %%% collect fresh vars
      bigM(M),M1 is M-1,domain(VARS,0,M1), %%% fix (wide) domains for the fresh vars
      build_constraint(PBody1,I,C1,pos),   %%% C = 1 iff the body is satisfied by I
      build_constraint(NBody1,J,C2,neg), 
      H1 =..[F|ARGS],
      tuple_num(ARGS,VAR,ARITY),
      select(atom(F,ARITY,OLD,NEG),I,PARTI),
      nin_set(ARITY,VAR,OLD,C3),      %%% do not apply if head is already true 
      %%% nin_set(ARITY,VAR,NEG,C4),  %%% consistency check
      findall(X,(C1+C2+C3 #>= 3, X #= VAR,labeling([ff],VARS)),LIST),
      list_to_fdset(LIST,SET),
      fdset_union(OLD,SET,NEWPDOM).

%%% Test if a well-founded model is complete, hence stable

wf_stable([],_).
wf_stable([atom(false,_,_,_)|M],1) :- %% particular case
      !, wf_stable(M,1). %% for constraints-free programs
wf_stable([atom(_,AR,PDOM,NDOM)|M],Flag) :-
      complement_set(AR,PDOM,NDOM),
      wf_stable(M,Flag). 
                        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Non determinsitic fixpoint procedure.
%%% lev_fixpoint is used to avoid some symmetries
%%% e.g. equivalent sequences of independent rules applications 
%%% [e.g. (1,2) (2,1)]
%%% A simpler fixpoint rule (slow_fixpoint) is at the end of the file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Observe that the same atom can be introduce by two rules
%%% (eg one chosen ND and the other by tp and vice versa)
%%% In this case we compute several equal models. This is bad.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fixpoint(Rules,I1,I2) :-
       lev_fixpoint(Rules,I1,I2,[],0).

%%% This first clause of lev_fixpoint is redundant (the second is complete)
%%% However, it avoids some recomputations of "collect_applicable_rules"

lev_fixpoint(Rules,I1,I2, PrevRules,PrevIndex) :-
       length(PrevRules,N), 
       N > 0, 
       PrevIndex < N, !, 
       P1 is PrevIndex + 1,
       ( Index in P1 .. N,
         nth1(Index,PrevRules,GRULE),
         apply_rule(GRULE, I1, M, _),
         tp(Rules,M,IN2),
         constraints_check(IN2),
         lev_fixpoint(Rules,IN2,I2,PrevRules,Index); %%
         lev_fixpoint(Rules,I1,I2,PrevRules,N) ).

lev_fixpoint(Rules,I1,I2, PrevRules,PrevIndex) :-
        collect_applicable_rules(Rules,I1,ApplRules), %%% slow point
       (ApplRules=[],
%          tp(Rules,I1,IT1),         
%          constraints_check(IT1),   
         !, I2=I1; %%%IT1=I2; %% no rules ->  stable model
        diff_list(ApplRules,PrevRules,NewRules),      %%% diff_list -> n log n 
        append(PrevRules,NewRules,NewLevelledRules),
        length(NewLevelledRules,N),
        P1 is PrevIndex + 1, 
        Index in P1 .. N,
        nth1(Index,NewLevelledRules,GRULE), %% position according to order (deterministic!)
        apply_rule_1(GRULE, I1, IN, _),
        tp(Rules,IN,IN2),         
        constraints_check(IN2),      
        lev_fixpoint(Rules,IN2,I2,NewLevelledRules,Index)).

%%%%%%%%% constraints_check/1
%%% Tests if the constraints are verified.
%%% Observe that for each constraint :- Goal
%%% we add a rule
%%% false :- Goal, not false.
%%% When false is present in a model ->
%%% a constraint has been falsified (Goal is true in the Model)
%%% Using apply_rule1 should be useless

constraints_check(Model) :-
    member(atom(false,0,[],_),Model).     

%%%%% collect_applicable_rules/3
% collects all the applicable rules given the interpretation I.
% Notice the (non standard) the use of findall/4 to avoid an append

collect_applicable_rules([Rule|Rules],I,List):-
       collect_applicable_rules(Rules,I,B),
       findall(GroundRule,(apply_rule(Rule,I, M, GroundRule),constraints_check(M)),List,B).
collect_applicable_rules([],_I,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% non deterministic rule application: apply_rule (_1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% used for building the set of applicable rules (constraints included)
%% cannot include apply_rule_1, otherwise if a constraint cannot be satisfied,
%% it results that the rule is not applicable

apply_rule(rule([H],Bpos,Bneg),I1,I2,rule([H1],Bpos1,Bneg1)) :-
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% domain updating
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dom_update([],I,I).
dom_update([A|R],I1,I2) :-      
      A =.. [F|ARGS],
      tuple_num(ARGS,D,ARITY),
      select(atom(F,ARITY,PDOM,NEGDOM),I1,Irest),
      fdset_add_element(NEGDOM,D,NEWDOM),          %% if D is already in NEGDOM
      dom_update(R,[atom(F,ARITY,PDOM,NEWDOM)|Irest],I2).%% then NEWDOM=NEGDOM
           
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% expression parses algebraic expressions into FD expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            
expression(F,[TX,TY],C2) :-
       term_conv(TX,TERMX),
       term_conv(TY,TERMY),  
       (F = '=', !, C2  #<=>  TERMX #= TERMY;       
        F = '==',!, C2  #<=>  TERMX #= TERMY;       
        F = '<', !, C2  #<=>  TERMX #< TERMY;
        F = 'neq', !, C2  #<=>  TERMX #\= TERMY).       

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
      OP = 'abs',  !,  TO #= abs(A1).

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
%%%% 2 for programs with constraints or functions
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
                   
%%% pair_proj is biderectional and works for pairs

pair_proj(PAIR,X,Y) :-
        bigM(M),
        PAIR #= M*X + Y,
        X #= PAIR/M,
        Y #= PAIR mod M.
        
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
       fdset_subtract(Universe,UDOM,NDOM).

%%% diff_list(X,Y,Z) returns in Z the list of elements in X and not in Y
  
%%% nlogn version (if sort is n log n)
diff_list(ApplRules,PrevRules,NewRules) :-
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
   %% RETRACT ALL VALUES
   retractall(sol(_)),
   retractall(it(_)),
   %% ASSERT INITIAL VALUES
   assert(sol(0)),
   assert(it(It)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%    PRINTING PREDICATES   %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeln(A) :- write(A),nl.

write_list([]):-nl.
write_list([A|R]) :- 
     writeln(A),
     write_list(R).

writetime(String) :-
     statistics(runtime,[_,T]),
     write(String),write(T),writeln(' ms ###').

%%%% write_model/2: if the flag is "write" prints a stable model 

write_model(write,MOD) :-
   !,
   retract(sol(N)),N1 is N+1,assert(sol(N1)),
   writeln('###############################################'),
   writeln('############# Computed model ##################'),
   writeln('###############################################'),
   write_fdmodel(MOD),
   writeln('###############################################').
write_model(_,_) :-
   writeln('### Computed model not printed, as required ###'),
   retract(sol(N)),N1 is N+1,assert(sol(N1)).

   
write_fdmodel([]).
write_fdmodel([atom(false,0,_,_)|R]):- !,
   write_fdmodel(R).
write_fdmodel([atom(P,0,Dom,_)|R]):-
   format("### ~a/0: ",[P]),
   (Dom=[],!, writeln(false); writeln(true)),
    write_fdmodel(R).
write_fdmodel([atom(P,1,Dom,_)|R]):-
   format("### ~a/1: ",[P]),
   fdset_to_list(Dom,LDom), writelist(LDom),
   write_fdmodel(R).
write_fdmodel([atom(P,2,Dom,_)|R]):-
   format("### ~a/2: ",[P]),
   fdset_to_list(Dom,LDom),write_proj(2,LDom),
   write_fdmodel(R).
write_fdmodel([atom(P,3,Dom,_)|R]):-
   format("### ~a/3: ",[P]),
   fdset_to_list(Dom,LDom), write_proj(3,LDom),
   write_fdmodel(R).
write_proj(_,[]) :- nl.
write_proj(2,[D|R]) :-
   bigM(M), X is D//M, Y is D mod M,
   format("(~d,~d) ",[X,Y]),
   write_proj(2,R).
write_proj(3,[D|R]) :-
   bigM(M), X is D//(M*M), Y is (D-X*M*M)//M, Z is D mod M,
   format("(~d,~d,~d) ",[X,Y,Z]),   
   write_proj(3,R).
writelist([]) :- nl.
writelist([D|R]) :-
   write(D),write(' '), 
   writelist(R).   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
%%% EXTRA CODE 1. SIMPLE ASP COMPUTATION (without ordering control)   
%%% It can replace fixpoint predicate 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slow_fixpoint(Rules,I0,M) :-
      (nd_choice(Rules,I0,I1),
       tp(Rules,I1,I2),  
       constraints_check(I2),
       slow_fixpoint(Rules,I2,M);
       stable(Rules,I0),!, M=I0).

nd_choice([Rule|_], I1,I2 ) :- 
      apply_rule(Rule, I1, I2,_).
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
funbuild([],[],_,_,[]).   
funbuild([X|Xs],[Y|Ys],DOM,RAN,[D|Fun]) :-
           X in_set DOM, Y in_set RAN,
           tuple_num([X,Y],D,2),
           funbuild(Xs,Ys,DOM,RAN,Fun).
           
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AD-HOC PART
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constraint_adhoc(Program,FUN,Model) :-
          ( 
          Program='test3.lp' -> constraint_test3(FUN),!;
          Program='test4.lp' -> constraint_queens(FUN),!;
          Program='test5.lp' -> constraint_schur(FUN),!;
          Program='test6.lp' -> constraint_marriage(Model,FUN),!;
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


%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
