:- expects_dialect(sicstus).
:- set_prolog_flag(double_quotes, codes).
:- style_check(-discontiguous).
% ---------------------------------------------------------------------------
% Evaluation of States ------------------------------------------------------
% ---------------------------------------------------------------------------
solve(Expression) :- 
        maxvar( Expression, Max ), 
        retractall(maxVarIni(_)), assert(maxVarIni(Max)),!,
        retractall(solution),
        retractall(time(_)), 
        statistics(runtime,[TimeIni,_]), 
        assert(time(TimeIni)),
        narrowing( [er([],Expression,[])] ),
        ( solution 
        ;\+(solution),
         statistics(runtime,[TimeFin,_]), 
         Time is TimeFin - TimeIni,
         write('No solution. '), format(' ~0d ms',[Time]),nl ).

pass :- assert( debugmode ).
nopass :- retractall( debugmode ).

debugStep(States) :-
        ( debugmode,!,
          write('-------------------------------------------------------'),nl,
          write2_states( States ),nl,
          write('-------------------------------------------------------'),nl,
          control
         ;\+(debugmode),! ).

control :-
        write('Curry Debug Control ---'),nl,
        write('a - abort'),nl,
        write('c - continue without debug'),nl,
        write('t - tracing in debug mode'),nl,
        write('(any other key) - next evaluation step'),nl,
        on_exception(_,get0(Key),control),
        ( Key=10;get0(10) ),
        ( (Key = 97;Key = 65),!, fail                  % a = 97, A = 65
         ;(Key = 116;Key = 84),!, 
          retractall(debugmode),assert(debugmode)      % t = 116, T = 84
         ;(Key = 99;Key = 67),!, retractall(debugmode) % c = 99, C = 67
         ;write('') ).

narrowing( [] ) :- !.
narrowing( States ) :-
        debugStep(States),
        on_exception( _,narrowing( States, StatesR ),
                        (control,narrowing(States)) ),!,
        ( check_IO(StatesR),!,
          narrowing( StatesR )
         ;write('Sorry, no IO function allowed in nodeterministic evaluation'),
          nl ).

% Check for IO functions
check_IO( [] ).
check_IO( [_] ).
check_IO( [S1,S2|Ss] ) :- noIO([S1,S2|Ss]).

noIO( [er(_,Expr,_)|_] ) :- io_Expr(Expr),!,fail.
noIO( [_|States] ) :- noIO(States).
noIO( [] ).

io_Expr( f(F,_,_) ) :- 
        function(F,_,FType),
        extractType(FType,Type),
        Type = type('IO',_,_).
io_Expr( c(_,_,Terms) ) :- io_ExprL(Terms).
io_Expr( f(_,_,Terms) ) :- io_ExprL(Terms).

io_ExprL( [X|_] ) :- io_Expr(X).
io_ExprL( [_|Xs] ) :- io_ExprL(Xs).

narrowing( [er(Susts,Exp,C)|States], ResultStates ) :- % general
        maxvar( Exp, Max ), retractall(maxVar(_)), assert(maxVar(Max)),
        evalInc( C, Exp, StatesE ),!,
        narrowing_join( StatesE, Susts, RStates ),
        sappend( RStates, States, ResultStates ).
narrowing( [er(Susts,Exp,C)|States], ResultStates ) :-
        normal_form( Exp ),!, 
        assert(solution),
        time(TimeIni), statistics(runtime,[TimeFin,_]), 
        Time is TimeFin - TimeIni,
        (  Exp=c('IO',1,_),nl
         ; \+(Exp=c('IO',1,_)) ),
        write('Solution: '), write2( er(Susts,Exp,C) ),nl,
        format(' ~0d ms',[Time]),nl,
        retractall(time(_)), 
        statistics(runtime,[NewTimeIni,_]), 
        assert(time(NewTimeIni)),
        !,narrowing( States, ResultStates ).
narrowing( [er(Susts,Exp,C)|States], ResultStates ) :-
        time(TimeIni), statistics(runtime,[TimeFin,_]), 
        Time is TimeFin - TimeIni,
        write('Cannot be evaluated: '), write2( er(Susts,Exp,C) ),nl,
        format(' ~0d ms',[Time]),nl,
        retractall(time(_)), 
        statistics(runtime,[NewTimeIni,_]), 
        assert(time(NewTimeIni)),
        !,narrowing( States, ResultStates ).
narrowing( [], [] ).

narrowing_join( [], _, [] ).
narrowing_join( [er( Susts1, Exp, C )|States1], Susts,
                [er( Susts4, Exp, C )|States2] ) :-
        applys( Susts1, Susts, Susts2 ),
        sappend( Susts2, Susts1, Susts3 ),
        maxVarIni(Max), restrict( Susts3, Max, Susts4 ),
        narrowing_join( States1, Susts, States2 ).

normal_form( f('success',0,[]) ) :- !.
normal_form( c(_,_,Terms) ) :- normal_formL( Terms ).
normal_formL( [] ).
normal_formL( [Term|Terms] ) :- normal_form( Term ), normal_formL( Terms ).

restrict( [], _, [] ).
restrict( [s(N,Term)|Susts1], Max, [s(N,Term)|Susts2] ) :- N =< Max,
        restrict( Susts1, Max, Susts2 ).
restrict( [s(N,_)|Susts1], Max, Susts2 ) :- N > Max,
        restrict( Susts1, Max, Susts2 ).



% replace --------------------------------------------------------
% Compound resulting components with original expression and original 
% list of incremental info
% (list of components, Occurrence, List of inc. info, Exp, States to return)
replace([exp(Ox,Sust,RExp,C)|LEval1],Expression,[State|States1]) :- !,
       % Apply Substitution to Expression
       apply(Sust,Expression,ExpS),
       % Rewrite Expression at Ox by ExpS
       (RExp=noExp,!,Exp=ExpS % Special case to avoid returning an empty exp
       ;part(ExpS,Ox,RExp,Exp)),
       % Check if operation-rooted
       (RExp=f(F,_,_),F\=='success',!,
        % Get subtree
        (dt(F,_,TreeF),!,CNew=[(Ox,TreeF)|C]
        ;CNew=[(Ox,noTree)|C])
       ;CNew=C),
       % Result
       State=er(Sust,Exp,CNew),
       !,replace(LEval1,Expression,States1).
replace([],_,[]).

%- Add initial occurrence and list of inc. info to evaluated position
addOccInc([exp(Ox1,Sust,Exp,C1)|LEval1],Ox,C,[exp(Ox2,Sust,Exp,C2)|LEval2]) :-
       sappend(Ox,Ox1,Ox2),
       addOccInc2(C1,Ox,C,C2),
       !,addOccInc(LEval1,Ox,LEval2).
addOccInc([],_,[]).

%- Add initial occurrence to list of incremental info
addOccInc2([(Ox1,T)|C1],Ox,C,[(Ox2,T)|C2]) :-
       sappend(Ox,Ox1,Ox2),
       !,addOccInc2(C1,Ox,C,C2).
addOccInc2([],_,C,C).

% EVAL -------------------------------------------------------------------
% Uses a special representation. It returns a list of 
% exp(Occurrence,Substitution,Right expression,Visited terms)
% to avoid multiple replacements of expressions

% All calls to some kind of eval function have the initial occurrence
% and the initial list of inc. info
% They also return only list of elements "exp/4" as the original definition
% of strategy

% 'noExp' and 'noTree'

% evalInc ------------
% Manages incremental info to narrow only inner function-rooted expressions
% if possible 
evalInc( [], Expression, States) :- !,
      evalExp( Expression, [], [], LEval ),
      !,replace(LEval,Expression,States).
evalInc( [(Ox,SubTree)|C], Expression, States ) :- 
      %-Get subexpression
      pos(Expression,Ox,f(F,N,Args)),
      %-Get subtree of function (if possible)
      (SubTree=noTree,  %%% It is a builtin function without def tree
       evalBuiltIn( f(F,N,Args), Ox, C, LEval)
      ;SubTree\==noTree,
       evalTree(SubTree,f(F,N,Args),Ox,C,LEval) ),
      %-Rebuild List of evaluation with subexp info
      !,replace(LEval,Expression,States).
evalInc( [_|C], Expression, States ) :- !,evalInc(C,Expression,States).
      
% evalExp ------------
% It evals any expression, constructor or function rooted
evalExp( f(F,N,Terms), Ox, C, LEval ) :-
        length( Terms, N ),         % needed with High Order
        (dt(F,N,DefTree),!,
         evalTree( DefTree, f(F,N,Terms), Ox, C, LEval )
        ;evalBuiltIn( f(F,N,Terms), Ox, C, LEval) ).
evalExp( c(_,_,Terms), Ox, C, LEval ) :-
        evalExp_some( Terms, 1, Ox, C, LEval ).

%-Find some evaluable subexpression
evalExp_some( [Term|_], N, Ox, C, LEval ) :-
        evalExp( Term, [N], [], LEval1 ),!,
        addOccInc(LEval1,Ox,C,LEval).
evalExp_some( [_|Terms], N, Ox, C, LEval ) :-
        N1 is N + 1,
        evalExp_some( Terms, N1, Ox, C, LEval ).

% evalTree -----------
% It manages definitional tree 
evalTree( trule(TermL,TermR), Expression, Ox, C, LEval ) :- !,      % -- Rule
        % Rename variables
        maxVar(Inc), 
        rename(TermL,Inc,TermLx), rename(TermR,Inc,TermRx),
        % Find substitution
        susts( TermLx, Expression, Sust ),
        % Apply substitution
        apply( Sust, TermRx, Term ),
        % Return resulting component 
        % (empty occurrence, no substitution, right exp, list of inc. info)
        LEval = [exp(Ox,[],Term,C)].

evalTree( tor(Tree1,Tree2), Expression, Ox, C, LEval ) :- !,     % -- OR
        evalTree( Tree1, Expression, Ox, C, LEval1 ),
        evalTree( Tree2, Expression, Ox, C, LEval2 ),
        sappend( LEval1, LEval2, LEval ).

evalTree( tbranch(IOx,_,Trees), Expression, Ox, C, LEval ) :- % -- Constructor
        pos( Expression, IOx, c(F,_,_) ),!,
        evalTree_some( Trees, Expression, F, Ox, C, LEval ).

% Find a subtree which constructor unifies with term's constructor
evalTree_some( [], _, _, _, _, [] ). % -- error no one evaluable
evalTree_some( [(F,Tree)|_], Expression, F, Ox, C, LEval ) :- !,
        evalTree( Tree, Expression, Ox, C, LEval ).
evalTree_some( [_|Trees], Expression, F, Ox, C, LEval ) :-
        evalTree_some( Trees, Expression, F, Ox, C, LEval ).

evalTree( tbranch(IOx,flex,Trees), Expression, Ox, C, LEval ) :- % -- Variable
        pos( Expression, IOx, v(Var) ),!,
        evalTree_instantiate(Trees,Var, tbranch(IOx,flex,Trees), Ox, C, LEval).

% Instantiate variable to all constructors and 
% DO NOT perform any subsequent step
evalTree_instantiate( [], _, _, _, _, [] ).
evalTree_instantiate( [(F,_)|Trees], Var, CurrentTree, Ox, C, LEval ) :-
        % Build correct instantiation
        (constructor(F,Arity),!
        ;F='Nil', !,Arity=0
        ;F='Cons',!,Arity=2
        ;tuple2arity(F,Arity),!
        ;Arity=0  % Default assumption
        ),
        maxVar(Inc), newvars(Arity,Inc,Vars),
        Term = c(F,Arity,Vars),
        % Return component
        LEval=[exp([],[s(Var,Term)],noExp,[(Ox,CurrentTree)|C])|LEval1],
        % Execute list
        !,evalTree_instantiate( Trees, Var, CurrentTree, Ox, C, LEval1 ).

evalTree( tbranch(IOx,M,Trees), Expression, Ox, C, LEval) :- % -- Function call
        pos( Expression, IOx, f(F,N,Terms) ),!,
        sappend(Ox,IOx,Ox1),
        evalExp(f(F,N,Terms),Ox1,[(Ox,tbranch(IOx,M,Trees))|C],LEval).

% evalBuiltIn ------------
% -- Special: conditional function
evalBuiltIn( f('\\=>',2,[Constraint,Exp]), Ox, C, LEval ) :-
     (%--Constraint cannot be evaluable
      Constraint=f('success',0,[]),!,
      LEval=[ exp(Ox,[],Exp,C) ]
     ;%--Evaluation of Constraint
      sappend(Ox,[1],Ox1),
      evalExp( Constraint, Ox1, [(Ox,noTree)|C], LEval ) ).

% -- Special: partial function application
evalBuiltIn( f('\\@',_,[ Expr| TermsAdd ]), Ox, C, LEval ) :- !,
      ( %-Evaluable Expression
        sappend(Ox,[1],Ox1),
        evalExp( Expr, Ox1, [(Ox,noTree)|C], LEval ),!
      ; %-Function to add arguments
        %%Expr cannot be evaluated, so needs extra args
        Expr = f(F,N,Terms),
        sappend( Terms, TermsAdd, TermsR ),
        LEval = [ exp(Ox,[],f(F,N,TermsR),C) ] ).

% -- Constraint Evaluation
evalBuiltIn( f('&',2,[f('success',0,[]),Ct2]), Ox, C, LEval ) :- !,
        LEval = [exp(Ox,[],Ct2,C)].

evalBuiltIn( f('&',2,[Ct1,Ct2]), Ox, C, LEval ) :- !,
        (  sappend(Ox,[1],Ox1),
           evalExp( Ct1, Ox1, [(Ox,noTree)|C], LEval ),!
        ; %%%%% To achieve correctness new incremental info should be removed
           sappend(Ox,[2],Ox2),
           evalExp( Ct2, Ox2, [], LEvalX ),!,
           removeInc(LEvalX,[(Ox,noTree)|C],LEval)
        ).

% Remove Incremental Info and replace it by previous incremental info
removeInc([],_,[]).
removeInc([exp(Ox,Sust,Exp,_)|L1],C,[exp(Ox,Sust,Exp,C)|L2]) :-
        removeInc(L1,C,L2).

% -- Simple Constraint Evaluation
evalBuiltIn( f('=:=',2,[Ct1,Ct2]), Ox, C, LEval ) :- !,
        evalCT( Ct1, Ct2, Ox, C, LEval ).

%Function
evalCT( f(F,N,Terms), _, Ox, C, LEval ) :- !,
        sappend(Ox,[1],Ox1),
        evalExp( f(F,N,Terms), Ox1, [(Ox,noTree)|C], LEval ).

%Function
evalCT( _, f(F,N,Terms), Ox, C, LEval ) :- !,
        sappend(Ox,[2],Ox1),
        evalExp( f(F,N,Terms), Ox1, [(Ox,noTree)|C], LEval ).

%Same Constructor
evalCT( c(F,N,Args1), c(F,N,Args2), Ox, C, LEval ) :- !,
        LEval = [exp( Ox, [], Constraint, C )],
        eval_join( Args1, Args2, Constraint ).

%Different Constructor
evalCT( c(F1,N1,Args1), c(F2,N2,Args2), _, _, [] ) :- !,
        (F1 \== F2
        ;N1 =\= N2
        ;length(Args1,M1), length(Args2,M2), M1 =\= M2).

%Variable
evalCT( v(N), v(M), Ox, C, LEval ) :- !,
        LEval = [exp( Ox, [s( N, v(M) )], f('success',0,[]), C )].

%Variable and Constructor
evalCT( v(M), c(F,N,Terms), Ox, C, LEval ) :- 
       (occur_check( M, c(F,N,Terms) ),!,
        LEval = []
       ;maxVar(Max),
        eval_freshvars( Terms, Max, [], Vars ),
        eval_join( Vars, Terms, Constraint ),
        LEval = [exp( Ox, [s( M, c(F,N,Vars) )], Constraint, C )]).

%Contructor and Variable
evalCT( c(F,N,Terms), v(M), Ox, C, LEval ) :- 
       (occur_check( M, c(F,N,Terms) ),!,
        LEval = []
       ;maxVar(Max),
        eval_freshvars( Terms, Max, [], Vars ),
        eval_join( Vars, Terms, Constraint ),
        LEval = [exp( Ox, [s( M, c(F,N,Vars) )], Constraint, C )]).

eval_join( [], [], f('success',0,[]) ).
eval_join( [Exp1], [Exp2], f('=:=',2,[Exp1,Exp2]) ).
eval_join( [Exp1,Exp11|Exps1], [Exp2,Exp22|Exps2], Constraint ) :-
        Constraint = f('&',2,[ f('=:=',2,[Exp1,Exp2]), Rest ]),
        eval_join( [Exp11|Exps1], [Exp22|Exps2], Rest ).

eval_freshvars( [], _, Vars, Vars ).
eval_freshvars( [_|Terms], Max, Vars1, Vars3 ) :-
        Max1 is Max + 1,
        sappend( Vars1, [v(Max1)], Vars2 ),
        eval_freshvars( Terms, Max1, Vars2, Vars3 ).

occur_check( M, v(M) ).
occur_check( M, c(_,_,Terms) ) :- occur_checkL( M, Terms ).

occur_checkL( M, [Term|_] ) :- occur_check( M, Term ),!.
occur_checkL( M, [_|Terms] ) :- occur_checkL( M, Terms ).

% -- Special: Strict Equality
evalBuiltIn( f('==',2,[ Term1, Term2 ]), Ox, C, LEval ) :- !,
        (  
           % Case Constructors
           (  Term1 = c(F,N,Args1), Term2 = c(F,N,Args2), N > 0,
              eval_equality( Args1, Args2, Term3 )
            ; Term1 = c(F,0,[]), Term2 = c(F,0,[]),
              Term3=c('True',0,[])
            ; Term1 = c(F1,N1,_), Term2 = c(F2,N2,_), (F1\==F2;N1=\=N2),
              Term3=c('False',0,[]) ),
           LEval = [ exp(Ox, [], Term3, C ) ]

         ; % Case Function
           ( Term1 = f(F,N,Terms), sappend(Ox,[1],Ox1)
            ;Term2 = f(F,N,Terms), sappend(Ox,[2],Ox1) ), !,
           evalExp( f(F,N,Terms), Ox1, [(Ox,noTree)|C], LEval )
        ).

eval_equality( [X], [Y], f('==',2,[ X, Y ]) ).
eval_equality( [X1,X2|Xs], [Y1,Y2|Ys], 
               f('&&',2,[ f('==',2,[ X1, Y1 ]), Rest ]) ) :-
        eval_equality([X2|Xs],[Y2|Ys],Rest).

% -- Special: Arithmetic functions
evalBuiltIn( f('+',2,[ Term1, Term2 ]), Ox, C, LEval ) :- !, 
        eval_Op( '+', Term1, Term2, Ox, C, LEval ).
evalBuiltIn( f('-',2,[ Term1, Term2 ]), Ox, C, LEval ) :- !, 
        eval_Op( '-', Term1, Term2, Ox, C, LEval ).
evalBuiltIn( f('*',2,[ Term1, Term2 ]), Ox, C, LEval ) :- !, 
        eval_Op( '*', Term1, Term2, Ox, C, LEval ).
evalBuiltIn( f('div',2,[ Term1, Term2 ]), Ox, C, LEval ) :- !, 
        eval_Op( 'div', Term1, Term2, Ox, C, LEval ).
evalBuiltIn( f('mod',2,[ Term1, Term2 ]), Ox, C, LEval ) :- !, 
        eval_Op( 'mod', Term1, Term2, Ox, C, LEval ).
evalBuiltIn( f('>',2,[ Term1, Term2 ]), Ox, C, LEval ) :- !, 
        eval_Op( '>', Term1, Term2, Ox, C, LEval ).
evalBuiltIn( f('<',2,[ Term1, Term2 ]), Ox, C, LEval ) :- !, 
        eval_Op( '<', Term1, Term2, Ox, C, LEval ).
evalBuiltIn( f('>=',2,[ Term1, Term2 ]), Ox, C, LEval ) :- !, 
        eval_Op( '>=', Term1, Term2, Ox, C, LEval ).
evalBuiltIn( f('=<',2,[ Term1, Term2 ]), Ox, C, LEval ) :- !, 
        eval_Op( '=<', Term1, Term2, Ox, C, LEval ).

% common behaviour
eval_Op( Op, Term1, Term2, Ox, C, LEval ) :-
        (  Term1 = c(ANum1,0,[]), Term2 = c(ANum2,0,[]), !,
           eval_OpValue( Op, ANum1, ANum2, ANum3 ),
           LEval = [ exp( Ox, [], c(ANum3,0,[]), C ) ]

         ; (  Term1 = f(F,N,Terms), sappend(Ox,[1],Ox1)
            ; Term2 = f(F,N,Terms), sappend(Ox,[2],Ox1) ), !,
           evalExp( f(F,N,Terms), Ox1, [(Ox,noTree)|C], LEval )
        ).

eval_OpValue( '+', Num1, Num2, Num3 ) :- 
        Num3 is Num1 + Num2.
eval_OpValue( '-', Num1, Num2, Num3 ) :- 
        Num3 is Num1 - Num2.
eval_OpValue( '*', Num1, Num2, Num3 ) :- 
        Num3 is Num1 * Num2.
eval_OpValue( 'div', Num1, Num2, Num3 ) :- 
        Num3 is Num1 // Num2.
eval_OpValue( 'mod', Num1, Num2, Num3 ) :- 
        Num3 is Num1 mod Num2.
eval_OpValue( '>', Num1, Num2, Num3 ) :- 
        Num1 > Num2, !, Num3 = 'True' ; Num3 = 'False'.
eval_OpValue( '<', Num1, Num2, Num3 ) :- 
        Num1 < Num2, !, Num3 = 'True' ; Num3 = 'False'.
eval_OpValue( '>=', Num1, Num2, Num3 ) :- 
        Num1 >= Num2, !, Num3 = 'True' ; Num3 = 'False'.
eval_OpValue( '=<', Num1, Num2, Num3 ) :- 
        Num1 =< Num2, !, Num3 = 'True' ; Num3 = 'False'.

% -- Special: IO functions
evalBuiltIn( f('>>',2,[Exp1,Exp2]), Ox, C, LEval ) :- !,
        ( normal_form(Exp1),!,
          LEval = [ exp(Ox, [], Exp2, C) ] 
        ; sappend(Ox,[1],Ox1),
          evalExp( Exp1, Ox1, [(Ox,noTree)|C], LEval )
        ).

evalBuiltIn( f('>>=',2,[Exp1,Exp2]), Ox, C, LEval ) :- !,
        ( normal_form(Exp1),
          Exp1 = c('IO',1,[ExpIO]), 
          Exp2 = f(F,N,Args), sappend(Args,[ExpIO],Exps),
          LEval = [ exp( Ox, [], f(F,N,Exps), C ) ] 
        ; sappend(Ox,[1],Ox1),
          evalExp( Exp1, Ox1, [(Ox,noTree)|C], LEval )
        ).

evalBuiltIn( f('putChar',1,[Exp]), Ox, C, LEval ) :- !,
        ( normal_form(Exp),
          Exp = c([Ch],0,[]), 
          name(S,[Ch]), write(S),
          LEval = [ exp( Ox, [], c('IO',1,[ c('Tuple0',0,[]) ]), C ) ] 
        ; sappend(Ox,[1],Ox1),
          evalExp( Exp, Ox1, [(Ox,noTree)|C], LEval )
        ).

evalBuiltIn( f('getChar',0,[]), Ox, C, LEval ) :- !,
        prompt(_,'?'),
        get0(Ch),
        LEval = [ exp( Ox, [], c('IO',1,[c([Ch],0,[])]), C ) ].

evalBuiltIn( f('done',0,[]), Ox, C, LEval ) :- !,
        LEval = [ exp( Ox, [], c('IO',1,[ c('Tuple0',0,[]) ]), C ) ].

evalBuiltIn( f('return',1,[Exp]), Ox, C, LEval ) :- !,
        ( normal_form(Exp),
          Exp = c(Const,N,Exps), 
          LEval = [ exp( Ox, [], c('IO',1,[ c(Const,N,Exps) ]), C ) ] 
        ; sappend(Ox,[1],Ox1),
          evalExp( Exp, Ox1, [(Ox,noTree)|C], LEval )
        ).

evalBuiltIn( f('readFile',1,[Exp]), Ox, C, LEval ) :- !,
        ( normal_form(Exp),!,
          Exp = c(_,_,_), 
          toString(Exp,Str), name(FileName,Str),
          % Read complete file 
          (file_exists(FileName),
           open(FileName, read, Stream),
           read_all( FileStr, Stream ),
           close(Stream),!,
           toString(IntStr,FileStr),
           LEval = [ exp( Ox, [], c('IO',1,[IntStr]), C ) ]
          ;LEval=[] )
        ; sappend(Ox,[1],Ox1),
          evalExp( Exp, Ox1, [(Ox,noTree)|C], LEval )
        ).

evalBuiltIn( f('writeFile',2,[Exp1,Exp2]), Ox, C, LEval ) :- !,
         (normal_form(Exp1),
          normal_form(Exp2),!,
          toString(Exp1,Str), name(FileName,Str),
          toString(Exp2,FileStr), name(Output,FileStr),
          ( open(FileName, write, Stream),
            write(Stream,Output),
            close(Stream),!,
            LEval = [ exp( [], [], c('IO',1,[ c('Tuple0',0,[]) ]), [] ) ]
           ;LEval=[] ) 
         ;normal_form(Exp1),!,
          sappend(Ox,[2],Ox1),
          evalExp( Exp2, Ox1, [(Ox,noTree)|C], LEval )
         ;sappend(Ox,[1],Ox1),
          evalExp( Exp1, Ox1, [(Ox,noTree)|C], LEval )
         ).

toString( c('Nil',0,[]), [] ).
toString( c('Cons',2,[ c([Ch],0,[]), String ]), [Ch|Str] ) :-
        toString( String, Str ).

% -- Special: Lambda Abstractions
evalBuiltIn( f('\\',N,[Vars,Expr|Exprs]), Ox, C, LEval ) :- !,
        % check arity
        length([Vars,Expr|Exprs],N),
        % Unify args with passed expressions
        Vars = c('\\Vars',Np,Pattern),
        susts( f('\\',Np,Pattern), f('\\',Np,Exprs), Sust ),
        % Obtain final term
        apply( Sust, Expr, Term ),
        % Return value
        LEval = [exp(Ox,[],Term,C)].

% -- Special: Char-Int Conversion
evalBuiltIn( f('ord',1,[Exp]), Ox, C, LEval ) :- !,
         (normal_form(Exp),!,
          Exp = c([Ch],0,[]), 
          LEval = [ exp( Ox, [], c(Ch,0,[]), C ) ]
         ;sappend(Ox,[1],Ox1),
          evalExp( Exp, Ox1, [(Ox,noTree)|C], LEval )).
evalBuiltIn( f('chr',1,[Exp]), Ox, C, LEval ) :- !,
         (normal_form(Exp),!,
          Exp = c(Ch,0,[]), 
          LEval = [ exp( Ox, [], c([Ch],0,[]), C ) ]
         ;sappend(Ox,[1],Ox1),
          evalExp( Exp, Ox1, [(Ox,noTree)|C], LEval )).

% -- Show function
evalBuiltIn( f('show',1,[Exp]), Ox, C, LEval ) :- !,
         (normal_form(Exp),!,
          show(Exp,Str),
          toString(ResultExpr,Str),
          LEval = [ exp( Ox, [], ResultExpr, C) ]
         ;sappend(Ox,[1],Ox1),
          evalExp( Exp, Ox1, [(Ox,noTree)|C], LEval )).

% -- Undefined function
evalBuiltIn( f('undefined',0,[]), _, _, [] ) :- !.

