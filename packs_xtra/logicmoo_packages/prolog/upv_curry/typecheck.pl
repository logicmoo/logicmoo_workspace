:- set_prolog_flag(double_quotes, codes).
:- style_check(-discontiguous).
:- discontiguous buildRules/4.
:- expects_dialect(sicstus).
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  -----------------------------------------------------------------------
                  Check Type  - Milner / Mycroft algorithm
  -----------------------------------------------------------------------
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

:- dynamic
        lastTypeVar/1, typeVar/2,             % know next variable
        lastSimpVar/1, simpVar/2,             % simplify a type expresion
        defFunc/2,                            % which function defining now
        functionUserType/3,                   % function type defined by user
        annotation/2,                         % needed for deftree
        function/3, constructor/3,            % to know types of elements
        typeError/3,                          % raised type errors
        strategy/1, annotationAll/1.          % Info for Deftree module

basicTypes :- 
   % Basic Functions
   assert(function('==',2,curried(elem(typevar(1)), 
                                  curried(elem(typevar(1)),
                                          elem(type('Bool',0,[])))) )),
   assert(function('undefined',0,elem(typevar(1)))),
   assert(function('IfThenElse',3,
                     curried(elem(type('Bool',0,[])),
                           curried(elem(typevar(1)),
                                   curried(elem(typevar(1)),
                                           elem(typevar(1))))))).

cleanTypeVars :- 
        retractall(typeVar(_,_)), 
        retract(lastTypeVar(_)), 
        assert(lastTypeVar(0)).

cleanTypeAll :- 
        retractall(function(_,_,_)), retractall(functionUserType(_,_,_)),
        retractall(constructor(_,_,_)), retractall(type(_,_,_)),
        retractall(annotation(_,_)), retractall(defFunc(_,_)), 
        retractall(annotationAll(_)), 
        retractall(strategy(_)), assert(strategy(leftright)),
        cleanTypeVars.

:- basicTypes,assert(lastTypeVar(0)),assert(lastSimpVar(0)).

% --- Main Predicate, must return the list to deftrees 
%     without data and annotation nodes
checkType( Source, Checked ) :-
        cleanTypeAll,prelude_typechecker,
        write('Checking Types...'),nl,
        fixCheckType(Source,Checked).

% Fix point procedure for Type Checking
fixCheckType(Source,Checked) :-
        findall(function(X,Y,Z),function(X,Y,Z),FuncTypesBefore),
        checkType2(Source,SourceChecked),
        findall(function(X,Y,Z),function(X,Y,Z),FuncTypesAfter),
        ( %-No change in types of functions
          sameSet(FuncTypesAfter,FuncTypesBefore),!,
          Checked=SourceChecked
         ;%-Some type change
          fixCheckType(SourceChecked,Checked) ).

% Is the same set?
sameSet([],_).
sameSet([X|Xs],Set) :-
        member(X,Set),!,
        sameSet(Xs,Set).

% Type Check Case
checkType2( [], [] ).
checkType2( [pragma(Pragma)|Xs], Ys ) :-
        checkPragma(Pragma),
        !,checkType2(Xs,Ys).
checkType2( [data(Type,Constructors)|Xs], Ys ) :-
        checkData(Type,Constructors),
        !,checkType2(Xs,Ys).
checkType2( [annotation(F,Ann)|Xs], Ys ) :-
        checkAnn(annotation(F,Ann)),
        !,checkType2(Xs,Ys).
checkType2( [function(F,N,TypeFunction)|Xs], Ys ) :-
        checkFunctionType(function(F,N,TypeFunction)),
        !,checkType2(Xs,Ys).
checkType2( [rule(L,R)|Xs], Zs ) :-
        clean_forward(R,R1),
        (checkRule(L,R1,Rules)
        ;write('Type Error processing '),write2(trule(L,R)),nl,fail),
        sappend(Rules,Ys,Zs),
        !,checkType2(Xs,Ys).

% -- Data Types Control -------------------------------------------
checkPragma( Pragma ) :-
        ( (Pragma=flex; Pragma=rigid),
          (annotationAll(Pragma),
           format("Syntactic error, pragma ~a already included\n",[Pragma]),fail
          ;\+(annotationAll(Pragma)),
           assert(annotationAll(Pragma)) )
         ;Pragma=optmatch,
          (strategy(optmatch),
           format("Syntactic error, pragma ~a already included\n",[Pragma]),fail
          ;strategy(leftright),
           retractall(strategy(leftright)),
           assert(strategy(optmatch)) ) ).

% -- Data Types Control -------------------------------------------
checkData( Type, Constructors ) :-
        checkConstL(Constructors,elem(Type)).

checkConstL( [], _ ).
checkConstL( [X|Xs], Type ) :-
        checkConst(X,Type),
        checkConstL(Xs,Type).

checkConst( const(F,0,[]), Type ) :- 
        assert(constructor(F,0,Type)).

checkConst( const(F,N,Args), Type ) :- N > 0,
        typeConsts(Args,TypeArgs),
        addType(TypeArgs,Type,TypeConst),
        assert(constructor(F,N,TypeConst)).

typeConsts( [Type], Type ).
typeConsts( [Type1,Type2|Types], curried(Type1,Type3) ) :-
        typeConsts( [Type2|Types], Type3 ).

% -- Annotations Control -------------------------------------------
checkAnn( annotation(F,Ann) ) :-
        \+(annotation(F,_)),
        assert(annotation(F,Ann)).
checkAnn( annotation(F,_) ) :-
        annotation(F,_),
        format("Type error, function ~a has two or more evaluation annotations\n",[F]),
        fail.

% -- Function Type ---------------------------------------------------
checkFunctionType(function(F,N,TypeFunction)) :- % Normal Type Declaration
        \+(function(F,N,_)),
        \+(builtin(F)),
        assert(functionUserType(F,N,TypeFunction)).
checkFunctionType(function(F,N,TypeFunction)) :- % Type Decl for built-in functions
        \+(function(F,N,_)),
        builtin(F),
        assert(function(F,N,TypeFunction)).
checkFunctionType(function(F,N,_)) :-
        function(F,N,_),
        format("Type error, function ~a has a type\n",[F]),
        fail.

% -- Rules Control ---------------------------------------------------------
% Args has only constructors and vars, 
% Rx is R modified if there exists a forward node 
% replaced by its function symbol
checkRule(f(F,N,Args),R,Rules) :-
        cleanTypeVars,

        %Step 1: Check Arguments
        (  N > 0, sCheckExprs(Args, TypeArgs, [], Sust1),
           nextTypeVar(NewVar), TypeResul=elem(typevar(NewVar)), 
           addType(TypeArgs,TypeResul,TypeFunc1) 
         ; N = 0, nextTypeVar(NewVar), 
           TypeResul=elem(typevar(NewVar)), TypeFunc1=TypeResul,
           Sust1=[] ),
        %Variables to next step: Susts1, TypeFunc1, TypeResul

        %Step 2: Check Previous user Type or Previous Type
        %        Asserts a type for this function
        (  %%%% Non User's Type, use Milner type inference algorithm
           \+(functionUserType(F,_,_)),
           (  retract(function(F,_,OldType)), Old = yes
            ; \+(function(F,_,_)), Old = no ),
           assert(function(F,N,TypeFunc1)), assert(defFunc(F,N))
         ; %%%% User's Type, use Mycroft algorithm
           functionUserType(F,N,UserType), Old = no,
           retractall(function(F,N,_)),assert(function(F,N,UserType)), 
           retractall(defFunc(_,_)) ),
        %Variables to next step: Old, OldType(if Old=yes)

        %Step 3: Check right hand expression
        (  %Multiple conditional expressions
           R=multiple(Exps),!,
           checkMultiple(Exps,Sust1,Sust2,TypeR1,f(F,N,Args),Rules)
         ; %Simple conditional expression
           R=f('\\=>',2,[_,_]),!,
           checkMultiple([R],Sust1,Sust2,TypeR1,f(F,N,Args),Rules)
         ; %Simple Right Expression
           Rules=[rule(f(F,N,Args),R)],
           sCheckExpr(R,TypeR1,Sust1,Sust2)
        ),!,
        applySusts(Sust2,TypeR1,TypeR2),
        unifyType(TypeResul,TypeR2,SustU), 
        composeSusts(Sust2,SustU,Sust3),
        applySusts(Sust3,TypeFunc1,TypeFunc1a),
        adjust_final_type(TypeFunc1a,TypeFunc2),
        %Variables to next step: TypeFunc2
        
        %Step 4: If theres is an older type, 
        % check if current and older are unifiable
        (  Old = yes, 
           freshVarType(OldType,OldType1), 
           (  unifyType(OldType1,TypeFunc2,SustsOld),!, 
              applySusts(SustsOld,TypeFunc2,TypeFunc3)
            ; format('Incompatible types of function ~a:\n',[F,N]),
              tab(5),write2(OldType1),nl, 
              tab(5),write2(TypeFunc2),nl,fail )
         ; Old = no, TypeFunc3=TypeFunc2 ),
        %Variables to next step: TypeFunc3

        %Step 5: Stores Function Type
        (  %Asserts obtained type
           \+(functionUserType(F,N,_)),
           retractall(function(F,N,TypeFunc1)), retractall(defFunc(F,N)),
           simpType(TypeFunc3,TypeAssert), assert(function(F,N,TypeAssert))
         ; %Asserts user's type
           retractall(functionUserType(F,N,UserType)),
           assert(functionUserType(F,N,UserType)),
           retractall(function(F,N,_)), assert(function(F,N,UserType)) ).

% remove forward node if there's a type for it
clean_forward( c(C,N,Args), c(C,N,ArgsX) ) :-
        clean_forwardL( Args, ArgsX ).
clean_forward( f(F,N,Args), f(F,N,ArgsX) ) :-
        clean_forwardL( Args, ArgsX ).
clean_forward( multiple(Cts), multiple(CtsX) ) :-
        clean_forwardL( Cts, CtsX ).
clean_forward( v(V), v(V) ).
clean_forward( forward(F,Nf,Args), Resul ) :-
        ( function(F,N,_),
          Resul=f(F,N,ArgsX)
         ;\+(function(F,_,_)),
          Resul=forward(F,Nf,ArgsX) ),
        clean_forwardL( Args, ArgsX ).

clean_forwardL( [], [] ).
clean_forwardL( [X|Xs], [Y|Ys] ) :-
        clean_forward(X,Y),
        clean_forwardL(Xs,Ys).
        
% Corrects the final type of functions that returns a complex type
adjust_final_type(curried(X,Xs),curried(X,Ys)) :-
        adjust_final_type(Xs,Ys).
adjust_final_type(elem(curried(X,Xs)),curried(X,Xs)).
adjust_final_type(elem(typevar(X)),elem(typevar(X))).
adjust_final_type(elem(type(T,N,Args)),elem(type(T,N,Args))).

% --- Check Conditional Expressions
% Args: conditional expressions
%       previous Substitution
%       end type of function
%       left Expression
%       returned Rules
checkMultiple(Exps,Sust1,Sust2,ResultType,Left,Rules) :-
        checkMExprs(Exps,Sust1,Sust2,_,ResultType,GuardType),
        buildRules(GuardType,Left,Exps,Rules).

%Check conditional right expression
checkMExprs([],Sust,Sust,RType,RType,_).
checkMExprs([Expr|Exprs],Sust1,Sust6,RType1,RType3,GType) :-
        Expr = f('\\=>',2,[ExpCond,Exp]),
        %-ExpCond should be a Constraint or a Boolean
        sCheckExpr(ExpCond,TypeExpCond,Sust1,Sust2),
        (  unifyType(TypeExpCond,elem(type('Constraint',0,[])),SustU1),!,
           GType=elem(type('Constraint',0,[]))
         ; unifyType(TypeExpCond,elem(type('Bool',0,[])),SustU1),!,
           GType=elem(type('Bool',0,[]))
         ; %Two errors (because it is not Constraint and Boolean)
           assert(typeError(TypeExpCond,elem(type('Constraint',0,[])),ExpCond)),
           assert(typeError(TypeExpCond,elem(type('Bool',0,[])),ExpCond)),
           fail ),
        composeSusts(Sust2,SustU1,Sust3),
        %-Find Expr's type 
        sCheckExpr(Exp,TypeExpr,Sust3,Sust4),
        applySusts(Sust4,TypeExpr,TypeExpr2),
        %-Check unifiable with type of other conditionals
        (  unifyType(TypeExpr2,RType1,SustU2)
         ; \+(unifyType(TypeExpr2,RType1,_)),
           assert(typeError(TypeExpr2,RType1,Expr)),
           fail ),
        composeSusts(Sust4,SustU2,Sust5),
        applySusts(SustU2,RType1,RType2),
        %Process Rest
        !,checkMExprs(Exprs,Sust5,Sust6,RType2,RType3,GType).

%Only one boolean conditional (Translate to constrained expression)
buildRules(elem(type('Bool',0,[])),Left,[Expr],Rules) :- !, 
        Expr = f('\\=>',2,[ExpBool,Exp]),
        Right = f('\\=>',2,[ f('=:=',2,[ExpBool,c('True',0,[])]) ,Exp]),
        Rules=[rule(Left,Right)].

%Translate multiple boolean conditional to nested if_then_else
buildRules(elem(type('Bool',0,[])),Left,Exps,Rules) :- !, 
        Rules=[rule(Left,NestedIf)],
        buildNestedIf(Exps,NestedIf).

buildNestedIf([Expr],NestedIf) :-
        Expr = f('\\=>',2,[ExpCond,Exp]),
        NestedIf = f('IfThenElse',3,[ExpCond,Exp,f('undefined',0,[])]).
buildNestedIf([Expr1,Expr2|Exps],NestedIf) :-
        Expr1 = f('\\=>',2,[ExpCond,Exp]),
        NestedIf = f('IfThenElse',3,[ExpCond,Exp,RestIf]),
        buildNestedIf([Expr2|Exps],RestIf).
        
%Translate multiple constrained conditional to different equations
buildRules(elem(type('Constraint',0,[])),Left,Exps,Rules) :- !, 
        buildDiffRules(Exps,Left,Rules).

buildDiffRules([],_,[]).
buildDiffRules([Exp|Exps],Left,Rules) :-
        Rules=[rule(Left,Exp)|RestRules],
        buildDiffRules(Exps,Left,RestRules).

% --- Secure Expression Checking handling error messages
sCheckExpr( Expr, Type, Susts1, Susts2 ) :-
        retractall(typeError(_,_,_)),
        ( checkExpr( Expr, Type, Susts1, Susts2 ),!
         ;findall((Type1,Type2,Exp3),typeError(Type1,Type2,Exp3),ErrorList),
          showErrors(ErrorList),
          !,fail ).

%-Used in checkRule. Must be equal to CheckExpr except error handling
sCheckExprs( [Expr|Exprs], TypeExpr, Susts1, Susts3 ) :-
        retractall(typeError(_,_,_)),
        ( %--Normal Execution
          checkExpr( Expr, Type1, Susts1, Susts2 ),
          %-correct to guarantee consistency
          ( Type1=curried(_,_), Type1x=elem(Type1)
           ;Type1=elem(_), Type1x=Type1 ),
          %-Get type of Exprs
          (  sCheckExprs( Exprs, Type2, Susts2, Susts3 ),
             TypeExpr=curried(Type1x,Type2)
           ; Exprs=[], TypeExpr=Type1x, Susts3=Susts2 ),
          !
         ;%--Error Handling
          findall((Type1,Type2,Exp3),typeError(Type1,Type2,Exp3),ErrorList),
          showErrors(ErrorList),
          !,fail ).

showErrors([]).
showErrors([(Type1,Type2,Exp)|Xs]) :-
        write('Type error, type '),
        write2(Type1),write(' is not correct when expected type '),write2(Type2),
        write(' in expression '),write2(Exp),nl,
        showErrors(Xs).

% --- Type of a Expression ------------------------------------------------
% returns the sustitution computed, 
% the result type and the result expression
% Note: no wrong cut must be included to allow the procedure to find the correct
%       type due to the inclusion of the overloaded operators (slow but works)

%---Builtin constructors
checkExpr( c([_],0,[]), elem(type('Char',0,[])), Susts, Susts ) :- !.

checkExpr( c(Num,0,[]), elem(type('Int',0,[])), Susts, Susts ) :- 
        integer(Num),!.

checkExpr( c('Cons',2,[Term1,List1]), TypeResul, Susts1, Susts4 ) :- !,
        checkExpr(Term1,TypeTerm,Susts1,Susts2),
        checkExpr(List1,TypeList,Susts2,Susts3),
        TypeResul = elem(type('List',1,[TypeTerm])),
        (  unifyType(TypeList,TypeResul,SustsU)
         ; \+(unifyType(TypeList,TypeResul,_)),
           assert(typeError(TypeList,TypeResul,List1)),
           fail ),
        composeSusts(Susts3,SustsU,Susts4).
checkExpr( c('Nil',0,[]), TypeResul, Susts, Susts ) :- !,
        nextTypeVar(NextVar),
        TypeResul = elem(type('List',1,[elem(typevar(NextVar))])).

checkExpr( c('Tuple0',0,[]), TypeResul, Susts, Susts ) :- !,
        TypeResul = elem(type('Tuples',0,[])).
checkExpr( c(TupleName,N,Exprs), TypeResul, Susts1, Susts2 ) :-
        tuple2arity(TupleName,N),N>0,!,
        checkExprL( Exprs, TypeExprs, Susts1, Susts2 ),
        TypeResul = elem(type('Tuples',N,TypeExprs)).

%---Normal Constructors
checkExpr( c(F,0,[]), TypeF, Susts, Susts ) :- 
        constructor(F,0,Type), freshVarType(Type,TypeF).
checkExpr( c(F,N,Args), TypeResul, Susts1, Susts3 ) :- N > 0,
        constructor(F,N,TypeConst), freshVarType(TypeConst,TypeConstF),
        checkExprs(Args,TypeArgs,Susts1,Susts2),
        (  applyType(TypeArgs,TypeConstF,TypeResul,SustsA)
         ; \+(applyType(TypeArgs,TypeConstF,_,_)),
           nextTypeVar(NewVar),
           addType(TypeArgs,elem(typevar(NewVar)),TypeArgs1),
           assert(typeError(TypeArgs1,TypeConstF,c(F,N,Args))),
           fail ),
        composeSusts(Susts2,SustsA,Susts3).

%---Apply function (High Order)
checkExpr( f('\\@',_,[ExprF|Terms]), TypeExpr, Susts1, Susts4 ) :- !,
        checkExpr(ExprF,TypeExprF1,Susts1,Susts2),
        (TypeExprF1=elem(curried(X,Y)),!,TypeExprF=curried(X,Y)
        ;TypeExprF=TypeExprF1),
        checkExprs(Terms,TypeExprs,Susts2,Susts3),
        nextTypeVar(NewVar), TypeExpr = elem(typevar(NewVar)),
        addType(TypeExprs,TypeExpr,TypeExprFunc), 
        (  unifyType(TypeExprF,TypeExprFunc,SustsU)
         ; \+(unifyType(TypeExprF,TypeExprFunc,_)),
           assert(typeError(TypeExprFunc,TypeExprF,f('\\@',_,[ExprF|Terms]))),
           fail ),
        composeSusts(Susts3,SustsU,Susts4).

%---Lambda Application (High Order)
checkExpr( f('\\',_,[Vars,Expr|Exprs]), TypeResul, Susts1, Susts5 ) :- 
        % get pattern expression
        Vars=c('\\Vars',_,Pattern),
        % get function type
        checkExprs( Pattern, TypePattern, Susts1, Susts2 ),
        checkExpr( Expr, TypeExpr, Susts2, Susts3 ),
        addType(TypePattern,TypeExpr,TypeFunc),
        % get args type
        ( Exprs=[_|_],
          checkExprs( Exprs, TypeExprs, Susts3, Susts4 ),
          applyType(TypeExprs,TypeFunc,TypeResul,SustsA),
          composeSusts(Susts4,SustsA,Susts5)
         ;Exprs=[],
          Susts5=Susts3, TypeResul=TypeFunc  ).

%---Standard check function
checkExpr( f(F,N,[]), TypeFuncF, Susts, Susts ) :- 
        function(F,N,TypeFunc),
        (  \+(defFunc(F,N)), freshVarType(TypeFunc,TypeFuncF)
         ; defFunc(F,N), TypeFuncF=TypeFunc ).
checkExpr( f(F,N,Args), TypeResul, Susts1, Susts3 ) :- N>0,
        checkExprs(Args,TypeArgs,Susts1,Susts2),
        function(F,N,TypeFuncOrig), 
        (  \+(defFunc(F,N)), freshVarType(TypeFuncOrig,TypeFunc)
         ; defFunc(F,N), TypeFunc=TypeFuncOrig ),
        applySusts(Susts2,TypeFunc,TypeFuncF),
        (  applyType(TypeArgs,TypeFuncF,TypeResul,SustsA)
         ; \+(applyType(TypeArgs,TypeFuncF,_,_)),
           nextTypeVar(NewVar),
           addType(TypeArgs,elem(typevar(NewVar)),TypeArgs1),
           assert(typeError(TypeArgs1,TypeFuncF,f(F,N,Args))),
           fail ),
        composeSusts(Susts2,SustsA,Susts3).

%---Function not seen yet
checkExpr( forward(F,N,Args), TypeResul, Susts1, Susts2 ) :-
        \+(function(F,N,_)),
        (Args=[_|_],checkExprs(Args,_,Susts1,Susts2)
        ;Args=[],Susts2=Susts1),
        nextTypeVar(NewVar), TypeResul = elem(typevar(NewVar)).

checkExpr( forward(F,N,Args), TypeResul, Susts1, Susts2 ) :-
        function(F,N,_),
        checkExpr( f(F,N,Args), TypeResul, Susts1, Susts2 ).

%---Variables
checkExpr( v(N), TypeVar, Susts, Susts ) :-
        typeVar(N,M),
        applySusts(Susts,elem(typevar(M)),TypeVar).
checkExpr( v(N), TypeVar, Susts, Susts ) :-
        \+(typeVar(N,_)), nextTypeVar(M), assert(typeVar(N,M)),
        applySusts(Susts,elem(typevar(M)),TypeVar).

% ---
checkExprs( [Expr|Exprs], TypeExpr, Susts1, Susts3 ) :-
        checkExpr( Expr, Type1, Susts1, Susts2 ),
        %-correct to guarantee consistency
        ( Type1=curried(_,_), Type1x=elem(Type1)
%         ;Type1=elem(_), Type1x=Type1 ),
         ;Type1=elem(_), Type1x=Type1 ),
        %-Get type of Exprs
        (  checkExprs( Exprs, Type2, Susts2, Susts3 ),
           TypeExpr=curried(Type1x,Type2)
         ; Exprs=[], TypeExpr=Type1x, Susts3=Susts2 ).

checkExprL( [Expr|Exprs], TypeResul, Susts1, Susts3 ) :-
        checkExpr( Expr, TypeExpr, Susts1, Susts2 ),
        (  TypeResul=[TypeExpr|TypeExprs],
           checkExprL( Exprs, TypeExprs, Susts2, Susts3 )
         ; Exprs=[], TypeResul=[TypeExpr], Susts3=Susts2 ).

/* ------------------------------------
                Types
   ------------------------------------ */
addType( curried(Type1,Type2), Type, curried(Type1,Type3) ) :- !, 
        addType( Type2, Type, Type3 ).
addType( elem(TypeE), Type, curried(elem(TypeE),Type) ).

% --- gets the result type with unification steps
applyType( curried(Type1,Type1L), curried(Type2,Type2L), Type3, Susts ) :- 
        unifyType(Type1,Type2,Susts1),
        applySusts(Susts1,Type1L,Type1Lx), applySusts(Susts1,Type2L,Type2Lx),
        applyType(Type1Lx,Type2Lx,Type3,Susts2),!,
        composeSusts(Susts1,Susts2,Susts).
applyType( elem(Type1), curried(Type2,Type3), Type3x, Susts1 ) :- 
        unifyType(elem(Type1),Type2,Susts1),
        applySusts(Susts1,Type3,Type3x).

% --- unify two types
unifyType( curried(elem(Type1),Type1L), curried(elem(Type2),Type2L), Susts3 ) :- !,
        unifyEType(Type1,Type2,Susts1),
        applySusts(Susts1,Type1L,Type1Lx), applySusts(Susts1,Type2L,Type2Lx),
        unifyType(Type1Lx,Type2Lx,Susts2),
        composeSusts(Susts1,Susts2,Susts3).
unifyType( elem(Type1), elem(Type2), Susts ) :-
        unifyEType( Type1, Type2, Susts ).
unifyType( elem(typevar(Var)), curried(Type2,Type2L), Susts ) :- % special
        unifyEType( typevar(Var), curried(Type2,Type2L), Susts ).
unifyType( curried(Type1,Type1L), elem(typevar(Var)), Susts ) :- % special
        unifyEType( curried(Type1,Type1L), typevar(Var), Susts ).

unifyEType( curried(Type1,Type1L), curried(Type2,Type2L), Susts ) :- 
        unifyType( curried(Type1,Type1L), curried(Type2,Type2L), Susts ).
unifyEType( type(F,N,Args1), type(F,N,Args2), Susts ) :- 
        unifyTypeL( Args1, Args2, [], Susts ).
unifyEType( typevar(N), typevar(N), [] ) :- !. % special case when checking recursive
                                               % function definition
unifyEType( typevar(N), Type, [s(N,Type)] ) :- 
        \+(unifyOccurChk(typevar(N),Type)).
unifyEType( Type, typevar(N), [s(N,Type)] ) :- 
        \+(Type=typevar(_)), \+(unifyOccurChk(typevar(N),Type)).

unifyTypeL( [], [], Susts, Susts ).
unifyTypeL( [X1|Xs1], [X2|Xs2], Susts1, Susts3 ) :-
        unifyType(X1,X2,Susts), sappend(Susts1,Susts,Susts2),
        unifyTypeL(Xs1,Xs2,Susts2,Susts3).

% case for the args of a function is not included in this ocurrence check
unifyOccurChk( typevar(N), elem(Type) ) :-
         unifyOccurChk( typevar(N), Type ).
unifyOccurChk( typevar(N), typevar(N) ).
unifyOccurChk( typevar(N), curried(Type,TypeL) ) :- 
         unifyOccurChk(typevar(N),Type)
        ;unifyOccurChk(typevar(N),TypeL).

% --- get new type
freshVarType( Type1, Type2 ) :-
        lastTypeVar(MaxVar),
        renameType( Type1, MaxVar, Type2 ),
        maxvarType( Type2, 0, MaxVar2 ),
        updateTypeVars(MaxVar2).

maxvarType( curried(Type1,Type2), MaxVar1, MaxVar3 ) :-
        maxvarType( Type1, MaxVar1, MaxVar2 ),
        maxvarType( Type2, MaxVar2, MaxVar3 ).
maxvarType( elem(Type), MaxVar1, MaxVar2 ) :-
        maxvarType( Type, MaxVar1, MaxVar2 ).
maxvarType( type(_,_,Vars), MaxVar1, MaxVar2 ) :-
        maxvarTypeL( Vars, MaxVar1, MaxVar2 ).
maxvarType( typevar(Var), MaxVar1, MaxVar2 ) :-
        ( Var =< MaxVar1, MaxVar2 = MaxVar1
         ;Var > MaxVar1, MaxVar2 = Var ).

maxvarTypeL( [], MaxVar, MaxVar ).
maxvarTypeL( [X|Xs], MaxVar1, MaxVar3 ) :-
        maxvarType( X, MaxVar1, MaxVar2 ),
        maxvarTypeL( Xs, MaxVar2, MaxVar3 ).

renameType( curried(Type1,Type2), MaxVar, curried(Type1x,Type2x) ) :-
        renameType( Type1, MaxVar, Type1x ),
        renameType( Type2, MaxVar, Type2x ).
renameType( elem(Type1), MaxVar, elem(Type2) ) :- 
        renameType( Type1, MaxVar, Type2 ).
renameType( type(F,N,Vars1), MaxVar, type(F,N,Vars2) ) :- 
        renameTypeL(Vars1,MaxVar,Vars2).
renameType( typevar(Var1), MaxVar, typevar(Var2) ) :- 
        Var2 is Var1 + MaxVar.

renameTypeL( [], _, [] ).
renameTypeL( [X1|Xs1], MaxVar, [X2|Xs2] ) :-
        renameType( X1, MaxVar, X2 ),
        renameTypeL( Xs1, MaxVar, Xs2 ).

% --- apply substitutions to types
applySusts([],Type,Type).
applySusts([s(N,Type)|Susts],Type1,Type3) :-
        applySust(Type1,N,Type,Type2),
        applySusts(Susts,Type2,Type3).

applySust(curried(Type1,Type2),N,Type,curried(Type1x,Type2x)) :-
        applySust(Type1,N,Type,Type1x),
        applySust(Type2,N,Type,Type2x).
applySust(elem(Type1),M,Type,elem(Type3)) :-
        applySust(Type1,M,Type,Type2),
        (Type2=elem(Type3),! ; Type3=Type2 ).
applySust(type(F,N,Vars1),M,Type,type(F,N,Vars2)) :-
        applySustL(Vars1,M,Type,Vars2).
applySust(typevar(N),N,Type,Type).
applySust(typevar(M),N,_,typevar(M)) :- M =\= N.

applySustL( [], _, _, [] ).
applySustL( [X1|Xs1], N, Type, [X2|Xs2] ) :-
        applySust(X1,N,Type,X2),
        applySustL(Xs1,N,Type,Xs2).

% --- to compose sustitutions
composeSusts([],Susts,Susts).
composeSusts([s(N,Type1)|Susts1],Susts,[s(N,Type2)|Susts2]) :-
        applySusts(Susts,Type1,Type2),
        delRedundant(Susts,N,SustsX),
        composeSusts(Susts1,SustsX,Susts2).

delRedundant([],_,[]).
delRedundant([s(N,_)|Susts1],N,Susts2) :- delRedundant(Susts1,N,Susts2).
delRedundant([s(M,Type)|Susts1],N,[s(M,Type)|Susts2]) :- M =\= N, 
        delRedundant(Susts1,N,Susts2).

% --- simplify type expresion
simpType( Type1, Type2 ) :- 
        cleanSimpVars, 
        simpType2( Type1, Type2 ), 
        cleanSimpVars.

simpType2( typevar(N), typevar(M) ) :- 
        simpVar(N,typevar(M)).
simpType2( typevar(N), typevar(M) ) :- 
        \+(simpVar(N,_)), 
        nextSimpVar(M), assert(simpVar(N,typevar(M))).
simpType2( curried(Type1,Type1s), curried(Type2,Type2s) ) :-
        simpType2( Type1, Type2 ),
        simpType2( Type1s, Type2s ).
simpType2( type(F,N,Vars1), type(F,N,Vars2) ) :- 
        simpType2L(Vars1,Vars2).
simpType2( elem(Type1), elem(Type2) ) :- 
        simpType2( Type1, Type2 ).

simpType2L( [], [] ).
simpType2L( [X|Xs], [Y|Ys] ) :- simpType2(X,Y), simpType2L(Xs,Ys).

nextSimpVar( Next ) :-
        lastSimpVar(Last), Next is Last + 1, 
        retract(lastSimpVar(Last)), assert(lastSimpVar(Next)).
cleanSimpVars :- retractall(simpVar(_,_)), retract(lastSimpVar(_)), 
        assert(lastSimpVar(0)).

% --- Variables Control
nextTypeVar( Next ) :-
        lastTypeVar(Last), Next is Last + 1, 
        retract(lastTypeVar(Last)), assert(lastTypeVar(Next)).

updateTypeVars(Last) :- 
        retract(lastTypeVar(N)), 
        (  Last > N, assert(lastTypeVar(Last)) 
         ; Last =< N, assert(lastTypeVar(N)) ).

