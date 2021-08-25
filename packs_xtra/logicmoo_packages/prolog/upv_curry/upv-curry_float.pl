/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                            UPV-Curry Interpreter

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

:-module(curry,[process_curry/1,main/0]).

process_curry(Input):-
  must_det_l((
    is_cdl(Input,InputS),
    string_codes(InputS,InputS1))),
    process_then(InputS1,true),!.


% =========================================================================
%  Add this directory and the pack files (also Logicmoo Library Utils)
% =========================================================================
:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir), DirFor = upv_curry,
   absolute_file_name('../../..',Y,[relative_to(Dir),file_type(directory)]),
   (user:file_search_path(DirFor,Dir);asserta(user:file_search_path(DirFor,Dir))) ->
   (user:file_search_path(pack,Y);asserta(user:file_search_path(pack,Y))) -> attach_packs.
:- initialization(attach_packs).
:- user:ensure_loaded(library(logicmoo_utils)).
% =========================================================================
:- expects_dialect(sicstus).
:- thread_local(current_prolog_flag_double_quotes/1).
:- current_prolog_flag(double_quotes, ResetTo),asserta(current_prolog_flag_double_quotes(ResetTo)).
:- set_prolog_flag(double_quotes, codes).

is_cdl(A,A):-var(A),!.
is_cdl(A,B):-string(A),!,show_call(string_codes(A,B)).
is_cdl(A,A):-must(is_list(A)),!.
is_cdl(A,B):- sicstus_atom_chars(A,B),!. 

sappend(A0,B0,C0):- must(is_cdl(A0,A)),must(is_cdl(B0,B)),must(is_cdl(C0,C)),append(A,B,C).
sicstus_atom_chars(N,S):-name(N,S).


% Last update: 12/29/1998

% This is the UPV-Curry system including floats.
% NEW comment indicates added source lines not present in the basic UPV-Curry.
% OLD comment indicates previous source lines present in the basic UPV-Curry.

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ---------------------------------------------------------------------
                     Parser for Haskell syntax
  ---------------------------------------------------------------------
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*
 Syntax:
          Term:
          c(F,N,Terms) - constructor with name F arity N (original) and 
                         arguments Terms
          f(F,N,Terms) - function with same shape
          v(N)         - variable number N
        State:
          [er(Susts,Exp)] - State with sustitutions computed and 
                            last expression result
          Non evaluable expression denoted by fail
          [] - empty state for evaluation with error
        Substitutions:
          [s(N,Term)] - List of pairs (variable, expression)
        Definitional Tree:
          branch(Pattern,Ocurrence,Type,Trees)
          rule(TermL,TermR)
          tor(Tree1,Tree2)

 Notes:
  - Conditional rules expressed with constructor symbol f('\=>',2,[...])
   where " l | c = r " is written as 
   "rule( l, f('\\=>',2,[c,r]) )"
  - High Order (partial application of functions) using "f('\@',N,[...])" 
   with function symbol in the first argument and 
   the argument expressions in the other arguments.
  - Lambda Expressions using "f('\',N1,[c('\Var',N2,Pattern),Expr|Args])" 
   where Pattern is the arguments of the lambda expression, 
   Expr is the right expression of the lambda expression and
   Args is the set of expressions passed as arguments 
   to the lambda expression (if any).
  - In SICSTus Prolog the symbol '\' is treated as special, 
   so '\=>' has to be '\\=>' and so on.
  - "forward" node denotes a reference to a function not passed yet.
*/

:- dynamic 
        lastvar/1, var_seen/2, % variables control
        noRedef/1, % does not allowed to redefine a function
        infix/3, % associative disambiguity (c - xfx, r - xfy, l - yfx)
        type/2, function/2, constructor/2, % elements found
        ini/0. % to allow new variables in expressions

/*
  returns:
        rule/2        - accepted rule
           c/3        - constructor
           f/3        - function
           forward/3  - function non existent when found
           v/1        - variable
           multiple/1 - multiple right expressions in conditional rule
        annotation/3  - annotation for function F/N (flex or rigid)
        function/3    - user type for function F/N
        pragma/1      - pragmas included in program
        data/2        - data type
           const/3    - constructor found
           curried/2  - "->" operator 
           elem/1     - each element in a "->" application
           type/3     - type found
           typevar/1  - type variable passed
*/

basicParsing :- 
   % Basic types
   assert(builtin('Int')),        assert(type('Int',0)),
   assert(builtin('Float')),      assert(type('Float',0)),        % NEW
   assert(builtin('Char')),       assert(type('Char',0)),
   assert(builtin('String')),     assert(type('String',0)),
   assert(builtin('Constraint')), assert(type('Constraint',0)),
   % Global functions
   assert(builtin('show')),
   assert(builtin('undefined')),  assert(function('undefined',0)),
   assert(infix(':',5,r)), %This is not a real operator, but we need some precedence
   % Conditional Functions
   assert(builtin('IfThenElse')), assert(function('IfThenElse',3)),
   assert(builtin('if')), 
   assert(builtin('then')), 
   assert(builtin('else')),
   % Constraint functions
   assert(builtin('=:=')),
   assert(builtin('success')),
   assert(builtin('&')),
   % Arithmetic functions
   %assert(builtin('+')),                                             % OLD
   assert(builtin('+')),   assert(function('+',2)),                   % NEW
   %assert(builtin('-')),                                             % OLD
   assert(builtin('-')),   assert(function('-',2)),                   % NEW
   %assert(builtin('*')),                                             % OLD
   assert(builtin('*')),   assert(function('*',2)),                   % NEW
   %assert(builtin('div')),                                           % OLD
   assert(builtin('div')), assert(function('div',2)),                 % NEW
   %assert(builtin('mod')),                                           % OLD
   assert(builtin('mod')), assert(function('mod',2)),                 % NEW
   assert(builtin('/')),   assert(function('/',2)),                   % NEW
   % Relational functions
   assert(builtin('==')),  assert(function('==',2)),
   %assert(builtin('<')),                                             % OLD
   assert(builtin('<')),   assert(function('<',2)),                   % NEW
   %assert(builtin('>')),                                             % OLD
   assert(builtin('>')),   assert(function('>',2)),                   % NEW
   %assert(builtin('=<')),                                            % OLD
   assert(builtin('=<')),  assert(function('=<',2)),                  % NEW
   %assert(builtin('>=')),                                            % OLD
   assert(builtin('>=')),  assert(function('>=',2)),                  % NEW
   % IO functions
   assert(builtin('IO')),         assert(type('IO',1)),
   assert(builtin('>>')),
   assert(builtin('>>=')),
   assert(builtin('putChar')),
   assert(builtin('getChar')),
   assert(builtin('done')),
   assert(builtin('return')),
   assert(builtin('readFile')),
   assert(builtin('writeFile')),
   % Conversion Functions
   assert(builtin('ord')),
   assert(builtin('chr')),
   % Float - Int Conversion       % NEW
   assert(builtin('float')),      % NEW
   assert(builtin('int')).        % NEW

cleanFuncConsTypes :-
        retractall(ini),
        retractall(infix(_,_,_)),
        retractall(builtin(_)),
        retractall(type(_,_)),
        retractall(constructor(_,_)),
        retractall(function(_,_)).

:- cleanFuncConsTypes,basicParsing,
   assert(lastvar(0)).

% ----------------------------------------
% Get file and parse it
% ----------------------------------------

parser( File, FinalResult ) :-
        (  file_exists(File),!
         ; write('Parsing error, file doesn\'t exist '), write(File),nl,fail ),
        (  open( File, read, Stream ),
           read_all( Text, Stream ),
           close( Stream ),!
         ; write('Parsing error, file cannot be read '), 
           write(File),nl,fail ),        
        parse_text(File,Text,FinalResult).

parse_text(File, TextA, FinalResult):-
       is_cdl(TextA,Text),
       (
        (  process( Text, Input ),!         %clean enters, tabs, and blanks
          ; write('Lexical error, file cannot be processed '), write(File),
            nl,fail ),
        (  retractall(annotation(_,_)),
           program( Result, Input, [] ),!,
           treat( Result, FinalResult )
         ; write('Syntactic error, file cannot be parsed '), write(File),
           nl,fail )
        ),
       !. % sure no stupid backtrack will be done

parse_text(Text, FinalResult):-
    parse_text('(current_input)', Text, FinalResult).
parse_text(Text):- parse_text(Text, FinalResult),dmsg(FinalResult),
   checkType( FinalResult, Rules ),
        % level_confluence, non_ovelapping,
        deftrees(Rules).



command_exec( Input ) :-
        ( append( ":exec ", Str, Input )
         ;append( ":x ",    Str, Input ) ), !,
        parse_text(Str),!.

read_all( Text, Stream ) :-
        get0( Stream, X ),
        ( X =\= -1, Text = [X|Text2], !,read_all( Text2, Stream )
         ;X =:= -1, Text = [] ).

% ----- delete extra characters
process( Text1, Text4 ) :-
        clean_comments( Text1, Text2 ),!,
        clean_enters( Text2, Text3 ),!,
        clean_blanks( Text3, Text4 ).
        
% --- clean all comments (nested comments {- ... -})
clean_comments( [45,45|Text1], Text3 ) :- !,
        clean_simple_comment( Text1, Text2 ),
        clean_comments( Text2, Text3 ).
clean_comments( [123,45|Text1], Text3 ) :- !,
        clean_complex_comment( Text1, Text2, 1 ),
        clean_comments( Text2, Text3 ).
clean_comments( [X|Text1], [X|Text2] ) :-
        clean_comments( Text1, Text2 ).
clean_comments( [], [] ).

clean_simple_comment( [10|Text], [10|Text] ) :- !.
clean_simple_comment( [13|Text], [10|Text] ) :- !.
clean_simple_comment( [_|Text1], Text2 ) :-
        clean_simple_comment( Text1, Text2 ).
clean_simple_comment( [], [10] ). % sure terminate

clean_complex_comment( Text, Text, 0 ) :- !.
clean_complex_comment( [123,45|Text1], Text2, N ) :- !,
        N1 is N + 1,
        clean_complex_comment( Text1, Text2, N1 ).
clean_complex_comment( [45,125|Text1], Text2, N ) :- !,
        N1 is N - 1,
        clean_complex_comment( Text1, Text2, N1 ).
clean_complex_comment( [_|Text1], Text2, N ) :-
        clean_complex_comment( Text1, Text2, N ).
clean_complex_comment( [], _, _ ) :-
        write('Lexical error, expected -}'),nl,fail.

% --- clean unnecessary enters (leave only those which bound functions)
clean_enters( [], [] ) :- !.
clean_enters( Text1, Text5 ) :-
        get_ToEnter( Text1, Text1x1, Text2 ),
        (
           Text1x1 = [],                       % a blank line
           clean_enters( Text2, Text5 )
         ; append( Text1x1, Text1x3, Text1x ), % normal case
           clean_Declaration( Text2, Text1x2, Text3, 0 ),
           append( Text1x2, [10], Text1x3 ),
           append( Text1x, Text4, Text5 ),!,
           clean_enters( Text3, Text4 ) 
        ).

% - third arg indicates actual tab stop
clean_Declaration( [], [], [], _ ) :- !.
clean_Declaration( Text1, Text1X, Text3, Tab ) :-
        get_tab( Text1, 0, TabT ),
        (  TabT > Tab,
           get_ToEnter( Text1, Text1x1, Text2 ),
           append( Text1x1, Text1x2, Text1X ),
           clean_Declaration( Text2, Text1x2, Text3, Tab )
         ; TabT =:= Tab,
           Text1X = [],
           Text3 = Text1 ).

get_tab( [32|Text], Tab1, Tab3 ) :- 
        Tab2 is Tab1 + 1,
        get_tab( Text, Tab2, Tab3 ).
get_tab( [9|Text], Tab1, Tab3 ) :- 
        Tab2 is Tab1 + 1,
        get_tab( Text, Tab2, Tab3 ).
get_tab( [X|_], Tab, Tab ) :- X \== 32, X \== 9.

get_ToEnter( [], [], [] ).
get_ToEnter( [10|Text], [], Text ) :- !.
get_ToEnter( [13|Text], [], Text ) :- !.
get_ToEnter( [X|Text1], [X|Text2], Text3 ) :- !,
        get_ToEnter( Text1, Text2, Text3 ).

% --- clean all blanks and tabs between identifiers and symbols
%     also puts a blank at the end of an identifier
clean_blanks( [], [] ).
clean_blanks( [32|Text1], Text2 ) :- !,
        clean_blanks( Text1, Text2 ).
clean_blanks( [9|Text1], Text2 ) :- !,
        clean_blanks( Text1, Text2 ).
clean_blanks( [X|Text1], Text4 ) :-
        (  clean_char(   [X|Text1], Text1x, Text2 ),!
         ; clean_string( [X|Text1], Text1x, Text2 ),!
         ; clean_efloat( [X|Text1], Text1x, Text2 ),!     % NEW
         ; clean_id(     [X|Text1], Text1x, Text2 ),!
         ; clean_sym(    [X|Text1], Text1x, Text2 ) ),
        append( Text1x, Text3, Text4 ),
        clean_blanks( Text2, Text3 ).

clean_id( [C|Cs1], [C|Cs2], Cs3 ) :-
        ( C >= "0", C =< "9"
         ;C >= "A", C =< "Z"
         ;C >= "a", C =< "z"
         ;[C] == "_" ),!,
        clean_id2(Cs1,Cs2,Cs3).

clean_id2( [C|Cs1], [C|Cs2], Cs3 ) :-
        ( C >= "0", C =< "9"
         ;C >= "A", C =< "Z"
         ;C >= "a", C =< "z"
         ;[C] == "_" ),!,
        clean_id2(Cs1,Cs2,Cs3).
clean_id2( [32|Cs], [32], Cs ).
clean_id2( [9|Cs],  [32], Cs ).
clean_id2( [10|Cs], [32,10], Cs ).
clean_id2( [X|Cs], [32,X], Cs ) :-
        X \== 32, X \== 9, X \== 10.

clean_sym( [X|Text], [X], Text ).

clean_char( [39,C,39|Text], [39,C,39], Text ).         % 'a','b',...
clean_char( [39,C1,C2,39|Text], [39,C1,C2,39], Text ). % '\n','\t',...

clean_string( [34|Text1], [34|Text4], Text3 ) :-
        get_ToQuote( Text1, Text2, Text3 ),
        append(Text2,[34],Text4).

get_ToQuote( [34|Text], [], Text ).
get_ToQuote( [X|Text1], [X|Text2], Text3 ) :- !,
        get_ToQuote( Text1, Text2, Text3 ).

% Clean Float with Exponent (1.0e-1) because the "e" letter does not work well % NEW
clean_efloat( Text1, Float, Text6 ) :-                                         % NEW
        %-Sure float with exponent                                             % NEW
        clean_number( Text1, Number1, Text2 ),                                 % NEW
        Text2=[46|Text3],     % "."=46                                         % NEW
        clean_number( Text3, Number2, Text4 ),                                 % NEW
        ( Text4=[101|Text5] ; Text4=[69|Text5] ),                              % NEW
        clean_number( Text5, Number3, Text6 ),                                 % NEW
        %-Build correct string                                                 % NEW
        append( Number1, ".", S1 ), append( S1, Number2, S2 ),                 % NEW
        append( S2, "E ", S3 ), append( S3, Number3, Float ).                  % NEW

clean_number( [45|Cs1], [45|Cs2] , Cs3 ) :- !,                                 % NEW
        clean_number(Cs1,Cs2,Cs3).                                             % NEW
clean_number( [C|Cs1], [C|Cs2], Cs3 ) :- C >= "0", C =< "9",!,                 % NEW
        clean_number(Cs1,Cs2,Cs3).                                             % NEW
clean_number( Cs, [32], Cs ).                                                  % NEW

% controls no forward node could be returned
treat( [], [] ).
treat( [X|Xs], [Y|Ys] ) :-
        treat2(X,Y),treat(Xs,Ys).

treat2( rule(L,R), rule(L,Rx) ) :- !,
        treatE(R,Rx).
treat2( X, X ).

treatE( forward(F,_,Exprs), forward(F,N,ExprsX) ) :- 
        function(F,N),!,
        treatEL(Exprs,ExprsX).
treatE( forward(F,_,_), _ ) :- 
        \+(function(F,_)),!,
        format('Parsing error, function ~a doesn\'t exist\n',[F]),
        fail.
treatE( c(C,N,Args), c(C,N,ArgsX) ) :- !,treatEL(Args,ArgsX).
treatE( f(F,N,Args), f(F,N,ArgsX) ) :- !,treatEL(Args,ArgsX).
treatE( multiple(Cts), multiple(CtsX) ) :- !,treatEL(Cts,CtsX).
treatE( X, X ).

treatEL( [], [] ).
treatEL( [X|Xs], [Y|Ys] ) :-
        treatE(X,Y),
        treatEL(Xs,Ys).

% ----------------------------------------
% parser grammar
% ----------------------------------------

program( Program ) -->
        {cleanFuncConsTypes,prelude_parser},
        {write('Parsing...'),nl},
        block( Program ).

block( Block ) -->
        (  pragmaDeclarationL(Pragmas)
         ; \+(pragmaDeclarationL(_)),{Pragmas=[]} ),!,
        (  fixityDeclarationL
         ; \+(fixityDeclarationL) ),!,
        {append(Pragmas,BlockL,Block)},
        blockDeclarationL(BlockL).

% Fixity Declaration ---------------------------------------------------------
fixityDeclarationL -->
        fixityDeclaration,!,
        (
           fixityDeclarationL,!
         ; \+(fixityDeclarationL)
        ).

fixityDeclaration -->
        ( "infixc ", {Associ=c}
         ;"infixl ",{Associ=l}
         ;"infixr ",{Associ=r} ),        
        natural(Nat),{number_chars(Num,Nat)},
        infixOpL(Associ,Num),
        "\n".

infixOpL( Associ, Num ) -->
        infixOp(Associ,Num),
        (
          ",",infixOpL(Associ,Num)
         ; \+(",")
        ).

% dcgNaf(P,A,B):- \+ phrase(P,A,B).

infixOp(Associ,Num) -->
        ( "`",id(ID),"`"
         ;opSim(ID) ),
        {name(F,ID)},
        {(  \+(infix(F,_,_)),!,
            ( assert(infix(F,Num,Associ))
             ;retract(infix(F,Num,Associ)) )
          ; infix(F,_,_),!,
            format('Syntactic error, fixity declaration ~a already exists\n',[F]),
            fail
          )}.

natural(Nat) --> 
        digit(Digit),{Nat=[Digit|Nat1]},
        (  natural(Nat1),!
         ; " ",{Nat1=[]} ).
digit( C ) --> [C],{ C >= 48, C =< 57 }.

% Pragmas Delcaration --------------------------------------------------------
pragmaDeclarationL( [Pragma|Pragmas] ) -->
        pragmaDeclaration(Pragma),!,
        (
           pragmaDeclarationL(Pragmas),!
         ; \+(pragmaDeclarationL(_)),{Pragmas=[]}
        ).

pragmaDeclaration( pragma(Pragma) ) -->
        "pragma ",
        ( "flex ",{Pragma=flex}
         ;"rigid ",{Pragma=rigid}
         ;"optmatch ",{Pragma=optmatch} ),
        "\n".

% Block Declaration ---------------------------------------------------------
blockDeclarationL( Result ) -->
        blockDeclaration(Block),!,
        (
           blockDeclarationL(Rest),!,
           {append(Block,Rest,Result)}
         ; {Result = Block} 
        ).

blockDeclaration( [Data] ) --> dataDeclaration( Data ).
blockDeclaration( [DeclFunction] ) --> functionDeclaration( DeclFunction ).

% types ---------------------------------------------------------------------
dataDeclaration(data(Type,Constructors)) --> 
        "data ", typeDeclaration(Type,Constructors), "\n".

typeDeclaration(type(TypeName,N,TypeVars),Consts) --> 
        {cleanVars}, 
        typeConstrID(TypeName),
        {  \+(type(TypeName,_))
         ; type(TypeName,_), 
           format('Sorry, ~a type just exists',[TypeName]) },
        (  typeVarIDL(TypeVars), !, {length(TypeVars,N)}
         ; {N=0,TypeVars=[]} ),
        (  "=",!
         ; {write('Expected ='),nl,!} ),
        {assert(type(TypeName,N))},
        (  constrDeclarationL(Consts),!
         ; {retract(type(TypeName,N))} ).

typeConstrID( TypeName ) --> id(ID), {atom_chars(TypeName,ID)}.

typeVarIDL( [Var|Vars] ) --> 
        typeVarID( Var ), 
        (  typeVarIDL( Vars ),!
         ; {Vars=[]} ).

typeVarID( typevar(N) ) --> variableID(N).

constrDeclarationL([Type|Types]) -->
        constrDeclaration(Type), 
        (  "|", constrDeclarationL(Types) 
         ; \+("|"),{Types=[]} ).

constrDeclaration( const(ConstrID,N,TypeExprs) ) --> 
        dataConstrID( ConstrID ), 
        (  typeExprL(TypeExprs), !, {length(TypeExprs,N)}
         ; {N=0,TypeExprs=[]} ),
        {assert(constructor(ConstrID,N))}.

dataConstrID( TypeName ) --> id(ID), {atom_chars(TypeName,ID)},
        {\+(type(TypeName,_)),\+(function(TypeName,_)),
         \+(constructor(TypeName,_))}.

typeExpr( TypeExpr ) --> 
        basictypeExpr(Type1),
        ( "->", !,typeExpr(Type2), 
          {TypeExpr=curried(Type1,Type2)}
         ;{TypeExpr=Type1} ).

basictypeExpr( elem(type('List',1,[elem(type('Char',0,[]))])) ) --> 
        "String ",!.

basictypeExpr( TypeExpr ) --> 
        id(ID), {atom_chars(Type,ID), type(Type,N)},!,
        ( {N=0},!, % Type with 0-arity
          {TypeExpr=elem(type(Type,0,[]))}
         ;{N>0},!, % Type with N-arity
          typeExprL(Vars), {length(Vars,N)},
          {TypeExpr=elem(type(Type,N,Vars))}  ).
basictypeExpr( elem(type('Tuples',0,[])) ) --> "()".
basictypeExpr( Type ) --> 
        "(",!, 
        typeExpr( Type1 ),
        ( ",",!,   %--Tuple expression
          typeExprs(TypeExprs),")",
          {length([Type1|TypeExprs],N)},
          {Type=elem(type('Tuples',N,[Type1|TypeExprs]))}
         ;")",!,   %--Parenthesized type expression 
          {Type=elem(Type1)} ).
basictypeExpr( elem(type('List',1,[TypeExpr])) ) --> "[",!,typeExpr(TypeExpr),"]".
basictypeExpr( elem(typevar(N)) ) --> variableAnt(N),!.
basictypeExpr( elem(typevar(N)) ) --> {ini},variableID(N),!.

typeExprL( [TypeExpr|TypeExprs] ) -->
        basictypeExpr(TypeExpr),
        (  typeExprL(TypeExprs),!
         ; {TypeExprs=[]} ).

typeExprs( [TypeExpr|TypeExprs] ) -->
        typeExpr(TypeExpr),
        (  ",",typeExprs(TypeExprs),!
         ; {TypeExprs=[]} ).

% Functions ---------------------------------------------------------------------
functionDeclaration(Decl) -->
          evalAnn(Decl),!
        ; functionType(Decl),!
        ; equation(Decl),!.

% --- Function Evaluation Annotation
evalAnn(annotation(F,Type)) --> 
        (  functionName(F)
         ; "(",functionOp(F),")"
         ; "`",functionOp(F),"`" ), 
        "eval ",
        (  "flex ", {Type = flex},!
         ; "rigid ", {Type = rigid},!
         ; {write('Evaluation annotation can only be flex or rigid'),
            nl,fail} ),
        "\n".

% --- Function Type Declaration
functionType(function(F,N,TypeFunction)) --> 
        (  functionName(F)
         ; "(",functionOp(F),")"
         ; "`",functionOp(F),"`" ), 
        "::",!, 
        {assert(ini)},
        ( typeExpr(TypeFunction), {retract(ini)}
         ;{retract(ini)} ),
        {findArity(TypeFunction,0,N)},
        {( \+(function(F,N)),
           assert(function(F,N))
          ;function(F,N) )},
        "\n".

findArity( curried(_,E), N, N2 ) :-
        N1 is N + 1,
        findArity(E,N1,N2).
findArity( elem(_), N, N ).

% --- Function Declaration
equation( rule( f(F,N,P), Exp ) ) --> 
        {cleanVars}, 

        (
           "data ",!,{fail} % control no data decl get as function
         ; functionName(F), pattern(P), !,{length(P,N), Infix=no} 
         ; functionName(F), \+(functionOp(_)), {N=0, P=[], Infix=no}
         ; constrTerm(Term1),functionOp(F),constrTerm(Term2),!,
           { N=2, P=[Term1,Term2], Infix = si } 
        ), 

        ( {\+(builtin(F))}
         ;{builtin(F), 
           format("Parsing error, function ~a cannot be redefined\n",[F]),fail} ),

        {( \+(function(F,N)),
           ( assert(function(F,N)) ; retract(function(F,N)) )
          ;function(F,N) )},

        ( {Infix=si, \+(infix(F,_,_)), 
           write('Parsing error, infix declaration needed before '),
           format('function declaration ~a',[F]),nl,fail}
         ;{Infix=si, infix(F,_,_)}
         ;{Infix=no} ),
        !, % ensure no backtracking

        (  %Right Expression
           "=", expr(Exp)
         ; %Conditional Expression
           constExpr(Exps),
           {Exp=multiple(Exps)}
         ; %Error 
           getchars(Text),
           {format("Syntactic error, incorrect expression <~a> found\n",[Text]),
            retract(function(F,N)),fail} ),
        "\n".

functionName(F) --> id(C), { name(F,C), \+(constructor(F,_)) }.

functionOp(F) --> opSim(C), {name(F,C)}.
functionOp(F) --> "`", id(C), "`", {name(F,C)}.

pattern([C|Cs]) -->
        constrTerm(C),
        (  pattern(Cs),!
         ; {Cs=[]} ).

:- style_check(-discontiguous).

constrTerm( Expr ) --> "'",!,char(Expr),"'".
constrTerm( Expr ) --> string(Expr),!.
constrTerm( c(Num,0,[]) ) --> number(Num),!.

constrTerm( c('Tuple',0,[]) ) --> "()",!.
constrTerm( Expr ) --> 
        "(",constrTerm(Expr1),!,
        (":",!,
         constrTermCons( Exprs ),
         {Expr=c('Cons',2,[Expr1,Exprs])}
        ;",",!,
         constrTermS( Exprs ),
         {length([Expr1|Exprs],N)},
         {Expr=c('Tuple',N,[Expr1|Exprs])}
        ;{Expr=Expr1} ),
        ")".

constrTerm( c(F,0,[]) ) --> 
        constructorID(F,0),!.
constrTerm( c(F,N,Terms) ) --> 
        "(", constructorID(F,N),!, constrTermL( Terms ), ")",
        {length(Terms,N)}.

constrTerm( c('Nil',0,[]) ) --> "[]",!.
constrTerm( Expr ) --> 
        "[",
        constrTerm_list(Expr), 
        "]",!.

constrTerm_list( c('Cons',2,[Term,List]) ) --> 
        constrTerm(Term),
        (  ",",!,constrTerm_list(List)
         ; {List = c('Nil',0,[])} ).

constrTerm( _ ) --> 
        id(ID), {atom_chars(F,ID),var_seen(F,_)},
        {format("Syntactic error, no left linear, repeated variable ~a\n",[F]),
        fail}.
constrTerm( v(Var) ) --> variableID(Var).
constrTerm( v(Var) ) --> variableVoid(Var).
constrTerm( v(Var) ) --> variableAnt(Var),
        {write('Sorry, non left linear equation'),nl,fail}.

constrTermL( [Exp|Exps] ) --> 
        constrTerm( Exp ), 
        (  constrTermL( Exps ),!
         ; {Exps=[]} ).

constrTermS( [Exp|Exps] ) --> 
        constrTerm( Exp ), 
        (  ",",constrTermS( Exps ),!
         ; {Exps=[]} ).

constrTermCons( Expr ) --> 
        constrTerm( Exp ), 
        (  ":",constrTermCons( Exps ),!,
           {Expr=c('Cons',2,[Exp,Exps])}
         ; {Expr=Exp} ).

% Conditional rule (constrained expression)--------------------------
constExpr( [Exp|Exps] ) -->
         "|", expr(ExpCond), "=", expr( ExpR ),
         {Exp=f('\\=>',2,[ExpCond,ExpR])},!,
         ( constExpr(Exps),!
          ;{Exps=[]} ).

% Expressions ------------------------------------------------------
%
% expr is global, it makes calls to eexpr (Extended Expr),
% eexpr makes calls to bexpr (Basic Expr) and
% bexpr makes a call to expr with parenthesis


% --------------------
% --- General Expression
% --------------------

% --- define free variables
expr( Expr ) --> {\+(ini)}, % sure only expressions in rules
        "let ",!,  % inside this option
        variableIDL(Vars),
        ("free in ",
         expr(Expr),
         {del_vars(Vars)}         % in any case del_vars must be done
        ;{del_vars(Vars)} ),!.

del_vars([]).
del_vars([Var|Vars]) :-
     retract(var_seen(_,Var)),
     del_vars(Vars).

% --- conditional expression (syntactic sugar)
expr( Expr ) --> 
        "if ", !,expr(ExpBool),
        "then ", expr(ExpT),
        "else ", expr(ExpF),
        {Expr=f('IfThenElse',3,[ExpBool,ExpT,ExpF])}.

% --- a extended expression OR 
%     nested operators (also constructor list ':')
expr( Expr, S1, S3 ) :-   % no DCG form (more efficient)
     eexpr( Expr1, S1, S2 ),
     (  %--catch all terms and operators and then find associative order
        ( functionInfixID(_,_,_,S2,_) 
         ;S2=[58|_]  ),!,       % ':'=58
        exprInfixL( Exprs, S1, S3 ),!,
        associative( Exprs, Expr )
      ; %--simple extended expression
        Expr=Expr1, S3=S2
     ),!.
     
% Get associative for operator
functionInfixID( F, Prec, Asso ) --> 
        ("`";[26,96]),
        id(ID),
        ("`";[26,96]),!,
        {atom_chars(F,ID), infix(F,Prec,Asso)}.
functionInfixID( F, Prec, Asso ) --> opSim(ID),!,
        {atom_chars(F,ID), infix(F,Prec,Asso)}.

exprInfixL( Resul ) --> 
        eexpr( Expr1 ),
        (
           functionInfixID(F,Prec,Asso),!,
           {Resul=[Expr1,op(F,Prec,Asso)|ExprL]} 
        ; ":",!,
           {infix(':',Prec,Asso)},
           {Resul=[Expr1,op(':',Prec,Asso)|ExprL]} % Same precedence as ++
        ),
        (  %-More operators
           exprInfixL( ExprL ),!
         ; %-Only 2nd expr
           eexpr( Expr2 ),!,
           {ExprL=[Expr2]}
     
         ; %-Without 2nd expr (partial operator call)
          {ExprL=[]}
        ).

% find correct order calls to operators
% each operator has Asso (type of associative)
associative( ExprL, Expr ) :-
        getExpr( ExprL, Expr, _ ),!.

% get root operator (op, 1st, and 2nd arg)
getExpr( [Expr], Expr, inf ) :- !.  % inf is the greatest integer of SICStus
getExpr( [Expr1,op(F,Prec,_)], f(F,2,[Expr1]), Prec ) :- !.
getExpr( ExprL, ExprR, Prec ) :-
       %---first find correct ordered combination
        memberExpr( ExprL, ExprS1, op(F,Prec,Asso), ExprS2 ),
        %--find 1st arg from ExprS1
        getExpr( ExprS1, Expr1, Prec1 ), 
        ( Asso=c, Prec1> Prec
         ;Asso=l, Prec1>=Prec
         ;Asso=r, Prec1> Prec ),
        %--find 2nd arg from ExprS2
        ( %--two arguments for op
          ExprS2 = [_|_],
          getExpr( ExprS2, Expr2, Prec2 ), 
          ( Asso=c, Prec2> Prec
           ;Asso=l, Prec2> Prec
           ;Asso=r, Prec2>=Prec ),
          Args=[Expr1,Expr2]
         ;%--only one argument
          ExprS2 = [], 
          F \== ':',  %only allowed for functions
          Args=[Expr1]  ),
       %---build result expression
        !,
        ( %--Operator
          F \== ':',
          ( %-Defined Function
            function(F,2), 
            ExprR = f(F,2,Args)
           ;%-Non Defined Function
            \+(function(F,_)), 
            ExprR = forward(F,2,Args) )
         ;%--Constructor
          F = ':', 
          ExprR = c('Cons',2,Args) ).

%member for nested operators 
%base case
memberExpr( [ExprS1,op(F,Prec,Asso)|ExprS2], 
            [ExprS1], op(F,Prec,Asso), ExprS2 ).
%general case
memberExpr( [Expr1,Op1,Expr2,Op2|ExprS], [Expr1,Op1|ExprS1], Op, ExprS2 ) :- 
        memberExpr( [Expr2,Op2|ExprS], ExprS1, Op, ExprS2 ).

% --------------------
% --- Extended Expression (to control no infinite calls)
% --------------------

% --- a basic expression OR 
%     partial application
eexpr( Expr, S1, S3 ) :-   % no DCG form (more efficient)
     bexpr( Expr1, S1, S2 ),
     (  %--partial application
         ( 
            Expr1=v(_)      % a variable that will be instantiated
           ;Expr1=f(_,_,_)  % a expression that will become a function
        ),
         exprL(Terms,S2,S3),!,
         append([Expr1],Terms,Args), length(Args,N),
        Expr = f('\\@',N,Args)
      ; %--simple basic expression
        Expr=Expr1, S3=S2
     ),!.
     
% --- constructors N-arity (N>0)
eexpr( c(F,N,Terms) ) --> 
        constructorID(F,NF), {NF>0},!, exprL( Terms ), 
        {length(Terms,N),N=<NF}.

% --- functions N-arity (N>1)
eexpr( f(F,N,Terms) ) --> 
        functionID(F,N), {N>0},!, exprL( Terms ), 
        {length(Terms,N1), N1 =< N}.

% --- functions not seen N-arity (N>1)
eexpr( forward(F,N,Terms) ) --> {\+(ini)},   % function not passed yet
        id(ID), 
        {name(F,ID)}, 
        {\+(function(F,_)), \+(constructor(F,_)), \+(var_seen(F,_))},
        exprL(Terms), {length(Terms,N)}.

% List of basic expressions
exprL( [Exp|Exps] ) --> 
        bexpr( Exp ),!, 
        (  exprL( Exps )
         ; {Exps=[]} ).

% --- Special Case for sections (operator with only 2nd arg)
%     translated to "\x -> x op e"
eexpr( Expr ) -->
      functionInfixID(F,_,_), expr(Expr2),!,
      {inc_var(NVar)},
     {Expr = f('\\',3,[ c('\\Vars',1,[v(NVar)]), f(F,2,[v(NVar),Expr2]) ])}.

% --- Lambda Abstractions
%     Standard form "\ x,y,z -> expr"
%     Arguments to lambda function are added at list.
%     Number of expected arguments are increases by two to include
%          pattern expression and expression
eexpr( f('\\',N,[Vars,Expr]) ) -->
       % first know the current last variable 
        {lastvar(LVar1)},
        "\\",!,                 % now correct, we are in a lambda abstraction
       pattern(Pattern),     % get pattern 
       "->",               % check literal
       expr(Expr),          % get result expression
       !,
       % now we have to remove all new variables created in pattern
        {lastvar(LVar2),removeVars(LVar1,LVar2)},
       % build final expression
        { length(Pattern,Np), N is Np+2,  Vars=c('\\Vars',Np,Pattern) }.

% Ensure same state before lambda process
removeVars(V1,V1).
removeVars(V1,V2) :- V2 > V1,
     ( retract(var_seen(_,V2))
      ;\+(var_seen(_,V2))      ),
     V is V2 - 1,
     removeVars(V1,V).

% --------------------
% --- Basic Expression
% --------------------

% - vars
bexpr( v(Var) ) --> variableAnt(Var).       % seen variable
bexpr( v(Var) ) --> variableNew(Var).       % new free variable (+underscore)
bexpr( v(Var) ) --> {ini}, variableID(Var). % new free variable (-underscore)
bexpr( v(Var) ) --> variableVoid(Var).      % void free variable

% - character
bexpr( Exp ) --> "'",char(Exp),"'",!.

% - string
bexpr( ExpStr ) --> string(ExpStr),!.

% - number (integer)                                  % OLD
% - number (integer or float)                         % NEW
bexpr( c(Num,0,[]) ) --> number(Num),!.

% - lists
bexpr( c('Nil',0,[]) ) --> 
        "[]",!.
bexpr( Expr ) --> 
        "[",!, expr_list(Expr), "]".

expr_list( c('Cons',2,[Term,List]) ) --> 
        expr(Term),
        ( ",",!, expr_list(List)
         ;{List = c('Nil',0,[])} ),!.

% - empty tuple
bexpr( c('Tuple',0,[]) ) --> "()",!.

% - operator without args
% should be together with TUPLES and parenthesized exp
% to join cases in the same functor
bexpr( f(F,2,[]) ) -->
        "(",functionInfixID(F,_,_),")",!,
        {function(F,2)}.

bexpr( forward(F,2,[]) ) -->
        "(",functionInfixID(F,_,_),")",!,
        {\+(function(F,2))}.

% - tuples OR expression into parenthesis
bexpr( Expr ) --> 
     "(",
     expr( Expr1 ),
     (  %--tuples (first exp parsed)
        ",",!,                    
        expr_tuples( Exprs ),
        {length([Expr1|Exprs],N)},
        {Expr=c('Tuple',N,[Expr1|Exprs])}
      ; %--expr between parenthesis
        {Expr=Expr1}               
     ),
     ")",!.

expr_tuples( [Exp|Exps] ) -->
     expr( Exp ),
     ( ",",!,expr_tuples(Exps)
      ;{Exps=[]} ).

% - constructor 0-arity
bexpr( c(F,0,[]) ) --> constructorID(F,0),!.

% - function 0-arity
bexpr( f(F,0,[]) ) --> functionID(F,0),!.

% --- function call without args
bexpr( f(F,N,[]) ) -->
        functionID(F,N),!,
        \+(bexpr(_)).

bexpr( forward(F,0,[]) ) --> {\+(ini)},             % function still not seen
        id(ID), 
        {name(F,ID)}, 
        {\+(function(F,_)), \+(constructor(F,_)), \+(var_seen(F,_))},
        \+(bexpr(_)).

%---- Special ----------
exprIni( Termino ) --> 
        {retractall(ini),assert(ini)}, 
        expr(Termino),
        {retractall(ini)}.

% IDENTIFIERS ----------------------------------------------------
% new variable (without underscore)
variableID( N ) --> id(ID), 
        {atom_chars(F,ID), \+(var_seen(F,_)), 
         var_int(F,N),
         ( \+(constructor(F,_)),
           \+(function(F,_)),
           \+(type(F,_)),
           \+(builtin(F))
          ;del_var(N),fail )}.

% new free variable with underscore
variableNew( N ) --> 
        "_",id(IDx),{ID=[95|IDx],
        atom_chars(F,ID), \+(var_seen(F,_)), 
        var_int(F,N),
         ( \+(constructor(F,_)),
           \+(function(F,_)),
           \+(type(F,_)),
           \+(builtin(F))
         ;del_var(N),fail )}.

% previos seen variable
variableAnt( N ) --> 
        ( "_",id(IDx),{ID=[95|IDx]}
         ;id(ID) ),
        { atom_chars(F,ID), var_seen(F,N) }.

% void variable (only underscore)
variableVoid( N ) --> 
        "_ ",
        {inc_var(N),
         ( assert(var_seen('_',N)) 
          ;del_var(N) )}.

% list of variables
variableIDL( [N|Vars] ) -->
        variableID(N),
        (  ",",!,variableIDL(Vars)
         ; {Vars=[]} ).

functionID( F, N ) --> id(ID), {atom_chars(F,ID), function(F,N)}.
constructorID( C, N ) --> id(ID), {atom_chars(C,ID), constructor(C,N)}.

% Float Number                                                          % NEW
number( Num ) -->                                                       % NEW
        numberS(SNum1),".",!,numberS(SNum2),                            % NEW
        ( ("e "; "E "),!,                                               % NEW
          ("-",{Exp=[69,45|SExp]} ; {Exp=[69|SExp]} ),                  % NEW
          numberS(SExp)                                                 % NEW
         ;{Exp=""} ),                                                   % NEW
        {append(SNum1,".",S1),append(S1,SNum2,S2),append(S2,Exp,SNum)}, % NEW
        {number_chars(Num,SNum)}.                                       % NEW

% Integer Number
number( Num ) --> numberS(SNum),{number_chars(Num,SNum)}.

% String of 0 .. 9
numberS( [C|Cs] ) --> [C], 
        { C >= "0", C =< "9" }, 
        numberS( Cs ).
numberS( [] ) --> " ".

id([C|Cs]) --> [C],
        { C >= "A", C =< "Z"
         ;C >= "a", C =< "z" },!,
        restoid(Cs),!,
        ( {[C|Cs]=="data",!,fail} %this is not a valid id
         ;{[C|Cs]=="eval",!,fail} %this is not a valid id
         ;{[C|Cs]=="flex",!,fail} %this is not a valid id
         ;{[C|Cs]=="rigid",!,fail} %this is not a valid id
         ;{[C|Cs]=="let", !,fail} %this is not a valid id
         ;{[C|Cs]=="in",  !,fail} %this is not a valid id
         ;{[C|Cs]=="if",  !,fail} %this is not a valid id
         ;{[C|Cs]=="then",!,fail} %this is not a valid id
         ;{[C|Cs]=="else",!,fail} %this is not a valid id
         ;{true} ).

restoid([C|Cs]) --> [C],
        { C >= "0", C =< "9"
         ;C >= "A", C =< "Z"
         ;C >= "a", C =< "z"
         ;[C] == "_" },!,
        restoid(Cs).
restoid([]) --> " ".

opSim( Cs ) --> opSim2(Cs1),
        {( Cs1 = "=>",!, Cs = ">="
          ;Cs1 = "<=",!, Cs = "=<"
          ;Cs1 = "=",!,fail %this is not a valid op
          ;Cs1 = "|",!,fail %this is not a valid op
          ;Cs = Cs1 )}.

opSim2( "=:=" ) --> "=:=",!. % Special case not included using sym
opSim2([C|Cs]) --> sym([C]), opSim2(Cs).
opSim2([C]) --> sym([C]).

sym("~") --> "~".
sym("!") --> "!".
sym("@") --> "@".
sym("#") --> "#".
sym("$") --> "$".
sym("%") --> "%".
sym("^") --> "^".
sym("&") --> "&".
sym("*") --> "*".
sym("+") --> "+".
sym("-") --> "-".
sym("=") --> "=".
sym("<") --> "<".
sym(">") --> ">".
sym("?") --> "?".
sym(".") --> ".".
sym("/") --> "/".
sym("|") --> "|".

string( Expr ) --> [34],stringS(Expr).
stringS( c('Nil',0,[]) ) --> [34],!.
stringS( c('Cons',2,[Ch,Expr]) ) --> % List of chars
        char(Ch),stringS(Expr).

char( c([7],0,[]) )   --> "\\a".
char( c([8],0,[]) )   --> "\\b".
char( c([9],0,[]) )   --> "\\t".
char( c([10],0,[]) )  --> "\\n".
char( c([11],0,[]) )  --> "\\v".
char( c([12],0,[]) )  --> "\\f".
char( c([13],0,[]) )  --> "\\r".
char( c([27],0,[]) )  --> "\\e".
char( c([127],0,[]) ) --> "\\d".
char( c([C],0,[]) )   --> [C].

/* ------------------------------------
                General
   ------------------------------------ */
getchars(Text,S1,S2) :-
        get_ToEnter( S1, S, S2 ),
        name(Text,S).

cleanVars :-
        retractall(lastvar(_)),
        retractall(var_seen(_,_)),
        assert(lastvar(0)).

var_int(C,N) :-
        \+(var_seen(C,_)),
        inc_var(N),
        assert(var_seen(C,N)).

del_var(N) :-
        retract(var_seen(_,N)),
        ( lastvar(N),!,
          retract(lastvar(N)),findLastVar(N,N1),assert(lastvar(N1))
         ;\+(lastvar(N)) ).

findLastVar(0,0).
findLastVar(N,N) :- N > 0, 
        var_seen(_,N).
findLastVar(N,N2) :- N > 0, 
        \+(var_seen(_,N)),
        N1 is N - 1,
        findLastVar(N1,N2).

inc_var(N) :-
        retract(lastvar(UltVar)),
        N is UltVar + 1,
        assert(lastvar(N)).

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
                                           %elem(typevar(1))))))).       % OLD
                                           elem(typevar(1))))))),        % NEW

   % Arithmetic Functions                                                % NEW
   assert(function('+',2,curried(elem(type('Int',0,[])),                 % NEW
                                 curried(elem(type('Int',0,[])),         % NEW
                                         elem(type('Int',0,[])))) )),    % NEW
   assert(function('+',2,curried(elem(type('Float',0,[])),               % NEW
                                 curried(elem(type('Float',0,[])),       % NEW
                                         elem(type('Float',0,[])))) )),  % NEW
   assert(function('-',2,curried(elem(type('Int',0,[])),                 % NEW
                                 curried(elem(type('Int',0,[])),         % NEW
                                         elem(type('Int',0,[])))) )),    % NEW
   assert(function('-',2,curried(elem(type('Float',0,[])),               % NEW
                                 curried(elem(type('Float',0,[])),       % NEW
                                         elem(type('Float',0,[])))) )),  % NEW
   assert(function('*',2,curried(elem(type('Int',0,[])),                 % NEW
                                 curried(elem(type('Int',0,[])),         % NEW
                                         elem(type('Int',0,[])))) )),    % NEW
   assert(function('*',2,curried(elem(type('Float',0,[])),               % NEW
                                 curried(elem(type('Float',0,[])),       % NEW
                                         elem(type('Float',0,[])))) )),  % NEW
   assert(function('div',2,curried(elem(type('Int',0,[])),               % NEW
                                   curried(elem(type('Int',0,[])),       % NEW
                                           elem(type('Int',0,[])))) )),  % NEW
   assert(function('mod',2,curried(elem(type('Int',0,[])),               % NEW
                                   curried(elem(type('Int',0,[])),       % NEW
                                           elem(type('Int',0,[])))) )),  % NEW
   assert(function('/',2,curried(elem(type('Float',0,[])),               % NEW
                                 curried(elem(type('Float',0,[])),       % NEW
                                         elem(type('Float',0,[])))) )),  % NEW

   % Relational Functions                                                % NEW
   assert(function('<',2,curried(elem(type('Int',0,[])),                 % NEW
                                 curried(elem(type('Int',0,[])),         % NEW
                                         elem(type('Bool',0,[])))) )),   % NEW
   assert(function('<',2,curried(elem(type('Float',0,[])),               % NEW
                                 curried(elem(type('Float',0,[])),       % NEW
                                         elem(type('Bool',0,[])))) )),   % NEW
   assert(function('>',2,curried(elem(type('Int',0,[])),                 % NEW
                                 curried(elem(type('Int',0,[])),         % NEW
                                         elem(type('Bool',0,[])))) )),   % NEW
   assert(function('>',2,curried(elem(type('Float',0,[])),               % NEW
                                 curried(elem(type('Float',0,[])),       % NEW
                                         elem(type('Bool',0,[])))) )),   % NEW
   assert(function('=<',2,curried(elem(type('Int',0,[])),                % NEW
                                  curried(elem(type('Int',0,[])),        % NEW
                                          elem(type('Bool',0,[])))) )),  % NEW
   assert(function('=<',2,curried(elem(type('Float',0,[])),              % NEW
                                 curried(elem(type('Float',0,[])),       % NEW
                                         elem(type('Bool',0,[])))) )),   % NEW
   assert(function('>=',2,curried(elem(type('Int',0,[])),                % NEW
                                  curried(elem(type('Int',0,[])),        % NEW
                                          elem(type('Bool',0,[])))) )),  % NEW
   assert(function('>=',2,curried(elem(type('Float',0,[])),              % NEW
                                 curried(elem(type('Float',0,[])),       % NEW
                                         elem(type('Bool',0,[])))) )).   % NEW

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
        ;write('Type Error processing rule '),write2(L),nl,fail),
        append(Rules,Ys,Zs),
        !,checkType2(Xs,Ys).

% -- Data Types Control -------------------------------------------
checkPragma( Pragma ) :-
        ( (Pragma=flex; Pragma=rigid),
          ( annotationAll(Pragma),
            format("Syntactic error, pragma ~a already included\n",[Pragma]),fail
           ;\+(annotationAll(Pragma)),
            assert(annotationAll(Pragma)) )
         ;Pragma=optmatch,
          ( strategy(optmatch),
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

checkExpr( c(Num,0,[]), elem(type('Float',0,[])), Susts, Susts ) :-    % NEW
        float(Num),!.                                                  % NEW

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

checkExpr( c('Tuple',0,[]), TypeResul, Susts, Susts ) :- !,
        TypeResul = elem(type('Tuples',0,[])).
checkExpr( c('Tuple',N,Exprs), TypeResul, Susts1, Susts2 ) :- N>0,!,
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
        checkExpr(ExprF,TypeExprF,Susts1,Susts2),
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
applyType( elem(Type1), curried(Type2,Type3), Type3, Susts1 ) :- 
        unifyType(elem(Type1),Type2,Susts1).

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
        unifyType(X1,X2,Susts), append(Susts1,Susts,Susts2),
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
applySust(elem(Type1),M,Type,elem(Type2)) :-
        applySust(Type1,M,Type,Type2).
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

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ------------------------------------------------------------------------
                     Generate Definitional Trees
  ------------------------------------------------------------------------
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

:- dynamic dt/3.

%strategy(leftright).
%strategy(optmatch).

%annotationAll(flex).
%annotationAll(rigid).

% deftrees for primitive functions according to Curry report
basicDefTrees :-
   assert( dt('IfThenElse', 3,
           branch( f('IfThenElse',3,[ v(1), v(2), v(3) ]),[1],rigid,[
                     rule( f('IfThenElse',3,[ c('True',0,[]), v(2), v(3) ]), 
                           v(2) ),
                     rule( f('IfThenElse',3,[ c('False',0,[]), v(2), v(3) ]), 
                           v(3) ) 
             ] ) ) ).

:- basicDefTrees.

% -- general Prolog predicate
deftrees(ListRules) :-
        retractall( dt(_,_,_) ), 
        prelude_deftree,
        genAnnotation(ListRules),
        partition( ListRules, OrderedRules ),
        write('Generating definitional trees...'),
        gt_process( OrderedRules ),
        list_deftrees.

% -- generate annotations for all functions
genAnnotation( [] ).
genAnnotation( [rule(f(F,_,_),_)|Rs] ) :-
        annotation(F,_),
        genAnnotation(Rs).
genAnnotation( [rule(f(F,_,_),_)|Rs] ) :-
        \+(annotation(F,_)),
        ( annotationAll(flex),Mode=flex
         ;annotationAll(rigid),Mode=rigid ),
        assert(annotation(F,Mode)),
        genAnnotation(Rs).
genAnnotation( [rule(f(F,_,_),_)|Rs] ) :-
        \+(annotation(F,_)), \+(annotationAll(flex)), \+(annotationAll(rigid)),
        function(F,_,TypeFunc), 
        extractType(TypeFunc,TypeResul),
        (  TypeResul = type('Constraint',0,[]), Mode = flex
         ; Mode = rigid ),
        assert(annotation(F,Mode)),
        genAnnotation(Rs).

% get result type
extractType(elem(Type),Type) :- !.
extractType(curried(_,Type),TypeResul) :- extractType(Type,TypeResul).

% -- Partition rules by function symbol
partition( [], [] ).
partition( [rule(f(F,N,Terms),R)|Rules1], [SameRules|Rules3] ) :-
        same( F, N, Rules1, SameRules1, Rules2 ),
        SameRules = [ rule(f(F,N,Terms),R) | SameRules1 ],
        partition( Rules2, Rules3 ).

same( _, _, [], [], [] ).
same( F, N, 
        [rule(f(F,N,Terms),R)|Rules1], 
        [rule(f(F,N,Terms),R)|MRules2], Rules2 
     ) :- !,
        same( F, N, Rules1, MRules2, Rules2 ).
same( F, N, [rule(f(F1,N1,Terms),R)|Rules1], 
        MRules2, [rule(f(F1,N1,Terms),R)|Rules2] 
     ) :- 
        same( F, N, Rules1, MRules2, Rules2 ).

% -- Auxiliar for gt
gt_process( [] ).
gt_process( [SameRules|Rules] ) :-
        SameRules = [ rule( f(F,N,_), _ ) |_],
        newvars( N, 0, Vars ),
        strategy( Strategy ),
        annotation(F,Mode),
        (  gt( f(F,N,Vars), Mode, Strategy, SameRules, DefTree ),!
         ; write('Semantic error, '),
           format('definitional tree can not be built for function ~a\n',[F]),
           fail ),
        (  \+(dt( F, N, _ )), assert( dt( F, N, DefTree ) ) 
         ; dt(F,N,_), 
           format('Semantic error, previous definitional tree for ~a\n',[F]),
           fail ),
        !,gt_process( Rules ).

% Show all deftrees generated with function names of current loaded file.
list_deftrees :-
        prelude(_,_,[DefTrees|_]), collectFuncs(DefTrees,[],Funcs),
        findall(F,dt(F,_,_),FuncsTrees),
        list_deftrees1(FuncsTrees,Funcs).

list_deftrees1([],_) :- nl.
list_deftrees1([F|Fs],Funcs) :-
        ( \+(member(F,Funcs)),
          format("~a ",[F])
         ;member(F,Funcs) ),
        list_deftrees1(Fs,Funcs).

collectFuncs([],Funcs,Funcs).
collectFuncs([dt(F,_,_)|Xs],Funcs1,Funcs2) :- 
        collectFuncs(Xs,[F|Funcs1],Funcs2).

% -- Get demanded occurrencies --------------------------------------------
dp( f(_,_,Terms), ListRules, Positions ) :- 
        vars( Terms, [], 1, [], Vars ),
        dp( ListRules, Vars, ListRules, Positions ).

vars( [], _, _, Vars, Vars ).
vars( [v(_)|Terms], Ox, N, Vars1, Vars3 ) :- 
        N1 is N + 1,
        append( Ox, [N], Ox1 ),
        append( Vars1, [Ox1], Vars2 ),
        vars( Terms, Ox, N1, Vars2, Vars3 ).
vars( [c(_,_,Args)|Terms], Ox, N, Vars1, Vars3 ) :-
        N1 is N + 1,
        append( Ox, [N], Ox1 ),
        vars( Args, Ox1, 1, Vars1, Vars2 ),
        vars( Terms, Ox, N1, Vars2, Vars3 ).

dp( _, [], _, [] ). % -- no more 
dp( [], [_|Ox], ListRules, Positions ) :- % -- no more rules
        dp( ListRules, Ox, ListRules, Positions ).
dp( [rule(L,_)|_], [O|Ox], ListRules, [O|Positions] ) :- %--found constructor
        pos( L, O, c(_,_,_) ),
        dp( ListRules, Ox, ListRules, Positions ).
dp( [rule(L,_)|Rules], [O|Ox], ListRules, Positions ) :- %--found variable
        pos( L, O, v(_) ),
        dp( Rules, [O|Ox], ListRules, Positions ).

% -- Main predicate for Definitional Tree ------------------------------

% ---- branch node ( all constructors )
gt( f(F,N,Terms), M, optmatch, Rules, Mode ) :- 
        dp( f(F,N,Terms), Rules, [P|Positions] ),
        ( find_opt( [P|Positions], Rules, O ),!
         ;O=P ),
        groups( Rules, O, Grouped ),
        Mode = branch( f(F,N,Terms), O, M, SubTrees ),
        subtrees( Grouped, f(F,N,Terms), O, M, optmatch, SubTrees ).

find_opt( [V|_], Rules, V ) :-
        split( Rules, V, Rules, [] ).
find_opt( [V|Vs], Rules, O ) :-
        \+(split( Rules, V, Rules, [] )),
        find_opt( Vs, Rules, O ).        

% ----- branch node ( all constructors )
gt( f(F,N,Terms), M, leftright, Rules, Mode ) :- 
        dp( f(F,N,Terms), Rules, [O|_] ),
        split( Rules, O, Rules, [] ),
        groups( Rules, O, Grouped ),
        Mode = branch( f(F,N,Terms), O, M, SubTrees ),
        subtrees( Grouped, f(F,N,Terms), O, M, leftright, SubTrees ).

subtrees( [], _, _, _, _, [] ).
subtrees( [R|Rs], Term, O, M, Strategy, [SubTree|SubTrees] ) :-
        R = [rule( L, _ )|_], pos( L, O, c(F,N,_) ),
        maxvar( Term, Max ),
        newvars( N, Max, Vars ),
        part( Term, O, c(F,N,Vars), TermS ),
        gt( TermS, M, Strategy, R, SubTree ),
        subtrees( Rs, Term, O, M, Strategy, SubTrees ).

newvars( 0, _, [] ).
newvars( N, Inc, Vars ) :- N>0,
        N1 is N - 1,
        newvars( N1, Inc, Vars1 ),
        M is Inc + N,
        append( Vars1, [v(M)], Vars ).

% -------- or ( constructors and variables )
gt( f(F,N,Terms), M, Strategy, Rules, Mode ) :- 
        dp( f(F,N,Terms), Rules, [O|_] ),
        split( Rules, O, CRules, VRules ), CRules \== [], VRules \== [],
        Mode = tor( CMode, VMode ),
        gt( f(F,N,Terms), M, Strategy, CRules, CMode ),
        gt( f(F,N,Terms), M, Strategy, VRules, VMode ).

% -------- rule node
gt( f(F,N,Terms), _, _, Rules, Mode ) :- !,        
        dp( f(F,N,Terms), Rules, [] ),
        join_rules( Rules, Mode ).                % for orthogonal systems

join_rules( [rule(L,R)], rule(L,R) ).
join_rules( [rule(L,R),Rule2|Rules], tor( rule(L,R), Mode ) ) :-
        join_rules( [Rule2|Rules], Mode ).

% -- Split rules between constructors and variables -----------------
split( Rules, Ox, RCons, RVars ) :- split( Rules, Ox, [], RCons, [], RVars ).

split( [], _, RCons, RCons, RVars, RVars ).
split( [rule(L,R)|Rules], Ox, RCons1, RCons3, RVars1, RVars2 ) :-
        pos( L, Ox, c(_,_,_) ),
        append( RCons1, [rule(L,R)], RCons2 ),
        split( Rules, Ox, RCons2, RCons3, RVars1, RVars2 ).
split( [rule(L,R)|Rules], Ox, RCons1, RCons2, RVars1, RVars3 ) :-
        pos( L, Ox, v(_) ),
        append( RVars1, [rule(L,R)], RVars2 ),
        split( Rules, Ox, RCons1, RCons2, RVars2, RVars3 ).

% -- Group rules by same constructor -------------------------------
groups( [], _, [] ).
groups( [rule(L,R)|Rules1], Ox, [SameRules|Rules3] ) :-
        pos( L, Ox, c(F,N,_) ),
        extract( Rules1, Ox, F, N, [], SameRules1, Rules2 ), 
        SameRules = [rule(L,R)|SameRules1],
        groups( Rules2, Ox, Rules3 ).

extract( [], _, _, _, SameRules, SameRules, [] ).
extract( [rule(L,R)|Rules1], Ox, F, N, SameRules1, SameRules3, Rules2 ) :-
        pos( L, Ox, c(F,N,_) ),                            % -- included
        append( SameRules1, [rule(L,R)], SameRules2 ),
        extract( Rules1, Ox, F, N, SameRules2, SameRules3, Rules2 ).
extract( [rule(L,R)|Rules1], Ox, F, N, SameRules1, SameRules2, 
         [rule(L,R)|Rules2] ) :-
        pos( L, Ox, c(F1,N1,_) ), (F1 \== F ; N1 =\= N ),  % -- non included
        extract( Rules1, Ox, F, N, SameRules1, SameRules2, Rules2 ).

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ----------------------------------------------------------------------
          Curry's Kernel - Needed narrowing and Residuation.    
  ----------------------------------------------------------------------
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

:- dynamic debugmode/0, solution/0, time/1, maxVar/1.

% susts ----------------------------------------------------------
susts( c(F,N,T1), c(F,N,T2), Susts ) :- susts_terms( T1, T2, [], Susts ).
susts( f(F,N,T1), f(F,N,T2), Susts ) :- susts_terms( T1, T2, [], Susts ).
susts( v(N),      Term,      [s(N,Term)] ). % this isn't unification, 
                                            % is pattern matching

susts_terms( [], [], S, S ).
susts_terms( [Term1|Tx1], [Term2|Tx2], S1, S3 ) :-
        susts( Term1, Term2, Susts ),
        append( S1, Susts, S2),
        susts_terms( Tx1, Tx2, S2, S3 ).

% apply substitution-------------------------------------------------
% --- Terms
apply( [], Term, Term ).
apply( [s(N,TermS)|Susts], Term, TermR ) :-
        apply( Term, N, TermS, TermX ),
        apply( Susts, TermX, TermR ).

apply( c(F,N,Terms1), I, TermS, c(F,N,Terms2) ) :- 
        applyL( Terms1, I, TermS, Terms2 ).
apply( f(F,N,Terms1), I, TermS, f(F,N,Terms2) ) :- 
        applyL( Terms1, I, TermS, Terms2 ).
apply( v(N), N, TermS, TermS ).
apply( v(N), I, _, v(N) ) :- N =\= I.

applyL( [], _, _, [] ).
applyL( [Term1|Ts1], N, TermS, [Term2|Ts2] ) :-
        apply( Term1, N, TermS, Term2 ),
        applyL( Ts1, N, TermS, Ts2 ).

% --- Substitutions
applys( [], Susts, Susts ).
applys( [s(N,TermS)|LSust], Susts, SustsR ) :-
        applys( Susts, N, TermS, SustsX ),
        applys( LSust, SustsX, SustsR ).

applys( [], _, _, [] ).
applys( [s(N,Term1)|Susts1], I, TermS, [s(N,Term2)|Susts2] ) :-
        apply( Term1, I, TermS, Term2 ),
        applys( Susts1, I, TermS, Susts2 ).

% pos ---------------------------------------------------------
% Result expression in occurrence of expression
pos( Expression, [], Expression ).
pos( c(_,_,T1), [O|Ox], Term ) :- posL( T1, [O|Ox], Term ).
pos( f(_,_,T1), [O|Ox], Term ) :- posL( T1, [O|Ox], Term ).

posL( [Term|_], [1|Ox], TermR ) :- 
        pos( Term, Ox, TermR ).
posL( [_|Terms], [N|Ox], TermR ) :- N > 1, 
        N1 is N - 1, 
        posL( Terms, [N1|Ox], TermR ).

% part ---------------------------------------------------------
% Term in occurrence substituted by other
part( _, [], Exp, Exp ).
part( c(F,N,T1), [O|Ox], Term, c(F,N,T2) ) :- 
        partL( T1, [O|Ox], Term, T2 ).
part( f(F,N,T1), [O|Ox], Term, f(F,N,T2) ) :- 
        partL( T1, [O|Ox], Term, T2 ).

partL( [Term1|Terms], [1|Ox], Term, [Term2|Terms] ) :- 
        part( Term1, Ox, Term, Term2 ).
partL( [Term1|Terms1], [N|Ox], Term, [Term1|Terms2] ) :- N > 1, 
        N1 is N - 1, 
        partL( Terms1, [N1|Ox], Term, Terms2 ).

% controls nested constraints
part_simple( [_|Terms1], 1, Terms2, Terms3 ) :- 
        append( Terms2, Terms1, Terms3 ).
part_simple( [Term|Terms1], N, Terms2, [Term|Terms3] ) :- N > 1, 
        N1 is N - 1,
        part_simple( Terms1, N1, Terms2, Terms3 ).

% replace --------------------------------------------------------
replace( _, _, [], [] ).
replace( Expression, Occurrence, 
         [er(Susts,Exp)|States], [er(Susts,ExpR)|StatesR] ) :-
        apply( Susts, Expression, ExpressionS ),
        part( ExpressionS, Occurrence, Exp, ExpR ),
        replace( Expression, Occurrence, States, StatesR ).

% maxvar ---------------------------------------------------------
maxvar( c(_,_,T1), Max ) :- maxvarL( T1, 0, Max ).
maxvar( f(_,_,T1), Max ) :- maxvarL( T1, 0, Max ).
maxvar( v(N), N ).

maxvarL( [], Max, Max ).
maxvarL( [Term|Terms], Max1, Max3 ) :-
        maxvar( Term, Max2 ),
        ( Max2 >= Max1, maxvarL( Terms, Max2, Max3 )
         ;Max2 <  Max1, maxvarL( Terms, Max1, Max3 ) ).

% rename ---------------------------------------------------------
rename( rule( TermL, TermR ), Inc, rule( TermLx, TermRx ) ) :-
        rename( TermL, Inc, TermLx),
        rename( TermR, Inc, TermRx).
rename( tor( Tree1, Tree2 ), Inc, tor( Tree1x, Tree2x ) ) :-
        rename( Tree1, Inc, Tree1x ),
        rename( Tree2, Inc, Tree2x ).
rename(branch( Pattern1, Ox, Type, Trees1 ), Inc, 
       branch( Pattern2, Ox, Type, Trees2 ) ) :-
        rename( Pattern1, Inc, Pattern2 ),
        renameL( Trees1, Inc, Trees2 ).

rename( c(F,N,Terms1), Inc, c(F,N,Terms2) ) :-
        renameL( Terms1, Inc, Terms2 ).
rename( f(F,N,Terms1), Inc, f(F,N,Terms2) ) :-
        renameL( Terms1, Inc, Terms2 ).
rename( v(N1), Inc, v(N2) ) :- N2 is N1 + Inc.

renameL( [], _, [] ).
renameL( [Term1|Terms1], Inc, [Term2|Terms2] ) :-
        rename( Term1, Inc, Term2 ),
        renameL( Terms1, Inc, Terms2 ).

% EVAL -------------------------------------------------------------------
% -- Special: conditional function
eval( f('\\=>',2,[Constraint,Exp]), States ) :-
   (  %--Evaluation of Constraint
      eval( Constraint, StatesC ),!,
      replace( f('\\=>',2,[Constraint,Exp]), [1], StatesC, States )
    ; %--Constraint cannot be evaluable
      Constraint=f('success',0,[]),
      States=[ er([],Exp) ]              ).

% -- Special: partial function application
eval( f('\\@',NArgs,[ Expr| TermsAdd ]), States ) :- !,
      ( %-Evaluable Expression
        eval( Expr, StatesE ),!,
        replace( f('\\@',NArgs,[ Expr| TermsAdd ]), [1], StatesE, States )
      ; %-Function to add arguments
        Expr = f(F,N,Terms),
        append( Terms, TermsAdd, TermsR ),
        eval( f(F,N,TermsR), States )  ).

% -- Special: Strict Equality
eval( f('==',2,[ Term1, Term2 ]), States ) :- !,
        (  
           % Case Constructors
           (  Term1 = c(F,N,Args1), Term2 = c(F,N,Args2), N > 0,
              eval_equality( Args1, Args2, Term3 ), 
              States = [ er( [], Term3 ) ]
            ; Term1 = c(F,0,[]), Term2 = c(F,0,[]),
              States = [ er( [], c('True',0,[]) ) ]
            ; Term1 = c(F1,N1,_), Term2 = c(F2,N2,_), (F1\==F2;N1=\=N2),
              States = [ er( [], c('False',0,[]) ) ] )

         ; % Case Function
           ( Term1 = f(F,N,Terms), Ox = [1]
            ;Term2 = f(F,N,Terms), Ox = [2] ), !,
           eval( f(F,N,Terms), StatesF ),
           replace( f('==',2,[Term1,Term2]), Ox, StatesF, States )
        ).

eval_equality( [X], [Y], f('==',2,[ X, Y ]) ).
eval_equality( [X1,X2|Xs], [Y1,Y2|Ys], 
               f('&&',2,[ f('==',2,[ X1, Y1 ]), Rest ]) ) :-
        eval_equality([X2|Xs],[Y2|Ys],Rest).

% -- Special: Arithmetic functions
eval( f('+',2,[ Term1, Term2 ]), States ) :- !, 
        eval_Op( '+', Term1, Term2, States ).
eval( f('-',2,[ Term1, Term2 ]), States ) :- !, 
        eval_Op( '-', Term1, Term2, States ).
eval( f('*',2,[ Term1, Term2 ]), States ) :- !, 
        eval_Op( '*', Term1, Term2, States ).
eval( f('div',2,[ Term1, Term2 ]), States ) :- !, 
        eval_Op( 'div', Term1, Term2, States ).
eval( f('mod',2,[ Term1, Term2 ]), States ) :- !, 
        eval_Op( 'mod', Term1, Term2, States ).
eval( f('/',2,[ Term1, Term2 ]), States ) :- !,     % NEW
        eval_Op( '/', Term1, Term2, States ).       % NEW
eval( f('>',2,[ Term1, Term2 ]), States ) :- !, 
        eval_Op( '>', Term1, Term2, States ).
eval( f('<',2,[ Term1, Term2 ]), States ) :- !, 
        eval_Op( '<', Term1, Term2, States ).
eval( f('>=',2,[ Term1, Term2 ]), States ) :- !, 
        eval_Op( '>=', Term1, Term2, States ).
eval( f('=<',2,[ Term1, Term2 ]), States ) :- !, 
        eval_Op( '=<', Term1, Term2, States ).

% common behaviour
eval_Op( Op, Term1, Term2, States ) :-
        (  Term1 = c(ANum1,0,[]), Term2 = c(ANum2,0,[]), !,
           eval_OpValue( Op, ANum1, ANum2, ANum3 ),
           States = [ er( [], c(ANum3,0,[]) ) ]

         ; (  Term1 = f(F,N,Terms), Ox = [1]
            ; Term2 = f(F,N,Terms), Ox = [2] ), !,
           eval( f(F,N,Terms), StatesF ),
           replace( f(Op,2,[Term1,Term2]), Ox, StatesF, States )  
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
eval_OpValue( '/', Num1, Num2, Num3 ) :-   % NEW
        Num3 is Num1 / Num2.               % NEW
eval_OpValue( '>', Num1, Num2, Num3 ) :- 
        Num1 > Num2, !, Num3 = 'True' ; Num3 = 'False'.
eval_OpValue( '<', Num1, Num2, Num3 ) :- 
        Num1 < Num2, !, Num3 = 'True' ; Num3 = 'False'.
eval_OpValue( '>=', Num1, Num2, Num3 ) :- 
        Num1 >= Num2, !, Num3 = 'True' ; Num3 = 'False'.
eval_OpValue( '=<', Num1, Num2, Num3 ) :- 
        Num1 =< Num2, !, Num3 = 'True' ; Num3 = 'False'.

% -- Special: IO functions
eval( f('>>',2,[Exp1,Exp2]), States ) :- !,
        ( eval( Exp1, StatesF ),!,
          replace( f('>>',2,[Exp1,Exp2]), [1], StatesF, States )
        ; normal_form(Exp1),
          States = [ er( [], Exp2 ) ] 
        ).

eval( f('>>=',2,[Exp1,Exp2]), States ) :- !,
        ( eval( Exp1, StatesF ),!,
          replace( f('>>=',2,[Exp1,Exp2]), [1], StatesF, States )
        ; normal_form(Exp1),
          Exp1 = c('IO',1,[ExpIO]), 
          Exp2 = f(F,N,Args), append(Args,[ExpIO],Exps),
          States = [ er( [], f(F,N,Exps) ) ] 
        ).

eval( f('putChar',1,[Exp]), States ) :- !,
        ( eval( Exp, StatesF ),!,
          replace( f('putChar',1,[Exp]), [1], StatesF, States )
        ; normal_form(Exp),
          Exp = c([Ch],0,[]), 
          name(S,[Ch]), write(S),
          States = [ er( [], c('IO',1,[ c('Tuple',0,[]) ]) ) ] 
        ).

eval( f('getChar',0,[]), States ) :- !,
        prompt(_,'?'),
        get0(Ch),
        States = [ er( [], c('IO',1,[c([Ch],0,[])]) ) ].

eval( f('done',0,[]), States ) :- !,
        States = [ er( [], c('IO',1,[ c('Tuple',0,[]) ]) ) ].

eval( f('return',1,[Exp]), States ) :- !,
        ( eval( Exp, StatesF ),!,
          replace( f('return',1,[Exp]), [1], StatesF, States )
        
        ; normal_form(Exp),
          Exp = c(C,N,Exps), 
          States = [ er( [], c('IO',1,[ c(C,N,Exps) ]) ) ] 
        ).

eval( f('readFile',1,[Exp]), States ) :- !,
        ( eval( Exp, StatesF ),!,
          replace( f('readFile',1,[Exp]), [1], StatesF, States )
        
        ; normal_form(Exp),
          Exp = c(_,_,_), 
          toString(Exp,Str), name(FileName,Str),
          ( file_exists(FileName),
            open(FileName, read, Stream),
            read_all( FileStr, Stream ),
          close(Stream),
            toString(IntStr,FileStr),
            States = [ er( [], c('IO',1,[IntStr]) ) ]
           ; States=[] )
        ).

eval( f('writeFile',2,[Exp1,Exp2]), States ) :- !,
        ( eval( Exp1, States1 ),!,
          replace( f('writeFile',2,[Exp1,Exp2]), [1], States1, States )
         ;normal_form(Exp1),
          eval( Exp2, States2 ),!,
          replace( f('writeFile',2,[Exp1,Exp2]), [2], States2, States )
         ;normal_form(Exp1),
          normal_form(Exp2),
          toString(Exp1,Str), name(FileName,Str),
          toString(Exp2,FileStr), name(Output,FileStr),
          ( open(FileName, write, Stream),
            write(Stream,Output),
            close(Stream),!,
            States = [ er( [], c('IO',1,[ c('Tuple',0,[]) ]) ) ]
           ;States=[] )  ).

toString( c('Nil',0,[]), [] ).
toString( c('Cons',2,[ c([Ch],0,[]), String ]), [Ch|Str] ) :-
        toString( String, Str ).

% -- Special: Lambda Abstractions
eval( f('\\',N,[Vars,Expr|Exprs]), [er([],Term)] ) :- !,
        length([Vars,Expr|Exprs],N),
        Vars = c('\\Vars',Np,Pattern),
        susts( f('\\',Np,Pattern), f('\\',Np,Exprs), Sust ),
        apply( Sust, Expr, Term ).

% -- Special: Char-Int Conversion
eval( f('ord',1,[Exp]), States ) :- !,
        ( eval(Exp,StatesF),!,
          replace( f('ord',1,[Exp]), [1], StatesF, States )
         ;normal_form(Exp),
          Exp = c([C],0,[]), 
          States = [ er( [], c(C,0,[]) ) ] ).
eval( f('chr',1,[Exp]), States ) :- !,
        ( eval(Exp,StatesF),!,
          replace( f('chr',1,[Exp]), [1], StatesF, States )
         ;normal_form(Exp),
          Exp = c(C,0,[]), 
          States = [ er( [], c([C],0,[]) ) ] ).

% -- Special: Float-Int Conversion                            % NEW
eval( f('float',1,[Exp]), States ) :- !,                      % NEW
        ( eval(Exp,StatesF),!,                                % NEW
          replace( f('float',1,[Exp]), [1], StatesF, States ) % NEW
         ;normal_form(Exp),                                   % NEW
          Exp = c(Int,0,[]),                                  % NEW
          integer(Int), Float is float(Int),                  % NEW
          States = [ er( [], c(Float,0,[]) ) ] ).             % NEW

eval( f('int',1,[Exp]), States ) :- !,                        % NEW
        ( eval(Exp,StatesF),!,                                % NEW
          replace( f('int',1,[Exp]), [1], StatesF, States )   % NEW
         ;normal_form(Exp),                                   % NEW
          Exp = c(Float,0,[]),                                % NEW
          float(Float), Int is integer(Float),                % NEW
          States = [ er( [], c(Int,0,[]) ) ] ).               % NEW

% -- Show function
eval( f('show',1,[Exp]), States ) :- !,
        ( eval(Exp,StatesF),!,
          replace( f('show',1,[Exp]), [1], StatesF, States )
         ;normal_form(Exp),
          show(Exp,Str),
          toString(ResultExpr,Str),
          States = [ er( [], ResultExpr) ] ).

% -- Undefined function
eval( f('undefined',0,[]), States ) :- !,
          States = [].

% -- Constraint Evaluation
eval( f('&',2,[f('success',0,[]),Ct2]), States ) :- !,
        States = [ er([],Ct2) ].

eval( f('&',2,[Ct1,Ct2]), States ) :- !,
        (  eval( Ct1, StatesCt1 ),
           replace( f('&',2,[Ct1,Ct2]), [1], StatesCt1, States )
         ; eval( Ct2, StatesCt2 ),
           replace( f('&',2,[Ct1,Ct2]), [2], StatesCt2, States )
        ).

% -- Simple Constraint Evaluation
eval( f('=:=',2,[Ct1,Ct2]), States ) :- !,
        evalCT( (Ct1,Ct2), States ).

%Function
evalCT( ( f(F,N,Terms), Ct2 ), States ) :-
        eval( f(F,N,Terms), StatesT ),
        replace( f('=:=',2,[f(F,N,Terms),Ct2]), [1], StatesT, States ).

%Function
evalCT( ( Ct1, f(F,N,Terms) ), States ) :-
        eval( f(F,N,Terms), StatesT ),
        replace( f('=:=',2,[Ct1,f(F,N,Terms)]), [2], StatesT, States ).

%Same Constructor
evalCT( ( c(F,N,Args1), c(F,N,Args2) ), States ) :- 
        States = [er( [], Constraint )],
        eval_join( Args1, Args2, Constraint ).

%Different Constructor
evalCT( ( c(F1,N1,Args1), c(F2,N2,Args2) ), [] ) :- 
        F1 \== F2 ; N1 =\= N2 ; length(Args1,M1), length(Args2,M2), M1 =\= M2.

%Variable
evalCT( ( v(N), v(M) ), States ) :- 
        States = [er( [s( N, v(M) )], f('success',0,[]) )].

%Variable and Contructor
evalCT( ( v(M), c(F,N,Terms) ), States ) :- 
        \+( occur_check( M, c(F,N,Terms) ) ),
        maxVar(Max),
        eval_freshvars( Terms, Max, [], Vars ),
        eval_join( Vars, Terms, Constraint ),
        States = [er( [s( M, c(F,N,Vars) )], Constraint )].

%Contructor and Variable
evalCT( ( c(F,N,Terms), v(M) ), States ) :- 
        \+( occur_check( M, c(F,N,Terms) ) ),
        maxVar(Max),
        eval_freshvars( Terms, Max, [], Vars ),
        eval_join( Vars, Terms, Constraint ),
        States = [er( [s( M, c(F,N,Vars) )], Constraint )].

%Variable and Contructor
evalCT( ( v(M), c(F,N,Terms) ), [] ) :- % --- occur_check
        occur_check( M, c(F,N,Terms) ).

%Contructor and Variable
evalCT( ( c(F,N,Terms), v(M) ), [] ) :- % --- occur_check
        occur_check( M, c(F,N,Terms) ).

%---

eval_join( [], [], f('success',0,[]) ).
eval_join( [Exp1], [Exp2], f('=:=',2,[Exp1,Exp2]) ).
eval_join( [Exp1,Exp11|Exps1], [Exp2,Exp22|Exps2], Constraint ) :-
        Constraint = f('&',2,[ f('=:=',2,[Exp1,Exp2]), Rest ]),
        eval_join( [Exp11|Exps1], [Exp22|Exps2], Rest ).

eval_freshvars( [], _, Vars, Vars ).
eval_freshvars( [_|Terms], Max, Vars1, Vars3 ) :-
        Max1 is Max + 1,
        append( Vars1, [v(Max1)], Vars2 ),
        eval_freshvars( Terms, Max1, Vars2, Vars3 ).

occur_check( M, v(M) ).
occur_check( M, c(_,_,Terms) ) :- occur_checkL( M, Terms ).

occur_checkL( M, [Term|_] ) :- occur_check( M, Term ),!.
occur_checkL( M, [_|Terms] ) :- occur_checkL( M, Terms ).

% -- Main ---------------------------------------
eval( f(F,N,Terms), States ) :- 
        length( Terms, N ),         % needed with High Order
        dt( F, N, DefTree ),
        maxVar(Max),
        rename( DefTree, Max, ADefFRESH ),
        evalT( f(F,N,Terms), ADefFRESH, States ).
eval( c(F,N,Terms), States ) :- 
        eval_some( Terms, c(F,N,Terms), 1, States ).

% -- Find some evaluable subexpression
eval_some( [Term|_], Exp, N, States ) :-
        eval( Term, StatesT ),!,
        replace( Exp, [N], StatesT, States ).
eval_some( [_|Terms], Exp, N, States ) :-
        N1 is N + 1,
        eval_some( Terms, Exp, N1, States ).

% -- Main for Definitional Trees -------------------------------
evalT( Expression, rule(TermL,TermR), [er([],Term)] ) :-    % -- Rule
        susts( TermL, Expression, Susts ),
        apply( Susts, TermR, Term ).
evalT( Expression, tor( Tree1, Tree2 ), States ) :-         % -- OR
        evalT( Expression, Tree1, States1 ),
        evalT( Expression, Tree2, States2 ),
        append( States1, States2, States ).
evalT( Expression, branch( _, Ox, _, Trees ), States ) :-
        pos( Expression, Ox, c(F,N,Terms) ),
        evalT_some( Trees, Expression, Ox, c(F,N,Terms), States ).
evalT( Expression, branch( _, Ox, flex, Trees ), States ) :-
        pos( Expression, Ox, v(N) ),
        evalT_instantiate( Trees, Expression, Ox, N, States ).
evalT( Expression, branch( _, Ox, _, _ ), States ) :-
        pos( Expression, Ox, f(F,N,Terms) ),
        eval( f(F,N,Terms), StatesF ),
        replace( Expression, Ox, StatesF, States ).

pattern( branch( Pattern, _, _, _ ), Pattern ).
pattern( rule( Pattern, _ ), Pattern ).
pattern( tor( Tree1, Tree2 ), Pattern1 ) :-
        pattern( Tree1, Pattern1 ), pattern( Tree2, Pattern2 ),
        susts(Pattern1,Pattern2,_).

evalT_some( [], _, _, c(_,_,_), [] ). % -- error no one evaluable
evalT_some( [Tree|_], Expression, Occurrence, c(F,N,_), States ) :-
        pattern( Tree, Pattern ), pos( Pattern, Occurrence, c(F,N,_) ),!,
        evalT( Expression, Tree, States ).
evalT_some( [_|Trees], Expression, Occurrence, c(F,N,_), States ) :-
        evalT_some( Trees, Expression, Occurrence, c(F,N,_), States ).

evalT_instantiate( [], _, _, _, [] ).
evalT_instantiate( [Tree|Trees], Expression, Occurrence, N, 
                     [er(Susts,ExpressionS)|States] ) :-
        pattern( Tree, Pattern ), 
        pos( Pattern, Occurrence, TermS ),
        Susts = [s(N,TermS)], apply( Susts, Expression, ExpressionS ),
        evalT_instantiate( Trees, Expression, Occurrence, N, States ).

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ---------------------------------------------------------------------
                     General Module
  ---------------------------------------------------------------------
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

% ---------------------------------------------------------------------------
% Modified Write ------------------------------------------------------------
% ---------------------------------------------------------------------------

% EXPRESSION ---
write2( X ) :- show( X, Str ),name(Output,Str),write(Output).

% Characters 
show( c([7],0,[]), Str ) :- !,append([39],"\\a",S1),append(S1,[39],Str).
show( c([8],0,[]), Str ) :- !,append([39],"\\b",S1),append(S1,[39],Str).
show( c([9],0,[]), Str ) :- !,append([39],"\\t",S1),append(S1,[39],Str).
show( c([10],0,[]), Str ) :- !,append([39],"\\n",S1),append(S1,[39],Str).
show( c([11],0,[]), Str ) :- !,append([39],"\\v",S1),append(S1,[39],Str).
show( c([12],0,[]), Str ) :- !,append([39],"\\f",S1),append(S1,[39],Str).
show( c([13],0,[]), Str ) :- !,append([39],"\\r",S1),append(S1,[39],Str).
show( c([27],0,[]), Str ) :- !,append([39],"\\e",S1),append(S1,[39],Str).
show( c([127],0,[]), Str ) :- !,append([39],"\\d",S1),append(S1,[39],Str).
show( c([C],0,[]), Str ) :- !,append([39],[C],S1),append(S1,[39],Str).

% Lists
show( List, Str ) :- check_string(List),!,
        show_string(List,StrL),
        append([34],StrL,S1), append(S1,[34],Str).
show( c('Nil',0,[]), Str ) :- !, Str="[]".
show( c('Cons',2,[Term,List]), Str ) :-  
        checkGround(c('Cons',2,[Term,List])), !,
        show(Term,StrTerm),
        show_list(List,StrList),
        append("[",StrTerm,S1), append(S1,StrList,S2), append(S2,"]",Str).
show( c('Cons',2,[Term,List]), Str ) :-  !,
        show(Term,StrTerm), show(List,StrList),
        append("(",StrTerm,S1), append(S1,":",S2), 
        append(S2,StrList,S3), append(S3,")",Str).

check_string( c('Cons',2,[ c([_],0,[]), c('Nil',0,[]) ]) ).
check_string( c('Cons',2,[ c([_],0,[]), Str ]) ) :- 
        check_string(Str).

show_string( c('Nil',0,[]), [] ).
show_string( c('Cons',2,[ Char, List ]), Str ) :- 
        show(Char,StrChar), 
        ( StrChar=[39,C,39],     Ch=[C]
         ;StrChar=[39,C1,C2,39], Ch=[C1,C2] ),
        append(Ch,StrList,Str),
        !,show_string(List,StrList).

checkGround( c('Nil',0,[]) ).
checkGround( c('Cons',2,[_,List]) ) :- checkGround(List).

show_list( c('Nil',0,[]), [] ).
show_list( c('Cons',2,[Term,List]), Str ) :- 
        show(Term,StrTerm),
        append(",",StrTerm,S1),
        append(S1,StrList,Str),
        !,show_list(List,StrList).

% Tuples
show( c('Tuple',N,Exprs), Str ) :- !,length(Exprs,N),
        showS(Exprs,StrExprs),
        append("(",StrExprs,S1), append(S1,")",Str).

% Constructors
show( c(F,0,[]), Str ) :- !, name(F,Str).
show( c(F,_,Terms), Str ) :- 
        name(F,StrF), showL(Terms,StrTerms),
        append("(",StrF,S1), append(S1," ",S2), 
        append(S2,StrTerms,S3), append(S3,")",Str).

% Special Functions
show( f('\\@',_,[f(F,N,Args)|Terms]), Str ) :- !,
        append(Args,Terms,Args2),
        show( f(F,N,Args2), Str ).

show( f('IfThenElse',3,[EBool,ETrue,EFalse]), Str ) :- !,
        show(EBool,StrBool), show(ETrue,StrTrue), show(EFalse,StrFalse),
        append("if ",StrBool,S1), 
        append(S1," then ",S2), append(S2,StrTrue,S3),
        append(S3," else ",S4), append(S4,StrFalse,Str).

show( f('\\',_,[c('\\Vars',_,Pattern),Expr|Exprs] ), Str ) :- !,
        showL(Pattern,StrPattern), show(Expr,StrExpr), showL(Exprs,StrExprs),
        append("(\\ ",StrPattern,S1), append(S1," -> ",S2),
        append(S2,StrExpr,S3), append(S3,") ",S4),
        append(S4,StrExprs,Str).

show( f('\\=>',_,[Constraint,Expr]), Str ) :- !,
        show(Constraint,StrConstraint), show(Expr,StrExpr),
        append(StrConstraint," \\=> ",S1), append(S1,StrExpr,Str).

% Infix Functions
show( f(F,2,[Term1,Term2]), Str ) :- infix(F,_,_),!, 
        show(Term1,StrTerm1), show(Term2,StrTerm2), name(F,StrF),
        append("(",StrTerm1,S1), append(S1," ",S2), 
        append(S2,StrF,S3), append(S3," ",S4), 
        append(S4,StrTerm2,S5), append(S5,")",Str).
show( f(F,2,[Term1]), Str ) :- infix(F,_,_), !, 
        show(Term1,StrTerm1), name(F,StrF),
        append("(",StrTerm1,S1), append(S1," ",S2), 
        append(S2,StrF,S3), append(S3,")",Str).
show( f(F,2,[]), Str ) :- infix(F,_,_), !, 
        name(F,StrF),
        append("(",StrF,S1), append(S1,")",Str).

% Functions
show( f(F,_,Terms), Str ) :- 
        showL(Terms,StrTerms), name(F,StrF),
        append("(",StrF,S1), append(S1," ",S2),
        append(S2,StrTerms,S3), append(S3,")",Str).
show( forward(F,_,Terms), Str ) :- 
        showL(Terms,StrTerms), name(F,StrF),
        append("(",StrF,S1), append(S1," ",S2),
        append(S2,StrTerms,S3), append(S3,")",Str).

% Variables
show( v(N), Str ) :- var_seen(Name,N), !, name(Name,Str).
show( v(N), Str ) :- name(N,StrN), append("var",StrN,Str).

% List of expressions
showL( [], "" ).
showL( [X], Str ) :- show(X,Str).
showL( [X,Y|Xs], Str ) :- 
        show(X,StrX),
        append(StrX," ",S1), append(S1,StrRest,Str),
        !,showL([Y|Xs],StrRest).

showS( [], "" ).
showS( [X], Str ) :- show(X,Str).
showS( [X,Y|Xs], Str ) :- 
        show(X,StrX),
        append(StrX,", ",S1), append(S1,StrRest,Str),
        !,showS([Y|Xs],StrRest).

% STATES ----
% State
write2( er([],f('success',0,[])) ) :- !,
        write('[ ]').

write2( er([],Exp) ) :- !,
        write('[ '),
        write2(Exp), 
        write(' ]').

write2( er(Susts,f('success',0,[])) ) :- !,
        write('[ '),
        write('{'), write2CTL(Susts), write('}'),
        write(' ]').

write2( er(Susts,Exp) ) :- 
        write('[ '),
        write('{'), write2CTL(Susts), write('} '), 
        write2(Exp),
        write(' ]').

% Sustitution
write2( s(N,Term) ) :- write2(v(N)), write('='), write2(Term).

write2_states( [] ).
write2_states( [X] ) :- write2(X).
write2_states( [X,Y|Xs] ) :- write2(X), nl, write2_states([Y|Xs]).

% TYPES ----
write2( elem(type('List',1,[elem(type('Char',0,[]))])) ) :- !,write('String').
write2( type('List',1,[Type]) ) :- !,write('['),write2(Type),write(']').
write2( type('Tuples',N,Exprs) ) :- !,
        length(Exprs,N),write('('),write2CTL(Exprs),write(')').

write2( type(F,0,[]) ) :- !,write(F).
write2( type(F,N,Vars) ) :- N > 0, 
        write('('), write(F), write(' '), write2TL(Vars), write(')').
write2( typevar(N) ) :- write('var'), write(N).

write2( elem(T) ) :- write2T(T).
write2( curried(elem(T),Ts) ) :- write2T(T), write(' -> '), write2(Ts).

write2T( curried(Type1,Type2) ) :- !, 
        write('('), write2(curried(Type1,Type2)), write(')').
write2T( Type ) :- write2(Type).

write2TL( [] ).
write2TL( [X] ) :- write2T(X).
write2TL( [X,Y|Xs] ) :- write2T(X), write(' '), write2TL([Y|Xs]).

% DEFTREES --
write2( rule(L,R) ) :- write2_dt(rule(L,R),0).
write2( branch(P,O,M,Dts) ) :- write2_dt(branch(P,O,M,Dts),0).
write2( tor(L,R) ) :- write2_dt(tor(L,R),0).

write2_dt( rule(L,f('\\=>',2,[Cond,Expr])), Spaces ) :- !,tab(Spaces),
        write('rule('),
        write2(L),write(' | '), write2(Cond),write(' = '),write2(Expr),
        write(')'),nl.
write2_dt( rule(L,R), Spaces ) :- tab(Spaces),
        write('rule('),write2(L), write(' = '), write2(R), write(')'),nl.
write2_dt( branch(P,O,M,Dts), Spaces ) :- tab(Spaces), Spaces1 is Spaces + 1,
        write('branch('),write2(P),write(','),
        write2Ox(O),write(','),write(M),write(') ->'),nl,
        write2_dtL( Dts, Spaces1 ).
write2_dt( tor(L,R), Spaces ) :- tab(Spaces), Spaces1 is Spaces + 1,
        write('or ->'), nl,
        write2_dtL( [L,R], Spaces1 ).
                
write2_dtL( [], _ ).
write2_dtL( [X|Xs], Spaces ) :- write2_dt(X, Spaces), write2_dtL(Xs,Spaces).

% Ocurrence
write2Ox( [] ).
write2Ox( [X] ) :- write(X).
write2Ox( [X,Y|Xs] ) :- write(X), write('.'), write2Ox([Y|Xs]).

% List of expressions
write2L( [] ).
write2L( [X] ) :- write2(X).
write2L( [X,Y|Xs] ) :- write2(X), write(' '), write2L([Y|Xs]).

write2CTL( [] ).
write2CTL( [X] ) :- write2(X).
write2CTL( [X,Y|Xs] ) :- write2(X), write(', '), write2CTL([Y|Xs]).

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
        narrowing( [er([],Expression)] ),
        ( solution 
        ;\+(solution),
         statistics(runtime,[TimeFin,_]), 
         Time is TimeFin - TimeIni,
         write('No solution. '), format(' ~0d ms',[Time]),nl ).

pass :- assert( debugmode ).
nopass :- retractall( debugmode ).

debugStep(States) :-
        ( debugmode, 
          write('-------------------------------------------------------'),nl,
          write2_states( States ),nl,
          write('-------------------------------------------------------'),nl,
          control
         ;\+(debugmode) ).

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

noIO( [er(_,Expr)|_] ) :- io_Expr(Expr),!,fail.
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

narrowing( [er(Susts,Exp)|States], ResultStates ) :- % general
        maxvar( Exp, Max ), retractall(maxVar(_)), assert(maxVar(Max)),
        eval( Exp, StatesE ),!,
        narrowing_join( StatesE, Susts, RStates ),
        append( RStates, States, ResultStates ).
narrowing( [er(Susts,Exp)|States], ResultStates ) :-
        normal_form( Exp ),!, 
        assert(solution),
        time(TimeIni), statistics(runtime,[TimeFin,_]), 
        Time is TimeFin - TimeIni,
        (  Exp=c('IO',1,_),nl
         ; \+(Exp=c('IO',1,_)) ),
        write('Solution: '), write2( er(Susts,Exp) ),
        format(' ~0d ms',[Time]),nl,
        retractall(time(_)), 
        statistics(runtime,[NewTimeIni,_]), 
        assert(time(NewTimeIni)),
        !,narrowing( States, ResultStates ).
narrowing( [er(Susts,Exp)|States], ResultStates ) :-
        time(TimeIni), statistics(runtime,[TimeFin,_]), 
        Time is TimeFin - TimeIni,
        write('Cannot be evaluated: '), write2( er(Susts,Exp) ),
        format(' ~0d ms',[Time]),nl,
        retractall(time(_)), 
        statistics(runtime,[NewTimeIni,_]), 
        assert(time(NewTimeIni)),
        !,narrowing( States, ResultStates ).
narrowing( [], [] ).

narrowing_join( [], _, [] ).
narrowing_join( [er( Susts1, Exp )|States1], Susts, 
                [er( Susts4, Exp )|States2] ) :-
        applys( Susts1, Susts, Susts2 ),
        append( Susts2, Susts1, Susts3 ),
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

% ---------------------------------------------------------------------------
% Menu & Global Control -----------------------------------------------------
% ---------------------------------------------------------------------------

:- use_module(library(system)), use_module(library(lists)).
:- dynamic 
     haltSystem/0, 
     prelude/3. %Parser,TypeChecker,DefTree

main_and_halt:- main,halt.

main :- haltSystem,!,retractall(haltSystem).
main :- 
        on_exception(_,
         (prompt(_,'> '), prompt( InputS1 ),
         process_then(InputS1,main)),main).
        


process_then(InputS1,Then):-
          process( InputS1, InputS2 ), 
          once( 
          ( InputS2=[], Input=[], Then) ;

          ( append( Input, [10], InputS2 ),
             (  command_read( Input )
              ; command_debug( Input )
              ; command_deftree( Input )
              ; command_type( Input )
              ; command_help( Input )
              ; command_cd( Input )
              ; command_pwd( Input )
              ; command_exit( Input )
              ; command_exec( Input )
              ; (nopass,command_solve( Input ) )))), 
         !,Then.


prompt( Input ) :-
        get0(X),
        ( (X =:= 10 ; X =:= 13), !, Input = []
         ;Input = [X|Input2], prompt(Input2) ).

command_solve( `` ) :- !.
command_solve( Input ) :- %No known command accepted
        ( sappend( `:load `,  _, Input )
         ;sappend( `:l `,     _, Input )
         ;sappend( `:ex `,  _, Input )
         ;sappend( `:x `,  _, Input )
         ;sappend( `:debug `, _, Input )
         ;sappend( `:d `,     _, Input )
         ;sappend( `:deftree `, _, Input )
         ;sappend( `:e `, _, Input )
         ;sappend( `:type `, _, Input )
         ;sappend( `:t `, _, Input )
         ;sappend( `:help `, _, Input )
         ;sappend( `:h `, _, Input )
         ;sappend( `:cd `, _, Input )
         ;sappend( `:c `, _, Input )
         ;sappend( `:type `, _, Input )
         ;sappend( `:t `, _, Input )
         ;sappend( `:quit `, _, Input )
         ;sappend( `:q `, _, Input )      ),
        !.
command_solve( Input ) :-
        cleanVars,
        exprIni( Exp, Input, []),
        sCheckExpr( Exp, _, [], _),!,
        ( solve( Exp )
         ;write('') ).
command_solve( Text ) :- 
        name( Atom, Text ),
        write('ERROR: Incorrect expression '), write(Atom), write(' found'),nl.

command_read( Input ) :-
        ( append( ":load ", Str, Input )
         ;append( ":l ",    Str, Input ) ), !,
        Str = [34|Rest],
        get_ToQuote( Rest, Name, [] ),
        atom_chars( File, Name ),
        parser( File, Source ),
        checkType( Source, Rules ),
        % level_confluence, non_ovelapping,
        deftrees(Rules).

command_debug( Input ) :-
        ( append( ":debug ", Expression, Input )
         ;append( ":d ",     Expression, Input ) ),!,
        ( pass,
          command_solve( Expression ),
          nopass
         ;nopass).

command_deftree( Input ) :-
        ( append( ":deftree ", Function, Input )
         ;append( ":e ",       Function, Input ) ),!,
        ( append( Name, [32], Function )
         ;Name=Function ),
        name(F,Name),
        (  (dt(F,_,DT),!, format("~a:\n",[F]), write2(DT), nl)
         ; ( \+(dt(F,_,_ /*DT*/)), format("There is no definitional tree\n",[]) )).

command_type( Input ) :-
        ( append( ":type ", ExpS, Input )
         ;append( ":t ",    ExpS, Input ) ),!,
        ( exprIni(Exp,ExpS,[])
        ; functionInfixID(F,_,_,ExpS,[]), Exp=f(F,2,[]) ),
        sCheckExpr( Exp, TypeExpr, [], Susts ),
        applySusts(Susts,TypeExpr,Type), simpType(Type,ShowType),
        write2(ShowType),nl.

command_help( Input ) :- 
        ( Input = ":help "
         ;Input = ":h "   ),!,
        nl,
        write('Commands:'),nl,
        write('   :load "<File>"      - Load Curry file in interpreter [:l]'),nl,
        write('   <Expression>        - Evaluate expression'),nl,
        write('   :debug <Expression> - Debug expression evaluation    [:d]'),nl,
        write('   :deftree <Function> - Show function deftree          [:e]'),nl,
        write('   :type <Expression>  - Show expression type           [:t]'),nl,
        write('   :cd "<Directory>"   - Change working directory       [:c]'),nl,
        write('   :pwd                - Show working directory         [:p]'),nl,
        write('   :help               - This help menu                 [:h]'),nl,
        write('   :quit               - Exit interpreter               [:q]'),nl,
        nl,
        write(' :? refers to the short form of each command (only first character)'),
        nl,
        nl.

command_cd( Input ) :-
        ( append( ":cd ", Str, Input )
         ;append( ":c ",  Str, Input ) ), !,
        Str = [34|Rest],
        get_ToQuote( Rest, Name, [] ),
        atom_chars( Dir, Name ),
        working_directory(_,Dir).

command_pwd( Input ) :-
        ( Input = ":pwd "
         ;Input = ":p "   ),!,
        working_directory(Dir,Dir),
        write(Dir),nl.

command_exit( Input ) :-
        ( Input = ":quit "
         ;Input = ":q "   ),!,
        assert(haltSystem).

%---Save all information about loaded program in the functor "prelude"
save_state :-
     % Info from Parser
        findall(type(X,Y),type(X,Y),               TypesParser),
        findall(function(X,Y),function(X,Y),       FuncsParser),
        findall(constructor(X,Y),constructor(X,Y), ConsParser),
        findall(infix(X,Y,Z),infix(X,Y,Z),         InfixParser),
        findall(builtin(X),builtin(X),             NoRedefParser),

     % Info from Type Checker
        findall(annotation(X,Y),annotation(X,Y),       AnnCheckType),
        findall(function(X,Y,Z),function(X,Y,Z),       FuncsCheckType),
        findall(constructor(X,Y,Z),constructor(X,Y,Z), ConsCheckType),

     % Info from Def Trees
        findall(dt(X,Y,Z),dt(X,Y,Z),                 DTDefTree),

      retractall(prelude(_,_,_)),
      assert(
       prelude(
        [TypesParser,FuncsParser,ConsParser,InfixParser,NoRedefParser],
        [AnnCheckType,FuncsCheckType,ConsCheckType],
        [DTDefTree]
      )).

%---Retrieve information about prelude for parser 
% (called in "program")
prelude_parser :-
      prelude(InfoParser,_,_),
      assertAll(InfoParser).

% Retrieve information about prelude for type checker 
% (called in "checkType")
prelude_typechecker :-
      prelude(_,InfoTypeChecker,_),
      assertAll(InfoTypeChecker).

% Retrieve information about prelude for deftree generation 
% (called in "deftrees")
prelude_deftree :-
      prelude(_,_,InfoDefTree),
      assertAll(InfoDefTree).

assertAll([]).
assertAll([X|Xs]) :- assertAll1(X),assertAll(Xs).

assertAll1([]).
assertAll1([X|Xs]) :- assert(X),assertAll1(Xs).

%---Load Prelude File
load_prelude :-
   write('Loading prelude...'),nl,
   name(':load "prelude_float"',Command), %--needed to allow "
   on_exception(_,
     ( command_read(Command),! ;halt ),
     (write('Internal error processing prelude file'),nl,halt) ),
   nl.

:- 
   nl,nl,
   write('***********UPV-Curry interpreter*******************'),
   nl,nl, 
   % Save state with basic info
   save_state,
   % Load Prelude
   load_prelude,
   % Save state with prelude info
   save_state,
   % Help
   command_help(":help "),
   % Interacting Prompt
   !.

:-must_det_l((retract(current_prolog_flag_double_quotes(ResetTo)),
  set_prolog_flag(double_quotes, ResetTo))).

% :- initialization(main).
