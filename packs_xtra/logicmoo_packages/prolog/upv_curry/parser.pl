:- set_prolog_flag(double_quotes, codes).
:- style_check(-discontiguous).
:- expects_dialect(sicstus).
:- discontiguous constrTerm/3. 
:- discontiguous constrTermNested1/3.
:- discontiguous constrTermNested2/3.
:- discontiguous bexpr/3.

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
   where ` l | c = r ` is written as `rule( l, f('\=>',2,[c,r]) )`
  - High Order (partial application of functions) using `f('\@',N,[...])` 
   with function symbol in the first argument and 
   the argument expressions in the other arguments.
  - Lambda Expressions using `f('\',N1,[c('\Var',N2,Pattern),Expr|Args])` 
   where Pattern is the arguments of the lambda expression, 
   Expr is the right expression of the lambda expression and
   Args is the set of expressions passed as arguments 
   to the lambda expression (if any).
  - Sharing implemented as f('\#',2,[Variable,Expresion])
  - In SICSTus Prolog the symbol '\' is treated as special, 
   so '\=>' has to be '\\=>' and so on.
  - `forward` node denotes a reference to a function not passed yet.
*/

:- dynamic 
        lastvar/1, var_seen/2, % variables control
        noRedef/1, % does not allowed to redefine a function
        infix/3, % associative disambiguity (c - xfx, r - xfy, l - yfx)
        type/2, function/2, constructor/2, % elements found
        ini/0, % to allow new variables in expressions to evaluate
        nomfun/1. %local fuction names control

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
           curried/2  - `->` operator 
           elem/1     - each element in a `->` application
           type/3     - type found
           typevar/1  - type variable passed
*/

basicParsing :- 
   % Basic types
   assert(builtin('Int')),        assert(type('Int',0)),
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
   assert(builtin('+')),
   assert(builtin('-')),
   assert(builtin('*')),
   assert(builtin('div')),
   assert(builtin('mod')),
   % Relational functions
   assert(builtin('==')),         assert(function('==',2)),
   assert(builtin('<')),
   assert(builtin('>')),
   assert(builtin('=<')),
   assert(builtin('>=')),
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
   % init control of the name of local functions.
   assert(nomfun([(0,upv,dsic)])).
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

parser( File1, FinalResult ) :- \+ file_exists(File1),
    filematch_ext(['','.curry','.cur'],File1,Match),
    file_exists(Match),!,
    parser( Match, FinalResult ).

parser( File1, FinalResult ) :-
        (  file_exists(File1),!,File=File1
         ; sicstus_atom_chars( File1, Str1 ),
           sappend(Str1,`.curry`,Str2),
           sicstus_atom_chars( File2, Str2 ),
           file_exists(File2),!,File=File2
         ; sicstus_atom_chars( File1, Str1 ),
           sappend(Str1,`.cur`,Str2),
           sicstus_atom_chars( File2, Str2 ),
           file_exists(File2),!,File=File2
         ; write('Parsing error, file doesn\'t exist '), write(File1),nl,fail ),
        (  open( File, read, Stream ),
           read_all( Text, Stream ),
           close( Stream ),!
         ; write('Parsing error, file cannot be read '), 
           write(File),nl,fail ),        
        (  process( Text, Input ),!         %clean enters, tabs, and blanks
         ; write('Lexical error, file cannot be processed '), write(File),
           nl,fail ),
        (  retractall(annotation(_,_)),
           retractall(nomfun(_)),assert(nomfun([(0,upv,dsic)])),
           program( Result, Input, [] ),!,
           treat( Result, FinalResult )
         ; write('Syntactic error, file cannot be parsed '), write(File),
           nl,fail ),
        !. 

read_all( Text, Stream ) :-
        get0( Stream, X ),
        ( X =\= -1, Text = [X|Text2], !,read_all( Text2, Stream )
         ;X =:= -1, Text = [] ).

% ----- delete extra characters
process( Text1, Text5 ) :-
        clean_comments( Text1, Text2 ),!,
        process_where( Text2, Text3 ),!,
        clean_enters( Text3, Text4 ),!,
        clean_blanks( Text4, Text5 ),!.
        
% ----------------------------------------
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
clean_simple_comment( [], [10] ).

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
         ; sappend( Text1x1, Text1x3, Text1x ), % normal case
           clean_Declaration( Text2, Text1x2, Text3, 0 ),
           sappend( Text1x2, [10], Text1x3 ),
           sappend( Text1x, Text4, Text5 ),!,
           clean_enters( Text3, Text4 ) 
        ).

% -- Obtain a set of lines of the same function symbol
% in one only line
% fourth arg indicates actual tab stop
clean_Declaration( [], [], [], _ ) :- !.
clean_Declaration( Text1, Text1X, Text3, Tab ) :-
        get_tab( Text1, 0, TabT ),
        (  TabT > Tab,
           get_ToEnter( Text1, Text1x1, Text2 ),
           sappend( Text1x1, Text1x2, Text1X ),
           clean_Declaration( Text2, Text1x2, Text3, Tab )
         ; TabT =< Tab,
           Text1X = [],
           Text3 = Text1 ).

get_tab( [32|Text], Tab1, Tab3 ) :-
        Tab2 is Tab1 + 1,
        get_tab( Text, Tab2, Tab3 ).
get_tab( [9|Text], Tab1, Tab3 ) :-
        Tab2 is Tab1 + 1,
        get_tab( Text, Tab2, Tab3 ).
get_tab( [10|_], Tab1, Tab2 ) :- Tab2 is Tab1 + 1.
get_tab( [13|_], Tab1, Tab2 ) :- Tab2 is Tab1 + 1.
get_tab( [X|_], Tab, Tab ) :- X \== 32, X \== 9, X \== 10, X \== 13.
get_tab( [], Tab, Tab ).

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
         ; clean_id(     [X|Text1], Text1x, Text2 ),!
         ; clean_sym(    [X|Text1], Text1x, Text2 ) ),
        sappend( Text1x, Text3, Text4 ),
        clean_blanks( Text2, Text3 ).

clean_id( [C|Cs1], [C|Cs2], Cs3 ) :-
        ( C >= `0`, C =< `9`
         ;C >= `A`, C =< `Z`
         ;C >= `a`, C =< `z`
         ;[C] == `_` ),!,
        clean_id2(Cs1,Cs2,Cs3).

clean_id2( [C|Cs1], [C|Cs2], Cs3 ) :-
        ( C >= `0`, C =< `9`
         ;C >= `A`, C =< `Z`
         ;C >= `a`, C =< `z`
         ;[C] == `_` ),!,
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
        sappend(Text2,[34],Text4).

get_ToQuote( [34|Text], [], Text ).
get_ToQuote( [X|Text1], [X|Text2], Text3 ) :- !,
        get_ToQuote( Text1, Text2, Text3 ).

%---change where expressions separated by enters and
%   aligned in the same tab position
%   to a where expressions started by '{', separated by ';', and ended by '}'
process_where( S1, S2 ) :- pwhere1(S1,S2,0).

%- process whole string looking for where expressions
pwhere1( S1, S2, Tab ) :-
        S1 = [119,104,101,114,101,C|Xs],
         \+((C >= `0`, C =< `9`
            ;C >= `A`, C =< `Z`
            ;C >= `a`, C =< `z`
            ;[C] == `_`        )),
        S2 = [119,104,101,114,101,32,10,C|Ys],!,
        TabIni is Tab+5,
        build_blank_line(TabIni,IniS),sappend(IniS,[C|Xs],Ys0),
        pwhere2(Ys0,Ys1,S3,TabIni),
        sappend(Ys1,S4,Ys),
        !,pwhere1(S3,S4,0).
pwhere1( [10|Xs], [10|Ys], _ ) :- pwhere1(Xs,Ys,0).
pwhere1( [13|Xs], [13|Ys], _ ) :- pwhere1(Xs,Ys,0).
pwhere1( [X|Xs], [X|Ys], Tab ) :-
        Tab1 is Tab+1,
        pwhere1(Xs,Ys,Tab1).
pwhere1( [], [], _ ).
        

%- pwhere - process a where expression
%  some useful info: 123=='{', 125=='}', 59==';'
%
% Two possibilities: where expression with {} or
%                    where expression without {}
pwhere2( S1, Xs2, S2, _ ) :-
        get_ToEnter( S1, Xs, S2 ),
        clean_blanks(Xs,Xs1),
        Xs1=[123|_],
        !, % the where expression is correctly introduced
        sappend(Xs,[10],Xs2).

pwhere2( S1, Ys0, S3, TabIni ) :-
        build_blank_line(TabIni,Ini),sappend(Ini,[123,10|Ys],Ys0),
        get_ToEnter( S1, Xs, S2 ),
        % check if Xs (rest of line) is only blanks
        ( clean_blanks(Xs,[]),  % only blanks
          skip_blank_lines(S2,S4),
          get_tab(S4,0,TabItems)
        ; TabItems is TabIni+1   ),
        pwhereItems(S1,Ys1,S3,TabItems),
        %--add final char
        %-build blank line
        build_blank_line(TabItems,Blank),
        sappend(Blank,[125,10],EndLine),
        sappend(Ys1,EndLine,Ys4),
        %process where of taken text
        process_where(Ys4,Ys).

pwhereItems([],[],[],_).
pwhereItems(S1,Xs,S3,Tab) :-
        get_tab( S1, 0, TabT ),
        get_ToEnter( S1, Xs1, S2 ),
        % check if it is a blank line
        (clean_blanks(Xs1,[]), % blank line
         sappend(Xs1,[10|Ys],Xs),
         !,pwhereItems(S2,Ys,S3,Tab)
        ;( TabT >= Tab,
           %-check to put ';' (yes if next tab=Tab)
           skip_blank_lines(S2,S4),
           get_tab(S4,0,Tab2),
           (Tab2=:=Tab,
            build_blank_line(Tab,Blank),
            sappend(Blank,[59,10|Ys],Blank2),
            sappend([10],Blank2,EndLine),
            sappend(Xs1,EndLine,Xs)
           ;Tab2=\=Tab,
            build_blank_line(Tab,Blank),
            sappend(Blank,[10|Ys],Blank2),
            sappend([10],Blank2,EndLine),
            sappend(Xs1,EndLine,Xs) ),
           !,pwhereItems(S2,Ys,S3,Tab)
         ; TabT  < Tab,
           Xs=[],S3=S1   )).

skip_blank_lines([],[]).
skip_blank_lines(S1,S3) :-
        get_ToEnter(S1,Xs,S2),
        (clean_blanks(Xs,[]),   %blank line
         !,skip_blank_lines(S2,S3)
        ;S3=S1 ).

build_blank_line(0,[]) :- !.
build_blank_line(N,[32|Xs]) :- N1 is N-1,build_blank_line(N1,Xs).

% ----------------------------------------
% controls no forward node could be returned
treat( [], [] ).
treat( [X|Xs], [Y|Ys] ) :-
        treat2(X,Y),treat(Xs,Ys).

treat2( rule(L,R), rule(L,Rx) ) :- !,
        treatE(R,Rx).
treat2( data(Type,Constructors), data(Type,ConstructorsX) ) :-
        treatD(Constructors,ConstructorsX).
treat2( X, X ).

% Search forward node inside rules. They are NOT removed.
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

% Search forward node inside data declarations. They are removed.
treatD([],[]).
treatD([const(ID,N,TypeExprs1)|Xs],[const(ID,N,TypeExprs2)|Ys]) :-
        treatD1(TypeExprs1,TypeExprs2),
        treatD(Xs,Ys).

treatD1([],[]).
treatD1([X|Xs],[Y|Ys]) :- treatD2(X,Y),treatD1(Xs,Ys).

treatD2(elem(X),elem(Y)) :- treatD3(X,Y).
treatD2(curried(X1,X2),curried(Y1,Y2)) :- treatD3(X1,Y1),treatD2(X2,Y2).

treatD3(forward(Type,N,TypeExprs1),type(Type,N,TypeExprs2)) :-
       ( type(Type,N),!,
         treatD1(TypeExprs1,TypeExprs2)
       ; format('Parsing error, type ~a doesn\'t exist\n',[Type]),
         fail ).
treatD3(type(Type,N,TypeExprs1),type(Type,N,TypeExprs2)) :- !,
       treatD1(TypeExprs1,TypeExprs2).
treatD3(X,X).


% ----------------------------------------
% parser grammar
% ----------------------------------------

program( Program ) -->
        {cleanFuncConsTypes,prelude_parser},
        {write('Parsing...'),nl},
        pblock( Program ).

pblock( Block ) -->
        (  pragmaDeclarationL(Pragmas)
         ; \+(pragmaDeclarationL(_)),{Pragmas=[]} ),!,
        (  fixityDeclarationL
         ; \+(fixityDeclarationL) ),!,
        {sappend(Pragmas,BlockL,Block)},
        blockDeclarationL(BlockL).

% Fixity Declaration ---------------------------------------------------------
fixityDeclarationL -->
        fixityDeclaration,!,
        (
           fixityDeclarationL,!
         ; \+(fixityDeclarationL)
        ).

fixityDeclaration -->
        ( `infixc `, {Associ=c}
         ;`infixl `,{Associ=l}
         ;`infixr `,{Associ=r} ),
        natural(Nat),{number_chars(Num,Nat)},
        infixOpL(Associ,Num),
        `\n`.

infixOpL( Associ, Num ) -->
        infixOp(Associ,Num),
        (
          `,`,infixOpL(Associ,Num)
         ;\+(`,`)
        ).

infixOp(Associ,Num) -->
        ( ````,id(ID),````
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
         ; ` `,{Nat1=[]} ).
digit( C ) --> [C],{ C >= 48, C =< 57 }.

% Pragmas Delcaration --------------------------------------------------------
pragmaDeclarationL( [Pragma|Pragmas] ) -->
        pragmaDeclaration(Pragma),!,
        (
           pragmaDeclarationL(Pragmas),!
         ; \+(pragmaDeclarationL(_)),{Pragmas=[]}
        ).

pragmaDeclaration( pragma(Pragma) ) -->
        `pragma `,
        ( `flex `,{Pragma=flex}
         ;`rigid `,{Pragma=rigid}
         ;`optmatch `,{Pragma=optmatch} ),
        `\n`.

% Block Declaration ---------------------------------------------------------
blockDeclarationL( Result ) -->
        blockDeclaration(Block),!,
        (
           blockDeclarationL(Rest),!,
           {sappend(Block,Rest,Result)}
         ; {Result = Block} 
        ).
blockDeclaration( [Data] ) --> dataDeclaration( Data ).
blockDeclaration( DeclFunction ) --> functionDeclaration( DeclFunction ).

% types ---------------------------------------------------------------------
dataDeclaration(data(Type,Constructors)) --> 
        `data `, typeDeclaration(Type,Constructors), `\n`.

typeDeclaration(type(TypeName,N,TypeVars),Consts) --> 
        {cleanVars}, 
        typeConstrID(TypeName),
        {  \+(type(TypeName,_))
         ; type(TypeName,_), 
           format('Sorry, ~a type just exists',[TypeName]) },
        (  typeVarIDL(TypeVars), !, {length(TypeVars,N)}
         ; {N=0,TypeVars=[]} ),
        (  `=`,!
         ; {write('Expected ='),nl,!} ),
        {assert(type(TypeName,N))},
        (  constrDeclarationL(Consts),!
         ; {retract(type(TypeName,N))} ).

typeConstrID( TypeName ) --> id(ID), {sicstus_atom_chars(TypeName,ID)}.

typeVarIDL( [Var|Vars] ) --> 
        typeVarID( Var ), 
        (  typeVarIDL( Vars ),!
         ; {Vars=[]} ).

typeVarID( elem(typevar(N)) ) --> variableID(N).

constrDeclarationL([Type|Types]) -->
        constrDeclaration(Type), 
        (  `|`, constrDeclarationL(Types) 
         ; \+(`|`),{Types=[]} ).

constrDeclaration( const(ConstrID,N,TypeExprs) ) --> 
        dataConstrID( ConstrID ), 
        (  typeExprL(TypeExprs), !, {length(TypeExprs,N)}
         ; {N=0,TypeExprs=[]} ),
        {assert(constructor(ConstrID,N))}.

dataConstrID( TypeName ) --> id(ID), {sicstus_atom_chars(TypeName,ID)},
        {\+(type(TypeName,_)),\+(function(TypeName,_)),
         \+(constructor(TypeName,_))}.

typeExpr( TypeExpr ) --> 
        basictypeExpr(Type1),
        ( `->`, !,typeExpr(Type2), 
          {TypeExpr=curried(Type1,Type2)}
         ;{TypeExpr=Type1} ).

basictypeExpr( elem(type('List',1,[elem(type('Char',0,[]))])) ) --> 
        `String `,!.

basictypeExpr( TypeExpr ) --> 
        id(ID), {sicstus_atom_chars(Type,ID), type(Type,N)},!,
        ( {N=0},!, % Type with 0-arity
          {TypeExpr=elem(type(Type,0,[]))}
         ;{N>0},!, % Type with N-arity
          typeExprL(Vars), {length(Vars,N)},
          {TypeExpr=elem(type(Type,N,Vars))}  ).
basictypeExpr( elem(type('Tuples',0,[])) ) --> `()`.
basictypeExpr( Type ) --> 
        `(`,!, 
        typeExpr( Type1 ),
        ( `,`,!,   %--Tuple expression
          typeExprs(TypeExprs),`)`,
          {length([Type1|TypeExprs],N)},
          {Type=elem(type('Tuples',N,[Type1|TypeExprs]))}
         ;`)`,!,   %--Parenthesized type expression 
          ( {Type1=elem(_), !, Type=Type1} ; {Type=elem(Type1)} ) ).
basictypeExpr( elem(type('List',1,[TypeExpr])) ) --> `[`,!,typeExpr(TypeExpr),`]`.
basictypeExpr( elem(typevar(N)) ) --> variableAnt(N),!.
basictypeExpr( elem(typevar(N)) ) --> {ini},variableID(N),!.

% Forward expression
basictypeExpr( TypeExpr ) -->
        id(ID), {sicstus_atom_chars(Type,ID), \+(type(Type,_N))},!,
        ( typeExprL(Vars), !, {length(Vars,Num)},
          ({TypeExpr=elem(forward(Type,Num,Vars))}; {Num=0}),
          !, % Type with 0-arity
          {TypeExpr=elem(forward(Type,0,[]))} ).

typeExprL( [TypeExpr|TypeExprs] ) -->
        basictypeExpr(TypeExpr),
        (  typeExprL(TypeExprs),!
         ; {TypeExprs=[]} ).

typeExprs( [TypeExpr|TypeExprs] ) -->
        typeExpr(TypeExpr),
        (  `,`,typeExprs(TypeExprs),!
         ; {TypeExprs=[]} ).

% Functions -------------------------------------------------------------------
functionDeclaration(Decl) -->
         evalAnn(Decl),!
       ; functionType(Decl),!
       ; {cleanVars},equation(Dect,R),!,{sappend(Dect,R,Decl),clean_nomfun},`\n`.

% --- Function Evaluation Annotation
evalAnn([annotation(F,Type)]) --> 
        (  functionName(F)
         ; `(`,functionOp(F),`)`
         ; ````,functionOp(F),```` ), 
        `eval `,
        (  `flex `, {Type = flex},!
         ; `rigid `, {Type = rigid},!
         ; {write('Evaluation annotation can only be flex or rigid'),
            nl,fail} ),
        `\n`.

% --- Function Type Declaration
functionType([function(F,N,TypeFunction)]) --> 
        (  functionName(F)
         ; `(`,functionOp(F),`)`
        ; ````,functionOp(F),```` ), 
        `::`,!, 
        {assert(ini)},
        ( typeExpr(TypeFunction), {retract(ini)}
         ;{retract(ini)} ),
        {findArity(TypeFunction,0,N)},
        {( \+(function(F,N)),
           assert(function(F,N))
          ;function(F,N) )},
        `\n`.

findArity( curried(_,E), N, N2 ) :-
        N1 is N + 1,
        findArity(E,N1,N2).
findArity( elem(_), N, N ).


% --- Function Declaration
equation([ rule( f(F,N,P), Exp )], Nrule ) --> 
        %{cleanVars}, 

        (
           `data `,!,{fail} % control no data decl get as function
         ; functionName(F), pattern(P), !,{length(P,N), Infix=no} 
         ; functionName(F), \+(functionOp(_)), {N=0, P=[], Infix=no}
         ; constrTerm(Term1),functionOp(F),constrTerm(Term2),!,
           { N=2, P=[Term1,Term2], Infix = si } 
        ), 

        ( {\+(builtin(F))}
         ;{builtin(F), 
          format(`Parsing error, function ~a cannot be redefined\n`,[F]),fail}),

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
           `=`, expr(Expi)
         ; %Conditional Expression
           constExpr(Exps),
           {Expi=multiple(Exps)}
         ; %Error 
           getchars(Text),
           {format(`Syntactic error, incorrect expression <~a> found\n`,[Text]),
            retract(function(F,N)),fail} ),
        !,awhereL(F,N,P,Expi,Exp,Nrule).
           % `\n`.

functionDeclaration2(Decl) -->
          evalAnn(Decl),!
        ; functionType(Decl),!
        ; equation(Dect,R),!,{[C|_]=Dect,rule(f(F,N,_),_)=C},
          % functions with arity 0 aren't allowed inside where
          (({N>0,!,sappend(Dect,R,Decl)});
           {retract(function(F,_)),fail}).

%awhereL(F,N,P,Eent,Esa,Rest) --> 
%        ((`where `,!,`{`,awhereN(F,N,P,Eent,Esa,Rest));
%        ({Rest=[],Esa=Eent})).
awhereL(F,N,P,Eent,Esa,Rest) --> 
   ((`where `,!,`{`,awhereN(F,N,P,Eent,Esa,RestAux),
  {write('RestAux:'),write(RestAux),nl},
 {treatAllL(RestAux,Rest)},
  {write('Rest:'),write(Rest),nl}
   );
   ({Rest=[],Esa=Eent})).

awhereN(F,N,P,Eent,Esa,Rest) -->  awhere(F,N,P,Eent,Esa1,Rest1),
             ((`;`,!, awhereN(F,N,P,Esa1,Esa,Rest2),{sappend(Rest1,Rest2,Rest)});
	     (`}`,{Rest=Rest1,Esa=Esa1})).


awhere(F,N,P,Eent,Esa,Rest) -->
               ((variableIDL(_),`free `,!,{treatE3(Eent,Esa), Rest=[]}); 
               ({lastvar(Nv)},
                ((functionDeclaration2(R),
                 {treat4(R,Nv,Ls),remove_duplicates(Ls,Ln),app_par(R,Ro,Ln),
                 treatE6(Eent,Esa,Ln),lastvar(Nd),removeVars2(Nv,Nd),!,
                 Rest=Ro});
                %function declaration fails, remove the asserted variables
                ({lastvar(Nd),removeVars2(Nv,Nd),fail}))); 
               (pattern(R), `=`,expr(D),!, {
                make_proj(R,R,Rest0),treatE7(Eent,Esa0,D)},
                awhereL(F,N,P,Esa0,Esa,Rest1),{sappend(Rest0,Rest1,Rest)})).

% changes the name, arity and parametres of local functions (Definition). 
app_par([D|A], [F|A], Ln) :- reverse(Ln,Lrn),app_par2(D,F,Lrn).

app_par2( rule(L,R), rule(Ln,R), Lp) :- treat5(L,Ln,Lp).

treat5( f(F,N,P), f(Fn,Nn,Pn), Lp) :- treat25(N,P,Nn,Pn,Lp),(
			((Nn>N),!,retract(function(F,N)),assert(function(F,Nn)))
		 	;true),
		        ((nomfun(J),member((_,Fn,F),J),!);
     			 new_namefun(F,Fn),assert(function(Fn,Nn))).

% increments the arity with no local variables, and put them in the parameters
treat25(N,P,N,P,[]).  
treat25(N,P,Nn,Pn,[E|Er]) :- treat25(N,P,Na,Pa,Er),
                             Nn is Na+1,sappend(Pa,[E],Pn).

new_namefun(F,Fn) :-     retract(nomfun([(H1,W1,W2)|J])),
    	 		 H is H1 +1,name(H,Hi),
                         sappend([64|`fun`],Hi,Fn1),name(Fn,Fn1),
                         sappend([(H,Fn,F)],[(H1,W1,W2)|J],T),
			 assert(nomfun(T)).

% controls no incorrect forward node could be returned, 
% replaced by free variables 
treatE3( forward(F,_,Exprs), v(N) ) :-
        var_seen(F,N),!,
        treatEL3(Exprs,_).
treatE3( forward(F,N,Exprs), forward(F,N,ExprsX)) :-
        !,treatEL3(Exprs,ExprsX).
treatE3( c(C,N,Args), c(C,N,ArgsX) ) :- !,treatEL3(Args,ArgsX).
treatE3( f(F,N,Args), f(F,N,ArgsX) ) :- !,treatEL3(Args,ArgsX).
treatE3( multiple(Cts), multiple(CtsX) ) :- !,treatEL3(Cts,CtsX).
treatE3( X, X ).

treatEL3( [], [] ).
treatEL3( [X|Xs], [Y|Ys] ) :-
        treatE3(X,Y),
        treatEL3(Xs,Ys).

%introduces in a list the variables that  aren't parametres.   
treat4( [], _, []).
treat4( [X|Xs], N,L ) :- treat24(X,N,Le),treat4(Xs,N,Ls),sappend(Le,Ls,L).

treat24( rule(_,R), N,Ls ) :- !,
        treatE4(R,N,Ls).
treat24( _,_,_).

treatE4( forward(_,_,Exprs), N, L ) :-
        !, treatEL4(Exprs,N,L).

treatE4( c(_,_,Args),Nv,L ) :- !,treatEL4(Args,Nv,L).
treatE4( f(_,_,Args),Nv,L ) :- !,treatEL4(Args,Nv,L).
treatE4( multiple(Cts),Nv,L ) :- !,treatEL4(Cts,Nv,L).
treatE4( v(D), Nv, E) :- !,((D=<Nv,E=[v(D)],!);E=[]).
                              
treatE4( _, _, [] ).

treatEL4( [], _,[] ).
treatEL4( [X|Xs],N,L ) :-
        treatE4(X,N,D),
        treatEL4(Xs,N,Ls),(sappend(Ls,D,L),!);true.

% Changes the name, arity and parametres of the local functions 
treatE6( forward(F,_,Exprs), forward(Fn,N,Exprsl), L ) :-
        function(F,N),nomfun(J),\+(member((_,F,_),J)),!,
        treatEL6(Exprs,Exprsi, L), sappend(Exprsi,L,Exprsl),
        member((_,Fn,F),J).
treatE6( forward(F,N,Exp), forward(F,N,Expn) ,_) :-
        \+(function(F,_)),!,
         treatEL6(Exp,Expn,_).
treatE6( c(C,N,Args), c(C,N,ArgsX), L ) :- !,treatEL6(Args,ArgsX,L).
treatE6( f(F,_,Exprs), f(Fn,N,Exprsl), L ) :-
        function(F,N),nomfun(J),member((_,Fn,F),J),!,
        treatEL6(Exprs,Exprsi, L), sappend(Exprsi,L,Exprsl).
treatE6( f(F,N,Args), f(F,N,ArgsX), L ) :- !,treatEL6(Args,ArgsX,L).
treatE6( multiple(Cts), multiple(CtsX), L ) :- !,treatEL6(Cts,CtsX,L).
treatE6( X, X, _ ).

treatEL6( [], [],_ ).
treatEL6( [X|Xs], [Y|Ys], L ) :-
        treatE6(X,Y,L),
        treatEL6(Xs,Ys,L).

% translates right hand sides of rules avoiding locally declared functions
treatAll(rule(L,R),rule(L,R2)) :- treatE6(R,R2,[]).

treatAllL( [], []).
treatAllL( [X|Xs], [Y|Ys]) :-
        treatAll(X,Y),
        treatAllL(Xs,Ys).

% controls no incorrect forward node could be returned, 
% replaced by projections 
treatE7( forward(F,_,_), forward(Fn,1,[Es]),Es) :-
        nomfun(L),member((_,Fn,F),L),!.
treatE7( forward(F,J,Expr), forward(F,J,ExprX),Es ) :-!,treatEL7(Expr,ExprX,Es).
treatE7( c(C,N,Args), c(C,N,ArgsX), Es ) :- !,treatEL7(Args,ArgsX,Es).
treatE7( f(F,N,Args), f(F,N,ArgsX), Es ) :- !,treatEL7(Args,ArgsX,Es).
treatE7( multiple(Cts), multiple(CtsX), Es ) :- !,treatEL7(Cts,CtsX,Es).
treatE7( X, X , _).

treatEL7( [], [], _ ).
treatEL7( [X|Xs], [Y|Ys], Es ) :-
        treatE7(X,Y,Es),
        treatEL7(Xs,Ys,Es).

%clean the data base of functions    
clean_nomfun :- nomfun(J),clean_nomfun2(J),retract(nomfun([E|_])),
                  E=(N,_,_),E1=(N,upv,dsic),assert(nomfun([E1])).
clean_nomfun2([_|[]]).
clean_nomfun2([(_,_,F)|R]):- ((retract(function(F,_)),!);true), 
                              clean_nomfun2(R).
 

make_proj(_,[],[]):-!.
make_proj(R,[v(Nv)|L],Rs):-!,make_proj(R,L,R1),var_seen(Vv,Nv),
                    new_namefun(Vv,Fn),J=rule(f(Fn,1,R),v(Nv)),
                    assert(function(Fn,1)),sappend(R1,[J],Rs).

make_proj(R,[c(_,N,L1)|L2],Rs):-  (N=0,!,make_proj(R,L2,Rs));
                                 (make_proj(R,L1,R1),make_proj(R,L2,R2),
                                 sappend(R1,R2,Rs)).
%%--End of where managing

functionName(F) --> id(C), { name(F,C), \+(constructor(F,_)) }.
functionOp(F) --> opSim(C), {name(F,C)}.
functionOp(F) --> ````, id(C), ````, {name(F,C)}.

pattern([C|Cs]) -->
        constrTerm(C),
        (  pattern(Cs),!
         ; {Cs=[]} ).

% Parse a correct left hand side term
% (only constructors and non repeated variables)
constrTerm( Expr ) --> `'`,!,char(Expr),`'`.
constrTerm( Expr ) --> string(Expr),!.
constrTerm( c(Num,0,[]) ) --> number(Num),!.

constrTerm( v(Var) ) --> variableID(Var).
constrTerm( v(Var) ) --> variableVoid(Var).
constrTerm( v(Var) ) --> variableAnt(Var),
        {var_seen(F,Var),
         format(`Syntactic error, no left linear, repeated variable ~a\n`,[F]),
         fail}.

constrTerm( c('Tuple0',0,[]) ) --> `()`,!.
constrTerm( Expr ) --> % possibilities: tuple of cTerms of cTerm inside ()
        `(`,constrTermNested1(Expr1),!,
        (`,`,!,
         constrTermS( Exprs ),
         {length([Expr1|Exprs],N)},
         {arity2tuple(N,TupleName)},
         {Expr=c(TupleName,N,[Expr1|Exprs])}
        ;{Expr=Expr1} ),
        `)`.

constrTerm( c(F,N,[]) ) --> constructorID(F,N),!.
constrTermNested2( c(F,N,Terms) ) --> 
        constructorID(F,NF),{NF>0},!, constrTermL( Terms ),
        {length(Terms,N),N=<NF}.

constrTerm( c('Nil',0,[]) ) --> `[]`,!.
constrTermNested1( Expr ) -->
        constrTermNested2(Expr1),!,
        (`:`,!,
         constrTermCons( Exprs ),
         {Expr=c('Cons',2,[Expr1,Exprs])}
        ;{Expr=Expr1} ).
constrTerm( Expr ) --> 
        `[`,
        constrTerm_list(Expr), 
        `]`,!.

constrTerm_list( c('Cons',2,[Term,List]) ) --> % list of constTerms separated by ',' but inside []
        constrTerm(Term),
        (  `,`,!,constrTerm_list(List)
         ; {List = c('Nil',0,[])} ).

constrTermL( [Exp|Exps] ) --> % list of constrTerms separated by ' '
        constrTerm( Exp ), 
        (  constrTermL( Exps ),!
         ; {Exps=[]} ).

constrTermS( [Exp|Exps] ) --> % list of constTerms separated by ','
        constrTerm( Exp ), 
        (  `,`,constrTermS( Exps ),!
         ; {Exps=[]} ).

constrTermCons( Expr ) -->  % list of constTerms separated by ':'
        constrTerm( Exp ), 
        (  `:`,constrTermCons( Exps ),!,
           {Expr=c('Cons',2,[Exp,Exps])}
         ; {Expr=Exp} ).

% Should be the last possibilities
constrTermNested1(Expr) --> constrTermNested2(Expr).
constrTermNested2(Expr) --> constrTerm(Expr).

% Conditional rule (constrained expression)--------------------------
constExpr( [Exp|Exps] ) -->
         `|`, expr(ExpCond), `=`, expr( ExpR ),
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

% --- conditional expression (syntactic sugar)
expr( Expr ) --> 
        `if `, !,expr(ExpBool),
        `then `, expr(ExpT),
        `else `, expr(ExpF),
        {Expr=f('IfThenElse',3,[ExpBool,ExpT,ExpF])}.

% --- a extended expression OR 
%     nested operators (also constructor list ':')
expr( Expr, S1, S3 ) :-   % no DCG form (more efficient)
     eexpr( Expr1, S1, S2 ),
     (  %--catch all terms and operators and then find associative order
        ( functionInfixID(_,_,_,S2,_) 
         ;S2=[58|_]  ),!,       % ':'=58
        % List of expression is restart from S1
        exprInfixL( Exprs, S1, S3 ),!,
        associative( Exprs, Expr ) % Find correct order between operators
      ; %--simple extended expression
        Expr=Expr1, S3=S2
     ),!.
     
% Get associative for operator
functionInfixID( F, Prec, Asso ) --> 
        (````;[26,96]),
        id(ID),
        (````;[26,96]),!,
        {sicstus_atom_chars(F,ID), infix(F,Prec,Asso)}.
functionInfixID( F, Prec, Asso ) --> opSim(ID),!,
        {sicstus_atom_chars(F,ID), infix(F,Prec,Asso)}.

exprInfixL( Resul ) --> 
        eexpr( Expr1 ),
        % - Find operator
        (
           functionInfixID(F,Prec,Asso),!,
           {Resul=[Expr1,op(F,Prec,Asso)|ExprL]} 
        ; `:`,!,
           {infix(':',Prec,Asso)},
           {Resul=[Expr1,op(':',Prec,Asso)|ExprL]} % Same precedence as ++
        ),
        % - Find more expressions
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
getExpr( [Expr], Expr, Inf ) :- current_prolog_flag(max_tagged_integer,Inf), !.  % inf is the greatest integer of SICStus
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

% --- Negative expressions 
% Sections (-x) are avoided. However, sections (x-) not.
eexpr( f('-',2,[c(0,0,[]),Expr]) ) -->
       `-`,eexpr(Expr),!. 

% --- constructors N-arity (N>0)
eexpr( c(F,N,Terms) ) --> 
        constructorID(F,NF), {NF>0}, exprL( Terms ),!,
        {length(Terms,N),N=<NF}.

% --- functions N-arity (N>1)
eexpr( f(F,N,Terms) ) --> 
        functionID(F,N), {N>0}, exprL( Terms ), !,
        {length(Terms,N1), N1 =< N}.

% --- functions not seen N-arity (N>1)
eexpr( forward(F,N,Terms) ) --> {\+(ini)},   % function not passed yet
        id(ID), 
        {name(F,ID)}, 
        {\+(function(F,_)), \+(constructor(F,_)), \+(var_seen(F,_))},
        exprL(Terms), !,
        {length(Terms,N)}.

% --- Special Case for sections (operator with only 2nd arg)
%     translated to `\x -> x op e`
eexpr( Expr ) -->
      functionInfixID(F,_,_), expr(Expr2),!,
      {inc_var(NVar)},
      {Expr=f('\\',3,[ c('\\Vars',1,[v(NVar)]), f(F,2,[v(NVar),Expr2]) ])}.

% --- Lambda Abstraction
%     Standard form `\ x,y,z -> expr`
%     Arguments to lambda function are added at list.
%     Number of expected arguments are increases by two to include
%          pattern expression and expression
eexpr( f('\\',N,[Vars,Expr]) ) -->
       `\\`,!,             % we are in a lambda abstraction
       {lastvar(LVar1)},   % first know the current last variable 
       pattern(Pattern),   % get pattern 
       `->`,               % check literal
       expr(Expr),         % get result expression
       !,
       % now we have to remove all new variables created in pattern
        {lastvar(LVar2),removeVars(LVar1,LVar2)},
       % build final expression
        {length(Pattern,Np), N is Np+2,  Vars=c('\\Vars',Np,Pattern) }.

% --- a basic expression OR partial application
eexpr( Expr, S1, S3 ) :-   % no DCG form (more efficient)
     bexpr( Expr1, S1, S2 ),
     (%--partial application
      (
       Expr1=v(_)      % a variable that will be instantiated
      ;Expr1=f(_,_,_)  % a expression that will become a function
      ),
      exprL(Terms,S2,S3),!,
      sappend([Expr1],Terms,Args), length(Args,N),
      Expr = f('\\@',N,Args)
     ;%--simple basic expression
      Expr=Expr1, S3=S2
     ),!.
     
% List of basic expressions
exprL( [Exp|Exps] ) --> 
        bexpr( Exp ),!, 
        (  exprL( Exps )
         ; {Exps=[]} ).

% Ensure same state before lambda process
removeVars(V1,V1).
removeVars(V1,V2) :- V2 > V1,
     ( retract(var_seen(_,V2))
      ;\+(var_seen(_,V2))      ),
     V is V2 - 1,
     removeVars(V1,V).

% Ensure same state before the equation failed
removeVars2(V1,V1):- retract(lastvar(_)),assert(lastvar(V1)).
removeVars2(V1,V2) :- V2 > V1,
     retract(var_seen(_,V2)),
     V is V2 - 1,
     removeVars2(V1,V).

% --------------------
% --- Basic Expression
% --------------------

% - vars
bexpr( v(Var) ) --> variableAnt(Var).       % seen variable
bexpr( v(Var) ) --> variableNew(Var).       % new free variable (+underscore)
bexpr( v(Var) ) --> {ini}, variableID(Var). % new free variable (-underscore)
bexpr( v(Var) ) --> variableVoid(Var).      % void free variable

% - character
bexpr( Exp ) --> `'`,char(Exp),`'`,!.

% - string
bexpr( ExpStr ) --> string(ExpStr),!.

% - number (integer)
bexpr( c(Num,0,[]) ) --> number(Num),!.

% - lists
bexpr( c('Nil',0,[]) ) --> 
        `[]`,!.
bexpr( Expr ) --> 
        `[`,!, expr_list(Expr), `]`.

expr_list( c('Cons',2,[Term,List]) ) --> 
        expr(Term),
        ( `,`,!, expr_list(List)
         ;{List = c('Nil',0,[])} ),!.

% - empty tuple
bexpr( c('Tuple0',0,[]) ) --> `()`,!.

% - operator without args
% should be together with TUPLES and parenthesized exp
% to join cases in the same functor
bexpr( f(F,2,[]) ) -->
        `(`,functionInfixID(F,_,_),`)`,!,
        {function(F,2)}.

bexpr( forward(F,2,[]) ) -->
        `(`,functionInfixID(F,_,_),`)`,!,
        {\+(function(F,2))}.

% - tuples OR expression into parenthesis
bexpr( Expr ) --> 
     `(`,
     expr( Expr1 ),
     (  %--tuples (first exp parsed)
        `,`,!,                    
        expr_tuples( Exprs ),
        {length([Expr1|Exprs],N)},
        {arity2tuple(N,TupleName)},
        {Expr=c(TupleName,N,[Expr1|Exprs])}
      ; %--expr between parenthesis
        {Expr=Expr1}               
     ),
     `)`,!.

expr_tuples( [Exp|Exps] ) -->
     expr( Exp ),
     ( `,`,!,expr_tuples(Exps)
      ;{Exps=[]} ).

% - constructor 0-arity
bexpr( c(F,0,[]) ) --> constructorID(F,0),!.

% - function 0-arity
bexpr( f(F,0,[]) ) --> functionID(F,0),!.

% --- function call without args
bexpr( f(F,N,[]) ) -->
        functionID(F,N),!.
bexpr( forward(F,0,[]) ) --> {\+(ini)},             % function still not seen
        id(ID), 
        {name(F,ID)}, 
        {\+(function(F,_)), \+(constructor(F,_)), \+(var_seen(F,_))}.

%---- Special ----------
exprIni( Termino ) --> 
        {retractall(ini),assert(ini)}, 
        expr(Termino),
        {retractall(ini)}.

% IDENTIFIERS ----------------------------------------------------
% new variable (without underscore)
variableID( N ) --> id(ID), 
        {sicstus_atom_chars(F,ID), \+(var_seen(F,_)), 
         var_int(F,N),
         ( \+(constructor(F,_)),
           \+(function(F,_)),
           \+(type(F,_)),
           \+(builtin(F))
          ;del_var(N),fail )}.

% new free variable with underscore
variableNew( N ) --> 
        `_`,id(IDx),{ID=[95|IDx],
        sicstus_atom_chars(F,ID), \+(var_seen(F,_)), 
        var_int(F,N),
         ( \+(constructor(F,_)),
           \+(function(F,_)),
           \+(type(F,_)),
           \+(builtin(F))
         ;del_var(N),fail )}.

% previous seen variable
variableAnt( N ) --> 
        ( `_`,id(IDx),{ID=[95|IDx]}
         ;id(ID) ),
        { sicstus_atom_chars(F,ID), var_seen(F,N) }.

% void variable (only underscore)
variableVoid( N ) --> 
        `_ `,
        {inc_var(N),
         ( assert(var_seen('_',N)) 
          ;del_var(N) )}.

% list of variables
variableIDL( [N|Vars] ) -->
        variableID(N),
        (  `,`,!,variableIDL(Vars)
         ; {Vars=[]} ).

functionID( F, N ) --> id(ID), {sicstus_atom_chars(F,ID), function(F,N)}.
constructorID( C, N ) --> id(ID), {sicstus_atom_chars(C,ID), constructor(C,N)}.

number( Num ) --> 
        numberS(SNum),
        {number_chars(Num,SNum)}.

numberS( [C|Cs] ) --> [C], 
        { C >= `0`, C =< `9` }, 
        numberS( Cs ).
numberS( [] ) --> ` `.

id([C|Cs]) --> [C],
        { C >= `A`, C =< `Z`
         ;C >= `a`, C =< `z` },!,
        restoid(Cs),!,
        ( {[C|Cs]==`data`,!,fail} %this is not a valid id
         ;{[C|Cs]==`eval`,!,fail} %this is not a valid id
         ;{[C|Cs]==`flex`,!,fail} %this is not a valid id
         ;{[C|Cs]==`rigid`,!,fail} %this is not a valid id
         ;{[C|Cs]==`choice`,!,fail} %this is not a valid id
         ;{[C|Cs]==`let`,!,fail} %this is not a valid id
         ;{[C|Cs]==`in`,!,fail} %this is not a valid id
         ;{[C|Cs]==`if`,!,fail} %this is not a valid id
         ;{[C|Cs]==`then`,!,fail} %this is not a valid id
         ;{[C|Cs]==`else`,!,fail} %this is not a valid id
         ;{[C|Cs]==`where`,!,fail} %this is not a valid id
         ;{[C|Cs]==`free`,!,fail} %this is not a valid id
         ;{[C|Cs]==`infix`,!,fail} %this is not a valid id
         ;{[C|Cs]==`infixl`,!,fail} %this is not a valid id
         ;{[C|Cs]==`infixr`,!,fail} %this is not a valid id
         ;{[C|Cs]==`pragma`,!,fail} %this is not a valid id
         ;{[C|Cs]==`optmatch`,!,fail} %this is not a valid id
         ;{true} ).

restoid([C|Cs]) --> [C],
        { C >= `0`, C =< `9`
         ;C >= `A`, C =< `Z`
         ;C >= `a`, C =< `z`
         ;[C] == `_` },!,
        restoid(Cs).
restoid([]) --> ` `.

opSim( Cs ) --> opSim2(Cs1),
        {( Cs1 = `=>`,!, Cs = `>=`
          ;Cs1 = `<=`,!, Cs = `=<`
          ;Cs1 = `=`,!,fail %this is not a valid op
          ;Cs1 = `|`,!,fail %this is not a valid op
          ;Cs = Cs1 )}.

opSim2([C|Cs]) --> sym([C]), opSim2(Cs).
opSim2([C]) --> sym([C]).

sym(`~`) --> `~`.
sym(`!`) --> `!`.
sym(`@`) --> `@`.
sym(`#`) --> `#`.
sym(`$`) --> `$`.
sym(`%`) --> `%`.
sym(`^`) --> `^`.
sym(`&`) --> `&`.
sym(`*`) --> `*`.
sym(`+`) --> `+`.
sym(`-`) --> `-`.
sym(`=`) --> `=`.
sym(`<`) --> `<`.
sym(`>`) --> `>`.
sym(`?`) --> `?`.
sym(`.`) --> `.`.
sym(`/`) --> `/`.
sym(`|`) --> `|`.
sym(`:`) --> `:`.

string( Expr ) --> [34],stringS(Expr).
stringS( c('Nil',0,[]) ) --> [34],!.
stringS( c('Cons',2,[Ch,Expr]) ) --> % List of chars
        char(Ch),stringS(Expr).

char( c([7],0,[]) )   --> `\\a`.
char( c([8],0,[]) )   --> `\\b`.
char( c([9],0,[]) )   --> `\\t`.
char( c([10],0,[]) )  --> `\\n`.
char( c([11],0,[]) )  --> `\\v`.
char( c([12],0,[]) )  --> `\\f`.
char( c([13],0,[]) )  --> `\\r`.
char( c([27],0,[]) )  --> `\\e`.
char( c([127],0,[]) ) --> `\\d`.
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

