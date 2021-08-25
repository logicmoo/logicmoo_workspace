:- set_prolog_flag(double_quotes, codes).
:- style_check(-discontiguous).
:- discontiguous show/2.
:- discontiguous write2/1.
:- discontiguous write2/1.
:- discontiguous write2/1.
:- discontiguous write2/1.
:- expects_dialect(sicstus).

% ---------------------------------------------------------------------------
% Modified Write ------------------------------------------------------------
% ---------------------------------------------------------------------------

% EXPRESSION ---
write2( X ) :- show( X, Str ),name(Output,Str),write(Output).

% Characters 
show( c([7],0,[]), Str ) :- !,sappend([39],"\\a",S1),sappend(S1,[39],Str).
show( c([8],0,[]), Str ) :- !,sappend([39],"\\b",S1),sappend(S1,[39],Str).
show( c([9],0,[]), Str ) :- !,sappend([39],"\\t",S1),sappend(S1,[39],Str).
show( c([10],0,[]), Str ) :- !,sappend([39],"\\n",S1),sappend(S1,[39],Str).
show( c([11],0,[]), Str ) :- !,sappend([39],"\\v",S1),sappend(S1,[39],Str).
show( c([12],0,[]), Str ) :- !,sappend([39],"\\f",S1),sappend(S1,[39],Str).
show( c([13],0,[]), Str ) :- !,sappend([39],"\\r",S1),sappend(S1,[39],Str).
show( c([27],0,[]), Str ) :- !,sappend([39],"\\e",S1),sappend(S1,[39],Str).
show( c([127],0,[]), Str ) :- !,sappend([39],"\\d",S1),sappend(S1,[39],Str).
show( c([C],0,[]), Str ) :- !,sappend([39],[C],S1),sappend(S1,[39],Str).

% Lists
show( List, Str ) :- check_string(List),!,
        show_string(List,StrL),
        sappend([34],StrL,S1), sappend(S1,[34],Str).
show( c('Nil',0,[]), Str ) :- !, Str="[]".
show( c('Cons',2,[Term,List]), Str ) :-  
        checkGround(c('Cons',2,[Term,List])), !,
        show(Term,StrTerm),
        show_list(List,StrList),
        sappend("[",StrTerm,S1), sappend(S1,StrList,S2), sappend(S2,"]",Str).
show( c('Cons',2,[Term,List]), Str ) :-  !,
        show(Term,StrTerm), show(List,StrList),
        sappend("(",StrTerm,S1), sappend(S1,":",S2), 
        sappend(S2,StrList,S3), sappend(S3,")",Str).

check_string( c('Cons',2,[ c([_],0,[]), c('Nil',0,[]) ]) ).
check_string( c('Cons',2,[ c([_],0,[]), Str ]) ) :- 
        check_string(Str).

show_string( c('Nil',0,[]), [] ).
show_string( c('Cons',2,[ Char, List ]), Str ) :- 
        show(Char,StrChar), 
        ( StrChar=[39,C,39],     Ch=[C]
         ;StrChar=[39,C1,C2,39], Ch=[C1,C2] ),
        sappend(Ch,StrList,Str),
        !,show_string(List,StrList).

checkGround( c('Nil',0,[]) ).
checkGround( c('Cons',2,[_,List]) ) :- checkGround(List).

show_list( c('Nil',0,[]), [] ).
show_list( c('Cons',2,[Term,List]), Str ) :- 
        show(Term,StrTerm),
        sappend(",",StrTerm,S1),
        sappend(S1,StrList,Str),
        !,show_list(List,StrList).

% Tuples
show( c(TupleName,N,Exprs), Str ) :-
        tuple2arity(TupleName,N),!,length(Exprs,N),
        showS(Exprs,StrExprs),
        sappend("(",StrExprs,S1), sappend(S1,")",Str).

% Constructors
show( c(F,0,[]), Str ) :- !, name(F,Str).
show( c(F,_,Terms), Str ) :- 
        name(F,StrF), showL(Terms,StrTerms),
        sappend("(",StrF,S1), sappend(S1," ",S2), 
        sappend(S2,StrTerms,S3), sappend(S3,")",Str).

% Special Functions
show( f('\\@',_,[f(F,N,Args)|Terms]), Str ) :- !,
        show(f(F,N,Args),StrF), showL(Terms,StrTerms),
        sappend(StrF," \\@ ",Str2), sappend(Str2,StrTerms,Str).
        

show( f('IfThenElse',3,[EBool,ETrue,EFalse]), Str ) :- !,
        show(EBool,StrBool), show(ETrue,StrTrue), show(EFalse,StrFalse),
        sappend("if ",StrBool,S1), 
        sappend(S1," then ",S2), sappend(S2,StrTrue,S3),
        sappend(S3," else ",S4), sappend(S4,StrFalse,Str).

show( f('\\',_,[c('\\Vars',_,Pattern),Expr|Exprs] ), Str ) :- !,
        showL(Pattern,StrPattern), show(Expr,StrExpr), showL(Exprs,StrExprs),
        sappend("(\\ ",StrPattern,S1), sappend(S1," -> ",S2),
        sappend(S2,StrExpr,S3), sappend(S3,") ",S4),
        sappend(S4,StrExprs,Str).

show( f('\\=>',_,[Constraint,Expr]), Str ) :- !,
        show(Constraint,StrConstraint), show(Expr,StrExpr),
        sappend(StrConstraint," \\=> ",S1), sappend(S1,StrExpr,Str).

% Infix Functions
show( f(F,2,[Term1,Term2]), Str ) :- infix(F,_,_),!, 
        show(Term1,StrTerm1), show(Term2,StrTerm2), name(F,StrF),
        sappend("(",StrTerm1,S1), sappend(S1," ",S2), 
        sappend(S2,StrF,S3), sappend(S3," ",S4), 
        sappend(S4,StrTerm2,S5), sappend(S5,")",Str).
show( f(F,2,[Term1]), Str ) :- infix(F,_,_), !, 
        show(Term1,StrTerm1), name(F,StrF),
        sappend("(",StrTerm1,S1), sappend(S1," ",S2), 
        sappend(S2,StrF,S3), sappend(S3,")",Str).
show( f(F,2,[]), Str ) :- infix(F,_,_), !, 
        name(F,StrF),
        sappend("(",StrF,S1), sappend(S1,")",Str).

% Functions
show( f(F,_,Terms), Str ) :- 
        showL(Terms,StrTerms), name(F,StrF),
        sappend("(",StrF,S1), sappend(S1," ",S2),
        sappend(S2,StrTerms,S3), sappend(S3,")",Str).
show( forward(F,_,Terms), Str ) :- 
        showL(Terms,StrTerms), name(F,StrF),
        sappend("(",StrF,S1), sappend(S1," ",S2),
        sappend(S2,StrTerms,S3), sappend(S3,")",Str).

% Variables
show( v(N), Str ) :- var_seen(Name,N), !, name(Name,Str).
show( v(N), Str ) :- name(N,StrN), sappend("var",StrN,Str).

% List of expressions
showL( [], "" ).
showL( [X], Str ) :- show(X,Str).
showL( [X,Y|Xs], Str ) :- 
        show(X,StrX),
        sappend(StrX," ",S1), sappend(S1,StrRest,Str),
        !,showL([Y|Xs],StrRest).

showS( [], "" ).
showS( [X], Str ) :- show(X,Str).
showS( [X,Y|Xs], Str ) :- 
        show(X,StrX),
        sappend(StrX,", ",S1), sappend(S1,StrRest,Str),
        !,showS([Y|Xs],StrRest).

% STATES ----
% State
write2( er([],f('success',0,[]),C) ) :- !,
        write('[ ]'),
        write2Inc(C).

write2( er([],Exp,C) ) :- !,
        write('[ '),
        write2(Exp), 
        write(' ]'),
        write2Inc(C).

write2( er(Susts,f('success',0,[]),C) ) :- !,
        write('[ '),
        write('{'), write2CTL(Susts), write('}'),
        write(' ]'),
        write2Inc(C).

write2( er(Susts,Exp,C) ) :- 
        write('[ '),
        write('{'), write2CTL(Susts), write('} '), 
        write2(Exp),
        write(' ]'),
        write2Inc(C).

% Show incremental info of states in screen
write2Inc(_).
/*
write2Inc(C) :- write('Visited:'),!,write2Inc2(C).
write2Inc2([]).
write2Inc2([(Ox,_)]) :-
        write2Inc3(Ox).
write2Inc2([(Ox,_),X|C]) :-
        write2Inc3(Ox),write(','),
        !,write2Inc2([X|C]).

write2Inc3( [] ) :- !,write('[]').
write2Inc3( X ) :- write2Ox(X).
*/

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
write2( trule(L,R) ) :- write2_dt(trule(L,R),0).
write2( tbranch(O,M,Dts) ) :- write2_dt(tbranch(O,M,Dts),0).
write2( tor(L,R) ) :- write2_dt(tor(L,R),0).

write2_dt( trule(L,f('\\=>',2,[Cond,Expr])), Spaces ) :- !,tab(Spaces),
        write('rule('),
        write2(L),write(' | '), write2(Cond),write(' = '),write2(Expr),
        write(')'),nl.
write2_dt( trule(L,R), Spaces ) :- tab(Spaces),
        write('rule('),
        write2(L), write(' = '), write2(R), write(')'),nl.
write2_dt( tbranch(O,M,Dts), Spaces ) :- tab(Spaces), Spaces1 is Spaces + 1,
        write('branch('),
        write2Ox(O),write(','),write(M),write(') ->'),nl,
        write2_dtL( Dts, Spaces1 ).
write2_dt( tor(L,R), Spaces ) :- tab(Spaces), Spaces1 is Spaces + 1,
        write('or ->'), nl,
        write2_dt( L, Spaces1 ),
        write2_dt( R, Spaces1 ).
                
write2_dtL( [], _ ).
write2_dtL( [(F,X)|Xs], Spaces ) :- 
        Space1 is Spaces - 1, tab(Space1),
        write(F),write(' >'),nl,
        write2_dt(X, Spaces), write2_dtL(Xs,Spaces).

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

