:- expects_dialect(sicstus).
:- set_prolog_flag(double_quotes, codes).
:- style_check(-discontiguous).
:- discontiguous gt/5.
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ------------------------------------------------------------------------
                     Generate Definitional Trees
  ------------------------------------------------------------------------
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

:- dynamic dt/3.

%-Possible values
%strategy(leftright).
%strategy(optmatch).

%-Possible values
%annotationAll(flex).
%annotationAll(rigid).

% deftrees for primitive functions according to Curry report
basicDefTrees :-
   assert( dt('IfThenElse', 3,
           tbranch( [1],rigid,[
                    ('True',
                     trule( f('IfThenElse',3,[ c('True',0,[]), v(2), v(3) ]), 
                           v(2) )),
                    ('False',
                     trule( f('IfThenElse',3,[ c('False',0,[]), v(2), v(3) ]), 
                           v(3) ))
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

% ------- branch node ( all constructors )
gt( f(F,N,Terms), M, Strategy, Rules, Mode ) :- 
        dp( f(F,N,Terms), Rules, [P|Positions] ),
        ( Strategy = optmatch,
          find_opt( [P|Positions], Rules, O )
         ;O=P ),
        split( Rules, O, Rules, [] ),!,
        groups( Rules, O, Grouped ),
        Mode = tbranch( O, M, SubTrees ),
        subtrees( Grouped, f(F,N,Terms), O, M, Strategy, SubTrees ).

find_opt( [V|_], Rules, V ) :-
        split( Rules, V, Rules, [] ).
find_opt( [V|Vs], Rules, O ) :-
        \+(split( Rules, V, Rules, [] )),
        find_opt( Vs, Rules, O ).        

subtrees( [], _, _, _, _, [] ).
subtrees( [R|Rs], Term, O, M, Strategy, [(Const,SubTree)|SubTrees] ) :- 
        R = [rule( L, _ )|_], pos( L, O, c(Const,N,_) ),
        maxvar( Term, Max ),
        newvars( N, Max, Vars ),
        part( Term, O, c(Const,N,Vars), TermS ),
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
        split( Rules, O, CRules, VRules ), CRules \== [], VRules \== [],!,
        Mode = tor( CMode, VMode ),
        gt( f(F,N,Terms), M, Strategy, CRules, CMode ),
        gt( f(F,N,Terms), M, Strategy, VRules, VMode ).

% -------- rule node
gt( f(F,N,Terms), _, _, Rules, DTree ) :- !,
        dp( f(F,N,Terms), Rules, [] ),
        join_rules( Rules, DTree ).       % for non orthogonal systems

join_rules( [rule(L,R)], trule(L,R) ).
join_rules( [rule(L,R),Rule2|Rules], tor( trule(L,R), DTree ) ) :-
        join_rules( [Rule2|Rules], DTree ).

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

