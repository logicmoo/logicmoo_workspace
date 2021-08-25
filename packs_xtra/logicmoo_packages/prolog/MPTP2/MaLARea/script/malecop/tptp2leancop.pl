%% File: tptp2leancop.pl  -  Version: 1.33  -  Date: 5 March 2012
%%
%% Purpose: The transformation of TPTP files together to DNF files for a further usage.
%%
%% Authors: Jiri Vyskocil
%%
%% Usage: tptp2leancop(Ls,S).% transforms Ls as list of files into DNF files with
%%                           % settings S. Result files have suffix ".leancop"
%%
%% Settings is list with:
%% fast - unique ids in all axioms in all problems are needed!!! Speed up ~2x 
%% no_equal_axioms
%% dumpsyms(DumpFile)
%% dumpterms(DumpFile1)
%% dumpdnf2axioms(DumpFile2)
%% or MaleCop transformation settings... 
   
:- dynamic(dnf/5).
:- dynamic(index_2_axioms/2).
:- dynamic(skolem/3).
:- dynamic(matrix_database/2).
:- dynamic(hash2nr/2). % contiguous mapping of term hashes

/*
append([],[]).
append([Ls],Ls).
append([Ls|As],Rs) :-
  append(As,Ts),
  append(Ls,Ts,Rs).
*/

:-[leancop_main].

:- op(100,fy,-). % due to problems with -3^[] as (-3)^[] instead of -(3^[])

%%%

tptp2leancop(Files,Settings) :-
    retractall(dnf(_,_,_,_,_)),
    retractall(index_2_axioms(_,_)),
    retractall(skolem(_,_,_)),
    retractall(matrix_database(_,_)),
    retractall(hash2nr(_,_)),
    asserta(skolem(invalid,skolem_constant,1)),
    flag(problem_number,_,1),
    flag(index_of_dnf,_,1),
    flag(index_of_hash,_,1),
    tptp2leancop_main(Files,Settings),
    dump_global_info(Settings).
%%%

dump_global_info(Settings):-
	( member(dumpsyms(DumpFile),Settings) -> dump_dnf_symbols(DumpFile); true ),
	( member(dumpterms(DumpFile1),Settings) -> dump_dnf_termhashs(DumpFile1); true ), 
	( member(dumpdnf2axioms(DumpFile2),Settings) -> dump_dnf2axioms(DumpFile2); true ). 

tptp2leancop_main(Files,Settings):-
	   member(File,Files),
	   tell(user_error),
	   write('% '),
	   write(File),
	   write('...'),
	   told,
	   tptp2leancop_database(File,Settings,R),
	   tell(user_error),
	   writeln(done),
	   ( name(File,Ns),append(Ns,".leancop",Fs),name(NewFile,Fs),
	     save_leancop(NewFile,R) -> true
	    ),
	   told,
	   fail.



dump_dnf2axioms(DumpFile) :-
	tell(DumpFile),
	((dnf(_HC1,_C1,_G,Index,_),
	  findall(I,index_2_axioms(Index,I),Ls),
	  sort(Ls,IDs),
	  print(contents(Index,IDs)),
	  write('.'),nl,
	  fail);
	 told).

dump_dnf_symbols(DumpFile) :-
	tell(DumpFile),
%	logic_syms(LogicSyms),
	((dnf(_HC1,C1,_G,Index,_),
	  renvars(C1,C2),
	  collect_symbols_top(C2,PredSyms,Syms),
%	  subtract(Syms1, LogicSyms, Syms),
	  print(symbols(Index,PredSyms,Syms)),
	  write('.'),nl,
	  fail);
	 told).


%%%

logic_syms([-,'^',~,'#','.',[]]).

%%% collect nonvar symbols from term

collect_symbols_top(Xs,Ps,Ls):-
        maplist(collect_predicate_symbols,Xs,Qs,LRs),
        sort(Qs,Ps),
        append(LRs,Rs),
	maplist(collect_symbols,Rs,L1),!,
	append(L1,L2),
	flatten(L2,L3),
	sort(L3,Ls).

collect_predicate_symbols(-X,P/N,As) :- X=..[P|As], functor(X,_,N).
collect_predicate_symbols(X,P/N,As)  :- X=..[P|As], functor(X,_,N).

collect_predicate_symbols([],[]).

collect_symbols(X,[]):- var(X),!.
collect_symbols(X,[X/0]):- atomic(X),!.
collect_symbols(X1,T2):-
	X1 =.. [H1|T1], functor(X1,_,N),
	maplist(collect_symbols,T1,T3),
	append(T3,T4),
	flatten(T4,T5),
	sort([H1/N|T5],T2).

%%%

dump_dnf_termhashs(DumpFile) :-
	tell(DumpFile),
	((dnf(_HC1,C1,_G,Index,_),
	  renvars(C1,C2),
	  collect_termhashs_top(C2,L,R),
	  print(terms(Index,L,R)),
	  write('.'),nl,
	  fail);
	 told).

%%%



% logic_syms(['-'/1/1,'^'/2/1,'#'/0/1,'.'/2/1,[]/0/1,(~)/1/1]).

%%% collect nonvar symbols from term

collect_symbols_top(X,L):-
	collect_symbols(X,L1),!,
	flatten(L1,L2),
	sort(L2,L).
collect_symbols(X,[]):- var(X),!.
collect_symbols(X,[X/0/1]):- atomic(X),!.
collect_symbols(X1,T2):-
	X1 =.. [H1|T1],length(T1,L),
	maplist(collect_symbols,T1,T3),
	flatten(T3,T4),
	sort([H1/L/1|T4],T2).


%%% collect term hashes from term, first result with distinct vars,
%%% and second with giving all variables the same value.  Grounding is
%%% done by numerals starting at 1000000 - we do not want additional
%%% '$VAR' functor - this makes the term list longer.

collect_termhashs_top(X,L,R):-
	numbervars_distinct(X,X1),
	numbervars_same(X,X2),
	collect_termhashs(X1,L1),
	collect_termhashs(X2,R1),!,
	flatten(L1,L2),
	flatten(R1,R2),
	sort(R2,R),
	sort(L2,L).
collect_termhashs(X,[C]):- atomic(X),!, my_term_hash(X,C).
collect_termhashs(X1,T2):-
	X1 =.. [H1|T1],
	my_term_hash(X1,C),
	maplist(collect_termhashs,T1,T3),
	flatten(T3,T4),
	sort([C|T4],T2).

my_term_hash(X1,C):-
	term_hash(X1,C0),
	(hash2nr(C0,C) -> true;  flag(index_of_hash,C,C+1), assert(hash2nr(C0,C))).


%%% 
numbervars_same(X,Out) :-
	copy_term(X,Out),
	term_variables(Out,Vars),
	ground_same(Vars).

ground_same([]).
ground_same([1000000|T]):- ground_same(T).

ground_distinct([],_).
ground_distinct([I|T],I):- J is I + 1, ground_distinct(T,J).


numbervars_distinct(X,Out) :-
	copy_term(X,Out),
	term_variables(Out,Vars),
	ground_distinct(Vars,1000001).

%%%%%%

save_leancop(File,List) :-
    open(File,write,Stream), 
    ( forall(member(L,List),(copy_term(L,C), numbervars(C,0,_), print(Stream,C),write(Stream,'.'),nl(Stream)))
    -> close(Stream) ; close(Stream), fail )
    .
    
%%%

get_unique_name_of_problem(NAME) :-
    flag(problem_number,PRN,PRN+1),
    name(PRN,PRN_NAME),name(leancop_problem_,PROBLEM_NAME),
    append(PROBLEM_NAME,PRN_NAME,R_NAME),
    name(NAME,R_NAME).

%%%
 prepare_tptp(File,Ns,Fs,Conjecture) :-
    axiom_path(AxPath), 
    ( AxPath='' -> 
        AxDir='' 
      ; name(AxPath,AxL), append(AxL,[47],DirL), name(AxDir,DirL) 
    ),
    leancop_tptp2_(File,AxDir,[_],Ns,Fs,Conj),
%   writeln(Q),
%    writeln(Fs),
%    ( Conj\=[] -> Problem1=Problem ; Problem1=(~Problem) ),
    ( Conj=[] -> Conjecture=(#) ; Conjecture=Conj ).
%    writeln(ok-Conj).
%%%
echo(M) :- tell(user_error),write(M),told.
%%%

tptp2leancop_database(File,Settings,Result) :-
    prepare_tptp(File,Ns,Fs,Conjecture),
    (
     member(no_equal_axioms,Settings) ->
       Es=[]
     ;
       leancop_equal_axioms([Conjecture|Fs],Es)
    ),
    length(Es,L),length(Qs,L),
    append(Fs,Es,As),append(Ns,Qs,Ds),
    append_axioms_to_matrix(As,Ds,Matrix,IDs,Settings),
%    open(buf,append,STR),write_canonical(STR,Matrix),close(STR),
    make_matrix((Conjecture,#),Conj_Matrix,/*Settings*/[def]),
    length(Conj_Matrix,K),length(Vs,K),
    get_unique_name_of_problem(PROBLEM),
%    echo('now '),
    assert_dnf(Conj_Matrix,Vs,PROBLEM,conj,R1),
%    echo('done1 '),
    assert_dnf(Matrix,IDs,PROBLEM,axiom,R2),
%    writeln(Vs),nl,writeln(IDs),
    append(R1,R2,Result),
    !.
/*
%    Problem=Problem1,
    writeln(Problem1=Conj),
    leancop_equal(Problem1,Problem2),
    writeln(Problem2),
    make_matrix(Problem2,Matrix,Settings),
    writeln(Matrix=Settings),
    (member([-(#)],Matrix) -> S=conj ; S=pos),
%    make_matrix(Problem1,Matrix,Settings),
    assert_dnf(Matrix,S,Result),!
    ,writeln(Result).
*/
    /* %%%%
    ( prove2(Matrix,Settings,Proof) ->
      ( Conj\=[] -> Result='Theorem' ; Result='Unsatisfiable' ) ;
      ( Conj\=[] -> Result='Non-Theorem' ; Result='Satisfiable' )
    ),
    output_result(File,Matrix,Proof,Result,Conj).
*/
%%% renames # to #(PROBLEM) in Result
rename_sharp([],PROBLEM,[]).
rename_sharp([#|Ls],PROBLEM,[#(PROBLEM)|RLs]) :-
	!,rename_sharp(Ls,PROBLEM,RLs).
rename_sharp([-(#)|Ls],PROBLEM,[-(#(PROBLEM))|RLs]) :-
	!,rename_sharp(Ls,PROBLEM,RLs).
rename_sharp([H|Ls],PROBLEM,[H|RLs]) :-
	!,rename_sharp(Ls,PROBLEM,RLs).
	
%%% write clauses into Prolog's database

append_axioms_to_matrix([],[],[],[],_).
append_axioms_to_matrix([A|As],[ID|Ns],Matrix,IDs,Settings) :-
    ( member(fast,Settings) ->
      ( matrix_database(ID,M) ->
          true
          ; make_matrix(~(A),M,Settings), assertz(matrix_database(ID,M))
      )  
      ; make_matrix(~(A),M,Settings)
    ),
    clone_id(M,ID,Xs),
    append_axioms_to_matrix(As,Ns,W,Ys,Settings),
    append(M,W,Matrix), append(Xs,Ys,IDs).

clone_id([],_,[]).    
clone_id([X|Ls],N,[N|Ns]) :-
    clone_id(Ls,N,Ns). 

%%% write clauses into Prolog's database

assert_dnf([[- #]|M],[I|D],PRN,Set) :- assert_dnf(M,D,PRN,Set,NM).

assert_dnf([],[],_,_,[]).
assert_dnf([C|M],[I|D],PRN,Set,[dnf(Index,AX,C1,G,Features)|NM]) :-
%    (Set\=conj, \+member(-_,C) -> C1=[#|C] ; C1=C),
    rename_sharp(C,PRN,CSHARP),
    C1=CSHARP,
%    (C1=[#|_] -> AX=conjecture ; AX=axiom),
    (Set=conj -> AX=conjecture ; AX=axiom),
    (ground(C) -> G=g ; G=n),
    /*assert_renvar(C1,C2),*/ copy_term(C1,CC1),numbervars(CC1,0,_),term_hash(CC1,HC1),
    (dnf(HC1,C1_,G,Index,Fs),=@@@=(C1_,C1) -> %writeln(Index),
        % It is already in database
%        assert(dnf(HC1,C2,C1,G,Index,Features))
        true
      ; flag(index_of_dnf,Index,Index+1),
        assert(dnf(HC1,C1,G,Index,Features))
     ),
     (nonvar(I) ->
         %retract(dnf(_,_,_,_,Index,_)),
         %sort([I|IDs],New_IDs), writeln(I = IDs),
         assert(index_2_axioms(Index,I))
         ; true
     ),!,
    assert_dnf(M,D,PRN,Set,NM).
/*
assert_dnf2([],_,_).
assert_dnf2([L|C],C1,G) :-
    assert_renvar([L],[L2]), append(C1,C,C2), append(C1,[L],C3),
    (dnf(L2,L_,C2_,_G,Index),(L_,C2_)=@=(L,C2) -> 
       true % It is already in database    
     ; flag(index_of_dnf,Index,Index+1), assert(dnf(L2,L,C2,G,Index))
    ), assert_dnf2(C,C3,G) .
*/
% ------------------------------------------------------------------
%  make_matrix(+Fml,-Matrix,+Settings)
%    -  transform first-order formula into set of clauses (matrix)
%
%  Fml, Matrix: first-order formula and matrix
%
%  Settings: list of settings, which can contain def, nodef and conj;
%            if it contains nodef/def, no definitional transformation
%            or a complete definitional transformation is done,
%            otherwise a definitional transformation is done for
%            the conjecture and the standard transformation is done
%            for the axioms; conjecture is marked if conj is given
%
%  Syntax of Fml: negation '~', disjunction ';', conjunction ',',
%      implication '=>', equivalence '<=>', universal/existential
%      quantifier 'all X:<Formula>'/'ex X:<Formula>' where 'X' is a
%      Prolog variable, and atomic formulae are Prolog atoms.
%
%  Example: make_matrix(ex Y:(all X:((p(Y) => p(X)))),Matrix,[]).
%           Matrix = [[-(p(X1))], [p(1 ^ [X1])]]

make_matrix(Fml,Matrix,Set) :-
    skolem(_,_,S), !,
    univar(Fml,[],F1),
    ( member(conj,Set), F1=(A=>C) -> F2=((A,#)=>(#,C)) ; F2=F1 ),
    ( member(nodef,Set) ->
       def_nnf(F2,NNF,S,END,nnf), dnf(NNF,DNF)
       ;
       \+member(def,Set), F2=(B=>D) ->
        def_nnf(~(B),NNF,S,I,nnf), dnf(NNF,DNF1),
        def_nnf(D,DNF2,I,END,def), DNF=(DNF2;DNF1)
        ;
        def_nnf(F2,DNF,S,END,def)
    ),
    asserta(skolem(invalid,skolem_constant,END)),
    mat(DNF,Matrix).

% ------------------------------------------------------------------
%  def_nnf(+Fml,-DEF)  -  transform formula into a definitional
%                         Skolemized negation normal form (DEF)
%  Fml, DEF: first-order formula and formula in DEF
%
%  Example: def_nnf(ex Y:(all X:((p(Y) => p(X)))),DEF,def).
%           DEF = ~ p(X1) ; p(1 ^ [X1])

get_skolem_index(Fml,FreeV,I,I1,SKOLEM_IDX) :- !, 
    assert_renvar([Fml-FreeV],[SC]),
    (    
           
         skolem(SC,T,SKOLEM_IDX), 
	 =@@@=((Fml-FreeV),T) -> 
	  I1=I
	; asserta(skolem(SC,Fml-FreeV,I)), SKOLEM_IDX=I, I1 is I+1
    ).

get_skolem_name(N,FreeV,SKOLEM) :-
    name(sk,SKs),name(N,Ns),append(SKs,Ns,SKNs),name(SKN,SKNs),
    SKOLEM=..[SKN|FreeV].

def_nnf(Fml,DEF,I,I1,Set) :-
    def(Fml,[],NNF,DEF1,_,I,I1,Set), def(DEF1,NNF,DEF).

def([],Fml,Fml).
def([(A,(B;C))|DefL],DEF,Fml) :- !, def([(A,B),(A,C)|DefL],DEF,Fml).
def([A|DefL],DEF,Fml) :- def(DefL,(A;DEF),Fml).

def(Fml,FreeV,NNF,DEF,Paths,I,I1,Set) :-
    ( Fml = ~(~A)      -> Fml1 = A;
      Fml = ~(all X:F) -> Fml1 = (ex X: ~F);
      Fml = ~(ex X:F)  -> Fml1 = (all X: ~F);
      Fml = ~((A ; B)) -> Fml1 = ((~A , ~B));
      Fml = ~((A , B)) -> Fml1 = (~A ; ~B);
      Fml = (A => B)   -> Fml1 = (~A ; B);
      Fml = ~((A => B))-> Fml1 = ((A , ~B));
      Fml = (A <=> B)  ->
      ( Set=def        -> Fml1 = ((A => B) , (B => A));
                          Fml1 = ((A , B) ; (~A , ~B)) );
      Fml = ~((A<=>B)) -> Fml1 = ((A , ~B) ; (~A , B)) ), !,
    def(Fml1,FreeV,NNF,DEF,Paths,I,I1,Set).

def((ex X:F),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    def(F,[X|FreeV],NNF,DEF,Paths,I,I1,Set).

def((all X:Fml),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    get_skolem_index((all X:Fml),FreeV,I,I2,N),
    get_skolem_name(N,FreeV,SKOLEM),
%    copy_term((X,Fml,FreeV),((N^FreeV),Fml1,FreeV)),
    copy_term((X,Fml,FreeV),(SKOLEM,Fml1,FreeV)),
    def(Fml1,FreeV,NNF,DEF,Paths,I2,I1,Set).

def((A ; B),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    def(A,FreeV,NNF1,DEF1,Paths1,I,I2,Set),
    def(B,FreeV,NNF2,DEF2,Paths2,I2,I1,Set),
    append(DEF1,DEF2,DEF), Paths is Paths1 * Paths2,
    (Paths1 > Paths2 -> NNF = (NNF2;NNF1);
                        NNF = (NNF1;NNF2)).

def((A , B),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    def(A,FreeV,NNF3,DEF3,Paths1,I,I2,Set),
    ( NNF3=(_;_), Set=def -> get_skolem_index([NNF3],FreeV,I2,I3,N),
                             get_skolem_name(N,FreeV,SK1),
                             append([(~SK1,NNF3)],DEF3,DEF1),
                             NNF1=SK1 
                           ; DEF1=DEF3, NNF1=NNF3, I3 is I2 ),
    def(B,FreeV,NNF4,DEF4,Paths2,I3,I4,Set),
    ( NNF4=(_;_), Set=def -> get_skolem_index([NNF4],FreeV,I4,I1,M),
                             get_skolem_name(M,FreeV,SK2),
                             append([(~SK2,NNF4)],DEF4,DEF2),
                             NNF2=SK2 
                           ; DEF2=DEF4, NNF2=NNF4, I1 is I4 ),
    append(DEF1,DEF2,DEF), Paths is Paths1 + Paths2,
    (Paths1 > Paths2 -> NNF = (NNF2,NNF1);
                        NNF = (NNF1,NNF2)).

def(Lit,_,Lit,[],1,I,I,_).

%%% translate into leanCoP syntax

leancop_tptp2_(File,F) :- leancop_tptp2_(File,'',[_],_,F,_).

leancop_tptp2_(File,AxPath,AxNames,IDs,F,Con) :-
    open(File,read,Stream), ( fof2cop_(Stream,AxPath,AxNames,IDs,F,Con)
    -> close(Stream) ; close(Stream), fail ).

fof2cop_(Stream,AxPath,AxNames,IDs,F,Con) :-
    read(Stream,Term),
    ( Term=end_of_file -> IDs=[], F=[], Con=[] ;
      ( Term=..[fof,Name,Type,Fml|_] ->
        ( \+member(Name,AxNames) -> true ; fml2cop([Fml],[Fml1]) ),
        ( Type=conjecture -> Con=Fml1 ; Con=Con1 ) ;
        ( Term=include(File), AxNames2=[_] ;
          Term=include(File,AxNames2) ) -> name(AxPath,AL),
          name(File,FL), append(AL,FL,AxL), name(AxFile,AxL),
          leancop_tptp2_(AxFile,'',AxNames2,IDs2, Fs,_), Con=Con1
      ), fof2cop_(Stream,AxPath,AxNames,IDs1,F1,Con1),
      ( Term=..[fof,N,Type|_] -> 
         ((Type=conjecture;\+member(N,AxNames)) -> 
            (F1=[] -> IDs=[], F=[] ; IDs=IDs1, F=F1) 
          ; (F1=[] -> IDs=[Name], F=[Fml1] ; IDs= [Name|IDs1], F=[Fml1|F1]) 
          )
         ; append(IDs2,IDs1,IDs),append(Fs,F1,F)
      )    
    ).

fml2cop([],[]).
fml2cop([F|Fml],[F1|Fml1]) :-
    op_tptp2(F,F1,FL,FL1) -> fml2cop(FL,FL1), fml2cop(Fml,Fml1).

%%% add equality axioms

leancop_equal_axioms(Fs,F) :-
    collect_predfunc(Fs,PL,FL), append(PL2,[(=,2)|PL3],PL),
    append(PL2,PL3,PL1) -> basic_equal_axioms_(F0), 
      subst_pred_axioms_(PL1,F2), append(F0,F2,F3), 
      subst_func_axioms_(FL,F4), append(F3,F4,F)
    ; F=[].

basic_equal_axioms_(F) :-
    F=[( all X:(X=X) ),
       ( all X:all Y:((X=Y)=>(Y=X)) ),
       ( all X:all Y:all Z:(((X=Y),(Y=Z))=>(X=Z)) )].

% generate substitution axioms

subst_pred_axioms_([],[]).
subst_pred_axioms_([(P,I)|PL],F) :-
    subst_axiom(A,B,C,D,E,I), subst_pred_axioms_(PL,F1), P1=..[P|C],
    P2=..[P|D], E=(B,P1=>P2), F=[A|F1].

subst_func_axioms_([],[]).
subst_func_axioms_([(P,I)|FL],F) :-
    subst_axiom(A,B,C,D,E,I), subst_func_axioms_(FL,F1), P1=..[P|C],
    P2=..[P|D], E=(B=>(P1=P2)), F=[A|F1].
/*
subst_axiom((all X:all Y:E),(X=Y),[X],[Y],E,1).
subst_axiom(A,B,[X|C],[Y|D],E,I) :-
    I>1, I1 is I-1, subst_axiom(A1,B1,C,D,E,I1),
    A=(all X:all Y:A1), B=((X=Y),B1).

% collect predicate & function symbols

collect_predfunc([],[],[]).
collect_predfunc([F|Fml],PL,FL) :-
    ( ( F=..[<=>|F1] ; F=..[=>|F1] ; F=..[;|F1] ; F=..[','|F1] ;
        F=..[~|F1] ; (F=..[all,_:F2] ; F=..[ex,_:F2]), F1=[F2] ) ->
      collect_predfunc(F1,PL1,FL1) ; F=..[P|Arg], length(Arg,I),
      I>0 ->  PL1=[(P,I)], collect_func(Arg,FL1) ; PL1=[], FL1=[] ),
    collect_predfunc(Fml,PL2,FL2),
    union1(PL1,PL2,PL), union1(FL1,FL2,FL).

collect_func([],[]).
collect_func([F|FunL],FL) :-
    ( \+var(F), F=..[F1|Arg], length(Arg,I), I>0 ->
      collect_func(Arg,FL1), union1([(F1,I)],FL1,FL2) ; FL2=[] ),
    collect_func(FunL,FL3), union1(FL2,FL3,FL).

union1([],L,L).
union1([H|L1],L2,L3) :- member(H,L2), !, union1(L1,L2,L3).
union1([H|L1],L2,[H|L3]) :- union1(L1,L2,L3).
*/

nwriteln(T) :- copy_term(T,CT), numbervars(CT,0,_), print(CT),nl, !.


=@@@=(X,Y) :- copy_term(X,CX), copy_term(Y,CY), 
              numbervars(CX,0,_), numbervars(CY,0,_),!,
              CX=CY.

/*
=@@@=(X,Y) :- copy_term(X,CX), copy_term(Y,CY),
            numbervars(CX,0,_), numbervars(CY,0,_),!,
            (   CX=CY
            ->  (   X =@= Y
                ->  true
                ;   format(user_error, 'WRONG (must succeed): ~q~n', [ X =@= Y
])
                )
            ;   (   X =@= Y
                ->  format(user_error, 'WRONG (must fail): ~q~n', [ X =@= Y ])
                ;   true
                ),
                fail
            ).
*/

% Useless code for losers.

read_lines(S,Res):-
       read_line_to_codes(S,C),
       (C= end_of_file,Res=[];
           read_lines(S,Res1),
           string_to_atom(C,C1),
           Res = [C1|Res1]).

read_file_lines(File, List):-
       open(File,read,S),
       read_lines(S,List),
       close(S),!.
