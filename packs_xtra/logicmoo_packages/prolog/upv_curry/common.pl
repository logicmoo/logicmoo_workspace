:- set_prolog_flag(double_quotes, codes).
:- expects_dialect(sicstus).


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ----------------------------------------------------------------------
          Curry's Kernel - Needed narrowing and Residuation.    
  ----------------------------------------------------------------------
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
:- thread_local debugmode/0, solution/0, time/1,
   maxVar/1. % Stores the last variable identifier for current
             % expression in evaluation process

% susts ----------------------------------------------------------
susts( c(F,N,T1), c(F,N,T2), Susts ) :- susts_terms( T1, T2, [], Susts ).
susts( f(F,N,T1), f(F,N,T2), Susts ) :- susts_terms( T1, T2, [], Susts ).
susts( v(N),      Term,      [s(N,Term)] ). % this isn't unification, 
                                            % is pattern matching

susts_terms( [], [], S, S ).
susts_terms( [Term1|Tx1], [Term2|Tx2], S1, S3 ) :-
        susts( Term1, Term2, Susts ),
        sappend( S1, Susts, S2),
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

% posTree ---------------------------------------------------------
% SubTree in a definitional tree
posTree( Tree, [], Tree ) :- !.
posTree( tbranch(_,_,L), [O|Ox], Term ) :- !,posTreeL( L, [O|Ox], Term ).
posTree( tor(L1,L2), [O|Ox], Term ) :- !,posTreeL( [L1,L2], [O|Ox], Term ).

posTreeL( [(_,Tree)|_], [1|Ox], TreeR ) :- !,
        posTree( Tree, Ox, TreeR ).
posTreeL( [_|Trees], [N|Ox], TreeR ) :- N > 1,
        N1 is N - 1,
        posTreeL( Trees, [N1|Ox], TreeR ).

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
rename( c(F,N,Terms1), Inc, c(F,N,Terms2) ) :-
        renameL( Terms1, Inc, Terms2 ).
rename( f(F,N,Terms1), Inc, f(F,N,Terms2) ) :-
        renameL( Terms1, Inc, Terms2 ).
rename( v(N1), Inc, v(N2) ) :- N2 is N1 + Inc.

renameL( [], _, [] ).
renameL( [Term1|Terms1], Inc, [Term2|Terms2] ) :-
        rename( Term1, Inc, Term2 ),
        renameL( Terms1, Inc, Terms2 ).


% Tuple arity ---------------------------------------------------------
% Extract arity of tuples from name
tuple2arity(Name,Arity) :-
        atom(Name),
        name(Name,Chars),
        sappend(`Tuple`,ArityChars,Chars),
        number_chars(Arity,ArityChars).
   
arity2tuple(Arity,Name) :-
        number_codes(Arity,ArityChars),
        sappend(`Tuple`,ArityChars,Chars),
        sicstus_atom_chars(Name,Chars).

