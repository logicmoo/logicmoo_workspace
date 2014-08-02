:- module(mklinear, [mklinear/3]).

mklinear(Term, Linear, BindingL) :-
    term_variables(Term, VarL),
    exclude(singleton(Term), VarL, VarM), 
    mklinear(Term, Linear, VarM, BindingL, []).

singleton(T, V) :-
    occurrences_of_var(V, T, 1).

mklinear(Term, Linear, VarL) -->
    {compound(Term)},
    !,
    { functor(Term, F, A),
      functor(Linear, F, A)
    },
    mklinear(1, Term, Linear, VarL).
mklinear(Term, Var, VarL) -->
    ( { var(Term),
	member(Var0, VarL),
	Term==Var0
      }
    ->[Term=Var]
    ; {Term=Var}
    ).

mklinear(N, Term, Linear, VarL) -->
    { arg(N, Term, Arg),
      !,
      arg(N, Linear, LArg),
      succ(N, N1)
    },
    mklinear(Arg, LArg, VarL),
    mklinear(N1, Term, Linear, VarL).
mklinear(_, _, _, _) --> [].
