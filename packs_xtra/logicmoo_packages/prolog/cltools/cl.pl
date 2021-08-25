/* -*- Mode: Prolog -*- */

:- module(cl,
          [cltext/1,
           assert_cltext/1,
           clear_cltext/0,
           sentence/1,
           text_sentence/2,
           remove_matching_sentences/3,
           macro_expand/3]).

:- multifile(cltext/1).
:- dynamic(cltext/1).

assert_cltext(T) :-
        assert(cltext(T)).
clear_cltext :-
        retractall(cltext(_)).


sentence(S) :-
        cltext(SL),
        text_sentence(SL,S).
text_sentence(cltext(SL),S) :-
        text_sentence(SL,S).
text_sentence(module(_,_,SL),S) :-
        text_sentence(SL,S).
text_sentence(SL,S) :-
        member(S,SL).
text_sentence(SL,S) :-
        member('$comment'(_,S),SL).

remove_matching_sentences(_,S,S) :-
        var(S),
        !.
remove_matching_sentences(_,S,S) :-
        atom(S),
        !.
remove_matching_sentences(_Templates,[],[]) :- !.
remove_matching_sentences(Templates,[S|SL],SL2) :- 
        \+ \+ member(S,Templates),
        !,
        remove_matching_sentences(Templates,SL,SL2).
remove_matching_sentences(Templates,[S|SL],[S2|SL2]) :- 
        !,
        remove_matching_sentences(Templates,S,S2),
        remove_matching_sentences(Templates,SL,SL2).
remove_matching_sentences(Templates,S,S2) :-
        S=..L,
        remove_matching_sentences(Templates,L,L2),
        S2=..L2.


% quantified sentence to prolog
% 1 level only
qsent_prolog(forall(VarSyms,S),forall(Vars,PlTerm)) :-
        findall(Sym-_,member(Sym,VarSyms),SymVarMap),
        symvarmap_vars(SymVarMap,Vars),
        mapsyms(SymVarMap,S,PlTerm).

symvarmap_vars([],[]).
symvarmap_vars([_-V|L],[V|L2]) :- symvarmap_vars(L,L2).

mapsyms(_,S,S) :-
        var(S),
        !.
mapsyms(Map,S,S2) :-
        atom(S),
        member(S-S2,Map),
        !.
mapsyms(_,S,S) :-
        atom(S),
        !.
mapsyms(Map,S,S2) :-
        S=..L,
        maplist(mapsyms(Map),L,L2),
        L2=[V1|_],
        (   var(V1)
        ->  S2=L2
        ;   S2=..L2).

%% macro_expand(+Text,+MacroText,?Text2)
% 
macro_expand(Text,MacroText,Text2) :-
        findall(S2,
                (   text_sentence(MacroText,S),
                    qsent_prolog(S,S2)),
                Macros),
        format(user_error,'macros=~w~n',[Macros]),
        findall(S2,
                (   text_sentence(Text,S),
                    tr(S,Macros,S2)),
                Text2).


tr(S,Macros,S2) :-
        setof(S2,Macros^tr1(S,Macros,S2),S2s),
        !,
        member(S2,S2s).
tr(S,_,S).

tr1('$comment'(X,S),Macros,'$comment'(X,S2)) :-
        !,
        tr1(S,Macros,S2).
tr1(S,Macros,S2) :-
        member(Macro,Macros),
        Macro=forall(_Vars,if(S,S2)).
tr1(S,Macros,S2) :-
        member(Macro,Macros),
        Macro=forall(_Vars,iff(S,S2)).
%tr(S,Macros,S2) :-
%        member(Macro,Macros),
%        Macro=forall(_Vars,iff(If,Then)),
%        holds(If).

holds(and(X,Y)) :-
        holds(X),
        holds(Y).
holds(X) :-
        sentence(X).




        
